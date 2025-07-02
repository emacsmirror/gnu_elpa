;;; minimail.el --- Simple, non-blocking IMAP email client            -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Augusto Stoffel

;; Author: Augusto Stoffel <arstoffel@gmail.com>
;; Keywords: mail

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Note on symbol names:
;; - Private symbols start with the "empty prefix" consisting of a
;;   single dash.  This is expanded via the shorthand mechanism to the
;;   usual `minimail--' prefix.
;; - Public symbols are always written in full and have the
;;   `minimail-' prefix.
;; - The `athunk-' prefix is expanded to `minimail--athunk-'.  This
;;   part of the code is independent and could be moved to a separate
;;   package.

;;; Code:

(require 'gnus-art)
(require 'peg)      ;need peg.el from Emacs 30, which is ahead of ELPA
(require 'rx)
(require 'smtpmail)
(require 'vtable)

(eval-when-compile
  (require 'subr-x))

;;; Syntactic sugar for cooperative multitasking
;;
;; A delayed and potentially concurrent computation is represented by
;; what we dub an asynchronous thunk or "athunk".  That is simply a
;; function that takes as argument a callback function or
;; "continuation".  The continuation in turn takes two arguments: an
;; error symbol (which can be nil to indicate success) and a value.
;; The role of the athunk, when called, is to perform some
;; computations and arrange for the continuation to be eventually
;; called.
;;
;; Below are some utilities to write "high-level code" with athunks.
;; This is in a similar vein to the async/await pattern found in some
;; other languages.  The high level code looks linear and doesn't
;; involve callbacks.  There is support for non-local error treatment.
;;
;; References:
;; - https://jyp.github.io/posts/elisp-cps.html
;; - https://emacsconf.org/2022/talks/async/

(defmacro athunk-let* (bindings &rest body)
  "Sequentially resolve athunks then evaluate BODY.
BINDINGS is a list of elements of the form (SYMBOL FORM), where FORM
evaluates to an athunk.  Return an athunk which resolves to the value of
the last form in BODY."
  (declare (indent 1))
  (let* ((csym (gensym))                ;the continuation
         (esym (gensym))                ;the error, possibly nil
         (vsym (gensym))                ;the computed value
         (form `(condition-case ,vsym ,(macroexp-progn body)
                  (:success (funcall ,csym nil ,vsym))
                  (t (funcall ,csym (car ,vsym) (cdr ,vsym))))))
    (pcase-dolist (`(,var ,athunk) (reverse bindings))
      (setq form `(funcall ,athunk
                           (lambda (,esym ,vsym)
                             (if ,esym
                                 (funcall ,csym ,esym ,vsym)
                               (let ((,var ,vsym)) ,form))))))
    `(lambda (,csym) ,form)))

(defun athunk-gather (athunks)
  "Resolve all ATHUNKS and return a vector of results."
  (let* ((n (length athunks))
         (result (make-vector n nil)))
    (lambda (cont)
      (dotimes (i n)
        (funcall (pop athunks)
                 (lambda (err val)
                   (if err
                       (funcall cont err val)
                     (setf (aref result i) val)
                     (when (zerop (cl-decf n))
                       (funcall cont nil result)))))))))

(defmacro athunk-let (bindings &rest body)
  "Concurrently resolve athunks then evaluate BODY.
BINDINGS is a list of elements of the form (SYMBOL FORM), where FORM
evaluates to an athunk.  Return an athunk which resolves to the value of
the last form in BODY."
  (declare (indent 1))
  (if (length< bindings 2)              ;optimization
      `(athunk-let* ,bindings ,@body)
    (let ((vec (gensym))
          (athunks (mapcar #'cadr bindings))
          (vars (mapcar #'car bindings)))
      `(athunk-let* ((,vec (athunk-gather (list ,@athunks))))
         (let ,(seq-map-indexed (lambda (v i) `(,v (aref ,vec ,i))) vars)
           ,@body)))))

(defun athunk-sleep (secs &optional value)
  "Return an athunk that waits SECS seconds and then returns VALUE."
  (lambda (cont)
    (run-with-timer secs nil cont nil value)))

(defun athunk-do (athunk)
  "Execute ATHUNK for side-effects.
Any uncatched errors are signaled, but notice this will happen at a
later point in time."
  (prog1 nil
    (funcall athunk (lambda (err val) (when err (signal err val))))))

(defun athunk-debug (athunk &optional prefix)
  "Execute ATHUNK and display result in a message."
  (prog1 nil
    (funcall athunk (lambda (err val)
                      (message "%s:%s:%S" (or prefix "athunk-debug") err val)
                      (when err (signal err val))))))

(defmacro athunk-wrap (&rest body)
  "Wrap BODY in an athunk for delayed execution."
  (declare (indent 0))
  `(athunk-let* nil ,@body))

(defmacro athunk-condition-case (var form &rest handlers)
  "Like `condition-case', but for asynchronous code.

FORM should evaluate to an athunk.  Return a new athunk that normally
produces the same result as the original; however, if an error is
signaled and one of the HANDLERS apply, then evaluate the handler an
return its result instead.

See `condition-case' for the exact meaning of VAR and HANDLERS."
  (declare (indent 2))
  (let ((csym (gensym))                 ;the continuation
        (esym (gensym))                 ;the error, possibly nil
        (vsym (gensym))                 ;the computed value
        (hsym (gensym)))                ;helper symbol to hold an error
    (unless (assq :success  handlers)
      (push  `(:success ,vsym) handlers))
    `(lambda (,csym)
       (funcall ,form
                (lambda (,esym ,vsym)
                  (condition-case ,hsym
                      (condition-case ,var
                          (when ,esym (signal ,esym ,vsym))
                        ,@handlers)
                    (:success (funcall ,csym nil ,hsym))
                    (t (funcall ,csym (car ,hsym) (cdr ,hsym)))))))))

(defmacro athunk-ignore-errors (&rest body)
  "Like `ignore-errors', but for asynchronous code."
  (declare (indent 0))
  `(athunk-condition-case nil ,(macroexp-progn body) (error nil)))

(defmacro athunk-memoize (place &rest body)
  "Like `with-memoization' for asynchronous code.
BODY should evaluate to an athunk.  When it's resolved, store the result
in PLACE.  If there is already a value stored in PLACE, use it instead."
  (declare (indent 1))
  ;; TODO: error handling
  `(lambda (cont)
     (pcase-exhaustive ,place
       (`(athunk--cached . ,val)
        (funcall cont nil val))
       (`(athunk--pending . ,conts)
        (nconc conts `(,cont)))
       ('nil
        (setf ,place `(athunk--pending ,cont))
        (funcall ,(macroexp-progn body)
                 (lambda (err val)
                   (let ((conts (cdr ,place)))
                     (setf ,place (unless err `(athunk--cached . ,val)))
                     (dolist (k conts)
                       (funcall k err val))))))))) ;FIXME: should ignore errors?

(defmacro athunk-unmemoize (place)
  "Forget the memoized value in PLACE.
This only has an effect if the value has been already computed; if it is
pending the computation is not canceled."
  (gv-letplace (getter setter) place
    `(when (eq 'athunk--cached (car ,getter)) ,(funcall setter nil))))

;;; Variables and options

(defgroup minimail nil
  "Simple, non-blocking IMAP email client."
  :prefix "minimail-"
  :group 'mail)

(defcustom minimail-accounts nil
  "Account configuration for the Minimail client.
This is an alist where keys are names used to refer to each account and
values are a plist with the following information:

:mail-address
  The email address of this account, used to override the global value
  of `user-mail-address'.

:incoming-url
  Information about the IMAP server as a URL. Normally, it suffices to
  enter \"imaps://<server-address>\".  More generally, it can take the form

    imaps://<username>:<password>@<server-address>:<port>

  If username is omitted, use the :mail-address property instead.

  If password is omitted (which is highly recommended), use the
  auth-source mechanism. See Info node `(auth) Top' for details.

:outgoing-url
  Information about the SMTP server as a URL.  Normally, it suffices
  to enter \"smtps://<server-address>\", but you can provide more
  details as in :incoming-url.

:signature
  Overrides the global value of `message-signature'.

:signature-file
  Overrides the global value of `message-signature-file'.

All entries all optional, except for `:incoming-url'."
  :type '(alist
          :key-type (symbol :tag "Account identifier")
          :value-type (plist
                       :tag "Account properties"
                       :value-type string
                       :options (:mail-address
                                 :incoming-url
                                 :outgoing-url
                                 (:signature (choice
                                              (const :tag "None" ignore)
		                              (const :tag "Use `.signature' file" t)
		                              (string :tag "String to insert")
                                              (sexp :tag "Expression to evaluate")))
                                 (:signature-file file)))))

(defcustom minimail-reply-cite-original t
  "Whether to cite the original message when replying."
  :type 'boolean)

(defcustom minimail-connection-idle-timeout 60
  "Time in seconds a network connection can remain open without activity."
  :type 'boolean)

(defface minimail-unread '((t :inherit bold))
  "Face for unread messages.")

;;; Miscellaneous stuff

(defvar -account-state nil
  "Alist mapping accounts to assorted state information about them.")

(defvar-local -local-state nil
  "Place to store assorted buffer-local information.")

(defvar-local -mode-line-suffix nil)

(defvar -minibuffer-update-hook nil
  "Hook run when minibuffer completion candidates are updated.")

(defvar -debug-buffer nil
  "If non-nil, name of a buffer to display debug information.")

(define-error '-imap-error "error in IMAP response")

(defmacro -get-in (alist key &rest rest)
  (let ((v `(alist-get ,key ,alist nil nil #'equal)))
    (if rest `(-get-in ,v ,(car rest) ,@(cdr rest)) v)))

(defun -get-data (string)
  "Get data stored as a string property in STRING."
  (get-text-property 0 '-data string))

(defvar -log-buffer nil
  "Name of the log buffer, or nil to disable logging.")

(defun -log-message-1 (&rest args)
  "Helper function for `minimail--log-message'.
ARGS is the entire argument list of `minimail--log-message'."
  (with-current-buffer (get-buffer-create -log-buffer)
    (setq-local outline-regexp "")
    (goto-char (point-max))
    (when-let* ((w (get-buffer-window)))
      (set-window-point w (point)))
    (insert #(""  0 1 (invisible t))
            (propertize (format-time-string "[%T] ") 'face 'error)
            (apply #'format args)
            ?\n)))

(defmacro -log-message (string &rest args)
  "Write a message to buffer pointed by `minimail--log-buffer', if non-nil.
The message is formed by calling `format' with STRING and ARGS."
  `(when -log-buffer (-log-message-1 ,string ,@args)))

;;; Low-level IMAP communication

;; References:
;; - IMAP4rev1: https://datatracker.ietf.org/doc/html/rfc3501
;; - IMAP4rev2: https://datatracker.ietf.org/doc/html/rfc9051
;; - IMAP URL syntax: https://datatracker.ietf.org/doc/html/rfc5092

(defvar-local -imap-callbacks nil)
(defvar-local -imap-capability nil)
(defvar-local -imap-command-queue nil)
(defvar-local -imap-idle-timer nil)
(defvar-local -imap-last-tag nil)
(defvar-local -next-position nil) ;TODO: necessary? can't we just rely on point position?
(defvar-local -selected-mailbox nil)

(defun -imap-connect (account)
  "Return a network stream connected to ACCOUNT."
  (let* ((props (or (alist-get account minimail-accounts)
                    (error "Invalid account: %s" account)))
         (url (url-generic-parse-url (plist-get props :incoming-url)))
         (stream-type (pcase (url-type url)
                        ("imaps" 'tls)
                        ("imap" 'starttls)
                        (other (user-error "\
In `minimail-accounts', incoming-url must have imaps or imap scheme, got %s" other))))
         (user (cond ((url-user url) (url-unhex-string (url-user url)))
                     ((plist-get props :mail-address))))
         (pass (or (url-password url)
                   (auth-source-pick-first-password
                    :user user
                    :host (url-host url)
                    :port (url-portspec url))
                   (error "No password found for account %s" account)))
         (buffer (generate-new-buffer (format " *minimail-%s*" account)))
         (proc (open-network-stream
                (format "minimail-%s" account)
                buffer
                (url-host url)
                (or (url-portspec url)
                    (pcase stream-type
                      ('tls 993)
                      ('starttls 143)))
                :type stream-type
                :coding 'binary
                :nowait t)))
    (add-function :after (process-filter proc) #'-imap-process-filter)
    (set-process-sentinel proc #'-imap-process-sentinel)
    (set-process-query-on-exit-flag proc nil)
    (with-current-buffer buffer
      (set-buffer-multibyte nil)
      (setq -imap-last-tag 0)
      (setq -imap-idle-timer (run-with-timer
                              minimail-connection-idle-timeout nil
                              #'delete-process proc))
      (setq -next-position (point-min)))
    (-imap-enqueue
     proc nil
     (cond
      ;; TODO: use ;AUTH=... notation as in RFC5092?
      ((string-empty-p user) "AUTHENTICATE ANONYMOUS\r\n")
      (t (format "AUTHENTICATE PLAIN %s"
                 (base64-encode-string (format "\0%s\0%s"
                                               user pass)))))
     (lambda (status message)
       (unless (eq status 'ok)
         (lwarn 'minimail :error "IMAP authentication error (%s):\n%s"
                account message))))
    proc))

(defun -imap-process-sentinel (proc message)
  (-log-message "sentinel: %s %s" proc (process-status proc))
  (pcase (process-status proc)
    ('open
     (with-current-buffer (process-buffer proc)
       (when-let* ((queued (pop -imap-command-queue)))
         (apply #'-imap-send proc queued))))
    ((or 'closed 'failed)
     (with-current-buffer (process-buffer proc)
       (pcase-dolist (`(_ _ . ,cb) -imap-callbacks)
         (funcall cb 'error message)))
     (kill-buffer (process-buffer proc)))))

(defun -imap-process-filter (proc _)
  (timer-set-time -imap-idle-timer
                  (time-add nil minimail-connection-idle-timeout))
  (let ((pos -next-position))
    (when (< pos (point-max))
      (goto-char pos)
      (while (re-search-forward "{\\([0-9]+\\)}\r\n" nil t)
        (let ((pos (+ (point) (string-to-number (match-string 1)))))
          (setq -next-position pos)
          (goto-char (min pos (point-max)))))
      (if (re-search-forward (rx bol
                                 ?A (group (+ digit))
                                 ?\s (group (+ alpha))
                                 ?\s (group (* (not control)))
                                 (? ?\r) ?\n)
                             nil t)
          (pcase-let* ((end (match-beginning 0))
                       (cont (match-end 0))
                       (tag (string-to-number (match-string 1)))
                       (status (intern (downcase (match-string 2))))
                       (message (match-string 3))
                       (`(,mailbox . ,callback) (alist-get tag -imap-callbacks)))
            (setf (alist-get tag -imap-callbacks nil t) nil)
            (-log-message "response: %s %s\n%s"
                          proc
                          (or -selected-mailbox "(unselected)")
                          (buffer-string))
            (unwind-protect
                (if (and mailbox (not (equal mailbox -selected-mailbox)))
                    (error "Wrong mailbox: %s expected, %s selected"
                           mailbox -selected-mailbox)
                  (with-restriction (point-min) end
                    (goto-char (point-min))
                    (funcall callback status message)))
              (delete-region (point-min) cont)
              (setq -next-position (point-min))
              (when-let* ((queued (pop -imap-command-queue)))
                (apply #'-imap-send proc queued))))
        (goto-char (point-max))
        (setq -next-position (pos-bol))))))

(defun -imap-send (proc tag mailbox command)
  "Execute an IMAP COMMAND (provided as a string) in network stream PROC.
TAG is an IMAP tag for the command.
Ensure the given MAILBOX is selected before issuing the command, unless
it is nil."
  (if (or (not mailbox)
          (equal mailbox -selected-mailbox))
      (process-send-string proc (format "A%s %s\r\n" tag command))
    ;; Need to select a different mailbox
    (let ((newtag (cl-incf -imap-last-tag))
          (cont (lambda (status message)
                  (if (eq 'ok status)
                      (progn
                        (setq -selected-mailbox mailbox)
                        ;; Trick: this will cause the process filter
                        ;; to call `-imap-send' with the original
                        ;; command next.
                        (push (list tag mailbox command) -imap-command-queue))
                    (let ((callback (alist-get tag -imap-callbacks)))
                      (setf (alist-get tag -imap-callbacks nil t) nil)
                      (funcall callback status message))))))
      (push `(,newtag nil . ,cont) -imap-callbacks)
      (process-send-string proc (format "A%s SELECT %s\r\n"
                                        newtag (-imap-quote mailbox))))))

(defun -imap-enqueue (proc mailbox command callback)
  (with-current-buffer (process-buffer proc)
    (let ((tag (cl-incf -imap-last-tag)))
      (if (or -imap-callbacks
              ;; Sending to process in `connect' state blocks Emacs,
              ;; so delay it
              (not (eq 'open (process-status proc))))
          (cl-callf nconc -imap-command-queue `((,tag ,mailbox ,command)))
        (-imap-send proc tag mailbox command))
      (push `(,tag ,mailbox . ,callback) -imap-callbacks))))

;;; IMAP parsing

;; References:
;; - Formal syntax: https://datatracker.ietf.org/doc/html/rfc3501#section-9

(defalias '-imap-quote #'json-serialize ;good enough approximation
  "Make a quoted string as per IMAP spec.")

(defalias '-imap-unquote #'json-parse-string
  "Parse a quoted string as per IMAP spec.")

(defconst -imap-months
  ["Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"])

(defun -imap-parse-error (pegs)
  "Report an error parsing IMAP server response."
  (-log-message "parsing failed at position %s:%s: %S"
                (line-number-at-pos)
                (- (point) (pos-bol) -1)
                pegs)
  (error "Error parsing IMAP response"))

(define-peg-ruleset -imap-peg-rules
  (sp        () (char ?\s))
  (untagged  () (bol) "* ")
  (crlf      () "\r\n")
  (anil      () "NIL" `(-- nil))
  (number    () (substring (+ [0-9])) `(s -- (string-to-number s)))
  (achar     () (and (not [cntrl "(){] %*\"\\"]) (any))) ;characters allowed in an atom
  (atom      () (substring (+ achar)))  ;non-quoted identifier a.k.a. atom
  (qchar     () (or (and "\\" (any))    ;character of a quoted string
                    (and (not "\"") (any))))
  (qstring   () ;;quoted string
             (substring "\"" (* qchar) "\"")
             `(s -- (-imap-unquote s)))
  (literal   ()
             (guard (re-search-forward "\\=~?{\\([0-9]+\\)}\r\n" nil t))
             (region
              (guard (progn (forward-char (string-to-number (match-string 1)))
                            t))))
  (string    () (or qstring (and literal `(start end -- (buffer-substring-no-properties start end)))))
  (qpstring  () ;;quoted string, QP-encoded
             string
             `(s -- (mail-decode-encoded-word-string s)))
  (astring   () (or atom string))
  (nstring   () (or anil string))
  (nqpstring () (or anil qpstring))
  (timezone  ()
             (or (and (char ?+) `(-- +1))
                 (and (char ?-) `(-- -1)))
             number
             `(sign n -- (let* ((h (/ n 100))
                                (m (mod n 100)))
                           (* sign (+ (* 3600 h) (* 60 m))))))
  (month     ()
             (substring [A-Z] [a-z] [a-z])
             `(s -- (1+ (seq-position -imap-months s))))
  (imapdate  ()
             (char ?\") (opt sp) number (char ?-) month (char ?-) number
             sp number (char ?:) number (char ?:) number
             sp timezone (char ?\")
             `(day month year hour min sec tz
                   -- `(,sec ,min ,hour ,day ,month ,year nil -1 ,tz)))
  (flag      ()
             (substring (opt "\\") (+ achar))
             `(s -- (intern s)))
  (to-eol    () ;;consume input until eol
             (* (and (not [cntrl]) (any))) "\r\n")
  (to-rparen () ;;consume input until closing parens
             (* (or (and "(" to-rparen)
                    (and "\"" (* qchar) "\"")
                    (and (not [cntrl "()\""]) (any))))
             ")")
  (balanced  () "(" to-rparen))

(defun -parse-capability ()
  (with-peg-rules
      (-imap-peg-rules
       (iatom (substring (+ (and (not (char ?=)) achar)))
              `(s -- (intern (downcase s))))
       (paramcap iatom (char ?=) (or number iatom)
                 `(k v -- (cons k v)))
       (caps (list (* sp (or paramcap iatom)))))
    (car-safe
     (peg-run (peg untagged "CAPABILITY" caps crlf)
              #'-imap-parse-error))))

(defun -parse-list ()
  (with-peg-rules
      (-imap-peg-rules
       (flags (list (* (opt sp) flag)))
       (item untagged "LIST (" flags ") "
             (or astring anil)
             sp astring
             `(f d n -- `(,n (delimiter . ,d) (attributes . ,f))))
       (status untagged "STATUS "
               (list astring " ("
                     (* (opt sp)
                        (or (and "MESSAGES " number `(n -- `(messages . ,n)))
                            (and "RECENT " number `(n -- `(recent . ,n)))
                            (and "UIDNEXT " number `(n -- `(uid-next . ,n)))
                            (and "UIDVALIDITY " number `(n -- `(uid-validity . ,n)))
                            (and "UNSEEN " number `(n -- `(unseen . ,n)))))
                     ")"))
       (response (list (* (or item status) crlf))))
    (let* ((lines (car (peg-run (peg response))))
           (grouped (seq-group-by #'car lines)))
      (mapcar (pcase-lambda (`(,k . ,v)) `(,k . ,(mapcan #'cdr v)))
              grouped))))

(defun -parse-select ()
  (with-peg-rules
      (-imap-peg-rules
       (item (or
              (and "FLAGS (" (list (* (opt sp) flag)) ")" crlf
                   `(v -- `(flags . ,v)))
              (and number " EXISTS" crlf
                   `(n -- `(exists . ,n)))
              (and number " RECENT" crlf
                   `(n -- `(recent . ,n)))
              (and "OK [UNSEEN " number "]" to-eol
                   `(n -- `(unseen . ,n)))
              (and "OK [UIDNEXT " number "]" to-eol
                   `(n -- `(uid-next . ,n)))
              (and "OK [UIDVALIDITY " number "]" to-eol
                   `(n -- `(uid-validity . ,n)))
              (and "OK" to-eol))))
    (car-safe
     (peg-run (peg (list (* untagged item)))))))

(defun -parse-fetch ()
  (with-peg-rules
      (-imap-peg-rules
       (address "("
                (list (and nqpstring `(s -- `(name . ,s)))
                      sp (and nstring `(s -- `(adl . ,s)))
                      sp (and nstring `(s -- `(mailbox . ,s)))
                      sp (and nstring `(s -- `(host . ,s))))
                ")")
       (addresses (or anil
                      (and "(" (list (* address)) ")")))
       (envelope "ENVELOPE ("
                 (list nstring `(s -- `(date . ,(when s (parse-time-string s))))
                       sp nqpstring `(s -- `(subject . ,s))
                       sp addresses `(v -- `(from . ,v))
                       sp addresses `(v -- `(sender . ,v))
                       sp addresses `(v -- `(reply-to . ,v))
                       sp addresses `(v -- `(to . ,v))
                       sp addresses `(v -- `(cc . ,v))
                       sp addresses `(v -- `(bcc . ,v))
                       sp nstring `(v -- `(in-reply-to . ,v))
                       sp nstring `(v -- `(message-id . ,v)))
                 ")"
                 `(s -- `(envelope . ,s)))
       (body-param (or anil
                       (list "("
                             (+ (opt sp) qstring sp qstring
                                `(k v -- (cons k v)))
                             ")")))
       (body-single "("
                    (list qstring `(s -- `(media-type . ,s))
                          sp qstring `(s -- `(media-subtype . ,s))
                          sp body-param `(s -- `(media-params . ,s))
                          sp nstring `(s -- `(id . ,s))
                          sp nstring `(s -- `(description . ,s))
                          sp qstring `(s -- `(encoding . ,s))
                          sp number `(s -- `(octets . ,s))
                          (opt sp envelope sp (or body-multi body-single))
                          (opt sp number `(s -- `(lines . ,s)))
                          (* sp astring)) ;body extensions, ignored here
                    ")")
       (body-multi "("
                   (list (list (+ (or body-multi body-single)))
                         `(v -- `(parts . ,v))
                         sp qstring
                         `(s -- '(media-type . "MULTIPART") `(media-subtype . ,s)))
                   ")")
       ;; (body "BODY " (or body-single body-multi)
       ;;       `(s -- `(body . ,s)))
       (body "BODY " ;; (funcall (lambda () (forward-sexp) t))
             balanced
             )
       (content "BODY[] " literal `(start end -- `(content ,start . ,end)))
       (flags "FLAGS (" (list (* (opt sp) flag)) ")"
              `(v -- `(flags . ,v)))
       (internaldate "INTERNALDATE " imapdate
                     `(v -- `(date . ,v)))
       (size "RFC822.SIZE " number `(n -- `(rfc822-size . ,n)))
       (uid "UID " number `(n -- `(uid . ,n)))
       (item untagged number `(n -- `(id . ,n))
             " FETCH ("
             (* (opt sp) (or envelope body content flags internaldate size uid))
             ")" crlf))
    (car-safe
     (peg-run (peg (list (* (list item))))))))

(defun -parse-search ()
  (with-peg-rules
      (-imap-peg-rules
       (search "SEARCH" (list (* sp number)))
       (esearch "ESEARCH"
                (list
                 (* sp (or (and "(TAG " qstring `(s -- `(tag . ,s)) ")")
                           (and "UID" `(-- '(uid . t)))
                           (and "ALL " atom `(s -- `(set . ,s)))
                           (and "MIN " number `(n -- `(min . ,n)))
                           (and "MAX " number `(n -- `(max . ,n)))
                           (and "COUNT " number `(n -- `(count . ,n))))))))
    (car-safe
     (peg-run (peg untagged (or esearch search) crlf)))))

;;; Async IMAP requests

(defun -amake-request (account mailbox command)
  "Issue COMMAND to the ACCOUNT's IMAP server.
If MAILBOX is non-nil, ensure it is selected beforehand.

Returns an athunk which resolves to a temporary buffer containing the
server response.  The temporary buffer is cleaned up automatically after
being used."
  (lambda (cont)
    (let ((proc (-get-in -account-state account 'process)))
      (unless (process-live-p proc)
        (setq proc (-imap-connect account))
        (setf (-get-in -account-state account 'process) proc))
      (-imap-enqueue
       proc mailbox command
       (lambda (status message)
         (if (not (eq 'ok status))
             (funcall cont '-imap-error (list status message))
           (let* ((buffer (current-buffer))
                  (tmpbuf (generate-new-buffer " *minimail-temp*"))
                  (continue (lambda ()
                              (unwind-protect
                                  (funcall cont nil tmpbuf)
                                (kill-buffer tmpbuf)))))
             (with-current-buffer tmpbuf
               (set-buffer-multibyte nil)
               (insert-buffer-substring buffer)
               (goto-char (point-min)))
             (run-with-idle-timer 0 nil continue))))))))

(defun -aget-capability (account)
  (athunk-memoize (-get-in -account-state account 'capability)
    (athunk-let* ((buffer (-amake-request account nil "CAPABILITY")))
      (with-current-buffer buffer
        (-parse-capability)))))

(defun aget-mailbox-listing (account &optional refresh)
  (when refresh
    (athunk-unmemoize (-get-in -account-state account 'mailboxes)))
  (athunk-memoize (-get-in -account-state account 'mailboxes)
    (let* ((props (alist-get account minimail-accounts))
           (url (url-generic-parse-url (plist-get props :incoming-url)))
           (path (string-remove-prefix "/" (car (url-path-and-query url)))))
      (athunk-let*
          ((caps (-aget-capability account))
           (buffer (-amake-request
                    account nil
                    (format "LIST %s *%s"
                            (-imap-quote path)
                            (if (memq 'list-status caps)
                                " RETURN (STATUS (MESSAGES UIDNEXT UNSEEN))"
                              "")))))
      (with-current-buffer buffer
        (-parse-list))))))

(defun -aget-mailbox-status (account mailbox)
  (athunk-let*
      ((buffer (-amake-request account nil
                               (format "EXAMINE %s" (-imap-quote mailbox)))))
    (with-current-buffer buffer
      (-parse-select))))

(defun -afetch-id (account mailbox uid)
  "Fetch a message ID given its UID, MAILBOX and ACCOUNT."
  (athunk-let*
      ((buffer (-amake-request account mailbox
                               (format "%sFETCH %s (UID)"
                                       (if uid "UID " "")
                                       (or uid "*")))))
    ;;FIXME: uid=nil was supposed to retrieve the highest id, but
    ;;servers seem to implement some kind of caching that make it not
    ;;work.
    (with-current-buffer buffer
      (alist-get 'id (car (-parse-fetch))))))

(defun -afetch-mailbox (account mailbox num &optional end)
  (athunk-let*
      ((status (-aget-mailbox-status account mailbox))
       (buffer (-amake-request account mailbox
                               (let* ((endid (alist-get 'exists status))
                                      (last (if end (1- endid) endid)) ;FIXME?
                                      (first (max 1 (- last num -1))))
                                 (format "FETCH %s:%s (UID FLAGS RFC822.SIZE ENVELOPE)"
                                         first last)))))
    (with-current-buffer buffer
      (-parse-fetch))))

(defun -afetch-message (account mailbox uid)
  (athunk-let*
      ((buffer (-amake-request account mailbox
                               (format "UID FETCH %s (BODY[])" uid))))
    (with-current-buffer buffer
      (pcase-let* ((data (car (-parse-fetch)))
                   (`(,start . ,end) (alist-get 'content data)))
        (narrow-to-region start end)
        (goto-char (point-min))
        ;; Somehow needed to make quoted-printable decoding work...
        (replace-string-in-region "\r\n" "\n")
        buffer))))

(defun -afetch-search (account mailbox text)
  (athunk-let*
      ((sbuf (-amake-request account mailbox
                             (format "UID SEARCH CHARSET UTF-8 TEXT %s"
                                     (-imap-quote text))))
       (fbuf (let ((uids (with-current-buffer sbuf (-parse-search))))
               (-amake-request account mailbox
                               (format "UID FETCH %s (UID FLAGS RFC822.SIZE ENVELOPE)"
                                       (mapconcat #'number-to-string uids ","))))))
    (with-current-buffer fbuf
      (-parse-fetch))))

;;; Major modes

(defmacro -with-associated-buffer (buffer &rest body)
  (declare (indent 1))
  (let ((bsym (gensym)))
    `(let ((,bsym (if (derived-mode-p ',(intern (format "minimail-%s-mode" buffer)))
                      (current-buffer)
                    (-get-in -local-state ',(intern (format "%s-buffer" buffer))))))
       (unless (buffer-live-p ,bsym)
         (user-error "No %s buffer" ',buffer))
       (with-current-buffer ,bsym ,@body))))

;;;; Mailbox buffer

(defvar minimail-mailbox-history nil
  "History variable for mailbox selection.")

(defun -mailbox-buffer-name (account mailbox)
  (format "%s:%s" account mailbox))

(defun -mailbox-annotate (cand)
  "Return an annotation for `devdocs--read-entry' candidate CAND."
  (let-alist (car (-get-data cand))
    (when .messages
      (if (cl-plusp .unseen)
          (format #("  %s messages, %s unseen" 1 2 (display (space :align-to 40)))
                  .messages .unseen)
        (format #("  %s messages" 1 2 (display (space :align-to 40)))
                  .messages)))))

(defun -read-mailbox (prompt &optional accounts)
  "Read the name of a mailbox from one of the ACCOUNTS using PROMPT.
If ACCOUNTS is nil, use all configured accounts.
Return a cons cell consisting of the account symbol and mailbox name."
  (let* (cands
         ov
         (accounts (or accounts
                       (mapcar #'car minimail-accounts)
                       (user-error "No accounts configured")))
         (metadata '(metadata
                     (category . minimail-mailbox)
                     (annotation-function . -mailbox-annotate)))
         (coll (lambda (string predicate action)
                 (if (eq action 'metadata)
                     metadata
                   (complete-with-action action cands string predicate)))))
    (minibuffer-with-setup-hook
        (lambda()
          (setq ov (make-overlay (- (minibuffer-prompt-end) 2)
                                 (- (minibuffer-prompt-end) 1)))
          (overlay-put ov 'display " (loading):")
          (dolist (acct accounts)
            (let* ((buffer (current-buffer))
                   (mkcand (pcase-lambda (`(,mbx . ,props))
                             (unless (memq '\\Noselect (alist-get 'attributes props))
                               (propertize (-mailbox-buffer-name acct mbx)
                                           '-data `(,props ,acct . ,mbx))))))
              (athunk-do
               (athunk-let*
                   ((mailboxes (athunk-condition-case err
                                   (aget-mailbox-listing acct)
                                 (t (overlay-put ov 'display " (error):")
                                    (message "Error loading mailboxes for account %s: %S"
                                             acct err)
                                    nil))))
                 (when ov ;non-nil means we're still reading from minibuffer
                   (setq cands (nconc (delq nil (mapcar mkcand mailboxes)) cands))
                   (with-current-buffer buffer
                     (run-hooks '-minibuffer-update-hook))
                   (cl-remf accounts acct)
                   (unless accounts (delete-overlay ov))))))))
      (let ((cand (unwind-protect
                      (completing-read prompt coll nil t nil 'minimail-mailbox-history)
                    (setq ov nil))))
        (cdr (-get-data (or (car (member cand cands))
                            (user-error "Not a mailbox!"))))))))

(defun -read-mailbox-maybe (prompt)
  "Read a mailbox using PROMPT, unless current buffer is related to a mailbox."
  (let ((acct (alist-get 'account -local-state))
        (mbx (alist-get 'mailbox -local-state)))
    (if mbx (cons acct mbx) (-read-mailbox prompt (ensure-list acct)))))

;;;###autoload
(defun minimail-find-mailbox (account mailbox)
  "List messages in a mailbox."
  (interactive (let ((v (-read-mailbox "Find mailbox: ")))
                 `(,(car v) ,(cdr v))))
  (pop-to-buffer
   (let* ((name (-mailbox-buffer-name account mailbox))
          (buffer (get-buffer name)))
     (unless buffer
       (setq buffer (get-buffer-create name))
       (with-current-buffer buffer
         (minimail-mailbox-mode)
         (setq-local -local-state `((account . ,account)
                                    (mailbox . ,mailbox)))
         (-mailbox-refresh)))
     buffer)))

;;;###autoload
(defun minimail-search (account mailbox text)
  "Search for a message."
  (interactive (pcase-let*
                   ((`(,acct . ,mbx) (-read-mailbox-maybe "Search in mailbox: "))
                    (text (read-from-minibuffer "Search text: ")))
                 `(,acct ,mbx ,text)))
  (pop-to-buffer
   (let* ((name (format "*search in %s*"
                        (-mailbox-buffer-name account mailbox)))
          (buffer (get-buffer-create name)))
     (with-current-buffer buffer
       (minimail-mailbox-mode)
       (setq-local -local-state `((account . ,account)
                                  (mailbox . ,mailbox)
                                  (search . ,text)))
       (-mailbox-refresh))
     buffer)))

(defvar-keymap minimail-mailbox-mode-map
  "RET" #'minimail-show-message
  "n" #'minimail-next-message
  "p" #'minimail-previous-message
  "r" #'minimail-reply
  "R" #'minimail-reply-all
  "f" #'minimail-forward
  "s" #'minimail-search
  "g" #'revert-buffer
  "q" #'minimail-quit-windows
  "SPC" #'minimail-message-scroll-up
  "S-SPC" #'minimail-message-scroll-down
  "DEL" #'minimail-message-scroll-down)

(define-derived-mode minimail-mailbox-mode special-mode
  '("Mailbox" -mode-line-suffix)
  "Major mode for mailbox listings."
  :interactive nil
  (setq-local
   revert-buffer-function #'-mailbox-refresh
   truncate-lines t))

(defun -format-names (names &rest _)
  (string-join
   (mapcar (lambda (data)
             (propertize
              (or (alist-get 'name data)
                  (when-let* ((host (alist-get 'host data)))
                    (concat (alist-get 'mailbox data "?") "@" host))
                  "?")
              'help-echo
              (lambda (&rest _)
                (let-alist data
                  (mail-header-make-address
                   .name (concat (or .mailbox "?") "@" (or .host "?")))))))
           names)
   ", "))

(defun -format-date (date &rest _)
  (setq date (-get-data date))
  (let* ((current-time-list nil)
         (timestamp (encode-time date))
         (today (let* ((v (decode-time)))
                  (setf (decoded-time-hour v) 0)
                  (setf (decoded-time-minute v) 0)
                  (setf (decoded-time-second v) 0)
                  v))
         ;; Message age in seconds since start of this day
         (age (- (encode-time today) timestamp))
         (fmt (cond
               ((<= age (- (* 24 60 60))) "%Y %b %d")
               ((<= age 0) "%R")
               ((<= age (* 6 24 60 60)) "%a %R")
               ((<= (encode-time `(0 0 0 1 1 ,(decoded-time-year today)))
                    timestamp)
                "%b %d")
               (t "%Y %b %d"))))
    (propertize
     (format-time-string fmt timestamp)
     'help-echo (lambda (&rest _)
                  (format-time-string "%a, %d %b %Y %T %z"
                                      timestamp
                                      (decoded-time-zone date))))))

(defvar minimail-flag-icons
  '(((\\Flagged  . "★")
     ($Important . #("★" 0 1 (face shadow))))
    ((\\Answered . "↩")
     ($Forwarded . "→")
     ($Junk      . #("⚠" 0 1 (face shadow)))
     ($Phishing  . #("⚠" 0 1 (face error))))))

(defvar minimail-mailbox-mode-columns
  `((id
     :name "#"
     :getter ,(lambda (v _) (alist-get 'id v)))
    (flags
     :name ""
     :getter ,(lambda (v _)
                (let ((flags (alist-get 'flags v)))
                  (propertize
                   (mapconcat
                    (lambda (icons)
                      (or (seq-some (pcase-lambda (`(,flag . ,icon))
                                      (when (memq flag flags) icon))
                                    icons)
                          " "))
                    minimail-flag-icons)
                   'help-echo
                   (lambda (&rest _)
                     (format "Message flags: %s" flags))))))
    (from
     :name "From"
     :max-width 30
     :getter ,(lambda (v _) (-get-in v 'envelope 'from))
     :formatter -format-names)
    (subject
     :name "Subject"
     :max-width 60
     :getter ,(lambda (v _)
                (replace-regexp-in-string ;TODO: sanitize here or while parsing?
                 (rx control) ""
                 (-get-in v 'envelope 'subject))))
    (date
     :name "Date"
     :width 12
     :getter ,(lambda (v _)
                (let ((current-time-list nil)
                      (date (-get-in v 'envelope 'date)))
                  (propertize (format "%09x" (encode-time date))
                              '-data date)))
     :formatter -format-date)))

(defun -mailbox-after-insert-line (_table line &rest _)
  (when (derived-mode-p 'minimail-mailbox-mode)
    (cond
     ((not (memq '\\Seen (alist-get 'flags (car line))))
      (add-face-text-property (pos-bol 0) (pos-eol 0) 'minimail-unread)))))

(advice-add #'vtable--insert-line :after #'-mailbox-after-insert-line)

(defun -mailbox-refresh (&rest _)
  (unless (derived-mode-p #'minimail-mailbox-mode)
    (user-error "This should be called only from a mailbox buffer."))
  (let ((buffer (current-buffer)))
    (setq -mode-line-suffix ":Loading")
    (athunk-do
     (let-alist -local-state
       (athunk-let*
           ((messages (athunk-condition-case err
                          (if .search
                              (-afetch-search .account .mailbox .search)
                            (-afetch-mailbox .account .mailbox 100))
                        (t (with-current-buffer buffer
                             (setq -mode-line-suffix ":Error"))
                           (signal (car err) (cdr err))))))
         (with-current-buffer buffer
           (setq -mode-line-suffix nil)
           (let ((inhibit-read-only t))
             (if-let* ((vtable (vtable-current-table)))
                 (progn
                   (setf (vtable-objects vtable) messages)
                   (vtable-revert-command))
               (erase-buffer)
               (make-vtable
                :keymap minimail-mailbox-mode-map
                :columns (mapcar (lambda (v) (alist-get v minimail-mailbox-mode-columns))
                                 '(flags date from subject))
                :sort-by '((1 . descend))
                :objects messages)))))))))

(defun minimail-show-message ()
  (interactive nil minimail-mailbox-mode)
  (let-alist -local-state
    (let ((message (vtable-current-object))
          (mbxbuf (current-buffer))
          (msgbuf (if (buffer-live-p .message-buffer)
                      .message-buffer
                    (setf (alist-get 'message-buffer -local-state)
                          (generate-new-buffer (-message-buffer-name .account .mailbox ""))))))
      (cl-pushnew '\\Seen (alist-get 'flags message))
      (vtable-update-object (vtable-current-table) message)
      (setq-local overlay-arrow-position (copy-marker (pos-bol)))
      (with-current-buffer msgbuf
        (-display-message .account .mailbox (alist-get 'uid message))
        (setf (alist-get 'mailbox-buffer -local-state) mbxbuf)))))

(defun minimail-next-message (count)
  (interactive "p" minimail-mailbox-mode minimail-message-mode)
  (-with-associated-buffer mailbox
    (if (not overlay-arrow-position)
        (goto-char (point-min))
      (goto-char overlay-arrow-position)
      (goto-char (pos-bol (1+ count))))
    (when-let* ((window (get-buffer-window)))
      (set-window-point window (point)))
    (minimail-show-message)))

(defun minimail-previous-message (count)
  (interactive "p" minimail-mailbox-mode minimail-message-mode)
  (minimail-next-message (- count)))

(defun minimail-quit-windows (&optional kill) ;FIXME: use quit-window-hook instead
  (interactive "P" minimail-mailbox-mode minimail-message-mode)
  (-with-associated-buffer mailbox
    (when-let* ((msgbuf (alist-get 'message-buffer -local-state))
                (window (get-buffer-window msgbuf)))
      (quit-restore-window window (if kill 'kill 'bury)))
    (when-let* ((window (get-buffer-window)))
      (quit-window kill window))))

;;;; Message buffer

(defvar-keymap minimail-message-mode-map
  :doc "Keymap for Help mode."
  :parent (make-composed-keymap button-buffer-map special-mode-map)
  "n" #'minimail-next-message
  "p" #'minimail-previous-message
  "r" #'minimail-reply
  "R" #'minimail-reply-all
  "f" #'minimail-forward
  "s" #'minimail-search
  "SPC" #'minimail-message-scroll-up
  "S-SPC" #'minimail-message-scroll-down
  "DEL" #'minimail-message-scroll-down)

(define-derived-mode minimail-message-mode special-mode
  '("Message" -mode-line-suffix)
  "Major mode for email messages."
  :interactive nil
  (setq buffer-undo-list t)
  (add-hook 'kill-buffer-hook #'-cleanup-mime-handles nil t))

(defun -message-buffer-name (account mailbox uid)
  (format "%s:%s[%s]" account mailbox uid))

(defun -render-message ()
  "Render message in current buffer using the Gnus machinery."
  ;; Based on mu4e-view.el
  (let* ((ct (mail-fetch-field "Content-Type"))
         (ct (and ct (mail-header-parse-content-type ct)))
         (charset (intern-soft (mail-content-type-get ct 'charset)))
         (charset (if (and charset (coding-system-p charset))
                      charset
                    (detect-coding-region (point-min) (point-max) t))))
    (setq-local
     nobreak-char-display nil
     gnus-newsgroup-charset charset
     gnus-blocked-images "."            ;FIXME: make customizable
     gnus-article-buffer (current-buffer)
     gnus-summary-buffer nil
     gnus-article-wash-types nil
     gnus-article-image-alist nil)
    ;; just continue if some of the decoding fails.
    (ignore-errors (run-hooks 'gnus-article-decode-hook))
    (setq gnus-article-decoded-p gnus-article-decode-hook)
    (save-restriction
      (message-narrow-to-headers-or-head)
      (setf (alist-get 'references -local-state)
            (message-fetch-field "references"))
      (setf (alist-get 'message-id -local-state)
            (message-fetch-field "message-id" t)))
    (gnus-display-mime)
    (when gnus-mime-display-attachment-buttons-in-header
      (gnus-mime-buttonize-attachments-in-header))
    (when-let* ((window (get-buffer-window gnus-article-buffer)))
      (set-window-point window (point-min)))
    (set-buffer-modified-p nil)))

(defun -message-window-adjust-height (window)
  "Try to resize a message WINDOW sensibly.
If the window above it is a mailbox window, make the message window
occupy 3/4 of the available height, but without making the mailbox
window shorter than 6 lines."
  (when-let* ((otherwin (window-in-direction 'above window))
              (otherbuf (window-buffer otherwin)))
    (when (with-current-buffer otherbuf
            (derived-mode-p #'minimail-mailbox-mode))
      (let* ((h1 (window-height window))
             (h2 (window-height otherwin))
             (h3 (max 6 (round (* 0.25 (+ h1 h2))))))
        (adjust-window-trailing-edge otherwin (- h3 h2))))))

(defvar -display-message-base-action
  `((display-buffer-reuse-window
     display-buffer-in-direction)
    (direction . below)
    (window-height . -message-window-adjust-height)))

(defun -cleanup-mime-handles ()
  (mm-destroy-parts gnus-article-mime-handles)
  (setq gnus-article-mime-handles nil)
  (setq gnus-article-mime-handle-alist nil))

(defun -erase-message-buffer ()
  (erase-buffer)
  (dolist (ov (overlays-in (point-min) (point-max)))
    (delete-overlay ov))
  (-cleanup-mime-handles))

(defun -message-mode-advice (newfn)
  (lambda (fn &rest args)
    (apply (if (derived-mode-p 'minimail-message-mode) newfn fn) args)))

(advice-add #'gnus-msg-mail :around
            (-message-mode-advice #'message-mail) ;FIXME: only works if message-mail-user-agent is set
            '((name . -gnus-msg-mail)))

(advice-add #'gnus-button-reply :around
            (-message-mode-advice #'message-reply) ;FIXME: same
            '((name . -gnus-button-reply)))

(defun -display-message (account mailbox uid)
  (let ((buffer (current-buffer)))
    (unless (derived-mode-p #'minimail-message-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (minimail-message-mode)))
    (setq -mode-line-suffix ":Loading")
    (setf (alist-get 'next-message -local-state)
          (list account mailbox uid))
    (athunk-do
     (athunk-let*
         ((msgbuf (athunk-condition-case err
                      (-afetch-message account mailbox uid)
                    (t (with-current-buffer buffer
                         (setq -mode-line-suffix ":Error"))
                       (signal (car err) (cdr err))))))
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (when (equal (alist-get 'next-message -local-state)
                        (list account mailbox uid))
             (let ((inhibit-read-only t))
               (setq -mode-line-suffix nil)
               (setf (alist-get 'account -local-state) account)
               (setf (alist-get 'mailbox -local-state) mailbox)
               (-erase-message-buffer)
               (rename-buffer (-message-buffer-name account mailbox uid) t)
               (insert-buffer-substring msgbuf)
               (-render-message)))))))
    (display-buffer buffer -display-message-base-action)))

(defun minimail-message-scroll-up (arg &optional reverse)
  (interactive "^P" minimail-message-mode minimail-mailbox-mode)
  (-with-associated-buffer message
    (condition-case nil
        (when-let* ((window (get-buffer-window)))
          (with-selected-window window
            (funcall (if reverse #'scroll-down-command #'scroll-up-command)
                     arg)))
      (t (-with-associated-buffer mailbox
           (minimail-next-message
            (funcall (if reverse '- '+)
                     (cl-signum (prefix-numeric-value arg)))))))))

(defun minimail-message-scroll-down (arg)
  (interactive "^P" minimail-message-mode)
  (minimail-message-scroll-up arg t))

(defun minimail-reply (cite &optional to-address wide)
  (interactive (list (xor current-prefix-arg minimail-reply-cite-original))
               minimail-message-mode
               minimail-mailbox-mode)
  (-with-associated-buffer message
    (when-let* ((window (get-buffer-window)))
      (select-window window))
    (let ((message-mail-user-agent 'minimail)
          (message-reply-buffer (current-buffer))
          (msgid (alist-get 'message-id -local-state))
          (refs (alist-get 'references -local-state)))
      (message-reply to-address wide)
      (when msgid
        (save-excursion
          (goto-char (point-min))
          (insert "In-Reply-To: " msgid ?\n)
          (insert "References: ")
          (when refs (insert refs ?\s))
          (insert msgid ?\n)
          (narrow-to-region (point) (point-max))))
      (when cite (message-yank-original)))))

(defun minimail-reply-all (cite &optional to-address)
  (interactive (list (xor current-prefix-arg minimail-reply-cite-original))
               minimail-message-mode
               minimail-mailbox-mode)
  (minimail-reply cite to-address t))

(defun minimail-forward ()
  (interactive nil minimail-message-mode minimail-mailbox-mode)
  (-with-associated-buffer message
    (when-let* ((window (get-buffer-window)))
      (select-window window))
    (let ((message-mail-user-agent 'minimail))
      (message-forward))))

;;; MUA definition

;;;###autoload
(define-mail-user-agent 'minimail
  #'minimail-message-mail
  #'message-send-and-exit
  #'message-kill-buffer
  'message-send-hook)

(defun -send-mail-via-smtpmail ()
  "Call `smtpmail-send-it' with parameters from the X-Minimail-Account header."
  (let ((account (save-restriction
                   (message-narrow-to-headers-or-head)
                   (mail-fetch-field "X-Minimail-Account"
                                     nil nil nil t))))
    (let* ((props (or (alist-get (intern-soft account) minimail-accounts)
                      (user-error "Invalid Minimail account: %s" account)))
           (url (url-generic-parse-url (plist-get props :outgoing-url)))
           (smtpmail-store-queue-variables t)
           (smtpmail-smtp-server (url-host url))
           (smtpmail-smtp-user (cond ((url-user url) (url-unhex-string (url-user url)))
                                     ((plist-get props :mail-address))))
           (smtpmail-stream-type (pcase (url-type url)
                                   ("smtps" 'tls)
                                   ("smtp" 'starttls)
                                   (other (user-error "\
In `minimail-accounts', outgoing-url must have smtps or smtp scheme, got %s" other))))
           (smtpmail-smtp-service (or (url-portspec url)
                                      (pcase smtpmail-stream-type
                                        ('tls 465)
                                        ('starttls 587)))))
      (smtpmail-send-it))))

;;;###autoload
(defun minimail-message-mail (&optional to subject &rest rest)
  (pcase-let*
      ((`(,account . ,props)
        (or (seq-some (lambda (it)
                        (when (plist-member (cdr it) :outgoing-url) it))
                      `(,(assq (alist-get 'account -local-state) minimail-accounts)
                        ,@minimail-accounts))
            (user-error "No mail account has been configured to send messages")))
       (setup (lambda ()
                (setq-local
                 user-full-name (or (plist-get props :full-name)
                                    user-full-name)
                 user-mail-address (or (plist-get props :mail-address)
                                       user-mail-address)
                 message-signature (or (plist-get props :signature)
                                       message-signature)
                 message-signature-file (or (plist-get props :signature-file)
                                            message-signature-file)))))
    (let ((message-mail-user-agent 'message-user-agent)
          (message-mode-hook (cons setup message-mode-hook)))
      (apply #'message-mail to subject rest))
    (setq-local message-send-mail-function #'-send-mail-via-smtpmail)
    (message-add-header (format "X-Minimail-Account: %s" account))
    (message-sort-headers)
    (cond
     ((not to) (message-goto-to))
     ((not subject) (message-goto-subject))
     (t (message-goto-body)))
    t))

;;; Completion framework integration

;;;; Vertico

(defvar vertico--input)

(defun -minibuffer-update-vertico ()
  (declare-function vertico--exhibit "ext:vertico")
  (when vertico--input
    (setq vertico--input t)
    (vertico--exhibit)))

(with-eval-after-load 'vertico
  (add-hook '-minibuffer-update-hook #'-minibuffer-update-vertico))

;;;; Mct

(with-eval-after-load 'mct
  (add-hook '-minibuffer-update-hook 'mct--live-completions-refresh))

;; Local Variables:
;; read-symbol-shorthands: (("-" . "minimail--") ("athunk-" . "minimail--athunk-"))
;; End:

(provide 'minimail)
;;; minimail.el ends here

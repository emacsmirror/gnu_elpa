;;; minimail.el --- Simple, non-blocking IMAP email client  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Free Software Foundation, Inc.

;; Author: Augusto Stoffel <arstoffel@gmail.com>
;; Keywords: mail
;; URL: https://github.com/astoff/minimail
;; Package-Requires: ((emacs "30.1"))
;; Version: 0.3

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
(require 'smtpmail)
(require 'tree-widget)
(require 'vtable)

(eval-when-compile
  (require 'let-alist)
  (require 'rx)
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
;; - https://nullprogram.com/blog/2019/03/10/
;; - https://lists.gnu.org/archive/html/emacs-devel/2023-03/msg00630.html

(defmacro athunk--λ (args &rest body)
  "Like `lambda', but sneak location of definition in the docstring."
  (declare (indent 1))
  `(lambda ,args
     ,(when-let* ((pos (byte-compile--warning-source-offset)))
        (save-excursion
          (goto-char pos)
          (format "%s:%s:%s"
                  (file-name-nondirectory byte-compile-current-file)
                  (1+ (count-lines (point-min) (pos-bol)))
                  (1+ (current-column)))))
     ,(macroexp-progn body)))

(defmacro athunk--let*-1 (cont bindings form)
  "Helper macro for `athunk-let*'."
  (declare (indent 1))
  (cl-flet ((protect (form)
              (let ((esym (gensym)))
                `(condition-case ,esym ,form
                   (t (funcall ,cont (car ,esym) (cdr ,esym)))))))
    (pcase-exhaustive bindings
      ('()
       `(funcall ,cont nil ,(protect form)))
      (`((,var ,exp) . ,rest)
       `(let ((,var ,(protect exp)))
          (athunk--let*-1 ,cont ,rest ,form)))
      (`((,var <- ,athunk) . ,rest)
       (let ((esym (gensym))                ;the error, possibly nil
             (vsym (gensym)))               ;the computed value
         `(funcall ,(protect athunk)
                   (athunk--λ (,esym ,vsym)
                     (if ,esym
                         (funcall ,cont ,esym ,vsym)
                       (let ((,var ,vsym))
                         (athunk--let*-1 ,cont ,rest ,form))))))))))

(defmacro athunk-let* (bindings &rest body)
  "Sequentially resolve athunks then evaluate BODY.
BINDINGS are elements of the form (SYMBOL FORM) or (SYMBOL <- FORM).
The former simply binds FORM's value to SYMBOL.  In the latter, FORM
should evaluate to an athunk, and SYMBOL is bound to it resolved value.

Return an athunk which resolves to the value of the last form in BODY."
  (declare (indent 1) (debug ((&rest (sexp . [&or ("<-" form) (form)])) body)))
  (let ((csym (gensym)))
    `(lambda (,csym)
       (athunk--let*-1 ,csym ,bindings ,(macroexp-progn body)))))

(defmacro athunk-wrap (&rest body)
  "Wrap BODY in an athunk for delayed execution."
  (declare (indent 0))
  `(athunk-let* nil ,@body))

(defun athunk-gather (athunks)
  "Resolve all ATHUNKS and return a vector of results."
  (let* ((n (length athunks))
         (result (make-vector n nil)))
    (lambda (cont)
      (dotimes (i n)
        (run-with-timer
         0 nil (pop athunks)
         (lambda (err val)
           (if err
               (funcall cont err val)
             (setf (aref result i) val)
             (when (zerop (cl-decf n))
               (run-with-timer 0 nil cont nil result)))))))))

(defmacro athunk-let (bindings &rest body)
  "Concurrently resolve athunks then evaluate BODY.
BINDINGS are elements of the form (SYMBOL FORM) or (SYMBOL <- FORM).
The former simply binds FORM's value to SYMBOL.  In the latter, FORM
should evaluate to an athunk, and SYMBOL is bound to it resolved value.

Return an athunk which resolves to the value of the last form in BODY."
  (declare (indent 1))
  (if (length< bindings 2)
      `(athunk-let* ,bindings ,@body)
    (let ((vec (gensym))
          (athunks (mapcar (lambda (binding)
                             (pcase-exhaustive binding
                               (`(,_ <- ,athunk) athunk)
                               (`(,_ ,val) `(athunk-wrap ,val))))
                           bindings))
          (vars (mapcar #'car bindings)))
      `(athunk-let* ((,vec <- (athunk-gather (list ,@athunks))))
         (let ,(seq-map-indexed (lambda (v i) `(,v (aref ,vec ,i))) vars)
           ,@body)))))

(defun athunk-run (athunk)
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

(defun athunk-sleep (secs &optional value)
  "Return an athunk that waits SECS seconds and then returns VALUE."
  (lambda (cont)
    (run-with-timer secs nil cont nil value)))

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
    `(lambda (,csym)
       (funcall ,form
                (lambda (,esym ,vsym)
                  (condition-case ,hsym
                      (condition-case ,var
                          (if ,esym (signal ,esym ,vsym) ,vsym)
                        ,@handlers)
                    (:success (funcall ,csym nil ,hsym))
                    (t (funcall ,csym (car ,hsym) (cdr ,hsym)))))))))

(defmacro athunk-ignore-errors (&rest body)
  "Like `ignore-errors', but for asynchronous code."
  (declare (indent 0))
  `(athunk-condition-case nil ,(macroexp-progn body) (error nil)))

(defun athunk--semaphore (state athunk priority)
  (lambda (cont)
    (let ((task (lambda ()
                  (funcall athunk
                           (lambda (err val)
                             (if-let* ((next (pop (cdr state))))
                                 (run-with-timer 0 nil next)
                               (cl-incf (car state)))
                             (funcall cont err val))))))
      (cond
       ((> (car state) 0) (cl-decf (car state)) (funcall task))
       (priority (push task (cdr state)))
       (t (nconc state (list task)))))))

(cl-defmacro athunk-with-semaphore (place athunk &key (concurrency 1) use-lifo-queue)
  "Return an athunk which acquires a semaphore then runs ATHUNK.
Note that the expression ATHUNK is evaluated before acquiring the
semaphore.

PLACE is a generalized variable pointing to the semaphore state.  It is
initialized automatically."
  (declare (indent 1))
  `(athunk--semaphore (with-memoization ,place (list ,concurrency))
                      ,athunk
                      ,use-lifo-queue))

(cl-defun athunk-run-polling (athunk &key (interval 1) (max-tries -1))
  "Run ATHUNK, polling every INTERVAL seconds and blocking until done.
Give up after MAX-TRIES, if that is non-negative."
  (let (done err val)
    (funcall athunk (lambda (e v) (setq done t err e val v)))
    (while (not (or done (zerop max-tries)))
      (cl-decf max-tries)
      (sleep-for interval))
    (when err (signal err val))
    (when (not done) (error "athunk timed out"))
    val))

;;; Customizable options

(defgroup minimail nil
  "Simple, non-blocking IMAP email client."
  :prefix "minimail-"
  :group 'mail)

(defcustom minimail-accounts
  '((yhetil :incoming-url "imaps://:@yhetil.org/yhetil.emacs"
            :thread-style hierarchical))
  "Account configuration for the Minimail client.
This is an alist where keys are names used to refer to each account and
values are a plist with the following information:

:mail-address
  The email address of this account, overrides the global value of
  `user-mail-address'.

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
  Message signature, as value accepted by `message-signature' or,
  alternatively, (file FILENAME).

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
                                              (cons :tag "Use signature file"
                                                    (const file) file)
                                              (string :tag "String to insert")
                                              (function :tag "Function to call")))))))

(defcustom minimail-reply-cite-original t
  "Whether to cite the original message when replying."
  :type 'boolean)

(defcustom minimail-connection-idle-timeout 60
  "Time in seconds a network connection can remain open without activity."
  :type 'boolean)

(defcustom minimail-message-cache-size 20
  "Maximum number of messages to keep cached in memory."
  :type 'natnum)

(defcustom minimail-mailbox-mode-columns
  '((\\Sent flag-flagged flag-answered date recipients subject)
    (t flag-flagged flag-answered date from subject))
  "Columns to display in `minimail-mailbox-mode' buffers.
This is an alist mapping mailbox selectors to lists of column names as
defined in `minimail-mailbox-mode-column-alist'."
  :type '(repeat alist))

(defcustom minimail-mailbox-mode-sort-by
  '((t (date . descend) (thread . ascend)))
  "Sorting criteria for `minimail-mailbox-mode' buffers.
This is an alist mapping mailbox selectors to lists of the form

  ((COLUMN . DIRECTION) ...)

COLUMN is a column name, as in `minimail-mailbox-mode-columns'.
DIRECTION is either `ascend' or `descend'.

As a special case, an entry of the form (thead . DIRECTION) enables
sorting by thread."
  :type '(repeat alist))

(defcustom minimail-fetch-limit 100
  "Maximum number of messages to fetch at a time when displaying a mailbox."
  :type 'natnum)

(defcustom minimail-thread-style 'shallow
  "How to display message threads."
  :type '(choice (const :tag "Shallow" shallow)
                 (const :tag "Hierarchical" hierarchical)
                 (const :tag "Don't compute threads" nil)))

(defcustom minimail-subject-faces '(((not \\Seen) . minimail-unseen)
                                    (t . vtable))
  "Face to apply to subject strings based on the message flags.
This is used in `minimail-mailbox-mode' buffers."
  :type '(repeat alist))

(defface minimail-unseen '((t :weight bold :inherit vtable))
  "Face to indicate unseen messages.")

(defface minimail-mode-line-loading '((t :inherit mode-line-emphasis))
  "Face to indicate a background operation in the mode line.")

(defface minimail-mode-line-error '((t :inherit error))
  "Face to indicate an error in the mode line.")

;;; Internal variables and helper functions

(defvar -account-state nil
  "Alist mapping accounts to assorted state information about them.")

(defvar-local -local-state nil
  "Place to store assorted buffer-local information.")

(defvar-local -current-account nil)
(defvar-local -current-mailbox nil)

(defvar -minibuffer-update-hook nil
  "Hook run when minibuffer completion candidates are updated.")

(defvar -message-rendering-function #'-gnus-render-message
  "Function used to render a message buffer for display.")

(define-error '-imap-error "error in IMAP response")

(defmacro -get-in (alist key &rest rest)
  (let ((v `(alist-get ,key ,alist nil nil #'equal)))
    (if rest `(-get-in ,v ,(car rest) ,@(cdr rest)) v)))

(defun -get-data (string)
  "Get data stored as a string property in STRING."
  (cl-assert (stringp string))
  (get-text-property 0 'minimail string))

(defvar -logging-options nil
  "Whether to log server responses and other debug information.")

(defun -log-message-1 (args)
  "Helper function for `minimail--log-message'.
ARGS is the entire argument list of `minimail--log-message'."
  (let* ((props (ensure-list -logging-options))
         (maxsize (plist-get props :max-size))
         (bufname (or (plist-get props :buffer-name) "*minimail-log*"))
         (buffer (or (get-buffer bufname)
                     (with-current-buffer (get-buffer-create bufname)
                       (view-mode)
                       (setq-local outline-regexp "")
                       (setq buffer-undo-list t)
                       (current-buffer)))))
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-max))
        (let ((inhibit-read-only t)
              (start (point)))
          (insert (format-time-string "[%T] "))
          (put-text-property start (1+ start) 'invisible t)
          (put-text-property start (point) 'face 'warning)
          (insert (apply #'format args))
          (unless (eq (char-before) ?\n) (insert ?\n))
          (goto-char start)
          (while (re-search-forward "{\\([0-9]+\\)}\r\n" nil t)
            (let* ((start (prog1 (point)
                            (forward-char (string-to-number (match-string 1)))))
                   (ov (make-overlay start (point))))
              (overlay-put ov 'display (buttonize "[...]" #'delete-overlay ov
                                                  "Show literal data"))))
          (when maxsize
            (goto-char (- (point-max) maxsize))
            (delete-region (point-min) (pos-bol))))))))

(defmacro -log-message (string &rest args)
  "Write a message to buffer pointed by `minimail--log-buffer', if non-nil.
The message is formed by calling `format' with STRING and ARGS."
  `(when -logging-options (-log-message-1 (list ,string ,@args))))

(defvar minimail-mailbox-history nil
  "History variable for mailbox selection.")

(defun -mailbox-display-name (account mailbox)
  (format "%s:%s" account mailbox))

(defun -key-match-p (condition key)
  "Check whether KEY satisfies CONDITION.
KEY is a string or list of strings."
  (pcase-exhaustive condition
    ('t t)
    ((or (pred symbolp) (pred stringp))
     (if (listp key) (assoc-string condition key) (string= condition key)))
    (`(regexp ,re)
     (if (listp key)
         (seq-some (lambda (s) (string-match-p re s)) key)
       (string-match-p re key)))
    (`(not ,cond . nil) (not (-key-match-p cond key)))
    (`(or . ,conds) (seq-some (lambda (c) (-key-match-p c key)) conds))
    (`(and . ,conds) (seq-every-p (lambda (c) (-key-match-p c key)) conds))))

(defun -assoc-query (key alist)
  "Look up KEY in ALIST, a list of condition-value pairs.
Return the first matching cons cell."
  (seq-some (lambda (it) (when (-key-match-p (car it) key) it)) alist))

(defun -alist-query (key alist &optional default)
  "Look up KEY in ALIST, a list of condition-value pairs.
Return the first matching value."
  (if-let* ((it (-assoc-query key alist))) (cdr it) default))

(defun -settings-scalar-get (keyword account &optional mailbox)
  "Retrieve the most specific configuration value for KEYWORD.

1. Start looking up `minimail-accounts' -> ACCOUNT -> KEYWORD.
   a. If the entry exists and is not an alist, return it.
   b. If it is an alist, look up MAILBOX in it and return the
      associated value.
2. If not found and KEYWORD has a fallback variable associated to it,
   return its value.
3. Else, return nil."
  (let* ((found (plist-member (alist-get account minimail-accounts) keyword))
         (val (cadr found)))
    (cond ((consp (car-safe val)) ;it's a query alist
           (-alist-query (when mailbox
                           (cons mailbox
                                 ;; Due to caching, will essentially never block.
                                 (athunk-run-polling
                                  (-aget-mailbox-flags account mailbox)
                                  :interval 0.1 :max-tries 10)))
                         val))
          (found val)
          (t (symbol-value
              (alist-get keyword
                         '((:fetch-limit . minimail-fetch-limit)
                           (:full-name . user-full-name)
                           (:mail-address . user-mail-address)
                           (:signature . message-signature)
                           (:thread-style . minimail-thread-style))))))))

(defun -settings-alist-get (keyword account mailbox)
  "Retrieve the most specific configuration value for KEYWORD.

Inspect `minimail-accounts' -> ACCOUNT -> KEYWORD, which should be an
alist; if it contains a key matching MAILBOX, return that value.

Otherwise, take the fallback value for KEYWORD, which should also be an
alist, and look up MAILBOX in it."
  (when (stringp mailbox)
    (setq mailbox
          (cons mailbox
                ;; Due to caching, will essentially never block.
                (athunk-run-polling
                 (-aget-mailbox-flags account mailbox)
                 :interval 0.1 :max-tries 10))))
  (if-let* ((alist (plist-get (alist-get account minimail-accounts) keyword))
            (val (-assoc-query mailbox alist)))
      (cdr val)
    (let* ((vars '((:mailbox-columns . minimail-mailbox-mode-columns)
                   (:mailbox-sort-by . minimail-mailbox-mode-sort-by)))
           (var (alist-get keyword vars)))
      (-alist-query mailbox (symbol-value var)))))

(defun -alist-merge (&rest alists)
  "Merge ALISTS keeping only the first occurrence of each key."
  (let (seen result)
    (dolist (alist alists)
      (dolist (it alist)
        (unless (memq (car it) seen)
          (push (car it) seen)
          (push it result))))
    (nreverse result)))

(defun -set-mode-line-suffix (state)
  (setq mode-line-process
        (pcase state
          ('loading
           `(":" (:propertize "Loading"
                              face minimail-mode-line-loading)))
          (`(,error . ,data)
           `(":" (:propertize "Error"
                              help-echo ,(format "%s: %s" error data)
                              face minimail-mode-line-error))))))
;;;; vtable hacks

(defun -ensure-vtable (&optional noerror)
  "Return table under point or signal an error.
But first move point inside table if near the end of buffer."
  (when (eobp) (goto-char (pos-bol 0)))
  (or (vtable-current-table)
      (progn (goto-char (pos-bol 0)) (vtable-current-table))
      (unless noerror
        (user-error "No table under point"))))

;;; Low-level IMAP communication

;; References:
;; - IMAP4rev1: https://datatracker.ietf.org/doc/html/rfc3501
;; - IMAP4rev2: https://datatracker.ietf.org/doc/html/rfc9051
;; - IMAP URL syntax: https://datatracker.ietf.org/doc/html/rfc5092

(defvar-local -imap-callbacks nil)
(defvar-local -imap-command-queue nil)
(defvar-local -imap-idle-timer nil)
(defvar-local -imap-last-tag nil)
(defvar-local -next-position nil) ;TODO: necessary? can't we just rely on point position?

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
      ;; TODO: use ;AUTH=... notation as in RFC 5092?
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
                                 ?T (group (+ digit))
                                 ?\s (group (+ alpha))
                                 ?\s (group (* (not control)))
                                 (? ?\r) ?\n)
                             nil t)
          (pcase-let* ((cont (match-end 0))
                       (tag (string-to-number (match-string 1)))
                       (status (intern (downcase (match-string 2))))
                       (message (match-string 3))
                       (`(,mailbox . ,callback) (alist-get tag -imap-callbacks)))
            (setf (alist-get tag -imap-callbacks nil t) nil)
            (-log-message "response: %s %s\n%s"
                          proc
                          (or -current-mailbox "(unselected)")
                          (buffer-string))
            (unwind-protect
                (if (and mailbox (not (equal mailbox -current-mailbox)))
                    (error "Wrong mailbox: %s expected, %s selected"
                           mailbox -current-mailbox)
                  (with-restriction (point-min) cont
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
          (equal mailbox -current-mailbox))
      (process-send-string proc (format "T%s %s\r\n" tag command))
    ;; Need to select a different mailbox
    (let ((newtag (cl-incf -imap-last-tag))
          (cont (lambda (status message)
                  (if (eq 'ok status)
                      (progn
                        (setq -current-mailbox mailbox)
                        ;; Trick: this will cause the process filter
                        ;; to call `-imap-send' with the original
                        ;; command next.
                        (push (list tag mailbox command) -imap-command-queue))
                    (let ((callback (alist-get tag -imap-callbacks)))
                      (setf (alist-get tag -imap-callbacks nil t) nil)
                      (funcall callback status message))))))
      (push `(,newtag nil . ,cont) -imap-callbacks)
      (process-send-string proc (format "T%s SELECT %s\r\n"
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

(defun -imap-quote (s)
  "Make a UTF-7 encoded quoted string as per IMAP spec."
  (when (string-match-p (rx (not ascii)) s)
    (setq s (encode-coding-string s 'utf-7-imap)))
  (setq s (replace-regexp-in-string (rx (group (or ?\\ ?\"))) "\\\\\\1" s))
  (concat "\"" s "\""))

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
  (dquote   ()  (char ?\"))
  (crlf      () "\r\n")
  (anil      () "NIL" `(-- nil))
  (tagged    () (bol) (char ?T) (+ [0-9]) sp) ;we always format our tags as T<number>
  (untagged  () (bol) "* ")
  (number    () (substring (+ [0-9])) `(s -- (string-to-number s)))
  (achar     () (and (not [cntrl "(){] %*\"\\"]) (any))) ;characters allowed in an atom
  (atom      () (substring (+ achar)))  ;non-quoted identifier a.k.a. atom
  (qchar     () (or (and (char ?\\) [?\" ?\\]) ;character of a quoted string
                    (and (not dquote) (any))))
  (qstring   () dquote (substring (* qchar)) dquote ;quoted string
                `(s -- (replace-regexp-in-string (rx ?\\ (group nonl)) "\\1" s)))
  (literal   ()
             (char ?{)
             (guard (re-search-forward (rx point (group (+ digit)) "}\r\n") nil t))
             (region ;little hack: assume match data didn't change between the guards
              (guard (progn (forward-char (string-to-number (match-string 1)))
                            t))))
  (lstring   () literal      ;a "safe" string extracted from a literal
                `(start end -- (replace-regexp-in-string
                                (rx control) ""
                                (buffer-substring-no-properties start end))))
  (string    () (or qstring lstring))
  (qpstring  ()                         ;quoted string, QP-encoded
             string
             `(s -- (mail-decode-encoded-word-string s)))
  (astring   () (or atom string))
  (astring7  ()
             astring
             `(s -- (if (string-search "&" s) (decode-coding-string s 'utf-7-imap) s)))
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
             (substring (opt "\\") (+ achar)))
  (to-eol    ()                         ;consume input until eol
             (* (and (not [cntrl]) (any))) crlf)
  (to-rparen ()                    ;consume input until closing parens
             (* (or (and "(" to-rparen)
                    (and dquote (* qchar) dquote)
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
             sp astring7
             `(f d n -- `(,n (delimiter . ,d) (flags . ,f))))
       (status untagged "STATUS "
               (list astring7 " ("
                     (* (opt sp)
                        (or (and "MESSAGES " number `(n -- `(messages . ,n)))
                            (and "RECENT " number `(n -- `(recent . ,n)))
                            (and "UIDNEXT " number `(n -- `(uid-next . ,n)))
                            (and "UIDVALIDITY " number `(n -- `(uid-validity . ,n)))
                            (and "UNSEEN " number `(n -- `(unseen . ,n)))))
                     ")"))
       (response (list (* (or item status) crlf))))
    (car (peg-run (peg response) #'-imap-parse-error))))

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
                      sp (and nstring `(_ --)) ;discard useless addr-adl field
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
       ;; (body "BODY " (or body-single body-multi) `(s -- `(body . ,s)))
       (body "BODY " balanced)
       (content "BODY[] " literal `(start end -- `(content ,start . ,end)))
       (flags "FLAGS (" (list (* (opt sp) flag)) ")"
              `(v -- `(flags . ,v)))
       (x-gm-labels "X-GM-LABELS (" (list (* (opt sp) astring7)) ")"
                    `(v -- `(x-gm-labels . ,v)))
       (thread-id (or (and "THREADID " (or anil (and "(" atom ")")))
                      (and "X-GM-THRID " number))
                  `(v -- `(thread-id . ,v)))
       (email-id (or (and "EMAILID (" atom ")") (and "X-GM-MSGID " number))
                 `(v -- `(email-id . ,v)))
       (internal-date "INTERNALDATE " imapdate
                     `(v -- `(internal-date . ,v)))
       (size "RFC822.SIZE " number `(n -- `(rfc822-size . ,n)))
       (uid "UID " number `(n -- `(uid . ,n)))
       (item untagged number `(n -- `(id . ,n))
             " FETCH ("
             (* (opt sp) (or uid flags size envelope content thread-id x-gm-labels))
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
             (run-with-timer 0 nil continue))))))))

(defun -aget-capability (account)
  (athunk-let*
      ((cached (-get-in -account-state account 'capability))
       (new <- (if cached ;race condition here, but it's innocuous :-)
                   (athunk-wrap nil)
                 (athunk-let*
                     ((buffer <- (-amake-request account nil "CAPABILITY")))
                   (with-current-buffer buffer
                     (-parse-capability))))))
    (or cached (setf (-get-in -account-state account 'capability) new))))

(defun -format-sequence-set (set)
  "Format a set of message IDs as a string.
SET can be a number, t to represent the highest ID, an element of the
form (range START END) or a list of the above.  Ranges, in IMAP fashion,
are 1-based and inclusive of the end."
  (pcase-exhaustive set
    ('t "*")
    ((pred numberp set) (number-to-string set))
    (`(range ,from ,to)
     (concat (-format-sequence-set from) ":" (-format-sequence-set to)))
    ((pred (not null))
     (mapconcat #'-format-sequence-set set ","))))

(defvar -aget-mailbox-listing nil
  "Synchronization data for the function of same name.")

(defun -aget-mailbox-listing (account &optional refresh)
  (athunk-with-semaphore (alist-get account -aget-mailbox-listing)
    (athunk-let*
        ((cached (unless refresh (-get-in -account-state account 'mailboxes)))
         (new <- (if cached
                     (athunk-wrap nil)
                   (athunk-let*
                       ((props (alist-get account minimail-accounts))
                        (url (url-generic-parse-url (plist-get props :incoming-url)))
                        (path (string-remove-prefix "/" (car (url-path-and-query url))))
                        (caps <- (-aget-capability account))
                        (return (delq nil (list (when (memq 'special-use caps)
                                                  "SPECIAL-USE")
                                                (when (memq 'list-status caps)
                                                  "STATUS (MESSAGES UIDNEXT UNSEEN)"))))
                        (cmd (format (if return "LIST %s * RETURN (%s)" "LIST %s *")
                                     (-imap-quote path)
                                     (string-join return " ")))
                        (buffer <- (-amake-request account nil cmd)))
                     (with-current-buffer buffer
                       (mapcar (pcase-lambda (`(,k . ,v)) `(,k . ,(mapcan #'cdr v)))
                               (seq-group-by #'car (-parse-list))))))))
      (or cached (setf (-get-in -account-state account 'mailboxes) new)))))

(defun -aget-mailbox-flags (account mailbox)
  "Return the list of flags of ACCOUNT's MAILBOX."
  (athunk-let* ((mailboxes <- (-aget-mailbox-listing account)))
    (-get-in mailboxes mailbox 'flags)))

(defun -aget-mailbox-status (account mailbox)
  (athunk-let*
      ((cmd (format "EXAMINE %s" (-imap-quote mailbox)))
       (buffer <- (-amake-request account nil cmd)))
    (with-current-buffer buffer
      (-parse-select))))

(defun -afetch-id (account mailbox uid)
  "Fetch the current ID of a message given its UID, MAILBOX and ACCOUNT."
  (athunk-let*
      ((buffer <- (-amake-request account mailbox
                                  (format "UID FETCH %s (UID)" uid))))
    ;; NOTE: The command "FETCH * (UID)" is supposed to retrieve the
    ;; highest id, but servers seem to implement some kind of caching
    ;; that makes it not work.
    (with-current-buffer buffer
      (alist-get 'id (car (-parse-fetch))))))

(defvar -message-cache nil)

(defvar -afetch-message-body nil
  "Synchronization data for the function of same name.")

(defun -afetch-message-body (account mailbox uid)
  "Fetch body of message with the given UID in ACCOUNT's MAILBOX.
Return the request response buffer narrowed to the message content."
  (athunk-with-semaphore (alist-get account -afetch-message-body)
    (athunk-let*
        ((key (list account mailbox uid))
         (cached (assoc key -message-cache))
         (buffer <- (if cached
                        (athunk-wrap nil)
                      (-amake-request account mailbox
                                      (format "UID FETCH %s (BODY[])" uid)))))
      (if cached
          (prog1 (cdr cached)
            (setq -message-cache (cons cached (delq cached -message-cache))))
        (with-current-buffer buffer
          (pcase-let* ((response (car (-parse-fetch)))
                       (`(,start . ,end) (alist-get 'content response))
                       (content (buffer-substring-no-properties start end)))
            (push (cons key content) -message-cache)
            (cl-callf2 ntake minimail-message-cache-size -message-cache)
            content))))
    :use-lifo-queue t))

(defun -afetch-message-info (account mailbox set &optional brief sequential)
  "Fetch metadata for the given message SET in ACCOUNT's MAILBOX.

If BRIEF is non-nil, fetch flags but not envelope information.

If SEQUENTIAL is non-nil, SEQ is regarded as a set of sequential IDs
rather than UIDs."
  (athunk-let*
      ((caps <- (-aget-capability account))
       (cmd (format "%sFETCH %s (UID FLAGS%s%s%s)"
                    (if sequential "" "UID ")
                    (-format-sequence-set set)
                    (if (memq 'x-gm-ext-1 caps) " X-GM-LABELS" "")
                    (cond ((memq 'objectid caps) " THREADID")
                          ((memq 'x-gm-ext-1 caps) " X-GM-THRID")
                          (t ""))
                    (if brief "" " RFC822.SIZE ENVELOPE")))
       (buffer <- (-amake-request account mailbox cmd)))
    (with-current-buffer buffer (-parse-fetch))))

(defun -afetch-old-messages (account mailbox limit &optional before)
  "Fetch a mailbox message listing with up to LIMIT elements.
If BEFORE is nil, retrieve the newest messages.  Otherwise, retrieve
messages with UID smaller than BEFORE."
  (athunk-let*
      ((end <- (if before
                   (-afetch-id account mailbox before)
                 (athunk-let ((status <- (-aget-mailbox-status account mailbox)))
                   (1+ (alist-get 'exists status)))))
       (start (max 1 (- end limit)))
       (messages <- (if (> end 1)
                        (-afetch-message-info account mailbox
                                              `(range ,start ,(1- end))
                                              nil t)
                      (athunk-wrap nil))))
    messages))

(defun -afetch-new-messages (account mailbox messages)
  "Return an updated list of MESSAGES in ACCOUNT's MAILBOX."
  (athunk-let*
      ((uidmin (seq-min (mapcar (lambda (v) (let-alist v .uid)) messages)))
       (newflags <- (-afetch-message-info account mailbox `(range ,uidmin t) t))
       ;; Forget about vanished messages and update flags of the rest
       (messages (mapcan (let ((hash (make-hash-table)))
                           (dolist (msg newflags)
                             (puthash (let-alist msg .uid) msg hash))
                           (lambda (msg)
                             (when-let ((newmsg (gethash (let-alist msg .uid) hash)))
                               (list (-alist-merge newmsg msg)))))
                         messages))
       ;; Fetch new messages
       (newuids (mapcan (let ((hash (make-hash-table)))
                          (dolist (msg messages)
                            (puthash (let-alist msg .uid) t hash))
                          (lambda (msg)
                            (let-alist msg
                              (unless (gethash .uid hash)
                                (list .uid)))))
                        newflags))
       (newmessages <- (if newuids
                           (-afetch-message-info account mailbox newuids)
                         (athunk-wrap nil))))
    (nconc newmessages messages)))

(defun -format-search-1 (item)
  (pcase-exhaustive item
    (`(or ,first . nil)
     (-format-search-1 first))
    (`(or ,first . ,rest)
     (concat "OR " (-format-search-1 first) " " (-format-search-1 `(or . ,rest))))
    (`(not . ,v)
     (concat "NOT " (-format-search-1 v)))
    ((or 'all 'answered 'deleted 'flagged 'seen 'draft)
     (upcase (symbol-name item)))
    (`(,(and k (or 'keyword 'larger 'smaller)) . ,v) ;atom or number argument
     (format "%s %s" (upcase (symbol-name k)) v))
    (`(,(and k (or 'bcc 'body 'cc 'from 'subject 'text 'to)) . ,v) ;string argument
     (format "%s %S" (upcase (symbol-name k)) v))
    (`(,(and k (or 'before 'on 'since 'sentbefore 'senton 'sentsince)) . ,v) ;date argument
     (pcase-let ((`(_ _ _ ,day ,month ,year) (parse-time-string v)))
       (format "%s %s-%s-%s"
               (upcase (symbol-name k))
               day (aref -imap-months (1- month)) year)))
    (`(header ,k . ,v)
     (format "HEADER %S %S" k v))
    ((pred proper-list-p)
     (concat "(" (-format-search item) ")"))))

(defun -format-search (query)
  (mapconcat #'-format-search-1 (or query '(all)) " "))

(defun -afetch-search (account mailbox query limit)
  ;; TODO: add after argument, add UID 1:<after> arg
  (athunk-let*
      ((buffer <- (-amake-request account mailbox
                                  (concat "UID SEARCH CHARSET UTF-8 "
                                          (-format-search query))))
       (set (with-current-buffer buffer (-parse-search)))
       (messages <- (-afetch-message-info account mailbox (last set limit))))
    messages))

(defun -amove-messages (account mailbox destination uids)
  (athunk-let*
      ((caps <- (-aget-capability account))
       (cmd (if (memq 'move caps)
                (format "UID MOVE %s %s"
                        (-format-sequence-set uids)
                        (-imap-quote destination))
              (error "Account %s doesn't support moving messages" account)))
       (_ <- (-amake-request account mailbox cmd)))
    t))

(defun -astore-message-flags (account mailbox uids flags &optional remove)
  (athunk-let*
      ((cmd (format "UID STORE %s %sFLAGS (%s)"
                    (-format-sequence-set uids)
                    (if remove "-" "+")
                    (mapconcat (lambda (v) (if (symbolp v) (symbol-name v) v))
                               (ensure-list flags)
                               " ")))
       (buffer <- (-amake-request account mailbox cmd))
       (result (with-current-buffer buffer (-parse-fetch))))
    ;; Possibly update displayed mailbox
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when-let* ((table (and (eq -current-account account)
                                (equal -current-mailbox mailbox)
                                (derived-mode-p 'minimail-mailbox-mode)
                                (vtable-current-table))))
          (dolist (msg (vtable-objects table))
            (when-let* ((uid (alist-get 'uid msg))
                        (new (seq-find (lambda (it) (eq (alist-get 'uid it) uid))
                                       result)))
              (setf (alist-get 'flags msg) (alist-get 'flags new))
              (vtable-update-object table msg))))))
    result))

;;; Commands

(defun -mailbox-buffer (&optional noerror)
  "Return the relevant mailbox buffer for the current context.
If not found and NOERROR is nil, signal an error."
  (let ((buffer (if (derived-mode-p 'minimail-mailbox-mode)
                    (current-buffer)
                  (alist-get 'mailbox-buffer -local-state))))
    (prog1 buffer
      (unless (or noerror (buffer-live-p buffer))
        (user-error "No mailbox buffer")))))

(defun -message-buffer (&optional noerror)
  "Return the relevant message buffer for the current context.
If not found and NOERROR is nil, signal an error."
  (let ((buffer (if (derived-mode-p 'minimail-message-mode)
                    (current-buffer)
                  (alist-get 'message-buffer -local-state))))
    (prog1 buffer
      (unless (or noerror (buffer-live-p buffer))
        (user-error "No message buffer")))))

(defun -mailbox-annotation (mailbox)
  "Return an annotation for MAILBOX.
MAILBOX can be an alist as returned by `minimail--aget-mailbox-listing'
or a string containing such an alist as a property."
  (let-alist (if (stringp mailbox) (car (-get-data mailbox)) mailbox)
    (when .messages
      (let ((suffix (if (eq 1 .messages) "" "s")))
        (if (cl-plusp .unseen)
            (format " %s message%s, %s unseen" .messages suffix .unseen)
          (format " %s message%s" .messages suffix))))))

(defun -read-mailbox (prompt &optional accounts)
  "Read the name of a mailbox from one of the ACCOUNTS using PROMPT.
If ACCOUNTS is nil, use all configured accounts.
Return a cons cell consisting of the account symbol and mailbox name."
  (let* (cands
         ov
         (accounts (or (ensure-list accounts)
                       (mapcar #'car minimail-accounts)
                       (user-error "No accounts configured")))
         (metadata '(metadata
                     (category . minimail-mailbox)
                     (annotation-function . -mailbox-annotation)))
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
            (athunk-run
             (athunk-let*
                 ((mkcand (pcase-lambda (`(,mbx . ,props))
                            (unless (-key-match-p '(or \\Noselect \\NonExistent)
                                                  (alist-get 'flags props))
                              (propertize (-mailbox-display-name acct mbx)
                                          'minimail `(,props ,acct . ,mbx)))))
                  (mailboxes <- (athunk-condition-case err
                                    (-aget-mailbox-listing acct)
                                  (t (overlay-put ov 'display " (error):")
                                     (message "Error loading mailboxes for account %s: %S"
                                              acct err)
                                     nil))))
               (when ov ;non-nil means we're still reading from minibuffer
                 (setq cands (nconc (delq nil (mapcar mkcand mailboxes)) cands))
                 (with-current-buffer (overlay-buffer ov)
                   (run-hooks '-minibuffer-update-hook))
                 (cl-remf accounts acct)
                 (unless accounts (delete-overlay ov)))))))
      (let ((cand (unwind-protect
                      (completing-read prompt coll nil t nil 'minimail-mailbox-history)
                    (setq ov nil))))
        (cdr (-get-data (or (car (member cand cands))
                            (user-error "Not a mailbox!"))))))))

(defun -read-mailbox-maybe (prompt)
  "Read a mailbox using PROMPT, unless current buffer is related to a mailbox."
  (if -current-mailbox
      (cons -current-account -current-mailbox)
    (-read-mailbox prompt -current-account)))

(defun -selected-messages ()
  (cond
   ((derived-mode-p 'minimail-message-mode)
    (list -current-account -current-mailbox (alist-get 'uid -local-state)))
   ((derived-mode-p 'minimail-mailbox-mode)
    (list -current-account
          -current-mailbox
          (alist-get 'uid (or (vtable-current-object)
                              (user-error "No selected message")))))))

;;;###autoload
(defun minimail-find-mailbox (account mailbox)
  "List messages in a mailbox."
  (interactive (let ((v (-read-mailbox "Find mailbox: ")))
                 `(,(car v) ,(cdr v))))
  (pop-to-buffer
   (let* ((name (-mailbox-display-name account mailbox))
          (buffer (get-buffer name)))
     (unless buffer
       (setq buffer (get-buffer-create name))
       (with-current-buffer buffer
         (minimail-mailbox-mode)
         (setq -current-account account)
         (setq -current-mailbox mailbox)
         (-mailbox-buffer-populate)))
     buffer)))

;;;###autoload
(defun minimail-search (account mailbox query)
  "Perform a search in ACCOUNT's MAILBOX."
  (interactive (pcase-let*
                   ((`(,acct . ,mbx) (-read-mailbox-maybe "Search in mailbox: "))
                    (text (read-from-minibuffer "Search text: ")))
                 `(,acct ,mbx ((text . ,text)))))
  (pop-to-buffer
   (let* ((name (format "*search in %s*"
                        (-mailbox-display-name account mailbox)))
          (buffer (get-buffer-create name)))
     (with-current-buffer buffer
       (minimail-mailbox-mode)
       (setq -current-account account)
       (setq -current-mailbox mailbox)
       (setq -local-state `((search . ,query)))
       (-mailbox-buffer-populate))
     buffer)))

(defun -amove-messages-and-redisplay (account mailbox destination uids)
  (athunk-let*
      ((prog (make-progress-reporter
              (format-message "Moving messages to `%s'..."
                              (-mailbox-display-name account destination))))
       (_ <- (-amove-messages account mailbox destination uids)))
    (progress-reporter-done prog)
    (when-let*
        ((mbxbuf (seq-some (lambda (buf)
                             (with-current-buffer buf
                               (and (derived-mode-p 'minimail-mailbox-mode)
                                    (eq account -current-account)
                                    (equal mailbox -current-mailbox)
                                    buf)))
                           (buffer-list))))
      (with-current-buffer mbxbuf
        (let* ((table (vtable-current-table))
               (current (when-let* ((msgbuf (alist-get 'message-buffer -local-state))
                                    (_ (buffer-live-p msgbuf)))
                          (with-current-buffer msgbuf
                            (alist-get 'uid -local-state))))
               (messages (vtable-objects table)))
          (dolist (msg messages)
            (when (memq (alist-get 'uid msg) uids)
              (vtable-remove-object table msg)))
          (when (memq current uids)     ;move to next message
            (minimail-show-message)))))))

(defun minimail-move-to-mailbox (&optional destination)
  (interactive nil minimail-mailbox-mode minimail-message-mode)
  (pcase-let* ((`(,acct ,mbx . ,uids) (-selected-messages))
               (prompt (if (length= uids 1)
                           "Move message to: "
                         (format "Move %s messages to: " (length uids))))
               (dest (or destination
                         (cdr (-read-mailbox prompt (list acct))))))
    (athunk-run (-amove-messages-and-redisplay acct mbx dest uids))))

(defun -find-mailbox-by-flag (flag mailboxes)
  "Return the first item of MAILBOXES which has the given FLAG.
FLAG can be a string or, more generally, a condition for
`minimail--key-match-p'."
  (seq-some (pcase-lambda (`(,mbx . ,items))
              (when (-key-match-p flag (alist-get 'flags items)) mbx))
            mailboxes))

(defun minimail-move-to-archive ()
  (interactive nil minimail-mailbox-mode minimail-message-mode)
  (pcase-let* ((`(,acct ,mbx . ,uids) (-selected-messages)))
    (athunk-run
     (athunk-let*
         ((mailboxes <- (-aget-mailbox-listing acct))
          (dest (or (plist-get (alist-get acct minimail-accounts)
                               :archive-mailbox)
                    (-find-mailbox-by-flag '\\Archive mailboxes)
                    (-find-mailbox-by-flag '\\All mailboxes)
                    (user-error "Archive mailbox not found")))
          (_ <- (-amove-messages-and-redisplay acct mbx dest uids)))))))

(defun minimail-move-to-trash ()
  (interactive nil minimail-mailbox-mode minimail-message-mode)
  (pcase-let* ((`(,acct ,mbx . ,uids) (-selected-messages)))
    (athunk-run
     (athunk-let*
         ((mailboxes <- (-aget-mailbox-listing acct))
          (dest (or (plist-get (alist-get acct minimail-accounts)
                               :trash-mailbox)
                    (-find-mailbox-by-flag '\\Trash mailboxes)
                    (user-error "Trash mailbox not found")))
          (_ <- (-amove-messages-and-redisplay acct mbx dest uids)))))))

(defun minimail-move-to-junk ()
  (interactive nil minimail-mailbox-mode minimail-message-mode)
  (pcase-let* ((`(,acct ,mbx . ,uids) (-selected-messages)))
    (athunk-run
     (athunk-let*
         ((mailboxes <- (-aget-mailbox-listing acct))
          (dest (or (plist-get (alist-get acct minimail-accounts)
                               :junk-mailbox)
                    (-find-mailbox-by-flag '\\Junk mailboxes)
                    (user-error "Junk mailbox not found")))
          (_ <- (-amove-messages-and-redisplay acct mbx dest uids)))))))

(defun minimail-execute-server-command (account mailbox command)
  "Execute an IMAP command for debugging purposes."
  (interactive (pcase-let ((`(,account . ,mailbox)
                            (-read-mailbox-maybe "IMAP command in: ")))
                 (list account mailbox
                       (read-from-minibuffer
                        (format-prompt "IMAP command in %s" nil
                                       (-mailbox-display-name account mailbox))))))
  (pcase-let ((`(,status ,message)
               (athunk-run-polling
                (athunk-condition-case v
                    (-amake-request account mailbox command)
                  (:success `(ok ,(with-current-buffer v (buffer-string))))
                  (-imap-error (cdr v)))
                :interval 0.1 :max-tries 100)))
    (prog1 status (princ message))))

;;; Mailbox buffer

(defvar-local -thread-tree nil
  "The thread tree for the current buffer, as in RFC 5256.")

(defvar-keymap minimail-base-keymap
  :doc "Common keymap for Minimail mailbox and message buffers."
  "n" #'minimail-next-message
  "p" #'minimail-previous-message
  "r" #'minimail-reply
  "R" #'minimail-reply-all
  "f" #'minimail-forward
  "s" #'minimail-search
  "A" #'minimail-move-to-archive
  "J" #'minimail-move-to-junk
  "D" #'minimail-move-to-trash
  "M" #'minimail-move-to-mailbox
  "SPC" #'minimail-message-scroll-up
  "S-SPC" #'minimail-message-scroll-down
  "DEL" #'minimail-message-scroll-down)

(defvar-keymap minimail-mailbox-mode-map
  :parent (make-composed-keymap (list minimail-base-keymap vtable-map)
                                special-mode-map)
  "RET" #'minimail-show-message
  "+" #'minimail-load-more-messages
  "T" #'minimail-toggle-sort-by-thread
  "g" #'revert-buffer)

(define-derived-mode minimail-mailbox-mode special-mode "Mailbox"
  "Major mode for mailbox listings."
  :interactive nil
  (add-hook 'quit-window-hook #'-quit-message-window nil t)
  (setq-local
   revert-buffer-function #'-mailbox-buffer-refresh
   truncate-lines t))

(defun -base-subject (string)
  "Simplify message subject STRING for sorting and threading purposes.
Cf. RFC 5256, §2.1."
  (replace-regexp-in-string message-subject-re-regexp "" (downcase string)))

(defun -format-names (addresses)
  (propertize
   (mapconcat
    (lambda (addr)
      (let-alist addr
        (or .name .mailbox "(unknown)")))
    addresses
    ", ")
   'help-echo
   (lambda (&rest _)
     (mapconcat
      (lambda (addr)
        (let-alist addr
          (mail-header-make-address .name (concat .mailbox "@" .host))))
      addresses
      "\n"))))

(defun -format-date (date)
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

(defun -message-timestamp (msg)
  "The message's envelope date as a Unix timestamp."
  (let-alist msg
    (let ((current-time-list nil))
      (encode-time (or .envelope.date
                       .internal-date
                       '(0 0 0 1 1 1970 nil nil 0))))))

(define-icon -message-unseen nil
  '((emoji "✉️") (symbol "●") (text "."))
  "Icon for unseen messages."
  :help-echo "Unseen"
  :version "0.3")

(define-icon -message-flagged nil
  '((emoji "⭐") (symbol "★") (text "!"))
  "Icon for flagged messages."
  :help-echo "Flagged"
  :version "0.3")

(define-icon -message-important nil
  '((emoji "🔸") (symbol "⬥") (text "i"))
  "Icon for important messages."
  :help-echo "Important"
  :version "0.3")

(define-icon -message-answered nil
  '((emoji "↩️") (symbol "↩") (text "A"))
  "Icon for answered messages."
  :help-echo "Answered"
  :version "0.3")

(define-icon -message-forwarded nil
  '((emoji "➡️") (symbol "→") (text "F"))
  "Icon for answered messages."
  :help-echo "Forwarded"
  :version "0.3")

(define-icon -message-junk nil
  '((emoji "♻️") (symbol "♻") (text "J"))
  "Icon for junk messages."
  :help-echo "Junk"
  :version "0.3")

(define-icon -message-phishing nil
  '((emoji "⚠️") (symbol "⚠") (text "J" :face warning))
  "Icon for phishing messages."
  :help-echo "Phishing"
  :version "0.3")

(defvar minimail-mailbox-mode-column-alist
  ;; NOTE: We must slightly abuse the vtable API in several of our
  ;; column definitions.  The :getter attribute returns a string used
  ;; as sort key while :formatter fetches from it the actual display
  ;; string, embedded as a string property.
  `((id
     :name "#"
     :getter ,(lambda (msg _) (alist-get 'id msg)))
    (flag-seen
     :name "Seen"
     :min-width 1
     :getter ,(lambda (msg _)
                (let ((icon (-alist-query (alist-get 'flags msg)
                                          '(((not \\Seen) . -message-unseen)))))
                  (if icon (icon-string icon) ""))))
    (flag-flagged
     :name "Flagged"
     :min-width 1
     :getter
     ,(lambda (msg _)
        (let ((icon (-alist-query (append (alist-get 'x-gm-labels msg)
                                          (alist-get 'flags msg))
                                  '((\\Flagged  . -message-flagged)
                                    ((or $Important \\Important) . -message-important)
                                    ($Phishing  . -message-phishing)
                                    ($Junk      . -message-junk)))))
          (if icon (icon-string icon) ""))))
    (flag-answered
     :name "Answered"
     :min-width 1
     :getter ,(lambda (msg _)
                (let ((icon (-alist-query (alist-get 'flags msg)
                                          '((\\Answered . -message-answered)
                                            ($Forwarded . -message-forwarded)))))
                  (if icon (icon-string icon) ""))))
    (from
     :name "From"
     :max-width 30
     :getter ,(lambda (msg _)
                (let-alist msg
                  (-format-names .envelope.from))))
    (to
     :name "To"
     :max-width 30
     :getter ,(lambda (msg _)
                (let-alist msg
                  (-format-names .envelope.to))))
    (recipients
     :name "Recipients"
     :max-width 30
     :getter ,(lambda (msg _)
                (let-alist msg
                  (-format-names (append .envelope.to
                                         .envelope.cc
                                         .envelope.bcc)))))
    (subject
     :name "Subject"
     :max-width 80
     :getter ,(lambda (msg _tbl)
                (let-alist msg
                  (propertize (let ((s (-base-subject (or .envelope.subject ""))))
                                (if (string-empty-p s) "\0" s))
                              'minimail msg)))
     :formatter ,(lambda (s)
                   (let-alist (-get-data s)
                     (concat
                      (when (alist-get 'sort-by-thread -local-state)
                        (-thread-subject-prefix .uid))
                      (propertize (or .envelope.subject "")
                                  'face (-alist-query .flags minimail-subject-faces))))))
    (date
     :name "Date"
     :width 12
     :getter ,(lambda (msg _)
                ;; The envelope date as Unix timestamp, formatted as a
                ;; hex string.  This ensures the correct sorting.
                (propertize (format "%09x" (-message-timestamp msg))
                            'minimail (let-alist msg .envelope.date)))
     :formatter ,(lambda (s)
                   (propertize (-format-date (-get-data s))
                               'face 'vtable)))
    (size
     :name "Size"
     :getter ,(lambda (msg _) (let-alist msg .rfc822-size))
     :formatter ,(lambda (v)
                   (propertize (file-size-human-readable-iec v)
                               'face 'vtable)))))

(defun -mailbox-buffer-update (messages)
  "Set vtable objects to MESSAGES and do all necessary adjustments."
  (setf (vtable-objects (-ensure-vtable)) messages)
  (let ((uid (alist-get 'uid (vtable-current-object))))
    (vtable-revert-command)
    (when-let* ((msg (seq-find (lambda (msg) (let-alist msg (eq .uid uid)))
                               messages)))
      (vtable-goto-object msg)))
  (setq -thread-tree (funcall (pcase-exhaustive
                                  (-settings-scalar-get :thread-style
                                                        -current-account
                                                        -current-mailbox)
                                ('shallow #'-thread-tree-shallow)
                                ('hierarchical #'-thread-tree-hierarchical)
                                ('nil #'ignore))
                              messages))
  (when-let* ((how (alist-get 'sort-by-thread -local-state)))
    (-sort-messages-by-thread (eq how 'descend)))
  (save-excursion
    (goto-char (point-max))
    (let ((inhibit-read-only t)
          (ids (mapcar (lambda (msg) (let-alist msg .id)) messages)))
      (when (get-text-property (1- (pos-eol 0)) 'button)
        (delete-region (pos-bol 0) (point)))
      (when (> (seq-min ids) 1)
        (insert (format "Showing %s of %s messages " (length messages) (seq-max ids))
                (buttonize "[load more]"
                           (lambda (_) (minimail-load-more-messages)))
                "\n")))))

(defun -mailbox-buffer-populate (&rest _)
  "Fetch messages for the first time and create a vtable in the current buffer."
  (let* ((buffer (current-buffer))
         (account -current-account)
         (mailbox -current-mailbox)
         (limit (-settings-scalar-get :fetch-limit account mailbox))
         (search (alist-get 'search -local-state)))
    (-set-mode-line-suffix 'loading)
    (athunk-run
     (athunk-let*
         ((messages <- (athunk-condition-case err
                           (if search
                               (-afetch-search account mailbox search limit)
                             (-afetch-old-messages account mailbox limit))
                         (t (with-current-buffer buffer
                              (-set-mode-line-suffix err))
                            (signal (car err) (cdr err))))))
       (with-current-buffer buffer
         (-set-mode-line-suffix nil)
         (let* ((inhibit-read-only t)
                (vtable-map (make-sparse-keymap)) ;only way to disable extra keymap
                (colnames (-settings-alist-get :mailbox-columns account mailbox))
                (sortnames (-settings-alist-get :mailbox-sort-by account mailbox)))
           (make-vtable
            :objects messages ;Ideally we would create the table empty
                              ;and populate later
            :columns (mapcar (lambda (v)
                               (alist-get v minimail-mailbox-mode-column-alist))
                             colnames)
            :sort-by (mapcan (pcase-lambda (`(,col . ,dir))
                               (when-let ((i (seq-position colnames col)))
                                 `((,i . ,dir))))
                             sortnames))
           (setf (alist-get 'sort-by-thread -local-state)
                 (alist-get 'thread sortnames)))
         (-mailbox-buffer-update messages))))))

(defun -mailbox-buffer-refresh (&rest _)
  (unless (derived-mode-p #'minimail-mailbox-mode)
    (user-error "This should be called only from a mailbox buffer."))
  (let* ((buffer (current-buffer))
         (account -current-account)
         (mailbox -current-mailbox)
         (messages (vtable-objects (-ensure-vtable)))
         (search (alist-get 'search -local-state)))
    (when search (error "Not implemented"))
    (-set-mode-line-suffix 'loading)
    (athunk-run
     (athunk-let*
         ((messages <- (athunk-condition-case err
                           (-afetch-new-messages account mailbox messages)
                         (t (with-current-buffer buffer
                              (-set-mode-line-suffix err))
                            (signal (car err) (cdr err))))))
       (with-current-buffer buffer
         (-set-mode-line-suffix nil)
         (-mailbox-buffer-update messages))))))

(defun minimail-load-more-messages (&optional count)
  (interactive (list (when current-prefix-arg
                       (prefix-numeric-value current-prefix-arg)))
               minimail-mailbox-mode)
  (let* ((buffer (current-buffer))
         (account -current-account)
         (mailbox -current-mailbox)
         (messages (vtable-objects (-ensure-vtable)))
         (search (alist-get 'search -local-state))
         (limit (or count (-settings-scalar-get :fetch-limit account mailbox)))
         (before (seq-min (mapcar (lambda (msg) (let-alist msg .uid)) messages))))
    (when search (error "Not implemented"))
    (-set-mode-line-suffix 'loading)
    (athunk-run
     (athunk-let*
         ((old <- (athunk-condition-case err
                      (-afetch-old-messages account mailbox limit before)
                    (t (with-current-buffer buffer
                         (-set-mode-line-suffix err))
                       (signal (car err) (cdr err))))))
       (with-current-buffer buffer
         (-set-mode-line-suffix nil)
         (unless old (user-error "No more old messages"))
         (-mailbox-buffer-update (nconc old messages)))))))

(defun minimail-show-message ()
  (interactive nil minimail-mailbox-mode)
  (let* ((account -current-account)
         (mailbox -current-mailbox)
         (message (vtable-current-object))
         (mbxbuf (current-buffer))
         (msgbuf (if-let* ((buffer (alist-get 'message-buffer -local-state))
                           (_ (buffer-live-p buffer)))
                     buffer
                   (setf (alist-get 'message-buffer -local-state)
                         (generate-new-buffer
                          (-message-buffer-name account mailbox ""))))))
    (let-alist message
      (unless (member "\\Seen" .flags)
        (push "\\Seen" (cdr (assq 'flags message)))
        (vtable-update-object (vtable-current-table) message))
      (setq-local overlay-arrow-position (copy-marker (pos-bol)))
      (with-current-buffer msgbuf
        (-display-message account mailbox .uid)
        (setf (alist-get 'mailbox-buffer -local-state) mbxbuf)))))

(defun minimail-show-message-raw ()
  (interactive nil minimail-mailbox-mode)
  (let ((-message-rendering-function #'ignore))
    (minimail-show-message)))

(defun minimail-next-message (count)
  (interactive "p" minimail-mailbox-mode minimail-message-mode)
  (with-current-buffer (-mailbox-buffer)
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

(defun -quit-message-window (&optional kill)
  "If there is a window showing a message from this mailbox, quit it.
If KILL is non-nil, kill the message buffer instead of burying it."
  (when-let* ((msgbuf (alist-get 'message-buffer -local-state))
              (window (get-buffer-window msgbuf)))
    (quit-restore-window window (if kill 'kill 'bury))))

;;;; Sorting by thread

(defun -thread-position (uid)
  "Position of UID in the thread tree when regarded as a flat list."
  (let ((i 0))
    (named-let recur ((tree -thread-tree))
      (pcase (car tree)
        ((pred null))
        ((pred (eq uid)) i)
        ((pred numberp) (cl-incf i) (recur (cdr tree)))
        (subtree (or (recur subtree) (recur (cdr tree))))))))

(defun -thread-root (uid)
  "The root of the thread to which the given UID belongs."
  (named-let recur ((root nil) (tree -thread-tree))
    (pcase (car tree)
      ((pred null))
      ((pred (eq uid)) (or root uid))
      ((and (pred numberp) n) (recur (or root n) (cdr tree)))
      (subtree (or (recur root subtree) (recur root (cdr tree)))))))

(defun -thread-level (uid)
  "The nesting level of UID in the thread tree."
  (named-let recur ((level 0) (tree -thread-tree))
    (pcase (car tree)
      ((pred null) nil)
      ((pred (eq uid)) level)
      ((pred numberp) (recur (1+ level) (cdr tree)))
      (subtree (or (recur level subtree) (recur level (cdr tree)))))))

(defun -thread-subject-prefix (uid)
  "A prefix added to message subjects when sorting by thread."
  (make-string (* 2 (or (-thread-level uid) 0)) ?\s))

(defun -thread-tree-shallow (messages)
  "Compute a shallow message thread tree from MESSAGES.
Use server-side thread identifiers if available; otherwise, infer the
thread structure from the message sujects, as in the ORDEREDSUBJECT
algorithm described in RFC 5256.  The return value is as described in
loc. cit. §4, with message UIDs as tree leaves."
  (let* ((hash (make-hash-table :test #'equal))
         (threads (progn
                    (dolist (msg messages)
                      (let-alist msg
                        (push msg (gethash (or .thread-id
                                               (-base-subject
                                                (or .envelope.subject "")))
                                           hash))))
                    (mapcar (lambda (thread)
                              (sort thread :key #'-message-timestamp :in-place t))
                            (hash-table-values hash)))))
    (mapcar (lambda (thread)
              (cons (let-alist (car thread) .uid)
                    (mapcar (lambda (v) (let-alist v (list .uid))) (cdr thread))))
            threads)))

(defun -thread-tree-hierarchical (messages)
  "Compute a hierarchical message thread tree from MESSAGES.
This relies solely on Message-ID and In-Reply-To headers from the IMAP
envelope and doesn't use server-side threading information.  The return
value is as described in RFC 5256, §4, with message UIDs as tree leaves."
  (let* ((msgid (make-hash-table :test #'equal)) ;map Message-ID -> UID
         (children (make-hash-table))            ;map UID -> list of children messages
         (roots nil))                            ;list of root messages
    (dolist (msg messages)
      (let-alist msg
        (when .envelope.message-id
          (puthash .envelope.message-id .uid msgid))))
    (dolist (msg messages)
      (if-let* ((inreply (let-alist msg
                           (and .envelope.in-reply-to
                                (string-match "<.*?>" .envelope.in-reply-to)
                                (match-string-no-properties 0 .envelope.in-reply-to))))
                (parent (gethash inreply msgid)))
          (push msg (gethash parent children))
        (push msg roots)))
    (cl-labels
        ((recur (msg)
           (let-alist msg
             (pcase (gethash .uid children)
               ('nil (list .uid))
               (`(,one) (cons .uid (recur one)))
               (many (cons .uid (mapcar #'recur (sort many :key #'-message-timestamp))))))))
      (mapcar #'recur roots))))

(defun -sort-messages-by-thread (&optional descend)
  "Sort messages with grouping by threads.

Within a thread, sort each message after its parents.  Across threads,
preserve the existing order, in the sense that thread A sorts before
thread B if some message from A comes before all messages of B.  This
makes sense when the current sort order is in the “most relevant at top”
style.  If DESCEND is non-nil, use the opposite convention."
  (let* ((table (-ensure-vtable))
         (mhash (make-hash-table)) ;maps message id -> root id and position within thread
         (rhash (make-hash-table)) ;maps root id -> position across threads
         (lessp (lambda (o1 o2)
                  (pcase-let ((`(,ri . ,pi) (gethash (let-alist o1 .uid) mhash))
                              (`(,rj . ,pj) (gethash (let-alist o2 .uid) mhash)))
                    (if (eq ri rj)
                        (< pi pj)
                      (< (gethash ri rhash)
                         (gethash rj rhash))))))
         objects)
    (save-excursion
      ;; Get objects in current sort order (unlike `vtable-objects').
      (goto-char (vtable-beginning-of-table))
      (while-let ((obj (vtable-current-object)))
        (push obj objects)
        (forward-line)))
    (cl-callf nreverse objects)
    (dolist (obj objects)
      (let* ((count (hash-table-count mhash))
             (msgid (let-alist obj .uid))
             (rootid (or (-thread-root msgid) -1))
             (pos (or (-thread-position msgid) -1)))
        (puthash msgid (cons rootid pos) mhash)
        (if descend
            (puthash rootid count rhash)
          (cl-callf (lambda (i) (or i count)) (gethash rootid rhash)))))
    (setf (vtable-objects table) (sort objects :lessp lessp :in-place t))
    ;; Little hack to force vtable to redisplay with our new sorting.
    (cl-letf (((vtable-sort-by table) nil))
      (vtable-revert-command))))

(defun minimail-toggle-sort-by-thread ()
  "Toggle sorting messages by thread."
  (interactive nil minimail-mailbox-mode)
  (let* ((old (alist-get 'sort-by-thread -local-state))
         (new (cadr (memq old '(nil ascend descend)))))
    (message "Sorting by thread: %s" (or new "disabled"))
    ;; First re-sort the table by the original criteria, either
    ;; because that's the final goal (new is nil) or in preparation
    ;; for the thread sorting step.
    (vtable-revert)
    (setf (alist-get 'sort-by-thread -local-state) new)
    (when new (-sort-messages-by-thread (eq new 'descend)))))

;; Ensure we preserve sorting by column in the following sequence of
;; step: sort by thread, then sort by column, then refresh buffer.
(advice-add #'vtable-sort-by-current-column :before
            (lambda (&rest _)
              (when (derived-mode-p 'minimail-mailbox-mode)
                (setf (alist-get 'sort-by-thread -local-state) nil)))
            '((name . minimail)))

;;; Message buffer

(defvar -message-erase-function #'erase-buffer
  "Function called to erase a message buffer.")

(defvar-keymap minimail-message-mode-map
  :parent (make-composed-keymap (list minimail-base-keymap button-buffer-map)
                                special-mode-map))

(define-derived-mode minimail-message-mode special-mode "Message"
  "Major mode for email messages."
  :interactive nil
  (setq buffer-undo-list t))

(defun -message-buffer-name (account mailbox uid)
  (format "%s:%s[%s]" account mailbox uid))

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

(defun -display-message (account mailbox uid)
  (let ((render -message-rendering-function)
        (buffer (current-buffer)))
    (unless (derived-mode-p #'minimail-message-mode)
      (minimail-message-mode))
    (-set-mode-line-suffix 'loading)
    (setf (alist-get 'next-message -local-state)
          (list account mailbox uid))
    (athunk-run
     (athunk-let*
         ((text <- (athunk-condition-case err
                       (-afetch-message-body account mailbox uid)
                     (t (with-current-buffer buffer
                          (-set-mode-line-suffix err))
                        (signal (car err) (cdr err))))))
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (when (equal (alist-get 'next-message -local-state)
                        (list account mailbox uid))
             (let ((inhibit-read-only t))
               (-set-mode-line-suffix nil)
               (funcall -message-erase-function)
               (setq -current-account account)
               (setq -current-mailbox mailbox)
               (setf (alist-get 'uid -local-state) uid)
               (rename-buffer (-message-buffer-name account mailbox uid) t)
               (decode-coding-string text 'raw-text-dos nil buffer)
               ;(setq last-coding-system-used nil)
               (save-restriction
                 (message-narrow-to-headers-or-head)
                 (setf (alist-get 'references -local-state)
                       (message-fetch-field "references"))
                 (setf (alist-get 'message-id -local-state)
                       (message-fetch-field "message-id" t)))
               (funcall render)
               (set-buffer-modified-p nil)
               (when-let* ((w (get-buffer-window (current-buffer))))
                 (set-window-point w (point-min)))))))))
    (display-buffer buffer -display-message-base-action)))

(defun minimail-message-scroll-up (arg &optional reverse)
  (interactive "^P" minimail-message-mode minimail-mailbox-mode)
  (with-current-buffer (-message-buffer)
    (condition-case nil
        (if-let* ((window (get-buffer-window)))
            (with-selected-window window
              (funcall (if reverse #'scroll-down-command #'scroll-up-command)
                       arg))
          (display-buffer (current-buffer) -display-message-base-action))
      (beginning-of-buffer (with-current-buffer (-mailbox-buffer)
                             (minimail-next-message -1)))
      (end-of-buffer (with-current-buffer (-mailbox-buffer)
                       (minimail-next-message 1))))))

(defun minimail-message-scroll-down (arg)
  (interactive "^P" minimail-message-mode minimail-mailbox-mode)
  (minimail-message-scroll-up arg t))

(defun minimail-reply (cite &optional to-address wide)
  (interactive (list (xor current-prefix-arg minimail-reply-cite-original))
               minimail-message-mode
               minimail-mailbox-mode)
  (with-current-buffer (-message-buffer) ;FIXME: in mailbox mode, should
                                         ;reply to message at point, not
                                         ;the currently displayed one
    (when-let* ((window (get-buffer-window)))
      (select-window window))
    (let ((message-mail-user-agent 'minimail)
          (message-reply-buffer (current-buffer))
          (account -current-account)
          (mailbox -current-mailbox)
          (uid (alist-get 'uid -local-state))
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
      (push (lambda ()
              (athunk-run
               (-astore-message-flags account mailbox uid '\\Answered)))
            ;;FIXME: Use this or rather `message-sent-hook'?
            message-send-actions)
      (when cite (message-yank-original)))))

(defun minimail-reply-all (cite &optional to-address)
  (interactive (list (xor current-prefix-arg minimail-reply-cite-original))
               minimail-message-mode
               minimail-mailbox-mode)
  (minimail-reply cite to-address t))

(defun minimail-forward ()
  (interactive nil minimail-message-mode minimail-mailbox-mode)
  (with-current-buffer (-message-buffer)
    (when-let* ((window (get-buffer-window)))
      (select-window window))
    (let ((message-mail-user-agent 'minimail)
          (account -current-account)
          (mailbox -current-mailbox)
          (uid (alist-get 'uid -local-state)))
      (message-forward)
      (push (lambda ()
              (athunk-run
               (-astore-message-flags account mailbox uid '$Forwarded)))
            message-send-actions))))

;;;; Gnus graft
;; Cf. `mu4e--view-render-buffer' from mu4e-view.el

(defun -gnus-render-message ()
  "Render message in the current buffer using the Gnus machinery."
  (add-hook 'kill-buffer-hook
            (lambda () (mm-destroy-parts gnus-article-mime-handles))
            nil t)
  (add-function :before (local '-message-erase-function)
                (lambda ()
                  (mm-destroy-parts gnus-article-mime-handles)
                  (dolist (ov (overlays-in (point-min) (point-max)))
                    (delete-overlay ov))))
  ;; Gnus has the curious habit of not declaring its buffer-local
  ;; variables as such, so we need to take care to include all
  ;; relevant variables here.
  (setq-local
   gnus-article-buffer (current-buffer)
   gnus-article-charset nil
   gnus-article-current nil
   gnus-article-current-summary nil
   gnus-article-decoded-p nil
   gnus-article-ignored-charsets nil
   gnus-article-image-alist nil
   gnus-article-mime-handle-alist nil
   gnus-article-mime-handles nil
   gnus-article-wash-types nil
   gnus-blocked-images ""               ;FIXME: make customizable
   gnus-newsgroup-charset nil
   gnus-newsgroup-name nil
   gnus-summary-buffer nil
   message-mail-user-agent t            ;for mouse buttons
   nobreak-char-display nil)
  (run-hook-wrapped 'gnus-article-decode-hook
                    (lambda (fun)
                      (condition-case err
                          (funcall fun)
                        (t (-log-message "gnus-article-decode-hook error: %s: %S"
                                         fun err)))))
  (setq gnus-article-decoded-p gnus-article-decode-hook)
  (gnus-display-mime)
  (when gnus-mime-display-attachment-buttons-in-header
    (gnus-mime-buttonize-attachments-in-header)))

(advice-add #'gnus-article-check-buffer :before-until
            (lambda () (derived-mode-p #'minimail-message-mode))
            '((name . minimail)))

;;; Overview buffer

(defvar-local -tree-widgets nil)

(defvar-keymap minimail-overview-mode-map
  :parent (make-composed-keymap widget-keymap special-mode-map))

(define-derived-mode minimail-overview-mode special-mode "Minimail"
  "Major mode for browsing a mailbox tree."
  :interactive nil
  (setq buffer-undo-list t)
  (add-hook 'tree-widget-before-create-icon-functions #'-overview-create-icon nil t)
  (setq-local revert-buffer-function (lambda (&rest _)
                                       (-overview-buffer-populate t))))

(define-icon -mailbox nil
  '((emoji "🗂️") (text ""))
  "Generic icon for mailboxes."
  :version "0.3")

(define-icon -mailbox-closed -mailbox
  '((emoji "📁") (symbol "⊞ ")(text "[+]"))
  "Icon for mailboxes with children, when closed."
  :version "0.3")

(define-icon -mailbox-open -mailbox
  '((emoji "📂") (symbol "⊟ ") (text "[-]"))
  "Icon for mailboxes with children, when open."
  :version "0.3")

(define-icon -mailbox-archive -mailbox
  '((emoji "🗃️"))
  "Icon for archive mailboxes."
  :version "0.3")

(define-icon -mailbox-drafts -mailbox
  '((emoji "📝"))
  "Icon for drafts mailboxes."
  :version "0.3")

(define-icon -mailbox-flagged -mailbox
  '((emoji "⭐"))
  "Icon for flagged mailboxes."
  :version "0.3")

(define-icon -mailbox-important -mailbox
  '((emoji "🔶"))
  "Icon for important mailboxes."
  :version "0.3")

(define-icon -mailbox-inbox -mailbox
  '((emoji "📥"))
  "Icon for inbox mailboxes."
  :version "0.3")

(define-icon -mailbox-junk -mailbox
  '((emoji "♻️"))
  "Icon for junk mailboxes."
  :version "0.3")

(define-icon -mailbox-sent -mailbox
  '((emoji "📤"))
  "Icon for sent mailboxes."
  :version "0.3")

(define-icon -mailbox-trash -mailbox
  '((emoji "🗑️"))
  "Icon for trash mailboxes."
  :version "0.3")

(defun -overview-create-icon (icon)
  (widget-put icon :glyph-name nil)
  (widget-put icon :tag
              (icon-string
               (pcase (widget-type icon)
                 ('tree-widget-leaf-icon
                  (widget-put icon :tab-order -1)
                  (widget-get (widget-get icon :node) :icon))
                 ('tree-widget-open-icon '-mailbox-open)
                 (_ '-mailbox-closed)))))

(defun -overview-tree-expand (widget)
  (let ((acct (widget-get widget :account))
        (path (widget-get widget :path)))
    (mapcan
     (pcase-lambda (`(,name . ,props))
       (let-alist props
         (when (equal path (cdr .path))
           (let ((node (if (-key-match-p '(or \\Noselect \\NonExistent) .flags)
                           `(item :tag ,(car .path))
                         `(link :tag ,(car .path)
                                :format "%[%t%]%d"
                                :button-prefix ""
                                :button-suffix ""
                                :doc ,(propertize (or (-mailbox-annotation props) "")
                                                  'face 'completions-annotations)
                                :icon ,(seq-some
                                        (pcase-lambda (`(,cond . ,icon))
                                          (when (-key-match-p cond .flags) icon))
                                        '(((or \\All \\Archive) . -mailbox-archive)
                                          (\\Drafts             . -mailbox-drafts)
                                          (\\Flagged            . -mailbox-flagged)
                                          (\\Important          . -mailbox-important)
                                          (\\Junk               . -mailbox-junk)
                                          (\\Sent               . -mailbox-sent)
                                          (\\Trash              . -mailbox-trash)
                                          (t                    . -mailbox)))
                                :action ,(lambda (&rest _)
                                           (minimail-find-mailbox acct name))))))
             (if (-key-match-p '(or \\HasNoChildren \\Noinferiors) .flags)
                 `(,node)
               `((tree-widget
                  :node ,node
                  :account ,acct
                  :path ,.path
                  :expander -overview-tree-expand)))))))
     (widget-get (alist-get acct -tree-widgets) :mailboxes))))

(defun -overview-buffer-populate (&optional refresh)
  "Insert mailbox tree widgets to the current buffer.
Unless REFRESH is non-nil, use cached mailbox information."
  (let ((inhibit-read-only t)
        (buffer (current-buffer))
        (accounts (or (mapcar #'car minimail-accounts)
                      (user-error "No accounts configured"))))
    (-set-mode-line-suffix 'loading)
    (when (and (bolp) (eolp))
      (dolist (acct accounts)
        (setf (alist-get acct -tree-widgets)
              (widget-create 'tree-widget
                             :tag (symbol-name acct)
                             :account acct
                             :path nil
                             :open t
                             :expander-p #'always ;don't cache children
                             :expander #'-overview-tree-expand)))
      (goto-char (point-min)))
    (dolist (acct accounts)
      (athunk-run
       (athunk-let*
           ((mailboxes <- (athunk-condition-case err
                              (-aget-mailbox-listing acct refresh)
                            (t (-set-mode-line-suffix err)
                               (signal (car err) (cdr err)))))
            ;; Add path property to the mailbox items.
            (props (alist-get acct minimail-accounts))
            (url (url-generic-parse-url (plist-get props :incoming-url)))
            (basepath (string-remove-prefix "/" (car (url-path-and-query url))))
            (mailboxes (mapcar (pcase-lambda (`(,name . ,props))
                                 (let* ((delim (let-alist props .delimiter))
                                        (path (nreverse
                                               (append
                                                (unless (string-empty-p basepath)
                                                  (list basepath))
                                                (split-string
                                                 (string-remove-prefix basepath name)
                                                 (regexp-quote delim) t)))))
                                   `(,name (path . ,path) ,@props)))
                               mailboxes)))
         (with-current-buffer buffer
           (widget-put (alist-get acct -tree-widgets) :mailboxes mailboxes)
           (cl-remf accounts acct)
           (unless accounts (-set-mode-line-suffix nil))
           (let ((tree (alist-get acct -tree-widgets)))
             (when (widget-get tree :open)
               ;; Close and open to refresh children.
               ;; FIXME: Keep open/closed state of children as well, keep point.
               (widget-put tree :open nil)
               (widget-apply tree :action)))))))))

;;;###autoload
(defun minimail-show-mailboxes ()
  "Browse the mailbox tree of your email accounts."
  (interactive)
  (pop-to-buffer "*minimail-accounts*")
  (unless (derived-mode-p 'minimail-overview-mode)
    (minimail-overview-mode)
    (-overview-buffer-populate)))

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
      ((account ;some account set up for sending, prioritizing the current one
        (or (seq-some (pcase-lambda (`(,account . ,props))
                        (when (plist-member props :outgoing-url) account))
                      `(,(assq -current-account minimail-accounts)
                        ,@minimail-accounts))
            (user-error "No mail account has been configured to send messages")))
       (mailbox (and (eq -current-account account) -current-mailbox))
       (name (-settings-scalar-get :full-name account mailbox))
       (addr (-settings-scalar-get :mail-address account mailbox))
       (`(,sig . ,sigfile)
        (pcase (-settings-scalar-get :signature account mailbox)
          (`(file ,fname . nil) (cons t fname))
          (v (cons v message-signature-file))))
       (setup (lambda ()
                (setq-local -current-account account
                            -current-mailbox mailbox
                            user-full-name name
                            user-mail-address addr
                            message-signature sig
                            message-signature-file sigfile))))
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

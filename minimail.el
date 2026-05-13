;;; minimail.el --- Simple, non-blocking IMAP email client  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Free Software Foundation, Inc.

;; Author: Augusto Stoffel <arstoffel@gmail.com>
;; Keywords: mail
;; URL: https://codeberg.org/astoff/minimail
;; Package-Requires: ((emacs "30.1") (compat "31") (transient "0.12"))
;; Version: 0.5

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
(require 'transient)
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
                  (line-number-at-pos)
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
             (when (zerop (decf n))
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
Any uncaught errors are signaled, but notice this will happen at a
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
                               (incf (car state)))
                             (funcall cont err val))))))
      (cond
       ((> (car state) 0) (decf (car state)) (funcall task))
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

(cl-defun athunk-run-polling (athunk &key
                                     (message nil)
                                     (interval 1)
                                     (max-tries -1))
  "Run ATHUNK, polling every INTERVAL seconds and blocking until done.
Give up after MAX-TRIES, if that is non-negative.
If MESSAGE is non-nil, show a progress reporter, but only if the
athunk doesn't resolve immediately."
  (let (done err val reporter)
    (funcall athunk (lambda (e v) (setq done t err e val v)))
    (while (not (or done (zerop max-tries)))
      (cond
       (reporter (progress-reporter-update reporter))
       (message (setq reporter (make-progress-reporter message))))
      (decf max-tries)
      (sleep-for interval))
    (when err (signal err val))
    (unless done (error "athunk timed out"))
    (when reporter (progress-reporter-done reporter))
    val))

;;; Customizable options

(defgroup minimail '((auth-sources custom-variable)
                     (mail-user-agent custom-variable)
                     (message custom-group))
  "Simple, non-blocking IMAP email client."
  :prefix "minimail-"
  :group 'mail
  :link '(url-link :tag "website" "https://codeberg.org/astoff/minimail"))

(cl-defun -custom-type-query-alist (&key key-type value-type allow-single value)
  (let* ((kt (pcase-exhaustive key-type
               ('mailbox '(choice
                           (const :tag "Default" t)
                           (const :tag "\"All Mail\" mailbox" \\All)
                           (const :tag "\"Archive\" mailbox" \\Archive)
                           (const :tag "\"Drafts\" mailbox" \\Drafts)
                           (const :tag "\"Important\" mailbox" \\Flagged)
                           (const :tag "\"Junk\" mailbox" \\Junk)
                           (const :tag "\"Sent\" mailbox" \\Sent)
                           (const :tag "\"Trash\" mailbox" \\Trash)
                           (string :tag "Specific mailbox")
                           (list :tag "Mailboxes matching regexp"
                                 (const regexp) regexp)
                           (sexp :tag "Complex selector")))
               ('flag '(choice
                        (const :tag "Default" t)
                        (const :tag "Unseen" (not \\Seen))
                        (const :tag "Flagged (a.k.a. Starred)" \\Flagged)
                        (const :tag "Important" (or $Important \\Important))
                        (const :tag "Answered" \\Answered)
                        (const :tag "Forwarded" $Forwarded)
                        (const :tag "Phishing" $Phishing)
                        (const :tag "Junk" $Junk)
                        (sexp :tag "Complex selector")))))
         (alist `(alist :key-type ,kt :value-type ,value-type)))
    (if allow-single
        `(radio :tag "Scope"
                :value ,value
                ,value-type
                (alist :tag "Mailbox-specific values"
                       :match ,(lambda (_ v) (consp v))
                       ,@(cdr alist)))
      alist)))

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
  :type (-custom-type-query-alist
         :key-type 'mailbox
         :value-type '(repeat
                       (choice (const :tag "Message number" id)
                               (const :tag "Seen" flag-seen)
                               (const :tag "Flagged" flag-flagged)
                               (const :tag "Answered" flag-answered)
                               (const :tag "From" from)
                               (const :tag "To" to)
                               (const :tag "Recipients" recipients)
                               (const :tag "Subject" subject)
                               (const :tag "Date" date)
                               (const :tag "Size" size)))))

(defcustom minimail-mailbox-mode-sort-by
  '((t (date . descend) (thread . ascend)))
  "Sorting criteria for `minimail-mailbox-mode' buffers.
This is an alist mapping mailbox selectors to lists of the form

  ((COLUMN . DIRECTION) ...)

COLUMN is a column name, as in `minimail-mailbox-mode-columns'.
DIRECTION is either `ascend' or `descend'.

As a special case, an entry of the form (thread . DIRECTION) enables
sorting by thread."
  :type (-custom-type-query-alist
         :key-type 'mailbox
         :value-type
         `(repeat :tag "Sorting criteria"
                  (cons (choice :tag "Column"
                                (const :tag "Thread" thread)
                                ,@(let* ((v (get 'minimail-mailbox-mode-columns
                                                 'custom-type))
                                         (v (plist-get (cdr v) :value-type)))
                                    (cdadr v)))
                        (choice :tag "Direction"
                                (const ascend)
                                (const descend))))))

(defcustom minimail-fetch-limit 100
  "Maximum number of messages to fetch at a time when displaying a mailbox."
  :type 'natnum)

(defcustom minimail-thread-style nil
  "How to display message threads.

`hierarchical' shows a traditional thread tree.

`shallow' organizes threads in flat listings ordered by date.  It uses
server-side thread information when available, which may be more
accurate.

The value nil means to use `shallow' if server-side thread information
is available, `hierarchical' otherwise."
  :type '(choice (const :tag "Shallow" shallow)
                 (const :tag "Hierarchical" hierarchical)
                 (const :tag "Shallow if server-side threading available" nil)))

(defcustom minimail-subject-faces '(((not \\Seen) . minimail-unseen)
                                    (t . vtable))
  "Face to apply to subject strings based on the message flags.
This is used in `minimail-mailbox-mode' buffers."
  :type (-custom-type-query-alist :key-type 'flag :value-type 'face))

(defgroup minimail-accounts
  '((message-server-alist custom-variable))
  "Minimail account settings."
  :group 'minimail)

(defcustom minimail-accounts
  '((yhetil :incoming-url "imaps://;AUTH=ANONYMOUS@yhetil.org/yhetil.emacs"
            :thread-style hierarchical))
  "Account configuration for the Minimail client.
This is an alist where each key is a symbol used to refer to an account
and the corresponding value is a plist with the following information:

:incoming-url
  Information about the IMAP server as a URL. Normally, it suffices to
  enter \"imaps://<server-address>\".  More generally, it can take the
  form

    imaps://<username>:<password>@<server-address>:<port>

  If the username contains special characters, they must be percent
  encoded; in particular \"@\" must be entered as \"%40\".  If omitted,
  the username is taken from the :mail-address property below.

  If the password is omitted (which is highly recommended), use the
  auth-source mechanism.  See Info node `(auth) Help for users' for
  details.

  If your server requires STARTTLS connection instead of the default
  TLS, use \"imap\" as URL scheme.

:mail-address
  The address used to compose messages.  Can be either a plain email
  address or in the form \"Fulano de Tal <user@example.net>\".
  Overrides the global values of `user-mail-address' and, optionally,
  `user-full-name'.

:signature
  Message signature, as value accepted by `message-signature' or,
  alternatively, (file FILENAME).

:thread-style
  Account or mailbox-specific override for `minimail-thread-style'.

:fetch-limit
  Account or mailbox-specific override for `minimail-fetch-limit'.

:mailbox-columns
  Mailbox-specific override for `minimail-mailbox-mode-columns'.

:mailbox-sort-by
  Mailbox-specific override for `minimail-mailbox-mode-sort-by'.

All entries all optional, except for :incoming-url.

Account or mailbox-specific overrides may be a simple value, which
applies to all mailboxes of the account in question, or an alist
mapping mailbox selectors to a value.

A mailbox selector may be a symbol or string, which means to match
mailbox with that name or IMAP flag.  More complex selectors are
possible, see `minimail--key-match-p'."
  :type
  `(alist
    :key-type (symbol :tag "Account identifier")
    :value-type
    (plist
     :tag "Account properties"
     :options
     ((:incoming-url string)
      (:mail-address    ,(-custom-type-query-alist
                          :allow-single t :value ""
                          :key-type 'mailbox :value-type 'string))
      (:signature       ,(-custom-type-query-alist
                          :allow-single t
                          :key-type 'mailbox
                          :value-type '(choice
                                        (const :tag "Default" nil)
                                        (function-item :tag "No signature" ignore)
                                        (string :tag "String to insert")
                                        (function :tag "Function to call")
                                        (list :tag "Use signature file"
                                              (const :format "" file) file))))
      (:thread-style    ,(-custom-type-query-alist
                          :allow-single t
                          :key-type 'mailbox
                          :value-type (get 'minimail-thread-style 'custom-type)))
      (:fetch-limit     ,(-custom-type-query-alist
                          :allow-single t :value 100
                          :key-type 'mailbox :value-type 'natnum))
      (:mailbox-columns ,(-custom-type-query-alist
                          :allow-single t
                          :key-type 'mailbox
                          :value-type (plist-get
                                       (cdr (get 'minimail-mailbox-mode-columns
                                                 'custom-type))
                                       :value-type)))
      (:mailbox-sort-by ,(-custom-type-query-alist
                          :allow-single t
                          :key-type 'mailbox
                          :value-type (plist-get
                                       (cdr (get 'minimail-mailbox-mode-sort-by
                                                 'custom-type))
                                       :value-type)))))))

;;;; Faces

(defgroup minimail-faces nil
  "Faces used by Minimail."
  :group 'minimail
  :group 'faces)

(defface minimail-unseen '((t :weight bold :inherit vtable))
  "Face to indicate unseen messages.")

(defface minimail-mode-line-loading nil
  "Face to indicate a background operation in the mode line.")

(defface minimail-mode-line-error '((t :inherit error))
  "Face to indicate an error in the mode line.")

(defface minimail-line-drawing '((t :inherit default))
  "Face used for drawings such as thread trees.")

;;;; Icons

(defgroup minimail-icons nil
  "Icons used by Minimail."
  :group 'minimail)

(define-icon minimail-mailbox nil
  '((emoji "🗂️") (text ""))
  "Generic icon for mailboxes."
  :version "0.3")

(define-icon minimail-mailbox-closed minimail-mailbox
  '((emoji "📁") (symbol "⊞ ")(text "[+]"))
  "Icon for mailboxes with children, when closed."
  :version "0.3")

(define-icon minimail-mailbox-open minimail-mailbox
  '((emoji "📂") (symbol "⊟ ") (text "[-]"))
  "Icon for mailboxes with children, when open."
  :version "0.3")

(define-icon minimail-mailbox-archive minimail-mailbox
  '((emoji "🗃️"))
  "Icon for archive mailboxes."
  :version "0.3")

(define-icon minimail-mailbox-drafts minimail-mailbox
  '((emoji "📝"))
  "Icon for drafts mailboxes."
  :version "0.3")

(define-icon minimail-mailbox-flagged minimail-mailbox
  '((emoji "⭐"))
  "Icon for flagged mailboxes."
  :version "0.3")

(define-icon minimail-mailbox-important minimail-mailbox
  '((emoji "🔶"))
  "Icon for important mailboxes."
  :version "0.3")

(define-icon minimail-mailbox-inbox minimail-mailbox
  '((emoji "📥"))
  "Icon for inbox mailboxes."
  :version "0.3")

(define-icon minimail-mailbox-junk minimail-mailbox
  '((emoji "♻️"))
  "Icon for junk mailboxes."
  :version "0.3")

(define-icon minimail-mailbox-sent minimail-mailbox
  '((emoji "📤"))
  "Icon for sent mailboxes."
  :version "0.3")

(define-icon minimail-mailbox-trash minimail-mailbox
  '((emoji "🗑️"))
  "Icon for trash mailboxes."
  :version "0.3")

(define-icon minimail-message-unseen nil
  '((emoji "✉️") (symbol "●") (text "."))
  "Icon for unseen messages."
  :help-echo "Unseen"
  :version "0.3")

(define-icon minimail-message-flagged nil
  '((emoji "⭐") (symbol "★") (text "!"))
  "Icon for flagged messages."
  :help-echo "Flagged"
  :version "0.3")

(define-icon minimail-message-important nil
  '((emoji "🔸") (symbol "⬥") (text "i"))
  "Icon for important messages."
  :help-echo "Automatically flagged as important"
  :version "0.3")

(define-icon minimail-message-answered nil
  '((emoji "↩️") (symbol "↩") (text "A"))
  "Icon for answered messages."
  :help-echo "Answered"
  :version "0.3")

(define-icon minimail-message-forwarded nil
  '((emoji "➡️") (symbol "→") (text "F"))
  "Icon for answered messages."
  :help-echo "Forwarded"
  :version "0.3")

(define-icon minimail-message-junk nil
  '((emoji "♻️") (symbol "♻") (text "J"))
  "Icon for junk messages."
  :help-echo "Junk"
  :version "0.3")

(define-icon minimail-message-phishing nil
  '((emoji "⚠️") (symbol "⚠") (text "J" :face warning))
  "Icon for phishing messages."
  :help-echo "Phishing"
  :version "0.3")

(define-icon minimail-thread-leaf-terminal nil
  '((symbol "└─ " :face minimail-line-drawing)
    (text "`-- " :face minimail-line-drawing))
  "Tree-drawing symbol for a leaf which is last among its siblings."
  :version "0.4")

(define-icon minimail-thread-leaf-nonterminal nil
  '((symbol "├─ " :face minimail-line-drawing)
    (text "|-- " :face minimail-line-drawing))
  "Tree-drawing symbol for leaf which is not last among its siblings."
  :version "0.4")

(define-icon minimail-thread-parent-terminal nil
  '((symbol "   " :face minimail-line-drawing)
    (text "    " :face minimail-line-drawing))
  "Tree-drawing symbol for a parent which is last among its siblings."
  :version "0.4")

(define-icon minimail-thread-parent-nonterminal nil
  '((symbol "│  " :face minimail-line-drawing)
    (text "|   " :face minimail-line-drawing))
  "Tree-drawing symbol for a parent which is not last among its siblings."
  :version "0.4")

(define-icon minimail-thread-false-root nil
  '((text ""))
  "Tree-drawing symbol for a thread root that is a reply."
  :version "0.4")

;;; Internal variables and helper functions

(defvar -account-state nil
  "Alist mapping accounts to assorted state information about them.")

(defvar-local -local-state nil
  "Place to store assorted buffer-local information.")

(defvar-local -current-mailbox nil
  "The mailbox corresponding to the current buffer.
In mailbox and message buffers, this variable holds a cons cell
(ACCOUNT . MAILBOX-NAME) of a symbol and a string and is the data
expected as MAILBOX argument of many functions.

In IMAP process buffers, this variable holds the name of the selected
mailbox, or nil when the connection is in unselected state.")

(defvar-local -message-list nil)

(defvar -minibuffer-update-hook nil
  "Hook run when minibuffer completion candidates are updated.")

(defvar -message-rendering-function #'-gnus-render-message
  "Function used to render a message buffer for display.")

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

(defun -mailbox-display-name (mailbox)
  "String identifying MAILBOX, used e.g. as a buffer name."
  (format "%s:%s" (car mailbox) (cdr mailbox)))

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

(defun -mailbox-flags (mailbox)
  "Return the flags of MAILBOX, as a list of strings."
  ;; This could potentially block but shouldn't, since if we know the
  ;; mailbox we must already have fetched the mailbox listing.
  (alist-get 'flags (assoc (cdr mailbox)
                           (-get-mailbox-listing (car mailbox)))))

(defun -settings-scalar-get (keyword account-or-mailbox)
  "Retrieve the most specific configuration value for KEYWORD.

ACCOUNT-OR-MAILBOX may be a symbol identifying an account or a cons
cell (ACCOUNT . MAILBOX-NAME).  We then proceed as follows:

1. Start looking up `minimail-accounts' -> ACCOUNT -> KEYWORD.
   a. If the entry exists and is not an alist, return it.
   b. If it is an alist, look up MAILBOX-NAME in it and return the
      associated value.
2. If not found and KEYWORD has a fallback variable associated to it,
   return its value.
3. Else, return nil."
  (let* ((acct (or (car-safe account-or-mailbox) account-or-mailbox))
         (mbname (cdr-safe account-or-mailbox))
         (found (plist-member (alist-get acct minimail-accounts) keyword))
         (val (cadr found)))
    (when (consp (car-safe val)) ;it's a query alist
      (let ((key (when mbname
                   (cons mbname (-mailbox-flags (cons acct mbname))))))
        (setq found (-assoc-query key val))
        (setq val (cdr found))))
    (if found val
      (symbol-value
       (alist-get keyword
                  '((:fetch-limit . minimail-fetch-limit)
                    ;; Unlike other keywords, we care to distinguish
                    ;; when this one is explicitly set
                    ;; (:mail-address . user-mail-address)
                    (:signature . message-signature)
                    (:thread-style . minimail-thread-style)))))))

(defun -settings-alist-get (keyword mailbox)
  "Retrieve the most specific configuration value for KEYWORD.

MAILBOX should be a cons cell of the form (ACCOUNT . MAILBOX-NAME).
Inspect `minimail-accounts' -> ACCOUNT -> KEYWORD, which should be an
alist; if it contains a key matching MAILBOX-NAME, return that value.

Otherwise, if KEYWORD has an associated fallback variable, look up
MAILBOX-NAME in it."
  (let* ((acct (car mailbox))
         (flags (-mailbox-flags mailbox))
         (keys (cons (cdr mailbox) flags)))
    (if-let* ((alist (plist-get (alist-get acct minimail-accounts) keyword))
              (val (-assoc-query keys alist)))
        (cdr val)
      (let* ((vars '((:mailbox-columns . minimail-mailbox-mode-columns)
                     (:mailbox-sort-by . minimail-mailbox-mode-sort-by)))
             (var (alist-get keyword vars)))
        (-alist-query mailbox (symbol-value var))))))

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

(defun -split-mail-address (address)
  (if (string-match "\\(.*\\)<\\(.*\\)>" address)
      (let ((addr (match-string 2 address))
            (name (string-trim (match-string 1 address))))
        (cons addr (unless (string-empty-p name) name)))
    (list address)))

;;;; vtable tricks

(defun -vtable-ensure-table ()
  "Return table under point or signal an error.
But first move point inside table if near the end of buffer."
  (or (vtable-current-table)
      (and (text-property-search-backward 'vtable nil t)
           (forward-line -1)
           (vtable-current-table))
      (user-error "No table found")))

(defun -vtable-find-object (pred &optional direction)
  "Go to the first object satisfying PRED in the current table.
If found, move point and return the object; else, just return nil.

If DIRECTION is ±1, only search below/above starting from the current
position."
  (let ((start (point)) obj)
    (unless direction
      (vtable-beginning-of-table)
      (setq direction +1))
    (cl-assert (memq direction '(+1 -1)))
    (catch 'done
      (setq obj (vtable-current-object))
      (while obj
        (when (funcall pred obj) (throw 'done obj))
        (setq obj (when (zerop (forward-line direction))
                    (vtable-current-object))))
      (prog1 nil (goto-char start)))))

(defvar -vtable-before-sort-by-current-column-hook nil)
(advice-add #'vtable-sort-by-current-column :before
            (lambda (&rest _)
              (run-hooks '-vtable-before-sort-by-current-column-hook))
            '((name . -vtable-before-sort-by-current-column-hook)))

;;; IMAP

;; References:
;; - IMAP4rev1: https://datatracker.ietf.org/doc/html/rfc3501
;; - IMAP4rev2: https://datatracker.ietf.org/doc/html/rfc9051
;; - IMAP URL syntax: https://datatracker.ietf.org/doc/html/rfc5092

;;;; IMAP parsing

;; References:
;; - Formal syntax: https://datatracker.ietf.org/doc/html/rfc3501#section-9

(defun -imap-quote (s)
  "Make a UTF-7 encoded quoted string as per IMAP spec."
  (when (string-match-p (rx (any control nonascii)) s)
    (setq s (encode-coding-string s 'utf-7-imap)))
  (setq s (replace-regexp-in-string (rx (group (or ?\\ ?\"))) "\\\\\\1" s))
  (concat "\"" s "\""))

(defun -imap-encode-command (string)
  "Encode quoted strings as IMAP literals where needed.
STRING is assumed to be a reasonable IMAP command, except that it may
contain quoted strings with non-ASCII characters."
  (if (not (string-match-p (rx (any control nonascii)) string))
      string
    (with-temp-buffer
      (insert string)
      (goto-char (point-min))
      (while (search-forward "\"" nil t)
        (backward-char)
        (let ((p (point))
              (s (read (current-buffer))))
          (when (string-match-p (rx (any control nonascii)) s)
            (delete-region p (point))
            (insert (format "{%s+}\r\n" (string-bytes s)) s))))
      (buffer-string))))

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
  (dquote    ()  (char ?\"))
  (crlf      () "\r\n")
  (anil      () "NIL" `(-- nil))
  (tagged    () (bol) (+ [0-9]) sp) ;we always format our tags as a number
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
       (address "(" nqpstring sp nstring sp nstring sp nstring ")"
                `(name adl mailbox host -- (vector name adl mailbox host)))
       (addresses (or anil (and "(" (list (* address)) ")")))
       (envelope "ENVELOPE ("
                 nstring sp nqpstring sp addresses sp addresses sp addresses
                 sp addresses sp addresses sp addresses sp nstring sp nstring
                 ")"
                 `( date subject from sender reply-to
                    to cc bcc in-reply-to message-id
                    -- `(envelope . ,(vector date subject from sender reply-to
                                             to cc bcc in-reply-to message-id))))
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

;; Helpers to access message information from the FETCH command
(defsubst -address-name    (addr) (elt addr 0))
(defsubst -address-mailbox (addr) (elt addr 2))
(defsubst -address-host    (addr) (elt addr 3))

(defsubst -message-id          (msg) (let-alist msg .id))
(defsubst -message-uid         (msg) (let-alist msg .uid))
(defsubst -message-flags       (msg) (let-alist msg .flags))
(defsubst -message-date        (msg) (elt (let-alist msg .envelope) 0))
(defsubst -message-subject     (msg) (elt (let-alist msg .envelope) 1))
(defsubst -message-from        (msg) (elt (let-alist msg .envelope) 2))
(defsubst -message-reply-to    (msg) (elt (let-alist msg .envelope) 4))
(defsubst -message-to          (msg) (elt (let-alist msg .envelope) 5))
(defsubst -message-cc          (msg) (elt (let-alist msg .envelope) 6))
(defsubst -message-bcc         (msg) (elt (let-alist msg .envelope) 7))
(defsubst -message-in-reply-to (msg) (elt (let-alist msg .envelope) 8))
(defsubst -message-message-id  (msg) (elt (let-alist msg .envelope) 9))

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

;;;; IMAP network process

(define-error '-imap-error "IMAP error")
(define-error '-imap-response-no "IMAP NO response" '-imap-error)
(define-error '-imap-response-bad "IMAP BAD response" '-imap-error)

(defvar-local -imap-callbacks nil)
(defvar-local -imap-idle-timer nil)
(defvar-local -imap-last-tag nil)

(defun -imap-connect (name host port type callback)
  "Return a network stream connected to an IMAP server.
NAME, HOST, PORT and TYPE are as in `open-network-stream'.

CALLBACK is called with two arguments, nil and the network process, when
the connection is finally open.  Note that sending data to the process
before that, while possible, blocks Emacs."
  (let* ((buffer (generate-new-buffer (format " *%s*" name)))
         (proc (open-network-stream
                name buffer host port
                :type type
                :coding 'binary
                :nowait t)))
    (set-process-filter proc #'-imap-process-filter)
    (set-process-sentinel proc #'-imap-process-sentinel)
    (set-process-query-on-exit-flag proc nil)
    (with-current-buffer buffer
      (set-buffer-multibyte nil)
      (setq -imap-last-tag 0)
      (setq -imap-callbacks `((0 . ,callback)))
      (setq -imap-idle-timer (run-with-timer
                              minimail-connection-idle-timeout nil
                              #'delete-process proc)))
    proc))

(defun -imap-process-sentinel (proc message)
  "Sentinel for IMAP processes."
  (-log-message "sentinel: %s %s" proc (process-status proc))
  (pcase (process-status proc)
    ('open
     (with-current-buffer (process-buffer proc)
       (let ((callback (alist-get 0 -imap-callbacks)))
         (setf (alist-get 0 -imap-callbacks nil t) nil)
         (funcall callback nil proc))))
    ((or 'closed 'failed)
     (with-current-buffer (process-buffer proc)
       (pcase-dolist (`(_ . ,callback) -imap-callbacks)
         (funcall callback 'error message)))
     (kill-buffer (process-buffer proc)))))

(defun -imap-process-filter (proc string)
  "Filter function for IMAP processes."
  (with-current-buffer (process-buffer proc)
    (timer-set-time -imap-idle-timer
                    (time-add nil minimail-connection-idle-timeout))
    (save-excursion
      (goto-char (point-max))
      (insert string))
    (cond
     ;; Case 1: we are in the process of receiving an IMAP literal
     ;; string.  Keep point at the beginning of the literal string to
     ;; check again next time if it's complete.  If this test fails,
     ;; then as a side effect we skip over any complete literal
     ;; strings that may be present in the buffer.
     ((catch 'done
        (while (re-search-forward (rx "{" (group (+ digit)) "}\r\n") nil t)
          (let ((end (+ (point) (string-to-number (match-string 1)))))
            (if (<= end (point-max))
                (goto-char end)
              (goto-char (match-beginning 0))
              (throw 'done t))))))
     ;; Case 2: we find a tagged response so we call the respective
     ;; callback.
     ((re-search-forward (rx bol (group (+ digit))
                             ?\s (group (+ alpha))
                             ?\s (group (* (not control)))
                             (? ?\r) ?\n)
                         nil t)
      (let* ((tag (string-to-number (match-string 1)))
             (error (pcase (upcase (match-string 2))
                      ("NO" '-imap-response-no)
                      ("BAD" '-imap-response-bad)))
             (message (match-string 3))
             (callback (alist-get tag -imap-callbacks)))
        (-log-message "response: %s %s\n%s"
                      proc
                      (or -current-mailbox "(unselected)")
                      (buffer-string))
        (setf (alist-get tag -imap-callbacks nil t) nil)
        (with-restriction (point-min) (point)
          (unwind-protect
              (funcall callback error message)
            (delete-region (point-min) (point-max))))))
     ;; Case 3: no tagged response yet.  Look for one again when more
     ;; data arrives.
     (t (goto-char (point-max))
        (goto-char (pos-bol))))))

(defun -imap-send-command (proc command callback)
  "Send COMMAND to IMAP process PROC.
When a response arrives, CALLBACK is called, on a buffer containing the
server response, with two arguments: an error symbol and a message."
  (with-current-buffer (process-buffer proc)
    (let ((tag (incf -imap-last-tag)))
      (-log-message "request: %s %s\n%s"
                    proc
                    (or -current-mailbox "(unselected)")
                    command)
      (process-send-string proc (format "%s %s\r\n" tag command))
      (push (cons tag callback) -imap-callbacks))))

;;;; Async IMAP requests

(defun -asend-command (proc command)
  "Athunk to send COMMAND to IMAP process PROC.
For an OK response, this yields a temporary a buffer containing the
server response.  The buffer is automatically cleaned up after use.
For a NO or BAD response or a network error, signals an error."
  (lambda (cont)
    (-imap-send-command
     proc command
     (lambda (error message)
       (if error
           (funcall cont error message)
         (let ((buffer (current-buffer))
               (tmpbuf (generate-new-buffer " *minimail-temp*")))
           (with-current-buffer tmpbuf
             (set-buffer-multibyte nil)
             (insert-buffer-substring buffer)
             (goto-char (point-min)))
           (run-with-timer 0 nil (lambda ()
                                   (unwind-protect
                                       (funcall cont nil tmpbuf)
                                     (kill-buffer tmpbuf))))))))))

(defun -amake-process (account)
  "Athunk yielding a new, authenticated IMAP process for ACCOUNT."
  (athunk-let*
      ((props (or (alist-get account minimail-accounts)
                  (error "Invalid account: %s" account)))
       (url (url-generic-parse-url (plist-get props :incoming-url)))
       (type (pcase-exhaustive (url-type url)
               ("imaps" 'tls)
               ("imap" 'starttls)))
       ;; If URL has no username, guess it from the :mail-address
       ;; account property.
       (user (or (when (url-user url)
                   (url-unhex-string (url-user url)))
                 (when-let* ((addr (-settings-scalar-get :mail-address account)))
                   (car (-split-mail-address addr)))
                 (user-error "No username found for account %s" account)))
       ;; AUTH=<mechanism> notation as in RFC 5092.  If found, strip
       ;; it out of the username.  If nothing found here, we also look
       ;; at the :smtp-auth property of the auth-info entry (which
       ;; they should have called :sasl-mechanism, as it applies to
       ;; other protocols).
       (mech (and (url-user url)
                  (string-match (rx ";AUTH=" (group (+ alnum)) eos) user)
                  (prog1 (intern (downcase (match-string 1 user)))
                    (setq user (substring user 0 (match-beginning 0))))))
       (auth (cond
              ((eq mech 'anonymous) nil)
              ((url-password url)
               `(:secret ,(url-password url)))
              ((let ((enable-recursive-minibuffers t))
                 (car (auth-source-search
                       :user user
                       :host (url-host url)
                       :port (url-portspec url)
                       :max 1 :create t))))
              ((error "No password found for account %s" account))))
       (secret <- (if (plist-get auth :async)
                      (lambda (cont) (funcall (plist-get auth :secret) cont))
                    (athunk-wrap (auth-info-password auth))))
       (proc <- (lambda (cont)
                  (-imap-connect (format "minimail-%s" account)
                                 (url-host url)
                                 (or (url-portspec url)
                                     (pcase type
                                       ('tls 993)
                                       ('starttls 143)))
                                 type
                                 cont)))
       (cmd (seq-some
             (lambda (mech)
               (pcase mech
                 ('anonymous            ;RFC 2245
                  (format "AUTHENTICATE ANONYMOUS\r\n%s"
                          (base64-encode-string user t)))
                 ('oauthbearer          ;RFC 7628
                  (format "AUTHENTICATE OAUTHBEARER %s"
                          (base64-encode-string
                           (format "n,a=%s,\1auth= Bearer %s\1\1" user secret) t)))
                 ('xoauth2              ;de facto standard
                  (format "AUTHENTICATE XOAUTH2 %s"
                          (base64-encode-string
                           (format "user=%s\1auth= Bearer %s\1\1" user secret) t)))
                 ('plain
                  (format "AUTHENTICATE PLAIN %s"
                          (base64-encode-string
                           (format "\0%s\0%s" user secret) t)))))
             (ensure-list (or mech (plist-get auth :smtp-auth) 'plain))))
       (_ <- (athunk-condition-case err
                 (-asend-command proc cmd)
               (-imap-error
                (lwarn 'minimail :error "IMAP authentication error (%s):\n%S"
                       account (caddr err))
                (signal (car err) (cdr err))))))
    (when (plist-get auth :save-function)
      (let ((enable-recursive-minibuffers t))
        (funcall (plist-get auth :save-function))))
    proc))

(defvar -amake-request nil
  "Synchronization data for the function of same name.")

(defun -amake-request (account-or-mailbox command)
  "Issue COMMAND to the IMAP server.
ACCOUNT-OR-MAILBOX can be a cons cell (ACCOUNT . MAILBOX-NAME) or just
an account name as a symbol.

This yields the same result as `minimail--asend-command', but takes care
of picking an existing connection process or starting a new one, and
ensures that MAILBOX-NAME, if given, is selected."
  (let ((account (or (car-safe account-or-mailbox) account-or-mailbox))
        (mailbox (cdr-safe account-or-mailbox)))
    (athunk-with-semaphore (alist-get account -amake-request)
      (athunk-let*
          ((oldproc (alist-get 'process (alist-get account -account-state)))
           (proc <- (if (process-live-p oldproc)
                        (athunk-wrap oldproc)
                      (-amake-process account)))
           (_ (setf (alist-get 'process (alist-get account -account-state))
                    proc))
           (select <- (if (or (not mailbox)
                              (equal mailbox (buffer-local-value
                                              '-current-mailbox
                                              (process-buffer proc))))
                          (athunk-wrap nil)
                        (-asend-command proc (format "SELECT %s"
                                                     (-imap-quote mailbox)))))
           (_ (when select
                (setf (buffer-local-value '-current-mailbox
                                          (process-buffer proc))
                      mailbox)))
           (buffer <- (-asend-command proc command)))
        buffer))))

(defun -aget-capability (account)
  (athunk-let*
      ((cached (alist-get 'capability (alist-get account -account-state)))
       (new <- (if cached ;race condition here, but it's innocuous :-)
                   (athunk-wrap nil)
                 (athunk-let*
                     ((buffer <- (-amake-request account "CAPABILITY")))
                   (with-current-buffer buffer
                     (-parse-capability))))))
    (or cached
        (setf (alist-get 'capability (alist-get account -account-state)) new))))

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
        ((cached (unless refresh
                   (alist-get 'mailboxes (alist-get account -account-state))))
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
                        (buffer <- (-amake-request account cmd)))
                     (with-current-buffer buffer
                       (mapcar (pcase-lambda (`(,k . ,v)) `(,k . ,(mapcan #'cdr v)))
                               (seq-group-by #'car (-parse-list))))))))
      (or cached
          (setf (alist-get 'mailboxes (alist-get account -account-state)) new)))))

(defun -get-mailbox-listing (account)
  "Blocking version of `minimail--aget-mailbox-listing'.
To be used where we know the information is already cached."
  (athunk-run-polling
   (-aget-mailbox-listing account)
   :message "Fetching mailboxes"
   :interval 0.1 :max-tries 100))

(defun -aget-mailbox-status (mailbox)
  "Get the IMAP status of MAILBOX, as returned by the EXAMINE command."
  (athunk-let*
      ((cmd (format "EXAMINE %s" (-imap-quote (cdr mailbox))))
       (buffer <- (-amake-request (car mailbox) cmd)))
    (with-current-buffer buffer
      (-parse-select))))

(defun -afetch-id (mailbox uid)
  "Fetch the current ID of a message given its MAILBOX and UID."
  (athunk-let*
      ((buffer <- (-amake-request mailbox (format "UID FETCH %s (UID)" uid))))
    ;; NOTE: The command "FETCH * (UID)" is supposed to retrieve the
    ;; highest id, but servers seem to implement some kind of caching
    ;; that makes it not work.
    (with-current-buffer buffer
      (-message-id (car (-parse-fetch))))))

(defvar -message-cache nil)

(defvar -afetch-message-body nil
  "Synchronization data for the function of same name.")

(defun -afetch-message-body (mailbox uid)
  "Fetch body of message given its MAILBOX and UID, returning a string."
  (athunk-with-semaphore (alist-get (car mailbox) -afetch-message-body)
    (athunk-let*
        ((key (cons uid mailbox))
         (cached (assoc key -message-cache))
         (buffer <- (if cached
                        (athunk-wrap nil)
                      (-amake-request mailbox
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

(defun -afetch-message-info (mailbox set &optional brief sequential)
  "Fetch metadata for the given message SET in MAILBOX.

If BRIEF is non-nil, fetch flags but not envelope information.

If SEQUENTIAL is non-nil, SEQ is regarded as a set of sequential IDs
rather than UIDs."
  (athunk-let*
      ((caps <- (-aget-capability (car mailbox)))
       (cmd (format "%sFETCH %s (UID FLAGS%s%s%s)"
                    (if sequential "" "UID ")
                    (-format-sequence-set set)
                    (if (memq 'x-gm-ext-1 caps) " X-GM-LABELS" "")
                    (cond ((memq 'objectid caps) " THREADID")
                          ((memq 'x-gm-ext-1 caps) " X-GM-THRID")
                          (t ""))
                    (if brief "" " RFC822.SIZE ENVELOPE")))
       (buffer <- (-amake-request mailbox cmd)))
    (with-current-buffer buffer (-parse-fetch))))

(defun -afetch-old-messages (mailbox limit &optional before)
  "Fetch a message listing for MAILBOX with up to LIMIT elements.
If BEFORE is nil, retrieve the newest messages.  Otherwise, retrieve
messages with UID smaller than BEFORE."
  (athunk-let*
      ((end <- (if before
                   (-afetch-id mailbox before)
                 (athunk-let* ((status <- (-aget-mailbox-status mailbox)))
                   (1+ (alist-get 'exists status)))))
       (start (max 1 (- end limit)))
       (messages <- (if (> end 1)
                        (-afetch-message-info mailbox
                                              `(range ,start ,(1- end))
                                              nil t)
                      (athunk-wrap nil))))
    messages))

(defun -afetch-new-messages (mailbox messages)
  "Return an updated message listing for MAILBOX.
MESSAGES is a list of message metadata alists.  Return a similar list
where new messages are added, deleted messages are removed and old
messages have their flags updated."
  (athunk-let*
      ((uidmin (seq-min (mapcar #'-message-uid messages)))
       (newflags <- (-afetch-message-info mailbox `(range ,uidmin t) t))
       ;; Forget about vanished messages and update flags of the rest
       (messages (let ((hash (make-hash-table)))
                   (dolist (msg newflags)
                     (puthash (-message-uid msg) msg hash))
                   (mapcan (lambda (msg)
                             (when-let* ((newmsg (gethash (-message-uid msg) hash)))
                               (list (-alist-merge newmsg msg))))
                           messages)))
       ;; Fetch new messages
       (newuids (let ((hash (make-hash-table)))
                  (dolist (msg messages)
                    (puthash (-message-uid msg) t hash))
                  (mapcan (lambda (msg)
                            (let ((uid (-message-uid msg)))
                              (unless (gethash uid hash)
                                (list uid))))
                          newflags)))
       (newmessages <- (if newuids
                           (-afetch-message-info mailbox newuids)
                         (athunk-wrap nil))))
    (nconc newmessages messages)))

(defun -afetch-search (mailbox query limit)
  "Search MAILBOX using QUERY string.
Return a list of up to LIMIT message metadata alists, as in
`minimail--afetch-message-info'."
  ;; TODO: add after argument, add UID 1:<after> arg
  (athunk-let*
      ((caps <- (-aget-capability (car mailbox)))
       (encoded (-imap-encode-command query))
       (charset (unless (eq query encoded)
                  (unless (seq-intersection '(literal+ literal-) caps)
                    (user-error
                     "Cannot use non-ASCII characters for search on this account"))
                  "CHARSET UTF-8 "))
       (cmd (concat "UID SEARCH " charset encoded))
       (buffer <- (-amake-request mailbox cmd))
       (set (with-current-buffer buffer (-parse-search)))
       (messages <- (-afetch-message-info mailbox (last set limit))))
    messages))

(defun -astore-message-flags (mailbox set flags &optional remove)
  "Add FLAGS to each message in the MAILBOX message SET.
If REMOVE is non-nil, remove those flags instead.

If MAILBOX is displayed in some buffer, update it."
  (athunk-let*
      ((cmd (format "UID STORE %s %sFLAGS (%s)"
                    (-format-sequence-set set)
                    (if remove "-" "+")
                    (mapconcat (lambda (v) (if (symbolp v) (symbol-name v) v))
                               (ensure-list flags)
                               " ")))
       (buffer <- (-amake-request mailbox cmd))
       (result (with-current-buffer buffer (-parse-fetch))))
    ;; Possibly update displayed mailbox
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (and (equal -current-mailbox mailbox)
                   (derived-mode-p 'minimail-mailbox-mode))
          (save-window-excursion      ;because of vtable-update-object
            (dolist (msg -message-list)
              (when-let* ((uid (-message-uid msg))
                          (new (seq-find (lambda (it) (eq (-message-uid it) uid))
                                         result)))
                (setf (cdr (assq 'flags msg)) (-message-flags new))
                (ignore-errors
                  (vtable-update-object (-vtable-ensure-table) msg))))))))
    result))

(defun -acopy-messages (mailbox set destination)
  "Copy message SET from MAILBOX to DESTINATION."
  (-amake-request mailbox (format "UID COPY %s %s"
                                  (-format-sequence-set set)
                                  (-imap-quote destination))))

(defun -amove-messages-and-redisplay ( mailbox set destination
                                       &optional mailbox-buffer)
  "Move message SET from MAILBOX to DESTINATION.
If MAILBOX-BUFFER is non-nil, updating the UI when done."
  (athunk-let*
      ((caps <- (-aget-capability (car mailbox)))
       (cmd (if (memq 'move caps)
                (format "UID MOVE %s %s"
                        (-format-sequence-set set)
                        (-imap-quote destination))
              (error "Account %s doesn't support moving messages"
                     (car mailbox))))
       (_ <- (-amake-request mailbox cmd)))
    (when (buffer-live-p mailbox-buffer)
      (with-current-buffer mailbox-buffer
        (let ((messages (seq-remove (lambda (msg)
                                      (memq (-message-uid msg) set))
                                    -message-list))
              (showing (when-let* ((msgbuf (-find-buffer 'message t)))
                         (get-buffer-window msgbuf)))
              (next (-vtable-find-object
                     (lambda (m) (not (memq (-message-uid m) set)))
                     +1)))
          (-mailbox-buffer-update messages)
          (unless next
            ;; Above, we tried to move point to the next message not
            ;; to be removed.  If not found, then go to the last
            ;; remaining message.
            (vtable-end-of-table)
            (forward-line -1))
          (when (and showing (not overlay-arrow-position))
            ;; If we were displaying a message and now it's gone,
            ;; display message under point.
            (minimail-show-message)))))))

;;; Commands

(defun -find-buffer (type &optional noerror)
  "Return a TYPE buffer with same account and mailbox as the current ones.
TYPE can be `mailbox' or `message'.
If there is no such buffer and NOERROR is nil, signal an error."
  (let ((mailbox -current-mailbox)
        (mode (pcase-exhaustive type
                ('mailbox 'minimail-mailbox-mode)
                ('message 'minimail-message-mode))))
    (or (save-current-buffer
          (seq-find (lambda (buffer)
                      (set-buffer buffer)
                      (and (equal mailbox -current-mailbox)
                           (derived-mode-p mode)))
                    (buffer-list)))
        (unless noerror (user-error "No %s buffer" type)))))

(defun -mailbox-annotation (mbinfo)
  "Return an annotation for a mailbox given its metadata.
MBINFO can be an alist as returned by `minimail--aget-mailbox-listing'
or a string containing such an alist as a property."
  (let-alist (if (stringp mbinfo) (car (-get-data mbinfo)) mbinfo)
    (when .messages
      (let ((suffix (if (eq 1 .messages) "" "s")))
        (if (plusp .unseen)
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
                 ((mkcand (pcase-lambda (`(,mbname . ,props))
                            (unless (-key-match-p '(or \\Noselect \\NonExistent)
                                                  (alist-get 'flags props))
                              (propertize (-mailbox-display-name (cons acct mbname))
                                          'minimail `(,props ,acct . ,mbname)))))
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
  "Read a mailbox using PROMPT, unless a choice can be guessed from context."
  (or (get-text-property (point) '-current-mailbox)
      (when (cdr -current-mailbox) -current-mailbox)
      (-read-mailbox prompt (car -current-mailbox))))

(defun -current-message ()
  "Return the message object under point."
  (or (vtable-current-object) (user-error "No message under point")))

(defun -selected-messages ()
  "Return a list of message UIDs to act on.
Can be called in a mailbox or in a message buffer."
  (cond
   ((derived-mode-p 'minimail-message-mode)
    (list (alist-get 'uid (car -message-list))))
   ((derived-mode-p 'minimail-mailbox-mode)
    (if (use-region-p)
        ;; TODO: Ideally we would use a dired-style marking
        ;; mechanism, if vtable one day provides such a thing.
        (let (uids)
          (setq deactivate-mark t)
          (save-excursion
            (save-restriction
              (narrow-to-region (region-beginning) (region-end))
              (goto-char (point-min))
              (while (not (eobp))
                (push (-message-uid (vtable-current-object)) uids)
                (forward-line))))
          (or (nreverse (delq nil uids))
              (user-error "No message selected")))
      (list (-message-uid (-current-message)))))))

;;;###autoload
(defun minimail-find-mailbox (mailbox)
  "List messages in a mailbox.
MAILBOX is a cons cell of the account name as a symbol and mailbox name
as a string."
  (interactive (list (-read-mailbox "Find mailbox: ")))
  (pop-to-buffer
   (let* ((name (-mailbox-display-name mailbox))
          (buffer (get-buffer name)))
     (unless buffer
       (setq buffer (get-buffer-create name))
       (with-current-buffer buffer
         (minimail-mailbox-mode)
         (setq -current-mailbox mailbox)
         (-mailbox-buffer-populate)))
     buffer)))

;;;; Searching

(defclass -transient-search-option (transient-cons-option)
  ((format :initform " %k %d%v"))
  "Class for email search options.")

(cl-defmethod transient-format-value ((obj -transient-search-option))
  (if (not (oref obj value)) "" (concat ": " (cl-call-next-method))))

(defclass -transient-search-switch (-transient-search-option)
  ((choices :initform '(nil yes no)))
  "Class for email search switches.")

(cl-defmethod transient-infix-read ((obj -transient-search-switch))
  (cadr (member (oref obj value) (oref obj choices))))

(defun -format-search (items)
  (mapconcat
   (pcase-lambda (`(,key . ,value))
     (let ((tag (upcase (symbol-name key))))
       (pcase-exhaustive key
         ((or 'answered 'deleted 'flagged 'seen 'draft)
          (concat (pcase-exhaustive value ('yes nil) ('no "UN")) tag))
         ((or 'keyword 'larger 'smaller 'unkeyword) ;atom or number argument
          (format "%s %s" tag value))
         ((or 'bcc 'body 'cc 'from 'subject 'text 'to 'x-gm-raw) ;string argument
          (format "%s %S" tag value))
         ((or 'before 'on 'since 'sentbefore 'senton 'sentsince) ;date argument
          (pcase-let ((`(,day ,month ,year) (drop 3 (parse-time-string value))))
            (format "%s %s-%s-%s" tag day (aref -imap-months (1- month)) year))))))
   items " "))

(transient-define-prefix -search-transient ()
  "Transient menu for `minimail-search'."
  ["Email search"
   ("m" "Mailbox" :cons 'mailbox :class -transient-search-option
    :prompt "Search in mailbox: "
    :always-read t
    :reader
    (lambda (prompt _ _)
      (let ((mb (-read-mailbox prompt)))
        (propertize (-mailbox-display-name mb) 'minimail mb)))
    :init-value
    (lambda (obj)
      (let ((mb (-read-mailbox-maybe (transient-prompt obj))))
        (setf (oref obj value)
              (propertize (-mailbox-display-name mb) 'minimail mb)))))
   ("s" "Text (anywhere in message)" :cons 'text :class -transient-search-option
    :prompt "Messages containing: ")]
  [["Who"
    ("f" "From" :cons 'from :class -transient-search-option
     :prompt "Messages from: ")
    ("t" "To" :cons 'to :class -transient-search-option
     :prompt "Messages to: ")
    ("c" "CC"  :cons 'cc :class -transient-search-option
     :prompt "Messages with copy to: ")]
   ["When"
    ("d" "Date" :cons 'senton :class -transient-search-option
     :prompt "Messages dated exactly: "
     :reader transient-read-date)
    ("<" "Before" :cons 'sentbefore :class -transient-search-option
     :prompt "Messages dated before: "
     :reader transient-read-date)
    (">" "Since" :cons 'sentsince :class -transient-search-option
     :prompt "Messages dated after: "
     :reader transient-read-date)]
   ["What"
    ("u" "Subject" :cons 'subject :class -transient-search-option
     :prompt "Messages with subject containing: ")
    ("b" "Body" :cons 'body :class -transient-search-option
     :prompt "Messages with body containing: ")
    ("z" "Size" :cons nil :class -transient-search-option
     :prompt "\
Enter a number, optionally preceded by < or > and suffixed with a multiplier.
Messages with size (bytes): "
     :reader
     (lambda (prompt init hist)
       (let ((input (read-from-minibuffer prompt init nil nil hist)))
         (when (string-match (rx (group (? (any "<>"))) (? "=")
                                 (* space) (group (+ digit))
                                 (* space) (group (? (any "kKmM"))))
                             input)
           (let ((comp (pcase (match-string 1 input)
                         ("<" '("≤ " . smaller))
                         ((or ">" "") '("≥ " . larger))))
                 (n (* (string-to-number (match-string 2 input))
                       (pcase (downcase (match-string 3 input))
                         ("k" 1024) ("m" 1048576) (_ 1)))))
             (propertize (concat (car comp) (file-size-human-readable n))
                         'minimail (cons (cdr comp) n)))))))]
   ["Flags"
    ("." "Seen" :cons 'seen :class -transient-search-switch)
    ("!" "Flagged" :cons 'flagged :class -transient-search-switch)
    ("a" "Answered" :cons 'answered :class -transient-search-switch)]]
  [[("RET" "Search"
     (lambda (arg)
       (interactive "P")
       (let* ((items (transient-args transient-current-command))
              (mailbox (or (-get-data (alist-get 'mailbox items))
                           (error "No mailbox")))
              (query (-format-search
                      ;; Drop mailbox entry, replace (nil . STRING)
                      ;; entries by the (CLAUSE . ARGUMENT) sneaked
                      ;; into the string.
                      (mapcan (lambda (it)
                                (pcase (car it)
                                  ('mailbox nil)
                                  ('nil (list (-get-data (cdr it))))
                                  (_ (list it))))
                              items))))
         (when arg
           (setq query (read-from-minibuffer "IMAP search: " query)))
         (minimail-search mailbox query))))]])
(put '-search-transient 'completion-predicate 'ignore)

(defun minimail-search (mailbox query)
  "Perform a search in a mailbox.
Search MAILBOX for QUERY and pop to the result buffer.

Interactively, or if QUERY is t, show a transient menu.  Else, QUERY
should be a string as in RFC 3501, § 6.4.4, except that quoted terms in
it may contain non-ASCII characters."
  (interactive '(nil t))
  (if (eq query t)
      (let ((-current-mailbox (or mailbox -current-mailbox)))
        (call-interactively #'-search-transient))
    (when (or (string-match-p (rx cntrl) query)
              (string-blank-p query))
      (user-error "Invalid search query"))
    (let* ((name (format "*search in %s*" (-mailbox-display-name mailbox)))
           (buffer (generate-new-buffer name)))
      (pop-to-buffer buffer)
      (minimail-mailbox-mode)
      (setq -current-mailbox mailbox)
      (setq -local-state `((search . ,query)))
      (-mailbox-buffer-populate)
      buffer)))

;;;; Copying and moving

(defun minimail-copy-to-mailbox (&optional destination)
  "Copy messages to a given DESTINATION mailbox.
Interactively, or if DESTINATION is nil, ask for one.
In a mailbox buffer, act on the message under point or, if the region is
active, all messages in the region.  In a message buffer, act on the
current message."
  (interactive nil minimail-mailbox-mode minimail-message-mode)
  (let* ((set (-selected-messages)))
    (athunk-run
     (-acopy-messages -current-mailbox set
                      (or destination
                          (cdr (-read-mailbox
                                (if (length= set 1) "Copy message to: "
                                  (format "Copy %s messages to: "
                                          (length set)))
                                (car -current-mailbox))))))))

(defun minimail-move-to-mailbox (&optional destination)
  "Move messages to a given DESTINATION mailbox.
Interactively, or if DESTINATION is nil, ask for one.
In a mailbox buffer, act on the message under point or, if the region is
active, all messages in the region.  In a message buffer, act on the
current message."
  (interactive nil minimail-mailbox-mode minimail-message-mode)
  (let* ((set (-selected-messages)))
    (athunk-run
     (-amove-messages-and-redisplay
      -current-mailbox set
      (or destination
          (cdr (-read-mailbox
                (if (length= set 1) "Move message to: "
                  (format "Move %s messages to: "
                          (length set)))
                (car -current-mailbox))))
      (-find-buffer 'mailbox t)))))

(defun -find-mailbox-by-flag (account flag)
  "Return the first item of MAILBOXES which has the given FLAG.
FLAG can be a string or, more generally, a condition for
`minimail--key-match-p'."
  (seq-some (pcase-lambda (`(,mailbox . ,items))
              (when (-key-match-p flag (alist-get 'flags items))
                mailbox))
            (-get-mailbox-listing account)))

(defun minimail-move-to-archive ()
  "Move messages to the archive mailbox.
In a mailbox buffer, act on the message under point or, if the region is
active, all messages in the region.  In a message buffer, act on the
current message."
  (interactive nil minimail-mailbox-mode minimail-message-mode)
  (minimail-move-to-mailbox
   (or (-settings-scalar-get :archive-mailbox -current-mailbox)
       (-find-mailbox-by-flag (car -current-mailbox) '(or \\Archive \\All))
       (user-error "Archive mailbox not found"))))

(defun minimail-move-to-trash ()
  "Move messages to the trash mailbox.
In a mailbox buffer, act on the message under point or, if the region is
active, all messages in the region.  In a message buffer, act on the
current message."
  (interactive nil minimail-mailbox-mode minimail-message-mode)
  (minimail-move-to-mailbox
   (or (-settings-scalar-get :trash-mailbox -current-mailbox)
       (-find-mailbox-by-flag (car -current-mailbox) '\\Trash)
       (user-error "Trash mailbox not found"))))

(defun minimail-move-to-junk ()
  "Move messages to the junk mailbox.
In a mailbox buffer, act on the message under point or, if the region is
active, all messages in the region.  In a message buffer, act on the
current message."
  (interactive nil minimail-mailbox-mode minimail-message-mode)
  (minimail-move-to-mailbox
   (or (-settings-scalar-get :junk-mailbox -current-mailbox)
       (-find-mailbox-by-flag (car -current-mailbox) '\\Junk)
       (user-error "Junk mailbox not found"))))

(defun minimail-execute-server-command (mailbox command)
  "Execute an IMAP COMMAND in MAILBOX for debugging purposes."
  (interactive (let ((mailbox (-read-mailbox-maybe "IMAP command in: ")))
                 (list mailbox
                       (read-from-minibuffer
                        (format-prompt "IMAP command in %s" nil
                                       (-mailbox-display-name mailbox))))))
  (pcase-let ((`(,status ,message)
               (athunk-run-polling
                (athunk-condition-case v
                    (-amake-request mailbox command)
                  (:success `(ok ,(with-current-buffer v (buffer-string))))
                  (-imap-error (cdr v)))
                :message "Contacting IMAP server"
                :interval 0.1 :max-tries 100)))
    (prog1 status (princ message))))

;;; Mailbox buffer

(defvar-local -thread-data nil)
(defvar-local -sort-by-thread nil)

(defvar-keymap minimail-base-keymap
  :doc "Common keymap for Minimail mailbox and message buffers."
  "SPC" #'minimail-message-scroll-up
  "DEL" #'minimail-message-scroll-down
  "S-SPC" #'minimail-message-scroll-down
  "n" #'minimail-next-message
  "N" #'minimail-next-message-unseen
  "p" #'minimail-previous-message
  "P" #'minimail-previous-message-unseen
  "r" #'minimail-reply
  "R" #'minimail-reply-all
  "f" #'minimail-forward
  "s" #'minimail-search
  "C" #'minimail-copy-to-mailbox
  "A" #'minimail-move-to-archive
  "D" #'minimail-move-to-trash
  "J" #'minimail-move-to-junk
  "M" #'minimail-move-to-mailbox
  "t" #'minimail-toggle-message-flags
  "!" #'minimail-toggle-message-flagged
  "." #'minimail-toggle-message-seen
  "c" #'compose-mail)

(defvar -base-menu
  '(["Search Messages" minimail-search
     :help "Search for messages in this mailbox"]
    ["Compose New Message" compose-mail
     :help "Start writing a new mail message"]
    "---"
    ["Configure Accounts" (customize-group 'minimail-accounts)]
    ["Customize Minimail"  (customize-group 'minimail)]))

(defvar-keymap minimail-mailbox-mode-map
  :parent (make-composed-keymap (list minimail-base-keymap vtable-map)
                                special-mode-map)
  "RET" #'minimail-show-message
  "<double-mouse-1>" #'minimail-show-message
  "<double-down-mouse-1>" #'ignore
  "+" #'minimail-load-more-messages
  "=" #'minimail-quit-message-window
  "T" #'minimail-sort-by-thread
  "g" #'revert-buffer)

(easy-menu-define nil minimail-mailbox-mode-map
  "Menu for `minimail-mailbox-mode'."
  `("Mailbox"
    ["Fetch New Messages" revert-buffer
     :help "Check the server for new messages"]
    ["Fetch More Old Messages" minimail-load-more-messages
     :active (length< -message-list (-mailbox-total-messages))
     :help "Add older messages to the current mailbox buffer"]
    ("Sort by Thread"
     ["Off" (minimail-sort-by-thread nil)
      :style radio :selected (not -sort-by-thread)
      :help "Do not group message by thread"]
     ["Ascending" (minimail-sort-by-thread 'ascend)
      :style radio :selected (eq -sort-by-thread 'ascend)
      :help "Most relevant threads towards the top of the buffer"]
     ["Descending" (minimail-sort-by-thread 'descend)
      :style radio :selected (eq -sort-by-thread 'descend)
      :help "Most relevant threads towards the bottom of the buffer"])
    ,@-base-menu))

(defun -message-context-menu (menu &optional click)
  "Context menu for messages.
MENU and CLICK are as expected of a member of `context-menu-functions'."
  (when-let* ((set (save-excursion
                     (unless (use-region-p)
                       ;; Act either on the message at point or on all
                       ;; messages encompassed by region.
                       (mouse-set-point click))
                     (ignore-errors (-selected-messages)))))
    (let* ((mailbox -current-mailbox)
           (msg (seq-find (lambda (m) (eq (-message-uid m) (car set)))
                          -message-list))
           (flags (-message-flags msg))
           (flagitems (lambda (items)
                        (mapcar
                         (lambda (flag)
                           (let ((selected (assoc-string flag flags)))
                             `[,(-flag-readable-name flag)
                               (minimail-toggle-message-flags
                                ',set ',flag ,(if selected -1 +1))
                               :style toggle :selected ,selected]))
                         items)))
           (callback (lambda (action dest)
                       (athunk-run
                        (pcase action
                          ('copy (-acopy-messages mailbox set dest))
                          ('move (-amove-messages-and-redisplay
                                  mailbox set dest
                                  (-find-buffer 'mailbox t)))))))
           (mbitems (pcase-lambda (`(,action))
                      (mapcan
                       (pcase-lambda (`(,name . ,props))
                         (unless (-key-match-p '(or \\Noselect \\NonExistent)
                                               (alist-get 'flags props))
                           (list `[,name (funcall ,callback ,action ,name)])))
                       (-get-mailbox-listing (car mailbox))))))
      (easy-menu-add-item
       menu nil (if (length= set 1) "Message" (format "%s Messages" (length set))))
      (easy-menu-add-item
       menu nil `("Flags" :filter ,flagitems
                  \\Seen \\Flagged \\Answered $Forwarded $Junk))
      (easy-menu-add-item
       menu nil `("Copy To" :filter ,mbitems 'copy))
      (easy-menu-add-item
       menu nil `("Move To" :filter ,mbitems 'move))))
  menu)

(define-derived-mode minimail-mailbox-mode special-mode "Mailbox"
  "Major mode for mailbox listings."
  :interactive nil
  (add-hook 'quit-window-hook #'minimail-quit-message-window nil t)
  ;; Ensure we preserve sorting by column in the following sequence of
  ;; steps: sort by thread, then sort by column, then refresh buffer.
  (add-hook '-vtable-before-sort-by-current-column-hook
            (lambda () (setq -sort-by-thread nil))
            nil t)
  (add-hook 'context-menu-functions '-message-context-menu nil t)
  (setq-local
   buffer-undo-list t
   revert-buffer-function #'-mailbox-buffer-refresh
   truncate-lines t))

(defun -base-subject (string)
  "Simplify message subject STRING for sorting and threading purposes."
  (let ((case-fold-search t))
    (replace-regexp-in-string message-subject-re-regexp ""
                              (downcase (or string "")))))

(defun -format-subject (message)
  "Subject string of MESSAGE formatted for the mailbox buffer."
  (let* ((ancestors (when -sort-by-thread
                      (reverse (-thread-ancestors message)))))
    (concat
     ;; Indicate an incomplete thread
     (when (-message-in-reply-to (car ancestors))
       (when (eq (car ancestors) message)
         (icon-string 'minimail-thread-false-root)))
     ;; Thread tree prefix
     (mapconcat
      (lambda (msg)
        (icon-string
         (let ((siblings (-thread-children (-thread-parent msg))))
           (if (cdr (memq msg siblings)) ;not the last among its siblings
               (if (eq msg message)
                   'minimail-thread-leaf-nonterminal
                 'minimail-thread-parent-nonterminal)
             (if (eq msg message)
                 'minimail-thread-leaf-terminal
               'minimail-thread-parent-terminal)))))
      (cdr ancestors))
     ;; The subject itself, possibly mangled
     (let* ((subject (or (-message-subject message) ""))
            (boring (-base-subject (-message-subject (-thread-parent message))))
            (shortened (when (and (not (string-blank-p boring))
                                  (string-suffix-p boring subject t))
                         (substring subject 0 (- (length boring)))))
            (flags (-message-flags message)))
       (propertize (if shortened
                       (concat shortened (truncate-string-ellipsis))
                     subject)
                   'face (-alist-query flags minimail-subject-faces))))))

(defun -format-names (addresses)
  (propertize
   (mapconcat
    (lambda (addr)
      (or (-address-name addr) (-address-mailbox addr) "(unknown)"))
    addresses
    ", ")
   'help-echo
   (lambda (&rest _)
     (mapconcat
      (lambda (addr)
        (mail-header-make-address
         (-address-name addr)
         (concat (-address-mailbox addr) "@" (-address-host addr))))
      addresses
      "\n"))))

(defun -format-date (timestamp)
  (let* ((current-time-list nil)
         (today (let ((v (decode-time)))
                  (setf (decoded-time-hour v) 0)
                  (setf (decoded-time-minute v) 0)
                  (setf (decoded-time-second v) 0)
                  v))
         ;; Message age in seconds since start of this day
         (age (- (encode-time today) timestamp))
         (fmt (cond
               ((zerop timestamp) "")
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
                  (format-time-string "%a, %d %b %Y %T" timestamp)))))

(defun -message-timestamp (message)
  "The MESSAGE envelope date as a Unix timestamp."
    (let ((current-time-list nil))
      (condition-case nil
          (encode-time (parse-time-string (-message-date message)))
        (t 0))))

(defvar minimail-mailbox-mode-column-alist
  ;; NOTE: We must slightly abuse the vtable API in several of our
  ;; column definitions.  The :getter attribute returns a string used
  ;; as sort key while :formatter fetches from it the actual display
  ;; string, embedded as a string property.
  `((id
     :name "#"
     :getter -message-id)
    (flag-seen
     :name "Seen"
     :min-width 1
     :getter ,(lambda (msg _)
                (let ((icon (-alist-query
                             (-message-flags msg)
                             '(((not \\Seen) . minimail-message-unseen)))))
                  (if icon (icon-string icon) ""))))
    (flag-flagged
     :name "Flagged"
     :min-width 1
     :getter
     ,(lambda (msg _)
        (let ((icon (-alist-query
                     (append (alist-get 'x-gm-labels msg)
                             (-message-flags msg))
                     '((\\Flagged                   . minimail-message-flagged)
                       ((or $Important \\Important) . minimail-message-important)
                       ($Phishing                   . minimail-message-phishing)
                       ($Junk                       . minimail-message-junk)))))
          (if icon (icon-string icon) ""))))
    (flag-answered
     :name "Answered"
     :min-width 1
     :getter ,(lambda (msg _)
                (let ((icon (-alist-query
                             (-message-flags msg)
                             '((\\Answered . minimail-message-answered)
                               ($Forwarded . minimail-message-forwarded)))))
                  (if icon (icon-string icon) ""))))
    (from
     :name "From"
     :max-width 30
     :getter ,(lambda (msg _) (-format-names (-message-from msg))))
    (to
     :name "To"
     :max-width 30
     :getter ,(lambda (msg _) (-format-names (-message-to msg))))
    (recipients
     :name "Recipients"
     :max-width 30
     :getter ,(lambda (msg _)
                (-format-names (append (-message-to msg)
                                       (-message-cc msg)
                                       (-message-bcc msg)))))
    (subject
     :name "Subject"
     :max-width 80
     :getter ,(lambda (msg _)
                (let ((s (-base-subject (-message-subject msg))))
                  (propertize (if (string-empty-p s) "\0" s)
                              'minimail msg)))
     :formatter ,(lambda (s) (-format-subject (-get-data s))))
    (date
     :name "Date"
     :width 12
     :align left
     :getter ,(lambda (msg _) (-message-timestamp msg))
     :formatter -format-date)
    (size
     :name "Size"
     :getter ,(lambda (msg _) (alist-get 'rfc822-size msg))
     :formatter ,(lambda (v)
                   (propertize (file-size-human-readable-iec v)
                               'face 'vtable)))))

(defun -mailbox-total-messages ()
  ;; FIXME: Wrong for search mailboxes
  (seq-reduce (lambda (i msg) (max i (-message-id msg)))
              -message-list 0))

(defun -mailbox-buffer-update (messages)
  "Set `minimail--message-list' to MESSAGES and do all necessary adjustments."
  (-vtable-ensure-table)
  (setq -message-list messages)
  (setq -thread-data nil)
  (let ((point-uid (-message-uid (vtable-current-object)))
        (arrow-uid (when overlay-arrow-position
                     (save-excursion
                       (goto-char overlay-arrow-position)
                       (-message-uid (vtable-current-object))))))
    (vtable-revert-command)
    (when -sort-by-thread
      ;; FIXME: Can we avoid drawing the table twice?
      (-sort-by-thread (eq -sort-by-thread 'descend)))
    (when arrow-uid
      (save-excursion
        (if (-vtable-find-object (lambda (m) (eq (-message-uid m) arrow-uid)))
            (move-marker overlay-arrow-position (point))
          (setq overlay-arrow-position nil))))
    (-vtable-find-object (lambda (m) (eq (-message-uid m) point-uid))))
  (save-excursion
    (let ((inhibit-read-only t)
          (count (length -message-list))
          (total (-mailbox-total-messages)))
      (vtable-end-of-table)
      (delete-region (point) (point-max))
      (insert (if (eq count total)
                  "Showing all messages\n"
                (format "Showing %s of %s messages %s\n" count total
                        (buttonize "[load more]"
                                   (lambda (_)
                                     (minimail-load-more-messages)))))))))

(defun -mailbox-buffer-populate (&rest _)
  "Fetch messages for the first time and create a vtable in the current buffer."
  (let* ((buffer (current-buffer))
         (mailbox -current-mailbox)
         (limit (-settings-scalar-get :fetch-limit mailbox))
         (search (alist-get 'search -local-state)))
    (-set-mode-line-suffix 'loading)
    (athunk-run
     (athunk-let*
         ((messages <- (athunk-condition-case err
                           (if search
                               (-afetch-search mailbox search limit)
                             (-afetch-old-messages mailbox limit))
                         (t (with-current-buffer buffer
                              (-set-mode-line-suffix err))
                            (signal (car err) (cdr err))))))
       (with-current-buffer buffer
         (-set-mode-line-suffix nil)
         (let* ((inhibit-read-only t)
                (vtable-map (make-sparse-keymap)) ;only way to disable extra keymap
                (colnames (-settings-alist-get :mailbox-columns mailbox))
                (sortnames (-settings-alist-get :mailbox-sort-by mailbox)))
           (make-vtable
            :objects-function (lambda () (or -message-list '(())))
            :columns (mapcar (lambda (v)
                               (alist-get v minimail-mailbox-mode-column-alist))
                             colnames)
            :sort-by (mapcan (pcase-lambda (`(,col . ,dir))
                               (when-let* ((i (seq-position colnames col)))
                                 `((,i . ,dir))))
                             sortnames))
           (setq -sort-by-thread (alist-get 'thread sortnames)))
         (-mailbox-buffer-update messages))))))

(defun -mailbox-buffer-refresh (&rest _)
  (unless (derived-mode-p #'minimail-mailbox-mode)
    (user-error "This should be called only from a mailbox buffer."))
  (let* ((buffer (current-buffer))
         (mailbox -current-mailbox)
         (messages -message-list)
         (search (alist-get 'search -local-state)))
    (when search (error "Not implemented"))
    (-set-mode-line-suffix 'loading)
    (athunk-run
     (athunk-let*
         ((messages <- (athunk-condition-case err
                           (-afetch-new-messages mailbox messages)
                         (t (with-current-buffer buffer
                              (-set-mode-line-suffix err))
                            (signal (car err) (cdr err))))))
       (with-current-buffer buffer
         (-set-mode-line-suffix nil)
         (-mailbox-buffer-update messages))))))

(defun minimail-load-more-messages (&optional count)
  "Add older messages to the current mailbox buffer.
With a prefix argument (or COUNT argument from Lisp), load up to that
many message, otherwise use `minimail-fetch-limit'."
  (interactive (list (when current-prefix-arg
                       (prefix-numeric-value current-prefix-arg)))
               minimail-mailbox-mode)
  (let* ((buffer (current-buffer))
         (mailbox -current-mailbox)
         (messages -message-list)
         (search (alist-get 'search -local-state))
         (limit (or count (-settings-scalar-get :fetch-limit mailbox)))
         (before (seq-min (mapcar (lambda (msg) (-message-uid msg)) messages))))
    (when search (error "Not implemented"))
    (-set-mode-line-suffix 'loading)
    (athunk-run
     (athunk-let*
         ((old <- (athunk-condition-case err
                      (-afetch-old-messages mailbox limit before)
                    (t (with-current-buffer buffer
                         (-set-mode-line-suffix err))
                       (signal (car err) (cdr err))))))
       (with-current-buffer buffer
         (-set-mode-line-suffix nil)
         (unless old (user-error "No more old messages"))
         (-mailbox-buffer-update (nconc old messages)))))))

(defun minimail-show-message ()
  (interactive nil minimail-mailbox-mode)
  (let ((msg (-current-message)))
    (unless (member "\\Seen" (-message-flags msg))
      (push "\\Seen" (cdr (assq 'flags msg)))
      (vtable-update-object (vtable-current-table) msg))
    (setq-local overlay-arrow-position (copy-marker (pos-bol)))
    (athunk-run (-adisplay-message -current-mailbox (-message-uid msg)))))

(defun minimail-show-message-raw ()
  (interactive nil minimail-mailbox-mode)
  (let ((-message-rendering-function #'ignore))
    (minimail-show-message)))

(defun minimail-next-message (count)
  (interactive "p" minimail-mailbox-mode minimail-message-mode)
  (with-current-buffer (-find-buffer 'mailbox)
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

(defun minimail-next-message-unseen (direction)
  (interactive "p" minimail-mailbox-mode minimail-message-mode)
  (with-current-buffer (-find-buffer 'mailbox)
    (if (not overlay-arrow-position)
        (goto-char (point-min))
      (goto-char overlay-arrow-position))
    (unless (-vtable-find-object
             (lambda (msg)
               (not (assoc-string '\\Seen (-message-flags msg))))
             (cl-signum direction))
      (user-error "No unseen messages %s"
                  (if (minusp direction) 'above 'below)))
    (when-let* ((window (get-buffer-window)))
      (set-window-point window (point)))
    (minimail-show-message)))

(defun minimail-previous-message-unseen (direction)
  (interactive "p" minimail-mailbox-mode minimail-message-mode)
  (minimail-next-message-unseen (- direction)))

(defun minimail-quit-message-window (&optional kill)
  "If there is a window showing a message from this mailbox, quit it.
If KILL is non-nil, kill the message buffer instead of burying it."
  (interactive nil minimail-mailbox-mode)
  (when-let* ((msgbuf (-find-buffer 'message t))
              (window (get-buffer-window msgbuf)))
    (quit-restore-window window (if kill 'kill 'bury))))

(defun -flag-readable-name (flag)
  (let ((v (if (symbolp flag) (symbol-name flag) flag)))
    (propertize (if (string-match-p (rx bos (or ?\\ ?$)) v)
                    (substring v 1) v)
                'minimail flag)))

(defun minimail-toggle-message-flags (set flags &optional how)
  "Toggle FLAGS of a message SET in the current mailbox.
HOW can be positive to add, negative to remove, or nil to toggle each of
the flags.  When SET has more than one message, toggling means to add a
flag if at least one message is missing it, and remove it otherwise.

Interactively, SET is given by the selected messages or the message
under point, FLAGS is read from the minibuffer and HOW comes from the
prefix argument."
  ;; FIXME: It doesn't make sense to have a public function with a SET
  ;; argument, as there is not public API to obtain such a list.
  (interactive (let* ((how (prefix-numeric-value (or current-prefix-arg 0)))
                      ;; TODO: use flags from EXAMINE
                      (flags (mapcar #'-flag-readable-name
                                     '(\\Seen \\Flagged \\Answered
                                       $Forwarded $Junk $Phishing)))
                      (query (mapcar
                              (lambda (it) (or (car (member it flags)) it))
                              (completing-read-multiple
                               (format "%s flags: " (cond ((plusp how) "Add")
                                                          ((minusp how) "Remove")
                                                          (t "Toggle")))
                               flags nil t))))
                 (list (-selected-messages)
                       (seq-uniq (delq nil (mapcar #'-get-data query)))
                       how))
               minimail-mailbox-mode minimail-message-mode)
  (dolist (flag (ensure-list flags))
    (let* ((flagp (lambda (uid)
                    (let ((msg (seq-find (lambda (m) (eq (-message-uid m) uid))
                                         -message-list)))
                      (assoc-string flag (-message-flags msg)))))
           (remove
            (cond ((plusp how) nil)
                  ((minusp how) t)
                  (t (seq-every-p flagp (ensure-list set))))))
      (athunk-run
       (-astore-message-flags -current-mailbox set flag remove)))))

(defun minimail-toggle-message-seen (set &optional how)
  "Toggle the Seen flag of the current message(s).
The meaning of SET and HOW, as well as the interactive behavior, are as
in `minimail-toggle-message-flags'."
  (interactive (list (-selected-messages)
                     (prefix-numeric-value (or current-prefix-arg 0)))
               minimail-mailbox-mode minimail-message-mode)
  (minimail-toggle-message-flags set '\\Seen how))

(defun minimail-toggle-message-flagged (set &optional how)
  "Toggle the Flagged flag of the current message(s).
The meaning of SET and HOW, as well as the interactive behavior, are as
in `minimail-toggle-message-flags'."
  (interactive (list (-selected-messages)
                     (prefix-numeric-value (or current-prefix-arg 0)))
               minimail-mailbox-mode minimail-message-mode)
  (minimail-toggle-message-flags set '\\Flagged how))

;;;; Sorting by thread

(defun -thread-data-shallow (messages)
  "Compute a shallow message thread tree from MESSAGES.
Use server-side thread identifiers if available, otherwise infer the
thread structure from the message subjects."
  (let* ((result (make-hash-table))
         (threads (make-hash-table :test #'equal)))
    (dolist (msg messages)
      (push msg (gethash (or (alist-get 'thread-id msg)
                             (-base-subject (-message-subject msg)))
                         threads)))
    (dolist (thread (hash-table-values threads))
      (cl-callf sort thread :key #'-message-timestamp :in-place t)
      (setf (alist-get 'children (gethash (car thread) result)) (cdr thread))
      (dolist (child (cdr thread))
        (setf (alist-get 'parent (gethash child result)) (car thread))))
    result))

(defun -thread-data-hierarchical (messages)
  "Compute a hierarchical message thread tree from MESSAGES.
This relies solely on Message-ID and In-Reply-To headers from the IMAP
envelope and doesn't use server-side threading information."
  (let* ((result (make-hash-table))
         (msgids (make-hash-table :test #'equal))) ;map Message-ID -> msg
    (dolist (msg messages)
      (when-let* ((msgid (-message-message-id msg)))
        (puthash msgid msg msgids)))
    (dolist (msg messages)
      (when-let*
          ((inreply (-message-in-reply-to msg))
           (inreply (when (string-match "<.*?>" inreply)
                      (match-string-no-properties 0 inreply)))
           (parent (gethash inreply msgids)))
        (setf (alist-get 'parent (gethash msg result)) parent)
        (push msg (alist-get 'children (gethash parent result)))))
    (dolist (msg messages)
      (cl-callf sort (alist-get 'children (gethash msg result))
        :key #'-message-timestamp
        :in-place t))
    result))

(defun -thread-data ()
  (with-memoization -thread-data
    (funcall
     (pcase-exhaustive
         (or (-settings-scalar-get :thread-style -current-mailbox)
             (let ((caps (athunk-run-polling
                          (-aget-capability (car -current-mailbox))
                          :interval 0.1 :max-tries 100)))
               (if (or (memq 'objectid caps) (memq 'x-gm-ext-1 caps))
                   'shallow 'hierarchical)))
       ('shallow #'-thread-data-shallow)
       ('hierarchical #'-thread-data-hierarchical))
     -message-list)))

(defun -thread-parent (message)
  "The parent of MESSAGE in the thread tree."
  (alist-get 'parent (gethash message (-thread-data))))

(defun -thread-children (message)
  "All immediate children of MESSAGE in the thread tree."
  (alist-get 'children (gethash message (-thread-data))))

(defun -thread-ancestors (message)
  "All ancestors of MESSAGE, from itself up until the thread root."
  (with-memoization (alist-get 'ancestors (gethash message (-thread-data)))
    (let ((parent (-thread-parent message)))
      (cons message (when parent (-thread-ancestors parent))))))

(defun -thread-flat (message)
  "The subthread headed by MESSAGE, as a flat list."
  (with-memoization (alist-get 'flat (gethash message (-thread-data)))
    (cons message (seq-mapcat #'-thread-flat (-thread-children message)))))

(defun -thread-position (message)
  "The position of MESSAGE within its thread."
  (let ((root (car (last (-thread-ancestors message)))))
    (seq-position (-thread-flat root) message #'eq)))

(defun -sort-by-thread (&optional descend)
  "Sort messages with grouping by threads.

Within a thread, sort each message after its parents.  Across threads,
preserve the existing order, in the sense that thread A sorts before
thread B if some message from A comes before all messages of B.  This
makes sense when the current sort order is in the “most relevant at top”
style.  If DESCEND is non-nil, use the opposite convention."
  (let* ((table (-vtable-ensure-table))
         (rootpos (make-hash-table))
         (key (lambda (msg)
                (let ((root (car (last (-thread-ancestors msg)))))
                  (cons (gethash root rootpos) (-thread-position msg)))))
         (i 0)
         messages)
    (save-excursion
      ;; Get messages in current sort order (unlike `vtable-objects')
      ;; and at the same time fill rootpos with the sorting position
      ;; of each thread root.
      (vtable-beginning-of-table)
      (while-let ((msg (vtable-current-object)))
        (let* ((root (car (last (-thread-ancestors msg))))
               (j (gethash root rootpos))
               (k (cond ((not j) i)
                        (descend (max i j))
                        (t (min i j)))))
          (puthash root k rootpos))
        (push msg messages)
        (incf i)
        (forward-line)))
    (setq -message-list (sort messages :key key :in-place t))
    ;; Little hack to force vtable to redisplay with our new sorting.
    (cl-letf (((vtable-sort-by table) nil))
      (vtable-revert-command))))

(defun minimail-sort-by-thread (arg)
  "Toggle sorting messages by thread.
ARG can be one specific thread sorting style (nil, ascend or descend),
or t to toggle between those options."
  (interactive '(t) minimail-mailbox-mode)
  (setq -sort-by-thread
        (pcase-exhaustive arg
          ((or 'nil 'ascend 'descend) arg)
          ('t (cadr (memq -sort-by-thread '(nil ascend descend))))))
  (message "Sorting by thread: %s" (or -sort-by-thread 'off))
  ;; First re-sort the table by the original criteria, either because
  ;; that's the final goal or in preparation for the thread sorting
  ;; step.
  (vtable-revert-command)
  (when -sort-by-thread
    (-sort-by-thread (eq -sort-by-thread 'descend))))

;;; Message buffer

(defvar -message-erase-function #'erase-buffer
  "Function called to erase a message buffer.")

(defvar-keymap minimail-message-mode-map
  :parent (make-composed-keymap (list minimail-base-keymap button-buffer-map)
                                special-mode-map))

(easy-menu-define nil minimail-message-mode-map
  "Menu for `minimail-message-mode'."
  ;; TODO: Add the content of the context menu for messages here.
  `("Message"
    ["Reply" minimail-reply]
    ["Reply All" minimail-reply-all]
    ["Forward" minimail-forward]
    ("Mailing List"
     ["Copy Permalink" minimail-message-archived-at
      :active (minimail-message-archived-at)]
     ["Browse List Archives" minimail-message-list-archive
      :active (minimail-message-list-archive)]
     ["Unsubscribe" minimail-message-list-unsubscribe
      :active (minimail-message-list-unsubscribe)])
    ,@-base-menu))

(define-derived-mode minimail-message-mode special-mode "Message"
  "Major mode for email messages."
  :interactive nil
  (setq buffer-undo-list t))

(defun -message-buffer-name (mailbox uid)
  "Name of buffer to display MAILBOX message with given UID."
  (format "%s[%s]" (-mailbox-display-name mailbox) uid))

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

(defun -all-headers ()
  (let (headers)
    (save-restriction
      (message-narrow-to-head)
      (while (re-search-forward "^\\([^ \t:]+\\):" nil t)
        (let ((key (downcase (match-string 1)))
              (value (string-trim
                      (replace-regexp-in-string
                       "\n[\t ]+" " "
                       (buffer-substring-no-properties
                        (point)
                        (re-search-forward "^\\<\\|\\'"))))))
          (push value (alist-get key headers nil nil #'equal)))))
    (dolist (it headers)
      (cl-callf nreverse (cdr it)))
    (nreverse headers)))

(defvar-local -adisplay-message nil
  "Synchronization data for the function of same name.")

(defun -adisplay-message (mailbox uid)
  "Display the message identified by MAILBOX and UID.
Return an athunk which yields the buffer displaying the message.
If the operation of displaying the message is canceled, say because
the user selected another message in the meanwhile, yield nil."
  (let ((render -message-rendering-function)
        (buffer (or (-find-buffer 'message t)
                    (generate-new-buffer
                     (-message-buffer-name mailbox "")))))
    (with-current-buffer buffer
      (unless (derived-mode-p #'minimail-message-mode)
        (minimail-message-mode))
      (-set-mode-line-suffix 'loading)
      (setq -adisplay-message (cons uid mailbox)))
    (display-buffer buffer -display-message-base-action)
    (athunk-let*
        ((text <- (athunk-condition-case err
                      (-afetch-message-body mailbox uid)
                    (t (with-current-buffer buffer
                         (-set-mode-line-suffix err))
                       (signal (car err) (cdr err))))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (when (equal -adisplay-message (cons uid mailbox))
            (let ((inhibit-read-only t))
              (-set-mode-line-suffix nil)
              (funcall -message-erase-function)
              (rename-buffer (-message-buffer-name mailbox uid) t)
              (decode-coding-string text 'raw-text-dos nil buffer)
              (setq -current-mailbox mailbox)
              (setq -message-list `(((uid . ,uid) (headers . ,(-all-headers)))))
              (funcall render)
              (set-buffer-modified-p nil)
              (when-let* ((window (get-buffer-window (current-buffer))))
                (set-window-point window (point-min)))
              buffer)))))))

(defun minimail-message-scroll-up (arg &optional reverse)
  (interactive "^P" minimail-message-mode minimail-mailbox-mode)
  (with-current-buffer (-find-buffer 'message)
    (condition-case nil
        (if-let* ((window (get-buffer-window)))
            (with-selected-window window
              (funcall (if reverse #'scroll-down-command #'scroll-up-command)
                       arg))
          (display-buffer (current-buffer) -display-message-base-action))
      (beginning-of-buffer (with-current-buffer (-find-buffer 'mailbox)
                             (minimail-next-message -1)))
      (end-of-buffer (with-current-buffer (-find-buffer 'mailbox)
                       (minimail-next-message 1))))))

(defun minimail-message-scroll-down (arg)
  (interactive "^P" minimail-message-mode minimail-mailbox-mode)
  (minimail-message-scroll-up arg t))

;;;; Reply and forward

(defun -reply-recipients (headers wide)
  "Compute recipients of a reply from the original message HEADERS."
  ;; We really want to reuse the rather complex logic of
  ;; `message-get-reply-headers' which, sadly, reads headers from the
  ;; current buffer.  At the same time, we want to honor buffer-local
  ;; values of variables such as `user-mail-address'.  So we use the
  ;; current buffer (a message buffer) as scratch area.
  (save-restriction
    (widen)
    (goto-char (point-min))
    (pcase-dolist (`(,key . ,values) headers)
      (when (assoc-string key '( from to cc reply-to subject
                                 mail-copies-to mail-followup-to
                                 mail-reply-to original-to))
        (dolist (value values)
          (insert key ": " (mail-decode-encoded-word-string value) ?\n))))
    (insert ?\n)
    (let ((end (point-marker)))
      (prog1
          (or (funcall (if wide message-wide-reply-to-function
                         message-reply-to-function))
              (message-get-reply-headers wide))
        (delete-region (point-min) end)))))

(defun minimail-reply (&optional cite wide)
  (interactive (list (xor current-prefix-arg minimail-reply-cite-original))
               minimail-message-mode
               minimail-mailbox-mode)
  (athunk-run
   (athunk-let*
       ((buffer <- (if (derived-mode-p 'minimail-message-mode)
                       (athunk-wrap (current-buffer))
                     (-adisplay-message -current-mailbox
                                        (-message-uid (-current-message))))))
     (set-buffer (or buffer (user-error "No message buffer")))
     (let ((mailbox -current-mailbox)
           (uid (alist-get 'uid (car -message-list)))
           (headers (alist-get 'headers (car -message-list))))
       (-compose-mail                   ;this changes current buffer
        nil nil nil nil nil buffer
        `((funcall ,(lambda ()
                        (athunk-run
                         (-astore-message-flags mailbox uid '\\Answered))))))
       (pcase-dolist (`(,key . ,value) (-reply-recipients headers wide))
         (message-replace-header (symbol-name key) value))
       (message-replace-header
        "Subject" (concat "Re: "
                          (message-simplify-subject
                           (mail-decode-encoded-word-string
                            (cadr (assoc-string 'subject headers))))))
       (message-replace-header
        "In-Reply-To" (cadr (assoc-string 'message-id headers)))
       (message-replace-header
        "References" (concat
                      (cadr (assoc-string 'references headers))
                      (when (assoc-string 'references headers) " ")
                      (cadr (assoc-string 'message-id headers))))
       (message-sort-headers)
       (message-hide-headers)
       (setq buffer-undo-list nil)
       (message-goto-body)
       (when cite
         (with-undo-amalgamate
           (message-yank-original)))))))

(defun minimail-reply-all (cite)
  (interactive (list (xor current-prefix-arg minimail-reply-cite-original))
               minimail-message-mode
               minimail-mailbox-mode)
  (minimail-reply cite t))

(defun minimail-forward ()
  (interactive nil minimail-message-mode minimail-mailbox-mode)
  (athunk-run
   (athunk-let*
       ((buffer <- (if (derived-mode-p 'minimail-message-mode)
                       (athunk-wrap (current-buffer))
                     (-adisplay-message -current-mailbox
                                        (-message-uid (-current-message))))))
     (set-buffer (or buffer (user-error "No message buffer")))
     (let* ((mailbox -current-mailbox)
            (uid (alist-get 'uid (car -message-list)))
            (message-forward-decoded-p t)
            (subject (message-make-forward-subject)))
       (-compose-mail                   ;this changes current buffer
        nil subject nil nil nil nil
        `((funcall ,(lambda ()
                        (athunk-run
                         (-astore-message-flags mailbox uid '$Forwarded))))))
       (message-forward-make-body buffer)))))

;;;; Mailing list commands

(defun -act-on-header-url (action key)
  (let* ((headers (alist-get 'headers (car -message-list)))
         (url (cadr (assoc-string key headers))))
    (when url (setq url (string-trim url "<" ">.*")))
    (when (and action (not url))
      (user-error "Message has no %s header" (capitalize (symbol-name key))))
    (pcase action
      ('copy
       (message "Copied `%s'" url)
       (kill-new url))
      ('browse
       (when (or (string-prefix-p "mailto:" url)
                 (y-or-n-p (format "%s\nBrowse this URL?" url)))
         (browse-url url))))
    url))

(defun minimail-message-archived-at (&optional action)
  "Get an archival URL for this message.
Interactively, copy the URL to kill ring or, with a prefix argument,
browse it."
  (interactive (list (if current-prefix-arg 'browse 'copy))
               minimail-message-mode)
  (-act-on-header-url action 'archived-at))

(defun minimail-message-list-unsubscribe (&optional action)
  "Get the mailing list unsubscription URL.
Interactively, browse the URL or, with a prefix argument, copy it to the
kill ring."
  (interactive (list (if current-prefix-arg 'copy 'browse))
               minimail-message-mode)
  (-act-on-header-url action 'list-unsubscribe))

(defun minimail-message-list-archive (&optional action)
  "Get the mailing list archive URL.
Interactively, browse the URL or, with a prefix argument, copy it to the
kill ring."
  (interactive (list (if current-prefix-arg 'copy 'browse))
               minimail-message-mode)
  (-act-on-header-url action 'list-archive))

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
   gnus-header-button-alist nil         ;FIXME: make customizable
   gnus-button-alist nil                ;FIXME: make customizable
   gnus-newsgroup-charset nil
   gnus-newsgroup-name nil
   gnus-summary-buffer nil
   nobreak-char-display nil)
  (run-hooks 'gnus-article-decode-hook)
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
  :parent (make-composed-keymap widget-keymap special-mode-map)
  ":" #'minimail-execute-server-command
  "s" #'minimail-search
  "c" #'compose-mail)

(easy-menu-define nil minimail-overview-mode-map
  "Menu for `minimail-overview-mode'."
  `("Minimail" ,@-base-menu))

(define-derived-mode minimail-overview-mode special-mode "Minimail"
  "Major mode for browsing a mailbox tree."
  :interactive nil
  (setq buffer-undo-list t)
  (add-hook 'tree-widget-before-create-icon-functions #'-overview-create-icon nil t)
  (setq-local revert-buffer-function (lambda (&rest _)
                                       (-overview-buffer-populate t))))

(defun -overview-create-icon (icon)
  (widget-put icon :glyph-name nil)
  (widget-put icon :tag
              (icon-string
               (pcase (widget-type icon)
                 ('tree-widget-leaf-icon
                  (widget-put icon :tab-order -1)
                  (widget-get (widget-get icon :node) :icon))
                 ('tree-widget-open-icon 'minimail-mailbox-open)
                 (_ 'minimail-mailbox-closed)))))

(defun -overview-tree-expand (widget)
  (let ((acct (widget-get widget :account))
        (path (widget-get widget :path)))
    (mapcan
     (pcase-lambda (`(,name . ,props))
       (let-alist props
         (when (equal path (cdr .path))
           (let ((node (if (-key-match-p '(or \\Noselect \\NonExistent) .flags)
                           `(item :tag ,(car .path))
                         `(link
                           :tag ,(propertize (car .path)
                                             '-current-mailbox (cons acct name))
                           :format "%[%t%]%d"
                           :button-prefix ""
                           :button-suffix ""
                           :doc ,(propertize (or (-mailbox-annotation props) "")
                                             'face 'completions-annotations)
                           :icon
                           ,(seq-some
                             (pcase-lambda (`(,cond . ,icon))
                               (when (-key-match-p cond .flags) icon))
                             '(((or \\All \\Archive) . minimail-mailbox-archive)
                               (\\Drafts             . minimail-mailbox-drafts)
                               (\\Flagged            . minimail-mailbox-flagged)
                               (\\Important          . minimail-mailbox-important)
                               (\\Junk               . minimail-mailbox-junk)
                               (\\Sent               . minimail-mailbox-sent)
                               (\\Trash              . minimail-mailbox-trash)
                               (t                    . minimail-mailbox)))
                           :action
                           ,(lambda (&rest _)
                              (minimail-find-mailbox
                               (get-text-property (point) '-current-mailbox)))))))
             (if (-key-match-p '(or \\HasNoChildren \\Noinferiors) .flags)
                 `(,node)
               `((tree-widget :node ,node
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
            (mailboxes (mapcar
                        (pcase-lambda (`(,name . ,props))
                          (let* ((delim (alist-get 'delimiter props))
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
(progn
  (define-mail-user-agent 'minimail
    #'-compose-mail
    #'message-send-and-exit
    #'message-kill-buffer
    'message-send-hook)
  ;; Probably can't use `cl-pushnew' here.
  (let ((item '(const :tag "Minimail" minimail))
        (items (get 'mail-user-agent 'custom-type)))
    (unless (member item items)
      (push item (cdr items)))))

;;;###autoload
(defun -compose-mail (&rest args)
  (let* ((mailbox (or (get-text-property (point) '-current-mailbox)
                      -current-mailbox))
         (address (or (-settings-scalar-get :mail-address mailbox)
                      ;; Don't read `user-mail-address' directly as it may be
                      ;; some generated nonsense.
                      (message-user-mail-address)
                      (completing-read
                       "Send mail as: "
                       (mapcan
                        (lambda (it)
                          (let ((addr (plist-get (cdr it) :mail-address)))
                            (if (stringp addr)
                                (list addr)
                              (mapcar #'cdr addr))))
                        minimail-accounts))))
         (hook (lambda ()
                 (setq -current-mailbox mailbox)
                 (pcase-let
                     ((`(,addr . ,name) (-split-mail-address address)))
                   (setq-local user-mail-address addr)
                   (when name
                     (setq-local user-full-name name)))
                 (pcase (-settings-scalar-get :signature mailbox)
                   (`(file ,fname . nil)
                    (setq-local message-signature t
                                message-signature-file fname))
                   (v (setq-local message-signature v))))))
    (let ((message-mail-user-agent nil))
      (add-hook 'message-mode-hook hook -90)
      (unwind-protect
          (apply #'message-mail args)
        (remove-hook 'message-mode-hook hook)))
    (message-sort-headers)
    (message-position-point)
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

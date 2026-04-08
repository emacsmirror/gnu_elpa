;;; futur-server.el --- An ELisp server for the Futur library  -*- lexical-binding: t -*-

;; Copyright (C) 2026  Free Software Foundation, Inc.

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

;; This aims to provide a batch server to run arbitrary ELisp code,
;; similar to what you might do via
;;
;;     emacs --batch --eval "(foo bar)"
;;
;; but where the server stays around to try and avoid startup time.
;; The aim is to be usable even for simple a short queries that
;; expect an "immediate" answer, for example because we want the
;; execution to be sandboxed.

;;; Code:

(eval-when-compile (require 'cl-lib))

;;;; Base protocol

;; (trace-function 'futur--read-stdin)
;; (trace-function 'futur--print-stdout)

(defvar futur-server-include-backtraces nil
  "If non-nil, include a backtrace when returning an error.")

(defconst futur-elisp--impossible-string "\n# \"# "
  "String that will necessarily cause `read' to signal an error.")

(defvar futur--read-from-minibuffer
  (symbol-function 'read-from-minibuffer))

(defvar futur--initial-buffer-list (mapcar #'buffer-name (buffer-list)))

(defun futur--read-stdin ()
  "Read a sexp from a single line on stdin."
  (unless noninteractive (error "futur--read-stdin works only in batch mode"))
  (funcall futur--read-from-minibuffer "" nil nil t))

(defun futur--print-stdout (sexp sid)
  "Print SEXP on stdout using ID as the leading marker."
  (unless noninteractive (error "futur--print-stdout works only in batch mode"))
  (let ((coding-system-for-write 'emacs-internal)
        (print-length nil)
        (print-level nil)
        (print-circle t)
        (print-gensym t)
        (print-escape-newlines nil)
        ;; SWP aren't currently printed in a `read'able way, so we may
        ;; as well print them bare.
        (print-symbols-bare t))
    (princ sid t)
    (condition-case err
        (prin1 sexp t)
      (t (prin1 `(:print-error . ,err) t)))
    (princ futur-elisp--impossible-string t)
    (terpri t)))

;; (defun futur-server-call-in-context (ctxname ctx func &rest args)
;;   (futur--obarray ctxname ctx)
;;   (apply func args))

(define-error 'futur-inhibited-interaction
              "Interaction inhibited in futur servers")

(defun futur-server--apply (rid func args)
  (let* ((backtrace nil))
    (condition-case err
        (let* ((inhibit-quit nil)
               (res
                (if (not (and (fboundp 'handler-bind)
                              futur-server-include-backtraces))
                    (apply func args)
                  (letrec ((debugfun
                            (lambda (_err)
                              (with-temp-buffer
                                (cl-letf ((standard-output
                                           (current-buffer))
                                          ;; Use barebones prin1, so we can
                                          ;; try and `read' the objects.
                                          ((symbol-function 'cl-prin1) nil))
                                  (debug-early-backtrace debugfun))
                                (setq backtrace
                                      (buffer-substring-no-properties
                                       (point-min) (point-max)))))))
                    (handler-bind ((t debugfun))
                      (apply func args))))))
          `(:funcall-success ,rid . ,res))
      (t
       (if (stringp backtrace)
           `(:funcall-error ,rid ,backtrace ,@err)
         `(:funcall-error ,rid . ,err))))))

(defun futur-server ()
  ;; Try and make sure the client code gets an error if it tries to use stdin.
  (let ((errorfun (lambda (&rest _) (signal 'futur-inhibited-interaction nil))))
    (dolist (fun '( read-from-minibuffer yes-or-no-p read-string
                    ;; FIXME: Maybe some of these could still be acceptable,
                    ;; e.g. when called with a timeout?
                    read-key-sequence read-char read-event read-char-exclusive
                    ;; FIXME: There are still ways to try and read from stdin,
                    ;; e.g. via `interactive' specs.
                    ))
      (fset fun errorfun)))
  ;; FIXME: Prevent client code from using stdout?

  ;; We want the `futur-elisp' client to be able to interrupt long-running
  ;; requests, and so far the only way we found is to abuse the SIGUSR1
  ;; escape hatch that was designed for debugging.
  ;; FIXME: This is hackish and doesn't work under w32 and Android.
  ;; https://lists.gnu.org/archive/html/emacs-devel/2026-03/msg00100.html
  (setq debug-on-event 'sigusr1)
  (add-function :around debugger
                (lambda (orig-fun reason object)
                  (if (and (eq 'error reason) (equal '(quit) object))
                      (signal object nil) ;FIXME: Use `error-resignal'.
                    (funcall orig-fun reason object))))
  ;; Initialize the cache of obarray snapshots.
  ;; Do it before we bind `inhibit-quit' to t, otherwise requests that use
  ;; `futur-reset-context' might inadvertently set it back to t.
  (futur-reset-context 'futur--server-internal nil)
  ;; We don't need a cryptographically secure ID, but just something that's
  ;; unlikely to occur by accident elsewhere.
  (let* ((sid (format " fes:%s "
                      (secure-hash 'sha1
                                   (format "%S:%S:%S"
                                           (random t) (current-time)
                                           (emacs-pid)))))
         (sid-sym (intern (string-trim sid)))
         ;; We want to be able to `quit' out of processing a request,
         ;; but if we receive the `quit' "too late", i.e. after we finished
         ;; computing the result, we don't want that `quit' to kill our REPL.
         (inhibit-quit t))
    (futur--print-stdout :ready sid)
    (while t                            ;The REPL.
      (let ((input (condition-case err (cons :read-success (futur--read-stdin))
                     (t err))))
        (pcase input
          ;; Check `sid-sym' for every request, since we may have just read
          ;; "successfully" the garbage that follows a failed read.
          (`(:read-success ,(pred (eq sid-sym)) ,rid ,func . ,args)
           ;; Ignore quits that occur between requests.  Ideally, we'd
           ;; do it earlier, like when we receive the first byte of
           ;; the request, but this is buried within `read-from-minibuffer'.
           (setq quit-flag nil)
           ;; Confirm we read successfully so the client can
           ;; distinguish where problems come from.
           (futur--print-stdout `(:read-success ,rid) sid)
           (futur--print-stdout (futur-server--apply rid func args) sid))
          (`(:read-success . ,rest)
           (futur--print-stdout `(:unrecognized-request . ,rest) sid))
          (_
           ;; FIXME: We can get an `end-of-file' error if the input line
           ;; is not a complete sexp but also if stdin was closed.
           ;; To distinguish the two it seems we have to look at
           ;; the actual error string :-(.
           (if (equal input '(end-of-file "Error reading from stdin"))
               (kill-emacs)
             (futur--print-stdout `(:read-error . ,input) sid))))))))

;;;; Manage execution contexts

(defun futur--obarray-snapshot ()
  "Return a snapshot of `obarray'.
Does not pay attention to buffer-local values of variables."
  ;; FIXME: Optimize away those symbols which still have the same values as
  ;; in all other snapshots?
  (let ((snapshot (obarray-make))
        ;; Always capture the value of `inhibit-quit' as being non-nil,
        ;; so that revert doesn't risk setting it temporarily to nil.
        (inhibit-quit t))
    (mapatoms
     (lambda (sym)
       (let ((fun (symbol-function sym))
             (plist (symbol-plist sym))
             (boundp (default-boundp sym)))
         (if (and (null fun) (null plist)
                  (or (keywordp sym) (not boundp)))
             nil
           (let ((ns (intern (symbol-name sym) snapshot))
                 (generic (plist-get plist 'cl--generic)))
             (setf (symbol-function ns) fun)
             ;; Copy symbols' plist because they're modified by side-effect :-(
             (setf (symbol-plist ns) (copy-sequence plist))
             ;; Generic-function objects are also mutated :-(
             (when generic
               (put ns 'futur--cl-generic
                    (cons (cl--generic-method-table generic)
                          (mapcar #'copy-sequence
                                  (cl--generic-dispatches generic)))))
             (when boundp
               (setf (default-value ns) (default-value sym))))))))
    snapshot))

(defun futur--obarray-revert (snapshot)
  "Revert `obarray' to the value it had when SNAPSHOT was taken."
  ;; We don't have `default-makunbound', so simulate it by
  ;; going to a dummy temp buffer.
  (unless snapshot (error "Can't use nil as obarray"))
  (with-temp-buffer
    (when (< emacs-major-version 31)
      ;; FIXME: Really ugly hack to temporarily work around bug#80538.
      (unintern "pcomplete-ignore-case" obarray)
      (unintern "url-bug-address" obarray)
      (unintern "executable-binary-suffixes" obarray))
    ;; We map only over `obarray', which takes care of all the symbols
    ;; present in `obarray', some of which are also in `snapshot'.
    ;; Strictly speaking, we should also map over `snapshot' to handle
    ;; those symbols that are missing from `obarray', but since
    ;; `snapshot' holds a previous state of `obarray', such symbols
    ;; can occur only if someone used `unintern', which should hopefully
    ;; never happen in the `obarray'.
    (let ((inhibit-quit t)) ;; Interruption can bring an inconsistent state.
      (mapatoms
       (lambda (sym)
         (let ((ss (intern-soft (symbol-name sym) snapshot)))
           (if (null ss)
               (progn
                 (setf (symbol-function sym) nil)
                 (setf (symbol-plist sym) nil)
                 (unless (keywordp sym) (makunbound sym)))
             ;; FIXME: Emacs<31 would try and compile trampolines needlessly
             ;; (and unsuccessfully (because we're in a halfway state).
             (unless (eq (symbol-function sym) (symbol-function ss))
               (setf (symbol-function sym) (symbol-function ss)))
             (let ((plist (symbol-plist ss)))
               ;; Copy symbols' plist because `put' mutates them.  :-(
               (setf (symbol-plist sym) (copy-sequence plist))
               (let ((generic-extra (plist-get plist 'futur--cl-generic)))
                 (when generic-extra
                   (let ((generic (plist-get plist 'cl--generic)))
                     (setf (cl--generic-method-table generic)
                           (car generic-extra))
                     (setf (cl--generic-dispatches generic)
                           (cdr generic-extra))))))
             ;; FIXME: Do we need to do something special for var-aliases?
             (condition-case err
                 (if (default-boundp ss)
                     ;; FIXME: Test (eq (default-value sym) (default-value ss))?
                     (setf (default-value sym) (default-value ss))
                   (when (default-boundp sym)
                     (cl-assert (not (keywordp sym)))
                     (unless (keywordp sym) (makunbound sym))))
               (setting-constant nil)
               (error
                ;; Variable watchers might run and fail because of
                ;; currently undefined functions and variables.
                (message "While setting %S with watchers %S, error: %S"
                         sym (get sym 'watchers) err)
                (cl-letf (((get sym 'watchers) nil))
                  (if (default-boundp ss)
                      (setf (default-value sym) (default-value ss))
                    (when (default-boundp sym)
                      (cl-assert (not (keywordp sym)))
                      (unless (keywordp sym) (makunbound sym)))))
                )))))))))

(defun futur--list-prefix-p (prefix other-list)
  (while (and (consp prefix) (consp other-list)
              (equal (car prefix) (car other-list)))
    (setq prefix (cdr prefix))
    (setq other-list (cdr other-list)))
  (null prefix))

(defalias 'futur-reset-context
  ;; Store the snapshots inside the closure rather than in a global
  ;; variable, so that `futur--obarray-revert' doesn't undo it.
  (let ((snapshots '()))
    (lambda (name target)
      "Reset vars and functions to a known state.
NAME is the name chosen for that state.
TARGET is the description of the context.  It should be a list
of elements that can be:
- A file name (string) that should be `load'ed.
- A feature (symbol) that should be `require'd.
- A list (funcall FUNC . ARGS) that should be called.
The elements are processed in order, starting from the state at startup.
NAME is used only for the purpose of overwriting a previous state from
the cache."
      (when (and target (null snapshots))
        (error "`futur--obarray' was not properly initialized: %S" target))
      ;; Kill all subprocesses: for cleanliness but also because their
      ;; sentinels and filters may refer to functions&vars that we're about
      ;; to undefine.
      (mapc #'delete-process (process-list))
      ;; Kill buffers except the initial ones.  Part of the reason is
      ;; "cleanliness" but part of the reason is also that those buffer's
      ;; local vars can refer to variables and functions which we're about
      ;; to undefine!
      (dolist (buf (buffer-list))
        (unless (member (buffer-name buf) futur--initial-buffer-list)
          (kill-buffer buf)))
      (pcase-let (;; (start-time (float-time))
                  (`(,_ ,old-target ,snapshot) (assq name snapshots)))
        (cond
         ((and snapshot (equal old-target target))
          (futur--obarray-revert snapshot)
          ;; (message "Time to reset-context %S: %.2f"
          ;;          snapshot (- (float-time) start-time))
          )
         (t
          (let ((nearest '())
                (target-len (length target))
                (score -1))
            (dolist (entry snapshots)
              (let* ((old-target (nth 1 entry))
                     (old-target-len (length old-target)))
                (when (and (< score old-target-len)
                           (<= old-target-len target-len)
                           (futur--list-prefix-p old-target target))
                  (setq score old-target-len)
                  (setq nearest entry))))
            (if (null nearest)
                (when snapshots (error "Internal error in futur--obarray: %S %S"
                                       target snapshots))
              (futur--obarray-revert (nth 2 nearest)))
            (let ((target-rest (nthcdr (length (nth 1 nearest)) target)))
              (if (and nearest (null target-rest))
                  ;; Just a new name for an existing obarray.
                  (setf (alist-get name snapshots) (cdr nearest))
                (dolist (cmd target-rest)
                  (pcase-exhaustive cmd
                    (`(funcall ,func . ,args) (apply func args))
                    ;; FIXME: Fallback on `.el' if `.elc' is missing, and
                    ;; load `.eln' if applicable.
                    ((pred stringp) (unless (assoc cmd load-history)
                                      (load cmd 'noerror 'nomessage)))
                    ((pred symbolp) (require cmd))))
                (setf (alist-get name snapshots)
                      (list target (futur--obarray-snapshot))))))
          ;; (message "Time to setup-context: %.2f"
          ;;          (- (float-time) start-time))
          ))))))

(provide 'futur-server)
;;; futur-server.el ends here

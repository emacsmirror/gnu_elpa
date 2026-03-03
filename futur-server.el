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

;;;; Base protocol

;; (require 'trace)
;; (trace-function 'futur--read-stdin)
;; (trace-function 'futur--print-stdout)

(defconst futur--elisp-impossible-string "\n# \"# "
  "String that will necessarily cause `read' to signal an error.")

(defun futur--read-stdin ()
  "Read a sexp from a single line on stdin."
  (unless noninteractive (error "futur--read-stdin works only in batch mode"))
  (read-from-minibuffer "" nil nil t))

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
    (princ futur--elisp-impossible-string t)
    (terpri t)))

;; (defun futur-server-call-in-context (ctxname ctx func &rest args)
;;   (futur--obarray ctxname ctx)
;;   (apply func args))

(defun futur-server ()
  ;; We don't need a cryptographically secure ID, but just something that's
  ;; *very* unlikely to occur by accident elsewhere and which `read' wouldn't
  ;; process without signaling an error.
  (let* ((sid (format " fes:%s "
                      (secure-hash 'sha1
                                   (format "%S:%S:%S"
                                           (random t) (current-time)
                                           (emacs-pid)))))
         (sid-sym (intern (string-trim sid))))
    ;; Initialize the cache of obarray snapshots.
    (futur-reset-context 'futur--server-internal nil)
    (futur--print-stdout :ready sid)
    (while t
      (let ((input (condition-case err (cons :read-success (futur--read-stdin))
                     (t err))))
        (pcase input
          ;; Check `sid-sym' for every request, since we may have just read
          ;; "successfully" the garbage that follows a failed read.
          (`(:read-success ,(pred (eq sid-sym)) ,rid ,func . ,args)
           ;; Confirm we read successfully so the client can
           ;; distinguish where problems come from.
           (futur--print-stdout `(:read-success ,rid) sid)
           (let ((result
                  (condition-case err
                      `(:funcall-success ,rid . ,(apply func args))
                    (t `(:funcall-error ,rid . ,err)))))
             (futur--print-stdout result sid)))
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
  (let ((snapshot (obarray-make)))
    (mapatoms
     (lambda (sym)
       (let ((fun (symbol-function sym))
             (plist (symbol-plist sym))
             (boundp (default-boundp sym)))
         (if (and (null fun) (null plist)
                  (or (keywordp sym) (not boundp)))
             nil
           (let ((ns (intern (symbol-name sym) snapshot)))
             (setf (symbol-function ns) fun)
             (setf (symbol-plist ns) plist)
             (when boundp
               (setf (default-value ns) (default-value sym))))))))
    snapshot))

(defun futur--obarray-revert (snapshot)
  "Revert `obarray' to the value it had when SNAPSHOT was taken."
  ;; We don't have `default-makunbound', so simulate it by
  ;; going to a dummy temp buffer.
  (unless snapshot (error "Can't use nil as obarray"))
  (with-temp-buffer
    ;; We map only over `obarray', which takes care of all the symbols
    ;; present in `obarray', some of which are also in `snapshot'.
    ;; Strictly speaking, we should also map over `snapshot' to handle
    ;; those symbols that are missing from `obarray', but since
    ;; `snapshot' holds a previous state of `obarray', such symbols
    ;; can occur only if someone used `unintern', which should hopefully
    ;; never happen in the `obarray'.
    (mapatoms
     (lambda (sym)
       (let ((ss (intern-soft (symbol-name sym) snapshot)))
         (if (null ss)
             (progn
               (setf (symbol-function sym) nil)
               (setf (symbol-plist sym) nil)
               (unless (keywordp sym) (makunbound sym)))
           (setf (symbol-function sym) (symbol-function ss))
           (setf (symbol-plist sym) (symbol-plist ss))
           ;; FIXME: Do we need to do something special for var-aliases?
           (ignore-error setting-constant
             (if (default-boundp ss)
                 (setf (default-value sym) (default-value ss))
               (when (default-boundp sym)
                 (unless (keywordp sym) (makunbound sym)))))))))))

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
- A file name that should be `load'ed.
- A feature that shoujd be `require'd.
- A function that should be called.
The elements are processed in order, starting from the state at startup.
NAME is used only for the purpose of overwriting a previous state from
the cache."
      (when (and target (null snapshots))
        (error "`futur--obarray' was not properly initialized: %S" target))
      (pcase-let ((`(,_ ,old-target ,snapshot) (assq name snapshots)))
        (cond
         ((and snapshot (equal old-target target))
          (futur--obarray-revert snapshot))
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
                    ((pred stringp) (unless (assoc cmd load-history)
                                      (load cmd 'noerror 'nomessage)))
                    ((pred symbolp) (require cmd))))
                (setf (alist-get name snapshots)
                      (list target (futur--obarray-snapshot))))))))))))

(provide 'futur-server)
;;; futur-server.el ends here

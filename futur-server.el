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
    (prin1 sexp t)
    (princ futur--elisp-impossible-string t)
    (terpri t)))

(defun futur-elisp-server ()
  ;; We don't need a cryptographically secure ID, but just something that's
  ;; *very* unlikely to occur by accident elsewhere and which `read' wouldn't
  ;; process without signaling an error.
  (let* ((sid (format " fes:%s "
                      (secure-hash 'sha1
                                   (format "%S:%S:%S"
                                           (random t) (current-time)
                                           (emacs-pid)))))
         (sid-sym (intern (string-trim sid))))
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
                                 

(provide 'futur-server)
;;; futur-server.el ends here

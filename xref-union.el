;;; xref-union.el --- Combine multiple Xref backends  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Free Software Foundation, Inc.

;; Author: Philip Kaludercic <philipk@posteo.net>
;; Maintainer: Philip Kaludercic <~pkal/public-inbox@lists.sr.ht>
;; URL: https://git.sr.ht/~pkal/xref-union/
;; Version: 0.1.1
;; Package-Requires: ((emacs "25.1"))

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

;; This package provides a way to combine multiple Xref source
;; (e.g. Etags and Eglot) and have the results all at once.

;; To enable, toggle the `xref-union-mode' minor mode.  If you want to
;; exclude certain modes, take a look at the user option
;; `xref-union-excluded-backends'.

;; You can also manually make use of `xref-union' by adding an object
;; of the form (union XREF-BACKEND-1 XREF-BACKEND-2 ...) to
;; `xref-backend-functions'

;;; Code:

(eval-when-compile (require 'subr-x))
(require 'xref)

(defgroup xref-union '()
  "Combine multiple Xref backends."
  :group 'xref)

(defcustom xref-union-excluded-backends #'ignore
  "Predicate to exclude backends in `xref-union-mode'.
The function is invoked with a single argument, the backend.  If
a non-nil value is returned, the backend will not be added to the
union backend, otherwise it will be."
  :type 'function)


;;;; Xref interface

(defun xref-union-same-p (l1 l2)
  "Check if the locations L1 and L2 are the same.
Same in this context means they reference the same object."
  (= (xref-location-marker (xref-item-location l1))
     (xref-location-marker (xref-item-location l2))))

(cl-defmethod xref-backend-identifier-at-point ((backends (head union)))
  "Collect the results of multiple Xref BACKENDS."
  (seq-uniq
   (cl-loop for backend in (cdr backends)
	    append (xref-backend-identifier-at-point backend))
   #'xref-union-same-p))

(cl-defmethod xref-backend-identifier-completion-table ((backends (head union)))
  "Collect the results of multiple Xref BACKENDS."
  (lambda (string pred _action)
    (cl-loop for backend in (cdr backends)
	     append (let ((b (xref-backend-identifier-at-point backend)))
                      (all-completions string b pred)))))

(cl-defmethod xref-backend-definitions ((backends (head union)) ident)
  "Collect the results of multiple Xref BACKENDS.
IDENT is specified in `xref-backend-definitions'."
  (seq-uniq
   (cl-loop for backend in (cdr backends)
	    append (xref-backend-definitions backend ident))
   #'xref-union-same-p))

(cl-defmethod xref-backend-references ((backends (head union)) ident)
  "Collect the results of multiple Xref BACKENDS.
IDENT is specified in `xref-backend-references'."
  (seq-uniq
   (cl-loop for backend in (cdr backends)
	    append (xref-backend-references backend ident))
   #'xref-union-same-p))

(cl-defmethod xref-backend-apropos ((backends (head union)) pattern)
  "Collect the results of multiple Xref BACKENDS.
PATTERN is specified in `xref-backend-apropos'."
  (seq-uniq
   (cl-loop for backend in (cdr backends)
	    append (xref-backend-apropos backend pattern))
   #'xref-union-same-p))


;;;; Minor mode

(defvar-local xref-union--current nil
  "Reference to the current union backend.")

(define-minor-mode xref-union-mode
  "Enable a Xref backend that combines all others."
  :global nil
  (when xref-union--current
    (remove-hook 'xref-backend-functions xref-union--current))
  (when xref-union-mode
    (let (backends)
      ;; Collect all (local and global) functions in
      ;; `xref-backend-functions' into a local list.
      (run-hook-wrapped
       'xref-backend-functions
       (lambda (b)
         (setq b (funcall b))
         (when (and b (funcall xref-union-excluded-backends b))
           (push b backends))
         nil))
      (setq xref-union--current (cons 'union backends))
      (add-hook 'xref-backend-functions xref-union--current))))

;; LocalWords: backend backends

(provide 'xref-union)
;;; xref-union.el ends here

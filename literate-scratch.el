;;; literate-scratch.el --- Lisp Interaction w/ text paragraphs  -*- lexical-binding: t -*-

;; Copyright (C) 2023-2024  Free Software Foundation, Inc.

;; Author: Sean Whitton <spwhitton@spwhitton.name>
;; Maintainer: Sean Whitton <spwhitton@spwhitton.name>
;; Package-Requires: ((emacs "29.1"))
;; Version: 1.0
;; URL: https://git.spwhitton.name/dotfiles/tree/.emacs.d/site-lisp/literate-scratch.el
;; Keywords: lisp text

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Variant Lisp Interaction mode for easier interleaving of paragraphs of
;; plain text with Lisp code.  This means you can have
;;
;;     ;; This buffer is for text that is not saved ...
;;     ;; To create a file, visit it with C-x C-f and ...
;;
;;     (here is some Lisp)
;;
;;     Here is a plain text paragraph )( including some unmatched parentheses.
;;     Uh oh!  Paredit won't like that.
;;
;;     (here is some more Lisp)
;;
;; but (e.g.) Paredit won't complain about the unmatched parentheses.
;; Indeed, the whole plain text paragraph is font-locked as a comment.
;;
;; If you use Paredit but want to be able to use *scratch* for both Lisp
;; interaction and blocks of plain text, then this mode is for you.
;; Also compatible with the `orgalist-mode' and `orgtbl-mode' minor modes.
;;
;; To enable this mode after installing this file, simply customise
;; `initial-major-mode' to `literate-scratch-mode'.

;;; News:

;; Ver 1.0 2024/06/21 Sean Whitton
;;     Initial release.
;;     Thanks to Philip Kaludercic for review.

;;; Code:

(defun literate-scratch--extend (start end)
  (save-excursion
    (let ((res1
	   (and (goto-char start)
		(not (looking-at paragraph-separate))
		(and-let* ((new (car (bounds-of-thing-at-point 'paragraph))))
		  (and (< new start)
		       (setq start new)))))
	  (res2
	   (and (goto-char end)
		(not (looking-at paragraph-separate))
		(and-let* ((new (cdr (bounds-of-thing-at-point 'paragraph))))
		  (and (> new end)
		       (setq end new))))))
      (and (or res1 res2)
	   (cons start end)))))

(defun literate-scratch--propertize (start end)
  (goto-char start)
  (let ((start (1- start)))
    (catch 'finish
      (while t
	(when-let* ((comment-start (nth 8 (syntax-ppss))))
	  (put-text-property (1- (point)) (point) 'syntax-table
			     (eval-when-compile
			       (string-to-syntax "!")))
	  (put-text-property comment-start (point) 'syntax-multiline t))
	(forward-paragraph 1)
	(backward-paragraph 1)
	(unless (> end (point) start)
	  (throw 'finish nil))
	(setq start (point))
	(unless
	    (save-excursion
	      (catch 'done
		(while t
		  ;; Examine the syntax of the paragraph's first char.
		  ;; If it's whitespace, we need to check the previous
		  ;; paragraph, to handle multiple paragraphs within a defun.
		  (let ((syn
			 (char-syntax
			  (char-after
			   ;; (1+ point) unless at end-of-buf or on first line
			   ;; of a paragraph beginning right at beg-of-buf.
			   (if (looking-at "\\`[[:space:]]*[^[:space:]\n]")
			       (point)
			     (1+ (point)))))))
		    (cond ((bobp) (throw 'done (memq syn '(?\( ?<))))
			  ((memq syn '(?\( ?<)) (throw 'done t))
			  ((not (eq syn ?\s)) (throw 'done nil))))
		  (backward-paragraph 1))))
	  (put-text-property (point) (1+ (point)) 'syntax-table
			     (eval-when-compile
			       (string-to-syntax "!"))))
	(forward-paragraph 1))))
  ;; Now also call the usual `syntax-propertize-function' for this mode.
  (elisp-mode-syntax-propertize start end))

;;;###autoload
(define-derived-mode literate-scratch-mode lisp-interaction-mode
  "Lisp Interaction"
  "Variant `lisp-interaction-mode' designed for the *scratch* buffer.

Paragraphs that don't start with `(' or `;' are treated as block comments.
This makes it easier to interleave paragraphs of plain text with Lisp.

You can enable this mode by customizing the variable `initial-major-mode'
to `literate-scratch-mode'."
  (add-hook 'syntax-propertize-extend-region-functions
	    #'syntax-propertize-multiline t t)
  (add-hook 'syntax-propertize-extend-region-functions
	    #'literate-scratch--extend t t)
  (setq-local syntax-propertize-function #'literate-scratch--propertize))

(provide 'literate-scratch)

;;; literate-scratch.el ends here

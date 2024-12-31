;;; greader-translate.el --- translation mode for greader -*- lexical-binding: t; -*-
;; Copyright (C) 2017-2025  Free Software Foundation, Inc.
;; Filename: greader-translate.el
;; Description: On the fly translation for greader.
;; Author: Michelangelo Rodriguez <michelangelo.rodriguez@gmail.com>
;; Created: Mar Giu 11 04:37:23 2024 (+0200)
;; URL: https://gitlab.com/michelangelo-rodriguez/greader
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; With greader-translate-mode it is possible to translate "on the
;; fly" the buffer you are reading.
;; In order to use it, the package `google-translate' must be installed.
;; Also, you have to set the variable `greader-translate-lang-src'
;; properly.
;; Properly means that it must be understood by `google-translate'
;; package.
;; If thinks are set correctly, in order to have the translation you
;; simply must to enable `greader-translate-mode', start your reading
;; with `greader-read', and enjoy.
;; The target language is taken from the function
;; `greader-set-language
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:
(declare-function google-translate-translate nil)
(declare-function greader-get-language nil)
(defgroup greader-translate nil
  "On the fly translation mode for greader."
  :group 'greader)
(defvar greader-translate-lang-src nil
  "language to translate from.
If nil, it will use same language as `lingva-src'.")

(defcustom greader-translate-timeout 30
  "Timeout for `greader-translate'.
After this timeout, `greader-translate' will generate an error."
  :type 'integer)

(defvar greader-translate-function 'greader-translate-with-google
  "Function to use for text translation.
`greader-translate-with-google is provided.
The function should receive the text to translate, and return the
translated text on success, or the original text on failure.")

(defun greader-translate-with-google (sentence)
  "Translate text SENTENCE using `google-translate' library."
  (require 'google-translate)
  (require 'google-translate-core-ui)
  (if (not (string-blank-p sentence))
      (progn
	(unless greader-translate-lang-src
	  (error "Please configure `greader-translate-lang-src'
first"))
	(with-timeout (greader-translate-timeout
		       (greader-translate-mode -1)
		       (setq sentence nil)
		       (error "Error while translating. Translation
mode disabled"))
	  (let ((inhibit-message t)
		(ring-length (length kill-ring)))
	    (google-translate-translate greader-translate-lang-src
					(greader-get-language) sentence
					'kill-ring)
	    (while (equal ring-length (length kill-ring))
	      (sit-for 0.1)))
	  (setq sentence (pop kill-ring))))
    sentence))

(defun greader-translate (sentence)
  "Translate the text in SENTENCE and return it translated or nil in
case of error."
  (funcall greader-translate-function sentence))

;;;###autoload
(define-minor-mode greader-translate-mode
  nil
  :lighter " gr-transl"
  (if greader-translate-mode
      (progn
	(add-hook 'greader-after-get-sentence-functions
		  #'greader-translate 0 t))
    (remove-hook 'greader-after-get-sentence-functions
		 #'greader-translate t)))

(provide 'greader-translate)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; greader-translate.el ends here

;;; gnosis-monkeytype.el --- Typing Module for Gnosis  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2026  Free Software Foundation, Inc.

;; Author: Thanos Apollo <public@thanosapollo.org>
;; Keywords: extensions
;; URL: https://thanosapollo.org/projects/gnosis

;; Version: 0.0.1

;; Package-Requires: ((emacs "27.2") (compat "29.1.4.2"))

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

;; Monketype module for gnosis.

;;; Code:

(require 'gnosis-utils)

(defvar gnosis-script-input-method-alist)


(defface gnosis-monkeytype-face-dimmed
  '((((class color) (background light)) :foreground "grey50")
    (((class color) (background  dark)) :foreground "grey50"))
  "Face for untyped text."
  :group 'gnosis)

(defface gnosis-monkeytype-face-correct
  '((t :inherit success))
  "Face for correctly typed text."
  :group 'gnosis)

(defface gnosis-monkeytype-face-wrong
  '((t :inherit error))
  "Face for incorrectly typed text."
  :group 'gnosis)

(defcustom gnosis-monkeytype-themata '("basic" "cloze" "mc-cloze" "mcq")
  "Thema Types to monketype."
  :type '(repeat string)
  :group 'gnosis)

(defcustom gnosis-monkeytype-enable t
  "Enable gnosis monkeytyping for wrong answers."
  :type 'boolean
  :group 'gnosis)

(defvar gnosis-monkeytype-buffer-name "*gnosis-monkeytype*")

(defvar gnosis-monkeytype-string nil)

(defvar gnosis-monkeytype--start-time nil
  "Time of first keystroke, or nil if not yet started.")

(defun gnosis-monkeytype--format-text (text)
  "Format TEXT using a temp buffer."
  (with-temp-buffer
    (insert (propertize text 'face 'gnosis-monkeytype-face-dimmed))
    (delete-trailing-whitespace)
    (buffer-string)))

(defun gnosis-monkeytype--handler (_beg end _len)
  "Handler buffer change at END."
  (when (and (eq (current-buffer) (get-buffer gnosis-monkeytype-buffer-name))
	     (eq this-command 'self-insert-command))
    (unless gnosis-monkeytype--start-time
      (setq gnosis-monkeytype--start-time (current-time)))
    (let ((correct-char (char-after end))
          (typed-char (char-before end)))
      ;; Debugging ;;
      ;; (when (and correct-char typed-char)
      ;; 	(message "Comparing: %c with %c POS: %d" typed-char correct-char end))
      (if (and correct-char typed-char (char-equal correct-char typed-char))
          (progn
            (delete-char -1)
            (put-text-property (1- end) end 'face 'gnosis-monkeytype-face-correct)
            (goto-char end)
            ;; Check if complete
            (when (= end (1+ (length gnosis-monkeytype-string)))
	      (gnosis-monkeytype--calculate-wpm
	       gnosis-monkeytype-string gnosis-monkeytype--start-time)
	      (kill-buffer (current-buffer))
              (exit-recursive-edit))
	    ;; Forward line when at the end
	    (and (eolp) (forward-line 1)))
        (when (and correct-char typed-char)
          (delete-char -1)
          (goto-char (1- end)))))))

(defun gnosis-monkeytype-exit ()
  "Exit monkeytyping."
  (interactive nil gnosis-monkeytype-mode)
  (remove-hook 'after-change-functions #'gnosis-monkeytype--handler t)
  (kill-buffer (current-buffer))
  (ignore-errors (throw 'monkeytype-loop t))
  (exit-recursive-edit))

(defun gnosis-monkeytype--calculate-wpm (text start-time)
  "Calculate and display WPM based on TEXT and START-TIME.
A \"word\" is 5 characters (standard typing test definition)."
  (let* ((elapsed-minutes (/ (float-time (time-since start-time)) 60.0))
         (wpm (/ (/ (length text) 5.0) elapsed-minutes)))
    (message "WPM: %s" (propertize (format "%.0f" wpm) 'face 'success))))

(defun gnosis-monkeytype (text &optional mistakes)
  "Monkeytype TEXT.

Optionally, highlight MISTAKES."
  (with-current-buffer (get-buffer-create gnosis-monkeytype-buffer-name)
    (erase-buffer)
    (let ((text-formatted (gnosis-utils-highlight-words
			   text mistakes 'gnosis-monkeytype-face-wrong
			   'gnosis-monkeytype-face-dimmed)))
      (gnosis-monkeytype-mode)
      (insert text-formatted)
      (fill-paragraph)
      (setq gnosis-monkeytype-string (buffer-string))
      (setq gnosis-monkeytype--start-time nil)
      (switch-to-buffer (get-buffer-create gnosis-monkeytype-buffer-name))
      (goto-char (point-min))
      (add-hook 'after-change-functions #'gnosis-monkeytype--handler nil t)
      (let ((method (alist-get (gnosis-utils-detect-script text)
			       gnosis-script-input-method-alist)))
	(when method (activate-input-method method))
	(unwind-protect
	    (recursive-edit)
	  (when method (deactivate-input-method)))))))

(defun gnosis-monkeytype-region (start end)
  "Monkeytype the selected region from START to END."
  (interactive "r")
  (gnosis-monkeytype (buffer-substring-no-properties start end))
  (deactivate-mark))

(defun gnosis-monkeytype--ignore-del ()
  "Ignore DEL key in monkeytype buffer."
  (interactive nil gnosis-monkeytype-mode)
  (message "DEL key is disabled."))

(defvar-keymap gnosis-monkeytype-mode-map
  :doc "gnosis-monkeytype mode map"
  :parent text-mode-map
  "DEL" #'gnosis-monkeytype--ignore-del
  "RET" #'forward-line
  "C-c C-k" #'gnosis-monkeytype-exit)

(define-derived-mode gnosis-monkeytype-mode text-mode "Gnosis Monkeytype"
  "Gnosis Monkeytype Mode."
  :interactive nil
  :lighter " gnosis-monkeytype-mode"
  :keymap gnosis-monkeytype-mode-map
  (setq-local post-self-insert-hook nil)
  (setq-local header-line-format
	      (substitute-command-keys
	       " Monkeytype. \\[gnosis-monkeytype-exit] to exit.")))

(provide 'gnosis-monkeytype)
;;; gnosis-monkeytype.el ends here

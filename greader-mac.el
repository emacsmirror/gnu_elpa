;;; greader-mac.el --- a back-end for Mac-Os tts. -*- lexical-binding: t; -*-
;; Copyright (C) 2017-2025  Free Software Foundation, Inc.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see
;; <http://www.gnu.org/licenses/>.

;;; Code:
(require 'subr-x)
(defgroup greader-mac
  nil
  "Back-end of mac for greader."
  :group 'greader)

(defcustom greader-mac-voice nil
  "Set the desired voice for the tts `say’.
nil means to use the system voice."
  :tag "mac tts voice"
  :type '(choice (const :tag "system voice" nil)
		 (string :tag "Specify")))

(defcustom greader-mac-rate 200
  "Rate of tts expressed in words per minute."
  :tag "Mac tts speech rate"
  :type 'integer)

(defcustom greader-mac-executable-name "say"
  "Name of the program that actually produces the voice."  
  :tag "mac tts command"
  :type 'string)

(defvar greader-mac-end-of-sentence-regexp "[.?!,][«»\"]?[\n ]"
  "A regexp that matches the end of the string to send to the tts.")

(defun greader-mac-set-rate (&optional rate)
  "Return a string suitable for setting mac RATE."
  (if (not rate)
      (concat "-r" (number-to-string greader-mac-rate))
    (progn
      (setq-local greader-mac-rate rate)
      (concat "-r" (number-to-string rate)))))

(defun greader-mac-set-voice (voice)
  "Set specified VOICE for `say'.
When called interactively, this function reads a string from the minibuffer
providing completion."
  (interactive
   (list (read-string "Voice: " nil nil (greader--mac-get-voices))))
  (when voice
    (setq-local greader-mac-voice
                (if (string-equal "system" voice) nil voice)))
  (when greader-mac-voice (concat "-v" greader-mac-voice)))

;;;###autoload
(defun greader-mac (command &optional arg)
  "Back-end main function of greader-mac.
COMMAND must be a string suitable for `make-process'."
  (pcase command
    ;; ('get-text
    ;;  (greader-mac-get-sentence))
    ;; ('next-text
    ;;  (greader-mac-forward-sentence))
    ('executable
     greader-mac-executable-name)
    ('lang
     (greader-mac-set-voice arg))
    ('set-voice
     (call-interactively #'greader-mac-set-voice))
    ('rate
     (cond
      ((equal arg 'value)
       greader-mac-rate)
      (t
       (greader-mac-set-rate arg))))
    ('punctuation
     nil)
    ('get-rate
     greader-mac-rate)
    (_
     'not-implemented)))
(put 'greader-mac 'greader-backend-name "greader-mac")

(defun greader-mac-get-sentence ()
  (let ((sentence-start (point)))
    (save-excursion
      (greader-mac-forward-sentence)
      (if (> (point) sentence-start)
	  (string-trim (buffer-substring-no-properties sentence-start (point)) "[ \t\n\r]+")
	nil))))

(defun greader-mac-forward-sentence ()
  (re-search-forward greader-mac-end-of-sentence-regexp nil 'move))

(defun greader--mac-get-voices ()
  "Return a list which contains all voices suitable for this backend."
  (with-temp-buffer
    (call-process "say" nil t nil "-v" "?")
    (goto-char (point-min))
    (let ((lines (list "system")))
      (while (not (eobp))
	(let ((mymarker (make-marker)))
	  (search-forward "_")
	  (backward-word)
	  (re-search-backward "[^ ]")
	  (forward-char)
	  (set-marker mymarker (point))
	  (end-of-line)
	  (delete-region mymarker (point)))	  
	(push (string-chop-newline (thing-at-point 'line)) lines)
	(forward-line))
      (reverse lines))))

(provide 'greader-mac)

;;; greader-espeak.el --- greader back-end for espeak. -*- lexical-binding: t; -*-
;; Copyright (C) 2017-2026  Free Software Foundation, Inc.

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

;;; commentary:

;;; code:
(require 'ring)
(defgroup greader-espeak
  nil
  "Back-end of espeak for greader."
  :group 'greader)

(defcustom greader-espeak-language "en"
  "Specifies the language of this back-end.
For a comprehensive list of languages and voices available in espeak
type in a terminal: espeak --list-languages"
  :tag "greader espeak language"
  :type 'string)

(defcustom greader-espeak-rate 200
  "Specifies the rate os speech in words per minute."
  :tag "greader espeak rate"
  :type 'integer)

(defcustom greader-espeak-executable-name "espeak"
  "File name of espeak executable.
this variable determines authomatically if espeak is present in your
PATH
environment, then if this variable is nil,
it means that you must first install espeak."
  :tag "espeak executable"
  :type 'string)

(defcustom greader-espeak-punctuation nil
  "Espeak punctuation switch."
  :tag "espeak punctuation"
  :type 'boolean)

(defun greader-espeak-set-rate (&optional rate)
  "Return a string suitable for setting espeak RATE."
  (if (not rate)
      (concat "-s" (number-to-string greader-espeak-rate))
    (progn
      (setq-local greader-espeak-rate rate)
      (concat "-s" (number-to-string rate)))))

(defun greader-espeak-set-language (&optional lang)
  "Return a suitable string for espeak language.
LANG must be recognized by espeak or espeak-ng."
  (if (not lang)
      (concat "-v" greader-espeak-language)
    (progn
      (setq-local greader-espeak-language lang)
      (concat "-v" lang))))

(defun greader--espeak-list-voices ()
  "Return a list of language codes available in espeak."
  (with-temp-buffer
    (call-process greader-espeak-executable-name nil t nil "--list-voices")
    (goto-char (point-min))
    (forward-line 1)                    ; skip header line
    (let (voices)
      (while (not (eobp))
        (when (looking-at "[ \t]*[0-9]+[ \t]+\\([[:alnum:]_-]+\\)")
          (push (match-string 1) voices))
        (forward-line 1))
      (delete-dups (nreverse voices)))))

(defvar-local  greader-espeak--punctuation-ring (make-ring 2))
(ring-insert greader-espeak--punctuation-ring "yes")
(ring-insert greader-espeak--punctuation-ring "no")
(defvar-local greader-espeak--ring-item (if greader-espeak-punctuation "yes"
				    "no"))

;;;###autoload
(defun greader-espeak (command &optional arg)
  "Back-end main function for espeak.
COMMAND must be a string suitable for `make-process'.
ARG is applied depending on the command."
  (pcase command
    ('executable
     greader-espeak-executable-name)
    ('lang
     (greader-espeak-set-language arg))
    ('rate
     (cond
      ((equal arg 'value)
       greader-espeak-rate)
      (t
       (greader-espeak-set-rate arg))))
    ('punctuation
     (pcase arg
       ((or 'toggle 'yes 'no)
	(setq greader-espeak--ring-item (ring-next
					 greader-espeak--punctuation-ring
					 greader-espeak--ring-item))
	(pcase greader-espeak--ring-item
	  ("yes"
	   (setq-local greader-espeak-punctuation t)
	   (message "Punctuation enabled in current buffer.")
	   "--punct")
	  ("no"
	   (setq-local greader-espeak-punctuation nil)
	   (message "Punctuation disabled in current buffer.")
	   nil)))
       ('nil
	(if greader-espeak-punctuation
	    "--punct"
	  nil))))
    ('set-voice
     (completing-read "Language: " (greader--espeak-list-voices) nil t))
    ('save-voice
     (customize-save-variable 'greader-espeak-language arg))
    ('get-language
     greader-espeak-language)
    ('get-rate
     greader-espeak-rate)
    ('audio-write
     (let ((text (car arg))
	   (filename (cadr arg)))
       (call-process greader-espeak-executable-name
		     nil "*espeak-output*" nil
		     (concat "-s" (number-to-string greader-espeak-rate))
		     (concat "-v" greader-espeak-language)
		     (concat "-w" filename)
		     text)))
    (_
     'not-implemented)))
(put 'greader-espeak 'greader-backend-name "greader-espeak")

(provide 'greader-espeak)
;;; greader-espeak.el ends here

;;; greader-piper.el --- greader back-end for piper synthesizer. -*- lexical-binding: t; -*-
;; Copyright (C) 2017-2024  Free Software Foundation, Inc.

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
;; This is the back-end for the piper synthesizer.
;; It uses a shell script to call piper.

;;; code:
(require 'package)
(defgroup greader-piper
  nil
  "piper back-end."
  :group 'greader)

(defcustom greader-piper-script-path
  (find-library-name "greader")
  "Piper script path."
  :type 'string)

(defcustom greader-piper-script-url
  "https://gitlab.com/michelangelo-rodriguez/greader/-/raw/master/piper.sh"
  "Url of the script `piper.sh'."
  :type 'string)
(defun greader-piper-find-script ()
  "Check if the piper script is really present.
If the script is not present, propose to download it from gitlab.
if the script is present or downloaded, then return the path.
If the script is nor present neither downloaded, then generate an
Error."
  (if (file-exists-p greader-piper-script-path)
      greader-piper-script-path
    (let* ((default-directory greader-piper-script-path)
	   (answer (yes-or-no-p "Do you want to download the script
  \"piper.sh\" from gitlab?")))
      (if answer
	  (progn
	    (setq answer (call-process "curl" nil "*piper-script download*"
				       nil greader-piper-script-url))
	    (unless (file-exists-p greader-piper-script-path)
	      (Error "Error while downloading %s\nPlease try later or
open an issue" greader-piper-script-url)))
	nil))))

;;;###autoload
(defun greader-piper (command &optional arg)
  "Entry point for greader-piper."
  (pcase command
    ('executable
     (greader-piper-find-script))
    (_
     'not-implemented)
    ))
(put 'greader-piper 'greader-backend-name "greader-piper")

(provide 'greader-piper)

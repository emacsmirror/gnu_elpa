;; greader-dict.el - dictionary module for greader. -*- lexical-binding: t; -*-
;; 
;; Filename: greader-dict.el
;; Description: 
;; Author: Michelangelo Rodriguez
;; Maintainer: 
;; Created: Lun Gen  8 09:52:58 2024 (+0100)
;; Version: 
;; Package-Requires: ()
;; Last-Updated: 
;;           By: 
;;     Update #: 0
;; URL: 
;; Doc URL: 
;; Keywords: 
;; Compatibility: 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; dictionary module for greader.
;; This module gives greader the ability to define different wais Of
;; pronounce a given sequence of characters.

;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change Log:
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; Copyright (C) 2023, 2024  Free Software Foundation, Inc.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:
(require 'greader)
(defvar greader-dictionary (make-hash-table :test 'equal))
(defvar greader-dict-match-indicator "%\*"
  "Regexp that will be used for match delimiter.")

;; The following two functions deal, respectively, with
;; replace a dictionary item with the value specified in
;; `greader-dictionari' and its possible variants.
;; The `greader-dict-substitute-match' function takes care of substitution
;; an item even within a word, a sort of partial substitution.
;; The `greader-dict-substitute-word' function takes care of that instead
;; replace a dictionary item only if the sequence matches
;; replace is surrounded by one or more blank class characters.
;; This will allow the user to specify whether a given rule
;; pronunciation in the dictionary should be applied more literally,
;; (for example, a pronunciation rule can be defined such that if a
;; word contains the sequence "ez" it is replaced with the
;; sequence "es", for which, for example, "Rodriguez" would be replaced
;; with "Rodrigues").
(defun greader-dict-substitute-match (match)
  "Replace MATCH with the matching value in `greader-dictionary."
  (let ((normalized-match (string-remove-suffix
			   greader-dict-match-indicator match)))
    (save-excursion
      (goto-char (point-min))
      (while (search-forward normalized-match nil t)
	(replace-match (gethash match greader-dictionary))))))

(defun greader-dict-substitute-word (match)
  "substitute match only if it constitutes an entire word."
  (save-excursion
    (goto-char (point-min))
    (let ((word (concat match "\\W")))
      (while (re-search-forward word nil t)
	(setq word (match-string 0))
	(let ((replacement (concat (gethash match
					    greader-dictionary)
				   (char-to-string (aref word (1- (length word)))))))
	  (replace-match replacement))))))

;; This function adds to the `greader-dictionary' variable the
;; key/value pair that you pass as arguments.
(defcustom greader-dict-save-after-time 30
  "Amount of idleness to wait before saving dictionary data.
A value of 0 indicates saving immediately."
  :type 'number)
(defvar greader-dict--timer nil)
(defun greader-dict-add (word replacement)
  "Add the WORD REPLACEMent pair to `greader-dictionary'.
If you want to add a partial replacement, you should
add `\*'to the end of WORD string parameter."
  (puthash (downcase word) replacement greader-dictionary)
  (cond
   ((> greader-dict-save-after-time 0)
    (when (timerp greader-dict--timer)
      (cancel-timer greader-dict--timer))
    (run-with-idle-timer greader-dict-save-after-time nil #'greader-dict-write-file))
   ((= greader-dict-save-after-time 0)
    (greader-dict-write-file))
   (t
    nil)))

;; This function removes the association indicated by the key argument.
(defun greader-dict-remove (key)
  "Remove the association specified by KEY from the variable
`greader-dictionary'."
  (remhash key greader-dictionary))

(defun greader-dict-item-type (key)
  "Return the type of KEY.
Possible return values are:
`word' for a wole word,
`match' for partial matches.
There may be more in the future.
Return nil if KEY is not present in `greader-dictionary'."
  (cond
   ((not key)
    nil)
   ((not (string-suffix-p greader-dict-match-indicator key))
    'word)
   ((string-suffix-p greader-dict-match-indicator key)
    'match)
   (t nil)))

(defun greader-dict--get-key-from-word (word)
  "Return key related to WORD, nil otherwise."
  (let ((key nil))
    (maphash (lambda (k v)
	       (when (string-search (string-remove-suffix
				     greader-dict-match-indicator k) word)
		 (setq key k))) greader-dictionary)
    key))

;; This function checks that, in the string you pass to it, there are
;; effectively words to be replaced. If so, use apis
;; previously defined to adequately replace the words that
;; could need it.
;; This is the function to add to
;; `greader-after-get-sentence-functions'.
(defun greader-dict-check-and-replace (text)
  "Return the TEXT passed to it, eventually modified according to
`greader-dictionary' and variants."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (re-search-forward "\\w" nil t)
    (while (not (eobp))
      (let*
	  ((key (greader-dict--get-key-from-word (downcase
						  (thing-at-point 'word))))
	   (modified-word (concat (downcase (thing-at-point 'word)) greader-dict-match-indicator)))
	(cond
	 ((equal (greader-dict-item-type key) 'word)
	  (greader-dict-substitute-word (string-remove-suffix
					 greader-dict-match-indicator key)))
	 ((equal (greader-dict-item-type key) 'match)
	  (greader-dict-substitute-match key))
	 ((not (greader-dict-item-type key))
	  nil)))
      (forward-word))
    (buffer-string)))

;; This function saves the contents of the hash table.
(defcustom greader-dict-directory (concat user-emacs-directory
					  ".greader-dict/"
					  (greader-get-language) "/")
  "The directory containing greader-dict files."
  :type 'directory)
(defcustom greader-dict-filename "greader-dict.global"
  "File name where dictionary definitions are stored."
  :type 'string)

(defun greader-dict-write-file ()
  "Save greader-dictionary stored in `greader-dict-filename'."
  (unless (file-exists-p greader-dict-directory)
    (make-directory greader-dict-directory t))
  (with-temp-buffer
    (maphash
     (lambda (k v)
       (insert (concat k "=" v "\n"))) greader-dictionary)
    (write-file (concat greader-dict-directory
			greader-dict-filename))))

(defun greader-dict-read-from-dict-file ()
  "populate `greader-dictionary' with the contents of
`greader-dict-filename'."
  (when (file-exists-p (concat greader-dict-directory
			       greader-dict-filename))
    (with-temp-buffer
      (insert-file-contents (concat greader-dict-directory
				    greader-dict-filename))
      (when-let ((lines (string-lines (buffer-string))))
	(dolist (line lines)
	  (setq line (split-string line "="))
	  (greader-dict-add (car line) (car (cdr line))))))))

;; Command for saving interactively dictionary data.
(defun greader-dict-save ()
  "Save dictionary data.
You should use this command when you want to save your dictionary and
`greader-dict-save-after-time' is set to a negative number.
Otherwise, data saving is done automatically when you add a definition
to the dictionary."
  (interactive)
  (greader-dict-write-file))

;; This command Adds a definition to `greader-dictionary'.
;; If the region is active and it does not constitute more than one word,
;; the command will propose the selected word as the original word to
;; substitute.
;; The selected word will be added to `greader-dictionary' as
;; "match", then the definition thus obtained can be applied to
;; any character sequence that includes it.
;; However, if the region is not active, this function will try to
;; determine the word to add through the function
;; `thing-at-point'. In case this function returns a word,
;; it will be used to propose it as the original word to be replaced.
;; In this last case, the word will be added to
;; `greader-dictionary' as "word", so it must constitute itself
;; a word to be replaced.
(defun greader-dict-add-entry ()
  (interactive)
  (let (key value)
    (cond
     ((region-active-p)
      (when (= (count-words(region-beginning) (region-end)) 1)
	(setq key (concat (read-string "Original word to substitute:" nil nil
				       (thing-at-point 'word)) greader-dict-match-indicator))
	(setq value (read-string (concat "substitute match " key
					 "with:")))
	(greader-dict-add key value)))
     (t
      (when-let ((default-word (thing-at-point 'word)))
	(setq key (read-string "Original word to substitute:" nil nil
			       default-word))
	(setq value (read-string (concat "substitute word " key
					 "with:")))
	(greader-dict-add key value))))))

;; greader-dict-mode.
(defvar-keymap greader-dict-mode-map
  :doc "keymap for `greader-dict-mode'."
  "C-r d a" #'greader-dict-add-entry
  "C-r d s" #'greader-dict-save)
(defun greader-dict--replace-wrapper (text)
  "Function to add to `greader-after-get-sentence-functions'.
It simply calls `greader-dict-check-and-replace' with TEXT as its
argument, only if `greader-dict-mode' is enabled."
  (when greader-dict-mode
    (greader-dict-check-and-replace text)))
;;;###autoload
(define-minor-mode greader-dict-mode
  "Dictionary module for greader.
With this mode it is possible to instruct greader to pronounce in an 
alternative way the words that the tts mispronounces in a given language.
There are two types of definitions understood by greader-dict-mode:
\"word definitions\" are those that must be surrounded by
Non-constituent word characters;
\"Match definitions\" are those that can be replaced regardless of
surrounding characters.
The definition type is determined when you add a new definition:
If you use the region to mark a word, you can select a partial word or
the entire word, and greader-dict-mode will understand that you want
to add a match definition.
If instead you add simply the word under the point, it will be added
as a word definition."
  :lighter " gr-dictionary"
  (cond
   (greader-dict-mode
    (when (hash-table-empty-p greader-dictionary)
      (greader-dict-read-from-dict-file))
    (add-hook 'greader-after-get-sentence-functions
	      #'greader-dict--replace-wrapper 1))
   (t
    (remove-hook 'greader-after-get-sentence-functions
		 #'greader-dict--replace-wrapper))))

(provide 'greader-dict)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; greader-dict.el ends here

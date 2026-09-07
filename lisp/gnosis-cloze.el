;;; gnosis-cloze.el --- Cloze manipulation for gnosis  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2026  Free Software Foundation, Inc.

;; Author: Thanos Apollo <public@thanosapollo.org>
;; Keywords: extensions

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

;; Pure cloze string manipulation: creation, extraction, hints,
;; tag removal.  No database dependency.

;;; Code:

(require 'cl-lib)
(require 'gnosis-utils)

(defvar gnosis-face-cloze)
(defvar gnosis-face-false)
(defvar gnosis-face-unanswered)

(defvar gnosis-cloze-string "(...)")

(defun gnosis-cloze--replace (str clozes cloze-string &optional case-fold)
  "Replace the first occurrence of each of CLOZES in STR with CLOZE-STRING.
Process CLOZES in order against the preceding result, trimming enclosing
quotes and retaining the cloze's leading and trailing whitespace pattern.
Use standard syntax and case tables; CASE-FOLD enables case-insensitive
matching.  Preserve text properties outside replaced spans, and give the
mask `gnosis-face-cloze'.  Return a new string without changing the inputs,
rendering Org, or opening a buffer."
  (let ((case-fold-search case-fold)
        (search-spaces-regexp nil))
    (with-syntax-table (standard-syntax-table)
      (with-case-table (standard-case-table)
        (save-match-data
          (cl-reduce
           (lambda (text cloze)
             (let* ((cloze-text (gnosis-utils-trim-quotes cloze))
                    (replacement
                     (concat (and (string-match "^\\s-+" cloze-text)
                                  (match-string 0 cloze-text))
                             (propertize cloze-string 'face 'gnosis-face-cloze)
                             (and (string-match "\\s-+$" cloze-text)
                                  (match-string 0 cloze-text)))))
               (if (string-match (regexp-quote cloze-text) text)
                   (replace-match replacement t t text)
                 text)))
           clozes :initial-value (copy-sequence str)))))))

(defun gnosis-cloze-add-hints (str hints &optional cloze-string)
  "Replace CLOZE-STRING in STR with HINTS, skipping empty hints."
  (cl-assert (listp hints) nil "Hints must be a list.")
  (let ((cloze-string (or cloze-string gnosis-cloze-string)))
    (with-temp-buffer
      (insert str)
      (goto-char (point-min))
      (cl-loop for hint in hints
               while (search-forward cloze-string nil t)
               do
	       (when (and hint (not (string-empty-p hint)) (not (string= hint "nil"))
			  (not (string= "\"\"" hint))
			  (search-backward cloze-string nil t))
                 (replace-match (propertize (format "(%s)" hint)
					    'face 'gnosis-face-cloze))
                 (goto-char (match-end 0)))) ; Move point to end of match
      (buffer-string))))

(defun gnosis-cloze-mark-false (str answers)
  "Mark contents of STR as false for ANSWERS.

First item of answers will be marked as false, while the rest unanswered."
  (let* ((false (car answers))
	 (unanswered (cdr answers))
         (str-with-false (and answers
			      (gnosis-utils-highlight-words str (list false)
							    'gnosis-face-false)))
	 final)
    (if unanswered
	(setq final (gnosis-utils-highlight-words str-with-false
						  (if (listp unanswered) unanswered
						    (list unanswered))
						  'gnosis-face-unanswered))
      (setq final (or str-with-false str)))
    final))

(defun gnosis-cloze-check (sentence clozes)
  "Return t if all CLOZES are found in SENTENCE."
  (cl-every (lambda (cloze)
              (string-match-p
               (regexp-quote
		(gnosis-utils-trim-quotes cloze))
               sentence))
            clozes))

;; TODO: use a better name to indicate that it also removes hints from STRING.
(defun gnosis-cloze-remove-tags (string)
  "Replace cloze tags and hints in STRING.

Works with both single (:), double colons (::), single braces ({}) and
double braces ({{}}).

Also removes content after a double semicolon (::),
which indicates a hint."
  (let* ((regex "{\\{1,2\\}c[0-9]+:\\{1,2\\}\\(.*?\\)\\(::[^{}]*\\)?}\\{1,2\\}")
         (result (replace-regexp-in-string regex "\\1" string)))
    result))

(defun gnosis-cloze-extract-contents (str)
  "Extract cloze contents for STR.

Return a list of cloze tag contents for STR, organized by cX-tag.

Valid cloze formats include:
\"This is an {c1:example}\"
\"This is an {{c1::example}}\""
  (let ((result-alist '())
        (start 0))
    (while (string-match "{\\{1,2\\}c\\([0-9]+\\)::?\\(.*?\\)}\\{1,2\\}" str start)
      (let* ((tag (match-string 1 str))
             (content (match-string 2 str)))
        (if (assoc tag result-alist)
            (push content (cdr (assoc tag result-alist)))
          (push (cons tag (list content)) result-alist))
        (setf start (match-end 0))))
    (mapcar (lambda (tag-group) (nreverse (cdr tag-group)))
	    (nreverse result-alist))))

(defun gnosis-cloze-extract-answers (nested-lst)
  "Extract cloze answers for string clozes inside the NESTED-LST.

This function should be used in combination with
`gnosis-cloze-extract-contents'."
  (mapcar (lambda (lst)
            (mapcar (lambda (str)
                      (replace-regexp-in-string "::\\(.*\\)" "" str))
                    lst))
          nested-lst))

(defun gnosis-cloze-extract-hints (nested-lst)
  "Extract cloze hints for string clozes inside the NESTED-LST.

This function should be used in combination with
`gnosis-cloze-extract-contents'."
  (mapcar (lambda (lst)
            (mapcar (lambda (str)
                      (when (string-match "::\\(.*\\)" str)
                        (match-string 1 str)))
                    lst))
          nested-lst))

(provide 'gnosis-cloze)
;;; gnosis-cloze.el ends here

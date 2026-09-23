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
(require 'seq)
(require 'gnosis-utils)

(defvar gnosis-face-cloze)
(defvar gnosis-face-false)
(defvar gnosis-face-unanswered)

(defvar gnosis-cloze-string "(...)")

(defun gnosis-cloze--word-char-p (char)
  "Return non-nil if CHAR is a word constituent in the current syntax table."
  (and char (eq (char-syntax char) ?w)))

(defun gnosis-cloze--digit-char-p (char)
  "Return non-nil if CHAR is an ASCII decimal digit."
  (and char (<= ?0 char) (<= char ?9)))

(defun gnosis-cloze--adjacent-continues-p (string start end toward-start)
  "Return non-nil if the character beside STRING[START,END) continues the token.
TOWARD-START non-nil inspects the character before START; otherwise the
character after END.  Word characters and underscores continue.  A period
or comma continues only when it sits between ASCII digits."
  (let* ((adj-index (if toward-start (1- start) end))
         (adjacent (and (>= adj-index 0)
                        (< adj-index (length string))
                        (aref string adj-index)))
         (edge-index (if toward-start start (1- end)))
         (edge (and (>= edge-index 0)
                    (< edge-index (length string))
                    (aref string edge-index)))
         (far-index (and adjacent
                         (if toward-start (1- adj-index) (1+ adj-index))))
         (far (and far-index
                   (>= far-index 0)
                   (< far-index (length string))
                   (aref string far-index))))
    (cond
     ((null adjacent) nil)
     ((or (gnosis-cloze--word-char-p adjacent) (eq adjacent ?_)) t)
     ((and (memq adjacent '(?. ?,))
           (gnosis-cloze--digit-char-p edge)
           (gnosis-cloze--digit-char-p far)))
     (t nil))))

(defun gnosis-cloze--standalone-p (string start end)
  "Return non-nil if STRING[START,END) is not attached to a larger token."
  (not (or (gnosis-cloze--adjacent-continues-p string start end t)
           (gnosis-cloze--adjacent-continues-p string start end nil))))

(defun gnosis-cloze--occurrence (string needle &optional from excluded)
  "Return (START . END) for NEEDLE in STRING at or after FROM.
Prefer the first standalone occurrence; if none exist, use the first
substring occurrence.  FROM is 0-based and defaults to 0.  Honor
`case-fold-search' and the current case table.  Use standard syntax for
word characters.  Skip spans overlapping EXCLUDED (START . END) pairs.
Return nil when NEEDLE is absent."
  (save-match-data
    (let ((quoted (regexp-quote needle))
          (search-spaces-regexp nil)
          (limit (length string))
          first)
      (with-syntax-table (standard-syntax-table)
        ;; An overlapping match can hide a later disjoint starting position.
        (cl-loop for pos = (or from 0) then (1+ beg)
                 while (<= pos limit)
                 for matched = (string-match quoted string pos)
                 while matched
                 for beg = (match-beginning 0)
                 for end = (match-end 0)
                 unless (seq-some (lambda (span)
                                    (and (< beg (cdr span)) (< (car span) end)))
                                  excluded)
                 do (unless first (setq first (cons beg end)))
                 and when (gnosis-cloze--standalone-p string beg end)
                 return (cons beg end)
                 finally return first)))))

(defun gnosis-cloze-highlight (str answers face &optional default-face)
  "Highlight the preferred occurrence of each of ANSWERS in STR with FACE.
Prefer a standalone match when one remains after the previous answer;
otherwise highlight the first remaining substring.  Optionally use
DEFAULT-FACE for the rest of STR.  Return a new string without changing
STR or ANSWERS."
  (cl-assert (listp answers) nil "Answers to mark must be a list.")
  (let ((search-spaces-regexp nil))
    (save-match-data
      (with-temp-buffer
        (insert (if default-face (propertize str 'face default-face) str))
        (goto-char (point-min))
        (dolist (answer answers)
          (let* ((answer-text (gnosis-utils-trim-quotes answer))
                 (origin (point))
                 (occ (gnosis-cloze--occurrence
                       (buffer-substring-no-properties origin (point-max))
                       answer-text)))
            (when occ
              (let ((beg (+ origin (car occ)))
                    (end (+ origin (cdr occ))))
                (goto-char beg)
                (delete-region beg end)
                (insert
                 (mapconcat
                  (lambda (char)
                    (if (not (memq char '(?\s ?\t ?\n)))
                        (propertize (char-to-string char) 'face face)
                      (char-to-string char)))
                  answer-text
                  ""))))))
        (buffer-string)))))

(defun gnosis-cloze--replace (str clozes cloze-string &optional case-fold)
  "Replace the preferred occurrence of each of CLOZES in STR with CLOZE-STRING.
Process CLOZES in order against the preceding result, trimming enclosing
quotes and retaining the cloze's leading and trailing whitespace pattern.
Prefer a standalone match when one remains, so \"2\" hides the later digit
in \"See 12.2 and the 2 items.\" rather than a decimal fraction.  If no
standalone match exists, keep the first substring so interior-only cards
such as \"port\" in transporter still hide.  Use the standard case table;
CASE-FOLD enables case-insensitive matching.  Preserve text properties
outside replaced spans, and give the mask `gnosis-face-cloze'.  Return a
new string without changing the inputs, rendering Org, or opening a buffer."
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
                                  (match-string 0 cloze-text))))
                    (occ (gnosis-cloze--occurrence text cloze-text)))
               (if occ
                   (concat (substring text 0 (car occ))
                           replacement
                           (substring text (cdr occ)))
                 text)))
           clozes :initial-value (copy-sequence str)))))))

(defun gnosis-cloze-add-hints (str hints &optional cloze-string with-evidence)
  "Replace CLOZE-STRING in STR with literal HINTS, skipping empty hints.
With WITH-EVIDENCE, return (TEXT . SHOWN-HINTS), retaining only inserted hints."
  (cl-assert (listp hints) nil "Hints must be a list.")
  (let ((cloze-string (or cloze-string gnosis-cloze-string)) shown)
    (with-temp-buffer
      (insert str)
      (goto-char (point-min))
      (cl-loop for hint in hints
               while (search-forward cloze-string nil t)
               do
	       (when (and hint (not (string-empty-p hint)) (not (string= hint "nil"))
			  (not (string= "\"\"" hint))
			  (search-backward cloze-string nil t))
                 (push hint shown)
                 (replace-match (propertize (format "(%s)" hint)
					    'face 'gnosis-face-cloze)
                                t t)
                 (goto-char (match-end 0)))) ; Move point to end of match
      (if with-evidence (cons (buffer-string) (nreverse shown)) (buffer-string)))))

(defun gnosis-cloze--spans (str answers)
  "Return disjoint preferred spans in STR for ANSWERS, in answer order.
A missing occurrence is nil.  Match using the native renderer's case rules."
  (let ((case-fold-search (default-value 'case-fold-search)) spans)
    (with-case-table (standard-case-table)
      (mapcar (lambda (answer)
                (let ((span (gnosis-cloze--occurrence
                             str (gnosis-utils-trim-quotes answer) nil spans)))
                  (when span (push span spans))
                  span))
              answers))))

(defun gnosis-cloze--render (str answers remaining hints &optional failed)
  "Return (TEXT . SHOWN-HINTS) for original ANSWERS in STR.
REMAINING contains original blank indices, never answer strings.  Mask them
with their corresponding HINTS, or, when FAILED, mark the first remaining
blank false and the rest unanswered.  Mark other blanks correct.  Assign
each occurrence once before projecting any class, preserving token matching."
  (let ((case-fold-search (default-value 'case-fold-search))
        replacements shown)
    (cl-loop for answer in answers for index from 0
             for text = (gnosis-utils-trim-quotes answer)
             for span in (gnosis-cloze--spans str answers)
             when span do
             (let* ((pending (memq index remaining))
                    (hint (and pending (not failed) (nth index hints)))
                    (hint (and hint (not (member hint '("" "nil" "\"\""))) hint))
                    (replacement
                     (if (and pending (not failed))
                         (gnosis-cloze--replace text (list text)
                                               (if hint (format "(%s)" hint) gnosis-cloze-string))
                       (gnosis-cloze-highlight
                        text (list text)
                        (cond ((not pending) 'gnosis-face-correct)
                              ((= index (car remaining)) 'gnosis-face-false)
                              (t 'gnosis-face-unanswered))))))
               (when hint (push (cons (car span) hint) shown))
               (push (cons span replacement) replacements)))
    ;; Right-to-left replacement retains all original character positions.
    (cons (cl-reduce
           (lambda (text entry)
             (concat (substring text 0 (caar entry)) (cdr entry)
                     (substring text (cdar entry))))
           (sort replacements (lambda (a b) (> (caar a) (caar b))))
           :initial-value (copy-sequence str))
          (mapcar #'cdr (sort shown (lambda (a b) (< (car a) (car b))))))))

(defun gnosis-cloze-mark-false (str answers)
  "Mark the first occurrence in STR for ANSWERS false, the rest unanswered."
  (car (gnosis-cloze--render str answers (number-sequence 0 (1- (length answers))) nil t)))

(defun gnosis-cloze-check (sentence clozes)
  "Return t if CLOZES have disjoint preferred occurrences in SENTENCE.
Reserve spans in answer order, exactly as the native renderer does.
A match may be standalone or, when no standalone remains, the legacy
substring occurrence."
  (and (cl-every #'identity (gnosis-cloze--spans sentence clozes)) t))

;; TODO: use a better name to indicate that it also removes hints from STRING.
(defun gnosis-cloze-remove-tags (string)
  "Replace cloze tags and hints in STRING.

Works with both single (:), double colons (::), single braces ({}) and
double braces ({{}}).  Preserve literal newlines in the cloze contents.

Also remove content after a double colon (::), which indicates a hint."
  (let* ((regex "{\\{1,2\\}c[0-9]+:\\{1,2\\}\\(\\(?:.\\|\n\\)*?\\)\\(::[^{}]*\\)?}\\{1,2\\}")
         (result (replace-regexp-in-string regex "\\1" string)))
    result))

(defun gnosis-cloze-extract-contents (str)
  "Extract cloze contents for STR.

Return a list of cloze tag contents for STR, organized by cX-tag.
Include every member, preserving literal newlines in contents and hints.

Valid cloze formats include:
\"This is an {c1:example}\"
\"This is an {{c1::example}}\""
  (let ((result-alist '())
        (start 0))
    (while (string-match "{\\{1,2\\}c\\([0-9]+\\)::?\\(\\(?:.\\|\n\\)*?\\)}\\{1,2\\}" str start)
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
                      (replace-regexp-in-string "::\\(?:.\\|\n\\)*" "" str))
                    lst))
          nested-lst))

(defun gnosis-cloze-extract-hints (nested-lst)
  "Extract cloze hints for string clozes inside the NESTED-LST.

This function should be used in combination with
`gnosis-cloze-extract-contents'."
  (mapcar (lambda (lst)
            (mapcar (lambda (str)
                      (when (string-match "::\\(\\(?:.\\|\n\\)*\\)" str)
                        (match-string 1 str)))
                    lst))
          nested-lst))

(provide 'gnosis-cloze)
;;; gnosis-cloze.el ends here

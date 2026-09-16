;;; gnosis-answer.el --- Explicit typed answers -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Free Software Foundation, Inc.
;; Author: Thanos Apollo <public@thanosapollo.org>
;; Keywords: extensions
;; URL: https://thanosapollo.org/projects/gnosis

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
;; Pure typed-answer matching and native accepted-alias list sections.
;; Aliases are authored spellings, never inferred synonyms.  Section encoding
;; adds one "- " marker per item; decoding removes exactly that marker.

;;; Code:
(require 'cl-lib)
(require 'subr-x)
(require 'seq)
(require 'gnosis-utils)

(defun gnosis-answer-aliases-eligible-p (type answer)
  "Return non-nil if TYPE and canonical ANSWER support accepted aliases.
TYPE is case-insensitive.  ANSWER must be a proper list containing one
nonblank string.  This does not validate aliases or portable media."
  (and (member (downcase type) '("basic" "image-occlusion" "model-name"))
       (proper-list-p answer) (= (length answer) 1)
       (stringp (car answer))
       (not (string-empty-p (string-trim (car answer))))))

(defun gnosis-answer-validate-aliases (aliases)
  "Validate ALIASES and return the original value without modifying it.
Require a proper list of nonempty, single-line, non-whitespace strings."
  (unless (and (proper-list-p aliases)
               (seq-every-p (lambda (alias)
                              (and (stringp alias)
                                   (not (string-empty-p (string-trim alias)))
                                   (not (string-match-p "[\n\r]" alias))))
                            aliases))
    (user-error "Accepted aliases must be nonempty single-line strings"))
  aliases)

(defun gnosis-answer-match-p (canonical input aliases tolerance)
  "Match CANONICAL or explicit ALIASES against INPUT using TOLERANCE.
Apply the existing quote, whitespace, case and short-string rules to each
candidate independently.  Never change canonical reveal text or inputs."
  (gnosis-answer-validate-aliases aliases)
  (let ((normalize (lambda (text)
                     (downcase (replace-regexp-in-string
                                "\\s-" "" (gnosis-utils-trim-quotes text))))))
    (let ((typed (funcall normalize input)))
      (seq-some
       (lambda (candidate)
         (let* ((expected (funcall normalize candidate))
                (size (max (length expected) (length typed))))
           (if (> size tolerance)
               (<= (string-distance expected typed) tolerance)
             (string= expected typed))))
       (cons canonical aliases)))))

(defun gnosis-answer-aliases-to-section (aliases)
  "Encode validated ALIASES as a native Org list section.
Add exactly one list marker per item, preserving spelling and order."
  (mapconcat (lambda (alias) (concat "- " alias))
             (gnosis-answer-validate-aliases aliases) "\n"))

(defun gnosis-answer-aliases-from-section (text)
  "Decode accepted-alias section TEXT, removing one list marker per line.
Blank sections mean nil.  Reject non-list and multiline list items."
  (gnosis-answer-validate-aliases
   (mapcar (lambda (line)
             (unless (string-prefix-p "- " line)
               (user-error "Accepted aliases require one list item per line"))
             (substring line 2))
           (unless (string-empty-p (string-trim text))
             (split-string (string-trim text "[\n\r]+" "[\n\r]+") "\n" t)))))

(provide 'gnosis-answer)
;;; gnosis-answer.el ends here

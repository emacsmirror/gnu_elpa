;;; calibre-search.el --- Filter books based on search criteria.  -*- lexical-binding: t; -*-
;; Copyright (C) 2023  Free Software Foundation, Inc.

;; This file is part of calibre.el.

;; calibre.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; calibre.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with calibre.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; This file provides an interface for filtering the current library
;; on criteria such as Author, Publisher, Tags, etc.

;;; Code:
(require 'transient)
(require 'calibre-core)

(defmacro calibre-search--choice-function (field &optional an plural)
  "Create a function to prompt the user to select a value for FIELD.
FIELD should be the name of a metadata field associated with
books.  A corresponding calibre-core--get-FIELDs function must
exist, i.e. if FIELD is author the function
calibre-core--get-authors must exist.

If AN is non-nil the new function's docstring will read: Prompt
the user to select an FIELD, otherwise it will read: Prompt the
user to select a FIELD.

If PLURAL is non-nil FIELD is assumed to be a plural word, and an s
will not be appended to the calibre-db--get-FIELD function's name."
  `(defun ,(intern (format "calibre-search-chose-%s" field)) ()
     ,(format "Prompt the user to select %s %s" (if an "an" "a") field)
     (interactive)
     (completing-read ,(format "%s: " (capitalize field))
                      (,(intern (format "calibre-core--get-%s%s"
                                        field
                                        (if plural "" "s")))))))

(calibre-search--choice-function "title")
(calibre-search--choice-function "author" t)
(calibre-search--choice-function "publisher")
(calibre-search--choice-function "tag")
(calibre-search--choice-function "series" nil t)
(calibre-search--choice-function "format")

(defun calibre-search--operation (args)
  "Return the appropriate symbol for a filter operation.
ARGS is the argument list of a transient command."
  (if (seq-contains-p args "--exclude")
      '-
    '+))

(defmacro calibre-library--search-function (field)
  "Create a function adding a filter for FIELD."
  `(defun ,(intern (format "calibre-library-search-%s" field)) (val &optional args)
     (interactive (list (,(intern (format "calibre-search-chose-%s" field)))
                        (transient-args 'calibre-search)))
     (setf calibre-library--filters (cons
                                     (vector (calibre-search--operation args)
                                             (quote ,(intern field))
                                             val)
                                     calibre-library--filters))
     (calibre-library--refresh)))

(calibre-library--search-function "title")
(calibre-library--search-function "author")
(calibre-library--search-function "publisher")
(calibre-library--search-function "series")
(calibre-library--search-function "tag")
(calibre-library--search-function "format")

(defun calibre-library-clear-last-search ()
  "Clear the last applied search filter."
  (interactive)
  (when calibre-library--filters
    (setf calibre-library--filters (cdr calibre-library--filters)))
  (calibre-library--refresh))

(transient-define-prefix calibre-search ()
  "Filter the library view."
  :transient-suffix 'transient--do-call
  ["Arguments"
   ("-e" "Exclude" "--exclude")]
  ["Search"
   ("T" "Title" calibre-library-search-title)
   ("a" "Author" calibre-library-search-author)
   ("p" "Publisher" calibre-library-search-publisher)
   ("t" "Tag" calibre-library-search-tag)
   ("s" "Series" calibre-library-search-series)
   ("f" "Format" calibre-library-search-format)]
  ["Actions"
   ("C" "Compose" calibre-search-compose)
   ("u" "Undo" calibre-library-clear-last-search)
   ("c" "Clear" calibre-library-clear-filters)
   ("q" "Exit" transient-quit-one)])


(defvar calibre-search-composing-filter nil)
(defun calibre-search-compose-apply (&optional args)
  "Apply the composite filter under construction."
  (interactive (list (transient-args 'calibre-search-compose)))
  (if (not calibre-search-composing-filter)
      (error "Can not apply an empty composite filter")
    (let ((filter (vector (calibre-search--operation args)
                          calibre-search-composing-filter)))
      (setf calibre-library--filters (cons filter calibre-library--filters)
            calibre-search-composing-filter nil)
      (calibre-library--refresh))))

(defun calibre-search-compose-cleanup ()
  "Clear any filter currently being composed."
  (interactive)
  (setf calibre-search-composing-filter nil))

(defmacro calibre-search--composition-function (field)
  "Create a function adding a filter for FIELD to a composite filter."
  `(defun ,(intern (format "calibre-search-compose-%s" field)) ()
     ,(format "Add a filter for %s to the composite filter under construction." field)
     (interactive)
     (setf calibre-search-composing-filter
           (cons (vector ',(intern field) (,(intern (format "calibre-search-chose-%s" field)))) calibre-search-composing-filter))))

(calibre-search--composition-function "title")
(calibre-search--composition-function "author")
(calibre-search--composition-function "publisher")
(calibre-search--composition-function "tag")
(calibre-search--composition-function "series")
(calibre-search--composition-function "format")

(transient-define-prefix calibre-search-compose ()
  "Create a composite filter."
  :transient-suffix 'transient--do-call
  ["Arguments"
   ("-e" "Exclude" "--exclude")]
  ["Compose"
   ("T" "Title" calibre-search-compose-title)
   ("a" "Author" calibre-search-compose-author)
   ("p" "Publisher" calibre-search-compose-publisher)
   ("t" "Tag" calibre-search-compose-tag)
   ("s" "Series" calibre-search-compose-series)
   ("f" "Format" calibre-search-compose-format)]
  ["Actions"
   ("A" "Apply" calibre-search-compose-apply :transient transient--do-return)
   ("q" "Quit" calibre-search-compose-cleanup :transient transient--do-return)])

(provide 'calibre-search)

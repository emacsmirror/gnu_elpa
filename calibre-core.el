;;; calibre-core.el --- Abstract interface for the Calibre Library  -*- lexical-binding: t; -*-

;; Copyright (C) 2023,2024  Free Software Foundation, Inc.

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
;; This file contains the abstract interface for accessing the calibre
;; library.  This abstract layer absolves the upper layers of caring
;; which interface is used.

;;; Code:
(require 'calibre-db)
(require 'calibre-cli)

(defconst calibre-library-buffer "*Library*")

(defmacro calibre-core--interface (function &rest args)
  "Determine which interface to call FUNCTION from.
ARGS are arguments to pass to FUNCTION.  The functions
calibre-db--FUNCTION and calibre-cli--FUNCTION must exist."
  (let ((interface (if (and (fboundp 'sqlite-available-p) (sqlite-available-p))
                       'calibre-db
                     'calibre-cli)))
    `(,(intern (format "%s--%s" interface function)) ,@args)))

(defvar calibre--books nil)
(defun calibre--books (&optional force)
  "Return the in memory list of books.
If FORCE is non-nil the list is refreshed from the database."
  (when (or force (not calibre--books))
    (setf calibre--books (calibre-core--interface get-books)))
  calibre--books)

(defun calibre-book-summary (book)
  "Return the summary of BOOK."
  (calibre-core--interface get-summary book))

(defvar calibre-library--filters nil)
(defun calibre-library-clear-filters ()
  "Clear all active filters."
  (interactive)
  (setf calibre-library--filters nil)
  (calibre-library--refresh))

(defvar calibre--library nil
  "The active library.")
(defun calibre--library ()
  "Return the active library.
If no library is active, prompt the user to select one."
  (unless calibre--library
    (calibre-select-library))
  (alist-get calibre--library calibre-libraries nil nil #'string=))

(defun calibre-select-library (&optional library)
  "Prompt the user to select a library from `calibre-libraries'.
If LIBRARY is non-nil, select that instead."
  (interactive)
  (unless calibre-libraries
    (error "No Libraries defined"))
  (setf calibre--library (if library
                             library
                           (let ((names (calibre--library-names)))
                             (if (not (length> names 1))
                               (car names)
                             (completing-read "Library: " names nil t))))
        calibre--db nil
        calibre--books nil)
  (calibre-library--refresh t))

(defun calibre-library--refresh (&optional force)
  "Refresh the contents of the library buffer.
If FORCE is non-nil fetch book data from the database."
  (let ((buffer (get-buffer calibre-library-buffer)))
    (when buffer
      (with-current-buffer buffer
        (let ((book (tabulated-list-get-id)))
          (calibre-with-preserved-marks (not force)
            (setf tabulated-list-entries
                  (mapcar #'calibre-book--print-info
                          (calibre-library--filter calibre-library--filters
                                                   (calibre--books force))))
            (tabulated-list-print)
            (if book
                (calibre-library--find-book book)
              (goto-char (point-max)))))))))

(defun calibre-library--set-header ()
  "Set the header of the Library buffer."
  (let ((buffer (get-buffer calibre-library-buffer)))
    (when buffer
      (with-current-buffer buffer
        (setf tabulated-list-format (calibre-library--header-format))
        (tabulated-list-init-header)))))

(defcustom calibre-library-time-format "%x"
  "String specifying format for displaying time related metadata.
See `format-time-string' for an explanation of how to write this
string."
  :type 'string
  :set (lambda (symbol value)
         (set-default symbol value)
         (calibre-library--refresh))
  :group 'calibre
  :package-version '("calibre" . "1.1.0"))

(defcustom calibre-library-columns '((id . 4)
                                     (title . 35)
                                     (authors . 20)
                                     (publisher . 10)
                                     (series . 15)
                                     (series-index . 3)
                                     (tags . 10)
                                     (formats . 10))
  "The metadata fields to display in the library buffer.
Each entry is a key identifying a metadata field and the width that
column should have."
  :type '(repeat (cons
                  :tag "Column"
                  (choice
                   :tag "Attribute"
                   (const :tag "ID" id)
                   (const :tag "Title" title)
                   (const :tag "Author(s)" authors)
                   (const :tag "Publisher(s)" publisher)
                   (const :tag "Series" series)
                   (const :tag "Series Index" series-index)
                   (const :tag "Tags" tags)
                   (const :tag "Formats" formats)
                   (const :tag "Publication date" pubdate))
                  (integer :tag "Width")))
  :set (lambda (symbol value)
         (set-default symbol value)
         (calibre-library--set-header)
         (calibre-library--refresh))
  :package-version '("calibre" . "1.1.0")
  :group 'calibre)

(defun calibre-library--header-format ()
  "Create the header for the Library buffer.
Return a vector suitable as the value of `tabulated-list-format'
with values determined by `calibre-library-columns'."
  (vconcat
   (mapcar (lambda (x)
             (let ((column (car x))
                   (width (cdr x)))
               (cl-case column
                 (id `("ID" ,width (lambda (a b)
                                     (< (calibre-book-id (car a))
                                        (calibre-book-id (car b))))
                       :right-align t))
                 (title `("Title" ,width
                          (lambda (a b)
                            (string< (downcase (calibre-book-title (car a)))
                                     (downcase (calibre-book-title (car b)))))))
                 (authors `("Author(s)" ,width t))
                 (publisher `("Publisher" ,width t))
                 (series `("Series" ,width (lambda (a b)
                                             (calibre-book-sort-by-series (car a) (car b)))))
                 (series-index `("#" ,width (lambda (a b)
                                              (calibre-book-sort-by-series (car a) (car b)))
                                 :right-align t))
                 (tags `("Tags" ,width))
                 (formats `("Formats" ,width))
                 (pubdate `("Publication Date" ,width (lambda (a b)
                                                        (time-less-p (calibre-book-pubdate (car a))
                                                                     (calibre-book-pubdate (car b)))))))))
           calibre-library-columns)))

(defun calibre-book--print-info (book)
  "Return list suitable as a value of `tabulated-list-entries'.
BOOK is a `calibre-book'."
  (list book
        (vconcat
         (mapcar
          (lambda (x)
            (let ((column (car x)))
              (cl-case column
                (id (int-to-string (calibre-book-id book)))
                (title (calibre-book-title book))
                (authors (string-join (calibre-book-authors book) ", "))
                (publisher (or (calibre-book-publisher book) ""))
                (series (or (calibre-book-series book) ""))
                (series-index
                 (if (calibre-book-series book)
                     (format "%.1f" (calibre-book-series-index book))
                   ""))
                (tags (string-join (calibre-book-tags book) ", "))
                (formats (string-join
                          (mapcar (lambda (f)
                                    (upcase (symbol-name f)))
                                  (calibre-book-formats book))
                          ", "))
                (pubdate (if (calibre-book-pubdate book)
                             (format-time-string
                              calibre-library-time-format
                              (calibre-book-pubdate book))
                           "Invalid")))))
                 calibre-library-columns))))

(defun calibre-book--file (book format)
  "Return the path to BOOK in FORMAT."
  (let ((path (calibre-book-path book))
        (file-name (calibre-book-file-name book)))
    (expand-file-name (file-name-concat (calibre--library)
                                        path
                                        (format "%s.%s" file-name format)))))

(defun calibre-composite-filter-p (object)
  "Return t if OBJECT is a composite filter."
  (and (vectorp object) (length= object 2) (listp (elt object 1))))

(defun calibre--get-filter-items (filter)
  "Return the id's of books matching FILTER."
  (if (calibre-composite-filter-p filter)
      (seq-let (op filters) filter
        (seq-reduce (if (eq op '+)
                       #'seq-union
                     #'seq-intersection)
                   (mapcar (lambda (f)
                             (calibre--get-filter-items (vconcat `[,op] f)))
                           filters)
                   (if (eq op '+)
                       '()
                     (calibre--books))))
    (seq-let (_ field value &rest params) filter
      (let ((fuzzy-match (seq-contains-p params '~)))
        (cl-case field
        (title (calibre-core--interface get-title-books value fuzzy-match))
        (author (calibre-core--interface get-author-books value fuzzy-match))
        (tag (calibre-core--interface get-tag-books value fuzzy-match))
        (publisher (calibre-core--interface get-publisher-books value fuzzy-match))
        (series (calibre-core--interface get-series-books value fuzzy-match))
        (format (calibre-core--interface get-format-books value fuzzy-match)))))))

(defun calibre-library--filter (filters books)
  "Return those books in BOOKS that match FILTERS.
FILTERS should be a list of vectors, for the exact contents see
`calibre-virtual-libraries'."
  (let* ((include (seq-filter (lambda (f) (eq (elt f 0) '+)) filters))
         (exclude (seq-filter (lambda (f) (eq (elt f 0) '-)) filters))
         (include-ids (when include
                        (seq-reduce #'seq-intersection
                                    (mapcar #'calibre--get-filter-items include)
                                    (mapcar #'calibre-book-id calibre--books))))
         (exclude-ids (when exclude
                        (seq-reduce #'seq-union
                                    (mapcar #'calibre--get-filter-items exclude)
                                    '()))))
    (seq-filter (lambda (b)
                  (not (seq-find (lambda (id)
                                   (= id (calibre-book-id b)))
                                 exclude-ids)))
                  (if include-ids
                      (seq-filter (lambda (b)
                                    (seq-find (lambda (id)
                                                (= id (calibre-book-id b)))
                                              include-ids))
                                        books)
                    (if include
                        nil
                      books)))))

;; The ignored optional argument makes these functions valid arguments
;; to completion-table-dynamic.
(defun calibre-core--get-titles (&optional _)
  "Return a list of the titles in the active library."
  (calibre-core--interface get-titles))

(defun calibre-core--get-authors (&optional _)
  "Return a list of the authors in the active library."
  (calibre-core--interface get-authors))

(defun calibre-core--get-tags (&optional _)
  "Return a list of the tags in the active library."
  (calibre-core--interface get-tags))

(defun calibre-core--get-formats (&optional _)
  "Return a list of the file formats stored in the active library."
  (calibre-core--interface get-formats))

(defun calibre-core--get-series (&optional _)
  "Return a list of the series in the active library."
  (calibre-core--interface get-series))

(defun calibre-core--get-publishers (&optional _)
  "Return a list of the publishers in the active library."
  (calibre-core--interface get-publishers))

;; Completion tables
(defvar calibre-authors-completion-table
  (completion-table-dynamic #'calibre-core--get-authors))
(defvar calibre-publishers-completion-table
  (completion-table-dynamic #'calibre-core--get-publishers))
(defvar calibre-series-completion-table
  (completion-table-dynamic #'calibre-core--get-series))
(defvar calibre-tags-completion-table
  (completion-table-dynamic #'calibre-core--get-tags))

(provide 'calibre-core)
;;; calibre-core.el ends here

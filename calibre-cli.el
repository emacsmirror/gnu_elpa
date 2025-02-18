;;; calibre-cli.el --- Fallback CLI interface when SQLite is not available  -*- lexical-binding: t; -*-

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
;; This file contains an interface to calibre using calibredb and JSON
;; parsing instead of SQLite.  This interface is significantly slower
;; than the SQLite interface, and should therefore only be used when
;; SQLite support is not available.

;;; Code:
(require 'compat) ; for string-split
(require 'calibre)
(require 'calibre-book)
(require 'calibre-util)

(declare-function json-parse-buffer (&rest args) "json.c")

(defun calibre-cli--list (&rest fields)
  "List books in the active library.
If FIELDS is a list of metadata fields to list, if none are
specified the default is \\='all\\='."
  (with-temp-buffer
    (apply #'call-process
           `(,calibre-calibredb-executable
             nil
             t
             nil
             "--with-library"
             ,(calibre--library)
             "list"
             ,@(if fields
                (flatten-list (mapcar (lambda (f) `("-f" ,f)) fields))
                '("-f" "all"))
             "--for-machine"))
    (goto-char (point-min))
    (json-parse-buffer :object-type 'alist :array-type 'list)))

(defun calibre-cli--get-books ()
  "Return all books in the Calibre library `calibre-library-dir'."
  (mapcar #'calibre-cli--make-book (calibre-cli--list)))

(defun calibre-cli--get-summary (book)
  "Return the summary of BOOK."
  (calibre-and=>
   (with-temp-buffer
     (apply #'call-process
            `(,calibre-calibredb-executable
              nil
              t
              nil
              "--with-library"
              ,(calibre--library)
              "list"
              "--search" ,(format "id:%d" (calibre-book-id book))
              "--fields" "comments"
              "--for-machine"))
     (goto-char (point-min))
     (json-parse-buffer :object-type 'alist :array-type 'list))
   (lambda (res)
     (calibre-and=> (assq 'comments (car res)) #'cdr))))

(defun calibre-cli--make-book (json)
  "Make a `calibre-book' from JSON."
  (let-alist json
    (make-calibre-book :id .id
                       :title .title
                       :authors (calibre-cli--parse-authors .authors)
                       :publisher .publisher
                       :series .series
                       :series-index .series_index
                       :timestamp (calibre-cli--parse-timestamp .timestamp)
                       :pubdate (calibre-cli--parse-timestamp .pubdate)
                       :last-modified (calibre-cli--parse-timestamp .last_modified)
                       :tags (sort .tags)
                       :formats (calibre-cli--parse-formats .formats)
                       :path (calibre-cli--parse-path .formats)
                       :file-name (calibre-cli--parse-file-name .formats))))

(defun calibre-cli--parse-authors (authors)
  "Parse AUTHORS a string, into a list of authors.
AUTHORS should be a comma separated string."
  (string-split authors " & "))

(defun calibre-cli--parse-timestamp (timestamp)
  "Parse TIMESTAMP into a Lisp timestamp."
  (iso8601-parse timestamp))

(defun calibre-cli--parse-formats (files)
  "Return the formats of the book whose files are FILES."
  (mapcar (lambda (f)
            (intern (file-name-extension f)))
          files))

(defun calibre-cli--parse-path (files)
  "Return the path within the library of the book whose files are FILES."
  (string-replace (calibre--library)
                  ""
                  (file-name-directory (car files))))

(defun calibre-cli--parse-file-name (files)
  "Return the File Name of the book whose files are FILES."
  (file-name-base (car files)))

(defmacro calibre-cli--search-operation (field docstring)
  "Create a function to search for books matching FIELD.

DOCSTRING is the docstring of the created function."
  `(defun ,(intern (format "calibre-cli--get-%s-books" field)) (,field &optional fuzzy-match)
     ,docstring
     (with-temp-buffer
       (call-process calibre-calibredb-executable
                     nil
                     t
                     nil
                     "search"
                     "--with-library"
                     (calibre--library)
                     (format ,(format "%s%%s%%s" field ) (if fuzzy-match ":" ":=") ,field))
       (mapcar #'string-to-number
               (string-split
                (buffer-substring-no-properties (point-min) (point-max))
                ",")))))

(calibre-cli--search-operation title "Return the id's of books whose title is TITLE.")
(calibre-cli--search-operation series "Return the id's of books that are part of SERIES.")
(calibre-cli--search-operation publisher "Return the id's of books published by PUBLISHER.")
(calibre-cli--search-operation format "Return the id's of books available in FORMAT.")
(calibre-cli--search-operation tag "Return the id's of books tagged with TAG.")
(calibre-cli--search-operation author "Return the id's of books written by AUTHOR.")

(defun calibre-cli--get-titles ()
  "Return a list of the titles in the active library."
  (cl-remove-duplicates
   (mapcar (lambda (b)
             (alist-get 'title b))
           (calibre-cli--list "title"))
   :test #'string=))

(defun calibre-cli--get-authors ()
  "Return a list of the authors in the active library."
  (cl-remove-duplicates
   (seq-reduce #'seq-union
               (mapcar (lambda (b)
                         (calibre-cli--parse-authors (alist-get 'authors b)))
                       (calibre-cli--list "authors"))
               '())
   :test #'string=))

(defun calibre-cli--get-publishers ()
  "Return a list of the publishers in the active library."
  (remq nil (cl-remove-duplicates
             (mapcar (lambda (b)
                       (alist-get 'publisher b))
                     (calibre-cli--list "publisher"))
             :test #'string=)))

(defun calibre-cli--get-series ()
  "Return a list of the series in the active library."
  (remq nil (cl-remove-duplicates
             (mapcar (lambda (b)
                       (alist-get 'series b))
                     (calibre-cli--list "series"))
             :test #'string=)))

(defun calibre-cli--get-tags ()
  "Return a list of the tags in the active library."
  (seq-reduce #'seq-union
               (mapcar (lambda (b)
                         (alist-get 'tags b))
                       (calibre-cli--list "tags"))
               '()))

(defun calibre-cli--get-formats ()
  "Return a list of the file formats stored in the active library."
  (seq-reduce #'seq-union
              (mapcar (lambda (b)
                        (calibre-cli--parse-formats (alist-get 'formats b)))
                      (calibre-cli--list "formats"))
              '()))

(provide 'calibre-cli)
;;; calibre-cli.el ends here

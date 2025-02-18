;;; calibre-db.el --- Interact with the Calibre database -*- lexical-binding:t -*-

;; Copyright (C) 2023,2024  Free Software Foundation, Inc.

;; Author: Kjartan Oli Agustsson <kjartanoli@disroot.org>
;; Maintainer: Kjartan Oli Agustsson <kjartanoli@disroot.org>

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
;; Fetch data from the library database.

;;; Code:
(require 'calibre)
(require 'calibre-book)
(require 'calibre-util)

(declare-function sqlite-open (&optional file) "sqlite.c")
(declare-function sqlite-select (db query &optional values return-type))

(defun calibre-db--parse-timestamp (timestamp)
  "Return a Lisp timestamp from TIMESTAMP.
TIMESTAMP is a string of the form YYYY-MM-DD HH:MM:SS.xxxxxx+00:00."
  (ignore-errors (parse-iso8601-time-string (string-replace " " "T" timestamp))))

(defun calibre-db--make-book (entry)
  "Create a `calibre-book' from ENTRY.
ENTRY is a list of the form:
\(ID TITLE SERIES SERIES-INDEX TIMESTAMP PUBDATE LAST-MODIFIED PATH)."
  (seq-let
      [id
       title
       series
       series-index
       timestamp
       pubdate
       last-modified
       path]
      entry
    (make-calibre-book :id id
                       :title title
                       :authors (calibre-db--get-book-authors id)
                       :publisher (calibre-db--get-book-publisher id)
                       :series series
                       :series-index series-index
                       :timestamp (calibre-db--parse-timestamp timestamp)
                       :pubdate (calibre-db--parse-timestamp pubdate)
                       :last-modified (calibre-db--parse-timestamp last-modified)
                       :tags (sort (calibre-db--get-book-tags id))
                       :formats (calibre-db--get-book-formats id)
                       :path path
                       :file-name (calibre-db--get-book-file-name id))))

(defun calibre-db--get-book-authors (id)
  "Return a list of authors for the book identified by ID."
  (flatten-list (sqlite-select (calibre--db)
                               "SELECT authors.name
FROM authors
INNER JOIN books_authors_link al ON authors.id = al.author
WHERE al.book = ?" `[,id])))

(defun calibre-db--get-book-publisher (id)
  "Return the publisher of the book identified by ID."
  (car (flatten-list (sqlite-select (calibre--db)
                               "SELECT publishers.name
FROM publishers
INNER JOIN books_publishers_link pl ON publishers.id = pl.publisher
WHERE pl.book = ?" `[,id]))))

(defun calibre-db--get-book-file-name (id)
  "Return the file name, sans extension, of the book identified by ID."
  (car (car (sqlite-select (calibre--db)
                           "SELECT name
FROM data
WHERE book = ?" `[,id]))))

(defun calibre-db--get-book-tags (id)
  "Return a list of tags for the book identified by ID."
  (flatten-list (sqlite-select (calibre--db)
                               "SELECT tags.name
FROM books
INNER JOIN books_tags_link tl ON books.id = tl.book
INNER JOIN tags ON tl.tag = tags.id
WHERE books.id = ?"
                               `[,id])))

(defun calibre-db--get-book-formats (id)
  "Return a list of formats for the book identified by ID."
  (mapcar (lambda (f)
            (intern (downcase f)))
          (flatten-list (sqlite-select (calibre--db)
                               "SELECT format FROM data WHERE book = ?"
                               `[,id]))))

(defun calibre-db--get-titles ()
  "Return a list of the titles in the active library."
  (flatten-list (sqlite-select (calibre--db)
                               "SELECT title FROM books;")))

(defun calibre-db--get-authors ()
  "Return a list of the authors in the active library."
  (flatten-list (sqlite-select (calibre--db)
                               "SELECT name FROM authors;")))

(defun calibre-db--get-series ()
  "Return a list of the series in the active library."
  (flatten-list (sqlite-select (calibre--db)
                               "SELECT name FROM series;")))

(defun calibre-db--get-tags ()
  "Return a list of the tags in the active library."
  (flatten-list (sqlite-select (calibre--db)
                               "SELECT name FROM tags;")))

(defun calibre-db--get-publishers ()
  "Return a list of the publishers in the active library."
  (flatten-list (sqlite-select (calibre--db)
                               "SELECT name FROM publishers;")))

(defun calibre-db--get-formats ()
  "Return a list of the file formats stored in the active library."
  (flatten-list (sqlite-select (calibre--db)
                               "SELECT DISTINCT format FROM data;")))

(defvar calibre--db nil)
(defun calibre--db ()
  "Return the metadata database."
  (unless calibre--db
    (let ((file-name (file-name-concat (calibre--library) "metadata.db")))
      (if (not (file-exists-p file-name))
          (progn
            (message "Metedata database %s does not exist.  Add some books to the library to create it." file-name)
            (setf calibre--db nil))
        (setf calibre--db
              (sqlite-open
               file-name)))))
  calibre--db)

(defun calibre-db--get-books ()
  "Return all books in the Calibre library `calibre-library-dir'."
  (if (not (calibre--db))
      nil
    (mapcar #'calibre-db--make-book
            (sqlite-select (calibre--db)
                           "SELECT
 books.id, title,
 series.name,
 series_index,
 timestamp,
 pubdate,
 last_modified,
 path
FROM books
LEFT JOIN books_series_link sl ON books.id = sl.book
LEFT JOIN series ON sl.series = series.id;"))))

(defmacro calibre-db--query (query arg &optional fuzzy)
  "Run QUERY on the Calibre database.

ARG is a value to passed to a WHERE clause in QUERY.  FUZZY
determines whether the WHERE clause matches using LIKE or exact
matching.  If FUZZY is non-nil LIKE will be used.  QUERY must
contain exactly one %s where the matching against ARG should be
substituted.

This means QUERY should probably end:
WHERE column %s
with column being the name of some column."
  `(flatten-list (sqlite-select (calibre--db)
                                (format ,query (if ,fuzzy "LIKE ?" "= ?"))
                                (if ,fuzzy
                                    (vector (format "%%%s%%" ,arg))
                                  (vector ,arg)))))

(defun calibre-db--get-summary (book)
  "Return the summary of BOOK."
  (calibre-and=>
   (calibre-db--query
    "SELECT text FROM comments WHERE book %s"
    (calibre-book-id book))
   #'car))

(defmacro calibre-db--search-function (field docstring query)
  "Create a search function for FIELD with DOCSTRING as docstring.

QUERY is the SQL query used to perform the search, suitable as an
argument to `calibre-db--query'."
  (declare (indent 1))
  `(defun ,(intern (format "calibre-db--get-%s-books" field))
       (,field &optional fuzzy-match)
     ,docstring
     (calibre-db--query ,query ,field fuzzy-match)))

(calibre-db--search-function title
  "Return the id's of books whose title is TITLE."
  "SELECT id FROM books WHERE title %s")

(calibre-db--search-function author
  "Return the id's of books written by AUTHOR."
  "SELECT book
FROM books_authors_link al
LEFT JOIN authors a ON al.author = a.id
WHERE a.name %s")

(calibre-db--search-function tag
  "Return the id's of books tagged with TAG."
  "SELECT book
FROM books_tags_link tl
LEFT JOIN tags t ON tl.tag = t.id
WHERE t.name %s")

(calibre-db--search-function publisher
  "Return the id's of books published by PUBLISHER."
  "SELECT book
FROM books_publishers_link pl
LEFT JOIN publishers p ON pl.publisher = p.id
WHERE p.name %s")

(calibre-db--search-function series
  "Return the id's of books that are part of SERIES."
  "SELECT book
FROM books_series_link sl
LEFT JOIN series s ON sl.series = s.id
WHERE s.name %s")

(calibre-db--search-function format
  "Return the id's of books available in FORMAT."
  "SELECT book
FROM data
WHERE format %s")

(provide 'calibre-db)
;;; calibre-db.el ends here

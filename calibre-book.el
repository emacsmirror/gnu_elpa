;;; calibre-book.el --- Create and manage Lisp representations of books -*- lexical-binding:t -*-

;; Copyright (C) 2023  Free Software Foundation, Inc.

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
;; Functions for creating and accessing the values of Lisp
;; representations of books

;;; Code:
(require 'parse-time)

(cl-defstruct calibre-book
  (id nil
      :read-only t
      :type integer
      :documentation "The ID of the book in the Calibre database.")
  (title nil
         :type string
         :documentation "The title of the book.")
  (authors nil
           :type list
           :documentation "The authors of the book.")
  (publisher nil
             :type (or string null)
             :documentation "The publisher of the book.")
  (series nil
          :type (or string null)
          :documentation "The series the book is a part of.")
  (series-index 1
                :type number
                :documentation "The book's position within its series.")
  (formats nil
           :read-only t
           :type list
           :documentation "The formats the book is available in.")
  (timestamp nil
             :read-only t
             :type list)
  (pubdate nil
           :type list
           :documentation "The book's publication date.")
  (last-modified nil
                 :type list
                 :documentation "The last time the book was modified.")
  (tags nil
        :type list
        :documentation "Tags associated with the book.")
  (path nil
        :type string
        :documentation "The book's position within the library")
  (file-name nil
             :type string
             :documentation "The book's filename, sans extension."))

(defcustom calibre-format-preferences '(pdf epub)
  "The preference order of file formats."
  :type '(repeat :tag "Format" symbol)
  :package-version '("calibre" . "0.1.0")
  :group 'calibre)

(defun calibre-book--pick-format (book)
  "Return the preferred format for BOOK."
  (let ((pref (seq-intersection calibre-format-preferences
                                (calibre-book-formats book))))
    (car (if pref pref (calibre-book-formats book)))))

(defun calibre-book-sort-by-series (a b)
  "Return t if A should appear before B when sorting by series."
  (if (not (calibre-book-series a))
      t
    (if (not (calibre-book-series b))
        nil
      (let ((v (compare-strings (calibre-book-series a) nil nil
                                (calibre-book-series b) nil nil)))
        (if (numberp v)
            (< v 0)
          (< (calibre-book-series-index a)
             (calibre-book-series-index b)))))))

(provide 'calibre-book)
;;; calibre-book.el ends here

;;; calibre-info.el --- View details about a particular book -*- lexical-binding:t -*-

;; Copyright (C) 2023-2025  Free Software Foundation, Inc.

;; Author: Kjartan Oli Agustsson <kjartanoli@disroot.org>
;; Maintainer: Kjartan Oli Agustsson <kjartanoli@disroot.org>

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

;;; Commentary:
;; View detailed information about a particular book.

;;; Code:
(require 'calibre-book)
(require 'calibre-core)

(declare-function calibre-library-book-at-point "calibre-library" ())

(defvar-local calibre-info-buffer-book nil)

(define-derived-mode calibre-info-mode special-mode
  "Book Information"
  (setq-local revert-buffer-function #'calibre-info-revert))

(defun calibre-info-render-book (book buffer)
  "Render BOOK information into BUFFER."
  (with-current-buffer buffer
    (calibre-info-mode)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert "Title: " (calibre-book-title book) "\n")
      (insert "Authors:\n")
      (dolist (author (calibre-book-authors book))
        (insert " " author "\n"))
      (when (calibre-book-publisher book)
        (insert "Publisher: " (calibre-book-publisher book) "\n"))
      (insert "Published: "
              (if (calibre-book-pubdate book)
                  (format-time-string calibre-library-time-format
                                      (calibre-book-pubdate book))
                "Invalid")
              "\n")
      (insert "Last modified: "
              (if (calibre-book-last-modified book)
                  (format-time-string calibre-library-time-format
                                      (calibre-book-last-modified book))
                "Invalid")
              "\n")
      (when (calibre-book-series book)
        (insert "Series: " (calibre-book-series book)
                " (book " (number-to-string
                           (calibre-book-series-index book))
                ")\n"))
      (when (calibre-book-tags book)
        (insert "Tags:\n")
        (dolist (tag (calibre-book-tags book))
          (insert " " tag "\n")))
      (insert "\n")
      (insert "Formats:\n")
      (dolist (format (calibre-book-formats book))
        (insert " " (upcase (symbol-name format)) "\n"))
      (when (calibre-book-summary book)
        (insert "\n")
        (insert
         (with-temp-buffer
           (insert (calibre-book-summary book))
           (goto-char (point-min))
           (while (not (eobp))
             (fill-paragraph)
             (forward-paragraph))
           (buffer-substring (point-min) (point-max))))))))

(defun calibre-info-revert (_ignore-auto _noconfirm)
  "Revert the current information buffer."
  (calibre-info-render-book calibre-info-buffer-book
                            (current-buffer)))

(defun calibre-info-view-book (book)
  "Display information about BOOK."
  (interactive (list (calibre-library-book-at-point))
               calibre-library-mode)
  (let ((buffer (get-buffer-create (format "*%s*" (calibre-book-title book)))))
    (with-current-buffer buffer
      (setq-local calibre-info-buffer-book book))
    (calibre-info-render-book book buffer)
    (display-buffer buffer)))

(provide 'calibre-info)
;;; calibre-info.el ends here

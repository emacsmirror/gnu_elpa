;;; isbn.el --- ISBN library for Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Maximilian Cook

;; Created: 05 Mar 2026

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see
;; <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Manage ISBNs via elisp.

;;; Code:

(require 'seq)
(eval-when-compile (require 'subr-x))

(defgroup isbn nil
  "ISBN library for Emacs."
  :prefix "isbn-"
  :group 'data)

;;;; Conversion

(defun isbn-convert (isbn &optional format)
  "Convert ISBN between 10- and 13-digit formats.

Optional argument FORMAT (integer: 10, 13) forces conversion to a
specified format."
  (setq isbn (string-replace "-" "" isbn))
  ;; The algorithms are taken from
  ;; <https://www.isbnservices.com/convert-isbn-10-to-isbn-13/>.
  (pcase (length isbn)
    (10
     (if (eq format 10) isbn
       (let* ((isbn (concat "978" (substring isbn 0 -1)))
              (check (- 10
                        (mod
                         (apply #'+
                                (seq-map-indexed
                                 (lambda (digit index)
                                   (* (string-to-number (char-to-string digit))
                                      (if (evenp index) 1 3)))
                                 isbn))
                         10))))
         (format "%s%i" isbn check))))
    (13
     (if (eq format 13) isbn
       (unless (string-prefix-p "978" isbn)
         (user-error "ISBN does not start with `978'"))
       (let* ((isbn (string-remove-prefix "978" isbn))
              (check (mod
                      (apply #'+
                             (seq-map-indexed
                              (lambda (digit index)
                                (* (string-to-number (char-to-string digit)) (1+ index)))
                              (substring isbn 0 -1)))
                      11)))
         (concat isbn (if (eq check 10) "X" (int-to-string check))))))
    (_ (user-error "ISBN is not 10 or 13 digits"))))

;;;; Ranges

;; TODO: Add the option to work with an offline source.
(defvar isbn-ranges-source
  "https://isbnbarcode.org/api/isbn-ranges.json")

;; Cache the ranges so that nested operations (hyphenation ->
;; validation) do not request the ranges needlessly.
(defvar isbn-ranges nil
  "All valid ISBN ranges the International ISBN Agency.")

(defun isbn-ranges ()
  "Return a hash table of all valid ISBN ranges."
  (or
   isbn-ranges
   (with-current-buffer
       (url-retrieve-synchronously
        isbn-ranges-source :silent :inhibit-cookies 5)
     (json-parse-buffer))))

(provide 'isbn)

;;; isbn.el ends here

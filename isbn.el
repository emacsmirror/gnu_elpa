;;; isbn.el --- ISBN library for Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Maximilian Cook

;; Author: Maximilian Cook <mcookly@fclr.net>
;; Created: 05 Mar 2026
;; Version: 0.1.0
;; Keywords: data
;; URL: https://codeberg.org/mcookly/isbn.el

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see
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

;;;; Core

(defun isbn--list->string (isbn)
  "Convert ISBN (list) to a string."
  (mapconcat
   (lambda (digit)
     (if (eq digit 10) "X"
       (int-to-string digit)))
   isbn))

(defun isbn--string->list (isbn)
  "Convert ISBN (string) to a list of numbers."
  (mapcar
   (lambda (digit)
     (if (eq digit 88) 10
       (string-to-number (char-to-string digit))))
   (string-replace "-" "" isbn)))

;;;; Validation

;;;###autoload
(defun isbn-valid-p (isbn)
  "Return t if ISBN is valid, otherwise nil."
  (unless (string-match-p (rx (not (any digit ?X ?x ?-))) isbn)
    (setq isbn (isbn--string->list isbn))
    (pcase (length isbn)
      (10 (thread-first
            (apply #'+
                   (seq-map-indexed
                    (lambda (digit index) (* digit (1+ index)))
                    isbn))
            (mod 11) (eq 0)))
      (13 (thread-first
            (apply #'+
                   (seq-map-indexed
                    (lambda (digit index) (* digit (if (evenp index) 1 3)))
                    (butlast isbn)))
            (+ (car (last isbn)))
            (mod 10) (eq 0))))))

;;;###autoload
(defun isbn-isbn10-p (isbn)
  "Return t if ISBN is matches the ISBN-10 format."
  (and
   (isbn-valid-p isbn)
   (eq (length (isbn--string->list isbn)) 10)))

;;;###autoload
(defun isbn-isbn13-p (isbn)
  "Return t if ISBN is matches the ISBN-13 format."
  (and
   (isbn-valid-p isbn)
   (eq (length (isbn--string->list isbn)) 13)))

;;;; Conversion

;;;###autoload
(defun isbn-convert (isbn &optional format)
  "Convert ISBN between 10- and 13-digit formats.

Optional argument FORMAT (integer: 10, 13) forces conversion to a
specified format."
  (unless (isbn-valid-p isbn)
    (user-error "ISBN is invalid"))
  (setq isbn (string-replace "-" "" isbn))
  ;; The algorithms are taken from
  ;; <https://www.isbnservices.com/convert-isbn-10-to-isbn-13/>.
  (pcase isbn
    ((pred isbn-isbn10-p)
     (if (eq format 10) isbn
       (let* ((isbn (concat "978" (substring isbn 0 -1)))
              (checkint (seq-map-indexed
                         (lambda (digit index) (* digit (if (evenp index) 1 3)))
                         (isbn--string->list isbn)))
              (check (- 10 (mod (apply #'+ checkint) 10))))
         (format "%s%i" isbn check))))
    ((pred isbn-isbn13-p)
     (if (eq format 13) isbn
       (unless (string-prefix-p "978" isbn)
         (user-error "ISBN does not start with `978'"))
       (let* ((isbn (string-remove-prefix "978" isbn))
              (checkint (seq-map-indexed
                         (lambda (digit index) (* digit (1+ index)))
                         (isbn--string->list (substring isbn 0 -1))))
              (check (mod (apply #'+ checkint) 11)))
         (concat isbn (if (eq check 10) "X" (int-to-string check))))))
    (_ (user-error "ISBN is not 10 or 13 digits"))))

;;;; Hyphenation

;; TODO: Add the option to work with an offline source.
(defvar isbn-hyphenation-ranges-source
  "https://isbnbarcode.org/api/isbn-ranges.json")

;; Cache the ranges so that nested operations (hyphenation ->
;; validation) do not request the ranges needlessly; they do not
;; update that often.
(defvar isbn-hyphenation-ranges-cache nil
  "Cache of all valid ISBN ranges from the International ISBN Agency.")

(defun isbn-hyphenation--ranges ()
  "Return a hash table of all valid ISBN hyphenation ranges.
Results are cached in `isbn-hyphenation-ranges-cache'."
  (or
   isbn-hyphenation-ranges-cache
   (setq isbn-hyphenation-ranges-cache
         (with-current-buffer
             (or (url-retrieve-synchronously
                  isbn-hyphenation-ranges-source nil :inhibit-cookies 5)
                 (error "Failed to retrieve hyphenation data"))
           (json-parse-buffer)))))

(defun isbn-hyphenation--ranges-get (source &rest keys)
  "Recursively retrieve KEYS from SOURCE."
  (if keys
      (apply #'isbn-hyphenation--ranges-get (gethash (pop keys) source) keys)
    source))

(defun isbn-hyphenation--ranges-member-p (number range)
  "Return t if NUMBER (string) is within RANGE (string)."
  (setq number (string-to-number number))
  (let ((range (mapcar #'string-to-number (string-split range "-"))))
    (and (>= number (car range)) (<= number (cadr range)))))

;;;###autoload
(defun isbn-hyphenate (isbn)
  "Hyphenate ISBN as an ISBN-13."
  (setq isbn (isbn-convert isbn 13))
  (let* ((isbn-hyphenation-ranges (isbn-hyphenation--ranges))
         hyphenated
         (remaining (lambda ()
                      (let ((len (length (apply #'concat hyphenated))))
                        (substring isbn len (+ len 7))))))
    ;; The data from the International ISBN Agency comes in two sets:
    ;; the EAN prefix (978 or 979) and the registration groups.  The
    ;; former determines how long the group identifier is, and the
    ;; latter determines how long the publisher code is.  The rest of
    ;; the ISBN can be determined automatically.

    ;; 1. EAN prefix
    (push (substring isbn 0 3) hyphenated)

    ;; 2. Group identifier
    (condition-case-unless-debug nil
        (let* ((rem (funcall remaining))
               (match (catch 'res
                        (seq-do
                         (lambda (pref)
                           (when (string= (car hyphenated) (gethash "Prefix" pref))
                             (throw 'res pref)))
                         (isbn-hyphenation--ranges-get
                          isbn-hyphenation-ranges
                          "ISBNRangeMessage" "EAN.UCCPrefixes" "EAN.UCC"))))
               (length (string-to-number
                        (catch 'length
                          (seq-do
                           (lambda (rule)
                             (when (isbn-hyphenation--ranges-member-p
                                    rem (gethash "Range" rule))
                               (throw 'length (gethash "Length" rule))))
                           (isbn-hyphenation--ranges-get match "Rules" "Rule"))))))
          (unless (eq length 0)
            (push (substring rem 0 length) hyphenated)))
      (error (error "Could not determine group identifier")))

    ;; 3. Publisher code
    (condition-case-unless-debug nil
        (let* ((prefix (string-join (reverse hyphenated) "-"))
               (rem (funcall remaining))
               (match (catch 'res
                        (seq-do (lambda (pref)
                                  (when (string= prefix (gethash "Prefix" pref))
                                    (throw 'res pref)))
                                (isbn-hyphenation--ranges-get
                                 isbn-hyphenation-ranges
                                 "ISBNRangeMessage" "RegistrationGroups" "Group"))))
               (length (string-to-number
                        (catch 'length
                          (seq-do
                           (lambda (rule)
                             (when (isbn-hyphenation--ranges-member-p
                                    rem (gethash "Range" rule))
                               (throw 'length (gethash "Length" rule))))
                           (isbn-hyphenation--ranges-get match "Rules" "Rule"))))))
          (push (substring rem 0 length) hyphenated))
      (error (error "Could not determine publisher code")))

    ;; 4. Title identifier
    (push (substring isbn (length (string-join hyphenated)) -1) hyphenated)

    ;; 5. Check digit
    (push (substring isbn -1) hyphenated)

    (string-join (reverse hyphenated) "-")))

(provide 'isbn)

;;; isbn.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

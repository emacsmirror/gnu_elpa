;;; isbn-tests.el --- Tests for isbn  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Maximilian Cook

;; This file is NOT part of GNU Emacs.

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

;;; Code:

(require 'ert)
(require 'ert-x)
(require 'isbn)

;;;; Core

(ert-deftest isbn-tests-list<->string ()
  "Test `isbn--list->string' and `isbn--string->list'."

  ;; `isbn--list->string'
  (should (string= (isbn--list->string '(0 1 2 3 4 5 6 7 8 9 10)) "0123456789X"))

  ;; `isbn--string->list' (case-insensitivity is important)
  (should (equal (isbn--string->list "0123456789x") '(0 1 2 3 4 5 6 7 8 9 10)))
  (should (equal (isbn--string->list "0123456789X") '(0 1 2 3 4 5 6 7 8 9 10))))

;;;; API

;;;;; Conversion

(ert-deftest isbn-tests-convert ()
  "Test `isbn-convert'."
  (should (string= (isbn-convert "0983159289")       "9780983159285"))
  (should (string= (isbn-convert "9780983159285")    "0983159289"))
  (should (string= (isbn-convert "0983159289"    10) "0983159289"))
  (should (string= (isbn-convert "9780983159285" 13) "9780983159285")))

;;;;; Hyphenation

(ert-deftest isbn-tests-hyphenation-ranges-cache ()
  "Test `isbn-hyphenation-ranges-update' \
and `isbn-hyphenation-ranges-maybe-prompt-update'."
  (ert-with-temp-directory dir
    (let ((isbn-hyphenation-ranges-cache-file (file-name-concat dir "cache")))

      ;; `isbn-hyphenation-ranges-update'
      (isbn-hyphenation-ranges-update)
      (should (file-exists-p isbn-hyphenation-ranges-cache-file))
      (should isbn-hyphenation-ranges-cache)

      ;; `isbn-hyphenation-ranges-maybe-prompt-update'
      (let ((inhibit-interaction t))
        (should-not (isbn-hyphenation-ranges-maybe-prompt-update))
        (should     (isbn-hyphenation-ranges-maybe-prompt-update 0))))))

(ert-deftest isbn-tests-hyphenate ()
  "Test `isbn-hyphenate'."
  (unless (libxml-available-p)
    (ert-skip "libxml2 support is not available"))
  (let ((isbn-hyphenation-ranges-cache-file nil)
        isbn-hyphenation-ranges-cache)
    (should-error (isbn-hyphenate "9780983159285")))
  (should (string= (isbn-hyphenate "9780983159285") "978-0-9831592-8-5")))

;;;;; Validation

(ert-deftest isbn-tests-valid-p ()
  "Test `isbn-valid-p'."

  ;; ISBN-10
  (should     (isbn-valid-p "0983159289")) ; valid
  (should-not (isbn-valid-p "0983159280")) ; invalid

  ;; ISBN-13
  (should     (isbn-valid-p "9780983159285"))      ; valid
  (should-not (isbn-valid-p "9780983159280")))     ; invalid

;;; isbn-tests.el ends here

;;; isbn-test.el --- Tests for isbn  -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'isbn)

;; To prevent spamming servers, cache the ranges for testing.
(isbn-hyphenation-ranges-update :nofile)

;;;; Core

(ert-deftest isbn-test:list->string ()
  "Test `isbn--list->string'."
  (should (string= (isbn--list->string '(0 1 2 3 4 5 6 7 8 9 10)) "0123456789X")))

(ert-deftest isbn-test:string->list ()
  "Test `isbn--string->list'."
  ;; Case-insensitivity is important.
  (should (equal (isbn--string->list "0123456789x") '(0 1 2 3 4 5 6 7 8 9 10)))
  (should (equal (isbn--string->list "0123456789X") '(0 1 2 3 4 5 6 7 8 9 10))))

;;;; API

;;;;; Conversion

(ert-deftest isbn-test:convert ()
  "Test `isbn-convert'."
  (should (string= (isbn-convert "0983159289")       "9780983159285"))
  (should (string= (isbn-convert "9780983159285")    "0983159289"))
  (should (string= (isbn-convert "0983159289"    10) "0983159289"))
  (should (string= (isbn-convert "9780983159285" 13) "9780983159285")))

;;;;; Hyphenation

;; TODO: Fix file check.
(ert-deftest isbn-test:hyphenation-ranges-update ()
  "Test `isbn-hyphenation-ranges-update'."
  (let (isbn-hyphenation-ranges-cache)
    (isbn-hyphenation-ranges-update :nofile)
    (should isbn-hyphenation-ranges-cache)))

(ert-deftest isbn-test:hyphenate ()
  "Test `isbn-hyphenate'."
  (unless (libxml-available-p)
    (ert-skip "libxml2 support is not available"))
  (let ((isbn-hyphenation-ranges-cache-file nil)
        isbn-hyphenation-ranges-cache)
    (should-error (isbn-hyphenate "9780983159285")))
  (should (string= (isbn-hyphenate "9780983159285") "978-0-9831592-8-5")))

;;;;; Validation

(ert-deftest isbn-test:valid-p ()
  "Test `isbn-valid-p'."

  ;; ISBN-10
  (should     (isbn-valid-p "0983159289")) ; valid
  (should-not (isbn-valid-p "0983159280")) ; invalid

  ;; ISBN-13
  (should     (isbn-valid-p "9780983159285"))      ; valid
  (should-not (isbn-valid-p "9780983159280")))     ; invalid

;;; isbn-test.el ends here

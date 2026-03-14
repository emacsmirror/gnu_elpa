;;; isbn-test.el --- Tests for isbn  -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'ert-x)
(require 'isbn)

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

(ert-deftest isbn-test:convert ()
  "Test `isbn-convert'."
  (should (string= (isbn-convert "0983159289")       "9780983159285"))
  (should (string= (isbn-convert "9780983159285")    "0983159289"))
  (should (string= (isbn-convert "0983159289"    10) "0983159289"))
  (should (string= (isbn-convert "9780983159285" 13) "9780983159285")))

(ert-deftest isbn-test:hyphenate ()
  "Test `isbn-hyphenate'."
  (ert-with-message-capture msgs
    (isbn-hyphenation--ranges)
    (should isbn-hyphenation-ranges-cache)
    (should (string-suffix-p "done\n" msgs)))
  (should (string= (isbn-hyphenate "9780983159285") "978-0-9831592-8-5")))

(ert-deftest isbn-test:valid-p ()
  "Test `isbn-valid-p'."

  ;; ISBN-10
  (should (eq (isbn-valid-p "0983159289") t))   ; valid
  (should (eq (isbn-valid-p "0983159280") nil)) ; invalid

  ;; ISBN-13
  (should (eq (isbn-valid-p "9780983159285") t))    ; valid
  (should (eq (isbn-valid-p "9780983159280") nil))) ; invalid

;;; isbn-test.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

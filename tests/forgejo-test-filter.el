;;; forgejo-test-filter.el --- Tests for forgejo-filter  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for filter parsing, serialization, entries formatting,
;; and notification matching.

;;; Code:

(require 'ert)
(require 'cl-lib)

(load (expand-file-name "../lisp/forgejo.el"
       (file-name-directory (or load-file-name buffer-file-name))))
(load (expand-file-name "../lisp/forgejo-tl.el"
       (file-name-directory (or load-file-name buffer-file-name))))
(load (expand-file-name "../lisp/forgejo-api.el"
       (file-name-directory (or load-file-name buffer-file-name))))
(load (expand-file-name "../lisp/forgejo-buffer.el"
       (file-name-directory (or load-file-name buffer-file-name))))
(load (expand-file-name "../lisp/forgejo-filter.el"
       (file-name-directory (or load-file-name buffer-file-name))))

;;; Group 1: Parse

(ert-deftest forgejo-test-filter-parse-state ()
  "Parse state prefix."
  (let ((result (forgejo-filter-parse "state:open")))
    (should (equal (plist-get result :state) "open"))))

(ert-deftest forgejo-test-filter-parse-label ()
  "Parse label prefix into :labels key."
  (let ((result (forgejo-filter-parse "label:bug")))
    (should (equal (plist-get result :labels) "bug"))))

(ert-deftest forgejo-test-filter-parse-multiple ()
  "Parse multiple prefixes."
  (let ((result (forgejo-filter-parse "state:open label:bug author:alice")))
    (should (equal (plist-get result :state) "open"))
    (should (equal (plist-get result :labels) "bug"))
    (should (equal (plist-get result :author) "alice"))))

(ert-deftest forgejo-test-filter-parse-bare-words ()
  "Bare words become :query."
  (let ((result (forgejo-filter-parse "fix typo")))
    (should (equal (plist-get result :query) "fix typo"))))

(ert-deftest forgejo-test-filter-parse-mixed ()
  "Mix of prefixes and bare words."
  (let ((result (forgejo-filter-parse "state:open fix typo")))
    (should (equal (plist-get result :state) "open"))
    (should (equal (plist-get result :query) "fix typo"))))

(ert-deftest forgejo-test-filter-parse-empty ()
  "Empty string returns nil."
  (should-not (forgejo-filter-parse ""))
  (should-not (forgejo-filter-parse nil)))

(ert-deftest forgejo-test-filter-parse-empty-value ()
  "Prefix with empty value is ignored."
  (should-not (forgejo-filter-parse "state:")))

(ert-deftest forgejo-test-filter-parse-search-prefix ()
  "The search: prefix maps to :query."
  (let ((result (forgejo-filter-parse "search:something")))
    (should (equal (plist-get result :query) "something"))))

(ert-deftest forgejo-test-filter-parse-author ()
  "The author: prefix maps to :author."
  (let ((result (forgejo-filter-parse "author:alice")))
    (should (equal (plist-get result :author) "alice"))))

;;; Group 2: Serialize

(ert-deftest forgejo-test-filter-serialize-state ()
  "Serialize state filter."
  (should (string= (forgejo-filter-serialize '(:state "open"))
                    "state:open")))

(ert-deftest forgejo-test-filter-serialize-multiple ()
  "Serialize multiple filters."
  (let ((result (forgejo-filter-serialize
                 '(:state "open" :labels "bug"))))
    (should (string-match-p "state:open" result))
    (should (string-match-p "label:bug" result))))

(ert-deftest forgejo-test-filter-serialize-with-query ()
  "Serialize with bare-word query."
  (let ((result (forgejo-filter-serialize
                 '(:state "open" :query "fix typo"))))
    (should (string-match-p "state:open" result))
    (should (string-match-p "fix typo" result))))

(ert-deftest forgejo-test-filter-serialize-empty ()
  "Serialize nil returns empty string."
  (should (string= (forgejo-filter-serialize nil) "")))

;;; Group 3: Round-trip

(ert-deftest forgejo-test-filter-round-trip ()
  "Parse then serialize produces equivalent query."
  (let* ((original "state:open label:bug")
         (parsed (forgejo-filter-parse original))
         (serialized (forgejo-filter-serialize parsed))
         (reparsed (forgejo-filter-parse serialized)))
    (should (equal (plist-get parsed :state) (plist-get reparsed :state)))
    (should (equal (plist-get parsed :labels) (plist-get reparsed :labels)))))

;;; Group 4: List entries

(ert-deftest forgejo-test-filter-list-entries ()
  "Convert API alists to tabulated-list entries."
  (let* ((items `(((number . 42)
                   (state . "open")
                   (title . "Test issue")
                   (labels . (((name . "bug") (color . "ff0000"))))
                   (user . ((login . "alice")))
                   (updated_at . "2020-06-15T10:00:00Z"))))
         (entries (forgejo-filter-list-entries items)))
    (should (= (length entries) 1))
    (let ((entry (car entries)))
      (should (= (car entry) 42))
      (let ((cols (cadr entry)))
        (should (string= (aref cols 0) "42"))
        (should (string= (aref cols 2) "Test issue"))
        (should (string-match-p "alice" (aref cols 4)))))))

(provide 'forgejo-test-filter)
;;; forgejo-test-filter.el ends here

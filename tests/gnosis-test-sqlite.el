;;; gnosis-test-sqlite.el --- Tests for gnosis-sqlite  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Free Software Foundation, Inc.

;; Author: Thanos Apollo <public@thanosapollo.org>

;;; Commentary:

;; Tests for the gnosis-sqlite built-in SQLite backend.
;; Verifies encoding/decoding compatibility with emacsql format,
;; expression compiler, schema compiler, and integration.

;;; Code:
(require 'ert)

(let ((parent-dir (file-name-directory
                   (directory-file-name
                    (file-name-directory (or load-file-name default-directory))))))
  (add-to-list 'load-path parent-dir))

(require 'gnosis-sqlite)
(require 'gnosis-algorithm)

;; Load gnosis-db--schemata for schema tests
(defvar gnosis-db--schemata)
(let ((gnosis-el (expand-file-name "../gnosis.el"
                   (file-name-directory (or load-file-name default-directory)))))
  (with-temp-buffer
    (insert-file-contents gnosis-el)
    (goto-char (point-min))
    (when (re-search-forward "(defconst gnosis-db--schemata" nil t)
      (goto-char (match-beginning 0))
      (eval (read (current-buffer))))))

;;; ---- Group 1: Encode/Decode round-trips ----

(ert-deftest gnosis-test-sqlite-encode-nil ()
  "nil encodes as nil (SQL NULL)."
  (should (eq (gnosis-sqlite--encode-param nil) nil)))

(ert-deftest gnosis-test-sqlite-encode-integer ()
  "Integers pass through unchanged."
  (should (equal (gnosis-sqlite--encode-param 42) 42)))

(ert-deftest gnosis-test-sqlite-encode-float ()
  "Floats pass through unchanged."
  (should (equal (gnosis-sqlite--encode-param 3.14) 3.14)))

(ert-deftest gnosis-test-sqlite-encode-string ()
  "Strings are prin1-encoded (matching emacsql)."
  (should (equal (gnosis-sqlite--encode-param "hello") "\"hello\"")))

(ert-deftest gnosis-test-sqlite-encode-symbol ()
  "Symbols are prin1-encoded."
  (should (equal (gnosis-sqlite--encode-param 'foo) "foo")))

(ert-deftest gnosis-test-sqlite-encode-list ()
  "Lists are prin1-encoded."
  (should (equal (gnosis-sqlite--encode-param '("a" "b")) "(\"a\" \"b\")")))

(ert-deftest gnosis-test-sqlite-encode-nested-list ()
  "Nested lists are prin1-encoded."
  (should (equal (gnosis-sqlite--encode-param '(1 (2 3))) "(1 (2 3))")))

(ert-deftest gnosis-test-sqlite-decode-nil ()
  "nil decodes as nil."
  (should (eq (gnosis-sqlite--decode nil) nil)))

(ert-deftest gnosis-test-sqlite-decode-integer ()
  "Integers decode unchanged."
  (should (equal (gnosis-sqlite--decode 42) 42)))

(ert-deftest gnosis-test-sqlite-decode-float ()
  "Floats decode unchanged."
  (should (equal (gnosis-sqlite--decode 3.14) 3.14)))

(ert-deftest gnosis-test-sqlite-decode-string ()
  "Quoted strings decode to unquoted strings."
  (should (equal (gnosis-sqlite--decode "\"hello\"") "hello")))

(ert-deftest gnosis-test-sqlite-decode-empty-string ()
  "Empty string decodes to empty string."
  (should (equal (gnosis-sqlite--decode "") "")))

(ert-deftest gnosis-test-sqlite-decode-symbol ()
  "Symbol names decode to symbols."
  (should (equal (gnosis-sqlite--decode "foo") 'foo)))

(ert-deftest gnosis-test-sqlite-decode-list ()
  "Printed lists decode back to lists."
  (should (equal (gnosis-sqlite--decode "(\"a\" \"b\")") '("a" "b"))))

(ert-deftest gnosis-test-sqlite-decode-nested-list ()
  "Printed nested lists decode correctly."
  (should (equal (gnosis-sqlite--decode "(1 (2 3))") '(1 (2 3)))))

(ert-deftest gnosis-test-sqlite-roundtrip-string ()
  "Encode then decode a string is identity."
  (let ((val "test string"))
    (should (equal (gnosis-sqlite--decode (gnosis-sqlite--encode-param val)) val))))

(ert-deftest gnosis-test-sqlite-roundtrip-list ()
  "Encode then decode a list is identity."
  (let ((val '("tag1" "tag2")))
    (should (equal (gnosis-sqlite--decode (gnosis-sqlite--encode-param val)) val))))

(ert-deftest gnosis-test-sqlite-roundtrip-date ()
  "Encode then decode a date list is identity."
  (let ((val (gnosis-algorithm-date)))
    (should (equal (gnosis-sqlite--decode (gnosis-sqlite--encode-param val)) val))))

(ert-deftest gnosis-test-sqlite-roundtrip-gnosis-value ()
  "Encode then decode gnosis algorithm value is identity."
  (let ((val gnosis-algorithm-gnosis-value))
    (should (equal (gnosis-sqlite--decode (gnosis-sqlite--encode-param val)) val))))

;;; ---- Group 2: Identifier conversion ----

(ert-deftest gnosis-test-sqlite-ident-simple ()
  "Simple symbol converts correctly."
  (should (equal (gnosis-sqlite--ident 'name) "name")))

(ert-deftest gnosis-test-sqlite-ident-dash ()
  "Dashes become underscores."
  (should (equal (gnosis-sqlite--ident 'deck-id) "deck_id")))

(ert-deftest gnosis-test-sqlite-ident-colon ()
  "Colons become dots (table:col -> table.col)."
  (should (equal (gnosis-sqlite--ident 'themata:id) "themata.id")))

(ert-deftest gnosis-test-sqlite-ident-review-log ()
  "review-log becomes review_log."
  (should (equal (gnosis-sqlite--ident 'review-log) "review_log")))

;;; ---- Group 3: Expression compiler ----

(ert-deftest gnosis-test-sqlite-expr-equal-literal ()
  "Literal number equality: (= 1 1)."
  (let ((result (gnosis-sqlite--compile-expr '(= 1 1))))
    (should (equal (car result) "1 = 1"))
    (should (null (cdr result)))))

(ert-deftest gnosis-test-sqlite-expr-equal-col-val ()
  "Column = value: (= id 42)."
  (let ((result (gnosis-sqlite--compile-expr '(= id 42))))
    (should (equal (car result) "id = ?"))
    (should (equal (cdr result) '(42)))))

(ert-deftest gnosis-test-sqlite-expr-equal-col-string ()
  "Column = string value: (= name \"test\").
Value is pre-encoded (prin1-to-string) in the compiler."
  (let ((result (gnosis-sqlite--compile-expr '(= name "test"))))
    (should (equal (car result) "name = ?"))
    (should (equal (cdr result) (list (prin1-to-string "test"))))))

(ert-deftest gnosis-test-sqlite-expr-equal-col-quoted ()
  "Column = quoted value: (= tags \\='(\"a\" \"b\")).
Value is pre-encoded (prin1-to-string) in the compiler."
  (let ((result (gnosis-sqlite--compile-expr '(= tags '("a" "b")))))
    (should (equal (car result) "tags = ?"))
    (should (equal (cdr result) (list (prin1-to-string '("a" "b")))))))

(ert-deftest gnosis-test-sqlite-expr-equal-col-col ()
  "Column = column: (= themata:id review-log:id)."
  (let ((result (gnosis-sqlite--compile-expr '(= themata:id review-log:id))))
    (should (equal (car result) "themata.id = review_log.id"))
    (should (null (cdr result)))))

(ert-deftest gnosis-test-sqlite-expr-and ()
  "AND expression: (and (= a 1) (= b 2))."
  (let ((result (gnosis-sqlite--compile-expr '(and (= a 1) (= b 2)))))
    (should (equal (car result) "(a = ?) AND (b = ?)"))
    (should (equal (cdr result) '(1 2)))))

(ert-deftest gnosis-test-sqlite-expr-or ()
  "OR expression: (or (= a 1) (= b 2))."
  (let ((result (gnosis-sqlite--compile-expr '(or (= a 1) (= b 2)))))
    (should (equal (car result) "(a = ?) OR (b = ?)"))
    (should (equal (cdr result) '(1 2)))))

(ert-deftest gnosis-test-sqlite-expr-like ()
  "LIKE expression: (like tags \"%test%\")."
  (let ((result (gnosis-sqlite--compile-expr '(like tags "%test%"))))
    (should (equal (car result) "tags LIKE ?"))
    (should (equal (cdr result) '("%test%")))))

(ert-deftest gnosis-test-sqlite-expr-like-quoted ()
  "LIKE with quoted value."
  (let ((result (gnosis-sqlite--compile-expr '(like tags '"%test%"))))
    (should (equal (car result) "tags LIKE ?"))
    (should (equal (cdr result) '("%test%")))))

(ert-deftest gnosis-test-sqlite-expr-in ()
  "IN expression with vector."
  (let ((result (gnosis-sqlite--compile-expr '(in id [1 2 3]))))
    (should (equal (car result) "id IN (?, ?, ?)"))
    (should (equal (cdr result) '(1 2 3)))))

(ert-deftest gnosis-test-sqlite-expr-gt ()
  "Greater than: (> n 5)."
  (let ((result (gnosis-sqlite--compile-expr '(> n 5))))
    (should (equal (car result) "n > ?"))
    (should (equal (cdr result) '(5)))))

(ert-deftest gnosis-test-sqlite-expr-lte ()
  "Less than or equal: (<= n 10)."
  (let ((result (gnosis-sqlite--compile-expr '(<= n 10))))
    (should (equal (car result) "n <= ?"))
    (should (equal (cdr result) '(10)))))

(ert-deftest gnosis-test-sqlite-expr-not ()
  "NOT expression."
  (let ((result (gnosis-sqlite--compile-expr '(not (= suspend 1)))))
    (should (equal (car result) "NOT (suspend = ?)"))
    (should (equal (cdr result) '(1)))))

(ert-deftest gnosis-test-sqlite-expr-subtract ()
  "Subtraction: (- 1 suspend)."
  (let ((result (gnosis-sqlite--compile-expr '(- 1 suspend))))
    (should (equal (car result) "1 - suspend"))
    (should (null (cdr result)))))

(ert-deftest gnosis-test-sqlite-expr-complex ()
  "Complex nested expression."
  (let ((result (gnosis-sqlite--compile-expr
                 '(and (> n 0) (= suspend 0)))))
    (should (equal (car result) "(n > ?) AND (suspend = ?)"))
    (should (equal (cdr result) '(0 0)))))

;;; ---- Group 4: Column compiler ----

(ert-deftest gnosis-test-sqlite-columns-star ()
  "Star compiles to *."
  (should (equal (gnosis-sqlite--compile-columns '*) "*")))

(ert-deftest gnosis-test-sqlite-columns-single ()
  "Single symbol compiles to identifier."
  (should (equal (gnosis-sqlite--compile-columns 'name) "name")))

(ert-deftest gnosis-test-sqlite-columns-vector ()
  "Vector of symbols compiles to comma-separated list."
  (should (equal (gnosis-sqlite--compile-columns '[id name deck-id])
                 "id, name, deck_id")))

;;; ---- Group 5: Schema compiler ----

(ert-deftest gnosis-test-sqlite-schema-decks ()
  "Compile decks schema."
  (let* ((schema (cadr (assq 'decks gnosis-db--schemata)))
         (result (gnosis-sqlite--compile-schema schema)))
    (should (string-match-p "id INTEGER PRIMARY KEY" result))
    (should (string-match-p "name TEXT NOT NULL" result))
    (should (string-match-p "UNIQUE (name)" result))))

(ert-deftest gnosis-test-sqlite-schema-themata ()
  "Compile themata schema."
  (let* ((schema (cadr (assq 'themata gnosis-db--schemata)))
         (result (gnosis-sqlite--compile-schema schema)))
    (should (string-match-p "id INTEGER PRIMARY KEY" result))
    (should (string-match-p "type TEXT NOT NULL" result))
    (should (string-match-p "tags TEXT DEFAULT untagged" result))
    (should (string-match-p "deck_id INTEGER NOT NULL" result))
    (should (string-match-p "FOREIGN KEY (deck_id) REFERENCES decks (id) ON DELETE CASCADE"
                            result))))

(ert-deftest gnosis-test-sqlite-schema-review-log ()
  "Compile review-log schema."
  (let* ((schema (cadr (assq 'review-log gnosis-db--schemata)))
         (result (gnosis-sqlite--compile-schema schema)))
    (should (string-match-p "id INTEGER PRIMARY KEY NOT NULL" result))
    (should (string-match-p "last_rev INTEGER NOT NULL" result))
    (should (string-match-p "suspend INTEGER NOT NULL" result))
    (should (string-match-p "FOREIGN KEY (id) REFERENCES themata (id) ON DELETE CASCADE"
                            result))))

(ert-deftest gnosis-test-sqlite-schema-thema-links ()
  "Compile thema-links schema."
  (let* ((schema (cadr (assq 'thema-links gnosis-db--schemata)))
         (result (gnosis-sqlite--compile-schema schema)))
    (should (string-match-p "source INTEGER" result))
    (should (string-match-p "dest TEXT" result))
    (should (string-match-p "FOREIGN KEY (source) REFERENCES themata (id) ON DELETE CASCADE"
                            result))
    (should (string-match-p "UNIQUE (source, dest)" result))))

(ert-deftest gnosis-test-sqlite-schema-all-tables ()
  "All schemata in gnosis-db--schemata compile without error."
  (pcase-dolist (`(,_table ,schema) gnosis-db--schemata)
    (should (stringp (gnosis-sqlite--compile-schema schema)))))

;;; ---- Group 6: Values compiler ----

(ert-deftest gnosis-test-sqlite-values-single-vector ()
  "Single vector compiles to (?, ?, ?) with encoded params."
  (let ((result (gnosis-sqlite--compile-values [1 "hello" nil])))
    (should (equal (car result) "(?, ?, ?)"))
    (should (equal (cdr result) '(1 "\"hello\"" nil)))))

;;; ---- Group 7: Integration tests ----

(ert-deftest gnosis-test-sqlite-integration-create-insert-select ()
  "Create table, insert, select round-trip."
  (let* ((db-file (make-temp-file "gnosis-sqlite-test-" nil ".db"))
         (db (gnosis-sqlite-open db-file)))
    (unwind-protect
        (progn
          (should (gnosis-sqlite-live-p db))
          ;; Create a simple table
          (gnosis-sqlite-execute db "CREATE TABLE test (id INTEGER PRIMARY KEY, name TEXT)")
          ;; Insert - gnosis-sqlite-execute encodes params automatically
          (gnosis-sqlite-execute db "INSERT INTO test VALUES (?, ?)"
                                 (list 1 "Alice"))
          (gnosis-sqlite-execute db "INSERT INTO test VALUES (?, ?)"
                                 (list 2 "Bob"))
          ;; Select all - gnosis-sqlite-select decodes results automatically
          (let ((rows (gnosis-sqlite-select db "SELECT * FROM test")))
            (should (equal (length rows) 2))
            (should (equal (car rows) '(1 "Alice")))
            (should (equal (cadr rows) '(2 "Bob"))))
          ;; Select with WHERE
          (let ((rows (gnosis-sqlite-select db "SELECT name FROM test WHERE id = ?"
                                            (list 1))))
            (should (equal (length rows) 1))
            (should (equal (caar rows) "Alice"))))
      (gnosis-sqlite-close db)
      (delete-file db-file))))

(ert-deftest gnosis-test-sqlite-integration-transaction-commit ()
  "Transaction commits on success."
  (let* ((db-file (make-temp-file "gnosis-sqlite-test-" nil ".db"))
         (db (gnosis-sqlite-open db-file)))
    (unwind-protect
        (progn
          (gnosis-sqlite-execute db "CREATE TABLE test (id INTEGER PRIMARY KEY)")
          (gnosis-sqlite-with-transaction db
            (gnosis-sqlite-execute db "INSERT INTO test VALUES (?)" '(1))
            (gnosis-sqlite-execute db "INSERT INTO test VALUES (?)" '(2)))
          (let ((rows (gnosis-sqlite-select db "SELECT * FROM test")))
            (should (equal (length rows) 2))))
      (gnosis-sqlite-close db)
      (delete-file db-file))))

(ert-deftest gnosis-test-sqlite-integration-transaction-rollback ()
  "Transaction rolls back on error."
  (let* ((db-file (make-temp-file "gnosis-sqlite-test-" nil ".db"))
         (db (gnosis-sqlite-open db-file)))
    (unwind-protect
        (progn
          (gnosis-sqlite-execute db "CREATE TABLE test (id INTEGER PRIMARY KEY)")
          (condition-case nil
              (gnosis-sqlite-with-transaction db
                (gnosis-sqlite-execute db "INSERT INTO test VALUES (?)" '(1))
                (error "Deliberate error"))
            (error nil))
          (let ((rows (gnosis-sqlite-select db "SELECT * FROM test")))
            (should (equal (length rows) 0))))
      (gnosis-sqlite-close db)
      (delete-file db-file))))

(ert-deftest gnosis-test-sqlite-integration-nested-transaction ()
  "Nested transactions: only outermost issues BEGIN/COMMIT."
  (let* ((db-file (make-temp-file "gnosis-sqlite-test-" nil ".db"))
         (db (gnosis-sqlite-open db-file)))
    (unwind-protect
        (progn
          (gnosis-sqlite-execute db "CREATE TABLE test (id INTEGER PRIMARY KEY)")
          (gnosis-sqlite-with-transaction db
            (gnosis-sqlite-execute db "INSERT INTO test VALUES (?)" '(1))
            (gnosis-sqlite-with-transaction db
              (gnosis-sqlite-execute db "INSERT INTO test VALUES (?)" '(2))))
          (let ((rows (gnosis-sqlite-select db "SELECT * FROM test")))
            (should (equal (length rows) 2))))
      (gnosis-sqlite-close db)
      (delete-file db-file))))

(ert-deftest gnosis-test-sqlite-integration-schema ()
  "Create all gnosis tables from schemata."
  (let* ((db-file (make-temp-file "gnosis-sqlite-test-" nil ".db"))
         (db (gnosis-sqlite-open db-file)))
    (unwind-protect
        (progn
          (pcase-dolist (`(,table ,schema) gnosis-db--schemata)
            (let ((sql (format "CREATE TABLE %s (%s)"
                               (gnosis-sqlite--ident table)
                               (gnosis-sqlite--compile-schema schema))))
              (gnosis-sqlite-execute db sql)))
          ;; Verify tables exist
          (let ((tables (gnosis-sqlite-select
                         db "SELECT name FROM sqlite_master WHERE type = 'table'")))
            (should (>= (length tables) 7))))
      (gnosis-sqlite-close db)
      (delete-file db-file))))

(ert-deftest gnosis-test-sqlite-integration-list-values ()
  "Lists stored and retrieved correctly (emacsql format)."
  (let* ((db-file (make-temp-file "gnosis-sqlite-test-" nil ".db"))
         (db (gnosis-sqlite-open db-file)))
    (unwind-protect
        (progn
          (gnosis-sqlite-execute db "CREATE TABLE test (id INTEGER, data TEXT)")
          (let ((tags '("math" "physics")))
            ;; encode-param handles the prin1-to-string internally
            (gnosis-sqlite-execute db "INSERT INTO test VALUES (?, ?)"
                                   (list 1 tags))
            (let ((rows (gnosis-sqlite-select db "SELECT data FROM test WHERE id = ?"
                                              '(1))))
              (should (equal (caar rows) tags)))))
      (gnosis-sqlite-close db)
      (delete-file db-file))))

(ert-deftest gnosis-test-sqlite-integration-date-values ()
  "Date lists stored and retrieved correctly."
  (let* ((db-file (make-temp-file "gnosis-sqlite-test-" nil ".db"))
         (db (gnosis-sqlite-open db-file)))
    (unwind-protect
        (progn
          (gnosis-sqlite-execute db "CREATE TABLE test (id INTEGER, date TEXT)")
          (let ((date (gnosis-algorithm-date)))
            (gnosis-sqlite-execute db "INSERT INTO test VALUES (?, ?)"
                                   (list 1 date))
            (let ((rows (gnosis-sqlite-select db "SELECT date FROM test WHERE id = ?"
                                              '(1))))
              (should (equal (caar rows) date)))))
      (gnosis-sqlite-close db)
      (delete-file db-file))))

(provide 'gnosis-test-sqlite)

(ert-run-tests-batch-and-exit)
;;; gnosis-test-sqlite.el ends here

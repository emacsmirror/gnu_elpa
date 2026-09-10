;;; gnosis-test-release-schema.el --- Public release boundary -*- lexical-binding: t; -*-
(require 'ert)
(require 'gnosis-test-schema-v8)

(ert-deftest gnosis-release-schema-fresh-is-nine ()
  (gnosis-test-with-old-db
    (gnosis-db-init)
    (should (= 9 (gnosis--db-version)))
    (gnosis-db--check-schema gnosis-db 9)))

(ert-deftest gnosis-release-schema-eight-opens-directly-as-complete-nine ()
  (gnosis-test-with-old-db
    (gnosis-test--create-v8-schema)
    (gnosis-db-init)
    (should (= 9 (gnosis--db-version)))
    (dolist (table '(study-history study-session scheduler-active practice-events
                    review-voids practice-voids))
      (should (gnosis-table-exists-p table)))
    (should (assoc 6 (sqlite-select gnosis-db "PRAGMA table_info(themata)")))
    (gnosis-db--check-schema gnosis-db 9)))

(ert-deftest gnosis-release-schema-private-versions-are-refused ()
  (dolist (version '(10 11 12))
    (gnosis-test-with-old-db
      (gnosis-db-init)
      (gnosis--db-set-version version)
      (should-error (gnosis-db-init) :type 'user-error)
      (should (= version (gnosis--db-version))))))
(ert-deftest gnosis-release-schema-nine-must-have-the-complete-layout ()
  (gnosis-test-with-old-db
    (gnosis-db-init)
    (sqlite-execute gnosis-db "DROP TABLE study_history")
    (sqlite-execute gnosis-db "ALTER TABLE themata DROP COLUMN accepted_aliases")
    (let ((before (sqlite-select gnosis-db
                   "SELECT type, name, sql FROM sqlite_master ORDER BY type, name")))
      (should-error (gnosis-db-init))
      (should (= 9 (gnosis--db-version)))
      (should (equal before (sqlite-select gnosis-db
                             "SELECT type, name, sql FROM sqlite_master ORDER BY type, name"))))))

(provide 'gnosis-test-release-schema)

;;; gnosis-test-release-schema.el --- Public release boundary -*- lexical-binding: t; -*-
(require 'ert)
(require 'gnosis-test-schema-v8)
(require 'gnosis-fixture-schema-v9)
(require 'gnosis-test-db-safety)
(require 'gnosis-test-helpers)

(defun gnosis-test-release--reject-practice-ownership (replacement)
  "Refuse schema 9 and 10 with the ownership clause changed to REPLACEMENT."
  (dolist (version '(9 10))
    (gnosis-test-safety
      (let* ((file (expand-file-name "gnosis.db" gnosis-dir))
             (gnosis-fixture-v9-ddl
              (mapcar
               (lambda (sql)
                 (if (string-prefix-p "CREATE TABLE practice_events " sql)
                     (string-replace
                      "FOREIGN KEY (thema_id) REFERENCES themata (id) ON DELETE CASCADE, "
                      replacement sql)
                   sql))
               gnosis-fixture-v9-ddl)))
        (let ((gnosis-db (gnosis-sqlite-open file)))
          (unwind-protect
              (progn
                (gnosis-fixture-create-v9)
                (sqlite-execute gnosis-db
                  "INSERT INTO themata VALUES (101, 'basic', 'Question', '(nil)', '(answer)', NULL, NULL)")
                (sqlite-execute gnosis-db
                  "INSERT INTO practice_events VALUES ('legacy', 101, 'batch', 1, 1000000, 1)")
                (when (= version 10)
                  (sqlite-execute gnosis-db
                    "CREATE TABLE practice_encounters (event_id TEXT PRIMARY KEY NOT NULL, data TEXT NOT NULL, FOREIGN KEY (event_id) REFERENCES practice_events (event_id) ON DELETE CASCADE)")
                  (gnosis-db--create-encounter-guards gnosis-db)
                  (sqlite-execute gnosis-db
                    "INSERT INTO practice_encounters VALUES ('legacy', 'retained response')")
                  (sqlite-execute gnosis-db "PRAGMA user_version = 10")))
            (sqlite-close gnosis-db)))
        (let ((before (gnosis-test-safety-snapshot file)))
          (dotimes (_ 2)
            (should-error (gnosis--ensure-db))
            (should-not gnosis-db)
            (should (equal before (gnosis-test-safety-snapshot file)))))))))

(ert-deftest gnosis-release-schema-missing-practice-ownership-is-refused ()
  (gnosis-test-release--reject-practice-ownership ""))

(ert-deftest gnosis-release-schema-noncascading-practice-ownership-is-refused ()
  (gnosis-test-release--reject-practice-ownership
   "FOREIGN KEY (thema_id) REFERENCES themata (id) ON DELETE NO ACTION, "))

(ert-deftest gnosis-release-schema-migrated-practice-deletion-preserves-unrelated ()
  "Create, accept and delete after migration, including retained layout variants."
  (dolist (retained-layout '(nil t))
    (gnosis-test-with-old-db
      (let ((gnosis-fixture-v9-ddl
             (if (not retained-layout) gnosis-fixture-v9-ddl
               (mapcar
                (lambda (sql)
                  (cond
                   ((string-prefix-p "CREATE TABLE themata " sql)
                    (string-replace "accepted_aliases TEXT"
                                    "archived_at_us INTEGER, accepted_aliases TEXT" sql))
                   ((string-prefix-p "CREATE TABLE thema_tag " sql)
                    (string-replace "UNIQUE (thema_id, tag)"
                                    "PRIMARY KEY (thema_id, tag)" sql))
                   ((string-prefix-p "CREATE TABLE thema_links " sql)
                    (string-replace "source INTEGER" "source TEXT" sql))
                   (t sql)))
                gnosis-fixture-v9-ddl))))
        (gnosis-fixture-create-v9))
      (gnosis-add-thema-fields "basic" "Keep" '("Hint") '("Answer")
                               "Context" '("test") 0 nil nil 101)
      (gnosis--insert-into 'practice-events '(["legacy" 101 "old" 1 1000000 1]))
      (let ((before (nth 2 (gnosis-test-safety-snapshot gnosis-test--db-file)))
            (asset (expand-file-name "assets/shared/fixture.bin" gnosis-dir)))
        (make-directory (file-name-directory asset) t)
        (with-temp-file asset (insert "Shared fixture bytes"))
        (gnosis-db-init)
        (should (= 10 (gnosis--db-version)))
        (should (equal before
                       (assoc-delete-all
                        "practice_encounters"
                        (nth 2 (gnosis-test-safety-snapshot gnosis-test--db-file)))))
        (should-not (gnosis-select '* 'practice-encounters))
        (gnosis-study-accept-practice
         (gnosis-review--encounter
          '(:mode practice :event-id "keep" :thema-id 101 :session-id "new"
            :attempt 1 :reviewed-at-us 2000000 :outcome success)
          '("basic" "Keep" ("Hint") ("Answer") nil "Context" nil)
          '(:kind "text" :text "Answer") '("Hint") 0))
        (let ((before-delete (gnosis-test-safety-snapshot gnosis-test--db-file)))
          (gnosis-add-thema-fields "basic" "Delete" '("Hint") '("Answer")
                                   "Context" '("test") 0 nil nil 102)
          (gnosis-study-accept-practice
           (gnosis-review--encounter
            '(:mode practice :event-id "delete" :thema-id 102 :session-id "new"
              :attempt 2 :reviewed-at-us 3000000 :outcome success)
            '("basic" "Delete" ("Hint") ("Answer") nil "Context" nil)
            '(:kind "text" :text "Answer") '("Hint") 0))
          (should (= 2 (length (gnosis-select '* 'practice-encounters))))
          (gnosis-sqlite-close gnosis-db)
          (setq gnosis-db (gnosis-sqlite-open gnosis-test--db-file))
          (gnosis-db-init)
          (should (gnosis-delete-thema 102 t))
          (should (equal before-delete
                         (gnosis-test-safety-snapshot gnosis-test--db-file))))
        (should (equal "Shared fixture bytes"
                       (with-temp-buffer
                         (insert-file-contents-literally asset)
                         (buffer-string))))
        (should-not (sqlite-select gnosis-db "PRAGMA foreign_key_check"))))))

(ert-deftest gnosis-release-schema-fresh-is-ten ()
  (gnosis-test-with-old-db
    (gnosis-db-init)
    (should (= 10 (gnosis--db-version)))
    (gnosis-db--check-schema gnosis-db 10)))

(ert-deftest gnosis-release-schema-eight-opens-through-nine-as-complete-ten ()
  (gnosis-test-with-old-db
    (gnosis-test--create-v8-schema)
    (gnosis-db-init)
    (should (= 10 (gnosis--db-version)))
    (dolist (table '(study-history study-session scheduler-active practice-events
                    review-voids practice-voids))
      (should (gnosis-table-exists-p table)))
    (should (assoc 6 (sqlite-select gnosis-db "PRAGMA table_info(themata)")))
    (gnosis-db--check-schema gnosis-db 10)))

(ert-deftest gnosis-release-schema-private-versions-are-refused ()
  (dolist (version '(11 12 13))
    (gnosis-test-with-old-db
      (gnosis-db-init)
      (gnosis--db-set-version version)
      (should-error (gnosis-db-init) :type 'user-error)
      (should (= version (gnosis--db-version))))))
(ert-deftest gnosis-release-schema-ten-must-have-the-complete-layout ()
  (gnosis-test-with-old-db
    (gnosis-db-init)
    (sqlite-execute gnosis-db "DROP TABLE study_history")
    (sqlite-execute gnosis-db "ALTER TABLE themata DROP COLUMN accepted_aliases")
    (let ((before (sqlite-select gnosis-db
                   "SELECT type, name, sql FROM sqlite_master ORDER BY type, name")))
      (should-error (gnosis-db-init))
      (should (= 10 (gnosis--db-version)))
      (should (equal before (sqlite-select gnosis-db
                             "SELECT type, name, sql FROM sqlite_master ORDER BY type, name"))))))

(ert-deftest gnosis-release-schema-nine-upgrades-without-fabricated-encounters ()
  (let (fresh)
    (gnosis-test-with-old-db
      (gnosis-db-init)
      (setq fresh (sqlite-select gnosis-db
                   "SELECT type, name, sql FROM sqlite_master ORDER BY type, name")))
    (gnosis-test-with-old-db
      (gnosis-fixture-create-v9)
      (sqlite-execute gnosis-db
        "INSERT INTO themata VALUES (101, 'basic', 'Question', '(nil)', '(answer)', NULL, NULL)")
      (sqlite-execute gnosis-db
        "INSERT INTO practice_events VALUES ('legacy', 101, 'batch', 1, 1000000, 1)")
      (let ((rows (sqlite-select gnosis-db "SELECT * FROM practice_events")))
        (gnosis-db-init)
        (should (= 10 (gnosis--db-version)))
        (should (equal rows (sqlite-select gnosis-db "SELECT * FROM practice_events")))
        (should-not (sqlite-select gnosis-db "SELECT * FROM practice_encounters"))
        (should (equal fresh (sqlite-select gnosis-db
                              "SELECT type, name, sql FROM sqlite_master ORDER BY type, name")))))))

(ert-deftest gnosis-release-schema-nine-private-layouts-fail-before-write ()
  (dolist (damage '("DROP TABLE study_history"
                    "ALTER TABLE themata DROP COLUMN accepted_aliases"
                    "DROP TRIGGER practice_events_no_replace"
                    "CREATE TABLE practice_encounters (event_id TEXT, data TEXT)"))
    (gnosis-test-with-old-db
      (gnosis-fixture-create-v9)
      (sqlite-execute gnosis-db damage)
      (let ((before (sqlite-select gnosis-db "SELECT type, name, sql FROM sqlite_master ORDER BY type, name")))
        (should-error (gnosis-db-init))
        (should (= 9 (gnosis--db-version)))
        (should (equal before (sqlite-select gnosis-db
                               "SELECT type, name, sql FROM sqlite_master ORDER BY type, name")))))))

(ert-deftest gnosis-release-schema-upgrade-chain-rolls-back-error-and-quit ()
  (dolist (version '(8 9))
    (dolist (failure '(error quit))
      (gnosis-test-with-old-db
        (if (= version 8) (gnosis-test--create-v8-schema) (gnosis-fixture-create-v9))
        (let ((before (sqlite-select gnosis-db
                        "SELECT type, name, sql FROM sqlite_master ORDER BY type, name"))
              caught)
          (cl-letf (((symbol-function 'gnosis-db--create-encounter-guards)
                     (lambda (_) (signal failure '("Fixture migration fault")))))
            (condition-case nil (gnosis-db-init)
              ((error quit) (setq caught t))))
          (should caught)
          (should (= version (gnosis--db-version)))
          (should (equal before (sqlite-select gnosis-db
                                 "SELECT type, name, sql FROM sqlite_master ORDER BY type, name"))))))))

(provide 'gnosis-test-release-schema)

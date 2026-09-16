;;; gnosis-test-export-import.el --- Export/import tests  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Free Software Foundation, Inc.

;; Author: Thanos Apollo <public@thanosapollo.org>

;;; Commentary:

;; Tests for SQLite export/import functionality.
;; Uses temporary SQLite databases so the user's real DB is untouched.

;;; Code:
(require 'ert)
(require 'gnosis)
(require 'gnosis-export-import)

(require 'gnosis-test-helpers)

(ert-deftest gnosis-test-export-list-literal-hyphens ()
  "Remove list markers once, preserving negative and literal field values."
  (dolist (case '(("- -90\n- --flag\n- - literal\n- alpha-beta"
                   ("-90" "--flag" "- literal" "alpha-beta"))
                  ("-90\n- -45" ("-90" "-45"))
                  ("--flag" ("--flag"))
                  ("-" (""))
                  ("- -" ("-"))
                  ("- ordinary\n- second" ("ordinary" "second"))))
    (with-temp-buffer
      (org-mode)
      (gnosis-export--insert-thema "NEW" "basic" "Question"
                                   (car case) (car case))
      (let ((thema (car (gnosis-export-parse-themata))))
        (should (equal (cadr case) (nth 3 thema)))
        (should (equal (cadr case) (nth 4 thema)))))))

;; ---- Group 1: SQLite export ----

(ert-deftest gnosis-test-export-basic ()
  "Export themata to SQLite database and verify contents."
  (gnosis-test-with-db
    (let* ((id1 (gnosis-test--add-basic-thema "What is 2+2?" "4"
                                              '("math" "basic")))
           (id2 (gnosis-test--add-basic-thema "Capital of Greece?" "Athens"
                                              '("geo")))
           (export-file (concat (make-temp-file "gnosis-export-") ".db")))
      (unwind-protect
          (progn
            (gnosis-export-db export-file)
            (should (file-exists-p export-file))
            (let ((edb (gnosis-sqlite-open export-file)))
              (unwind-protect
                  (progn
                    (should (= 2 (caar (gnosis-sqlite-select edb
                                         "SELECT COUNT(*) FROM themata"))))
                    ;; Tags in junction table
                    (let ((tags (mapcar #'car
                                  (gnosis-sqlite-select edb
                                    "SELECT tag FROM thema_tag WHERE thema_id = ?"
                                    (list id1)))))
                      (should (member "math" tags))
                      (should (member "basic" tags)))
                    ;; Metadata (plain text, not emacsql-encoded)
                    (should (sqlite-select edb
                              "SELECT value FROM gnosis_meta WHERE key = 'exported_at'")))
                    (should (equal "3" (caar (sqlite-select
                                               edb
                                               "SELECT value FROM gnosis_meta WHERE key = 'format_version'"))))
                    (should
                     (equal '(("extras") ("gnosis_meta") ("thema_tag")
                              ("themata"))
                            (sqlite-select
                             edb "SELECT name FROM sqlite_master
                                   WHERE type = 'table' ORDER BY name")))
                (gnosis-sqlite-close edb))))
        (when (file-exists-p export-file) (delete-file export-file))))))

(ert-deftest gnosis-test-export-empty ()
  "Export with no themata creates DB with count 0."
  (gnosis-test-with-db
    (let ((export-file (concat (make-temp-file "gnosis-export-empty-") ".db")))
      (unwind-protect
          (progn
            (gnosis-export-db export-file)
            (let ((edb (gnosis-sqlite-open export-file)))
              (unwind-protect
                  (progn
                    (should (= 0 (caar (gnosis-sqlite-select edb
                                         "SELECT COUNT(*) FROM themata"))))
                    (should (equal "0" (caar (sqlite-select edb
                                              "SELECT value FROM gnosis_meta WHERE key = 'thema_count'")))))
                (gnosis-sqlite-close edb))))
        (when (file-exists-p export-file) (delete-file export-file))))))

(ert-deftest gnosis-test-export-excludes-suspended ()
  "Export without include-suspended skips suspended themata."
  (gnosis-test-with-db
    (let ((active (gnosis-test--add-basic-thema "Active" "A1" '("a")))
          (suspended (gnosis-test--add-basic-thema
                      "Suspended" "A2" '("s") nil nil 1))
          (export-file (concat (make-temp-file "gnosis-export-susp-") ".db")))
      (unwind-protect
          (progn
            (gnosis-export-db export-file nil nil nil)
            (let ((edb (gnosis-sqlite-open export-file)))
              (unwind-protect
                  (should (equal (list (list active))
                                 (gnosis-sqlite-select
                                  edb "SELECT id FROM themata")))
                (gnosis-sqlite-close edb))))
        (when (file-exists-p export-file) (delete-file export-file))))))

(ert-deftest gnosis-test-export-includes-suspended ()
  "Export with include-suspended includes all themata."
  (gnosis-test-with-db
    (gnosis-test--add-basic-thema "Active" "A1" '("a"))
    (gnosis-test--add-basic-thema "Suspended" "A2" '("s") nil nil 1)
    (let ((export-file (concat (make-temp-file "gnosis-export-susp2-") ".db")))
      (unwind-protect
          (progn
            (gnosis-export-db export-file nil nil t)
            (let ((edb (gnosis-sqlite-open export-file)))
              (unwind-protect
                  (should (= 2 (caar (gnosis-sqlite-select edb
                                       "SELECT COUNT(*) FROM themata"))))
                (gnosis-sqlite-close edb))))
        (when (file-exists-p export-file) (delete-file export-file))))))

(ert-deftest gnosis-test-export-excludes-all-suspended ()
  "Export zero content when scheduler authority suspends every thema."
  (gnosis-test-with-db
    (gnosis-test--add-basic-thema "Suspended" "A" '("s") nil nil 1)
    (let ((export-file (concat (make-temp-file "gnosis-export-none-") ".db")))
      (unwind-protect
          (progn
            (gnosis-export-db export-file)
            (let ((edb (gnosis-sqlite-open export-file)))
              (unwind-protect
                  (should (= 0 (caar (gnosis-sqlite-select
                                      edb "SELECT COUNT(*) FROM themata"))))
                (gnosis-sqlite-close edb))))
        (when (file-exists-p export-file) (delete-file export-file))))))

(ert-deftest gnosis-test-export-extras ()
  "Export preserves parathema in extras table."
  (gnosis-test-with-db
    (let* ((id1 (gnosis-test--add-basic-thema "Q" "A" '("t") "See SICP"))
           (export-file (concat (make-temp-file "gnosis-export-extras-") ".db")))
      (unwind-protect
          (progn
            (gnosis-export-db export-file)
            (let ((edb (gnosis-sqlite-open export-file)))
              (unwind-protect
                  (let ((parathema (caar (gnosis-sqlite-select edb
                                          "SELECT parathema FROM extras WHERE id = ?"
                                          (list id1)))))
                    (should (string-search "See SICP" parathema)))
                (gnosis-sqlite-close edb))))
        (when (file-exists-p export-file) (delete-file export-file))))))

(ert-deftest gnosis-test-export-no-matching-tags-is-empty ()
  "An active tag filter matching nothing exports zero themata."
  (gnosis-test-with-db
    (gnosis-test--add-basic-thema "Q1" "A1" '("present"))
    (gnosis-test--add-basic-thema "Q2" "A2" '("other"))
    (let ((export-file (concat (make-temp-file "gnosis-export-filter-") ".db")))
      (unwind-protect
          (progn
            (gnosis-export-db export-file '("absent") nil t)
            (let ((edb (gnosis-sqlite-open export-file)))
              (unwind-protect
                  (should (= 0 (caar (gnosis-sqlite-select
                                      edb "SELECT COUNT(*) FROM themata"))))
                (gnosis-sqlite-close edb))))
        (when (file-exists-p export-file) (delete-file export-file))))))

(ert-deftest gnosis-test-save-rolls-back-all-fields-on-link-error ()
  "A failed save leaves no partially inserted thema rows."
  (gnosis-test-with-db
    (let ((gnosis-save-hook nil))
      (save-window-excursion
        (unwind-protect
            (progn
              (sqlite-execute
               gnosis-db
               "CREATE TEMP TRIGGER refuse_link BEFORE INSERT ON thema_links
                WHEN NEW.dest = '\"node-y\"'
                BEGIN SELECT RAISE(ABORT, 'Blocked source'); END")
              (gnosis-add-thema "basic" "Q [[id:node-x][X]] [[id:node-y][Y]]"
                                "hint" "A" nil '("probe"))
              (let ((text (buffer-string))
                    (err (should-error
                          (call-interactively (key-binding (kbd "C-c C-c")))
                          :type 'user-error)))
                (should (string-match-p "Blocked source"
                                        (error-message-string err)))
                (should (equal text (buffer-string)))
                (should (get-buffer "*Gnosis NEW*"))))
          (when (get-buffer "*Gnosis NEW*") (kill-buffer "*Gnosis NEW*")))))
    (dolist (table '(themata scheduler-baseline scheduler-state
                     extras thema-links thema-tag))
      (should (= 0 (caar (gnosis-sqlite-select
                          gnosis-db
                          (format "SELECT COUNT(*) FROM %s"
                                  (gnosis-sqlite--ident table)))))))))

;; ---- Group 2: Import diff ----

(ert-deftest gnosis-test-import-diff-detects-new ()
  "Import diff detects new themata not in the main DB."
  (gnosis-test-with-db
    (let* ((id1 (gnosis-test--add-basic-thema "Q1" "A1" '("t1")))
           (id2 (gnosis-test--add-basic-thema "Q2" "A2" '("t2")))
           (export-file (concat (make-temp-file "gnosis-diff-new-") ".db")))
      (unwind-protect
          (progn
            (gnosis-export-db export-file)
            ;; No format_version is the supported legacy v1 contract.
            (let ((edb (sqlite-open export-file)))
              (sqlite-execute edb
                              "DELETE FROM gnosis_meta WHERE key = 'format_version'")
              (sqlite-close edb))
            ;; Import into a fresh DB: all should be detected as new
            (let* ((db-file2 (make-temp-file "gnosis-test2-" nil ".db"))
                   (gnosis-db (gnosis-sqlite-open db-file2))
                   (gnosis--id-cache nil))
              (unwind-protect
                  (progn
                    (gnosis-db-init)
                    (let* ((diff (gnosis-import--diff export-file))
                           (new-rows (car diff))
                           (changed-rows (cadr diff)))
                      (should (= 2 (length new-rows)))
                      (should (= 0 (length changed-rows)))))
                (gnosis-sqlite-close gnosis-db)
                (delete-file db-file2))))
        (when (file-exists-p export-file) (delete-file export-file))))))

(ert-deftest gnosis-test-import-diff-detects-changes ()
  "Import diff detects changed themata."
  (gnosis-test-with-db
    (let* ((id1 (gnosis-test--add-basic-thema "Original Q" "Original A" '("t1")))
           (export-file (concat (make-temp-file "gnosis-diff-change-") ".db")))
      (unwind-protect
          (progn
            (gnosis-export-db export-file)
            ;; Modify keimenon in the export DB
            (let ((edb (sqlite-open export-file)))
              (sqlite-execute edb
                (format "UPDATE themata SET keimenon = '\"Changed Q\"' WHERE id = %d" id1))
              (sqlite-close edb))
            ;; Diff against current DB
            (let* ((diff (gnosis-import--diff export-file))
                   (new-rows (car diff))
                   (changed-rows (cadr diff)))
              (should (= 0 (length new-rows)))
              (should (= 1 (length changed-rows)))
              (should (= id1 (caar changed-rows)))))
        (when (file-exists-p export-file) (delete-file export-file))))))

(ert-deftest gnosis-test-import-diff-no-changes ()
  "Diff against identical export shows no changes."
  (gnosis-test-with-db
    (gnosis-test--add-basic-thema "Q1" "A1" '("t"))
    (let ((export-file (concat (make-temp-file "gnosis-diff-same-") ".db")))
      (unwind-protect
          (progn
            (gnosis-export-db export-file)
            (let* ((diff (gnosis-import--diff export-file))
                   (new-rows (car diff))
                   (changed-rows (cadr diff)))
              (should (= 0 (length new-rows)))
              (should (= 0 (length changed-rows)))))
        (when (file-exists-p export-file) (delete-file export-file))))))

;; ---- Group 3: Import apply ----

(ert-deftest gnosis-test-import-apply-new ()
  "Apply import inserts new themata with review state."
  (gnosis-test-with-db
    (let* ((id1 (gnosis-test--add-basic-thema "Q1" "A1" '("emacs" "editor")))
           (id2 (gnosis-test--add-basic-thema "Q2" "A2" '("lisp") "See SICP"))
           (export-file (concat (make-temp-file "gnosis-apply-new-") ".db")))
      (unwind-protect
          (progn
            (gnosis-export-db export-file)
            ;; Apply into a fresh DB
            (let* ((db-file2 (make-temp-file "gnosis-test2-" nil ".db"))
                   (gnosis-db (gnosis-sqlite-open db-file2))
                   (gnosis--id-cache nil))
              (unwind-protect
                  (progn
                    (gnosis-db-init)
                    (let ((execute (symbol-function 'sqlite-execute))
                          (gnosis-sqlite--max-vars 10))
                      (cl-letf
                          (((symbol-function 'sqlite-execute)
                            (lambda (db sql &optional params)
                              (when (> (length params) 10)
                                (error "Controlled bind-variable limit"))
                              (funcall execute db sql params))))
                        (gnosis-import--apply-changes
                         export-file (list id1 id2) nil
                         (gnosis-import--file-sha256 export-file))))
                    ;; Themata created
                    (should (= 2 (length (gnosis-select 'id 'themata nil t))))
                    ;; Tags preserved
                    (let ((tags (gnosis-select 'tag 'thema-tag
                                              `(= thema-id ,id1) t)))
                      (should (member "emacs" tags))
                      (should (member "editor" tags)))
                    ;; Extras preserved
                    (let ((p (gnosis-get 'parathema 'extras `(= id ,id2))))
                      (should (string-search "See SICP" p)))
                    (let ((today (gnosis--date-to-int
                                  (gnosis-date)))
                          (sorted-ids (sort (list id1 id2) #'<)))
                      (should
                       (equal (mapcar (lambda (id) (list id today 0 0))
                                      sorted-ids)
                              (gnosis-sqlite-select
                               gnosis-db "SELECT * FROM scheduler_baseline
                                           ORDER BY thema_id")))
                      (should
                       (equal (mapcar
                               (lambda (id)
                                 (list id 1 nil nil nil nil today 0 0 0))
                               sorted-ids)
                              (gnosis-sqlite-select
                               gnosis-db "SELECT * FROM scheduler_state
                                           ORDER BY thema_id")))))
                (gnosis-sqlite-close gnosis-db)
                (delete-file db-file2))))
        (when (file-exists-p export-file) (delete-file export-file))))))

(ert-deftest gnosis-test-import-rejects-future-version-before-mutation ()
  "Reject future content formats before changing the destination DB."
  (gnosis-test-with-db
    (let* ((id (gnosis-test--add-basic-thema "Q" "A" '("test")))
           (export-file (concat (make-temp-file "gnosis-future-") ".db")))
      (unwind-protect
          (progn
            (gnosis-export-db export-file)
            (let ((edb (sqlite-open export-file)))
              (sqlite-execute edb
                              "UPDATE gnosis_meta SET value = '999'
                                WHERE key = 'format_version'")
              (sqlite-close edb))
            (let* ((db-file2 (make-temp-file "gnosis-future-dst-" nil ".db"))
                   (gnosis-db (gnosis-sqlite-open db-file2)))
              (unwind-protect
                  (progn
                    (gnosis-db-init)
                    (should-error
                     (gnosis-import--apply-changes
                      export-file (list id) nil
                      (gnosis-import--file-sha256 export-file)))
                    (should-error
                     (gnosis-import--apply-snapshot
                      export-file (list id) nil))
                    (should (= 0 (caar (gnosis-sqlite-select
                                        gnosis-db
                                        "SELECT COUNT(*) FROM themata")))))
                (gnosis-sqlite-close gnosis-db)
                (delete-file db-file2))))
        (when (file-exists-p export-file) (delete-file export-file))))))

(ert-deftest gnosis-test-import-rejects-malformed-metadata-authority ()
  "Reject uppercase future metadata, views, and conflicting rows."
  (dolist (kind '(uppercase view duplicate))
    (let ((file (make-temp-file "gnosis-malformed-meta-" nil ".db")))
      (unwind-protect
          (let ((db (sqlite-open file)))
            (unwind-protect
                (pcase kind
                  ('uppercase
                   (sqlite-execute
                    db "CREATE TABLE GNOSIS_META
                         (key TEXT PRIMARY KEY, value TEXT)")
                   (sqlite-execute
                    db "INSERT INTO GNOSIS_META VALUES
                         ('format_version', '999')"))
                  ('view
                   (sqlite-execute db "CREATE TABLE source (key TEXT, value TEXT)")
                   (sqlite-execute
                    db "CREATE VIEW gnosis_meta AS SELECT key, value FROM source"))
                  ('duplicate
                   (sqlite-execute db "CREATE TABLE gnosis_meta
                                       (key TEXT, value TEXT)")
                   (sqlite-execute db "INSERT INTO gnosis_meta VALUES
                                       ('format_version', '1'),
                                       ('format_version', '2')")))
              (sqlite-close db))
            (should-error (gnosis-import--format-version file)))
        (when (file-exists-p file) (delete-file file))))))

(ert-deftest gnosis-test-import-rejects-source-replaced-after-diff ()
  "Reject a valid-v2 source replaced after the reviewed diff."
  (gnosis-test-with-db
    (let* ((id (gnosis-test--add-basic-thema "Reviewed" "A" '("test")))
           (source (concat (make-temp-file "gnosis-reviewed-") ".db"))
           (replacement (concat (make-temp-file "gnosis-replaced-") ".db")))
      (unwind-protect
          (progn
            (gnosis-export-db source)
            (let* ((diff (gnosis-import--diff source))
                   (source-id (nth 2 diff)))
              (should (stringp source-id))
              (copy-file source replacement t)
              (let ((db (sqlite-open replacement)))
                (sqlite-execute
                 db "UPDATE themata SET keimenon = '\"Replacement\"'")
                (sqlite-close db))
              (rename-file replacement source t)
              (let* ((db-file2 (make-temp-file "gnosis-replaced-dst-" nil ".db"))
                     (gnosis-db (gnosis-sqlite-open db-file2)))
                (unwind-protect
                    (progn
                      (gnosis-db-init)
                      (should-error
                       (gnosis-import--apply-changes
                        source (list id) nil source-id))
                      (should (= 0 (caar (gnosis-sqlite-select
                                          gnosis-db
                                          "SELECT COUNT(*) FROM themata")))))
                  (gnosis-sqlite-close gnosis-db)
                  (delete-file db-file2)))))
        (when (file-exists-p source) (delete-file source))
        (when (file-exists-p replacement) (delete-file replacement))))))

(ert-deftest gnosis-test-import-diff-uses-private-source-snapshot ()
  "Keep diff rows and identity on snapshot A while pathname serves B."
  (gnosis-test-with-db
    (gnosis-test--add-basic-thema "Reviewed" "A" '("test"))
    (let ((source (concat (make-temp-file "gnosis-diff-source-") ".db"))
          (replacement (concat (make-temp-file "gnosis-diff-swap-") ".db")))
      (unwind-protect
          (progn
            (gnosis-export-db source)
            (copy-file source replacement t)
            (let ((db (sqlite-open replacement)))
              (sqlite-execute
               db "UPDATE themata SET keimenon = '\"Replacement\"'")
              (sqlite-close db))
            (let ((real-diff (symbol-function 'gnosis-import--diff-snapshot))
                  (snapshot-called nil))
              (cl-letf
                  (((symbol-function 'gnosis-import--diff-snapshot)
                    (lambda (snapshot)
                      (setq snapshot-called t)
                      (let ((original (make-temp-file
                                       "gnosis-diff-original-" nil ".db")))
                        (unwind-protect
                            (progn
                              (copy-file source original t)
                              (copy-file replacement source t)
                              (funcall real-diff snapshot))
                          (copy-file original source t)
                          (delete-file original))))))
                (let ((diff (gnosis-import--diff source)))
                  (should snapshot-called)
                  (should-not (car diff))
                  (should-not (cadr diff))
                  (should (equal (nth 2 diff)
                                 (gnosis-import--file-sha256 source)))))))
        (when (file-exists-p source) (delete-file source))
        (when (file-exists-p replacement) (delete-file replacement))))))

(ert-deftest gnosis-test-import-scheduler-failure-rolls-back-content ()
  "Roll back imported content when scheduler initialization fails."
  (gnosis-test-with-db
    (let* ((id (gnosis-test--add-basic-thema "Q" "A" '("test")))
           (export-file (concat (make-temp-file "gnosis-rollback-") ".db")))
      (unwind-protect
          (progn
            (gnosis-export-db export-file)
            (let* ((db-file2 (make-temp-file "gnosis-rollback-dst-" nil ".db"))
                   (gnosis-db (gnosis-sqlite-open db-file2)))
              (unwind-protect
                  (progn
                    (gnosis-db-init)
                    (gnosis-sqlite-execute
                     gnosis-db
                     "CREATE TRIGGER controlled_import_scheduler_failure
                        BEFORE INSERT ON scheduler_state
                        BEGIN SELECT RAISE(ABORT, 'controlled failure'); END")
                    (should-error
                     (gnosis-import--apply-changes
                      export-file (list id) nil
                      (gnosis-import--file-sha256 export-file)))
                    (dolist (table '(themata scheduler-baseline scheduler-state))
                      (should (= 0 (caar (gnosis-sqlite-select
                                          gnosis-db
                                          (format "SELECT COUNT(*) FROM %s"
                                                  (gnosis-sqlite--ident
                                                   table))))))))
                (gnosis-sqlite-close gnosis-db)
                (delete-file db-file2))))
        (when (file-exists-p export-file) (delete-file export-file))))))

(ert-deftest gnosis-test-import-apply-changed ()
  "Apply import updates changed themata content."
  (gnosis-test-with-db
    (let* ((id1 (gnosis-test--add-basic-thema "Old question" "Old answer" '("old")))
           (id2 (gnosis-test--add-basic-thema "New question" "New answer" '("new")))
           (export-file (concat (make-temp-file "gnosis-apply-change-") ".db")))
      (unwind-protect
          (progn
            (gnosis-export-db export-file)
            ;; Modify keimenon in export DB
            (let ((edb (sqlite-open export-file)))
              (sqlite-execute edb
                (format "UPDATE themata SET keimenon = '\"New question\"' WHERE id = %d" id1))
              (sqlite-close edb))
            (gnosis-sqlite-execute
             gnosis-db "DELETE FROM themata WHERE id = ?" (list id2))
            (gnosis-sqlite-execute
             gnosis-db "UPDATE scheduler_state SET stability = 2.5,
                         difficulty = 3.5, last_reviewed_at_us = 1000000,
                         last_review_day = 20260830, due_day = 20990101,
                         reps = 7, lapses = 2, suspended = 1
                         WHERE thema_id = ?" (list id1))
            (let ((expected-state
                   (car (gnosis-sqlite-select
                         gnosis-db "SELECT * FROM scheduler_state
                                    WHERE thema_id = ?" (list id1)))))
            ;; Apply as changed
              (gnosis-import--apply-changes
               export-file (list id2) (list id1)
               (gnosis-import--file-sha256 export-file))
            ;; Verify
              (should (= 2 (length (gnosis-select 'id 'themata nil t))))
              (should (equal "New question"
                             (gnosis-get 'keimenon 'themata `(= id ,id1))))
              (should (equal expected-state
                             (car (gnosis-sqlite-select
                                   gnosis-db "SELECT * FROM scheduler_state
                                              WHERE thema_id = ?"
                                   (list id1)))))
              (should (gnosis-select 'thema-id 'scheduler-baseline
                                     `(= thema-id ,id2) t)))
        (when (file-exists-p export-file) (delete-file export-file)))))))

;; ---- Group 4: Edit mode support (unchanged) ----

(ert-deftest gnosis-test-export-insert-thema-content ()
  "Inserting a thema produces all sections."
  (with-temp-buffer
    (gnosis-export--insert-thema "1" "basic" "Question?" "- hint" "- answer" "extra"
				 '("tag1" "tag2"))
    (let ((text (buffer-substring-no-properties (point-min) (point-max))))
      (should (string-match-p "\\*\\* Keimenon" text))
      (should (string-match-p "Question?" text))
      (should (string-match-p "\\*\\* Hypothesis" text))
      (should (string-match-p "- hint" text))
      (should (string-match-p "\\*\\* Answer" text))
      (should (string-match-p "- answer" text))
      (should (string-match-p "\\*\\* Parathema" text))
      (should (string-match-p "extra" text)))))

(ert-deftest gnosis-test-export-insert-thema-read-only-no-leak ()
  "Read-only on PROPERTIES block does not leak into content sections."
  (with-temp-buffer
    (gnosis-export--insert-thema "1" "basic" "Q" "H" "A" "P" '("t"))
    (goto-char (point-min))
    (search-forward "** Keimenon")
    (should-not (get-text-property (point) 'read-only))
    (search-forward "Q")
    (should-not (get-text-property (match-beginning 0) 'read-only))))

(ert-deftest gnosis-test-export-multiple-themata-no-read-only-error ()
  "Inserting multiple themata in sequence does not signal text-read-only."
  (with-temp-buffer
    (gnosis-export--insert-thema "1" "basic" "Q1" "H1" "A1" "P1" '("t"))
    (gnosis-export--insert-thema "2" "cloze" "Q2" "H2" "A2" "P2" '("t"))
    (gnosis-export--insert-thema "3" "mcq" "Q3" "H3" "A3" "P3" '("t"))
    (let ((text (buffer-substring-no-properties (point-min) (point-max))))
      (should (string-match-p "GNOSIS_ID: 1" text))
      (should (string-match-p "GNOSIS_ID: 2" text))
      (should (string-match-p "GNOSIS_ID: 3" text))
      (should (string-match-p "Q3" text)))))

;; ---- Group 5: Tag filter tests ----

(ert-deftest gnosis-test-parse-filter-mixed ()
  "Parse mixed +/- tags into include and exclude lists."
  (let ((result (gnosis-tags--parse-filter '("+math" "-history" "+science"))))
    (should (equal (car result) '("math" "science")))
    (should (equal (cdr result) '("history")))))

(ert-deftest gnosis-test-parse-filter-include-only ()
  "Parse only include tags."
  (let ((result (gnosis-tags--parse-filter '("+a" "+b"))))
    (should (equal (car result) '("a" "b")))
    (should (null (cdr result)))))

(ert-deftest gnosis-test-parse-filter-exclude-only ()
  "Parse only exclude tags."
  (let ((result (gnosis-tags--parse-filter '("-x" "-y"))))
    (should (null (car result)))
    (should (equal (cdr result) '("x" "y")))))

(ert-deftest gnosis-test-parse-filter-ignores-no-prefix ()
  "Entries without +/- prefix are silently ignored."
  (let ((result (gnosis-tags--parse-filter '("bare" "+good" "-bad"))))
    (should (equal (car result) '("good")))
    (should (equal (cdr result) '("bad")))))

(ert-deftest gnosis-test-parse-filter-empty ()
  "Empty input returns (nil . nil)."
  (let ((result (gnosis-tags--parse-filter nil)))
    (should (null (car result)))
    (should (null (cdr result)))))

(ert-deftest gnosis-test-filter-by-tags-include ()
  "Include filter returns only themata with the specified tags."
  (gnosis-test-with-db
    (gnosis-test--add-basic-thema "Q1" "A1" '("math"))
    (gnosis-test--add-basic-thema "Q2" "A2" '("history"))
    (gnosis-test--add-basic-thema "Q3" "A3" '("math" "science"))
    (let ((ids (gnosis-filter-by-tags '("math") nil)))
      (should (= 2 (length ids))))))

(ert-deftest gnosis-test-filter-by-tags-exclude ()
  "Exclude filter removes themata with the specified tags."
  (gnosis-test-with-db
    (gnosis-test--add-basic-thema "Q1" "A1" '("math"))
    (gnosis-test--add-basic-thema "Q2" "A2" '("history"))
    (gnosis-test--add-basic-thema "Q3" "A3" '("science"))
    (let ((ids (gnosis-filter-by-tags nil '("math"))))
      (should (= 2 (length ids))))))

(ert-deftest gnosis-test-filter-by-tags-include-and-exclude ()
  "Include + exclude: include first, then subtract excluded."
  (gnosis-test-with-db
    (gnosis-test--add-basic-thema "Q1" "A1" '("math" "easy"))
    (gnosis-test--add-basic-thema "Q2" "A2" '("math" "hard"))
    (gnosis-test--add-basic-thema "Q3" "A3" '("history"))
    (let ((ids (gnosis-filter-by-tags '("math") '("hard"))))
      (should (= 1 (length ids))))))

(ert-deftest gnosis-test-filter-by-tags-empty ()
  "No include or exclude returns all themata."
  (gnosis-test-with-db
    (gnosis-test--add-basic-thema "Q1" "A1" '("a"))
    (gnosis-test--add-basic-thema "Q2" "A2" '("b"))
    (let ((ids (gnosis-filter-by-tags nil nil)))
      (should (= 2 (length ids))))))

;; ---- Group 6: ID cache tests ----

(ert-deftest gnosis-test-id-cache-generates-unique ()
  "gnosis-generate-id with cache produces unique IDs."
  (let ((gnosis--id-cache (make-hash-table :test 'equal)))
    (puthash 12345678901 t gnosis--id-cache)
    (let ((new-id (gnosis-generate-id 11)))
      (should-not (= new-id 12345678901))
      (should (gethash new-id gnosis--id-cache)))))

(ert-deftest gnosis-test-id-cache-no-db-query ()
  "gnosis-generate-id with cache works without a DB connection."
  (let ((gnosis--id-cache (make-hash-table :test 'equal))
        (gnosis-db nil))
    (let ((id (gnosis-generate-id)))
      (should (integerp id))
      (should (gethash id gnosis--id-cache)))))

;; ---- Group 7: Tag operations ----

(ert-deftest gnosis-test-tag-rename ()
  "Renaming a tag updates the junction table."
  (gnosis-test-with-db
    (let ((id1 (gnosis-test--add-basic-thema "Q1" "A1" '("old_name" "keep"))))
      (gnosis-tag-rename "old_name" "new_name")
      (let ((tags (gnosis-select 'tag 'thema-tag `(= thema-id ,id1) t)))
        (should (member "new_name" tags))
        (should-not (member "old_name" tags))
        (should (member "keep" tags))))))

(ert-deftest gnosis-test-tag-delete ()
  "Deleting a tag removes it from the junction table."
  (gnosis-test-with-db
    (let ((id1 (gnosis-test--add-basic-thema "Q1" "A1" '("doomed" "safe"))))
      (gnosis--delete 'thema-tag '(= tag "doomed"))
      (let ((tags (gnosis-select 'tag 'thema-tag `(= thema-id ,id1) t)))
        (should-not (member "doomed" tags))
        (should (member "safe" tags))))))

(provide 'gnosis-test-export-import)

;;; gnosis-test-export-import.el ends here

;;; gnosis-test-schema-ownership.el --- Required deletion cascades -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;;; Commentary:

;; Refuse malformed retained ownership before opening or migration, and keep
;; physical deletion working with nonempty scheduled and practice evidence.

;;; Code:

(require 'ert)
(require 'gnosis-fixture-schema-v9)
(require 'gnosis-test-db-safety)
(require 'gnosis-test-helpers)

(defun gnosis-test-ownership--add-evidence (id)
  "Create content and scheduled/practice evidence for thema ID."
  (gnosis-add-thema-fields "basic" "Question" '("Hint") '("Answer")
                           "Context" '("test") 0 nil nil id)
  (gnosis--insert-into 'thema-links `([,id "source-node"]))
  (let* ((event-id (gnosis-scheduler-event-id))
         (practice-id (format "practice-%s" id)))
    (gnosis-scheduler-accept-review event-id id 'success 1000000 20260913)
    (gnosis--insert-into 'review-voids `([,(format "review-void-%s" id) ,event-id]))
    (gnosis--insert-into 'practice-events `([,practice-id ,id "session" ,id 1 3]))
    (gnosis--insert-into 'practice-voids `([,(format "practice-void-%s" id) ,practice-id]))
    (when (gnosis-table-exists-p 'practice-encounters)
      (gnosis--insert-into 'practice-encounters `([,practice-id (:response "Answer")])))))

(defun gnosis-test-ownership--reject (table column parent parent-column)
  "Refuse missing/noncascading TABLE ownership through the public opener.
COLUMN references PARENT's PARENT-COLUMN in the released fixture."
  (dolist (version '(9 10))
    (dolist (damage '(missing noncascading))
      (ert-info ((format "%s schema %s %s" table version damage))
        (gnosis-test-safety
          (let* ((file (expand-file-name "gnosis.db" gnosis-dir))
                 (clause (format "FOREIGN KEY (%s) REFERENCES %s (%s) ON DELETE CASCADE"
                                 column parent parent-column))
                 (gnosis-fixture-v9-ddl
                  (mapcar
                   (lambda (sql)
                     (if (string-prefix-p (format "CREATE TABLE %s " table) sql)
                         (progn
                           (should (string-match-p (regexp-quote clause) sql))
                           (if (eq damage 'missing)
                               (string-replace (concat ", " clause) "" sql)
                             (string-replace clause
                                             (string-replace "CASCADE" "NO ACTION" clause)
                                             sql)))
                       sql))
                   gnosis-fixture-v9-ddl)))
            (let ((gnosis-db (gnosis-sqlite-open file)))
              (unwind-protect
                  (progn
                    (gnosis-fixture-create-v9)
                    (when (= version 10)
                      (sqlite-execute gnosis-db
                        "CREATE TABLE practice_encounters (event_id TEXT PRIMARY KEY NOT NULL, data TEXT NOT NULL, FOREIGN KEY (event_id) REFERENCES practice_events (event_id) ON DELETE CASCADE)")
                      (gnosis-db--create-encounter-guards gnosis-db)
                      (sqlite-execute gnosis-db "PRAGMA user_version = 10"))
                    (gnosis-test-ownership--add-evidence 101)
                    (gnosis-test-ownership--add-evidence 102)
                    ;; These checks cannot detect absent declarations.
                    (should (equal '(("ok")) (sqlite-select gnosis-db "PRAGMA quick_check")))
                    (should-not (sqlite-select gnosis-db "PRAGMA foreign_key_check")))
                (sqlite-close gnosis-db)))
            (let ((before (gnosis-test-safety-snapshot file)))
              (dotimes (_ 2)
                (let ((failure (should-error (gnosis--ensure-db))))
                  (should (string-match-p
                           (format "Invalid %s ownership constraint" table)
                           (error-message-string failure))))
                (should-not gnosis-db)
                (should (equal before (gnosis-test-safety-snapshot file)))))))))))

(ert-deftest gnosis-schema-ownership-baseline-is-required ()
  (gnosis-test-ownership--reject "scheduler_baseline" "thema_id" "themata" "id"))

(ert-deftest gnosis-schema-ownership-state-is-required ()
  (gnosis-test-ownership--reject "scheduler_state" "thema_id" "scheduler_baseline" "thema_id"))

(ert-deftest gnosis-schema-ownership-review-events-is-required ()
  (gnosis-test-ownership--reject "review_events" "thema_id" "scheduler_baseline" "thema_id"))

(ert-deftest gnosis-schema-ownership-review-voids-is-required ()
  (gnosis-test-ownership--reject "review_voids" "event_id" "review_events" "event_id"))

(ert-deftest gnosis-schema-ownership-practice-voids-is-required ()
  (gnosis-test-ownership--reject "practice_voids" "event_id" "practice_events" "event_id"))

(ert-deftest gnosis-schema-ownership-valid-deletion-preserves-unrelated ()
  "Delete selected owned rows on fresh/migrated storage, preserving everything else."
  (dolist (version '(0 9))
    (gnosis-test-safety
      (let* ((file (expand-file-name "gnosis.db" gnosis-dir))
             (asset (expand-file-name "assets/shared/fixture.bin" gnosis-dir)))
        (when (= version 9)
          (let ((gnosis-db (gnosis-sqlite-open file)))
            (unwind-protect
                (progn
                  (gnosis-fixture-create-v9)
                  (gnosis-test-ownership--add-evidence 100))
              (sqlite-close gnosis-db))))
        (gnosis--ensure-db)
        (gnosis-test-ownership--add-evidence 101)
        (gnosis--insert-into 'study-history '(["retained" (:summary "Keep history")]))
        (make-directory (file-name-directory asset) t)
        (with-temp-file asset (insert "Shared fixture bytes"))
        (let ((before (gnosis-test-safety-snapshot file)))
          (gnosis-test-ownership--add-evidence 102)
          (sqlite-close gnosis-db)
          (setq gnosis-db nil)
          (gnosis--ensure-db)
          (should (gnosis-delete-thema 102 t))
          (should (equal before (gnosis-test-safety-snapshot file)))
          (sqlite-close gnosis-db)
          (setq gnosis-db nil)
          (gnosis--ensure-db)
          (should (equal before (gnosis-test-safety-snapshot file))))
        (should (equal "Shared fixture bytes"
                       (with-temp-buffer
                         (insert-file-contents-literally asset)
                         (buffer-string))))))))

(ert-deftest gnosis-schema-ownership-v9-postflight-failure-preserves-evidence ()
  "Roll back a nonempty schema 9 opener on late schema 10 error or quit."
  (dolist (fault '(error quit))
    (gnosis-test-safety
      (let ((file (expand-file-name "gnosis.db" gnosis-dir)))
        (let ((gnosis-db (gnosis-sqlite-open file)))
          (unwind-protect
              (progn
                (gnosis-fixture-create-v9)
                (gnosis-test-ownership--add-evidence 101)
                (gnosis--insert-into 'study-history '(["retained" (:summary "Keep history")])))
            (sqlite-close gnosis-db)))
        (let ((before (gnosis-test-safety-snapshot file))
              (check (symbol-function 'gnosis-db--check-schema))
              candidate)
          (cl-letf (((symbol-function 'gnosis-db--check-schema)
                     (lambda (db version)
                       (funcall check db version)
                       (when (= version 10)
                         ;; Reach postflight only after DDL and version writes.
                         (should (equal '((10)) (sqlite-select db "PRAGMA user_version")))
                         (should (gnosis-table-exists-p 'practice-encounters))
                         (setq candidate db)
                         (signal fault '("Injected schema 10 postflight fault"))))))
            (should (eq fault (condition-case err
                                  (progn (gnosis--ensure-db) 'accepted)
                                (error (car err)) (quit 'quit)))))
          (should candidate)
          (should-not gnosis-db)
          (should-error (sqlite-select candidate "SELECT 1"))
          (should (equal before (gnosis-test-safety-snapshot file)))
          (should (gnosis--ensure-db))
          (should (= 10 (gnosis--db-version)))
          (should-not (gnosis-select '* 'practice-encounters))
          (should (equal (nth 2 before)
                         (assoc-delete-all
                          "practice_encounters"
                          (nth 2 (gnosis-test-safety-snapshot file))))))))))

(provide 'gnosis-test-schema-ownership)
;;; gnosis-test-schema-ownership.el ends here

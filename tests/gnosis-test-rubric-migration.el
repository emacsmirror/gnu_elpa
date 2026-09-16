;;; gnosis-test-rubric-migration.el --- Released rubric upgrade tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

(require 'ert)
(require 'gnosis-fixture-schema-v9)
(require 'gnosis-test-db-safety)
(require 'gnosis-test-helpers)

(defun gnosis-test-rubric--source (version &optional archive)
  "Create populated released VERSION on this connection, with optional ARCHIVE.
ARCHIVE is before or after for the retained schema-10 column positions."
  (if (= version 8)
      (progn
        (gnosis-test--create-v8-schema)
        (gnosis--insert-into 'themata '([101 "basic" "Question" ("Hint") ("Answer") "guid"]))
        (gnosis--insert-into 'review '([101 1 0]))
        (gnosis--insert-into 'review-log '([101 20260901 20260902 1 2 0 1 0 3])))
    (let ((gnosis-fixture-v9-ddl
           (if (eq archive 'before)
               (mapcar (lambda (sql)
                         (if (string-prefix-p "CREATE TABLE themata " sql)
                             (string-replace "accepted_aliases TEXT"
                                             "archived_at_us INTEGER, accepted_aliases TEXT" sql)
                           sql))
                       gnosis-fixture-v9-ddl)
             gnosis-fixture-v9-ddl)))
      (gnosis-fixture-create-v9))
    (when (= version 10) (gnosis-db--migrate-v10))
    (when (eq archive 'after)
      (sqlite-execute gnosis-db "ALTER TABLE themata ADD COLUMN archived_at_us INTEGER"))
    (gnosis-fixture-add-basic 101 "Question")
    (gnosis-update 'themata '(= accepted-aliases ("Alternative")) '(= id 101))
    (when archive (sqlite-execute gnosis-db "UPDATE themata SET archived_at_us = 123"))
    (gnosis-scheduler-accept-review (make-string 64 ?a) 101 'success 1000000 20260913)
    (gnosis--insert-into 'practice-events '(["retained" 101 "batch" 1 2000000 1]))
    (when (= version 10)
      (gnosis--insert-into 'practice-encounters '(["retained" (:response "Old answer")])))
    (gnosis--insert-into 'study-history '(["batch" (:mode practice :completed-p t)]))
    (gnosis--insert-into 'study-session '([1 (:mode practice :remaining (101))])))
  (gnosis-db--check-schema gnosis-db version))

(ert-deftest gnosis-rubric-migration-ten-preserves-populated-rows-and-reopens ()
  (dolist (archive '(nil before after))
    (gnosis-test-with-old-db
      (gnosis-test-rubric--source 10 archive)
      (let ((before (nth 2 (gnosis-test-safety-snapshot gnosis-test--db-file))))
        (dotimes (_ 2)
          (gnosis-db-init)
          (should (= 11 (gnosis--db-version)))
          (gnosis-db--check-schema gnosis-db 11)
          (should (equal before
                         (gnosis-test-safety-without-rubric
                          (nth 2 (gnosis-test-safety-snapshot gnosis-test--db-file)))))
          (sqlite-close gnosis-db)
          (setq gnosis-db (gnosis-sqlite-open gnosis-test--db-file)))))))

(ert-deftest gnosis-rubric-migration-chain-error-and-quit-roll-back-everything ()
  (dolist (version '(8 9 10))
    (dolist (fault '(error quit))
      (gnosis-test-with-old-db
        (gnosis-test-rubric--source version)
        (let ((before (gnosis-test-safety-snapshot gnosis-test--db-file))
              (set-version (symbol-function 'gnosis--db-set-version)))
          (cl-letf (((symbol-function 'gnosis--db-set-version)
                     (lambda (target)
                       (funcall set-version target)
                       (when (= target 11)
                         (should (member "rubric" (mapcar #'cadr
                                                  (sqlite-select gnosis-db "PRAGMA table_info(themata)"))))
                         (signal fault '("Rubric migration fault"))))))
            (should (eq fault (condition-case err (progn (gnosis-db-init) nil)
                                (error (car err)) (quit 'quit)))))
          (should (equal before (gnosis-test-safety-snapshot gnosis-test--db-file)))
          (gnosis-db-init)
          (should (= 11 (gnosis--db-version)))
          (should-not (gnosis-get 'rubric 'themata '(= id 101))))))))

(ert-deftest gnosis-rubric-migration-ten-postflight-closes-unpublished-owner ()
  (dolist (fault '(error quit))
    (gnosis-test-safety
      (let ((file (expand-file-name "gnosis.db" gnosis-dir)))
        (let ((gnosis-db (gnosis-sqlite-open file)))
          (unwind-protect (gnosis-test-rubric--source 10) (sqlite-close gnosis-db)))
        (let ((before (gnosis-test-safety-snapshot file))
              (check (symbol-function 'gnosis-db--check-schema)) candidate)
          (cl-letf (((symbol-function 'gnosis-db--check-schema)
                     (lambda (db version)
                       (funcall check db version)
                       (when (= version 11)
                         (setq candidate db)
                         (signal fault '("Rubric postflight fault"))))))
            (should (eq fault (condition-case err (progn (gnosis--ensure-db) nil)
                                (error (car err)) (quit 'quit)))))
          (should candidate)
          (should-not gnosis-db)
          (should-error (sqlite-select candidate "SELECT 1"))
          (should (equal before (gnosis-test-safety-snapshot file))))))))

(ert-deftest gnosis-rubric-migration-refuses-relabelled-private-nine-and-ten ()
  (dolist (version '(9 10))
    (gnosis-test-with-old-db
      (gnosis-test-rubric--source version)
      (sqlite-execute gnosis-db "ALTER TABLE themata ADD COLUMN rubric TEXT")
      (let ((before (gnosis-test-safety-snapshot gnosis-test--db-file)))
        (should-error (gnosis-db-init))
        (should (equal before (gnosis-test-safety-snapshot gnosis-test--db-file)))))))

(provide 'gnosis-test-rubric-migration)
;;; gnosis-test-rubric-migration.el ends here

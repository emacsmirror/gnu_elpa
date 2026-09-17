;;; gnosis-test-sqlite-batches.el --- Batch contracts -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;;; Commentary:
;; Native SQLite characterization for partitioning and transaction ownership.

;;; Code:

(require 'ert)
(require 'gnosis-test-helpers)

(ert-deftest gnosis-test-sqlite-batches-values-and-order ()
  "Batch wrappers preserve parameter encoding, ordering and caller sequences."
  (gnosis-test-with-db
    (sqlite-execute gnosis-db "CREATE TABLE batch_test (id PRIMARY KEY, value)")
    (dolist (id '(1 2 3 4 5))
      (gnosis-sqlite-execute gnosis-db "INSERT INTO batch_test VALUES (?, ?)" (list id id)))
    (let ((gnosis-sqlite--max-vars 4))
      (dolist (ids '((4 2 1 5 3) [4 2 1 5 3]))
        (dolist (value '(nil "" "nil" 7 1.5 "α\\1" ("list" "β")))
          (let* ((copy (copy-sequence ids)) (params (list value 0))
                 (params-copy (copy-tree params)))
            (should-not (gnosis-sqlite-execute-batch
                         gnosis-db "UPDATE batch_test SET value = ? WHERE id > ? AND id IN (%s)"
                         ids params))
            (should (equal (gnosis-sqlite-select-batch
                            gnosis-db "SELECT id, value FROM batch_test WHERE id > ? AND id IN (%s) ORDER BY id"
                            ids '(0))
                           (mapcar (lambda (id) (list id value)) '(1 2 4 3 5))))
            (should (equal params params-copy))
            (should (equal ids copy)))))
      ;; Duplicate rows across batches belong to generic SELECT callers.
      (should (equal (gnosis-sqlite-select-batch
                      gnosis-db "SELECT id FROM batch_test WHERE id IN (%s) ORDER BY id" '(1 2 3 4 1))
                     '((1) (2) (3) (4) (1))))
      (dolist (ids '(nil []))
        (should-not (gnosis-sqlite-execute-batch gnosis-db "INVALID %s" ids))
        (should-not (gnosis-sqlite-select-batch gnosis-db "INVALID %s" ids))))
    (gnosis-sqlite-close gnosis-db)
    (setq gnosis-db (gnosis-sqlite-open gnosis-test--db-file))
    (should (equal (gnosis-sqlite-select gnosis-db "SELECT value FROM batch_test ORDER BY id")
                   '((("list" "β")) (("list" "β")) (("list" "β")) (("list" "β")) (("list" "β")))))))

(ert-deftest gnosis-test-sqlite-batches-outer-transaction ()
  "A later failed batch propagates; only the caller owns rollback."
  (dolist (outer '(nil t))
    (dolist (fault '(error quit))
      (gnosis-test-with-db
        (sqlite-execute gnosis-db "CREATE TABLE batch_test (id PRIMARY KEY, value)")
        (dolist (id '(1 2 3))
          (sqlite-execute gnosis-db "INSERT INTO batch_test VALUES (?, 0)" (list id)))
        (let ((gnosis-sqlite--max-vars 2)
              (execute (symbol-function 'sqlite-execute))
              (calls 0) caught)
          (cl-letf (((symbol-function 'sqlite-execute)
                     (lambda (db sql &optional params)
                       (when (string-prefix-p "UPDATE batch_test" sql)
                         (cl-incf calls)
                         (when (= calls 2) (signal fault '("Injected later batch failure"))))
                       (funcall execute db sql params))))
            (condition-case err
                (if outer
                    (gnosis-sqlite-with-transaction gnosis-db
                      (gnosis-sqlite-execute-batch gnosis-db "UPDATE batch_test SET value = ? WHERE id IN (%s)" [1 2 3] '(7)))
                  (gnosis-sqlite-execute-batch gnosis-db "UPDATE batch_test SET value = ? WHERE id IN (%s)" [1 2 3] '(7)))
              ((error quit) (setq caught (car err)))))
          (should (eq caught fault))
          (should (= calls 2))
          (gnosis-sqlite-close gnosis-db)
          (setq gnosis-db (gnosis-sqlite-open gnosis-test--db-file))
          (should (equal (sqlite-select gnosis-db "SELECT value FROM batch_test ORDER BY id")
                         (if outer '((0) (0) (0)) '((7) (0) (0))))))))))

(ert-deftest gnosis-test-sqlite-batches-tag-retained-encoding ()
  "Odd tag batches retain their distinct serialization and atomic writer."
  (gnosis-test-with-db
    (dolist (id '(1 2 3 4 5))
      (gnosis-test--add-basic-thema "Q" "A" '("old") nil id))
    (let ((gnosis-sqlite--max-vars 5)
          (ids (list 1 2 3 4 5)) (tags (list nil "" "α"))
          (print-length 1) (print-level 1))
      (gnosis-modify-thema-tags ids tags '("old"))
      (should (equal ids '(1 2 3 4 5)))
      (should (equal tags '(nil "" "α")))
      (should (= (caar (sqlite-select gnosis-db "SELECT count(*) FROM thema_tag")) 15))
      (should (equal (sqlite-select gnosis-db "SELECT tag, typeof(tag) FROM thema_tag WHERE thema_id = 1 ORDER BY tag")
                     '(("\"\"" "text") ("\"α\"" "text") ("nil" "text"))))
      ;; Legacy vector input silently inserts no tags, unlike list input.
      ;; Characterize this distinction; this partitioning change does not
      ;; broaden the tag writer's list-oriented contract.
      (should-not (gnosis-modify-thema-tags [1 2] '("new") nil))
      (should (equal (sqlite-select gnosis-db "SELECT thema_id, tag FROM thema_tag WHERE thema_id IS NULL")
                     nil)))
    (gnosis-sqlite-close gnosis-db)
    (setq gnosis-db (gnosis-sqlite-open gnosis-test--db-file))
    (should (= (caar (sqlite-select gnosis-db "SELECT count(*) FROM thema_tag WHERE thema_id IS NOT NULL")) 15))))

(ert-deftest gnosis-test-sqlite-batches-tag-fault-rollback ()
  "Tag insertion failure after a real batch rolls removal and addition back."
  (dolist (fault '(error quit))
    (gnosis-test-with-db
      (dolist (id '(1 2 3))
        (gnosis-test--add-basic-thema "Q" "A" '("old") nil id))
      (let ((before (sqlite-select gnosis-db "SELECT * FROM thema_tag ORDER BY thema_id, tag"))
            (gnosis-sqlite--max-vars 4)
            (execute (symbol-function 'sqlite-execute))
            (calls 0) caught)
        (cl-letf (((symbol-function 'sqlite-execute)
                   (lambda (db sql &optional params)
                     (when (string-prefix-p "INSERT OR IGNORE INTO thema_tag" sql)
                       (cl-incf calls)
                       (when (= calls 2) (signal fault '("Injected tag failure"))))
                     (funcall execute db sql params))))
          (condition-case err (gnosis-modify-thema-tags '(1 2 3) '("new") '("old"))
            ((error quit) (setq caught (car err)))))
        (should (eq caught fault))
        (should (= calls 2))
        (gnosis-sqlite-close gnosis-db)
        (setq gnosis-db (gnosis-sqlite-open gnosis-test--db-file))
        (should (equal before (sqlite-select gnosis-db "SELECT * FROM thema_tag ORDER BY thema_id, tag")))))))

(provide 'gnosis-test-sqlite-batches)
;;; gnosis-test-sqlite-batches.el ends here

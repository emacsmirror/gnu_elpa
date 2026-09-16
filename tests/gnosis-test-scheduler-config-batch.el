;;; gnosis-test-scheduler-config-batch.el --- Bulk initialization tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Free Software Foundation, Inc.

;; Author: Thanos Apollo <public@thanosapollo.org>

;;; Commentary:

;; Verify bounded configuration reads without changing initialization facts.

;;; Code:

(require 'ert)
(require 'gnosis-scheduler)
(require 'gnosis-test-helpers)

(defun gnosis-test-scheduler-config-batch--content (rows)
  "Insert unscheduled themata for ROWS in the current test database."
  (dolist (row rows)
    (gnosis-sqlite-execute
     gnosis-db
     "INSERT INTO themata (id,type,keimenon,hypothesis,answer) VALUES (?,?,?,?,?)"
     (list (car row) "basic" "Question" '("") '("Answer")))))

(defun gnosis-test-scheduler-config-batch--snapshot ()
  "Return all content, configuration and scheduler evidence in key order."
  (mapcar (lambda (table)
            (gnosis-sqlite-select
             gnosis-db (format "SELECT * FROM %s ORDER BY 1" table)))
          '(themata scheduler_config scheduler_active scheduler_baseline
            scheduler_state review_events review_voids)))

(ert-deftest gnosis-test-scheduler-config-batch-projection-and-read-count ()
  "Read one current configuration for a multi-chunk batch; retain row facts."
  (gnosis-test-with-db
    (let* ((rows (cl-loop for id from 1 to 128
                          collect (list id (+ 20260901 (% id 20)) (% id 2))))
           (before (copy-tree rows))
           (config (gnosis-scheduler-set-retention 0.95))
           (reader (symbol-function 'gnosis-scheduler-active-config))
           (calls 0)
           ;; Seven state rows per chunk, regardless of the SQLite build.
           (gnosis-sqlite--max-vars 70))
      (gnosis-test-scheduler-config-batch--content rows)
      (cl-letf (((symbol-function 'gnosis-scheduler-active-config)
                 (lambda (&optional db)
                   (should (memq db gnosis-sqlite--transaction-dbs))
                   (cl-incf calls)
                   (funcall reader db))))
        (should-not (gnosis-scheduler-initialize-themata rows gnosis-db)))
      (should (equal before rows))
      (should
       (equal (mapcar (lambda (row) (list (car row) (cadr row) 0 0)) rows)
              (gnosis-sqlite-select
               gnosis-db "SELECT * FROM scheduler_baseline ORDER BY thema_id")))
      (should
       (equal (mapcar (lambda (row)
                        (list (car row) config nil nil nil nil (cadr row)
                              0 0 (nth 2 row)))
                      rows)
              (gnosis-sqlite-select
               gnosis-db "SELECT * FROM scheduler_state ORDER BY thema_id")))
      (should-not (gnosis-select '* 'review-events))
      (message "Bulk initialization: %d cards / %d active-config reads"
               (length rows) calls)
      (should (= calls 1))
      ;; A later call observes the newly selected configuration, not a cache.
      (let ((next (gnosis-scheduler-set-retention 0.8)))
        (gnosis-test-scheduler-config-batch--content '((129 20260921 1)))
        (should (= next (plist-get (gnosis-scheduler-initialize-thema
                                   129 20260921 1 gnosis-db)
                                  :config-id)))
        (should (= next (gnosis-get 'config-id 'scheduler-state
                                    '(= thema-id 129))))))))

(ert-deftest gnosis-test-scheduler-config-batch-empty-and-invalid ()
  "Reject empty and malformed input before acquiring a database."
  (cl-letf (((symbol-function 'gnosis--ensure-db)
             (lambda () (ert-fail "Invalid input acquired a database"))))
    (dolist (rows '(nil ((1 20260230 0)) ((1 20260901 2))
                       ((1 20260901)) (("1" 20260901 0))))
      (let ((before (copy-tree rows)))
        (should-error (gnosis-scheduler-initialize-themata rows))
        (should (equal before rows))))))

(ert-deftest gnosis-test-scheduler-config-batch-existing-and-mixed ()
  "Reject existing rows atomically, including after a fresh chunk was written."
  (gnosis-test-with-db
    (gnosis-test-scheduler-config-batch--content '((1 20260901 1) (2 20260902 0)))
    (gnosis-scheduler-initialize-thema 1 20260901 1 gnosis-db)
    (gnosis-scheduler-accept-review (make-string 64 ?a) 1 'success 100 20260901)
    (gnosis-scheduler-set-retention 0.95)
    (let ((snapshot (gnosis-test-scheduler-config-batch--snapshot))
          (gnosis-sqlite--max-vars 10))
      (dolist (rows '(((1 20260903 0)) ((2 20260902 0) (1 20260903 0))))
        (let ((before (copy-tree rows)))
          (should-error (gnosis-scheduler-initialize-themata rows gnosis-db))
          (should (equal before rows))
          (should (equal snapshot
                         (gnosis-test-scheduler-config-batch--snapshot))))))))

(ert-deftest gnosis-test-scheduler-config-batch-rollback ()
  "Roll back partial chunks on error or quit, including a caller transaction."
  (gnosis-test-with-db
    (gnosis-scheduler-set-retention 0.95)
    (let ((rows '((1 20260901 0) (2 20260902 1) (3 20260903 0)))
          (execute (symbol-function 'gnosis-sqlite-execute))
          (gnosis-sqlite--max-vars 10))
      (dolist (nested '(nil t))
        (unless nested (gnosis-test-scheduler-config-batch--content rows))
        (dolist (failure '(error quit))
          (let ((snapshot (gnosis-test-scheduler-config-batch--snapshot))
                (writes 0))
            (cl-letf (((symbol-function 'gnosis-sqlite-execute)
                       (lambda (db sql &optional params)
                         (prog1 (funcall execute db sql params)
                           (when (string-prefix-p "INSERT INTO scheduler_state" sql)
                             (when (= (cl-incf writes) 2)
                               (signal failure '("Interrupted initialization"))))))))
              (should
               (eq failure
                   (condition-case condition
                       (if nested
                           (gnosis-sqlite-with-transaction gnosis-db
                             (gnosis-test-scheduler-config-batch--content rows)
                             (gnosis-scheduler-initialize-themata rows gnosis-db))
                         (gnosis-scheduler-initialize-themata rows gnosis-db))
                     ((error quit) (car condition))))))
            (should (= writes 2))
            (should (equal snapshot
                           (gnosis-test-scheduler-config-batch--snapshot)))))
        (unless nested
          (gnosis-sqlite-execute gnosis-db "DELETE FROM themata"))))))

(provide 'gnosis-test-scheduler-config-batch)
;;; gnosis-test-scheduler-config-batch.el ends here

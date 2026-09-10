;;; gnosis-test-migration.el --- Released schema migration -*- lexical-binding: t; -*-
(require 'ert)
(require 'gnosis-test-schema-v8)
(require 'gnosis-test-helpers)
(require 'gnosis)

(defmacro gnosis-test-with-v8-db (&rest body)
  "Run BODY with a disposable released 0.10.6 database."
  (declare (indent 0) (debug t))
  `(gnosis-test-with-old-db
     (gnosis-test--create-v8-schema)
     ,@body))

(defun gnosis-test--populate-v8-scheduler-data ()
  "Insert representative content, schedules, and activity into a v8 DB."
  (dolist (row '((1 "Q1" ("A1")) (2 "Q2" ("A2"))))
    (gnosis-sqlite-execute
     gnosis-db
     "INSERT INTO themata
        (id, type, keimenon, hypothesis, answer, source_guid)
      VALUES (?, ?, ?, ?, ?, ?)"
     (list (nth 0 row) "basic" (nth 1 row) '("") (nth 2 row) nil))
    (gnosis-sqlite-execute
     gnosis-db "INSERT INTO review (id, gnosis, amnesia) VALUES (?, ?, ?)"
     (list (nth 0 row) 1 1)))
  (dolist (row '((1 20260820 20260825 3 5 0 2 0 7)
                 (2 20260821 20260901 0 0 0 0 1 0)))
    (gnosis-sqlite-execute
     gnosis-db
     "INSERT INTO review_log
        (id, last_rev, next_rev, c_success, t_success,
         c_fails, t_fails, suspend, n)
      VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)"
     row))
  (dolist (row '((20260820 5 2) (20260820 3 1) (20260821 4 0)))
    (gnosis-sqlite-execute
     gnosis-db
     "INSERT INTO activity_log (date, reviewed_total, reviewed_new)
      VALUES (?, ?, ?)"
     row)))

(ert-deftest gnosis-test-migrate-v8-to-v9-preserves-known-facts ()
  "Bootstrap scheduler storage without inventing FSRS memory or events."
  (gnosis-test-with-v8-db
    (gnosis-test--populate-v8-scheduler-data)
    (gnosis-sqlite-execute gnosis-db "PRAGMA foreign_keys = ON")
    (let ((content (gnosis-sqlite-select
                    gnosis-db "SELECT * FROM themata ORDER BY id")))
      (gnosis-db-init)
      (should (= 9 (gnosis--db-version)))
      (should (equal (mapcar (lambda (row) (append row '(nil))) content)
                     (gnosis-sqlite-select
                      gnosis-db "SELECT * FROM themata ORDER BY id"))))
    (should
     (equal '((1 20260825 7 2) (2 20260901 0 0))
            (gnosis-sqlite-select
             gnosis-db "SELECT * FROM scheduler_baseline ORDER BY thema_id")))
    (should
     (equal '((1 1 nil nil nil nil 20260825 7 2 0)
              (2 1 nil nil nil nil 20260901 0 0 1))
            (gnosis-sqlite-select
             gnosis-db "SELECT * FROM scheduler_state ORDER BY thema_id")))
    (should (= 0 (caar (gnosis-sqlite-select
                        gnosis-db "SELECT COUNT(*) FROM review_events"))))
    (should
     (equal '((20260820 8 3) (20260821 4 0))
            (gnosis-sqlite-select
             gnosis-db "SELECT * FROM review_activity_baseline ORDER BY date")))
    (should
     (equal '((20260820 8 3) (20260821 4 0))
            (gnosis-db-review-activity gnosis-db)))
    (dolist (legacy '(review review-log activity-log))
      (should-not (gnosis-table-exists-p legacy)))))

(ert-deftest gnosis-test-migrate-v8-to-v9-acceptance-journey ()
  "Migrate, review, replay, reopen, and hard-delete one overdue thema."
  (gnosis-test-with-v8-db
    (gnosis-sqlite-execute
     gnosis-db "INSERT INTO themata VALUES (?, ?, ?, ?, ?, ?)"
     '(1 "basic" "Question" ("") ("Answer") nil))
    (gnosis-sqlite-execute
     gnosis-db "INSERT INTO review VALUES (?, ?, ?)" '(1 1 1))
    (gnosis-sqlite-execute
     gnosis-db "INSERT INTO review_log VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)"
     '(1 20260810 20260820 1 1 0 1 0 2))
    (gnosis-sqlite-execute
     gnosis-db "INSERT INTO activity_log VALUES (?, ?, ?)" '(20260829 3 1))
    (gnosis-db-init)
    (should (equal '(1 20260820 2 1)
                   (car (gnosis-sqlite-select
                         gnosis-db "SELECT * FROM scheduler_baseline"))))
    (should (equal '(1 1 nil nil nil nil 20260820 2 1 0)
                   (car (gnosis-sqlite-select
                         gnosis-db "SELECT * FROM scheduler_state"))))
    (dolist (legacy '(review review-log activity-log))
      (should-not (gnosis-table-exists-p legacy)))
    (cl-letf (((symbol-function 'gnosis--today-int) (lambda () 20260830)))
      (should (equal '(1) (gnosis-review-get-due-themata)))
      (should-not (gnosis-review-is-thema-new-p 1))
      (should-not (gnosis-suspended-p 1)))
    (let* ((event-id (make-string 64 ?a))
           (first (gnosis-scheduler-accept-review
                   event-id 1 'success 1000000 20260830))
           (state-after
            (car (gnosis-sqlite-select gnosis-db
                                       "SELECT * FROM scheduler_state")))
           (retry (gnosis-scheduler-accept-review
                   event-id 1 'success 1000000 20260830))
           (activity '((20260829 3 1) (20260830 1 0))))
      (should (plist-get first :inserted-p))
      (should-not (plist-get retry :inserted-p))
      (should (= 1 (caar (gnosis-sqlite-select
                          gnosis-db "SELECT COUNT(*) FROM review_events"))))
      (should (equal state-after
                     (car (gnosis-sqlite-select
                           gnosis-db "SELECT * FROM scheduler_state"))))
      (should (equal activity (gnosis-review-activity)))
      (gnosis-sqlite-execute
       gnosis-db "DELETE FROM scheduler_state WHERE thema_id = ?" '(1))
      (gnosis-scheduler-rebuild-state 1 0 gnosis-db)
      (should (equal state-after
                     (car (gnosis-sqlite-select
                           gnosis-db "SELECT * FROM scheduler_state"))))
      (gnosis-sqlite-close gnosis-db)
      (setq gnosis-db (gnosis-sqlite-open gnosis-test--db-file))
      (gnosis-db-init)
      (should (equal state-after
                     (car (gnosis-sqlite-select
                           gnosis-db "SELECT * FROM scheduler_state"))))
      (should (equal activity (gnosis-review-activity)))
      (cl-letf (((symbol-function 'gnosis--today-int) (lambda () 20260830)))
        (should-not (gnosis-review-get-due-themata))
        (should-not (gnosis-review-is-thema-new-p 1))
        (should-not (gnosis-suspended-p 1)))
      (gnosis-delete-themata '(1))
      (dolist (table '(themata scheduler-baseline scheduler-state review-events))
        (should (= 0 (caar (gnosis-sqlite-select
                            gnosis-db
                            (format "SELECT COUNT(*) FROM %s"
                                    (gnosis-sqlite--ident table))))))))))

(ert-deftest gnosis-test-migrate-v8-to-v9-rolls-back-completely ()
  "Roll back scheduler DDL, data, and version when bootstrap fails."
  (gnosis-test-with-v8-db
    (gnosis-test--populate-v8-scheduler-data)
    (gnosis-sqlite-execute gnosis-db "PRAGMA foreign_keys = ON")
    (let ((content (gnosis-sqlite-select gnosis-db "SELECT * FROM themata"))
          (schedule (gnosis-sqlite-select gnosis-db "SELECT * FROM review_log"))
          (activity (gnosis-sqlite-select gnosis-db "SELECT * FROM activity_log"))
          failure-reached)
      (cl-letf (((symbol-function 'gnosis-db--create-scheduler-guards)
                 (lambda (_db)
                   (setq failure-reached t)
                   (error "controlled migration failure"))))
        (should-error (gnosis-db-init)))
      (should failure-reached)
      (should (= 8 (gnosis--db-version)))
      (dolist (table '(scheduler-config scheduler-baseline scheduler-state
                       review-events review-activity-baseline))
        (should-not (gnosis-table-exists-p table)))
      (should (equal content
                     (gnosis-sqlite-select gnosis-db "SELECT * FROM themata")))
      (should (equal schedule
                     (gnosis-sqlite-select gnosis-db "SELECT * FROM review_log")))
      (should (equal activity
                     (gnosis-sqlite-select gnosis-db "SELECT * FROM activity_log"))))))

(ert-deftest gnosis-test-migrate-v8-to-v9-rejects-missing-schedule ()
  "Reject incomplete legacy schedule authority without partial migration."
  (gnosis-test-with-v8-db
    (gnosis-test--populate-v8-scheduler-data)
    (gnosis-sqlite-execute gnosis-db "DELETE FROM review_log WHERE id = ?" '(2))
    (should-error (gnosis-db-init))
    (should (= 8 (gnosis--db-version)))
    (dolist (table '(scheduler-config scheduler-baseline scheduler-state
                     review-events review-activity-baseline))
      (should-not (gnosis-table-exists-p table)))
    (should (= 2 (caar (gnosis-sqlite-select
                        gnosis-db "SELECT COUNT(*) FROM themata"))))))

(provide 'gnosis-test-migration)

;;; gnosis-test-migration.el ends here

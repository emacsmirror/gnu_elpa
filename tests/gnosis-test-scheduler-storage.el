;;; gnosis-test-scheduler-storage.el --- Scheduler storage tests  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Free Software Foundation, Inc.

;; Author: Thanos Apollo <public@thanosapollo.org>
;; Keywords: extensions
;; URL: https://git.thanosapollo.org/gnosis
;; Version: 0.0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Verify the SQLite authority used by the FSRS scheduler.

;;; Code:

(require 'ert)
(require 'gnosis)
(require 'gnosis-fsrs)

(defmacro gnosis-test-scheduler--with-fresh-db (&rest body)
  "Run BODY against a freshly initialized temporary Gnosis database."
  (declare (indent 0) (debug t))
  `(let* ((dir (make-temp-file "gnosis-scheduler-" t))
          (gnosis-dir (file-name-as-directory dir))
          (gnosis-db nil)
          (gnosis-testing t))
     (unwind-protect
         (progn (gnosis--ensure-db) ,@body)
       (when gnosis-db (gnosis-sqlite-close gnosis-db))
       (delete-directory dir t))))

(ert-deftest gnosis-test-scheduler-config-fresh-default ()
  "Fresh databases install the exact pinned FSRS configuration."
  (gnosis-test-scheduler--with-fresh-db
    (should
     (equal
      (car (gnosis-sqlite-select
            gnosis-db
            "SELECT id, algorithm, model, implementation,
                    desired_retention, parameters
             FROM scheduler_config"))
      (list 1 "fsrs" "gnosis-fsrs6-v1" "fsrs-rs-6.6.1"
            gnosis-fsrs-default-retention
            gnosis-fsrs-default-parameters)))))

(ert-deftest gnosis-test-scheduler-config-enforces-identity-and-values ()
  "Scheduler configuration rejects duplicate identity and missing values."
  (gnosis-test-scheduler--with-fresh-db
    (should-error
     (gnosis-sqlite-execute
      gnosis-db
      "INSERT INTO scheduler_config VALUES (?, ?, ?, ?, ?, ?)"
      (list 1 "fsrs" "gnosis-fsrs6-v1" "fsrs-rs-6.6.1" 0.9
            gnosis-fsrs-default-parameters)))
    (should-error
     (gnosis-sqlite-execute
      gnosis-db
      "INSERT INTO scheduler_config VALUES (?, ?, ?, ?, ?, ?)"
      (list 2 "fsrs" "gnosis-fsrs6-v1" "fsrs-rs-6.6.1" nil
            gnosis-fsrs-default-parameters)))))

(ert-deftest gnosis-test-scheduler-config-is-immutable ()
  "Reject replacement, update, and deletion of scheduler configuration."
  (gnosis-test-scheduler--with-fresh-db
    (should-error
     (gnosis-sqlite-execute
      gnosis-db
      "INSERT OR REPLACE INTO scheduler_config
         SELECT * FROM scheduler_config WHERE id = 1"))
    (should-error
     (gnosis-sqlite-execute
      gnosis-db "UPDATE scheduler_config SET desired_retention = 0.8"))
    (should-error
     (gnosis-sqlite-execute gnosis-db "DELETE FROM scheduler_config"))
    (should (= 1 (caar (gnosis-sqlite-select
                        gnosis-db "SELECT COUNT(*) FROM scheduler_config"))))))

(ert-deftest gnosis-test-scheduler-baseline-roundtrip ()
  "Store one immutable scheduling baseline for a thema."
  (gnosis-test-scheduler--with-fresh-db
    (gnosis-sqlite-execute
     gnosis-db
     "INSERT INTO themata (id, type, keimenon, hypothesis, answer, source_guid) VALUES (?, ?, ?, ?, ?, ?)"
     '(1 "basic" "Question" ("") ("Answer") nil))
    (gnosis-sqlite-execute
     gnosis-db
     "INSERT INTO scheduler_baseline
        (thema_id, due_day, reps, lapses) VALUES (?, ?, ?, ?)"
     '(1 20260830 7 2))
    (should
     (equal '((1 20260830 7 2))
            (gnosis-sqlite-select
             gnosis-db
             "SELECT thema_id, due_day, reps, lapses
                FROM scheduler_baseline")))))

(ert-deftest gnosis-test-scheduler-baseline-enforces-one-existing-thema ()
  "Reject duplicate baselines and baselines without a thema."
  (gnosis-test-scheduler--with-fresh-db
    (should-error
     (gnosis-sqlite-execute
      gnosis-db
      "INSERT INTO scheduler_baseline VALUES (?, ?, ?, ?)"
      '(404 20260830 0 0)))
    (gnosis-sqlite-execute
     gnosis-db
     "INSERT INTO themata (id, type, keimenon, hypothesis, answer, source_guid) VALUES (?, ?, ?, ?, ?, ?)"
     '(1 "basic" "Question" ("") ("Answer") nil))
    (gnosis-sqlite-execute
     gnosis-db
     "INSERT INTO scheduler_baseline VALUES (?, ?, ?, ?)"
     '(1 20260830 0 0))
    (should-error
     (gnosis-sqlite-execute
      gnosis-db
      "INSERT INTO scheduler_baseline VALUES (?, ?, ?, ?)"
      '(1 20260831 0 0)))))

(ert-deftest gnosis-test-scheduler-state-roundtrips-bootstrap-projection ()
  "Round-trip a migrated projection with no FSRS memory state."
  (gnosis-test-scheduler--with-fresh-db
    (gnosis-sqlite-execute
     gnosis-db "INSERT INTO themata (id, type, keimenon, hypothesis, answer, source_guid) VALUES (?, ?, ?, ?, ?, ?)"
     '(1 "basic" "Question" ("") ("Answer") nil))
    (gnosis-sqlite-execute
     gnosis-db "INSERT INTO scheduler_baseline VALUES (?, ?, ?, ?)"
     '(1 20260830 7 2))
    (gnosis-sqlite-execute
     gnosis-db
     "INSERT INTO scheduler_state VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
     '(1 1 nil nil nil nil 20260830 7 2 0))
    (should
     (equal '((1 1 nil nil nil nil 20260830 7 2 0))
            (gnosis-sqlite-select gnosis-db "SELECT * FROM scheduler_state")))))

(ert-deftest gnosis-test-scheduler-state-enforces-authority-and-due-index ()
  "Require baseline/config authority and index due projections."
  (gnosis-test-scheduler--with-fresh-db
    (should-error
     (gnosis-sqlite-execute
      gnosis-db
      "INSERT INTO scheduler_state VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
      '(404 1 nil nil nil nil 20260830 0 0 0)))
    (gnosis-sqlite-execute
     gnosis-db "INSERT INTO themata (id, type, keimenon, hypothesis, answer, source_guid) VALUES (?, ?, ?, ?, ?, ?)"
     '(1 "basic" "Question" ("") ("Answer") nil))
    (gnosis-sqlite-execute
     gnosis-db "INSERT INTO scheduler_baseline VALUES (?, ?, ?, ?)"
     '(1 20260830 0 0))
    (should-error
     (gnosis-sqlite-execute
      gnosis-db
      "INSERT INTO scheduler_state VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
      '(1 404 nil nil nil nil 20260830 0 0 0)))
    (gnosis-sqlite-execute
     gnosis-db
     "INSERT INTO scheduler_state VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
     '(1 1 nil nil nil nil 20260830 0 0 0))
    (should-error
     (gnosis-sqlite-execute
      gnosis-db
      "INSERT INTO scheduler_state VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
      '(1 1 nil nil nil nil 20260831 0 0 0)))
    (should
     (equal '(idx_scheduler_state_due)
            (mapcar #'car
                    (gnosis-sqlite-select
                     gnosis-db
                     "SELECT name FROM sqlite_master
                        WHERE type = 'index'
                          AND name = 'idx_scheduler_state_due'"))))))

(defun gnosis-test-scheduler--insert-event-fixture
    (db &optional rating elapsed config-id event-id thema-id review-day)
  "Insert one event into DB, optionally overriding fixture fields."
  (gnosis-sqlite-execute
   db
   "INSERT INTO review_events
      (event_id, thema_id, config_id, reviewed_at_us, review_day,
       rating, elapsed_days, prior_stability, prior_difficulty,
       stability, difficulty, raw_interval_days, calendar_interval_days,
       due_day, reps_before, reps_after, lapses_before, lapses_after, new_p)
    VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
   (list (or event-id "event-1") (or thema-id 1) (or config-id 1)
         1000000 (or review-day 20260830)
         (or rating 3) (or elapsed 0)
         nil nil 2.3065 2.118104 2.3065 2 20260901 0 1 0 0 1)))

(ert-deftest gnosis-test-review-events-roundtrip-complete-evidence ()
  "Round-trip one complete immutable review event."
  (gnosis-test-scheduler--with-fresh-db
    (gnosis-sqlite-execute
     gnosis-db "INSERT INTO themata (id, type, keimenon, hypothesis, answer, source_guid) VALUES (?, ?, ?, ?, ?, ?)"
     '(1 "basic" "Question" ("") ("Answer") nil))
    (gnosis-sqlite-execute
     gnosis-db "INSERT INTO scheduler_baseline VALUES (?, ?, ?, ?)"
     '(1 20260830 0 0))
    (gnosis-test-scheduler--insert-event-fixture gnosis-db)
    (should
     (equal '("event-1" 1 1 1000000 20260830 3 0 nil nil
              2.3065 2.118104 2.3065 2 20260901 0 1 0 0 1)
            (car (gnosis-sqlite-select gnosis-db "SELECT * FROM review_events"))))))

(ert-deftest gnosis-test-review-events-enforce-binary-ordered-evidence ()
  "Reject malformed evidence and index canonical replay order."
  (gnosis-test-scheduler--with-fresh-db
    (gnosis-sqlite-execute
     gnosis-db "INSERT INTO themata (id, type, keimenon, hypothesis, answer, source_guid) VALUES (?, ?, ?, ?, ?, ?)"
     '(1 "basic" "Question" ("") ("Answer") nil))
    (should-error (gnosis-test-scheduler--insert-event-fixture gnosis-db))
    (gnosis-sqlite-execute
     gnosis-db "INSERT INTO scheduler_baseline VALUES (?, ?, ?, ?)"
     '(1 20260830 0 0))
    (should-error
     (gnosis-test-scheduler--insert-event-fixture gnosis-db nil nil 404))
    (should-error (gnosis-test-scheduler--insert-event-fixture gnosis-db 2))
    (should-error (gnosis-test-scheduler--insert-event-fixture gnosis-db 3 -1))
    (gnosis-test-scheduler--insert-event-fixture gnosis-db)
    (should-error (gnosis-test-scheduler--insert-event-fixture gnosis-db))
    (should
     (equal '(thema_id reviewed_at_us event_id)
            (mapcar (lambda (row) (nth 2 row))
                    (gnosis-sqlite-select
                     gnosis-db "PRAGMA index_info(idx_review_events_replay)"))))
    (should
     (equal '(review_day)
            (mapcar (lambda (row) (nth 2 row))
                    (gnosis-sqlite-select
                     gnosis-db "PRAGMA index_info(idx_review_events_day)"))))))

(ert-deftest gnosis-test-review-events-reject-direct-mutation ()
  "Reject direct update and deletion of immutable event evidence."
  (gnosis-test-scheduler--with-fresh-db
    (gnosis-sqlite-execute
     gnosis-db "INSERT INTO themata (id, type, keimenon, hypothesis, answer, source_guid) VALUES (?, ?, ?, ?, ?, ?)"
     '(1 "basic" "Question" ("") ("Answer") nil))
    (gnosis-sqlite-execute
     gnosis-db "INSERT INTO scheduler_baseline VALUES (?, ?, ?, ?)"
     '(1 20260830 0 0))
    (gnosis-test-scheduler--insert-event-fixture gnosis-db)
    (should-error
     (gnosis-sqlite-execute
      gnosis-db "UPDATE scheduler_baseline SET due_day = ? WHERE thema_id = ?"
      '(20260831 1)))
    (should-error
     (gnosis-sqlite-execute
      gnosis-db
      "INSERT OR REPLACE INTO scheduler_baseline
         SELECT thema_id, due_day + 1, reps, lapses
           FROM scheduler_baseline WHERE thema_id = ?"
      '(1)))
    (should-error
     (gnosis-sqlite-execute
      gnosis-db
      "INSERT OR REPLACE INTO review_events
         SELECT * FROM review_events WHERE event_id = ?"
      '("event-1")))
    (should-error
     (gnosis-sqlite-execute
      gnosis-db "DELETE FROM scheduler_baseline WHERE thema_id = ?" '(1)))
    (should-error
     (gnosis-sqlite-execute
      gnosis-db "UPDATE review_events SET due_day = ? WHERE event_id = ?"
      '(20260902 "event-1")))
    (should-error
     (gnosis-sqlite-execute
      gnosis-db "DELETE FROM review_events WHERE event_id = ?" '("event-1")))
    (should (= 20260830
               (caar (gnosis-sqlite-select
                      gnosis-db "SELECT due_day FROM scheduler_baseline"))))
    (should (= 1 (caar (gnosis-sqlite-select
                        gnosis-db "SELECT COUNT(*) FROM review_events"))))))

(ert-deftest gnosis-test-review-events-delete-only-with-hard-thema-delete ()
  "Cascade event deletion only when its owning thema is hard deleted."
  (gnosis-test-scheduler--with-fresh-db
    (gnosis-sqlite-execute
     gnosis-db "INSERT INTO themata (id, type, keimenon, hypothesis, answer, source_guid) VALUES (?, ?, ?, ?, ?, ?)"
     '(1 "basic" "Question" ("") ("Answer") nil))
    (gnosis-sqlite-execute
     gnosis-db "INSERT INTO scheduler_baseline VALUES (?, ?, ?, ?)"
     '(1 20260830 0 0))
    (gnosis-test-scheduler--insert-event-fixture gnosis-db)
    (gnosis-sqlite-execute gnosis-db "DELETE FROM themata WHERE id = ?" '(1))
    (should (= 0 (caar (gnosis-sqlite-select
                        gnosis-db "SELECT COUNT(*) FROM review_events"))))))

(ert-deftest gnosis-test-review-activity-unions-baseline-and-events ()
  "Aggregate preserved legacy activity with immutable review events."
  (gnosis-test-scheduler--with-fresh-db
    (gnosis-sqlite-execute
     gnosis-db
     "INSERT INTO review_activity_baseline VALUES (?, ?, ?), (?, ?, ?)"
     '(20260829 5 1 20260830 10 2))
    (dolist (row '((1 "Q1") (2 "Q2")))
      (gnosis-sqlite-execute
       gnosis-db "INSERT INTO themata (id, type, keimenon, hypothesis, answer, source_guid) VALUES (?, ?, ?, ?, ?, ?)"
       (list (car row) "basic" (cadr row) '("") '("A") nil))
      (gnosis-sqlite-execute
       gnosis-db "INSERT INTO scheduler_baseline VALUES (?, ?, ?, ?)"
       (list (car row) 20260830 0 0)))
    (gnosis-test-scheduler--insert-event-fixture gnosis-db)
    (gnosis-test-scheduler--insert-event-fixture
     gnosis-db nil nil nil "event-2" 2 20260831)
    (should
     (equal '((20260829 5 1) (20260830 11 3) (20260831 1 1))
            (gnosis-db-review-activity gnosis-db)))))

(ert-deftest gnosis-test-review-activity-api-preserves-evidence ()
  "Read aggregate activity without mutating its immutable evidence."
  (gnosis-test-scheduler--with-fresh-db
    (let* ((today (gnosis--today-int))
           (yesterday (gnosis--date-to-int (gnosis-date -1))))
      (gnosis-sqlite-execute
       gnosis-db
       "INSERT INTO review_activity_baseline VALUES (?, ?, ?), (?, ?, ?)"
       (list yesterday 3 1 today 5 1))
      (gnosis-sqlite-execute
       gnosis-db "INSERT INTO themata (id, type, keimenon, hypothesis, answer, source_guid) VALUES (?, ?, ?, ?, ?, ?)"
       '(1 "basic" "Q" ("") ("A") nil))
      (gnosis-sqlite-execute
       gnosis-db "INSERT INTO scheduler_baseline VALUES (?, ?, ?, ?)"
       (list 1 today 0 0))
      (gnosis-test-scheduler--insert-event-fixture
       gnosis-db nil nil nil nil nil today)
      (let ((before
             (list
              (gnosis-sqlite-select
               gnosis-db "SELECT * FROM review_activity_baseline")
              (gnosis-sqlite-select gnosis-db "SELECT * FROM review_events"))))
        (should (equal (list (list yesterday 3 1) (list today 6 2))
                       (gnosis-review-activity)))
        (should (= 6 (gnosis-get-date-total-themata today)))
        (should (= 2 (gnosis-get-date-new-themata today)))
        (should (= 4.5 (gnosis-calculate-average-daily-reviews 2)))
        (should
         (equal before
                (list
                 (gnosis-sqlite-select
                  gnosis-db "SELECT * FROM review_activity_baseline")
                 (gnosis-sqlite-select gnosis-db "SELECT * FROM review_events"))))))))

(ert-deftest gnosis-test-review-activity-missing-day-is-read-only ()
  "Return zero for missing activity without creating evidence."
  (gnosis-test-scheduler--with-fresh-db
    (let ((before (list
                   (gnosis-sqlite-select
                    gnosis-db "SELECT * FROM review_activity_baseline")
                   (gnosis-sqlite-select gnosis-db "SELECT * FROM review_events"))))
      (should (equal (list (gnosis--today-int) 0 0)
                     (gnosis-review-activity (gnosis--today-int))))
      (should (= 0 (gnosis-get-date-total-themata)))
      (should (= 0 (gnosis-get-date-new-themata)))
      (should
       (equal before
              (list
               (gnosis-sqlite-select
                gnosis-db "SELECT * FROM review_activity_baseline")
               (gnosis-sqlite-select gnosis-db "SELECT * FROM review_events")))))))

(ert-deftest gnosis-test-review-history-mutators-are-removed ()
  "Do not expose commands that mutate derived review activity."
  (require 'gnosis-review)
  (should-not (fboundp 'gnosis-history-clear))
  (should-not (fboundp 'gnosis-review-increment-activity-log)))

(ert-deftest gnosis-test-review-activity-baseline-is-immutable ()
  "Reject replacement, update, and deletion of preserved activity."
  (gnosis-test-scheduler--with-fresh-db
    (gnosis-sqlite-execute
     gnosis-db "INSERT INTO review_activity_baseline VALUES (?, ?, ?)"
     '(20260830 10 2))
    (should-error
     (gnosis-sqlite-execute
      gnosis-db "INSERT OR REPLACE INTO review_activity_baseline VALUES (?, ?, ?)"
      '(20260830 11 3)))
    (should-error
     (gnosis-sqlite-execute
      gnosis-db "UPDATE review_activity_baseline SET reviewed_total = 11"))
    (should-error
     (gnosis-sqlite-execute gnosis-db "DELETE FROM review_activity_baseline"))
    (should
     (equal '((20260830 10 2))
            (gnosis-sqlite-select
             gnosis-db "SELECT * FROM review_activity_baseline")))))

(provide 'gnosis-test-scheduler-storage)
;;; gnosis-test-scheduler-storage.el ends here

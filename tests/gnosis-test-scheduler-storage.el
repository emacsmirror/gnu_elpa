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

(ert-deftest gnosis-test-scheduler-baseline-roundtrip ()
  "Store one immutable scheduling baseline for a thema."
  (gnosis-test-scheduler--with-fresh-db
    (gnosis-sqlite-execute
     gnosis-db
     "INSERT INTO themata VALUES (?, ?, ?, ?, ?, ?)"
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
     "INSERT INTO themata VALUES (?, ?, ?, ?, ?, ?)"
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
     gnosis-db "INSERT INTO themata VALUES (?, ?, ?, ?, ?, ?)"
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
     gnosis-db "INSERT INTO themata VALUES (?, ?, ?, ?, ?, ?)"
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

(provide 'gnosis-test-scheduler-storage)
;;; gnosis-test-scheduler-storage.el ends here

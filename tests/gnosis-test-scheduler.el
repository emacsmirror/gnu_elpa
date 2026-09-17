;;; gnosis-test-scheduler.el --- Scheduler acceptance tests  -*- lexical-binding: t; -*-

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

;; Verify atomic, idempotent binary FSRS review acceptance.

;;; Code:

(require 'ert)
(require 'gnosis)
(require 'gnosis-anki)
(require 'gnosis-export-import)
(require 'gnosis-review)
(require 'gnosis-scheduler)
(require 'gnosis-test-helpers)

(defmacro gnosis-test-scheduler--with-db (&rest body)
  "Run BODY against a freshly initialized temporary database."
  (declare (indent 0) (debug t))
  `(let* ((dir (make-temp-file "gnosis-accept-" t))
          (gnosis-dir (file-name-as-directory dir))
          (gnosis-db nil)
          (gnosis-testing t))
     (unwind-protect
         (progn (gnosis--ensure-db) ,@body)
       (when gnosis-db (gnosis-sqlite-close gnosis-db))
       (delete-directory dir t))))

(defun gnosis-test-scheduler--seed-state (&optional thema-id suspended)
  "Insert a new scheduler projection for THEMA-ID with SUSPENDED state."
  (let ((id (or thema-id 1)))
    (gnosis-sqlite-execute
     gnosis-db "INSERT INTO themata (id, type, keimenon, hypothesis, answer, source_guid) VALUES (?, ?, ?, ?, ?, ?)"
     (list id "basic" "Question" '("") '("Answer") nil))
    (gnosis-sqlite-execute
     gnosis-db "INSERT INTO scheduler_baseline VALUES (?, ?, ?, ?)"
     (list id 20260830 0 0))
    (gnosis-sqlite-execute
     gnosis-db
     "INSERT INTO scheduler_state VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
     (list id 1 nil nil nil nil 20260830 0 0 (or suspended 0)))
    id))

(defun gnosis-test-scheduler--near-p (left right)
  "Return non-nil when LEFT and RIGHT differ by less than 0.001."
  (< (abs (- left right)) 0.001))

(defconst gnosis-test-scheduler--event-id (make-string 64 ?a)
  "Canonical event identity used by acceptance tests.")

(defconst gnosis-test-scheduler--event-id-2 (make-string 64 ?b)
  "Second canonical event identity used by replay tests.")

(ert-deftest gnosis-test-scheduler-event-id-is-generated-before-effects ()
  "Generate distinct stable text identities without touching storage."
  (let ((first (gnosis-scheduler-event-id))
        (second (gnosis-scheduler-event-id)))
    (should (string-match-p "\\`[[:xdigit:]]\\{64\\}\\'" first))
    (should-not (equal first second))))

(ert-deftest gnosis-test-scheduler-rejects-malformed-id-before-db-access ()
  "Reject noncanonical identities before opening or touching the database."
  (let (db-touched)
    (cl-letf (((symbol-function 'gnosis--ensure-db)
               (lambda () (setq db-touched t) (error "DB touched"))))
      (dolist (event-id (list "" "abc" (make-string 63 ?a)
                              (make-string 64 ?A)
                              (concat (make-string 63 ?a) "g")))
        (should-error
         (gnosis-scheduler-accept-review
          event-id 1 'success 1000000 20260830))))
    (should-not db-touched)))

(ert-deftest gnosis-test-scheduler-accept-good-atomically ()
  "Accept Good as one event and one matching state projection."
  (gnosis-test-scheduler--with-db
    (gnosis-test-scheduler--seed-state)
    (let ((result (gnosis-scheduler-accept-review
                   gnosis-test-scheduler--event-id 1 'success 1000000 20260830)))
      (should (= 3 (plist-get result :rating)))
      (should (= 2 (plist-get result :calendar-interval-days)))
      (should (= 20260901 (plist-get result :due-day)))
      (should (= 1 (plist-get result :reps-after)))
      (should (= 0 (plist-get result :lapses-after)))
      (should (= 1 (plist-get result :new-p)))
      (should (gnosis-test-scheduler--near-p
               2.3065 (plist-get result :stability)))
      (should (gnosis-test-scheduler--near-p
               2.118104 (plist-get result :difficulty)))
      (should (= 1 (caar (gnosis-sqlite-select
                          gnosis-db "SELECT COUNT(*) FROM review_events"))))
      (should
       (equal (list (plist-get result :stability)
                    (plist-get result :difficulty)
                    1000000 20260830 20260901 1 0)
              (car (gnosis-sqlite-select
                    gnosis-db
                    "SELECT stability, difficulty, last_reviewed_at_us,
                            last_review_day, due_day, reps, lapses
                       FROM scheduler_state WHERE thema_id = 1")))))))

(ert-deftest gnosis-test-scheduler-accept-failure-as-again ()
  "Map binary failure to Again and increment the lapse count."
  (gnosis-test-scheduler--with-db
    (gnosis-test-scheduler--seed-state)
    (let ((result (gnosis-scheduler-accept-review
                   gnosis-test-scheduler--event-id 1 'failure 1000000 20260830)))
      (should (= 1 (plist-get result :rating)))
      (should (= 1 (plist-get result :calendar-interval-days)))
      (should (= 20260831 (plist-get result :due-day)))
      (should (= 1 (plist-get result :lapses-after))))))

(ert-deftest gnosis-test-scheduler-retry-is-idempotent ()
  "Return retained evidence on exact retry and reject identity conflicts."
  (gnosis-test-scheduler--with-db
    (gnosis-test-scheduler--seed-state)
    (let* ((first (gnosis-scheduler-accept-review
                   gnosis-test-scheduler--event-id 1 'success 1000000 20260830))
           (retry (gnosis-scheduler-accept-review
                   gnosis-test-scheduler--event-id 1 'success 1000000 20260830)))
      (should (plist-get first :inserted-p))
      (should-not (plist-get retry :inserted-p))
      (should (gnosis-scheduler--evidence-equal-p first retry))
      (should-error
       (gnosis-scheduler-accept-review
        gnosis-test-scheduler--event-id 1 'failure 1000000 20260830))
      (should (= 1 (caar (gnosis-sqlite-select
                          gnosis-db "SELECT COUNT(*) FROM review_events"))))
      (should (= 1 (caar (gnosis-sqlite-select
                          gnosis-db "SELECT reps FROM scheduler_state")))))))

(ert-deftest gnosis-test-scheduler-acceptance-rolls-back-both-effects ()
  "Roll back event insertion when state projection cannot update."
  (gnosis-test-scheduler--with-db
    (gnosis-test-scheduler--seed-state)
    (gnosis-sqlite-execute
     gnosis-db
     "CREATE TRIGGER controlled_state_failure
        BEFORE UPDATE ON scheduler_state
        BEGIN SELECT RAISE(ABORT, 'controlled state failure'); END")
    (should-error
     (gnosis-scheduler-accept-review
      gnosis-test-scheduler--event-id 1 'success 1000000 20260830))
    (should (= 0 (caar (gnosis-sqlite-select
                        gnosis-db "SELECT COUNT(*) FROM review_events"))))
    (should
     (equal '((nil nil nil nil 20260830 0 0))
            (gnosis-sqlite-select
             gnosis-db
             "SELECT stability, difficulty, last_reviewed_at_us,
                     last_review_day, due_day, reps, lapses
                FROM scheduler_state WHERE thema_id = 1")))))

(ert-deftest gnosis-test-scheduler-rejects-nonbinary-input-before-effects ()
  "Reject unsupported ratings before inserting or updating anything."
  (gnosis-test-scheduler--with-db
    (gnosis-test-scheduler--seed-state)
    (should-error
     (gnosis-scheduler-accept-review gnosis-test-scheduler--event-id 1 'hard 1000000 20260830))
    (should (= 0 (caar (gnosis-sqlite-select
                        gnosis-db "SELECT COUNT(*) FROM review_events"))))
    (should (= 0 (caar (gnosis-sqlite-select
                        gnosis-db "SELECT reps FROM scheduler_state"))))))

(ert-deftest gnosis-test-scheduler-rejects-unsupported-config-snapshot ()
  "Reject an immutable config whose parameters are not the pinned model."
  (gnosis-test-scheduler--with-db
    (gnosis-test-scheduler--seed-state)
    (let ((parameters (copy-sequence gnosis-fsrs-default-parameters)))
      (aset parameters 0 9.9)
      (gnosis-sqlite-execute
       gnosis-db "INSERT INTO scheduler_config VALUES (?, ?, ?, ?, ?, ?)"
       (list 2 "fsrs" "gnosis-fsrs6-v1" "fsrs-rs-6.6.1" 0.9 parameters)))
    (gnosis-sqlite-execute
     gnosis-db "UPDATE scheduler_state SET config_id = 2 WHERE thema_id = 1")
    (should-error
     (gnosis-scheduler-accept-review
      gnosis-test-scheduler--event-id 1 'success 1000000 20260830))
    (should (= 0 (caar (gnosis-sqlite-select
                        gnosis-db "SELECT COUNT(*) FROM review_events"))))
    (should (= 0 (caar (gnosis-sqlite-select
                        gnosis-db "SELECT reps FROM scheduler_state"))))))

(ert-deftest gnosis-test-review-pending-time-crosses-day-boundary ()
  "Capture coherent evidence even when the next clock read crosses 03:00."
  (gnosis-test-scheduler--with-db
    (gnosis-test-scheduler--seed-state)
    (let* ((gnosis-day-start-hour 3)
           (boundary (encode-time 0 0 3 31 8 2026))
           (before (time-subtract boundary '(1 . 1000000)))
           (timestamp (car (time-convert before 1000000)))
           (convert (symbol-function 'time-convert))
           (reads 0)
           (clock (lambda () (if (= (cl-incf reads) 1) before boundary)))
           (pending
            (cl-letf (((symbol-function 'current-time) clock)
                      ((symbol-function 'time-convert)
                       (lambda (time form)
                         (funcall convert (or time (funcall clock)) form))))
              (gnosis-review--pending-result
               1 t gnosis-test-scheduler--event-id))))
      (should (= timestamp (plist-get pending :reviewed-at-us)))
      (should (= 20260830 (plist-get pending :review-day)))
      (let ((accepted (gnosis-review-result 1 t pending)))
        (should (= 20260901 (plist-get accepted :due-day)))
        (should-not (plist-get (gnosis-review-result 1 t pending) :inserted-p)))
      (should (equal (list (list timestamp 20260830))
                     (gnosis-sqlite-select
                      gnosis-db
                      "SELECT reviewed_at_us, review_day FROM review_events"))))))

(ert-deftest gnosis-test-review-pending-time-explicit-evidence ()
  "Derive an omitted day from the timestamp but retain an explicit day."
  (gnosis-test-scheduler--with-db
    (gnosis-test-scheduler--seed-state)
    (let* ((gnosis-day-start-hour 3)
           (timestamp (car (time-convert (encode-time 59 59 2 31 8 2026)
                                         1000000))))
      (cl-letf (((symbol-function 'current-time)
                 (lambda () (encode-time 0 0 3 31 8 2026))))
        (let* ((pending (gnosis-review--pending-result
                         1 t gnosis-test-scheduler--event-id timestamp))
               (explicit (gnosis-review--pending-result
                          1 t gnosis-test-scheduler--event-id timestamp 20260829))
               (override (gnosis-review--override-result explicit nil)))
          (should (= 20260830 (plist-get pending :review-day)))
          (dolist (result (list explicit override))
            (should (= timestamp (plist-get result :reviewed-at-us)))
            (should (= 20260829 (plist-get result :review-day)))
            (should (equal gnosis-test-scheduler--event-id
                           (plist-get result :event-id))))
          (should (= 20260831 (plist-get (plist-get explicit :preview)
                                        :due-day))))))))

(ert-deftest gnosis-test-review-relearning-preserves-due-count ()
  "Relearning and early reviews do not consume other themata's due count."
  (gnosis-test-scheduler--with-db
    (gnosis-test-scheduler--seed-state 1)
    (gnosis-test-scheduler--seed-state 2)
    (let ((gnosis-due-themata-total 2))
      (cl-letf (((symbol-function 'gnosis--today-int) (lambda () 20260830)))
        (gnosis-review-result
         1 nil (gnosis-review--pending-result
                1 nil gnosis-test-scheduler--event-id 1000000 20260830))
        (should (= 1 gnosis-due-themata-total))
        (gnosis-review-result
         1 t (gnosis-review--pending-result
              1 t gnosis-test-scheduler--event-id-2 2000000 20260830))
        (should (= 1 gnosis-due-themata-total))
        (should (equal '(2) (gnosis-review-get-due-themata)))))))

(ert-deftest gnosis-test-review-retry-repairs-post-commit-count-failure ()
  "Repair the due count on retry after its first post-commit read failed."
  (gnosis-test-scheduler--with-db
    (gnosis-test-scheduler--seed-state 1)
    (gnosis-test-scheduler--seed-state 2)
    (let ((gnosis-due-themata-total 2))
      (cl-letf (((symbol-function 'gnosis--today-int) (lambda () 20260830)))
        (let ((pending (gnosis-review--pending-result
                        1 nil gnosis-test-scheduler--event-id 1000000 20260830)))
          (cl-letf (((symbol-function 'gnosis-review-get-due-themata)
                     (lambda () (error "Controlled post-commit read failure"))))
            (should-error (gnosis-review-result 1 nil pending)))
          (should (= 1 (caar (gnosis-sqlite-select
                              gnosis-db "SELECT COUNT(*) FROM review_events"))))
          (should (= 2 gnosis-due-themata-total))
          (should-not (plist-get (gnosis-review-result 1 nil pending) :inserted-p))
          (should (= 1 gnosis-due-themata-total))
          (should (equal '(2) (gnosis-review-get-due-themata)))
          (should (= 1 (caar (gnosis-sqlite-select
                              gnosis-db "SELECT COUNT(*) FROM review_events")))))))))

(ert-deftest gnosis-test-scheduler-rebuilds-projection-from-events ()
  "Replay two events and rebuild the deleted projection exactly."
  (gnosis-test-scheduler--with-db
    (gnosis-test-scheduler--seed-state)
    (gnosis-scheduler-accept-review
     gnosis-test-scheduler--event-id 1 'success 1000000 20260830)
    (gnosis-scheduler-accept-review
     gnosis-test-scheduler--event-id-2 1 'failure 2000000 20260831)
    (let ((expected (car (gnosis-sqlite-select
                          gnosis-db "SELECT * FROM scheduler_state")))
          (first (gnosis-scheduler-replay-thema 1 0 gnosis-db)))
      (should (equal first (gnosis-scheduler-replay-thema 1 0 gnosis-db)))
      (gnosis-sqlite-execute gnosis-db
                             "DELETE FROM scheduler_state WHERE thema_id = 1")
      (let ((rebuilt (gnosis-scheduler-rebuild-state 1 0 gnosis-db)))
        (should (equal first rebuilt))
        (should (= 0 (plist-get rebuilt :new-p)))
        (should (equal expected
                       (car (gnosis-sqlite-select
                             gnosis-db "SELECT * FROM scheduler_state"))))))))

(ert-deftest gnosis-test-scheduler-replay-rejects-evidence-drift ()
  "Reject config, prior-memory, and result drift in immutable evidence."
  (gnosis-test-scheduler--with-db
    (gnosis-test-scheduler--seed-state)
    (gnosis-scheduler-accept-review
     gnosis-test-scheduler--event-id 1 'success 1000000 20260830)
    (let* ((baseline (car (gnosis-sqlite-select
                           gnosis-db "SELECT * FROM scheduler_baseline")))
           (event (car (gnosis-sqlite-select
                        gnosis-db "SELECT * FROM review_events")))
           (configs '((1 . 0.9))))
      (should (gnosis-scheduler-replay baseline (list event) configs 0))
      (should-error (gnosis-scheduler-replay baseline nil nil 0))
      (let* ((changed (copy-sequence event))
             (large (+ most-positive-fixnum 10))
             (distinct (string-to-number (number-to-string large))))
        (setcar (nthcdr 2 changed) large)
        (should (gnosis-scheduler-replay
                 baseline (list changed)
                 (list '(1 . 0.9) (cons distinct 0.9)) 0)))
      (dolist (mutation '((2 . 2) (8 . 4.0) (13 . 20260909)))
        (let ((changed (copy-sequence event)))
          (setcar (nthcdr (car mutation) changed) (cdr mutation))
          (should-error
           (gnosis-scheduler-replay baseline (list changed) configs 0)))))))

(ert-deftest gnosis-test-scheduler-rebuild-requires-current-suspension ()
  "Preserve the explicit current suspension fact during projection rebuild."
  (gnosis-test-scheduler--with-db
    (gnosis-test-scheduler--seed-state 1 1)
    (gnosis-scheduler-accept-review
     gnosis-test-scheduler--event-id 1 'success 1000000 20260830)
    (gnosis-sqlite-execute gnosis-db
                           "DELETE FROM scheduler_state WHERE thema_id = 1")
    (should-error (gnosis-scheduler-rebuild-state 1 nil gnosis-db))
    (gnosis-scheduler-rebuild-state 1 1 gnosis-db)
    (should (= 1 (caar (gnosis-sqlite-select
                        gnosis-db "SELECT suspended FROM scheduler_state"))))))

(ert-deftest gnosis-test-scheduler-ordinary-creation-initializes-state ()
  "Initialize scheduler storage through every ordinary creation path."
  (gnosis-test-scheduler--with-db
    (gnosis-add-thema-fields
     "basic" "Q1" '("") '("A1") "" '("test") 0 nil nil 101)
    (gnosis-update-thema 102 "Q2" '("") '("A2") "" '("test") nil "basic")
    (should-not
     (gnosis-save-thema '(103 "basic" "Q3" ("") ("A3") "" ("test") 1)))
    (gnosis-test--add-basic-thema "Q4" "A4" nil nil 104 0)
    (let ((today (gnosis--today-int)))
      (should
       (equal (list (list 101 today 0 0) (list 102 today 0 0)
                    (list 103 today 0 0) (list 104 today 0 0))
              (gnosis-sqlite-select
               gnosis-db "SELECT * FROM scheduler_baseline ORDER BY thema_id")))
      (should
       (equal `((101 1 nil nil nil nil ,today 0 0 0)
                (102 1 nil nil nil nil ,today 0 0 0)
                (103 1 nil nil nil nil ,today 0 0 0)
                (104 1 nil nil nil nil ,today 0 0 0))
              (gnosis-sqlite-select
               gnosis-db "SELECT * FROM scheduler_state ORDER BY thema_id"))))))

(ert-deftest gnosis-test-scheduler-ordinary-creation-rolls-back-together ()
  "Roll back content and scheduler rows when initialization fails."
  (gnosis-test-scheduler--with-db
    (gnosis-sqlite-execute
     gnosis-db
     "CREATE TRIGGER controlled_initializer_failure
        BEFORE INSERT ON scheduler_state
        BEGIN SELECT RAISE(ABORT, 'controlled initializer failure'); END")
    (should-error
     (gnosis-add-thema-fields
      "basic" "Question" '("") '("Answer") "" '("test") 0 nil nil 101))
    (dolist (table '(themata scheduler-baseline scheduler-state))
      (should (= 0 (caar (gnosis-sqlite-select
                          gnosis-db
                          (format "SELECT COUNT(*) FROM %s"
                                  (gnosis-sqlite--ident table)))))))))

(ert-deftest gnosis-test-scheduler-ordinary-creation-captures-one-day ()
  "Use one captured logical day for every new-card schedule row."
  (gnosis-test-scheduler--with-db
    (let (calls)
      (cl-letf (((symbol-function 'gnosis--today-int)
                 (lambda ()
                   (push t calls)
                   (if (= (length calls) 1) 20260830 20260831))))
        (gnosis-add-thema-fields
         "basic" "Question" '("") '("Answer") "" '("test") 0 nil nil 101))
      (should (= 1 (length calls)))
      (should (= 20260830
                 (caar (gnosis-sqlite-select
                        gnosis-db
                        "SELECT due_day FROM scheduler_baseline
                          WHERE thema_id = 101")))))))

(ert-deftest gnosis-test-scheduler-all-creation-paths-are-due-and-new ()
  "Read ordinary, Anki, and SQLite imports from scheduler authority."
  (gnosis-test-scheduler--with-db
    (let ((export-file (concat (make-temp-file "gnosis-due-import-") ".db"))
          (today (gnosis--today-int)))
      (unwind-protect
          (progn
            (gnosis-add-thema-fields
             "basic" "Ordinary" '("") '("A") "" '("test") 0 nil nil 101)
            (gnosis-anki--bulk-insert-chunk
             gnosis-db
             (list (list :type "basic" :keimenon "Anki" :hypothesis '("")
                         :answer '("A") :parathema "" :tags '("test")))
             '(102) today)
            (gnosis-add-thema-fields
             "basic" "SQLite" '("") '("A") "" '("test") 0 nil nil 103)
            (gnosis-export-db export-file)
            (gnosis-sqlite-execute gnosis-db
                                   "DELETE FROM themata WHERE id = 103")
            (gnosis-import--apply-changes
             export-file '(103) nil
             (gnosis-import--file-sha256 export-file))
            (let ((expected '(101 102 103))
                  (gnosis-new-themata-limit nil)
                  (gnosis-review-new-first t))
              (should (equal expected (gnosis-review-get-due-themata)))
              (should (equal expected (sort (gnosis-get-themata-by-reviews 0)
                                            #'<)))
              (dolist (id expected)
                (should (gnosis-review-is-thema-new-p id))
                (should-not (gnosis-suspended-p id)))))
        (when (file-exists-p export-file)
          (delete-file export-file))))))

(ert-deftest gnosis-test-scheduler-review-override-accepts-final-rating-once ()
  "Preview Good, override to Again, and accept one final immutable event."
  (gnosis-test-scheduler--with-db
    (gnosis-test-scheduler--seed-state)
    (gnosis-test-scheduler--seed-state 2)
    (let* ((today (gnosis--today-int))
           (gnosis-due-themata-total 2)
           (pending (gnosis-review--pending-result
                     1 t gnosis-test-scheduler--event-id 1000000 today))
           overridden final-success)
      (cl-letf (((symbol-function 'gnosis-display-next-review) #'ignore))
        (let ((next (gnosis-review-action--override t 1 pending)))
          (setq final-success (car next)
                overridden (cdr next))))
      (should-not final-success)
      (should (= 3 (plist-get (plist-get pending :preview) :rating)))
      (should (= 1 (plist-get (plist-get overridden :preview) :rating)))
      (should (equal (plist-get pending :event-id)
                     (plist-get overridden :event-id)))
      (should (equal (gnosis-review--result-date overridden)
                     (gnosis--int-to-date
                      (plist-get (plist-get overridden :preview) :due-day))))
      (should (= 0 (caar (gnosis-sqlite-select
                          gnosis-db "SELECT COUNT(*) FROM review_events"))))
      (should-error (gnosis-review-result 1 t overridden))
      (gnosis-review-result 1 nil overridden)
      (should (= 1 gnosis-due-themata-total))
      (gnosis-review-result 1 nil overridden)
      (should (= 1 gnosis-due-themata-total))
      (should (= 1 (caar (gnosis-sqlite-select
                          gnosis-db "SELECT COUNT(*) FROM review_events"))))
      (should (equal '(1 1 1)
                     (car (gnosis-sqlite-select
                           gnosis-db "SELECT rating, reps, lapses
                                      FROM review_events JOIN scheduler_state
                                        USING (thema_id)")))))))

(ert-deftest gnosis-test-scheduler-review-basic-displays-preview-before-accept ()
  "Display pending FSRS due date, then accept only on explicit result."
  (gnosis-test-scheduler--with-db
    (let ((id (gnosis-test--add-basic-thema "Question" "Answer"))
          displayed)
      (cl-letf (((symbol-function 'gnosis--read-string-with-input-method)
                 (lambda (&rest _) "Answer"))
                ((symbol-function 'gnosis-display-next-review)
                 (lambda (date _success) (setq displayed date)))
                ((symbol-function 'gnosis-display-image) #'ignore)
                ((symbol-function 'gnosis-display-keimenon) #'ignore)
                ((symbol-function 'gnosis-display-hint) #'ignore)
                ((symbol-function 'gnosis-display-basic-answer) #'ignore)
                ((symbol-function 'gnosis-display-parathema) #'ignore))
        (pcase-let ((`(,success . ,result) (gnosis-review-basic id)))
          (should success)
          (should (equal displayed (gnosis-review--result-date result)))
          (should (= 0 (caar (gnosis-sqlite-select
                              gnosis-db "SELECT COUNT(*) FROM review_events"))))
          (gnosis-review-result id success result)
          (should (= 1 (caar (gnosis-sqlite-select
                              gnosis-db "SELECT COUNT(*) FROM review_events")))))))))

(ert-deftest gnosis-test-scheduler-review-covers-timing-and-same-day ()
  "Accept Good early/on-time/overdue plus bootstrap and same-day reviews."
  (gnosis-test-scheduler--with-db
    (let* ((today (gnosis--today-int))
           (last-day (gnosis--date-to-int (gnosis-date -10)))
           (timing '((-1 . early) (0 . on-time) (1 . overdue))))
      (cl-loop for id from 201 to 203
               for (due-offset . class) in timing
               for event-char from ?a
               do (progn
                    (gnosis-test--add-basic-thema "Q" "A" nil nil id 0)
                    (gnosis-sqlite-execute
                     gnosis-db "UPDATE scheduler_state
                                   SET stability = 10.0, difficulty = 5.0,
                                       last_reviewed_at_us = 1,
                                       last_review_day = ?, due_day = ?, reps = 1
                                 WHERE thema_id = ?"
                     (list last-day
                           (gnosis--date-to-int
                            (gnosis-date (- due-offset)))
                           id))
                    (let ((pending (gnosis-review--pending-result
                                    id t (make-string 64 event-char)
                                    (+ 2000000 id) today)))
                      (let ((previous-due
                             (gnosis-get 'due-day 'scheduler-state
                                         `(= thema-id ,id))))
                        (pcase class
                          ('early (should (> previous-due today)))
                          ('on-time (should (= previous-due today)))
                          ('overdue (should (< previous-due today))))
                      (should (= 10 (plist-get (plist-get pending :preview)
                                               :elapsed-days)))
                        (gnosis-review-result id t pending)))))
      (gnosis-test--add-basic-thema "Bootstrap" "A" nil nil 204 0)
      (let ((first (gnosis-review--pending-result
                    204 t (make-string 64 ?d) 3000000 today)))
        (gnosis-review-result 204 t first))
      (let ((second (gnosis-review--pending-result
                     204 t (make-string 64 ?e) 3000001 today)))
        (gnosis-review-result 204 t second))
      (should (equal '(0 0)
                     (mapcar #'car
                             (gnosis-sqlite-select
                              gnosis-db "SELECT elapsed_days FROM review_events
                                         WHERE thema_id = 204
                                         ORDER BY reviewed_at_us"))))
      (should (equal '(3 3 3 3 3)
                     (mapcar #'car
                             (gnosis-sqlite-select
                              gnosis-db "SELECT rating FROM review_events
                                         ORDER BY thema_id, reviewed_at_us"))))
      (should (= 5 (caar (gnosis-sqlite-select
                          gnosis-db "SELECT COUNT(*) FROM review_events")))))))

(ert-deftest gnosis-test-scheduler-retention-rejects-calendar-overflow ()
  "Reject unrepresentable intervals before activating their configuration."
  (gnosis-test-scheduler--with-db
    (gnosis-test-scheduler--seed-state)
    (let ((before (mapcar (lambda (table) (gnosis-select '* table))
                         '(scheduler-config scheduler-active scheduler-state
                           review-events))))
      ;; 0.01 already fails on a first review; 0.1 only fails as memory grows.
      (dolist (retention '(0.01 0.1))
        (let* ((prior (and (= retention 0.1)
                           '(:stability 36500.0 :difficulty 1.0)))
               (transition (gnosis-fsrs-transition prior 0 'success retention)))
          (should (gnosis-fsrs--finite-number-p
                   (plist-get transition :raw-interval-days)))
          (should-error
           (gnosis-scheduler--add-days
            20261231 (plist-get transition :calendar-interval-days)))
          (should-error (gnosis-scheduler-set-retention retention)
                        :type 'user-error)))
      (should (equal before
                     (mapcar (lambda (table) (gnosis-select '* table))
                             '(scheduler-config scheduler-active scheduler-state
                               review-events)))))
    ;; A rejected preference must leave the next real review usable.
    (let* ((preview (gnosis-scheduler-preview-review
                     gnosis-test-scheduler--event-id 1 'success 1000000 20260830))
           (result (gnosis-scheduler-accept-review
                    gnosis-test-scheduler--event-id 1 'success 1000000 20260830
                    (plist-get preview :config-id) preview)))
      (should (= 20260901 (plist-get result :due-day)))
      (should (= 1 (plist-get (gnosis-scheduler-replay-thema 1 0) :reps))))))

(ert-deftest gnosis-test-scheduler-retention-rejects-invalid-before-db ()
  "Reject invalid or numerically overflowing preferences without storage."
  (let (db-touched)
    (cl-letf (((symbol-function 'gnosis--ensure-db)
               (lambda () (setq db-touched t) (error "DB touched"))))
      (dolist (retention '(nil "0.9" 0 1 -0.1 1.0e+INF -1.0e+INF
                              0.0e+NaN 1.0e-10 1.0e-100 5.0e-324))
        (should-error (gnosis-scheduler-set-retention retention)
                      :type 'user-error)))
    (should-not db-touched)))

(ert-deftest gnosis-test-scheduler-retention-accepted-calendar-and-replay ()
  "Preserve uncapped intervals through preview, acceptance, and replay."
  (gnosis-test-scheduler--with-db
    (gnosis-test-scheduler--seed-state)
    (cl-loop for retention in '(0.2 0.8 0.9 0.95 0.9999999999999999)
             for reviewed-at-us from 1000000
             for event-char from ?a
             for outcome in '(success failure success failure success)
             do
             (let* ((config (gnosis-scheduler-set-retention retention))
                    (event-id (make-string 64 event-char))
                    (state (gnosis-scheduler-replay-thema 1 0))
                    (prior (and (plist-get state :stability)
                                (list :stability (plist-get state :stability)
                                      :difficulty (plist-get state :difficulty))))
                    (expected (gnosis-fsrs-transition prior 0 outcome retention))
                    (preview (gnosis-scheduler-preview-review
                              event-id 1 outcome reviewed-at-us 20260830))
                    (result (gnosis-scheduler-accept-review
                             event-id 1 outcome reviewed-at-us 20260830
                             config preview))
                    (replayed (gnosis-scheduler-replay-thema 1 0)))
               (should (= config (gnosis-scheduler-set-retention retention)))
               (should (= (plist-get expected :calendar-interval-days)
                          (plist-get result :calendar-interval-days)))
               (should (= (plist-get expected :raw-interval-days)
                          (plist-get result :raw-interval-days)))
               (should (= (plist-get replayed :due-day)
                          (plist-get preview :due-day)))
               (should (gnosis-scheduler--day-time (plist-get result :due-day)))
               (should (gnosis-scheduler--evidence-equal-p preview result))))))

(ert-deftest gnosis-test-scheduler-retention-maximum-memory-calendar ()
  "Keep the longest supported interval usable without changing input state."
  (gnosis-test-scheduler--with-db
    (let* ((config (gnosis-scheduler-set-retention 0.2))
           (state (list config 36500.0 1.0 1 20261231 20261231 1 0 0))
           (before (copy-tree state))
           (result (gnosis-scheduler--compute-result
                    gnosis-test-scheduler--event-id 1 'success 2 20261231
                    state 0.2))
           (interval (plist-get result :calendar-interval-days))
           (due-day (plist-get result :due-day)))
      (should (= 36500.0 (plist-get result :stability)))
      (should (> interval 36500))
      (should (= interval (gnosis-scheduler--days-between 20261231 due-day)))
      (should (equal before state))
      (should (equal result
                     (gnosis-scheduler--compute-result
                      gnosis-test-scheduler--event-id 1 'success 2 20261231
                      state 0.2))))))

(provide 'gnosis-test-scheduler)
;;; gnosis-test-scheduler.el ends here

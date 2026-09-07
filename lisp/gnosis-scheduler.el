;;; gnosis-scheduler.el --- Atomic FSRS review acceptance  -*- lexical-binding: t; -*-

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

;; Accept binary reviews as immutable events and update the current FSRS
;; projection in the same SQLite transaction.

;;; Code:

(require 'cl-lib)
(require 'gnosis-db)
(require 'gnosis-fsrs)
(require 'seq)

(defconst gnosis-scheduler--event-keys
  '(:event-id :thema-id :config-id :reviewed-at-us :review-day :rating
    :elapsed-days :prior-stability :prior-difficulty :stability :difficulty
    :raw-interval-days :calendar-interval-days :due-day :reps-before
    :reps-after :lapses-before :lapses-after :new-p)
  "Ordered plist keys matching the review-events schema.")

(defun gnosis-scheduler-event-id ()
  "Return a new opaque review-event identity."
  (secure-hash 'sha256
               (gnosis-sqlite--serialize
                (list (current-time) (emacs-pid) (random) (user-uid)))))

(defun gnosis-scheduler--event-id-p (value)
  "Return non-nil when VALUE is a canonical event identity."
  (and (stringp value)
       (let ((case-fold-search nil))
         (string-match-p "\\`[0-9a-f]\\{64\\}\\'" value))))

(defun gnosis-scheduler--day-time (day)
  "Return local noon for YYYYMMDD integer DAY, rejecting invalid dates."
  (unless (and (integerp day) (> day 0))
    (error "Invalid scheduler review day"))
  (let* ((date (gnosis--int-to-date day))
         (time (encode-time 0 0 12 (nth 2 date) (nth 1 date) (nth 0 date)))
         (decoded (decode-time time))
         (roundtrip (gnosis--date-to-int
                     (list (nth 5 decoded) (nth 4 decoded) (nth 3 decoded)))))
    (unless (= day roundtrip)
      (error "Invalid scheduler review day"))
    time))

(defun gnosis-scheduler--days-between (earlier later)
  "Return logical days from EARLIER through LATER YYYYMMDD dates."
  (let* ((difference (time-subtract (gnosis-scheduler--day-time later)
                                    (gnosis-scheduler--day-time earlier)))
         (days (round (/ (float-time difference) 86400.0))))
    (when (< days 0)
      (error "Review precedes current scheduler state"))
    days))

(defun gnosis-scheduler--add-days (day days)
  "Return YYYYMMDD integer DAYS after DAY."
  (unless (and (integerp days) (>= days 0))
    (error "Invalid scheduler interval"))
  (let* ((date (gnosis--int-to-date day))
         (time (encode-time 0 0 12 (+ (nth 2 date) days)
                            (nth 1 date) (nth 0 date)))
         (decoded (decode-time time)))
    (gnosis--date-to-int
     (list (nth 5 decoded) (nth 4 decoded) (nth 3 decoded)))))

(defun gnosis-scheduler--event-result (row)
  "Return a scheduler result plist decoded from event ROW."
  (cl-loop for key in gnosis-scheduler--event-keys
           for value in row append (list key value)))

(defun gnosis-scheduler--retained-result
    (row thema-id rating reviewed-at-us review-day)
  "Return ROW when THEMA-ID, RATING, REVIEWED-AT-US, and REVIEW-DAY match."
  (unless (and (= thema-id (nth 1 row)) (= rating (nth 5 row))
               (= reviewed-at-us (nth 3 row)) (= review-day (nth 4 row)))
    (error "Review event identity conflicts with retained evidence"))
  (gnosis-scheduler--event-result row))

(defun gnosis-scheduler--config-retention (db config-id)
  "Return supported desired retention for CONFIG-ID in DB."
  (or (caar
       (gnosis-sqlite-select
        db "SELECT desired_retention FROM scheduler_config
             WHERE id = ? AND algorithm = ? AND model = ?
               AND implementation = ? AND parameters = ?"
        (list config-id gnosis-fsrs--algorithm gnosis-fsrs--model
              gnosis-fsrs--implementation
              gnosis-fsrs-default-parameters)))
      (error "Unsupported scheduler config")))

(defun gnosis-scheduler--compute-result
    (event-id thema-id outcome reviewed-at-us review-day state retention)
  "Return EVENT-ID evidence for THEMA-ID from OUTCOME and STATE.
REVIEWED-AT-US, REVIEW-DAY, and RETENTION complete the evidence."
  (let* ((config-id (nth 0 state))
         (prior-stability (nth 1 state))
         (prior-difficulty (nth 2 state))
         (last-reviewed-at-us (nth 3 state))
         (last-review-day (nth 4 state))
         (reps (nth 6 state))
         (lapses (nth 7 state))
         (memory-p (and prior-stability prior-difficulty)))
    (unless (or memory-p (and (null prior-stability) (null prior-difficulty)))
      (error "Incomplete scheduler memory state"))
    (when (and memory-p
               (or (null last-reviewed-at-us) (null last-review-day)
                   (<= reviewed-at-us last-reviewed-at-us)))
      (error "Invalid scheduler review ordering"))
    (let* ((elapsed (if memory-p
                        (gnosis-scheduler--days-between last-review-day review-day)
                      0))
           (prior (and memory-p
                       (list :stability prior-stability
                             :difficulty prior-difficulty)))
           (transition (gnosis-fsrs-transition prior elapsed outcome retention))
           (rating (if (eq outcome 'failure) 1 3))
           (interval (plist-get transition :calendar-interval-days))
           (lapses-after (+ lapses (if (= rating 1) 1 0))))
      (list :event-id event-id :thema-id thema-id :config-id config-id
            :reviewed-at-us reviewed-at-us :review-day review-day
            :rating rating :elapsed-days elapsed
            :prior-stability prior-stability :prior-difficulty prior-difficulty
            :stability (plist-get transition :stability)
            :difficulty (plist-get transition :difficulty)
            :raw-interval-days (plist-get transition :raw-interval-days)
            :calendar-interval-days interval
            :due-day (gnosis-scheduler--add-days review-day interval)
            :reps-before reps :reps-after (1+ reps)
            :lapses-before lapses :lapses-after lapses-after
            :new-p (if (zerop reps) 1 0)))))

(defun gnosis-scheduler--insert-event (db result)
  "Insert immutable scheduler RESULT into DB."
  (gnosis-sqlite-execute
   db
   "INSERT INTO review_events VALUES
      (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
   (mapcar (lambda (key) (plist-get result key))
           gnosis-scheduler--event-keys)))

(defun gnosis-scheduler--update-state (db result)
  "Update DB current projection from accepted RESULT."
  (unless
      (= 1
         (gnosis-sqlite-execute
          db
          "UPDATE scheduler_state
              SET stability = ?, difficulty = ?, last_reviewed_at_us = ?,
                  last_review_day = ?, due_day = ?, reps = ?, lapses = ?, config_id = ?
            WHERE thema_id = ?"
          (list (plist-get result :stability) (plist-get result :difficulty)
                (plist-get result :reviewed-at-us) (plist-get result :review-day)
                (plist-get result :due-day) (plist-get result :reps-after)
                (plist-get result :lapses-after) (plist-get result :config-id)
                (plist-get result :thema-id))))
    (error "Scheduler state projection disappeared")))

(defun gnosis-scheduler--evidence-equal-p (expected actual)
  "Return non-nil when EXPECTED and ACTUAL event evidence agree."
  (cl-every
   (lambda (key)
     (let ((left (plist-get expected key)) (right (plist-get actual key)))
       (if (and (floatp left) (floatp right))
           (< (abs (- left right)) 1.0e-6)
         (equal left right))))
   gnosis-scheduler--event-keys))

(defun gnosis-scheduler--state-result (thema-id state)
  "Return THEMA-ID scheduler plist decoded from replay STATE."
  (list :thema-id thema-id :config-id (nth 0 state)
        :stability (nth 1 state) :difficulty (nth 2 state)
        :last-reviewed-at-us (nth 3 state) :last-review-day (nth 4 state)
        :due-day (nth 5 state) :reps (nth 6 state) :lapses (nth 7 state)
        :suspended (nth 8 state) :new-p (if (zerop (nth 6 state)) 1 0)))

(defun gnosis-scheduler-initialize-themata (rows &optional db)
  "Initialize scheduler storage for ROWS in optional DB.
Each row is (THEMA-ID DUE-DAY SUSPENDED)."
  (unless (and rows
               (cl-every
                (lambda (row)
                  (and (= (length row) 3) (integerp (nth 0 row))
                       (memq (nth 2 row) '(0 1))
                       (progn (gnosis-scheduler--day-time (nth 1 row)) t)))
                rows))
    (error "Invalid scheduler initialization"))
  (let ((db (or db (gnosis--ensure-db))))
    (gnosis-sqlite-with-transaction db
      (let ((batch-size
             (max 1 (/ (gnosis-sqlite--max-variable-number db) 10))))
        (dolist (chunk (seq-partition rows batch-size))
          (gnosis-sqlite-execute
           db (concat "INSERT INTO scheduler_baseline VALUES "
                      (mapconcat (lambda (_) "(?,?,?,?)") chunk ","))
           (apply #'append
                  (mapcar (lambda (row)
                            (list (nth 0 row) (nth 1 row) 0 0))
                          chunk)))
          (gnosis-sqlite-execute
           db (concat "INSERT INTO scheduler_state VALUES "
                      (mapconcat (lambda (_) "(?,?,?,?,?,?,?,?,?,?)")
                                 chunk ","))
           (apply #'append
                  (mapcar (lambda (row)
                            (list (nth 0 row) (gnosis-scheduler-active-config db)
                                  nil nil nil nil (nth 1 row)
                                  0 0 (nth 2 row)))
                          chunk))))))))

(defun gnosis-scheduler-initialize-thema
    (thema-id due-day suspended &optional db)
  "Initialize THEMA-ID at DUE-DAY with SUSPENDED state in optional DB."
  (gnosis-scheduler-initialize-themata
   (list (list thema-id due-day suspended)) db)
  (gnosis-scheduler--state-result
   thema-id (list (gnosis-scheduler-active-config db)
                  nil nil nil nil due-day 0 0 suspended)))

(defun gnosis-scheduler-replay (baseline events configs suspended)
  "Replay BASELINE and EVENTS using CONFIGS and current SUSPENDED fact."
  (unless (and (= (length baseline) 4) (memq suspended '(0 1))
               (cl-every #'integerp baseline)
               (>= (nth 2 baseline) 0) (>= (nth 3 baseline) 0))
    (error "Invalid scheduler replay baseline"))
  (gnosis-scheduler--day-time (nth 1 baseline))
  (unless (cdr (assoc 1 configs))
    (error "Replay config missing"))
  (let* ((thema-id (nth 0 baseline))
         (initial (list 1 nil nil nil nil (nth 1 baseline)
                        (nth 2 baseline) (nth 3 baseline) suspended))
         (final
          (cl-reduce
           (lambda (state row)
             (unless (and (= (length row) 19)
                          (= thema-id (nth 1 row))
                          (gnosis-scheduler--event-id-p (nth 0 row)))
               (error "Incomplete scheduler replay event"))
             (let* ((config-id (nth 2 row))
                    (retention (cdr (assoc config-id configs)))
                    (outcome (pcase (nth 5 row)
                               (1 'failure) (3 'success)
                               (_ (error "Invalid replay rating"))))
                    (input-state (cons config-id (cdr state)))
                    (expected (gnosis-scheduler--compute-result
                               (nth 0 row) thema-id outcome (nth 3 row)
                               (nth 4 row) input-state
                               (or retention (error "Replay config missing"))))
                    (actual (gnosis-scheduler--event-result row)))
               (unless (gnosis-scheduler--evidence-equal-p expected actual)
                 (error "Review event does not replay"))
               (list config-id (plist-get expected :stability)
                     (plist-get expected :difficulty) (nth 3 row) (nth 4 row)
                     (plist-get expected :due-day)
                     (plist-get expected :reps-after)
                     (plist-get expected :lapses-after) suspended)))
           events :initial-value initial)))
    (gnosis-scheduler--state-result thema-id final)))

(defun gnosis-scheduler-replay-thema (thema-id suspended &optional db)
  "Replay THEMA-ID using current SUSPENDED fact and optional DB."
  (let* ((db (or db (gnosis--ensure-db)))
         (baseline (car (gnosis-sqlite-select
                         db "SELECT * FROM scheduler_baseline WHERE thema_id = ?"
                         (list thema-id))))
         (events (gnosis-sqlite-select
                  db "SELECT * FROM review_events WHERE thema_id = ?
                       AND event_id NOT IN (SELECT event_id FROM review_voids)
                       ORDER BY reviewed_at_us, event_id" (list thema-id)))
         (configs (gnosis-sqlite-select
                   db "SELECT id, desired_retention FROM scheduler_config
                        WHERE algorithm = ? AND model = ?
                          AND implementation = ? AND parameters = ?"
                   (list gnosis-fsrs--algorithm gnosis-fsrs--model
                         gnosis-fsrs--implementation
                         gnosis-fsrs-default-parameters))))
    (unless baseline (error "Scheduler baseline does not exist"))
    (gnosis-scheduler-replay baseline events
                             (mapcar (lambda (row) (cons (car row) (cadr row)))
                                     configs)
                             suspended)))

(defun gnosis-scheduler-rebuild-state (thema-id suspended &optional db)
  "Rebuild THEMA-ID projection with current SUSPENDED fact in optional DB."
  (let ((db (or db (gnosis--ensure-db))))
    (gnosis-sqlite-with-transaction db
      (let ((result (gnosis-scheduler-replay-thema thema-id suspended db)))
        (gnosis-sqlite-execute
         db "INSERT OR REPLACE INTO scheduler_state VALUES
              (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
         (list thema-id (plist-get result :config-id)
               (plist-get result :stability) (plist-get result :difficulty)
               (plist-get result :last-reviewed-at-us)
               (plist-get result :last-review-day) (plist-get result :due-day)
               (plist-get result :reps) (plist-get result :lapses) suspended))
        result))))

(defun gnosis-scheduler--validate-review
    (event-id thema-id outcome reviewed-at-us review-day)
  "Validate EVENT-ID, THEMA-ID, OUTCOME, REVIEWED-AT-US, and REVIEW-DAY."
  (unless (and (gnosis-scheduler--event-id-p event-id)
               (integerp thema-id) (memq outcome '(failure success))
               (integerp reviewed-at-us) (>= reviewed-at-us 0))
    (error "Invalid scheduler review input"))
  (gnosis-scheduler--day-time review-day))

(defun gnosis-scheduler--fresh-result
    (db event-id thema-id outcome reviewed-at-us review-day)
  "Compute fresh EVENT-ID evidence for THEMA-ID from DB.
OUTCOME, REVIEWED-AT-US, and REVIEW-DAY complete the review facts."
  (let ((state
         (car (gnosis-sqlite-select
               db "SELECT config_id, stability, difficulty,
                          last_reviewed_at_us, last_review_day, due_day,
                          reps, lapses, suspended
                     FROM scheduler_state WHERE thema_id = ?"
               (list thema-id)))))
    (unless state (error "Scheduler state does not exist"))
    (gnosis-scheduler--config-retention db (car state))
    (let ((latest (caar (gnosis-sqlite-select
                         db "SELECT MAX(reviewed_at_us) FROM review_events WHERE thema_id = ?"
                         (list thema-id))))
          (config (gnosis-scheduler-active-config db)))
      (when (and latest (<= reviewed-at-us latest))
        (error "Review precedes retained history, including voids"))
      (gnosis-scheduler--compute-result
       event-id thema-id outcome reviewed-at-us review-day (cons config (cdr state))
       (gnosis-scheduler--config-retention db config)))))

(defun gnosis-scheduler-preview-review
    (event-id thema-id outcome reviewed-at-us review-day)
  "Preview EVENT-ID evidence for THEMA-ID without mutation.
OUTCOME, REVIEWED-AT-US, and REVIEW-DAY complete the review facts."
  (gnosis-scheduler--validate-review
   event-id thema-id outcome reviewed-at-us review-day)
  (gnosis-scheduler--fresh-result
   (gnosis--ensure-db) event-id thema-id outcome reviewed-at-us review-day))

(defun gnosis-scheduler-accept-review
    (event-id thema-id outcome reviewed-at-us review-day
              &optional config preview)
  "Accept EVENT-ID for THEMA-ID, OUTCOME, REVIEWED-AT-US, and REVIEW-DAY.
Return event evidence with `:inserted-p' reporting this call's effect.
When CONFIG is non-nil, reject a fresh acceptance if the active
configuration no longer matches the preview.  PREVIEW also pins
the scheduling evidence shown before acceptance.
Retained retries are unchanged."
  (gnosis-scheduler--validate-review
   event-id thema-id outcome reviewed-at-us review-day)
  (let ((db (gnosis--ensure-db))
        (rating (if (eq outcome 'failure) 1 3)))
    (gnosis-sqlite-with-transaction db
      (let ((existing
             (car (gnosis-sqlite-select
                   db "SELECT * FROM review_events WHERE event_id = ?"
                   (list event-id)))))
        (when (gnosis-get 'event-id 'review-voids `(= event-id ,event-id))
          (error "Review event was voided"))
        (when (and (not existing) config
                   (not (= config (gnosis-scheduler-active-config db))))
          (user-error "Retention changed; cancel and reveal this question again"))
        (if existing
            (append
             (gnosis-scheduler--retained-result
              existing thema-id rating reviewed-at-us review-day)
             '(:inserted-p nil))
          (let ((result (gnosis-scheduler--fresh-result
                         db event-id thema-id outcome reviewed-at-us review-day)))
            (when (and preview
                       (not (equal
                             (mapcar (lambda (key) (plist-get preview key))
                                     gnosis-scheduler--event-keys)
                             (mapcar (lambda (key) (plist-get result key))
                                     gnosis-scheduler--event-keys))))
              (user-error "Schedule changed; cancel and reveal this question again"))
            (gnosis-scheduler--insert-event db result)
            (gnosis-scheduler--update-state db result)
            (append result '(:inserted-p t))))))))

(defun gnosis-scheduler-active-config (&optional db)
  "Return the active immutable configuration identity from DB."
  (or (caar (gnosis-sqlite-select (or db (gnosis--ensure-db))
                                 "SELECT config_id FROM scheduler_active WHERE id = 1"))
      (error "Active scheduler configuration missing")))

(defun gnosis-scheduler--validate-retention (retention day)
  "Validate RETENTION for the scheduler calendar in DAY's year.
DAY is a YYYYMMDD integer.  Check the interval at the model's maximum
stability, not just the first review.  Use December 31 so the calendar
conversion includes the largest month and day offsets.  Reject numeric
or calendar overflow rather than silently capping mathematical intervals."
  (unless (and (gnosis-fsrs--finite-number-p retention) (< 0 retention 1))
    (user-error "Desired retention must be a finite number between 0 and 1"))
  (condition-case err
      (let* ((transition
              (gnosis-fsrs-transition
               (list :stability gnosis-fsrs--maximum-stability :difficulty 1.0)
               0 'success retention))
             (interval (plist-get transition :calendar-interval-days))
             (year-end (gnosis--date-to-int (list (/ day 10000) 12 31)))
             (due-day (gnosis-scheduler--add-days year-end interval)))
        (unless (= interval (gnosis-scheduler--days-between year-end due-day))
          (error "Scheduler interval does not round-trip through the calendar")))
    (error (user-error "Unsupported desired retention: %s"
                       (error-message-string err)))))

;;;###autoload
(defun gnosis-scheduler-set-retention (retention)
  "Select user-wide desired RETENTION for future accepted reviews.
Append an immutable configuration snapshot.  Existing due dates and events
are unchanged.  This is a workload preference, not measured topic mastery.
RETENTION must be finite and strictly between 0 and 1.  Before writing,
require its longest model interval to fit the current year's scheduler
calendar.  Unsupported values signal `user-error'; intervals are not capped."
  (interactive "nDesired retention (strictly between 0 and 1): ")
  (gnosis-scheduler--validate-retention retention (gnosis--today-int))
  (let ((db (gnosis--ensure-db)))
    (gnosis-sqlite-with-transaction db
      (let* ((active (gnosis-scheduler-active-config db))
             (old (gnosis-scheduler--config-retention db active)))
        (if (= old retention) active
          (let ((id (1+ (caar (gnosis-sqlite-select db "SELECT MAX(id) FROM scheduler_config")))))
            (gnosis-sqlite-execute
             db "INSERT INTO scheduler_config VALUES (?, ?, ?, ?, ?, ?)"
             (list id gnosis-fsrs--algorithm gnosis-fsrs--model gnosis-fsrs--implementation
                   retention gnosis-fsrs-default-parameters))
            (gnosis-sqlite-execute db "UPDATE scheduler_active SET config_id = ? WHERE id = 1"
                                   (list id))
            (message "Desired retention applies to future reviews; current due dates unchanged")
            id))))))

(defun gnosis-scheduler-void-review (correction-id event-id)
  "Void latest effective EVENT-ID using idempotent CORRECTION-ID.
Keep original evidence and current suspension.  Reject superseded targets;
rebuild only from the baseline and effective, non-void review events."
  (unless (and (gnosis-scheduler--event-id-p correction-id)
               (gnosis-scheduler--event-id-p event-id))
    (error "Invalid correction identity"))
  (let ((db (gnosis--ensure-db)))
    (gnosis-sqlite-with-transaction db
      (let* ((retained (gnosis-get 'event-id 'review-voids `(= correction-id ,correction-id)))
             (event (car (gnosis-select '* 'review-events `(= event-id ,event-id))))
             (id (nth 1 event)))
        (cond
         (retained (unless (equal retained event-id) (error "Correction identity conflict")))
         ((null event) (user-error "Review no longer exists"))
         (t
          (unless (equal event-id
                         (caar (gnosis-sqlite-select
                                db "SELECT event_id FROM review_events WHERE thema_id = ?
                                     AND event_id NOT IN (SELECT event_id FROM review_voids)
                                     ORDER BY reviewed_at_us DESC, event_id DESC LIMIT 1" (list id))))
            (user-error "Only the latest effective review can be undone"))
          (gnosis-sqlite-execute db "INSERT INTO review_voids VALUES (?, ?)" (list correction-id event-id))
          (gnosis-scheduler-rebuild-state id (gnosis-get 'suspended 'scheduler-state `(= thema-id ,id)) db)))
        event-id))))

(provide 'gnosis-scheduler)
;;; gnosis-scheduler.el ends here

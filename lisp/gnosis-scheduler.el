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

(require 'gnosis-db)
(require 'gnosis-fsrs)

(defconst gnosis-scheduler--event-keys
  '(:event-id :thema-id :config-id :reviewed-at-us :review-day :rating
    :elapsed-days :prior-stability :prior-difficulty :stability :difficulty
    :raw-interval-days :calendar-interval-days :due-day :reps-before
    :reps-after :lapses-before :lapses-after :new-p)
  "Ordered plist keys matching the review-events schema.")

(defun gnosis-scheduler-event-id ()
  "Return a new opaque review-event identity."
  (secure-hash 'sha256 (prin1-to-string
                (list (current-time) (emacs-pid) (random) (user-uid)))))

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
                  last_review_day = ?, due_day = ?, reps = ?, lapses = ?
            WHERE thema_id = ?"
          (list (plist-get result :stability) (plist-get result :difficulty)
                (plist-get result :reviewed-at-us) (plist-get result :review-day)
                (plist-get result :due-day) (plist-get result :reps-after)
                (plist-get result :lapses-after) (plist-get result :thema-id))))
    (error "Scheduler state projection disappeared")))

(defun gnosis-scheduler-accept-review
    (event-id thema-id outcome reviewed-at-us review-day)
  "Accept EVENT-ID for THEMA-ID, OUTCOME, REVIEWED-AT-US, and REVIEW-DAY."
  (unless (and (stringp event-id)
               (let ((case-fold-search nil))
                 (string-match-p "\\`[0-9a-f]\\{64\\}\\'" event-id))
               (integerp thema-id) (memq outcome '(failure success))
               (integerp reviewed-at-us) (>= reviewed-at-us 0))
    (error "Invalid scheduler review input"))
  (gnosis-scheduler--day-time review-day)
  (let ((db (gnosis--ensure-db))
        (rating (if (eq outcome 'failure) 1 3)))
    (gnosis-sqlite-with-transaction db
      (let ((existing
             (car (gnosis-sqlite-select
                   db "SELECT * FROM review_events WHERE event_id = ?"
                   (list event-id)))))
        (if existing
            (gnosis-scheduler--retained-result
             existing thema-id rating reviewed-at-us review-day)
          (let* ((state
                  (car (gnosis-sqlite-select
                        db
                        "SELECT config_id, stability, difficulty,
                                last_reviewed_at_us, last_review_day, due_day,
                                reps, lapses, suspended
                           FROM scheduler_state WHERE thema_id = ?"
                        (list thema-id))))
                 (_ (unless state (error "Scheduler state does not exist")))
                 (retention
                  (caar
                   (gnosis-sqlite-select
                    db "SELECT desired_retention FROM scheduler_config
                         WHERE id = ? AND algorithm = ? AND model = ?
                           AND implementation = ? AND parameters = ?"
                    (list (nth 0 state) "fsrs" "gnosis-fsrs6-v1"
                          "fsrs-rs-6.6.1" gnosis-fsrs-default-parameters))))
                 (_ (unless retention (error "Unsupported scheduler config")))
                 (result (gnosis-scheduler--compute-result
                          event-id thema-id outcome reviewed-at-us review-day
                          state retention)))
            (gnosis-scheduler--insert-event db result)
            (gnosis-scheduler--update-state db result)
            result))))))

(provide 'gnosis-scheduler)
;;; gnosis-scheduler.el ends here

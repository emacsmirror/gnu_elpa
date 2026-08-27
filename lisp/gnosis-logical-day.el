;;; gnosis-logical-day.el --- Logical review-day calculations  -*- lexical-binding: t; -*-

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

;; Scheduler-neutral logical review-day calculations for Gnosis.

;;; Code:

(require 'time-date)

(defvar gnosis--legacy-day-start-hour
  (if (and (boundp 'gnosis-algorithm-day-start-hour)
           (eq (indirect-variable 'gnosis-algorithm-day-start-hour)
               'gnosis-algorithm-day-start-hour))
      (symbol-value 'gnosis-algorithm-day-start-hour)
    :unset)
  "Pre-load value of the obsolete logical-day boundary option.")

(when (and (boundp 'gnosis-algorithm-day-start-hour)
           (eq (indirect-variable 'gnosis-algorithm-day-start-hour)
               'gnosis-algorithm-day-start-hour))
  (makunbound 'gnosis-algorithm-day-start-hour))
(define-obsolete-variable-alias
  'gnosis-algorithm-day-start-hour 'gnosis-day-start-hour "0.11.0")

(defcustom gnosis-day-start-hour
  (if (eq gnosis--legacy-day-start-hour :unset)
      3
    gnosis--legacy-day-start-hour)
  "Hour at which a new logical review day begins.
The value must be an integer from 0 through 23.  Reviews before this
hour belong to the previous calendar day."
  :group 'gnosis
  :type 'integer)

(makunbound 'gnosis--legacy-day-start-hour)

(defun gnosis-date (&optional offset time)
  "Return the logical review date as (YEAR MONTH DAY).
OFFSET is an optional integer number of logical days.  TIME defaults to
`current-time'."
  (unless (or (null offset) (integerp offset))
    (user-error "Date offset must be an integer or nil"))
  (unless (and (integerp gnosis-day-start-hour)
               (<= 0 gnosis-day-start-hour 23))
    (user-error "Day start hour must be an integer from 0 through 23"))
  (let* ((decoded (decode-time (or time (current-time))))
         (logical-day
          (if (< (decoded-time-hour decoded) gnosis-day-start-hour)
              (decoded-time-add decoded (make-decoded-time :day -1))
            decoded))
         (target (if (and offset (not (zerop offset)))
                     (decoded-time-add
                      logical-day (make-decoded-time :day offset))
                   logical-day)))
    (list (decoded-time-year target)
          (decoded-time-month target)
          (decoded-time-day target))))

(defun gnosis-date-diff (date &optional date2)
  "Return the number of days from DATE through DATE2.
DATE and DATE2 are (YEAR MONTH DAY) lists.  DATE2 defaults to the current
logical review date.  Signal an error when DATE2 precedes DATE."
  (let* ((end (or date2 (gnosis-date)))
         (start-time (encode-time 0 0 0
                                  (nth 2 date) (nth 1 date) (nth 0 date)))
         (end-time (encode-time 0 0 0
                                (nth 2 end) (nth 1 end) (nth 0 end)))
         (difference (- (time-to-days end-time)
                        (time-to-days start-time))))
    (if (>= difference 0)
        difference
      (error "DATE2 must not precede DATE"))))

(defun gnosis-date-later-p (date1 date2)
  "Return non-nil when DATE1 is later than DATE2.
Both dates are (YEAR MONTH DAY) lists."
  (time-less-p
   (encode-time 0 0 0 (nth 2 date2) (nth 1 date2) (nth 0 date2))
   (encode-time 0 0 0 (nth 2 date1) (nth 1 date1) (nth 0 date1))))

(provide 'gnosis-logical-day)
;;; gnosis-logical-day.el ends here

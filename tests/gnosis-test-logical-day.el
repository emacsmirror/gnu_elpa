;;; gnosis-test-logical-day.el --- Logical review day tests  -*- lexical-binding: t; -*-

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

;; Verify scheduler-neutral logical review-day calculations.

;;; Code:

(require 'ert)
(require 'gnosis-logical-day nil t)

(defun gnosis-test-logical-day--time (hour minute)
  "Return a fixed local time on 2025-06-15 at HOUR and MINUTE."
  (encode-time 0 minute hour 15 6 2025))

(defun gnosis-test-logical-day--in-zone (zone function)
  "Call FUNCTION while using time-zone rule ZONE."
  (let ((original (getenv "TZ")))
    (unwind-protect
        (progn
          (setenv "TZ" zone)
          (funcall function))
      (setenv "TZ" original))))

(ert-deftest gnosis-test-logical-day-before-boundary ()
  "Treat 02:59 as the previous logical day with a 03:00 boundary."
  (let ((gnosis-day-start-hour 3))
    (should (equal '(2025 6 14)
                   (gnosis-date
                    nil (gnosis-test-logical-day--time 2 59))))))

(ert-deftest gnosis-test-logical-day-at-boundary ()
  "Treat 03:00 as the current logical day with a 03:00 boundary."
  (let ((gnosis-day-start-hour 3))
    (should (equal '(2025 6 15)
                   (gnosis-date
                    nil (gnosis-test-logical-day--time 3 0))))))

(ert-deftest gnosis-test-logical-day-spring-forward-boundary ()
  "Treat spring-forward 03:00 as the current logical day."
  (gnosis-test-logical-day--in-zone
   "America/New_York"
   (lambda ()
     (let ((gnosis-day-start-hour 3))
       (should (equal '(2025 3 9)
                      (gnosis-date nil (encode-time 0 0 3 9 3 2025))))))))

(ert-deftest gnosis-test-logical-day-fall-back-before-boundary ()
  "Treat fall-back 02:30 as the previous logical day."
  (gnosis-test-logical-day--in-zone
   "America/New_York"
   (lambda ()
     (let ((gnosis-day-start-hour 3))
       (should (equal '(2025 11 1)
                      (gnosis-date nil (encode-time 0 30 2 2 11 2025))))))))

(ert-deftest gnosis-test-logical-day-offset-after-boundary-shift ()
  "Apply OFFSET after resolving the logical review day."
  (let ((gnosis-day-start-hour 3))
    (should (equal '(2025 6 15)
                   (gnosis-date
                    1 (gnosis-test-logical-day--time 2 59))))))

(ert-deftest gnosis-test-logical-day-midnight-boundary ()
  "Treat the calendar date as the logical day at a midnight boundary."
  (let ((gnosis-day-start-hour 0))
    (should (equal '(2025 6 15)
                   (gnosis-date
                    nil (gnosis-test-logical-day--time 0 15))))))

(ert-deftest gnosis-test-logical-day-date-diff ()
  "Return non-negative differences across month and year boundaries."
  (should (= 1 (gnosis-date-diff '(2024 12 31) '(2025 1 1))))
  (should (= 30 (gnosis-date-diff '(2025 1 1) '(2025 1 31))))
  (should-error (gnosis-date-diff '(2025 1 2) '(2025 1 1))))

(ert-deftest gnosis-test-logical-day-later-p ()
  "Compare logical-day date lists chronologically."
  (should (gnosis-date-later-p '(2026 1 1) '(2025 12 31)))
  (should-not (gnosis-date-later-p '(2025 6 10) '(2025 6 10)))
  (should-not (gnosis-date-later-p '(2025 6 10) '(2025 6 11))))

(ert-deftest gnosis-test-logical-day-rejects-invalid-input ()
  "Reject invalid offsets and review-day boundaries."
  (should (fboundp 'gnosis-date))
  (should-error (gnosis-date 1.5))
  (should-error (let ((gnosis-day-start-hour -1)) (gnosis-date)))
  (should-error (let ((gnosis-day-start-hour 24)) (gnosis-date))))

(provide 'gnosis-test-logical-day)

;;; gnosis-test-logical-day.el ends here

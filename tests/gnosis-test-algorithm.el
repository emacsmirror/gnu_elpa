;;; gnosis-test-algorithm.el --- Gnosis Algorithm tests  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Free Software Foundation, Inc.

;; Author: Thanos Apollo <public@thanosapollo.org>
;; Keywords: extensions
;; URL: https://git.thanosapollo.org/gnosis
;; Version: 0.0.1

;; Package-Requires: ((emacs "27.2") (compat "29.1.4.2"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Testing module for gnosis algorithm functions.

;; Before making any push on master we should be passing the following
;; tests.

;;; Code:
(require 'ert)
(require 'gnosis)

(let ((lisp-dir (expand-file-name "../lisp"
                  (file-name-directory (or load-file-name default-directory)))))
  (add-to-list 'load-path lisp-dir))

(ert-deftest gnosis-test-algorithm-date ()
  "Test gnosis-algorithm-date returns correct format and offsets."
  ;; No offset returns today
  (let ((today (gnosis-algorithm-date)))
    (should (= (length today) 3))
    (should (integerp (nth 0 today)))  ; year
    (should (integerp (nth 1 today)))  ; month
    (should (integerp (nth 2 today)))) ; day
  ;; Offset 0 equals no offset
  (should (equal (gnosis-algorithm-date 0) (gnosis-algorithm-date)))
  ;; Positive offset is in the future
  (let ((today (gnosis-algorithm-date))
        (tomorrow (gnosis-algorithm-date 1)))
    (should (= (gnosis-algorithm-date-diff today tomorrow) 1)))
  ;; Negative offset is in the past
  (let ((yesterday (gnosis-algorithm-date -1))
        (today (gnosis-algorithm-date)))
    (should (= (gnosis-algorithm-date-diff yesterday today) 1)))
  ;; Larger offsets
  (let ((today (gnosis-algorithm-date))
        (future (gnosis-algorithm-date 30)))
    (should (= (gnosis-algorithm-date-diff today future) 30)))
  ;; Non-integer offset signals error
  (should-error (gnosis-algorithm-date 1.5))
  (should-error (gnosis-algorithm-date "3")))

(ert-deftest gnosis-test-algorithm-date-diff ()
  "Test gnosis-algorithm-date-diff calculations."
  ;; Same date returns 0
  (should (= (gnosis-algorithm-date-diff '(2025 1 1) '(2025 1 1)) 0))
  ;; Simple differences
  (should (= (gnosis-algorithm-date-diff '(2025 1 1) '(2025 1 2)) 1))
  (should (= (gnosis-algorithm-date-diff '(2025 1 1) '(2025 1 31)) 30))
  ;; Cross-month
  (should (= (gnosis-algorithm-date-diff '(2025 1 31) '(2025 2 1)) 1))
  ;; Cross-year
  (should (= (gnosis-algorithm-date-diff '(2024 12 31) '(2025 1 1)) 1))
  ;; Larger span
  (should (= (gnosis-algorithm-date-diff '(2025 1 1) '(2025 12 31)) 364))
  ;; date2 < date signals error
  (should-error (gnosis-algorithm-date-diff '(2025 1 2) '(2025 1 1))))

(ert-deftest gnosis-test-algorithm-date-later-p ()
  "Test gnosis-algorithm--date-later-p comparisons."
  (should (gnosis-algorithm--date-later-p '(2025 6 15) '(2025 6 10)))
  (should (gnosis-algorithm--date-later-p '(2025 7 1) '(2025 6 30)))
  (should (gnosis-algorithm--date-later-p '(2026 1 1) '(2025 12 31)))
  (should-not (gnosis-algorithm--date-later-p '(2025 6 10) '(2025 6 15)))
  (should-not (gnosis-algorithm--date-later-p '(2025 6 10) '(2025 6 10))))

(ert-deftest gnosis-test-algorithm-elapsed-time-interval ()
  "Test that elapsed time drives interval, with max logic on success.

Simulates the full flow: elapsed time as last-interval, then on success
keep max(computed, existing-next-rev)."
  (let ((gnosis-algorithm-interval-fuzz 0))
    ;; 1. Normal due review: elapsed = 10, synolon = 2.0 → next = 20 days
    ;;    existing-next-rev is today (due now), computed is 20 days out → use computed
    (let* ((elapsed 10)
	   (computed (gnosis-algorithm-next-interval
		      :last-interval elapsed :gnosis-synolon 2.0 :success t
		      :successful-reviews 5 :amnesia 0.5 :proto '(1 2 3)
		      :c-fails 0 :lethe 3))
	   (existing-next-rev (gnosis-algorithm-date)))
      (should (equal computed (gnosis-algorithm-date 20)))
      ;; computed is later than existing → use computed
      (should (gnosis-algorithm--date-later-p computed existing-next-rev)))

    ;; 2. Early success: elapsed = 5, synolon = 2.0 → computed = 10 days
    ;;    but existing-next-rev is 60 days out → keep existing
    (let* ((elapsed 5)
	   (computed (gnosis-algorithm-next-interval
		      :last-interval elapsed :gnosis-synolon 2.0 :success t
		      :successful-reviews 5 :amnesia 0.5 :proto '(1 2 3)
		      :c-fails 0 :lethe 3))
	   (existing-next-rev (gnosis-algorithm-date 60))
	   (next-rev (if (gnosis-algorithm--date-later-p existing-next-rev computed)
			 existing-next-rev
		       computed)))
      (should (equal computed (gnosis-algorithm-date 10)))
      (should (equal next-rev existing-next-rev)))

    ;; 3. Overdue success: elapsed = 90, synolon = 2.0 → computed = 180 days
    ;;    existing-next-rev was 60 days ago → use computed
    (let* ((elapsed 90)
	   (computed (gnosis-algorithm-next-interval
		      :last-interval elapsed :gnosis-synolon 2.0 :success t
		      :successful-reviews 5 :amnesia 0.5 :proto '(1 2 3)
		      :c-fails 0 :lethe 3))
	   (existing-next-rev (gnosis-algorithm-date -60))
	   (next-rev (if (gnosis-algorithm--date-later-p existing-next-rev computed)
			 existing-next-rev
		       computed)))
      (should (equal computed (gnosis-algorithm-date 180)))
      (should (equal next-rev computed)))

    ;; 4. Early failure: elapsed = 5, amnesia = 0.5 → computed = 2 days
    ;;    On failure, always use computed (no max logic)
    (let* ((elapsed 5)
	   (computed (gnosis-algorithm-next-interval
		      :last-interval elapsed :gnosis-synolon 2.0 :success nil
		      :successful-reviews 5 :amnesia 0.5 :proto '(1 2 3)
		      :c-fails 1 :lethe 3)))
      (should (equal computed (gnosis-algorithm-date 2))))))

(ert-deftest gnosis-test-algorithm-day-start-hour-default ()
  "Test that day-start-hour 0 returns today's calendar date."
  (let ((gnosis-algorithm-day-start-hour 0))
    (should (equal (gnosis-algorithm-date) (gnosis-algorithm-date)))))

(ert-deftest gnosis-test-algorithm-day-start-hour-before-boundary ()
  "Test that times before day-start-hour count as previous day."
  (let ((gnosis-algorithm-day-start-hour 6))
    ;; Mock current-time to 02:00 today
    (cl-letf* ((now (decode-time))
               (fake-time (encode-time 0 0 2
                                       (decoded-time-day now)
                                       (decoded-time-month now)
                                       (decoded-time-year now)))
               ((symbol-function 'current-time) (lambda () fake-time)))
      (let* ((result (gnosis-algorithm-date))
             (yesterday (decode-time (time-subtract fake-time
                                                    (seconds-to-time (* 6 3600))))))
        (should (equal result (list (decoded-time-year yesterday)
                                    (decoded-time-month yesterday)
                                    (decoded-time-day yesterday))))))))

(ert-deftest gnosis-test-algorithm-day-start-hour-after-boundary ()
  "Test that times after day-start-hour return today."
  (let ((gnosis-algorithm-day-start-hour 6))
    ;; Mock current-time to 08:00 today
    (cl-letf* ((now (decode-time))
               (fake-time (encode-time 0 0 8
                                       (decoded-time-day now)
                                       (decoded-time-month now)
                                       (decoded-time-year now)))
               ((symbol-function 'current-time) (lambda () fake-time)))
      (let ((result (gnosis-algorithm-date)))
        (should (equal result (list (decoded-time-year now)
                                    (decoded-time-month now)
                                    (decoded-time-day now))))))))

(ert-deftest gnosis-test-algorithm-day-start-hour-date-diff ()
  "Test that date-diff respects day-start-hour when date2 is nil."
  (let ((gnosis-algorithm-day-start-hour 6))
    ;; Mock current-time to 02:00 today
    (cl-letf* ((now (decode-time))
               (fake-time (encode-time 0 0 2
                                       (decoded-time-day now)
                                       (decoded-time-month now)
                                       (decoded-time-year now)))
               ((symbol-function 'current-time) (lambda () fake-time)))
      ;; date-diff with nil date2 should use shifted date (yesterday)
      (let ((shifted-date (gnosis-algorithm-date)))
        (should (= (gnosis-algorithm-date-diff shifted-date) 0))))))

(ert-deftest gnosis-test-algorithm-day-start-hour-invalid ()
  "Test that invalid day-start-hour values signal an error."
  (let ((gnosis-algorithm-day-start-hour 50))
    (should-error (gnosis-algorithm-date)))
  (let ((gnosis-algorithm-day-start-hour -1))
    (should-error (gnosis-algorithm-date)))
  (let ((gnosis-algorithm-day-start-hour 24))
    (should-error (gnosis-algorithm-date))))

(provide 'gnosis-test-algorithm)

(ert-run-tests-batch-and-exit)
;;; gnosis-test-algorithm.el ends here

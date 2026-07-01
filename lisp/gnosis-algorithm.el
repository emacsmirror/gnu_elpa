;;; gnosis-algorithm.el --- Spaced repetition  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2026  Free Software Foundation, Inc.

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

;; Module that handles date and interval calculation as well as
;; gnosis-score for gnosis.

;;; Code:

(require 'cl-lib)
(require 'calendar)
(require 'time-date)

(defcustom gnosis-algorithm-proto '(0 1 2)
  "Gnosis proto interval for the first successful reviews.

Values for the first proto successful intervals.  There is no
restriction for list length."
  :group 'gnosis
  :type '(list integer))

(defcustom gnosis-algorithm-gnosis-value '(0.35 0.30 1.3)
  "Starting gnosis score.

First item : Increase value (gnosis-plus)
Second item: Decrease value (gnosis-minus)
Third item : Total gnosis (gnosis-synolon/totalis) -> Total gnosis score"
  :group 'gnosis
  :type '(list float))

(defcustom gnosis-algorithm-amnesia-value 0.5
  "Gnosis amnesia value.

Used to calculate new interval upon a failed recall i.e the memory loss.

The closer this value is to 1, the more the memory loss."
  :group 'gnosis
  :type 'float)

(defcustom gnosis-algorithm-epignosis-value 0.1
  "Value to increase gnosis-plus upon anagnosis.

Epignosis means knowledge accuracy."
  :group 'gnosis
  :type 'float)

(defcustom gnosis-algorithm-agnoia-value 0.2
  "Value to increase gnosis-minus upon anagnosis.

Agnoia refers to the lack of knowledge."
  :group 'gnosis
  :type 'float)

(defcustom gnosis-algorithm-anagnosis-value 3
  "Threshold value for anagnosis event.

Anagosis is the process recognition & understanding of a context/gnosis.

Anagnosis events update gnosis-plus & gnosis-minus values, depending
on the success or failure of recall."
  :group 'gnosis
  :type 'integer)

(defcustom gnosis-algorithm-lethe-value 2
  "Threshold value for hitting a lethe event.

Lethe is the process of being unable to recall a memory/gnosis.

On lethe events the next interval is set to 0."
  :group 'gnosis
  :type 'integer)

(defcustom gnosis-algorithm-synolon-max 3.0
  "Maximum value for gnosis-synolon.

Caps the interval multiplier to prevent excessively long intervals
for mature cards."
  :group 'gnosis
  :type 'float)

(defcustom gnosis-algorithm-interval-fuzz 0.1
  "Fuzz factor for interval calculation.

Adds random variation to prevent review clustering.
A value of 0.1 means +/- 10%%.  Set to 0 to disable."
  :group 'gnosis
  :type 'float)

(defcustom gnosis-algorithm-day-start-hour 3
  "Hour at which a new review day begins (0-23).
When set to 0, days start at midnight (default).
When set to e.g. 6, reviews done between 00:00 and 05:59
count as the previous calendar day."
  :group 'gnosis
  :type 'integer)

(defun gnosis-algorithm-round-items (list)
  "Round all items in LIST to 2 decimal places."
  (cl-loop for item in list
	   collect (/ (round (* item 100)) 100.0)))

(defun gnosis-algorithm-fuzz-interval (interval)
  "Apply random fuzz to INTERVAL based on `gnosis-algorithm-interval-fuzz'.

Returns INTERVAL with +/- fuzz variation.  Returns unmodified for
intervals less than 2."
  (if (or (<= interval 2) (zerop gnosis-algorithm-interval-fuzz))
      interval
    (let ((fuzz (- (* 2 gnosis-algorithm-interval-fuzz
		      (/ (random 1001) 1000.0))
		   gnosis-algorithm-interval-fuzz)))
      (* interval (1+ fuzz)))))

(defun gnosis-algorithm-date (&optional offset)
  "Return the current date in a list (year month day).
Optional integer OFFSET is a number of days from the current date.

Respects `gnosis-algorithm-day-start-hour': when set to e.g. 6,
times before 06:00 count as the previous calendar day."
  (cl-assert (or (integerp offset) (null offset)) nil
             "Date offset must be an integer or nil")
  (cl-assert (<= 0 gnosis-algorithm-day-start-hour 23) nil
             "day-start-hour must be 0-23, got %d"
             gnosis-algorithm-day-start-hour)
  (let* ((shifted (time-subtract (current-time)
                                 (seconds-to-time
                                  (* gnosis-algorithm-day-start-hour 3600))))
         (decoded-time (decode-time shifted))
         (target (if (and offset (not (zerop offset)))
                     (decoded-time-add
                      decoded-time
                      (make-decoded-time :day offset))
                   decoded-time)))
    (list (decoded-time-year target)
          (decoded-time-month target)
          (decoded-time-day target))))

(defun gnosis-algorithm-date-diff (date &optional date2)
  "Find the difference between DATE2 and DATE.

If DATE2 is nil, current date will be used instead.

DATE format must be given as (year month day)."
  (let* ((given-date (encode-time 0 0 0 (caddr date) (cadr date) (car date)))
	 (date2 (if date2
		    (encode-time 0 0 0 (caddr date2) (cadr date2) (car date2))
		  (let ((today (gnosis-algorithm-date)))
		    (encode-time 0 0 0 (caddr today) (cadr today) (car today)))))
	 (diff (- (time-to-days date2)
		  (time-to-days given-date))))
    (if (>= diff 0) diff (error "`DATE2' must be higher than `DATE'"))))

(defun gnosis-algorithm--anagnosis-trigger-p (success c-successes c-failures anagnosis)
  "Return non-nil when SUCCESS reaches ANAGNOSIS.
C-SUCCESSES and C-FAILURES select the current streak count."
  (= (% (max 1 (if success c-successes c-failures)) anagnosis) 0))

(defun gnosis-algorithm--next-synolon (success g-synolon g-plus g-minus)
  "Return the next G-SYNOLON using SUCCESS, G-PLUS, and G-MINUS."
  (if success
      (min (+ g-synolon g-plus) gnosis-algorithm-synolon-max)
    (max 1.3 (- g-synolon g-minus))))

(defun gnosis-algorithm--next-plus (g-plus success anagnosis-p epignosis c-failures lethe)
  "Return the next G-PLUS.
SUCCESS, ANAGNOSIS-P, EPIGNOSIS, C-FAILURES, and LETHE determine rule effects."
  (let ((neo-plus (if (and success anagnosis-p)
                      (+ g-plus epignosis)
                    g-plus)))
    (if (and lethe (not success) (>= c-failures lethe))
        (max 0.1 (- neo-plus epignosis))
      neo-plus)))

(defun gnosis-algorithm--next-minus (g-minus success anagnosis-p agnoia)
  "Return the next G-MINUS from SUCCESS, ANAGNOSIS-P, and AGNOIA."
  (if (and (not success) anagnosis-p)
      (+ g-minus agnoia)
    g-minus))

(cl-defun gnosis-algorithm-next-gnosis
    (&key gnosis success epignosis agnoia anagnosis
          c-successes c-failures lethe)
  "Return the neo GNOSIS value. (gnosis-plus gnosis-minus gnosis-synolon)

Calculate the new e-factor given existing GNOSIS and SUCCESS, either t or nil.

Next GNOSIS is calculated as follows:

Upon a successful review, increase gnosis-synolon by gnosis-plus.

Upon a failed review, decrease gnosis-synolon by gnosis-minus.

ANAGNOSIS is an event threshold, updating either the gnosis-plus or
gnosis-minus values.

When C-SUCCESSES (consecutive successes) reach ANAGNOSIS,
increase gnosis-plus by EPIGNOSIS.

When C-FAILURES reach ANAGOSNIS, increase gnosis-minus by AGNOIA."
  (cl-assert (listp gnosis) nil
             "gnosis must be a list of floats.")
  (cl-assert (booleanp success) nil
             "success must be a boolean value")
  (cl-assert (and (floatp epignosis) (< epignosis 1)) nil
             "epignosis must be a float < 1")
  (cl-assert (and (floatp agnoia) (< agnoia 1)) nil
             "agnoia must be a float < 1")
  (cl-assert (integerp anagnosis) nil
             "anagnosis must be an integer.")
  (let* ((g-plus (nth 0 gnosis))
         (g-minus (nth 1 gnosis))
         (g-synolon (nth 2 gnosis))
         (anagnosis-p (gnosis-algorithm--anagnosis-trigger-p
                       success c-successes c-failures anagnosis))
         (neo-synolon (gnosis-algorithm--next-synolon
                       success g-synolon g-plus g-minus))
         (neo-plus (gnosis-algorithm--next-plus
                    g-plus success anagnosis-p epignosis c-failures lethe))
         (neo-minus (gnosis-algorithm--next-minus
                     g-minus success anagnosis-p agnoia)))
    (gnosis-algorithm-round-items (list neo-plus neo-minus neo-synolon))))

(defun gnosis-algorithm--proto-review-p (successful-reviews proto)
  "Return non-nil when SUCCESSFUL-REVIEWS is still in PROTO phase."
  (< successful-reviews (length proto)))

(defun gnosis-algorithm--normalize-last-interval (last-interval success)
  "Return LAST-INTERVAL normalized for SUCCESS."
  (if (and (<= last-interval 0) success) 1 last-interval))

(defun gnosis-algorithm--mature-failure-interval (last-interval gnosis-synolon amnesia)
  "Return capped failure interval from LAST-INTERVAL.
GNOSIS-SYNOLON and AMNESIA determine the success and failure caps."
  (let ((success-interval (* gnosis-synolon last-interval))
        (failure-interval (* amnesia last-interval)))
    (max (min success-interval failure-interval 7) 0)))

(defun gnosis-algorithm--next-interval-days
    (last-interval gnosis-synolon success successful-reviews amnesia proto c-fails lethe)
  "Return interval days before fuzzing and date conversion.
LAST-INTERVAL, GNOSIS-SYNOLON, SUCCESS, SUCCESSFUL-REVIEWS,
AMNESIA, PROTO, C-FAILS, and LETHE determine the scheduling rule."
  (cond ((and (gnosis-algorithm--proto-review-p successful-reviews proto)
              success)
         (nth successful-reviews proto))
        ((and (gnosis-algorithm--proto-review-p successful-reviews proto)
              (not success))
         0)
        ((and (>= c-fails lethe) (not success))
         0)
        (success
         (* gnosis-synolon last-interval))
        (t
         (gnosis-algorithm--mature-failure-interval
          last-interval gnosis-synolon amnesia))))

(cl-defun gnosis-algorithm-next-interval
    (&key last-interval gnosis-synolon success
          successful-reviews amnesia proto c-fails lethe)
  "Calculate next interval.

LAST-INTERVAL: Number of days since last review

C-FAILS: Total consecutive failed reviews.

GNOSIS-SYNOLON: Current gnosis-synolon (gnosis totalis).

SUCCESS: non-nil when review was successful.

SUCCESSFUL-REVIEWS: Number of successful reviews.

AMNESIA: \"Forget\" value, used to calculate next interval upon failed
review.

PROTO: List of proto intervals, for successful reviews.
Until successfully completing proto reviews, for every failed attempt
the next interval will be set to 0.

LETHE: Upon having C-FAILS >= lethe, set next interval to 0."
  (cl-assert (booleanp success) nil
             "Success value must be a boolean")
  (cl-assert (integerp successful-reviews) nil
             "Successful-reviews must be an integer")
  (cl-assert (and (floatp amnesia) (<= amnesia 1)) nil
             "Amnesia must be a float <=1")
  (cl-assert (and (<= amnesia 1) (> amnesia 0)) nil
             "Value of amnesia must be a float <= 1")
  (cl-assert (and (integerp lethe) (>= lethe 1)) nil
             "Value of lethe must be an integer >= 1")
  (let* ((last-interval (gnosis-algorithm--normalize-last-interval
                         last-interval success))
         (amnesia (- 1 amnesia))
         (interval (gnosis-algorithm--next-interval-days
                    last-interval gnosis-synolon success successful-reviews
                    amnesia proto c-fails lethe)))
    (gnosis-algorithm-date
     (round (gnosis-algorithm-fuzz-interval interval)))))

(defun gnosis-algorithm--date-later-p (date1 date2)
  "Return non-nil if DATE1 is later than DATE2.
Both dates are lists of (year month day)."
  (> (time-to-days (encode-time 0 0 0
				(caddr date1) (cadr date1)
				(car date1)))
     (time-to-days (encode-time 0 0 0
				(caddr date2) (cadr date2)
				(car date2)))))


(provide 'gnosis-algorithm)
;;; gnosis-algorithm.el ends here

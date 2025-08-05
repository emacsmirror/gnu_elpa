;;; iso-date.el --- Utilities for working with ISO dates -*- lexical-binding: t -*-

;; Copyright (C) 2025  Lucas Quintana

;; Author: Lucas Quintana <lmq10@protonmail.com>
;; Maintainer: Lucas Quintana <lmq10@protonmail.com>
;; URL: https://github.com/lmq-10/iso-date
;; Created: 2025-07-28
;; Version: 1.0.2
;; Package-Requires: ((emacs "28.1"))

;; This file is not part of GNU Emacs.

;; This file is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This simple library provides utilities for working with ISO date
;; strings (formatted as YYYY-MM-DD).  This includes:
;;
;; - Returning an ISO date in a relative way (e.g. two days from now)
;;
;; - Converting to Emacs internal format
;;
;; - Converting to `calendar.el' internal format
;;
;; - User commands for manipulating dates or integrating them with
;;   external packages
;;
;; - Integrating ISO dates with `thingatpt.el'
;;
;; - Integrating ISO dates with embark (see `embark-iso-date.el')

;;; Code:

(require 'calendar)
(require 'thingatpt)
(require 'time-date)

(declare-function org-read-date "org")
(declare-function org-agenda-list "org-agenda")
(declare-function diary-check-diary-file "diary-lib")
(declare-function calc-push-list "calc")
(declare-function calc "calc")

(defconst iso-date-regexp
  (rx (group (= 4 digit)) "-"
      (group (= 2 digit)) "-"
      (group (= 2 digit)))
  "Regexp matching an ISO 8601 date string.

This means a date formatted as YYYY-MM-DD.")

;;;; Conversion

(defun iso-date-to-calendar (date)
  "Convert ISO 8601 DATE to calendar internal format.

Provided that DATE follows ISO, the return value will always be a real
date, even if DATE is non-existant; for instance, 2025-02-31 becomes (3
3 2025) which really means 2025-03-03.  If you need to remain faithful
to the original string, use `iso-date-to-calendar-lax' instead.

See also `iso-date-from-calendar'."
  (when (string-match-p iso-date-regexp date)
    (calendar-gregorian-from-absolute (date-to-day date))))

(defun iso-date-to-calendar-lax (date)
  "Convert ISO 8601 DATE to calendar internal format.

This function does not do internal conversions, so the resulting date
can be non-existant.  It will, however, be faithful to what DATE really
shows."
  (when (string-match iso-date-regexp date)
    (let ((day (match-string-no-properties 3 date))
          (month (match-string-no-properties 2 date))
          (year (match-string-no-properties 1 date)))
      (mapcar #'string-to-number (list month day year)))))

(defun iso-date-from-calendar (date)
  "Convert DATE from calendar internal format to ISO 8601 format.

See also `iso-date-to-calendar'."
  (when (and (listp date) (length= date 3))
    (format-time-string
     "%F"
     (days-to-time (- (calendar-absolute-from-gregorian date) (time-to-days 0))))))

(defun iso-date-to-internal (date)
  "Convert an ISO 8601 DATE string to an Emacs timestamp.
The timestamp points to time 00:00 at that day.

See also `iso-date-from-internal'."
  (when (string-match-p iso-date-regexp date)
    (date-to-time date)))

(defun iso-date-from-internal (ts)
  "Convert TS to an ISO 8601 date string.
See `format-time-string' for the values TS can take.

See also `iso-date-to-internal'."
  (format-time-string "%F" ts))

;;;; Creation of strings

;;;###autoload
(defun iso-date (&rest keywords)
  "Return an ISO 8601 date string for current day.

KEYWORDS allow to modify the date returned.  They are passed to
`make-decoded-time'.  For instance, the following returns a date string
for yesterday:

\(iso-date :day -1)

A special keyword named START-DATE allows to set the starting day which
will be modified by the rest of KEYWORDS.  It should be an ISO 8601 date
string.  For instance, to add a month to a specific date:

\(iso-date :start-date \"2000-12-18\" :month +1)"
  (format-time-string
   "%F"
   (when keywords
     (encode-time
      (decoded-time-add
       (if-let* ((date (plist-get keywords :start-date)))
           (progn
             (setq keywords (remove :start-date (remove date keywords)))
             (parse-time-string date))
         (decode-time))
       (apply #'make-decoded-time keywords))))))

(defun iso-date-shift (shift date)
  "Apply SHIFT to DATE, return result.

SHIFT should be a string representing a time shift such as +4w.
Supported letters are d (day), w (week), m (month) and y (year).

DATE should be an ISO 8601 date string.  Returned date is also in
that format.

This is an alternative to the plist-based modification offered by
`iso-date'."
  (when (string-match
         (rx
          (group (optional (or "+" "-")) (one-or-more digit))
          (group (any letter)))
         shift)
    (let ((num (string-to-number (match-string-no-properties 1 shift)))
          (unit (match-string-no-properties 2 shift)))
      (iso-date
       :start-date date
       (pcase unit
         ("d" :day)
         ("w" (and (setq num (* 7 num)) :day))
         ("m" :month)
         ("y" :year)
         (_ (error "Unsupported time unit")))
       num))))

;;;; Extraction

(defun iso-date-year (date)
  "Return year component from ISO DATE, as an integer."
  (when (string-match iso-date-regexp date)
    (string-to-number (match-string-no-properties 1 date))))

(defun iso-date-month (date)
  "Return month component from ISO DATE, as an integer."
  (when (string-match iso-date-regexp date)
    (string-to-number (match-string-no-properties 2 date))))

(defun iso-date-day (date)
  "Return day component from ISO DATE, as an integer."
  (when (string-match iso-date-regexp date)
    (string-to-number (match-string-no-properties 3 date))))

;;;; Validation

(defun iso-date-valid-p (date)
  "Return non-nil if DATE is a valid ISO date."
  (when (string-match-p iso-date-regexp date)
    (calendar-date-is-valid-p (iso-date-to-calendar-lax date))))

;;;; Basic integration with other packages

(defun iso-date--read ()
  "Read date in YYYY-MM-DD format."
  (catch :ok
    (while (let ((date (read-string "Date (YYYY-MM-DD): ")))
             (when (string-match iso-date-regexp date)
               (throw :ok (match-string-no-properties 0 date)))
             (message "Date not in YYYY-MM-DD format")
             (sit-for 1)
             t))))

(defun iso-date-show-calendar (date)
  "Display calendar and go to DATE."
  (interactive (list (iso-date--read)))
  (calendar)
  (calendar-goto-date (iso-date-to-calendar date)))

(defun iso-date-show-org-agenda (date)
  "Show Org agenda for DATE."
  (interactive (list (iso-date--read)))
  (require 'org)
  (org-agenda-list nil date))

(defun iso-date-show-diary (date)
  "Display a buffer with diary entries for DATE."
  (interactive (list (iso-date--read)))
  (require 'diary-lib)
  (diary-check-diary-file)
  (diary-list-entries (iso-date-to-calendar date) 1))

(defun iso-date-send-to-calc (date)
  "Insert DATE into a calc window."
  (interactive (list (iso-date--read)))
  (require 'calc)
  (let ((abs (calendar-absolute-from-gregorian (iso-date-to-calendar date))))
    (calc)
    (calc-push-list `((date ,abs)))))

;;;; Insertion and manipulation

(defun iso-date-insert (&optional arg)
  "Insert an ISO date at point.

When ARG is non-nil (interactively, with a prefix argument), prompt for
a date using `org-read-date'."
  (interactive "*P")
  (if (not arg)
      (insert (format-time-string "%F"))
    (require 'org)
    (insert (org-read-date))))

(defun iso-date-at-point-day-up (&optional n)
  "Advance ISO date at point by one day.

With a prefix argument N, advance by N days."
  (interactive "*p")
  (when-let* ((bounds (iso-date-bounds))
              (date (iso-date-at-point)))
    (save-excursion
      (delete-region (car bounds) (cdr bounds))
      (insert (iso-date :start-date date :day n)))))

(defun iso-date-at-point-day-down (&optional n)
  "Advance ISO date at point by one day.

With a prefix argument N, advance by (- N) days."
  (interactive "*p")
  (iso-date-at-point-day-up (- n)))

(defun iso-date-at-point-do-shift (shift)
  "Advance ISO date at point by SHIFT.

See `iso-date-shift' for the values SHIFT can take."
  (interactive "*sShift (e.g. +3d): ")
  (when-let* ((bounds (iso-date-bounds))
              (date (iso-date-at-point)))
    (save-excursion
      (delete-region (car bounds) (cdr bounds))
      (insert (iso-date-shift shift date)))))

;;;; Displaying useful information

(defun iso-date-echo-difference (date)
  "Echo day difference between current date and DATE."
  (interactive (list (iso-date--read)))
  (let ((day-difference (- (date-to-day date) (time-to-days nil))))
    (cond ((> day-difference 0)
           (message "This date will arrive in %d days" day-difference))
          ((< day-difference 0)
           (message "This date was %d days ago" (- day-difference)))
          (t
           (message "This is today's date")))))

(defun iso-date-pretty-print (date)
  "Echo a pretty representation for DATE.
Format is defined by the variable `calendar-date-display-form'."
  (interactive (list (iso-date--read)))
  (message (calendar-date-string (iso-date-to-calendar date))))

;;;; Misc.

(defun iso-date-between-dates-p (start end date)
  "Return non-nil if DATE (an ISO string) is somewhere between START and END."
  (or
   (and (string-collate-lessp start date) (string-collate-lessp date end))
   (equal start date)
   (equal end date)))

(defun iso-date-list-dates-between (start end)
  "Return a list with all dates between START and END.
START and END should be ISO 8601 date strings.

Returned dates are also in that format."
  (let* ((time1 (iso-date-to-internal start))
         (time2 (iso-date-to-internal end))
         (pointer time1)
         (one-day (make-decoded-time :day 1))
         dates)
    (while (not (equal pointer time2))
      (push (format-time-string "%F" pointer) dates)
      (setq pointer (encode-time (decoded-time-add (decode-time pointer) one-day))))
    (push (format-time-string "%F" time2) dates)
    (reverse dates)))

;;;; thingatpt.el integration

;;;###autoload
(progn
  (put 'iso-date 'thing-at-point #'iso-date-at-point)
  (put 'iso-date 'bounds-of-thing-at-point #'iso-date-bounds))

;;;###autoload
(defun iso-date-at-point ()
  "Return ISO date at point."
  (when (thing-at-point-looking-at iso-date-regexp 10)
    (match-string-no-properties 0)))

;;;###autoload
(defun iso-date-bounds ()
  "Return bounds of ISO date at point."
  (when (thing-at-point-looking-at iso-date-regexp 10)
    (cons (match-beginning 0) (match-end 0))))

(provide 'iso-date)
;;; iso-date.el ends here

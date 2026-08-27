;;; gnosis-algorithm.el --- Spaced repetition  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2026  Free Software Foundation, Inc.

;; Author: Thanos Apollo <public@thanosapollo.org>
;; Keywords: extensions
;; URL: https://git.thanosapollo.org/gnosis
;; Version: 0.0.1


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
(require 'gnosis-logical-day)

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

(provide 'gnosis-algorithm)
;;; gnosis-algorithm.el ends here

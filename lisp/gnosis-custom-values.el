;;; gnosis-custom-values.el --- Per-tag values  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2026  Free Software Foundation, Inc.

;; Author: Thanos Apollo <public@thanosapollo.org>
;; Keywords: extensions

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

;; Lookup and validation of per-tag algorithm parameters
;; (amnesia, epignosis, agnoia, proto, anagnosis, lethe).

;;; Code:

(require 'cl-lib)

(defcustom gnosis-custom-values
  '((:tag "demo" (:proto (1 2) :anagnosis 3 :epignosis 0.5 :agnoia 0.3
			 :amnesia 0.45 :lethe 3)))
  "Custom review values for adjusting gnosis algorithm.

Each entry is a list of (:tag NAME PARAMETERS) where:
- NAME is the tag name string
- PARAMETERS is a plist with keys:
  :proto (list of integers), :anagnosis (integer),
  :epignosis (number), :agnoia (number),
  :amnesia (number 0-1), :lethe (positive integer)"
  :type '(repeat sexp)
  :initialize #'custom-initialize-default
  :set (lambda (symbol value)
         (gnosis-validate-custom-values value)
         (set-default symbol value))
  :group 'gnosis)

(defvar gnosis-custom--valid-values
  '(:proto :anagnosis :epignosis :agnoia :amnesia :lethe))

(defun gnosis-validate-custom-values (new-value)
  "Validate the structure and values of NEW-VALUE for gnosis-custom-values."
  (unless (listp new-value)
    (user-error "GNOSIS-CUSTOM-VALUES should be a list of entries"))
  (dolist (entry new-value)
    (unless (and (listp entry) (= (length entry) 3)
                 (eq (nth 0 entry) :tag)
                 (stringp (nth 1 entry))
                 (listp (nth 2 entry))) ; Ensure the third element is a plist
      (user-error
       (concat "Each entry should have a :tag keyword,"
               " a string, and a plist of custom values")))
    (let ((proto (plist-get (nth 2 entry) :proto))
          (anagnosis (plist-get (nth 2 entry) :anagnosis))
          (epignosis (plist-get (nth 2 entry) :epignosis))
          (agnoia (plist-get (nth 2 entry) :agnoia))
          (amnesia (plist-get (nth 2 entry) :amnesia))
          (lethe (plist-get (nth 2 entry) :lethe)))
      (unless (and (listp proto) (cl-every #'integerp proto))
        (user-error "Proto must be a list of integer values"))
      (unless (or (null anagnosis) (integerp anagnosis))
        (user-error "Anagnosis should be an integer"))
      (unless (or (null epignosis) (numberp epignosis))
        (user-error "Epignosis should be a number"))
      (unless (or (null agnoia) (numberp agnoia))
        (user-error "Agnoia should be a number"))
      (unless (or (null amnesia)
                  (and (numberp amnesia)
                       (<= amnesia 1) (>= amnesia 0)))
        (user-error "Amnesia should be a number between 0 and 1"))
      (unless (or (null lethe) (and (integerp lethe) (> lethe 0)))
        (user-error "Lethe should be an integer greater than 0")))))

(provide 'gnosis-custom-values)
;;; gnosis-custom-values.el ends here

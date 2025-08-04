;;; embark-iso-date.el --- Integrate ISO dates with embark -*- lexical-binding: t -*-

;; Copyright (C) 2025  Lucas Quintana

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

;; This file adds the `iso-date' target to `embark'.

;;; Code:

(require 'iso-date)

(eval-after-load 'embark
  '(progn
     (embark-define-thingatpt-target iso-date)
     (defvar-keymap embark-iso-date-map
       :parent embark-general-map
       "RET" #'iso-date-show-calendar
       "a" #'iso-date-show-org-agenda
       "c" #'iso-date-show-calendar
       "d" #'iso-date-show-diary
       "f" #'iso-date-echo-difference
       "p" #'iso-date-pretty-print
       "x" #'iso-date-send-to-calc
       "<up>" #'iso-date-at-point-day-up
       "<down>" #'iso-date-at-point-day-down)
     (add-to-list 'embark-repeat-actions 'iso-date-at-point-day-up)
     (add-to-list 'embark-repeat-actions 'iso-date-at-point-day-down)
     (add-to-list 'embark-target-finders 'embark-target-iso-date-at-point)
     (add-to-list 'embark-keymap-alist '(iso-date embark-iso-date-map))))

(provide 'embark-iso-date)
;;; embark-iso-date.el ends here

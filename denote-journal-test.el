;;; denote-journal-test.el --- Unit tests for Denote Journal -*- lexical-binding: t -*-

;; Copyright (C) 2025  Free Software Foundation, Inc.

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; Maintainer: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://github.com/protesilaos/denote

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Tests for Denote Journal.  Note that we are using Shorthands in
;; this file, so the "djt-" prefix really is "denote-journal-test-".
;; Evaluate the following to learn more:
;;
;;    (info "(elisp) Shorthands")

;;; Code:

(require 'ert)
(require 'denote-journal)

(ert-deftest djt-denote-journal-daily--title-format ()
  "Make sure that `denote-journal-daily--title-format' yields the desired format."
  ;; These three should prompt, but I am here treating the prompt as
  ;; if it already returned a string.  The test for the
  ;; `denote-title-prompt' can be separate.
  (should (stringp
           (cl-letf (((symbol-function 'denote-title-prompt) (lambda (&rest _) ""))
                     (denote-journal-title-format nil))
             (denote-journal-daily--title-format))))

  ;; And these return the expected values.
  (should (string-match-p
           "\\<.*?\\>"
           (let ((denote-journal-title-format 'day))
             (denote-journal-daily--title-format))))

  (should (string-match-p
           "\\<.*?\\> [0-9]\\{,2\\} \\<.*?\\> [0-9]\\{,4\\}"
           (let ((denote-journal-title-format 'day-date-month-year))
             (denote-journal-daily--title-format))))

  (should (string-match-p
           "\\<.*?\\> [0-9]\\{,2\\} \\<.*?\\> [0-9]\\{,4\\} [0-9]\\{,2\\}:[0-9]\\{,2\\} \\<.*?\\>"
           (let ((denote-journal-title-format 'day-date-month-year-12h))
             (denote-journal-daily--title-format))))

  (should (string-match-p
           "\\<.*?\\> [0-9]\\{,2\\} \\<.*?\\> [0-9]\\{,4\\} [0-9]\\{,2\\}:[0-9]\\{,2\\}"
           (let ((denote-journal-title-format 'day-date-month-year-24h))
             (denote-journal-daily--title-format)))))

(provide 'denote-journal-test)
;;; denote-journal-test.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("djt" . "denote-journal-test-"))
;; End:

;;; debbugs-tests.el --- tests for debbugs.el -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025 Free Software Foundation, Inc.

;; Author: Morgan Smith <Morgan.J.Smith@outlook.com>
;; Package: debbugs

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Please ensure tests don't actually make network calls.

;;; Code:

(require 'ert)
(require 'debbugs)
(require 'debbugs-test-helpers)

;;; Tests:

(ert-deftest--debbugs debbugs-test-get-bugs ()
  "Test \"get_bugs\"."
  (debbugs-get-bugs
   :tag "patch"
   :severity "critical"
   :status "open"
   :status "forwarded")
  (should (string-equal debbugs-test--soap-operation-name "get_bugs"))
  (should (equal debbugs-test--soap-parameters
                 '(["tag" "patch" "severity" "critical"
                    "status" "open" "status" "forwarded"]))))

(ert-deftest--debbugs debbugs-test-newest-bugs ()
  "Test \"newest_bugs\"."
  (debbugs-newest-bugs 4)
  (should (string-equal debbugs-test--soap-operation-name "newest_bugs"))
  (should (equal debbugs-test--soap-parameters '(4))))

(ert-deftest--debbugs debbugs-test-newest-bug-cached ()
  "Test getting the newest bug from the cache."
  ;; First time we get it from the server.
  (should (equal (debbugs-newest-bugs 1) '(0)))
  (should (equal debbugs-test--soap-operation-name "newest_bugs"))
  (should (equal debbugs-test--soap-parameters '(1)))
  (setq debbugs-test--soap-operation-name nil)
  (setq debbugs-test--soap-parameters nil)
  ;; Now it's cached
  (should (equal (debbugs-newest-bugs 1) '(0)))
  (should (equal debbugs-test--soap-operation-name nil))
  (should (equal debbugs-test--soap-parameters nil)))

(ert-deftest--debbugs debbugs-test-get-status ()
  "Test \"get_status\"."
  (should (equal (car (debbugs-get-status 64064))
                 (car debbugs-test--bug-status)))
  (should (string-equal debbugs-test--soap-operation-name "get_status"))
  (should (equal debbugs-test--soap-parameters '([64064])))
  (setq debbugs-test--soap-operation-name nil)
  (setq debbugs-test--soap-parameters nil)
  ;; cached
  (should (equal (car (debbugs-get-status 64064))
                 (car debbugs-test--bug-status)))
  (should (equal debbugs-test--soap-operation-name nil))
  (should (equal debbugs-test--soap-parameters nil)))

(ert-deftest--debbugs debbugs-test-get-usertag ()
  "Test \"get_usertag\"."
  (should (equal (debbugs-get-usertag :user "emacs") '("hi")))
  (should (string-equal debbugs-test--soap-operation-name "get_usertag"))
  (should (equal debbugs-test--soap-parameters '("emacs"))))

(provide 'debbugs-tests)

;;; debbugs-tests.el ends here

;;; debbugs-test-helpers.el --- Helpers for testing debbugs -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Free Software Foundation, Inc.

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

(require 'debbugs)

;;; Helper Data:

;; Generated using this:
;; (soap-invoke debbugs-wsdl debbugs-port "get_status" [64064])
(defconst debbugs-test--bug-status-soap-return
  '(((item
      (key . 64064)
      (value
       (package . "emacs") (found_date) (last_modified . 1689593050)
       (affects) (date . 1686745022) (fixed_versions)
       (originator . "Morgan Smith <Morgan.J.Smith@outlook.com>")
       (blocks) (archived . 1) (found) (unarchived) (tags . "patch")
       (severity . "normal") (location . "archive") (owner) (fixed)
       (blockedby) (pending . "done") (keywords . "patch") (id . 64064)
       (found_versions) (mergedwith) (summary) (forwarded)
       (log_modified . 1689593050)
       (done . "Michael Albinus <michael.albinus@gmx.de>")
       (source . "unknown")
       (msgid
        . "<DM5PR03MB31632E3A4FE170C62E7D4B0CC55AA@DM5PR03MB3163.namprd03.prod.outlook.com>")
       (bug_num . 64064) (subject . "[PATCH 0/4] debbugs improvements")
       (fixed_date)))))
  "Mock result from `soap-invoke' for bug 64064.")

;; Generated using this:
;; (debbugs-get-status 64064)
(defconst debbugs-test--bug-status
  '(((cache_time . 5000) (affects) (archived . t) (blockedby) (blocks)
     (bug_num . 64064) (date . 1686745022)
     (done . "Michael Albinus <michael.albinus@gmx.de>") (fixed) (fixed_date)
     (fixed_versions) (forwarded) (found) (found_date) (found_versions)
     (id . 64064) (keywords "patch") (last_modified . 1689593050)
     (location . "archive") (log_modified . 1689593050) (mergedwith)
     (msgid
      . "<DM5PR03MB31632E3A4FE170C62E7D4B0CC55AA@DM5PR03MB3163.namprd03.prod.outlook.com>")
     (originator . "Morgan Smith <Morgan.J.Smith@outlook.com>") (owner)
     (package "emacs") (pending . "done") (severity . "normal")
     (source . "unknown") (subject . "[PATCH 0/4] debbugs improvements") (summary)
     (tags "patch") (unarchived)))
  "Mock result from `debbugs-get-status' for bug 64064.")

;;; Helper Functions:

(defvar debbugs-test--soap-operation-name nil)
(defvar debbugs-test--soap-parameters nil)
(defun debbugs-test--soap-invoke-internal
    (callback _cbargs _wsdl _service operation-name &rest parameters)
  "Over-ride for testing."
  (setq debbugs-test--soap-operation-name operation-name)
  (setq debbugs-test--soap-parameters parameters)
  (let ((return
         (cond ((string-equal operation-name "get_status")
                debbugs-test--bug-status-soap-return)
               ((string-equal operation-name "get_usertag")
                '(((hi))))
               (t '((0))))))
    (if callback
        (progn
          (funcall callback return)
          nil)
      return)))

(defun debbugs-test--override-float-time (func &rest rest)
  "Override `float-time' for FUNC with args REST."
  (cl-letf (((symbol-function #'float-time)
             (lambda (&optional _specified-time) 5000)))
    (apply func rest)))

(defun debbugs-test--setup ()
  "Mock network and time functions.
These mock functions are needed to make the tests reproducible."
  (setq debbugs-test--soap-operation-name nil)
  (setq debbugs-test--soap-parameters nil)

  (add-function
   :override (symbol-function #'soap-invoke-internal)
   #'debbugs-test--soap-invoke-internal)

  (add-function
   :around (symbol-function #'debbugs-get-cache)
   #'debbugs-test--override-float-time)

  (add-function
   :around (symbol-function #'debbugs-put-cache)
   #'debbugs-test--override-float-time))

(defun debbugs-test--teardown ()
  "Restore functions to as they where before."
  (setq debbugs-test--soap-operation-name nil)
  (setq debbugs-test--soap-parameters nil)

  (remove-function
   (symbol-function #'soap-invoke-internal)
   #'debbugs-test--soap-invoke-internal)

  (remove-function
   (symbol-function #'debbugs-get-cache)
   #'debbugs-test--override-float-time)

  (remove-function
   (symbol-function #'debbugs-put-cache)
   #'debbugs-test--override-float-time))

(defmacro ert-deftest--debbugs (name args docstring &rest body)
  "The same as `ert-deftest' but runs setup and teardown functions."
  (declare
   (doc-string 3)
   (indent 2))
  `(ert-deftest ,name ,args ,docstring
                (unwind-protect
                    (progn
                      (debbugs-test--setup)
                      ,@body)
                  (debbugs-test--teardown))))


(provide 'debbugs-test-helpers)

;;; debbugs-test-helpers.el ends here

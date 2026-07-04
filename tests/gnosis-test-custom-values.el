;;; gnosis-test-custom-values.el --- Gnosis custom value tests  -*- lexical-binding: t; -*-

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

;; Testing module for gnosis custom algorithm values.

;;; Code:
(require 'ert)

(let ((lisp-dir (expand-file-name "../lisp"
                  (file-name-directory (or load-file-name default-directory)))))
  (add-to-list 'load-path lisp-dir))

(require 'gnosis-custom-values)

(ert-deftest gnosis-test-get-custom-tag-amnesia ()
  "Test recovery of tag amnesia values."
  (let ((test-values '((:tag "tag1" (:proto (0 1 3) :epignosis 0.5 :agnoia 0.3 :amnesia 0.3 :lethe 3))
		       (:tag "tag2" (:proto (1 2) :epignosis 0.5 :agnoia 0.3 :amnesia 0.5 :lethe 4))
		       (:tag "tag3" (:proto (2 4 10) :epignosis 0.5 :agnoia 0.5 :amnesia 0.9 :lethe 2)))))
    (should (equal (gnosis-get-custom-tag-values nil :amnesia '("tag1") test-values) (list 0.3)))
    (should (equal (gnosis-get-thema-tag-amnesia nil '("tag1") test-values) 0.3))
    (should (equal (gnosis-get-thema-tag-amnesia nil '("tag1" "tag2") test-values) 0.5))
    (should (equal (gnosis-get-thema-tag-amnesia nil '("tag1" "tag2" "tag3") test-values) 0.9))
    (should (equal (gnosis-get-thema-tag-amnesia nil '("tag2" "tag1") test-values) 0.5))))

(ert-deftest gnosis-test-get-proto ()
  (let ((test-values '((:tag "tag1" (:epignosis 0.5))
		       (:tag "tag2" (:proto (2 2 2) :epignosis 0.5))
		       (:tag "tag3" (:proto (1 1 1 1) :epignosis 0.5)))))
    ;; tag1 has no proto, falls back to algorithm default
    (should (equal (gnosis-get-thema-proto nil '("tag1") test-values) gnosis-algorithm-proto))
    (should (equal (gnosis-get-thema-proto nil '("tag1" "tag2") test-values) '(2 2 2)))
    (should (equal (gnosis-get-thema-proto nil '("tag1" "tag2" "tag3") test-values) '(2 2 2 1)))))

(ert-deftest gnosis-test-get-thema-amnesia ()
  (let ((test-values '((:tag "tag1" (:proto (10 1) :epignosis 0.5))
		       (:tag "tag2" (:proto (2 2 2) :epignosis 0.5 :amnesia 0.2))
		       (:tag "tag3" (:proto (1 1 1 1) :epignosis 0.5 :amnesia 0.6)))))
    ;; tag1 has no amnesia, falls back to algorithm default
    (should (equal (gnosis-get-thema-amnesia nil '("tag1") test-values) gnosis-algorithm-amnesia-value))
    (should (equal (gnosis-get-thema-amnesia nil '("tag1" "tag2") test-values) 0.2))
    (should (equal (gnosis-get-thema-amnesia nil '("tag1" "tag3") test-values) 0.6))
    (should (equal (gnosis-get-thema-amnesia nil '("tag2" "tag3") test-values) 0.6))))

(ert-deftest gnosis-test-get-thema-epginosis ()
  (let ((test-values '((:tag "tag1" (:proto (10 1) :amnesia 0.5))
		       (:tag "tag2" (:proto (2 2 2) :epignosis 0.6 :amnesia 0.2))
		       (:tag "tag3" (:proto (1 1 1 1) :epignosis 0.7 :amnesia 0.4)))))
    ;; tag1 has no epignosis, falls back to algorithm default
    (should (equal (gnosis-get-thema-epignosis nil '("tag1") test-values) gnosis-algorithm-epignosis-value))
    (should (equal (gnosis-get-thema-epignosis nil '("tag1" "tag2") test-values) 0.6))
    (should (equal (gnosis-get-thema-epignosis nil '("tag2" "tag3") test-values) 0.7))))

(ert-deftest gnosis-test-get-thema-agnoia ()
  (let ((test-values '((:tag "tag1" (:proto (10 1) :epignosis 0.4 :amnesia 0.5))
		       (:tag "tag2" (:proto (2 2 2) :epignosis 0.6 :amnesia 0.2 :agnoia 0.4))
		       (:tag "tag3" (:proto (1 1 1 1) :epignosis 0.7 :amnesia 0.4 :agnoia 0.5)))))
    ;; tag1 has no agnoia, falls back to algorithm default
    (should (equal (gnosis-get-thema-agnoia nil '("tag1") test-values) gnosis-algorithm-agnoia-value))
    (should (equal (gnosis-get-thema-agnoia nil '("tag1" "tag2") test-values) 0.4))
    (should (equal (gnosis-get-thema-agnoia nil '("tag1" "tag2" "tag3") test-values) 0.5))))

(ert-deftest gnosis-test-get-thema-anagnosis ()
  (let ((test-values '((:tag "tag1" (:proto (10 1)))
		       (:tag "tag2" (:proto (2 2 2) :amnesia 0.2 :agnoia 0.4 :anagnosis 2))
		       (:tag "tag3" (:proto (1 1 1 1) :amnesia 0.3)))))
    ;; tag1 has no anagnosis, falls back to algorithm default
    (should (equal (gnosis-get-thema-anagnosis nil '("tag1") test-values) gnosis-algorithm-anagnosis-value))
    (should (equal (gnosis-get-thema-anagnosis nil '("tag1" "tag2") test-values) 2))
    (should (equal (gnosis-get-thema-anagnosis nil '("tag2") test-values) 2))))

(ert-deftest gnosis-test-get-thema-lethe ()
  (let ((test-values '((:tag "tag1" (:proto (10 1) :lethe nil))
		       (:tag "tag2" (:proto (2 2 2) :lethe 2))
		       (:tag "tag3" (:proto (1 1 1 1) :amnesia 0.3 :lethe 1)))))
    ;; tag1 has :lethe nil (filtered out), falls back to algorithm default
    (should (equal (gnosis-get-thema-lethe nil '("tag1") test-values) gnosis-algorithm-lethe-value))
    (should (equal (gnosis-get-thema-lethe nil '("tag2") test-values) 2))
    ;; min of tag3=1, tag2=2
    (should (equal (gnosis-get-thema-lethe nil '("tag3" "tag2") test-values) 1))
    ;; tag1 lethe nil filtered, only tag2=2
    (should (equal (gnosis-get-thema-lethe nil '("tag1" "tag2") test-values) 2))))

(ert-deftest gnosis-test-validate-custom-values-amnesia-bounds ()
  "Custom-value validation rejects amnesia=0 at config time (C2)."
  (let ((make-entry
         (lambda (amnesia)
           (list :tag "demo"
                 (list :proto '(1 2) :anagnosis 3
                       :epignosis 0.5 :agnoia 0.3
                       :amnesia amnesia :lethe 3)))))
    ;; amnesia=0 is the C2 bug: must be rejected before review starts.
    (should-error
     (gnosis-validate-custom-values (list (funcall make-entry 0)))
     :type 'user-error)
    ;; amnesia=nil means "use the default" here, so it is accepted.
    (should (eq t (gnosis-validate-custom-values
                   (list (funcall make-entry nil)))))
    ;; amnesia in (0, 1] accepted (returns t).
    (dolist (amnesia '(0.1 0.5 1.0))
      (should (eq t (gnosis-validate-custom-values
                     (list (funcall make-entry amnesia))))))
    ;; amnesia > 1 rejected.
    (should-error
     (gnosis-validate-custom-values (list (funcall make-entry 1.5)))
     :type 'user-error)))

(provide 'gnosis-test-custom-values)

(ert-run-tests-batch-and-exit)
;;; gnosis-test-custom-values.el ends here

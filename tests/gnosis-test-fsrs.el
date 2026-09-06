;;; gnosis-test-fsrs.el --- FSRS conformance tests  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Free Software Foundation, Inc.

;; Author: Thanos Apollo <public@thanosapollo.org>
;; Keywords: extensions
;; URL: https://git.thanosapollo.org/gnosis
;; Version: 0.0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Verify the independent Elisp FSRS-6 kernel against the language-neutral
;; corpus generated through the pinned fsrs-rs oracle.

;;; Code:

(require 'ert)
(require 'json)
(require 'gnosis-fsrs)

(defconst gnosis-test-fsrs--corpus-file
  (expand-file-name "gnosis-fsrs-v2.json"
                    (file-name-directory
                     (or load-file-name buffer-file-name)))
  "Path to the generated FSRS conformance corpus.")

(defconst gnosis-test-fsrs--tolerance 0.001
  "Pinned maximum absolute error for corpus float comparisons.")

(defun gnosis-test-fsrs--corpus ()
  "Return the parsed FSRS conformance corpus."
  (with-temp-buffer
    (insert-file-contents gnosis-test-fsrs--corpus-file)
    (json-parse-buffer :object-type 'alist
                       :array-type 'list
                       :null-object nil
                       :false-object :false)))

(defun gnosis-test-fsrs--state (value)
  "Return internal state plist for corpus VALUE."
  (and value
       (list :stability (alist-get 'stability value)
             :difficulty (alist-get 'difficulty value))))

(defun gnosis-test-fsrs--keys (alist)
  "Return sorted symbol keys from ALIST."
  (sort (mapcar #'car alist)
        (lambda (a b) (string< (symbol-name a) (symbol-name b)))))

(defun gnosis-test-fsrs--require-keys (alist expected context)
  "Require ALIST to have exactly EXPECTED keys for CONTEXT."
  (unless (equal (gnosis-test-fsrs--keys alist)
                 (sort (copy-sequence expected)
                       (lambda (a b)
                         (string< (symbol-name a) (symbol-name b)))))
    (error "Unexpected FSRS corpus keys for %s" context)))

(defun gnosis-test-fsrs--validate-case (case)
  "Validate the exact schema of one corpus CASE."
  (gnosis-test-fsrs--require-keys
   case
   (append '(prior_state elapsed_days outcome expected)
           (and (assq 'desired_retention case) '(desired_retention)))
   "case")
  (when-let* ((state (alist-get 'prior_state case)))
    (gnosis-test-fsrs--require-keys
     state '(stability difficulty) "prior state"))
  (gnosis-test-fsrs--require-keys
   (alist-get 'expected case)
   '(stability difficulty raw_interval_days calendar_interval_days)
   "expected result"))

(defun gnosis-test-fsrs--validate-corpus (corpus)
  "Validate pinned metadata and exact nested schema of CORPUS."
  (let* ((oracle (alist-get 'oracle corpus))
         (outcomes (alist-get 'binary_outcomes oracle)))
    (gnosis-test-fsrs--require-keys
     corpus '(schema version oracle first_reviews independent_grid
                     sequential_history rounding_boundaries)
     "corpus")
    (gnosis-test-fsrs--require-keys
     oracle '(implementation crate_name crate_version model parameters
                             desired_retention float_tolerance enable_fuzz
                             enable_short_term binary_outcomes)
     "oracle")
    (gnosis-test-fsrs--require-keys outcomes '(failure success) "outcomes")
    (unless (and (equal "gnosis-fsrs-conformance" (alist-get 'schema corpus))
                 (= 2 (alist-get 'version corpus))
                 (equal "fsrs-rs" (alist-get 'implementation oracle))
                 (equal "fsrs" (alist-get 'crate_name oracle))
                 (equal "6.6.1" (alist-get 'crate_version oracle))
                 (equal "FSRS-6" (alist-get 'model oracle))
                 (= 0.9 (alist-get 'desired_retention oracle))
                 (= gnosis-test-fsrs--tolerance
                    (alist-get 'float_tolerance oracle))
                 (eq :false (alist-get 'enable_fuzz oracle))
                 (eq t (alist-get 'enable_short_term oracle))
                 (equal "Again" (alist-get 'failure outcomes))
                 (equal "Good" (alist-get 'success outcomes))
                 (equal (append gnosis-fsrs-default-parameters nil)
                        (alist-get 'parameters oracle)))
      (error "Unexpected FSRS corpus metadata"))
    (dolist (section '((first_reviews . 2)
                       (independent_grid . 640)
                       (sequential_history . 1000)
                       (rounding_boundaries . 4)))
      (let ((cases (alist-get (car section) corpus)))
        (unless (= (cdr section) (length cases))
          (error "Unexpected FSRS corpus count"))
        (mapc #'gnosis-test-fsrs--validate-case cases)))))

(defun gnosis-test-fsrs--assert-close (actual expected tolerance)
  "Assert ACTUAL is within TOLERANCE of EXPECTED."
  (should (<= (abs (- actual expected)) tolerance)))

(defun gnosis-test-fsrs--assert-result (result case tolerance)
  "Assert RESULT matches corpus CASE within TOLERANCE."
  (let ((expected (alist-get 'expected case)))
    (dolist (field '((:stability . stability)
                     (:difficulty . difficulty)
                     (:raw-interval-days . raw_interval_days)))
      (gnosis-test-fsrs--assert-close
       (plist-get result (car field))
       (alist-get (cdr field) expected)
       tolerance))
    (should (= (plist-get result :calendar-interval-days)
               (alist-get 'calendar_interval_days expected)))))

(defun gnosis-test-fsrs--transition (case retention prior-state)
  "Return transition for CASE at RETENTION from PRIOR-STATE."
  (gnosis-fsrs-transition
   prior-state
   (alist-get 'elapsed_days case)
   (intern (alist-get 'outcome case))
   (or (alist-get 'desired_retention case) retention)))

(ert-deftest gnosis-test-fsrs-canonical-corpus ()
  "Match every transition from the pinned fsrs-rs corpus."
  (let* ((corpus (gnosis-test-fsrs--corpus))
         (oracle (alist-get 'oracle corpus))
         (retention (alist-get 'desired_retention oracle)))
    (dolist (section '(first_reviews independent_grid rounding_boundaries))
      (dolist (case (alist-get section corpus))
        (gnosis-test-fsrs--assert-result
         (gnosis-test-fsrs--transition
          case retention
          (gnosis-test-fsrs--state (alist-get 'prior_state case)))
         case gnosis-test-fsrs--tolerance)))))

(ert-deftest gnosis-test-fsrs-corpus-contract-is-pinned ()
  "Reject corpus metadata or shape drift from the pinned oracle."
  (should-not (gnosis-test-fsrs--validate-corpus
               (gnosis-test-fsrs--corpus))))

(ert-deftest gnosis-test-fsrs-corpus-contract-kills-schema-drift ()
  "Reject weakened tolerance and unknown or missing corpus fields."
  (let ((corpus (gnosis-test-fsrs--corpus)))
    (setcdr (assq 'float_tolerance (alist-get 'oracle corpus)) 1.0)
    (should-error (gnosis-test-fsrs--validate-corpus corpus)))
  (let ((corpus (gnosis-test-fsrs--corpus)))
    (push '(unknown . t) corpus)
    (should-error (gnosis-test-fsrs--validate-corpus corpus)))
  (let ((corpus (gnosis-test-fsrs--corpus)))
    (setq corpus (assq-delete-all 'schema corpus))
    (should-error (gnosis-test-fsrs--validate-corpus corpus))))

(ert-deftest gnosis-test-fsrs-sequential-replay ()
  "Feed every Elisp transition into the next canonical history step."
  (let* ((corpus (gnosis-test-fsrs--corpus))
         (oracle (alist-get 'oracle corpus))
         (retention (alist-get 'desired_retention oracle))
         (state nil))
    (dolist (case (alist-get 'sequential_history corpus))
      (let ((result (gnosis-test-fsrs--transition case retention state)))
        (gnosis-test-fsrs--assert-result
         result case gnosis-test-fsrs--tolerance)
        (setq state (list :stability (plist-get result :stability)
                          :difficulty (plist-get result :difficulty)))))))

(ert-deftest gnosis-test-fsrs-rejects-invalid-input ()
  "Reject malformed state, elapsed time, outcome, and retention."
  (should-error
   (gnosis-fsrs-transition '(:stability 0 :difficulty 5) 1 'success 0.9))
  (should-error
   (gnosis-fsrs-transition '(:stability 1 :difficulty 11) 1 'success 0.9))
  (should-error (gnosis-fsrs-transition nil 1 'success 0.9))
  (should-error (gnosis-fsrs-transition nil 0 'easy 0.9))
  (should-error (gnosis-fsrs-transition nil 0 'success 1.0)))

(ert-deftest gnosis-test-fsrs-retention-controls-interval-not-memory ()
  "Higher retention shortens intervals without changing learned memory state."
  (dolist (outcome '(failure success))
    (let* ((state '(:stability 20.0 :difficulty 5.0))
           (results (mapcar (lambda (retention)
                              (gnosis-fsrs-transition state 20 outcome retention))
                            '(0.8 0.9 0.95)))
           (intervals (mapcar (lambda (result)
                                (plist-get result :raw-interval-days))
                              results)))
      (should (apply #'> intervals))
      (dolist (key '(:stability :difficulty))
        (should (apply #'= (mapcar (lambda (result) (plist-get result key))
                                  results)))))))

(provide 'gnosis-test-fsrs)
;;; gnosis-test-fsrs.el ends here

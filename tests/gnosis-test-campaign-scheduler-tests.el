;;; gnosis-test-campaign-scheduler-tests.el --- Retention oracles  -*- lexical-binding: t; -*-

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

;; Additional independent fsrs-rs 6.6.1 vectors at supported nondefault
;; retentions.  Regenerate with gnosis-fsrs-retention-oracle.rs, not Elisp.
;; This is mathematical conformance evidence, not measured learning efficacy.

;;; Code:

(require 'cl-lib)
(require 'gnosis-test-fsrs)

(defconst gnosis-test-campaign-scheduler-tests--file
  (expand-file-name "gnosis-fsrs-retention.json"
                    (file-name-directory
                     (or load-file-name buffer-file-name)))
  "Path to the independent nondefault-retention vectors.")

(defun gnosis-test-campaign-scheduler-tests--corpus ()
  "Read and validate the pinned nondefault-retention corpus."
  (let* ((gnosis-test-fsrs--corpus-file
          gnosis-test-campaign-scheduler-tests--file)
         (corpus (gnosis-test-fsrs--corpus))
         (oracle (alist-get 'oracle corpus))
         (cases (alist-get 'cases corpus)))
    (gnosis-test-fsrs--require-keys corpus '(schema version oracle cases) "root")
    (gnosis-test-fsrs--require-keys
     oracle '(implementation crate_version crate_sha256 parameters
                             calendar_policy enable_fuzz enable_short_term)
     "oracle")
    (should (equal (alist-get 'schema corpus)
                   "gnosis-fsrs-nondefault-retention"))
    (should (= (alist-get 'version corpus) 1))
    (should (equal (alist-get 'implementation oracle) "fsrs-rs"))
    (should (equal (alist-get 'crate_version oracle) "6.6.1"))
    (should (equal (alist-get 'crate_sha256 oracle)
                   "b8a99ea3dec9af37c9ed3835463ff6820a578a0026a8e370b4c1c0db48dfab3f"))
    (should (equal (alist-get 'calendar_policy oracle)
                   "ties-to-even; minimum 1 day"))
    (should (eq (alist-get 'enable_fuzz oracle) :false))
    (should (eq (alist-get 'enable_short_term oracle) t))
    ;; The JSON generator widens the upstream f32 parameter values to f64.
    (should (equal (alist-get 'parameters oracle)
                   (mapcar #'gnosis-fsrs--f32 gnosis-fsrs-default-parameters)))
    (mapc #'gnosis-test-fsrs--validate-case cases)
    ;; Pin every input, including both outcomes at and just below S_MAX.
    ;; Removing or duplicating a high-stability case must not silently pass.
    (should
     (equal
      (mapcar (lambda (case)
                (list (alist-get 'desired_retention case)
                      (gnosis-test-fsrs--state (alist-get 'prior_state case))
                      (alist-get 'elapsed_days case)
                      (alist-get 'outcome case)))
              cases)
      (cl-loop for retention in '(0.8 0.95) append
               (cl-loop for (stability elapsed) in
                        '((nil 0) (20.0 0) (20.0 20) (20.0 365)
                          (1000.0 0) (36499.0 0) (36500.0 0) (36500.0 36500))
                        append
                        (cl-loop for outcome in '("failure" "success")
                                 collect
                                 (list retention
                                       (and stability
                                            (list :stability stability
                                                  :difficulty 5.0))
                                       elapsed outcome))))))
    cases))

(defun gnosis-test-campaign-scheduler-tests--assert-retention (retention)
  "Assert independent state, raw and calendar results at RETENTION."
  (dolist (case (gnosis-test-campaign-scheduler-tests--corpus))
    (when (= retention (alist-get 'desired_retention case))
      (ert-info ((format "Retention %s input %S" retention case))
        (let* ((result (gnosis-test-fsrs--transition
                        case retention
                        (gnosis-test-fsrs--state (alist-get 'prior_state case))))
               (expected (alist-get 'expected case))
               (raw (alist-get 'raw_interval_days expected)))
          ;; Calendar days are exact even when raw f32 arithmetic differs.
          (should (= (plist-get result :calendar-interval-days)
                     (alist-get 'calendar_interval_days expected)))
          (dolist (field '((:stability . stability) (:difficulty . difficulty)))
            (gnosis-test-fsrs--assert-close
             (plist-get result (car field)) (alist-get (cdr field) expected)
             gnosis-test-fsrs--tolerance))
          ;; Retain the existing 0.001 absolute floor.  At large intervals
          ;; one f32 ULP exceeds it: allow four ULPs for reference f32 versus
          ;; Elisp f64 state arithmetic and interval conversion, NOT for days.
          ;; The exponent comes from the oracle value, never the actual result.
          (gnosis-test-fsrs--assert-close
           (plist-get result :raw-interval-days) raw
           (max gnosis-test-fsrs--tolerance
                (* 4 (expt 2.0 (- (cdr (frexp raw)) 24))))))))))

(ert-deftest gnosis-test-campaign-scheduler-tests-retention-80 ()
  "Match independent lower-retention states and exact calendar intervals."
  (gnosis-test-campaign-scheduler-tests--assert-retention 0.8))

(ert-deftest gnosis-test-campaign-scheduler-tests-retention-95 ()
  "Match independent higher-retention states and exact calendar intervals."
  (gnosis-test-campaign-scheduler-tests--assert-retention 0.95))

(provide 'gnosis-test-campaign-scheduler-tests)
;;; gnosis-test-campaign-scheduler-tests.el ends here

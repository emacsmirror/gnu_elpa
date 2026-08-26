;;; gnosis-fsrs.el --- Binary FSRS-6 scheduler  -*- lexical-binding: t; -*-

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

;; Pure FSRS-6 transitions for Gnosis binary review outcomes.  The formulas
;; and defaults follow fsrs-rs 6.6.1; failure selects Again and success Good.

;;; Code:

(defconst gnosis-fsrs-default-parameters
  [0.212 1.2931 2.3065 8.2956 6.4133 0.8334 3.0194 0.001 1.8722
   0.1666 0.796 1.4835 0.0614 0.2629 1.6483 0.6014 1.8729 0.5425
   0.0912 0.0658 0.1542]
  "Pinned FSRS-6 parameters from fsrs-rs 6.6.1.")

(defconst gnosis-fsrs-default-retention 0.9
  "Default desired retention for FSRS reviews.")

(defun gnosis-fsrs--f32 (value)
  "Return VALUE rounded to an IEEE-754 single-precision float."
  (if (zerop value)
      0.0
    (let* ((parts (frexp value))
           (significand (car parts))
           (exponent (cdr parts)))
      (ldexp (round (* significand 16777216.0)) (- exponent 24)))))

(defun gnosis-fsrs--weight (index)
  "Return pinned FSRS parameter at INDEX."
  (gnosis-fsrs--f32
   (aref gnosis-fsrs-default-parameters index)))

(defun gnosis-fsrs--clamp (value minimum maximum)
  "Clamp VALUE between MINIMUM and MAXIMUM."
  (min maximum (max minimum value)))

(defun gnosis-fsrs--finite-number-p (value)
  "Return non-nil when VALUE is a finite number."
  (and (numberp value)
       (= value value)
       (<= (abs value) 1.0e100)))

(defun gnosis-fsrs--initial-stability (rating)
  "Return initial stability for RATING."
  (gnosis-fsrs--weight (1- (gnosis-fsrs--clamp rating 1 4))))

(defun gnosis-fsrs--initial-difficulty (rating)
  "Return initial difficulty for RATING."
  (+ (gnosis-fsrs--weight 4)
     (- (exp (* (gnosis-fsrs--weight 5) (1- rating))))
     1.0))

(defun gnosis-fsrs--retrievability (elapsed-days stability)
  "Return retrievability after ELAPSED-DAYS at STABILITY."
  (let* ((decay (- (gnosis-fsrs--weight 20)))
         (factor (- (exp (/ (log 0.9) decay)) 1.0)))
    (expt (+ (* (/ elapsed-days stability) factor) 1.0) decay)))

(defun gnosis-fsrs--next-difficulty (difficulty rating)
  "Return difficulty after RATING from DIFFICULTY."
  (let* ((delta (* (- (gnosis-fsrs--weight 6)) (- rating 3.0)))
         (damped (/ (* (- 10.0 difficulty) delta) 9.0))
         (next (+ difficulty damped))
         (easy-target (gnosis-fsrs--initial-difficulty 4)))
    (gnosis-fsrs--clamp
     (+ (* (gnosis-fsrs--weight 7) (- easy-target next)) next)
     1.0 10.0)))

(defun gnosis-fsrs--success-stability (stability difficulty retrievability)
  "Return Good stability from STABILITY, DIFFICULTY and RETRIEVABILITY."
  (* stability
     (1+ (* (exp (gnosis-fsrs--weight 8))
            (- 11.0 difficulty)
            (expt stability (- (gnosis-fsrs--weight 9)))
            (- (exp (* (- 1.0 retrievability)
                       (gnosis-fsrs--weight 10)))
               1.0)))))

(defun gnosis-fsrs--failure-stability (stability difficulty retrievability)
  "Return Again stability from STABILITY, DIFFICULTY and RETRIEVABILITY."
  (min (* (gnosis-fsrs--weight 11)
          (expt difficulty (- (gnosis-fsrs--weight 12)))
          (- (expt (1+ stability) (gnosis-fsrs--weight 13)) 1.0)
          (exp (* (- 1.0 retrievability) (gnosis-fsrs--weight 14))))
       (/ stability
          (exp (* (gnosis-fsrs--weight 17)
                  (gnosis-fsrs--weight 18))))))

(defun gnosis-fsrs--short-term-stability (stability rating)
  "Return same-day stability from STABILITY and RATING."
  (let ((increase
         (* (exp (* (gnosis-fsrs--weight 17)
                    (+ (- rating 3.0) (gnosis-fsrs--weight 18))))
            (expt stability (- (gnosis-fsrs--weight 19))))))
    (* stability (if (>= rating 2) (max increase 1.0) increase))))

(defun gnosis-fsrs--next-stability
    (stability difficulty elapsed-days rating)
  "Return next STABILITY from DIFFICULTY, ELAPSED-DAYS, and RATING."
  (let* ((retrievability
          (gnosis-fsrs--retrievability elapsed-days stability))
         (long-term
          (if (= rating 1)
              (gnosis-fsrs--failure-stability
               stability difficulty retrievability)
            (gnosis-fsrs--success-stability
             stability difficulty retrievability))))
    (gnosis-fsrs--clamp
     (if (zerop elapsed-days)
         (gnosis-fsrs--short-term-stability stability rating)
       long-term)
     0.001 36500.0)))

(defun gnosis-fsrs--interval (stability desired-retention)
  "Return raw interval for STABILITY and DESIRED-RETENTION."
  (let* ((decay (gnosis-fsrs--f32 (- (gnosis-fsrs--weight 20))))
         (log-retention (gnosis-fsrs--f32
                         (log (gnosis-fsrs--f32 0.9))))
         (factor (gnosis-fsrs--f32
                  (- (gnosis-fsrs--f32
                      (exp (gnosis-fsrs--f32
                            (/ log-retention decay))))
                     1.0)))
         (power (gnosis-fsrs--f32
                 (expt (gnosis-fsrs--f32 desired-retention)
                       (gnosis-fsrs--f32 (/ 1.0 decay))))))
    (gnosis-fsrs--f32
     (* (gnosis-fsrs--f32
         (/ (gnosis-fsrs--f32 stability) factor))
        (gnosis-fsrs--f32 (- power 1.0))))))

(defun gnosis-fsrs--validate-input
    (prior-state elapsed-days outcome desired-retention)
  "Validate PRIOR-STATE, ELAPSED-DAYS, OUTCOME, and DESIRED-RETENTION."
  (unless (and (integerp elapsed-days) (>= elapsed-days 0)
               (memq outcome '(failure success))
               (gnosis-fsrs--finite-number-p desired-retention)
               (< 0 desired-retention 1))
    (error "Invalid FSRS transition input"))
  (if prior-state
      (let ((stability (plist-get prior-state :stability))
            (difficulty (plist-get prior-state :difficulty)))
        (unless (and (gnosis-fsrs--finite-number-p stability)
                     (> stability 0)
                     (gnosis-fsrs--finite-number-p difficulty)
                     (<= 1 difficulty 10))
          (error "Invalid FSRS memory state")))
    (unless (zerop elapsed-days)
      (error "New FSRS state must have zero elapsed days"))))

(defun gnosis-fsrs-transition
    (prior-state elapsed-days outcome &optional desired-retention)
  "Return the FSRS-6 transition for binary OUTCOME.

PRIOR-STATE is nil or a plist containing :stability and :difficulty.
ELAPSED-DAYS is the number of logical days since the prior review.
OUTCOME is either `failure' (Again) or `success' (Good).
DESIRED-RETENTION defaults to `gnosis-fsrs-default-retention'."
  (let* ((retention (or desired-retention gnosis-fsrs-default-retention))
         (rating (if (eq outcome 'failure) 1 3)))
    (gnosis-fsrs--validate-input
     prior-state elapsed-days outcome retention)
    (let* ((stability
            (if prior-state
                (gnosis-fsrs--next-stability
                 (gnosis-fsrs--clamp
                  (plist-get prior-state :stability) 0.001 36500.0)
                 (gnosis-fsrs--clamp
                  (plist-get prior-state :difficulty) 1.0 10.0)
                 elapsed-days rating)
              (gnosis-fsrs--initial-stability rating)))
           (difficulty
            (if prior-state
                (gnosis-fsrs--next-difficulty
                 (gnosis-fsrs--clamp
                  (plist-get prior-state :difficulty) 1.0 10.0)
                 rating)
              (gnosis-fsrs--clamp
               (gnosis-fsrs--initial-difficulty rating) 1.0 10.0)))
           (interval (gnosis-fsrs--interval stability retention)))
      (list :stability stability
            :difficulty difficulty
            :raw-interval-days interval
            :calendar-interval-days (max 1 (round interval))))))

(provide 'gnosis-fsrs)
;;; gnosis-fsrs.el ends here

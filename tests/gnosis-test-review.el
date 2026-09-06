;;; gnosis-test-review.el --- Tests for gnosis-review.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Free Software Foundation, Inc.

;; Author: Thanos Apollo <public@thanosapollo.org>

;;; Commentary:

;; Tests for the scheduling logic in gnosis-review.el.
;; Uses a temporary SQLite database via `gnosis-test-with-db'.

;;; Code:

(require 'ert)
(require 'gnosis)
(require 'gnosis-review)

(load (expand-file-name "gnosis-test-helpers.el"
       (file-name-directory (or load-file-name buffer-file-name))))

;;; ---- Group 1: gnosis-review-is-due-today-p ----

(ert-deftest gnosis-test-review-due-today-past ()
  "Thema with next-rev in the past is due today."
  (gnosis-test-with-db
    (let* ((id (gnosis-test--add-basic-thema "Q" "A")))
      ;; Set next-rev to yesterday
      (gnosis-update 'scheduler-state
                     `(= due-day ,(gnosis--date-to-int
                                    (gnosis-date -1)))
                     `(= thema-id ,id))
      (should (gnosis-review-is-due-today-p id)))))

(ert-deftest gnosis-test-review-due-today-today ()
  "Thema with next-rev today is due."
  (gnosis-test-with-db
    (let* ((id (gnosis-test--add-basic-thema "Q" "A")))
      ;; next-rev defaults to today in gnosis-test--add-basic-thema
      (should (gnosis-review-is-due-today-p id)))))

(ert-deftest gnosis-test-review-due-today-future ()
  "Thema with next-rev in the future is NOT due today."
  (gnosis-test-with-db
    (let* ((id (gnosis-test--add-basic-thema "Q" "A")))
      ;; Set next-rev to tomorrow
      (gnosis-update 'scheduler-state
                     `(= due-day ,(gnosis--date-to-int
                                    (gnosis-date 1)))
                     `(= thema-id ,id))
      (should-not (gnosis-review-is-due-today-p id)))))

;;; ---- Group 2: gnosis-review-is-due-p ----

(ert-deftest gnosis-test-review-is-due-active ()
  "Active thema due today returns t."
  (gnosis-test-with-db
    (let* ((id (gnosis-test--add-basic-thema "Q" "A")))
      (should (gnosis-review-is-due-p id)))))

(ert-deftest gnosis-test-review-is-due-suspended ()
  "Suspended thema due today returns nil."
  (gnosis-test-with-db
    (let* ((id (gnosis-test--add-basic-thema "Q" "A" nil nil nil 1)))
      (should-not (gnosis-review-is-due-p id)))))

;;; ---- Group 3: gnosis-review-get--due-themata ----

(ert-deftest gnosis-test-review-get-due-filters-suspended ()
  "Suspended themata are excluded from due list."
  (gnosis-test-with-db
    (let* ((_id1 (gnosis-test--add-basic-thema "Q1" "A1"))
           (_id2 (gnosis-test--add-basic-thema "Q2" "A2" nil nil nil 1)))
      (let ((gnosis-review-new-first nil)
            (gnosis-new-themata-limit nil))
        (let ((due (gnosis-review-get--due-themata)))
          ;; Only 1 should be due (the non-suspended one)
          (should (= 1 (length due))))))))

(ert-deftest gnosis-test-review-get-due-filters-future ()
  "Themata with future next-rev are excluded."
  (gnosis-test-with-db
    (let* ((id1 (gnosis-test--add-basic-thema "Q1" "A1"))
           (id2 (gnosis-test--add-basic-thema "Q2" "A2")))
      ;; Push id2 to future
      (gnosis-update 'scheduler-state
                     `(= due-day ,(gnosis--date-to-int
                                    (gnosis-date 5)))
                     `(= thema-id ,id2))
      (let ((gnosis-review-new-first nil)
            (gnosis-new-themata-limit nil))
        (let ((due (gnosis-review-get--due-themata)))
          (should (= 1 (length due)))
          (should (= id1 (caar due))))))))

(ert-deftest gnosis-test-review-get-due-new-first-ordering ()
  "With gnosis-review-new-first, new themata appear before old ones."
  (gnosis-test-with-db
    (let* ((new-id (gnosis-test--add-basic-thema "New Q" "A"))
           (old-id (gnosis-test--add-basic-thema "Old Q" "A")))
      ;; Make old-id reviewed in scheduler authority.
      (gnosis-sqlite-execute gnosis-db
        "UPDATE scheduler_state SET reps = 5 WHERE thema_id = ?" (list old-id))
      (let ((gnosis-review-new-first t)
            (gnosis-new-themata-limit nil))
        (let* ((due (gnosis-review-get--due-themata))
               (ids (mapcar #'car due)))
          (should (= 2 (length ids)))
          ;; New thema should come first
          (should (= new-id (car ids))))))))

;;; ---- Group 4: gnosis-review-is-thema-new-p ----

(ert-deftest gnosis-test-review-thema-new-p-zero-reviews ()
  "Thema with n=0 is new."
  (gnosis-test-with-db
    (let* ((id (gnosis-test--add-basic-thema "Q" "A")))
      (should (gnosis-review-is-thema-new-p id)))))

(ert-deftest gnosis-test-review-thema-new-p-reviewed ()
  "Thema with n>0 is NOT new."
  (gnosis-test-with-db
    (let* ((id (gnosis-test--add-basic-thema "Q" "A")))
      ;; Simulate a review in scheduler authority.
      (gnosis-sqlite-execute gnosis-db
        "UPDATE scheduler_state SET reps = 1 WHERE thema_id = ?" (list id))
      (should-not (gnosis-review-is-thema-new-p id)))))

(ert-deftest gnosis-test-review-reads-only-scheduler-authority ()
  "Read due, newness, and suspension from scheduler authority."
  (gnosis-test-with-db
    (let* ((id (gnosis-test--add-basic-thema "Q" "A"))
           (today (gnosis--today-int)))
      (should (gnosis-review-is-due-today-p id))
      (should (gnosis-review-is-due-p id))
      (should (gnosis-review-is-thema-new-p id))
      (should-not (gnosis-suspended-p id))
      (should (member id (gnosis-review-get-due-themata)))
      (should (member id (gnosis-get-themata-by-reviews 0)))
      (should-not (gnosis-review-get-overdue-themata))
      (gnosis-sqlite-execute
       gnosis-db "UPDATE scheduler_state SET due_day = ?, reps = 1
                   WHERE thema_id = ?" (list (1- today) id))
      (should (equal (list id) (gnosis-review-get-overdue-themata)))
      (should (= 1 (gnosis-review-count-overdue))))))

(ert-deftest gnosis-test-review-suspension-uses-scheduler-authority ()
  "Read and update suspension through scheduler authority."
  (gnosis-test-with-db
    (let ((id (gnosis-test--add-basic-thema "Q" "A")))
      (gnosis-toggle-suspend-themata (list id) 1 t)
      (should (gnosis-suspended-p id))
      (gnosis-toggle-suspend-themata (list id) 0 t)
      (should-not (gnosis-suspended-p id)))))

(ert-deftest gnosis-test-review-suspension-rejects-invalid-value ()
  "Reject invalid suspension values without changing scheduler state."
  (gnosis-test-with-db
    (let ((id (gnosis-test--add-basic-thema "Q" "A")))
      (should-error (gnosis-toggle-suspend-themata (list id) 2 t))
      (should-not (gnosis-suspended-p id))
      (should (member id (gnosis-review-get-due-themata))))))

(ert-deftest gnosis-test-review-failure-requeues-once-and-completes ()
  "Append a failed thema once and finish after its bounded retry."
  (let ((state (gnosis-review-state-create
                :reviewed 0 :total 2 :remaining '(1 2)))
        (outcomes '((1 . nil) (2 . t) (1 . nil)))
        seen)
    (cl-letf (((symbol-function 'gnosis-review--display-thema)
               (lambda (id)
                 (let ((expected (pop outcomes)))
                   (should (= id (car expected)))
                   (push id seen)
                   (list "basic" (cons (cdr expected) nil)))))
              ((symbol-function 'gnosis-review-actions)
               (lambda (success &rest _)
                 (list :rating (if success 3 1))))
              ((symbol-function 'gnosis-suspended-p) (lambda (_) nil))
              ((symbol-function 'get-register) (lambda (_) nil))
              ((symbol-function 'force-mode-line-update) #'ignore))
      (should (eq state (gnosis-review-session state))))
    (should-not outcomes)
    (should (equal '(1 2 1) (nreverse seen)))
    (should (= 3 (gnosis-review-state-reviewed state)))
    (should (= 3 (gnosis-review-state-total state)))
    (should-not (gnosis-review-state-remaining state))
    (should (equal '(1) (gnosis-review-state-requeued state)))))

(ert-deftest gnosis-test-review-relearning-queue-honors-quit ()
  "Quit escapes before a failed thema can extend the session queue."
  (let ((state (gnosis-review-state-create
                :reviewed 0 :total 2 :remaining '(1 2)))
        (calls 0))
    (cl-letf (((symbol-function 'gnosis-review--display-thema)
               (lambda (_id)
                 (cl-incf calls)
                 (list "basic" (cons nil nil))))
              ((symbol-function 'gnosis-review-actions)
               (lambda (&rest _) (throw 'review-loop 'quit))))
      (should (eq 'quit
                  (catch 'review-loop (gnosis-review-session state)))))
    (should (= 1 calls))
    (should (equal '(1 2) (gnosis-review-state-remaining state)))))

(ert-deftest gnosis-test-review-queue-uses-final-overridden-rating ()
  "Use accepted final ratings for both override directions."
  (let ((state (gnosis-review-state-create
                :reviewed 0 :total 2 :remaining '(1 2)))
        (initial '((1 . nil) (2 . t) (2 . t)))
        (ratings '(3 1 3))
        seen)
    (cl-letf (((symbol-function 'gnosis-review--display-thema)
               (lambda (id)
                 (let ((expected (pop initial)))
                   (should (= id (car expected)))
                   (push id seen)
                   (list "basic" (cons (cdr expected) nil)))))
              ((symbol-function 'gnosis-review-actions)
               (lambda (&rest _) (list :rating (pop ratings))))
              ((symbol-function 'gnosis-suspended-p) (lambda (_) nil))
              ((symbol-function 'get-register) (lambda (_) nil))
              ((symbol-function 'force-mode-line-update) #'ignore))
      (gnosis-review-session state))
    (should-not initial)
    (should-not ratings)
    (should (equal '(1 2 2) (nreverse seen)))
    (should (= 3 (gnosis-review-state-reviewed state)))
    (should (= 3 (gnosis-review-state-total state)))
    (should (equal '(2) (gnosis-review-state-requeued state)))))

(ert-deftest gnosis-test-review-queue-mismatch-precedes-effects ()
  "Reject an out-of-order thema before display or action effects."
  (let ((state (gnosis-review-state-create
                :reviewed 0 :total 1 :remaining '(1)))
        displayed acted)
    (cl-letf (((symbol-function 'gnosis-review--display-thema)
               (lambda (_id) (setq displayed t)))
              ((symbol-function 'gnosis-review-actions)
               (lambda (&rest _) (setq acted t))))
      (should-error (gnosis-review-process-thema 2 state)))
    (should-not displayed)
    (should-not acted)
    (should (equal '(1) (gnosis-review-state-remaining state)))))

(ert-deftest gnosis-test-review-deleted-failure-is-not-requeued ()
  "Do not append a failed thema after its delete action removes it."
  (let ((state (gnosis-review-state-create
                :reviewed 0 :total 1 :remaining '(1))))
    (cl-letf (((symbol-function 'gnosis-review--display-thema)
               (lambda (_id) (list "basic" (cons nil nil))))
              ((symbol-function 'gnosis-review-actions)
               (lambda (&rest _) :deleted))
              ((symbol-function 'gnosis-suspended-p) (lambda (_) nil))
              ((symbol-function 'get-register) (lambda (_) nil))
              ((symbol-function 'force-mode-line-update) #'ignore))
      (gnosis-review-session state))
    (should (= 1 (gnosis-review-state-reviewed state)))
    (should (= 1 (gnosis-review-state-total state)))
    (should-not (gnosis-review-state-remaining state))
    (should-not (gnosis-review-state-requeued state))))

(ert-deftest gnosis-test-review-suspended-failure-is-not-requeued ()
  "Do not append a failed thema that was suspended during its action."
  (let ((state (gnosis-review-state-create
                :reviewed 0 :total 1 :remaining '(1))))
    (cl-letf (((symbol-function 'gnosis-review--display-thema)
               (lambda (_id) (list "basic" (cons nil nil))))
              ((symbol-function 'gnosis-review-actions)
               (lambda (&rest _) '(:rating 1)))
              ((symbol-function 'gnosis-suspended-p) (lambda (_) t))
              ((symbol-function 'get-register) (lambda (_) nil))
              ((symbol-function 'force-mode-line-update) #'ignore))
      (gnosis-review-session state))
    (should (= 1 (gnosis-review-state-reviewed state)))
    (should (= 1 (gnosis-review-state-total state)))
    (should-not (gnosis-review-state-remaining state))
    (should-not (gnosis-review-state-requeued state))))

(provide 'gnosis-test-review)

(ert-run-tests-batch-and-exit)
;;; gnosis-test-review.el ends here

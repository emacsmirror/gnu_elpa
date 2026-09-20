;;; gnosis-test-practice-distance.el --- Practice retry order -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;;; Commentary:
;; Queue placement and retained acceptance on disposable study data.

;;; Code:

(require 'ert)
(require 'gnosis-review-test-support)

(defvar gnosis-practice-retry-distance)

(defun gnosis-test-distance--state (ids policy &optional mode)
  "Save a disposable session for IDS, POLICY and MODE."
  (let ((state (gnosis-review-state-create
                :mode (or mode 'practice) :policy policy
                :persistent-p t :database gnosis-db
                :session-id (gnosis-scheduler-event-id)
                :event-id (gnosis-scheduler-event-id)
                :remaining ids :selected ids :initial (length ids)
                :total (length ids))))
    (gnosis-review--save-session state)
    state))

(ert-deftest gnosis-distance-retained-placement ()
  "Default and custom distances settle both native and policy practice."
  (dolist (policy (list nil (gnosis-review-practice-policy)))
    (dolist (distance '(10 1 20))
      (dolist (count '(1 4 25))
        (gnosis-test-with-db
          (let* ((ids (number-sequence 101 (+ 100 count)))
                 (gnosis-practice-retry-distance distance))
            (dolist (id ids)
              (gnosis-test--add-basic-thema "Q" "A" nil nil id))
            (gnosis-scheduler-accept-review
             (gnosis-scheduler-event-id) 101 'success 100 20260901)
            (let* ((scheduled (list (gnosis-select '* 'scheduler-state)
                                    (gnosis-select '* 'review-events)))
                   (gnosis-review--state (gnosis-test-distance--state ids policy))
                   (before (copy-tree (gnosis-review--state-data gnosis-review--state)))
                   (pending (gnosis-review-algorithm 101 nil))
                   (offset (min (1- distance) (1- count)))
                   (expected (append (seq-take (cdr ids) offset) '(101)
                                     (nthcdr offset (cdr ids)))))
              (should (cadr scheduled))
              (gnosis-review-result 101 nil pending)
              (should (equal expected (gnosis-review-state-remaining gnosis-review--state)))
              (should (equal ids (plist-get before :remaining)))
              (should (= (1+ count) (gnosis-review-state-total gnosis-review--state)))
              (gnosis-review-result 101 nil pending)
              (should (= 1 (length (gnosis-select '* 'practice-events))))
              (should (equal expected (gnosis-review-state-remaining (gnosis-review--read-session))))
              (gnosis-sqlite-close gnosis-db)
              (setq gnosis-db (gnosis-sqlite-open gnosis-test--db-file))
              (gnosis-db-init)
              (setq gnosis-review--state (gnosis-review--read-session))
              (should (equal expected (gnosis-review-state-remaining gnosis-review--state)))
              (let ((slot (gnosis-review-state-undo gnosis-review--state)))
                (setq gnosis-review--state
                      (gnosis-review-undo (plist-get slot :event-id)
                                          (plist-get slot :correction-id))))
              (should (equal ids (gnosis-review-state-remaining gnosis-review--state)))
              (should (equal scheduled (list (gnosis-select '* 'scheduler-state)
                                             (gnosis-select '* 'review-events)))))))))))

(ert-deftest gnosis-distance-invalid-values-preserve-evidence ()
  (gnosis-test-with-db
    (gnosis-test--add-basic-thema "Q" "A" nil nil 101)
    (let* ((gnosis-review--state (gnosis-test-distance--state '(101) nil))
           (pending (gnosis-review-algorithm 101 nil))
           (before (gnosis-test-content--evidence)))
      (dolist (gnosis-practice-retry-distance '(0 -1 1.5 nil "10"))
        (should-error (gnosis-review-result 101 nil pending) :type 'user-error)
        (should (equal before (gnosis-test-content--evidence)))
        (should (= 0 (gnosis-review-state-reviewed gnosis-review--state)))))))

(ert-deftest gnosis-distance-pure-advance-preserves-policy-and-inputs ()
  "Placement changes neither retry eligibility nor successful continuations."
  (let* ((state (gnosis-review-state-create
                 :mode 'practice :policy (gnosis-review-practice-policy)
                 :remaining '(1 2 3) :outcomes '((1) (2 . t))
                 :requeued '(1 2)))
         (before (copy-tree state t)))
    (should-not (eq before state))
    (gnosis-review--advance state 1 nil t "next" nil 1)
    (should (equal before state)))
  (dolist (mode '(due practice))
    (dolist (policy (list nil (gnosis-review-practice-policy)))
      (let* ((ids (number-sequence 101 125))
             (state (gnosis-review-state-create
                     :mode mode :policy policy :remaining ids :total 25))
             (before (copy-tree state t))
             (next (gnosis-review--advance state 101 nil t "next" nil 1)))
        (should-not (eq before state))
        (should (equal before state))
        (should (equal (if (eq mode 'practice) (cons 101 (cdr ids))
                         (append (cdr ids) '(101)))
                       (gnosis-review-state-remaining next)))
        (should (equal '(101) (gnosis-review-state-requeued next)))
        (should (= 26 (gnosis-review-state-total next)))
        (should (equal '((101)) (gnosis-review-state-outcomes next)))
        (dolist (skipped '(nil t))
          (let ((next (gnosis-review--advance state 101 nil nil "next" skipped 1)))
            (should (equal (cdr ids) (gnosis-review-state-remaining next)))
            (should (equal '(101) (gnosis-review-state-skipped next)))
            (should-not (gnosis-review-state-requeued next))))
        ;; One native retry is exhausted, while policy failures may continue.
        (setf (gnosis-review-state-remaining next) ids)
        (let ((again (gnosis-review--advance next 101 nil t "again" nil 1)))
          (should (equal (if policy
                             (if (eq mode 'practice) ids (append (cdr ids) '(101)))
                           (cdr ids))
                         (gnosis-review-state-remaining again))))
        (setf (gnosis-review-state-outcomes state) '((101 . nil))
              (gnosis-review-state-requeued state) '(101))
        (let ((success (gnosis-review--advance state 101 t t "success" nil 1)))
          (should (equal (if policy (append (cdr ids) '(101)) (cdr ids))
                         (gnosis-review-state-remaining success))))
        (setf (gnosis-review-state-outcomes state)
              '((101) (101) (101) (101)))
        (should (equal (cdr ids)
                       (gnosis-review-state-remaining
                        (gnosis-review--advance state 101 nil t "cap" nil 1))))))))

(ert-deftest gnosis-distance-successive-failures ()
  (dolist (policy (list nil (gnosis-review-practice-policy)))
    (let* ((state (gnosis-review-state-create
                   :mode 'practice :policy policy :remaining '(1 2 3 4 5)))
           (first (gnosis-review--advance state 1 nil t "first" nil 3))
           (second (gnosis-review--advance first 2 nil t "second" nil 3)))
      (should (equal '(2 3 1 4 5) (gnosis-review-state-remaining first)))
      (should (equal '(3 1 2 4 5) (gnosis-review-state-remaining second)))
      (should (equal '(1 2 3 4 5) (gnosis-review-state-remaining state)))
      (should (equal '(2 1) (gnosis-review-state-requeued second))))))

(ert-deftest gnosis-distance-transient-practice-and-due ()
  "The nonpersistent legacy runner applies the preference only in practice."
  (dolist (mode '(practice due))
    (let* ((gnosis-practice-retry-distance 1)
           (state (gnosis-review-state-create :mode mode :remaining '(1 2 3)))
           (gnosis-monkeytype-enable nil))
      (cl-letf (((symbol-function 'gnosis-study-eligible-p) (lambda (_) t))
                ((symbol-function 'gnosis-review--display-thema)
                 (lambda (_) (list "basic" (cons nil nil))))
                ((symbol-function 'gnosis-review-actions)
                 (lambda (&rest _) '(:rating 1)))
                ((symbol-function 'gnosis-suspended-p) (lambda (_) nil))
                ((symbol-function 'get-register) (lambda (_) nil)))
        (gnosis-review-process-thema 1 state))
      (should (equal (if (eq mode 'practice) '(1 2 3) '(2 3 1))
                     (gnosis-review-state-remaining state))))))

(ert-deftest gnosis-distance-option-default-and-custom-validation ()
  (should (= 10 (eval (car (get 'gnosis-practice-retry-distance 'standard-value)) t)))
  (dolist (value '(0 -1 nil 1.5 "10"))
    (should-error (funcall (get 'gnosis-practice-retry-distance 'custom-set)
                          'gnosis-practice-retry-distance value)
                  :type 'user-error)))

(provide 'gnosis-test-practice-distance)
;;; gnosis-test-practice-distance.el ends here

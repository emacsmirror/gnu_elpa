;;; gnosis-test-review-quality.el --- Pending review regressions -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;;; Commentary:
;; Exercise presentation and action continuations with real disposable evidence.

;;; Code:
(require 'ert)
(require 'gnosis-test-review-content)

(defun gnosis-test-review-quality--seed ()
  "Retain unrelated scheduled and practice evidence in the current database."
  (gnosis-add-thema-fields "basic" "Seed question" nil '("seed") "Teaching" nil 0 nil nil 333)
  (dolist (mode '(due practice))
    (let* ((gnosis-review-buffer-name (format "*quality seed %s*" mode))
           (buffer (gnosis-review--setup-buffer '(333) mode)))
      (unwind-protect
          (with-current-buffer buffer
            (cl-letf (((symbol-function 'gnosis--read-string-with-input-method)
                       (lambda (&rest _) "seed")))
              (let ((answer (gnosis-review-basic 333)))
                (gnosis-review-result 333 (car answer) (cdr answer)))))
        (kill-buffer buffer))))
  (should (gnosis-select '* 'review-events))
  (should (gnosis-select '* 'practice-events)))

(defmacro gnosis-test-review-quality--with-basic (mode &rest body)
  "Run BODY in an owned basic review of MODE with unrelated history."
  (declare (indent 1))
  `(gnosis-test-with-db
     (gnosis-test-review-quality--seed)
     (gnosis-test-content--add "basic")
     (let* ((gnosis-review-buffer-name "*quality review*")
            (gnosis-review-basic-input 'typed)
            (gnosis-center-content nil)
            (buffer (gnosis-review--setup-buffer '(222) ,mode)))
       (unwind-protect (with-current-buffer buffer ,@body)
         (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest gnosis-test-review-quality-status-owns-only-its-region ()
  "Authored labels survive initial feedback and repeated overrides."
  (dolist (mode '(due practice))
    (gnosis-test-review-quality--with-basic mode
      (let ((question "Next review keeps question α.")
            (answer "Next review keeps answer β.")
            (explanation "Practice: schedule unchanged; Next review keeps explanation γ."))
        (gnosis-update 'themata `(= keimenon ,question) '(= id 222))
        (gnosis-update 'themata `(= answer '(,answer)) '(= id 222))
        (gnosis-update 'extras `(= parathema ,explanation) '(= id 222))
        (let* ((before (gnosis-test-content--evidence))
               (pending (cdr (gnosis-test-content--answer "basic" nil answer))))
          (dotimes (i 5)
            (dolist (text (list question answer explanation))
              (should (string-search text (buffer-string))))
            (goto-char (point-min))
            (should (= (how-many (if (eq mode 'due) "Next review:" "^Practice: schedule unchanged$")
                                (point-min) (point-max)) 1))
            (setq pending (gnosis-review--override-result pending (= (% i 2) 0)))
            (gnosis-display-next-review (gnosis-review--result-date pending) (= (% i 2) 0)))
          (should (equal before (gnosis-test-content--evidence)))
          ;; Erasing the presentation retires the old region, not its next text.
          (gnosis-display-keimenon question)
          (gnosis-display-next-review (gnosis-review--result-date pending) t)
          (should (string-search question (buffer-string))))))))

(ert-deftest gnosis-test-review-quality-long-actions-keep-pending-owner ()
  "Nonterminal actions do not consume one Lisp stack frame per prompt."
  (dolist (mode '(due practice))
    (gnosis-test-review-quality--with-basic mode
      (let* ((answer (gnosis-test-content--answer "basic"))
             (pending (cdr answer))
             (captured (copy-tree pending))
             (calls 0)
             (max-lisp-eval-depth 800)
             (before (gnosis-test-content--evidence))
             (choice '(?o ?o ?v ?d))
             fault)
        (cl-letf (((symbol-function 'read-char-choice)
                   (lambda (&rest _)
                     (cl-incf calls)
                     (if (<= calls 600) (nth (% (1- calls) 4) choice)
                       (signal 'quit nil))))
                  ((symbol-function 'y-or-n-p) (lambda (&rest _) nil))
                  ((symbol-function 'sleep-for) #'ignore))
          (setq fault (condition-case err
                          (gnosis-review-actions t 222 pending)
                        ((error quit) err))))
        (should (eq (car fault) 'quit))
        (should (= calls 601))
        (should (equal pending captured))
        (should (equal before (gnosis-test-content--evidence)))))))

(ert-deftest gnosis-test-review-quality-override-copy-and-defaults ()
  "Copy metadata identities but retain scheduled constructor backfill."
  (dolist (mode '(due practice))
    (gnosis-test-review-quality--with-basic mode
      (let* ((result (cdr (gnosis-test-content--answer "basic")))
             (extra (list 'opaque-owner))
             (original (append result (list :extension extra :model nil :image nil :edited-content nil)))
             (saved (copy-tree original))
             (next (gnosis-review--override-result original nil)))
        (dolist (key '(:event-id :reviewed-at-us :review-day :content :encounter :extension))
          (should (eq (plist-get original key) (plist-get next key))))
        (dolist (key '(:model :image :edited-content))
          (should (plist-member next key))
          (should-not (plist-get next key)))
        (should (equal saved original))
        (should (eq (plist-get next :outcome) 'failure))
        (when (eq mode 'due)
          (dolist (input '((:thema-id 222)
                           (:thema-id 222 :event-id nil :reviewed-at-us nil :review-day nil :preview nil)))
            (let ((filled (gnosis-review--override-result input t)))
              (dolist (key '(:event-id :reviewed-at-us :review-day :preview))
                (should (plist-get filled key)))
              (should-not (plist-member filled :image)))))
        (when (eq mode 'practice)
          (cl-letf (((symbol-function 'gnosis-scheduler-preview-review)
                     (lambda (&rest _) (ert-fail "Practice computed an FSRS preview"))))
            (gnosis-review--override-result next t)))))))

(ert-deftest gnosis-test-review-quality-mixed-terminal-actions ()
  "Flags, suspension and overrides retain one event through Next or quit."
  (dolist (mode '(due practice))
    (dolist (terminal '(?n ?q ?d))
      (gnosis-test-review-quality--with-basic mode
        (let* ((answer (gnosis-test-content--answer "basic"))
               (pending (cdr answer))
               (schedule (gnosis-select '* 'scheduler-state))
               (original (copy-tree pending))
               (seed-events (gnosis-select '* 'review-events '(= thema-id 333)))
               (choices (append '(?f ?s ?s ?o ?o) (list terminal)))
               (table (if (eq mode 'due) 'review-events 'practice-events)))
          (cl-letf (((symbol-function 'read-char-choice)
                     (lambda (&rest _)
                       (should choices)
                       (should-not (gnosis-select '* table '(= thema-id 222)))
                       (pop choices)))
                    ((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
            (let ((value (catch 'review-loop (gnosis-review-actions t 222 pending))))
              (when (= terminal ?d) (should (eq value :deleted)))))
          (should-not choices)
          (should (equal original pending))
          (should (equal seed-events (gnosis-select '* 'review-events '(= thema-id 333))))
          (if (= terminal ?d)
              (progn (should-not (gnosis-get 'id 'themata '(= id 222)))
                     (should-not (gnosis-select '* table '(= thema-id 222))))
            (should (= 1 (length (gnosis-select '* table '(= thema-id 222)))))
            (should (equal '("needs_work") (gnosis-get-tags-for-ids '(222))))
            (should-not (gnosis-suspended-p 222))
            (let ((once (gnosis-test-content--evidence)))
              (if (= terminal ?q)
                  ;; Legacy nonpersistent quit advances its owner; retry must refuse.
                  (should-error (gnosis-review-result 222 t pending)
                                :type 'gnosis-review-content-changed)
                (gnosis-review-result 222 t pending))
              (should (equal once (gnosis-test-content--evidence))))
            (when (eq mode 'practice)
              (should (equal schedule (gnosis-select '* 'scheduler-state))))))))))

(provide 'gnosis-test-review-quality)
;;; gnosis-test-review-quality.el ends here

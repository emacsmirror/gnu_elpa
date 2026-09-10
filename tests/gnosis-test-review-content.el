;;; gnosis-test-review-content.el --- Encounter content guards -*- lexical-binding: t; -*-

;;; Commentary:
;; Content drift must not become study evidence, in either review mode.

;;; Code:
(require 'ert)
(require 'gnosis-test-helpers)
(require 'gnosis-review)
(require 'gnosis-export-import)

(defun gnosis-test-content--add (kind)
  "Create a disposable question of KIND."
  (gnosis-add-thema-fields kind "The old answer"
                           (if (equal kind "cloze") '("hint") '("old" "new"))
                           '("old") "Explanation" nil 0 nil nil 222))

(defun gnosis-test-content--state (mode)
  "Retain a review state for MODE in the current buffer."
  (setq-local gnosis-review--state
              (gnosis-review-state-create
               :mode mode :persistent-p t :database gnosis-db
               :session-id (gnosis-scheduler-event-id)
               :event-id (gnosis-scheduler-event-id)
               :remaining '(222) :selected '(222) :initial 1 :total 1))
  (gnosis-review--save-session gnosis-review--state))

(defun gnosis-test-content--evidence ()
  "Read all scheduled/practice evidence and session projections."
  (mapcar (lambda (table)
            (gnosis-sqlite-select gnosis-db (concat "SELECT * FROM " table)))
          '("review_events" "practice_events" "scheduler_state"
            "study_session" "study_history")))

(defun gnosis-test-content--answer (kind &optional during-input)
  "Review KIND, calling DURING-INPUT inside the actual input boundary."
  (let ((gnosis-review-buffer-name (buffer-name)))
    (cl-letf (((symbol-function 'gnosis-completing-read)
             (lambda (&rest _) (when during-input (funcall during-input)) "old"))
            ((symbol-function 'gnosis--read-string-with-input-method)
             (lambda (&rest _) (when during-input (funcall during-input)) "old")))
      (funcall (intern (concat "gnosis-review-" kind)) 222))))

(ert-deftest gnosis-test-content-input-drift ()
  "Each response kind rejects drift before producing a pending grade."
  (dolist (kind '("mcq" "cloze" "mc-cloze"))
    (dolist (mode '(due practice))
      (gnosis-test-with-db
        (gnosis-test-content--add kind)
        (with-temp-buffer
          (gnosis-test-content--state mode)
          (let ((before (gnosis-test-content--evidence))
                (state (copy-tree (gnosis-review--state-data gnosis-review--state))))
            (should-error
             (gnosis-test-content--answer
              kind (lambda () (gnosis-update 'themata '(= answer '("new")) '(= id 222))))
             :type 'user-error)
            (should (equal before (gnosis-test-content--evidence)))
            (should (equal state (gnosis-review--state-data gnosis-review--state)))))))))

(ert-deftest gnosis-test-content-final-drift-and-override ()
  "Final acceptance, including overrides, rejects changed content atomically."
  (dolist (kind '("mcq" "cloze" "mc-cloze"))
    (dolist (mode '(due practice))
      (dolist (override '(nil t))
        (gnosis-test-with-db
          (gnosis-test-content--add kind)
          (with-temp-buffer
            (gnosis-test-content--state mode)
            (let* ((answer (gnosis-test-content--answer kind))
                   (success (if override (not (car answer)) (car answer)))
                   (result (if override (gnosis-review--override-result (cdr answer) success)
                             (cdr answer)))
                   (before (gnosis-test-content--evidence)))
              (gnosis-update 'themata '(= answer '("new")) '(= id 222))
              (should-error (gnosis-review-result 222 success result) :type 'user-error)
              (should (equal before (gnosis-test-content--evidence)))
              (should (equal '(222) (gnosis-review-state-remaining gnosis-review--state))))))))))

(ert-deftest gnosis-test-content-unchanged-retry ()
  "Unchanged answers accept once, including persistent retry after advancement."
  (dolist (kind '("mcq" "cloze" "mc-cloze"))
    (dolist (mode '(due practice))
      (gnosis-test-with-db
        (gnosis-test-content--add kind)
        (with-temp-buffer
          (gnosis-test-content--state mode)
          (let* ((answer (gnosis-test-content--answer kind))
                 (scheduled (gnosis-sqlite-select gnosis-db "SELECT * FROM scheduler_state")))
            (gnosis-review-result 222 (car answer) (cdr answer))
            (let ((once (gnosis-test-content--evidence)))
              (gnosis-review-result 222 (car answer) (cdr answer))
              (should (equal once (gnosis-test-content--evidence))))
            (should-not (gnosis-review-state-remaining gnosis-review--state))
            (when (eq mode 'practice)
              (should (equal scheduled (gnosis-sqlite-select gnosis-db "SELECT * FROM scheduler_state"))))))))))

(ert-deftest gnosis-test-content-edit-action ()
  "Edit/save invalidates old outcomes; cancelling an unchanged draft does not."
  (dolist (kind '("mcq" "cloze" "mc-cloze"))
    (dolist (mode '(due practice))
      (dolist (change '(nil t))
        (gnosis-test-with-db
          (gnosis-test-content--add kind)
          (with-temp-buffer
            (gnosis-test-content--state mode)
            (let* ((answer (gnosis-test-content--answer kind))
                   (before (gnosis-test-content--evidence))
                   (gnosis-save-hook nil)
                   (gnosis-review-editing-p nil)
                   (origin (current-buffer))
                   returned)
              (unwind-protect
                  (cl-letf (((symbol-function 'recursive-edit)
                             (lambda ()
                               (when change
                                 (goto-char (point-min))
                                 (search-forward "The old answer")
                                 (replace-match "The old revised question" t t))
                               (cl-letf (((symbol-function 'exit-recursive-edit) #'ignore))
                                 (call-interactively
                                  (key-binding (kbd (if change "C-c C-c" "C-c C-k")))))
                               (set-buffer origin)))
                            ((symbol-function 'gnosis-review-actions)
                             (lambda (&rest _) (setq returned t))))
                    (if change
                        (should-error
                         (gnosis-review-action--edit (car answer) 222 (cdr answer))
                         :type 'user-error)
                      (gnosis-review-action--edit (car answer) 222 (cdr answer))
                      (should returned))
                    (should (equal (gnosis-get 'keimenon 'themata '(= id 222))
                                   (if change "The old revised question" "The old answer")))
                    (should-not (and change returned))
                    (should (equal before (gnosis-test-content--evidence))))
                (when (get-buffer "*Gnosis Edit*")
                  (kill-buffer "*Gnosis Edit*"))))))))))

(ert-deftest gnosis-test-content-presentation-drift ()
  "Question, choices and explanation are part of the pending content."
  (dolist (kind '("mcq" "cloze" "mc-cloze" "basic"))
    (dolist (mutation '((themata (= keimenon "Changed question"))
                        (themata (= hypothesis '("Changed hint")))
                        (extras (= parathema "Changed explanation"))
                        (extras (= review-image "Changed image"))))
      (gnosis-test-with-db
        (gnosis-test-content--add kind)
        (with-temp-buffer
          (gnosis-test-content--state 'due)
          (let ((answer (gnosis-test-content--answer kind))
                (before (gnosis-test-content--evidence)))
            (gnosis-update (car mutation) (cadr mutation) '(= id 222))
            (should-error (gnosis-review-result 222 (car answer) (cdr answer))
                          :type 'user-error)
            (should (equal before (gnosis-test-content--evidence)))))))))

(ert-deftest gnosis-test-content-encounter-drift ()
  "Input cannot outlive its buffer-local encounter, even in the same database."
  (dolist (kind '("mcq" "cloze" "mc-cloze"))
    (dolist (mode '(due practice))
      (gnosis-test-with-db
        (gnosis-test-content--add kind)
        (with-temp-buffer
          (gnosis-test-content--state mode)
          (let ((before (gnosis-test-content--evidence)))
            (should-error
             (gnosis-test-content--answer
              kind (lambda ()
                     (setq gnosis-review--state
                           (copy-gnosis-review-state gnosis-review--state))))
             :type 'user-error)
            (should (equal before (gnosis-test-content--evidence)))))))))

(ert-deftest gnosis-test-content-cancel-and-restart ()
  "Cancelled input leaves the queue intact and a fresh answer can settle it."
  (dolist (kind '("mcq" "cloze" "mc-cloze"))
    (dolist (mode '(due practice))
      (gnosis-test-with-db
        (gnosis-test-content--add kind)
        (with-temp-buffer
          (gnosis-test-content--state mode)
          (let ((before (gnosis-test-content--evidence)) cancelled)
            (condition-case nil
                (gnosis-test-content--answer kind (lambda () (signal 'quit nil)))
              (quit (setq cancelled t)))
            (should cancelled)
            (should (equal before (gnosis-test-content--evidence)))
            (let ((answer (gnosis-test-content--answer kind)))
              (gnosis-review-result 222 (car answer) (cdr answer)))
            (should-not (gnosis-review-state-remaining gnosis-review--state))))))))

(provide 'gnosis-test-review-content)
;;; gnosis-test-review-content.el ends here

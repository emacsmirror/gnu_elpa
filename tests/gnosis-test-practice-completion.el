;;; gnosis-test-practice-completion.el --- Practice handoff -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;;; Commentary:
;; Completion notifications and explicit scheduled continuation on disposable data.

;;; Code:

(require 'ert)
(require 'gnosis-agent)
(require 'gnosis-test-helpers)

(defvar gnosis-practice-completed-hook)

(defmacro gnosis-test-completion (&rest body)
  "Run BODY with isolated study data and native summary buffers."
  (declare (indent 0) (debug t))
  `(gnosis-test-with-db
     (let ((buffers (buffer-list))
           (gnosis-review-buffer-name "*Gnosis Completion Test*")
           (gnosis-review--running nil)
           (gnosis-practice-completed-hook nil)
           (gnosis-new-themata-limit nil)
           (gnosis-center-content nil)
           (gnosis-monkeytype-enable nil)
           (register-alist nil))
       (save-window-excursion
         (unwind-protect
             (progn
               (gnosis-test--add-basic-thema "A" "a" nil nil 101)
               (gnosis-test--add-basic-thema "B" "b" nil nil 102)
               ,@body)
           (dolist (buffer (seq-difference (buffer-list) buffers))
             (when (buffer-live-p buffer) (kill-buffer buffer))))))))

(defun gnosis-test-completion-answer (id state)
  "Accept one disposable successful answer for ID in STATE."
  (let ((gnosis-review--state state))
    (gnosis-review-result id t (gnosis-review-algorithm id t))))

(defun gnosis-test-completion-run (ids)
  "Complete practice IDS through the shared runner with deterministic answers."
  (cl-letf (((symbol-function 'gnosis-review-process-thema)
             #'gnosis-test-completion-answer))
    (gnosis-review-loop ids 'practice)))

(ert-deftest gnosis-completion-notifies-after-commit-not-on-summary-reopen ()
  (gnosis-test-completion
    (let* (events
           (gnosis-practice-completed-hook
            (list (lambda (event)
                    (should-not gnosis-review--running)
                    (should-not (gnosis-review-state-remaining (gnosis-review--read-session)))
                    (let* ((gnosis-db (plist-get event :connection))
                           (result (gnosis-agent-results (plist-get event :session-id))))
                      (should (equal "completed" (plist-get result :status)))
                      (should-not (plist-get result :policy))
                      (should-not (plist-get result :targets))
                      (should (equal "completed" (plist-get (aref (plist-get result :items) 0) :reason)))
                      (should (= 1 (length (gnosis-study-practice-events
                                           (plist-get event :session-id))))))
                    (push event events))))
           (before (gnosis-select '* 'scheduler-state))
           (state (gnosis-test-completion-run '(101))))
      (should (= 1 (length events)))
      (should (equal 1 (plist-get (car events) :api-version)))
      (should (equal "practice" (plist-get (car events) :mode)))
      (should (equal gnosis-test--db-file (plist-get (car events) :database)))
      (should (eq gnosis-db (plist-get (car events) :connection)))
      (should (equal (gnosis-review-state-session-id state)
                     (plist-get (car events) :session-id)))
      (should (equal before (gnosis-select '* 'scheduler-state)))
      (should-not (gnosis-select '* 'review-events))
      (gnosis-review--show-summary state)
      (should (= 1 (length events))))))

(ert-deftest gnosis-completion-summary-scheduled-action-is-separate ()
  (gnosis-test-completion
    (let* ((practice (gnosis-test-completion-run '(101)))
           (id (gnosis-review-state-session-id practice))
           (history (gnosis-get 'data 'study-history `(= session-id ,id)))
           (events (gnosis-study-practice-events id))
           prompt scheduled)
      (should (eq #'gnosis-review-summary-scheduled (key-binding (kbd "s"))))
      (cl-letf (((symbol-function 'y-or-n-p)
                 (lambda (text) (setq prompt text) t))
                ((symbol-function 'gnosis-review--run-state)
                 (lambda (_buffer state) (setq scheduled state))))
        (call-interactively (key-binding (kbd "s"))))
      (should (string-search "New answers affect scheduling" prompt))
      (should (eq 'due (gnosis-review-state-mode scheduled)))
      (should-not (equal id (gnosis-review-state-session-id scheduled)))
      (should (equal '(101) (gnosis-review-state-selected scheduled)))
      (should (equal history (gnosis-get 'data 'study-history `(= session-id ,id))))
      (should (equal events (gnosis-study-practice-events id)))
      (should-error (call-interactively (key-binding (kbd "s"))) :type 'user-error))))

(ert-deftest gnosis-completion-interrupt-resume-shares-native-and-agent-path ()
  (dolist (entry '(native agent))
    (gnosis-test-completion
      (let* ((gnosis-agent--launches nil)
             events
             (gnosis-practice-completed-hook (list (lambda (event) (push event events)))))
        (cl-letf (((symbol-function 'run-with-timer) (lambda (&rest _) nil)))
          (if (eq entry 'native)
              (cl-letf (((symbol-function 'gnosis-review-process-thema)
                         (lambda (id state)
                           (gnosis-test-completion-answer id state)
                           (throw 'review-loop nil))))
                (gnosis-review-loop '(101 102) 'practice))
            (gnosis-agent-start-practice :thema-ids '(101 102) :limit 2)
            (cl-letf (((symbol-function 'gnosis-review-process-thema)
                       (lambda (id state)
                         (gnosis-test-completion-answer id state)
                         (throw 'review-loop nil))))
              (gnosis-agent--launch (car gnosis-agent--launches))))
          (should-not events)
          (let ((id (gnosis-review-state-session-id (gnosis-review--read-session))))
            (should (equal "unfinished" (plist-get (gnosis-agent-results id) :status)))
            (cl-letf (((symbol-function 'gnosis-review-process-thema)
                       #'gnosis-test-completion-answer))
              (if (eq entry 'native)
                  (gnosis-review-resume)
                (gnosis-agent-resume id)
                (gnosis-agent--launch (car gnosis-agent--launches))))
            (should (= 1 (length events)))
            (should (equal id (plist-get (car events) :session-id)))
            (should (= 2 (length (gnosis-study-practice-events id))))))))))

(ert-deftest gnosis-completion-empty-cancelled-unfinished-and-scheduled-do-not-notify ()
  (gnosis-test-completion
    (let* (events
           (gnosis-practice-completed-hook (list (lambda (event) (push event events)))))
      (gnosis-review-loop nil 'practice)
      (cl-letf (((symbol-function 'gnosis-review-process-thema)
                 (lambda (&rest _) (signal 'quit nil))))
        (condition-case nil (gnosis-review-loop '(101) 'practice) (quit nil)))
      (should-not events)
      (let ((id (gnosis-review-state-session-id (gnosis-review--read-session))))
        (gnosis-agent-cancel id)
        (should (equal "cancelled" (plist-get (gnosis-agent-status id) :status))))
      (cl-letf (((symbol-function 'gnosis-review-process-thema)
                 #'gnosis-test-completion-answer))
        (gnosis-review-loop '(102) 'due))
      (should-not events))))

(ert-deftest gnosis-completion-subscriber-errors-quits-and-payload-mutation-isolated ()
  (gnosis-test-completion
    (let* (events
           (gnosis-practice-completed-hook
            (list (lambda (event)
                    (aset (plist-get event :session-id) 0 ?X)
                    (setcar event :changed)
                    (error "Subscriber failed"))
                  (lambda (_) (signal 'quit nil))
                  (lambda (event) (push event events))))
           (state (gnosis-test-completion-run '(101))))
      (should (= 1 (length events)))
      (should (equal (gnosis-review-state-session-id state)
                     (plist-get (car events) :session-id)))
      (should (derived-mode-p 'gnosis-review-summary-mode))
      (should (string-search "Practice — schedule unchanged" (buffer-string)))
      (should (= 1 (length (gnosis-study-practice-events
                           (gnosis-review-state-session-id state))))))))

(ert-deftest gnosis-completion-reentrant-subscriber-leaves-successor-summary ()
  (gnosis-test-completion
    (let* (successor summary
           (gnosis-practice-completed-hook
            (list (lambda (_)
                    (setq successor (gnosis-review-loop '(102) 'due)
                          summary (window-buffer (selected-window))))))
           (origin (gnosis-test-completion-run '(101))))
      (should (eq summary (window-buffer (selected-window))))
      (with-current-buffer summary
        (should (string-search "Review — FSRS accepted" (buffer-string)))
        (should (equal (gnosis-review-state-session-id successor)
                       (plist-get (cdr gnosis-review--summary-target) :session-id))))
      (should (equal (gnosis-review--state-data successor)
                     (gnosis-review--state-data (gnosis-review--read-session))))
      (should (= 1 (length (gnosis-study-practice-events
                           (gnosis-review-state-session-id origin)))))
      (should (= 1 (length (gnosis-select '* 'review-events)))))))

(ert-deftest gnosis-completion-callback-database-replacement-keeps-origin-queryable ()
  (gnosis-test-completion
    (let* ((origin gnosis-db)
           (other-file (expand-file-name "other.db" gnosis-dir))
           (other (gnosis-sqlite-open other-file))
           result event)
      (unwind-protect
          (progn
            (let ((gnosis-db other)) (gnosis-db-init))
            (let ((gnosis-practice-completed-hook
                   (list (lambda (_) (setq gnosis-db other))
                         (lambda (payload)
                           (setq event payload)
                           (let ((gnosis-db (plist-get payload :connection)))
                             (setq result (gnosis-agent-results
                                           (plist-get payload :session-id))))))))
              (gnosis-test-completion-run '(101)))
            (should (eq gnosis-db other))
            (should (eq origin (plist-get event :connection)))
            (should (equal "completed" (plist-get result :status)))
            (should (= 1 (plist-get (plist-get result :summary) :attempts)))
            (should-not (gnosis-select '* 'study-session))
            (should-error (gnosis-review-summary-scheduled) :type 'user-error))
        (setq gnosis-db origin)
        (gnosis-sqlite-close other)))))

(ert-deftest gnosis-completion-scheduled-current-content-and-explicit-exclusions ()
  (gnosis-test-completion
    (gnosis-test--add-basic-thema "C" "c" nil nil 103)
    (gnosis-test--add-basic-thema "D" "d" nil nil 104)
    (let* ((practice (gnosis-test-completion-run '(101 102 103 104)))
           (id (gnosis-review-state-session-id practice))
           (history (gnosis-get 'data 'study-history `(= session-id ,id)))
           (gnosis-new-themata-limit 0)
           prompt)
      (gnosis-update-thema 101 "Repaired A" '("") '("fixed") "" '("test") nil)
      (gnosis-delete-themata '(102))
      (gnosis-update 'scheduler-state '(= suspended 1) '(= thema-id 103))
      (gnosis-update 'scheduler-state `(= due-day ,(gnosis--date-to-int (gnosis-date 3))) '(= thema-id 104))
      (let ((events (gnosis-study-practice-events id))
            (other (gnosis-select '* 'scheduler-state '(in thema-id [103 104]))))
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (text) (setq prompt text) t))
                  ((symbol-function 'gnosis-review-process-thema)
                   (lambda (thema state)
                     (should (equal "Repaired A" (gnosis-get 'keimenon 'themata `(= id ,thema))))
                     (should (equal '("fixed") (gnosis-get 'answer 'themata `(= id ,thema))))
                     (gnosis-test-completion-answer thema state))))
          (gnosis-review-summary-scheduled))
        (should (string-search "Due: 1" prompt))
        (should (string-search "1 deleted, 1 suspended, 0 ineligible, 1 not yet due" prompt))
        (should (string-search "New cards are not capped" prompt))
        (should (equal history (gnosis-get 'data 'study-history `(= session-id ,id))))
        (should (equal events (gnosis-study-practice-events id)))
        (should (equal other (gnosis-select '* 'scheduler-state '(in thema-id [103 104]))))
        (should (= 1 (length (gnosis-select '* 'review-events))))))))

(ert-deftest gnosis-completion-scheduled-cancel-quit-and-setup-error-preserve-checkpoint ()
  (gnosis-test-completion
    (gnosis-test-completion-run '(101))
    (let ((target (gnosis-review--session-target))
          (history (gnosis-select '* 'study-history))
          (events (gnosis-select '* 'practice-events)))
      (dolist (failure '(decline quit error setup))
        (cl-letf (((symbol-function 'y-or-n-p)
                   (lambda (_)
                     (pcase failure
                       ('decline nil) ('quit (signal 'quit nil))
                       ('error (error "Prompt failed")) (_ t))))
                  ((symbol-function 'gnosis-review--setup-buffer)
                   (lambda (&rest _) (error "Setup failed"))))
          (condition-case nil (gnosis-review-summary-scheduled) ((error quit) nil)))
        (should (equal target (gnosis-review--session-target)))
        (should (equal history (gnosis-select '* 'study-history)))
        (should (equal events (gnosis-select '* 'practice-events)))
        (should-not (gnosis-select '* 'review-events))))))

(ert-deftest gnosis-completion-scheduled-empty-selection-preserves-checkpoint ()
  (gnosis-test-completion
    (gnosis-test-completion-run '(101))
    (gnosis-update 'scheduler-state `(= due-day ,(gnosis--date-to-int (gnosis-date 3))) '(= thema-id 101))
    (let ((target (gnosis-review--session-target)) text)
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) (ert-fail "Unexpected prompt")))
                ((symbol-function 'message) (lambda (format &rest args)
                                              (setq text (apply #'format format args)))))
        (gnosis-review-summary-scheduled))
      (should (string-search "1 not yet due" text))
      (should (equal target (gnosis-review--session-target))))))

(ert-deftest gnosis-completion-scheduled-refuses-retired-or-changed-owner ()
  (dolist (change '(file checkpoint eligibility))
    (gnosis-test-completion
      (gnosis-test-completion-run '(101))
      (let ((before (gnosis-review--session-target)))
        (cl-letf (((symbol-function 'y-or-n-p)
                   (lambda (_)
                     (pcase change
                       ('file (set-visited-file-name (expand-file-name "draft" gnosis-dir))
                              (set-visited-file-name nil))
                       ('checkpoint (gnosis-review--reserve-practice '(102) nil nil))
                       ('eligibility (gnosis-update 'scheduler-state '(= suspended 1)
                                                      '(= thema-id 101))))
                     t)))
          (should-error (gnosis-review-summary-scheduled) :type 'user-error))
        (unless (eq change 'checkpoint)
          (should (equal before (gnosis-review--session-target))))
        (should-not (gnosis-select '* 'review-events))))))

(ert-deftest gnosis-completion-rollback-does-not-notify ()
  (gnosis-test-completion
    (let* (events
           (gnosis-practice-completed-hook (list (lambda (event) (push event events))))
           (save (symbol-function 'gnosis-review--save-session)))
      (cl-letf (((symbol-function 'gnosis-review--save-session)
                 (lambda (state)
                   (funcall save state)
                   (unless (gnosis-review-state-remaining state)
                     (error "Checkpoint write failed")))))
        (should-error (gnosis-test-completion-run '(101))))
      (should-not events)
      (should (equal '(101) (gnosis-review-state-remaining (gnosis-review--read-session))))
      (should-not (gnosis-select '* 'practice-events))
      (should-not (gnosis-select '* 'practice-encounters)))))

(ert-deftest gnosis-completion-reopen-results-does-not-replay-notification ()
  (gnosis-test-completion
    (let* (events
           (gnosis-practice-completed-hook (list (lambda (event) (push event events))))
           (state (gnosis-test-completion-run '(101)))
           (id (gnosis-review-state-session-id state)))
      (gnosis-sqlite-close gnosis-db)
      (setq gnosis-db (gnosis-sqlite-open gnosis-test--db-file))
      (gnosis-db-init)
      (should (equal "completed" (plist-get (gnosis-agent-results id) :status)))
      (gnosis-review--show-summary (gnosis-review--read-session))
      (should (= 1 (length events))))))

(ert-deftest gnosis-completion-undo-and-recompletion-keeps-dedup-identity ()
  (gnosis-test-completion
    (let* (events
           (gnosis-practice-completed-hook (list (lambda (event) (push event events))))
           (state (gnosis-test-completion-run '(101)))
           (id (gnosis-review-state-session-id state)))
      (gnosis-review-undo)
      (cl-letf (((symbol-function 'gnosis-review-process-thema)
                 #'gnosis-test-completion-answer))
        (with-current-buffer (window-buffer (selected-window))
          (gnosis-review-resume)))
      (should (= 2 (length events)))
      (should (equal (car events) (cadr events)))
      (let ((result (gnosis-agent-results id)))
        (should (equal "completed" (plist-get result :status)))
        (should (= 1 (plist-get (plist-get result :summary) :attempts)))
        (should (= 2 (plist-get (aref (plist-get result :items) 0) :total-attempts)))))))

(ert-deftest gnosis-completion-button-retains-original-summary-owner ()
  (gnosis-test-completion
    (gnosis-test-completion-run '(101))
    (let* ((summary (current-buffer))
           (button (next-button (point-min)))
           (action (button-get button 'action)))
      (should button)
      (gnosis-test-completion-run '(102))
      (let ((target (gnosis-review--session-target)))
        (should-error (funcall action button) :type 'user-error)
        (should (equal target (gnosis-review--session-target)))
        (with-current-buffer summary
          (set-visited-file-name (expand-file-name "foreign" gnosis-dir))
          (set-visited-file-name nil))
        (should-error (funcall action button) :type 'user-error)
        (should (equal target (gnosis-review--session-target)))))))

(provide 'gnosis-test-practice-completion)
;;; gnosis-test-practice-completion.el ends here

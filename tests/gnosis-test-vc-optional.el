;;; gnosis-test-vc-optional.el --- Optional Git boundaries -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;;; Commentary:
;; Exercise production study completion without the optional Git executable.

;;; Code:

(require 'ert)
(require 'gnosis-test-helpers)
(require 'gnosis-review)
(require 'gnosis-anki)
(require 'gnosis-export-import)

(defun gnosis-test-vc--journey (mode &optional quit-after-first)
  "Complete a native batch in MODE; optionally QUIT-AFTER-FIRST acceptance."
  (let* ((a (gnosis-test--add-basic-thema "Question A" "Answer"))
         (b (and quit-after-first
                 (gnosis-test--add-basic-thema "Question B" "Answer")))
         (gnosis-testing nil)
         (gnosis-review-basic-input 'typed)
         (before (gnosis-select '* 'scheduler-state))
         (table (if (eq mode 'due) 'review-events 'practice-events))
         (other (if (eq mode 'due) 'practice-events 'review-events))
         (buffers (buffer-list)))
    (unwind-protect
        (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "Answer"))
                  ((symbol-function 'read-char-choice)
                   (lambda (&rest _) (if quit-after-first ?q ?n))))
          (let ((state (gnosis-review-loop (if b (list a b) (list a)) mode)))
            (should (= (gnosis-review-state-reviewed state) 1))
            (should (equal (gnosis-review-state-remaining state) (and b (list b))))
            (should (= (length (gnosis-select '* table)) 1))
            (should-not (gnosis-select '* other))
            (when (eq mode 'practice)
              (should (equal before (gnosis-select '* 'scheduler-state))))
            (should (derived-mode-p 'gnosis-review-summary-mode))
            (should (string-match-p "Accepted attempts: 1" (buffer-string)))
            (should (string-match-p (if (eq mode 'due)
                                       "Review — FSRS accepted"
                                     "Practice — schedule unchanged")
                                   (buffer-string)))
            (let ((events (gnosis-select '* table)))
              ;; Refresh does not accept the same answer again.
              (gnosis-review--show-summary (gnosis-review--read-session))
              (should (equal events (gnosis-select '* table))))
            state))
      (dolist (buffer (seq-difference (buffer-list) buffers))
        (kill-buffer buffer)))))

(ert-deftest gnosis-test-vc-optional-due-without-git ()
  (gnosis-test-with-db
    (let ((exec-path nil) (gnosis-vc-auto-push t))
      (gnosis-test-vc--journey 'due)
      (should-not (file-exists-p (expand-file-name ".git" gnosis-dir))))))

(ert-deftest gnosis-test-vc-optional-practice-without-git ()
  (gnosis-test-with-db
    (let ((exec-path nil))
      (gnosis-test-vc--journey 'practice)
      (should-not (file-exists-p (expand-file-name ".git" gnosis-dir))))))

(ert-deftest gnosis-test-vc-optional-quit-after-acceptance ()
  (dolist (mode '(due practice))
    (gnosis-test-with-db
      (let ((exec-path nil))
        (gnosis-test-vc--journey mode t)
        (should-not (file-exists-p (expand-file-name ".git" gnosis-dir)))))))

(ert-deftest gnosis-test-vc-optional-answer-cancel ()
  (dolist (mode '(due practice))
    (gnosis-test-with-db
      (let ((id (gnosis-test--add-basic-thema "Question" "Answer"))
            (gnosis-testing nil)
            (exec-path nil)
            (buffers (buffer-list)))
        (unwind-protect
            (cl-letf (((symbol-function 'read-string)
                       (lambda (&rest _) (signal 'quit nil))))
              (should (eq 'cancelled
                          (condition-case nil
                              (gnosis-review-loop (list id) mode)
                            (quit 'cancelled))))
              (should-not (gnosis-select '* 'review-events))
              (should-not (gnosis-select '* 'practice-events))
              (should (derived-mode-p 'gnosis-review-summary-mode))
              (should (string-match-p "Accepted attempts: 0" (buffer-string)))
              (should-not (file-exists-p (expand-file-name ".git" gnosis-dir))))
          (dolist (buffer (seq-difference (buffer-list) buffers))
            (kill-buffer buffer)))))))

(ert-deftest gnosis-test-vc-optional-callers-without-git ()
  (gnosis-test-with-db
    (let ((gnosis-testing nil) (exec-path nil))
      (gnosis--commit-bulk-link 1 "source")
      (gnosis--commit-link-cleanup 1 2 3 4)
      (gnosis-anki--commit-import 1 "source.apkg")
      (gnosis-import--commit 1 2 "source.db")
      (should-not (file-exists-p (expand-file-name ".git" gnosis-dir)))
      ;; SQLite import also remains optional in an existing repository.
      (make-directory (expand-file-name ".git" gnosis-dir))
      (gnosis-import--commit 1 2 "source.db")
      (should-error (gnosis-vc-push) :type 'error))))

(ert-deftest gnosis-test-vc-optional-setup-and-launch-failures ()
  (dolist (boundary '(gnosis--ensure-git-repo start-process))
    (dolist (mode '(due practice))
      (gnosis-test-with-db
        (let (messages)
          (cl-letf (((symbol-function boundary)
                     (lambda (&rest _) (error "Injected Git failure")))
                    ((symbol-function 'message)
                     (lambda (format &rest args)
                       (push (apply #'format format args) messages))))
            (gnosis-test-vc--journey mode))
          (should (seq-some
                   (lambda (text)
                     (string-match-p "Automatic Git commit failed: Injected Git failure"
                                     text))
                   messages)))))))

(ert-deftest gnosis-test-vc-optional-does-not-hide-quit ()
  (gnosis-test-with-db
    (let ((gnosis-testing nil))
      (cl-letf (((symbol-function 'gnosis--ensure-git-repo)
                 (lambda () (signal 'quit nil))))
        (should (eq 'cancelled
                    (condition-case nil (gnosis-review-commit 1)
                      (quit 'cancelled))))))))

(defun gnosis-test-vc-optional--wait ()
  "Wait at most five seconds for this test's Git chain to settle."
  (let ((deadline (+ (float-time) 5)))
    (while (and (seq-some (lambda (proc)
                           (string-prefix-p "gnosis-git" (process-name proc)))
                         (process-list))
                (< (float-time) deadline))
      (accept-process-output nil 0.05))
    (should-not (seq-some (lambda (proc)
                           (string-prefix-p "gnosis-git" (process-name proc)))
                         (process-list)))))

(ert-deftest gnosis-test-vc-optional-real-commit-and-push-failure ()
  (skip-unless (executable-find "git"))
  (gnosis-test-with-db
    (let ((process-environment (copy-sequence process-environment))
          (gnosis-vc-auto-push t)
          (pushes 0)
          messages)
      (setenv "GIT_AUTHOR_NAME" "Gnosis Test")
      (setenv "GIT_AUTHOR_EMAIL" "test@example.invalid")
      (setenv "GIT_COMMITTER_NAME" "Gnosis Test")
      (setenv "GIT_COMMITTER_EMAIL" "test@example.invalid")
      (setenv "GIT_CONFIG_NOSYSTEM" "1")
      (cl-letf (((symbol-function 'gnosis-vc-push)
                 (lambda () (cl-incf pushes) (error "Injected push failure")))
                ((symbol-function 'message)
                 (lambda (format &rest args)
                   (push (apply #'format format args) messages))))
        (gnosis-test-vc--journey 'due)
        (gnosis-test-vc-optional--wait))
      (should (= pushes 1))
      (should (seq-some
               (lambda (text)
                 (string-match-p "Automatic Git push failed: Injected push failure" text))
               messages))
      (let ((default-directory gnosis-dir))
        (should (equal "Total themata reviewed: 1"
                       (string-trim
                        (with-temp-buffer
                          (should (zerop (call-process "git" nil t nil "log" "-1" "--format=%s")))
                          (buffer-string))))))
      (should (= 1 (length (gnosis-select '* 'review-events)))))))

(ert-deftest gnosis-test-vc-optional-nonzero-commit-does-not-push ()
  (skip-unless (executable-find "git"))
  (gnosis-test-with-db
    (let ((gnosis-testing nil)
          (gnosis-vc-auto-push t)
          (pushes 0)
          messages)
      (gnosis--ensure-git-repo)
      ;; A real rejecting hook gives a deterministic commit failure,
      ;; independent of the developer's Git identity configuration.
      (let ((hook (expand-file-name ".git/hooks/pre-commit" gnosis-dir)))
        (with-temp-file hook (insert "#!/bin/sh\nexit 1\n"))
        (set-file-modes hook #o700))
      (cl-letf (((symbol-function 'gnosis-vc-push) (lambda () (cl-incf pushes)))
                ((symbol-function 'message)
                 (lambda (format &rest args)
                   (push (apply #'format format args) messages))))
        (gnosis-test-vc--journey 'practice)
        (gnosis-test-vc-optional--wait))
      (should (zerop pushes))
      (should (seq-some (lambda (text) (string-match-p "git commit failed" text))
                        messages))
      (should (= 1 (length (gnosis-select '* 'practice-events)))))))

(ert-deftest gnosis-test-vc-optional-existing-only-and-testing ()
  (gnosis-test-with-db
    (let ((gnosis-testing nil))
      (gnosis-import--commit 1 2 "source.db")
      (should-not (file-exists-p (expand-file-name ".git" gnosis-dir))))
    (gnosis-review-commit 1)
    (should-not (file-exists-p (expand-file-name ".git" gnosis-dir)))))

(provide 'gnosis-test-vc-optional)
;;; gnosis-test-vc-optional.el ends here

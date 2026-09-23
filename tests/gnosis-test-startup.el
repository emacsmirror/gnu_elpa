;;; gnosis-test-startup.el --- First-open regressions -*- lexical-binding: t; -*-

(require 'ert)
(require 'gnosis-test-db-safety)
(require 'gnosis-fixture-schema-v9)
(require 'gnosis-dashboard)

(defun gnosis-test-startup--v9 ()
  "Create a closed released database with content, schedules and history."
  (let ((gnosis-db (gnosis-sqlite-open
                    (expand-file-name "gnosis.db" gnosis-dir))))
    (unwind-protect
        (progn
          (gnosis-fixture-create-v9)
          (gnosis-fixture-add-basic 101 "Retained question")
          (gnosis-scheduler-accept-review (make-string 64 ?a) 101 'success
                                          1000000 20260913)
          (gnosis--insert-into 'practice-events
                               '(["legacy" 101 "batch" 1 2000000 1]))
          (gnosis--insert-into 'study-history
                               '(["batch" (:mode practice :completed-p t)]))
          (gnosis-db--check-schema gnosis-db 9))
      (sqlite-close gnosis-db))))

(defun gnosis-test-startup--check-upgrade (before)
  "Check the published upgrade and retained rows from snapshot BEFORE."
  (should gnosis-db)
  (should (= 11 (gnosis--db-version)))
  (gnosis-db--check-schema gnosis-db 11)
  (should-not (gnosis-select '* 'practice-encounters))
  (should (equal (nth 2 before)
                 (assoc-delete-all
                  "practice_encounters"
                  (gnosis-test-safety-without-rubric
                   (nth 2 (gnosis-test-safety-snapshot
                           (expand-file-name "gnosis.db" gnosis-dir))))))))

(ert-deftest gnosis-startup-migration-without-git ()
  "Publish a valid upgrade on its first attempt even with retained Git metadata."
  (gnosis-test-safety
    (gnosis-test-startup--v9)
    (make-directory (expand-file-name ".git" gnosis-dir))
    (let ((before (gnosis-test-safety-snapshot
                   (expand-file-name "gnosis.db" gnosis-dir)))
          (exec-path nil)
          (gnosis-testing nil))
      (gnosis--ensure-db)
      (gnosis-test-startup--check-upgrade before)
      (sqlite-close gnosis-db)
      (setq gnosis-db nil)
      (gnosis--ensure-db)
      (gnosis-test-startup--check-upgrade before))))

(ert-deftest gnosis-startup-migration-git-launch-failure ()
  "Report a Git launch error without failing or undoing a validated upgrade."
  (gnosis-test-safety
    (gnosis-test-startup--v9)
    (make-directory (expand-file-name ".git" gnosis-dir))
    (let ((before (gnosis-test-safety-snapshot
                   (expand-file-name "gnosis.db" gnosis-dir)))
          (gnosis-testing nil)
          attempted messages)
      (cl-letf (((symbol-function 'executable-find) (lambda (_) "/fixture/git"))
                ((symbol-function 'gnosis--git-cmd)
                 (lambda (&rest _)
                   (setq attempted t)
                   (signal 'file-error '("Git launch fault"))))
                ((symbol-function 'message)
                 (lambda (format-string &rest args)
                   (push (apply #'format format-string args) messages))))
        (gnosis--ensure-db))
      (should attempted)
      (should (seq-some (lambda (text)
                         (string-match-p "Automatic Git commit failed" text))
                       messages))
      (gnosis-test-startup--check-upgrade before))))

(ert-deftest gnosis-startup-migration-git-is-deferred-and-local ()
  "Publish before Git completes; never initialize or push migration repositories."
  (dolist (completion '(success failure launch-error))
    (gnosis-test-safety
      (gnosis-test-startup--v9)
      (make-directory (expand-file-name ".git" gnosis-dir))
      (let ((before (gnosis-test-safety-snapshot
                     (expand-file-name "gnosis.db" gnosis-dir)))
            (gnosis-testing nil)
            (gnosis-vc-auto-push t)
            commands callback pushed)
        (cl-letf (((symbol-function 'executable-find) (lambda (_) "/fixture/git"))
                  ((symbol-function 'gnosis--ensure-git-repo)
                   (lambda () (ert-fail "Migration initialized a repository")))
                  ((symbol-function 'gnosis-vc-push)
                   (lambda () (setq pushed t)))
                  ((symbol-function 'gnosis--git-cmd)
                   (lambda (args &optional sentinel)
                     (push args commands)
                     (when (and (eq completion 'launch-error)
                                (equal (car args) "commit"))
                       (error "Deferred Git launch fault"))
                     (setq callback sentinel))))
          (gnosis--ensure-db)
          ;; No command has completed: even a blocked hook cannot delay open.
          (should (equal commands '(("add" "--" "gnosis.db"))))
          (gnosis-test-startup--check-upgrade before)
          (funcall callback nil
                   (if (eq completion 'failure) "exited abnormally\n" "finished\n"))
          (if (eq completion 'failure)
              (should (= 1 (length commands)))
            (should (equal (car commands)
                           '("commit" "--only" "-m" "Migrate database v9 -> v11"
                             "--" "gnosis.db")))
            (when (eq completion 'success)
              (funcall callback nil "finished\n")))
          (should-not pushed)
          (gnosis-test-startup--check-upgrade before))))))

(ert-deftest gnosis-startup-migration-git-quit-is-not-swallowed ()
  "A quit during Git launch closes the unpublished handle, not the committed data."
  (gnosis-test-safety
    (gnosis-test-startup--v9)
    (make-directory (expand-file-name ".git" gnosis-dir))
    (let ((before (gnosis-test-safety-snapshot
                   (expand-file-name "gnosis.db" gnosis-dir)))
          (open (symbol-function 'gnosis-sqlite-open))
          (gnosis-testing nil)
          candidate caught)
      (cl-letf (((symbol-function 'executable-find) (lambda (_) "/fixture/git"))
                ((symbol-function 'gnosis--git-cmd)
                 (lambda (&rest _) (signal 'quit nil)))
                ((symbol-function 'gnosis-sqlite-open)
                 (lambda (file) (setq candidate (funcall open file)))))
        (condition-case nil (gnosis--ensure-db)
          (quit (setq caught t))))
      (should caught)
      (should-not gnosis-db)
      (should-error (sqlite-select candidate "SELECT 1"))
      ;; Migration already committed before the optional Git action.
      (gnosis--ensure-db)
      (gnosis-test-startup--check-upgrade before))))

(ert-deftest gnosis-startup-migration-validation-error-and-quit ()
  "True initialization failures roll back and never publish or invoke Git."
  (dolist (fault '(error quit))
    (gnosis-test-safety
      (gnosis-test-startup--v9)
      (make-directory (expand-file-name ".git" gnosis-dir))
      (let ((before (gnosis-test-safety-snapshot
                     (expand-file-name "gnosis.db" gnosis-dir)))
            (check (symbol-function 'gnosis-db--check-schema))
            (open (symbol-function 'gnosis-sqlite-open))
            (gnosis-testing nil)
            candidate caught)
        (cl-letf (((symbol-function 'gnosis-db--check-schema)
                   (lambda (db version)
                     (funcall check db version)
                     (when (= version 10) (signal fault '("Validation fault")))))
                  ((symbol-function 'gnosis-sqlite-open)
                   (lambda (file) (setq candidate (funcall open file))))
                  ((symbol-function 'gnosis--commit-migration)
                   (lambda (&rest _) (ert-fail "Git ran before validation"))))
          (condition-case err (gnosis--ensure-db)
            ((error quit) (setq caught (car err)))))
        (should (eq caught fault))
        (should-not gnosis-db)
        (should-error (sqlite-select candidate "SELECT 1"))
        (should (equal before (gnosis-test-safety-snapshot
                              (expand-file-name "gnosis.db" gnosis-dir))))))))

(ert-deftest gnosis-startup-dashboard-creates-missing-parents ()
  "The public first-use command creates a nested destination and usable schema."
  (gnosis-test-safety
    (let ((gnosis-dir (expand-file-name "missing/parent/gnosis" gnosis-dir))
          (gnosis-dashboard-buffer-name " *gnosis-startup*"))
      (unwind-protect
          (save-window-excursion
            (cl-letf (((symbol-function 'keymap-popup) #'ignore))
              (call-interactively #'gnosis-dashboard))
            (should (file-directory-p gnosis-dir))
            (should (file-exists-p (expand-file-name "gnosis.db" gnosis-dir)))
            (should gnosis-db)
            (gnosis-db--check-schema gnosis-db 11)
            (with-current-buffer gnosis-dashboard-buffer-name
              (should (derived-mode-p 'gnosis-dashboard-mode))))
        (when-let* ((buffer (get-buffer gnosis-dashboard-buffer-name)))
          (kill-buffer buffer))))))

(ert-deftest gnosis-startup-dashboard-refuses-file-destinations ()
  "A file in either destination position survives failed startup unchanged."
  (dolist (nested '(nil t))
    (gnosis-test-safety
      (let* ((file (expand-file-name "retained" gnosis-dir))
             (gnosis-dir (if nested (expand-file-name "child/gnosis" file) file))
             (gnosis-dashboard-buffer-name " *gnosis-startup-refusal*"))
        (with-temp-file file (insert "Retained file"))
        (unwind-protect
            (save-window-excursion
              (cl-letf (((symbol-function 'keymap-popup) #'ignore))
                (should-error (call-interactively #'gnosis-dashboard)
                              :type 'file-error))
              (should-not gnosis-db)
              (should (equal "Retained file"
                             (with-temp-buffer
                               (insert-file-contents-literally file)
                               (buffer-string)))))
          (when-let* ((buffer (get-buffer gnosis-dashboard-buffer-name)))
            (kill-buffer buffer)))))))

(provide 'gnosis-test-startup)
;;; gnosis-test-startup.el ends here

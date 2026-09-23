;;; gnosis-test-campaign-journal-task.el --- Task source ownership tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;;; Commentary:

;; Exercise task completion through physical aliases and confirmation changes.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'gnosis-journal)

(defun gnosis-test-campaign-task--case (kind action)
  "Run ACTION with a visited task source configured through alias KIND.
ACTION receives the owner buffer, canonical filename and configured filename."
  (let* ((dir (make-temp-file "gnosis-task-owner-" t))
         (file (expand-file-name "tasks.org" dir))
         (alias (expand-file-name "alias.org" dir))
         (org-id-track-globally nil)
         (create-lockfiles nil)
         (find-file-existing-other-name t)
         (org-log-done nil)
         (gnosis-journal-todo-keywords '("TODO" "NEXT"))
         (text (concat "#+title: Tasks\n#+todo: TODO NEXT | DONE\n"
                       "* NEXT Focus\n:PROPERTIES:\n:ID: task-focus\n:END:\n"
                       "* TODO Other\n:PROPERTIES:\n:ID: task-other\n:END:\n"))
         owner)
    (unwind-protect
        (save-window-excursion
          (with-temp-file file (insert text))
          (pcase kind
            ('symlink (make-symbolic-link file alias))
            ('hardlink (add-name-to-file file alias))
            ('canonical (setq alias file)))
          (setq owner (find-file-noselect file))
          (with-current-buffer owner
            (goto-char (point-max))
            (insert "Unsaved draft\n"))
          (let ((gnosis-journal-todo-files (list alias)))
            (with-temp-buffer
              (org-mode)
              (insert "[[id:task-focus][Focus]]\n")
              (goto-char (point-min))
              (funcall action owner file alias)))
          (with-temp-buffer
            (insert-file-contents file)
            (should (equal (buffer-string) text))))
      (dolist (buffer (buffer-list))
        (when (and (buffer-file-name buffer)
                   (file-in-directory-p (buffer-file-name buffer) dir))
          (with-current-buffer buffer (set-buffer-modified-p nil))
          (kill-buffer buffer)))
      (when (buffer-live-p owner)
        (with-current-buffer owner (set-buffer-modified-p nil))
        (kill-buffer owner))
      (delete-directory dir t))))

(defun gnosis-test-campaign-task--complete (kind)
  "Complete only the requested task through KIND, without saving its draft."
  (gnosis-test-campaign-task--case
   kind
   (lambda (owner file _alias)
     (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
       (gnosis-journal-complete-task))
     (with-current-buffer owner
       (should (equal buffer-file-name file))
       (should (buffer-modified-p))
       (should (gnosis-journal--goto-id "task-focus"))
       (should (equal (org-get-todo-state) "DONE"))
       (should (gnosis-journal--goto-id "task-other"))
       (should (equal (org-get-todo-state) "TODO"))
       (should (string-suffix-p "Unsaved draft\n" (buffer-string)))))))

(ert-deftest gnosis-test-campaign-task-canonical ()
  "Canonical completion preserves unrelated tasks and unsaved text."
  (gnosis-test-campaign-task--complete 'canonical))

(ert-deftest gnosis-test-campaign-task-symlink ()
  "A configured symlink reuses its canonical visiting owner."
  (gnosis-test-campaign-task--complete 'symlink))

(ert-deftest gnosis-test-campaign-task-hardlink ()
  "A configured hardlink reuses its canonical visiting owner."
  (gnosis-test-campaign-task--complete 'hardlink))

(ert-deftest gnosis-test-campaign-task-confirmation-guards ()
  "Confirmation cannot authorize a reassociated or changed source task."
  (dolist (change '(reassociate mode missing-id state retarget killed))
    (gnosis-test-campaign-task--case
     'symlink
     (lambda (owner file alias)
       (let (after)
         (cl-letf (((symbol-function 'y-or-n-p)
                    (lambda (&rest _)
                      (with-current-buffer owner
                        (pcase change
                          ('reassociate
                           (set-visited-file-name
                            (expand-file-name "successor.org"
                                              (file-name-directory file))))
                          ('mode (text-mode))
                          ('missing-id
                           (gnosis-journal--goto-id "task-focus")
                           (org-entry-delete nil "ID"))
                          ('state
                           (gnosis-journal--goto-id "task-focus")
                           (org-todo "TODO"))
                          ('retarget
                           (let ((other (expand-file-name
                                         "other.org" (file-name-directory file))))
                             (with-temp-file other (insert "Successor\n"))
                             (delete-file alias)
                             (make-symbolic-link other alias)))
                          ('killed
                           (set-buffer-modified-p nil)
                           (kill-buffer owner)))
                        (when (buffer-live-p owner)
                          (setq after (buffer-string))))
                      t)))
           (should-error (gnosis-journal-complete-task) :type 'user-error))
         (when (buffer-live-p owner)
           (with-current-buffer owner
             (should (equal (buffer-string) after)))))))))

(ert-deftest gnosis-test-campaign-task-cancellation ()
  "Declining or quitting confirmation leaves the owner's draft unchanged."
  (dolist (quit '(nil t))
    (gnosis-test-campaign-task--case
     'hardlink
     (lambda (owner _file _alias)
       (let ((before (with-current-buffer owner (buffer-string))))
         (cl-letf (((symbol-function 'y-or-n-p)
                    (lambda (&rest _) (when quit (signal 'quit nil)) nil)))
           (if quit
               (should (eq 'quit
                           (condition-case nil
                               (gnosis-journal-complete-task)
                             (quit 'quit))))
             (should-error (gnosis-journal-complete-task) :type 'user-error)))
         (with-current-buffer owner
           (should (buffer-modified-p))
           (should (equal (buffer-string) before))))))))

(provide 'gnosis-test-campaign-journal-task)
;;; gnosis-test-campaign-journal-task.el ends here

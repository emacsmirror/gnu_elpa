;;; gnosis-vc.el --- Version control for gnosis  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2026  Free Software Foundation, Inc.

;; Author: Thanos Apollo <public@thanosapollo.org>
;; Keywords: extensions

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Git operations for the gnosis database: push, pull, chain commands.

;;; Code:

(require 'vc-git)
(require 'gnosis-db)

(defcustom gnosis-vc-auto-push nil
  "Run `vc-push' at the end of every review session."
  :type 'boolean
  :group 'gnosis)

(defun gnosis--git-cmd (args &optional sentinel)
  "Run git with ARGS list and detect password requests.

ARGS is a list of strings passed directly to git (no shell interpretation).
Optional SENTINEL is called with (process event) on completion.
Bind `default-directory' to `gnosis-dir' while starting git.
SENTINEL must retain any directory context it needs after that binding ends."
  (let* ((default-directory gnosis-dir)
         (git (or (executable-find "git")
                  (error "Git is not installed or not in PATH")))
         (process (apply #'start-process "gnosis-git" nil git args)))
    (set-process-filter
     process
     (lambda (proc output)
       (when (string-match-p "password:" output)
         (process-send-string proc
			      (concat (read-passwd "Password: ") "\n")))
       (message "%s" output)))
    (when sentinel
      (set-process-sentinel process sentinel))
    process))

(defun gnosis--ensure-git-repo ()
  "Ensure `gnosis-dir' is a git repository."
  (let ((default-directory gnosis-dir))
    (unless (file-exists-p (expand-file-name ".git" gnosis-dir))
      (vc-git-create-repo))))

(defun gnosis--git-chain (commands &optional on-finish on-error)
  "Run git COMMANDS sequentially, each as an arg list for `gnosis--git-cmd'.
Call ON-FINISH with no args after the last command succeeds.
Keep `gnosis-dir' and `default-directory' bound to the initiating absolute
directory through every command and ON-FINISH, including any push it starts.
Abort chain on failure with a message.  If supplied, call ON-ERROR with
an error condition raised by a deferred continuation; otherwise signal it.
Keyboard quit is not caught."
  (let* ((directory (file-name-as-directory (expand-file-name gnosis-dir)))
         (gnosis-dir directory)
         (default-directory directory))
    (if (null commands)
        (when on-finish (funcall on-finish))
      (gnosis--git-cmd
       (car commands)
       (lambda (_proc event)
         (let ((gnosis-dir directory)
               (default-directory directory))
           (condition-case err
               (if (string-match-p "finished" event)
                   (gnosis--git-chain (cdr commands) on-finish on-error)
                 (message "gnosis: git %s failed: %s"
                          (car (car commands)) (string-trim event)))
             (error (if on-error (funcall on-error err)
                      (signal (car err) (cdr err)))))))))))

(defun gnosis-vc--auto-commit (message &optional existing-only no-push)
  "Optionally commit gnosis.db with MESSAGE after a completed database change.
Skip Git when testing or when its executable is unavailable.  Unless
EXISTING-ONLY is non-nil, initialize a repository if necessary.  Report
Git setup or launch errors separately; they do not undo the database change.
Push after a successful commit when `gnosis-vc-auto-push' is non-nil,
unless NO-PUSH is non-nil."
  (unless gnosis-testing
    (if (not (executable-find "git"))
        (message "Gnosis: Automatic Git commit skipped; Git is unavailable")
      (condition-case err
          (let* ((gnosis-dir (file-name-as-directory (expand-file-name gnosis-dir)))
                 (default-directory gnosis-dir))
            (when (or (not existing-only)
                      (file-exists-p (expand-file-name ".git" gnosis-dir)))
              (unless existing-only (gnosis--ensure-git-repo))
              (gnosis--git-chain
               `(("add" "gnosis.db") ("commit" "-m" ,message))
               (lambda ()
                 (when (and (not no-push) gnosis-vc-auto-push)
                   (condition-case err
                       (gnosis-vc-push)
                     (error
                      (message "Gnosis: Automatic Git push failed: %s"
                               (error-message-string err))))))
               (lambda (err)
                 (message "Gnosis: Automatic Git commit failed: %s"
                          (error-message-string err))))))
        (error
         (message "Gnosis: Automatic Git commit failed: %s"
                  (error-message-string err)))))))

;;;###autoload
(defun gnosis-vc-push ()
  "Run `git push' for gnosis repository."
  (interactive)
  (gnosis--git-cmd '("push")))

(defvar gnosis-vc--pull-owner nil
  "Current pull's unique (CONNECTION DIRECTORY ABSOLUTE-DIRECTORY), or nil.
DIRECTORY is the original option value; ABSOLUTE-DIRECTORY pins relative paths.
Only this operation may reopen and publish its original database.")

(defun gnosis-vc--pull-current-p (owner db)
  "Return non-nil if pull OWNER still owns the current connection DB."
  (and (eq owner gnosis-vc--pull-owner)
       (eq db gnosis-db)
       (equal (cadr owner) gnosis-dir)))

(defun gnosis-vc--reopen-db (owner)
  "Reopen OWNER's database and return its newly published connection.
Return nil if ownership changed.  Never publish an unvalidated candidate."
  (when (gnosis-vc--pull-current-p owner (car owner))
    ;; Unpublish before closing: failure must not leave a trusted old handle.
    (setq gnosis-db nil)
    (when (car owner) (gnosis-sqlite-close (car owner)))
    (let ((candidate (gnosis-db--open (caddr owner))) published)
      (unwind-protect
          (when (gnosis-vc--pull-current-p owner nil)
            (setq gnosis-db candidate published t)
            candidate)
        (unless published (gnosis-sqlite-close candidate))))))

(defun gnosis-vc--finish-pull (owner process)
  "Settle terminal PROCESS for pull OWNER without touching a successor."
  (when (memq (process-status process) '(exit signal))
    (unwind-protect
        (when (gnosis-vc--pull-current-p owner (car owner))
          (if (and (eq (process-status process) 'exit)
                   (zerop (process-exit-status process)))
              (condition-case err
                  (when (gnosis-vc--reopen-db owner)
                    (message "Gnosis: Pull successful, database reopened"))
                (error
                 (message "Gnosis: Failed to reopen database: %s"
                          (error-message-string err))))
            (message "Gnosis: Git pull failed with exit code %s"
                     (process-exit-status process))))
      (when (eq owner gnosis-vc--pull-owner)
        (setq gnosis-vc--pull-owner nil)))))

;;;###autoload
(defun gnosis-vc-pull ()
  "Run `git pull' for gnosis repository.

Reopen and validate the original database after a successful pull, provided
its connection and directory are still current.  Reopening failure leaves no
published connection; subsequent commands must validate storage again."
  (interactive)
  (let ((owner (list gnosis-db (copy-sequence gnosis-dir)
                     (expand-file-name gnosis-dir)))
        started)
    (setq gnosis-vc--pull-owner owner)
    (unwind-protect
        (prog1
            (gnosis--git-cmd
             '("pull")
             (lambda (proc _event) (gnosis-vc--finish-pull owner proc)))
          (setq started t))
      (when (and (not started) (eq owner gnosis-vc--pull-owner))
        (setq gnosis-vc--pull-owner nil)))))

(provide 'gnosis-vc)
;;; gnosis-vc.el ends here

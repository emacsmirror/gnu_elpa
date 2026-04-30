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
  "Run git with ARGS list, watching for password prompts.

ARGS is a list of strings passed directly to git (no shell interpretation).
Optional SENTINEL is called with (process event) on completion.
Binds `default-directory' to `gnosis-dir' so sentinels run in
the correct directory regardless of buffer context."
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

(defun gnosis--git-chain (commands &optional on-finish)
  "Run git COMMANDS sequentially, each as an arg list for `gnosis--git-cmd'.
Call ON-FINISH with no args after the last command succeeds.
Abort chain on failure with a message."
  (if (null commands)
      (when on-finish (funcall on-finish))
    (gnosis--git-cmd (car commands)
		     (lambda (_proc event)
		       (if (string-match-p "finished" event)
			   (gnosis--git-chain (cdr commands) on-finish)
			 (message "gnosis: git %s failed: %s"
				  (car (car commands)) (string-trim event)))))))

;;;###autoload
(defun gnosis-vc-push ()
  "Run `git push' for gnosis repository."
  (interactive)
  (gnosis--git-cmd '("push")))

;;;###autoload
(defun gnosis-vc-pull ()
  "Run `git pull' for gnosis repository.

Reopens the gnosis database after successful pull."
  (interactive)
  (gnosis--git-cmd
   '("pull")
   (lambda (proc event)
     (cond
      ((string-match-p "finished" event)
       (when (zerop (process-exit-status proc))
	 (condition-case err
	     (progn
	       (when (and gnosis-db (gnosis-sqlite-live-p gnosis-db))
		 (gnosis-sqlite-close gnosis-db))
	       (setf gnosis-db
                     (gnosis-sqlite-open (expand-file-name "gnosis.db" gnosis-dir)))
	       (gnosis-db-init)
	       (message "Gnosis: Pull successful, database reopened"))
	   (error (message "Gnosis: Failed to reopen database: %s"
			   (error-message-string err))))))
      ((string-match-p "exited abnormally" event)
       (message "Gnosis: Git pull failed with exit code %s"
                (process-exit-status proc)))))))

(provide 'gnosis-vc)
;;; gnosis-vc.el ends here

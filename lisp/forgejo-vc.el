;;; forgejo-vc.el --- VC integration for Forgejo  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Thanos Apollo

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

;; Git/VC integration for Forgejo: repository detection from git remote,
;; AGit-Flow PR submission, PR fetch/update, and a `C-x v f' transient
;; for repo-local operations.

;;; Code:

(require 'cl-lib)
(require 'transient)
(require 'forgejo-utils)
(require 'forgejo)

(declare-function forgejo-issue-list "forgejo-issue.el"
                  (&optional owner repo))
(declare-function forgejo-pull-list "forgejo-pull.el"
                  (&optional owner repo))

;;; Git detection (pure: git command -> data)

(defun forgejo-vc--repo-from-remote ()
  "Detect host, owner, and repo from the current git remote.
Returns (HOST OWNER REPO) where HOST is a full URL like
\"https://codeberg.org\", or nil if detection fails."
  (when-let* ((remote-url
               (string-trim
                (with-output-to-string
                  (with-current-buffer standard-output
                    (process-file "git" nil '(t nil) nil
                                  "remote" "get-url" "origin")))))
              ((not (string-empty-p remote-url))))
    (let ((result
           (cond
            ;; HTTPS: https://host/owner/repo[.git]
            ((string-match
              "\\`https?://\\([^/]+\\)/\\([^/]+\\)/\\(.+?\\)\\(?:\\.git\\)?\\'" remote-url)
             (list (format "https://%s" (match-string 1 remote-url))
                   (match-string 2 remote-url)
                   (match-string 3 remote-url)))
            ;; SSH with protocol: ssh://[user@]host[:port]/owner/repo[.git]
            ((string-match
              "\\`ssh://\\(?:[^@]+@\\)?\\([^/:]+\\)[:/]\\([^/]+\\)/\\(.+?\\)\\(?:\\.git\\)?\\'" remote-url)
             (list (format "https://%s" (match-string 1 remote-url))
                   (match-string 2 remote-url)
                   (match-string 3 remote-url)))
            ;; SCP-style: [user@]host:owner/repo[.git]
            ((string-match
              "\\`\\(?:[^@]+@\\)?\\([^:]+\\):\\([^/]+\\)/\\(.+?\\)\\(?:\\.git\\)?\\'" remote-url)
             (list (format "https://%s" (match-string 1 remote-url))
                   (match-string 2 remote-url)
                   (match-string 3 remote-url))))))
      result)))

(defun forgejo-vc--upstream-branch (branch)
  "Return the upstream remote/branch for BRANCH, or nil."
  (let ((result (string-trim
                 (with-output-to-string
                   (with-current-buffer standard-output
                     (process-file "git" nil '(t nil) nil
                                   "rev-parse" "--abbrev-ref"
                                   (concat branch "@{upstream}")))))))
    (unless (string-empty-p result) result)))

(defun forgejo-vc--remote (branch)
  "Return the push remote for BRANCH, or \"origin\"."
  (let ((result (string-trim
                 (with-output-to-string
                   (with-current-buffer standard-output
                     (process-file "git" nil '(t nil) nil
                                   "config"
                                   (format "branch.%s.remote" branch)))))))
    (if (string-empty-p result) "origin" result)))

;;; AGit-Flow helpers (pure: args -> refspec/options)

(defun forgejo-vc--sanitize-ref (name)
  "Sanitize NAME for use in a git ref path.
Removes or replaces characters forbidden by git: spaces, ~, ^, :, ?, *, [, \\."
  (replace-regexp-in-string "[~^:?*\\[\\\\[:space:]]+" "-" name))

(defun forgejo-vc--refspec (source target topic)
  "Build AGit-Flow refspec from SOURCE branch, TARGET branch, and TOPIC."
  (format "%s:refs/for/%s/%s" source target (forgejo-vc--sanitize-ref topic)))

(defun forgejo-vc--encode-description (text)
  "Base64-encode TEXT with {base64} prefix for Forgejo."
  (concat "{base64}"
          (base64-encode-string (encode-coding-string text 'utf-8)
                                :no-line-break)))

(defun forgejo-vc--push-options (title description)
  "Build list of push-option arguments from TITLE and DESCRIPTION."
  (list "-o" (concat "title=" title)
        "-o" (concat "description="
                     (forgejo-vc--encode-description description))))

;;; AGit-Flow commands (side-effectful: execute git)

(defun forgejo-vc--git-push (remote refspec push-options)
  "Push REFSPEC to REMOTE with PUSH-OPTIONS via git."
  (let* ((buf (get-buffer-create "*forgejo PR*"))
         (_ (with-current-buffer buf (erase-buffer)))
         (process (apply #'start-process "forgejo-pr" buf
                         "git" "push" "-v" remote refspec push-options)))
    (set-process-sentinel
     process
     (lambda (proc _event)
       (when (memq (process-status proc) '(exit signal))
         (if (zerop (process-exit-status proc))
             (message "PR pushed successfully.")
           (message "PR push failed (exit %d). See %s"
                    (process-exit-status proc)
                    (buffer-name (process-buffer proc)))))))))

;;;###autoload
(defun forgejo-vc-submit (remote topic target &optional force-push-p)
  "Submit a PR via Forgejo's AGit-Flow workflow.
REMOTE is the git remote, TOPIC is the session identifier,
TARGET is the remote branch to target.
With prefix arg FORCE-PUSH-P, force-push to update an existing PR."
  (interactive
   (let* ((branch (car (vc-git-branches)))
          (remote (forgejo-vc--remote branch))
          (upstream (forgejo-vc--upstream-branch branch)))
     (list remote
           (let ((input (read-string (format "Topic (default: %s): " branch))))
             (if (string-empty-p input) branch input))
           (if upstream
               (let ((input (read-string
                             (format "Target branch (default: %s): " upstream))))
                 (if (string-empty-p input) upstream input))
             (completing-read "Target branch: " (vc-git-branches) nil t))
           current-prefix-arg)))
  (let ((target (replace-regexp-in-string "\\`.+/" "" target)))
    (if force-push-p
        (forgejo-vc--git-push
         remote
         (forgejo-vc--refspec "HEAD" target topic)
         (list "-o" "force-push=true"))
      (let* ((title (read-string "PR Title: "))
             (desc (read-string-from-buffer "PR Description" "")))
        (forgejo-vc--git-push
         remote
         (forgejo-vc--refspec "HEAD" target topic)
         (forgejo-vc--push-options title desc))))))

;;;###autoload
(defun forgejo-vc-fetch (n)
  "Fetch pull request N and check out the pr-N branch."
  (interactive "nPR number: ")
  (let ((branch (format "pr-%d" n))
        (ref (format "refs/pull/%d/head" n)))
    (vc-git-command nil 0 nil "fetch" "origin" (format "%s:%s" ref branch))
    (vc-git-command nil 0 nil "checkout" branch)
    (message "Checked out %s" branch)))

;;;###autoload
(defun forgejo-vc-update ()
  "Update the current pr-N branch from the remote PR head."
  (interactive)
  (let ((branch (car (vc-git-branches))))
    (if (string-match "\\`pr-\\([0-9]+\\)\\'" branch)
        (let ((n (match-string 1 branch)))
          (vc-git-command nil 0 nil "fetch" "origin"
                          (format "pull/%s/head" n))
          (vc-git-command nil 0 nil "reset" "--hard" "FETCH_HEAD")
          (message "Updated %s to latest PR head." branch))
      (user-error "Not on a pr-N branch: %s" branch))))

;;; Repo-aware wrappers (use detected repo, no prompt)

(defun forgejo-vc--require-repo ()
  "Return (HOST OWNER REPO) from git remote, or signal error."
  (or (forgejo-vc--repo-from-remote)
      (user-error "Not in a git repo with a Forgejo remote")))

(defun forgejo-vc-issues ()
  "List issues for the current repository."
  (interactive)
  (cl-destructuring-bind (host owner repo) (forgejo-vc--require-repo)
    (forgejo-with-host host
      (forgejo-issue-list owner repo))))

(defun forgejo-vc-pulls ()
  "List pull requests for the current repository."
  (interactive)
  (cl-destructuring-bind (host owner repo) (forgejo-vc--require-repo)
    (forgejo-with-host host
      (forgejo-pull-list owner repo))))

(defun forgejo-vc-browse ()
  "Open the current repository in the browser."
  (interactive)
  (cl-destructuring-bind (host owner repo) (forgejo-vc--require-repo)
    (forgejo-with-host host
      (forgejo-utils-browse-repo owner repo))))

;;; Transient + keymap

;;;###autoload (autoload 'forgejo-vc "forgejo-vc" nil t)
(transient-define-prefix forgejo-vc ()
  "Forgejo operations for the current repository."
  [["View"
    ("i" "Issues" forgejo-vc-issues)
    ("p" "Pull requests" forgejo-vc-pulls)]
   ["PR"
    ("s" "Submit PR" forgejo-vc-submit)
    ("f" "Fetch PR" forgejo-vc-fetch)
    ("u" "Update PR branch" forgejo-vc-update)]
   ["Actions"
    ("b" "Browse repo" forgejo-vc-browse)]])

;;;###autoload
(with-eval-after-load 'vc
  (keymap-set vc-prefix-map "f" #'forgejo-vc))

(provide 'forgejo-vc)
;;; forgejo-vc.el ends here

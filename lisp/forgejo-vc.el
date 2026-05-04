;;; forgejo-vc.el --- VC integration for Forgejo  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Free Software Foundation, Inc.

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
(require 'vc-git)
(require 'keymap-popup)
(require 'forgejo-utils)
(require 'forgejo-settings)
(require 'forgejo)

(declare-function forgejo-issue-list "forgejo-issue.el"
                  (&optional owner repo))
(declare-function forgejo-pull-list "forgejo-pull.el"
                  (&optional owner repo))
(declare-function forgejo-api-post "forgejo-api.el"
                  (host endpoint &optional params json-body callback))
(declare-function forgejo-db-get-repo "forgejo-db.el"
                  (host owner name))
(declare-function forgejo-repo-sync-metadata "forgejo-repo.el"
                  (host-url owner repo))

;;; Repo counts (async, cached per-directory)

(defvar forgejo-vc--counts (make-hash-table :test 'equal)
  "Cache of (ISSUE-COUNT . PR-COUNT) keyed by \"owner/repo\".")

(defvar-local forgejo-vc--repo-key nil
  "Cached \"owner/repo\" key for the current buffer.")

(defun forgejo-vc--ensure-repo-key ()
  "Set `forgejo-vc--repo-key' from git remote if not already cached."
  (or forgejo-vc--repo-key
      (when-let* ((context (forgejo-vc--repo-from-remote)))
        (setq forgejo-vc--repo-key
              (format "%s/%s" (nth 1 context) (nth 2 context))))))

(defun forgejo-vc--issue-count ()
  "Return cached open issue count for the current repo, or nil."
  (car (gethash (forgejo-vc--ensure-repo-key) forgejo-vc--counts)))

(defun forgejo-vc--pr-count ()
  "Return cached open PR count for the current repo, or nil."
  (cdr (gethash (forgejo-vc--ensure-repo-key) forgejo-vc--counts)))

(defun forgejo-vc--repo-metadata ()
  "Return cached repo metadata alist for the current repo, or nil."
  (when-let* ((context (forgejo-vc--repo-from-remote))
              (host-url (nth 0 context))
              (host (url-host (url-generic-parse-url host-url)))
              (owner (nth 1 context))
              (repo (nth 2 context)))
    (forgejo-db-get-repo host owner repo)))

(defun forgejo-vc--no-issues-p ()
  "Return non-nil when the current repo has issues disabled."
  (when-let* ((meta (forgejo-vc--repo-metadata)))
    (not (alist-get 'has_issues meta))))

(defun forgejo-vc--no-pulls-p ()
  "Return non-nil when the current repo has pull requests disabled."
  (when-let* ((meta (forgejo-vc--repo-metadata)))
    (not (alist-get 'has_pull_requests meta))))

(defun forgejo-vc--fetch-counts ()
  "Fetch open issue/PR counts for the current repo asynchronously.
Also syncs repo metadata if not yet cached."
  (when-let* ((context (forgejo-vc--repo-from-remote))
              (host (nth 0 context))
              (owner (nth 1 context))
              (repo (nth 2 context))
              (key (format "%s/%s" owner repo)))
    (let ((hostname (url-host (url-generic-parse-url host))))
      (unless (forgejo-db-get-repo hostname owner repo)
        (forgejo-repo-sync-metadata host owner repo))
      (when-let* ((meta (forgejo-db-get-repo hostname owner repo))
                  ((alist-get 'has_issues meta)))
        (forgejo-api-get
         host (format "repos/%s/%s/issues" owner repo)
         '(("state" . "open") ("type" . "issues") ("limit" . "1"))
         (lambda (_data headers)
           (let ((existing (gethash key forgejo-vc--counts)))
             (puthash key
                      (cons (plist-get headers :total-count) (cdr existing))
                      forgejo-vc--counts)))))
      (when-let* ((meta (forgejo-db-get-repo hostname owner repo))
                  ((alist-get 'has_pull_requests meta)))
        (forgejo-api-get
         host (format "repos/%s/%s/issues" owner repo)
         '(("state" . "open") ("type" . "pulls") ("limit" . "1"))
         (lambda (_data headers)
           (let ((existing (gethash key forgejo-vc--counts)))
             (puthash key
                      (cons (car existing) (plist-get headers :total-count))
                      forgejo-vc--counts))))))))

;;; Git detection (pure: git command -> data)

(defun forgejo-vc--remotes ()
  "Return list of git remote names."
  (let ((output (string-trim
                 (with-output-to-string
                   (with-current-buffer standard-output
                     (process-file "git" nil '(t nil) nil "remote"))))))
    (when (not (string-empty-p output))
      (split-string output "\n" t))))

(defun forgejo-vc--remote-url (remote)
  "Return the URL for git REMOTE, or nil."
  (let ((url (string-trim
              (with-output-to-string
                (with-current-buffer standard-output
                  (process-file "git" nil '(t nil) nil
                                "remote" "get-url" remote))))))
    (unless (string-empty-p url) url)))

(defun forgejo-vc--parse-remote-url (remote-url)
  "Parse REMOTE-URL into (HOST OWNER REPO), or nil."
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
          (match-string 3 remote-url)))))

(defun forgejo-vc--repo-from-remote ()
  "Detect host, owner, repo, and remote name from any git remote.
Tries all remotes and returns the first that parses as a forge URL.
Returns (HOST OWNER REPO REMOTE-NAME) or nil."
  (cl-some (lambda (remote)
             (when-let* ((url (forgejo-vc--remote-url remote))
                         (parsed (forgejo-vc--parse-remote-url url)))
               (append parsed (list remote))))
           (forgejo-vc--remotes)))

(defun forgejo-vc--upstream-branch (branch)
  "Return the upstream remote/branch for BRANCH, or nil."
  (let ((result (string-trim
                 (with-output-to-string
                   (with-current-buffer standard-output
                     (process-file "git" nil '(t nil) nil
                                   "rev-parse" "--abbrev-ref"
                                   (concat branch "@{upstream}")))))))
    (unless (string-empty-p result) result)))

(defun forgejo-vc--remote-branches ()
  "Return list of all remote branches as \"remote/branch\" strings.
Uses local tracking branches first, falls back to `git ls-remote'."
  (or (let ((output (string-trim
                     (with-output-to-string
                       (with-current-buffer standard-output
                         (process-file "git" nil '(t nil) nil
                                       "branch" "-r" "--format"
                                       "%(refname:short)"))))))
        (when (not (string-empty-p output))
          (cl-remove-if-not (lambda (s) (string-match-p "/" s))
                            (split-string output "\n" t))))
      (forgejo-vc--ls-remote-branches)))

(defun forgejo-vc--ls-remote-branches ()
  "Fetch branches from all remotes via `git ls-remote'.
Returns a list of \"remote/branch\" strings."
  (let ((remotes (forgejo-vc--remotes))
        (result nil))
    (dolist (remote remotes)
      (let ((output (string-trim
                     (with-output-to-string
                       (with-current-buffer standard-output
                         (process-file "git" nil '(t nil) nil
                                       "ls-remote" "--heads" remote))))))
        (when (not (string-empty-p output))
          (dolist (line (split-string output "\n" t))
            (when (string-match "refs/heads/\\(.+\\)\\'" line)
              (push (format "%s/%s" remote (match-string 1 line)) result))))))
    (nreverse result)))

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

;;; AGit-Flow autofill (pure: git data -> defaults)

(defun forgejo-vc--commit-count (upstream)
  "Return the number of commits between UPSTREAM and HEAD.
Returns 0 if UPSTREAM is not a valid ref."
  (string-to-number
   (string-trim
    (with-output-to-string
      (with-current-buffer standard-output
        (process-file "git" nil '(t nil) nil
                      "rev-list" "--count"
                      (concat upstream "..HEAD")))))))

(defun forgejo-vc--commit-subjects (upstream)
  "Return list of commit subjects between UPSTREAM and HEAD, oldest first."
  (let ((output (string-trim
                 (with-output-to-string
                   (with-current-buffer standard-output
                     (process-file "git" nil '(t nil) nil
                                   "log" "--format=%s"
                                   (concat upstream "..HEAD")
                                   "--reverse"))))))
    (when (not (string-empty-p output))
      (split-string output "\n" t))))

(defun forgejo-vc--head-subject ()
  "Return the subject line of HEAD."
  (string-trim
   (with-output-to-string
     (with-current-buffer standard-output
       (process-file "git" nil '(t nil) nil "log" "-1" "--format=%s")))))

(defun forgejo-vc--head-body ()
  "Return the body (everything after the subject) of HEAD."
  (string-trim
   (with-output-to-string
     (with-current-buffer standard-output
       (process-file "git" nil '(t nil) nil "log" "-1" "--format=%b")))))

(defun forgejo-vc--autofill-defaults (upstream)
  "Return (TITLE . BODY) defaults based on commits since UPSTREAM."
  (let ((count (forgejo-vc--commit-count upstream)))
    (cond
     ((<= count 1)
      (cons (forgejo-vc--head-subject) (forgejo-vc--head-body)))
     (t
      (let ((subjects (forgejo-vc--commit-subjects upstream)))
        (cons (forgejo-vc--head-subject)
              (mapconcat #'identity subjects "\n")))))))

;;; PR template discovery

(defun forgejo-vc--default-branch (remote)
  "Return the default branch name for REMOTE, or nil."
  (let ((result (string-trim
                 (with-output-to-string
                   (with-current-buffer standard-output
                     (process-file "git" nil '(t nil) nil
                                   "symbolic-ref"
                                   (format "refs/remotes/%s/HEAD" remote)))))))
    (when (and (not (string-empty-p result))
               (string-match "\\`refs/remotes/[^/]+/\\(.+\\)\\'" result))
      (match-string 1 result))))

(defun forgejo-vc--find-pr-template (remote)
  "Search for a PR template file in REMOTE's default branch.
Checks .forgejo/, .gitea/, .github/ directories.
Returns the template content as a string, or nil."
  (when-let* ((default-br (forgejo-vc--default-branch remote))
              (ref (format "%s/%s" remote default-br)))
    (cl-some
     (lambda (path)
       (with-temp-buffer
         (when (zerop (process-file "git" nil '(t nil) nil
                                    "show" (format "%s:%s" ref path)))
           (let ((content (buffer-string)))
             (unless (string-empty-p (string-trim content))
               content)))))
     '(".forgejo/PULL_REQUEST_TEMPLATE.md"
       ".forgejo/pull_request_template.md"
       ".gitea/PULL_REQUEST_TEMPLATE.md"
       ".gitea/pull_request_template.md"
       ".github/PULL_REQUEST_TEMPLATE.md"
       ".github/pull_request_template.md"))))

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
          (all-remote-branches (forgejo-vc--remote-branches))
          (default-target (forgejo-vc--upstream-branch branch))
          (choice (completing-read
                   (if default-target
                       (format "Target (default: %s): " default-target)
                     "Target (remote/branch): ")
                   all-remote-branches nil nil nil nil default-target))
          (remote (if (string-match "\\`\\([^/]+\\)/\\(.+\\)\\'" choice)
                      (match-string 1 choice)
                    (forgejo-vc--remote branch)))
          (target (if (string-match "\\`\\([^/]+\\)/\\(.+\\)\\'" choice)
                      (match-string 2 choice)
                    choice)))
     (let* ((pr-branch-p (string-match-p "\\`pr-[0-9]+\\'" branch))
            (default-topic (unless pr-branch-p branch)))
       (list remote
             (if default-topic
                 (let ((input (read-string (format "Topic (default: %s): " default-topic))))
                   (if (string-empty-p input) default-topic input))
               (let ((input (read-string "Topic: ")))
                 (when (string-empty-p input)
                   (user-error "Topic is required for pr-N branches"))
                 input))
             target
             current-prefix-arg))))
  (let ((target (replace-regexp-in-string "\\`.+/" "" target)))
    (if force-push-p
        (forgejo-vc--git-push
         remote
         (forgejo-vc--refspec "HEAD" target topic)
         (list "-o" "force-push=true"))
      (let* ((upstream (format "%s/%s" remote target))
             (defaults (forgejo-vc--autofill-defaults upstream))
             (default-title (car defaults))
             (default-body (cdr defaults))
             (template (forgejo-vc--find-pr-template remote))
             (use-template (and template
                                (y-or-n-p "PR template found. Use it? ")))
             (initial-body
              (cond
               ((and (not (string-empty-p default-body)) use-template)
                (concat default-body "\n\n" template))
               (use-template template)
               ((not (string-empty-p default-body)) default-body)
               (t nil)))
             (title (read-string "PR Title: " default-title))
             (_ (when (string-empty-p title)
                  (user-error "PR title cannot be empty")))
             (desc (forgejo-utils-read-body initial-body)))
        (unless desc
          (user-error "PR submission cancelled"))
        (forgejo-vc--git-push
         remote
         (forgejo-vc--refspec "HEAD" target topic)
         (forgejo-vc--push-options title desc))))))

;;;###autoload
(defun forgejo-vc-fetch (n)
  "Fetch pull request N and check out the pr-N branch.
If the branch already exists, update it to the latest PR head."
  (interactive "nPR number: ")
  (let* ((context (or (forgejo-vc--repo-from-remote)
                      (user-error "No Forgejo remote found")))
         (remote (nth 3 context))
         (branch (format "pr-%d" n))
         (ref (format "pull/%d/head" n))
         (dir default-directory)
         (branch-exists (member branch (vc-git-branches))))
    (message "Fetching PR #%d from %s..." n remote)
    (let ((stderr-buf (generate-new-buffer " *forgejo-fetch-stderr*")))
      (make-process
       :name (format "forgejo-fetch-pr-%d" n)
       :command (list "git" "fetch" remote ref)
       :stderr stderr-buf
       :sentinel
       (lambda (_proc event)
         (cond
          ((string-match-p "finished" event)
           (let ((default-directory dir))
             (if branch-exists
                 (progn
                   (vc-git-command nil 0 nil "checkout" branch)
                   (vc-git-command nil 0 nil "reset" "--hard" "FETCH_HEAD"))
               (vc-git-command nil 0 nil "checkout" "-b" branch "FETCH_HEAD")))
           (message "Checked out %s from %s" branch remote))
          ((string-match-p "\\(?:exited\\|signal\\)" event)
           (let ((err (with-current-buffer stderr-buf
                        (string-trim (buffer-string)))))
             (message "Failed to fetch PR #%d: %s" n err))))
         (kill-buffer stderr-buf))))))

;;;###autoload
(defun forgejo-vc-update ()
  "Update the current pr-N branch from the remote PR head."
  (interactive)
  (let ((branch (car (vc-git-branches))))
    (if (string-match "\\`pr-\\([0-9]+\\)\\'" branch)
        (let* ((n (match-string 1 branch))
               (context (forgejo-vc--repo-from-remote))
               (remote (or (nth 3 context)
                           (forgejo-vc--remote branch))))
          (vc-git-command nil 0 nil "fetch" remote
                          (format "pull/%s/head" n))
          (vc-git-command nil 0 nil "reset" "--hard" "FETCH_HEAD")
          (message "Updated %s to latest PR head." branch))
      (user-error "Not on a pr-N branch: %s" branch))))

;;; Push + manual merge

(defun forgejo-vc--head-sha ()
  "Return the full SHA of HEAD."
  (string-trim
   (with-output-to-string
     (with-current-buffer standard-output
       (process-file "git" nil '(t nil) nil "rev-parse" "HEAD")))))

(defun forgejo-vc--mark-merged (host owner repo n sha)
  "Mark PR N in OWNER/REPO on HOST as manually merged at SHA."
  (forgejo-api-post
   host
   (format "repos/%s/%s/pulls/%d/merge" owner repo n)
   nil
   `((Do . "manually-merged")
     (MergeCommitID . ,sha))
   (lambda (_data _headers)
     (message "PR #%d marked as manually merged." n))))

;;;###autoload
(defun forgejo-vc-push (remote branch &optional mark-merged-p)
  "Push BRANCH to REMOTE.
With prefix argument MARK-MERGED-P, also prompt for a PR number
and mark it as manually merged after a successful push."
  (interactive
   (let* ((current-branch (car (vc-git-branches)))
          (all-remote-branches (forgejo-vc--remote-branches))
          (default-target (forgejo-vc--upstream-branch current-branch))
          (choice (completing-read
                   (if default-target
                       (format "Push to (default: %s): " default-target)
                     "Push to (remote/branch): ")
                   all-remote-branches nil nil nil nil default-target))
          (remote (if (string-match "\\`\\([^/]+\\)/\\(.+\\)\\'" choice)
                      (match-string 1 choice)
                    (forgejo-vc--remote current-branch)))
          (branch (if (string-match "\\`\\([^/]+\\)/\\(.+\\)\\'" choice)
                      (match-string 2 choice)
                    choice)))
     (list remote branch current-prefix-arg)))
  (let* ((context (forgejo-vc--repo-from-remote))
         (pr-number (when mark-merged-p
                      (read-number "PR number to mark as merged: ")))
         (dir default-directory))
    (make-process
     :name "forgejo-push"
     :command (list "git" "push" remote
                    (format "HEAD:%s" branch))
     :sentinel
     (lambda (_proc event)
       (cond
        ((string-match-p "finished" event)
         (message "Pushed to %s/%s." remote branch)
         (when pr-number
           (let* ((default-directory dir)
                  (host (nth 0 context))
                  (owner (nth 1 context))
                  (repo (nth 2 context))
                  (sha (forgejo-vc--head-sha)))
             (forgejo-vc--mark-merged host owner repo pr-number sha))))
        ((string-match-p "\\(?:exited\\|signal\\)" event)
         (message "Push failed: %s" (string-trim event))))))))

;;; Repo-aware wrappers (use detected repo, no prompt)

(defun forgejo-vc--require-repo ()
  "Return (HOST OWNER REPO) from git remote, or signal error."
  (if-let* ((context (forgejo-vc--repo-from-remote)))
      (list (nth 0 context) (nth 1 context) (nth 2 context))
    (user-error "Not in a git repo with a Forgejo remote")))

(defun forgejo-vc-issues ()
  "List issues for the current repository."
  (interactive)
  (cl-destructuring-bind (_host owner repo) (forgejo-vc--require-repo)
    (forgejo-issue-list owner repo)))

(defun forgejo-vc-pulls ()
  "List pull requests for the current repository."
  (interactive)
  (cl-destructuring-bind (_host owner repo) (forgejo-vc--require-repo)
    (forgejo-pull-list owner repo)))

(defun forgejo-vc-browse ()
  "Open the current repository in the browser."
  (interactive)
  (cl-destructuring-bind (host owner repo) (forgejo-vc--require-repo)
    (forgejo-utils-browse-repo host owner repo)))

;;; Popup keymap

(defun forgejo-vc--no-remote-p ()
  "Return non-nil when not in a repo with a Forgejo remote."
  (not (forgejo-vc--repo-from-remote)))

(keymap-popup-define forgejo-vc-map
  "Forgejo operations for the current repository."
  :group "View"
  "i" ((lambda ()
         (cond
          ((forgejo-vc--no-issues-p) "Issues (disabled)")
          ((forgejo-vc--issue-count)
           (format "Issues (%s)" (propertize
                                  (number-to-string (forgejo-vc--issue-count))
                                  'face 'warning)))
          (t "Issues")))
       forgejo-vc-issues
       :inapt-if (lambda () (forgejo-vc--no-issues-p)))
  "p" ((lambda ()
         (cond
          ((forgejo-vc--no-pulls-p) "Pull requests (disabled)")
          ((forgejo-vc--pr-count)
           (format "Pull requests (%s)" (propertize
                                         (number-to-string (forgejo-vc--pr-count))
                                         'face 'forgejo-open-face)))
          (t "Pull requests")))
       forgejo-vc-pulls
       :inapt-if (lambda () (forgejo-vc--no-pulls-p)))
  :group "PR"
  "s" ("Submit PR" forgejo-vc-submit :c-u "force push"
       :inapt-if (lambda () (forgejo-vc--no-remote-p)))
  "f" ("Fetch PR" forgejo-vc-fetch
       :inapt-if (lambda () (forgejo-vc--no-remote-p)))
  "u" ("Update PR branch" forgejo-vc-update
       :inapt-if (lambda () (forgejo-vc--no-remote-p)))
  "P" ("Push" forgejo-vc-push :c-u "mark PR merged"
       :inapt-if (lambda () (forgejo-vc--no-remote-p)))
  :group "Actions"
  "S" ("Settings" forgejo-settings)
  "b" ("Browse repo" forgejo-vc-browse))

;;;###autoload
(defun forgejo-vc ()
  "Forgejo operations for the current repository."
  (interactive)
  (forgejo-vc--fetch-counts)
  (keymap-popup forgejo-vc-map))

;;;###autoload
(with-eval-after-load 'vc
  (keymap-set vc-prefix-map "f" #'forgejo-vc))

(provide 'forgejo-vc)
;;; forgejo-vc.el ends here

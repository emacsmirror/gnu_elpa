;;; forgejo-transient.el --- Transient menus for Forgejo  -*- lexical-binding: t; -*-

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

;; Transient menus for Forgejo.  Provides the top-level `forgejo'
;; entry point and the action-at-point dispatch for repo search results.

;;; Code:

(require 'transient)
(require 'forgejo)
(require 'forgejo-utils)

(declare-function forgejo-repo-search "forgejo-repo.el" (query))
(declare-function forgejo-issue-list "forgejo-issue.el"
                  (&optional owner repo))
(declare-function forgejo-pull-list "forgejo-pull.el"
                  (&optional owner repo))
(declare-function forgejo-repo-search--owner-repo-at-point "forgejo-repo.el" ())

(defvar forgejo-host)
(defvar forgejo-repo--host)

;;; Top-level menu

;;;###autoload (autoload 'forgejo "forgejo-transient" nil t)
(transient-define-prefix forgejo ()
  "Forgejo."
  [["Navigate"
    ("s" "Search repos" forgejo-repo-search)
    ("i" "Issues" forgejo-issue-list)
    ("p" "Pull requests" forgejo-pull-list)]
   ["Actions"
    ("b" "Browse repo" forgejo-browse-repo)
    ("q" "Quit" transient-quit-one)]])

;;; Browse

(defun forgejo-browse-repo ()
  "Open a repository in the browser."
  (interactive)
  (let ((input (read-string "Repository (owner/repo): ")))
    (if (string-match "\\`\\([^/]+\\)/\\([^/]+\\)\\'" input)
        (forgejo-utils-browse-repo (match-string 1 input)
                                   (match-string 2 input))
      (user-error "Invalid format; expected owner/repo"))))

;;; Repo search action-at-point

;;;###autoload (autoload 'forgejo-repo-action-at-point "forgejo-transient" nil t)
(transient-define-prefix forgejo-repo-action-at-point ()
  "Actions for repository at point."
  [["Open"
    ("i" "Issues" forgejo-repo-action--issues)
    ("p" "Pull requests" forgejo-repo-action--pulls)
    ("b" "Browse" forgejo-repo-action--browse)]])

(defun forgejo-repo-action--issues ()
  "List issues for the repo at point."
  (interactive)
  (when-let* ((pair (forgejo-repo-search--owner-repo-at-point)))
    (forgejo-with-host forgejo-repo--host
      (forgejo-issue-list (car pair) (cdr pair)))))

(defun forgejo-repo-action--pulls ()
  "List pull requests for the repo at point."
  (interactive)
  (when-let* ((pair (forgejo-repo-search--owner-repo-at-point)))
    (forgejo-with-host forgejo-repo--host
      (forgejo-pull-list (car pair) (cdr pair)))))

(defun forgejo-repo-action--browse ()
  "Open the repo at point in the browser."
  (interactive)
  (when-let* ((pair (forgejo-repo-search--owner-repo-at-point)))
    (forgejo-with-host forgejo-repo--host
      (forgejo-utils-browse-repo (car pair) (cdr pair)))))

;;; Issue detail actions

(declare-function forgejo-issue-view-browse "forgejo-issue.el" ())
(declare-function forgejo-issue-comment "forgejo-issue.el" ())

;;;###autoload (autoload 'forgejo-issue-actions "forgejo-transient" nil t)
(transient-define-prefix forgejo-issue-actions ()
  "Actions for the current issue."
  [["Issue"
    ("c" "Comment" forgejo-issue-comment)
    ("b" "Browse in browser" forgejo-issue-view-browse)]])

;;; PR detail actions

(declare-function forgejo-pull-view-browse "forgejo-pull.el" ())
(declare-function forgejo-pull-comment "forgejo-pull.el" ())

;;;###autoload (autoload 'forgejo-pull-actions "forgejo-transient" nil t)
(transient-define-prefix forgejo-pull-actions ()
  "Actions for the current pull request."
  [["Pull Request"
    ("c" "Comment" forgejo-pull-comment)
    ("f" "Fetch branch" forgejo-pull-view-fetch)
    ("l" "Commit log" forgejo-pull-view-log)
    ("b" "Browse in browser" forgejo-pull-view-browse)]])

(provide 'forgejo-transient)
;;; forgejo-transient.el ends here

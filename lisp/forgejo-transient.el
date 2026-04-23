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
(declare-function forgejo-repo-create "forgejo-repo.el" (name))

(declare-function forgejo-notification-list "forgejo-notification.el" ())

(defvar forgejo-host)
(defvar forgejo-repo--host)

;;; Top-level menu

;;;###autoload (autoload 'forgejo "forgejo-transient" nil t)
(transient-define-prefix forgejo ()
  "Forgejo."
  [["Navigate"
    ("s" "Search repos" forgejo-repo-search)
    ("i" "Issues" forgejo-issue-list)
    ("p" "Pull requests" forgejo-pull-list)
    ("n" "Notifications" forgejo-notification-list)]
   ["Actions"
    ("c" "Create repo" forgejo-repo-create)
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
(declare-function forgejo-issue-view-refresh "forgejo-issue.el" ())
(declare-function forgejo-issue-comment "forgejo-issue.el" ())
(declare-function forgejo-issue-reply "forgejo-issue.el" ())
(declare-function forgejo-issue-toggle-state "forgejo-issue.el" ())
(declare-function forgejo-issue-edit "forgejo-issue.el" ())
(declare-function forgejo-issue-add-label "forgejo-issue.el" ())
(declare-function forgejo-issue-remove-label "forgejo-issue.el" ())
(declare-function forgejo-issue-add-assignee "forgejo-issue.el" ())
(declare-function forgejo-issue-remove-assignee "forgejo-issue.el" ())
(declare-function forgejo-issue-set-milestone "forgejo-issue.el" ())

;;;###autoload (autoload 'forgejo-issue-add-metadata "forgejo-transient" nil t)
(transient-define-prefix forgejo-issue-add-metadata ()
  "Add metadata to the current issue."
  [["Add"
    ("l" "Label" forgejo-issue-add-label)
    ("a" "Assignee" forgejo-issue-add-assignee)
    ("m" "Milestone" forgejo-issue-set-milestone)]])

;;;###autoload (autoload 'forgejo-issue-remove-metadata "forgejo-transient" nil t)
(transient-define-prefix forgejo-issue-remove-metadata ()
  "Remove metadata from the current issue."
  [["Remove"
    ("l" "Label" forgejo-issue-remove-label)
    ("a" "Assignee" forgejo-issue-remove-assignee)]])

;;;###autoload (autoload 'forgejo-issue-actions "forgejo-transient" nil t)
(transient-define-prefix forgejo-issue-actions ()
  "Actions for the current issue."
  [["Actions"
    ("c" "Comment" forgejo-issue-comment)
    ("r" "Reply at point" forgejo-issue-reply)
    ("e" "Edit at point" forgejo-issue-edit)
    ("x" "Toggle open/close" forgejo-issue-toggle-state)]
   ["Metadata"
    ("a" "Add metadata" forgejo-issue-add-metadata)
    ("d" "Remove metadata" forgejo-issue-remove-metadata)]
   ["Navigate"
    ("g" "Refresh" forgejo-issue-view-refresh)
    ("b" "Open in browser" forgejo-issue-view-browse)
    ("q" "Quit" quit-window)]])

;;; PR detail actions

(declare-function forgejo-pull-view-browse "forgejo-pull.el" ())
(declare-function forgejo-pull-view-refresh "forgejo-pull.el" ())
(declare-function forgejo-pull-comment "forgejo-pull.el" ())
(declare-function forgejo-pull-reply "forgejo-pull.el" ())
(declare-function forgejo-pull-toggle-state "forgejo-pull.el" ())
(declare-function forgejo-pull-edit "forgejo-pull.el" ())
(declare-function forgejo-review-submit "forgejo-review.el" ())
(declare-function forgejo-pull-view-diff "forgejo-pull.el" ())
(declare-function forgejo-pull-add-label "forgejo-pull.el" ())
(declare-function forgejo-pull-remove-label "forgejo-pull.el" ())
(declare-function forgejo-pull-add-assignee "forgejo-pull.el" ())
(declare-function forgejo-pull-remove-assignee "forgejo-pull.el" ())
(declare-function forgejo-pull-set-milestone "forgejo-pull.el" ())

;;;###autoload (autoload 'forgejo-pull-add-metadata "forgejo-transient" nil t)
(transient-define-prefix forgejo-pull-add-metadata ()
  "Add metadata to the current pull request."
  [["Add"
    ("l" "Label" forgejo-pull-add-label)
    ("a" "Assignee" forgejo-pull-add-assignee)
    ("m" "Milestone" forgejo-pull-set-milestone)]])

;;;###autoload (autoload 'forgejo-pull-remove-metadata "forgejo-transient" nil t)
(transient-define-prefix forgejo-pull-remove-metadata ()
  "Remove metadata from the current pull request."
  [["Remove"
    ("l" "Label" forgejo-pull-remove-label)
    ("a" "Assignee" forgejo-pull-remove-assignee)]])

;;;###autoload (autoload 'forgejo-pull-actions "forgejo-transient" nil t)
(transient-define-prefix forgejo-pull-actions ()
  "Actions for the current pull request."
  [["Actions"
    ("c" "Comment" forgejo-pull-comment)
    ("r" "Reply at point" forgejo-pull-reply)
    ("e" "Edit at point" forgejo-pull-edit)
    ("R" "Submit review" forgejo-review-submit)
    ("x" "Toggle open/close" forgejo-pull-toggle-state)]
   ["Metadata"
    ("a" "Add metadata" forgejo-pull-add-metadata)
    ("d" "Remove metadata" forgejo-pull-remove-metadata)]
   ["Navigate"
    ("=" "PR diff" forgejo-pull-view-diff)
    ("f" "Fetch branch" forgejo-pull-view-fetch)
    ("l" "Commit log" forgejo-pull-view-log)
    ("g" "Refresh" forgejo-pull-view-refresh)
    ("b" "Open in browser" forgejo-pull-view-browse)
    ("q" "Quit" quit-window)]])

(provide 'forgejo-transient)
;;; forgejo-transient.el ends here

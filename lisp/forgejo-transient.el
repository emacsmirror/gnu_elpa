;;; forgejo-transient.el --- Popup menus for Forgejo  -*- lexical-binding: t; -*-

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

;; Popup menus for Forgejo.  Provides the top-level `forgejo'
;; entry point and the action-at-point dispatch for repo search results.

;;; Code:

(require 'keymap-popup)
(require 'forgejo)
(require 'forgejo-utils)

(declare-function forgejo-repo-search "forgejo-repo.el" (query))
(declare-function forgejo-issue-list "forgejo-issue.el"
                  (&optional owner repo))
(declare-function forgejo-pull-list "forgejo-pull.el"
                  (&optional owner repo))
(declare-function forgejo-repo-search--owner-repo-at-point "forgejo-repo.el" ())
(declare-function forgejo-repo-create "forgejo-repo.el" (name))
(declare-function forgejo-watch-list "forgejo-watch.el" ())

(defvar forgejo-repo--host)

;;; Top-level menu

(keymap-popup-define forgejo-map
  "Forgejo."
  :group "Navigate"
  "s" ("Search repos" forgejo-repo-search)
  "i" ("Issues" forgejo-issue-list)
  "p" ("Pull requests" forgejo-pull-list)
  "n" ("Watch" forgejo-watch-list)
  :group "Actions"
  "c" ("Create repo" forgejo-repo-create)
  "b" ("Browse repo" forgejo-browse-repo))

;;;###autoload
(defun forgejo ()
  "Forgejo."
  (interactive)
  (keymap-popup 'forgejo-map))

;;; Browse

(defun forgejo-browse-repo ()
  "Open a repository in the browser."
  (interactive)
  (let ((input (read-string "Repository (owner/repo): ")))
    (if (string-match "\\`\\([^/]+\\)/\\([^/]+\\)\\'" input)
        (let ((host-url (or forgejo-repo--host (forgejo--resolve-host))))
          (forgejo-utils-browse-repo host-url
                                     (match-string 1 input)
                                     (match-string 2 input)))
      (user-error "Invalid format; expected owner/repo"))))

;;; Repo search action-at-point

(keymap-popup-define forgejo-repo-action-map
  "Actions for repository at point."
  :group "Open"
  "i" ("Issues" forgejo-repo-action--issues)
  "p" ("Pull requests" forgejo-repo-action--pulls)
  "b" ("Browse" forgejo-repo-action--browse))

;;;###autoload
(defun forgejo-repo-action-at-point ()
  "Actions for repository at point."
  (interactive)
  (keymap-popup 'forgejo-repo-action-map))

(defun forgejo-repo-action--issues ()
  "List issues for the repo at point."
  (interactive)
  (when-let* ((pair (forgejo-repo-search--owner-repo-at-point)))
    (forgejo-issue-list (car pair) (cdr pair))))

(defun forgejo-repo-action--pulls ()
  "List pull requests for the repo at point."
  (interactive)
  (when-let* ((pair (forgejo-repo-search--owner-repo-at-point)))
    (forgejo-pull-list (car pair) (cdr pair))))

(defun forgejo-repo-action--browse ()
  "Open the repo at point in the browser."
  (interactive)
  (when-let* ((pair (forgejo-repo-search--owner-repo-at-point)))
    (forgejo-utils-browse-repo forgejo-repo--host (car pair) (cdr pair))))

(provide 'forgejo-transient)
;;; forgejo-transient.el ends here

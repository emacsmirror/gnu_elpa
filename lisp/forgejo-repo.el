;;; forgejo-repo.el --- Repository context for Forgejo  -*- lexical-binding: t; -*-

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

;; Repository selection and context management.  Provides buffer-local
;; variables for the current owner/repo and interactive completion for
;; choosing a repository.

;;; Code:

(require 'cl-lib)
(require 'url-parse)
(require 'keymap-popup)
(require 'forgejo)
(require 'forgejo-utils)
(require 'forgejo-api)
(require 'forgejo-vc)
(require 'forgejo-db)
(require 'forgejo-buffer)
(require 'forgejo-tl)

(declare-function forgejo-issue-list "forgejo-issue.el"
                  (&optional owner repo))

;;; Buffer-local state

(defvar-local forgejo-repo--host nil
  "Buffer-local base URL of the Forgejo instance (e.g. \"https://codeberg.org\").")

(defvar-local forgejo-repo--owner nil
  "Buffer-local owner of the current repository.")

(defvar-local forgejo-repo--name nil
  "Buffer-local name of the current repository.")

;;; Interactive selection

(defun forgejo-repo-read ()
  "Prompt for OWNER/REPO with completion from cached repos.
Triggers async fetch of user repos on first call.
Returns (HOST OWNER REPO) where HOST is the resolved instance URL."
  (unless forgejo-repo--user-repos-fetched
    (forgejo-repo--fetch-user-repos))
  (let* ((cached (forgejo-repo--cached-names))
         (git-context (forgejo-vc--repo-from-remote))
         (default (when git-context
                    (format "%s/%s" (nth 1 git-context) (nth 2 git-context))))
         (input (completing-read
                 (if default
                     (format "Repository (default %s): " default)
                   "Repository (owner/repo): ")
                 cached nil nil nil nil default)))
    (if (string-match "\\`\\([^/]+\\)/\\([^/]+\\)\\'" input)
        (let* ((owner (match-string 1 input))
               (repo (match-string 2 input))
               (host (cond
                      ((and git-context (string= input default))
                       (nth 0 git-context))
                      (t (forgejo--resolve-host-for-repo owner repo)))))
          (list host owner repo))
      (user-error "Invalid format; expected owner/repo"))))

(defvar forgejo-repo--user-repos-fetched nil
  "Non-nil if user repos have been fetched this session.")

(defun forgejo-repo--cached-names ()
  "Return a list of \"owner/repo\" strings from the cache.
Includes user repos and repos from previously viewed issues.
When no buffer-local host is set, aggregates across all hosts."
  (condition-case nil
      (if forgejo-repo--host
          (forgejo-repo--cached-names-for-host
           (url-host (url-generic-parse-url forgejo-repo--host)))
        (delete-dups
         (cl-mapcan
          (lambda (entry)
            (forgejo-repo--cached-names-for-host
             (url-host (url-generic-parse-url (car entry)))))
          forgejo-hosts)))
    (error nil)))

(defun forgejo-repo--cached-names-for-host (host)
  "Return \"owner/repo\" strings from the cache for HOST."
  (delete-dups
   (append
    (forgejo-db-get-user-repos host)
    (mapcar (lambda (row) (format "%s/%s" (nth 0 row) (nth 1 row)))
            (forgejo-db--select
             "SELECT DISTINCT owner, repo FROM issues WHERE host = ?"
             (list host))))))

;;; Fetch user repos

(defun forgejo-repo--fetch-user-repos ()
  "Fetch repositories for the authenticated user on all configured hosts."
  (dolist (entry forgejo-hosts)
    (let* ((host-url (car entry))
           (host (url-host (url-generic-parse-url host-url))))
      (condition-case nil
          (forgejo-api-get
           host-url "user/repos"
           '(("limit" . "50") ("sort" . "updated"))
           (lambda (data _headers)
             (forgejo-db-save-user-repos host data)))
        (error nil))))
  (setq forgejo-repo--user-repos-fetched t))

;;; Repository search view

(defvar-local forgejo-repo-search--query nil
  "Current search query string.")

(defvar-local forgejo-repo-search--page nil
  "Current page number.")

(declare-function forgejo-repo-action-at-point "forgejo.el" ())

(keymap-popup-define forgejo-repo-search-mode-map
  "Forgejo repository search."
  :parent tabulated-list-mode-map
  :group "Actions"
  "RET" ("Actions" forgejo-repo-action-at-point)
  "i" ("Issues" forgejo-repo-search-issues-at-point)
  "P" ("Pull requests" forgejo-repo-search-pulls-at-point)
  "b" ("Browse" forgejo-repo-search-browse-at-point)
  :group "Navigate"
  "g" ("Refresh" forgejo-repo-search-refresh)
  "n" ("Next page" forgejo-repo-search-next-page)
  "p" ("Previous page" forgejo-repo-search-prev-page))

(define-derived-mode forgejo-repo-search-mode tabulated-list-mode
  "Forgejo Repos"
  "Major mode for browsing Forgejo repository search results."
  :group 'forgejo
  (setq tabulated-list-padding 1
        tabulated-list-format
        (vector `("Name" 20 t)
                `("Owner" 15 t)
                `("Stars" 5 t :right-align t)
                `("Issues" 6 t :right-align t)
                `("Lang" 10 t)
                `("Updated" 12 t)
                `("Description" ,(/ (window-width) 3) nil)))
  (tabulated-list-init-header)
  (run-hook-with-args 'forgejo-buffer-setup-functions (current-buffer)))

(defun forgejo-repo-search--entries (repos)
  "Convert REPOS (list of API alists) to `tabulated-list-entries'."
  (mapcar
   (lambda (repo)
     (let-alist repo
       (list (cons (alist-get 'login .owner) .name)
             (vector
              (propertize .name 'face 'font-lock-keyword-face)
              (propertize (or (alist-get 'login .owner) "")
                          'face 'forgejo-comment-author-face)
              (number-to-string (or .stars_count 0))
              (number-to-string (or .open_issues_count 0))
              (propertize (or .language "") 'face 'font-lock-type-face)
              (propertize (forgejo-buffer--relative-time .updated_at)
                          'face 'shadow)
              (propertize (or .description "") 'face 'font-lock-doc-face)))))
   repos))

;;;###autoload
(defun forgejo-repo-search (query)
  "Search Forgejo repositories matching QUERY."
  (interactive "sSearch repos: ")
  (let* ((host-url (forgejo--resolve-host))
         (buf-name (format "*forgejo-repos: %s*" query)))
    (forgejo-api-get
     host-url "repos/search"
     (list (cons "q" query)
           (cons "sort" "updated")
           (cons "order" "desc")
           (cons "limit" (number-to-string (forgejo-api-default-limit))))
     (lambda (data _headers)
       (let* ((repos (if (and (listp data) (alist-get 'data data))
                         (alist-get 'data data)
                       data))
              (entries (forgejo-repo-search--entries repos)))
         (with-current-buffer (get-buffer-create buf-name)
           (forgejo-repo-search-mode)
           (setq forgejo-repo--host host-url
                 forgejo-repo-search--query query
                 forgejo-repo-search--page 1
                 tabulated-list-entries entries)
           (forgejo-tl-print)
           (goto-char (point-min))
           (switch-to-buffer (current-buffer))))))))

(defun forgejo-repo-search-refresh ()
  "Refresh the current repo search."
  (interactive)
  (when forgejo-repo-search--query
    (forgejo-repo-search forgejo-repo-search--query)))

(defun forgejo-repo-search--owner-repo-at-point ()
  "Return (OWNER . REPO) for the entry at point."
  (when-let* ((id (tabulated-list-get-id)))
    id))

(defun forgejo-repo-search-issues-at-point ()
  "List issues for the repo at point."
  (interactive)
  (when-let* ((pair (forgejo-repo-search--owner-repo-at-point)))
    (forgejo-issue-list (car pair) (cdr pair))))

(defun forgejo-repo-search-pulls-at-point ()
  "List pull requests for the repo at point."
  (interactive)
  (when-let* ((pair (forgejo-repo-search--owner-repo-at-point)))
    (declare-function forgejo-pull-list "forgejo-pull.el"
                      (&optional owner repo))
    (forgejo-pull-list (car pair) (cdr pair))))

(defun forgejo-repo-search-browse-at-point ()
  "Open the repo at point in the browser."
  (interactive)
  (when-let* ((pair (forgejo-repo-search--owner-repo-at-point)))
    (forgejo-utils-browse-repo forgejo-repo--host (car pair) (cdr pair))))

(defun forgejo-repo-search-next-page ()
  "Load the next page of search results."
  (interactive)
  (let ((page (or forgejo-repo-search--page 1)))
    (setq forgejo-repo-search--page (1+ page))
    (forgejo-api-get
     forgejo-repo--host "repos/search"
     (list (cons "q" forgejo-repo-search--query)
           (cons "sort" "updated")
           (cons "order" "desc")
           (cons "page" (number-to-string forgejo-repo-search--page))
           (cons "limit" (number-to-string (forgejo-api-default-limit))))
     (lambda (data _headers)
       (let* ((repos (if (and (listp data) (alist-get 'data data))
                         (alist-get 'data data)
                       data))
              (entries (forgejo-repo-search--entries repos)))
         (setq tabulated-list-entries entries)
         (forgejo-tl-print)
         (goto-char (point-min)))))))

(defun forgejo-repo-search-prev-page ()
  "Load the previous page of search results."
  (interactive)
  (when (and forgejo-repo-search--page (> forgejo-repo-search--page 1))
    (setq forgejo-repo-search--page (1- forgejo-repo-search--page))
    (forgejo-api-get
     forgejo-repo--host "repos/search"
     (list (cons "q" forgejo-repo-search--query)
           (cons "sort" "updated")
           (cons "order" "desc")
           (cons "page" (number-to-string forgejo-repo-search--page))
           (cons "limit" (number-to-string (forgejo-api-default-limit))))
     (lambda (data _headers)
       (let* ((repos (if (and (listp data) (alist-get 'data data))
                         (alist-get 'data data)
                       data))
              (entries (forgejo-repo-search--entries repos)))
         (setq tabulated-list-entries entries)
         (forgejo-tl-print)
         (goto-char (point-min)))))))

;;; Repository creation

;;;###autoload
(defun forgejo-repo-create (name)
  "Create a new repository named NAME on the current Forgejo instance."
  (interactive "sRepository name: ")
  (let ((host-url (forgejo--resolve-host)))
    (forgejo-utils-create-repo host-url name)))

(provide 'forgejo-repo)
;;; forgejo-repo.el ends here

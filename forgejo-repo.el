;;; forgejo-repo.el --- Repository context for Forgejo  -*- lexical-binding: t; -*-

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

;; Repository selection and context management.  Provides buffer-local
;; variables for the current owner/repo and interactive completion for
;; choosing a repository.

;;; Code:

(require 'cl-lib)
(require 'url-parse)
(require 'forgejo)
(require 'forgejo-utils)

(declare-function forgejo-api-get "forgejo-api.el"
                  (endpoint &optional params callback))
(declare-function forgejo-api-default-limit "forgejo-api.el" ())
(declare-function forgejo-db-save-issues "forgejo-db.el"
                  (host owner repo issues))
(declare-function forgejo-db--select "forgejo-db.el" (sql &rest args))
(declare-function forgejo-db--ensure "forgejo-db.el" ())
(declare-function forgejo-issue-list "forgejo-issue.el"
                  (&optional owner repo))
(declare-function forgejo-buffer--relative-time "forgejo-buffer.el"
                  (time-string))
(declare-function forgejo-buffer--flex-width "forgejo-buffer.el"
                  (fixed-total &optional min-width))
(declare-function forgejo-tl-print "forgejo-tl.el"
                  (&optional remember-pos))

(defvar forgejo-host)

;;; Buffer-local state

(defvar-local forgejo-repo--host nil
  "Buffer-local base URL of the Forgejo instance (e.g. \"https://codeberg.org\").")

(defvar-local forgejo-repo--owner nil
  "Buffer-local owner of the current repository.")

(defvar-local forgejo-repo--name nil
  "Buffer-local name of the current repository.")

;;; Repository detection

(declare-function forgejo-vc--repo-from-remote "forgejo-vc.el" ())

;;; Interactive selection

(defun forgejo-repo-read ()
  "Prompt for OWNER/REPO with completion from cached repos.
Triggers async fetch of user repos on first call.
Returns (HOST OWNER REPO) where HOST is the detected instance
URL or nil when entered manually."
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
        (list (and git-context
                   (string= input default)
                   (nth 0 git-context))
              (match-string 1 input)
              (match-string 2 input))
      (user-error "Invalid format; expected owner/repo"))))

(declare-function forgejo-db-get-user-repos "forgejo-db.el" (host))
(declare-function forgejo-db-save-user-repos "forgejo-db.el" (host repos))

(defvar forgejo-repo--user-repos-fetched nil
  "Non-nil if user repos have been fetched this session.")

(defun forgejo-repo--cached-names ()
  "Return a list of \"owner/repo\" strings from the cache.
Includes user repos and repos from previously viewed issues."
  (let ((host (url-host (url-generic-parse-url
                         (or forgejo-repo--host forgejo-host)))))
    (condition-case nil
        (delete-dups
         (append
          (forgejo-db-get-user-repos host)
          (mapcar (lambda (row)
                    (format "%s/%s" (nth 0 row) (nth 1 row)))
                  (forgejo-db--select
                   "SELECT DISTINCT owner, repo FROM issues WHERE host = ?"
                   (list host)))))
      (error nil))))

;;; Fetch user repos

(defun forgejo-repo--fetch-user-repos ()
  "Fetch repositories for the authenticated user and cache them."
  (let ((host (url-host (url-generic-parse-url
                         (or forgejo-repo--host forgejo-host)))))
    (forgejo-api-get
     "user/repos"
     '(("limit" . "50") ("sort" . "updated"))
     (lambda (data _headers)
       (forgejo-db-save-user-repos host data)
       (setq forgejo-repo--user-repos-fetched t)))))

;;; ---- Repository search view ----

(defvar-local forgejo-repo-search--query nil
  "Current search query string.")

(defvar-local forgejo-repo-search--page nil
  "Current page number.")

(declare-function forgejo-repo-action-at-point "forgejo-transient.el" ())

(defvar forgejo-repo-search-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'forgejo-repo-action-at-point)
    (define-key map (kbd "i") #'forgejo-repo-search-issues-at-point)
    (define-key map (kbd "P") #'forgejo-repo-search-pulls-at-point)
    (define-key map (kbd "g") #'forgejo-repo-search-refresh)
    (define-key map (kbd "b") #'forgejo-repo-search-browse-at-point)
    (define-key map (kbd "n") #'forgejo-repo-search-next-page)
    (define-key map (kbd "p") #'forgejo-repo-search-prev-page)
    map)
  "Keymap for `forgejo-repo-search-mode'.")


(define-derived-mode forgejo-repo-search-mode tabulated-list-mode
  "Forgejo Repos"
  "Major mode for browsing Forgejo repository search results."
  :group 'forgejo
  (setq tabulated-list-padding 1
        tabulated-list-format
        (vector '("Name" 20 t)
                '("Owner" 15 t)
                '("Stars" 5 t :right-align t)
                '("Issues" 6 t :right-align t)
                '("Lang" 10 t)
                '("Updated" 12 t)
                (list "Description" (forgejo-buffer--flex-width 75) nil)))
  (tabulated-list-init-header))

(defun forgejo-repo-search--entries (repos)
  "Convert REPOS (list of API alists) to `tabulated-list-entries'."
  (mapcar
   (lambda (repo)
     (let-alist repo
       (list (cons (alist-get 'login .owner) .name)
             (vector
              .name
              (or (alist-get 'login .owner) "")
              (number-to-string (or .stars_count 0))
              (number-to-string (or .open_issues_count 0))
              (or .language "")
              (forgejo-buffer--relative-time .updated_at)
              (or .description "")))))
   repos))

;;;###autoload
(defun forgejo-repo-search (query)
  "Search Forgejo repositories matching QUERY."
  (interactive "sSearch repos: ")
  (let ((buf-name (format "*forgejo-repos: %s*" query)))
    (forgejo-api-get
     "repos/search"
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
           (setq forgejo-repo--host forgejo-host
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
    (forgejo-with-host forgejo-repo--host
      (forgejo-repo-search forgejo-repo-search--query))))

(defun forgejo-repo-search--owner-repo-at-point ()
  "Return (OWNER . REPO) for the entry at point."
  (when-let* ((id (tabulated-list-get-id)))
    id))

(defun forgejo-repo-search-issues-at-point ()
  "List issues for the repo at point."
  (interactive)
  (when-let* ((pair (forgejo-repo-search--owner-repo-at-point)))
    (forgejo-with-host forgejo-repo--host
      (forgejo-issue-list (car pair) (cdr pair)))))

(defun forgejo-repo-search-pulls-at-point ()
  "List pull requests for the repo at point."
  (interactive)
  (when-let* ((pair (forgejo-repo-search--owner-repo-at-point)))
    (declare-function forgejo-pull-list "forgejo-pull.el"
                      (&optional owner repo))
    (forgejo-with-host forgejo-repo--host
      (forgejo-pull-list (car pair) (cdr pair)))))

(defun forgejo-repo-search-browse-at-point ()
  "Open the repo at point in the browser."
  (interactive)
  (when-let* ((pair (forgejo-repo-search--owner-repo-at-point)))
    (forgejo-with-host forgejo-repo--host
      (forgejo-utils-browse-repo (car pair) (cdr pair)))))

(defun forgejo-repo-search-next-page ()
  "Load the next page of search results."
  (interactive)
  (let ((page (or forgejo-repo-search--page 1)))
    (setq forgejo-repo-search--page (1+ page))
    (forgejo-with-host forgejo-repo--host
      (forgejo-api-get
       "repos/search"
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
           (goto-char (point-min))))))))

(defun forgejo-repo-search-prev-page ()
  "Load the previous page of search results."
  (interactive)
  (when (and forgejo-repo-search--page (> forgejo-repo-search--page 1))
    (setq forgejo-repo-search--page (1- forgejo-repo-search--page))
    (forgejo-with-host forgejo-repo--host
      (forgejo-api-get
       "repos/search"
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
           (goto-char (point-min))))))))

(provide 'forgejo-repo)
;;; forgejo-repo.el ends here

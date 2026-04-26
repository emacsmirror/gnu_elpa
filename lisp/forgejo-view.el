;;; forgejo-view.el --- Shared detail-view layer for issues and PRs  -*- lexical-binding: t; -*-

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

;; Shared buffer-locals, render functions, and action commands for issue
;; and PR detail views.  Both `forgejo-issue-view-mode' and
;; `forgejo-pull-view-mode' set the buffer-locals defined here, so
;; action commands can be written once.

;;; Code:

(require 'cl-lib)
(require 'ewoc)
(require 'url-parse)
(require 'keymap-popup)
(require 'forgejo)
(require 'forgejo-tl)
(require 'forgejo-buffer)
(require 'forgejo-filter)
(require 'forgejo-utils)
(require 'forgejo-api)
(require 'forgejo-db)

(defvar forgejo-repo--host)
(defvar forgejo-repo--owner)
(defvar forgejo-repo--name)

;;; Shared buffer-locals

(defvar-local forgejo-view--data nil
  "Item alist for the current detail view (issue or PR).")

(defvar-local forgejo-view--ewoc nil
  "EWOC instance for the current detail view.")

(defvar-local forgejo-view--sync-fn nil
  "Function to sync the current item from the API.
Called with (HOST OWNER REPO NUMBER BUF-NAME &optional RESTORE-LINE).")

(defvar-local forgejo-view--browse-fn nil
  "Function to open the current item in the browser.
Called with (HOST-URL OWNER REPO NUMBER).")

;;; Shared view keymap

(keymap-popup-define forgejo-view-mode-map
  "Forgejo detail view."
  :parent special-mode-map
  :group "Actions"
  "c" ("Comment" forgejo-view-comment)
  "e" ("Edit at point" forgejo-view-edit)
  "x" ("Toggle open/close" forgejo-view-toggle-state)
  "D" ("Delete at point" forgejo-view-delete-at-point)
  :group "Metadata"
  "L" ("Add label" forgejo-view-add-label)
  "A" ("Add assignee" forgejo-view-add-assignee)
  "M" ("Set milestone" forgejo-view-set-milestone)
  :group "Navigate"
  "RET" ("Follow link" forgejo-buffer-follow-link)
  "g" ("Refresh" forgejo-view-refresh)
  "b" ("Open in browser" forgejo-view-browse)
  "n" ("Next" ewoc-goto-next)
  "p" ("Previous" ewoc-goto-prev))

;;; Node access

(defun forgejo-view--node-at-point ()
  "Return the data plist of the EWOC node at point, or nil."
  (forgejo-buffer--node-at-point forgejo-view--ewoc))

;;; Item routing

(declare-function forgejo-issue-view "forgejo-issue.el"
                  (owner repo number))
(declare-function forgejo-pull-view "forgejo-pull.el"
                  (owner repo number))

(defun forgejo-view-item (owner repo number)
  "View issue or PR NUMBER in OWNER/REPO.
Checks the DB to determine if it's a PR, falls back to issue view."
  (let ((cached (forgejo-db-get-issue
                 (url-host (url-generic-parse-url forgejo-repo--host))
                 owner repo number)))
    (if (and cached (alist-get 'pull_request cached))
        (forgejo-pull-view owner repo number)
      (forgejo-issue-view owner repo number))))

;;; List-view format

(defun forgejo-view--list-format (columns)
  "Build a `tabulated-list-format' vector from COLUMNS.
Each element of COLUMNS is (NAME WIDTH-OR-RATIO SORT . PROPS).
Float widths are multiplied by `window-width'."
  (let ((w (window-width)))
    (apply #'vector
           (mapcar (lambda (col)
                     (let* ((name (car col))
                            (width-spec (nth 1 col))
                            (sort (nth 2 col))
                            (props (nthcdr 3 col))
                            (width (if (floatp width-spec)
                                       (truncate (* w width-spec))
                                     width-spec)))
                       (append (list name width sort) props)))
                   columns))))

;;; List-view rendering

(defun forgejo-view--render-from-db (buf-name host-url host owner repo
                                     filters query-fn list-mode)
  "Render cached items into BUF-NAME from the DB.
HOST-URL is the full instance URL.  HOST is the hostname string.
OWNER, REPO identify the repository.  FILTERS is the filter plist.
QUERY-FN is called with (HOST OWNER REPO FILTERS) to fetch rows.
LIST-MODE is the major mode symbol for the list buffer."
  (let ((entries (forgejo-filter-list-entries
                  (funcall query-fn host owner repo filters))))
    (with-current-buffer (get-buffer-create buf-name)
      (unless (derived-mode-p list-mode)
        (funcall list-mode))
      (setq forgejo-repo--host host-url
            forgejo-repo--owner owner
            forgejo-repo--name repo
            tabulated-list-format (forgejo-view--list-format
                                   forgejo-filter-list-columns)
            tabulated-list-entries entries)
      (unless tabulated-list-sort-key
        (setq tabulated-list-sort-key '("#" . t)))
      (tabulated-list-init-header)
      (forgejo-tl-print t)
      (current-buffer))))

;;; Detail-view rendering

(defun forgejo-view--render-detail (buf-name host-url owner repo item-alist
                                    timeline-alists view-mode-fn
                                    sync-fn browse-fn)
  "Render detail view into BUF-NAME from alist data.
HOST-URL is the full instance URL.  ITEM-ALIST is the issue or PR data.
TIMELINE-ALISTS is the timeline.  VIEW-MODE-FN activates the major mode.
SYNC-FN and BROWSE-FN are stored as buffer-locals for action commands."
  (let ((nodes (forgejo-buffer--build-nodes item-alist timeline-alists)))
    (with-current-buffer (get-buffer-create buf-name)
      (let ((inhibit-read-only t))
        (erase-buffer))
      (funcall view-mode-fn)
      (setq forgejo-repo--host host-url
            forgejo-repo--owner owner
            forgejo-repo--name repo
            forgejo-view--data item-alist
            forgejo-view--sync-fn sync-fn
            forgejo-view--browse-fn browse-fn)
      (setq forgejo-view--ewoc
            (ewoc-create #'forgejo-buffer--pp nil nil t))
      (dolist (node nodes)
        (ewoc-enter-last forgejo-view--ewoc node))
      (setq header-line-format
            (forgejo-buffer--header-line item-alist))
      (goto-char (point-min))
      (set-buffer-modified-p nil)
      (current-buffer))))

;;; Re-render from DB

(defun forgejo-view--re-render (buf-name host-url host owner repo number
                                render-fn &optional restore-line)
  "Re-render detail buffer BUF-NAME from fresh DB data.
HOST-URL is the instance.  HOST is the hostname.
RENDER-FN is called with (BUF-NAME HOST-URL OWNER REPO ITEM TIMELINE).
Restores point to RESTORE-LINE if given."
  (when (buffer-live-p (get-buffer buf-name))
    (let* ((issue (forgejo-db-get-issue host owner repo number))
           (tl-rows (forgejo-db-get-timeline host owner repo number))
           (tl-alists (mapcar #'forgejo-db--row-to-timeline-alist tl-rows)))
      (funcall render-fn buf-name host-url owner repo issue tl-alists)
      (when restore-line
        (with-current-buffer buf-name
          (goto-char (point-min))
          (forward-line (1- restore-line)))))))

;;; Action commands

(defun forgejo-view-toggle-state ()
  "Toggle the state of the item at point or in the current view."
  (interactive)
  (let* ((number (or (and (bound-and-true-p forgejo-view--data)
                          (alist-get 'number forgejo-view--data))
                     (tabulated-list-get-id)))
         (host-url forgejo-repo--host)
         (host (url-host (url-generic-parse-url host-url)))
         (item (forgejo-db-get-issue host forgejo-repo--owner
                                     forgejo-repo--name number))
         (state (alist-get 'state item)))
    (when (and number state)
      (forgejo-utils-toggle-state
       host-url forgejo-repo--owner forgejo-repo--name number state
       (forgejo--post-action-callback)))))

(defun forgejo-view-comment ()
  "Post a comment on the current item."
  (interactive)
  (when-let* ((data forgejo-view--data)
              (number (alist-get 'number data))
              (refresh (forgejo--post-action-callback)))
    (forgejo-utils-post-comment
     forgejo-repo--host
     (format "repos/%s/%s/issues/%d/comments"
             forgejo-repo--owner forgejo-repo--name number)
     "Comment"
     nil
     (lambda (_data _headers)
       (message "Comment posted on %s/%s#%d"
                forgejo-repo--owner forgejo-repo--name number)
       (funcall refresh)))))

(defun forgejo-view-edit ()
  "Edit the item at point.
On the header, prompts for title or body.  On a comment, edits the body."
  (interactive)
  (when-let* ((node (forgejo-view--node-at-point))
              (data forgejo-view--data)
              (number (alist-get 'number data)))
    (pcase (plist-get node :type)
      ('header
       (pcase (car (read-multiple-choice "Edit" '((?t "title") (?b "body"))))
         (?t (forgejo-utils-edit-title
              forgejo-repo--host forgejo-repo--owner forgejo-repo--name number
              (plist-get node :title)
              (forgejo--post-action-callback)))
         (?b (forgejo-utils-edit-body
              forgejo-repo--host forgejo-repo--owner forgejo-repo--name number
              (plist-get node :body)
              (forgejo--post-action-callback)))))
      ('comment
       (forgejo-utils-edit-comment
        forgejo-repo--host forgejo-repo--owner forgejo-repo--name
        (plist-get node :id) (plist-get node :body)
        (forgejo--post-action-callback)))
      (_ (user-error "No editable item at point")))))

(defun forgejo-view-add-label ()
  "Add a label to the current item."
  (interactive)
  (when-let* ((data forgejo-view--data)
              (number (alist-get 'number data))
              (host (url-host (url-generic-parse-url forgejo-repo--host))))
    (forgejo-utils-add-label
     forgejo-repo--host forgejo-repo--owner forgejo-repo--name number host
     (forgejo--post-action-callback))))

(defun forgejo-view-remove-label ()
  "Remove a label from the current item."
  (interactive)
  (when-let* ((data forgejo-view--data)
              (number (alist-get 'number data))
              (labels (alist-get 'labels data))
              (host (url-host (url-generic-parse-url forgejo-repo--host))))
    (forgejo-utils-remove-label
     forgejo-repo--host forgejo-repo--owner forgejo-repo--name number labels
     host (forgejo--post-action-callback))))

(defun forgejo-view-add-assignee ()
  "Add an assignee to the current item."
  (interactive)
  (when-let* ((data forgejo-view--data)
              (number (alist-get 'number data))
              (host (url-host (url-generic-parse-url forgejo-repo--host))))
    (forgejo-utils-add-assignee
     forgejo-repo--host forgejo-repo--owner forgejo-repo--name number
     (alist-get 'assignees data) host
     (forgejo--post-action-callback))))

(defun forgejo-view-remove-assignee ()
  "Remove an assignee from the current item."
  (interactive)
  (when-let* ((data forgejo-view--data)
              (number (alist-get 'number data))
              (assignees (alist-get 'assignees data)))
    (forgejo-utils-remove-assignee
     forgejo-repo--host forgejo-repo--owner forgejo-repo--name number
     assignees (forgejo--post-action-callback))))

(defun forgejo-view-set-milestone ()
  "Set or clear the milestone on the current item."
  (interactive)
  (when-let* ((data forgejo-view--data)
              (number (alist-get 'number data))
              (host (url-host (url-generic-parse-url forgejo-repo--host))))
    (forgejo-utils-set-milestone
     forgejo-repo--host forgejo-repo--owner forgejo-repo--name number host
     (forgejo--post-action-callback))))

(defun forgejo-view-refresh ()
  "Force sync the current detail view."
  (interactive)
  (when-let* ((data forgejo-view--data)
              (number (alist-get 'number data))
              (sync-fn forgejo-view--sync-fn))
    (let ((host (url-host (url-generic-parse-url forgejo-repo--host)))
          (line (line-number-at-pos)))
      (funcall sync-fn host forgejo-repo--owner
               forgejo-repo--name number
               (buffer-name) line))))

(defun forgejo-view-browse ()
  "Open the current item in the browser."
  (interactive)
  (when-let* ((data forgejo-view--data)
              (number (alist-get 'number data))
              (browse-fn forgejo-view--browse-fn))
    (let ((browse-url-browser-function #'browse-url-default-browser))
      (funcall browse-fn forgejo-repo--host forgejo-repo--owner
               forgejo-repo--name number))))

;;; Delete

(defun forgejo-view-delete-at-point ()
  "Delete the item at point.
On a comment, deletes the comment.  On the header, deletes the issue/PR."
  (interactive)
  (when-let* ((node (forgejo-view--node-at-point))
              (type (plist-get node :type)))
    (pcase type
      ('comment
       (forgejo-view--delete
        (format "repos/%s/%s/issues/comments/%d"
                forgejo-repo--owner forgejo-repo--name (plist-get node :id))
        (format "comment %d" (plist-get node :id))
        (forgejo--post-action-callback)))
      ('header
       (forgejo-view--delete
        (format "repos/%s/%s/issues/%d"
                forgejo-repo--owner forgejo-repo--name
                (alist-get 'number forgejo-view--data))
        (format "%s/%s#%d" forgejo-repo--owner forgejo-repo--name
                (alist-get 'number forgejo-view--data))
        (lambda () (quit-window 'kill))))
      (_ (user-error "No deletable item at point")))))

(defun forgejo-view--delete (endpoint description callback)
  "Delete ENDPOINT after confirming with DESCRIPTION.  Call CALLBACK on success."
  (when (y-or-n-p (format "Delete %s? " description))
    (forgejo-api-delete
     forgejo-repo--host endpoint nil
     (lambda (_data _headers)
       (message "Deleted %s" description)
       (when callback (funcall callback))))))

(provide 'forgejo-view)
;;; forgejo-view.el ends here

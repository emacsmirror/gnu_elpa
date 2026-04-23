;;; forgejo-utils.el --- Shared utilities for Forgejo  -*- lexical-binding: t; -*-

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

;; Non-display utilities: URL construction, browse helpers.

;;; Code:

(defvar forgejo-host)

(require 'forgejo-api)
(require 'forgejo-db)

;;; URL builders

(defun forgejo-utils-repo-url (owner repo)
  "Return the web URL for OWNER/REPO."
  (format "%s/%s/%s" forgejo-host owner repo))

(defun forgejo-utils-issue-url (owner repo number)
  "Return the web URL for issue NUMBER in OWNER/REPO."
  (format "%s/%s/%s/issues/%d" forgejo-host owner repo number))

(defun forgejo-utils-pull-url (owner repo number)
  "Return the web URL for pull request NUMBER in OWNER/REPO."
  (format "%s/%s/%s/pulls/%d" forgejo-host owner repo number))

;;; Browse helpers

(defun forgejo-utils-browse-repo (owner repo)
  "Open OWNER/REPO in the browser."
  (browse-url (forgejo-utils-repo-url owner repo)))

(defun forgejo-utils-browse-issue (owner repo number)
  "Open issue NUMBER of OWNER/REPO in the browser."
  (browse-url (forgejo-utils-issue-url owner repo number)))

(defun forgejo-utils-browse-pull (owner repo number)
  "Open pull request NUMBER of OWNER/REPO in the browser."
  (browse-url (forgejo-utils-pull-url owner repo number)))

;;; State toggle

(defun forgejo-utils-toggle-state (owner repo number current-state callback)
  "Toggle issue/PR NUMBER in OWNER/REPO between open and closed.
CURRENT-STATE is \"open\" or \"closed\".  CALLBACK is called on success."
  (let* ((new-state (if (string= current-state "open") "closed" "open"))
         (action (if (string= new-state "closed") "Close" "Reopen")))
    (when (y-or-n-p (format "%s %s/%s#%d? " action owner repo number))
      (forgejo-api-patch
       (format "repos/%s/%s/issues/%d" owner repo number)
       `((state . ,new-state))
       (lambda (_data _headers)
         (message "%sd %s/%s#%d" action owner repo number)
         (when callback (funcall callback)))))))

;;; Comment

(defun forgejo-utils-comment (owner repo number)
  "Post a comment on issue/PR NUMBER in OWNER/REPO.
Opens a buffer for composing the comment body."
  (let ((body (read-string-from-buffer "Comment" "")))
    (when (and body (not (string-empty-p (string-trim body))))
      (forgejo-api-post
       (format "repos/%s/%s/issues/%d/comments" owner repo number)
       nil
       `((body . ,body))
       (lambda (_data _headers)
         (message "Comment posted on %s/%s#%d" owner repo number))))))

;;; Issue creation

(defun forgejo-utils-get-issue-templates (owner repo)
  "Fetch issue templates for OWNER/REPO synchronously.
Returns a list of alists with `name' and `content' keys, or nil."
  (let ((url-request-method "GET")
        (url-request-extra-headers
         `(("Authorization" . ,(encode-coding-string
                                (concat "token " (forgejo-token)) 'ascii))
           ("Accept" . "application/json"))))
    (condition-case nil
        (with-current-buffer
            (url-retrieve-synchronously
             (format "%s/api/v1/repos/%s/%s/issue_templates"
                     forgejo-host owner repo)
             t)
          (goto-char (point-min))
          (re-search-forward "\r?\n\r?\n" nil t)
          (let ((data (json-parse-buffer :object-type 'alist
                                         :array-type 'list)))
            (kill-buffer (current-buffer))
            (when (listp data) data)))
      (error nil))))

(defun forgejo-utils-create-issue (owner repo)
  "Create a new issue in OWNER/REPO.
Fetches templates if available, lets user pick one, then compose."
  (let* ((templates (forgejo-utils-get-issue-templates owner repo))
         (template-content
          (when templates
            (let* ((names (mapcar (lambda (tmpl) (alist-get 'name tmpl)) templates))
                   (choice (completing-read "Template (or empty for none): "
                                            names nil nil)))
              (when (not (string-empty-p choice))
                (alist-get 'content
                           (cl-find choice templates
                                    :key (lambda (tmpl) (alist-get 'name tmpl))
                                    :test #'string=))))))
         (title (read-string "Issue title: "))
         (body (read-string-from-buffer "Issue body"
                                        (or template-content ""))))
    (when (and title (not (string-empty-p (string-trim title))))
      (forgejo-api-post
       (format "repos/%s/%s/issues" owner repo)
       nil
       `((title . ,title)
         ,@(when (and body (not (string-empty-p (string-trim body))))
             `((body . ,body))))
       (lambda (_data _headers)
         (message "Issue created: %s/%s \"%s\"" owner repo title))))))

;;; Label creation

(defun forgejo-utils-create-label (owner repo host callback)
  "Create a new label in OWNER/REPO.
HOST is the hostname for DB cache update.  CALLBACK is called on success."
  (let* ((name (read-string "Label name: "))
         (color (read-color "Label color: "))
         (description (read-string "Description (optional): ")))
    (when (string-empty-p name)
      (user-error "Label name cannot be empty"))
    ;; Convert color name to hex if needed, strip # prefix
    (unless (string-match-p "\\`#?[0-9a-fA-F]+\\'" color)
      (let ((rgb (color-values color)))
        (unless rgb
          (user-error "Unknown color: %s" color))
        (setq color (format "%02x%02x%02x"
                            (/ (nth 0 rgb) 256)
                            (/ (nth 1 rgb) 256)
                            (/ (nth 2 rgb) 256)))))
    (when (string-prefix-p "#" color)
      (setq color (substring color 1)))
    (forgejo-api-post
     (format "repos/%s/%s/labels" owner repo)
     nil
     `((name . ,name)
       (color . ,color)
       ,@(unless (string-empty-p description)
           `((description . ,description))))
     (lambda (data _headers)
       ;; Cache the new label locally
       (forgejo-db-save-labels host owner repo (list data))
       (message "Created label %s in %s/%s" name owner repo)
       (when callback (funcall callback))))))

;;; PR review

(defun forgejo-utils-submit-review (owner repo number callback)
  "Submit a review on PR NUMBER in OWNER/REPO.
Prompts for the review type and an optional body.
CALLBACK is called on success."
  (let* ((type (completing-read "Review type: "
                                '("approve" "request_changes" "comment")
                                nil t))
         (event (pcase type
                  ("approve" "APPROVED")
                  ("request_changes" "REQUEST_CHANGES")
                  ("comment" "COMMENT")))
         (body (read-string-from-buffer "Review body" "")))
    (forgejo-api-post
     (format "repos/%s/%s/pulls/%d/reviews" owner repo number)
     nil
     `((event . ,event)
       ,@(when (and body (not (string-empty-p (string-trim body))))
           `((body . ,body))))
     (lambda (_data _headers)
       (message "Review submitted: %s on %s/%s#%d" type owner repo number)
       (when callback (funcall callback))))))

;;; Edit

(defun forgejo-utils-edit-body (owner repo number current-body callback)
  "Edit the body of issue/PR NUMBER in OWNER/REPO.
CURRENT-BODY is pre-filled in the editor.  CALLBACK is called on success."
  (let ((body (read-string-from-buffer "Edit body" (or current-body "")))
        (host (url-host (url-generic-parse-url forgejo-host)))
        (context (format "%s/%s" owner repo)))
    (when (and body (not (string= body (or current-body ""))))
      (forgejo-api-patch
       (format "repos/%s/%s/issues/%d" owner repo number)
       `((body . ,body))
       (lambda (_data _headers)
         ;; PATCH response does not include body_html, so render
         ;; the markdown via the API before saving to the DB.
         (forgejo-api-render-markdown-async
          body context
          (lambda (html)
            (forgejo-db--execute
             "UPDATE issues SET body = ?, body_html = ?
              WHERE host = ? AND owner = ? AND repo = ? AND number = ?"
             (list body (or html "") host owner repo number))
            (message "Updated body of %s/%s#%d" owner repo number)
            (when callback (funcall callback)))))))))

(defun forgejo-utils-edit-comment (owner repo comment-id current-body callback)
  "Edit comment COMMENT-ID in OWNER/REPO.
CURRENT-BODY is pre-filled in the editor.  CALLBACK is called on success."
  (let ((body (read-string-from-buffer "Edit comment" (or current-body "")))
        (host (url-host (url-generic-parse-url forgejo-host)))
        (context (format "%s/%s" owner repo)))
    (when (and body (not (string= body (or current-body ""))))
      (forgejo-api-patch
       (format "repos/%s/%s/issues/comments/%d" owner repo comment-id)
       `((body . ,body))
       (lambda (_data _headers)
         ;; PATCH response does not include body_html, so render
         ;; the markdown via the API before saving to the DB.
         (forgejo-api-render-markdown-async
          body context
          (lambda (html)
            (forgejo-db--execute
             "UPDATE timeline_events SET body = ?, body_html = ?
              WHERE host = ? AND owner = ? AND repo = ? AND id = ?"
             (list body (or html "") host owner repo comment-id))
            (message "Updated comment %d in %s/%s" comment-id owner repo)
            (when callback (funcall callback)))))))))

;;; Label/assignee/milestone management

(defun forgejo-utils-add-label (owner repo number host callback)
  "Add a label to issue/PR NUMBER in OWNER/REPO.
HOST is the hostname for DB lookups.  If the entered label does not
exist, offers to create it first.  CALLBACK is called on success."
  (let* ((cached (forgejo-db-get-labels host owner repo))
         (names (mapcar (lambda (row) (nth 4 row)) cached))
         (name (completing-read "Add label: " names nil nil))
         (id (forgejo-db-get-label-id host owner repo name)))
    (if id
        (forgejo-utils--apply-label owner repo number name id callback)
      ;; Label doesn't exist, offer to create it
      (if (y-or-n-p (format "Label %S doesn't exist. Create it? " name))
          (forgejo-utils-create-label
           owner repo host
           (lambda ()
             (let ((new-id (forgejo-db-get-label-id host owner repo name)))
               (if new-id
                   (forgejo-utils--apply-label owner repo number name new-id callback)
                 (user-error "Failed to resolve label %S after creation" name)))))
        (user-error "Label %S not found" name)))))

(defun forgejo-utils--apply-label (owner repo number name id callback)
  "Add label NAME (with ID) to issue/PR NUMBER in OWNER/REPO.
CALLBACK is called on success."
  (forgejo-api-post
   (format "repos/%s/%s/issues/%d/labels" owner repo number)
   nil
   `((labels . ,(vector id)))
   (lambda (_data _headers)
     (message "Added label %s to %s/%s#%d" name owner repo number)
     (when callback (funcall callback)))))

(defun forgejo-utils-remove-label (owner repo number current-labels host callback)
  "Remove a label from issue/PR NUMBER in OWNER/REPO.
CURRENT-LABELS is the list of label alists on the issue.
HOST is the hostname for DB lookups.  CALLBACK is called on success."
  (let* ((names (mapcar (lambda (l) (alist-get 'name l)) current-labels))
         (name (completing-read "Remove label: " names nil t))
         (id (forgejo-db-get-label-id host owner repo name)))
    (unless id
      (user-error "Label %S not found in cache" name))
    (forgejo-api-delete
     (format "repos/%s/%s/issues/%d/labels/%d" owner repo number id)
     nil
     (lambda (_data _headers)
       (message "Removed label %s from %s/%s#%d" name owner repo number)
       (when callback (funcall callback))))))

(defun forgejo-utils-add-assignee (owner repo number current-assignees host callback)
  "Add an assignee to issue/PR NUMBER in OWNER/REPO.
CURRENT-ASSIGNEES is the list of current assignee alists.
HOST is the hostname for DB lookups.  CALLBACK is called on success."
  (let* ((authors (forgejo-db-get-authors host owner repo))
         (login (completing-read "Add assignee: " authors nil nil))
         (current (mapcar (lambda (a) (alist-get 'login a)) current-assignees))
         (new-list (delete-dups (append current (list login)))))
    (when (string-empty-p login)
      (user-error "No assignee specified"))
    (forgejo-api-patch
     (format "repos/%s/%s/issues/%d" owner repo number)
     `((assignees . ,new-list))
     (lambda (_data _headers)
       (message "Assigned %s to %s/%s#%d" login owner repo number)
       (when callback (funcall callback))))))

(defun forgejo-utils-remove-assignee (owner repo number current-assignees callback)
  "Remove an assignee from issue/PR NUMBER in OWNER/REPO.
CURRENT-ASSIGNEES is the list of assignee alists.  CALLBACK on success."
  (let* ((logins (mapcar (lambda (a) (alist-get 'login a)) current-assignees))
         (login (completing-read "Remove assignee: " logins nil t))
         (new-list (remove login logins)))
    (forgejo-api-patch
     (format "repos/%s/%s/issues/%d" owner repo number)
     `((assignees . ,(or (vconcat new-list) [])))
     (lambda (_data _headers)
       (message "Unassigned %s from %s/%s#%d" login owner repo number)
       (when callback (funcall callback))))))

(defun forgejo-utils-set-milestone (owner repo number host callback)
  "Set or clear the milestone on issue/PR NUMBER in OWNER/REPO.
HOST is the hostname for DB lookups.  CALLBACK is called on success."
  (let* ((cached (forgejo-db-get-milestones host owner repo))
         (titles (mapcar (lambda (row) (nth 4 row)) cached))
         (title (completing-read "Milestone (empty to clear): " titles nil nil))
         (id (if (string-empty-p title) 0
               (or (forgejo-db-get-milestone-id host owner repo title)
                   (user-error "Milestone %S not found in cache" title)))))
    (forgejo-api-patch
     (format "repos/%s/%s/issues/%d" owner repo number)
     `((milestone . ,id))
     (lambda (_data _headers)
       (message "%s milestone on %s/%s#%d"
                (if (= id 0) "Cleared" (format "Set milestone %s on" title))
                owner repo number)
       (when callback (funcall callback))))))

;;; Filter prompt

(defun forgejo-utils-read-filter (default-query completions-alist)
  "Read a filter query string with prefix-aware completion.
DEFAULT-QUERY is the initial input.  COMPLETIONS-ALIST is an alist
of (PREFIX . VALUES) where VALUES is a list of strings or nil for
free-text prefixes.  Returns the query string."
  (let* ((candidates
          (cl-loop for (prefix . values) in completions-alist
                   if values
                   append (mapcar (lambda (v) (concat (symbol-name prefix) ":" v))
                                  values)
                   else collect (concat (symbol-name prefix) ":")))
         (crm-separator ",")
         (initial (replace-regexp-in-string " " "," (or default-query "")))
         (selections (completing-read-multiple "Filter: " candidates nil nil
                                               initial)))
    (mapconcat #'identity selections " ")))

(defun forgejo-utils-serialize-filter (filters)
  "Serialize a filter plist back to a query string.
The reverse of `forgejo-utils-parse-filter'."
  (let ((key-map '((:state . "state")
                   (:labels . "label")
                   (:milestone . "milestone")
                   (:assignee . "author")
                   (:poster . "poster")))
        (parts nil))
    (cl-loop for (key . prefix) in key-map
             for val = (plist-get filters key)
             when val do (push (concat prefix ":" val) parts))
    (when-let* ((query (plist-get filters :query)))
      (push query parts))
    (mapconcat #'identity (nreverse parts) " ")))

(defun forgejo-utils-parse-filter (query-string)
  "Parse QUERY-STRING into a filter plist.
Recognized prefixes are mapped to plist keys: state -> :state,
label -> :labels, milestone -> :milestone, author -> :assignee,
poster -> :poster, search -> :query.
Bare words without a prefix are collected as :query (title search)."
  (let ((tokens (split-string (or query-string "") " " t))
        (result nil)
        (bare-words nil)
        (prefix-map '(("state" . :state)
                      ("label" . :labels)
                      ("milestone" . :milestone)
                      ("author" . :assignee)
                      ("poster" . :poster)
                      ("search" . :query))))
    (dolist (token tokens)
      (if (string-match "\\`\\([^:]+\\):\\(.*\\)\\'" token)
          (let* ((prefix (match-string 1 token))
                 (value (match-string 2 token))
                 (key (cdr (assoc prefix prefix-map))))
            (when (and key (not (string-empty-p value)))
              (setq result (plist-put result key value))))
        (push token bare-words)))
    (when bare-words
      (setq result (plist-put result :query
                              (mapconcat #'identity (nreverse bare-words) " "))))
    result))

(provide 'forgejo-utils)
;;; forgejo-utils.el ends here

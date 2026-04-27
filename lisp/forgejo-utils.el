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

(require 'forgejo-api)
(require 'forgejo-db)
(require 'markdown-mode)

(declare-function forgejo-token "forgejo.el" (host-url))

;;; URL builders

(defun forgejo-utils-repo-url (host-url owner repo)
  "Return the web URL for OWNER/REPO on HOST-URL."
  (format "%s/%s/%s" host-url owner repo))

(defun forgejo-utils-issue-url (host-url owner repo number)
  "Return the web URL for issue NUMBER in OWNER/REPO on HOST-URL."
  (format "%s/%s/%s/issues/%d" host-url owner repo number))

(defun forgejo-utils-pull-url (host-url owner repo number)
  "Return the web URL for pull request NUMBER in OWNER/REPO on HOST-URL."
  (format "%s/%s/%s/pulls/%d" host-url owner repo number))

;;; Browse helpers

(defun forgejo-utils-browse-repo (host-url owner repo)
  "Open OWNER/REPO on HOST-URL in the browser."
  (browse-url (forgejo-utils-repo-url host-url owner repo)))

(defun forgejo-utils-browse-issue (host-url owner repo number)
  "Open issue NUMBER of OWNER/REPO on HOST-URL in the browser."
  (browse-url (forgejo-utils-issue-url host-url owner repo number)))

(defun forgejo-utils-browse-pull (host-url owner repo number)
  "Open pull request NUMBER of OWNER/REPO on HOST-URL in the browser."
  (browse-url (forgejo-utils-pull-url host-url owner repo number)))

;;; State toggle

(defun forgejo-utils-toggle-state (host-url owner repo number current-state
					    callback)
  "Toggle issue/PR NUMBER in OWNER/REPO between open and closed.
HOST-URL is the instance.  CURRENT-STATE is \"open\" or \"closed\".
CALLBACK is called on success."
  (let* ((new-state (if (string= current-state "open") "closed" "open"))
         (action (if (string= new-state "closed") "Close" "Reopen")))
    (when (y-or-n-p (format "%s %s/%s#%d? " action owner repo number))
      (forgejo-api-patch
       host-url
       (format "repos/%s/%s/issues/%d" owner repo number)
       `((state . ,new-state))
       (lambda (_data _headers)
         (message "%sd %s/%s#%d" action owner repo number)
         (when callback (funcall callback)))))))

;;; Filter prompt

(defvar crm-separator)

(defun forgejo-utils-read-filter (default-query completions-alist)
  "Read a filter query string with prefix-aware completion.
DEFAULT-QUERY is the initial input.  COMPLETIONS-ALIST is an alist
of (PREFIX . VALUES) where VALUES is a list of strings or nil for
free-text prefixes.  Returns the query string."
  (let* ((candidates
          (cl-loop for (prefix . values) in completions-alist
                   if values
                   append (mapcar (lambda (v)
                                    (concat (symbol-name prefix) ":" v))
                                  values)
                   else collect (concat (symbol-name prefix) ":")))
         (crm-separator ",")
         (initial (replace-regexp-in-string " " ","
                                            (or default-query "")))
         (selections (completing-read-multiple
                      "Filter: " candidates nil nil initial)))
    (mapconcat #'identity selections " ")))

;;; Issue/PR # completion

(defvar-local forgejo-utils--capf-candidates nil
  "Cached completion candidates for # references.")

(defun forgejo-utils-issue-capf ()
  "Completion-at-point function for #N issue/PR references."
  (when-let* ((bounds (forgejo-utils--capf-bounds)))
    (let ((start (car bounds))
          (end (cdr bounds)))
      (list start end
            (forgejo-utils--capf-collection)
            :annotation-function #'forgejo-utils--capf-annotate
            :exclusive 'no))))

(defun forgejo-utils--capf-bounds ()
  "Return (START . END) for the # reference at point, or nil."
  (save-excursion
    (let ((end (point)))
      (when (re-search-backward "#" (line-beginning-position) t)
        (cons (point) end)))))

(defun forgejo-utils--capf-collection ()
  "Return completion candidates for # references."
  (or forgejo-utils--capf-candidates
      (setq forgejo-utils--capf-candidates
            (mapcar (lambda (pair)
                      (format "#%d" (car pair)))
                    (forgejo-utils--capf-load-candidates)))))

(defun forgejo-utils--capf-load-candidates ()
  "Load issue/PR candidates from the DB for the current repo context."
  (when-let* ((host (and (boundp 'forgejo-repo--host)
                         (url-host (url-generic-parse-url forgejo-repo--host))))
              (owner (and (boundp 'forgejo-repo--owner) forgejo-repo--owner))
              (repo (and (boundp 'forgejo-repo--name) forgejo-repo--name)))
    (forgejo-db-get-issue-titles host owner repo)))

(defun forgejo-utils--capf-annotate (candidate)
  "Return the title annotation for CANDIDATE (#N)."
  (when (string-match "#\\([0-9]+\\)" candidate)
    (let* ((number (string-to-number (match-string 1 candidate)))
           (titles (forgejo-utils--capf-load-candidates))
           (title (alist-get number titles)))
      (when title (concat " " title)))))

;;; @mention completion

(defvar-local forgejo-utils--mention-candidates nil
  "Cached completion candidates for @ mentions.")

(defun forgejo-utils-mention-capf ()
  "Completion-at-point function for @user mentions."
  (when-let* ((bounds (forgejo-utils--mention-bounds)))
    (list (car bounds) (cdr bounds)
          (forgejo-utils--mention-collection)
          :exclusive 'no)))

(defun forgejo-utils--mention-bounds ()
  "Return (START . END) for the @ mention at point, or nil."
  (save-excursion
    (let ((end (point)))
      (when (re-search-backward "@" (line-beginning-position) t)
        (cons (point) end)))))

(defun forgejo-utils--mention-collection ()
  "Return completion candidates for @ mentions."
  (or forgejo-utils--mention-candidates
      (setq forgejo-utils--mention-candidates
            (mapcar (lambda (login) (concat "@" login))
                    (forgejo-utils--mention-load-users)))))

(defun forgejo-utils--mention-load-users ()
  "Load usernames from the DB for the current repo context."
  (when-let* ((host (and (boundp 'forgejo-repo--host)
                         (url-host (url-generic-parse-url forgejo-repo--host))))
              (owner (and (boundp 'forgejo-repo--owner) forgejo-repo--owner))
              (repo (and (boundp 'forgejo-repo--name) forgejo-repo--name)))
    (forgejo-db-get-authors host owner repo)))

;;; Comment

(defvar forgejo-compose-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") nil)
    map)
  "Keymap for `forgejo-compose-mode'.
Unbinds C-c C-c so `string-edit-minor-mode' handles it.")

(define-derived-mode forgejo-compose-mode gfm-mode "Forgejo Compose"
  "Major mode for composing Forgejo comments.
Inherits `gfm-mode' for markdown highlighting."
  :group 'forgejo)

(defun forgejo-utils-read-body (prompt &optional initial)
  "Read multi-line text with markdown highlighting and # completion.
PROMPT is used in the header line.  Buffer has no read-only regions."
  (let* ((host (and (boundp 'forgejo-repo--host) forgejo-repo--host))
         (owner (and (boundp 'forgejo-repo--owner) forgejo-repo--owner))
         (repo (and (boundp 'forgejo-repo--name) forgejo-repo--name))
         (result (or initial ""))
         (hook-fn (lambda ()
                    (setq-local forgejo-repo--host host
                                forgejo-repo--owner owner
                                forgejo-repo--name repo)
                    (setq-local completion-at-point-functions
                                (list #'forgejo-utils-issue-capf
                                      #'forgejo-utils-mention-capf))
                    (setq-local header-line-format
                                (substitute-command-keys
                                 (format "%s  \\<string-edit-minor-mode-map>\\[string-edit-done] to submit, \\[string-edit-abort] to cancel"
                                         prompt)))
                    (run-hooks 'forgejo-compose-hook))))
    (add-hook 'forgejo-compose-mode-hook hook-fn)
    (unwind-protect
        (progn
          (string-edit nil (or initial "")
                       (lambda (edited)
                         (setq result edited)
                         (exit-recursive-edit))
                       :abort-callback (lambda ()
                                         (setq result nil)
                                         (exit-recursive-edit))
                       :major-mode-sym #'forgejo-compose-mode)
          (recursive-edit))
      (remove-hook 'forgejo-compose-mode-hook hook-fn))
    result))

(defun forgejo-utils-post-comment (host-url endpoint prompt &optional initial
					    callback)
  "Post a comment to ENDPOINT on HOST-URL.
PROMPT is the composition buffer title.  INITIAL is optional
pre-filled text (e.g. quoted reply).  CALLBACK receives (DATA HEADERS)
on success.  Returns nil if the user aborts."
  (let ((body (forgejo-utils-read-body prompt initial)))
    (cond
     ((null body) (message "Aborted"))
     ((string-empty-p (string-trim body))
      (when (y-or-n-p "Submit empty comment? ")
        (forgejo-api-post
         host-url endpoint nil
         `((body . ""))
         (lambda (data headers)
           (when callback (funcall callback data headers))))))
     (t (forgejo-api-post
         host-url endpoint nil
         `((body . ,(string-trim body)))
         (lambda (data headers)
           (when callback (funcall callback data headers))))))))

;;; Issue creation

(defun forgejo-utils-get-issue-templates (host-url owner repo)
  "Fetch issue templates for OWNER/REPO on HOST-URL synchronously.
Returns a list of alists with `name' and `content' keys, or nil."
  (let ((url-request-method "GET")
        (url-request-extra-headers
         `(("Authorization" . ,(encode-coding-string
                                (concat "token " (forgejo-token host-url))
                                'ascii))
           ("Accept" . "application/json"))))
    (condition-case nil
        (with-current-buffer
            (url-retrieve-synchronously
             (format "%s/api/v1/repos/%s/%s/issue_templates"
                     host-url owner repo)
             t)
          (goto-char (point-min))
          (re-search-forward "\r?\n\r?\n" nil t)
          (let ((data (json-parse-buffer :object-type 'alist
                                         :array-type 'list)))
            (kill-buffer (current-buffer))
            (when (listp data) data)))
      (error nil))))

(defun forgejo-utils-create-issue (host-url owner repo)
  "Create a new issue in OWNER/REPO on HOST-URL.
Fetches templates if available, lets user pick one, then compose."
  (let* ((templates (forgejo-utils-get-issue-templates host-url owner repo))
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
         (body (when (not (string-empty-p (string-trim title)))
                 (forgejo-utils-read-body "Issue body"
                                          (or template-content "")))))
    (cond
     ((string-empty-p (string-trim title))
      (user-error "Title cannot be empty"))
     ((null body)
      (message "Aborted"))
     (t
      (when (or (not (string-empty-p (string-trim body)))
                (y-or-n-p "Submit issue without body? "))
        (forgejo-api-post
         host-url
         (format "repos/%s/%s/issues" owner repo)
         nil
         `((title . ,title)
           ,@(when (and body (not (string-empty-p (string-trim body))))
               `((body . ,body))))
         (lambda (_data _headers)
           (message "Issue created: %s/%s \"%s\"" owner repo title))))))))

;;; Repository creation

(defun forgejo-utils-create-repo (host-url name)
  "Create a new public repository named NAME on HOST-URL.
Copies the SSH clone URL to the kill ring on success."
  (forgejo-api-post
   host-url "user/repos" nil
   `((name . ,name))
   (lambda (data _headers)
     (let ((ssh-url (alist-get 'ssh_url data)))
       (when ssh-url (kill-new ssh-url))
       (message "Repository created: %s %s" name
                (if ssh-url (concat "(copied: " ssh-url ")") ""))))))

;;; Label creation

(defun forgejo-utils-create-label (host-url owner repo host callback)
  "Create a new label in OWNER/REPO on HOST-URL.
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
     host-url
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

;;; Edit

(defun forgejo-utils-edit-title (host-url owner repo number current-title
					  callback)
  "Edit the title of issue/PR NUMBER in OWNER/REPO on HOST-URL.
CURRENT-TITLE is pre-filled.  CALLBACK is called on success."
  (let ((title (read-string "Title: " current-title)))
    (when (and title (not (string= title (or current-title ""))))
      (forgejo-api-patch
       host-url
       (format "repos/%s/%s/issues/%d" owner repo number)
       `((title . ,title))
       (lambda (_data _headers)
         (let ((host (url-host (url-generic-parse-url host-url))))
           (forgejo-db--execute
            "UPDATE issues SET title = ?
             WHERE host = ? AND owner = ? AND repo = ? AND number = ?"
            (list title host owner repo number)))
         (message "Updated title of %s/%s#%d" owner repo number)
         (when callback (funcall callback)))))))

(defun forgejo-utils-edit-body (host-url owner repo number current-body
					 callback)
  "Edit the body of issue/PR NUMBER in OWNER/REPO on HOST-URL.
CURRENT-BODY is pre-filled in the editor.  CALLBACK is called on success."
  (let ((body (forgejo-utils-read-body "Edit body" current-body))
        (host (url-host (url-generic-parse-url host-url))))
    (when (and body (not (string= body (or current-body ""))))
      (forgejo-api-patch
       host-url
       (format "repos/%s/%s/issues/%d" owner repo number)
       `((body . ,body))
       (lambda (_data _headers)
         (forgejo-db--execute
          "UPDATE issues SET body = ?
           WHERE host = ? AND owner = ? AND repo = ? AND number = ?"
          (list body host owner repo number))
         (message "Updated body of %s/%s#%d" owner repo number)
         (when callback (funcall callback)))))))

(defun forgejo-utils-edit-comment (host-url owner repo comment-id current-body
					    callback)
  "Edit comment COMMENT-ID in OWNER/REPO on HOST-URL.
CURRENT-BODY is pre-filled in the editor.  CALLBACK is called on success."
  (let ((body (forgejo-utils-read-body "Edit comment" current-body))
        (host (url-host (url-generic-parse-url host-url))))
    (when (and body (not (string= body (or current-body ""))))
      (forgejo-api-patch
       host-url
       (format "repos/%s/%s/issues/comments/%d" owner repo comment-id)
       `((body . ,body))
       (lambda (_data _headers)
         (forgejo-db--execute
          "UPDATE timeline_events SET body = ?
           WHERE host = ? AND owner = ? AND repo = ? AND id = ?"
          (list body host owner repo comment-id))
         (message "Updated comment %d in %s/%s" comment-id owner repo)
         (when callback (funcall callback)))))))

;;; Label/assignee/milestone management

(defun forgejo-utils-add-label (host-url owner repo number host callback)
  "Add a label to issue/PR NUMBER in OWNER/REPO on HOST-URL.
HOST is the hostname for DB lookups.  If the entered label does not
exist, offers to create it first.  CALLBACK is called on success."
  (let* ((cached (forgejo-db-get-labels host owner repo))
         (names (mapcar (lambda (row) (nth 4 row)) cached))
         (name (completing-read "Add label: " names nil nil))
         (id (forgejo-db-get-label-id host owner repo name)))
    (if id
        (forgejo-utils--apply-label host-url owner repo number name id callback)
      ;; Label doesn't exist, offer to create it
      (if (y-or-n-p (format "Label %S doesn't exist. Create it? " name))
          (forgejo-utils-create-label
           host-url owner repo host
           (lambda ()
             (let ((new-id (forgejo-db-get-label-id host owner repo name)))
               (if new-id
                   (forgejo-utils--apply-label host-url owner repo number
                                               name new-id callback)
                 (user-error "Failed to resolve label %S after creation"
                             name)))))
        (user-error "Label %S not found" name)))))

(defun forgejo-utils--apply-label (host-url owner repo number name id callback)
  "Add label NAME (with ID) to issue/PR NUMBER in OWNER/REPO on HOST-URL.
CALLBACK is called on success."
  (forgejo-api-post
   host-url
   (format "repos/%s/%s/issues/%d/labels" owner repo number)
   nil
   `((labels . ,(vector id)))
   (lambda (_data _headers)
     (message "Added label %s to %s/%s#%d" name owner repo number)
     (when callback (funcall callback)))))

(defun forgejo-utils-remove-label (host-url owner repo number current-labels
					    host callback)
  "Remove a label from issue/PR NUMBER in OWNER/REPO on HOST-URL.
CURRENT-LABELS is the list of label alists on the issue.
HOST is the hostname for DB lookups.  CALLBACK is called on success."
  (let* ((names (mapcar (lambda (l) (alist-get 'name l)) current-labels))
         (name (completing-read "Remove label: " names nil t))
         (id (forgejo-db-get-label-id host owner repo name)))
    (unless id
      (user-error "Label %S not found in cache" name))
    (forgejo-api-delete
     host-url
     (format "repos/%s/%s/issues/%d/labels/%d" owner repo number id)
     nil
     (lambda (_data _headers)
       (message "Removed label %s from %s/%s#%d" name owner repo number)
       (when callback (funcall callback))))))

(defun forgejo-utils-add-assignee (host-url owner repo number current-assignees
					    host callback)
  "Add an assignee to issue/PR NUMBER in OWNER/REPO on HOST-URL.
CURRENT-ASSIGNEES is the list of current assignee alists.
HOST is the hostname for DB lookups.  CALLBACK is called on success."
  (let* ((authors (forgejo-db-get-authors host owner repo))
         (login (completing-read "Add assignee: " authors nil nil))
         (current (mapcar (lambda (a) (alist-get 'login a)) current-assignees))
         (new-list (delete-dups (append current (list login)))))
    (when (string-empty-p login)
      (user-error "No assignee specified"))
    (forgejo-api-patch
     host-url
     (format "repos/%s/%s/issues/%d" owner repo number)
     `((assignees . ,new-list))
     (lambda (_data _headers)
       (message "Assigned %s to %s/%s#%d" login owner repo number)
       (when callback (funcall callback))))))

(defun forgejo-utils-remove-assignee (host-url owner repo number
					       current-assignees callback)
  "Remove an assignee from issue/PR NUMBER in OWNER/REPO on HOST-URL.
CURRENT-ASSIGNEES is the list of assignee alists.  CALLBACK on success."
  (let* ((logins (mapcar (lambda (a) (alist-get 'login a)) current-assignees))
         (login (completing-read "Remove assignee: " logins nil t))
         (new-list (remove login logins)))
    (forgejo-api-patch
     host-url
     (format "repos/%s/%s/issues/%d" owner repo number)
     `((assignees . ,(or (vconcat new-list) [])))
     (lambda (_data _headers)
       (message "Unassigned %s from %s/%s#%d" login owner repo number)
       (when callback (funcall callback))))))

(defun forgejo-utils-set-milestone (host-url owner repo number host callback)
  "Set or clear the milestone on issue/PR NUMBER in OWNER/REPO.
HOST-URL is the instance.  HOST is the hostname for DB lookups.
CALLBACK is called on success."
  (let* ((cached (forgejo-db-get-milestones host owner repo))
         (titles (mapcar (lambda (row) (nth 4 row)) cached))
         (title (completing-read "Milestone (empty to clear): " titles nil nil))
         (id (if (string-empty-p title) 0
               (or (forgejo-db-get-milestone-id host owner repo title)
                   (user-error "Milestone %S not found in cache" title)))))
    (forgejo-api-patch
     host-url
     (format "repos/%s/%s/issues/%d" owner repo number)
     `((milestone . ,id))
     (lambda (_data _headers)
       (message "%s milestone on %s/%s#%d"
                (if (= id 0) "Cleared" (format "Set milestone %s on" title))
                owner repo number)
       (when callback (funcall callback))))))

(provide 'forgejo-utils)
;;; forgejo-utils.el ends here

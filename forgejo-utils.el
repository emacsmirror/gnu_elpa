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

(declare-function forgejo-token "forgejo.el" ())

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

;;; Comment

(declare-function forgejo-api-post "forgejo-api.el"
                  (endpoint &optional params json-body callback))

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

(declare-function forgejo-api-get "forgejo-api.el"
                  (endpoint &optional params callback))

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

(provide 'forgejo-utils)
;;; forgejo-utils.el ends here

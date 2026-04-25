;;; forgejo-settings.el --- Repository settings for Forgejo  -*- lexical-binding: t; -*-

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

;; View and edit repository settings via transient menus.
;; Data flows through closures, no global state.
;; Fetches fresh from API each time settings are opened.

;;; Code:

(require 'transient)
(require 'forgejo)
(require 'forgejo-api)
(require 'forgejo-utils)
(require 'forgejo-db)

(defvar forgejo-repo--host)

;;; API operations

(defun forgejo-settings--fetch (host-url owner repo callback)
  "Fetch repo settings for OWNER/REPO on HOST-URL, call CALLBACK with alist."
  (forgejo-api-get
   host-url (format "repos/%s/%s" owner repo) nil
   (lambda (data _headers)
     (when callback (funcall callback data)))))

(defun forgejo-settings--save (host-url owner repo field value callback)
  "Save FIELD with VALUE for OWNER/REPO on HOST-URL.
Calls CALLBACK with updated data on success."
  (forgejo-api-patch
   host-url (format "repos/%s/%s" owner repo)
   `((,field . ,value))
   (lambda (data _headers)
     (message "Updated %s for %s/%s" field owner repo)
     (when callback (funcall callback data)))))

;;; Accessors (pure, take data as argument)

(defun forgejo-settings--owner (data)
  "Extract owner login from repo DATA."
  (alist-get 'login (alist-get 'owner data)))

(defun forgejo-settings--repo (data)
  "Extract repo name from repo DATA."
  (alist-get 'name data))

;;; Transient

(declare-function forgejo-vc--repo-from-remote "forgejo-vc.el" ())

;;;###autoload
(defun forgejo-settings ()
  "Open repository settings for the current repo context.
Fetches fresh settings from the API, then opens the transient."
  (interactive)
  (let* ((host-url (or (bound-and-true-p forgejo-repo--host)
                       (forgejo--resolve-host)))
         (owner (or (bound-and-true-p forgejo-repo--owner)
                    (nth 1 (forgejo-vc--repo-from-remote))
                    (user-error "No repo context")))
         (repo (or (bound-and-true-p forgejo-repo--name)
                   (nth 2 (forgejo-vc--repo-from-remote))
                   (user-error "No repo context"))))
    (forgejo-settings--fetch
     host-url owner repo
     (lambda (data)
       (forgejo-settings--show host-url data)))))

(defun forgejo-settings--format-value (value)
  "Format VALUE with a distinct face for transient display."
  (propertize (format "%s" (or value ""))
              'face 'forgejo-comment-author-face))

(defun forgejo-settings--show (host-url data)
  "Display settings transient for repo DATA on HOST-URL."
  (let ((owner (forgejo-settings--owner data))
        (repo (forgejo-settings--repo data))
        (desc (or (alist-get 'description data) ""))
        (website (or (alist-get 'website data) ""))
        (host (url-host (url-generic-parse-url host-url))))
    (transient-define-prefix forgejo-settings--current ()
      "Repository settings."
      [:description
       (lambda () (format "Settings: %s/%s" owner repo))
       ("d" (lambda () (format "Description  %s"
                               (forgejo-settings--format-value
                                (truncate-string-to-width desc 40 nil nil "..."))))
        (lambda ()
          (interactive)
          (let ((new (read-string "Description: " desc)))
            (unless (string= new desc)
              (forgejo-settings--save
               host-url owner repo 'description new
               (lambda (_updated)
                 (setq desc new)
                 (transient-setup 'forgejo-settings--current)))))))
       ("w" (lambda () (format "Website  %s"
                               (forgejo-settings--format-value website)))
        (lambda ()
          (interactive)
          (let ((new (read-string "Website: " website)))
            (unless (string= new website)
              (forgejo-settings--save
               host-url owner repo 'website new
               (lambda (_updated)
                 (setq website new)
                 (transient-setup 'forgejo-settings--current)))))))
       ("l" "Labels" forgejo-settings--labels)]
      [:hide always])
    (transient-define-prefix forgejo-settings--labels ()
      "Label management."
      [:description
       (lambda () (format "Labels: %s/%s" owner repo))
       ("a" "Create label"
        (lambda ()
          (interactive)
          (forgejo-utils-create-label host-url owner repo host nil)))
       ("c" "Change color"
        (lambda ()
          (interactive)
          (let* ((cached (forgejo-db-get-labels host owner repo))
                 (names (mapcar (lambda (row) (nth 4 row)) cached))
                 (name (completing-read "Label: " names nil t))
                 (id (forgejo-db-get-label-id host owner repo name))
                 (color (read-color "New color: ")))
            (when id
              (unless (string-match-p "\\`#?[0-9a-fA-F]+\\'" color)
                (let ((rgb (color-values color)))
                  (unless rgb (user-error "Unknown color: %s" color))
                  (setq color (format "%02x%02x%02x"
                                      (/ (nth 0 rgb) 256)
                                      (/ (nth 1 rgb) 256)
                                      (/ (nth 2 rgb) 256)))))
              (when (string-prefix-p "#" color)
                (setq color (substring color 1)))
              (forgejo-api-patch
               host-url
               (format "repos/%s/%s/labels/%d" owner repo id)
               `((color . ,color))
               (lambda (data _headers)
                 (forgejo-db-save-labels host owner repo (list data))
                 (message "Updated color of %s to #%s" name color)))))))
       ("d" "Delete label"
        (lambda ()
          (interactive)
          (let* ((cached (forgejo-db-get-labels host owner repo))
                 (names (mapcar (lambda (row) (nth 4 row)) cached))
                 (name (completing-read "Delete label: " names nil t))
                 (id (forgejo-db-get-label-id host owner repo name)))
            (when (and id (y-or-n-p (format "Delete label %s? " name)))
              (forgejo-api-delete
               host-url
               (format "repos/%s/%s/labels/%d" owner repo id)
               nil
               (lambda (_data _headers)
                 (forgejo-db--execute
                  "DELETE FROM labels WHERE host = ? AND owner = ? AND repo = ? AND id = ?"
                  (list host owner repo id))
                 (forgejo-settings--remove-label-from-issues
                  host owner repo name)
                 (message "Deleted label %s from %s/%s" name owner repo)))))))])
    (forgejo-settings--current)))

(defun forgejo-settings--remove-label-from-issues (host owner repo label-name)
  "Remove LABEL-NAME from all cached issues in HOST/OWNER/REPO."
  (let ((rows (forgejo-db--select
               "SELECT number, labels FROM issues
                WHERE host = ? AND owner = ? AND repo = ? AND labels IS NOT NULL"
               (list host owner repo))))
    (dolist (row rows)
      (let* ((number (nth 0 row))
             (labels-json (nth 1 row))
             (labels (when labels-json (forgejo-db--decode-json labels-json)))
             (filtered (cl-remove-if
                        (lambda (l) (equal (alist-get 'name l) label-name))
                        labels)))
        (unless (= (length labels) (length filtered))
          (forgejo-db--execute
           "UPDATE issues SET labels = ? WHERE host = ? AND owner = ? AND repo = ? AND number = ?"
           (list (forgejo-db--encode-json filtered) host owner repo number)))))))

(provide 'forgejo-settings)
;;; forgejo-settings.el ends here

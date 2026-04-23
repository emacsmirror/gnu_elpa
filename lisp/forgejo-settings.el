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

;;; API operations

(defun forgejo-settings--fetch (owner repo callback)
  "Fetch repo settings for OWNER/REPO, call CALLBACK with alist."
  (forgejo-api-get
   (format "repos/%s/%s" owner repo)
   nil
   (lambda (data _headers)
     (when callback (funcall callback data)))))

(defun forgejo-settings--save (owner repo field value callback)
  "Save FIELD with VALUE for OWNER/REPO.
Calls CALLBACK with updated data on success."
  (forgejo-api-patch
   (format "repos/%s/%s" owner repo)
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
  (let ((owner (or (bound-and-true-p forgejo-repo--owner)
                   (nth 1 (forgejo-vc--repo-from-remote))
                   (user-error "No repo context")))
        (repo (or (bound-and-true-p forgejo-repo--name)
                  (nth 2 (forgejo-vc--repo-from-remote))
                  (user-error "No repo context")))
        (host (or (bound-and-true-p forgejo-repo--host)
                  (nth 0 (forgejo-vc--repo-from-remote)))))
    (forgejo-with-host host
      (forgejo-settings--fetch
       owner repo
       (lambda (data)
         (forgejo-settings--show data))))))

(defun forgejo-settings--show (data)
  "Display settings transient for repo DATA.
Re-defines the transient prefix with current values captured in closures."
  (let ((owner (forgejo-settings--owner data))
        (repo (forgejo-settings--repo data))
        (desc (or (alist-get 'description data) ""))
        (website (or (alist-get 'website data) ""))
        (manual (eq (alist-get 'allow_manual_merge data) t))
        (refresh (lambda (new-data)
                   ;; Dismiss current transient, re-show with new data
                   (when (bound-and-true-p transient--prefix)
                     (transient-quit-one))
                   (forgejo-settings--show new-data))))
    (transient-define-prefix forgejo-settings--current ()
      "Repository settings."
      [:description
       (lambda () (format "Settings: %s/%s" owner repo))
       ("d" (lambda () (format "Description (%s)"
                               (truncate-string-to-width desc 40 nil nil "...")))
        (lambda ()
          (interactive)
          (let ((new (read-string "Description: " desc)))
            (unless (string= new desc)
              (forgejo-settings--save
               owner repo 'description new refresh)))))
       ("w" (lambda () (format "Website (%s)" website))
        (lambda ()
          (interactive)
          (let ((new (read-string "Website: " website)))
            (unless (string= new website)
              (forgejo-settings--save
               owner repo 'website new refresh)))))
       ("m" (lambda () (format "Manual merge (%s)" (if manual "on" "off")))
        (lambda ()
          (interactive)
          (forgejo-settings--save
           owner repo 'allow_manual_merge (not manual) refresh)))])
    (forgejo-settings--current)))

;;; AGit-Flow check

(defun forgejo-settings-check-manual-merge (owner repo callback)
  "Check if manual merge is enabled for OWNER/REPO.
Calls CALLBACK with t if enabled, nil if not."
  (forgejo-settings--fetch
   owner repo
   (lambda (data)
     (funcall callback (eq (alist-get 'allow_manual_merge data) t)))))

(provide 'forgejo-settings)
;;; forgejo-settings.el ends here

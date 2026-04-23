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
;; `forgejo-settings--data' is the source of truth, fetched from
;; the API and updated in place after each change.

;;; Code:

(require 'transient)
(require 'forgejo)
(require 'forgejo-api)

;;; Data (source of truth)

(defvar forgejo-settings--data nil
  "Cached repo settings alist from the API.
This is the source of truth for all settings operations.")

(defun forgejo-settings--owner (&optional data)
  "Extract owner from DATA or `forgejo-settings--data'."
  (let ((d (or data forgejo-settings--data)))
    (alist-get 'login (alist-get 'owner d))))

(defun forgejo-settings--repo (&optional data)
  "Extract repo name from DATA or `forgejo-settings--data'."
  (let ((d (or data forgejo-settings--data)))
    (alist-get 'name d)))

(defun forgejo-settings--get (field &optional data)
  "Get FIELD from DATA or `forgejo-settings--data'."
  (alist-get field (or data forgejo-settings--data)))

;;; API operations

(defun forgejo-settings--fetch (owner repo callback)
  "Fetch repo settings for OWNER/REPO, call CALLBACK with alist."
  (forgejo-api-get
   (format "repos/%s/%s" owner repo)
   nil
   (lambda (data _headers)
     (when callback (funcall callback data)))))

(defun forgejo-settings--save (field value &optional owner repo)
  "Save FIELD with VALUE for OWNER/REPO.
OWNER and REPO default to values from `forgejo-settings--data'."
  (let ((o (or owner (forgejo-settings--owner)))
        (r (or repo (forgejo-settings--repo))))
    (forgejo-api-patch
     (format "repos/%s/%s" o r)
     `((,field . ,value))
     (lambda (_data _headers)
       (message "Updated %s for %s/%s" field o r)))))

;;; Setting commands

(defun forgejo-settings-set-description ()
  "Edit the repository description."
  (interactive)
  (let ((current (or (forgejo-settings--get 'description) "")))
    (let ((new (read-string "Description: " current)))
      (unless (string= new current)
        (forgejo-settings--save 'description new)
        (setf (alist-get 'description forgejo-settings--data) new)))))

(defun forgejo-settings-set-website ()
  "Edit the repository website."
  (interactive)
  (let ((current (or (forgejo-settings--get 'website) "")))
    (let ((new (read-string "Website: " current)))
      (unless (string= new current)
        (forgejo-settings--save 'website new)
        (setf (alist-get 'website forgejo-settings--data) new)))))

(defun forgejo-settings-toggle-manual-merge ()
  "Toggle the manual merge setting."
  (interactive)
  (let* ((current (forgejo-settings--get 'allow_manual_merge))
         (new (not (eq current t))))
    (forgejo-settings--save 'allow_manual_merge new)
    (setf (alist-get 'allow_manual_merge forgejo-settings--data) new)
    (message "Manual merge %s" (if new "enabled" "disabled"))))

;;; Transient

(defun forgejo-settings--description-desc ()
  "Format description for transient display."
  (format "Description (%s)"
          (truncate-string-to-width
           (or (forgejo-settings--get 'description) "") 40 nil nil "...")))

(defun forgejo-settings--website-desc ()
  "Format website for transient display."
  (format "Website (%s)" (or (forgejo-settings--get 'website) "")))

(defun forgejo-settings--manual-merge-desc ()
  "Format manual merge status for transient display."
  (format "Manual merge (%s)"
          (if (eq (forgejo-settings--get 'allow_manual_merge) t) "on" "off")))

(transient-define-prefix forgejo-settings--transient ()
  "Repository settings."
  [:description
   (lambda () (format "Settings: %s/%s"
                      (forgejo-settings--owner)
                      (forgejo-settings--repo)))
   ("d" forgejo-settings-set-description
    :description forgejo-settings--description-desc)
   ("w" forgejo-settings-set-website
    :description forgejo-settings--website-desc)
   ("m" forgejo-settings-toggle-manual-merge
    :description forgejo-settings--manual-merge-desc)])

;;; Entry point

(declare-function forgejo-vc--repo-from-remote "forgejo-vc.el" ())

;;;###autoload
(defun forgejo-settings ()
  "Open repository settings for the current repo context."
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
         (setq forgejo-settings--data data)
         (forgejo-settings--transient))))))

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

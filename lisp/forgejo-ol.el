;;; forgejo-ol.el --- Org-link integration for Forgejo  -*- lexical-binding: t; -*-

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

;; Org-link type `forgejo:' for storing and following links to Forgejo
;; issues and pull requests.  Links work offline via the local DB.
;;
;; Format: forgejo:owner/repo#NUM
;; Example: [[forgejo:thanosapollo/emacs-forgejo#31][org-store-link support]]

;;; Code:

(require 'ol)
(require 'forgejo)
(require 'forgejo-db)

(declare-function forgejo-view-item "forgejo-view.el"
                  (owner repo number))
(declare-function forgejo--resolve-host-for-repo "forgejo.el"
                  (owner repo))

(defvar forgejo-repo--host)
(defvar forgejo-repo--owner)
(defvar forgejo-repo--name)
(defvar forgejo-view--data)

;;;###autoload
(defun forgejo-ol-store-link (&optional _interactive)
  "Store an org link to the Forgejo issue or PR at point."
  (when (derived-mode-p 'forgejo-issue-list-mode 'forgejo-pull-list-mode
                        'forgejo-issue-view-mode 'forgejo-pull-view-mode)
    (let* ((owner forgejo-repo--owner)
           (repo forgejo-repo--name)
           (number (cond
                    ((derived-mode-p 'forgejo-issue-view-mode
                                     'forgejo-pull-view-mode)
                     (alist-get 'number forgejo-view--data))
                    (t (tabulated-list-get-id))))
           (title (cond
                   ((derived-mode-p 'forgejo-issue-view-mode
                                    'forgejo-pull-view-mode)
                    (alist-get 'title forgejo-view--data))
                   (t (when-let* ((host (url-host
                                         (url-generic-parse-url
                                          forgejo-repo--host)))
                                  (issue (forgejo-db-get-issue
                                          host owner repo number)))
                        (alist-get 'title issue))))))
      (when (and owner repo number)
        (let ((link (format "forgejo:%s/%s#%d" owner repo number))
              (desc (format "%s/%s#%d: %s" owner repo number (or title ""))))
          (org-link-store-props :type "forgejo" :link link :description desc))))))

;;;###autoload
(defun forgejo-ol-follow (path _prefix)
  "Follow a forgejo: link.  PATH is owner/repo#NUM."
  (when (string-match "\\`\\([^/]+\\)/\\([^#]+\\)#\\([0-9]+\\)\\'" path)
    (let* ((owner (match-string 1 path))
           (repo (match-string 2 path))
           (number (string-to-number (match-string 3 path)))
           (forgejo-repo--host
            (forgejo--resolve-host-for-repo owner repo)))
      (forgejo-view-item owner repo number))))

;;;###autoload
(defun forgejo-ol-export (path desc backend _channel)
  "Export a forgejo: link.  PATH is owner/repo#NUM."
  (let ((url (when (string-match "\\`\\([^/]+\\)/\\([^#]+\\)#\\([0-9]+\\)\\'" path)
               (let ((owner (match-string 1 path))
                     (repo (match-string 2 path))
                     (number (match-string 3 path)))
                 (format "https://%s/%s/%s/issues/%s"
                         (url-host (url-generic-parse-url
                                    (forgejo--resolve-host)))
                         owner repo number))))
        (desc (or desc path)))
    (pcase backend
      ('html (format "<a href=\"%s\">%s</a>" url desc))
      ('md (format "[%s](%s)" desc url))
      (_ desc))))

;;;###autoload
(with-eval-after-load 'ol
  (org-link-set-parameters "forgejo"
                           :store #'forgejo-ol-store-link
                           :follow #'forgejo-ol-follow
                           :export #'forgejo-ol-export))

(provide 'forgejo-ol)
;;; forgejo-ol.el ends here

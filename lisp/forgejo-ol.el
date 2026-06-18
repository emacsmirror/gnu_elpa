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
;; Format: forgejo:owner/repo#NUM             -- issue or PR
;;         forgejo:owner/repo#NUM::CID        -- specific comment in NUM
;; Example: [[forgejo:thanosapollo/emacs-forgejo#31][org-store-link support]]

;;; Code:

(require 'ol)
(require 'forgejo)
(require 'forgejo-db)
(require 'forgejo-utils)

(declare-function forgejo-view-item "forgejo-view.el"
                  (owner repo number &optional comment-id))
(declare-function forgejo--resolve-host-for-repo "forgejo.el"
                  (owner repo))
(declare-function forgejo-view--node-at-point "forgejo-view.el" ())

(defvar forgejo-repo--host)
(defvar forgejo-repo--owner)
(defvar forgejo-repo--name)
(defvar forgejo-view--data)

(defconst forgejo-ol--path-regexp
  (concat "\\`\\([^/]+\\)/\\([^#!]+\\)"
          forgejo-utils-ref-separator
          "\\([0-9]+\\)\\(?:::\\([0-9]+\\)\\)?\\'")
  "Regexp matching a `forgejo:' link path.
Accepts both #N (issue) and !N (pull request) separators.
Groups: 1=owner, 2=repo, 3=number, 4=optional comment id.")

(defun forgejo-ol--comment-at-point ()
  "Return the comment id of the EWOC node at point, or nil."
  (when (derived-mode-p 'forgejo-issue-view-mode 'forgejo-pull-view-mode)
    (when-let* ((node (forgejo-view--node-at-point)))
      (and (eq (plist-get node :type) 'comment)
           (plist-get node :id)))))

;;;###autoload
(defun forgejo-ol-store-link (&optional _interactive)
  "Store an org link to the Forgejo issue, PR, or comment at point."
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
                        (alist-get 'title issue)))))
           (comment-id (forgejo-ol--comment-at-point)))
      (when (and owner repo number)
        (let ((link (if comment-id
                        (format "forgejo:%s/%s#%d::%d"
                                owner repo number comment-id)
                      (format "forgejo:%s/%s#%d" owner repo number)))
              (desc (if comment-id
                        (format "%s/%s#%d (comment %d): %s"
                                owner repo number comment-id (or title ""))
                      (format "%s/%s#%d: %s"
                              owner repo number (or title "")))))
          (org-link-store-props :type "forgejo" :link link :description desc))))))

;;;###autoload
(defun forgejo-ol-follow (path _prefix)
  "Follow a forgejo: link.
PATH is owner/repo#NUM or owner/repo#NUM::COMMENT-ID."
  (when (string-match forgejo-ol--path-regexp path)
    (let* ((owner (match-string 1 path))
           (repo (match-string 2 path))
           (number (string-to-number (match-string 3 path)))
           (comment-id (and (match-string 4 path)
                            (string-to-number (match-string 4 path))))
           (forgejo-repo--host
            (forgejo--resolve-host-for-repo owner repo)))
      (forgejo-view-item owner repo number comment-id))))

;;;###autoload
(defun forgejo-ol-export (path desc backend _channel)
  "Export a forgejo: link to BACKEND.
PATH is owner/repo#NUM or owner/repo#NUM::COMMENT-ID.
DESC is the link description."
  (let ((url (when (string-match forgejo-ol--path-regexp path)
               (let ((owner (match-string 1 path))
                     (repo (match-string 2 path))
                     (number (match-string 3 path))
                     (comment-id (match-string 4 path)))
                 (format "https://%s/%s/%s/issues/%s%s"
                         (url-host (url-generic-parse-url
                                    (forgejo--resolve-host)))
                         owner repo number
                         (if comment-id
                             (format "#issuecomment-%s" comment-id)
                           "")))))
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

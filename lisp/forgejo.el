;;; forgejo.el --- Emacs Forgejo Front-end  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Thanos Apollo

;; Author: Thanos Apollo <public@thanosapollo.org>
;; Keywords: tools vc git forgejo
;; URL: https://codeberg.org/thanosapollo/emacs-forgejo
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (markdown-mode "2.6") (keymap-popup "0.1.0"))

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

;; Emacs front-end for Forgejo instances (Codeberg, self-hosted).
;;
;; Browse, filter, and manage issues and pull requests from Emacs.
;; Submit PRs via AGit-Flow git push options.  All API responses are
;; cached in a local SQLite database so views render instantly after
;; the first sync.
;;
;; Setup:
;;
;;   Configure your Forgejo instance(s):
;;
;;       (setq forgejo-hosts '(("https://codeberg.org" "token")))
;;
;;   Or store your token in ~/.authinfo.gpg:
;;
;;        machine codeberg.org login YOUR_USERNAME password YOUR_TOKEN
;;
;; Usage:
;;      M-x forgejo       top-level menu
;;      C-x v f           forgejo-vc menu (inside a forgejo project)

;;; Code:

(require 'cl-lib)
(require 'auth-source)
(require 'url-parse)
(require 'keymap-popup)

(defgroup forgejo nil
  "Emacs front-end for Forgejo instances."
  :group 'external
  :prefix "forgejo-")

(defcustom forgejo-compose-hook nil
  "Hook run in composition buffers after setup.
Use this to enable modes like `markdown-mode' or `flyspell-mode'."
  :type 'hook
  :group 'forgejo)


(defcustom forgejo-hosts '(("https://codeberg.org"))
  "List of known Forgejo instances.
Each entry is (URL) or (URL TOKEN).  When TOKEN is omitted,
auth-source is used for that host.

Only hosts in this list are accepted; a git remote pointing to an
unknown host will signal an error.

Example:
  \\='((\"https://codeberg.org\")
    (\"https://git.myorg.com\" \"tok_abc123\"))"
  :type '(repeat
          (choice (list (string :tag "Host URL"))
                  (list (string :tag "Host URL")
                        (string :tag "API token"))))
  :group 'forgejo)

(defcustom forgejo-token nil
  "Personal access token.
Used as fallback when no token is found via `forgejo-hosts' or
auth-source."
  :type '(choice string (const nil))
  :group 'forgejo)

(defcustom forgejo-token-use-auth-source t
  "When non-nil, look up the token via `auth-source'.
Falls back to `forgejo-token' if auth-source returns nothing."
  :type 'boolean
  :group 'forgejo)

(defcustom forgejo-default-sort "recentupdate"
  "Default sort order for issue and PR lists."
  :type '(choice (const "recentupdate")
                 (const "latest")
                 (const "oldest")
                 (const "leastupdate")
                 (const "mostcomment")
                 (const "leastcomment"))
  :group 'forgejo)

(defcustom forgejo-timeline-page-size 30
  "Number of timeline events to fetch per request."
  :type 'integer
  :group 'forgejo)

(defcustom forgejo-db-dir (locate-user-emacs-file "forgejo")
  "Directory for the local SQLite cache database."
  :type 'directory
  :group 'forgejo)

(defcustom forgejo-issue-default-filter
  '("state:open")
  "Default filter query for issue lists.
The first string element is the global default.  Cons cells
of (\"owner/repo\" . \"query\") override for specific repos."
  :type '(repeat (choice string (cons string string)))
  :group 'forgejo)

(defcustom forgejo-pull-default-filter
  '("state:open")
  "Default filter query for pull request lists.
Same format as `forgejo-issue-default-filter'."
  :type '(repeat (choice string (cons string string)))
  :group 'forgejo)

(defun forgejo--sort-by-number (a b)
  "Compare entries A and B numerically by their ID."
  (< (car a) (car b)))

(defun forgejo--sort-by-updated (a b)
  "Compare entries A and B by their updated timestamp."
  (let ((ta (get-text-property 0 'forgejo-timestamp (aref (cadr a) 5)))
        (tb (get-text-property 0 'forgejo-timestamp (aref (cadr b) 5))))
    (string< (or ta "") (or tb ""))))

(defun forgejo--default-filter-for (owner repo filters)
  "Look up the default filter query for OWNER/REPO from FILTERS.
FILTERS is a list like `forgejo-issue-default-filter'."
  (let ((key (format "%s/%s" owner repo)))
    (or (cdr (cl-assoc key filters :test #'string=))
        (cl-find-if #'stringp filters)
        "")))

(defvar forgejo-db nil
  "SQLite database connection for the local cache.")

(defvar forgejo--api-default-limit nil
  "Cached default_paging_num from the instance's /settings/api.")


;;; Post-action hook

(defcustom forgejo-post-action-functions '(forgejo--refresh-current-view)
  "Functions run after a successful mutation action.
Called in the originating buffer's context.  Default refreshes
the current view (issue detail, PR detail, or list)."
  :type 'hook
  :group 'forgejo)

(declare-function forgejo-view-refresh "forgejo-view.el" ())
(declare-function forgejo-issue-refresh "forgejo-issue.el" ())
(declare-function forgejo-pull-refresh "forgejo-pull.el" ())

(defun forgejo--refresh-current-view ()
  "Refresh the current Forgejo view buffer based on its major mode."
  (cond
   ((bound-and-true-p forgejo-view--data)
    (forgejo-view-refresh))
   ((derived-mode-p 'forgejo-issue-list-mode)
    (forgejo-issue-refresh))
   ((derived-mode-p 'forgejo-pull-list-mode)
    (forgejo-pull-refresh))))

(defun forgejo--post-action-callback ()
  "Return a callback that runs `forgejo-post-action-functions'.
Captures the current buffer so the hook runs in the right context."
  (let ((buf (current-buffer)))
    (lambda ()
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (run-hooks 'forgejo-post-action-functions))))))

;;; Host resolution

(declare-function forgejo-vc--repo-from-remote "forgejo-vc.el" ())
(declare-function forgejo-db-get-hosts-for-repo "forgejo-db.el"
                  (owner repo))

(defun forgejo--resolve-host ()
  "Determine the Forgejo host URL from context.
Resolution order:
1. Buffer-local `forgejo-repo--host' (if bound and non-nil)
2. Git remote detection matched against `forgejo-hosts'
3. Sole entry in `forgejo-hosts'
4. Prompt user to pick from `forgejo-hosts'"
  (or (and (boundp 'forgejo-repo--host) forgejo-repo--host)
      (forgejo--host-from-remote)
      (forgejo--host-from-hosts-list)))

(defun forgejo--host-from-remote ()
  "Try to detect host from git remotes, match against `forgejo-hosts'.
Returns a URL string or nil."
  (when-let* ((context (ignore-errors (forgejo-vc--repo-from-remote)))
              (remote-url (nth 0 context))
              (remote-host (url-host (url-generic-parse-url remote-url))))
    (when-let* ((entry (cl-find remote-host forgejo-hosts
                                :key (lambda (e)
                                       (url-host
                                        (url-generic-parse-url (car e))))
                                :test #'string=)))
      (car entry))))

(defun forgejo--host-from-hosts-list ()
  "Pick host from `forgejo-hosts'.
If exactly one entry, use it.  Otherwise prompt."
  (unless forgejo-hosts
    (user-error "No Forgejo instances configured; set `forgejo-hosts'"))
  (if (= (length forgejo-hosts) 1)
      (caar forgejo-hosts)
    (completing-read "Forgejo instance: "
                     (mapcar #'car forgejo-hosts) nil t)))

(defun forgejo--resolve-host-for-repo (owner repo)
  "Resolve the host URL for OWNER/REPO.
Checks DB first, then falls back to `forgejo--resolve-host'."
  (let ((db-hosts (ignore-errors
                    (forgejo-db-get-hosts-for-repo owner repo))))
    (cond
     ((= (length db-hosts) 1)
      (forgejo--host-url-for-hostname (car db-hosts)))
     ((> (length db-hosts) 1)
      (let ((urls (mapcar #'forgejo--host-url-for-hostname db-hosts)))
        (completing-read (format "Host for %s/%s: " owner repo)
                         urls nil t)))
     (t (forgejo--resolve-host)))))

(defun forgejo--host-url-for-hostname (hostname)
  "Look up full URL for HOSTNAME from `forgejo-hosts'.
Falls back to \"https://HOSTNAME\" if not found."
  (or (car (cl-find hostname forgejo-hosts
                    :key (lambda (e)
                           (url-host (url-generic-parse-url (car e))))
                    :test #'string=))
      (format "https://%s" hostname)))

;;; Token resolution

(defun forgejo--hosts-token (host-url)
  "Look up inline token for HOST-URL in `forgejo-hosts'."
  (cadr (cl-find (url-host (url-generic-parse-url host-url))
                 forgejo-hosts
                 :key (lambda (e) (url-host (url-generic-parse-url (car e))))
                 :test #'string=)))

(defun forgejo--auth-source-token (host-url)
  "Look up the Forgejo token via `auth-source' for HOST-URL."
  (when-let* ((host (url-host (url-generic-parse-url host-url)))
              (found (car (auth-source-search :host host :max 1)))
              (secret (plist-get found :secret)))
    (if (functionp secret)
        (funcall secret)
      secret)))

(defun forgejo--validate-host (host-url)
  "Signal an error if HOST-URL is not in `forgejo-hosts'."
  (unless (cl-find (url-host (url-generic-parse-url host-url))
                   forgejo-hosts
                   :key (lambda (e)
                          (url-host (url-generic-parse-url (car e))))
                   :test #'string=)
    (user-error "Host %s not configured in `forgejo-hosts'"
                (url-host (url-generic-parse-url host-url)))))

(defun forgejo-token (host-url)
  "Return the API token for HOST-URL.
Resolution order: inline token from `forgejo-hosts', auth-source,
`forgejo-token' variable."
  (forgejo--validate-host host-url)
  (or (forgejo--hosts-token host-url)
      (and forgejo-token-use-auth-source
           (forgejo--auth-source-token host-url))
      forgejo-token
      (user-error "No token for host %s; add to `forgejo-hosts' or auth-source"
                  (url-host (url-generic-parse-url host-url)))))

;;; Top-level menu

(declare-function forgejo-repo-search "forgejo-repo.el" (query))
(declare-function forgejo-issue-list "forgejo-issue.el"
                  (&optional owner repo))
(declare-function forgejo-pull-list "forgejo-pull.el"
                  (&optional owner repo))
(declare-function forgejo-repo-create "forgejo-repo.el" (name))
(declare-function forgejo-watch-list "forgejo-watch.el" ())
(declare-function forgejo-utils-browse-repo "forgejo-utils.el"
                  (host-url owner repo))

(defvar forgejo-repo--host)

(keymap-popup-define forgejo-map
  "Forgejo."
  :group "Navigate"
  "s" ("Search repos" forgejo-repo-search)
  "i" ("Issues" forgejo-issue-list)
  "p" ("Pull requests" forgejo-pull-list)
  "n" ("Watch" forgejo-watch-list)
  :group "Actions"
  "c" ("Create repo" forgejo-repo-create)
  "b" ("Browse repo" forgejo-browse-repo))

;;;###autoload
(defun forgejo ()
  "Forgejo."
  (interactive)
  (keymap-popup 'forgejo-map))

(defun forgejo-browse-repo ()
  "Open a repository in the browser."
  (interactive)
  (let ((input (read-string "Repository (owner/repo): ")))
    (if (string-match "\\`\\([^/]+\\)/\\([^/]+\\)\\'" input)
        (let ((host-url (or forgejo-repo--host (forgejo--resolve-host))))
          (forgejo-utils-browse-repo host-url
                                     (match-string 1 input)
                                     (match-string 2 input)))
      (user-error "Invalid format; expected owner/repo"))))

;;; Repo search action-at-point

(declare-function forgejo-repo-search--owner-repo-at-point "forgejo-repo.el" ())

(keymap-popup-define forgejo-repo-action-map
  "Actions for repository at point."
  :group "Open"
  "i" ("Issues" forgejo-repo-action--issues)
  "p" ("Pull requests" forgejo-repo-action--pulls)
  "b" ("Browse" forgejo-repo-action--browse))

(defun forgejo-repo-action-at-point ()
  "Actions for repository at point."
  (interactive)
  (keymap-popup 'forgejo-repo-action-map))

(defun forgejo-repo-action--issues ()
  "List issues for the repo at point."
  (interactive)
  (when-let* ((pair (forgejo-repo-search--owner-repo-at-point)))
    (forgejo-issue-list (car pair) (cdr pair))))

(defun forgejo-repo-action--pulls ()
  "List pull requests for the repo at point."
  (interactive)
  (when-let* ((pair (forgejo-repo-search--owner-repo-at-point)))
    (forgejo-pull-list (car pair) (cdr pair))))

(defun forgejo-repo-action--browse ()
  "Open the repo at point in the browser."
  (interactive)
  (when-let* ((pair (forgejo-repo-search--owner-repo-at-point)))
    (forgejo-utils-browse-repo forgejo-repo--host (car pair) (cdr pair))))

(provide 'forgejo)
;;; forgejo.el ends here

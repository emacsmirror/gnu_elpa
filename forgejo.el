;;; forgejo.el --- Emacs Forgejo Front-end  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Thanos Apollo

;; Author: Thanos Apollo <public@thanosapollo.org>
;; Keywords: extensions
;; URL:
;; Package-Requires: ((emacs "29.1"))

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

;; Emacs front-end for Forgejo instances.
;;
;; Browse, filter, and view issues and pull requests.  Submit PRs via
;; AGit-Flow push options.  Caches API responses in a local SQLite
;; database for fast re-display.

;;; Code:

(require 'auth-source)
(require 'url-parse)

(defgroup forgejo nil
  "Emacs front-end for Forgejo instances."
  :group 'external
  :prefix "forgejo-")

(defcustom forgejo-host "https://codeberg.org"
  "URL of the Forgejo instance."
  :type 'string
  :group 'forgejo)

(defcustom forgejo-token nil
  "Personal access token.
When `forgejo-token-use-auth-source' is non-nil, auth-source is
tried first and this value is used as fallback."
  :type '(choice string (const nil))
  :group 'forgejo)

(defcustom forgejo-token-use-auth-source t
  "When non-nil, look up the token via `auth-source' first.
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

(defvar forgejo-db nil
  "SQLite database connection for the local cache.")

(defvar forgejo--api-default-limit nil
  "Cached default_paging_num from the instance's /settings/api.")

(defvar forgejo--api-max-items nil
  "Cached max_response_items from the instance's /settings/api.")

(defmacro forgejo-with-host (host &rest body)
  "Execute BODY with `forgejo-host' bound to HOST.
If HOST is nil, `forgejo-host' retains its current value."
  (declare (indent 1))
  `(let ((forgejo-host (or ,host forgejo-host)))
     ,@body))

(defun forgejo--auth-source-token ()
  "Look up the Forgejo token via `auth-source'.
Searches by host derived from `forgejo-host'."
  (when-let* ((host (url-host (url-generic-parse-url forgejo-host)))
              (found (car (auth-source-search :host host :max 1)))
              (secret (plist-get found :secret)))
    (if (functionp secret)
        (funcall secret)
      secret)))

(defun forgejo-token ()
  "Return the Forgejo API token.
Tries auth-source first when `forgejo-token-use-auth-source' is
non-nil, then falls back to the `forgejo-token' variable."
  (or (and forgejo-token-use-auth-source
           (forgejo--auth-source-token))
      forgejo-token
      (user-error "No Forgejo token configured; set `forgejo-token' or add one to auth-source")))

(provide 'forgejo)
;;; forgejo.el ends here

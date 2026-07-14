;;; xc.el --- Emacs interface for xc  -*- lexical-binding: t; -*-

;; Copyright © 2026 Ryan Prior <rprior@protonmail.com>
;; Author: Ryan Prior
;; URL: https://codeberg.org/ryanprior/xc.el
;; Package-Version: 20260713.1
;; Package-Requires: ((emacs "28.1"))
;; Keywords: processes docs
;; Version: 1.9

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:
;; An Emacs interface to the xc task-runner, <https://xcfile.dev/>
;; Use `xc' to launch a task defined in your README

;; The common use case for `xc' is to run build, test, deploy type commands
;; that are documented in your README. Putting the documented steps on
;; the ``hot path'' ensures they don't get out-of-date and aids in
;; discoverability. A unique feature of `xc' is that it supports both
;; Markdown and org-mode READMEs.

;;; Code:

(defgroup xc nil
  "An Emacs interface for xc."
  :prefix "xc-"
  :group 'tools
  :link '(url-link :tag "Codeberg" "https://codeberg.org/ryanprior/xc.el"))

(defcustom xc-executable nil
  "Use this executable to run xc.

Either a string containing the name or the path of the executable, or
nil to use the default `xc'."
  :group 'xc
  :type 'string)

(defcustom xc-type nil
  "Look for xc tasks in this type of documentation.

Either `org' or `md', or nil for default."
  :group 'xc
  :type 'string)

(defun xc--executable ()
  "Return non-nil when xc executable is found, or show a warning."
  (if-let* ((name (or xc-executable "xc"))
            (exec (executable-find name)))
      exec
    (if (string-equal name "xc")
        (display-warning 'xc "could not find `xc' on your PATH. Install it, adjust your PATH, or customize `xc-executable' to point to it.")
      (display-warning 'xc (format "no such file \"%s\" (from custom `xc-executable' value.)" xc-executable)))
    nil))

(defun xc--run (args)
  "Run xc with provided ARGS; nil if it fails."
  (with-temp-buffer
    (when-let* ((xc (xc--executable))
                (command (concat xc " " args))
                (exit-code (call-process-shell-command command nil (current-buffer) t)))
      (if (zerop exit-code)
          (buffer-string)
        nil))))

(defun xc--tasks (&optional file)
  "Get available tasks using xc.

If FILE is provided, look for tasks in that file specifically."
  (when-let* ((xc-output (thread-first
                           "-short"
                           (concat
                            (and xc-type (concat " -type " xc-type))
                            (and file (concat " -file " file)))
                           xc--run)))
    (split-string xc-output)))

;;;###autoload
(defun xc (task)
  "Run TASK using `xc'."
  (interactive (if-let* ((tasks (xc--tasks)))
                   (list (completing-read "Task: " tasks nil t))
                 (progn
                   (thread-last
                     (if (project-current)
                         (project-root (project-current))
                       default-directory)
                     (format "No tasks for %s")
                     (display-warning 'xc))
                   '(nil))))
  (when task
    (let* ((output-buffer (concat "*xc-" task "*")))
      (thread-first (xc--executable)
                    (concat " " task
                            (and xc-type (concat " -type " xc-type)))
                    (async-shell-command output-buffer))
      (pop-to-buffer output-buffer))))

(provide 'xc)

;;; xc.el ends here

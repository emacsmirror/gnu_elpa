;;; ex.el --- Emacs interface for xc  -*- lexical-binding: t; -*-

;; Copyright © 2025 Ryan Prior <rprior@protonmail.com>
;; Author: Ryan Prior
;; URL: https://codeberg.org/ryanprior/xc.el
;; Package-Version: 20251029.1
;; Keywords: task-runner xc markdown org-mode
;; Version: 1

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

;;; Code:

(defgroup xc nil
  "An Emacs interface for xc"
  :prefix "xc-"
  :group 'tools
  :link '(url-link :tag "Codeberg" "https://codeberg.org/ryanprior/xc.el"))

(defcustom xc-executable nil
  "The executable of the xc task runner

Either a string containing the name or the path of the executable, or
nil to use the default `xc'."
  :group 'xc
  :type 'string)

(defcustom xc-type nil
  "Type of documentation xc should look for

Either `org' or `md', or nil for default."
  :group 'xc
  :type 'string)

(defun xc--run (args)
  (with-temp-buffer
    (let* ((xc (or xc-executable "xc"))
           (command (concat xc " " args))
           (exit-code (call-process-shell-command command nil (current-buffer) t)))
      (if (zerop exit-code)
          (buffer-string)
        nil))))

(defun xc--tasks (&optional file)
  "Get available tasks using xc"
  (when-let ((xc-output (thread-first
                          "-short"
                          (concat
                           (and xc-type (concat " -type " xc-type))
                           (and file (concat " -file " file)))
                          xc--run)))
    (split-string xc-output)))

;;;###autoload
(defun xc (task)
  "Run TASK using `xc'"
  (interactive (list (completing-read "Task: " (xc--tasks))))
  (let* ((output-buffer (concat "*xc-" task "*")))
    (thread-first xc-executable
                  (concat " " task
                          (and xc-type (concat " -type " xc-type)))
                  (async-shell-command output-buffer))
    (pop-to-buffer output-buffer)))

(defun xc-treemacs-RET-task-action (&optional _)
  "Run the task associated with the Treemacs button at point"
  (let* ((btn (treemacs-current-button))
         (btn-path (treemacs-button-get btn :path))
         (default-directory (nth 0 btn-path))
         (task-name (treemacs-button-get btn :key)))
    (xc task-name)))

(defun xc-treemacs-DBLCLICK-task-action (&optional _)
  "Run the task associated with the Treemacs button at point"
  (xc-treemacs-RET-task-action))

(defun xc--treemacs-get-tasks (&optional _)
  "Get available tasks for the Treemacs project at point"
  (let* ((btn (treemacs-current-button))
         (btn-path (treemacs-button-get btn :path))
         (default-directory (nth 0 btn-path)))
    (xc--tasks)))

(defun xc--treemacs-tasks-p (&optional _)
  (let* ((btn (treemacs-current-button))
         (btn-path (treemacs-button-get btn :path))
         (default-directory btn-path))
    (xc--tasks)))

;;;###autoload
(defun xc-treemacs-init ()
  "Initialize xc-Treemacs integration"
  (interactive)
  (require 'treemacs-treelib)
  (treemacs-define-entry-node-type xc-tasks
    :key 'xc-tasks
    :label "[XC TASKS]"
    :open-icon (treemacs-get-icon-value 'list)
    :closed-icon (treemacs-get-icon-value 'list)
    :children (xc--treemacs-get-tasks)
    :child-type 'xc-task-node)

  (treemacs-define-leaf-node-type xc-task-node
    :icon "• "
    :label (propertize item 'face 'font-lock-string-face)
    :key item
    :ret-action #'xc-treemacs-RET-task-action
    :double-click-action #'xc-treemacs-DBLCLICK-task-action)

  (treemacs-enable-project-extension
   :extension 'xc-tasks
   :position 'top
   :predicate #'xc--treemacs-tasks-p))

(provide 'xc)
;;; xc.el ends here

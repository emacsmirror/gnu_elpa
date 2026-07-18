;;; xc-treemacs.el --- Treemacs interface for xc  -*- lexical-binding: t; -*-

;; Copyright © 2026  Free Software Foundation, Inc.

;; Author: Ryan Prior <rprior@protonmail.com>
;; URL: https://codeberg.org/ryanprior/xc.el
;; Package-Version: 20260717.1
;; Package-Requires: ((emacs "25.1")(xc "1.10")(treemacs "3.2"))
;; Keywords: processes docs
;; Version: 1.10

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
;; A Treemacs interface to the xc task-runner, <https://xcfile.dev/>

;; The common use case for `xc' is to run build, test, deploy type commands
;; that are documented in your README. Putting the documented steps on
;; the ``hot path'' ensures they don't get out-of-date and aids in
;; discoverability. A unique feature of `xc' is that it supports both
;; Markdown and org-mode READMEs.

;;; Code:

(require 'treemacs-treelib nil t)
(require 'treemacs-core-utils nil t)
(require 'xc)

(declare-function treemacs-current-button "treemacs-core-utils")
(declare-function treemacs-button-get "treemacs-core-utils")
(declare-function treemacs-enable-project-extension "treemacs-treelib")

(defun xc-treemacs-RET-task-action (&optional _)
  "Run the task associated with the Treemacs button at point."
  (let* ((btn (treemacs-current-button))
         (btn-path (treemacs-button-get btn :path))
         (default-directory (nth 0 btn-path))
         (task-name (treemacs-button-get btn :key)))
    (xc task-name)))

(defun xc-treemacs-DBLCLICK-task-action (&optional _)
  "Run the task associated with the Treemacs button at point."
  (xc-treemacs-RET-task-action))

(defun xc-treemacs--get-tasks (&optional _)
  "Get available tasks for the Treemacs project at point."
  (let* ((btn (treemacs-current-button))
         (btn-path (treemacs-button-get btn :path))
         (default-directory (nth 0 btn-path)))
    (xc--tasks)))

(defun xc-treemacs--tasks-p (&optional _)
  "Non-nil if the Treemacs project at point has xc tasks; nil otherwise."
  (let* ((btn (treemacs-current-button))
         (btn-path (treemacs-button-get btn :path))
         (default-directory btn-path))
    (xc--tasks)))

(xc--if-macro-fboundp treemacs-define-entry-node-type
    (progn
      (treemacs-define-entry-node-type xc-tasks
        :key 'xc-tasks
        :label "[XC TASKS]"
        :open-icon (treemacs-get-icon-value 'list)
        :closed-icon (treemacs-get-icon-value 'list)
        :children (xc-treemacs--get-tasks)
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
       :predicate #'xc-treemacs--tasks-p)))

(provide 'xc-treemacs)

;;; xc-treemacs.el ends here

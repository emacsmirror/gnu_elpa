;;; doc-view-follow.el --- Synchronize windows showing the same document -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025  Free Software Foundation, Inc.

;; Author: Paul D. Nelson <ultrono@gmail.com>
;; Version: 0.2
;; URL: https://github.com/ultronozm/doc-view-follow.el
;; Package-Requires: ((emacs "29.1"))
;; Keywords: convenience

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

;; doc-view-follow.el provides a convenient way to synchronize page
;; navigation between multiple windows displaying the same document
;; (PS/PDF/DVI/DjVu, etc.).  It is essentially an analogue of the
;; built-in `follow-mode', but for document buffers rather than text
;; buffers.
;;
;; With `doc-view-follow-mode' enabled, navigating pages in one window
;; automatically adjusts the other windows to show adjacent pages.  In
;; particular, this allows a "book view" where the document is shown
;; in two side-by-side windows on consecutive pages.
;;
;; `doc-view-mode' (built-in to Emacs) and `pdf-view-mode' (from
;; pdf-tools) are supported
;;
;; Usage:
;; - Enable globally: M-x global-doc-view-follow-mode
;; - Enable locally: M-x doc-view-follow-mode
;;
;; Once activated, split your Emacs frame (e.g., vertically with
;; \\[split-window-right]) and open your document in both windows.
;; Navigating with standard commands (like 'n' for next page and 'p'
;; for previous page) in one window automatically synchronizes the
;; view in the other.

;;; Code:

(require 'follow)
(require 'compat)

(defgroup doc-view-follow nil
  "Synchronize pages between windows displaying the same document."
  :group 'convenience)

(defvar-keymap doc-view-follow-mode-submap
  "1"   #'follow-delete-other-windows-and-split
  "b"   #'follow-switch-to-buffer
  "C-b" #'follow-switch-to-buffer-all
  "<"   #'follow-first-window
  ">"   #'follow-last-window
  "n"   #'follow-next-window
  "p"   #'follow-previous-window)

(defvar doc-view-follow-mode-prefix-key follow-mode-prefix-key
  "Prefix key to use for follow commands in Doc View Follow mode.
By default, this matches `follow-mode-prefix-key'.")

(defun doc-view-follow--update-prefix-key (_sym newval op _where)
  "Update doc-view-follow mode keymap when the prefix key changes.
Called when `follow-mode-prefix-key' is customized."
  (when (and (eq op 'set) (boundp 'doc-view-follow-mode-map))
    (keymap-unset doc-view-follow-mode-map doc-view-follow-mode-prefix-key t)
    (setq doc-view-follow-mode-prefix-key newval)
    (keymap-set doc-view-follow-mode-map newval doc-view-follow-mode-submap)))

(add-variable-watcher
 'follow-mode-prefix-key #'doc-view-follow--update-prefix-key)

(defvar-keymap doc-view-follow-mode-map
  "<remap> <doc-view-first-page>" #'doc-view-follow-beginning-of-buffer
  "<remap> <doc-view-last-page>" #'doc-view-follow-end-of-buffer
  "<remap> <pdf-view-first-page>" #'doc-view-follow-beginning-of-buffer
  "<remap> <pdf-view-last-page>" #'doc-view-follow-end-of-buffer)

(keymap-set doc-view-follow-mode-map
            doc-view-follow-mode-prefix-key doc-view-follow-mode-submap)

;;;###autoload
(cl-defgeneric doc-view-follow-supported-p (_mode)
  "Check if MODE supports `doc-view-follow-mode'."
  nil)

(cl-defgeneric doc-view-follow-setup (mode)
  "Setup function for `doc-view-follow-mode' in MODE."
  (error "`doc-view-follow-mode' is not supported in %s" mode))

(cl-defgeneric doc-view-follow-teardown (mode)
  "Teardown function for `doc-view-follow-mode' in MODE."
  (error "`doc-view-follow-mode' is not supported in %s" mode))

(cl-defgeneric doc-view-follow-set-page (_page mode)
  "Set PAGE in MODE document buffer."
  (error "`doc-view-follow-mode' is not supported in %s" mode))

(cl-defgeneric doc-view-follow-get-page (mode)
  "Get current page number in MODE document buffer."
  (error "`doc-view-follow-mode' is not supported in %s" mode))

(cl-defgeneric doc-view-follow-get-page-count (mode)
  "Get total number of pages in MODE document buffer."
  (error "`doc-view-follow-mode' is not supported in %s" mode))

(defvar doc-view-follow--sync-in-progress nil
  "Flag to prevent recursive sync operations.")

(defun doc-view-follow-beginning-of-buffer ()
  "Navigate to the beginning of the document, Doc-View-Follow mode style.

This selects the first window in the window chain and displays the first
page of the document in that window.  Other windows in the chain will
display consecutive pages."
  (interactive)
  (let ((windows (follow-all-followers)))
    (when (> (length windows) 0)
      (select-window (car windows))
      (doc-view-follow-set-page 1 major-mode)
      (doc-view-follow-sync-pages))))

(defun doc-view-follow-end-of-buffer ()
  "Navigate to the end of the document, Doc-View-Follow mode style.

This selects the last window in the window chain and displays the last
page of the document in that window."
  (interactive)
  (let ((windows (follow-all-followers)))
    (when (> (length windows) 0)
      (select-window (car (last windows)))
      (let ((last-page (doc-view-follow-get-page-count major-mode)))
        (doc-view-follow-set-page last-page major-mode)
        (doc-view-follow-sync-pages)))))

;;;###autoload
(define-minor-mode doc-view-follow-mode
  "Minor mode to sync pages between document windows.

With `doc-view-follow-mode' enabled, navigating pages in one window
automatically adjusts the other windows to show adjacent pages.  This
allows a \"book view\" where the document is shown in multiple windows
displaying consecutive pages.

The following commands are available on the keymap bound to the value of
`follow-mode-prefix':

\\{doc-view-follow-mode-submap}"
  :global nil
  :keymap doc-view-follow-mode-map
  (unless (doc-view-follow-supported-p major-mode)
    (error "`doc-view-follow-mode' not supported in %s" major-mode))
  (if doc-view-follow-mode
      (progn
        (doc-view-follow-setup major-mode)
        (doc-view-follow-sync-pages))
    (doc-view-follow-teardown major-mode)))

(defun doc-view-follow-sync-pages (&rest _)
  "Synchronize document pages between windows."
  (when (and doc-view-follow-mode (not doc-view-follow--sync-in-progress))
    (let ((doc-view-follow--sync-in-progress t)
          (windows (follow-all-followers)))
      (when (> (length windows) 1)
        (let* ((idx (seq-position windows (selected-window)))
               (page (- (doc-view-follow-get-page major-mode) idx)))
          (dolist (win windows)
            (unless (eq win (selected-window))
              (with-selected-window win
                (doc-view-follow-set-page page major-mode)))
            (setq page (1+ page))))))))

(defun doc-view-follow--maybe-enable ()
  "Enable `doc-view-follow-mode' if appropriate for this buffer."
  (when (doc-view-follow-supported-p major-mode)
    (unless doc-view-follow-mode
      (doc-view-follow-mode 1))))

;;;###autoload
(define-globalized-minor-mode global-doc-view-follow-mode
  doc-view-follow-mode doc-view-follow--maybe-enable)

(with-eval-after-load 'doc-view
  (declare-function doc-view-goto-page "doc-view")
  (declare-function doc-view-last-page-number "doc-view")
  (declare-function doc-view-current-page "doc-view")

  (cl-defmethod doc-view-follow-supported-p ((_mode (eql doc-view-mode))) t)
  (cl-defmethod doc-view-follow-setup ((_mode (eql doc-view-mode)))
    (advice-add 'doc-view-goto-page :after #'doc-view-follow-sync-pages))
  (cl-defmethod doc-view-follow-teardown ((_mode (eql doc-view-mode)))
    (advice-remove 'doc-view-goto-page #'doc-view-follow-sync-pages))
  (cl-defmethod doc-view-follow-set-page (page (_mode (eql doc-view-mode)))
    (doc-view-goto-page
     (max 1 (min page (doc-view-follow-get-page-count 'doc-view-mode)))))
  (cl-defmethod doc-view-follow-get-page ((_mode (eql doc-view-mode)))
    (doc-view-current-page))
  (cl-defmethod doc-view-follow-get-page-count ((_mode (eql doc-view-mode)))
    (doc-view-last-page-number)))

(with-eval-after-load 'pdf-tools
  (declare-function pdf-view-goto-page "pdf-view")
  (declare-function pdf-cache-number-of-pages "pdf-cache")
  (defvar pdf-view-inhibit-redisplay)
  (cl-defmethod doc-view-follow-supported-p ((_mode (eql pdf-view-mode))) t)
  (cl-defmethod doc-view-follow-setup ((_mode (eql pdf-view-mode)))
    (add-hook 'pdf-view-after-change-page-hook
              #'doc-view-follow-sync-pages nil t))
  (cl-defmethod doc-view-follow-teardown ((_mode (eql pdf-view-mode)))
    (remove-hook 'pdf-view-after-change-page-hook
                 #'doc-view-follow-sync-pages t))
  (cl-defmethod doc-view-follow-set-page (page (_mode (eql pdf-view-mode)))
    (let ((pdf-view-inhibit-redisplay nil))
      (pdf-view-goto-page
       (max 1 (min page (doc-view-follow-get-page-count 'pdf-view-mode))))))
  (cl-defmethod doc-view-follow-get-page ((_mode (eql pdf-view-mode)))
    (require 'image-mode)
    (declare-function image-mode-window-get "image-mode")
    (image-mode-window-get 'page))
  (cl-defmethod doc-view-follow-get-page-count ((_mode (eql pdf-view-mode)))
    (pdf-cache-number-of-pages)))

(provide 'doc-view-follow)
;;; doc-view-follow.el ends here

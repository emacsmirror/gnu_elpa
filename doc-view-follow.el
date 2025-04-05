;;; doc-view-follow.el --- Synchronize windows showing the same document -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025  Free Software Foundation, Inc.

;; Author: Paul D. Nelson <ultrono@gmail.com>
;; Version: 0.2
;; URL: https://github.com/ultronozm/doc-view-follow.el
;; Package-Requires: ((emacs "27.1"))
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
;; `doc-view-mode' (built-in to Emacs) is supported by default.  To
;; enable support for `pdf-view-mode' (from pdf-tools), add the
;; following to your init file:
;;
;;   (with-eval-after-load 'pdf-view
;;     (require 'doc-view-follow-pdf-tools))
;;
;; The file `doc-view-follow-pdf-tools.el' shows by example how the
;; package might be extended to other document viewing modes.
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

(defgroup doc-view-follow nil
  "Synchronize pages between two windows displaying the same document."
  :group 'convenience)

(cl-defgeneric doc-view-follow-supported-p ()
  "Check if the current buffer is supported by `doc-view-follow-mode'."
  nil)

(cl-defgeneric doc-view-follow-setup ()
  "Setup function for `doc-view-follow-mode'."
  (error "`doc-view-follow-mode' is not supported in this buffer"))

(cl-defgeneric doc-view-follow-teardown ()
  "Teardown function for `doc-view-follow-mode'."
  (error "`doc-view-follow-mode' is not supported in this buffer"))

(cl-defgeneric doc-view-follow-set-page (_page)
  "Go to PAGE in the current document buffer.
Clamps PAGE to the valid range of pages."
  (error "`doc-view-follow-mode' is not supported in this buffer"))

(cl-defgeneric doc-view-follow-get-page ()
  "Return the current page number in the document buffer."
  (error "`doc-view-follow-mode' is not supported in this buffer"))

(defvar doc-view-follow--sync-in-progress nil
  "Flag to prevent recursive sync operations.")

;;;###autoload
(define-minor-mode doc-view-follow-mode
  "Minor mode to sync pages between windows showing the same document."
  :global nil
  :lighter nil
  (unless (doc-view-follow-supported-p)
    (error "`doc-view-follow-mode' is not supported in this buffer"))
  (if doc-view-follow-mode
      (progn
        (doc-view-follow-setup)
        (doc-view-follow-sync-pages))
    (doc-view-follow-teardown)))

(defun doc-view-follow-sync-pages (&rest _args)
  "Sync pages between windows showing the same document."
  (when (and doc-view-follow-mode
             (not doc-view-follow--sync-in-progress))
    (let ((doc-view-follow--sync-in-progress t)
          (windows (follow-all-followers)))
      (when (> (length windows) 1)
        (let* ((current-i (seq-position windows (selected-window)))
               (current-page (doc-view-follow-get-page))
               (page (- current-page current-i)))
          (dolist (win windows)
            (unless (eq (selected-window) win)
              (with-selected-window win
                (doc-view-follow-set-page page)))
            (incf page)))))))

(defun doc-view-follow--maybe-enable ()
  "Enable `doc-view-follow-mode' if appropriate for this buffer."
  (when (doc-view-follow-supported-p)
    (unless doc-view-follow-mode
      (doc-view-follow-mode 1))))

;;;###autoload
(define-globalized-minor-mode global-doc-view-follow-mode
  doc-view-follow-mode doc-view-follow--maybe-enable)

(require 'doc-view)

(cl-defmethod doc-view-follow-supported-p (&context (major-mode doc-view-mode))
  "When MAJOR-MODE is `doc-view-mode', return t."
  t)

(cl-defmethod doc-view-follow-setup (&context (major-mode doc-view-mode))
  "When MAJOR-MODE is `doc-view-mode', setup `doc-view-follow-mode'."
  (advice-add 'doc-view-goto-page :after #'doc-view-follow-sync-pages))

(cl-defmethod doc-view-follow-teardown (&context (major-mode doc-view-mode))
  "When MAJOR-MODE is `doc-view-mode', teardown `doc-view-follow-mode'."
  (unless
      (seq-some (lambda (buf)
                  (and (eq (buffer-local-value 'major-mode buf) 'doc-view-mode)
                       (buffer-local-value 'doc-view-follow-mode buf)))
                (buffer-list))
    (advice-remove 'doc-view-goto-page #'doc-view-follow-sync-pages)))

(cl-defmethod doc-view-follow-set-page (page &context (major-mode doc-view-mode))
  "When MAJOR-MODE is `doc-view-mode', go to PAGE in the document buffer."
  (let ((page (max 1 (min page (doc-view-last-page-number)))))
    (doc-view-goto-page page)))

(cl-defmethod doc-view-follow-get-page (&context (major-mode doc-view-mode))
  "When MAJOR-MODE is `doc-view-mode', return the current page number."
  (doc-view-current-page))

(provide 'doc-view-follow)
;;; doc-view-follow.el ends here

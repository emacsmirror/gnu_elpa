;;; doc-follow.el --- synchronize windows showing the same document -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Paul D. Nelson

;; Author: Paul D. Nelson <nelson.paul.david@gmail.com>
;; Version: 0.1
;; URL: https://github.com/ultronozm/doc-follow.el
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

;; doc-follow.el provides a convenient way to synchronize page
;; navigation between multiple windows displaying the same document
;; (PS/PDF/DVI/DjVu, etc.).
;;
;; With `doc-follow-mode' enabled, navigating pages in one window
;; automatically adjusts the other windows to show adjacent pages.  In
;; particular, this allows a "book view" where the document is shown
;; in two side-by-side windows on consecutive pages.
;;
;; `doc-view-mode' (built-in to Emacs) and `pdf-view-mode' (from
;; pdf-tools) are supported by default.  Additional viewing modes can
;; be added via the `doc-follow-modes' variable.
;;
;; Usage:
;; - Enable globally: M-x global-doc-follow-mode
;; - Enable locally: M-x doc-follow-mode
;;
;; Once activated, split your Emacs frame (e.g., vertically with
;; \\[split-window-right]) and open your document in both windows.
;; Navigating with standard commands (like 'n' for next page and 'p'
;; for previous page) in one window automatically synchronizes the
;; view in the other.

;;; Code:

(require 'follow)

(defgroup doc-follow nil
  "Synchronize pages between two windows displaying the same document."
  :group 'convenience)

(defvar doc-follow-modes
  '((doc-view-mode
     :goto doc-view-goto-page
     :next doc-view-next-page
     :prev doc-view-previous-page
     :current (lambda () (doc-view-current-page))
     :max (lambda () (doc-view-last-page-number)))
    (pdf-view-mode
     :goto pdf-view-goto-page
     :next pdf-view-next-page-command
     :prev pdf-view-previous-page-command
     :current (lambda () (pdf-view-current-page))
     :max (lambda () (pdf-cache-number-of-pages))))
  "Alist of supported major modes and relevant functions.
Each entry has the format: (MAJOR-MODE . CONFIG), where CONFIG is a list
with entries:

:goto GOTO-PAGE-FUNCTION
:next NEXT-PAGE-FUNCTION
:prev PREV-PAGE-FUNCTION
:current FUNCTION-RETURNING-CURRENT-PAGE
:max FUNCTION-RETURNING-MAX-PAGE

Other packages can add support for additional document viewing modes
by adding entries to this list.")

(defun doc-follow--call-func (mode-config action &rest args)
  "Call function for ACTION from MODE-CONFIG with ARGS."
  (apply (plist-get mode-config action) args))

(defvar doc-follow--sync-in-progress nil
  "Flag to prevent recursive sync operations.")

;;;###autoload
(define-minor-mode doc-follow-mode
  "Minor mode to sync pages between two windows showing the same document."
  :global nil
  (if doc-follow-mode
      (doc-follow--manage-advice 'add)
    (unless (doc-follow--some-buffer-active-p)
      (doc-follow--manage-advice 'remove))))

(defun doc-follow--sync-pages (&rest _args)
  "Sync pages between windows showing the same document."
  (when (and doc-follow-mode
             (not doc-follow--sync-in-progress))
    (let ((doc-follow--sync-in-progress t))
      (when-let*
          ((cfg (cdr (assoc major-mode doc-follow-modes)))
           (windows (follow-all-followers))
           ((> (length windows) 1)))
        (let* ((current-page (doc-follow--call-func cfg :current))
               (max-page (doc-follow--call-func cfg :max))
               (current-window (selected-window))
               (window-index (seq-position windows current-window)))
          (seq-do-indexed
           (lambda (win i)
             (let ((target-page
                    (min max-page
                         (max 1 (+ current-page (- i window-index))))))
               (with-selected-window win
                 (doc-follow--call-func cfg :goto target-page))))
           windows))))))

(defun doc-follow--manage-advice (add-or-remove)
  "Add or remove advice for all functions in `doc-follow-modes'.
ADD-OR-REMOVE should be either \\='add or \\='remove."
  (dolist (mode-entry doc-follow-modes)
    (dolist (action '(:goto :next :prev))
      (when-let* ((func (plist-get (cdr mode-entry) action)))
        (if (eq add-or-remove 'add)
            (advice-add func :after #'doc-follow--sync-pages)
          (advice-remove func #'doc-follow--sync-pages))))))

(defun doc-follow--some-buffer-active-p ()
  "Return non-nil if some buffer has `doc-follow-mode' active."
  (seq-some (lambda (buf)
              (buffer-local-value 'doc-follow-mode buf))
            (buffer-list)))

(defun doc-follow--maybe-enable ()
  "Enable `doc-follow-mode' if appropriate for this buffer."
  (when (assq major-mode doc-follow-modes)
    (doc-follow-mode 1)))

;;;###autoload
(define-globalized-minor-mode global-doc-follow-mode
  doc-follow-mode
  doc-follow--maybe-enable)

(provide 'doc-follow)
;;; doc-follow.el ends here

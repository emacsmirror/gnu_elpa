;;; doc-follow.el --- Sync two windows showing the same document  -*- lexical-binding: t; -*-

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

;; This package provides a minor mode, `doc-follow-mode', that
;; synchronizes page navigation between windows displaying the same
;; document, making it so that when you navigate to a page in one
;; window, the other windows will navigate to neighboring pages.

;; Supports `doc-view-mode' and `pdf-view-mode'.  You can customize
;; the `doc-follow-modes' variable to add support for additional
;; document viewing modes or modify the behavior for existing modes.

;;; Code:

(require 'timer)

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

(defun doc-follow--order-windows (windows)
  "Order WINDOWS based on their position: leftmost, then topmost."
  (sort windows (lambda (window-a window-b)
                  (let* ((edges-a (window-edges window-a))
                         (edges-b (window-edges window-b))
                         (left-a (nth 0 edges-a))
                         (left-b (nth 0 edges-b))
                         (top-a (nth 1 edges-a))
                         (top-b (nth 1 edges-b)))
                    (or (< left-a left-b)
                        (and (= left-a left-b) (< top-a top-b)))))))

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
           (windows (doc-follow--order-windows
                     (get-buffer-window-list nil nil nil)))
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
  "Add or remove advice for all functions in `doc-follow-modes`.
ADD-OR-REMOVE should be either 'add or 'remove."
  (dolist (mode-entry doc-follow-modes)
    (dolist (action '(:goto :next :prev))
      (when-let ((func (plist-get (cdr mode-entry) action)))
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

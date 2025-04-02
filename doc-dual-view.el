;;; doc-dual-view.el --- Sync two windows showing the same document  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Paul D. Nelson

;; Author: Paul D. Nelson <nelson.paul.david@gmail.com>
;; Version: 0.1
;; URL: https://github.com/ultronozm/doc-dual-view.el
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

;; This package provides a minor mode, `doc-dual-view-mode', that
;; synchronizes page navigation between two windows displaying the
;; same document, making it so that when you navigate to a page in one
;; window, the other window will navigate to a neighboring page, so
;; that the second window is always one page ahead of the first.

;; Supports `doc-view-mode' and `pdf-view-mode'.  You can customize
;; the `doc-dual-view-modes' variable to add support for additional
;; document viewing modes or modify the behavior for existing modes.

;;; Code:

(require 'timer)

(defgroup doc-dual-view nil
  "Synchronize pages between two windows displaying the same document."
  :group 'convenience)

(defcustom doc-dual-view-modes
  '((pdf-view-mode
     (pdf-view-goto-page
      pdf-view-next-page-command
      pdf-view-previous-page-command)
     (lambda () (pdf-view-current-page)) ; convert macro to function
     (lambda () (pdf-cache-number-of-pages)))
    (doc-view-mode
     (doc-view-goto-page
      doc-view-next-page
      doc-view-previous-page)
     (lambda () (doc-view-current-page))
     (lambda () (doc-view-last-page-number))))
  "Alist for supported modes.
List of mode-specific functions of the form

(MAJOR-MODE GOTO-FUNCTIONS CURRENT-PAGE-FUNCTION MAX-PAGE-FUNCTION),

where GOTO-FUNCTIONS is of the form (GOTO-FUNC NEXT-FUNC PREV-FUNC)."
  :type '(repeat (list (symbol :tag "Major Mode")
                       (repeat :tag "Goto Page Functions" symbol)
                       (function :tag "Current Page Function")
                       (function :tag "Max Page Function"))))

(defun doc-dual-view--order-windows (windows)
  "Order WINDOWS based on their position: leftmost, then topmost."
  (sort windows
        (lambda (window-a window-b)
          (let* ((edges-a (window-edges window-a))
                 (edges-b (window-edges window-b))
                 (left-a (nth 0 edges-a))
                 (left-b (nth 0 edges-b))
                 (top-a (nth 1 edges-a))
                 (top-b (nth 1 edges-b)))
            (or (< left-a left-b)
                (and (= left-a left-b)
                     (< top-a top-b)))))))

(defvar doc-dual-view--sync-in-progress nil
  "Flag to prevent recursive sync operations.")

(defun doc-dual-view--sync-pages (&rest _args)
  "Sync pages between windows showing the same document."
  (unless doc-dual-view--sync-in-progress
    (let ((doc-dual-view--sync-in-progress t))
      (when-let*
          ((mode-funcs (assoc major-mode doc-dual-view-modes))
           (windows (doc-dual-view--order-windows
                     (get-buffer-window-list nil nil nil)))
           ((> (length windows) 1)))
        (let* ((goto-func (car (nth 1 mode-funcs)))
               (current-page-func (nth 2 mode-funcs))
               (current-page (funcall current-page-func))
               (max-page-func (nth 3 mode-funcs))
               (max-page (funcall max-page-func))
               (current-window (selected-window))
               (window-index (seq-position windows current-window)))
          (seq-do-indexed
           (lambda (win i)
             (when (and (not (eq win current-window))
                        (window-live-p win))
               (let ((target-page
                      (min max-page
                           (max 1 (+ current-page (- i window-index))))))
                 (with-selected-window win
                   (let ((current (funcall current-page-func)))
                     (when (not (= current target-page))
                       (run-with-idle-timer
                        0.001 nil
                        (lambda (target-win func page)
                          (when (window-live-p target-win)
                            (with-selected-window target-win
                              (funcall func page))))
                        win goto-func target-page)))))))
           windows))))))

;;;###autoload
(define-minor-mode doc-dual-view-mode
  "Minor mode to sync pages between two windows showing the same document."
  :global nil
  (dolist (mode-funcs doc-dual-view-modes)
    (let ((goto-funcs (cadr mode-funcs)))
      (dolist (goto-func goto-funcs)
        (if doc-dual-view-mode
            (advice-add goto-func :after #'doc-dual-view--sync-pages)
          (advice-remove goto-func #'doc-dual-view--sync-pages))))))

(defun doc-dual-view--maybe-enable ()
  "Enable `doc-dual-view-mode' if appropriate for this buffer."
  (when (assq major-mode doc-dual-view-modes)
    (doc-dual-view-mode 1)))

;;;###autoload
(define-globalized-minor-mode global-doc-dual-view-mode
  doc-dual-view-mode
  doc-dual-view--maybe-enable)

(provide 'doc-dual-view)
;;; doc-dual-view.el ends here

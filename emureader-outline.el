;;; emureader-outline.el --- imenu and outline-mode integration for the EMuReader  -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Free Software Foundation, Inc.

;; Author: Divya Ranjan Pattanaik <divya@dialetheia.xyz>
;;         Tushar <tusharhero@sdf.org>

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

;;; Code:

(require 'emureader)
(require 'imenu)
(require 'outline)

(defvar emureader-current-doc-outline)

;;; Imenu

;;;###autoload
(defun emureader--outline-make-imenu-entry (plist)
  "Convert one outline PLIST to an imenu entry."
  (let* ((title (plist-get plist :title))
         (page (1+ (plist-get plist :page))) ; MuPDF does 0-indexing
         (children (plist-get plist :children)))
    (if children
        (cons title
              (mapcar #'emureader--outline-make-imenu-entry children))
      (cons title page))))

;;;###autoload
(defun emureader--outline-imenu-create-index ()
  "Turn `emureader-current-doc-outline' into an imenu index."
  (when emureader-current-doc-outline
    (mapcar #'emureader--outline-make-imenu-entry
	    emureader-current-doc-outline)))

;;;###autoload
(defun emureader--outline-imenu-goto (_name page)
  "Switch to PAGE.

Ignores the _NAME argument.
Just wraps `emureader-goto-page' for imenu compatibility."
  (emureader-goto-page page))

;;; Outline

(defvar-local emureader-outline--doc-buffername nil
  "Name of the document buffer whose outline was generated.")

(defvar-local emureader-outline--doc-window nil
  "The window from where the outline was generated.")

;;;###autoload
(defun emureader-outline-show ()
  "Show the document outline in a separate buffer.

The outline buffer inherits it's name from the original
document it was created from."
  (interactive)
  (unless (derived-mode-p 'emureader-mode)
    (user-error "Not in a emureader-mode buffer"))
  (unless emureader-current-doc-outline
    (user-error "This document has no outline"))
  (let* ((outline-data emureader-current-doc-outline)
         (source-buffer (current-buffer))
         (bufname (format "*Outline of %s*" (buffer-name source-buffer))))
    (with-current-buffer (get-buffer-create bufname)
      (when-let* ((emptyp (= 0 (buffer-size)))
		  (inhibit-read-only t))
        (emureader-outline-mode)
	(setq emureader-outline--doc-buffername (buffer-name source-buffer))
	(setq emureader-outline--doc-window (get-buffer-window source-buffer))
        (emureader--outline-insert-outline outline-data 1)
	(goto-char (point-min))))
    (pop-to-buffer bufname)))

(defun emureader--outline-insert-outline (outline level)
  "Recursively insert OUTLINE entries at LEVEL.

Each heading title is its own clickable button."
  (dolist (entry outline)
    (let ((title (plist-get entry :title))
          (page (1+ (plist-get entry :page))) ; MuPDF does 0-indexing
	  (children (plist-get entry :children)))
      ;; This cannot be part of label (title) because that will
      ;; obscure the outline TAB bindings with button bindings.
      (insert (concat (make-string level ?*) " "))
      (insert-text-button
       title
       'emureader-page page
       'action #'emureader-outline-goto-entry
       'follow-link t
       'help-echo "Jump to section.")
      (insert "\n")
      (if children
          (emureader--outline-insert-outline children (1+ level))))))

(defun emureader-outline-select-doc-window ()
  "Display and switch to the original document's window."
  (interactive)
  (let ((buf (get-buffer emureader-outline--doc-buffername)))
    (unless (buffer-live-p buf)
      (user-error "Document buffer no longer exists"))
    (let ((win (or (and (window-live-p emureader-outline--doc-window)
			(eq (window-buffer emureader-outline--doc-window) buf)
			emureader-outline--doc-window)
		   (get-buffer-window buf))))
      (if win
	  (select-window win)
	(pop-to-buffer buf))
      (setq emureader-outline--doc-window (selected-window)))))

(defvar-keymap emureader-outline-mode-map
  :doc "Keymap for `emureader-outline-mode'"
  "p"        #'previous-line
  "n"        #'next-line
  "o"        #'emureader-outline-select-doc-window
  "q"        #'quit-window
  "RET"      #'emureader-outline-visit-page
  "M-RET"    #'emureader-outline-visit-page)

(define-derived-mode emureader-outline-mode outline-mode "EMuReader Outline"
  "Major mode for navigating document outlines."
  (setq buffer-read-only t)
  (setq-local outline-regexp "^\\*+ ")
  (use-local-map emureader-outline-mode-map))

(defun emureader-outline-goto-entry (button)
  "Shared logic to jump to an outline BUTTON."
  (let ((page (button-get button 'emureader-page)))
    (unless (numberp page)
      (user-error "Invalid outline entry: no page info"))
    (emureader-outline-select-doc-window)
    ;; activates `window-buffer-change-functions' and `window-configuration-change-hook'
    (redisplay)
    (emureader-goto-page page)))

(defun emureader-outline-visit-page ()
  "Jump to the page at point in the associated emureader buffer."
  (interactive)
  (save-excursion
    (end-of-line)   ; This is done to make sure the we always have a
    (backward-char) ; button at point.
    (emureader-outline-goto-entry (button-at (point)))))

(provide 'emureader-outline)
;;; emureader-outline.el ends here

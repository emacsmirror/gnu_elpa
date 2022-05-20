;;; ffsanim.el --- Form Feed Slides animate -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Amin Bandali <bandali@gnu.org>

;; Author: Amin Bandali <bandali@gnu.org>
;; Version: 0.1.5
;; Keywords: outlines, tools

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

;; A simple mode for doing simple plain text presentations where the
;; slides are separated using the form feed character ().  Uses
;; animate.el to animate each slide.

;; Configuration: TODO

;; Usage:

;; (add-to-list 'load-path (b/lisp "ffs"))
;; (run-with-idle-timer 0.5 nil #'require 'ffsanim)
;; (with-eval-after-load 'ffsanim
;;   (defvar b/original-default-height)
;;   (defvar b/ffsanim-default-height 300)
;;   (global-set-key
;;    (kbd "C-c f s")
;;    (lambda ()
;;      (interactive)
;;      (setq
;;       b/original-default-height (face-attribute 'default :height))
;;      (set-face-attribute
;;       'default nil :height b/ffsanim-default-height)
;;      (message " ")
;;      (ffsanim)))
;;   (define-key
;;    ffsanim-mode-map (kbd "q")
;;    (lambda ()
;;      (interactive)
;;      (quit-window)
;;      (set-face-attribute
;;       'default nil :height b/original-default-height)
;;      (message " "))))

;;; Code:

(require 'animate)

(defgroup ffsanim nil
  "Major mode for form feed-separated plain text presentations."
  :version "29.1"
  :prefix "ffsanim-")

(defcustom ffsanim-buffer-name "*ffsanim*"
  "The name of the ffsanim presentation buffer."
  :group 'ffsanim
  :type 'string)

(defcustom ffsanim-edit-buffer-name "*ffsanim-edit*"
  "The name of the ffsanim-edit buffer used when editing a slide."
  :group 'ffsanim
  :type 'string)

(defvar ffsanim--source-buffer-name ""
  "The name of the form feed-separated \"source\" buffer for a
presentation.")

(defun ffsanim--buffer ()
  "Get the ffsanim presentation buffer."
  (get-buffer-create ffsanim-buffer-name))

(defmacro ffsanim-define-move-to-slide (name &optional doc &rest body)
  "Define a function for moving to a slide.
Symbol NAME is the name describing the movement.
DOC is the documentation string to use for the function."
  (declare (debug (&define name [&optional stringp] def-body))
           (doc-string 2) (indent defun))
  (when (and doc (not (stringp doc)))
    ;; `doc' is the first element of `body', not an actual docstring
    (push doc body)
    (setq doc nil))
  (let* ((sn (symbol-name name))
         (fname (intern (format "ffsanim-%s-slide" (downcase sn)))))
    `(defun ,fname ()
       ,doc
       (interactive)
       (let ((s (progn
                  (pop-to-buffer-same-window
                   (get-buffer ffsanim--source-buffer-name))
                  ,@body
                  (narrow-to-page)
                  (prog1 (buffer-string)
                    (widen)
                    (pop-to-buffer-same-window (ffsanim--buffer)))))
             (animation-buffer-name (buffer-name (ffsanim--buffer)))
             (inhibit-read-only t))
         (animate-sequence (split-string s "\n") 0)))))

(defun ffsanim-edit-slide (&optional add-before-or-after)
  "Pop to a new buffer to edit a slide.
If ADD-BEFORE-OR-AFTER is nil or not given, we are editing an
existing slide.  Otherwise, if it is `add-before' then the new
slide will be added before the current slide, and if it is
`add-after' then the new slide will be added after the current
slide.  The logic for handling this is in `ffsanim-edit-done'."
  (interactive)
  (let* (m
         (s (with-current-buffer (get-buffer ffsanim--source-buffer-name)
              (setq m major-mode)
              (if add-before-or-after   ; if we are adding a new slide
                  "\n"                  ; start with just a newline
                (narrow-to-page)
                (prog1 (buffer-string)
                  (widen))))))
    (pop-to-buffer-same-window
     (get-buffer-create ffsanim-edit-buffer-name))
    (funcall m)
    (ffsanim-edit-mode 1)
    (insert s)
    (goto-char (point-min))
    (setq-local ffsanim--new-location add-before-or-after)
    (message
     (substitute-command-keys "Edit, then use `\\[ffsanim-edit-done]' \
to apply your changes or `\\[ffsanim-edit-discard]' to discard them."))))

(defun ffsanim-edit-discard ()
  "Discard current ffsanim-edit buffer and return to the presentation."
  (interactive)
  (let ((buf (current-buffer)))
    (quit-windows-on buf)
    (kill-buffer buf))
  (pop-to-buffer-same-window (ffsanim--buffer)))

(defun ffsanim-edit-done ()
  "Apply the ffsanim-edit changes and return to the presentation."
  (interactive)
  (let* (f
         (str (buffer-string))
         (s (if (string-suffix-p "\n" str)
                str
              (concat str "\n")))
         (l ffsanim--new-location))
    (with-current-buffer (get-buffer ffsanim--source-buffer-name)
      (save-excursion
        (cond
         ((eq l 'add-before)
          (backward-page)
          (insert (format "\n%s" s))
          (setq f #'ffsanim-previous-slide))
         ((eq l 'add-after)
          (forward-page)
          (insert (format "\n%s" s))
          (setq f #'ffsanim-next-slide))
         ((null l)
          (narrow-to-page)
          (delete-region (point-min) (point-max))
          (insert s)
          (widen)
          (setq f #'ffsanim-current-slide)))))
    (ffsanim-edit-discard)
    (funcall f)))

(defun ffsanim-new-slide-before ()
  "Add a new slide before the current slide."
  (interactive)
  (ffsanim-edit-slide 'add-before))

(defun ffsanim-new-slide-after ()
  "Add a new slide after the current slide."
  (interactive)
  (ffsanim-edit-slide 'add-after))

(defvar ffsanim--old-mode-line-format nil
  "The value of `mode-line-format' in the ffsanim presentation buffer
before the last call to `ffsanim--toggle-mode-line'.")

(defun ffsanim--toggle-mode-line ()
  "Toggle the display of the mode-line in the current buffer."
  (interactive)
  (if mode-line-format
      (setq-local ffsanim--old-mode-line-format mode-line-format
                  mode-line-format nil)
    (setq-local mode-line-format ffsanim--old-mode-line-format
                ffsanim--old-mode-line-format nil))
  (redraw-display))

(ffsanim-define-move-to-slide previous
  "Go to the previous slide."
  (backward-page)
  (backward-page))

(ffsanim-define-move-to-slide next
  "Go to the next slide."
  (forward-page))

(ffsanim-define-move-to-slide current
  "Reload and renimate the current slide."
  nil)

(ffsanim-define-move-to-slide first
  "Go to the first slide."
  (goto-char (point-min)))

(ffsanim-define-move-to-slide last
  "Go to the last slide."
  (goto-char (point-max)))

(define-derived-mode ffsanim-mode special-mode "ffsanim"
  "Major mode for form feed-separated plain text presentations."
  :group 'ffsanim
  :interative nil
  (setq-local animate-total-added-delay 0.3)
  (show-paren-local-mode -1)
  (display-battery-mode -1)
  (ffsanim--toggle-mode-line)
  (ffsanim-current-slide))

(defvar ffsanim-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-k") #'ffsanim-edit-discard)
    (define-key map (kbd "C-c C-c") #'ffsanim-edit-done)
    map)
  "Keymap for `ffsanim-edit-mode'.")

(define-minor-mode ffsanim-edit-mode
  "Minor mode for editing a single ffsanim slide.
When done editing the slide, run \\[ffsanim-edit-done] to apply your
changes, or \\[ffsanim-edit-discard] to discard them."
  :group 'ffsanim
  :lighter " ffsanim-edit"
  :keymap ffsanim-edit-mode-map
  (defvar-local ffsanim--new-location nil
    "The location where the new slide should be inserted.
See the docstring for `ffsanim-edit-slide' for more details."))

(define-key ffsanim-mode-map (kbd "p") #'ffsanim-previous-slide)
(define-key ffsanim-mode-map (kbd "n") #'ffsanim-next-slide)
(define-key ffsanim-mode-map (kbd "DEL") #'ffsanim-previous-slide)
(define-key ffsanim-mode-map (kbd "SPC") #'ffsanim-next-slide)
(define-key ffsanim-mode-map (kbd "g") #'ffsanim-current-slide)
(define-key ffsanim-mode-map (kbd "<") #'ffsanim-first-slide)
(define-key ffsanim-mode-map (kbd ">") #'ffsanim-last-slide)
(define-key ffsanim-mode-map (kbd "e") #'ffsanim-edit-slide)
(define-key ffsanim-mode-map (kbd "O") #'ffsanim-new-slide-before)
(define-key ffsanim-mode-map (kbd "o") #'ffsanim-new-slide-after)
(define-key ffsanim-mode-map (kbd "m") #'ffsanim--toggle-mode-line)

(defun ffsanim ()
  "Start an ffsanim presentation with current buffer as source."
  (interactive)
  (setq ffsanim--source-buffer-name (buffer-name))
  (pop-to-buffer-same-window (ffsanim--buffer))
  (ffsanim-mode))

(provide 'ffsanim)
;;; ffsanim.el ends here

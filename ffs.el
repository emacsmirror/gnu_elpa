;;; ffs.el --- Form Feed Slides mode       -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Amin Bandali <bandali@gnu.org>

;; Author: Amin Bandali <bandali@gnu.org>
;; Version: 0.1.0
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
;; slides are separated using the form feed character ().

;; Configuration: TODO

;; Usage:

;; Put this file, ffs.el, in a directory in your `load-path', then add
;; something like the following to your init file:
;;
;; (require 'ffs)
;; (global-set-key (kbd "C-c f s") #'ffs)

;; Then, open a text file/buffer that you would like you to use as the
;; source of your presentation and type `M-x ffs RET' or a keyboard
;; shortcut you defined (like the above example) to start ffs, at
;; which point you should be able to see "ffs" appear as one of the
;; currently enabled minor modes in your mode-line.  Once ffs is
;; enabled, you can invoke its various commands.  To see a list of
;; available commands, you can either type `M-x ffs- TAB' (to get a
;; completion of commands starting with the "ffs-" prefix), or see the
;; definition of `ffs-minor-mode-map' near the end of this file.

;;; Code:

(defgroup ffs nil
  "Minor mode for form feed-separated plain text presentations."
  :version "29.1"
  :prefix "ffs-")

(defcustom ffs-default-face-height 370
  "The value of the `height' property for the `default' face to use
during the ffs presentation."
  :group 'ffs
  :type '(choice (const nil)
                 (integer :value 300)))

(defcustom ffs-edit-buffer-name "*ffs-edit*"
  "The name of the ffs-edit buffer used when editing a slide."
  :group 'ffs
  :type 'string)

(defvar ffs--slides-buffer nil
  "The main ffs presentation slides buffer.
When the user enables ffs in a buffer using `\\[ffs]', we store a
reference to that buffer in this variable.

As a special case, in a speaker notes buffer selected by the user
using `\\[ffs-find-speaker-notes-file]' from the main ffs slides
buffer, this variable will point to the main ffs slides buffer
rather than the speaker notes buffer.")

(defvar ffs--notes-buffer nil
  "The ffs speaker notes buffer (only if selected).
When the user chooses (and opens) a speaker notes file using
`\\[ffs-find-speaker-notes-file]', a reference to the file's
corresponding buffer is stored in this variable, local to the
main ffs presentation slides buffer (`ffs--slides-buffer').")

(defvar ffs--old-mode-line-format nil
  "The old value of `mode-line-format' before enabling
`ffs--no-mode-line-minor-mode'.")

(defvar ffs--old-cursor-type nil
  "The old value of `cursor-type' before enabling
`ffs--no-cursor-minor-mode'.")

(defvar ffs--old-default-face-height nil
  "The old value of the `default' face's `height' property before
starting the ffs presentation.")

(define-minor-mode ffs--no-mode-line-minor-mode
  "Minor mode for hiding the mode-line."
  :lighter nil
  (if ffs--no-mode-line-minor-mode
      (progn
        (unless ffs--old-mode-line-format
          (setq-local ffs--old-mode-line-format mode-line-format))
        (setq-local mode-line-format nil))
    (setq-local mode-line-format ffs--old-mode-line-format)
    (when ffs--old-mode-line-format
      ffs--old-mode-line-format nil))
  (redraw-display))

(define-minor-mode ffs--no-cursor-minor-mode
  "Minor mode for hiding the cursor."
  :lighter nil
  (if ffs--no-cursor-minor-mode
      (progn
        (unless ffs--old-cursor-type
          (setq-local ffs--old-cursor-type cursor-type))
        (setq-local cursor-type nil))
    (setq-local cursor-type ffs--old-cursor-type)
    (when ffs--old-cursor-type
      ffs--old-cursor-type nil)))

(defun ffs--toggle-dark-mode ()
  "Swap the frame background and foreground colours."
  (interactive)
  (let ((bg (frame-parameter nil 'background-color))
        (fg (frame-parameter nil 'foreground-color)))
    (set-background-color fg)
    (set-foreground-color bg)))

(defun ffs--goto-previous (buffer)
  "Go to the previous slide in the given BUFFER."
  (interactive)
  (with-current-buffer buffer
    (let ((n (buffer-narrowed-p)))
      (when n
        (goto-char (point-min))
        (widen)
        (backward-page))
      (backward-page)
      (when n (narrow-to-page)))))

(defun ffs-goto-previous ()
  "Go to the previous slide in the main ffs presentation and the
speaker notes buffer (if any)."
  (interactive)
  (ffs--goto-previous ffs--slides-buffer)
  (when ffs--notes-buffer
    (ffs--goto-previous ffs--notes-buffer)
    (redraw-display)))

(defun ffs--goto-next (buffer)
  "Go to the next slide in the given BUFFER."
  (interactive)
  (with-current-buffer buffer
    (let ((n (buffer-narrowed-p))
          (e (= (- (point-max) (point-min)) 0)))
      (when n
        (goto-char (point-min))
        (widen))
      (unless e (forward-page))
      (when n (narrow-to-page)))))

(defun ffs-goto-next ()
  "Go to the next slide in the main ffs presentation and the
speaker notes buffer (if any)."
  (interactive)
  (ffs--goto-next ffs--slides-buffer)
  (when ffs--notes-buffer
    (ffs--goto-next ffs--notes-buffer)
    (redraw-display)))

(defun ffs--goto-first (buffer)
  "Go to the first slide in the given BUFFER."
  (interactive)
  (with-current-buffer buffer
    (let ((n (buffer-narrowed-p)))
      (when n (widen))
      (goto-char (point-min))
      (when n (narrow-to-page)))))

(defun ffs-goto-first ()
  "Go to the first slide in the main ffs presentation and the
speaker notes buffer (if any)."
  (interactive)
  (ffs--goto-first ffs--slides-buffer)
  (when ffs--notes-buffer
    (ffs--goto-first ffs--notes-buffer)
    (redraw-display)))

(defun ffs--goto-last (buffer)
  "Go to the last slide in the given BUFFER."
  (interactive)
  (let ((n (buffer-narrowed-p)))
    (when n (widen))
    (goto-char (point-max))
    (when n (narrow-to-page))))

(defun ffs-goto-last ()
  "Go to the last slide in the main ffs presentation and the
speaker notes buffer (if any)."
  (interactive)
  (ffs--goto-last ffs--slides-buffer)
  (when ffs--notes-buffer
    (ffs--goto-last ffs--notes-buffer)
    (redraw-display)))

(defun ffs-start ()
  "Start the presentation."
  (interactive)
  (ffs-minor-mode 1)
  (ffs--no-mode-line-minor-mode 1)
  (ffs--no-cursor-minor-mode 1)
  (when (integerp ffs-default-face-height)
    (setq-local
     ffs--old-default-face-height
     (face-attribute 'default :height))
    (face-remap-add-relative
     'default :height ffs-default-face-height))
  (show-paren-local-mode -1)
  (display-battery-mode -1)
  (flyspell-mode -1)
  (narrow-to-page))

(defun ffs-quit ()
  "Quit the presentation."
  (interactive)
  (let ((n (buffer-narrowed-p))
        (e (= (- (point-max) (point-min)) 0)))
    (when (integerp ffs-default-face-height)
      (face-remap-add-relative
       'default :height ffs--old-default-face-height))
    (show-paren-local-mode 1)
    (display-battery-mode 1)
    (flyspell-mode 1)
    (ffs--no-mode-line-minor-mode -1)
    (ffs--no-cursor-minor-mode -1)
    (if n
        (progn
          (goto-char (point-min))
          (widen))
      (ffs-minor-mode -1))
    (when e (forward-char -1))))

(defun ffs-edit (&optional add-above-or-below)
  "Pop to a new buffer to edit a slide.
If ADD-ABOVE-OR-BELOW is nil or not given, we are editing an
existing slide.  Otherwise, if it is `add-above' then the new
slide will be added above/before the current slide, and if it is
`add-below' then the new slide will be added below/after the
current slide.  The logic is implemented in `ffs-edit-done'."
  (interactive)
  (let* ((b (current-buffer))
         (m major-mode)
         (n (buffer-narrowed-p))
         (s (if add-above-or-below      ; if we are adding a new slide
                "\n"                    ; start with just a newline
              (unless n (narrow-to-page))
              (prog1 (buffer-string)
                (unless n (widen))))))
    (pop-to-buffer-same-window
     (get-buffer-create ffs-edit-buffer-name))
    (funcall m)
    (ffs-edit-minor-mode 1)
    (insert s)
    (goto-char (point-min))
    (set-buffer-modified-p nil)
    (setq-local
     ffs--edit-source-buffer b
     ffs--new-location add-above-or-below)
    (message
     (substitute-command-keys "Edit, then use `\\[ffs-edit-done]' \
to apply your changes or `\\[ffs-edit-discard]' to discard them."))))

(defun ffs-new-above ()
  "Add a new slide above/before the current slide."
  (interactive)
  (ffs-edit 'add-above))

(defun ffs-new-below ()
  "Add a new slide below/after the current slide."
  (interactive)
  (ffs-edit 'add-below))

(defun ffs-edit-discard ()
  "Discard current ffs-edit buffer and return to the presentation."
  (interactive)
  (let ((b (current-buffer)))
    (quit-windows-on b)
    (kill-buffer b)))

(defun ffs-edit-done ()
  "Apply the ffs-edit changes and return to the presentation."
  (interactive)
  (let* (f
         (str (buffer-string))
         (s (if (string-suffix-p "\n" str)
                str
              (concat str "\n")))
         (l ffs--new-location))
    (with-current-buffer ffs--edit-source-buffer
      (let ((inhibit-read-only t))
        (save-excursion
          (cond
           ((eq l 'add-above)
            (backward-page)
            (insert (format "\n%s" s))
            (setq f #'ffs-previous-slide))
           ((eq l 'add-below)
            (forward-page)
            (insert (format "\n%s" s))
            (setq f #'ffs-next-slide))
           ((null l)
            (narrow-to-page)
            (delete-region (point-min) (point-max))
            (insert s)
            (widen))))))
    (ffs-edit-discard)
    (when (functionp f)
      (funcall f))))

(defun ffs--undo (&optional arg)
  "Like `undo', but it works even when the buffer is read-only."
  (interactive "P")
  (let ((inhibit-read-only t))
    (undo arg)))

(defun ffs-find-speaker-notes-file (file)
  "Prompt user for a speaker notes file, open it in a new frame."
  (interactive "Fspeakers notes buffer: ")
  (let ((b (current-buffer)))
    (save-excursion
      (find-file-other-frame file)
      (ffs-minor-mode 1)
      (setq-local
       ffs--slides-buffer b
       ffs--notes-buffer (current-buffer)))
    (setq-local ffs--notes-buffer (get-file-buffer file))))

(defun ffs-export-slides-to-pdf ()
  (interactive)
  (with-current-buffer ffs--slides-buffer
    (ffs-goto-first)
    (let ((c 1)
          (fringe fringe-mode))
      (fringe-mode 0)
      (while (not (eobp))
        (let ((fn (format "%s-%03d.pdf"
                          (file-name-sans-extension (buffer-name))
                          c))
              (data (x-export-frames nil 'pdf)))
          (with-temp-file fn
            (insert data)))
        (setq c (+ c 1))
        (ffs-goto-next))
      (fringe-mode fringe))))

(defvar ffs-edit-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-k") #'ffs-edit-discard)
    (define-key map (kbd "C-c C-c") #'ffs-edit-done)
    map)
  "Keymap for `ffs-edit-minor-mode'.")

(define-minor-mode ffs-edit-minor-mode
  "Minor mode for editing a single ffs slide.
When done editing the slide, run \\[ffs-edit-done] to apply your
changes, or \\[ffs-edit-discard] to discard them."
  :group 'ffs
  :lighter " ffs-edit"
  :keymap ffs-edit-minor-mode-map
  (defvar-local ffs--edit-source-buffer nil
    "The ffs presentation buffer of the slide being edited.")
  (defvar-local ffs--new-location nil
    "The location where the new slide should be inserted.
See the docstring for `ffs-edit' for more details."))

(defvar ffs-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "p") #'ffs-goto-previous)
    (define-key map (kbd "n") #'ffs-goto-next)
    (define-key map (kbd "DEL") #'ffs-goto-previous)
    (define-key map (kbd "SPC") #'ffs-goto-next)
    (define-key map (kbd "[") #'ffs-goto-previous)
    (define-key map (kbd "]") #'ffs-goto-next)
    (define-key map (kbd "<") #'ffs-goto-first)
    (define-key map (kbd ">") #'ffs-goto-last)
    (define-key map (kbd "s") #'ffs-start)
    (define-key map (kbd "q") #'ffs-quit)
    (define-key map (kbd "e") #'ffs-edit)
    (define-key map (kbd "O") #'ffs-new-above)
    (define-key map (kbd "o") #'ffs-new-below)
    (define-key map (kbd "m") #'ffs--no-mode-line-minor-mode)
    (define-key map (kbd "c") #'ffs--no-cursor-minor-mode)
    (define-key map (kbd "d") #'ffs--toggle-dark-mode)
    (define-key map (kbd "N") #'narrow-to-page)
    (define-key map (kbd "W") #'widen)
    (define-key map [remap undo] #'ffs--undo)
    (define-key map (kbd "C-c n") #'ffs-find-speaker-notes-file)
    map)
  "Keymap for `ffs-minor-mode'.")

(define-minor-mode ffs-minor-mode
  "Minor mode for form feed-separated plain text presentations."
  :group 'ffs
  :lighter " ffs"
  :keymap ffs-minor-mode-map
  (setq-local
   ffs--old-mode-line-format mode-line-format
   ffs--old-cursor-type cursor-type
   ffs--old-default-face-height
   (face-attribute 'default :height))
  (setq buffer-read-only ffs-minor-mode))

(defun ffs ()
  "Enable `ffs-minor-mode' for presenting the current buffer."
  (interactive)
  (ffs-minor-mode 1)
  (setq-local ffs--slides-buffer (current-buffer)))

(provide 'ffs)
;;; ffs.el ends here

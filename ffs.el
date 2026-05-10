;;; ffs.el --- Form Feed Slides       -*- lexical-binding: t; -*-

;; Copyright (c) 2022-2026 Amin Bandali <bandali@gnu.org>

;; Author: Amin Bandali <bandali@gnu.org>
;; Maintainer: Amin Bandali <bandali@gnu.org>
;; URL: https://git.kelar.org/~bandali/ffs
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: convenience, focus, narrowing, outlines, presentation, text

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
;; slides are separated using the `page-delimiter', by default the
;; form feed character (^L).

;; Configuration: TODO

;; Usage:

;; After installing `ffs', open a text file/buffer containing your
;; slides do `M-x ffs RET'.  Optionally, you can add a key binding for
;; `ffs' to your init file:
;;
;; (global-set-key (kbd "C-c f s") #'ffs)

;; You should then see "ffs" appear as one of the currently enabled
;; minor modes in your mode-line.  Once ffs is enabled, you can invoke
;; its various commands.  To see a list of available commands, you can
;; either type `M-x ffs- TAB' (to get a completion of commands
;; starting with the "ffs-" prefix), or see the definition of
;; `ffs-mode-map' near the end of this file.

;;; Code:

;; TODO:
;; add user option for echoing slide count
;; dedication in README/manual
;; acronym and backronyms

(defgroup ffs nil
  "Minor mode for form feed-separated plain text presentations."
  :group 'editing)

(defcustom ffs-default-face-height 370
  "Value of `height' property of `default' face during presentations.
If a natural number (non-negative integer), it will be used as the
`height' of the `default' face during presentations, useful for having
a larger font size when presenting.

If nil, don't change the `default' face's `height' in presentations."
  :group 'ffs
  :type '(choice (const nil)
                 (natnum :value 300)))

(defcustom ffs-start-hook nil
  "Hook run when starting presenting (at the end of `ffs-start')."
  :group 'ffs
  :type 'hook)

(defcustom ffs-quit-hook nil
  "Hook run when quitting presenting (at the end of `ffs-quit')."
  :group 'ffs
  :type 'hook)

(defcustom ffs-edit-hook nil
  "Hook run when editing a slide (at the end of `ffs-edit')."
  :group 'ffs
  :type 'hook)

(defcustom ffs-edit-done-hook nil
  "Hook run after editing a slide (at the end of `ffs-edit-done')."
  :group 'ffs
  :type 'hook)

(defcustom ffs-page-delimiter ""
  "The default page delimiter for ffs, the form feed character.
If you use a custom `page-delimiter' regexp, be sure to customize this
as well."
  :group 'ffs
  :type 'string)

(defvar ffs-edit-buffer-name "*ffs-edit*"
  "The name of the ffs-edit buffer used when editing a slide.")

(defvar ffs-find-speaker-notes-function #'find-file-other-frame
  "The function to use when finding a speaker's note file.")

(defvar-local ffs--edit-source-buffer nil
  "The ffs presentation buffer of the slide being edited.")

(defvar-local ffs--new-location nil
  "The location where the new slide should be inserted.
See the docstring for `ffs-edit' for more details.")

(defvar-local ffs--slides-buffer nil
  "The main ffs presentation slides buffer.
When the user enables ffs in a buffer using `\\[ffs]', we store a
reference to that buffer in this variable.

As a special case, in a speaker notes buffer selected by the user
using `\\[ffs-find-speaker-notes-file]' from the main ffs slides
buffer, this variable will point to the main ffs slides buffer
rather than the speaker notes buffer.")

(defvar-local ffs--notes-buffer nil
  "The ffs speaker notes buffer (only if selected).
When the user chooses (and opens) a speaker notes file using
`\\[ffs-find-speaker-notes-file]', a reference to the file's
corresponding buffer is stored in this variable, local to the
main ffs presentation slides buffer (`ffs--slides-buffer').")

(defvar-local ffs--old-mode-line-format nil
  "Old value of `mode-line-format'.")

(defvar-local ffs--old-cursor-type nil
  "Old value of `cursor-type'.")

(defvar-local ffs--default-face-height-cookie nil
  "Cookie returned from `face-remap-add-relative', for later removal.")

(define-minor-mode ffs-no-mode-line-minor-mode
  "Minor mode for hiding the mode-line."
  :lighter nil
  (if ffs-no-mode-line-minor-mode
      (progn
        (unless ffs--old-mode-line-format
          (setq-local ffs--old-mode-line-format mode-line-format))
        (setq-local mode-line-format nil))
    (setq-local mode-line-format ffs--old-mode-line-format)
    (when ffs--old-mode-line-format
      ffs--old-mode-line-format nil))
  (redraw-display))

(define-minor-mode ffs-no-cursor-minor-mode
  "Minor mode for hiding the cursor."
  :lighter nil
  (if ffs-no-cursor-minor-mode
      (progn
        (unless ffs--old-cursor-type
          (setq-local ffs--old-cursor-type cursor-type))
        (setq-local cursor-type nil))
    (setq-local cursor-type ffs--old-cursor-type)
    (when ffs--old-cursor-type
      ffs--old-cursor-type nil)))

;; XXX user option for more sophisticated behaviour?
;; set-face-attribute  for specific frames
;; face-list  reset to defaults
(defun ffs-toggle-dark-mode ()
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

;; XXX macro to abstract this pattern?
(defun ffs-goto-previous ()
  "Go to previous slide in ffs presentation and speaker notes buffer."
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
  "Go to next slide in ffs presentation and speaker notes buffer."
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
  "Go to first slide in ffs presentation and speaker notes buffer."
  (interactive)
  (ffs--goto-first ffs--slides-buffer)
  (when ffs--notes-buffer
    (ffs--goto-first ffs--notes-buffer)
    (redraw-display)))

(defun ffs--goto-last (buffer)
  "Go to the last slide in the given BUFFER."
  (interactive)
  (with-current-buffer buffer
    (let ((n (buffer-narrowed-p)))
      (when n (widen))
      (goto-char (point-max))
      (when n (narrow-to-page)))))

(defun ffs-goto-last ()
  "Go to last slide in ffs presentation and speaker notes buffer."
  (interactive)
  (ffs--goto-last ffs--slides-buffer)
  (when ffs--notes-buffer
    (ffs--goto-last ffs--notes-buffer)
    (redraw-display)))

(defun ffs-start ()
  "Start the presentation."
  (interactive)
  (unless (and ffs-mode (buffer-narrowed-p))
    (ffs-mode 1)
    (when (natnump ffs-default-face-height)
      (setq-local
       ffs--default-face-height-cookie
       (face-remap-add-relative
        'default :height ffs-default-face-height)))
    (narrow-to-page)
    (run-hooks 'ffs-start-hook)))

(declare-function face-remap-remove-relative "face-remap" (cookie))
(defun ffs-quit ()
  "Quit the presentation.
If not currently presenting, quit (disable) `ffs-mode'."
  (interactive)
  (when ffs-mode
    (when ffs--default-face-height-cookie
      (face-remap-remove-relative ffs--default-face-height-cookie))
    (if (buffer-narrowed-p) ; currently presenting
        (progn
          (goto-char (point-min))
          (widen))
      (ffs-mode -1)))
    (run-hooks 'ffs-quit-hook))

(defun ffs-edit (&optional add-above-or-below)
  "Pop to a new buffer to edit a slide.
If ADD-ABOVE-OR-BELOW is nil or not given, we are editing an
existing slide.  Otherwise, if it is `add-above' then the new
slide will be added above/before the current slide, and if it is
`add-below' then the new slide will be added below/after the
current slide.  The logic is implemented in `ffs-edit-done'."
  (interactive)
  (let* ((b (current-buffer))
         (fc fill-column)
         (m major-mode)
         (n (buffer-narrowed-p))
         (s (if add-above-or-below      ; if we are adding a new slide
                "\n"                    ; start with just a newline
              (unless n (narrow-to-page))
              (prog1 (buffer-string)
                (unless n (widen))))))
    ;; XXX (with-current-buffer buffer ...)
    ;; (display-buffer buffer)
    ;;   ^ also accepts an alist that we can make a user option
    (pop-to-buffer-same-window
     (generate-new-buffer ffs-edit-buffer-name))
    (erase-buffer)
    (funcall m)
    (ffs-edit-mode 1)
    (insert s)
    (goto-char (point-min))
    (set-buffer-modified-p nil)
    (setq-local
     ffs--edit-source-buffer b
     ffs--new-location add-above-or-below
     fill-column fc
     header-line-format
     (substitute-command-keys "Edit, then use `\\[ffs-edit-done]' \
to apply your changes or `\\[ffs-edit-discard]' to discard them.")))
  (run-hooks 'ffs-edit-hook))

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
  (let* ((f nil)
         (str (buffer-string))
         (sn (if (string-suffix-p "\n" str)
                 str
               (concat str "\n")))
         (s (format "\n%s%s" sn ffs-page-delimiter))
         (l ffs--new-location))
    ;; XXX maybe break out into helper, would be helpful when testing
    (with-current-buffer ffs--edit-source-buffer
      (let ((inhibit-read-only t))
        (save-excursion
          (cond
           ((eq l 'add-above)
            (backward-page)
            (insert s)
            (setq f #'ffs-goto-previous))
           ((eq l 'add-below)
            (forward-page)
            (insert s)
            (setq f #'ffs-goto-next))
           ((null l)
            (narrow-to-page)
            (delete-region (point-min) (point-max))
            (insert s)
            (widen))))))
    (ffs-edit-discard)
    (when (functionp f)
      (funcall f)))
  (run-hooks 'ffs-edit-done-hook))

(defun ffs-undo (&optional arg)
  "Like `undo', but it works even when the buffer is read-only.
Repeat this command to undo more changes.
A numeric ARG serves as a repeat count."
  (interactive "P")
  ;; XXX could rebinding `amalgamating-undo-limit' be useful?
  (let ((inhibit-read-only t))
    (undo arg)))

(defun ffs-find-speaker-notes-file (file)
  "Prompt user for a speaker notes FILE, open it in a new frame."
  (interactive "Fspeakers notes buffer: ")
  (let ((b (current-buffer)))
    (save-excursion
      (funcall ffs-find-speaker-notes-function file)
      (ffs-mode 1)
      (setq-local
       ffs--slides-buffer b
       ffs--notes-buffer (current-buffer)))
    (setq-local ffs--notes-buffer (get-file-buffer file))))

;; XXX write all slides to one file?
(defun ffs-export-slides-to-pdf ()
  "Export slides to PDF (needs Emacs built with Cairo)."
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

(defvar ffs-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-k") #'ffs-edit-discard)
    (define-key map (kbd "C-c C-c") #'ffs-edit-done)
    map)
  "Keymap for `ffs-edit-mode'.")

(define-minor-mode ffs-edit-mode
  "Minor mode for editing a single ffs slide.
When done editing the slide, run \\[ffs-edit-done] to apply your
changes, or \\[ffs-edit-discard] to discard them."
  :group 'ffs
  :lighter " ffs-edit"
  :keymap ffs-edit-mode-map)

(defvar ffs-mode-map
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
    (define-key map (kbd "m") #'ffs-no-mode-line-minor-mode)
    (define-key map (kbd "c") #'ffs-no-cursor-minor-mode)
    (define-key map (kbd "d") #'ffs-toggle-dark-mode)
    (define-key map (kbd "S") #'ffs-find-speaker-notes-file)
    (define-key map (kbd "N") #'narrow-to-page)
    (define-key map (kbd "W") #'widen)
    (define-key map [remap undo] #'ffs-undo) ; `C-_', `C-/', `C-x u'
    map)
  "Keymap for `ffs-mode'.")

;;;###autoload
(define-minor-mode ffs-mode
  "Minor mode for form feed-separated plain text presentations."
  :group 'ffs
  :lighter " ffs"
  :keymap ffs-mode-map
  (setq-local
   buffer-read-only ffs-mode
   ffs--slides-buffer (current-buffer)
   ffs--old-mode-line-format mode-line-format
   ffs--old-cursor-type cursor-type))

(defalias 'ffs #'ffs-mode)

(provide 'ffs)
;;; ffs.el ends here

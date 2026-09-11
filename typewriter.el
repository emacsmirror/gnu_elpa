;;; typewriter.el --- Turn Emacs into a text adder  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Enrico Flor <enrico@eflor.net>
;; Maintainer: Enrico Flor <enrico@eflor.net>
;; URL: https://github.com/enricoflor/typewriter.el
;; Version: 1.1.0
;; Keywords: wp

;; Package-Requires: ((emacs "30.1"))

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see
;; <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides typewriter-mode, a small minor mode that
;; deliberately handicaps Emacs to an extreme degree in order to
;; provide something as close as possible to the strict forward-only
;; typewriter experience.  Some find that the lack of editing
;; facilities fosters a state of concentration and focus that makes
;; certain types of creative writing more satisfying.
;;
;; The package has several configuration options (M-x customize-group
;; RET typewriter).
;;
;; Although no systematic test has been carried out, this package's
;; minimality should ensure its compatibility with packages that
;; change the layout of text in the window, such as the fairly popular
;; olivetti, or any configuration that (for instance) hides or alters
;; element of the emacs interface.

;;; Code:

(defgroup typewriter nil
  "Configuration options for `typewriter-mode'."
  :prefix "typewriter-"
  :link '(url-link :tag "Website for typewriter-mode"
                   "https://github.com/enricoflor/typewriter.el")
  :group 'wp)

(defcustom typewriter-preserve-undo-history t
  "If non-nil, keep tracking undo history while in `typewriter-mode'.

You still cannot undo while the mode is active (buffer will be in
`read-only-mode'), but the history will be fully available after you
exit the mode."
  :type 'boolean)

(defcustom typewriter-recenter t
  "If non-nil, keep the line where cursor is centered in the window."
  :type 'boolean)

(defcustom typewriter-fill-column nil
  "The margin limit in `typewriter-mode'.

If set to an integer, the typewriter will lock up and \\='ding\\=' when
you reach this column. You must press RET to continue.  If nil, no
margin is enforced.  While `typewriter-mode' is active, the buffer-local
value of `fill-column' is identical to the value of this variable."
  :type '(choice (const :tag "No margin" nil)
                 (natnum :tag "Margin at column")))

(defcustom typewriter-warning-bell-offset 8
  "Number of columns before the margin to sound the warning bell."
  :type '(choice (const :tag "No warning bell" nil)
                 (natnum :tag "Warning bell offset at")))

(defcustom typewriter-show-chars-remaining t
  "If non-nil, show columns remaining before the margin in the mode line.

Has no effect unless `typewriter-fill-column' is also set to an integer,
since without a margin there is nothing to count down to."
  :type 'boolean)

(defcustom typewriter-mode-line-format " [%d]"
  "Format string for the mode line character counter.

This is passed directly to `format'.  The `%d' construct will be
replaced by the number of remaining characters.  Include a leading space
to visually separate the counter from preceding items in the mode line."
  :type 'string)

(defcustom typewriter-tab-width 8
  "The number of columns a tab key advances the carriage.

If 0, `typewriter-tab' is disabled."
  :type 'natnum)

(defcustom typewriter-keystroke-hook nil
  "Hook run after successfully striking a key (inserting a character)."
  :type 'hook)

(defcustom typewriter-carriage-return-hook nil
  "Hook run after inserting a new line."
  :type 'hook)

(defun typewriter--mode-line-remaining ()
  "Return a mode line string with columns left before the margin.

Returns the empty string outside `typewriter-mode', or when
`typewriter-show-chars-remaining' or `typewriter-fill-column' is nil."
  (declare (side-effect-free t))
  (if (and typewriter-show-chars-remaining
           typewriter-fill-column)
      (let ((curr (save-excursion
                    (end-of-line)
                    (current-column))))
        (format typewriter-mode-line-format
                (max 0 (- typewriter-fill-column curr))))
    ""))

(defun typewriter-backward-char ()
  "Move the carriage left without deleting, allowing overstrikes."
  (interactive)
  (let ((active-line-start (save-excursion
                             (goto-char (point-max))
                             (line-beginning-position))))
    (if (> (point) active-line-start)
        (backward-char 1)
      ;; carriage can't go back further than the left margin!
      (ding)
      (message "Carriage is at the left margin!"))))

(defun typewriter-tab ()
  "Glide the carriage to the next tab stop without erasing existing ink.

If `typewriter-tab-width' is not positive, does nothing except message
the user."
  (interactive)
  (if (> typewriter-tab-width 0)
      (let* ((col (current-column))
             ;; calculate the next multiple of the tab width
             (next-stop (* (/ (+ col typewriter-tab-width) typewriter-tab-width)
                           typewriter-tab-width))
             (inhibit-read-only t))
        ;; move-to-column with t automatically pads spaces only if
        ;; needed
        (move-to-column next-stop t))
    (message "TAB is disabled (typewriter-tab-width is not positive)")))

(defun typewriter-self-insert ()
  "Typewriter replacement for `self-insert-command'.

The buffer is normally kept read-only for the whole time
`typewriter-mode' is on, and `typewriter--pre-command' has already
decided, before this command runs, whether the pending insertion is
allowed.  So by the time control reaches here, insertion is always meant
to succeed, and the only job left is to make the buffer briefly writable
for it."
  (interactive)
  (let ((inhibit-read-only t))
    (call-interactively #'self-insert-command))
  (when typewriter-recenter (funcall #'recenter)))

(defun typewriter-newline ()
  "Typewriter replacement for `newline'.

The buffer is normally kept read-only for the whole time
`typewriter-mode' is on, and `typewriter--pre-command' has already
decided, before this command runs, whether the pending insertion is
allowed.  So by the time control reaches here, insertion is always meant
to succeed, and the only job left is to make the buffer briefly writable
for it."
  (interactive)
  (let ((inhibit-read-only t))
    (call-interactively #'newline))
  (when typewriter-recenter (funcall #'recenter)))

(defun typewriter--bell-ring (&optional maybe)
  "Ring the typewriter bell, or ring it only near the margin.

If MAYBE is nil, ring unconditionally.  If MAYBE is non-nil, ring only
when point is `typewriter-warning-bell-offset' columns short of
`typewriter-fill-column' and the current command is not
`typewriter-newline'."
  (when (or (not maybe)
            (and typewriter-fill-column
                 typewriter-warning-bell-offset
                 (= (current-column)
                    (- typewriter-fill-column
                       typewriter-warning-bell-offset))
                 (not (eq this-command 'typewriter-newline))))
    (ding)))

(defun typewriter--pre-command ()
  "Enforce margins and ink permanence for the pending keystroke."
  (when (memq this-command '(typewriter-self-insert
                             typewriter-newline
                             typewriter-tab
                             typewriter-backward-char))

    ;; Only typewriter-newline forces point to the true end of the
    ;; buffer; self-insert and tab must evaluate overstrike/margin
    ;; logic at wherever point actually is, since a user may have
    ;; navigated to an earlier position on the last line, or even an
    ;; earlier line, to overstrike there.
    (when (eq this-command 'typewriter-newline)
      (if (save-excursion
            (end-of-line)
            (skip-chars-forward " \t\n")
            (eobp))
          (goto-char (point-max))
        (forward-line 1)
        (setq this-command 'ignore)))

    (unless (eq this-command 'ignore)
      (let ((col (current-column)))
        (cond
         ((and (eq this-command 'typewriter-self-insert)
               (not (eobp))
               (not (eolp))
               (not (looking-at-p "\t\\|\s")))
          ;; trying to type over existing ink
          (typewriter--bell-ring)
          (message
           (substitute-command-keys
            "You can only overstrike blank spaces.  \\[typewriter-mode] to toggle off and edit"))
          (setq this-command 'ignore))

         ((and typewriter-fill-column
               (not (eq this-command 'typewriter-newline))
               (>= col typewriter-fill-column))
          ;; we're at the margin
          (typewriter--bell-ring t)
          (typewriter--bell-ring)
          (message
           (substitute-command-keys
            "Margin reached!  Press \\[typewriter-newline] to return the carriage."))
          (setq this-command 'ignore))

         (t
          ;; just type
          (typewriter--bell-ring t)
          ;; This lift of inhibit-read-only is unrelated to the one in
          ;; typewriter-self-insert: that one wraps the insertion
          ;; command itself, while this one only covers deleting the
          ;; placeholder space/tab that the new character is about to
          ;; overstrike.
          (when (and (eq this-command 'typewriter-self-insert)
                     (looking-at-p "\t\\|\s"))
            (let ((inhibit-read-only t))
              (delete-char 1)))))))))

(defun typewriter--post-command ()
  "Run configured hooks after a keystroke or carriage return.

Also, refresh the mode line if `typewriter-show-chars-remaining' is
non-nil."
  (cond ((eq this-command 'typewriter-self-insert)
         (run-hooks 'typewriter-keystroke-hook))
        ((eq this-command 'typewriter-newline)
         (run-hooks 'typewriter-carriage-return-hook)))
  (when (and typewriter-fill-column
             typewriter-show-chars-remaining)
    (force-mode-line-update)))

(defun typewriter--error-handler (data context caller)
  "Handle read-only errors with a custom unlogged message."
  (if (eq (car data) 'buffer-read-only)
      (message
       (substitute-command-keys
        "You're in typewriter mode.  \\[typewriter-mode] to toggle off and edit"))
    (command-error-default-function data context caller)))

(defvar-keymap typewriter-mode-map
  :doc "Keymap for `typewriter-mode'."
  "RET" #'typewriter-newline
  "TAB" #'typewriter-tab
  "DEL" #'typewriter-backward-char
  "<remap> <self-insert-command>" #'typewriter-self-insert)

(defconst typewriter--overridden-variables '(buffer-read-only
                                             indent-line-function
                                             tab-width
                                             tab-stop-list
                                             electric-indent-mode
                                             command-error-function
                                             fill-column)
  "Buffer-local variables that `typewriter-mode' temporarily overrides.

Used to save their pre-mode values into `typewriter--saved-state' on
enable, and restore them on disable.")

(defvar-local typewriter--saved-state nil
  "Alist of (VARIABLE . VALUE) saved before `typewriter-mode' was enabled.

Populated from `typewriter--overridden-variables'.")

(define-minor-mode typewriter-mode
  "A minor mode emulating a strict typewriter.

Only allows appending text to the end of the buffer or on whitespace.
No deletions or arbitrary edits."
  :init-value nil
  :lighter " Typewriter"
  :keymap typewriter-mode-map

  (if typewriter-mode

      (progn
        (unless typewriter-preserve-undo-history
          (setq-local buffer-undo-list t))
        ;; Save original variable states before overriding
        (setq typewriter--saved-state
              (mapcar (lambda (sym) (cons sym (symbol-value sym)))
                      typewriter--overridden-variables))
        (setq-local buffer-read-only t
                    indent-line-function #'tab-to-tab-stop
                    ;; tab-width still needs a 1 floor because it's
                    ;; (probably) read by many other things that don't
                    ;; assume a 0 value (tolerated by this minor mode
                    ;; as a value for typewriter-tab-width)
                    tab-width (max 1 typewriter-tab-width)
                    tab-stop-list nil
                    electric-indent-mode nil
                    command-error-function #'typewriter--error-handler
                    fill-column typewriter-fill-column)
        (add-hook 'pre-command-hook #'typewriter--pre-command nil t)
        (add-hook 'post-command-hook #'typewriter--post-command nil t)
        (when (<= typewriter-tab-width 0)
          (message "typewriter-tab-width is %S; TAB key will be disabled"
                   typewriter-tab-width)
          (sit-for 2)))

    ;; Only restore undo tracking if WE were the ones who disabled it
    (when (eq buffer-undo-list t)
      (kill-local-variable 'buffer-undo-list))
    (when typewriter--saved-state
      (dolist (entry typewriter--saved-state)
        (set (make-local-variable (car entry)) (cdr entry)))
      (setq typewriter--saved-state nil))
    (remove-hook 'pre-command-hook #'typewriter--pre-command t)
    (remove-hook 'post-command-hook #'typewriter--post-command t)))

(add-to-list 'mode-line-misc-info
             '(typewriter-mode ("" (:eval (typewriter--mode-line-remaining))))
             t)

(provide 'typewriter)

;;; typewriter.el ends here

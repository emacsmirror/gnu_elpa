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
;; elements of the Emacs interface.

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
you reach this column.  You must press RET to continue.  If nil, no
margin is enforced and `fill-column' is left alone; otherwise, while
`typewriter-mode' is active, the buffer-local value of `fill-column' is
set to the value of this variable."
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

(define-obsolete-variable-alias 'typewriter-keystroke-hook
  'typewriter-insert-hook "1.1.1")

(defcustom typewriter-insert-hook nil
  "Hook run after successfully inserting a character."
  :type 'hook)

(defcustom typewriter-carriage-return-hook nil
  "Hook run after inserting a new line."
  :type 'hook)

(defcustom typewriter-backward-char-hook nil
  "Hook run after moving backward."
  :type 'hook)

(defcustom typewriter-tab-hook nil
  "Hook run after successfully inserting a tab."
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
        (progn (backward-char 1)
               (run-hooks 'typewriter-backward-char-hook))
      ;; carriage can't go back further than the left margin!
      (ding)
      (message "Carriage is at the left margin!"))))

(defun typewriter-tab ()
  "Glide the carriage to the next tab stop without erasing existing ink.

The carriage never glides past `typewriter-fill-column': if the next tab
stop is beyond it, the carriage stops at the margin.  If
`typewriter-tab-width' is not positive, does nothing except message the
user."
  (interactive)
  (if (> typewriter-tab-width 0)
      (let* ((col (current-column))
             ;; calculate the next multiple of the tab width
             (next-stop (* (/ (+ col typewriter-tab-width) typewriter-tab-width)
                           typewriter-tab-width))
             (inhibit-read-only t))
        (when typewriter-fill-column
          (setq next-stop (min next-stop typewriter-fill-column)))
        ;; move-to-column with t automatically pads spaces only if
        ;; needed
        (move-to-column next-stop t)
        (run-hooks 'typewriter-tab-hook))
    (message "TAB is disabled (typewriter-tab-width is not positive)")))

;; Some modes put functions on `post-self-insert-hook' that edit the
;; buffer behind the typewriter's back: `electric-pair-mode' inserts a
;; closing delimiter after point, which would then count as ink and
;; block the carriage.  So the hook is disabled for every strike.

(defun typewriter--maybe-recenter ()
  "Recenter the selected window if `typewriter-recenter' is non-nil.

Do nothing if the selected window doesn't show the current buffer, in
which case `recenter' would signal an error."
  (when (and typewriter-recenter
             (eq (window-buffer) (current-buffer)))
    (recenter)))

(defun typewriter--strike (count insert &optional first-prepared)
  "Strike a character COUNT times by calling INSERT with no arguments.

Before each strike, `typewriter--prepare-strike' decides whether it is
allowed, and striking stops at the first refusal (for example, at the
margin).  If FIRST-PREPARED is non-nil, the first strike has already
been prepared by `typewriter--pre-command' and is not checked again.
`typewriter-insert-hook' runs after each strike."
  (catch 'refused
    (dotimes (i count)
      (unless (or (and first-prepared (= i 0))
                  (typewriter--prepare-strike 'typewriter-self-insert))
        (throw 'refused nil))
      (let ((inhibit-read-only t)
            (post-self-insert-hook nil))
        (funcall insert))
      (run-hooks 'typewriter-insert-hook)))
  (typewriter--maybe-recenter))

(defun typewriter-self-insert (n)
  "Typewriter replacement for `self-insert-command'.

Strike the typed character N times (N below 1 counts as 1), stopping as
soon as a strike is refused, so that a numeric prefix can neither push
existing ink to the right nor go past the margin.

The buffer is kept read-only for the whole time `typewriter-mode' is on.
`typewriter--pre-command' has already allowed the first strike before
this command runs, and each further strike is checked here."
  (interactive "p")
  (typewriter--strike (max n 1) (lambda () (self-insert-command 1)) t))

(defun typewriter-newline ()
  "Typewriter replacement for `newline'.

The buffer is normally kept read-only for the whole time
`typewriter-mode' is on, and `typewriter--pre-command' has already
decided, before this command runs, whether the pending insertion is
allowed.  So by the time control reaches here, insertion is always meant
to succeed, and the only job left is to make the buffer briefly writable
for it."
  (interactive)
  (let ((inhibit-read-only t)
        (post-self-insert-hook nil))
    (call-interactively #'newline))
  (run-hooks 'typewriter-carriage-return-hook)
  (typewriter--maybe-recenter))

(defun typewriter--strikable-p (character)
  "Return non-nil if striking CHARACTER would leave ink.

Control characters (including newline and tab) and line or paragraph
separators move the carriage instead, so they cannot be struck."
  (not (memq (get-char-code-property character 'general-category)
             '(Cc Zl Zp))))

(defun typewriter-insert-char (character &optional count inherit)
  "Typewriter replacement for `insert-char'.

Read CHARACTER the way `insert-char' does, then strike it as if it had
been typed: the margin and the overstrike rules apply, the bell rings as
usual, and `typewriter-insert-hook' runs.  With COUNT, strike CHARACTER
that many times, stopping as soon as a strike is refused (for example,
at the margin).  INHERIT is passed on to `insert-char'.

Characters that move the carriage rather than leave ink (newline,
tab, other control characters) are refused: use
\\[typewriter-newline] and \\[typewriter-tab] for that.

Unlike `typewriter-self-insert', the checks cannot happen in
`typewriter--pre-command', because the character is only known once it
has been read from the minibuffer (and the user may still quit before
that)."
  (interactive
   (list (read-char-by-name "Insert character (Unicode name or hex): ")
         (prefix-numeric-value current-prefix-arg)
         t))
  (if (typewriter--strikable-p character)
      (typewriter--strike (or count 1)
                          (lambda () (insert-char character 1 inherit)))
    (typewriter--bell-ring)
    (message
     (substitute-command-keys
      "Only printing characters can be struck.  Use \\[typewriter-newline] and \\[typewriter-tab] to move the carriage"))))

(defun typewriter--input-method-function (fn &rest args)
  "Run the input method FN with ARGS, as if the buffer were writable.

Input methods (quail, robin, the `ucs' method, and so on) return the raw
key untranslated when `buffer-read-only' is non-nil.  Since
`typewriter-mode' keeps the buffer read-only at all times, this would
make every input method a no-op.  Binding `buffer-read-only' to nil here
only lasts while the input method is reading and translating a key; what
it returns is then processed as ordinary keystrokes, subject to the
usual typewriter rules."
  (let ((buffer-read-only nil))
    (apply fn args)))

(defun typewriter--wrap-input-method ()
  "Make the input method of the current buffer usable in `typewriter-mode'.

See `typewriter--input-method-function'.  Called when the mode is turned
on and from `input-method-activate-hook', since activating an input
method resets `input-method-function'."
  ;; The default value of `input-method-function' is `list', not nil,
  ;; so test `current-input-method' to tell if one is really active.
  (when (and current-input-method
             input-method-function
             (not (advice-function-member-p
                   #'typewriter--input-method-function
                   input-method-function)))
    (add-function :around (local 'input-method-function)
                  #'typewriter--input-method-function)))

(defun typewriter--unwrap-input-method ()
  "Undo `typewriter--wrap-input-method'."
  (when (and input-method-function
             (advice-function-member-p #'typewriter--input-method-function
                                       input-method-function))
    (remove-function (local 'input-method-function)
                     #'typewriter--input-method-function)))

(defun typewriter-ns-put-working-text ()
  "Show the text being composed by a macOS input method at point.

Wrapper around `ns-put-working-text', which inserts the in-progress
composition into the buffer (and deletes it before the final text is
sent as ordinary keystrokes)."
  (interactive)
  (let ((inhibit-read-only t))
    (call-interactively 'ns-put-working-text)))

(defun typewriter-ns-unput-working-text ()
  "Remove the text being composed by a macOS input method.

Wrapper around `ns-unput-working-text'."
  (interactive)
  (let ((inhibit-read-only t))
    (call-interactively 'ns-unput-working-text)))

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

(defun typewriter--clear-blank ()
  "Delete the blank under the carriage, which is about to be overstruck.

A tab is first spread into as many spaces as the columns it spans, so
that overstriking it doesn't pull the ink after it to the left."
  (let ((inhibit-read-only t))
    (when (eq (char-after) ?\t)
      (let ((width (- (save-excursion (forward-char 1) (current-column))
                      (current-column))))
        (delete-char 1)
        (save-excursion (insert (make-string width ?\s)))))
    (when (eq (char-after) ?\s)
      (delete-char 1))))

(defun typewriter--prepare-strike (command)
  "Get ready for COMMAND to act on the carriage at point, if allowed.

If COMMAND is not allowed, ring the bell, explain why in the echo area
and return nil.  If it is, ring the warning bell when appropriate and,
when COMMAND is `typewriter-self-insert' and the carriage is on a blank,
clear that blank, which is about to be overstruck; then return t."
  (let ((col (current-column)))
    (cond
     ((and (eq command 'typewriter-self-insert)
           (not (eobp))
           (not (eolp))
           (not (looking-at-p "\t\\|\s")))
      ;; trying to type over existing ink
      (typewriter--bell-ring)
      (message
       (substitute-command-keys
        "You can only overstrike blank spaces.  \\[typewriter-mode] to toggle off and edit"))
      nil)

     ((and typewriter-fill-column
           (not (eq command 'typewriter-newline))
           (>= col typewriter-fill-column))
      ;; we're at the margin
      (typewriter--bell-ring t)
      (typewriter--bell-ring)
      (message
       (substitute-command-keys
        "Margin reached!  Press \\[typewriter-newline] to return the carriage."))
      nil)

     (t
      ;; just type
      (typewriter--bell-ring t)
      (when (eq command 'typewriter-self-insert)
        (typewriter--clear-blank))
      t))))

(defun typewriter--pre-command ()
  "Enforce margins and ink permanence for the pending keystroke."
  (when (memq this-command '(typewriter-self-insert
                             typewriter-newline
                             typewriter-tab
                             typewriter-backward-char))

    (when (eq this-command 'typewriter-newline)
      (if (save-excursion
            (end-of-line)
            (skip-chars-forward " \t\n")
            (eobp))
          (goto-char (point-max))
        (forward-line 1)
        (setq this-command 'ignore)))

    (unless (or (eq this-command 'ignore)
                (typewriter--prepare-strike this-command))
      (setq this-command 'ignore))))

(defun typewriter--post-command ()
  "Refresh the mode line if `typewriter-show-chars-remaining' is non-nil.

The typewriter hooks are not run here but by the commands themselves,
after a successful action."
  (when (and typewriter-fill-column
             typewriter-show-chars-remaining)
    (force-mode-line-update)))

(defun typewriter--error-handler (data context caller)
  "Handle read-only errors with a custom unlogged message.

Other errors are passed on, with DATA, CONTEXT and CALLER, to
`command-error-default-function'."
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
  "<remap> <self-insert-command>" #'typewriter-self-insert
  "<remap> <insert-char>" #'typewriter-insert-char
  "<remap> <ns-put-working-text>" #'typewriter-ns-put-working-text
  "<remap> <ns-unput-working-text>" #'typewriter-ns-unput-working-text)

(defconst typewriter--overridden-variables '(buffer-read-only
                                             indent-line-function
                                             tab-width
                                             tab-stop-list
                                             electric-indent-mode
                                             auto-fill-function
                                             command-error-function
                                             fill-column)
  "Buffer-local variables that `typewriter-mode' temporarily overrides.

Used to save their pre-mode values into `typewriter--saved-state' on
enable, and restore them on disable.")

(defvar-local typewriter--saved-state nil
  "Alist of (VARIABLE . VALUE) saved before `typewriter-mode' was enabled.

Populated from `typewriter--overridden-variables'.")

(defvar-local typewriter--undo-disabled nil
  "Non-nil if `typewriter-mode' turned off undo recording in this buffer.")

(define-minor-mode typewriter-mode
  "A minor mode emulating a strict typewriter.

Only allows appending text to the end of the buffer or on whitespace.
No deletions or arbitrary edits."
  :init-value nil
  :lighter " Typewriter"
  :keymap typewriter-mode-map

  (if typewriter-mode

      (progn
        (when (and (not typewriter-preserve-undo-history)
                   (not (eq buffer-undo-list t)))
          (setq typewriter--undo-disabled t)
          (setq-local buffer-undo-list t))
        ;; Save original variable states before overriding, unless the
        ;; mode was already on (`(typewriter-mode 1)' called twice), in
        ;; which case the current values are our own overrides.
        (unless typewriter--saved-state
          (setq typewriter--saved-state
                (mapcar (lambda (sym) (cons sym (symbol-value sym)))
                        typewriter--overridden-variables)))
        (setq-local buffer-read-only t
                    indent-line-function #'tab-to-tab-stop
                    ;; tab-width still needs a 1 floor because it's
                    ;; (probably) read by many other things that don't
                    ;; assume a 0 value (tolerated by this minor mode
                    ;; as a value for typewriter-tab-width)
                    tab-width (max 1 typewriter-tab-width)
                    tab-stop-list nil
                    electric-indent-mode nil
                    ;; Auto-fill would break lines by deleting ink.
                    auto-fill-function nil
                    command-error-function #'typewriter--error-handler)
        ;; Never set `fill-column' to nil: plenty of code (including
        ;; auto-fill, should the user turn it back on) assumes it is a
        ;; number.
        (when typewriter-fill-column
          (setq-local fill-column typewriter-fill-column))
        (typewriter--wrap-input-method)
        (add-hook 'input-method-activate-hook
                  #'typewriter--wrap-input-method nil t)
        (add-hook 'pre-command-hook #'typewriter--pre-command nil t)
        (add-hook 'post-command-hook #'typewriter--post-command nil t)
        (when (<= typewriter-tab-width 0)
          (message "typewriter-tab-width is %S; TAB key will be disabled"
                   typewriter-tab-width)
          (sit-for 2)))

    ;; Only restore undo tracking if WE were the ones who disabled it
    (when typewriter--undo-disabled
      (setq buffer-undo-list nil
            typewriter--undo-disabled nil))
    (when typewriter--saved-state
      (dolist (entry typewriter--saved-state)
        (set (make-local-variable (car entry)) (cdr entry)))
      (setq typewriter--saved-state nil))
    (typewriter--unwrap-input-method)
    (remove-hook 'input-method-activate-hook
                 #'typewriter--wrap-input-method t)
    (remove-hook 'pre-command-hook #'typewriter--pre-command t)
    (remove-hook 'post-command-hook #'typewriter--post-command t)))

(add-to-list 'mode-line-misc-info
             '(typewriter-mode ("" (:eval (typewriter--mode-line-remaining))))
             t)

(provide 'typewriter)

;;; typewriter.el ends here

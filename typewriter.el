;;; typewriter.el --- Turn Emacs into a text adder  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Enrico Flor <enrico@eflor.net>
;; Maintainer: Enrico Flor <enrico@eflor.net>
;; URL: https://github.com/enricoflor/typewriter.el
;; Version: 1.2.1
;; Keywords: text

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

;;; News:

;; Version 1.2.0
;;
;; - New command `typewriter-strikethrough' (C-c -) strikes
;;   `typewriter-strikethrough-char' (X by default) over existing
;;   text on the last line, to cross it out.  Set the option to nil
;;   to disable it. (thanks to Christopher Howard for suggesting).
;;
;; - Characters can be struck with an input method (C-\), with C-x 8
;;   sequences and with `insert-char' (C-x 8 RET).  The usual rules
;;   about margins and overstriking apply. (thanks Christopher Howard
;;   for pointing out the bug).
;;
;; - New hooks: `typewriter-backward-char-hook' and
;;   `typewriter-tab-hook'.
;;
;; - `typewriter-keystroke-hook' is renamed `typewriter-insert-hook'.
;;   The old name still works, but is obsolete.
;;
;; - TAB stops at the margin instead of going past it.
;;
;; - Auto-fill is turned off while the mode is on.
;;
;; - A numeric prefix (C-u 3 a) no longer pushes text to the right
;;   or past the margin.
;;
;; - Overstriking a tab no longer moves the text after it.
;;
;; - Fixed: `electric-pair-mode' blocking the carriage, turning the
;;   mode on twice, undo being re-enabled when it was already off,
;;   and an error from `typewriter-recenter' when the buffer is not
;;   in the selected window.
;;
;; - Typewriter-mode is now autoloaded (thanks bcc32 for suggesting).

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

(defcustom typewriter-strikethrough-char ?X
  "Character that `typewriter-strikethrough' strikes over existing ink.

If this variable is a character, the command `typewriter-strikethrough'
strikes it at the carriage, even over ink, but only on the last line of
the buffer.  A one-character string such as \"X\" is accepted too.  If
nil, strikethrough is disabled.

The character should be one column wide: a wider one pushes the ink
after it to the right."
  :type '(choice (const :tag "Disabled" nil)
                 (character :tag "Strikethrough character"))
  :package-version '(typewriter . "1.2.0"))

(define-obsolete-variable-alias 'typewriter-keystroke-hook
  'typewriter-insert-hook "1.2.0")

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

(defun typewriter--maybe-recenter ()
  "Recenter the selected window if `typewriter-recenter' is non-nil.

Do nothing if the selected window doesn't show the current buffer, in
which case `recenter' would signal an error."
  (when (and typewriter-recenter
             (eq (window-buffer) (current-buffer)))
    (recenter)))

(defun typewriter--strike (command count insert &optional first-prepared)
  "Strike a character COUNT times by calling INSERT with no arguments.

Before each strike, `typewriter--prepare-strike' decides whether it is
allowed for COMMAND, and striking stops at the first refusal (for
example, at the margin).  If FIRST-PREPARED is non-nil, the first
strike has already been prepared by `typewriter--pre-command' and is
not checked again.  `typewriter-insert-hook' runs after each strike."
  (catch 'refused
    (dotimes (i count)
      (unless (or (and first-prepared (= i 0))
                  (typewriter--prepare-strike command))
        (throw 'refused nil))
      ;; `post-self-insert-hook' is disabled because some of its
      ;; functions edit the buffer: `electric-pair-mode', for one,
      ;; inserts a closing delimiter after point, which then blocks
      ;; the carriage as if it were ink.
      (let ((inhibit-read-only t)
            (post-self-insert-hook nil))
        (funcall insert))
      (run-hooks 'typewriter-insert-hook)))
  (typewriter--maybe-recenter))

(defun typewriter-self-insert (n)
  "Typewriter replacement for `self-insert-command'.

Strike the typed character N times (N below 1 counts as 1), stopping as
soon as a strike is refused, on ink or at the margin.

The buffer is kept read-only for the whole time `typewriter-mode' is on.
`typewriter--pre-command' has already allowed the first strike before
this command runs, and each further strike is checked here."
  (interactive "p")
  (typewriter--strike 'typewriter-self-insert (max n 1)
                      (lambda () (self-insert-command 1))
                      t))

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
      (typewriter--strike 'typewriter-self-insert (or count 1)
                          (lambda () (insert-char character 1 inherit)))
    (typewriter--bell-ring)
    (message
     (substitute-command-keys
      "Only printing characters can be struck.  Use \\[typewriter-newline] and \\[typewriter-tab] to move the carriage"))))

(defun typewriter--strikethrough-char ()
  "Return `typewriter-strikethrough-char' as a character.

A one-character string is converted to its character.  Any other value
is returned as is."
  (let ((char typewriter-strikethrough-char))
    (if (and (stringp char) (= (length char) 1))
        (aref char 0)
      char)))

(defun typewriter-strikethrough (n)
  "Strike `typewriter-strikethrough-char' at the carriage, N times.

Unlike ordinary typing, this can strike over existing ink, which is how
you cross out text on a typewriter.  It only works on the last line of
the buffer, the one the carriage is on: once you have returned the
carriage, the lines above are out of reach.  The margin rules apply as
usual, and each strike advances the carriage by one column, so a numeric
prefix N crosses out N characters.  `typewriter-insert-hook' runs after
each strike."
  (interactive "p")
  (let ((char (typewriter--strikethrough-char)))
    (cond
     ((null char)
      (message
       "Strikethrough is disabled (typewriter-strikethrough-char is nil)"))
     ((not (and (characterp char) (typewriter--strikable-p char)))
      (typewriter--bell-ring)
      (message
       "typewriter-strikethrough-char should be a printing character, like ?X"))
     (t
      (typewriter--strike 'typewriter-strikethrough (max n 1)
                          (lambda () (insert-char char)))))))

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

(defun typewriter--clear-ink ()
  "Delete the ink under the carriage, which is about to be struck through.

If the ink is wider than `typewriter-strikethrough-char' (for example, a
double-width character), leave spaces behind, so that the ink after it
stays in place."
  (let* ((inhibit-read-only t)
         (width (- (save-excursion (forward-char 1) (current-column))
                   (current-column)))
         (pad (- width (char-width (typewriter--strikethrough-char)))))
    (delete-char 1)
    (when (> pad 0)
      (save-excursion (insert (make-string pad ?\s))))))

(defun typewriter--prepare-strike (command)
  "Get ready for COMMAND to act on the carriage at point, if allowed.

If COMMAND is not allowed, ring the bell, explain why in the echo area
and return nil.  If it is, ring the warning bell when appropriate and,
when COMMAND is `typewriter-self-insert' or `typewriter-strikethrough',
clear what is under the carriage, which is about to be overstruck; then
return t.  `typewriter-self-insert' may only overstrike blanks, while
`typewriter-strikethrough' may also strike ink, but only on the last
line."
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

     ((and (eq command 'typewriter-strikethrough)
           (< (point) (save-excursion (goto-char (point-max))
                                       (line-beginning-position))))
      (typewriter--bell-ring)
      (message "You can only strike through text on the last line")
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
      (cond ((eq command 'typewriter-self-insert)
             (typewriter--clear-blank))
            ((eq command 'typewriter-strikethrough)
             (if (or (eolp) (looking-at-p "\t\\|\s"))
                 (typewriter--clear-blank)
               (typewriter--clear-ink))))
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
  "Refresh the mode line if `typewriter-show-chars-remaining' is non-nil."
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
  "C-c -" #'typewriter-strikethrough
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

;;;###autoload
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
        ;; mode was already on (`(typewriter-mode 1)' called twice),
        ;; in which case the current values are our own overrides.
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

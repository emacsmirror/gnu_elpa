;;; window-commander.el --- Simply execute commands on windows -*- lexical-binding: t -*-

;; Copyright (C) 2023 Free Software Foundation, Inc.

;; Author: Daniel Semyonov <daniel@dsemy.com>
;; Maintainer: Daniel Semyonov <daniel@dsemy.com>
;; Version: 3.0.2
;; Package-Requires: ((emacs "27.1"))
;; Keywords: convenience
;; URL: https://dsemy.com/projects/window-commander

;; This file is not part of GNU Emacs.

;; Window Commander is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.

;; Window Commander is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Window Commander provides a minor mode for switching to windows or
;; performing actions on them using IDs assigned to them automatically.
;;
;; Usage:
;;
;; Enable `wincom-mode':
;;
;; (wincom-mode)
;;
;; For use-package users:
;;
;; (use-package window-commander
;;   :config
;;   (wincom-mode))
;;
;; When `wincom-mode' is active:
;; - A window ID is displayed using a mode line lighter and/or a display
;;   function (see `wincom-display-lighter').
;; - Window IDs are assigned to all windows on all frames except for
;;   the minibuffer (by default, see `wincom-scope').
;; - `other-window' (C-x o by default) is remapped to `wincom-select'.
;;
;; C-x o ID	switches focus to the window which corresponds to ID.
;;
;; C-x o 0 ID	deletes the window which corresponds to ID.
;;
;; C-x o 1 ID	makes the window which corresponds to ID the sole
;;		window of its frame.
;;
;; C-x o 2 ID	splits the window which corresponds to ID from below.
;;
;; C-x o 3 ID	splits the window which corresponds to ID from the right.
;;
;; C-x 0 4 ID	displays the buffer of the next command in the window
;;		which corresponds to ID.
;;
;; C-x 0 t ID	swaps the states of the current window and the window
;;		which corresponds to ID.
;;
;; C-x o m	switches focus to the minibuffer if it's active.
;;
;; More commands can be added through `wincom-command-map':
;;
;; (define-key wincom-command-map (kbd "z") #'my-command)
;;
;; You can customize Window Commander further using the customize interface:
;;
;; M-x customize-group RET window-commander RET
;;
;; For more information see info node `(window-commander)'.

;;; Code:

(eval-when-compile
  (require 'subr-x)
  ;; Avoid byte-compilation warnings.
  (defvar wincom-mode)
  (defvar wincom-command-map)
  (declare-function wincom-selected-window-prefix nil))

;;;; Customization:

(defgroup window-commander nil
  "Window Commander."
  :link '(custom-manual "(window-commander) Top")
  :group 'convenience
  :prefix "wincom-")

(defun wincom--set-and-maybe-update (sym val)
  "Set SYM's variable cell to VAL and call `wincom--update' conditionally."
  (set-default sym val)
  (and (boundp wincom-mode) wincom-mode (wincom--update)))

(defcustom wincom-id-chars '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
  "Base set of characters from which window IDs are constructed.
This list should contain at least two characters."
  :link '(info-link "(window-commander) Customization")
  :type '( repeat :validate
           (lambda (w)
             (unless (nth 1 (widget-value w))
               (widget-put
                w :error
                "`wincom-id-chars' should contain at least two characters")
               w))
           character)
  :initialize #'custom-initialize-changed
  :set #'wincom--set-and-maybe-update
  :risky t
  :package-version '(swsw . 1.0))

(defcustom wincom-scope t
  "Scope of all window operations.
- t means consider all windows on all existing frames.
- 0 (the number zero) means consider all windows on all visible and
  iconified frames.
- `visible' means consider all windows on all visible frames.
- `current' means consider only the currently selected frame."
  :link '(info-link "(window-commander) Customization")
  :type '(radio (const :tag "All windows on all frames" t)
                (const
                 :tag "All windows on all visible and iconified frames" 0)
                (const :tag "All windows on all visible frames" visible)
                (const
                 :tag "All windows on the currently selected frame"
                 current))
  :initialize #'custom-initialize-changed
  :set #'wincom--set-and-maybe-update
  :risky t
  :package-version '(swsw . 1.1))

(defcustom wincom-minimum 3
  "Minimum number of tracked windows for which interactive selection occurs."
  :link '(info-link "(window-commander) Window Commands")
  :type 'integer
  :risky t
  :package-version '(swsw . 2.3))

(defcustom wincom-display-lighter t
  "Whether or not to show a mode line lighter.
- non-nil means show a mode line lighter.
- nil means don't show a mode line lighter."
  :link '(info-link "(window-commander) Display functions")
  :type '(radio (const :tag "Show mode line lighter" t)
                (const :tag "Don't show mode line lighter" nil))
  :package-version '(swsw . 2.2))

(defcustom wincom-mode-hook nil
  "Hook run when enabling or disabling Window Commander mode."
  :link '(info-link "(window-commander) Display functions")
  :type 'hook
  :options '(wincom-mode-line-display-function
             wincom-mode-line-conditional-display-function)
  :initialize #'custom-initialize-changed
  :set (lambda (sym hooks)
         (let ((wincom-mode nil))
           (when (boundp sym) (run-hooks sym)))
         (set-default sym hooks)
         (run-hooks sym))
  :package-version '(swsw . 1.0))

(defcustom wincom-id-format " <%s>"
  "Format string for the window ID.
%s is replaced with a representation of the window's ID."
  :link '(info-link "(window-commander) Customization")
  :type 'string
  :package-version '(swsw . 1.0))

;;;; Window tracking:

(defvar wincom--id-counter nil
  "Counter which determines the next possible ID.")
(put 'wincom--id-counter 'risky-local-variable t)

(defvar wincom--id-map (make-sparse-keymap)
  "Key map for window ID selection.")

(defvar wincom-window-count 0
  "Amount of windows that have been assigned an ID.")
(put 'wincom-window-count 'risky-local-variable t)

(defvar wincom--current-frame nil
  "Current frame (set by `wincom--update'), used to detect frame changes.")
(put 'wincom--current-frame 'risky-local-variable t)

(defun wincom--get-scope ()
  "Return the current scope in which windows should be tracked."
  (if (eq wincom-scope 'current)
      (selected-frame)
    wincom-scope))

(defun wincom--get-id-length ()
  "Return the current length of a window ID."
  (if-let ((windows (length (window-list-1 nil nil (wincom--get-scope))))
           ((= windows 1)))
      1 ; If there is only one window, return 1.
    (ceiling (log windows (length wincom-id-chars)))))

(defun wincom--next-id ()
  "Get the next available ID."
  (let ((len (length wincom-id-chars)) (adv-flag t) id)
    (setq wincom--id-counter
          ;; Translate the current value of the counter to the
          ;; corresponding ID.
          (mapcar (lambda (elt)
                    (push (nth elt wincom-id-chars) id)
                    ;; Advance `wincom--id-counter'.
                    (when adv-flag
                      (if (= len (setq elt (1+ elt)))
                          (setq elt 0)
                        (setq adv-flag nil)))
                    elt)
                  wincom--id-counter))
    id))

(defun wincom--update-window (window)
  "Update information for WINDOW."
  (when-let ((id (if (window-minibuffer-p window)
                     (progn
                       (setq wincom-window-count (1+ wincom-window-count))
                       nil)
                   (wincom--next-id))))
    ;; Create a key sequence from the ID, which corresponds to a
    ;; command which calls the last command (with the corresponding
    ;; window as the sole argument).
    ;; This allows controlling which command is invoked when
    ;; choosing an ID by setting `this-command' in a command which
    ;; sets the transient map to `wincom--id-map'.
    (define-key wincom--id-map (apply #'vector id)
                `(lambda ()
                   (interactive)
                   (funcall last-command ,window)))
    (set-window-parameter window 'wincom-id id)
    (setq wincom-window-count (1+ wincom-window-count))))

;; This is a separate function only to prevent running `wincom--update'
;; on any window state change.
(defun wincom--update-frame ()
  "Run `wincom--update' if the current frame isn't `wincom--current-frame'.
This check is skipped (and this function does nothing) if `wincom-scope'
is t."
  (unless (or (eq (wincom--get-scope) t)
              (eq wincom--current-frame (selected-frame)))
    (wincom--update)))

(defun wincom--update (&optional _frame)
  "Update information for all windows."
  (setq wincom--id-map (make-sparse-keymap))
  (set-keymap-parent wincom--id-map wincom-command-map)
  (setq wincom--id-counter nil
        wincom-window-count 0
        wincom--current-frame (selected-frame))
  ;; Clear and resize `wincom--id-counter' according to the ID length.
  ;; `wincom--id-counter' is treated as a base-N number where N is the
  ;; length of `wincom-id-chars' and each digit M represents the Mth
  ;; char in `wincom-id-chars'.
  (dotimes (_var (wincom--get-id-length))
    (push 0 wincom--id-counter))
  (walk-windows #'wincom--update-window nil (wincom--get-scope)))

;;;; Display functions:

(defun wincom-format-id (window)
  "Format an ID string for WINDOW."
  (format-spec
   wincom-id-format
   `((?s . ,(apply #'string (window-parameter window 'wincom-id))))))

(defun wincom--display-mode-line-show ()
  "Display window IDs at the beginning of the mode line."
  (setq-default mode-line-format
                `((wincom-mode
                   (:eval (wincom-format-id (selected-window))))
                  ,@(assq-delete-all
                     'wincom-mode
                     (default-value 'mode-line-format))))
  (force-mode-line-update t))

(defun wincom--display-mode-line-hide ()
  "Remove window IDs from the beginning of the mode line."
  (setq-default mode-line-format
                (assq-delete-all
                 'wincom-mode
                 (default-value 'mode-line-format)))
  (force-mode-line-update t))

(defun wincom-display-mode-line ()
  "Display window IDs at the beginning of the mode line.
Display window IDs if Window Commander mode is enabled, and disable
displaying window IDs if Window Commander Mode is disabled.
This display function respects `wincom-id-format'."
  (if wincom-mode (wincom--display-mode-line-show)
    (wincom--display-mode-line-hide)))

(defun wincom-display-mode-line-conditional ()
  "Display window IDs at the beginning of the mode line during window selection.
Add a hook to `wincom-before-command-hook' which displays window IDs on
the mode line and add a hook to `wincom-after-command-hook' which hides
window IDs from the mode line if Window Commander mode is enabled,
and remove those hooks if Window Commander mode is disabled.
This display function respects `wincom-id-format'."
  (if wincom-mode
      (progn
        (add-hook 'wincom-before-command-hook #'wincom--display-mode-line-show)
        (add-hook 'wincom-after-command-hook #'wincom--display-mode-line-hide))
    (remove-hook 'wincom-before-command-hook #'wincom--display-mode-line-show)
    (remove-hook 'wincom-after-command-hook #'wincom--display-mode-line-show)))

;;;; Window commands:

(defun wincom-run-window-command (fun)
  "Run FUN as a window command.
Run `wincom-before-command-hook', set `this-command' to FUN and set a
transient map for ID selection which runs `wincom-after-command-hook' on
exit."
  (let* ((set 'set-text-conversion-style)
         (set (and (fboundp set) set))
         (s text-conversion-style)
         (overriding-text-conversion-style nil)
         (b (current-buffer)))
    (when set
      (funcall set nil)
      (frame-toggle-on-screen-keyboard nil nil))
    (run-hooks 'wincom-before-command-hook)
    (setq this-command fun)
    (set-transient-map wincom--id-map nil
                       `(lambda ()
                          (and ',set (buffer-live-p ,b)
                               (with-current-buffer ,b (funcall ',set ',s)))
                          (run-hooks 'wincom-after-command-hook)))))

(defmacro wincom-define-window-command (name args &rest body)
  "Define NAME as a window command with DOCSTRING as its documentation string.

Inside BODY, WINDOW and PREFIX (symbols) are bound to the selected
window and the raw prefix argument, respectively.
If PREFIX is omitted or nil, the resulting command will not accept a
prefix argument.

Currently, only a single KEYWORD-ARG is recognized, `:minibuffer':
When it's non-nil, allow the minibuffer to be selected by
`next-window' (when there are less than `wincom-minimum' tracked windows).

For more information, see info node `(window-commander) Window Commands'.

\(fn NAME (WINDOW [PREFIX]) [DOCSTRING] [KEYWORD ARG]... BODY...)"
  (declare (debug (&define name lambda-list [&optional stringp]
                           [&rest (gate keywordp form)] def-body))
           (doc-string 3) (indent defun))
  (let* ((window (car args)) (prefix (cadr args))
         (docstring (car body))
         (kargs (if (keywordp (car body)) body (cdr body)))
         (minibuffer (plist-get kargs :minibuffer)))
    `(defun ,name ,(and prefix `(,prefix))
       ,(when (stringp docstring) (format "%s

If less than `wincom-minimum' windows have been assigned an ID,
use the window returned by `next-window' (according to the
value of `wincom-scope'%s).
Otherwise, either a window is selected using its ID or a separate
window command is chosen.

  This is a window command, intended to be used only when Window
  Commander mode is enabled; for more information, see info node
  `(window-commander) Window Commands'.
" docstring (if minibuffer "" ", excluding the minibuffer")))
       (declare (modes wincom-mode)
                (interactive-only t))
       (interactive ,(and prefix "P"))
       (if-let ((f (lambda (,window)
                     ,@body))
                ((>= wincom-window-count
                     ;; Raise the minimum by one if the current
                     ;; command excludes the minibuffer and it's in scope.
                     (if-let (((not ,minibuffer))
                              (m (active-minibuffer-window))
                              (c (window-frame m))
                              (s wincom-scope)
                              (v (or (frame-visible-p c) (eq s t)))
                              ((or (eq s t)
                                   (equal c (selected-frame))
                                   (and (eq v 'icon) (eq s 0))
                                   (and (eq v t)
                                        (or (eq s 'visible) (eq s 0))))))
                         (1+ wincom-minimum)
                       wincom-minimum))))
           (wincom-run-window-command f)
         (funcall f (next-window nil (unless ,minibuffer 'exclude)
                                 (wincom--get-scope)))))))

(wincom-define-window-command wincom-select (window)
  "Select a window."
  :minibuffer t
  (select-window window))

(wincom-define-window-command wincom-delete (window)
  "Delete a window."
  (delete-window window))

(wincom-define-window-command wincom-delete-other (window)
  "Make a window the sole window of its frame."
  (delete-other-windows window))

(wincom-define-window-command wincom-split-window-below (window size)
  "Split a window from below.
If optional argument SIZE is omitted or nil, both windows get the same
height, or close to it.  If SIZE is positive, the upper window gets
SIZE lines.  If SIZE is negative, the lower window gets -SIZE lines."
  (split-window-below (and size (prefix-numeric-value size)) window))

(wincom-define-window-command wincom-split-window-right (window size)
  "Split a window from the right.
If optional argument SIZE is omitted or nil, both windows get the same
width, or close to it.  If SIZE is positive, the left-hand window gets
SIZE columns.  If SIZE is negative, the right-hand window gets -SIZE
columns.  Here, SIZE includes the width of the window’s scroll bar; if
there are no scroll bars, it includes the width of the divider column
to the window’s right, if any."
  (split-window-right (and size (prefix-numeric-value size)) window))

(defun wincom-display-buffer-selected-window (buffer alist)
  "Display BUFFER in the selected (through wincom) window.
ALIST is an association list of action symbols and values.  See
Info node `(elisp) Buffer Display Action Alists' for details of
such alists.

The function fails if ALIST has no `window' element or its value isn't a
live window, or if it is a minibuffer window or is dedicated to another
buffer; in that case return nil.
Otherwise, return the value of the `window' element.

This is an action function for buffer display, see Info
node ‘(elisp) Buffer Display Action Functions’.  It should be
called only by ‘display-buffer’ or a function directly or
indirectly called by the latter."
  (let ((window (cdr (assq 'window alist))))
    (unless (or (not (windowp window))
                (window-minibuffer-p window)
                (window-dedicated-p window))
      (window--display-buffer buffer window 'reuse alist))))

(when (fboundp 'display-buffer-override-next-command)
  (wincom-define-window-command wincom-selected-window-prefix (window)
    "Display the buffer of the next command in a window."
    (display-buffer-override-next-command
     (lambda (buffer alist)
       (setq alist (append `((window . ,window)) alist))
       (cons (wincom-display-buffer-selected-window buffer alist) 'reuse))
     nil (format "[window-commander-%s]" (window-parameter window 'wincom-id)))
    (message "Display next command buffer in the selected window...")))

(wincom-define-window-command wincom-swap (window)
  "Swap the states of a window and the currently selected window."
  (window-swap-states nil window)
  (and (eq (current-buffer) (window-buffer window)) (wincom--update)))

(defun wincom-select-minibuffer ()
  "Select the active minibuffer window (if it exists)."
  (declare (modes wincom-mode))
  (interactive)
  (select-window (or (active-minibuffer-window)
                     (user-error "There is no active minibuffer window"))))

(defvar wincom-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?o] #'wincom-select)
    (define-key map [?0] #'wincom-delete)
    (define-key map [?1] #'wincom-delete-other)
    (define-key map [?2] #'wincom-split-window-below)
    (define-key map [?3] #'wincom-split-window-right)
    (when (fboundp 'display-buffer-override-next-command)
      (define-key map [?4] #'wincom-selected-window-prefix))
    (define-key map [?t] #'wincom-swap)
    (define-key map [?m] #'wincom-select-minibuffer)
    map)
  "Key map for window commands.
This key map is set as the parent of `wincom--id-map' during ID
selection.")

;;;; wincom mode:

;;;###autoload
(define-minor-mode wincom-mode
  "Toggle Window Commander mode.

When Window Commander mode is enabled, window IDs are shown as mode line
lighters of the form \"<ID>\" (by default), and `other-window' is remapped to
`wincom-select' (a command used to select windows according to their ID).

The following key bindings are available after starting window
selection:

\\{wincom-command-map}"
  :global t
  :lighter (:eval (and wincom-display-lighter
                       (not (functionp wincom-display-lighter))
                       (wincom-format-id (selected-window))))
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map [remap other-window] #'wincom-select)
            map)
  (if wincom-mode
      (progn
        (wincom--update)
        (when (functionp wincom-display-lighter)
          (funcall wincom-display-lighter))
        (add-hook 'window-configuration-change-hook #'wincom--update)
        (add-hook 'window-state-change-hook #'wincom--update-frame)
        (add-hook 'minibuffer-setup-hook #'wincom--update)
        (add-hook 'minibuffer-exit-hook #'wincom--update)
        (add-hook 'after-delete-frame-functions #'wincom--update))
    (when (functionp wincom-display-lighter)
      (funcall wincom-display-lighter))
    (remove-hook 'window-configuration-change-hook #'wincom--update)
    (remove-hook 'window-state-change-hook #'wincom--update-frame)
    (remove-hook 'minibuffer-setup-hook #'wincom--update)
    (remove-hook 'minibuffer-exit-hook #'wincom--update)
    (remove-hook 'after-delete-frame-functions #'wincom--update)))

(provide 'window-commander)

;;; window-commander.el ends here

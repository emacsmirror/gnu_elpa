;;; ergoemacs-term.el --- Terminal emulator support -*- lexical-binding: t -*-

;; Copyright © 2013-2025  Free Software Foundation, Inc.

;; Filename: ergoemacs-term.el
;; Description: Make the ergoemacs editing keys work inside terminal emulators
;; Author: Matthew L. Fidler
;; Maintainer:
;; Version:
;; Last-Updated:
;;           By:
;; URL:
;; Doc URL:
;; Keywords:
;; Compatibility:
;;
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; `ergoemacs-mode' installs its keys in an emulation keymap, so they win
;; over the keymap of a terminal emulator like `eat' or `term'.  That is
;; normally what one wants -- but a terminal buffer is not really editable.
;; It only shows the screen the inferior program draws; text inserted or
;; deleted there never reaches the program and is wiped out by the next
;; redraw.  So in a terminal the ergoemacs editing keys either do nothing
;; useful or actively corrupt the display, and the keys the terminal
;; program wanted (M-b, M-DEL, C-r ...) never arrive.
;;
;; This file makes those keys mean inside a terminal what they mean
;; everywhere else, by remapping the commands they run to commands that
;; send the equivalent key to the terminal.  Alt+e (kill the word before
;; the cursor) sends M-DEL, Ctrl+z sends C-_, Ctrl+v writes the current
;; kill to the terminal's input, and so on; see
;; `ergoemacs-term-command-alist'.
;;
;; Commands that do not change buffer text are deliberately left alone:
;; copying, searching the scrollback with Ctrl+f, setting the mark and
;; selecting all still act on the Emacs buffer, which is exactly where the
;; text one wants to copy lives.
;;
;; `eat' and `term' are supported out of the box.  Other terminal
;; emulators can be added to `ergoemacs-term-backends'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(defvar ergoemacs-mode)

(declare-function ergoemacs-copy-line-or-region "ergoemacs-functions")
(declare-function ergoemacs-cut-line-or-region "ergoemacs-functions")
(declare-function ergoemacs-paste "ergoemacs-functions")
(declare-function ergoemacs-paste-cycle "ergoemacs-functions")


;;; Backends

(defvar ergoemacs-term-backends nil
  "List of terminal emulators `ergoemacs-mode' knows how to talk to.

Each element is a list of the form

  (LIVE-P SEND YANK &optional KEYS-P)

LIVE-P is called with no arguments in the current buffer.  It returns
non-nil when the buffer displays a terminal that is still running, that
is, when a paste belongs in the terminal's input rather than in the
buffer.

SEND is called with an event and a repeat count, and sends the event to
that terminal.

YANK is called with a string, and sends it to the terminal as a paste.

KEYS-P is a second, stricter predicate: it returns non-nil only when the
terminal is also taking ordinary keys.  It exists so that a terminal
being browsed in an Emacs-like mode (`eat-emacs-mode', `term-line-mode')
keeps the ordinary ergoemacs editing keys while its paste key still
reaches the program.  It defaults to LIVE-P.")

(defun ergoemacs-term-backend (&optional keys)
  "Return the terminal backend of the current buffer, or nil.

With KEYS non-nil, return the backend only when the terminal is taking
keys; see `ergoemacs-term-backends'."
  (catch 'found
    (dolist (backend ergoemacs-term-backends)
      (when (and (funcall (nth 0 backend))
                 (or (not keys)
                     (funcall (or (nth 3 backend) (nth 0 backend)))))
        (throw 'found backend)))
    nil))

(defun ergoemacs-term-p (&optional keys)
  "Return non-nil when the current buffer displays a live terminal.

With KEYS non-nil, return non-nil only when that terminal is also taking
keys, that is, when typing in the buffer types at the program instead of
editing the buffer."
  (and (ergoemacs-term-backend keys) t))

(defun ergoemacs-term-send-event (event &optional n)
  "Send EVENT to the terminal of the current buffer, N times.
N defaults to 1."
  (let ((backend (ergoemacs-term-backend)))
    (unless backend
      (error "Not in a terminal buffer"))
    (funcall (nth 1 backend) event (or n 1))))

(defun ergoemacs-term-send-keys (keys &optional n)
  "Send KEYS to the terminal of the current buffer, N times.
KEYS is a key description understood by `kbd'.  N defaults to 1."
  (let ((events (listify-key-sequence (kbd keys))))
    (dotimes (_ (max 1 (or n 1)))
      (dolist (event events)
        (ergoemacs-term-send-event event 1)))))

(defun ergoemacs-term-yank-string (string)
  "Send STRING to the terminal of the current buffer as a paste."
  (let ((backend (ergoemacs-term-backend)))
    (unless backend
      (error "Not in a terminal buffer"))
    (funcall (nth 2 backend) string)))


;;; The keys

(defvar ergoemacs-mode-term-keymap (make-sparse-keymap)
  "Keymap `ergoemacs-mode' activates in live terminal buffers.

It holds nothing but command remappings, so that it works whatever key a
theme or a keyboard layout happens to put a command on.  It is rebuilt
by `ergoemacs-term--update-keymap' from `ergoemacs-term-command-alist'.")

(defvar ergoemacs-mode-term-mode nil
  "Non-nil when the current buffer displays a live terminal.
This enables `ergoemacs-mode-term-keymap'.  It is maintained by
`ergoemacs-term--select-keymaps'; do not set it by hand.")
(make-variable-buffer-local 'ergoemacs-mode-term-mode)

(defun ergoemacs-term--select-keymaps ()
  "Set `ergoemacs-mode-term-mode' for the current buffer.
Called by `ergoemacs--select-keymaps' around every command."
  (let ((term (and ergoemacs-mode (ergoemacs-term-p))))
    ;; This runs in every buffer, so only give a buffer its own binding when
    ;; there is something to say.
    (when (or term (local-variable-p 'ergoemacs-mode-term-mode))
      (setq ergoemacs-mode-term-mode term))))

(defcustom ergoemacs-term-command-alist
  '(;; Moving point is moving the terminal's cursor.
    (backward-char                    . "<left>")
    (left-char                        . "<left>")
    (forward-char                     . "<right>")
    (right-char                       . "<right>")
    (previous-line                    . "<up>")
    (next-line                        . "<down>")
    (backward-word                    . "M-b")
    (ergoemacs-backward-word          . "M-b")
    (forward-word                     . "M-f")
    (ergoemacs-forward-word           . "M-f")
    (move-beginning-of-line           . "C-a")
    (beginning-of-line                . "C-a")
    (ergoemacs-move-beginning-of-line . "C-a")
    (move-end-of-line                 . "C-e")
    (end-of-line                      . "C-e")
    (ergoemacs-move-end-of-line       . "C-e")
    ;; Deleting text.
    (delete-backward-char             . "DEL")
    (backward-delete-char             . "DEL")
    (backward-delete-char-untabify    . "DEL")
    (ergoemacs-delete-backward-char   . "DEL")
    (delete-char                      . "C-d")
    (ergoemacs-delete-char            . "C-d")
    (backward-kill-word               . "M-DEL")
    (ergoemacs-backward-kill-word     . "M-DEL")
    (kill-word                        . "M-d")
    (ergoemacs-kill-word              . "M-d")
    (kill-line                        . "C-k")
    (ergoemacs-kill-line              . "C-k")
    (ergoemacs-kill-line-backward     . "C-u")
    (transpose-chars                  . "C-t")
    (transpose-words                  . "M-t")
    ;; A terminal keeps no undo history of its own, but readline and the
    ;; shells built on it undo with C-_.
    (undo                             . "C-_")
    (ergoemacs-undo                   . "C-_")
    ;; Nothing to redo either, so give the redo key back its terminal
    ;; meaning: yank what the terminal itself last killed.
    (redo                             . "C-y")
    (ergoemacs-redo                   . "C-y")
    ;; Reverting a terminal is meaningless; C-r searches the history.
    (ergoemacs-revert-buffer          . "C-r"))
  "Commands to translate into terminal keys inside a terminal buffer.

Each element is (COMMAND . KEYS), where KEYS is a key description
understood by `kbd'.  When a key runs COMMAND in a buffer displaying a
terminal that is taking keys, KEYS is sent to that terminal instead --
so the ergoemacs key keeps its meaning even though the editing happens
inside the program rather than in the buffer.

Commands that only read the buffer, such as copying, searching the
scrollback or setting the mark, are left out on purpose: those act on
what Emacs has, which is what one wants."
  :type '(alist :key-type (symbol :tag "Command")
                :value-type (string :tag "Terminal key"))
  :set (lambda (sym val)
         (set-default sym val)
         (ergoemacs-term--update-keymap))
  :initialize #'custom-initialize-default
  :group 'ergoemacs-mode)

;;;###autoload
(defun ergoemacs-term-send-key (&optional n)
  "Send this command's terminal equivalent to the terminal, N times.

The command that was remapped to this one is looked up in
`ergoemacs-term-command-alist'.  When there is no entry for it, or when
the current buffer is not a terminal taking keys, run that command
instead."
  (interactive "p")
  (let* ((command (or this-original-command this-command))
         (keys (and (not (eq command 'ergoemacs-term-send-key))
                    (cdr (assq command ergoemacs-term-command-alist)))))
    (cond
     ((and keys (ergoemacs-term-p 'keys))
      (ergoemacs-term-send-keys keys n))
     ((eq command 'ergoemacs-term-send-key)
      ;; Bound to a key directly rather than reached through a remapping;
      ;; there is no command left to stand in for.
      (ding))
     (t
      (setq this-command command)
      (call-interactively command)))))

;;;###autoload
(defun ergoemacs-term-paste ()
  "Send the most recent kill to the terminal.

Text yanked into a terminal buffer is never seen by the program running
in it and vanishes at the next redraw, so write it to the terminal's
input instead."
  (interactive)
  (if (ergoemacs-term-p)
      (ergoemacs-term-yank-string (current-kill 0))
    (call-interactively #'ergoemacs-paste)))

;;;###autoload
(defun ergoemacs-term-paste-cycle ()
  "Pick an entry from the kill ring and send it to the terminal.

A terminal has no notion of cycling through previous pastes -- the last
paste is already gone into the program -- so this asks which kill to
send instead of replacing the previous one."
  (interactive)
  (if (ergoemacs-term-p)
      (ergoemacs-term-yank-string
       (if (fboundp 'read-from-kill-ring)
           (read-from-kill-ring "Send to terminal: ")
         (current-kill 1)))
    (call-interactively #'ergoemacs-paste-cycle)))

(defcustom ergoemacs-term-cut-line-keys "C-u"
  "Key the terminal is sent to discard its input line.
This is what `ergoemacs-term-cut-line-or-region' sends when there is no
region to copy.  \"C-u\" is what readline, and every shell built on it,
uses."
  :type 'string
  :group 'ergoemacs-mode)

;;;###autoload
(defun ergoemacs-term-cut-line-or-region (&optional arg)
  "Copy the region, or discard the terminal's input line.

Text cannot be cut out of a terminal buffer -- it belongs to the screen
the program drew, not to Emacs -- so with an active region this copies
it, and with no region it sends `ergoemacs-term-cut-line-keys'.

ARG is passed on to the command that ends up doing the work."
  (interactive "P")
  (cond
   ((use-region-p)
    (ergoemacs-copy-line-or-region arg))
   ((ergoemacs-term-p 'keys)
    (ergoemacs-term-send-keys ergoemacs-term-cut-line-keys))
   (t
    (ergoemacs-cut-line-or-region arg))))

(defvar ergoemacs-term-remap-alist
  '((ergoemacs-paste . ergoemacs-term-paste)
    (yank . ergoemacs-term-paste)
    (ergoemacs-paste-cycle . ergoemacs-term-paste-cycle)
    (yank-pop . ergoemacs-term-paste-cycle)
    (ergoemacs-cut-line-or-region . ergoemacs-term-cut-line-or-region)
    (kill-region . ergoemacs-term-cut-line-or-region))
  "Commands `ergoemacs-mode' replaces wholesale in a terminal buffer.

Unlike `ergoemacs-term-command-alist' these do not stand for a single
terminal key, so each gets its own command.")

(defun ergoemacs-term--update-keymap (&rest _)
  "Rebuild `ergoemacs-mode-term-keymap'."
  (let ((map (make-sparse-keymap)))
    (dolist (elt ergoemacs-term-command-alist)
      (define-key map (vector 'remap (car elt)) #'ergoemacs-term-send-key))
    (dolist (elt ergoemacs-term-remap-alist)
      (define-key map (vector 'remap (car elt)) (cdr elt)))
    ;; Keep the keymap object itself: `ergoemacs-override-alist' holds it
    ;; by value.
    (setcdr ergoemacs-mode-term-keymap (cdr map))))


;;; `eat'

(defvar eat-terminal)
(defvar eat--line-mode)
(defvar eat--semi-char-mode)
(defvar eat--char-mode)
(defvar eat--eshell-semi-char-mode)
(defvar eat--eshell-char-mode)
(declare-function eat-term-input-event "eat")
(declare-function eat-term-send-string-as-yank "eat")

(defun ergoemacs-term-eat-p ()
  "Return non-nil when the current buffer runs an `eat' terminal.
This includes `eat-emacs-mode', where the buffer is read-only but the
program is still running and a paste still belongs in its input."
  (and (bound-and-true-p eat-terminal)
       (fboundp 'eat-term-input-event)
       ;; In line mode the buffer really is an editable input line.
       (not (bound-and-true-p eat--line-mode))))

(defun ergoemacs-term-eat-keys-p ()
  "Return non-nil when an `eat' terminal is taking keys."
  (and (ergoemacs-term-eat-p)
       (or (bound-and-true-p eat--semi-char-mode)
           (bound-and-true-p eat--char-mode)
           (bound-and-true-p eat--eshell-semi-char-mode)
           (bound-and-true-p eat--eshell-char-mode))))

(defun ergoemacs-term-eat-send (event n)
  "Send EVENT to the `eat' terminal N times."
  (eat-term-input-event eat-terminal n event))

(defun ergoemacs-term-eat-yank (string)
  "Send STRING to the `eat' terminal as a paste."
  (eat-term-send-string-as-yank eat-terminal string))

(add-to-list 'ergoemacs-term-backends
             '(ergoemacs-term-eat-p ergoemacs-term-eat-send
               ergoemacs-term-eat-yank ergoemacs-term-eat-keys-p)
             'append)


;;; `term'

(declare-function term-send-raw-string "term")
(declare-function term-in-char-mode "term")

(defconst ergoemacs-term--term-key-strings
  '((up . "\eOA") (down . "\eOB") (right . "\eOC") (left . "\eOD")
    (home . "\e[1~") (insert . "\e[2~") (deletechar . "\e[3~")
    (delete . "\e[3~") (end . "\e[4~") (prior . "\e[5~")
    (next . "\e[6~"))
  "What `term-mode' sends for the function keys ergoemacs translates.
These are the strings term.el's own `term-send-up' and friends use.")

(defun ergoemacs-term-term-p ()
  "Return non-nil when the current buffer runs a `term-mode' terminal."
  (and (derived-mode-p 'term-mode)
       (fboundp 'term-send-raw-string)
       (get-buffer-process (current-buffer))
       t))

(defun ergoemacs-term-term-keys-p ()
  "Return non-nil when a `term-mode' terminal is taking keys."
  (and (ergoemacs-term-term-p)
       (term-in-char-mode)))

(defun ergoemacs-term-term--string (event)
  "Return the string `term-mode' should send for EVENT, or nil."
  (or (cdr (assq event ergoemacs-term--term-key-strings))
      (and (characterp event) (char-to-string event))
      ;; A meta character is not a `characterp': send it as ESC + the
      ;; character it is built on, the way a terminal expects.
      (and (numberp event)
           (memq 'meta (event-modifiers event))
           (let ((base (event-convert-list
                        (append (remq 'meta (event-modifiers event))
                                (list (event-basic-type event))))))
             (and (characterp base) (format "\e%c" base))))))

(defun ergoemacs-term-term-send (event n)
  "Send EVENT to the `term-mode' terminal N times."
  (let ((string (ergoemacs-term-term--string event)))
    (when string
      (dotimes (_ (or n 1))
        (term-send-raw-string string)))))

(defun ergoemacs-term-term-yank (string)
  "Send STRING to the `term-mode' terminal as a paste."
  (term-send-raw-string string))

(add-to-list 'ergoemacs-term-backends
             '(ergoemacs-term-term-p ergoemacs-term-term-send
               ergoemacs-term-term-yank ergoemacs-term-term-keys-p)
             'append)

(ergoemacs-term--update-keymap)

(provide 'ergoemacs-term)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ergoemacs-term.el ends here

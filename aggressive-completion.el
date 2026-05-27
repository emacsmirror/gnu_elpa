;;; aggressive-completion.el --- Automatic minibuffer completion -*- lexical-binding: t -*-

;; Copyright (C) 2021-2026 Free Software Foundation, Inc.

;; Author: Tassilo Horn <tsdh@gnu.org>
;; Maintainer: Tassilo Horn <tsdh@gnu.org>
;; Keywords: minibuffer completion
;; Package-Requires: ((emacs "27.1"))
;; Version: 1.7

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Aggressive completion mode (`aggressive-completion-mode') is a minor mode
;; which automatically completes for you after a short delay
;; (`aggressive-completion-delay') and shows all possible completions using the
;; standard completion help (unless the number of possible completions exceeds
;; `aggressive-completion-max-shown-completions' or
;; `aggressive-completion-auto-completion-help' is set to nil).
;;
;; Automatic completion is done after all commands in
;; `aggressive-completion-auto-complete-commands'.  The function doing
;; auto-completion is defined by `aggressive-completion-auto-complete-fn' which
;; defaults to `minibuffer-complete'.
;;
;; Aggressive completion can be toggled using
;; `aggressive-completion-toggle-auto-complete' (bound to `M-t' by default)
;; which is especially useful when trying to find a not yet existing file or
;; switch to a new buffer.
;;
;; You can switch from minibuffer to *Completions* buffer and back again using
;; `aggressive-completion-switch-to-completions' (bound to `M-c' by default).
;; All keys bound to this command in `aggressive-completion-minibuffer-map'
;; will be bound to `other-window' in `completion-list-mode-map' so that those
;; keys act as switch-back-and-forth commands.
;;
;; Aggressive completion can be used together, in theory, with other completion
;; UIs.  Using the following configuration, it works quite well with the
;; vertico package:
;;
;; Disable completion help since vertico shows the candidates anyhow.
;;
;; (setq aggressive-completion-auto-completion-help nil)
;;
;; A command which just expands the common part without selecting a candidate.
;;
;; (defun th/vertico-complete ()
;;   (interactive)
;;   (minibuffer-complete)
;;   (when vertico--count-ov ;; Only if vertico is active.
;;     (vertico--exhibit)))
;;
;; Use that for auto-completion.
;;
;; (setq aggressive-completion-auto-complete-fn #'th/vertico-complete)
;;
;; The inline help messages like "Next char not uniqe" make point bump to the
;; right because of vertico's overlays which is a bit annoying.  And since you
;; already see your candidates anyhow, the messages aren't important anyhow.
;;
;; (setq completion-show-inline-help nil)

;;; Code:

(eval-when-compile
  ;; For `when-let'.
  (require 'subr-x))

(defgroup aggressive-completion nil
  "Aggressive completion completes for you."
  :group 'minibuffer)

(defcustom aggressive-completion-delay 0.3
  "Delay in seconds before aggressive completion kicks in."
  :type 'number)

(defcustom aggressive-completion-auto-complete t
  "Complete automatically if non-nil."
  :type 'boolean)

(defcustom aggressive-completion-auto-completion-help t
  "Show completion help automatically if non-nil."
  :type 'boolean)

(defcustom aggressive-completion-max-shown-completions 1000
  "Maximum number of possible completions for showing completion help."
  :type 'integer)

(defcustom aggressive-completion-auto-complete-commands
  '( self-insert-command yank)
  "Commands after which automatic completion is performed."
  :type '(repeat function))

(defcustom aggressive-completion-auto-complete-fn
  #'minibuffer-complete
  "Function used to perform auto-completion.
The default is `minibuffer-complete'.  The idea is that you can
set it to some function of some alternative completion UI such as
Ivy, Selectrum, or Vertico."
  :type 'function)

(defcustom aggressive-completion-trigger-immediately t
  "If non-nil, trigger immediately after entering the minibuffer.
That means the *Completions* buffer will be shown immediately
(if there are at most `aggressive-completion-max-shown-completions').
If `nil' this will only happen after at least one command has
been performed in the minibuffer (e.g., you've typed something)."
  :type 'boolean)

(defvar aggressive-completion--timer nil)

(defvar-local aggressive-completion--minibuffer-tick nil)

(defun aggressive-completion--get-minibuffer-tick ()
  "Return the current state of the minibuffer."
  (when (minibufferp)
    (cons (point) (buffer-modified-tick))))

(defun aggressive-completion--do ()
  "Perform aggressive completion."
  (when (window-minibuffer-p)
    (let ((tick (aggressive-completion--get-minibuffer-tick)))
      (if (and (null aggressive-completion--minibuffer-tick)
               (not aggressive-completion-trigger-immediately))
          ;; The minibuffer has just been activated and we should not trigger
          ;; immediately. Set the tick so we only start with the next command.
          (setq-local aggressive-completion--minibuffer-tick tick)
        (unless (equal aggressive-completion--minibuffer-tick
                       tick)
          (setq-local aggressive-completion--minibuffer-tick tick)
          (let* ((completions (completion-all-sorted-completions))
                 ;; Don't ding if there are no completions, etc.
                 (visible-bell nil)
                 (ring-bell-function #'ignore)
                 ;; Automatic completion should not cycle.
                 (completion-cycle-threshold nil)
                 (completion-cycling nil))
            (let ((i 0))
              (while (and (<= i aggressive-completion-max-shown-completions)
                          (consp completions))
                (setq completions (cdr completions))
                (cl-incf i))
              (if (and (> i 0)
                       (<= i aggressive-completion-max-shown-completions))
                  (if (and aggressive-completion-auto-complete
                           (memq last-command
                                 aggressive-completion-auto-complete-commands))
                      ;; Perform automatic completion.
                      (progn
                        (let ((completion-auto-help
                               aggressive-completion-auto-completion-help))
                          (funcall aggressive-completion-auto-complete-fn))
                        (when (and aggressive-completion-auto-completion-help
                                   (not (window-live-p
                                         (get-buffer-window "*Completions*"))))
                          (minibuffer-completion-help)))
                    ;; Only show the completion help.
                    (when aggressive-completion-auto-completion-help
                      (minibuffer-completion-help)))
                ;; Close the *Completions* buffer if there are too many
                ;; or zero completions.
                (when-let ((win (get-buffer-window "*Completions*")))
                  (when (and
                         (window-live-p win)
                         (or
                          ;; With zero completions, we can always close.
                          (zerop i)
                          ;; When we've requested completion help via hitting
                          ;; TAB twice explicitly, it shouldn't be closed
                          ;; forcefully here.
                          (not (memq last-command
                                     (cons aggressive-completion-auto-complete-fn
                                           '(minibuffer-completion-help
                                             minibuffer-complete
                                             completion-at-point))))))
                    (quit-window nil win)))))))))))

(defun aggressive-completion--timer-restart ()
  "Restart `aggressive-completion--timer'."
  (when aggressive-completion--timer
    (cancel-timer aggressive-completion--timer))

  (setq aggressive-completion--timer
        (run-with-idle-timer aggressive-completion-delay nil
                             #'aggressive-completion--do)))

(defun aggressive-completion-toggle-auto-complete ()
  "Toggle automatic completion."
  (interactive)
  (setq aggressive-completion-auto-complete
        (not aggressive-completion-auto-complete)))

;; Add an alias so that we can find out the bound key using
;; `where-is-internal'.
(defalias 'aggressive-completion-switch-to-completions
  #'switch-to-completions)

(declare-function icomplete-fido-backward-updir "icomplete" nil)

(defvar aggressive-completion-minibuffer-map
  (let ((map (make-sparse-keymap)))
    (require 'icomplete)
    (define-key map (kbd "DEL") #'icomplete-fido-backward-updir)
    (define-key map (kbd "M-t") #'aggressive-completion-toggle-auto-complete)
    (define-key map (kbd "M-c") #'aggressive-completion-switch-to-completions)
    map)
  "The local minibuffer keymap when `aggressive-completion-mode' is enabled.")

(defun aggressive-completion--setup ()
  "Setup aggressive completion."
  (when (and (not executing-kbd-macro)
             (window-minibuffer-p)
             minibuffer-completion-table)
    (setq-local aggressive-completion--minibuffer-tick nil)
    (set-keymap-parent aggressive-completion-minibuffer-map (current-local-map))
    (use-local-map aggressive-completion-minibuffer-map)

    ;; If `aggressive-completion-switch-to-completions' is bound to keys, bind
    ;; the same keys in `completion-list-mode-map' to `other-window' so that
    ;; one can conveniently switch back and forth using the same key.
    (dolist (key (where-is-internal
	          #'aggressive-completion-switch-to-completions))
      (define-key completion-list-mode-map key #'other-window))

    (add-hook 'post-command-hook
              #'aggressive-completion--timer-restart nil t)))

;;;###autoload
(define-minor-mode aggressive-completion-mode
  "Perform aggressive minibuffer completion."
  :lighter " ACmp"
  :global t
  (if aggressive-completion-mode
      (add-hook 'minibuffer-setup-hook #'aggressive-completion--setup)
    (remove-hook 'minibuffer-setup-hook #'aggressive-completion--setup)))

(provide 'aggressive-completion)

;;; aggressive-completion.el ends here

;; Local Variables:
;; fill-column: 79
;; End:

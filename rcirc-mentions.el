;;; rcirc-mentions.el --- Log mentions of your nick or keywords in a separate buffer  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Free Software Foundation, Inc.
;;
;; Author: Tassilo Horn <tsdh@gnu.org>
;; Contributors: Philip Kaludercic <philipk@posteo.net>
;; Version: 1.0.5
;; Keywords: rcirc, irc
;; URL: https://sr.ht/~tsdh/rcirc-mentions/
;; Package-Requires: ((emacs "29.1"))
;;
;; This file is part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; The builtin `rcirc-track-minor-mode' provides a way to be informed when your
;; nick was mentioned in some channel and provides key bindings to quickly jump
;; to that channel.  But what if you cannot interact immediately?  That's where
;; `rcirc's mentions buffer comes into play.
;;
;; When you activate `rcirc-mentions-log-mode' in channel buffers (or
;; `rcirc-mentions-global-log-mode'), all mentions of your nick or a keyword in
;; `rcirc-keywords' will be collected in a buffer named `*rcirc mentions*'
;; (customizable by `rcirc-mentions-buffer-name').  Those log entries have a
;; timestamp, the text of the message containing the mention, and most
;; importantly, they contain a clickable link to the location of the original
;; mention in some channel buffer.  That way, you have a logbook of the most
;; important events in one central place.
;;
;; You can quickly switch to the mentions buffer from rcirc buffers where
;; `rcirc-mentions-log-mode' is active using
;; `rcirc-mentions-switch-to-mentions-buffer'.  The default binding is `C-c
;; C-.', the keymap is `rcirc-mentions-log-mode-map'.
;;
;; The mentions buffer uses a separate major mode,
;; `rcirc-mentions-buffer-mode', which provides some navigation commands, e.g.,
;; `rcirc-mentions-next' (`n') to move to the next mention, and
;; `rcirc-mentions-prev' (`p') to move to the previous mention.
;;
;;; Code:

(require 'rcirc)

(defgroup rcirc-mentions '()
  "Log mentions of your nick or keywords."
  :group 'rcirc)

(defcustom rcirc-mentions-buffer-name "*rcirc mentions*"
  "The name of the mentions buffer.
Mentions of your nick or a keyword in `rcirc-keywords' will be logged in
a buffer of this name if the mention occurs in a buffer where
`rcirc-mentions-log-mode' is enabled."
  :type 'string)

(defvar-keymap rcirc-mentions-log-mode-map
  :doc "The keymap of `rcirc-mentions-log-mode'."
  "C-c C-." #'rcirc-mentions-switch-to-mentions-buffer)

(define-minor-mode rcirc-mentions-log-mode
  "Minor mode to log mentions in the current channel in a separate buffer.
The name of this buffer is defined by `rcirc-mentions-buffer-name'.  In
this buffer, mentions of your nick or a keyword in `rcirc-keywords' will
be logged linking to the original message in the respective channel
buffer.

Also see `rcirc-mentions-global-log-mode' to enable/disable
`rcirc-mentions-log-mode' in all rcirc buffers."
  :lighter " Mentions"
  :interactive (rcirc-mode)
  (if rcirc-mentions-log-mode
      (add-hook 'rcirc-print-functions #'rcirc-mentions--print-function nil t)
    (remove-hook 'rcirc-print-functions #'rcirc-mentions--print-function t)))

;;;###autoload
(define-minor-mode rcirc-mentions-global-log-mode
  "Global minor mode for `rcirc-mentions-log-mode'.
Enabling the mode activates `rcirc-mentions-log-mode' in all current and
future `rcirc-mode' buffers.  Disabling the mode deactivates it in all
current and future `rcirc-mode' buffers."
  :global t
  (let ((state (if rcirc-mentions-global-log-mode
                   (progn
                     (add-hook 'rcirc-mode-hook
                               #'rcirc-mentions-log-mode)
                     1)
                 (remove-hook 'rcirc-mode-hook
                              #'rcirc-mentions-log-mode)
                 -1)))
    ;; Activate or deactivate it in all current `rcirc-mode' buffers.
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (derived-mode-p #'rcirc-mode)
          (rcirc-mentions-log-mode state))))))

(defun rcirc-mentions--determine-mention-types (my-nick _sender text)
  "Determine the mention types of the message TEXT from SENDER.
MY-NICK is your nick for this connection."
  (let (types)
    (with-temp-buffer
      (insert text)
      ;; Check if the new text contains my nick.
      (goto-char (point-min))
      (when (re-search-forward
             (concat "\\b" (regexp-quote my-nick) "\\b") nil t)
        (setq types (cons 'nick types)))
      ;; Check if the new text contains a keyword.
      (goto-char (point-min))
      (when (and rcirc-keywords
                 (re-search-forward (regexp-opt rcirc-keywords 'words) nil t))
        (setq types (cons 'keyword types))))
    types))

(defun rcirc-mentions--print-function (process sender response _target text)
  "The function being added to `rcirc-print-functions'.
Those are called with the PROCESS of the connection, the SENDER, the
RESPONSE, the TARGET and the message TEXT."
  (when (and rcirc-mentions-log-mode
             (string= response "PRIVMSG")
             (not (string= sender (rcirc-nick process))))
    (let ((types (rcirc-mentions--determine-mention-types
                  (rcirc-nick process) sender text)))
      (when types
        (rcirc-mentions--update-mentions-buffer types text)))))

(defun rcirc-mentions-next ()
  "Move to the next mention."
  (interactive)
  (when-let* ((b (next-button (point))))
    (goto-char (button-start b))))

(defun rcirc-mentions-prev ()
  "Move to the previous mention."
  (interactive)
  (when-let* ((b (previous-button (point))))
    (goto-char (button-start b))))

(defvar-keymap rcirc-mentions-buffer-mode-map
  "n" #'rcirc-mentions-next
  "p" #'rcirc-mentions-prev)

;; TODO: perhaps should this be based on `tabulated-list-mode'?
(define-derived-mode rcirc-mentions-buffer-mode special-mode
  "RcircMentions"
  "Major mode in the rcirc mentions buffer."
  ;; special-mode already sets buffer-read-only.
  ;;
  ;; We want to see the original fontification of the channel buffer.
  (font-lock-mode 1))

(defun rcirc-mentions--update-mentions-buffer (types text)
  "Update the mentions buffer with a mention of TYPES in TEXT.
TYPES is a list with symbols `nick' and/or `keyword'.

Assumes that the channel buffer containing the message is current."
  ;; Since TEXT is not fontified, we try to figure out the last message ourself
  ;; by finding the last message with a rcirc-text property value equal to
  ;; TEXT.
  (let* ((end (save-excursion
                (goto-char (point-max))
                (while (not (string= (get-text-property
                                      (max (point-min) (1- (point)))
                                      'rcirc-text)
                                     text))
                  (if-let* ((pos (previous-single-property-change
                                  (point) 'rcirc-text)))
                      (goto-char pos)
                    (error "Cannot find end of message with rcirc-text %S"
                           text)))
                (point)))
         (start (save-excursion
                  (goto-char (previous-single-property-change end 'rcirc-text))
                  ;; Include the mentioning nick.
                  (beginning-of-line)
                  (point)))
         (msg (buffer-substring start end))
         (activity-marker (set-marker (make-marker) start))
         (action
          (lambda (_button)
            (let ((buf (marker-buffer activity-marker)))
              (if (buffer-live-p buf)
                  (if-let* ((pos (marker-position activity-marker)))
                      (progn
                        (switch-to-buffer-other-window buf)
                        (goto-char activity-marker))
                    (message "The buffer %s has been truncated." buf))
                (message "The originating buffer has disappeared."))))))
    (with-current-buffer (get-buffer-create rcirc-mentions-buffer-name)
      (rcirc-mentions-buffer-mode)
      (let ((inhibit-read-only t)
            ;; Stay at the current position in the buffer when we are
            ;; not on the very last mention.
            (orig-pos (when (save-excursion
                              (text-property-search-forward
                               'face t
                               (lambda (_v pv)
                                 (and (listp pv)
                                      (eq 'separator-line
                                          (plist-get pv :inherit))
                                      (plist-get pv :extend)))))
                        (point))))
        (goto-char (point-max))
        (unless (bobp)
          (insert (make-separator-line)))
        (insert (rcirc-facify
                 (format-time-string
                  (string-trim-right rcirc-time-format)
                  (current-time))
                 'rcirc-timestamp)
                ": ")
        (let ((link-beg (point)))
          (insert-button
           (format "%s mentioned in %s."
                   (cond
                    ((and (memq 'nick types)
                          (memq 'keyword types))
                     "Nick and keyword")
                    ((memq 'nick types) "Nick")
                    ((memq 'keyword types) "Keyword")
                    ;; Well, this should not happen...
                    (t "Something"))
                   (buffer-name (marker-buffer activity-marker)))
           'action action
           'follow-link t)
          (insert "\n" msg "\n")
          (goto-char (or orig-pos link-beg)))))))

(defun rcirc-mentions-switch-to-mentions-buffer ()
  "Switch to the mentions buffer if it exists.
See `rcirc-mentions-log-mode' and `rcirc-mentions-buffer-name'."
  (interactive)
  (if-let* ((buf (get-buffer rcirc-mentions-buffer-name)))
      (if-let* ((win (get-buffer-window buf)))
          (select-window win)
        (switch-to-buffer buf))
    (message "No mentions so far...")))

(provide 'rcirc-mentions)
;;; rcirc-mentions.el ends here

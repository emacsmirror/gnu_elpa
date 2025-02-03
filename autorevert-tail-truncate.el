;;; autorevert-tail-truncate.el -- Summary;: -*- mode: emacs-lisp; lexical-binding: t; -*-

;; Copyright (C) 2025 Stephane Marks
;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Stephane Marks <shipmints@gmail.com>
;; Maintainer: Stephane Marks <shipmints@gmail.com>
;; Url: https://github.com/shipmints/autorevert-tail-truncate.el
;; Created: 2025-02-03
;; Version: 1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: convenience, tools, log files, autorevert

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; `auto-revert-tail-truncate-mode' extends `auto-revert-tail-mode' to
;; automate truncating the tailed buffer to a user-specified number of
;; lines.  This allows you, for example, to tail log files in an
;; auto-reverting buffer forever without running out of memory.  By
;; default, a newly tailed buffer is immediately truncated for the
;; same reason.  Also, by default, the buffer's undo log is disabled
;; along with font-lock to both preserve memory and optimize CPU
;; consumption.
;;
;; Use the command auto-revert-tail-truncate-file to open a file in a
;; new buffer with `auto-revert-tail-truncate-mode' enabled.
;;
;; Add a function to `auto-revert-tail-truncate-mode-hook' to control
;; additional features in your tailed buffers; e.g., truncate-lines,
;; controlling so-long-mode threshold, disabling spell checking, or
;; enabling other minor modes for specific log-file formats (the
;; visual features of which may require you to enable font-lock for
;; those buffers).
;;
;; If you want this mode enabled for all *.log files, run the command
;; `auto-revert-tail-mode-on-log-files'.

;;; Code:

(require 'autorevert)

(defcustom auto-revert-tail-truncate-max-lines 5000
  "Truncate buffer to this maximum number of lines.
If nil, truncation is disabled.  Bind this as a buffer-local variable to
control the maximum number lines to retain for specific buffers."
  :group 'auto-revert
  :type 'natnum)

(defcustom auto-revert-tail-truncate-file-size-hint 64
  "Bytes per line hint to `auto-revert-tail-truncate-file'.
This is multiplied by `auto-revert-tail-truncate-max-lines' to compute
the initial bytes to load."
  :group 'auto-revert
  :type 'natnum)

(defcustom auto-revert-tail-truncate-revert-interval auto-revert-interval
  "Number of seconds between Auto-Revert Tail Truncate Mode file checks.
The default value is `auto-revert-interval', which see."
  :group 'auto-revert
  :type 'natnum)

(defcustom auto-revert-tail-truncate-verbose auto-revert-verbose
  "When nil, `auto-revert-tail-truncate-mode' does not generate messages.
When non-nil, a message is generated whenever a buffer is reverted.
The default value is `auto-revert-verbose', which see."
  :group 'auto-revert
  :type 'boolean)

(defcustom auto-revert-tail-truncate-immediately t
  "If non-nil, buffer is truncated when this mode is enabled.
Set the maximum number of lines to retain in the buffer using the
option `auto-revert-tail-truncate-max-lines', which see."
  :group 'auto-revert
  :type 'boolean)

(defcustom auto-revert-tail-truncate-read-only t
  "If non-nil, the tailed buffer is set to be read only."
  :group 'auto-revert
  :type 'boolean)

(defcustom auto-revert-tail-truncate-disable-undo t
  "If non-nil, the buffer's undo log is disabled to save memory."
  :group 'auto-revert
  :type 'boolean)

(defcustom auto-revert-tail-truncate-disable-font-lock t
  "If non-nil, font-lock is disabled on the tailed buffer."
  :group 'auto-revert
  :type 'boolean)

(defcustom auto-revert-tail-truncate-check-vc-info auto-revert-check-vc-info
  "If non-nil `auto-revert-tail-truncate-mode' updates vc info.
The default value is `auto-revert-check-vc-info', which see."
  :group 'auto-revert
  :type 'boolean)

(defcustom auto-revert-tail-truncate-mode-text " TailTrunc"
  "Mode line string when `auto-revert-tail-truncate-mode' is active."
  :group 'auto-revert
  :type 'string)

(defvar-local auto-revert-tail-truncate-mode nil
  "Non-nil when `auto-revert-tail-truncate-mode' is active.
Never set this variable directly, use the command
`auto-revert-tail-truncate-mode' instead.")
(put 'auto-revert-tail-truncate-mode 'permanent-local t)

;;;###autoload
(define-minor-mode auto-revert-tail-truncate-mode
  "`auto-revert-tail-mode' with automatic buffer line truncation.
Truncation does not affect the content of underlying file, only the
buffer in which it is displayed.

Customize `auto-revert-tail-mode-hook' to control features such as
`truncate-lines' for the tailed buffer."
  :group 'find-file
  :lighter auto-revert-tail-truncate-mode-text
  (if auto-revert-tail-truncate-mode
      (progn
        (setq-local auto-revert-tail-mode-text nil) ; this mode's lighter has priority
        (auto-revert-tail-mode)
        (make-local-variable 'auto-revert-interval)
        (make-local-variable 'auto-revert-timer)
        (setopt auto-revert-interval auto-revert-tail-truncate-revert-interval)
        (setq-local auto-revert-verbose auto-revert-tail-truncate-verbose)
        (when auto-revert-tail-truncate-read-only
          (read-only-mode))
        (when auto-revert-tail-truncate-disable-undo
          (buffer-disable-undo))
        (when auto-revert-tail-truncate-disable-font-lock
          (font-lock-mode -1))
        (make-local-variable 'auto-revert-check-vc-info)
        (setq auto-revert-check-vc-info auto-revert-tail-truncate-check-vc-info)
        (goto-char (point-max))
        (when auto-revert-tail-truncate-immediately
          (auto-revert--tail-truncate))
        (add-hook 'after-revert-hook #'auto-revert--tail-truncate nil 'local))
    (remove-hook 'after-revert-hook #'auto-revert--tail-truncate 'local)
    (auto-revert-tail-mode -1)))

(defun turn-on-auto-revert-tail-truncate-mode ()
  "Turn on `auto-revert-tail-truncate-mode'.

This function is designed to be added to hooks, for example:
\(add-hook \\='my-logfile-mode-hook #\\='turn-on-auto-revert-tail-truncate-mode)"
  (auto-revert-tail-truncate-mode 1))

(defun auto-revert--tail-truncate ()
  "Called via buffer local `after-revert-hook' to truncate the buffer."
  (when auto-revert-tail-truncate-max-lines
    (with-silent-modifications
      (save-restriction
        (widen)
        (save-excursion
          (goto-char (point-max))
          (forward-line (- auto-revert-tail-truncate-max-lines))
          (beginning-of-line)
          (let ((inhibit-read-only t))
            (delete-region (point-min) (point))))))))

;;;###autoload
(defun auto-revert-tail-truncate-file (filename)
  "Prompt for a file to tail using `auto-revert-tail-truncate-mode'.
Invoke programmatically specifying FILENAME."
  (interactive "fFile to auto revert tail (with truncate): ")
  (let* ((filename (abbreviate-file-name (expand-file-name filename)))
         (max-size (* auto-revert-tail-truncate-max-lines
                      auto-revert-tail-truncate-file-size-hint))
         (size (file-attribute-size (file-attributes filename)))
         (beg (if (< size max-size) 0 (- size max-size)))
         (end size)
         (existing-buf (get-file-buffer filename))
         (buf))
    (when existing-buf
      (when (y-or-n-p "Use and truncate the existing buffer (\"no\" makes a new buffer)?")
        (setq buf existing-buf)))
    (unless buf
      (setq buf (create-file-buffer filename))
      (with-current-buffer buf
        (with-silent-modifications
          (insert-file-contents filename nil beg end)
          (set-visited-file-name filename nil t)
          (set-buffer-modified-p nil)))) ; also silences auto-revert-tail-mode "Buffer is modified..." warning
    (with-current-buffer buf
      (auto-revert-tail-truncate-mode))
    (pop-to-buffer buf)))

;;;###autoload
(defun auto-revert-tail-mode-on-log-files ()
  "Enable `auto-revert-tail-truncate-mode' for all *.log files."
  (interactive)
  (add-to-list 'auto-mode-alist '("\\.log\\'" . auto-revert-tail-truncate-mode)))

(provide 'autorevert-tail-truncate)

;;; autorevert-tail-truncate.el ends here

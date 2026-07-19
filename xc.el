;;; xc.el --- Emacs interface for xc  -*- lexical-binding: t; -*-

;; Copyright © 2026  Free Software Foundation, Inc.

;; Author: Ryan Prior <rprior@protonmail.com>
;; URL: https://codeberg.org/ryanprior/xc.el
;; Package-Version: 20260719.1
;; Package-Requires: ((emacs "28.1")(compat "29"))
;; Keywords: processes docs
;; Version: 1.11

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:
;; An Emacs interface to the xc task-runner, <https://xcfile.dev/>
;; Use `xc' to launch a task defined in your README

;; The common use case for `xc' is to run build, test, deploy type commands
;; that are documented in your README. Putting the documented steps on
;; the ``hot path'' ensures they don't get out-of-date and aids in
;; discoverability. A unique feature of `xc' is that it supports both
;; Markdown and org-mode READMEs.

;;; Code:

(require 'compat)
(require 'project)
(require 'subr-x)
(require 'term)

(defgroup xc nil
  "An Emacs interface for xc."
  :prefix "xc-"
  :group 'tools
  :link '(url-link :tag "Codeberg" "https://codeberg.org/ryanprior/xc.el"))

(defcustom xc-executable "xc"
  "Use this executable to run xc.
A string containing the file name of the executable."
  :type 'string)

(defvar-keymap xc-term-finished-mode-map
  "q" #'quit-window)

(define-minor-mode xc-term-finished-mode
  "Mode for xc tasks that have finished."
  :keymap xc-term-finished-mode-map)

(defcustom xc-type ""
  "Look for xc tasks in this type of documentation.

Either `org' or `md', or empty for default."
  :type 'string)

(defmacro xc--if-macro-fboundp (name then &rest else)
  "Execute THEN if macro NAME is bound and ELSE otherwise.
Essentially,

  (xc--if-macro-fboundp name then else...)

is equivalent to

  (if (fboundp \\='name) then else...)

but takes care of byte-compilation issues where the byte-code for the
latter could signal an error if it has been compiled with Emacs 24.1 and
is then later run by Emacs 24.5."
  (declare (indent 2) (debug (symbolp form &rest form)))
  (if (fboundp name)      ;If macro exists at compile-time, just use it.
      then
    `(if (fboundp ',name) ;Else, check if it exists at run-time.
         (eval ',then)    ;If it does, run the "then" code.
       ,@else)))

(defun xc--executable ()
  "Return non-nil when xc executable is found, or show a warning."
  (if-let* ((exec (executable-find xc-executable)))
      exec
    (if (string-equal xc-executable "xc")
        (display-warning 'xc "could not find `xc' on your PATH. Install it, adjust your PATH, or customize `xc-executable' to point to it.")
      (display-warning 'xc (format "no such file \"%s\" (from custom `xc-executable' value.)" xc-executable)))
    nil))

(defun xc--run (args)
  "Run xc with provided ARGS; nil if it fails."
  (with-temp-buffer
    (when-let* ((xc (xc--executable))
                (exit-code (apply #'call-process
                                  xc nil (current-buffer) t args)))
      (if (zerop exit-code)
          (buffer-string)
        nil))))

(defun xc--tasks (&optional file)
  "Get available tasks using xc.

If FILE is provided, look for tasks in that file specifically."
  (when-let* ((xc-output (xc--run
                          `("-short"
                            ,@(and (not (string-empty-p xc-type))
                                   (list "-type" xc-type))
                            ,@(and file (list "-file" file))))))
    (split-string xc-output)))

(defun xc--set-process-sentinel (buffer)
  "Put BUFFER in `xc-term-finished-mode' when its process exits."
  (let* ((proc (get-buffer-process buffer))
         (old-sentinel (process-sentinel proc)))
    (set-process-sentinel
     proc
     (lambda (proc event)
       (when old-sentinel
         (funcall old-sentinel proc event))
       (when (memq (process-status proc) '(exit signal))
         (with-current-buffer buffer
           (xc-term-finished-mode 1)))))))

;;;###autoload
(defun xc (task)
  "Run TASK using `xc'."
  (interactive (if-let* ((tasks (xc--tasks)))
                   (list (completing-read "Task: " tasks nil t))
                 (progn
                   (thread-last
                     (if (project-current)
                         (project-root (project-current))
                       default-directory)
                     (format "No tasks for %s")
                     (display-warning 'xc)
                     (pop-to-buffer "*Warnings*"))
                   '(nil))))
  (when task
    (let ((proc-buffer (apply #'make-term (concat "xc-" task)
                              (xc--executable)
                              nil
                              `(,@(and (not (string-empty-p xc-type))
                                       (list "-type" xc-type))
                                ,task))))
      (xc--set-process-sentinel proc-buffer)
      (pop-to-buffer proc-buffer)
      (setq-local term-scroll-to-bottom-on-output 'this)
      (term-char-mode))))

(provide 'xc)

;;; xc.el ends here

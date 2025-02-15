;;; calibre-exec.el --- Execute operations on a Library  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Free Software Foundation, Inc.

;; This file is part of calibre.el.

;; calibre.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; calibre.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with calibre.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; This file contains the infrastructure to execute operations, such
;; as adding, modifying, or removing books, on the active library.

;;; Code:
(require 'calibre-core)

(defconst calibre-exec--process-buffer "*calibre*"
  "The name of the buffer containing output from processes.")

(defvar calibre-exec--commands nil
  "A list of commands queued for execution.")

(defvar calibre-exec--executing nil
  "A boolean indicating whether execution of commands is ongoing.")

(defun calibre-exec--process-sentinel (_ event)
  "Process filter for Calibre library operations.
EVENT is the process event, see Info node
`(elisp)Sentinels'"
  (if (string= event "finished\n")
      (progn
        (kill-buffer calibre-exec--process-buffer)
        (calibre-library--refresh t))
    (error "Calibre process failed.  See %s for details" calibre-exec--process-buffer))
  (if calibre-exec--commands
      (calibre-exec--next-command)
    (setf calibre-exec--executing nil)))

(cl-defun calibre-exec--execute (args &optional (sentinel #'calibre-exec--process-sentinel))
  "Execute calibredb with arguments ARGS.
ARGS should be a list of strings to be passed as arguments to
calibredb.  SENTINEL is a process sentinel to install."
  (unless (executable-find calibre-calibredb-executable)
    (error "Could not find calibredb"))
  (let ((buffer (get-buffer-create calibre-exec--process-buffer))
        (command `(,calibre-calibredb-executable "--with-library" ,(calibre--library) ,@args)))
    (with-current-buffer buffer
      (insert (string-join command " ") "\n"))
    (make-process
     :name "calibre"
     :command command
     :buffer buffer
     :sentinel sentinel)))

(defun calibre-exec--next-command ()
  "Execute the next command in Calibre command if any.
Commands are stored in `calibre-exec--commands'"
  (when calibre-exec--commands
    (let ((command (car calibre-exec--commands)))
      (setf calibre-exec--commands (cdr calibre-exec--commands))
      (calibre-exec--execute command))))

(defun calibre-exec--queue-commands (commands)
  "Queue each calibredb command in COMMANDS for execution.
COMMANDS should be a list of lists that are valid arguments to
`calibre-exec--execute'."
  (setf calibre-exec--commands (append commands calibre-exec--commands)))

(defun calibre-exec--queue-command (command)
  "Queue COMMAND for execution.
COMMAND should be a valid argument to `calibre-exec--execute'."
  (calibre-exec--queue-commands (list command)))

(defun calibre-exec--start-execution ()
  "Start executing any commands queued."
  (unless calibre-exec--executing
    (setf calibre-exec--executing t)
    (calibre-exec--next-command)))

(provide 'calibre-exec)
;;; calibre-exec.el ends here

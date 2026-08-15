;;; flymake-harper.el --- Flymake backend for harper -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2026  Free Software Foundation, Inc.

;; Author: Manuel Uberti <manuel.uberti@inventati.org>,
;;         Philip Kaludercic <philipk@posteo.net>
;; Maintainer: Philip Kaludercic <philipk@posteo.net>
;; Version: 0.1.0
;; Keywords: convenience
;; Package-Requires: ((emacs "27.1"))
;; URL: https://codeberg.org/pkal/flymake-harper.el

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package adds support for harper (https://writewithharper.com/)
;; for Flymake.  This package is a fork of Manuel Uberti's
;; "flymake-proselint".  Once installed, the backend can be enabled
;; with by calling `flymake-harper-setup' manually or using a hook.
;; Configure `flymake-harper-disable' to disable certain types of
;; warnings, if you are experiencing too many false positives.

;;; Code:

(eval-when-compile
  (require 'subr-x)
  (require 'pcase))
(require 'flymake)
(require 'xdg)

(defgroup flymake-harper ()
  "Flymake backend for harper."
  :prefix "flymake-harper-"
  :link '(url-link "https://writewithharper.com/")
  :group 'flymake)

(defcustom flymake-harper-executable "harper-cli"
  "Name of the Harper executable.
Note that this is not the name of the LSP server, but the standalone CLI
program."
  :type 'string)

(defcustom flymake-harper-message-format
  "%m%s"
  "A format string to generate diagnostic messages.
The following %-sequences are replaced:

  %m - the message text
  %s - suggestions, in parentheses preceded by a space
  %c - the error code
  %p - numerical priority"
  :type 'string)

(defconst flymake-harper--options
  (eval-when-compile
    (ignore-errors
      ;; FIXME: If harper is not installed, this will be nil until the
      ;; package is re-compiled.
      (with-temp-buffer
        (call-process flymake-harper-executable nil t nil "config")
        (goto-char (point-min))
        (let ((config (json-parse-buffer :object-type 'alist)))
          (mapcar #'car config)))))
  "List of Harper options.")

(defconst flymake-harper--custom-type
  `(set :greedy t
        ,@(mapcar
           (lambda (opt)
             ;; TODO: Add a :tag
             `(const ,opt))
           flymake-harper--options))
  "Custom option type for harper configurations.")

(defun flymake-harper-safe-option-p (val)
  "Check if VAL is a safe (and valid) local value."
  (and (listp val)
       (catch 'fail
         (dolist (elem val)
           (unless (memq elem flymake-harper--options)
             (throw 'fail nil)))
         t)))

(defcustom flymake-harper-disable '(Dashes)
  "List of Harper options to disable.
See `flymake-harper--options' for a list of possible options.  This can
also be a buffer local variable that you can configure using file local
variables."
  :safe #'flymake-harper-safe-option-p
  :type flymake-harper--custom-type)

(defun flymake-harper-sentinel-1 (source data)
  "Handle a successfully parsed DATA from SOURCE.
DATA is a list of error diagnostics that are converted into
Flymake diagnostic objects."
  (let (diags)
    (dolist (lint data)
      (let ((rule (plist-get lint :rule))
            (span (plist-get lint :span)))
        (unless (memq rule flymake-harper-disable)
          (push (flymake-make-diagnostic
                 source
                 (1+ (plist-get span :char_start))
                 (1+ (plist-get span :char_end))
                 (pcase (plist-get lint :priority)
                   ;; FIXME: I am not sure if these are the actual
                   ;; thresholds, we should ask if around.
                   ((pred (< 50)) :error)
                   ((pred (< 25)) :warning)
                   (_ :note))
                 (format-spec
                  flymake-harper-message-format
                  `((?m . ,(plist-get lint :message))
                    (?c . ,(plist-get lint :rule))
                    (?s . ,(and-let* ((sug (plist-get lint :suggestions)))
                             (concat " (" (string-join sug ", ") ")")))
                    (?p . ,(plist-get lint :priority)))))
                diags))))
    diags))

(defvar-local flymake-harper--flymake-proc nil)

(defun flymake-harper-make-sentinel (source report-fn)
  "Create a sentinel on the buffer SOURCE that will call REPORT-FN."
  (lambda (proc _even)
    "Sentinel on PROC for handling Harper response.
 A successfully parsed message is passed onto the function
 `flymake-harper-sentinel-1' for further handling."
    (pcase (process-status proc)
      ('exit
       (unwind-protect
           (when (buffer-live-p (current-buffer))
             (with-current-buffer (process-buffer proc)
               (goto-char (point-min))
               (cond
                ((with-current-buffer source
                   (not (eq proc flymake-harper--flymake-proc)))
                 (flymake-log :warning "Canceling obsolete check %s" proc))
                ((= (point-max) (point-min))
                 (flymake-log :debug "Empty response"))
                ((condition-case err
                     (let ((response (json-parse-buffer :object-type 'plist
                                                        :array-type 'list)))
                       (thread-last
                         (plist-get (car response) :lints)
                         (flymake-harper-sentinel-1 source)
                         (funcall report-fn)))
                   (json-parse-error
                    (flymake-log :error "Invalid response: %S" err)))))))
         (with-current-buffer source
           (setq flymake-harper--flymake-proc nil))
         (kill-buffer (process-buffer proc))))
      ('signal (kill-buffer (process-buffer proc))))))

(defun flymake-harper-backend (report-fn &rest _args)
  "Flymake backend for Harper.
REPORT-FN is the flymake reporter function.  See the Info
node `(flymake) Backend functions' for more details."
  (unless (executable-find flymake-harper-executable)
    (user-error "Executable harper not found in $PATH"))

  (when (process-live-p flymake-harper--flymake-proc)
    (kill-process flymake-harper--flymake-proc))

  (let ((proc (make-process
               :name "harper-flymake" :noquery t :connection-type 'pipe
               :buffer (generate-new-buffer " *harper-flymake*")
               :command
               (list flymake-harper-executable "lint" "--format=json")
               :sentinel (flymake-harper-make-sentinel (current-buffer) report-fn)
               :stderr (get-buffer-create " *harper-flymake-stderr*"))))
    (setq flymake-harper--flymake-proc proc)
    (save-restriction
      (widen)
      (process-send-region proc (point-min) (point-max))
      (process-send-eof proc))))

(defun flymake-harper-setup ()
  "Set up the Harper as a Flymake backend in this buffer.
The Flymake minor mode will be enabled if it is not already enabled.."
  (interactive)
  (unless flymake-mode
    (flymake-mode t))
  (add-hook 'flymake-diagnostic-functions #'flymake-harper-backend nil t))

(provide 'flymake-harper)
;;; flymake-harper.el ends here

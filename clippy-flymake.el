;;; clippy-flymake.el --- Flymake backend for Clippy  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Michael Kirkland

;; Author: Michael Kirkland <mak.kirkland@proton.me>
;; Keywords: languages tools
;; Version: 1.0.0
;; Package-Requires: ((emacs "27"))

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

;; Flymake backend for Clippy (https://doc.rust-lang.org/clippy/), a
;; linter for the Rust programming language.
;;
;; Based on "An annotated example backend" in the Flymake docs:
;; https://www.gnu.org/software/emacs/manual/html_mono/flymake.html

;;; Code:

(require 'cl-lib)

(defgroup clippy-flymake nil
  "Flymake backend for Clippy."
  :group 'programming)

(defvar-local clippy-flymake--proc nil
  "Bound to cargo clippy process during it's execution.")

(defun clippy-flymake-setup ()
  "Enable Clippy Flymake diagnostics in the current buffer.

This function adds `clippy-flymake-backend' to
`flymake-diagnostic-functions', allowing Flymake to use Clippy
for Rust linting in the current buffer."
  (add-hook 'flymake-diagnostic-functions #'clippy-flymake-backend nil t))

(defun clippy-flymake-backend (report-fn &rest _args)
  "A standalone Flymake backend for Clippy.

For details on REPORT-FN, see `flymake-diagnostic-functions'."
  (unless (executable-find "cargo")
    (error "Cannot find cargo"))
  ;; If process is still running from the last check, kill it
  (when (process-live-p clippy-flymake--proc)
    (kill-process clippy-flymake--proc))
  (let* ((source-buffer (current-buffer))
         (filename (buffer-file-name source-buffer)))
    (save-restriction
      (widen)
      (setq
       clippy-flymake--proc
       (make-process
        :name "clippy-flymake" :noquery t :connection-type 'pipe
        :buffer (generate-new-buffer " *clippy-flymake*")
        :command '("cargo" "clippy" "--message-format=json")
        :sentinel
        (lambda (proc _event)
          ;; Check the process has indeed exited, as it might be
          ;; simply suspended
          (when (memq (process-status proc) '(exit signal))
            (unwind-protect
                ;; Only proceed if registered process is current process
                ;; (maybe a new call has been made since)
                (if (eq proc clippy-flymake--proc)
                    (with-current-buffer (process-buffer proc)
                      (goto-char (point-min))
                      (let (diagnostics)
                        ;; Parse the JSON output and process diagnostics
                        (while (re-search-forward "^{.*}$" nil t)
                          (let* ((json (json-parse-string (match-string 0)
                                                          :object-type 'alist))
                                 (diagnostic (clippy-flymake--parse-diagnostic
                                              json
                                              source-buffer)))
                            (when diagnostic
                              (cl-destructuring-bind (beg end type text)
                                  diagnostic
                                (push (flymake-make-diagnostic source-buffer
                                                               beg
                                                               end
                                                               type
                                                               text)
                                      diagnostics)))))
                        (funcall report-fn diagnostics)))
                  (flymake-log :warning "Cancelling obsolete check %s" proc))
              ;; Cleanup temporary buffer
              (kill-buffer (process-buffer proc))))))))))

(defun clippy-flymake--parse-diagnostic (json source-buffer)
  "Parse JSON diagnostic and return a LIST.

LIST contains ordered args required by FLYMAKE-MAKE-DIAGNOSTIC.

SOURCE-BUFFER is needed to find the buffer points corresponding
to the reported line and column numbers"
  (let* ((diagnostic (cdr (assq 'message json)))
         (message    (cdr (assq 'message diagnostic)))
         (level      (cdr (assq 'level   diagnostic)))
         (spans      (cdr (assq 'spans diagnostic)))
         (spans (when (> (length spans) 0)
                  (aref spans 0))))
    (when (and message spans (not (string= level "note")))
      (let* ((start-line (alist-get 'line_start   spans))
             (start-col  (alist-get 'column_start spans))
             (end-line   (alist-get 'line_end     spans))
             (end-col    (alist-get 'column_end   spans))
             (message    (concat level ": " message))
             (message    (clippy-flymake--include-help diagnostic message))
             (type (pcase level
                     ("error"   :error)
                     ("warning" :warning)))
             (beg (with-current-buffer source-buffer
                    (clippy-flymake-line-col-buffer-position start-line
                                                             start-col)))
             (end (with-current-buffer source-buffer
                    (clippy-flymake-line-col-buffer-position end-line
                                                             end-col))))
        (list beg end type message)))))

(defun clippy-flymake--include-help (diagnostic message)
  "Concatenate MESSAGE with help tips extracted from DIAGNOSTIC."
  (cl-loop
   for child across (cdr (assq 'children diagnostic))
   for msg     = (cdr (assq 'message child))
   for level   = (cdr (assq 'level   child))
   for spans   = (cdr (assq 'spans   child))
   for spans   = (when (> (length spans) 0)
                   (aref spans 0))
   for replace = (cdr (assq 'suggested_replacement spans))
   ;; When help messages don't span text, there's nothing to overlay
   when (and spans (string= level "help"))
   do (setq message
            ;; Include the help message
            (concat message
                    "\nhelp: "
                    msg
                    ;; Include suggested replacement
                    (when replace
                      (format ": %s" replace)))))
  message)

(defun clippy-flymake-line-col-buffer-position (line column)
  "Return the position in the current buffer at LINE and COLUMN."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (forward-line   (1- line))
      (move-to-column (1- column))
      (point))))

(provide 'clippy-flymake)

;;; clippy-flymake.el ends here

;;; readline-completion.el --- CAPF based on GNU Readline  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Free Software Foundation, Inc.

;; Author: Augusto Stoffel <arstoffel@gmail.com>
;; Keywords: convenience, processes

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

;; Usage:
;;
;; This should only be used on a comint buffer running a GNU Readline
;; based program.  To enable, call `readline-completion-mode'
;; interactively or add it the the relevant mode hook.
;;
;; Specifically for M-x shell running Bash, you should add the
;; following to your configuration:
;;
;;   (add-hook 'shell-mode-hook #'readline-completion-mode)
;;   (setq explicit-bash-args '("-io" "emacs"))
;;
;; (The second line is needed because Bash "helpfully" disables line
;; editing when TERM=dumb.)
;;
;; Background:
;;
;; This uses the export-completions command introduced in GNU Readline
;; 8.3, released July 2025.  See manpage readline(3).  Some related
;; discussions:
;;
;; https://lists.gnu.org/archive/html/bug-readline/2023-09/msg00017.html
;; https://lists.gnu.org/archive/html/bug-readline/2023-10/msg00001.html
;; https://lists.gnu.org/archive/html/bug-readline/2024-11/msg00006.html
;;
;; This package should be pretty tolerant of ~/.inputrc customization,
;; as long as M-x is bound to `execute-named-command'.

;;; Code:

(defvar -execute-named-command "\ex"
  "Readline binding for execute-named-command.")

(defvar -bracketed-paste '("\e[200~" . "\e[201~")
  "Escape sequences to start and end a bracketed paste.")

(defvar -abort-timeout 0.05 ;TODO: should it be higher for remote processes?
  "Time in seconds to wait when aborting.
If the process stays this long without producing any output, we assume
it has finished processing the completion request and printed a new
prompt.")

(defvar-local readline-completion-candidate-filter nil
  "Function to filter or modify completion candidates.
If non-nil, should be a function called with the completion prefix and a
completion candidate.  The function should return nil or a possibly
modified candidate.")

(defun readline-completion-shell-filter (prefix cand)
  "Quote completion candidates from the shell.
To be used as the value of `readline-completion-candidate-filter'."
  (let ((n (length prefix)))
    (if (or (length= cand n)
            (string-prefix-p "--" cand)
            (memq (char-before (- (point) n)) '(?\" ?')))
        cand
      (concat prefix (shell-quote-argument (substring cand n) t)))))

(defun -get-canditates (input)
  (let ((proc (get-buffer-process (current-buffer))))
    (unless (process-live-p proc)
      (user-error "Buffer has no process"))
    (with-temp-buffer
      (unwind-protect
          (let ((buffer (current-buffer))
                count acc result)
            (add-function
             :override (process-filter proc)
             (lambda (_proc string)
               (with-current-buffer buffer
                 (save-excursion
                   (goto-char (point-max))
                   (insert string))
                 ;(message "readline-completion:\n%s\n---" (buffer-string))
                 (when (and (not count)
                            (re-search-forward
                             ;; Parse output of export-completions as
                             ;; per readline(3).  Depending on TTY
                             ;; settings we may receive \r\n for
                             ;; newlines.
                             (rx point (* control)
                                 (group (+ digit)) (? "\r") "\n"
                                 (group (* (not control))) (? "\r") "\n"
                                 (group (+ digit)) ?: (group (+ digit))
                                 (? "\r") "\n")
                             nil t))
                   (setq count (string-to-number (match-string 1)))
                   (push (match-string 2) acc)
                   (push (string-to-number (match-string 3)) acc)
                   (push (string-to-number (match-string 4)) acc))
                 (when count
                   (while (and (plusp count) (not (eq (pos-eol) (point-max))))
                     (decf count)
                     (re-search-forward (rx point (* (not control))))
                     (push (match-string-no-properties 0) acc)
                     (forward-line))
                   (when (zerop count) (setq result (nreverse acc))))))
             '((name . readline-completion)))
            (process-send-string
             proc (concat
                   (car -bracketed-paste) input (cdr -bracketed-paste)
                   -execute-named-command "export-completions\n"
                   -execute-named-command "unix-line-discard\n"))
            (while-no-input
              (while (and (not result) (process-live-p proc))
                (accept-process-output proc)))
            (unless result
              ;; Now to the messy situation: clean up in case of
              ;; interruption.  We send ^C and must discard all output
              ;; until (and including) the next prompt.  Normally we
              ;; will just receive a prompt, but there's a remote
              ;; chance (not so remote for remote processes) that the
              ;; whole completion results arrive now.
              (process-send-string proc "\C-c")
              (while (accept-process-output proc -abort-timeout)))
            result)
        (remove-function (process-filter proc) 'readline-completion)))))

(defun readline-completion-at-point (&optional interactive)
  "Get completions from the comint subprocess using GNU Readline.
Can be used as a member of `completion-at-point-functions' or called
interactively."
  (interactive '(t) comint-mode)
  (cond
   (interactive
    (let ((completion-at-point-functions '(readline-completion-at-point)))
      (completion-at-point)))
   ((eq (pos-eol) (point-max))
    (pcase-let*
        ((boi (comint-line-beginning-position))
         (input (buffer-substring-no-properties boi (point)))
         (`(,prefix ,start ,end . ,candidates) (-get-canditates input)))
      (when-let* ((filter readline-completion-candidate-filter))
        (setq candidates (delq nil (mapcar (lambda (cand)
                                             (funcall filter prefix cand))
                                           candidates))))
      (when candidates
        (list (+ boi start) (+ boi end) candidates))))))

(defvar shell--start-prog)              ;from shell.el

;;;###autoload
(define-minor-mode readline-completion-mode
  "Set up completion at point using Readline."
  :interactive (comint-mode)
  (cond
   (readline-completion-mode
    (when (equal shell--start-prog "bash") ;TODO: how about other shells?
      (setq readline-completion-candidate-filter
            #'readline-completion-shell-filter))
    (add-hook 'completion-at-point-functions
              #'readline-completion-at-point nil t))
   (t
    (remove-hook 'completion-at-point-functions
                 #'readline-completion-at-point t))))

(provide 'readline-completion)

;; Local Variables:
;; read-symbol-shorthands: (("-" . "readline-completion--"))
;; End:

;;; readline-completion.el ends here

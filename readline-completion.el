;;; readline-completion.el --- CAPF based on GNU Readline  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Free Software Foundation, Inc.

;; Author: Augusto Stoffel <arstoffel@gmail.com>
;; Keywords: convenience, processes
;; URL: https://codeberg.org/astoff/readline-completion.el
;; Package-Requires: ((emacs "29.1") (compat "31.1"))
;; Version: 0.1

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
;;
;; This package provides completions from GNU Readline.  It can be
;; used in a comint buffer, to get completions from the underlying
;; process (as you normally would in a regular terminal) or anywhere
;; else to get completions from Bash.
;;
;;;; Setup
;;
;; To enable the package in `shell-mode' and `read-shell-command', add
;; the following to your configuration:
;;
;;   (with-eval-after-load 'shell
;;     (add-hook 'shell-dynamic-complete-functions #'readline-completion-at-point))
;;
;; To enable it in any other comint mode which you know to be
;; compatible, add `readline-completion-at-point' to
;; `completion-at-point-functions' in a relevant mode hook.
;;
;; For testing, or if you prefer a dedicated keybinding for this
;; completion source, you can call `readline-completion-at-point'
;; interactively.
;;
;;;; Background
;;
;; This package uses the export-completions command introduced in GNU
;; Readline 8.3, released July 2025.  See manpage readline(3).  Some
;; related discussions:
;;
;; https://lists.gnu.org/archive/html/bug-readline/2023-09/msg00017.html
;; https://lists.gnu.org/archive/html/bug-readline/2023-10/msg00001.html
;; https://lists.gnu.org/archive/html/bug-readline/2024-11/msg00006.html
;;
;; This package should be pretty tolerant of ~/.inputrc customization,
;; as long as M-x is bound to `execute-named-command'.

;;; Code:

(require 'cl-lib)
(require 'compat)

(defgroup readline-completion nil
  "Completion from GNU Readline."
  :prefix "readline-completion-"
  :group 'convenience
  :group 'comint)

(defcustom readline-completion-process-ttl 60
  "Time, in seconds, to keep an idle shell process alive."
  :type 'natnum)

(defvar -shell-process nil
  "A shell process used to obtain completion candidates.")

(defvar-local -candidate-filter nil
  "Function to filter or modify completion candidates.
If non-nil, should be a function called with the completion prefix and a
list of completion candidates.  It should should return a new list of
candidates.")

(defvar -execute-named-command "\ex"
  "Readline binding for execute-named-command.")

(defvar -bracketed-paste '("\e[200~" . "\e[201~")
  "Escape sequences to start and end a bracketed paste.")

(defvar -abort-timeout 0.05 ;TODO: should it be higher for remote processes?
  "Time in seconds to wait when aborting.
If the process stays this long without producing any output, we assume
it has finished processing the completion request and printed a new
prompt.")

(defun -shell-candidate-filter (prefix cands)
  "Quote completion candidates from the shell.
To be used as the value of `readline-completion--candidate-filter'."
  (setq cands (delete "" cands))
  (let ((n (length prefix)))
    (if (memq (char-before (- (point) n)) '(?\" ?'))
        cands
      (mapcar (lambda (cand)
                (setq cand (string-trim-right cand))
                (if (or (length= cand n)
                        (string-prefix-p "--" cand))
                    cand
                  (concat prefix (shell-quote-argument (substring cand n) t))))
              cands))))

(defun -get-process ()
  "Return a process to query for completions."
  ;; TODO: make this customizable?
  (cond ((and (derived-mode-p 'comint-mode)
              (not (derived-mode-p 'shell-mode)))
         (when (eq (pos-eol) (point-max))
           (get-buffer-process (current-buffer))))
        (t
         (unless (and (process-live-p -shell-process)
                      (equal (process-get -shell-process '-directory)
                             default-directory))
           (when (process-live-p -shell-process)
             (delete-process -shell-process))
           (setq -shell-process
                 (make-process
                  :name "readline-completion"
                  :command '("bash" "-i")
                  :noquery t :connection-type 'pty))
           (process-put -shell-process '-directory default-directory)
           (process-put -shell-process '-candidate-filter
                        #'-shell-candidate-filter)
           (process-put -shell-process '-timer
                        (run-with-timer readline-completion-process-ttl nil
                                        #'delete-process -shell-process))
           (let ((nonce (format "%016x" (random most-positive-fixnum))))
             (process-send-string -shell-process (format "\
history -d -1; set -o emacs; PS1=%s\n" nonce))
             (with-temp-buffer
               (cl-letf (((process-buffer -shell-process) (current-buffer)))
                 (while (not (and (goto-char (point-min))
                                  (search-forward nonce nil t)))
                   (accept-process-output -shell-process))))))
         (timer-set-time (process-get -shell-process '-timer)
                         readline-completion-process-ttl)
         -shell-process)))

(defun -get-candidates (proc input)
  "Get completions for the given INPUT string from process PROC.
Returns a list of the form (PREFIX START END . CANDIDATES), where PREFIX
is the substring of INPUT being completed and START and END are the
corresponding positions."
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
               ;;(message "readline-completion:\n%s\n---" (buffer-string))
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
      (remove-function (process-filter proc) 'readline-completion))))

;;;###autoload
(defun readline-completion-at-point (&optional interactive)
  "Get completions from a subprocess using GNU Readline.
Can be used as a member of `completion-at-point-functions' or called
interactively."
  (interactive '(t))
  (if interactive
      (let ((completion-at-point-functions '(readline-completion-at-point)))
        (completion-at-point))
    (when-let* ((proc (-get-process))
                (_ (process-live-p proc)))
      (pcase-let* ((boi (constrain-to-field (pos-bol) (point)))
                   (input (buffer-substring-no-properties boi (point)))
                   (`(,prefix ,start ,end . ,candidates)
                    (-get-candidates proc input)))
        (when-let* ((filter (or (process-get proc '-candidate-filter)
                                -candidate-filter)))
          (setq candidates (funcall filter prefix candidates)))
        (when candidates
          (list (+ boi start) (+ boi end) candidates))))))

(provide 'readline-completion)

;; Local Variables:
;; read-symbol-shorthands: (("-" . "readline-completion--"))
;; End:

;;; readline-completion.el ends here

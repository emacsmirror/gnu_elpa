;;; futur-hacks.el --- Force feed Futur into Emacs  -*- lexical-binding: t -*-

;; Copyright (C) 2026  Free Software Foundation, Inc.

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

;; This collects changes to Emacs that should ideally eventually
;; be installed into Emacs proper, but makes it possible to use
;; those features in older Emacsen anyway.

;;; Code:

(require 'trace)
(trace-function 'flymake--log-1)
(trace-function 'futur--elisp-process-sentinel)
(trace-function 'futur--elisp-process-answer)
(trace-function 'futur--elisp-process-filter)
(trace-function 'futur--elisp-set-destination)
(trace-function 'elisp-flymake--byte-compile-done)

(require 'futur-client)
(require 'cl-lib)

(defun futur--safe-macroexpand-all (sexp)
  (futur-blocking-wait-to-get-result
   (let* ((totalctx (mapcar #'car load-history))
          (tail (cl-member-if (lambda (f) (string-match-p "/leim-list\\.el" f))
                              totalctx))
          (trimmedctx (take (- (length totalctx) (length tail)) totalctx)))
     (futur--sandbox-funcall
      (lambda ()
        (declare-function futur-reset-context "futur-server")
        (let ((ctx trimmedctx))
          (while (and ctx (member (car ctx) load-history))
            (setq ctx (pop ctx)))
          (when ctx ;; Some element from context is missing.
            (futur-reset-context
             'elisp-macroexpand (reverse trimmedctx))))
        (setq trusted-content :all) ;; We're in the sandbox!
        (if (fboundp 'elisp--safe-macroexpand-all) ;Emacs-30?
            (elisp--safe-macroexpand-all sexp)
          (macroexpand-all sexp)))))))

(defun futur--flymake-byte-compile (report-fn &rest _args)
  "A Flymake backend for elisp byte compilation.
Spawn an Emacs process that byte-compiles a file representing the
current buffer state and calls REPORT-FN when done."
  (when elisp-flymake--byte-compile-process
    (cond
     ((process-live-p elisp-flymake--byte-compile-process)
      (kill-process elisp-flymake--byte-compile-process))
     ((and (fboundp 'futur-p) (futur-p elisp-flymake--byte-compile-process))
      (futur-abort elisp-flymake--byte-compile-process 'restarting)
      (setq elisp-flymake--byte-compile-process nil))))
  (let ((temp-file (make-temp-file
                    (expand-file-name
                     "elisp-flymake-byte-compile"
                     futur-sandbox-temp-dir)))
        (source-buffer (current-buffer))
        (coding-system-for-write 'utf-8-unix)
        (coding-system-for-read 'utf-8))
    (save-restriction
      (widen)
      (write-region (point-min) (point-max) temp-file nil 'nomessage))
    (let* ((loadpath elisp-flymake-byte-compile-load-path)
           ;; Hack: suppress warning about missing lexical cookie in
           ;; *scratch* buffers.
           (inhibit-lcw (derived-mode-p 'lisp-interaction-mode))
           (proc-futur
            (futur--sandbox-funcall
             (lambda ()
               (declare-function futur-reset-context "futur-server")
               (futur-reset-context 'flymake
                                    `((funcall package-activate-all)
                                      elisp-mode bytecomp byte-opt))
               (when inhibit-lcw
                 (setq bytecomp--inhibit-lexical-cookie-warning t))
               (setq load-path (append loadpath load-path))
               (setq trusted-content :all) ;; We're in a sandbox after all!
               ;; FIXME: Change `elisp-flymake--batch-compile-for-flymake'
               ;; so as to skip this "print to buffer + read" step.
               (with-temp-buffer
                 (let ((standard-output (current-buffer)))
                   (elisp-flymake--batch-compile-for-flymake temp-file)
                   (goto-char (point-min))
                   (search-forward ":elisp-flymake-output-start")
                   (read (current-buffer))))))))
      (setq elisp-flymake--byte-compile-process proc-futur)
      (futur--unwind-protect
       (futur-bind
        proc-futur
        (lambda (collected)
          (when (get-buffer "*trace-output*")
            (trace-values :byte-compile-collected collected))
          (cond
           ((not (and (buffer-live-p source-buffer)
                      (eq proc-futur (with-current-buffer source-buffer
                                       elisp-flymake--byte-compile-process))))
            (when (get-buffer "*trace-output*")
              (trace-values :byte-compile-throw-away))
            (flymake-log :warning
                         "byte-compile process obsolete: %S" proc-futur))
           (t
            (unwind-protect
                ;; FIXME: Change `elisp-flymake--byte-compile-done'
                ;; so we don't need this "print to buffer + read" step.
                (with-temp-buffer
                  (prin1 `(:elisp-flymake-output-start ,collected)
                         (current-buffer))
                  (elisp-flymake--byte-compile-done report-fn
                                                    source-buffer
                                                    (current-buffer)))
              (setq elisp-flymake--byte-compile-process nil)))))
        (lambda (err)
          (funcall report-fn
                   :panic
                   :explanation
                   (format "byte-compile process failed: %S" err))))
       (lambda ()
         (delete-file temp-file))))))

;;;###autoload
(define-minor-mode futur-hacks-mode
  "Various hacks to force Futur into various corners of Emacs.
Concretely this means:
- Run untrusted code inside a sandboxed subprocess.
  This is used for:
  -  TAB completion in ELisp mode (where we need to
    macroexpand the code).
  - Flymake byte-compilation in ELisp mode."
  :global t
  (advice-remove 'elisp--safe-macroexpand-all #'futur--safe-macroexpand-all)
  (advice-remove 'elisp-flymake-byte-compile #'futur--flymake-byte-compile)
  (when futur-hacks-mode
    (advice-add 'elisp-flymake-byte-compile :override
                #'futur--flymake-byte-compile)
    ;; FIXME: This has no effect in Emacs<30 where this function doesn't exist
    ;; and its content is instead "hidden" inside `elisp--local-variables'.
    (advice-add 'elisp--safe-macroexpand-all :override
                #'futur--safe-macroexpand-all)
    ))

(provide 'futur-hacks)

;;; futur-hacks.el ends here

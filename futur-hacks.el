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

;; (require 'trace)
;; (trace-function 'flymake--log-1)
;; (trace-function 'futur--elisp-process-sentinel)
;; (trace-function 'futur--elisp-process-answer)
;; (trace-function 'futur--elisp-process-filter)
;; (trace-function 'futur--elisp-set-destination)
;; (trace-function 'elisp-flymake--byte-compile-done)

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

(defun futur--smerge-refine-regions (beg1 end1 beg2 end2 props-c &optional preproc props-r props-a)
  "Do it asynchronously."
  (defvar smerge-refine-ignore-whitespace)
  (defvar smerge-refine-weight-hack)
  (declare-function smerge--refine-prepare-regions "smerge-mode")
  (declare-function smerge--refine-apply-diff "smerge-mode")
  ;; FIXME: Rate-limit with `futur-concurrency-bound'.
  ;; FIXME: Check that the regions haven't changed/disappeared.
  (pcase-let*
      ;; Cover the two regions with one `smerge--refine-region' overlay each.
      ((ol1 (make-overlay beg1 end1 (if (markerp beg1) (marker-buffer beg1))
                          ;; Make it shrink rather than spread when editing.
                          'front-advance nil))
       (ol2 (make-overlay beg2 end2 (if (markerp beg2) (marker-buffer beg2))
                          ;; Make it shrink rather than spread when editing.
                          'front-advance nil))
       (`(,file1 ,file2)
        (smerge--refine-prepare-regions ol1 ol2 preproc)))
    (smerge--refine-set-overlay-props ol1 ol2 props-c props-r props-a)

    ;; Call diff on those files.
    (futur-with-temp-buffer
      (let ((buf (current-buffer)))
        (futur-let*
            ((_exitcode
              <- (futur-unwind-protect
                     ;; Allow decoding the EOL format, as on MS-Windows the
                     ;; Diff utility might produce CR-LF EOLs.
                     (let ((coding-system-for-read 'utf-8-emacs))
                       ;; (trace-values :file1 (file-attributes file1))
                       ;; (trace-values :file2 (file-attributes file2))
                       (futur-process-call
                        diff-command nil buf nil
                        (if (and smerge-refine-ignore-whitespace
                                 (not smerge-refine-weight-hack))
                            ;; Pass -a so diff treats it as a text file
                            ;; even if it contains \0 and such.
                            ;; Pass -d so as to get the smallest change, but
                            ;; also and more importantly because otherwise it
                            ;; may happen that diff doesn't behave like
                            ;; `smerge-refine-weight-hack' expects it to.
                            ;; See https://lists.gnu.org/r/emacs-devel/2007-11/msg00401.html
                            "-awd" "-ad")
                        file1 file2))
                   (delete-file file1)
                   (delete-file file2))))
          ;; (trace-values :process-exit exitcode)
          ;; Process diff's output.
          (when (and (overlay-buffer ol1) (overlay-buffer ol2))
            (smerge--refine-apply-diff buf ol1 ol2
                                       props-c props-r props-a)))))))

(with-eval-after-load 'smerge-mode
  (defvar smerge--refine-long-words)
  (defvar smerge-refine-ignore-whitespace)
  (defvar smerge-refine-weight-hack)
  (declare-function smerge--refine-highlight-change "smerge-mode")
  (declare-function smerge--refine-chopup-region "smerge-mode")
  (declare-function smerge--refine-apply-diff-1 "ext:here-or-smerge-mode")
  (declare-function smerge--refine-set-overlay-props "ext:here-or-smerge-mode")

  (unless (fboundp 'smerge--refine-set-overlay-props)
    (defun smerge--refine-set-overlay-props (ol1 ol2 props-c props-r props-a)
      (let ((common-props
             (let ((props '((evaporate . t) (smerge--refine-region . t))))
               (dolist (prop (or props-a props-c))
                 (when (and (not (memq (car prop) '(face font-lock-face)))
                            (member prop (or props-r props-c))
                            (or (not (and props-c props-a props-r))
                                (member prop props-c)))
                   ;; This PROP is shared among all those overlays.
                   ;; Better keep it also for the `smerge--refine-region'
                   ;; overlays, so the client package recognizes them as
                   ;; being part of the refinement (e.g. it will hopefully
                   ;; delete them like the others).
                   (push prop props)))
               props)))
        (dolist (prop common-props)
          (overlay-put ol1 (car prop) (cdr prop))
          (overlay-put ol2 (car prop) (cdr prop))))))

  (unless (fboundp 'smerge--refine-prepare-regions) ;; Emacs-31
    (defun smerge--refine-prepare-regions ( ol1 ol2 preproc)
      (let* ((file1 (make-temp-file "diff1"))
             (file2 (make-temp-file "diff2"))
             (smerge--refine-long-words
              (if smerge-refine-weight-hack (make-hash-table :test #'equal))))

        (let ((write-region-inhibit-fsync t)) ; Don't fsync temp files.
          ;; Chop up regions into smaller elements and save into files.
          (smerge--refine-chopup-region
           (with-current-buffer (overlay-buffer ol1)
             (copy-marker (overlay-start ol1)))
           (overlay-end ol1) file1 preproc)
          (smerge--refine-chopup-region
           (with-current-buffer (overlay-buffer ol2)
             (copy-marker (overlay-start ol2)))
           (overlay-end ol2) file2 preproc))

        `(,file1 ,file2))))

  (unless (fboundp 'smerge--refine-apply-diff) ;; Emacs-31
    (defun smerge--refine-apply-diff ( diffbuf ol1 ol2
                                       props-c props-r props-a)
      ;; `smerge--refine-apply-diff-1' isn't careful to preserve the
      ;; position of point, so do it here.
      (let ((pt1 (with-current-buffer (overlay-buffer ol1) (point)))
            (pt2 (with-current-buffer (overlay-buffer ol2) (point))))
        (unwind-protect
            (smerge--refine-apply-diff-1 diffbuf ol1 ol2
                                         props-c props-r props-a)
          (with-current-buffer (overlay-buffer ol1)
            (goto-char pt1)
            ;; Usually ol1 and ol2 are in the same buffer, so `set-buffer'
            ;; from ol1 to maximize the chance that it's a no-op.
            (with-current-buffer (overlay-buffer ol2) (goto-char pt2))))))

    (defun smerge--refine-apply-diff-1 ( diffbuf ol1 ol2
                                         props-c props-r props-a)
      (with-current-buffer diffbuf
        (goto-char (point-min))
        ;; (trace-values :starting1 (current-buffer) (buffer-size))
        (let ((last1 nil)
              (last2 nil)
              (beg1 (with-current-buffer (overlay-buffer ol1)
                     (copy-marker (overlay-start ol1))))
              (beg2 (with-current-buffer (overlay-buffer ol2)
                     (copy-marker (overlay-start ol2))))
              (end1 (overlay-end ol1))
              (end2 (overlay-end ol2)))
          (while (not (eobp))
            (if (not (looking-at "\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)?\\([acd]\\)\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)?$"))
                (error "Unexpected patch hunk header: %s"
                       (buffer-substring (point) (line-end-position))))
            (let ((op (char-after (match-beginning 3)))
                  (m1 (match-string 1))
                  (m2 (match-string 2))
                  (m4 (match-string 4))
                  (m5 (match-string 5)))
              (setq last1
                    (smerge--refine-highlight-change
		     beg1 m1 (if (eq op ?a) t m2)
		     ;; Try to use props-c only for changed chars,
		     ;; fallback to props-r for changed/removed chars,
		     ;; but if props-r is nil then fallback to props-c.
		     (or (and (eq op '?c) props-c) props-r props-c)))
              (setq last2
                    (smerge--refine-highlight-change
		     beg2 m4 (if (eq op ?d) t m5)
		     ;; Same logic as for removed chars above.
		     (or (and (eq op '?c) props-c) props-a props-c))))
            (overlay-put last1 'smerge--refine-other last2)
            (overlay-put last2 'smerge--refine-other last1)
            (forward-line 1)            ;Skip hunk header.
            (and (re-search-forward "^[0-9]" nil 'move) ;Skip hunk body.
                 (goto-char (match-beginning 0))))
          ;; (cl-assert (or (null last1) (< (overlay-start last1) end1)))
          ;; (cl-assert (or (null last2) (< (overlay-start last2) end2)))
          (if smerge-refine-weight-hack
              (progn
                ;; (cl-assert (or (null last1) (<= (overlay-end last1) end1)))
                ;; (cl-assert (or (null last2) (<= (overlay-end last2) end2)))
                )
            ;; smerge-refine-forward-function when calling in chopup may
            ;; have stopped because it bumped into EOB whereas in
            ;; smerge-refine-weight-hack it may go a bit further.
            (if (and last1 (> (overlay-end last1) end1))
                (move-overlay last1 (overlay-start last1) end1))
            (if (and last2 (> (overlay-end last2) end2))
                (move-overlay last2 (overlay-start last2) end2))
            ))))))

;;;###autoload
(define-minor-mode futur-hacks-mode
  "Various hacks to force Futur into various corners of Emacs.
Concretely this means:
- Run untrusted code inside a sandboxed subprocess.
  This is used for:
  -  TAB completion in ELisp mode (where we need to
    macroexpand the code).
  - Flymake byte-compilation in ELisp mode.
- Run `smerge-refine-region' asynchronously."
  :global t
  (advice-remove 'elisp--safe-macroexpand-all #'futur--safe-macroexpand-all)
  (advice-remove 'elisp-flymake-byte-compile #'futur--flymake-byte-compile)
  (advice-remove 'smerge-refine-regions #'futur--smerge-refine-regions)
  (when futur-hacks-mode
    (advice-add 'elisp-flymake-byte-compile :override
                #'futur--flymake-byte-compile)
    (advice-add 'smerge-refine-regions :override #'futur--smerge-refine-regions)
    ;; FIXME: This has no effect in Emacs<30 where this function doesn't exist
    ;; and its content is instead "hidden" inside `elisp--local-variables'.
    (advice-add 'elisp--safe-macroexpand-all :override
                #'futur--safe-macroexpand-all)
    ))

(provide 'futur-hacks)

;;; futur-hacks.el ends here

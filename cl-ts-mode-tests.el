;;; cl-ts-mode-tests.el --- tests for cl-ts-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zach shaftel

;; This file is not part of GNU Emacs.

(require 'cl-ts-mode)
(require 'ert)
(eval-when-compile (require 'subr-x))

(ert-deftest cl-ts-mode-activation ()
  (let* ((major-mode-remap-alist
          `((lisp-mode . cl-ts-mode) ,@major-mode-remap-alist))
         (auto-mode-alist
          `(("\\.lisp\\'" . lisp-mode) ,@auto-mode-alist)))
    (with-temp-buffer
      (delay-mode-hooks
        (set-visited-file-name (expand-file-name "dummy.lisp" temporary-file-directory) t))
      (set-buffer-modified-p nil)
      (should (equal major-mode 'cl-ts-mode))
      (should-not (null treesit-primary-parser))
      (should (equal (treesit-parser-language treesit-primary-parser) 'common-lisp))))
  (when (boundp 'treesit-major-mode-remap-alist)
    (should (equal (alist-get 'lisp-mode treesit-major-mode-remap-alist) 'cl-ts-mode))))

(defconst cl-ts-mode-tests--standard-format-query
  (thread-last (custom--standard-value 'cl-ts-format-support-mode-query)
    (cl-ts-mode--build-format-query)
    (treesit-query-compile 'common-lisp)))

(ert-deftest cl-ts-format-parser-embedding ()
  (let ((cl-ts-format-support-mode-query cl-ts-mode-tests--standard-format-query))
    (with-temp-buffer
      (insert "(format t (formatter \"~A ~S\"))")
      (delay-mode-hooks (cl-ts-mode))
      (cl-ts-format-support-mode)
      (treesit-update-ranges (point-min) (point-max))
      (should (length= (treesit-parser-list nil 'cl-format t) 1))
      (let* ((root (treesit-parser-root-node
                    (car (treesit-parser-list nil 'cl-format t))))
             (first-directive (treesit-node-child root 0)))
        (should-not (treesit-node-check root 'has-error))
        (should (equal (treesit-node-type first-directive) "format_directive"))))))

(defmacro cl-ts-mode-tests--with-temp-changes (&rest body)
  (declare (indent 0))
  (let ((s (make-symbol "change-group")))
    `(let ((undo-outer-limit nil)
           (undo-limit most-positive-fixnum)
           (undo-strong-limit most-positive-fixnum)
           (,s (prepare-change-group)))
       (unwind-protect
           (progn
             (activate-change-group ,s)
             (save-excursion
               ,@body))
         (cancel-change-group ,s)
         (treesit-update-ranges)))))

(ert-deftest cl-ts-format-indentation ()
  (let ((cl-ts-format-support-mode-query cl-ts-mode-tests--standard-format-query)
        (cl-ts-mode-format-indent-excluded-commands ())
        marker)
    (with-temp-buffer
      (insert "(format t (formatter \"~A~<\n")
      (setq marker (point-marker))
      (insert "~S\n~:>\"))")
      (goto-char marker)
      (delay-mode-hooks (cl-ts-mode))
      (cl-ts-format-support-mode)
      (treesit-update-ranges)
      (let ((opener-marker (copy-marker (1- (point))))
            (opener-column (save-excursion (backward-char 2) (current-column))))
        (let ((cl-ts-mode-format-group-indent-offset 1)
              (cl-ts-mode-format-indent-predicate "\\`format_group\\'")
              (cl-ts-mode-format-indent-auto-escape-eol "~@")
              correct-col)
          (cl-ts-mode-tests--with-temp-changes
            (save-excursion (call-interactively #'indent-for-tab-command))
            (should (equal (current-indentation)
                           (+ opener-column cl-ts-mode-format-group-indent-offset)))
            (setq correct-col (current-indentation))
            (goto-char opener-marker)
            (should (looking-at-p (rx "~@" eol))))
          (let ((cl-ts-mode-format-indent-auto-escape-eol nil)
                (start-col (current-column)))
            (cl-ts-mode-tests--with-temp-changes
              (save-excursion (call-interactively #'indent-for-tab-command))
              (should (equal (current-indentation) start-col))))
          (let ((cl-ts-mode-format-indent-auto-escape-eol t))
            (cl-ts-mode-tests--with-temp-changes
              (save-excursion (call-interactively #'indent-for-tab-command))
              ;; make sure we still indented
              (should (equal (current-indentation) correct-col))
              (goto-char opener-marker)
              ;; but didn't auto escape
              (should (eolp)))))))))

(provide 'cl-ts-mode-tests)
;;; cl-ts-mode-tests.el ends here

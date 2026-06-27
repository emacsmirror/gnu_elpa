;;; lisp-ts-mode-tests.el --- tests for lisp-ts-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: zach shaftel <zach@shaf.tel>

;; This program is free software: you can redistribute it and/or modify
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

;;; Code:

(require 'lisp-ts-mode)
(require 'ert)
(eval-when-compile (require 'subr-x))

(ert-deftest lisp-ts-mode-activation ()
  (let* ((major-mode-remap-alist
          `((lisp-mode . lisp-ts-mode) ,@major-mode-remap-alist))
         (auto-mode-alist
          `(("\\.lisp\\'" . lisp-mode) ,@auto-mode-alist)))
    (with-temp-buffer
      (delay-mode-hooks
        (set-visited-file-name (expand-file-name "dummy.lisp" temporary-file-directory) t))
      (set-buffer-modified-p nil)
      (should (equal major-mode 'lisp-ts-mode))
      (should-not (null treesit-primary-parser))
      (should (equal (treesit-parser-language treesit-primary-parser) 'common-lisp))))
  (when (boundp 'treesit-major-mode-remap-alist)
    (should (equal (alist-get 'lisp-mode treesit-major-mode-remap-alist) 'lisp-ts-mode))))

(defconst lisp-ts-mode-tests--standard-format-query
  (thread-last (custom--standard-value 'lisp-ts-format-support-mode-query)
    (lisp-ts-mode--build-format-query)
    (treesit-query-compile 'common-lisp)))

(ert-deftest lisp-ts-format-parser-embedding ()
  (let ((lisp-ts-format-support-mode-query lisp-ts-mode-tests--standard-format-query))
    (with-temp-buffer
      (insert "(format t (formatter \"~A ~S\"))")
      (delay-mode-hooks (lisp-ts-mode))
      (lisp-ts-format-support-mode)
      (treesit-update-ranges (point-min) (point-max))
      (should (length= (treesit-parser-list nil 'cl-format t) 1))
      (let* ((root (treesit-parser-root-node
                    (car (treesit-parser-list nil 'cl-format t))))
             (first-directive (treesit-node-child root 0 t)))
        (should-not (treesit-node-check root 'has-error))
        (should (equal (treesit-node-type first-directive) "format_directive"))))))

(defmacro lisp-ts-mode-tests--with-temp-changes (&rest body)
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

(ert-deftest lisp-ts-format-indentation ()
  (let ((lisp-ts-format-support-mode-query lisp-ts-mode-tests--standard-format-query)
        (lisp-ts-mode-format-indent-function #'lisp-ts-mode--indent-format-line)
        marker)
    (with-temp-buffer
      (insert "(format t (formatter \"~A~<\n")
      (setq marker (point-marker))
      (insert "~S\n~:>\"))")
      (goto-char marker)
      (delay-mode-hooks (lisp-ts-mode))
      (lisp-ts-format-support-mode)
      (treesit-update-ranges)
      ;; FIXME: there are plenty more things to test, eg. string relative
      ;; indentation, indentation of the end directive etc.
      (let ((opener-marker (copy-marker (1- (point))))
            (opener-column (save-excursion (backward-char 2) (current-column)))
            (lisp-ts-mode-format-group-indent-offset 1)
            (lisp-ts-mode-format-indent-predicate "\\`format_group\\'")
            (lisp-ts-mode-format-indent-auto-escape-eol "~@")
            correct-col)
        (lisp-ts-mode-tests--with-temp-changes
          (save-excursion (call-interactively #'indent-for-tab-command))
          (should (equal (current-indentation)
                         (+ opener-column lisp-ts-mode-format-group-indent-offset)))
          (setq correct-col (current-indentation))
          (goto-char opener-marker)
          (should (looking-at-p (rx "~@" eol))))
        (let ((lisp-ts-mode-format-indent-auto-escape-eol nil)
              (start-col (current-column)))
          (lisp-ts-mode-tests--with-temp-changes
            (save-excursion (call-interactively #'indent-for-tab-command))
            (should (equal (current-indentation) start-col))))
        (let ((lisp-ts-mode-format-indent-auto-escape-eol t))
          (lisp-ts-mode-tests--with-temp-changes
            (save-excursion (call-interactively #'indent-for-tab-command))
            ;; make sure we still indented
            (should (equal (current-indentation) correct-col))
            (goto-char opener-marker)
            ;; but didn't auto escape
            (should (eolp))))))))

(provide 'lisp-ts-mode-tests)
;;; lisp-ts-mode-tests.el ends here

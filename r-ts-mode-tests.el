;;; r-ts-mode-tests.el --- ERT tests for r-ts-mode  -*- lexical-binding: t; -*-
;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Manuel Teodoro <ttm@teoten.me>
;; URL: https://codeberg.org/R-for-emacs/r-ts-mode
;; Package-Requires: ((emacs "30.1"))

;;; Commentary:
;; Minimal structure and placeholder for `r-ts-mode' unit tests using Emacs ERT
;; framework.

;;; Code:
(require 'ert)
(require 'cl-lib)
(require 'treesit)


;;;; Test utilities
;; ---------------------------------------------------------------------------
(defmacro r-ts-test--with-temp-r-ts (content &rest body)
  "Execute BODY in a temporary buffer containing CONTENT."
  `(with-temp-buffer
     (r-ts-mode)
     (insert ,content)
     ,@body))


;;;; Mode activation
;; ---------------------------------------------------------------------------

(ert-deftest r-ts-mode-test--mode-activates ()
  "R-ts-mode activates without errors when the R grammar is present."
  :tags '(:ts)
  (when (treesit-ready-p 'r t)
    (r-ts-test--with-temp-r-ts "x <- 1\n"
      (should (eq major-mode 'r-ts-mode)))))

(provide 'r-ts-mode-tests)
;;; r-ts-mode-tests.el ends here

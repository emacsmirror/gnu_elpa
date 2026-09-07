;;; r-ts-mode-tests.el --- ERT tests for r-ts-mode  -*- lexical-binding: t; -*-
;; Copyright (C) 2026  Manuel Teodoro Tenango

;; Author: Manuel Teodoro <ttm@teoten.me>
;; URL: https://codeberg.org/R-for-emacs/r-ts-mode
;; Package-Requires: ((emacs "30.1"))

;;; Commentary:
;; Minimal structure and placeholder for `r-ts-mode' unit tests using Emacs ERT
;; framework.

;;; Code:

(require 'ert)
(require 'cl-lib)


;;;; Test utilities
;; ---------------------------------------------------------------------------

(defmacro r-ts-test--with-r-buffer (content &rest body)
  "Execute BODY in a temporary buffer containing CONTENT.
If the R tree-sitter grammar is available the buffer is put in
`r-ts-mode'; otherwise it is left in `fundamental-mode' so that
pure-function tests can still run."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (insert ,content)
     (goto-char (point-min))
     (if (treesit-ready-p 'r t)
         (r-ts-mode)
       (fundamental-mode))
     ,@body))


;;;; Mode activation
;; ---------------------------------------------------------------------------

(ert-deftest r-ts-mode-test--mode-activates ()
  "R-ts-mode activates without errors when the R grammar is present."
  :tags '(:ts)
  (when (treesit-ready-p 'r t)
    (r-ts-test--with-r-buffer "x <- 1\n"
      (should (eq major-mode 'r-ts-mode)))))

(provide 'r-ts-mode-tests)
;;; r-ts-mode-tests.el ends here

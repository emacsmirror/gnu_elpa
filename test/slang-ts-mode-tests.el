;;; slang-ts-mode-tests.el --- tests for slang-ts-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Hikari

;; Author           : Hikari <aneris@disroot.org>
;; URL              : https://codeberg.org/hikari/slang-ts-mode
;; Version          : 0.1

;;; Code:

(require 'ert)
(require 'ert-font-lock)
(require 'treesit)

(ert-deftest slang-ts-test-indentation ()
  (skip-unless (treesit-ready-p 'slang t))
  (ert-test-erts-file (ert-resource-file "indent.erts")))

(ert-deftest slang-ts-test-font-lock ()
  (skip-unless (treesit-ready-p 'slang t))
  (let ((treesit-font-lock-level 4))
    (ert-font-lock-test-file (ert-resource-file "font-lock.slang")
                             'slang-ts-mode)))

(provide 'slang-ts-mode-tests)

;;; slang-ts-mode-tests.el ends here

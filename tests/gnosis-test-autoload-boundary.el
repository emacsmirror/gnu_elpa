;;; gnosis-test-autoload-boundary.el --- Autoload boundary tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Free Software Foundation, Inc.

;; Author: Thanos Apollo <public@thanosapollo.org>

;;; Commentary:

;; Exercise public commands from generated loaddefs in a fresh Emacs process.

;;; Code:

(require 'ert)
(require 'gnosis-autoloads)

(defvar gnosis-db)
(defvar gnosis-dir)
(defvar gnosis-nodes-dir)
(defvar gnosis-journal-dir)

(ert-deftest gnosis-test-nodes-db-sync-autoload-loads-journal ()
  "Node sync loads its journal owner even when both directories are empty."
  (let* ((root (make-temp-file "gnosis-autoload-boundary-" t))
         (gnosis-dir (file-name-as-directory root))
         (gnosis-nodes-dir (expand-file-name "nodes" root))
         (gnosis-journal-dir (expand-file-name "journal" root))
         (gnosis-db nil))
    (unwind-protect
        (progn
          (should (autoloadp (symbol-function 'gnosis-nodes-db-sync)))
          (gnosis-nodes-db-sync)
          (should (featurep 'gnosis-journal)))
      (when gnosis-db
        (gnosis-sqlite-close gnosis-db))
      (delete-directory root t))))

(provide 'gnosis-test-autoload-boundary)
;;; gnosis-test-autoload-boundary.el ends here

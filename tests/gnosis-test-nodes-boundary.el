;;; gnosis-test-nodes-boundary.el --- Node load boundary tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Free Software Foundation, Inc.

;; Author: Thanos Apollo <public@thanosapollo.org>

;;; Commentary:

;; Verify that node-only loading pulls in journal behavior only when a
;; journal path is actually inspected.

;;; Code:

(require 'ert)
(require 'gnosis-nodes)

(defvar gnosis-journal-dir)

(ert-deftest gnosis-test-nodes-loads-journal-for-journal-paths ()
  "Journal-aware node behavior loads its journal owner on demand."
  (should-not (featurep 'gnosis-journal))
  (let* ((dir (make-temp-file "gnosis-journal-boundary-" t))
         (file (expand-file-name "entry.org" dir))
         (gnosis-journal-dir dir))
    (unwind-protect
        (progn
          (with-temp-file file (insert "* Entry\n"))
          (with-temp-buffer
            (setq buffer-file-name file)
            (should (gnosis-nodes--journal-buffer-p))
            (should (featurep 'gnosis-journal))))
      (delete-directory dir t))))

(provide 'gnosis-test-nodes-boundary)
;;; gnosis-test-nodes-boundary.el ends here

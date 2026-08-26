;;; gnosis-test-journal-boundary.el --- Journal load boundary tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Free Software Foundation, Inc.

;; Author: Thanos Apollo <public@thanosapollo.org>

;;; Commentary:

;; Verify that the journal module loads the node operations its public
;; commands call.

;;; Code:

(require 'ert)
(require 'gnosis-journal)

(ert-deftest gnosis-test-journal-loads-node-dependency ()
  "Loading the journal defines every node operation it calls."
  (should (featurep 'gnosis-nodes))
  (dolist (function '(gnosis-nodes-select
                      gnosis-nodes--find
                      gnosis-nodes--create-file
                      gnosis-nodes-insert
                      gnosis-nodes-select-template
                      gnosis-nodes-find
                      gnosis-nodes-mode
                      gnosis-nodes-update-file
                      gnosis-nodes--file-changed-p))
    (should (fboundp function))))

(provide 'gnosis-test-journal-boundary)
;;; gnosis-test-journal-boundary.el ends here

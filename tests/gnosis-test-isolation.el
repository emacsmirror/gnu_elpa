;;; gnosis-test-isolation.el --- Test isolation for Gnosis  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Free Software Foundation, Inc.

;; Author: Thanos Apollo <public@thanosapollo.org>

;;; Commentary:

;; Prove that test fixtures cannot mutate the user's Gnosis directory or Git
;; repository.

;;; Code:

(require 'ert)
(require 'gnosis-anki)
(require 'gnosis-review)
(require 'gnosis-test-helpers)

(ert-deftest gnosis-test-with-db-isolates-filesystem-and-vc ()
  "Keep database, filesystem, and version-control effects inside the fixture."
  (let ((user-dir gnosis-dir)
        fixture-dir
        vc-called)
    (gnosis-test-with-db
      (setq fixture-dir gnosis-dir)
      (cl-letf (((symbol-function 'gnosis--ensure-git-repo)
                 (lambda () (setq vc-called t)))
                ((symbol-function 'gnosis--git-chain)
                 (lambda (&rest _) (setq vc-called t))))
        (gnosis-anki--commit-import 1 "fixture.db")
        (should gnosis-testing)
        (should-not (equal gnosis-dir user-dir))
        (should (file-in-directory-p gnosis-test--db-file gnosis-dir))
        (should-not vc-called)))
    (should-not (file-exists-p fixture-dir))))

(ert-deftest gnosis-test-review-commit-skips-vc-when-testing ()
  "Keep review commits free of version-control effects while testing."
  (let (vc-calls)
    (cl-letf (((symbol-function 'gnosis--ensure-git-repo)
               (lambda () (push 'ensure vc-calls)))
              ((symbol-function 'gnosis--git-chain)
               (lambda (&rest _) (push 'chain vc-calls))))
      (let ((gnosis-testing t))
        (gnosis-review-commit 1))
      (should-not vc-calls))))

(provide 'gnosis-test-isolation)
;;; gnosis-test-isolation.el ends here

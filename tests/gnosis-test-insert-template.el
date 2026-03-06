;;; gnosis-test-insert-template.el --- Tests for template insertion  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Free Software Foundation, Inc.

;; Author: Thanos Apollo <public@thanosapollo.org>

;;; Commentary:

;; Tests for `gnosis-org-expand-headings' and `gnosis-nodes-insert-template'.
;; All pure -- no database needed.

;;; Code:

(require 'ert)
(require 'org)

(load (expand-file-name "../gnosis-org.el"
       (file-name-directory (or load-file-name buffer-file-name))))
(load (expand-file-name "../gnosis-nodes.el"
       (file-name-directory (or load-file-name buffer-file-name))))

;;; ---- Group 1: gnosis-org-expand-headings ----

(ert-deftest gnosis-test-expand-headings-top-level ()
  "At top level (base-level 1), {*} should expand to *."
  (should (string= "* Heading\n"
                    (gnosis-org-expand-headings "{*} Heading\n" 1))))

(ert-deftest gnosis-test-expand-headings-level-2 ()
  "{*} at base-level 2 should expand to **."
  (should (string= "** Heading\n"
                    (gnosis-org-expand-headings "{*} Heading\n" 2))))

(ert-deftest gnosis-test-expand-headings-level-3 ()
  "{*} at base-level 3 should expand to ***."
  (should (string= "*** Heading\n"
                    (gnosis-org-expand-headings "{*} Heading\n" 3))))

(ert-deftest gnosis-test-expand-headings-extra-level ()
  "{**} adds one extra level beyond base."
  (should (string= "*** Heading\n"
                    (gnosis-org-expand-headings "{**} Heading\n" 2))))

(ert-deftest gnosis-test-expand-headings-two-extra ()
  "{***} adds two extra levels beyond base."
  (should (string= "**** Heading\n"
                    (gnosis-org-expand-headings "{***} Heading\n" 2))))

(ert-deftest gnosis-test-expand-headings-multiple-markers ()
  "Multiple markers in one template expand independently."
  (should (string= "** Notes\n\n*** Sub\n"
                    (gnosis-org-expand-headings "{*} Notes\n\n{**} Sub\n" 2))))

(ert-deftest gnosis-test-expand-headings-no-markers ()
  "Text without markers passes through unchanged."
  (should (string= "Just plain text\n"
                    (gnosis-org-expand-headings "Just plain text\n" 2))))

;;; ---- Group 2: gnosis-nodes-insert-template level calculation ----

(ert-deftest gnosis-test-insert-template-at-top-level ()
  "At top level (no heading), {*} should expand to * (level 1)."
  (let ((gnosis-nodes-templates
         '(("Test" (lambda () "{*} Heading\n"))))
        (gnosis-journal-dir "/nonexistent"))
    (with-temp-buffer
      (org-mode)
      (gnosis-nodes-insert-template)
      (should (string= "* Heading\n" (buffer-string))))))

(ert-deftest gnosis-test-insert-template-under-level-1 ()
  "Under a level-1 heading, {*} should expand to ** (child)."
  (let ((gnosis-nodes-templates
         '(("Test" (lambda () "{*} Heading\n"))))
        (gnosis-journal-dir "/nonexistent"))
    (with-temp-buffer
      (org-mode)
      (insert "* Parent\n")
      (gnosis-nodes-insert-template)
      (should (string= "* Parent\n** Heading\n" (buffer-string))))))

(ert-deftest gnosis-test-insert-template-under-level-2 ()
  "Under a level-2 heading, {*} should expand to *** (child)."
  (let ((gnosis-nodes-templates
         '(("Test" (lambda () "{*} Heading\n"))))
        (gnosis-journal-dir "/nonexistent"))
    (with-temp-buffer
      (org-mode)
      (insert "* Parent\n** Sub\n")
      (gnosis-nodes-insert-template)
      (should (string= "* Parent\n** Sub\n*** Heading\n" (buffer-string))))))

(ert-deftest gnosis-test-insert-template-nested-markers ()
  "{*} and {**} under level-1 should become ** and ***."
  (let ((gnosis-nodes-templates
         '(("Test" (lambda () "{*} Notes\n\n{**} Details\n"))))
        (gnosis-journal-dir "/nonexistent"))
    (with-temp-buffer
      (org-mode)
      (insert "* Parent\n")
      (gnosis-nodes-insert-template)
      (should (string= "* Parent\n** Notes\n\n*** Details\n"
                        (buffer-string))))))

(provide 'gnosis-test-insert-template)

(ert-run-tests-batch-and-exit)
;;; gnosis-test-insert-template.el ends here
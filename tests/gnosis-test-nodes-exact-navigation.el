;;; gnosis-test-nodes-exact-navigation.el --- Reveal chosen IDs -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.
;; Author: Thanos Apollo <public@thanosapollo.org>

;;; Commentary:

;; Explicit destination navigation must not restore a restriction over its ID.

;;; Code:

(require 'gnosis-test-node-selection)

(ert-deftest gnosis-test-nodes-exact-navigation-public-key ()
  "The public link key reveals node/journal siblings, retaining dirty text."
  (dolist (journal '(nil t))
    (dolist (same '(nil t))
      (gnosis-test-node-selection--with-vault
        (let* ((file (gnosis-test-node-selection--file
                      "destination.org" "root" "Root" nil
                      (concat "* One\n:PROPERTIES:\n:ID: one\n:END:\n"
                              "[[id:two][Two]]\n"
                              "* Two\n:PROPERTIES:\n:ID: two\n:END:\nTwo text.\n")
                      journal))
               (destination (find-file-noselect file)))
          (with-current-buffer destination
            (goto-char (point-max))
            (insert "Unsaved destination draft.\n")
            (goto-char (point-min))
            (search-forward "* One")
            (org-narrow-to-subtree)
            (search-forward "[[id:two]")
            (backward-char 3))
          (let ((text (with-current-buffer destination
                        (save-restriction (widen) (buffer-string))))
                (hooks (buffer-local-value 'after-save-hook destination)))
            (with-temp-buffer
              (org-mode)
              (gnosis-nodes-mode 1)
              (insert "* Source\n[[id:two][Two]]\n* Other\nKeep hidden.\n")
              (goto-char (point-min))
              (org-narrow-to-subtree)
              (search-forward "[[id:two]")
              (backward-char 3)
              (let ((origin (current-buffer))
                    (point (point))
                    (restriction (cons (point-min) (point-max))))
                (when same (set-buffer destination))
                (should (eq (key-binding (kbd "C-c C-o")) #'gnosis-nodes-goto-id))
                (call-interactively (key-binding (kbd "C-c C-o")))
                (should (eq (current-buffer) destination))
                (should (equal (org-id-get) "two"))
                (should-not (buffer-narrowed-p))
                (should-not (invisible-p (point)))
                (should (equal (buffer-string) text))
                (should (buffer-modified-p))
                (should (equal after-save-hook hooks))
                (should (derived-mode-p 'org-mode))
                (unless same
                  (with-current-buffer origin
                    (should (= (point) point))
                    (should (equal (cons (point-min) (point-max)) restriction))))))))))))

(ert-deftest gnosis-test-nodes-exact-navigation-find-reveals-root ()
  "Public find reaches an indexed root outside the destination restriction."
  (gnosis-test-node-selection--with-vault
    (let* ((file (gnosis-test-node-selection--file
                  "root.org" "root" "Root" nil
                  "* Child\n:PROPERTIES:\n:ID: child\n:END:\nChild text.\n"))
           (destination (find-file-noselect file)))
      (with-current-buffer destination
        (search-forward "* Child")
        (org-narrow-to-subtree))
      (gnosis-nodes-find "Root")
      (should (eq (current-buffer) destination))
      (should-not (buffer-narrowed-p))
      (should (equal (org-id-get) "root"))
      (should-not (buffer-modified-p)))))

(provide 'gnosis-test-nodes-exact-navigation)
;;; gnosis-test-nodes-exact-navigation.el ends here

;;; gnosis-test-audit-composition.el --- Cross-module ownership -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;;; Commentary:
;; Exercise real node indexing, dashboard writes and optional Git together.

;;; Code:

(require 'gnosis-test-node-paths)
(require 'gnosis-test-dashboard-marks)

(ert-deftest gnosis-test-composition-marked-link-final-owner-and-git ()
  "Keep the final owner guard before SQL and optional Git after committed SQL."
  (dolist (change '(nil sorted refresh file detached))
    (gnosis-test-node-paths--with-vault
      (with-temp-file b
        (insert ":PROPERTIES:\n:ID: nested\n:END:\n#+title: Target\n"))
      (gnosis-nodes-db-sync)
      (should (equal "sub/same.org" (gnosis-get 'file 'nodes '(= id "nested"))))
      (gnosis-test--add-basic-thema "Zulu" "Answer" nil nil 1)
      (gnosis-test--add-basic-thema "Zulu unmarked" "Answer" nil nil 2)
      (gnosis-test-dashboard--with-view
        (gnosis-dashboard-output-themata '(1 2))
        (call-interactively (local-key-binding (kbd "m")))
        (when (eq change 'sorted) (tabulated-list-sort 0))
        (let ((gnosis-testing nil)
              (exec-path nil)
              (commits 0)
              (prompts 0)
              (commit (symbol-function 'gnosis-vc--auto-commit)))
          (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "Zulu"))
                    ((symbol-function 'gnosis-completing-read)
                     (lambda (&rest _) "Target"))
                    ((symbol-function 'y-or-n-p)
                     (lambda (&rest _)
                       (cl-incf prompts)
                       (pcase change
                         ('sorted (drain))
                         ('refresh (gnosis-dashboard-output-themata '(1 2)))
                         ((or 'file 'detached)
                          (set-visited-file-name
                           (expand-file-name "view.txt" gnosis-dir) t)
                          (when (eq change 'detached)
                            (set-visited-file-name nil t))))
                       t))
                    ((symbol-function 'gnosis-vc--auto-commit)
                     (lambda (&rest args)
                       (cl-incf commits)
                       ;; A second connection sees only committed content.
                       (let ((other (sqlite-open
                                     (expand-file-name "gnosis.db" gnosis-dir))))
                         (unwind-protect
                             (should (equal
                                      '(("\"[[id:nested][Zulu]]\""))
                                      (sqlite-select other
                                                     "SELECT keimenon FROM themata WHERE id = 1")))
                           (sqlite-close other)))
                       (apply commit args))))
            (if (memq change '(refresh file detached))
                (should-error (call-interactively (local-key-binding (kbd "b")))
                              :type 'user-error)
              (call-interactively (local-key-binding (kbd "b")))))
          (should (= prompts 1))
          (should (= commits (if (memq change '(refresh file detached)) 0 1))))
        (should (equal "Zulu unmarked"
                       (gnosis-get 'keimenon 'themata '(= id 2))))
        (if (memq change '(refresh file detached))
            (progn
              (should (equal "Zulu" (gnosis-get 'keimenon 'themata '(= id 1))))
              (should-not (gnosis-select '* 'thema-links)))
          (should (equal '((1 "nested")) (gnosis-select '* 'thema-links)))
          (gnosis-test-dashboard--assert-marks nil))
        (should-not (file-exists-p (expand-file-name ".git" gnosis-dir)))))))

(ert-deftest gnosis-test-composition-nested-native-links-and-deletion ()
  "Native nested saves preserve nearest-ID edges and unrelated namesakes."
  (gnosis-test-node-paths--with-vault
    (with-temp-file a
      (insert ":PROPERTIES:\n:ID: root\n:END:\n#+title: Root\n[[id:nested]]\n"))
    (with-temp-file b
      (insert ":PROPERTIES:\n:ID: nested\n:END:\n#+title: Nested\n"
              "[[id:root][Root]]\n"
              "#+begin_example\n[[id:example-false]]\n#+end_example\n"
              "# [[id:comment-false]]\n=id:literal-false=\n"
              "* Child\n:PROPERTIES:\n:ID: child\n:END:\n"
              "[[id:root]]\n** Inherited\n<id:root>\n"
              "#+begin_src text\n[[id:source-false]]\n#+end_src\n"))
    (dolist (file (list a b))
      (with-current-buffer (find-file-noselect file)
        (should gnosis-nodes-mode)
        (goto-char (point-max))
        (insert "Saved natively.\n")
        (call-interactively #'save-buffer)))
    (let ((expected '(("child" "nested") ("child" "root")
                      ("nested" "root") ("root" "nested"))))
      (should (equal expected
                     (sort (gnosis-select '* 'node-links)
                           (lambda (x y) (string< (prin1-to-string x)
                                                 (prin1-to-string y))))))
      (let ((root-bytes (gnosis-test-node-paths--bytes a))
            (root-row (gnosis-select '* 'nodes '(= id "root"))))
        (with-current-buffer (find-file-noselect b)
          (goto-char (point-max))
          (insert "Another save.\n")
          (call-interactively #'save-buffer))
        (gnosis-nodes-db-sync)
        (should (equal expected
                       (sort (gnosis-select '* 'node-links)
                             (lambda (x y) (string< (prin1-to-string x)
                                                   (prin1-to-string y))))))
        (gnosis-nodes-goto-id "child")
        (should (equal buffer-file-name b))
        (should (equal (org-id-get) "child"))
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
          (gnosis-nodes-delete-file b))
        (should-not (file-exists-p b))
        (should (equal root-bytes (gnosis-test-node-paths--bytes a)))
        (should (equal root-row (gnosis-select '* 'nodes '(= id "root"))))
        (should-not (gnosis-select '* 'node-links))
        (should-not (gnosis-select '* 'nodes '(in id ["nested" "child"])))))))

(provide 'gnosis-test-audit-composition)
;;; gnosis-test-audit-composition.el ends here

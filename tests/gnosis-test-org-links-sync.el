;;; gnosis-test-org-links-sync.el --- Native Org link indexing tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Thanos Apollo <public@thanosapollo.org>

;;; Commentary:

;; Verify native Org link interpretation through disposable node indexes.

;;; Code:

(require 'ert)
(require 'gnosis-nodes)
(require 'gnosis-test-helpers)

(ert-deftest gnosis-test-org-links-sync-native-edges ()
  "Index only real ID links with their nearest enclosing node owner."
  (gnosis-test-with-db
    (let* ((gnosis-nodes-dir gnosis-dir)
           (file (expand-file-name "links.org" gnosis-nodes-dir))
           (content (concat
                     ":PROPERTIES:\n:ID: root\n:END:\n#+title: Links\n\n"
                     "[[id:root-target][Root target]]\n"
                     "[[https://example.org][id:not-a-target]]\n"
                     "#+begin_src org\n[[id:source-only]]\n#+end_src\n"
                     "* Child\n:PROPERTIES:\n:ID: child\n:END:\n"
                     "** Without ID\n[[id:child-target]]\n"
                     "#+begin_example\n[[id:example-only]]\n#+end_example\n"
                     "* Sibling without ID\n[[id:sibling-target]]\n"))
           (expected '(("child" "child-target") ("child" "root")
                       ("root" "root-target") ("root" "sibling-target"))))
      (with-temp-file file (insert content))
      ;; Repeated sync must preserve the same exact edges and source bytes.
      (dotimes (_ 2)
        (gnosis-nodes-update-file file t)
        (should (equal (sort (gnosis-select '[source dest] 'node-links)
                             (lambda (a b)
                               (string< (prin1-to-string a)
                                        (prin1-to-string b))))
                       expected)))
      (should (equal (with-temp-buffer
                       (insert-file-contents file)
                       (buffer-string))
                     content)))))

(provide 'gnosis-test-org-links-sync)
;;; gnosis-test-org-links-sync.el ends here

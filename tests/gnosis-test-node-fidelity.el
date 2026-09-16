;;; gnosis-test-node-fidelity.el --- Node source fidelity tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;;; Commentary:
;; File metadata edits preserve literal content; sync observes actual contents.

;;; Code:

(require 'ert)
(require 'gnosis-nodes)
(require 'gnosis-journal)
(require 'gnosis-test-helpers)
(require 'gnosis-test-org-preservation)

(ert-deftest gnosis-test-node-fidelity-literal-filetags ()
  "Native tagging must not overwrite FILETAGS inside literal blocks."
  (dolist (block '("#+begin_src text\n#+filetags: :example:\n#+end_src\n"
                   "#+begin_example\n#+FILETAGS: :example:\n#+end_example\n"))
    (gnosis-test-org-preservation--tag-file "#+title: Topic\n" nil nil block)))

(ert-deftest gnosis-test-node-fidelity-repeated-filetags ()
  "Collect every real FILETAGS keyword, including after a misleading block."
  (gnosis-test-with-db
    (let* ((gnosis-nodes-dir (expand-file-name "nodes" gnosis-dir))
           (gnosis-journal-dir (expand-file-name "journal" gnosis-dir))
           (gnosis-journal-file nil)
           (org-id-track-globally nil)
           (org-id-locations nil)
           (file (expand-file-name "topic.org" gnosis-nodes-dir))
           (block "#+begin_src text\n#+filetags: :literal:\n#+end_src\n")
           (body "\nBody [[id:target]]\n* Child :own:\n:PROPERTIES:\n:ID: child\n:END:\nText\n"))
      (make-directory gnosis-nodes-dir t)
      (with-temp-file file
        (insert ":PROPERTIES:\n:ID: root\n:END:\n#+title: Topic\n"
                block "#+FILETAGS: :first:\n#+filetags: :second:first:\n" body))
      (unwind-protect
          (save-window-excursion
            (find-file file)
            (gnosis-nodes-mode 1)
            (gnosis-nodes-update-file)
            (should (equal (gnosis-org-get-filetags) '("first" "second")))
            (goto-char (point-min))
            (search-forward "Body")
            (cl-letf (((symbol-function 'completing-read-multiple)
                       (lambda (&rest _) '("added"))))
              (call-interactively (key-binding (kbd "C-c C-q"))))
            (save-buffer)
            (kill-buffer)
            (find-file file)
            (should (string-match-p (regexp-quote block) (buffer-string)))
            (should (string-suffix-p body (buffer-string)))
            (goto-char (point-min))
            (should (equal (org-id-get) "root"))
            (should (equal (gnosis-org-get-filetags) '("first" "second" "added")))
            (should (equal (sort (gnosis-select 'tag 'node-tag '(= node-id "root") t)
                                 #'string<)
                           '("added" "first" "second")))
            (should (equal (sort (gnosis-select 'tag 'node-tag '(= node-id "child") t)
                                 #'string<)
                           '("added" "first" "own" "second"))))
        (when-let* ((buffer (get-file-buffer file)))
          (with-current-buffer buffer (set-buffer-modified-p nil))
          (kill-buffer buffer))))))

(defun gnosis-test-node-fidelity--sync (journal same-time)
  "Check external sync for JOURNAL or nodes with SAME-TIME timestamps."
  (gnosis-test-with-db
    (let* ((gnosis-nodes-dir (expand-file-name "nodes" gnosis-dir))
           (gnosis-journal-dir (expand-file-name "journal" gnosis-dir))
           (gnosis-journal-file nil)
           (table (if journal 'journal 'nodes))
           (directory (if journal gnosis-journal-dir gnosis-nodes-dir))
           (file (expand-file-name "topic.org" directory))
           (first-time '(27465 53760 100000 0))
           (second-time (if same-time first-time '(27465 53760 900000 0))))
      (make-directory directory t)
      (with-temp-file file
        (insert ":PROPERTIES:\n:ID: root\n:END:\n#+title: Before\n"))
      (set-file-times file first-time)
      (gnosis-nodes-update-file file)
      ;; This is the retained timestamp representation, not a fresh-schema test.
      (should (equal (gnosis-get 'mtime table '(= id "root"))
                     (format-time-string "%s" first-time)))
      (with-temp-file file
        (insert ":PROPERTIES:\n:ID: root\n:END:\n#+title: After!\n"))
      (set-file-times file second-time)
      (call-interactively #'gnosis-nodes-db-sync)
      (should (equal (gnosis-get 'title table '(= id "root")) "After!"))
      ;; Identical bytes, including after a timestamp-only change, never rebuild.
      (let ((before (gnosis-select '* table)))
        (cl-letf (((symbol-function 'gnosis-nodes--update-file)
                   (lambda (&rest _) (ert-fail "Unchanged file was reindexed"))))
          (gnosis-nodes-db-sync)
          (set-file-times file '(27465 53761 0 0))
          (gnosis-nodes-db-sync))
        (should (equal before (gnosis-select '* table)))))))

(ert-deftest gnosis-test-node-fidelity-node-same-second ()
  "Node sync observes subsecond external edits and leaves unchanged rows alone."
  (gnosis-test-node-fidelity--sync nil nil))

(ert-deftest gnosis-test-node-fidelity-journal-same-second ()
  "Public sync also observes journal edits within a retained timestamp second."
  (gnosis-test-node-fidelity--sync t nil))

(ert-deftest gnosis-test-node-fidelity-coarse-timestamps ()
  "Coarse or preserved filesystem timestamps cannot hide changed contents."
  (gnosis-test-node-fidelity--sync nil t)
  (gnosis-test-node-fidelity--sync t t))

(ert-deftest gnosis-test-node-fidelity-visited-sync ()
  "External sync indexes disk, preserving clean or modified visiting buffers."
  (dolist (journal '(nil t))
    (dolist (modified '(nil t))
      (gnosis-test-with-db
        (let* ((gnosis-nodes-dir (expand-file-name "nodes" gnosis-dir))
               (gnosis-journal-dir (expand-file-name "journal" gnosis-dir))
               (gnosis-journal-file nil)
               (table (if journal 'journal 'nodes))
               (directory (if journal gnosis-journal-dir gnosis-nodes-dir))
               (file (expand-file-name "topic.org" directory))
               (time '(27465 53760 0 0)))
          (make-directory directory t)
          (with-temp-file file
            (insert ":PROPERTIES:\n:ID: root\n:END:\n#+title: Before\n"))
          (set-file-times file time)
          (unwind-protect
              (with-current-buffer (find-file-noselect file)
                (gnosis-nodes-update-file)
                (when modified (goto-char (point-max)) (insert "Unsaved prose\n"))
                (let ((text (buffer-string)))
                  (dolist (title '("After!" "Again!"))
                    (let ((external (make-temp-file (expand-file-name "external-" directory))))
                      (with-temp-file external
                        (insert ":PROPERTIES:\n:ID: root\n:END:\n#+title: " title "\n"))
                      (rename-file external file t))
                    ;; First edit advances within the second; the next retains
                    ;; the exact timestamp while changing bytes again.
                    (set-file-times file '(27465 53760 900000 0))
                    (call-interactively #'gnosis-nodes-db-sync)
                    (should (equal (gnosis-get 'title table '(= id "root")) title))
                    (should-not (gnosis-nodes--file-changed-p file table))
                    (cl-letf (((symbol-function 'gnosis-nodes--update-file)
                               (lambda (&rest _) (ert-fail "Stable index rebuilt"))))
                      (gnosis-nodes-db-sync))
                    (should (equal (buffer-string) text))
                    (should (eq (buffer-modified-p) modified)))
                  ;; A normal native save still indexes the saved buffer.
                  (set-visited-file-modtime)
                  (goto-char (point-max))
                  (insert "Saved prose\n")
                  (save-buffer)
                  (should (equal (gnosis-get 'title table '(= id "root")) "Before"))
                  (should-not (gnosis-nodes--file-changed-p file table))))
            (when-let* ((buffer (get-file-buffer file)))
              (with-current-buffer buffer (set-buffer-modified-p nil))
              (kill-buffer buffer))))))))

(ert-deftest gnosis-test-node-fidelity-sync-read-failure ()
  "A failed content read preserves the index and remains retryable."
  (gnosis-test-with-db
    (let* ((gnosis-nodes-dir (expand-file-name "nodes" gnosis-dir))
           (gnosis-journal-dir (expand-file-name "journal" gnosis-dir))
           (gnosis-journal-file nil)
           (file (expand-file-name "topic.org" gnosis-nodes-dir))
           (time '(27465 53760 0 0)))
      (make-directory gnosis-nodes-dir t)
      (with-temp-file file
        (insert ":PROPERTIES:\n:ID: root\n:END:\n#+title: Before\n"))
      (set-file-times file time)
      (gnosis-nodes-update-file file)
      (let ((before (gnosis-select '* 'nodes)))
        (with-temp-file file
          (insert ":PROPERTIES:\n:ID: root\n:END:\n#+title: After\n"))
        (set-file-times file time)
        (cl-letf (((symbol-function 'gnosis-org--file-hash)
                   (lambda (_) (signal 'file-error '("Read refused")))))
          (should-error (gnosis-nodes-db-sync) :type 'file-error))
        (should (equal before (gnosis-select '* 'nodes)))
        (gnosis-nodes-db-sync)
        (should (equal (gnosis-get 'title 'nodes '(= id "root")) "After"))))))

(provide 'gnosis-test-node-fidelity)
;;; gnosis-test-node-fidelity.el ends here

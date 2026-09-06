;;; gnosis-test-nodes.el --- Tests for gnosis-nodes.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Free Software Foundation, Inc.

;; Author: Thanos Apollo <public@thanosapollo.org>

;;; Commentary:

;; Tests for node/journal sync logic.
;; Uses a temporary SQLite database and temp files.

;;; Code:

(require 'ert)
(require 'org)
(require 'gnosis)
(require 'gnosis-nodes)

(load (expand-file-name "gnosis-test-helpers.el"
       (file-name-directory (or load-file-name buffer-file-name))))

;;; Test helpers

(defvar gnosis-test-nodes--temp-dir nil
  "Temporary directory for test files.")

(defun gnosis-test-nodes--setup-dirs ()
  "Create temp directories for nodes and journal."
  (setq gnosis-test-nodes--temp-dir (make-temp-file "gnosis-test-nodes-" t))
  (make-directory (expand-file-name "journal" gnosis-test-nodes--temp-dir) t))

(defun gnosis-test-nodes--teardown-dirs ()
  "Remove temp directories."
  (when (and gnosis-test-nodes--temp-dir
             (file-directory-p gnosis-test-nodes--temp-dir))
    (delete-directory gnosis-test-nodes--temp-dir t)))

(defun gnosis-test-nodes--create-file (dir name content)
  "Create file NAME with CONTENT in DIR.  Return full path."
  (let ((path (expand-file-name name dir)))
    (with-temp-file path (insert content))
    path))

(defmacro gnosis-test-nodes-with-files (file-specs &rest body)
  "Create node FILE-SPECS and run BODY with their temporary directory.
Each FILE-SPECS entry is an (ID CONTENT) pair."
  (declare (indent 1) (debug t))
  `(progn
     (gnosis-test-nodes--setup-dirs)
     (unwind-protect
         (let ((gnosis-nodes-dir gnosis-test-nodes--temp-dir))
           (dolist (spec ,file-specs)
             (let ((id (nth 0 spec))
                   (content (nth 1 spec)))
               (gnosis-test-nodes--create-file
                gnosis-nodes-dir (format "%s.org" id)
                (format ":PROPERTIES:\n:ID: %s\n:END:\n#+title: %s\n\n%s"
                        id id content))))
           ,@body)
       (gnosis-test-nodes--teardown-dirs))))

(ert-deftest gnosis-test-nodes-search-content-returns-enclosing-node ()
  "Content search returns only the ID-bearing node enclosing a match."
  (gnosis-test-nodes--setup-dirs)
  (unwind-protect
      (let ((gnosis-nodes-dir gnosis-test-nodes--temp-dir))
        (gnosis-test-nodes--create-file
         gnosis-nodes-dir "multi-node.org"
         ":PROPERTIES:\n:ID: root-id\n:END:\n#+title: Root\n\n* Alpha\n:PROPERTIES:\n:ID: alpha-id\n:END:\nAlpha body.\n\n* Beta\n:PROPERTIES:\n:ID: beta-id\n:END:\nUnique beta needle.\n")
        (should (equal (gnosis-nodes-search-content "Unique beta needle")
                       '("beta-id"))))
    (gnosis-test-nodes--teardown-dirs)))

(ert-deftest gnosis-test-nodes-search-content-does-not-reattribute-filtered-node ()
  "Filtering out the enclosing node does not attribute its match to an ancestor."
  (gnosis-test-nodes--setup-dirs)
  (unwind-protect
      (let ((gnosis-nodes-dir gnosis-test-nodes--temp-dir))
        (gnosis-test-nodes--create-file
         gnosis-nodes-dir "multi-node.org"
         ":PROPERTIES:\n:ID: root-id\n:END:\n#+title: Root\n\n* Child\n:PROPERTIES:\n:ID: child-id\n:END:\nChild-only needle.\n")
        (should-not (gnosis-nodes-search-content
                     "Child-only needle" '("root-id"))))
    (gnosis-test-nodes--teardown-dirs)))

(ert-deftest gnosis-test-nodes-search-content-all-files ()
  "Content search returns matching file-level node IDs."
  (gnosis-test-nodes-with-files
      '(("node-aaa" "Emacs is a great editor")
        ("node-bbb" "Vim is also popular")
        ("node-ccc" "Emacs and Vim are both editors"))
    (should (equal (gnosis-nodes-search-content "Emacs")
                   '("node-aaa" "node-ccc")))))

(ert-deftest gnosis-test-nodes-search-content-with-filter ()
  "NODE-IDS restrict content search to that subset."
  (gnosis-test-nodes-with-files
      '(("node-aaa" "Emacs is a great editor")
        ("node-bbb" "Vim is also popular")
        ("node-ccc" "Emacs and Vim are both editors"))
    (should (equal (gnosis-nodes-search-content
                    "Emacs" '("node-aaa"))
                   '("node-aaa")))))

(ert-deftest gnosis-test-nodes-search-content-no-matches ()
  "Content search returns nil when no node matches."
  (gnosis-test-nodes-with-files
      '(("node-aaa" "Emacs is a great editor"))
    (should-not (gnosis-nodes-search-content "nonexistent-term"))))

(ert-deftest gnosis-test-nodes-search-content-rootless-preamble-has-no-owner ()
  "Content before the first heading has no owner without a file-level ID."
  (gnosis-test-nodes--setup-dirs)
  (unwind-protect
      (let ((gnosis-nodes-dir gnosis-test-nodes--temp-dir))
        (gnosis-test-nodes--create-file
         gnosis-nodes-dir "rootless.org"
         "#+title: Rootless\n\nPreamble needle.\n\n* Child\n:PROPERTIES:\n:ID: child-id\n:END:\nChild body.\n")
        (should-not (gnosis-nodes-search-content "Preamble needle")))
    (gnosis-test-nodes--teardown-dirs)))

;;; ---- Group 1: Journal file sync ----

(ert-deftest gnosis-test-nodes-journal-all-entries-inserted ()
  "All journal entries are inserted, even when some have master-child links."
  (gnosis-test-nodes--setup-dirs)
  (unwind-protect
      (let* ((gnosis-nodes-dir gnosis-test-nodes--temp-dir)
             (gnosis-journal-dir (expand-file-name "journal"
                                                   gnosis-test-nodes--temp-dir))
             (file (gnosis-test-nodes--create-file
                    gnosis-journal-dir "journal.org"
                    ":PROPERTIES:
:ID: topic-id
:END:
#+title: Journal
#+filetags:

* 2026-03-01
:PROPERTIES:
:ID: day-01
:END:
** Daily Notes

* 2026-03-02
:PROPERTIES:
:ID: day-02
:END:
** Notes
*** Nested Entry
:PROPERTIES:
:ID: nested-01
:END:
Some content with [[id:external-ref][a link]].

* 2026-03-03
:PROPERTIES:
:ID: day-03
:END:
** Notes
")))
        (gnosis-test-with-db
          (gnosis-nodes--update-file file 'journal)
          ;; All 4 entries (topic + 3 days + 1 nested) should be in journal table
          (let ((rows (gnosis-nodes-select 'id 'journal nil t)))
            (should (= 5 (length rows)))
            (should (member "topic-id" rows))
            (should (member "day-01" rows))
            (should (member "day-02" rows))
            (should (member "nested-01" rows))
            (should (member "day-03" rows)))))
    (gnosis-test-nodes--teardown-dirs)))

(ert-deftest gnosis-test-nodes-journal-no-node-links ()
  "Journal sync does not insert into node-links."
  (gnosis-test-nodes--setup-dirs)
  (unwind-protect
      (let* ((gnosis-nodes-dir gnosis-test-nodes--temp-dir)
             (gnosis-journal-dir (expand-file-name "journal"
                                                   gnosis-test-nodes--temp-dir))
             (file (gnosis-test-nodes--create-file
                    gnosis-journal-dir "journal.org"
                    ":PROPERTIES:
:ID: topic-id
:END:
#+title: Journal
#+filetags:

* Entry
:PROPERTIES:
:ID: entry-01
:END:
See [[id:some-node][link]].
** Sub Entry
:PROPERTIES:
:ID: sub-01
:END:
")))
        (gnosis-test-with-db
          (gnosis-nodes--update-file file 'journal)
          ;; node-links should be empty for journal files
          (let ((links (gnosis-nodes-select '* 'node-links nil)))
            (should (null links)))))
    (gnosis-test-nodes--teardown-dirs)))

(ert-deftest gnosis-test-nodes-regular-file-has-links ()
  "Regular node file inserts master and content links into node-links."
  (gnosis-test-nodes--setup-dirs)
  (unwind-protect
      (let* ((gnosis-nodes-dir gnosis-test-nodes--temp-dir)
             (gnosis-journal-dir (expand-file-name "journal"
                                                   gnosis-test-nodes--temp-dir))
             (file (gnosis-test-nodes--create-file
                    gnosis-test-nodes--temp-dir "20260306--test.org"
                    ":PROPERTIES:
:ID: node-root
:END:
#+title: Test Node

* Section
:PROPERTIES:
:ID: node-sec
:END:
Content with [[id:other-node][a link]].
")))
        (gnosis-test-with-db
          (gnosis-nodes--update-file file nil)
          ;; node-links should have master link (node-sec -> node-root)
          ;; and content link (node-sec -> other-node)
          (let ((links (gnosis-nodes-select '* 'node-links nil)))
            (should (= 2 (length links))))))
    (gnosis-test-nodes--teardown-dirs)))

(ert-deftest gnosis-test-nodes-refresh-preserves-incoming-links ()
  "Refreshing a destination node preserves links from other files."
  (gnosis-test-nodes--setup-dirs)
  (unwind-protect
      (let* ((gnosis-nodes-dir gnosis-test-nodes--temp-dir)
             (gnosis-journal-dir (expand-file-name
                                  "journal" gnosis-test-nodes--temp-dir))
             (file (gnosis-test-nodes--create-file
                    gnosis-nodes-dir "dest.org"
                    ":PROPERTIES:\n:ID: dest\n:END:\n#+title: Destination\n")))
        (gnosis-test-with-db
          (gnosis--insert-into
           'nodes '(["source" "source.org" "Source" 0 nil "0" "hash"]))
          (gnosis--insert-into
           'nodes '(["dest" "dest.org" "Destination" 0 nil "0" "hash"]))
          (gnosis--insert-into 'node-links '(["source" "dest"]))
          (gnosis-nodes-update-file file)
          (should (equal (gnosis-select '[source dest] 'node-links)
                         '(("source" "dest"))))))
    (gnosis-test-nodes--teardown-dirs)))

(ert-deftest gnosis-test-nodes-refresh-rolls-back-failed-replacement ()
  "Keep the old node index intact when parsing or insertion fails."
  (gnosis-test-with-db
    (let* ((gnosis-nodes-dir gnosis-dir)
           (gnosis-journal-dir (expand-file-name "journal" gnosis-dir))
           (file (gnosis-test-nodes--create-file
                  gnosis-dir "node.org"
                  ":PROPERTIES:\n:ID: root\n:END:\n#+title: Old\n#+filetags: :old:\n[[id:target][Target]]\n")))
      (gnosis-nodes-update-file file)
      (let ((before (mapcar (lambda (table) (gnosis-select '* table))
                           '(nodes node-tag node-links))))
        (dolist (failure '(parse insert))
          (with-temp-file file
            (insert ":PROPERTIES:\n:ID: root\n:END:\n"
                    (if (eq failure 'parse) "" "#+title: New\n")))
          (when (eq failure 'insert)
            (gnosis-sqlite-execute
             gnosis-db
             "CREATE TRIGGER reject_node BEFORE INSERT ON nodes
                BEGIN SELECT RAISE(ABORT, 'Controlled insertion failure'); END"))
          (let ((caught (condition-case err
                            (progn (gnosis-nodes-update-file file) nil)
                          (error err))))
            (should (equal before
                           (mapcar (lambda (table) (gnosis-select '* table))
                                   '(nodes node-tag node-links))))
            (should caught)))))))

(ert-deftest gnosis-test-nodes-refresh-replaces-owned-index ()
  "Replace removed headings, tags and outgoing links without losing backlinks."
  (gnosis-test-with-db
    (let* ((gnosis-nodes-dir gnosis-dir)
           (gnosis-journal-dir (expand-file-name "journal" gnosis-dir))
           (file (gnosis-test-nodes--create-file
                  gnosis-dir "node.org"
                  ":PROPERTIES:\n:ID: root\n:END:\n#+title: Old\n#+filetags: :old:\n* Child\n:PROPERTIES:\n:ID: child\n:END:\n[[id:target][Target]]\n")))
      (gnosis-nodes-update-file file)
      (gnosis--insert-into
       'nodes '(["source" "source.org" "Source" 0 nil "0" "hash"]))
      (gnosis--insert-into 'node-links '(["source" "root"]))
      (with-temp-file file
        (insert ":PROPERTIES:\n:ID: root\n:END:\n#+title: New\n#+filetags: :new:\n"))
      (gnosis-nodes-update-file file)
      (should (equal '(("root" "New"))
                     (gnosis-select '[id title] 'nodes '(= file "node.org"))))
      (should (equal '(("root" "new"))
                     (gnosis-select '* 'node-tag)))
      (should (equal '(("source" "root"))
                     (gnosis-select '* 'node-links))))))

(ert-deftest gnosis-test-nodes-force-sync-rolls-back-journal-failure ()
  "Keep both indexes when a forced rebuild encounters an invalid journal."
  (gnosis-test-with-db
    (let ((gnosis-nodes-dir gnosis-dir)
          (gnosis-journal-dir (expand-file-name "journal" gnosis-dir)))
      (make-directory gnosis-journal-dir t)
      (gnosis-test-nodes--create-file
       gnosis-dir "node.org"
       ":PROPERTIES:\n:ID: node\n:END:\n#+title: Replacement\n")
      (gnosis-test-nodes--create-file
       gnosis-journal-dir "journal.org" "Missing title\n")
      (gnosis--insert-into
       'nodes '(["node" "node.org" "Old" 0 nil "0" "hash"]))
      (gnosis--insert-into
       'journal '(["entry" "journal.org" "Old journal" 0 nil "0" "hash"]))
      (let ((before (mapcar (lambda (table) (gnosis-select '* table))
                           '(nodes journal))))
        (should-error (gnosis-nodes-db-sync t))
        (should (equal before (mapcar (lambda (table) (gnosis-select '* table))
                                     '(nodes journal))))))))

;;; ---- Group 2: Purge tables ----

(ert-deftest gnosis-test-nodes-purge-tables ()
  "Purge clears all node and journal tables."
  (gnosis-test-nodes--setup-dirs)
  (unwind-protect
      (let* ((gnosis-nodes-dir gnosis-test-nodes--temp-dir)
             (gnosis-journal-dir (expand-file-name "journal"
                                                   gnosis-test-nodes--temp-dir)))
        (gnosis-test-with-db
          ;; Insert some data
          (gnosis--insert-into 'nodes
            '(["n1" "test.org" "Title" 0 "nil" "0" "abc"]))
          (gnosis--insert-into 'journal
            '(["j1" "journal.org" "Entry" 1 "nil" "0" "def"]))
          (gnosis--insert-into 'node-tag '(["n1" "tag1"]))
          ;; Verify data exists
          (should (= 1 (length (gnosis-nodes-select '* 'nodes nil))))
          (should (= 1 (length (gnosis-nodes-select '* 'journal nil))))
          ;; Purge
          (gnosis-nodes--purge-tables)
          ;; All empty
          (should (null (gnosis-nodes-select '* 'nodes nil)))
          (should (null (gnosis-nodes-select '* 'journal nil)))
          (should (null (gnosis-nodes-select '* 'node-tag nil)))))
    (gnosis-test-nodes--teardown-dirs)))

(ert-deftest gnosis-test-nodes-goto-id-without-id-opens-at-point ()
  "Opening without an ID delegates directly to Org."
  (with-temp-buffer
    (org-mode)
    (insert "plain text")
    (goto-char (point-min))
    (let (opened)
      (cl-letf (((symbol-function 'gnosis-nodes-select)
                 (lambda (&rest _)
                   (ert-fail "Queried the database without an ID")))
                ((symbol-function 'org-open-at-point)
                 (lambda (&rest _) (setq opened t))))
        (gnosis-nodes-goto-id)
        (should opened)))))

(provide 'gnosis-test-nodes)

(ert-run-tests-batch-and-exit)
;;; gnosis-test-nodes.el ends here

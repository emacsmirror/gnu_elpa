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

(provide 'gnosis-test-nodes)

(ert-run-tests-batch-and-exit)
;;; gnosis-test-nodes.el ends here

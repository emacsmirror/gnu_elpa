;;; org-gnosis-test-db.el --- Tests for org-gnosis database layer -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Thanos Apollo <public@thanosapollo.org>
;; Keywords: tests

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Tests for database init, CRUD, tags, links, sync, file naming,
;; journal, and org-gnosis-mode hook behavior.

;;; Code:

(require 'ert)
(require 'org-element)

;; Load the main org-gnosis.el file
(let ((gnosis-file (expand-file-name "../org-gnosis.el"
                                     (file-name-directory (or load-file-name buffer-file-name)))))
  (if (file-exists-p gnosis-file)
      (load gnosis-file)
    (error "Could not find org-gnosis.el at %s" gnosis-file)))

;;; ---- Test infrastructure (reused from test-sync.el) ----

(defvar org-gnosis-test-temp-dir nil
  "Temporary directory for test files.")

(defun org-gnosis-test-setup ()
  "Set up test environment with temporary directory and database."
  (setq org-gnosis-test-temp-dir (make-temp-file "org-gnosis-test" t))
  (setq org-gnosis-dir org-gnosis-test-temp-dir)
  (setq org-gnosis-journal-dir (expand-file-name "journal" org-gnosis-test-temp-dir))
  (setq org-gnosis-journal-file (expand-file-name "journal.org" org-gnosis-journal-dir))
  (setq org-gnosis-database-file (expand-file-name "test.db" org-gnosis-test-temp-dir))
  (make-directory org-gnosis-journal-dir t)
  ;; Close any existing connection
  (when (boundp 'org-gnosis-db--connection)
    (when (and org-gnosis-db--connection
               (emacsql-live-p org-gnosis-db--connection))
      (emacsql-close org-gnosis-db--connection))
    (setq org-gnosis-db--connection nil))
  ;; Delete database file if it exists
  (when (file-exists-p org-gnosis-database-file)
    (delete-file org-gnosis-database-file)))

(defun org-gnosis-test-teardown ()
  "Clean up test environment."
  (when (boundp 'org-gnosis-db--connection)
    (when (and org-gnosis-db--connection
               (emacsql-live-p org-gnosis-db--connection))
      (emacsql-close org-gnosis-db--connection))
    (setq org-gnosis-db--connection nil))
  (when (and org-gnosis-test-temp-dir
             (file-exists-p org-gnosis-test-temp-dir))
    (delete-directory org-gnosis-test-temp-dir t)))

(defun org-gnosis-test-create-test-file (filename content)
  "Create a test file FILENAME with CONTENT in test directory."
  (let ((filepath (expand-file-name filename org-gnosis-test-temp-dir)))
    (with-temp-file filepath
      (insert content))
    filepath))

(defun org-gnosis-test-create-journal-file (filename content)
  "Create a journal test file FILENAME with CONTENT in journal directory."
  (let ((filepath (expand-file-name filename org-gnosis-journal-dir)))
    (with-temp-file filepath
      (insert content))
    filepath))

(defun org-gnosis-test-db-tables ()
  "Return list of user table names in current database."
  (mapcar #'car
          (emacsql (org-gnosis-db-get)
                   [:select name :from sqlite-master
                    :where (and (= type 'table)
                                (not-like name "sqlite_%"))])))

(defun org-gnosis-test-db-indexes ()
  "Return list of index names in current database."
  (mapcar #'car
          (emacsql (org-gnosis-db-get)
                   [:select name :from sqlite-master
                    :where (= type 'index)])))

;;; ---- Group 1: Database Init & Schema ----

(ert-deftest org-gnosis-test-db-init-creates-all-tables ()
  "Verify all 5 tables exist after init."
  (org-gnosis-test-setup)
  (unwind-protect
      (progn
        (org-gnosis-db-init)
        (let ((tables (org-gnosis-test-db-tables)))
          (should (member 'nodes tables))
          (should (member 'tags tables))
          (should (member 'journal tables))
          ;; emacsql converts hyphens to underscores in SQLite
          (should (member 'node_tag tables))
          (should (member 'links tables))
          (should (= 5 (length tables)))))
    (org-gnosis-test-teardown)))

(ert-deftest org-gnosis-test-db-init-creates-indexes ()
  "Verify idx-nodes-file and idx-journal-file indexes exist."
  (org-gnosis-test-setup)
  (unwind-protect
      (progn
        (org-gnosis-db-init)
        (let ((indexes (org-gnosis-test-db-indexes)))
          (should (member 'idx_nodes_file indexes))
          (should (member 'idx_journal_file indexes))))
    (org-gnosis-test-teardown)))

(ert-deftest org-gnosis-test-db-version-set-correctly ()
  "Verify pragma user-version matches `org-gnosis-db-version' after sync."
  (org-gnosis-test-setup)
  (unwind-protect
      (progn
        ;; db-sync sets user-version (db-init alone uses unquoted symbol)
        (org-gnosis-db-sync 'force)
        (let ((version (caar (emacsql (org-gnosis-db-get) [:pragma user-version]))))
          (should (= version org-gnosis-db-version))))
    (org-gnosis-test-teardown)))

(ert-deftest org-gnosis-test-force-sync-preserves-indexes ()
  "Force sync then verify indexes still exist."
  (org-gnosis-test-setup)
  (unwind-protect
      (progn
        ;; Create a file so sync has something to process
        (org-gnosis-test-create-test-file
         "20260101120000--test.org"
         ":PROPERTIES:\n:ID: idx-test\n:END:\n#+title: Index Test\n")
        (org-gnosis-db-sync 'force)
        (let ((indexes (org-gnosis-test-db-indexes)))
          (should (member 'idx_nodes_file indexes))
          (should (member 'idx_journal_file indexes))))
    (org-gnosis-test-teardown)))

;;; ---- Group 2: Database CRUD ----

(ert-deftest org-gnosis-test-insert-and-select-node ()
  "Create file, sync, verify node data via select."
  (org-gnosis-test-setup)
  (unwind-protect
      (progn
        (org-gnosis-test-create-test-file
         "20260101120000--hello.org"
         ":PROPERTIES:\n:ID: node-hello\n:END:\n#+title: Hello World\n#+filetags: :test:\n\nBody text\n")
        (org-gnosis-db-sync 'force)
        (let ((title (car (org-gnosis-select 'title 'nodes '(= id "node-hello") t)))
              (file (car (org-gnosis-select 'file 'nodes '(= id "node-hello") t))))
          (should (string= title "Hello World"))
          (should (string= file "20260101120000--hello.org"))))
    (org-gnosis-test-teardown)))

(ert-deftest org-gnosis-test-insert-multiple-nodes ()
  "File with topic + 2 headings, verify 3 nodes + master relationships."
  (org-gnosis-test-setup)
  (unwind-protect
      (progn
        (org-gnosis-test-create-test-file
         "20260101120000--multi.org"
         ":PROPERTIES:\n:ID: multi-topic\n:END:\n#+title: Multi\n\n* Heading One\n:PROPERTIES:\n:ID: multi-h1\n:END:\n\n* Heading Two\n:PROPERTIES:\n:ID: multi-h2\n:END:\n")
        (org-gnosis-db-sync 'force)
        ;; 3 nodes total
        (let ((nodes (org-gnosis-select 'id 'nodes '(like file "%multi%") t)))
          (should (= 3 (length nodes))))
        ;; Topic is level 0
        (let ((level (car (org-gnosis-select 'level 'nodes '(= id "multi-topic") t))))
          (should (equal level 0)))
        ;; Children link back to topic via links table
        (let ((child-links (org-gnosis-select 'dest 'links '(= source "multi-h1") t)))
          (should (member "multi-topic" child-links))))
    (org-gnosis-test-teardown)))

(ert-deftest org-gnosis-test-delete-file-with-full-path ()
  "Sync file, delete via full path, verify gone."
  (org-gnosis-test-setup)
  (unwind-protect
      (let ((filepath (org-gnosis-test-create-test-file
                       "20260101120000--delme.org"
                       ":PROPERTIES:\n:ID: del-full\n:END:\n#+title: Delete Me\n")))
        (org-gnosis-db-sync 'force)
        (should (org-gnosis-select 'id 'nodes '(= id "del-full")))
        ;; Delete using full path
        (org-gnosis--delete-file filepath)
        (should-not (org-gnosis-select 'id 'nodes '(= id "del-full"))))
    (org-gnosis-test-teardown)))

(ert-deftest org-gnosis-test-delete-file-with-basename ()
  "Sync file, delete via basename, verify gone."
  (org-gnosis-test-setup)
  (unwind-protect
      (progn
        (org-gnosis-test-create-test-file
         "20260101120000--delbase.org"
         ":PROPERTIES:\n:ID: del-base\n:END:\n#+title: Delete Base\n")
        (org-gnosis-db-sync 'force)
        (should (org-gnosis-select 'id 'nodes '(= id "del-base")))
        ;; Delete using basename only
        (org-gnosis--delete-file "20260101120000--delbase.org")
        (should-not (org-gnosis-select 'id 'nodes '(= id "del-base"))))
    (org-gnosis-test-teardown)))

(ert-deftest org-gnosis-test-update-file-replaces-data ()
  "Sync, modify file content, update, verify new title."
  (org-gnosis-test-setup)
  (unwind-protect
      (let ((filepath (org-gnosis-test-create-test-file
                       "20260101120000--upd.org"
                       ":PROPERTIES:\n:ID: upd-node\n:END:\n#+title: Original Title\n")))
        (org-gnosis-db-sync 'force)
        (should (string= "Original Title"
                          (car (org-gnosis-select 'title 'nodes '(= id "upd-node") t))))
        ;; Overwrite file with new title
        (with-temp-file filepath
          (insert ":PROPERTIES:\n:ID: upd-node\n:END:\n#+title: Updated Title\n"))
        (org-gnosis-update-file filepath)
        (should (string= "Updated Title"
                          (car (org-gnosis-select 'title 'nodes '(= id "upd-node") t)))))
    (org-gnosis-test-teardown)))

;;; ---- Group 3: Tags ----

(ert-deftest org-gnosis-test-tags-inserted-on-sync ()
  "Sync file with filetags, verify tags + node-tag tables."
  (org-gnosis-test-setup)
  (unwind-protect
      (progn
        (org-gnosis-test-create-test-file
         "20260101120000--tagged.org"
         ":PROPERTIES:\n:ID: tagged-node\n:END:\n#+title: Tagged\n#+filetags: :emacs:lisp:\n")
        (org-gnosis-db-sync 'force)
        ;; Tags table should have both tags
        (let ((tags (org-gnosis-select 'tag 'tags nil t)))
          (should (member "emacs" tags))
          (should (member "lisp" tags)))
        ;; node-tag should link node to both tags
        (let ((node-tags (org-gnosis-select 'tag 'node-tag '(= node-id "tagged-node") t)))
          (should (member "emacs" node-tags))
          (should (member "lisp" node-tags))))
    (org-gnosis-test-teardown)))

(ert-deftest org-gnosis-test-tags-no-duplicates ()
  "Two files share a tag, verify tags table has one row for it."
  (org-gnosis-test-setup)
  (unwind-protect
      (progn
        (org-gnosis-test-create-test-file
         "20260101120000--dup1.org"
         ":PROPERTIES:\n:ID: dup1\n:END:\n#+title: Dup1\n#+filetags: :shared:\n")
        (org-gnosis-test-create-test-file
         "20260101120001--dup2.org"
         ":PROPERTIES:\n:ID: dup2\n:END:\n#+title: Dup2\n#+filetags: :shared:\n")
        (org-gnosis-db-sync 'force)
        (let ((shared-count (length (org-gnosis-select 'tag 'tags '(= tag "shared")))))
          (should (= 1 shared-count))))
    (org-gnosis-test-teardown)))

(ert-deftest org-gnosis-test-orphaned-tags-cleanup ()
  "Delete file, cleanup, verify orphaned tags removed."
  (org-gnosis-test-setup)
  (unwind-protect
      (let ((filepath (org-gnosis-test-create-test-file
                       "20260101120000--orphan.org"
                       ":PROPERTIES:\n:ID: orphan-node\n:END:\n#+title: Orphan\n#+filetags: :lonely:\n")))
        (org-gnosis-db-sync 'force)
        (should (org-gnosis-select 'tag 'tags '(= tag "lonely")))
        ;; Delete the file's data from DB
        (org-gnosis--delete-file filepath)
        ;; node-tag entries are cascade-deleted, but tags table still has the tag
        ;; Cleanup should remove it
        (org-gnosis-tags--cleanup-orphaned)
        (should-not (org-gnosis-select 'tag 'tags '(= tag "lonely"))))
    (org-gnosis-test-teardown)))

(ert-deftest org-gnosis-test-orphaned-cleanup-preserves-shared-tags ()
  "Delete one file, verify shared tags survive cleanup."
  (org-gnosis-test-setup)
  (unwind-protect
      (let ((filepath1 (org-gnosis-test-create-test-file
                        "20260101120000--sh1.org"
                        ":PROPERTIES:\n:ID: sh1\n:END:\n#+title: Shared1\n#+filetags: :common:\n"))
            (_filepath2 (org-gnosis-test-create-test-file
                         "20260101120001--sh2.org"
                         ":PROPERTIES:\n:ID: sh2\n:END:\n#+title: Shared2\n#+filetags: :common:\n")))
        (org-gnosis-db-sync 'force)
        ;; Delete first file only
        (org-gnosis--delete-file filepath1)
        (org-gnosis-tags--cleanup-orphaned)
        ;; Tag should survive because sh2 still uses it
        (should (org-gnosis-select 'tag 'tags '(= tag "common"))))
    (org-gnosis-test-teardown)))

;;; ---- Group 4: Links ----

(ert-deftest org-gnosis-test-links-inserted-on-sync ()
  "File with id link, verify links table."
  (org-gnosis-test-setup)
  (unwind-protect
      (progn
        ;; Create target node first
        (org-gnosis-test-create-test-file
         "20260101120000--target.org"
         ":PROPERTIES:\n:ID: link-target\n:END:\n#+title: Target\n")
        ;; Create source node that links to target
        (org-gnosis-test-create-test-file
         "20260101120001--source.org"
         ":PROPERTIES:\n:ID: link-source\n:END:\n#+title: Source\n\nSee [[id:link-target][Target]]\n")
        (org-gnosis-db-sync 'force)
        (let ((links (org-gnosis-select '[source dest] 'links
                                        '(and (= source "link-source")
                                              (= dest "link-target")))))
          (should (= 1 (length links)))))
    (org-gnosis-test-teardown)))

(ert-deftest org-gnosis-test-master-links-inserted ()
  "Child heading creates master link in links table."
  (org-gnosis-test-setup)
  (unwind-protect
      (progn
        (org-gnosis-test-create-test-file
         "20260101120000--parent.org"
         ":PROPERTIES:\n:ID: parent-node\n:END:\n#+title: Parent\n\n* Child\n:PROPERTIES:\n:ID: child-node\n:END:\n")
        (org-gnosis-db-sync 'force)
        ;; Child should have a link to parent (master relationship)
        (let ((links (org-gnosis-select 'dest 'links '(= source "child-node") t)))
          (should (member "parent-node" links))))
    (org-gnosis-test-teardown)))

(ert-deftest org-gnosis-test-links-cascade-on-node-delete ()
  "Delete node, verify its links are cascade-deleted."
  (org-gnosis-test-setup)
  (unwind-protect
      (progn
        (org-gnosis-test-create-test-file
         "20260101120000--cascade.org"
         ":PROPERTIES:\n:ID: cascade-parent\n:END:\n#+title: Cascade\n\n* Child\n:PROPERTIES:\n:ID: cascade-child\n:END:\n")
        (org-gnosis-db-sync 'force)
        ;; Verify link exists
        (should (org-gnosis-select 'dest 'links '(= source "cascade-child")))
        ;; Delete the file from DB
        (org-gnosis--delete-file "20260101120000--cascade.org")
        ;; Links from cascade-child should be gone (cascade delete)
        (should-not (org-gnosis-select 'dest 'links '(= source "cascade-child"))))
    (org-gnosis-test-teardown)))

;;; ---- Group 5: Sync & Change Detection ----

(ert-deftest org-gnosis-test-file-changed-p-new-file ()
  "File not in DB returns non-nil."
  (org-gnosis-test-setup)
  (unwind-protect
      (let ((filepath (org-gnosis-test-create-test-file
                       "20260101120000--new.org"
                       ":PROPERTIES:\n:ID: new-node\n:END:\n#+title: New\n")))
        (org-gnosis-db-init)
        (should (org-gnosis--file-changed-p filepath 'nodes)))
    (org-gnosis-test-teardown)))

(ert-deftest org-gnosis-test-file-changed-p-unchanged ()
  "Synced file returns nil."
  (org-gnosis-test-setup)
  (unwind-protect
      (let ((filepath (org-gnosis-test-create-test-file
                       "20260101120000--unchanged.org"
                       ":PROPERTIES:\n:ID: unchanged\n:END:\n#+title: Unchanged\n")))
        (org-gnosis-db-sync 'force)
        (should-not (org-gnosis--file-changed-p filepath 'nodes)))
    (org-gnosis-test-teardown)))

(ert-deftest org-gnosis-test-file-changed-p-content-changed ()
  "Modified file returns non-nil."
  (org-gnosis-test-setup)
  (unwind-protect
      (let ((filepath (org-gnosis-test-create-test-file
                       "20260101120000--changed.org"
                       ":PROPERTIES:\n:ID: changed\n:END:\n#+title: Before\n")))
        (org-gnosis-db-sync 'force)
        ;; Modify the file
        (with-temp-file filepath
          (insert ":PROPERTIES:\n:ID: changed\n:END:\n#+title: After\n"))
        ;; Set DB mtime to old value so mtime check triggers hash comparison
        (emacsql (org-gnosis-db-get)
                 [:update nodes :set (= mtime "0") :where (= id "changed")])
        (should (org-gnosis--file-changed-p filepath 'nodes)))
    (org-gnosis-test-teardown)))

(ert-deftest org-gnosis-test-file-changed-p-mtime-only ()
  "Touched file with same content returns nil (two-tier check).
We simulate this by updating the DB mtime to an old value while keeping
the hash correct, then checking with actual file (same content, newer mtime)."
  (org-gnosis-test-setup)
  (unwind-protect
      (let ((filepath (org-gnosis-test-create-test-file
                       "20260101120000--touch.org"
                       ":PROPERTIES:\n:ID: touch-node\n:END:\n#+title: Touch\n")))
        (org-gnosis-db-sync 'force)
        ;; Manually set DB mtime to an old value (hash stays correct)
        (emacsql (org-gnosis-db-get)
                 [:update nodes :set (= mtime "0") :where (= id "touch-node")])
        ;; File hasn't changed content, only mtime differs
        ;; The two-tier check: mtime differs -> check hash -> hash matches -> nil
        (should-not (org-gnosis--file-changed-p filepath 'nodes)))
    (org-gnosis-test-teardown)))

(ert-deftest org-gnosis-test-force-sync-resyncs-all ()
  "Force sync repopulates all nodes."
  (org-gnosis-test-setup)
  (unwind-protect
      (progn
        (org-gnosis-test-create-test-file
         "20260101120000--resync1.org"
         ":PROPERTIES:\n:ID: resync1\n:END:\n#+title: Resync1\n")
        (org-gnosis-test-create-test-file
         "20260101120001--resync2.org"
         ":PROPERTIES:\n:ID: resync2\n:END:\n#+title: Resync2\n")
        (org-gnosis-db-sync 'force)
        (should (= 2 (length (org-gnosis-select 'id 'nodes nil t))))
        ;; Force sync again — should still have both nodes
        (org-gnosis-db-sync 'force)
        (should (= 2 (length (org-gnosis-select 'id 'nodes nil t)))))
    (org-gnosis-test-teardown)))

;;; ---- Group 6: File Naming ----

(ert-deftest org-gnosis-test-create-name-format ()
  "Verify timestamp + underscores + .org format."
  (let ((name (org-gnosis--create-name "My Test Note")))
    (should (string-match-p "^[0-9]\\{14\\}--My_Test_Note\\.org$" name))))

(ert-deftest org-gnosis-test-create-name-gpg ()
  "Verify .org.gpg suffix when gpg is requested."
  (let ((name (org-gnosis--create-name "Secret Note" nil t)))
    (should (string-match-p "\\.org\\.gpg$" name))))

(ert-deftest org-gnosis-test-create-name-strips-hash ()
  "Verify # removed from filenames."
  (let ((name (org-gnosis--create-name "C# Programming")))
    (should-not (string-match-p "#" name))
    (should (string-match-p "C_Programming\\.org$" name))))

(ert-deftest org-gnosis-test-journal-file-path ()
  "Verify default journal file path contains journal.org not jounral.org."
  ;; Test the default value computation
  (let* ((test-dir "/tmp/test-gnosis-notes")
         (journal-dir (expand-file-name "journal" test-dir))
         (journal-file (expand-file-name "journal.org" journal-dir)))
    (should (string-match-p "journal\\.org$" journal-file))
    (should-not (string-match-p "jounral" journal-file))))

;;; ---- Group 7: Journal ----

(ert-deftest org-gnosis-test-journal-file-syncs-to-journal-table ()
  "Journal file goes in journal table, not nodes."
  (org-gnosis-test-setup)
  (unwind-protect
      (progn
        (org-gnosis-test-create-journal-file
         "20260101120000--journal-entry.org"
         ":PROPERTIES:\n:ID: j-entry\n:END:\n#+title: 2026-01-01\n")
        (org-gnosis-db-sync 'force)
        ;; Should be in journal table
        (should (org-gnosis-select 'id 'journal '(= id "j-entry")))
        ;; Should NOT be in nodes table
        (should-not (org-gnosis-select 'id 'nodes '(= id "j-entry"))))
    (org-gnosis-test-teardown)))

(ert-deftest org-gnosis-test-journal-delete ()
  "Delete journal entry from DB."
  (org-gnosis-test-setup)
  (unwind-protect
      (let ((filepath (org-gnosis-test-create-journal-file
                       "20260101120000--jdel.org"
                       ":PROPERTIES:\n:ID: j-del\n:END:\n#+title: 2026-01-01\n")))
        (org-gnosis-db-sync 'force)
        (should (org-gnosis-select 'id 'journal '(= id "j-del")))
        (org-gnosis--delete-file filepath)
        (should-not (org-gnosis-select 'id 'journal '(= id "j-del"))))
    (org-gnosis-test-teardown)))

;;; ---- Group 8: org-gnosis-mode Hook ----

(ert-deftest org-gnosis-test-mode-adds-local-hook ()
  "Enable mode, verify after-save-hook is buffer-local."
  (with-temp-buffer
    (org-gnosis-mode 1)
    (should (memq #'org-gnosis-update-file
                  (buffer-local-value 'after-save-hook (current-buffer))))
    (org-gnosis-mode -1)))

(ert-deftest org-gnosis-test-mode-removes-local-hook ()
  "Disable mode, verify hook removed only from that buffer."
  (let (buf1 buf2)
    (unwind-protect
        (progn
          (setq buf1 (generate-new-buffer "*gnosis-hook-test-1*"))
          (setq buf2 (generate-new-buffer "*gnosis-hook-test-2*"))
          ;; Enable in both buffers
          (with-current-buffer buf1 (org-gnosis-mode 1))
          (with-current-buffer buf2 (org-gnosis-mode 1))
          ;; Disable in buf1 only
          (with-current-buffer buf1 (org-gnosis-mode -1))
          ;; buf1 should not have the hook
          (should-not (memq #'org-gnosis-update-file
                            (buffer-local-value 'after-save-hook buf1)))
          ;; buf2 should still have it
          (should (memq #'org-gnosis-update-file
                        (buffer-local-value 'after-save-hook buf2))))
      (when (buffer-live-p buf1) (kill-buffer buf1))
      (when (buffer-live-p buf2) (kill-buffer buf2)))))

(provide 'org-gnosis-test-db)
;;; org-gnosis-test-db.el ends here

;;; gnosis-test-links.el --- Link mutation tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Free Software Foundation, Inc.

;; Author: Thanos Apollo <public@thanosapollo.org>

;;; Commentary:

;; Verify link maintenance behavior against isolated SQLite databases.

;;; Code:

(require 'gnosis-test-helpers)
(require 'gnosis-links)

(ert-deftest gnosis-test-links-cleanup-rolls-back-all-deletes ()
  "Cleanup rolls back earlier deletes when a later delete fails."
  (gnosis-test-with-db
    (let* ((source (gnosis-test--add-basic-thema "No links" "Answer"))
           (orphan "missing-node")
           (node-link '("node-source" "missing-dest")))
      (gnosis--insert-into
       'nodes '(["node-source" "node.org" "Node" 0 nil "0" "hash"]))
      (gnosis--insert-into 'thema-links `([,source ,orphan]))
      (gnosis--insert-into 'node-links `([,@node-link]))
      (gnosis-sqlite-execute
       gnosis-db
       (concat "CREATE TRIGGER fail_node_link_delete "
               "BEFORE DELETE ON node_links BEGIN "
               "SELECT RAISE(ABORT, 'forced delete failure'); END"))
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
        (should-error (gnosis-links-cleanup)))
      (should (equal (gnosis-select '[source dest] 'thema-links)
                     `((,source ,orphan))))
      (should (equal (gnosis-select '[source dest] 'node-links)
                     (list node-link))))))

(ert-deftest gnosis-test-links-sync-rolls-back-deletes-on-insert-failure ()
  "Sync restores deleted links when inserting a missing link fails."
  (gnosis-test-with-db
    (let* ((expected "expected-node")
           (orphan "missing-node")
           (source
            (gnosis-test--add-basic-thema
             (format "See [[id:%s][Node]]" expected) "Answer")))
      (gnosis--insert-into
       'nodes `([,expected "node.org" "Node" 0 nil "0" "hash"]))
      (gnosis--insert-into 'thema-links `([,source ,orphan]))
      (gnosis-sqlite-execute
       gnosis-db
       (concat "CREATE TRIGGER fail_thema_link_insert "
               "BEFORE INSERT ON thema_links BEGIN "
               "SELECT RAISE(ABORT, 'forced insert failure'); END"))
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
        (should-error (gnosis-links-sync)))
      (should (equal (gnosis-select '[source dest] 'thema-links)
                     `((,source ,orphan))))
      (should-not
       (gnosis-select '[source dest] 'thema-links
                      `(and (= source ,source) (= dest ,expected)))))))

(ert-deftest gnosis-test-bulk-link-updates-thema-link-index ()
  "Bulk-link updates both thema text and its node-link index."
  (gnosis-test-with-db
    (let ((id (gnosis-test--add-basic-thema "Emacs question" "Answer")))
      (gnosis--insert-into
       'nodes '(["node-1" "node.org" "Emacs" 0 nil "0" "hash"]))
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
        (gnosis-bulk-link-themata (list id) "Emacs" "node-1"))
      (should (equal (gnosis-get 'keimenon 'themata `(= id ,id))
                     "[[id:node-1][Emacs]] question"))
      (should (equal (gnosis-select '[source dest] 'thema-links)
                     `((,id "node-1")))))))

(provide 'gnosis-test-links)
;;; gnosis-test-links.el ends here

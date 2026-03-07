;;; gnosis-test-helpers.el --- Shared test infrastructure  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Free Software Foundation, Inc.

;; Author: Thanos Apollo <public@thanosapollo.org>

;;; Commentary:

;; Shared macros and helper functions used across gnosis test suites.
;; Load this file in each test file that needs `gnosis-test-with-db',
;; `gnosis-test--add-deck', or `gnosis-test--add-basic-thema'.

;;; Code:

(require 'gnosis)

(let ((parent-dir (file-name-directory
                   (directory-file-name
                    (file-name-directory (or load-file-name default-directory))))))
  (add-to-list 'load-path parent-dir))

(defvar gnosis-test--db-file nil
  "Path to temporary test database file.")

(defmacro gnosis-test-with-db (&rest body)
  "Run BODY with a fresh temporary gnosis database.
Rebinds `gnosis-db' and initialises the schema."
  (declare (indent 0) (debug t))
  `(let* ((gnosis-test--db-file (make-temp-file "gnosis-test-" nil ".db"))
          (gnosis-db (gnosis-sqlite-open gnosis-test--db-file))
          (gnosis--id-cache nil))
     (unwind-protect
         (progn
           (gnosis-sqlite-with-transaction gnosis-db
             (pcase-dolist (`(,table ,schema) gnosis-db--schemata)
               (gnosis-sqlite-execute gnosis-db
                 (format "CREATE TABLE %s (%s)"
                         (gnosis-sqlite--ident table)
                         (gnosis-sqlite--compile-schema schema)))))
           ,@body)
       (gnosis-sqlite-close gnosis-db)
       (delete-file gnosis-test--db-file))))

(defun gnosis-test--add-deck (name)
  "Add a deck with NAME to the test DB.  Return its id."
  (let ((id (+ (random 90000) 10000)))
    (gnosis--insert-into 'decks `([,id ,name]))
    id))

(defun gnosis-test--add-basic-thema (deck-id keimenon answer
                                     &optional tags parathema thema-id suspend)
  "Insert a basic thema into the test DB.  Return its id.
DECK-ID, KEIMENON, ANSWER are required.
TAGS defaults to (\"test\"), PARATHEMA to \"\".
SUSPEND: 1 to suspend, 0 or nil for active."
  (let* ((id (or thema-id (gnosis-generate-id)))
         (tags (or tags '("test")))
         (parathema (or parathema ""))
         (suspend (or suspend 0))
         (hypothesis '(""))
         (answer (if (listp answer) answer (list answer))))
    (gnosis-sqlite-with-transaction gnosis-db
      (gnosis--insert-into 'themata `([,id "basic" ,keimenon ,hypothesis
                                           ,answer ,tags ,deck-id]))
      (gnosis--insert-into 'review `([,id ,gnosis-algorithm-gnosis-value
                                          ,gnosis-algorithm-amnesia-value]))
      (gnosis--insert-into 'review-log `([,id ,(gnosis-algorithm-date)
                                              ,(gnosis-algorithm-date) 0 0 0 0
                                              ,suspend 0]))
      (gnosis--insert-into 'extras `([,id ,parathema ""]))
      (cl-loop for tag in tags
	       do (gnosis--insert-into 'thema-tag `([,id ,tag]))))
    id))

(provide 'gnosis-test-helpers)
;;; gnosis-test-helpers.el ends here

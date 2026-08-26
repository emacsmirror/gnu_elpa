;;; gnosis-test-helpers.el --- Shared test infrastructure  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Free Software Foundation, Inc.

;; Author: Thanos Apollo <public@thanosapollo.org>

;;; Commentary:

;; Shared macros and helper functions used across gnosis test suites.
;; Load this file in each test file that needs `gnosis-test-with-db'
;; or `gnosis-test--add-basic-thema'.

;;; Code:

(require 'gnosis)

(let ((lisp-dir (expand-file-name "../lisp"
                  (file-name-directory (or load-file-name default-directory)))))
  (add-to-list 'load-path lisp-dir))

(defvar gnosis-test--db-file nil
  "Path to temporary test database file.")

(defvar gnosis-test--dir nil
  "Path to the temporary directory owned by the current test fixture.")

(defmacro gnosis-test-with-db (&rest body)
  "Run BODY with an isolated temporary Gnosis environment.
Rebind the database, data directory, and version-control guards."
  (declare (indent 0) (debug t))
  `(let* ((gnosis-test--dir (make-temp-file "gnosis-test-" t))
          (gnosis-dir (file-name-as-directory gnosis-test--dir))
          (gnosis-test--db-file (expand-file-name "gnosis.db" gnosis-dir))
          (gnosis-db (gnosis-sqlite-open gnosis-test--db-file))
          (gnosis-testing t)
          (gnosis-vc-auto-push nil)
          (gnosis--id-cache nil))
     (unwind-protect
         (unwind-protect
             (progn
               (gnosis-db-init)
               ,@body)
           (gnosis-sqlite-close gnosis-db))
       (delete-directory gnosis-test--dir t))))

(defun gnosis-test--add-basic-thema (keimenon answer
                                     &optional tags parathema thema-id suspend)
  "Insert a basic thema into the test DB.  Return its id.
KEIMENON, ANSWER are required.
TAGS defaults to (\"test\"), PARATHEMA to \"\".
SUSPEND: 1 to suspend, 0 or nil for active."
  (let* ((id (or thema-id (gnosis-generate-id)))
         (tags (or tags '("test")))
         (parathema (or parathema ""))
         (suspend (or suspend 0))
         (today (gnosis--today-int))
         (hypothesis '(""))
         (answer (if (listp answer) answer (list answer))))
    (gnosis-sqlite-with-transaction gnosis-db
      (gnosis--insert-into 'themata `([,id "basic" ,keimenon ,hypothesis
                                           ,answer nil]))
      (gnosis--insert-into 'review `([,id ,gnosis-algorithm-gnosis-value
                                          ,gnosis-algorithm-amnesia-value]))
      (gnosis--insert-into 'review-log `([,id ,today
                                              ,today 0 0 0 0
                                              ,suspend 0]))
      (gnosis-scheduler-initialize-thema id today suspend)
      (gnosis--insert-into 'extras `([,id ,parathema ""]))
      (cl-loop for tag in tags
	       do (gnosis--insert-into 'thema-tag `([,id ,tag]))))
    id))

(provide 'gnosis-test-helpers)
;;; gnosis-test-helpers.el ends here

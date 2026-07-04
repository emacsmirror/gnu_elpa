;;; gnosis-test-helpers.el --- Shared test infrastructure  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Free Software Foundation, Inc.

;; Author: Thanos Apollo <public@thanosapollo.org>

;;; Commentary:

;; Shared macros and helper functions used across gnosis test suites.
;; Load this file in each test file that needs `gnosis-test-with-db'
;; or `gnosis-test--add-basic-thema'.

;;; Code:

(require 'cl-lib)
(require 'gnosis)

(let ((lisp-dir (expand-file-name "../lisp"
                  (file-name-directory (or load-file-name default-directory)))))
  (add-to-list 'load-path lisp-dir))

(defvar gnosis-test--db-file nil
  "Path to temporary test database file.")

(defvar gnosis-test--dir nil
  "Path to temporary test Gnosis directory.")

(defvar gnosis-test--timers nil
  "Timers scheduled during the current test body.")

(defmacro gnosis-test-with-db (&rest body)
  "Run BODY with a fresh temporary gnosis database.
Rebinds `gnosis-db' and initialises the schema."
  (declare (indent 0) (debug t))
  `(let* ((gnosis-test--db-file (make-temp-file "gnosis-test-" nil ".db"))
          (gnosis-test--dir (make-temp-file "gnosis-test-dir-" t))
          (user-emacs-directory
           (file-name-as-directory
            (expand-file-name "emacs.d" gnosis-test--dir)))
          (gnosis-dir (file-name-as-directory gnosis-test--dir))
          (gnosis-nodes-dir
           (file-name-as-directory
            (expand-file-name "nodes" gnosis-test--dir)))
          (gnosis-journal-dir
           (file-name-as-directory
            (expand-file-name "journal" gnosis-nodes-dir)))
          (gnosis-testing t)
          (gnosis-vc-auto-push nil)
          (gnosis-db (gnosis-sqlite-open gnosis-test--db-file))
          (gnosis--id-cache nil)
          (gnosis-test--timers nil))
     (make-directory user-emacs-directory t)
     (make-directory gnosis-dir t)
     (make-directory gnosis-nodes-dir t)
     (make-directory gnosis-journal-dir t)
     (let ((gnosis-test--run-with-timer (symbol-function 'run-with-timer)))
       (cl-letf (((symbol-function 'gnosis--ensure-git-repo) #'ignore)
                 ((symbol-function 'gnosis--git-chain)
                  (lambda (&rest _) nil))
                 ((symbol-function 'gnosis-vc-push) #'ignore)
                 ((symbol-function 'run-with-timer)
                  (lambda (secs repeat function &rest args)
                    (let ((timer (apply gnosis-test--run-with-timer
                                        secs repeat function args)))
                      (push timer gnosis-test--timers)
                      timer))))
         (unwind-protect
             (progn
               (gnosis-sqlite-with-transaction gnosis-db
                 (pcase-dolist (`(,table ,schema) gnosis-db--schemata)
                   (gnosis-sqlite-execute gnosis-db
                     (format "CREATE TABLE %s (%s)"
                             (gnosis-sqlite--ident table)
                             (gnosis-sqlite--compile-schema schema)))))
               ,@body)
           (mapc #'cancel-timer gnosis-test--timers)
           (gnosis-sqlite-close gnosis-db)
           (when (file-exists-p gnosis-test--db-file)
             (delete-file gnosis-test--db-file))
           (when (file-directory-p gnosis-test--dir)
             (delete-directory gnosis-test--dir t)))))))

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
         (hypothesis '(""))
         (answer (if (listp answer) answer (list answer))))
    (gnosis-sqlite-with-transaction gnosis-db
      (gnosis--insert-into 'themata `([,id "basic" ,keimenon ,hypothesis
                                           ,answer nil]))
      (gnosis--insert-into 'review `([,id ,gnosis-algorithm-gnosis-value
                                          ,gnosis-algorithm-amnesia-value]))
      (gnosis--insert-into 'review-log `([,id ,(gnosis--today-int)
                                              ,(gnosis--today-int) 0 0 0 0
                                              ,suspend 0]))
      (gnosis--insert-into 'extras `([,id ,parathema ""]))
      (cl-loop for tag in tags
	       do (gnosis--insert-into 'thema-tag `([,id ,tag]))))
    id))

(provide 'gnosis-test-helpers)
;;; gnosis-test-helpers.el ends here

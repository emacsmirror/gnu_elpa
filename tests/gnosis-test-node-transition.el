;;; gnosis-test-node-transition.el --- Journal owner transitions -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;;; Commentary:
;; Both old and current native writers must survive journal reclassification.

;;; Code:

(require 'gnosis-test-node-retained)

(defmacro gnosis-test-node-transition--with-owner (writer relative namesake &rest body)
  "Run BODY after WRITER saves RELATIVE, with an ordinary NAMESAKE.
Bind OWNER, OTHER and SOURCE filenames and preserve unrelated source data."
  (declare (indent 3) (debug t))
  `(gnosis-test-node-paths--with-vault
     (mapc #'delete-file (list a b j))
     (let* ((owner (expand-file-name ,relative gnosis-nodes-dir))
            (other (expand-file-name ,namesake gnosis-nodes-dir))
            (source (expand-file-name "source.org" gnosis-nodes-dir))
            (gnosis-journal-todo-files (list source)))
       (make-directory (file-name-directory owner) t)
       (make-directory (file-name-directory other) t)
       (with-temp-file owner
         (insert ":PROPERTIES:\n:ID: owner\n:END:\n#+title: Owner\n#+filetags: :owner_tag:\n[[id:source]]\n* "
                 (format-time-string "%Y-%m-%d")
                 "\n+ [X] Exercise\n* Removed\n:PROPERTIES:\n:ID: removed\n:END:\n"))
       (funcall ,writer owner)
       (should (equal (if (eq ,writer #'gnosis-test-node-retained--save)
                         "same.org" ,relative)
                      (gnosis-get 'file 'nodes '(= id "owner"))))
       ;; Retain the real ordinary index before changing configuration.
       (setq gnosis-journal-file owner)
       (with-temp-file other
         (insert ":PROPERTIES:\n:ID: other\n:END:\n#+title: Other\n#+filetags: :other_tag:\n"))
       (with-temp-file source
         (insert ":PROPERTIES:\n:ID: source\n:END:\n#+title: Source\n#+filetags: :source_tag:\n[[id:owner]] [[id:removed]] [[id:other]]\n* TODO Exercise\n"))
       (gnosis-test-node-retained--native-save source)
       (let ((source-bytes (gnosis-test-node-paths--bytes source))
             (source-row (gnosis-select '* 'nodes '(= id "source")))
             (source-tags (gnosis-select '* 'node-tag '(= node-id "source"))))
         ,@body
         (should (equal source-bytes (gnosis-test-node-paths--bytes source)))
         (should (equal source-row (gnosis-select '* 'nodes '(= id "source"))))
         (should (equal source-tags (gnosis-select '* 'node-tag '(= node-id "source"))))))))

(defun gnosis-test-node-transition--adopted (owner)
  "Assert OWNER has journal rows and no orphan ordinary rows or junctions."
  (should-not (gnosis-select '* 'nodes '(in id ["owner" "removed"])))
  (should (equal (gnosis-nodes--file-key owner t)
                 (gnosis-get 'file 'journal '(= id "owner"))))
  (should (equal '("owner_tag")
                 (read (gnosis-get 'tags 'journal '(= id "owner")))))
  (should-not (gnosis-select '* 'node-tag '(in node-id ["owner" "removed"])))
  (should-not (gnosis-select '* 'node-links '(in source ["owner" "removed"])))
  (should (gnosis-select '* 'node-links '(and (= source "source") (= dest "owner")))))

(ert-deftest gnosis-test-node-transition-save-sync-adoption ()
  "Both representations transfer through native save, sync and namesake writes."
  (dolist (writer '(gnosis-test-node-retained--native-save gnosis-test-node-retained--save))
    (dolist (paths '(("sub/same.org" "same.org")
                     ("sub/same.org" "other/same.org")
                     ("same.org" "other/same.org")))
      (dolist (action '(save sync index-only namesake))
        (gnosis-test-node-transition--with-owner writer (car paths) (cadr paths)
          (let ((bytes (gnosis-test-node-paths--bytes owner))
                (incoming (gnosis-select '* 'node-links '(= source "source")))
                (todo-calls 0))
            (cl-letf (((symbol-function 'gnosis-journal--update-todos)
                       (lambda (file) (should (equal file owner))
                         (setq todo-calls (1+ todo-calls)))))
              (pcase action
                ('save (gnosis-test-node-retained--native-save owner))
                ('sync (gnosis-nodes-db-sync))
                ('index-only (gnosis-nodes-update-file owner t))
                ('namesake (gnosis-test-node-retained--native-save other))))
            (should (= todo-calls 0))
            (unless (eq action 'save)
              (should (equal bytes (gnosis-test-node-paths--bytes owner))))
            (should (equal incoming (gnosis-select '* 'node-links '(= source "source"))))
            (gnosis-test-node-transition--adopted owner)))))))

(ert-deftest gnosis-test-node-transition-delete ()
  "Delete both representations before or after journal save, without orphans."
  (dolist (writer '(gnosis-test-node-retained--native-save gnosis-test-node-retained--save))
    (dolist (relative '("same.org" "sub/same.org"))
      (dolist (adopted '(nil t))
        (gnosis-test-node-transition--with-owner writer relative "other/same.org"
          (when adopted (gnosis-nodes-update-file owner t))
          (let ((bytes (gnosis-test-node-paths--bytes other)))
            (switch-to-buffer (find-file-noselect owner))
            (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
              (call-interactively #'gnosis-nodes-delete-file))
            (should-not (file-exists-p owner))
            (should-not (gnosis-select '* 'nodes '(in id ["owner" "removed"])))
            (should-not (gnosis-select '* 'journal))
            (should-not (gnosis-select '* 'node-tag '(in node-id ["owner" "removed"])))
            (should-not (gnosis-select '* 'node-links '(in dest ["owner" "removed"])))
            (should (equal bytes (gnosis-test-node-paths--bytes other)))
            (should (gnosis-select '* 'node-links '(= dest "other")))))))))

(ert-deftest gnosis-test-node-transition-sql-rollback-retry ()
  "Real SQL faults restore both owner representations, including detached retry."
  (dolist (writer '(gnosis-test-node-retained--native-save gnosis-test-node-retained--save))
    (dolist (action '(save delete))
      (gnosis-test-node-transition--with-owner writer "sub/same.org" "same.org"
        (let ((before (gnosis-test-node-paths--index))
              (bytes (gnosis-test-node-paths--bytes other)))
          (switch-to-buffer (find-file-noselect owner))
          (sqlite-execute gnosis-db
                          "CREATE TRIGGER fail_transition AFTER DELETE ON nodes WHEN OLD.id = '\"owner\"' BEGIN SELECT RAISE(ABORT, 'transition fault'); END")
          (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
            (should (string-match-p
                     "transition fault"
                     (error-message-string
                      (should-error
                       (if (eq action 'save)
                           (gnosis-test-node-retained--native-save owner)
                         (call-interactively #'gnosis-nodes-delete-file)))))))
          (should (equal before (gnosis-test-node-paths--index)))
          (when (eq action 'delete)
            (should-not (file-exists-p owner))
            (should-not buffer-file-name)
            (should (equal owner (car gnosis-nodes--deleted-file)))
            (should (string-match-p "ID: owner" (buffer-string))))
          (sqlite-execute gnosis-db "DROP TRIGGER fail_transition")
          (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
            (if (eq action 'save)
                (progn (gnosis-nodes-update-file owner t)
                       (gnosis-test-node-transition--adopted owner))
              (call-interactively #'gnosis-nodes-delete-file)
              (should-not (gnosis-select '* 'nodes '(in id ["owner" "removed"])))
              (should-not (gnosis-select '* 'journal))))
          (should (equal bytes (gnosis-test-node-paths--bytes other))))))))

(ert-deftest gnosis-test-node-transition-explicit-replaced-ids ()
  "An explicit descendant owner retires even wholly replaced snapshot IDs."
  (gnosis-test-node-transition--with-owner
      #'gnosis-test-node-retained--native-save "sub/same.org" "same.org"
    (with-current-buffer (find-file-noselect owner)
      (erase-buffer)
      (insert ":PROPERTIES:\n:ID: replacement\n:END:\n#+title: Replacement\n")
      (call-interactively #'save-buffer))
    (should-not (gnosis-select '* 'nodes '(in id ["owner" "removed"])))
    (should-not (gnosis-select '* 'node-links '(in dest ["owner" "removed"])))
    (should (gnosis-select '* 'journal '(= id "replacement")))))

(ert-deftest gnosis-test-node-transition-reopened-missing-file ()
  "A fresh process reconciles explicit ownership after deletion and SQL failure."
  (dolist (adopted '(nil t))
    (gnosis-test-node-transition--with-owner
        #'gnosis-test-node-retained--native-save "sub/same.org" "same.org"
      ;; Index the root namesake too: retry must retain its exact row and tags.
      ;; Do this before reclassification so it cannot itself adopt OWNER.
      (let ((gnosis-journal-file nil))
        (gnosis-test-node-retained--native-save other))
      (when adopted (gnosis-nodes-update-file owner t))
      (let ((before (gnosis-test-node-paths--index))
            (other-row (gnosis-select '* 'nodes '(= id "other")))
            (other-tags (gnosis-select '* 'node-tag '(= node-id "other")))
            (other-bytes (gnosis-test-node-paths--bytes other)))
        (sqlite-execute gnosis-db
                        (format "CREATE TRIGGER fail_reopen AFTER DELETE ON %s BEGIN SELECT RAISE(ABORT, 'reopen fault'); END"
                                (if adopted "journal" "nodes")))
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
          (should (string-match-p
                   "reopen fault" (error-message-string
                                   (should-error (gnosis-nodes-delete-file owner))))))
        (should-not (file-exists-p owner))
        (should (equal before (gnosis-test-node-paths--index)))
        (sqlite-execute gnosis-db "DROP TRIGGER fail_reopen")
        (gnosis-sqlite-close gnosis-db)
        (unwind-protect
            (with-temp-buffer
              (let* ((print-length nil)
                     (print-level nil)
                     (form
                      `(progn
                         (setq load-path ',load-path
                               gnosis-dir ,gnosis-dir gnosis-testing t
                               gnosis-vc-auto-push nil
                               gnosis-nodes-dir ,gnosis-nodes-dir
                               gnosis-journal-dir ,gnosis-journal-dir
                               gnosis-journal-file ,owner
                               org-id-track-globally nil)
                         (require 'gnosis)
                         (require 'gnosis-journal)
                         (gnosis--ensure-db)
                         (unless (equal ',before
                                        (mapcar (lambda (table) (gnosis-select '* table))
                                                '(nodes journal node-tag node-links)))
                           (error "Reopen did not retain the failed transaction"))
                         (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
                           (gnosis-nodes-delete-file ,owner))
                         (when (or (gnosis-select '* 'nodes '(in id ["owner" "removed"]))
                                   (gnosis-select '* 'journal)
                                   (gnosis-select '* 'node-links '(in dest ["owner" "removed"])))
                           (error "Missing-file cleanup left orphan rows"))
                         (gnosis-sqlite-close gnosis-db)))
                     (status
                      (call-process (expand-file-name invocation-name invocation-directory)
                                    nil t nil "-Q" "--batch" "--eval"
                                    (prin1-to-string form))))
                (ert-info ((buffer-string)) (should (equal status 0)))))
          (setq gnosis-db (gnosis-sqlite-open gnosis-test--db-file)))
        (should-not (gnosis-select '* 'node-tag '(in node-id ["owner" "removed"])))
        (should (equal other-row (gnosis-select '* 'nodes '(= id "other"))))
        (should (equal other-tags (gnosis-select '* 'node-tag '(= node-id "other"))))
        (should (equal other-bytes (gnosis-test-node-paths--bytes other)))))))

(ert-deftest gnosis-test-node-transition-namesake-deletion ()
  "Ordinary namesake deletion adopts the explicit journal owner atomically."
  (dolist (relative '("same.org" "other/same.org"))
    (dolist (fault '(nil t))
      (gnosis-test-node-transition--with-owner
          #'gnosis-test-node-retained--native-save "sub/same.org" relative
        (let ((gnosis-journal-file nil))
          (gnosis-test-node-retained--native-save other))
        (let ((before (gnosis-test-node-paths--index))
              (owner-bytes (gnosis-test-node-paths--bytes owner)))
          (switch-to-buffer (find-file-noselect other))
          (when fault
            (sqlite-execute gnosis-db
                            "CREATE TRIGGER fail_namesake AFTER INSERT ON journal BEGIN SELECT RAISE(ABORT, 'namesake fault'); END")
            (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
              (should-error (call-interactively #'gnosis-nodes-delete-file)))
            (should (equal before (gnosis-test-node-paths--index)))
            (should-not buffer-file-name)
            (sqlite-execute gnosis-db "DROP TRIGGER fail_namesake"))
          (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
                    ((symbol-function 'gnosis-journal--update-todos)
                     (lambda (&rest _) (ert-fail "Index adoption invoked TODO hook"))))
            (call-interactively #'gnosis-nodes-delete-file))
          (should-not (file-exists-p other))
          (should-not (gnosis-select '* 'nodes '(= id "other")))
          (should-not (gnosis-select '* 'node-tag '(= node-id "other")))
          (should-not (gnosis-select '* 'node-links '(= dest "other")))
          (should (equal owner-bytes (gnosis-test-node-paths--bytes owner)))
          (gnosis-test-node-transition--adopted owner))))))

(provide 'gnosis-test-node-transition)
;;; gnosis-test-node-transition.el ends here

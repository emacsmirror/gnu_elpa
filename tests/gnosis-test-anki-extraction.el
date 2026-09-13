;;; gnosis-test-anki-extraction.el --- Anki temporary ownership tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Free Software Foundation, Inc.

;;; Commentary:

;; Exercise extraction's temporary directory through failure and transfer.

;;; Code:

(require 'ert)
(require 'gnosis-anki)

(ert-deftest gnosis-test-anki-extraction-cleans-nonlocal-exits ()
  "Errors and quits before transfer remove only the owned directory."
  (dolist (phase '(path discovery extraction zstd fallback remove-compressed))
    (dolist (condition '(error quit))
      (ert-info ((format "Phase %s, condition %s" phase condition))
        (let* ((root (make-temp-file "gnosis-extraction-test-" t))
               (temporary-file-directory (file-name-as-directory root))
               (sentinel (expand-file-name "unrelated" root))
               (expand (symbol-function 'expand-file-name))
               (remove (symbol-function 'delete-file))
               (fault (list condition "Controlled extraction failure"))
               owned removal-failed)
          (unwind-protect
              (progn
                (with-temp-file sentinel (insert "Keep me"))
                (cl-labels
                    ((fail () (signal (car fault) (cdr fault)))
                     (payload (name)
                       (with-temp-file (expand-file-name name owned)
                         (insert "Partial collection bytes"))))
                  (cl-letf
                      (((symbol-function 'expand-file-name)
                        (lambda (name &optional dir)
                          (if (and (eq phase 'path) (equal name "source.apkg"))
                              (fail)
                            (funcall expand name dir))))
                       ((symbol-function 'executable-find)
                        (lambda (program &rest _)
                          (cond
                           ((eq phase 'discovery) (fail))
                           ((and (eq phase 'fallback) (equal program "zstd")) nil)
                           (t program))))
                       ((symbol-function 'call-process)
                        (lambda (program _infile _destination _display &rest args)
                          (if (equal (cadr args) "source.apkg")
                              (error "Expected absolute archive path")
                            (if (string-suffix-p ".apkg" (cadr args))
                                (progn
                                  (setq owned (substring (nth 2 args) 2))
                                  (payload (nth 3 args))
                                  (when (eq phase 'extraction) (fail)))
                              (payload "partial-output")
                              (when (or (equal program "zstd")
                                        (eq phase 'fallback))
                                (unless (eq phase 'remove-compressed) (fail))))
                            0)))
                       ((symbol-function 'delete-file)
                        (lambda (file &optional trash)
                          (if (and (eq phase 'remove-compressed)
                                   (not removal-failed)
                                   (string-suffix-p ".anki21b" file))
                              (progn (setq removal-failed t) (fail))
                            (funcall remove file trash)))))
                    (should
                     (equal
                      (condition-case err
                          (gnosis-anki--extract-db "source.apkg")
                        ((error quit) err))
                      fault))))
                (when owned (should-not (file-exists-p owned)))
                (should (equal (directory-files root nil
                                               directory-files-no-dot-files-regexp)
                               '("unrelated")))
                (should (equal (with-temp-buffer
                                 (insert-file-contents sentinel)
                                 (buffer-string))
                               "Keep me")))
            (delete-directory root t)))))))

(ert-deftest gnosis-test-anki-extraction-cleans-unsuccessful-search ()
  "Missing tools and unsuccessful archive searches leave no directory."
  (dolist (tool-present '(nil t))
    (let* ((root (make-temp-file "gnosis-extraction-test-" t))
           (temporary-file-directory (file-name-as-directory root)))
      (unwind-protect
          (cl-letf (((symbol-function 'executable-find)
                     (lambda (_program &rest _) (and tool-present "7z")))
                    ((symbol-function 'call-process) (lambda (&rest _) 2)))
            (should-error (gnosis-anki--extract-db "source.apkg") :type 'user-error)
            (should-not (directory-files root nil
                                        directory-files-no-dot-files-regexp)))
        (delete-directory root t)))))

(ert-deftest gnosis-test-anki-extraction-transfers-until-parsing-finishes ()
  "Successful extraction retains bytes until parsing succeeds, errors or quits."
  (dolist (name '("collection.anki2" "collection.anki21" "collection.anki21b"))
    (dolist (outcome '(success error quit))
      (ert-info ((format "Archive member %s, parse outcome %s" name outcome))
        (let* ((root (make-temp-file "gnosis-extraction-test-" t))
               (temporary-file-directory (file-name-as-directory root))
               owned)
          (unwind-protect
              (cl-letf
                  (((symbol-function 'executable-find)
                    (lambda (program &rest _) program))
                   ((symbol-function 'call-process)
                    (lambda (program _infile _destination _display &rest args)
                      (if (equal program "zstd")
                          (with-temp-file (nth 3 args) (insert "Collection bytes"))
                        (setq owned (substring (nth 2 args) 2))
                        (when (equal (nth 3 args) name)
                          (with-temp-file (expand-file-name name owned)
                            (insert "Collection bytes"))))
                      0))
                   ((symbol-function 'gnosis-anki--parse-anki-db)
                    (lambda (file)
                      (should (file-exists-p file))
                      (should (equal (with-temp-buffer
                                       (insert-file-contents file)
                                       (buffer-string))
                                     "Collection bytes"))
                      (if (eq outcome 'success) '(0)
                        (signal outcome '("Controlled parse failure")))))
                   ((symbol-function 'gnosis-anki--build-guid-cache)
                    (lambda () (make-hash-table :test #'equal))))
                (let ((file (gnosis-anki--extract-db "source.apkg")))
                  (should (file-exists-p file))
                  (should (file-directory-p owned))
                  (should (equal (file-name-directory file)
                                 (file-name-as-directory owned)))
                  (if (eq outcome 'success)
                      (gnosis-anki--import-db file t)
                    (should (equal (condition-case err
                                       (gnosis-anki--import-db file t)
                                     ((error quit) err))
                                   (list outcome "Controlled parse failure"))))
                  (should-not (file-exists-p owned))
                  (should-not (directory-files root nil
                                              directory-files-no-dot-files-regexp))))
            (delete-directory root t)))))))

(ert-deftest gnosis-test-anki-extraction-real-archives ()
  "Extract real legacy and compressed archives and retain valid SQLite bytes."
  (skip-unless (and (or (executable-find "7z") (executable-find "7za"))
                    (executable-find "zstd") (sqlite-available-p)))
  (let* ((root (make-temp-file "gnosis-extraction-real-" t))
         (temporary-file-directory (file-name-as-directory root))
         (default-directory (file-name-as-directory root))
         (7z (or (executable-find "7z") (executable-find "7za")))
         (database (expand-file-name "collection.anki21" root)))
    (unwind-protect
        (progn
          (let ((db (sqlite-open database)))
            (unwind-protect
                (progn
                  (sqlite-execute db "CREATE TABLE col (models TEXT)")
                  (sqlite-execute db "INSERT INTO col VALUES ('{}')")
                  (sqlite-execute db
                                  "CREATE TABLE notes (id, mid, flds, tags, guid)"))
              (sqlite-close db)))
          (copy-file database "collection.anki2")
          (should (zerop (call-process "zstd" nil nil nil "-q" database
                                      "-o" "collection.anki21b")))
          (dolist (name '("collection.anki2" "collection.anki21" "collection.anki21b"))
            (ert-info ((format "Real archive member %s" name))
              (let ((archive (concat name ".apkg")))
                (should (zerop (call-process 7z nil nil nil
                                            "a" "-tzip" archive name)))
                (let ((file (gnosis-anki--extract-db archive)))
                  (unwind-protect
                      (progn
                        (should (file-exists-p file))
                        (should (equal (secure-hash 'sha256
                                                   (with-temp-buffer
                                                     (insert-file-contents-literally file)
                                                     (buffer-string)))
                                       (secure-hash 'sha256
                                                    (with-temp-buffer
                                                      (insert-file-contents-literally database)
                                                      (buffer-string)))))
                        (should (equal (gnosis-anki--parse-anki-db file) '(0))))
                    (gnosis-anki--cleanup-temp file))
                  (should-not (file-directory-p (file-name-directory file)))))))
          (should-not (directory-files root nil "\\`gnosis-anki-")))
      (delete-directory root t))))

(provide 'gnosis-test-anki-extraction)
;;; gnosis-test-anki-extraction.el ends here

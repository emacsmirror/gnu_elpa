;;; gnosis-test-anki-models.el --- Anki model and route tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;;; Commentary:

;; Exercise both schema readers and public imports with standalone collections.

;;; Code:

(require 'gnosis-test-anki)
(require 'json)

(defun gnosis-test-anki-models--source (file schema fields values kind templates)
  "Write FILE in SCHEMA with FIELDS, VALUES, KIND and TEMPLATES.
TEMPLATES contains (QFMT . AFMT) pairs or the symbol `malformed'."
  (let ((db (sqlite-open file)))
    (unwind-protect
        (progn
          (gnosis-test-anki--create-schema db)
          (if (eq schema 'modern)
              (progn
                (sqlite-execute db "INSERT INTO notetypes VALUES (10,'Note',0,0,?)"
                                (list (if (= kind 1) (unibyte-string 8 1) "")))
                (cl-loop for name in fields for ord from 0 do
                         (sqlite-execute db "INSERT INTO fields VALUES (10,?,?,'')"
                                         (list ord name)))
                (cl-loop for template in templates for ord from 0 do
                         (sqlite-execute
                          db "INSERT INTO templates VALUES (10,?,'Card',0,0,?)"
                          (list ord (if (eq template 'malformed)
                                        (unibyte-string 10 100 97)
                                      (gnosis-test-anki--make-template-config
                                       (car template) (cdr template)))))))
            (sqlite-execute db "DROP TABLE notetypes")
            (sqlite-execute db "CREATE TABLE col (models TEXT)")
            (sqlite-execute
             db "INSERT INTO col VALUES (?)"
             (list (json-serialize
                    (list :10
                          (list :id 10 :name "Note" :type kind
                                :flds (vconcat (mapcar (lambda (f) (list :name f)) fields))
                                :tmpls (vconcat
                                        (mapcar (lambda (pair)
                                                  (if (eq pair 'malformed)
                                                      '(:qfmt "" :afmt "")
                                                    (list :qfmt (car pair) :afmt (cdr pair))))
                                                templates))))))))
          (gnosis-test-anki--insert-note db 7 10 (mapconcat #'identity values "\x1f")
                                         " lesson::heart ")
          (gethash "10" (gnosis-anki--build-model-info db)))
      (sqlite-close db))))

(defun gnosis-test-anki-models--hash (file)
  "Return the literal byte digest of FILE."
  (with-temp-buffer
    (insert-file-contents-literally file)
    (secure-hash 'sha256 (current-buffer))))

(defun gnosis-test-anki-models--public (file tag suspend)
  "Interactively import FILE with TAG and SUSPEND, draining delayed writes."
  (let (pending)
    (cl-letf (((symbol-function 'read-file-name) (lambda (&rest _) file))
              ((symbol-function 'read-string) (lambda (&rest _) tag))
              ((symbol-function 'y-or-n-p) (lambda (&rest _) suspend))
              ((symbol-function 'run-with-timer)
               (lambda (_delay _repeat fn &rest args)
                 (push (lambda () (apply fn args)) pending))))
      (call-interactively #'gnosis-import-anki)
      (while pending (funcall (pop pending))))))

(ert-deftest gnosis-test-anki-models-policy ()
  "Keep both readers' complete basic model policy and first-template counts."
  (dolist (case '(((("{{Text}}" . "{{FrontSide}}{{Extra}}")) 1 ("Text") ("Extra"))
                  ((("{{Extra}}" . "{{Text}}")) 1 ("Extra") ("Text"))
                  ((("{{Deck}}{{Text}}{{Text}}{{Tags}}" . "{{FrontSide}}{{Text}}{{Extra}}{{Extra}}"))
                   1 ("Text") ("Extra"))
                  ((("Question" . "Answer")) 1 ("Extra") ("Text"))
                  ((("{{Text}}" . "{{Text}}")) 1 ("Extra") ("Text"))
                  ((("" . "")) 1 ("Extra") ("Text"))
                  (nil 0 ("Extra") ("Text"))
                  ((malformed) 1 ("Extra") ("Text"))
                  ((("{{Text}}" . "{{Extra}}") ("{{Extra}}" . "{{Text}}"))
                   2 ("Text") ("Extra"))))
    (dolist (schema '(modern legacy))
      (ert-info ((format "%S %S" schema case))
        (let ((file (make-temp-file "anki-model-policy-")))
          (unwind-protect
              (should
               (equal (gnosis-test-anki-models--source
                       file schema '("Extra" "Text") '("A" "Q") 0 (car case))
                      (append (list 0) (cdr case) '("Extra" "Text"))))
            (delete-file file)))))))

(ert-deftest gnosis-test-anki-models-cloze-mapping ()
  "Persist selected cloze fields, complete groups, hints and GUIDs in either schema."
  (dolist (schema '(modern legacy))
    (dolist (fields '(("Extra" "Text") ("Text" "Extra") ("Other" "Extra" "Text")))
      (dolist (extra '("<u>Details</u>" ""))
        (dolist (back '("{{cloze:Text}}{{Extra}}" "{{cloze:Text}}"))
          (ert-info ((format "%S %S %S %S" schema fields extra back))
            (gnosis-test-with-db
              (let* ((file (expand-file-name "source.anki2" gnosis-dir))
                     (text "The {{c1::<b>heart</b>::organ}} has {{c2::four::number}} chambers")
                     (values (mapcar (lambda (field)
                                       (pcase field ("Text" text) ("Extra" extra)
                                         (_ "{{c3::Unrelated::wrong}}")))
                                     fields))
                     (_ (gnosis-test-anki-models--source
                         file schema fields values 1 (list (cons "{{cloze:Text}}" back))))
                     (hash (gnosis-test-anki-models--hash file))
                     (parathema (if (string-search "Extra" back)
                                    (gnosis-anki--html-to-org extra) ""))
                     pending)
                ;; Test parsing/persistence independently of the public-route repair.
                (cl-letf (((symbol-function 'run-with-timer)
                           (lambda (_delay _repeat fn &rest args)
                             (push (lambda () (apply fn args)) pending))))
                  (dotimes (_ 2)
                    (gnosis-anki--import-db file nil "imported" t)
                    (while pending (funcall (pop pending)))
                    (should
                     (equal (gnosis-sqlite--decode-rows
                             (sqlite-select
                              gnosis-db "SELECT t.type,t.keimenon,t.hypothesis,t.answer,e.parathema
                                         FROM themata t JOIN extras e USING(id) ORDER BY t.answer"))
                            (list (list "cloze" "The *heart* has four chambers" '("number") '("four") parathema)
                                  (list "cloze" "The *heart* has four chambers" '("organ") '("heart") parathema))))
                    (should (equal (sqlite-select gnosis-db "SELECT source_guid FROM themata")
                                   '(("guid7") ("guid7"))))
                    (dolist (id (gnosis-select 'id 'themata nil t))
                      (should (equal (sort (gnosis-select 'tag 'thema-tag `(= thema-id ,id) t) #'string<)
                                     '("heart" "imported" "lesson")))
                      (should (= 1 (gnosis-get 'suspended 'scheduler-state `(= thema-id ,id)))))))
                (should (equal hash (gnosis-test-anki-models--hash file)))))))))))

(defun gnosis-test-anki-models--archive (source archive)
  "Package SOURCE as ARCHIVE with the native archive tool."
  (let ((default-directory (file-name-directory source))
        (7z (or (executable-find "7z") (executable-find "7za"))))
    (should (zerop (call-process 7z nil nil nil "a" "-tzip" archive
                                (file-name-nondirectory source))))))

(ert-deftest gnosis-test-anki-models-public-routes ()
  "Import all public formats without touching source bytes or parent directories."
  (dolist (extension '("anki2" "anki21" "apkg"))
    (when (equal extension "apkg")
      (skip-unless (or (executable-find "7z") (executable-find "7za"))))
    (dolist (suspend '(nil t))
      (gnosis-test-with-db
        (let* ((root (make-temp-file "anki-public-" t))
               (temporary-file-directory (file-name-as-directory root))
               (collection (expand-file-name "collection.anki21" root))
               (file (expand-file-name (concat "import." extension) root)))
          (unwind-protect
              (progn
                (gnosis-test-anki-models--source
                 collection 'modern '("Extra" "Text") '("Answer" "Question") 0
                 '(("{{Text}}" . "{{Extra}}")))
                (if (equal extension "apkg")
                    (gnosis-test-anki-models--archive collection file)
                  (copy-file collection file))
                (let ((hash (gnosis-test-anki-models--hash file))
                      (contents (directory-files root)))
                  (dotimes (_ 2)
                    ;; Direct routes must not depend on the archive tools.
                    (let ((exec-path (and (equal extension "apkg") exec-path)))
                      (gnosis-test-anki-models--public file (if suspend "imported" "") suspend))
                    (should (equal (gnosis-select '[keimenon answer] 'themata)
                                   '(("Question" ("Answer")))))
                    (let ((id (car (gnosis-select 'id 'themata nil t))))
                      (should (= (if suspend 1 0)
                                 (gnosis-get 'suspended 'scheduler-state `(= thema-id ,id))))
                      (should (equal (sort (gnosis-select 'tag 'thema-tag `(= thema-id ,id) t) #'string<)
                                     (if suspend '("heart" "imported" "lesson") '("heart" "lesson")))))
                    (should (equal hash (gnosis-test-anki-models--hash file)))
                    (should (equal contents (directory-files root))))))
            (delete-directory root t)))))))

(ert-deftest gnosis-test-anki-models-public-failure-retry ()
  "Retain direct sources and clean only archive stages on parse error or quit."
  (dolist (extension '("anki2" "anki21" "apkg"))
    (when (equal extension "apkg")
      (skip-unless (or (executable-find "7z") (executable-find "7za"))))
    (dolist (fault '(error quit))
      (gnosis-test-with-db
        (let* ((root (make-temp-file "anki-public-fault-" t))
               (temporary-file-directory (file-name-as-directory root))
               (collection (expand-file-name "collection.anki21" root))
               (file (expand-file-name (concat "import." extension) root))
               (parser (symbol-function 'gnosis-anki--parse-notes))
               reached)
          (unwind-protect
              (progn
                (gnosis-test-anki-models--source
                 collection 'legacy '("Text" "Extra") '("Q" "A") 0
                 '(("{{Text}}" . "{{Extra}}")))
                (if (equal extension "apkg")
                    (gnosis-test-anki-models--archive collection file)
                  (copy-file collection file))
                (let ((hash (gnosis-test-anki-models--hash file))
                      (contents (directory-files root)))
                  (cl-letf (((symbol-function 'gnosis-anki--parse-notes)
                             (lambda (&rest args)
                               (apply parser args)
                               (setq reached t)
                               (signal fault '("Controlled parse interruption")))))
                    (should (equal (condition-case err
                                       (gnosis-test-anki-models--public file "" nil)
                                     ((error quit) err))
                                   (list fault "Controlled parse interruption"))))
                  (should reached)
                  (should-not (gnosis-select 'id 'themata))
                  (should (equal contents (directory-files root)))
                  (should (equal hash (gnosis-test-anki-models--hash file)))
                  (gnosis-test-anki-models--public file "" nil)
                  (should (equal (gnosis-select '[keimenon answer] 'themata) '(("Q" ("A")))))
                  (should (equal contents (directory-files root)))
                  (should (equal hash (gnosis-test-anki-models--hash file)))))
            (delete-directory root t)))))))

(ert-deftest gnosis-test-anki-models-public-extraction-lifetime ()
  "Retire real extraction and its connection before writes or parse exits."
  (skip-unless (or (executable-find "7z") (executable-find "7za")))
  (dolist (outcome '(success empty error quit))
    (gnosis-test-with-db
      (let* ((root (make-temp-file "anki-lifetime-" t))
             (temporary-file-directory (file-name-as-directory root))
             (collection (expand-file-name "collection.anki21" root))
             (file (expand-file-name "import.apkg" root))
             (gnosis-anki--chunk-size 1)
             (parser (symbol-function 'gnosis-anki--parse-notes))
             (writer (symbol-function 'gnosis-anki--bulk-insert-chunk))
             source-db extracted pending writes)
        (unwind-protect
            (progn
              (gnosis-test-anki--overlap-source
               collection 'basic (unless (eq outcome 'empty) '(1 2)))
              (gnosis-test-anki-models--archive collection file)
              (let ((hash (gnosis-test-anki-models--hash file))
                    (contents (directory-files root)))
                (cl-letf
                    (((symbol-function 'read-file-name) (lambda (&rest _) file))
                     ((symbol-function 'read-string) (lambda (&rest _) "imported"))
                     ((symbol-function 'y-or-n-p) (lambda (&rest _) t))
                     ((symbol-function 'run-with-timer)
                      (lambda (_delay _repeat fn &rest args)
                        (push (lambda () (apply fn args)) pending)))
                     ((symbol-function 'gnosis-anki--parse-notes)
                      (lambda (db models)
                        (setq source-db db
                              extracted (nth 2 (car (sqlite-select db "PRAGMA database_list"))))
                        (should (file-exists-p extracted))
                        (prog1 (funcall parser db models)
                          (when (memq outcome '(error quit))
                            (signal outcome '("Controlled parse exit"))))))
                     ((symbol-function 'gnosis-anki--bulk-insert-chunk)
                      (lambda (&rest args)
                        ;; Assert the observations outside the chunk's error handler.
                        (push (file-exists-p (file-name-directory extracted)) writes)
                        (apply writer args))))
                  (let ((result (condition-case err
                                    (call-interactively #'gnosis-import-anki)
                                  ((error quit) err))))
                    (when (memq outcome '(error quit))
                      (should (equal result (list outcome "Controlled parse exit")))))
                  (should extracted)
                  (should-not (equal extracted collection))
                  (should-not (file-exists-p (file-name-directory extracted)))
                  (should-error (sqlite-select source-db "SELECT 1"))
                  (if (eq outcome 'success)
                      (progn
                        (should (= 1 (length pending)))
                        (should (= 1 (length (gnosis-select 'id 'themata))))
                        ;; Deliver only after the public import has returned.
                        (while pending (funcall (pop pending)))
                        (should (equal writes '(nil nil)))
                        (should (equal (sqlite-select gnosis-db
                                                      "SELECT source_guid FROM themata ORDER BY source_guid")
                                       '(("guid1") ("guid2")))))
                    (should-not pending)
                    (should-not writes)
                    (should-not (gnosis-select 'id 'themata))))
                (should (equal hash (gnosis-test-anki-models--hash file)))
                (should (equal contents (directory-files root)))))
          (delete-directory root t))))))

(ert-deftest gnosis-test-anki-models-public-unsupported ()
  "Reject unsupported extensions before asking import options or extracting."
  (dolist (extension '("db" "anki21b" "apkg.bak"))
    (let ((file (make-temp-file "anki-unsupported-" nil (concat "." extension)))
          prompted)
      (unwind-protect
          (cl-letf (((symbol-function 'read-string)
                     (lambda (&rest _) (setq prompted t) "")))
            (should-error (gnosis-import-anki file) :type 'user-error)
            (should-not prompted)
            (should (file-exists-p file)))
        (delete-file file)))))

(ert-deftest gnosis-test-anki-models-public-atomic-retry ()
  "A delayed sibling failure rolls back its note and preserves import sources."
  (dolist (extension '("anki2" "anki21" "apkg"))
    (when (equal extension "apkg")
      (skip-unless (or (executable-find "7z") (executable-find "7za"))))
    (dolist (schema '(modern legacy))
      (dolist (fault '(error quit))
        (gnosis-test-with-db
          (let* ((root (make-temp-file "anki-atomic-" t))
                 (temporary-file-directory (file-name-as-directory root))
                 (collection (expand-file-name "collection.anki21" root))
                 (file (expand-file-name (concat "import." extension) root))
                 (gnosis-anki--chunk-size 1)
                 (writer (symbol-function 'gnosis-anki--bulk-insert-chunk))
                 (writes 0))
            (unwind-protect
                (progn
                  (gnosis-test-anki-models--source
                   collection schema '("Extra" "Text")
                   '("Details" "{{c1::heart::organ}} {{c2::four::number}}") 1
                   '(("{{cloze:Text}}" . "{{cloze:Text}}{{Extra}}")))
                  (let ((db (sqlite-open collection)))
                    (unwind-protect
                        (gnosis-test-anki--insert-note
                         db 6 10 (concat "Prelude" "\x1f" "{{c1::first}}") "before")
                      (sqlite-close db)))
                  (if (equal extension "apkg")
                      (gnosis-test-anki-models--archive collection file)
                    (copy-file collection file))
                  (let ((hash (gnosis-test-anki-models--hash file))
                        (contents (directory-files root)))
                    (cl-letf (((symbol-function 'gnosis-anki--bulk-insert-chunk)
                               (lambda (&rest args)
                                 ;; Extraction is retired even before the first write.
                                 (should (equal contents (directory-files root)))
                                 (apply writer args)
                                 (when (= (cl-incf writes) 3)
                                   (signal fault '("Controlled sibling failure"))))))
                      (let ((outcome (condition-case err
                                         (gnosis-test-anki-models--public file "" nil)
                                       (quit err))))
                        (when (eq fault 'quit)
                          (should (equal outcome '(quit "Controlled sibling failure"))))))
                    (should (= writes 3))
                    (should (equal (sqlite-select gnosis-db "SELECT source_guid FROM themata")
                                   '(("guid6"))))
                    (dotimes (_ 2)
                      (gnosis-test-anki-models--public file "imported" t)
                      (should (equal (sqlite-select gnosis-db
                                                    "SELECT source_guid, COUNT(*) FROM themata
                                                     GROUP BY source_guid ORDER BY source_guid")
                                     '(("guid6" 1) ("guid7" 2)))))
                    (should-not (sqlite-select gnosis-db "PRAGMA foreign_key_check"))
                    (dolist (table '(extras scheduler_baseline scheduler_state))
                      (should (= 3 (caar (sqlite-select
                                         gnosis-db (format "SELECT COUNT(*) FROM %s" table))))))
                    (should (equal contents (directory-files root)))
                    (should (equal hash (gnosis-test-anki-models--hash file)))))
              (delete-directory root t))))))))

(provide 'gnosis-test-anki-models)
;;; gnosis-test-anki-models.el ends here

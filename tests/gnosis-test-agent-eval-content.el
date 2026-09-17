;;; gnosis-test-agent-eval-content.el --- Agent content tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'gnosis-export-import)
(require 'gnosis-review)
(require 'gnosis-test-helpers)
(require 'gnosis-test-schema-v8)

(defconst gnosis-test-agent-eval--answer
  "  α mechanism\n- [x] Consequence\n---\n* Literal heading\n,#+literal\n  final  \n")
(defconst gnosis-test-agent-eval--rubric
  " Essential: mechanism and consequence.\n- [ ] Missing mechanism fails.\n* Consequential error: reversing causality.\n  final  \n")

(defun gnosis-test-agent-eval--add (&optional id)
  "Create an agent-eval thema with ID and return its ID."
  (let ((id (or id (gnosis-generate-id))))
    (gnosis-add-thema-fields
     "agent-eval" "Explain causality" '("Think of the mechanism")
     (list gnosis-test-agent-eval--answer) "Teaching [[id:source][Source]]"
     '("test") 0 '("source") nil id nil gnosis-test-agent-eval--rubric)
    id))

(ert-deftest gnosis-test-agent-eval-content-storage ()
  (gnosis-test-with-db
    (let* ((id (gnosis-test-agent-eval--add))
           (before (gnosis--draft-content gnosis-db id)))
      (gnosis-db--check-schema gnosis-db 11)
      (gnosis-update-thema id "New question" nil '("New reference") "" nil nil)
      (should (equal gnosis-test-agent-eval--rubric
                     (gnosis-get 'rubric 'themata `(= id ,id))))
      (should-error (gnosis-update-thema id "Q" nil '("A") "" nil nil
                                         nil nil nil))
      (should-error (gnosis-update-thema id "Q" nil '("A") "" nil nil
                                         nil '("Alias")))
      (gnosis-update-thema id "Q" nil '("A") "" nil nil nil nil "New rubric")
      (should-not (equal before (gnosis--draft-content gnosis-db id)))
      (gnosis-sqlite-close gnosis-db)
      (setq gnosis-db (gnosis-sqlite-open gnosis-test--db-file))
      (gnosis-db-init)
      (should (equal "New rubric" (gnosis-get 'rubric 'themata `(= id ,id))))
      (gnosis-update-thema id "Q" nil '("A") "" nil nil "basic" nil nil)
      (should-not (gnosis-get 'rubric 'themata `(= id ,id))))))

(ert-deftest gnosis-test-agent-eval-content-invalid ()
  (gnosis-test-with-db
    (dolist (fields '(("Q" nil ("A") nil)
                      ("Q" nil ("A") " \n")
                      ("Q" nil ("") "Rubric")
                      ("Q" nil ("A" "B") "Rubric")
                      (" " nil ("A") "Rubric")
                      ("Q" (1) ("A") "Rubric")))
      (should-error
       (gnosis-add-thema-fields "agent-eval" (nth 0 fields) (nth 1 fields)
                                (nth 2 fields) "" nil 0 nil nil nil nil
                                (nth 3 fields))))
    (should-not (gnosis-select 'id 'themata))))

(ert-deftest gnosis-test-agent-eval-content-native-round-trip ()
  (gnosis-test-with-db
    (let ((id (gnosis-test-agent-eval--add)))
      (dotimes (_ 2)
        (with-temp-buffer
          (org-mode)
          (gnosis-export--insert-themata (list id))
          (should (string-match-p "\\*\\* Rubric" (buffer-string)))
          (let ((thema (car (gnosis-export-parse-themata))))
            (should (equal (nth 4 thema) (list gnosis-test-agent-eval--answer)))
            (should (equal (nth 9 thema) gnosis-test-agent-eval--rubric))
            (should-not (gnosis-save-thema thema)))))
      (should (equal gnosis-test-agent-eval--rubric
                     (gnosis-get 'rubric 'themata `(= id ,id)))))))

(ert-deftest gnosis-test-agent-eval-content-migrate ()
  (dolist (archive '(nil t))
    (gnosis-test-with-old-db
      (gnosis-test--create-v8-schema)
      (when archive
        (sqlite-execute gnosis-db "ALTER TABLE themata ADD COLUMN archived_at_us INTEGER"))
      (gnosis-sqlite-execute gnosis-db
        "INSERT INTO themata (id,type,keimenon,hypothesis,answer,source_guid) VALUES (1,?,?,?, ?,?)"
        '("basic" "Q" ("") ("A") "source-guid"))
      (gnosis-sqlite-execute gnosis-db "INSERT INTO review VALUES (1, 1, 0)")
      (gnosis-sqlite-execute gnosis-db
        "INSERT INTO review_log VALUES (1, ?, ?, 0, 0, 0, 0, 0, 0)"
        '(20260901 20260901))
      (gnosis-db-init)
      (should (= 11 (gnosis--db-version)))
      (should (equal '("A") (gnosis-get 'answer 'themata '(= id 1))))
      (should-not (gnosis-get 'rubric 'themata '(= id 1)))
      (gnosis-test-agent-eval--add 2)
      (gnosis-db--check-schema gnosis-db 11))))

(ert-deftest gnosis-test-agent-eval-content-exchange ()
  (gnosis-test-with-db
    (let* ((id (gnosis-test-agent-eval--add))
           (file (expand-file-name "content.db" gnosis-dir)))
      (gnosis-export-db file)
      (should (= 5 (gnosis-import--format-version file)))
      (gnosis-update 'themata '(= rubric "Changed rubric") `(= id ,id))
      (let ((diff (gnosis-import--diff file)))
        (should (equal (nth 3 (car (cadr diff)))
                       `(("rubric" "Changed rubric" ,gnosis-test-agent-eval--rubric))))
        (gnosis-update 'themata '(= rubric "Drift") `(= id ,id))
        (should-error (gnosis-import--apply-changes file nil (list id)
                                                   (nth 2 diff) (nth 3 diff)))
        (gnosis-update 'themata '(= rubric "Changed rubric") `(= id ,id))
        (gnosis-import--apply-changes file nil (list id) (nth 2 diff) (nth 3 diff)))
      (should (equal gnosis-test-agent-eval--rubric
                     (gnosis-get 'rubric 'themata `(= id ,id))))
      (let ((diff (gnosis-import--diff file))
            (source (gnosis-sqlite-open file)))
        (unwind-protect
            (gnosis-sqlite-execute source "UPDATE themata SET rubric = ?" '("Source drift"))
          (sqlite-close source))
        (should-error (gnosis-import--apply-changes file nil (list id) (nth 2 diff)))))))

(ert-deftest gnosis-test-agent-eval-content-native-commands ()
  (gnosis-test-with-db
    (let ((gnosis-save-hook nil)
          (gnosis-review-editing-p nil)
          (register-alist nil))
      (save-window-excursion
        (unwind-protect
            (progn
              (gnosis-add-thema "agent-eval" "Explain causality" nil
                                gnosis-test-agent-eval--answer "Teaching" nil nil
                                nil gnosis-test-agent-eval--rubric)
              (should (eq (key-binding (kbd "C-c C-c")) #'gnosis-save))
              (call-interactively (key-binding (kbd "C-c C-c")))
              (should-not (get-buffer "*Gnosis NEW*"))
              (let ((id (car (gnosis-select 'id 'themata nil t))))
                (gnosis-edit-thema id)
                (should (equal gnosis-test-agent-eval--rubric
                               (nth 9 (car (gnosis-export-parse-themata)))))
                (call-interactively (key-binding (kbd "C-c C-c")))
                (gnosis-edit-thema id)
                (gnosis-update 'themata '(= rubric "Intervening rubric") `(= id ,id))
                (should-error (call-interactively (key-binding (kbd "C-c C-c"))))
                (should (buffer-live-p (get-buffer "*Gnosis Edit*")))
                (call-interactively (key-binding (kbd "C-c C-k")))
                (should (equal "Intervening rubric"
                               (gnosis-get 'rubric 'themata `(= id ,id)))))
              (should-not (gnosis-select 'event-id 'review-events))
              (should-not (gnosis-select 'event-id 'practice-events)))
          (dolist (name '("*Gnosis NEW*" "*Gnosis Edit*"))
            (when (get-buffer name) (kill-buffer name))))))))

(ert-deftest gnosis-test-agent-eval-content-missing-rubric-refuses ()
  (gnosis-test-with-db
    (let* ((id (gnosis-test-agent-eval--add))
           (before (gnosis--draft-content gnosis-db id)))
      (with-temp-buffer
        (org-mode)
        (gnosis-export--insert-themata (list id))
        (let ((thema (car (gnosis-export-parse-themata))))
          (should (stringp (gnosis-save-thema (seq-take thema 9))))
          (setf (nth 9 thema) " \n ")
          (should (stringp (gnosis-save-thema thema)))))
      (should (equal before (gnosis--draft-content gnosis-db id))))))

(defun gnosis-test-agent-eval--set-type (type)
  "Replace GNOSIS_TYPE in the current native draft with TYPE."
  (goto-char (point-min))
  (search-forward ":GNOSIS_TYPE: ")
  (let ((inhibit-read-only t))
    (delete-region (point) (line-end-position))
    (insert type)))

(defun gnosis-test-agent-eval--delete-rubric ()
  "Remove the Rubric field from the current native draft."
  (goto-char (point-min))
  (search-forward "** Rubric")
  (let ((start (line-beginning-position))
        (inhibit-read-only t))
    (search-forward "** Parathema")
    (delete-region start (line-beginning-position))))

(defun gnosis-test-agent-eval--insert-rubric (text)
  "Insert Rubric TEXT before Parathema in the current native draft."
  (goto-char (point-min))
  (search-forward "** Parathema")
  (let ((inhibit-read-only t))
    (goto-char (line-beginning-position))
    (insert "** Rubric\n" text "\n\n")))

(defun gnosis-test-agent-eval--replace-section (title body)
  "Replace the native draft section TITLE body with BODY."
  (goto-char (point-min))
  (search-forward (concat "** " title))
  (forward-line 1)
  (let ((start (point))
        (inhibit-read-only t))
    (search-forward "** ")
    (goto-char (match-beginning 0))
    (delete-region start (point))
    (insert body "\n\n")))

(defun gnosis-test-agent-eval--record-study (id)
  "Record one scheduled and one practice success for ID."
  (dolist (mode '(due practice))
    (let ((review (gnosis-review--setup-buffer (list id) mode)))
      (unwind-protect
          (with-current-buffer review
            (gnosis-review-result id t (gnosis-review-algorithm id t)))
        (when (buffer-live-p review) (kill-buffer review))))))

(ert-deftest gnosis-test-agent-eval-content-native-type-conversion ()
  "Convert agent-eval through the public native C-c C-c binding."
  (gnosis-test-with-db
    (let ((gnosis-save-hook nil)
          (gnosis-review-editing-p nil)
          (register-alist nil)
          (id (gnosis-test-agent-eval--add))
          (rubric "Must name Paris."))
      (gnosis-update-thema id "Capital?" nil '("Paris") "Teaching" nil nil)
      (gnosis-test-agent-eval--record-study id)
      (should (gnosis-select '* 'review-events))
      (should (gnosis-select '* 'practice-events))
      (let ((before (gnosis-test-agent-eval--study-evidence)))
        (save-window-excursion
          (unwind-protect
              (progn
                (gnosis-edit-thema id)
                (gnosis-test-agent-eval--set-type "basic")
                (gnosis-test-agent-eval--delete-rubric)
                (should (eq (key-binding (kbd "C-c C-c")) #'gnosis-save))
                (call-interactively (key-binding (kbd "C-c C-k")))
                (should (equal "agent-eval" (gnosis-get 'type 'themata `(= id ,id))))
                (should (equal gnosis-test-agent-eval--rubric
                               (gnosis-get 'rubric 'themata `(= id ,id))))
                (should (equal before (gnosis-test-agent-eval--study-evidence)))
                (gnosis-edit-thema id)
                (gnosis-test-agent-eval--set-type "basic")
                (gnosis-test-agent-eval--delete-rubric)
                (should (equal "basic" (nth 1 (car (gnosis-export-parse-themata)))))
                (should-not (nth 9 (car (gnosis-export-parse-themata))))
                (call-interactively (key-binding (kbd "C-c C-c")))
                (should-not (get-buffer "*Gnosis Edit*"))
                (should (equal "basic" (gnosis-get 'type 'themata `(= id ,id))))
                (should-not (gnosis-get 'rubric 'themata `(= id ,id)))
                (gnosis-sqlite-close gnosis-db)
                (setq gnosis-db (gnosis-sqlite-open gnosis-test--db-file))
                (gnosis-db-init)
                (should (equal "basic" (gnosis-get 'type 'themata `(= id ,id))))
                (should (equal '("Paris") (gnosis-get 'answer 'themata `(= id ,id))))
                (should (equal "Teaching" (gnosis-get 'parathema 'extras `(= id ,id))))
                (should-not (gnosis-get 'rubric 'themata `(= id ,id)))
                (should (equal before (gnosis-test-agent-eval--study-evidence)))
                (gnosis-edit-thema id)
                (let ((row (car (gnosis-export-parse-themata))))
                  (should (equal "basic" (nth 1 row)))
                  (should (equal '("Paris") (nth 4 row)))
                  (should (equal "Teaching" (nth 5 row)))
                  (should-not (nth 9 row)))
                (call-interactively (key-binding (kbd "C-c C-k")))
                (gnosis-edit-thema id)
                (gnosis-test-agent-eval--set-type "agent-eval")
                (gnosis-test-agent-eval--insert-rubric rubric)
                (call-interactively (key-binding (kbd "C-c C-c")))
                (should (equal "agent-eval" (gnosis-get 'type 'themata `(= id ,id))))
                (should (equal rubric (gnosis-get 'rubric 'themata `(= id ,id))))
                (gnosis-edit-thema id)
                (gnosis-test-agent-eval--delete-rubric)
                (should-error (call-interactively (key-binding (kbd "C-c C-c"))))
                (should (buffer-live-p (get-buffer "*Gnosis Edit*")))
                (should (equal "agent-eval" (gnosis-get 'type 'themata `(= id ,id))))
                (should (equal rubric (gnosis-get 'rubric 'themata `(= id ,id))))
                (call-interactively (key-binding (kbd "C-c C-k")))
                (should (equal before (gnosis-test-agent-eval--study-evidence))))
            (when (get-buffer "*Gnosis Edit*") (kill-buffer "*Gnosis Edit*"))))))))

(ert-deftest gnosis-test-agent-eval-content-native-cloze-conversion ()
  "Convert agent-eval to cloze through the public native C-c C-c binding."
  (gnosis-test-with-db
    (let ((gnosis-save-hook nil)
          (gnosis-review-editing-p nil)
          (register-alist nil)
          (id (gnosis-test-agent-eval--add)))
      (gnosis-update-thema id "Say Paris." nil '("Paris") "Teaching" nil nil)
      (gnosis-test-agent-eval--record-study id)
      (let ((before (gnosis-test-agent-eval--study-evidence)))
        (save-window-excursion
          (unwind-protect
              (progn
                (gnosis-edit-thema id)
                (gnosis-test-agent-eval--set-type "cloze")
                (gnosis-test-agent-eval--delete-rubric)
                (should (eq (key-binding (kbd "C-c C-c")) #'gnosis-save))
                (should (equal "cloze" (nth 1 (car (gnosis-export-parse-themata)))))
                (should-not (nth 9 (car (gnosis-export-parse-themata))))
                (call-interactively (key-binding (kbd "C-c C-c")))
                (should-not (get-buffer "*Gnosis Edit*"))
                (should (equal "cloze" (gnosis-get 'type 'themata `(= id ,id))))
                (should-not (gnosis-get 'rubric 'themata `(= id ,id)))
                (gnosis-sqlite-close gnosis-db)
                (setq gnosis-db (gnosis-sqlite-open gnosis-test--db-file))
                (gnosis-db-init)
                (should (equal "cloze" (gnosis-get 'type 'themata `(= id ,id))))
                (should (equal "Say Paris." (gnosis-get 'keimenon 'themata `(= id ,id))))
                (should (equal '("Paris") (gnosis-get 'answer 'themata `(= id ,id))))
                (should (equal "Teaching" (gnosis-get 'parathema 'extras `(= id ,id))))
                (should-not (gnosis-get 'rubric 'themata `(= id ,id)))
                (should (equal before (gnosis-test-agent-eval--study-evidence)))
                (gnosis-edit-thema id)
                (let ((row (car (gnosis-export-parse-themata))))
                  (should (equal "cloze" (nth 1 row)))
                  (should (equal "Say Paris." (nth 2 row)))
                  (should (equal '("Paris") (nth 4 row)))
                  (should (equal "Teaching" (nth 5 row)))
                  (should-not (nth 9 row)))
                (call-interactively (key-binding (kbd "C-c C-k"))))
            (when (get-buffer "*Gnosis Edit*") (kill-buffer "*Gnosis Edit*"))))))))

(ert-deftest gnosis-test-agent-eval-content-native-mcq-conversion ()
  "Convert agent-eval to MCQ through the public native C-c C-c binding."
  (gnosis-test-with-db
    (let ((gnosis-save-hook nil)
          (gnosis-review-editing-p nil)
          (register-alist nil)
          (id (gnosis-test-agent-eval--add)))
      (gnosis-update-thema id "Capital?" nil '("Paris") "Teaching" nil nil)
      (gnosis-test-agent-eval--record-study id)
      (let ((before (gnosis-test-agent-eval--study-evidence)))
        (save-window-excursion
          (unwind-protect
              (progn
                (gnosis-edit-thema id)
                (gnosis-test-agent-eval--set-type "mcq")
                (gnosis-test-agent-eval--replace-section "Hypothesis" "- Paris\n- London")
                (gnosis-test-agent-eval--replace-section "Answer" "- Paris")
                (gnosis-test-agent-eval--delete-rubric)
                (should (eq (key-binding (kbd "C-c C-c")) #'gnosis-save))
                (let ((row (car (gnosis-export-parse-themata))))
                  (should (equal "mcq" (nth 1 row)))
                  (should (equal '("Paris" "London") (nth 3 row)))
                  (should (equal '("Paris") (nth 4 row)))
                  (should-not (nth 9 row)))
                (call-interactively (key-binding (kbd "C-c C-c")))
                (should-not (get-buffer "*Gnosis Edit*"))
                (should (equal "mcq" (gnosis-get 'type 'themata `(= id ,id))))
                (should-not (gnosis-get 'rubric 'themata `(= id ,id)))
                (gnosis-sqlite-close gnosis-db)
                (setq gnosis-db (gnosis-sqlite-open gnosis-test--db-file))
                (gnosis-db-init)
                (should (equal "mcq" (gnosis-get 'type 'themata `(= id ,id))))
                (should (equal "Capital?" (gnosis-get 'keimenon 'themata `(= id ,id))))
                (should (equal '("Paris" "London")
                               (gnosis-get 'hypothesis 'themata `(= id ,id))))
                (should (equal '("Paris") (gnosis-get 'answer 'themata `(= id ,id))))
                (should (equal "Teaching" (gnosis-get 'parathema 'extras `(= id ,id))))
                (should-not (gnosis-get 'rubric 'themata `(= id ,id)))
                (should (equal before (gnosis-test-agent-eval--study-evidence)))
                (gnosis-edit-thema id)
                (let ((row (car (gnosis-export-parse-themata))))
                  (should (equal "mcq" (nth 1 row)))
                  (should (equal "Capital?" (nth 2 row)))
                  (should (equal '("Paris" "London") (nth 3 row)))
                  (should (equal '("Paris") (nth 4 row)))
                  (should (equal "Teaching" (nth 5 row)))
                  (should-not (nth 9 row)))
                (call-interactively (key-binding (kbd "C-c C-k"))))
            (when (get-buffer "*Gnosis Edit*") (kill-buffer "*Gnosis Edit*"))))))))

(ert-deftest gnosis-test-agent-eval-content-conversion-rollback ()
  "Failed conversion leaves the original agent-eval intact."
  (gnosis-test-with-db
    (let ((gnosis-save-hook nil)
          (gnosis-review-editing-p nil)
          (register-alist nil)
          (id (gnosis-test-agent-eval--add)))
      (gnosis-update-thema id "Capital?" nil '("Paris") "Teaching" nil nil)
      (gnosis-test-agent-eval--record-study id)
      (let ((before (gnosis--draft-content gnosis-db id))
            (evidence (gnosis-test-agent-eval--study-evidence)))
        (should (stringp
                 (gnosis-save-thema
                  (list (format "%s" id) "cloze" "Capital?" '("") '("Paris")
                        "Teaching" nil 1 nil nil))))
        (should (equal before (gnosis--draft-content gnosis-db id)))
        (save-window-excursion
          (unwind-protect
              (progn
                (gnosis-edit-thema id)
                (gnosis-test-agent-eval--set-type "cloze")
                (gnosis-test-agent-eval--delete-rubric)
                (should-error (call-interactively (key-binding (kbd "C-c C-c"))))
                (should (buffer-live-p (get-buffer "*Gnosis Edit*")))
                (should (equal "agent-eval" (gnosis-get 'type 'themata `(= id ,id))))
                (should (equal gnosis-test-agent-eval--rubric
                               (gnosis-get 'rubric 'themata `(= id ,id))))
                (should (equal before (gnosis--draft-content gnosis-db id)))
                (call-interactively (key-binding (kbd "C-c C-k")))
                (should (equal evidence (gnosis-test-agent-eval--study-evidence))))
            (when (get-buffer "*Gnosis Edit*") (kill-buffer "*Gnosis Edit*"))))))))

(ert-deftest gnosis-test-agent-eval-content-legacy-formats ()
  (dolist (version '(1 2 3))
    (gnosis-test-with-db
      (let* ((id (gnosis-test--add-basic-thema "Q" "A"))
             (file (expand-file-name "legacy.db" gnosis-dir)))
        (gnosis-export-db file)
        (let ((source (gnosis-sqlite-open file)))
          (unwind-protect
              (progn
                (sqlite-execute source "ALTER TABLE themata DROP COLUMN rubric")
                (when (< version 3)
                  (sqlite-execute source "ALTER TABLE themata DROP COLUMN accepted_aliases"))
                (sqlite-execute source
                  "UPDATE gnosis_meta SET value = ? WHERE key = 'format_version'"
                  (list (number-to-string version))))
            (sqlite-close source)))
        (gnosis-update 'themata '(= keimenon "Changed") `(= id ,id))
        (let ((diff (gnosis-import--diff file)))
          (gnosis-import--apply-changes file nil (list id) (nth 2 diff) (nth 3 diff)))
        (should (equal "Q" (gnosis-get 'keimenon 'themata `(= id ,id))))
        (should-not (gnosis-get 'rubric 'themata `(= id ,id)))))))

(ert-deftest gnosis-test-agent-eval-content-import-new ()
  (gnosis-test-with-db
    (let* ((id (gnosis-test-agent-eval--add))
           (file (expand-file-name "source.db" gnosis-dir))
           (expected (gnosis-import--content-rows gnosis-db "main" (list id))))
      (gnosis-export-db file)
      (gnosis-test-with-db
        (let ((diff (gnosis-import--diff file)))
          (gnosis-import--apply-changes file (list id) nil (nth 2 diff) (nth 3 diff)))
        (should (equal expected (gnosis-import--content-rows gnosis-db "main" (list id))))
        (should-not (gnosis-select 'event-id 'review-events))
        (should-not (gnosis-select 'event-id 'practice-events))))))

(ert-deftest gnosis-test-agent-eval-content-export-invalid ()
  (gnosis-test-with-db
    (let* ((id (gnosis-test-agent-eval--add))
           (file (expand-file-name "preserved.db" gnosis-dir)))
      (with-temp-file file (insert "Previous bytes"))
      (dolist (rubric '(nil "" "[[gnosis-image:malformed]]"))
        (gnosis-sqlite-execute gnosis-db "UPDATE themata SET rubric = ? WHERE id = ?"
                               (list rubric id))
        (should-error (gnosis-export-db file))
        (should (equal "Previous bytes"
                       (with-temp-buffer (insert-file-contents file) (buffer-string))))))))

(ert-deftest gnosis-test-agent-eval-content-migration-rollback ()
  (dolist (fault '(error quit))
    (gnosis-test-with-old-db
      (gnosis-test--create-v8-schema)
      (let ((execute (symbol-function 'gnosis-sqlite-execute)))
        (cl-letf (((symbol-function 'gnosis-sqlite-execute)
                   (lambda (db sql &optional params)
                     (if (string-match-p "ADD COLUMN rubric" sql)
                         (signal fault '("Rubric ALTER fault"))
                       (funcall execute db sql params)))))
          (should (condition-case nil (progn (gnosis-db-init) nil)
                    ((error quit) t)))))
      (should (= 8 (gnosis--db-version)))
      (should-not (member "accepted_aliases"
                          (mapcar #'cadr (sqlite-select gnosis-db "PRAGMA table_info(themata)"))))
      (gnosis-db-init)
      (gnosis-db--check-schema gnosis-db 11))))

(ert-deftest gnosis-test-agent-eval-content-schema-missing-rubric ()
  (gnosis-test-with-db
    (sqlite-execute gnosis-db "ALTER TABLE themata DROP COLUMN rubric")
    (should-error (gnosis-db-init))
    (should (= 11 (gnosis--db-version)))))

(defun gnosis-test-agent-eval--study-evidence ()
  "Return retained study evidence and projections in the current database."
  (mapcar (lambda (table) (gnosis-select '* table))
          '(scheduler-state scheduler-baseline review-events review-voids
            practice-events practice-voids study-session study-history)))

(defun gnosis-test-agent-eval--import-save (existing)
  "Prove native save after importing absent extras, updating when EXISTING."
  (gnosis-test-with-db
    (let* ((id (gnosis-test-agent-eval--add))
           (file (expand-file-name "without-extras.db" gnosis-dir))
           (teaching "Authored teaching α\n[[id:source][Source]]")
           (gnosis-save-hook nil)
           (gnosis-review-editing-p nil)
           (register-alist nil))
      (gnosis-export-db file)
      (let ((source (sqlite-open file)))
        (unwind-protect (sqlite-execute source "DELETE FROM extras")
          (sqlite-close source)))
      (gnosis-test-with-db
        (when existing
          (gnosis-test-agent-eval--add id)
          (gnosis-update-thema id "Old question" nil '("Old reference")
                               "Old teaching" nil nil nil nil "Old rubric"))
        ;; Keep real accepted evidence, including on the changed target.
        (let ((studied (if existing id (gnosis-test--add-basic-thema "Q" "A"))))
          (dolist (mode '(due practice))
            (let ((review (gnosis-review--setup-buffer (list studied) mode)))
              (unwind-protect
                  (with-current-buffer review
                    (gnosis-review-result
                     studied t (gnosis-review-algorithm studied t)))
                (when (buffer-live-p review) (kill-buffer review))))))
        (should (gnosis-select '* 'review-events))
        (should (gnosis-select '* 'practice-events))
        (let ((before (gnosis-test-agent-eval--study-evidence))
              (diff (gnosis-import--diff file)))
          (should (equal (mapcar #'car (nth (if existing 1 0) diff)) (list id)))
          (gnosis-import--apply-changes
           file (unless existing (list id)) (when existing (list id))
           (nth 2 diff) (nth 3 diff))
          (when existing
            (should (equal before (gnosis-test-agent-eval--study-evidence)))))
        (gnosis-db--check-schema gnosis-db 11)
        (should-not (gnosis-select '* 'extras `(= id ,id)))
        (let ((before (gnosis-test-agent-eval--study-evidence)))
          (save-window-excursion
            (unwind-protect
                (progn
                  (gnosis-edit-thema id)
                  (goto-char (point-min))
                  (re-search-forward "^\\*\\* Parathema")
                  (forward-line 1)
                  (insert teaching)
                  (should (equal teaching (nth 5 (car (gnosis-export-parse-themata)))))
                  (should (eq (key-binding (kbd "C-c C-c")) #'gnosis-save))
                  (call-interactively (key-binding (kbd "C-c C-c")))
                  (should-not (get-buffer "*Gnosis Edit*"))
                  (should (equal before (gnosis-test-agent-eval--study-evidence)))
                  (gnosis-sqlite-close gnosis-db)
                  (setq gnosis-db (gnosis-sqlite-open gnosis-test--db-file))
                  (gnosis-db-init)
                  (gnosis-edit-thema id)
                  (let ((row (car (gnosis-export-parse-themata))))
                    (should (equal (nth 5 row) teaching))
                    (should (equal (nth 4 row) (list gnosis-test-agent-eval--answer)))
                    (should (equal (nth 9 row) gnosis-test-agent-eval--rubric)))
                  (should (equal before (gnosis-test-agent-eval--study-evidence)))
                  (call-interactively (key-binding (kbd "C-c C-k"))))
              (when (get-buffer "*Gnosis Edit*") (kill-buffer "*Gnosis Edit*")))))))))

(ert-deftest gnosis-test-agent-eval-content-import-new-save ()
  (gnosis-test-agent-eval--import-save nil))

(ert-deftest gnosis-test-agent-eval-content-import-existing-save ()
  (gnosis-test-agent-eval--import-save t))

(ert-deftest gnosis-test-agent-eval-content-update-extras ()
  "Persist absent extras and preserve existing images and nil/empty values."
  (dolist (image '(nil "" "review.png"))
    (dolist (parathema '(nil "" "Teaching α"))
      (dolist (existing '(nil t))
        (gnosis-test-with-db
          (let ((id (gnosis-test-agent-eval--add)))
            (if existing
                (gnosis-sqlite-execute
                 gnosis-db "UPDATE extras SET review_image = ? WHERE id = ?"
                 (list image id))
              (gnosis--delete 'extras `(= id ,id)))
            (gnosis-update-thema id "Explain causality" nil
                                 (list gnosis-test-agent-eval--answer)
                                 parathema nil nil)
            (should (equal (gnosis-select '[parathema review-image] 'extras `(= id ,id))
                           (list (list parathema (when existing image)))))))))))

(ert-deftest gnosis-test-agent-eval-content-extras-rollback ()
  "A later failure rolls back a newly inserted extras row and content."
  (gnosis-test-with-db
    (let* ((id (gnosis-test-agent-eval--add))
           (delete (symbol-function 'gnosis--delete)))
      (gnosis--delete 'extras `(= id ,id))
      (let ((before (gnosis--draft-content gnosis-db id)))
        (cl-letf (((symbol-function 'gnosis--delete)
                   (lambda (table &rest arguments)
                     (if (eq table 'thema-links)
                         (error "Injected link resync failure")
                       (apply delete table arguments)))))
          (should-error
           (gnosis-update-thema id "Changed question" nil '("Changed reference")
                                "Authored teaching" nil nil)))
        (should-not (gnosis-select '* 'extras `(= id ,id)))
        (should (equal before (gnosis--draft-content gnosis-db id)))))))

(provide 'gnosis-test-agent-eval-content)
;;; gnosis-test-agent-eval-content.el ends here

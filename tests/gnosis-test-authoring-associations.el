;;; gnosis-test-authoring-associations.el --- Authoring associations -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;;; Commentary:
;; Preserve source-link sets and exact tag identities at authoring boundaries.

;;; Code:

(require 'ert)
(require 'gnosis-test-draft-ownership)
(require 'gnosis-dashboard)

(ert-deftest gnosis-test-authoring-repeated-links-domain ()
  "Create and replace association sets without changing caller-owned values."
  (gnosis-test-with-db
    (let* ((links (list "first" "first" "second" "first"))
           (before (copy-sequence links))
           (question "Question [[id:first][one]] [[id:first][again]]")
           (context "See [[id:first][one]] and [[id:second][two]]."))
      (gnosis-add-thema-fields "basic" question nil '("Answer") context
                              '("good") 0 links nil 123)
      (should (equal links before))
      (should (equal (sort (gnosis-select 'dest 'thema-links '(= source 123) t)
                          #'string<) '("first" "second")))
      (gnosis-update-thema 123 question nil '("Answer") context '("good") links)
      (should (equal links before))
      (should (equal (gnosis-get 'keimenon 'themata '(= id 123)) question))
      (should (equal (gnosis-get 'parathema 'extras '(= id 123)) context))
      (should (= 2 (length (gnosis-select 'dest 'thema-links '(= source 123))))))
    (sqlite-execute gnosis-db
                    "CREATE TEMP TRIGGER refuse_link BEFORE INSERT ON thema_links
                     WHEN NEW.dest = '\"blocked\"'
                     BEGIN SELECT RAISE(ABORT, 'Blocked source'); END")
    (let ((before (gnosis-test-draft--rows)))
      ;; Real constraint failures must still abort both domain transactions.
      (should-error
       (gnosis-update-thema 123 "Changed" nil '("Wrong") "Changed"
                            '("changed") '("valid" "valid" "blocked")))
      (should (equal before (gnosis-test-draft--rows)))
      (should-error
       (gnosis-add-thema-fields "basic" "Wrong" nil '("Wrong") "Wrong"
                               '("changed") 0 '("valid" "valid" "blocked") nil 456))
      (should (equal before (gnosis-test-draft--rows))))))

(ert-deftest gnosis-test-authoring-repeated-links-native ()
  "Save repeated sources within and across fields, then remove an association."
  (gnosis-test-with-db
    (gnosis-test-draft--with-editor
      (let ((question "Question [[id:target][source]] [[id:target][again]]")
            (context "See [[id:target][source]] and [[id:other][other]]."))
        (gnosis-add-thema "basic" question nil "Answer" context)
        (call-interactively (key-binding (kbd "C-c C-c")))
        (let ((id (car (gnosis-select 'id 'themata nil t))))
          (should (equal (gnosis-get 'keimenon 'themata `(= id ,id)) question))
          (should (equal (gnosis-get 'parathema 'extras `(= id ,id)) context))
          (gnosis-edit-thema id)
          (call-interactively (key-binding (kbd "C-c C-c")))
          (should (equal (sort (gnosis-select 'dest 'thema-links `(= source ,id) t)
                              #'string<) '("other" "target")))
          (gnosis-edit-thema id)
          (goto-char (point-min))
          (search-forward " and [[id:other][other]]")
          (replace-match "" t t)
          (call-interactively (key-binding (kbd "C-c C-c")))
          (should (equal (gnosis-select 'dest 'thema-links `(= source ,id) t)
                         '("target"))))))))

(ert-deftest gnosis-test-authoring-dashboard-tags-refuse-loss ()
  "Dashboard tags keep exact names even when native Org cannot edit them."
  (gnosis-test-with-db
    (gnosis-test-draft--with-editor
      (gnosis-test--add-basic-thema "Question" "Answer" '("good") "Context" 123)
      (with-temp-buffer
        (gnosis-dashboard-output-themata '(123))
        (goto-char (point-min))
        (cl-letf (((symbol-function 'completing-read-multiple)
                   (lambda (&rest _) '("+needs-work"))))
          (call-interactively #'gnosis-dashboard-modify-tags)))
      (should (equal (sort (gnosis-get-tags-for-ids '(123)) #'string<)
                     '("good" "needs-work")))
      (let ((before (gnosis-test-draft--rows))
            (buffer (current-buffer))
            (text (buffer-string)))
        (should-error (gnosis-edit-thema 123) :type 'user-error)
        (should (eq buffer (current-buffer)))
        (should (equal text (buffer-string)))
        (should-not (get-buffer "*Gnosis Edit*"))
        (should (equal before (gnosis-test-draft--rows))))
      ;; Explicit domain removal remains available, without normalizing the name.
      (gnosis-modify-thema-tags '(123) nil '("needs-work"))
      (gnosis-edit-thema 123)
      (call-interactively (key-binding (kbd "C-c C-c")))
      (should (equal (gnosis-get-tags-for-ids '(123)) '("good"))))))

(ert-deftest gnosis-test-authoring-org-tags-render-refuses-loss ()
  "Reject unrepresentable retained tags before mutating an Org destination."
  (dolist (tag '("needs-work" "two words" "a:b" "" "line\nbreak"))
    (with-temp-buffer
      (org-mode)
      (insert "Existing export or draft text\n")
      (let ((before (buffer-string)))
        (should-error
         (gnosis-export--insert-thema "123" "basic" "Q" nil "A" "P"
                                      (list "good" tag))
         :type 'user-error)
        (should (equal before (buffer-string)))))))

(ert-deftest gnosis-test-authoring-org-tags-typed-refusal ()
  "Malformed typed tag syntax never becomes a replacement empty tag set."
  (dolist (suffix '(" :needs-work:good:" " :two words:" " :unfinished"))
    (gnosis-test-with-db
      (gnosis-test-draft--with-editor
        (gnosis-test--add-basic-thema "Question" "Answer" '("good") "Context" 123)
        (gnosis-edit-thema 123)
        (goto-char (point-min))
        (search-forward "* Thema :good:")
        (replace-match (concat "* Thema" suffix) t t)
        (let ((before (gnosis-test-draft--rows))
              (text (buffer-string))
              (draft (current-buffer)))
          (should-error (call-interactively (key-binding (kbd "C-c C-c")))
                        :type 'user-error)
          (should (eq draft (current-buffer)))
          (should (equal text (buffer-string)))
          (should (equal before (gnosis-test-draft--rows)))
          (call-interactively (key-binding (kbd "C-c C-k")))
          (should-not (buffer-live-p draft))
          (should (equal before (gnosis-test-draft--rows))))))))

(ert-deftest gnosis-test-authoring-org-tags-valid-roundtrip-and-removal ()
  "Native Unicode tags survive edit/save; explicit empty tags still clear."
  (gnosis-test-with-db
    (gnosis-test-draft--with-editor
      (let ((tags '("good" "needs_work" "Ελληνικά" "日本語" "@home" "#topic" "100%")))
        (gnosis-add-thema "basic" "Question" nil "Answer" "Context" tags)
        (call-interactively (key-binding (kbd "C-c C-c")))
        (let ((id (car (gnosis-select 'id 'themata nil t))))
          (gnosis-edit-thema id)
          (call-interactively (key-binding (kbd "C-c C-c")))
          (should (equal (sort (gnosis-get-tags-for-ids (list id)) #'string<)
                         (sort (copy-sequence tags) #'string<)))
          (gnosis-edit-thema id)
          (goto-char (point-min))
          (search-forward "* Thema")
          (delete-region (point) (line-end-position))
          (call-interactively (key-binding (kbd "C-c C-c")))
          (should-not (gnosis-get-tags-for-ids (list id))))))))

(ert-deftest gnosis-test-authoring-tag-prompt-refuses-loss ()
  "The native tag prompt must not normalize an accepted exact tag identity."
  (gnosis-test-with-db
    (gnosis-test-draft--with-editor
      (gnosis-add-thema "basic" "Question" nil "Answer" "Context" '("good"))
      (let ((before (buffer-string)))
        (cl-letf (((symbol-function 'completing-read-multiple)
                   (lambda (&rest _) '("needs-work"))))
          (should-error (call-interactively (key-binding (kbd "C-c C-q")))
                        :type 'user-error))
        (should (equal before (buffer-string)))))))

(ert-deftest gnosis-test-authoring-retained-tags-exchange ()
  "Content exchange retains exact names that native Org cannot represent."
  (gnosis-test-with-db
    (let* ((tags '("good" "needs-work" "a:b" "two words" "Ελληνικά"))
           (file (expand-file-name "content.db" gnosis-dir)))
      (gnosis-test--add-basic-thema "Question" "Answer" tags "Context" 123)
      (gnosis-export-db file)
      (gnosis-test-with-db
        (gnosis-import--apply-changes
         file '(123) nil (gnosis-import--file-sha256 file))
        (should (equal (sort (gnosis-get-tags-for-ids '(123)) #'string<)
                       (sort (copy-sequence tags) #'string<)))
        (gnosis-test--add-basic-thema "Neighbor" "Answer" '("good") "" 456)
        (gnosis-test-draft--with-editor
          (let ((before (gnosis-test-draft--rows)))
            (should-error (gnosis-edit-thema 123) :type 'user-error)
            (should (equal before (gnosis-test-draft--rows))))
          (with-temp-buffer
            (org-mode)
            (insert "Existing export\n")
            (let ((before (buffer-string)))
              (should-error (gnosis-export--insert-themata '(456 123))
                            :type 'user-error)
              (should (equal before (buffer-string))))))))))

(ert-deftest gnosis-test-authoring-unrepresentable-create-preserves-buffer ()
  "Unrepresentable creation tags never open or replace a native draft."
  (gnosis-test-with-db
    (gnosis-test-draft--with-editor
      (with-temp-buffer
        (insert "Unrelated unsaved text")
        (let ((buffer (current-buffer))
              (text (buffer-string))
              (before (gnosis-test-draft--rows)))
          (should-error
           (gnosis-add-thema "basic" "Q" nil "A" "P" '("needs-work"))
           :type 'user-error)
          (should (eq buffer (current-buffer)))
          (should (equal text (buffer-string)))
          (should-not (get-buffer "*Gnosis NEW*"))
          (should (equal before (gnosis-test-draft--rows))))))))

(provide 'gnosis-test-authoring-associations)
;;; gnosis-test-authoring-associations.el ends here

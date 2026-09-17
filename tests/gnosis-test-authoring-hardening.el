;;; gnosis-test-authoring-hardening.el --- Authoring recovery tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;;; Commentary:

;; Exercise import failures and compositional text/index transformations.

;;; Code:

(require 'ert)
(require 'gnosis-anki)
(require 'gnosis-journal)
(require 'gnosis-test-helpers)

(ert-deftest gnosis-test-authoring-html-boundaries ()
  "Preserve line and block boundaries without joining words."
  (dolist (html '("alpha<br>beta<br/>gamma<br />delta"
                  "alpha<div>beta</div>gamma<p>delta</p>"))
    (should (equal (split-string (gnosis-anki--html-to-org html) "\n+" t)
                   '("alpha" "beta" "gamma" "delta")))))

(ert-deftest gnosis-test-authoring-html-cloze-lines ()
  "Convert markup inside a cloze without losing the deletion."
  (let ((items (gnosis-anki--parse-cloze-note
                (concat "{{c1::alpha<br>beta}}" "\x1f" "extra") " test "
                (make-hash-table :test 'equal)
                (make-hash-table :test 'equal))))
    (should (= (length items) 1))
    (should (equal (plist-get (car items) :answer) '("alpha\nbeta")))
    (should (equal (plist-get (car items) :keimenon) "alpha\nbeta"))))

(ert-deftest gnosis-test-authoring-bare-id-links ()
  "Extract both bracket link forms, not other link types or plain IDs."
  (should (equal (gnosis-extract-id-links
                  "[[id:bare]] [[id:described][A description]] id:plain [[https://x][id:no]]")
                 '("bare" "described")))
  (should (equal (gnosis-extract-id-links "[[id:a]] [[id:b]]" 8) '("b"))))

(ert-deftest gnosis-test-authoring-many-id-links ()
  "Large documents do not exhaust the Lisp evaluation stack."
  (let* ((ids (mapcar #'number-to-string (number-sequence 1 1000)))
         (text (mapconcat (lambda (id) (format "[[id:%s][x]]" id)) ids " ")))
    (should (equal (gnosis-extract-id-links text) ids))))

(ert-deftest gnosis-test-authoring-tag-rename-simultaneous ()
  "Moving targets, cycles, merges and deletions use the original tag set."
  (gnosis-test-with-db
    (dolist (case '((("a" . "aa") ("aa" . "aaa"))
                    (("a" . "aa") ("aa" . "a"))
                    (("a" . "aa") ("aa" . ""))
                    (("a" . "z") ("aa" . "z"))))
      (let* ((id (gnosis-test--add-basic-thema "Q" "A" '("a" "aa" "keep")))
             (expected (sort (delete-dups
                              (cl-loop for tag in '("a" "aa" "keep")
                                       for final = (or (cdr (assoc tag case)) tag)
                                       unless (string-empty-p final) collect final))
                             #'string<)))
        (gnosis--tag-rename-batch case)
        (should (equal (sort (gnosis-select 'tag 'thema-tag `(= thema-id ,id) t)
                            #'string<)
                       expected))))))

(ert-deftest gnosis-test-authoring-tag-rename-rollback ()
  "A failed final tag write preserves all original tags."
  (gnosis-test-with-db
    (let ((id (gnosis-test--add-basic-thema "Q" "A" '("a" "aa"))))
      (sqlite-execute gnosis-db
                      "CREATE TRIGGER fail_tag BEFORE INSERT ON thema_tag
                       BEGIN SELECT RAISE(ABORT, 'controlled tag failure'); END")
      (should-error (gnosis--tag-rename-batch '(("a" . "aa") ("aa" . "aaa"))))
      (should (equal (sort (gnosis-select 'tag 'thema-tag `(= thema-id ,id) t)
                          #'string<) '("a" "aa"))))))

(ert-deftest gnosis-test-authoring-anki-note-retry ()
  "A failure between bounded writes rolls back every sibling of a note."
  (gnosis-test-with-db
    (let* ((gnosis-anki--chunk-size 1)
           (items (cl-loop for (guid text) in '(("first" "one")
                                               ("siblings" "two")
                                               ("siblings" "three"))
                           collect (list :guid guid :type "basic" :keimenon text
                                         :hypothesis '("") :answer '("A")
                                         :parathema "" :tags '("test"))))
           (pending nil))
      (cl-letf (((symbol-function 'gnosis-anki--parse-anki-db)
                 (lambda (_) (cons 0 items)))
                ((symbol-function 'run-with-timer)
                 (lambda (_delay _repeat function &rest args)
                   (setq pending (lambda () (apply function args))))))
        (sqlite-execute gnosis-db
                        "CREATE TRIGGER fail_sibling BEFORE INSERT ON themata
                         WHEN NEW.keimenon = '\"three\"'
                         BEGIN SELECT RAISE(ABORT, 'controlled sibling failure'); END")
        (gnosis-anki--import-db "prepared.anki2")
        (while pending
          (let ((next pending)) (setq pending nil) (funcall next)))
        (should (equal (gnosis-select 'keimenon 'themata nil t) '("one")))
        (sqlite-execute gnosis-db "DROP TRIGGER fail_sibling")
        (gnosis-anki--import-db "prepared.anki2")
        (while pending
          (let ((next pending)) (setq pending nil) (funcall next)))
        (should (equal (sort (gnosis-select 'keimenon 'themata nil t) #'string<)
                       '("one" "three" "two")))
        (dolist (table '(extras scheduler_baseline scheduler_state thema_tag))
          (should (= 3 (caar (sqlite-select gnosis-db
                                           (format "SELECT COUNT(*) FROM %s" table))))))))))

(ert-deftest gnosis-test-authoring-anki-cleanup-parse-failure ()
  "Temporary extraction is removed even if parsing fails before timers."
  (let* ((dir (make-temp-file "gnosis-anki-cleanup-" t))
         (file (expand-file-name "bad.anki2" dir)))
    (unwind-protect
        (progn
          (with-temp-file file (insert "not SQLite"))
          (should-error (gnosis-anki--import-db file t))
          (should-not (file-exists-p dir)))
      (when (file-exists-p dir) (delete-directory dir t)))))

(ert-deftest gnosis-test-authoring-anki-cleanup-finalize-failure ()
  "Finalization errors and quits retain committed rows after extraction cleanup."
  (dolist (fault '(error quit))
    (gnosis-test-with-db
      (let* ((dir (make-temp-file "gnosis-anki-finalize-" t))
             (file (expand-file-name "collection.anki2" dir))
             (items '((:guid "finalize" :type "basic" :keimenon "Q"
                            :hypothesis ("") :answer ("A")
                            :parathema "" :tags ("test"))))
             pending reached)
        (unwind-protect
            (cl-letf (((symbol-function 'gnosis-anki--parse-anki-db)
                       (lambda (_) (cons 0 items)))
                      ((symbol-function 'run-with-timer)
                       (lambda (_delay _repeat fn &rest args)
                         (push (lambda () (apply fn args)) pending))))
              (with-temp-file file (insert "Prepared collection"))
              (gnosis-anki--import-db file t "imported" t "test.apkg")
              (should-not (file-exists-p dir))
              (should (= 1 (length pending)))
              (let ((before (sqlite-select gnosis-db "SELECT * FROM themata")))
                (should (= 1 (length before)))
                (cl-letf (((symbol-function 'gnosis-anki--commit-import)
                           (lambda (count source)
                             (setq reached (list count source))
                             (signal fault '("Controlled finalization failure")))))
                  (let ((result (condition-case err (funcall (pop pending))
                                  (quit err))))
                    (when (eq fault 'quit)
                      (should (equal result '(quit "Controlled finalization failure"))))))
                (should (equal reached '(1 "test.apkg")))
                (should-not pending)
                ;; Retry the prepared note; completion failure is not SQL failure.
                (gnosis-anki--import-db "prepared.anki2")
                (should-not pending)
                (should (equal before (sqlite-select gnosis-db "SELECT * FROM themata")))
                (dolist (table '(extras scheduler_baseline scheduler_state))
                  (should (= 1 (caar (sqlite-select
                                     gnosis-db (format "SELECT COUNT(*) FROM %s" table))))))
                (let ((id (caar before)))
                  (should (= 1 (gnosis-get 'suspended 'scheduler-state `(= thema-id ,id))))
                  (should (equal (sort (gnosis-select 'tag 'thema-tag `(= thema-id ,id) t)
                                       #'string<)
                                 '("imported" "test"))))))
          (when (file-exists-p dir) (delete-directory dir t)))))))

(defun gnosis-test-authoring--file-bytes (file)
  "Return FILE's literal contents."
  (with-temp-buffer
    (insert-file-contents-literally file)
    (buffer-string)))

(ert-deftest gnosis-test-authoring-journal-recurring-completion ()
  "Rebuilding indexes never replays checkboxes; save and sync never complete tasks."
  (dolist (layout '(separate inside outside))
    (gnosis-test-with-db
      (let* ((gnosis-nodes-dir (expand-file-name "nodes" gnosis-dir))
             (gnosis-journal-dir (expand-file-name "journal" gnosis-nodes-dir))
             (gnosis-journal-file
              (pcase layout
                ('inside (expand-file-name "single.org" gnosis-nodes-dir))
                ('outside (expand-file-name "single.org" gnosis-dir))))
             (file (expand-file-name "todos.org" gnosis-dir))
             (gnosis-journal-todo-files (list file))
             (today (format-time-string "%Y-%m-%d"))
             (tomorrow (format-time-string "%Y-%m-%d"
                                           (time-add (current-time) (days-to-time 1))))
             (old-file (or gnosis-journal-file
                           (expand-file-name "old.org" gnosis-journal-dir)))
             (today-file (or gnosis-journal-file
                             (expand-file-name "today.org" gnosis-journal-dir)))
             (original (format (concat "* TODO Exercise\nSCHEDULED: <%s +1d>\n"
                                       ":PROPERTIES:\n:LAST_DONE_DATE: 2000-01-01\n:END:\n")
                               today))
             (org-id-track-globally nil)
             (create-lockfiles nil)
             (org-log-done nil)
             (org-log-repeat nil))
        (unwind-protect
            (save-window-excursion
              (make-directory gnosis-journal-dir t)
              (with-temp-file file (insert original))
              (with-temp-file old-file
                (insert (if gnosis-journal-file
                            "#+title: Journal\n* 2000-01-01\n"
                          "#+title: 2000-01-01\n* Goals\n")
                        ":PROPERTIES:\n:ID: old-entry\n:END:\n+ [X] Exercise\n"))
              (dolist (force '(nil t))
                (gnosis-nodes-db-sync force)
                (should (equal original (gnosis-test-authoring--file-bytes file))))
              (with-current-buffer (find-file-noselect today-file)
                (goto-char (point-max))
                (insert (if gnosis-journal-file (format "\n* %s\n" today)
                          (format "#+title: %s\n* Goals\n" today))
                        ":PROPERTIES:\n:ID: today-entry\n:END:\n+ [ ] Exercise\n")
                (save-buffer)
                (should (equal original (gnosis-test-authoring--file-bytes file)))
                (forward-line -1)
                (org-toggle-checkbox)
                ;; Even today's check is not completion authority during sync.
                (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
                  (gnosis-nodes-db-force-sync))
                (should (equal original (gnosis-test-authoring--file-bytes file)))
                (save-buffer))
              (with-current-buffer (find-file-noselect file)
                (goto-char (point-min))
                (should (equal (org-get-todo-state) "TODO"))
                (should (equal (org-entry-get nil "LAST_DONE_DATE")
                               "2000-01-01")))
              (let ((unchanged (gnosis-test-authoring--file-bytes file)))
                (with-current-buffer (find-file-noselect today-file)
                  (goto-char (point-max))
                  (insert "\nSaved again.\n")
                  (save-buffer))
                (gnosis-nodes-db-sync t)
                (should (equal unchanged (gnosis-test-authoring--file-bytes file)))))
          (dolist (path (delete-dups (list file old-file today-file)))
            (when-let* ((buffer (get-file-buffer path)))
              (with-current-buffer buffer (set-buffer-modified-p nil))
              (kill-buffer buffer))))))))

(ert-deftest gnosis-test-authoring-journal-outside-directory ()
  "A configured external journal indexes, refreshes and visits by identity."
  (gnosis-test-with-db
    (let* ((gnosis-nodes-dir (expand-file-name "nodes" gnosis-dir))
           (gnosis-journal-dir (expand-file-name "journal" gnosis-nodes-dir))
           (gnosis-journal-file (expand-file-name "single.org" gnosis-dir))
           (gnosis-journal-todo-files nil)
           (org-id-track-globally nil))
      (unwind-protect
          (save-window-excursion
            (make-directory gnosis-journal-dir t)
            (with-temp-file gnosis-journal-file
              (insert "#+title: Journal\n* Entry\n:PROPERTIES:\n:ID: external-entry\n:END:\n"))
            (gnosis-nodes-update-file gnosis-journal-file)
            (should-not (gnosis-select 'id 'nodes nil t))
            (should (equal (gnosis-select 'id 'journal nil t) '("external-entry")))
            (should-not (gnosis-nodes--file-changed-p gnosis-journal-file 'journal))
            (gnosis-nodes-update-file gnosis-journal-file)
            (gnosis-nodes-goto-id "external-entry")
            (should (equal (buffer-file-name) gnosis-journal-file))
            (should (equal (org-entry-get nil "ID") "external-entry"))
            (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
              (gnosis-nodes-delete-file))
            (should-not (file-exists-p gnosis-journal-file))
            (should-not (gnosis-select 'id 'journal nil t)))
        (when-let* ((buffer (get-file-buffer gnosis-journal-file)))
          (kill-buffer buffer))))))

(ert-deftest gnosis-test-authoring-journal-filename-policy ()
  "Equivalent journal directory strings retain the journal GPG policy."
  (gnosis-test-with-db
    (let* ((gnosis-journal-dir (expand-file-name "journal" gnosis-dir))
           (gnosis-journal-as-gpg t)
           (gnosis-nodes-create-as-gpg nil)
           (gnosis-nodes-timestring nil)
           requested)
      (save-window-excursion
        (with-temp-buffer
          ;; Observe filename construction without writing encrypted data.
          (insert "Existing draft")
          (let ((buffer (current-buffer)))
            (cl-letf (((symbol-function 'find-file-noselect)
                       (lambda (file &rest _)
                         (setq requested file)
                         buffer)))
              (gnosis-nodes--create-file
               "My #Journal" (file-name-as-directory gnosis-journal-dir))))
          (should (equal (file-name-nondirectory requested)
                         "My_Journal.org.gpg")))))))

(ert-deftest gnosis-test-authoring-import-chunks-plain-data ()
  "Group siblings without changing inputs or merging anonymous notes."
  (let* ((items '((:guid "a" :keimenon "one") (:keimenon "anonymous")
                  (:guid "b" :keimenon "two") (:guid "a" :keimenon "three")
                  (:keimenon "anonymous")))
         (snapshot (copy-tree items))
         (chunks (gnosis-anki--import-chunks items 1)))
    (should (equal (mapcar #'length chunks) '(2 1 1 1)))
    (should (equal (mapcar (lambda (item) (plist-get item :keimenon))
                           (apply #'append chunks))
                   '("one" "three" "anonymous" "two" "anonymous")))
    (should (equal items snapshot))
    (should (equal chunks (gnosis-anki--import-chunks items 1)))))

(defun gnosis-test-authoring--index-before-journal-adoption (file)
  "Index FILE in the legacy nodes-table/basename format.
When GNOSIS_TEST_BASE_NODES names a previous release's source, exercise
its real indexer before restoring the candidate implementation."
  (if-let* ((base (getenv "GNOSIS_TEST_BASE_NODES")))
      (let ((candidate (locate-library "gnosis-nodes.el")))
        (unwind-protect
            (progn
              (load base nil t)
              (gnosis-nodes-update-file file))
          (load candidate nil t)))
    (let ((gnosis-journal-file nil)
          (file-key (symbol-function 'gnosis-nodes--file-key)))
      ;; Pin the historical encoding, not the candidate's filename policy.
      (cl-letf (((symbol-function 'gnosis-nodes--file-key)
                 (lambda (file journal)
                   (if journal (funcall file-key file journal)
                     (file-name-nondirectory file)))))
        (gnosis-nodes-update-file file)))))

(defun gnosis-test-authoring--index-snapshot ()
  "Return ordered rows for the complete node index."
  (mapcar (lambda (table)
            (sqlite-select gnosis-db
                           (format "SELECT * FROM %s ORDER BY 1, 2" table)))
          '(nodes journal node_tag node_links)))

(ert-deftest gnosis-test-authoring-journal-adopts-existing-index ()
  "Adopt old node identities atomically, then navigate and delete them."
  (dolist (inside '(nil t))
    (gnosis-test-with-db
      (let* ((gnosis-nodes-dir (expand-file-name "nodes" gnosis-dir))
             (gnosis-journal-dir (expand-file-name "journal" gnosis-nodes-dir))
             (gnosis-journal-file (expand-file-name "single.org"
                                                    (if inside gnosis-nodes-dir
                                                      gnosis-dir)))
             (other-file (expand-file-name "single.org"
                                           (if inside gnosis-journal-dir
                                             gnosis-nodes-dir)))
             (source-file (expand-file-name "source.org" gnosis-nodes-dir))
             (gnosis-journal-todo-files nil)
             (org-id-track-globally nil))
        (unwind-protect
            (save-window-excursion
              (make-directory gnosis-journal-dir t)
              (with-temp-file gnosis-journal-file
                (insert (concat "#+title: Journal\n* Entry :old:\n"
                                ":PROPERTIES:\n:ID: external-entry\n:END:\n"
                                "[[id:source][Source]]\n"
                                "* Second\n:PROPERTIES:\n:ID: external-second\n:END:\n"
                                "* Obsolete :old:\n:PROPERTIES:\n:ID: obsolete\n:END:\n"
                                "[[id:source][Source]]\n")))
              (gnosis-test-authoring--index-before-journal-adoption gnosis-journal-file)
              (with-temp-file other-file
                (insert "#+title: Other\n* Other :keep:\n:PROPERTIES:\n:ID: other\n:END:\n"))
              ;; The legacy basename key can represent both physical files.
              ;; Add the unrelated file without the legacy basename-wide delete.
              (gnosis-nodes--insert-file-data
               (if inside 'journal 'nodes) "single.org" "0"
               (gnosis-org-get-file-info other-file))
              (with-temp-file source-file
                (insert (concat "#+title: Source\n* Source\n:PROPERTIES:\n:ID: source\n:END:\n"
                                "[[id:external-entry][Entry]] [[id:other][Other]]\n"
                                "[[id:obsolete][Removed]]\n")))
              (gnosis-nodes-update-file source-file)
              (should (equal (gnosis-select 'file 'nodes '(= id "external-entry") t)
                             '("single.org")))
              (should-not (gnosis-select 'id 'journal '(= id "external-entry")))
              ;; Remove a previously indexed heading before the first adoption.
              (with-current-buffer (find-file-noselect gnosis-journal-file)
                (goto-char (point-min))
                (search-forward "* Obsolete")
                (beginning-of-line)
                (delete-region (point) (point-max)))
              (let ((before (gnosis-test-authoring--index-snapshot)))
                (sqlite-execute gnosis-db
                                "CREATE TRIGGER fail_journal BEFORE INSERT ON journal
                                 WHEN NEW.id = '\"external-second\"'
                                 BEGIN SELECT RAISE(ABORT, 'controlled journal failure'); END")
                (should-error (gnosis-nodes-update-file gnosis-journal-file))
                (should (equal before (gnosis-test-authoring--index-snapshot)))
                (sqlite-execute gnosis-db "DROP TRIGGER fail_journal"))
              ;; Save the edited file while retaining surviving incoming references.
              (with-current-buffer (find-file-noselect gnosis-journal-file)
                (goto-char (point-min))
                (search-forward ":old:")
                (replace-match ":new:")
                (save-buffer))
              (dolist (id '("external-entry" "external-second"))
                (should-not (gnosis-select 'id 'nodes `(= id ,id)))
                (should (equal (gnosis-select 'file 'journal `(= id ,id) t)
                               (list (file-relative-name gnosis-journal-file gnosis-journal-dir)))))
              (dolist (table '(nodes journal))
                (should-not (gnosis-select 'id table '(= id "obsolete"))))
              (should-not (gnosis-select 'tag 'node-tag '(= node-id "obsolete")))
              (should-not (gnosis-select 'source 'node-links '(= dest "obsolete")))
              (should-not (gnosis-select 'dest 'node-links '(= source "obsolete")))
              (should-not (gnosis-select 'tag 'node-tag '(= node-id "external-entry")))
              (should (equal (read (car (gnosis-select 'tags 'journal '(= id "external-entry") t)))
                             '("new")))
              (should-not (gnosis-select 'dest 'node-links '(= source "external-entry")))
              (should (equal (gnosis-select 'source 'node-links '(= dest "external-entry") t)
                             '("source")))
              (gnosis-nodes-update-file gnosis-journal-file)
              (gnosis-nodes-goto-id "external-second")
              (should (equal (buffer-file-name) gnosis-journal-file))
              (should (equal (org-entry-get nil "ID") "external-second"))
              (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
                (gnosis-nodes-delete-file))
              (should-not (file-exists-p gnosis-journal-file))
              (dolist (id '("external-entry" "external-second" "obsolete"))
                (dolist (table '(nodes journal))
                  (should-not (gnosis-select 'id table `(= id ,id))))
                (should-not (gnosis-select 'tag 'node-tag `(= node-id ,id)))
                (should-not (gnosis-select 'source 'node-links `(= dest ,id)))
                (should-not (gnosis-select 'dest 'node-links `(= source ,id))))
              (should (file-exists-p other-file))
              (should (equal (read (car (gnosis-select 'tags (if inside 'journal 'nodes)
                                                      '(= id "other") t)))
                             '("keep")))
              (should (equal (gnosis-select 'source 'node-links '(= dest "other") t) '("source")))
              (gnosis-nodes-goto-id "other")
              (should (equal (buffer-file-name) other-file)))
          (dolist (file (list gnosis-journal-file other-file source-file))
            (when-let* ((buffer (get-file-buffer file)))
              (with-current-buffer buffer (set-buffer-modified-p nil))
              (kill-buffer buffer))))))))

(ert-deftest gnosis-test-authoring-html-attributed-breaks ()
  "Preserve attributed breaks without treating longer tag names as breaks."
  (dolist (tag '("<br class=\"line\">" "<br clear=all />"
                 "<br style='clear: both'/>" "<br class=line clear=all>"))
    (should (equal (gnosis-anki--html-to-org (concat "alpha" tag "beta"))
                   "alpha\nbeta")))
  (should (equal (gnosis-anki--html-to-org "alpha<bravo>beta") "alphabeta")))

(ert-deftest gnosis-test-authoring-html-break-real-import ()
  "Persist attributed breaks through SQLite-source basic and cloze imports."
  (gnosis-test-with-db
    (let* ((file (expand-file-name "source.anki2" gnosis-dir))
           (anki (sqlite-open file))
           pending)
      (unwind-protect
          (progn
            (sqlite-execute anki "CREATE TABLE col (models text NOT NULL)")
            (sqlite-execute
             anki "INSERT INTO col VALUES (?)"
             (list (concat "{\"1\":{\"id\":1,\"name\":\"Basic\",\"type\":0,"
                           "\"flds\":[{\"name\":\"Front\"},{\"name\":\"Back\"}]},"
                           "\"2\":{\"id\":2,\"name\":\"Cloze\",\"type\":1,"
                           "\"flds\":[{\"name\":\"Text\"},{\"name\":\"Extra\"}]}}")))
            (sqlite-execute
             anki "CREATE TABLE notes (id integer PRIMARY KEY, guid text NOT NULL,
                   mid integer NOT NULL, mod integer NOT NULL, usn integer NOT NULL,
                   tags text NOT NULL, flds text NOT NULL, sfld integer NOT NULL,
                   csum integer NOT NULL, flags integer NOT NULL, data text NOT NULL)")
            (cl-loop for tag in '("<br class=\"line\">" "<br clear=all />"
                                  "<br style='clear: both'/>")
                     for n from 1
                     do (dolist (mid '(1 2))
                          (sqlite-execute
                           anki "INSERT INTO notes VALUES (?, ?, ?, 0, 0, '', ?, 0, 0, 0, '')"
                           (list (+ (* n 10) mid) (format "break-%s-%s" n mid) mid
                                 (if (= mid 1) (concat "Q\x1f" "alpha" tag "beta")
                                   (concat "{{c1::alpha" tag "beta::hint" tag "line}}")))))))
        (sqlite-close anki))
      (cl-letf (((symbol-function 'run-with-timer)
                 (lambda (_delay _repeat function &rest args)
                   (setq pending (lambda () (apply function args))))))
        (gnosis-anki--import-db file)
        (while pending
          (let ((next pending)) (setq pending nil) (funcall next))))
      (should (= 6 (length (gnosis-select 'id 'themata))))
      (dolist (type '("basic" "cloze"))
        (should (equal (gnosis-select 'answer 'themata `(= type ,type) t)
                       '(("alpha\nbeta") ("alpha\nbeta") ("alpha\nbeta")))))
      (should (equal (gnosis-select 'hypothesis 'themata '(= type "cloze") t)
                     '(("hint\nline") ("hint\nline") ("hint\nline")))))))

(ert-deftest gnosis-test-authoring-journal-ambiguous-adoption ()
  "Require an index-only rebuild when no legacy identity proves ownership."
  (gnosis-test-with-db
    (let* ((gnosis-nodes-dir (expand-file-name "nodes" gnosis-dir))
           (gnosis-journal-dir (expand-file-name "journal" gnosis-nodes-dir))
           (gnosis-journal-file (expand-file-name "single.org" gnosis-dir))
           (other-file (expand-file-name "single.org" gnosis-nodes-dir))
           (gnosis-journal-todo-files nil)
           (org-id-track-globally nil))
      (unwind-protect
          (save-window-excursion
            (make-directory gnosis-journal-dir t)
            (with-temp-file gnosis-journal-file
              (insert "#+title: Journal\n* Old\n:PROPERTIES:\n:ID: obsolete\n:END:\n"))
            (gnosis-test-authoring--index-before-journal-adoption gnosis-journal-file)
            (with-temp-file other-file
              (insert "#+title: Other\n* Other :keep:\n:PROPERTIES:\n:ID: other\n:END:\n"))
            (gnosis-nodes--insert-file-data
             'nodes "single.org" "0" (gnosis-org-get-file-info other-file))
            (with-current-buffer (find-file-noselect gnosis-journal-file)
              (erase-buffer)
              (insert "#+title: Journal\n* Replacement\n:PROPERTIES:\n:ID: new\n:END:\n"))
            (let ((before (gnosis-test-authoring--index-snapshot))
                  (disk (gnosis-test-authoring--file-bytes gnosis-journal-file)))
              (should-error (gnosis-nodes-update-file gnosis-journal-file) :type 'user-error)
              (should (equal before (gnosis-test-authoring--index-snapshot)))
              (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
                (gnosis-nodes-db-force-sync))
              (should (equal disk (gnosis-test-authoring--file-bytes gnosis-journal-file)))
              (should (buffer-modified-p (get-file-buffer gnosis-journal-file))))
            (should (equal (gnosis-select 'id 'nodes nil t) '("other")))
            (should (equal (gnosis-select 'id 'journal nil t) '("new")))
            (should (equal (gnosis-select 'tag 'node-tag '(= node-id "other") t) '("keep")))
            (gnosis-nodes-goto-id "new")
            (should (equal (buffer-file-name) gnosis-journal-file)))
        (dolist (file (list gnosis-journal-file other-file))
          (when-let* ((buffer (get-file-buffer file)))
            (with-current-buffer buffer (set-buffer-modified-p nil))
            (kill-buffer buffer)))))))

(ert-deftest gnosis-test-authoring-sync-dirty-lockfiles ()
  "Sync real dirty buffers without reading lockfiles or completing TODOs."
  (gnosis-test-with-db
    (let* ((gnosis-nodes-dir (expand-file-name "nodes" gnosis-dir))
           (gnosis-journal-dir (expand-file-name "journal" gnosis-nodes-dir))
           (gnosis-journal-file nil)
           (todo (expand-file-name "todo.org" gnosis-nodes-dir))
           (journal (expand-file-name "today.org" gnosis-journal-dir))
           (gnosis-journal-todo-files (list todo))
           (today (format-time-string "%Y-%m-%d"))
           (create-lockfiles t)
           (org-id-track-globally nil))
      (unwind-protect
          (save-window-excursion
            (make-directory gnosis-journal-dir t)
            (with-temp-file todo
              (insert (format "#+title: Tasks\n* TODO Exercise\nSCHEDULED: <%s +1d>\n" today)
                      ":PROPERTIES:\n:ID: task\n:END:\n"))
            (with-temp-file journal
              (insert (format "#+title: %s\n* Goals\n" today)
                      ":PROPERTIES:\n:ID: entry\n:END:\n+ [X] Exercise\n"))
            (dolist (file (list todo journal))
              (with-current-buffer (find-file-noselect file)
                (goto-char (point-max))
                (insert "Unsaved text\n"))
              (should (file-symlink-p (make-lock-file-name file))))
            (let ((disk (mapcar #'gnosis-test-authoring--file-bytes (list todo journal)))
                  (buffers (mapcar (lambda (file)
                                     (with-current-buffer (get-file-buffer file)
                                       (buffer-string)))
                                   (list todo journal))))
              (dolist (force '(nil t))
                (gnosis-nodes-db-sync force)
                (should (equal (gnosis-select 'id 'nodes nil t) '("task")))
                (should (equal (gnosis-select 'id 'journal nil t) '("entry")))
                (should (equal disk (mapcar #'gnosis-test-authoring--file-bytes
                                            (list todo journal))))
                (should (equal buffers (mapcar (lambda (file)
                                                 (with-current-buffer (get-file-buffer file)
                                                   (buffer-string)))
                                               (list todo journal))))
                (dolist (file (list todo journal))
                  (should (buffer-modified-p (get-file-buffer file)))))))
        (dolist (file (list todo journal))
          (when-let* ((buffer (get-file-buffer file)))
            (with-current-buffer buffer (set-buffer-modified-p nil))
            (kill-buffer buffer)))))))

(ert-deftest gnosis-test-authoring-journal-historical-save ()
  "Saving an old checked journal must not complete today's recurring TODO."
  (gnosis-test-with-db
    (let* ((gnosis-nodes-dir (expand-file-name "nodes" gnosis-dir))
           (gnosis-journal-dir (expand-file-name "journal" gnosis-nodes-dir))
           (gnosis-journal-file nil)
           (journal (expand-file-name "old.org" gnosis-journal-dir))
           (todo (expand-file-name "todo.org" gnosis-dir))
           (gnosis-journal-todo-files (list todo))
           (today (format-time-string "%Y-%m-%d"))
           (original (format (concat "* TODO Exercise\nSCHEDULED: <%s +1d>\n"
                                     ":PROPERTIES:\n:LAST_DONE_DATE: 2000-01-01\n:END:\n")
                             today))
           (org-id-track-globally nil)
           (org-log-done nil)
           (org-log-repeat nil))
      (unwind-protect
          (save-window-excursion
            (make-directory gnosis-journal-dir t)
            (with-temp-file todo (insert original))
            (with-temp-file journal
              (insert "#+title: 2000-01-02\n* Goals\n:PROPERTIES:\n:ID: old\n:END:\n"
                      "+ [X] Exercise\nSpeling\n"))
            (with-current-buffer (find-file-noselect journal)
              (should (memq #'gnosis-nodes-update-file after-save-hook))
              (goto-char (point-min))
              (search-forward "Speling")
              (replace-match "Spelling")
              (save-buffer))
            (should (equal original (gnosis-test-authoring--file-bytes todo)))
            (with-current-buffer (find-file-noselect todo)
              (goto-char (point-min))
              (should (equal (format-time-string "%Y-%m-%d" (org-get-scheduled-time (point)))
                             today))
              (should (equal (org-entry-get nil "LAST_DONE_DATE") "2000-01-01"))))
        (dolist (file (list journal todo))
          (when-let* ((buffer (get-file-buffer file)))
            (with-current-buffer buffer (set-buffer-modified-p nil))
            (kill-buffer buffer)))))))

(defun gnosis-test-authoring--legacy-sync-files (replacement)
  "Create a legacy external journal and changed ordinary namesake.
REPLACEMENT is the journal's new heading ID.  Return all fixture paths."
  (let ((other (expand-file-name "single.org" gnosis-nodes-dir))
        (source (expand-file-name "source.org" gnosis-nodes-dir)))
    (make-directory gnosis-journal-dir t)
    (with-temp-file other
      (insert "#+title: Other\n* Other :keep:\n:PROPERTIES:\n:ID: other\n:END:\n"))
    (gnosis-test-authoring--index-before-journal-adoption other)
    (with-temp-file gnosis-journal-file
      (insert "#+title: Journal\n* Entry :old:\n:PROPERTIES:\n:ID: entry\n:END:\n"
              "* Removed :old:\n:PROPERTIES:\n:ID: removed\n:END:\n"
              "[[id:source][Source]]\n"))
    (set-file-times gnosis-journal-file (encode-time 0 0 0 1 1 2000))
    ;; The actual old indexer overwrites the ordinary basename's rows.
    (gnosis-test-authoring--index-before-journal-adoption gnosis-journal-file)
    (with-temp-file source
      (insert "#+title: Source\n* Source :keep:\n:PROPERTIES:\n:ID: source\n:END:\n"
              "[[id:entry][Entry]] [[id:removed][Removed]] [[id:other][Other]]\n"))
    (gnosis-nodes-update-file source t)
    (with-temp-file other
      (insert "#+title: Other\n* Other revised :keep:\n:PROPERTIES:\n:ID: other\n:END:\n"))
    (set-file-times other (encode-time 0 0 0 1 1 2001))
    (with-temp-file gnosis-journal-file
      (insert (format "#+title: Journal\n* Entry :new:\n:PROPERTIES:\n:ID: %s\n:END:\n"
                      replacement)))
    (should (gnosis-nodes--file-changed-p other 'nodes))
    (should (equal (sort (gnosis-select 'id 'nodes '(= file "single.org") t) #'string<)
                   '("entry" "removed")))
    (list gnosis-journal-file other source)))

(ert-deftest gnosis-test-authoring-journal-sync-retires-legacy ()
  "Normal sync retires the complete journal snapshot before a namesake update."
  (gnosis-test-with-db
    (let* ((gnosis-nodes-dir (expand-file-name "nodes" gnosis-dir))
           (gnosis-journal-dir (expand-file-name "journal" gnosis-nodes-dir))
           (gnosis-journal-file (expand-file-name "single.org" gnosis-dir))
           (gnosis-journal-todo-files nil)
           (org-id-track-globally nil)
           (files (gnosis-test-authoring--legacy-sync-files "entry"))
           (bytes (mapcar #'gnosis-test-authoring--file-bytes files)))
      (gnosis-nodes-db-sync)
      (should-not (gnosis-select 'source 'node-links '(= dest "removed")))
      (should-not (gnosis-select 'dest 'node-links '(= source "removed")))
      (should-not (gnosis-select 'tag 'node-tag '(= node-id "removed")))
      (should (equal (sort (gnosis-select 'id 'nodes nil t) #'string<) '("other" "source")))
      (should (equal (gnosis-select 'id 'journal nil t) '("entry")))
      (should (equal (gnosis-select 'title 'nodes '(= id "other") t) '("Other revised")))
      (dolist (id '("entry" "other"))
        (should (equal (gnosis-select 'source 'node-links `(= dest ,id) t) '("source"))))
      (dolist (id '("other" "source"))
        (should (equal (gnosis-select 'tag 'node-tag `(= node-id ,id) t) '("keep"))))
      (should (equal bytes (mapcar #'gnosis-test-authoring--file-bytes files))))))

(ert-deftest gnosis-test-authoring-journal-sync-rejects-unknown-owner ()
  "Normal sync rejects unknown or conflicting snapshots; force stays index-only."
  (dolist (conflict '(nil t))
    (gnosis-test-with-db
      (let* ((gnosis-nodes-dir (expand-file-name "nodes" gnosis-dir))
             (gnosis-journal-dir (expand-file-name "journal" gnosis-nodes-dir))
             (gnosis-journal-file (expand-file-name "single.org" gnosis-dir))
             (gnosis-journal-todo-files nil)
             (org-id-track-globally nil)
             (files (gnosis-test-authoring--legacy-sync-files (if conflict "entry" "new"))))
        (when conflict
          ;; Both current files claim the same legacy snapshot.
          (with-temp-file (cadr files)
            (insert "#+title: Other\n* Conflict\n:PROPERTIES:\n:ID: entry\n:END:\n"))
          (set-file-times (cadr files) (encode-time 0 0 0 1 1 2001)))
        (let ((before (gnosis-test-authoring--index-snapshot))
              (bytes (mapcar #'gnosis-test-authoring--file-bytes files)))
          (should-error (gnosis-nodes-db-sync) :type 'user-error)
          (should (equal before (gnosis-test-authoring--index-snapshot)))
          (should (equal bytes (mapcar #'gnosis-test-authoring--file-bytes files)))
          (unless conflict
            (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
              (gnosis-nodes-db-force-sync))
            (should (equal (sort (gnosis-select 'id 'nodes nil t) #'string<) '("other" "source")))
            (should (equal (gnosis-select 'id 'journal nil t) '("new")))
            (should (equal bytes (mapcar #'gnosis-test-authoring--file-bytes files)))))))))

(defun gnosis-test-authoring--save-ordinary (file)
  "Edit ordinary FILE through its native save hook."
  (with-current-buffer (find-file-noselect file)
    (should gnosis-nodes-mode)
    (should (memq #'gnosis-nodes-update-file after-save-hook))
    (goto-char (point-max))
    (insert "Ordinary authoring edit.\n")
    ;; Emacs may report after-save-hook errors instead of signalling them.
    ;; Tests must inspect the complete durable index, not the return value.
    (condition-case nil (save-buffer) (error nil))))

(defun gnosis-test-authoring--checked-todo (todo)
  "Create recurring TODO and today's checked goal in the fixture journal."
  (with-temp-file todo
    (insert (format "* TODO Exercise\nSCHEDULED: <%s +1d>\n"
                    (format-time-string "%Y-%m-%d"))))
  (with-temp-buffer
    (insert-file-contents gnosis-journal-file)
    (goto-char (point-max))
    (insert (format "* %s\n+ [X] Exercise\n" (format-time-string "%Y-%m-%d")))
    (write-region (point-min) (point-max) gnosis-journal-file nil 'silent)))

(ert-deftest gnosis-test-authoring-ordinary-save-reconciles-legacy ()
  "An ordinary save reconciles legacy ownership before any normal sync."
  (gnosis-test-with-db
    (let* ((gnosis-nodes-dir (expand-file-name "nodes" gnosis-dir))
           (gnosis-journal-dir (expand-file-name "journal" gnosis-nodes-dir))
           (gnosis-journal-file (expand-file-name "single.org" gnosis-dir))
           (todo (expand-file-name "todo.org" gnosis-dir))
           (gnosis-journal-todo-files (list todo))
           (org-id-track-globally nil)
           (files (gnosis-test-authoring--legacy-sync-files "entry")))
      (unwind-protect
          (progn
            (gnosis-test-authoring--checked-todo todo)
            (let ((before (gnosis-test-authoring--index-snapshot))
                  (todo-bytes (gnosis-test-authoring--file-bytes todo)))
              ;; Fail the ordinary insert after journal adoption has run.
              (sqlite-execute gnosis-db
                              "CREATE TRIGGER fail_ordinary BEFORE INSERT ON nodes
                               WHEN NEW.id = '\"other\"'
                               BEGIN SELECT RAISE(ABORT, 'controlled ordinary failure'); END")
              (gnosis-test-authoring--save-ordinary (cadr files))
              (should (equal before (gnosis-test-authoring--index-snapshot)))
              (should (equal todo-bytes (gnosis-test-authoring--file-bytes todo)))
              (sqlite-execute gnosis-db "DROP TRIGGER fail_ordinary"))
            (let ((todo-bytes (gnosis-test-authoring--file-bytes todo)))
              (gnosis-test-authoring--save-ordinary (cadr files))
              (should (equal todo-bytes (gnosis-test-authoring--file-bytes todo))))
            (should (equal (gnosis-select 'id 'journal nil t) '("entry")))
            (dolist (sync '(nil t))
              (when sync (gnosis-nodes-db-sync))
              (should (equal (sort (gnosis-select 'id 'nodes nil t) #'string<)
                             '("other" "source")))
              (should-not (gnosis-select 'source 'node-links '(= dest "removed")))
              (should-not (gnosis-select 'dest 'node-links '(= source "removed")))
              (should-not (gnosis-select 'tag 'node-tag '(= node-id "removed")))
              (should-not (gnosis-select 'tag 'node-tag '(= node-id "entry")))
              (dolist (id '("entry" "other"))
                (should (equal (gnosis-select 'source 'node-links `(= dest ,id) t)
                               '("source"))))))
        (dolist (file (cons todo files))
          (when-let* ((buffer (get-file-buffer file)))
            (with-current-buffer buffer (set-buffer-modified-p nil))
            (kill-buffer buffer)))))))

(ert-deftest gnosis-test-authoring-ordinary-save-rejects-legacy ()
  "Unknown or conflicting ownership survives save, sync and delete attempts."
  (dolist (conflict '(nil t))
    (gnosis-test-with-db
      (let* ((gnosis-nodes-dir (expand-file-name "nodes" gnosis-dir))
             (gnosis-journal-dir (expand-file-name "journal" gnosis-nodes-dir))
             (gnosis-journal-file (expand-file-name "single.org" gnosis-dir))
             (todo (expand-file-name "todo.org" gnosis-dir))
             (gnosis-journal-todo-files (list todo))
             (org-id-track-globally nil)
             (files (gnosis-test-authoring--legacy-sync-files (if conflict "entry" "new"))))
        (unwind-protect
            (progn
              (when conflict
                (with-temp-file (cadr files)
                  (insert "#+title: Other\n* Conflict\n:PROPERTIES:\n:ID: entry\n:END:\n")))
              ;; A checked goal for today would complete a real recurring TODO
              ;; if reconciliation or force recovery borrowed save authority.
              (gnosis-test-authoring--checked-todo todo)
              (let ((before (gnosis-test-authoring--index-snapshot))
                    (todo-bytes (gnosis-test-authoring--file-bytes todo)))
                (gnosis-test-authoring--save-ordinary (cadr files))
                (should (equal before (gnosis-test-authoring--index-snapshot)))
                (should-error (gnosis-nodes-db-sync) :type 'user-error)
                (should (equal before (gnosis-test-authoring--index-snapshot)))
                (dolist (file (list (cadr files) gnosis-journal-file))
                  (with-current-buffer (find-file-noselect file)
                    (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
                      (should-error (gnosis-nodes-delete-file) :type 'user-error)))
                  (should (file-exists-p file))
                  (should (equal before (gnosis-test-authoring--index-snapshot))))
                (let ((bytes (mapcar #'gnosis-test-authoring--file-bytes files)))
                  (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
                    (gnosis-nodes-db-force-sync))
                  (should (equal bytes (mapcar #'gnosis-test-authoring--file-bytes files))))
                (should (equal todo-bytes (gnosis-test-authoring--file-bytes todo)))
                (should (equal (gnosis-select 'id 'journal nil t)
                               (list (if conflict "entry" "new"))))))
          (dolist (file (cons todo files))
            (when-let* ((buffer (get-file-buffer file)))
              (with-current-buffer buffer (set-buffer-modified-p nil))
              (kill-buffer buffer))))))))

(ert-deftest gnosis-test-authoring-delete-reconciles-legacy ()
  "Either public delete order resolves legacy ownership before erasure."
  (dolist (journal-first '(nil t))
    (gnosis-test-with-db
      (let* ((gnosis-nodes-dir (expand-file-name "nodes" gnosis-dir))
             (gnosis-journal-dir (expand-file-name "journal" gnosis-nodes-dir))
             (gnosis-journal-file (expand-file-name "single.org" gnosis-dir))
             (gnosis-journal-todo-files nil)
             (org-id-track-globally nil)
             (files (gnosis-test-authoring--legacy-sync-files "entry"))
             (targets (if journal-first (list (car files) (cadr files))
                        (list (cadr files) (car files)))))
        (unwind-protect
            (progn
              ;; Also retain an ordinary snapshot under the shared basename.
              (gnosis-nodes--insert-file-data
               'nodes "single.org" "0" (gnosis-org-get-file-info (cadr files)))
              (with-current-buffer (find-file-noselect (car targets))
                (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
                  (gnosis-nodes-delete-file)))
              (should-not (file-exists-p (car targets)))
              (should (file-exists-p (cadr targets)))
              (should (equal (gnosis-select 'source 'node-links
                                            `(= dest ,(if journal-first "other" "entry")) t)
                             '("source")))
              (dolist (id (if journal-first '("entry" "removed") '("other" "removed")))
                (dolist (table '(nodes journal))
                  (should-not (gnosis-select 'id table `(= id ,id))))
                (should-not (gnosis-select 'tag 'node-tag `(= node-id ,id)))
                (should-not (gnosis-select 'source 'node-links `(= dest ,id)))
                (should-not (gnosis-select 'dest 'node-links `(= source ,id))))
              (when journal-first
                ;; Keeping the configured path must not block the survivor's
                ;; next ordinary save after the journal is explicitly deleted.
                (gnosis-test-authoring--save-ordinary (cadr files))
                (should-not (gnosis-nodes--file-changed-p (cadr files) 'nodes))
                (gnosis-nodes-db-sync)))
          (dolist (file files)
            (when-let* ((buffer (get-file-buffer file)))
              (with-current-buffer buffer (set-buffer-modified-p nil))
              (kill-buffer buffer))))))))

(provide 'gnosis-test-authoring-hardening)
;;; gnosis-test-authoring-hardening.el ends here

;;; gnosis-test-core-integration.el --- Import and Git interactions -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;;; Commentary:
;; Cover cross-module behavior beyond the individual import and Git suites.

;;; Code:

(require 'ert)
(require 'gnosis-test-anki)
(require 'gnosis-test-vc)
(require 'gnosis-review)

(ert-deftest gnosis-test-core-integration-auto-push-owner ()
  "Automatic add, commit and push retain a killed relative-path initiator."
  (gnosis-test-vc--with-directories
    (let ((gnosis-testing nil)
          (gnosis-vc-auto-push t)
          (origin (generate-new-buffer " *gnosis-auto-origin*"))
          (remote (expand-file-name "remote.git" root)))
      (unwind-protect
          (progn
            (dolist (args (list '("init" "--quiet" "--initial-branch=main")
                               '("config" "user.name" "Gnosis Test")
                               '("config" "user.email" "test@example.invalid")
                               (list "init" "--bare" "--quiet" remote)
                               (list "remote" "add" "origin" remote)
                               '("config" "push.autoSetupRemote" "true")))
              (should (zerop (apply #'call-process "git" nil nil nil args))))
            (with-temp-file (expand-file-name "gnosis.db" a) (insert "Owned data"))
            (with-current-buffer origin
              (setq default-directory (file-name-as-directory root))
              (let ((gnosis-dir "a/"))
                (gnosis-vc--auto-commit "Owned automatic commit")))
            (kill-buffer origin)
            (setq gnosis-dir b default-directory b)
            (gnosis-test-vc--wait
             (lambda () (and (= (length processes) 3)
                             (eq (process-status (car processes)) 'exit))))
            (should (equal (reverse directories) (list a a a)))
            (should (zerop (process-exit-status (car processes))))
            (should-not (file-exists-p (expand-file-name ".git" b)))
            (let ((default-directory remote))
              (should (equal "Owned automatic commit"
                             (string-trim
                              (with-temp-buffer
                                (should (zerop (call-process
                                                "git" nil t nil "log" "main" "-1"
                                                "--format=%s")))
                                (buffer-string)))))))
        (when (buffer-live-p origin) (kill-buffer origin))))))

(ert-deftest gnosis-test-core-integration-auto-deferred-failure ()
  "Deferred launch and status failures leave accepted study evidence alone."
  (skip-unless (executable-find "git"))
  (dolist (mode '(due practice))
    (dolist (failure '(launch status))
      (gnosis-test-with-db
        (let* ((a gnosis-dir)
               (b (file-name-as-directory (expand-file-name "successor" a)))
               (id (gnosis-test--add-basic-thema "Question" "Answer"))
               (gnosis-testing nil)
               (gnosis-vc-auto-push t)
               (gnosis-review-basic-input 'typed)
               (before (gnosis-select '* 'scheduler-state))
               (table (if (eq mode 'due) 'review-events 'practice-events))
               (start (symbol-function 'start-process))
               (process-environment
                (append '("GIT_CONFIG_NOSYSTEM=1" "GIT_AUTHOR_NAME=Gnosis Test"
                          "GIT_AUTHOR_EMAIL=test@example.invalid"
                          "GIT_COMMITTER_NAME=Gnosis Test"
                          "GIT_COMMITTER_EMAIL=test@example.invalid")
                        (cl-remove-if (lambda (entry) (string-prefix-p "GIT_" entry))
                                      process-environment)))
               (buffers (buffer-list))
               processes directories messages sentinel)
          (make-directory b)
          (unwind-protect
              (cl-letf
                  (((symbol-function 'start-process)
                    (lambda (&rest args)
                      (push default-directory directories)
                      (when (and (eq failure 'launch) (equal (nth 3 args) "commit"))
                        (error "Deferred commit launch fault"))
                      (let ((process (apply start args)))
                        (push process processes)
                        process)))
                   ((symbol-function 'message)
                    (lambda (fmt &rest args) (push (apply #'format fmt args) messages)))
                   ((symbol-function 'read-string) (lambda (&rest _) "Answer"))
                   ((symbol-function 'read-char-choice) (lambda (&rest _) ?n)))
                (gnosis-review-loop (list id) mode)
                (should (derived-mode-p 'gnosis-review-summary-mode))
                (should (= (length processes) 1))
                (setq sentinel (process-sentinel (car processes)))
                (set-process-sentinel (car processes) #'ignore)
                (gnosis-test-vc--wait
                 (lambda () (eq (process-status (car processes)) 'exit)))
                (when (eq failure 'status)
                  (let ((hook (expand-file-name ".git/hooks/pre-commit" a)))
                    (with-temp-file hook (insert "#!/bin/sh\nexit 1\n"))
                    (set-file-modes hook #o700)))
                ;; The native study buffer has already retired.  Dispatch the
                ;; real predecessor explicitly to observe escaped callback errors.
                (setq gnosis-dir b default-directory b)
                (funcall sentinel (car processes) "finished\n")
                (gnosis-test-vc--wait
                 (lambda () (seq-every-p
                             (lambda (p) (memq (process-status p) '(exit signal)))
                             processes)))
                (should (equal (reverse directories) (list a a)))
                (should (seq-some
                         (lambda (text)
                           (string-match-p
                            (if (eq failure 'launch) "Automatic Git commit failed: Deferred"
                              "git commit failed") text)) messages))
                (should (= 1 (length (gnosis-select '* table))))
                (let ((events (gnosis-select '* table)))
                  (gnosis-review--show-summary (gnosis-review--read-session))
                  (should (equal events (gnosis-select '* table))))
                (when (eq mode 'practice)
                  (should (equal before (gnosis-select '* 'scheduler-state))))
                (should-not (gnosis-select '* (if (eq mode 'due)
                                                 'practice-events 'review-events)))
                (should (equal gnosis-dir b)))
            (dolist (process processes)
              (set-process-sentinel process #'ignore)
              (when (process-live-p process) (delete-process process)))
            (dolist (buffer (seq-difference (buffer-list) buffers))
              (kill-buffer buffer))))))))

(ert-deftest gnosis-test-core-integration-cloze-authoring ()
  "Inline multiline answers and literal hints survive native save and edit."
  (gnosis-test-with-db
    (let* ((answer "α/β/γ\nsecond line")
           (hint "\\alpha\n\\& \\1 tail\\")
           (source (concat "Before {{c1::" answer "::" hint "}} after"))
           (buffers (buffer-list)))
      (unwind-protect
          (progn
            (gnosis-add-thema "cloze" source nil nil "Context" '("literal"))
            (call-interactively (key-binding (kbd "C-c C-c")))
            (let* ((id (gnosis-get 'id 'themata))
                   (stored (gnosis-select '[keimenon hypothesis answer]
                                          'themata `(= id ,id) t)))
              (should (equal stored (list (concat "Before " answer " after")
                                         (list hint) (list answer))))
              (gnosis-edit-thema id)
              (call-interactively (key-binding (kbd "C-c C-c")))
              (should (equal stored (gnosis-select '[keimenon hypothesis answer]
                                                  'themata `(= id ,id) t)))
              (should (equal (substring-no-properties
                              (gnosis-cloze-add-hints
                               (gnosis-cloze--replace (car stored) (nth 2 stored)
                                                     gnosis-cloze-string)
                               (nth 1 stored)))
                             (concat "Before (" hint ") after")))))
        (dolist (buffer (seq-difference (buffer-list) buffers))
          (kill-buffer buffer))))))

(ert-deftest gnosis-test-core-integration-import-groups ()
  "Reject empty groups before GUID/ID grouping and count late duplicates once."
  (skip-unless (and (executable-find "7z") (executable-find "zstd")))
  (gnosis-test-with-db
    (let* ((root (make-temp-file "gnosis-combined-import-" t))
           (temporary-file-directory (file-name-as-directory root))
           (default-directory (file-name-as-directory root))
           (gnosis-anki--chunk-size 1)
           (hint "\\alpha\n\\& \\1 tail\\")
           (text (concat "{{c1::<b></b>}} {{c1::must not shift::rejected}} "
                         "{{c2::5 mg/kg/day::" hint "}} "
                         "{{c3::α\nβ::second\n\\1}}"))
           (generate (symbol-function 'gnosis-generate-ids))
           pending counts completions)
      (unwind-protect
          (progn
            (dolist (name '("a" "b"))
              (let* ((file (expand-file-name "collection.anki2" root))
                     (db (sqlite-open file)))
                (unwind-protect
                    (progn
                      (gnosis-test-anki--create-schema db)
                      (gnosis-test-anki--insert-notetype db 100 "Basic" 'basic)
                      (gnosis-test-anki--insert-notetype db 200 "Cloze" 'cloze)
                      (when (equal name "a")
                        (gnosis-test-anki--insert-note db 1 100 (concat "Q" "\x1f" "A") "literal"))
                      (gnosis-test-anki--insert-note db 2 200
                                                   (concat text "\x1f" "Context") "literal"))
                  (sqlite-close db))
                (should (zerop (call-process "zstd" nil nil nil "-q" file
                                            "-o" "collection.anki21b")))
                (should (zerop (call-process "7z" nil nil nil "a" "-tzip"
                                            (concat name ".apkg") "collection.anki21b")))
                (delete-file file)
                (delete-file "collection.anki21b")))
            (cl-letf (((symbol-function 'run-with-timer)
                       (lambda (_delay _repeat fn &rest args)
                         (setq pending (append pending (list (lambda () (apply fn args)))))))
                      ((symbol-function 'read-string) (lambda (&rest _) ""))
                      ((symbol-function 'y-or-n-p) (lambda (&rest _) nil))
                      ((symbol-function 'gnosis-generate-ids)
                       (lambda (count) (push count counts) (funcall generate count)))
                      ((symbol-function 'message)
                       (lambda (fmt &rest args)
                         (when (equal fmt "Anki import complete: %d imported, %d skipped")
                           (push args completions)))))
              (gnosis-import-anki "a.apkg")
              (should (= (length pending) 1))
              (gnosis-import-anki "b.apkg")
              ;; Both archives have transferred and released extraction bytes
              ;; before the still-owned normalized timer data is consumed.
              (should-not (directory-files root nil "\\`gnosis-anki-"))
              (should (equal (reverse counts) '(1 2 2)))
              (while pending (funcall (pop pending)))
              (should (equal (sort completions (lambda (a b) (< (car a) (car b))))
                             '((1 3) (2 1))))
              (should (equal (sqlite-select gnosis-db
                                            "SELECT source_guid, count(*) FROM themata
                                             GROUP BY source_guid ORDER BY source_guid")
                             '(("guid1" 1) ("guid2" 2))))
              (let* ((ids (mapcar #'car (sqlite-select gnosis-db
                                                      "SELECT id FROM themata WHERE source_guid='guid2'")))
                     (fields (mapcar (lambda (id)
                                       (gnosis-select '[answer hypothesis] 'themata `(= id ,id) t))
                                     ids)))
                (should (member (list '("5 mg/kg/day") (list hint)) fields))
                (should (member '(("α\nβ") ("second\n\\1")) fields)))
              (dolist (table '(extras scheduler_baseline scheduler_state thema_tag))
                (should (= 3 (caar (sqlite-select
                                   gnosis-db (format "SELECT count(*) FROM %s" table))))))
              (let ((before (sqlite-select gnosis-db "SELECT * FROM themata ORDER BY id")))
                (gnosis-import-anki "a.apkg")
                (gnosis-import-anki "b.apkg")
                (should-not pending)
                (should (equal before (sqlite-select gnosis-db "SELECT * FROM themata ORDER BY id")))))
            (should-not (sqlite-select gnosis-db "PRAGMA foreign_key_check")))
        (delete-directory root t)))))

(provide 'gnosis-test-core-integration)
;;; gnosis-test-core-integration.el ends here

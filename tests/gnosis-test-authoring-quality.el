;;; gnosis-test-authoring-quality.el --- Committed native drafts -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;;; Commentary:
;; Exercise save settlement, bounded update queries and native export fields.

;;; Code:

(require 'ert)
(require 'gnosis-test-draft-ownership)
(require 'gnosis-review)

(defun gnosis-test-authoring-quality--seed ()
  "Seed unrelated, nonempty scheduled and practice evidence."
  (gnosis-add-thema-fields "basic" "Neighbor" nil '("Answer")
                           "Explanation" '("neighbor") 0 '("source") nil 111)
  (gnosis-scheduler-accept-review (gnosis-scheduler-event-id) 111 'success
                                  1000000 (gnosis--today-int))
  (gnosis-study-accept-practice
   '(:mode practice :event-id "authoring-practice" :thema-id 111
     :session-id "authoring-session" :attempt 1 :reviewed-at-us 2000000
     :outcome success))
  (should (gnosis-select '* 'review-events))
  (should (gnosis-select '* 'practice-events)))

(defun gnosis-test-authoring-quality--evidence ()
  "Return unrelated content and study evidence, including empty void tables."
  (mapcar
   (lambda (sql) (sqlite-select gnosis-db sql))
   '("SELECT * FROM themata WHERE id = 111"
     "SELECT * FROM extras WHERE id = 111"
     "SELECT * FROM thema_tag WHERE thema_id = 111 ORDER BY tag"
     "SELECT * FROM thema_links WHERE source = 111 ORDER BY dest"
     "SELECT * FROM scheduler_baseline WHERE thema_id = 111"
     "SELECT * FROM scheduler_state WHERE thema_id = 111"
     "SELECT * FROM review_events ORDER BY event_id"
     "SELECT * FROM review_voids ORDER BY event_id"
     "SELECT * FROM practice_events ORDER BY event_id"
     "SELECT * FROM practice_voids ORDER BY event_id"
     "SELECT * FROM practice_encounters ORDER BY event_id")))

(defun gnosis-test-authoring-quality--rows ()
  "Read committed creations through a new connection."
  (let ((db (sqlite-open gnosis-test--db-file)))
    (unwind-protect
        (sqlite-select db "SELECT * FROM themata WHERE id <> 111 ORDER BY id")
      (sqlite-close db))))

(defun gnosis-test-authoring-quality--create (kind)
  "Open a native creation draft of KIND."
  (if (equal kind "cloze")
      (gnosis-add-thema kind "{{c1::Alpha}} before {{c2::Beta}}" nil nil
                        "Explanation [[id:source][Source]]" '("probe"))
    (gnosis-add-thema kind "Question [[id:source][Source]]" nil "Answer"
                      "Explanation [[id:source][Source]]" '("probe")))
  (should (eq (key-binding (kbd "C-c C-c")) #'gnosis-save))
  (current-buffer))

(defun gnosis-test-authoring-quality--key (draft key)
  "Dispatch the actual binding for KEY in DRAFT."
  (switch-to-buffer draft)
  (call-interactively (key-binding (kbd key))))

(ert-deftest gnosis-test-authoring-quality-committed-retry ()
  "A successful occurrence cannot create again after teardown veto/error/quit."
  (dolist (kind '("basic" "double" "cloze"))
    (dolist (fault '(veto error quit))
      (gnosis-test-with-db
        (gnosis-test-authoring-quality--seed)
        (let ((before (gnosis-test-authoring-quality--evidence))
              (expected (if (equal kind "basic") 1 2)))
          (gnosis-test-draft--with-editor
            (let* ((draft (gnosis-test-authoring-quality--create kind))
                   (text (buffer-string))
                   (hook (if (eq fault 'veto)
                             'kill-buffer-query-functions 'kill-buffer-hook)))
              (set (make-local-variable hook)
                   (list (if (eq fault 'veto) (lambda () nil)
                           (lambda () (signal fault '("Teardown fault"))))))
              (unwind-protect
                  (progn
                    (condition-case condition
                        (gnosis-test-authoring-quality--key draft "C-c C-c")
                      ((error quit) (should (eq (car condition) fault))))
                    (should (buffer-live-p draft))
                    (should (equal text (with-current-buffer draft (buffer-string))))
                    (let ((committed (gnosis-test-authoring-quality--rows)))
                      (should (= expected (length committed)))
                      (with-current-buffer draft (set hook nil))
                      (dotimes (_ 2)
                        (let ((err (should-error
                                    (gnosis-test-authoring-quality--key draft "C-c C-c")
                                    :type 'user-error)))
                          (should (string-match-p "already saved" (error-message-string err))))
                        (should (equal committed (gnosis-test-authoring-quality--rows)))
                        (should (equal text (with-current-buffer draft (buffer-string))))))
                    (should (equal before (gnosis-test-authoring-quality--evidence)))
                    (gnosis-test-authoring-quality--key draft "C-c C-k")
                    (should-not (buffer-live-p draft)))
                (when (buffer-live-p draft)
                  (with-current-buffer draft (set hook nil)))))))))))

(ert-deftest gnosis-test-authoring-quality-distinct-identical-creates ()
  "Separate identical creation requests remain intentional, not duplicates."
  (dolist (kind '("basic" "double" "cloze"))
    (gnosis-test-with-db
      (gnosis-test-authoring-quality--seed)
      (let ((before (gnosis-test-authoring-quality--evidence))
            (expected (if (equal kind "basic") 1 2)))
        (gnosis-test-draft--with-editor
          (dotimes (iteration 2)
            (let ((draft (gnosis-test-authoring-quality--create kind)))
              (gnosis-test-authoring-quality--key draft "C-c C-c")
              (should-not (buffer-live-p draft)))
            (should (= (* expected (1+ iteration))
                       (length (gnosis-test-authoring-quality--rows))))
            (should (equal before (gnosis-test-authoring-quality--evidence)))))))))

(ert-deftest gnosis-test-authoring-quality-precommit-failure-retry ()
  "SQL errors and quits roll back siblings without consuming the draft."
  (dolist (kind '("basic" "double" "cloze"))
    (dolist (fault '(sql quit))
      (gnosis-test-with-db
        (gnosis-test-authoring-quality--seed)
        (let ((before (gnosis-test-draft--rows))
              (expected (if (equal kind "basic") 1 2)))
          (gnosis-test-draft--with-editor
            (let* ((draft (gnosis-test-authoring-quality--create kind))
                   (text (buffer-string))
                   (insert (symbol-function 'gnosis--insert-into)))
              (if (eq fault 'sql)
                  (progn
                    (sqlite-execute
                     gnosis-db
                     (format "CREATE TEMP TRIGGER authoring_abort BEFORE INSERT ON extras WHEN NEW.id <> 111 AND (SELECT count(*) FROM themata WHERE id <> 111) = %d BEGIN SELECT RAISE(ABORT, 'authoring-native-fault'); END" expected))
                    (should-error (gnosis-test-authoring-quality--key draft "C-c C-c")
                                  :type 'user-error)
                    (sqlite-execute gnosis-db "DROP TRIGGER authoring_abort"))
                (cl-letf (((symbol-function 'gnosis--insert-into)
                           (lambda (table &rest arguments)
                             (prog1 (apply insert table arguments)
                               (when (and (eq table 'extras)
                                          (= expected (caar (sqlite-select gnosis-db "SELECT count(*) FROM themata WHERE id <> 111"))))
                                 (signal 'quit nil))))))
                  (should (eq 'quit
                              (car (condition-case condition
                                       (gnosis-test-authoring-quality--key draft "C-c C-c")
                                     (quit condition)))))))
              (should (equal before (gnosis-test-draft--rows)))
              (should-not (gnosis-test-authoring-quality--rows))
              (should (equal text (with-current-buffer draft (buffer-string))))
              (gnosis-test-authoring-quality--key draft "C-c C-c")
              (should-not (buffer-live-p draft))
              (should (= expected (length (gnosis-test-authoring-quality--rows)))))))))))

(ert-deftest gnosis-test-authoring-quality-update-indexed-existence ()
  "Single updates return a constant number of rows, without a batch cache."
  (dolist (count '(1 32 1024))
    (gnosis-test-with-db
      (gnosis-sqlite-with-transaction gnosis-db
        (dotimes (id count)
          (gnosis-test--add-basic-thema "Question" "Answer" nil nil (1+ id))))
      (let ((select (symbol-function 'sqlite-select)) (rows 0) (queries 0))
        (cl-letf (((symbol-function 'sqlite-select)
                   (lambda (&rest args)
                     (let ((result (apply select args)))
                       (cl-incf queries)
                       (cl-incf rows (length result))
                       result))))
          (gnosis-update-thema 1 "Changed" nil '("Answer") "Extra" '("tag") '("link")))
        (message "Single update collection=%d queries=%d rows=%d" count queries rows)
        (should (<= rows 4))
        (should (equal (gnosis-get 'keimenon 'themata '(= id 1)) "Changed"))))))

(ert-deftest gnosis-test-authoring-quality-export-retained-fields ()
  "Export renders aliases and rubric from its one original thema row."
  (gnosis-test-with-db
    (gnosis-add-thema-fields "basic" "Question" '("") '("Answer")
                             "" '("tag") 0 nil nil 1 '("Exact spelling"))
    (gnosis-add-thema-fields "agent-eval" "Explain" nil '("- [ ] Literal\nGreek λ")
                             "Extra" nil 0 nil nil 2 nil "* Criterion\n- Detail")
    (dolist (new '(nil t))
      (let ((expected
             (with-temp-buffer
               (gnosis-export--insert-thema (if new "NEW" "2") "agent-eval"
                                            "Explain" "- " "- [ ] Literal\nGreek λ"
                                            "Extra" nil nil nil "* Criterion\n- Detail")
               (gnosis-export--insert-thema (if new "NEW" "1") "basic"
                                            "Question" "- " "- Answer" "" '("tag")
                                            nil '("Exact spelling") nil)
               (buffer-string)))
            (select (symbol-function 'sqlite-select)) (queries 0))
        (with-temp-buffer
          (cl-letf (((symbol-function 'sqlite-select)
                     (lambda (&rest args)
                       (cl-incf queries)
                       (apply select args))))
            (gnosis-export--insert-themata '(2 (1)) new))
          (should (equal expected (buffer-string))))
        (message "Native export N=2 queries=%d" queries)
        (should (= queries 7))))))

(ert-deftest gnosis-test-authoring-quality-commit-quit-settles-receipt ()
  "Deferred quit at commit cannot leave the same occurrence authorized."
  (gnosis-test-with-db
    (gnosis-test-authoring-quality--seed)
    (gnosis-test-draft--with-editor
      (gnosis-edit-thema 111)
      (insert "Edited ")
      (let ((draft (current-buffer))
            (receipt (list nil))
            (execute (symbol-function 'sqlite-execute)))
        (setq gnosis--draft-save-receipt receipt)
        (cl-letf (((symbol-function 'sqlite-execute)
                   (lambda (db sql &rest args)
                     (prog1 (apply execute db sql args)
                       (when (equal sql "COMMIT") (setq quit-flag t))))))
          (should (eq 'quit (car (condition-case condition
                                    (gnosis-test-authoring-quality--key draft "C-c C-c")
                                  (quit condition))))))
        (should (buffer-live-p draft))
        (should (equal (car receipt)
                       (list gnosis-db 111 (seq-take (gnosis--draft-content gnosis-db 111) 2))))
        (should-error (gnosis-test-authoring-quality--key draft "C-c C-c") :type 'user-error)
        (gnosis-test-authoring-quality--key draft "C-c C-k")))))

(ert-deftest gnosis-test-authoring-quality-collisions-and-reservations ()
  "Native siblings check stored IDs; asynchronous preallocation reserves IDs."
  (gnosis-test-with-db
    (gnosis-test-draft--with-editor
      (gnosis-test-authoring-quality--create "double")
      (let ((random-values '(0 0 1))
            (select (symbol-function 'gnosis-select)))
        (cl-letf (((symbol-function 'random) (lambda (&rest _) (pop random-values)))
                  ((symbol-function 'gnosis-select)
                   (lambda (value table &optional where flatten)
                     (when (and (eq value 'id) (eq table 'themata)) (should where))
                     (funcall select value table where flatten))))
          (gnosis-save))
        (should-not random-values))
      (should (= 2 (length (gnosis-select 'id 'themata)))))
    (let ((gnosis--id-cache (make-hash-table :test 'equal))
          (random-values '(0 0 1)))
      (cl-letf (((symbol-function 'random) (lambda (&rest _) (pop random-values))))
        (should (equal '(10 11) (gnosis-generate-ids 2 2))))
      (should (gethash 10 gnosis--id-cache))
      (should (gethash 11 gnosis--id-cache))
      ;; A reservation is not a stored row; update still creates missing IDs.
      (let (warnings)
        (cl-letf (((symbol-function 'display-warning)
                   (lambda (&rest args) (push args warnings))))
          (gnosis-update-thema 10 "Missing" nil '("Answer") "" nil nil "basic"))
        (should warnings))
      (should (equal "Missing" (gnosis-get 'keimenon 'themata '(= id 10)))))))

(provide 'gnosis-test-authoring-quality)
;;; gnosis-test-authoring-quality.el ends here

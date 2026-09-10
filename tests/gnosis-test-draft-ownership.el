;;; gnosis-test-draft-ownership.el --- Native draft safety -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;;; Commentary:
;; Exercise native save/cancel bindings against disposable databases.

;;; Code:

(require 'ert)
(require 'gnosis-test-helpers)
(require 'gnosis-export-import)

(defmacro gnosis-test-draft--with-editor (&rest body)
  "Run BODY and clean up only its native draft buffers."
  (declare (indent 0) (debug t))
  `(let ((gnosis-save-hook nil)
         (gnosis-review-editing-p nil))
     (save-window-excursion
       (unwind-protect (progn ,@body)
         (dolist (name '("*Gnosis Edit*" "*Gnosis NEW*"))
           (when (get-buffer name) (kill-buffer name)))))))

(defun gnosis-test-draft--rows ()
  "Return all persistent rows, ordered independently of query plans."
  (mapcar (lambda (table)
            (cons table (sort (sqlite-select gnosis-db
                                            (format "SELECT * FROM %s" table))
                              (lambda (a b)
                                (string< (prin1-to-string a)
                                         (prin1-to-string b))))))
          (mapcar #'car (sqlite-select gnosis-db
                                      "SELECT name FROM sqlite_master WHERE type = 'table' ORDER BY name"))))

(defun gnosis-test-draft--refuse ()
  "Require a save refusal with intact editable text and persistent rows."
  (let ((buffer (current-buffer))
        (text (buffer-string))
        (rows (gnosis-test-draft--rows)))
    (let ((err (should-error
                (call-interactively (key-binding (kbd "C-c C-c")))
                :type 'user-error)))
      (should (string-match-p "[Cc]opy\\|[Rr]eopen\\|reconcile"
                              (error-message-string err))))
    (should (buffer-live-p buffer))
    (should (eq buffer (current-buffer)))
    (should (equal text (buffer-string)))
    (should-not buffer-read-only)
    (should (equal rows (gnosis-test-draft--rows)))))

(ert-deftest gnosis-test-draft-foreign-edit-owner ()
  (gnosis-test-with-db
    (gnosis-test--add-basic-thema "Owner A" "A" nil nil 111)
    (gnosis-test-draft--with-editor
      (gnosis-edit-thema 111)
      (let ((original (gnosis-test-draft--rows)))
        (gnosis-test-with-db
          (gnosis-test--add-basic-thema "Owner B" "B" nil nil 111)
          (with-current-buffer "*Gnosis Edit*"
            (gnosis-test-draft--refuse)))
        (should (equal original (gnosis-test-draft--rows)))))))

(ert-deftest gnosis-test-draft-foreign-create-owner ()
  (gnosis-test-with-db
    (gnosis-test-draft--with-editor
      (gnosis-add-thema "basic" "New question" nil "New answer")
      (let ((original (gnosis-test-draft--rows)))
        (gnosis-test-with-db
          (with-current-buffer "*Gnosis NEW*"
            (gnosis-test-draft--refuse)))
        (should (equal original (gnosis-test-draft--rows)))))))

(ert-deftest gnosis-test-draft-reopened-owner ()
  (gnosis-test-with-db
    (gnosis-test--add-basic-thema "Original" "A" nil nil 111)
    (gnosis-test-draft--with-editor
      (gnosis-edit-thema 111)
      (gnosis-sqlite-close gnosis-db)
      (setq gnosis-db (gnosis-sqlite-open gnosis-test--db-file))
      (gnosis-test-draft--refuse))))

(ert-deftest gnosis-test-draft-content-drift ()
  (dolist (sql '("UPDATE themata SET keimenon = 'changed' WHERE id = 111"
                 "UPDATE themata SET answer = 'changed' WHERE id = 111"
                 "UPDATE themata SET hypothesis = 'changed' WHERE id = 111"
                 "UPDATE themata SET type = 'external' WHERE id = 111"
                 "UPDATE themata SET accepted_aliases = 'changed' WHERE id = 111"
                 "UPDATE extras SET parathema = 'changed' WHERE id = 111"
                 "UPDATE extras SET review_image = 'changed' WHERE id = 111"
                 "UPDATE thema_tag SET tag = 'changed' WHERE thema_id = 111"
                 "INSERT INTO thema_links VALUES (111, 'changed')"))
    (gnosis-test-with-db
      (gnosis-test--add-basic-thema "Original" "A" nil nil 111)
      (gnosis-test-draft--with-editor
        (gnosis-edit-thema 111)
        (sqlite-execute gnosis-db sql)
        (gnosis-test-draft--refuse)))))

(ert-deftest gnosis-test-draft-deleted-content ()
  (gnosis-test-with-db
    (gnosis-test--add-basic-thema "Original" "A" nil nil 111)
    (gnosis-test-draft--with-editor
      (gnosis-edit-thema 111)
      (gnosis-delete-themata '(111))
      (gnosis-test-draft--refuse))))

(ert-deftest gnosis-test-draft-save-and-cancel ()
  (gnosis-test-with-db
    (gnosis-test--add-basic-thema "Neighbor" "N" nil nil 222)
    (gnosis-test-draft--with-editor
      (gnosis-add-thema "basic" "Question [[id:source][Source]]" nil "Answer")
      (call-interactively (key-binding (kbd "C-c C-c")))
      (should-not (get-buffer "*Gnosis NEW*"))
      (let* ((id (car (gnosis-select 'id 'themata '(not (= id 222)) t)))
             (before (gnosis-test-draft--rows)))
        (gnosis-edit-thema id)
        (insert "Cancelled ")
        (call-interactively (key-binding (kbd "C-c C-k")))
        (should-not (get-buffer "*Gnosis Edit*"))
        (should (equal before (gnosis-test-draft--rows)))
        (gnosis-edit-thema id)
        (insert "Edited ")
        (call-interactively (key-binding (kbd "C-c C-c")))
        (should (equal (gnosis-get 'keimenon 'themata `(= id ,id))
                       "Edited Question [[id:source][Source]]"))
        (should (equal (gnosis-get 'keimenon 'themata '(= id 222)) "Neighbor"))
        (should (equal (gnosis-select 'dest 'thema-links `(= source ,id) t)
                       '("source")))))))

(ert-deftest gnosis-test-draft-closed-owner-preserves-text ()
  (gnosis-test-with-db
    (gnosis-test-draft--with-editor
      (gnosis-add-thema "basic" "Question" nil "Answer")
      (let ((text (buffer-string))
            (before (gnosis-test-draft--rows)))
        (gnosis-sqlite-close gnosis-db)
        (unwind-protect
            (progn
              (should-error (call-interactively (key-binding (kbd "C-c C-c")))
                            :type 'user-error)
              (should (equal text (buffer-string)))
              (should-not buffer-read-only))
          (setq gnosis-db (gnosis-sqlite-open gnosis-test--db-file)))
        (should (equal before (gnosis-test-draft--rows)))))))

(ert-deftest gnosis-test-draft-unowned-refuses-without-opening ()
  (let ((gnosis-db nil))
    (with-temp-buffer
      (gnosis-edit-mode)
      (gnosis-export--insert-thema "NEW" "basic" "Question" nil "Answer")
      (should-error (call-interactively (key-binding (kbd "C-c C-c")))
                    :type 'user-error)
      (should-not gnosis-db))))

(ert-deftest gnosis-test-draft-retarget-refuses ()
  (gnosis-test-with-db
    (gnosis-test--add-basic-thema "Original" "A" nil nil 111)
    (gnosis-test--add-basic-thema "Neighbor" "B" nil nil 222)
    (gnosis-test-draft--with-editor
      (gnosis-edit-thema 111)
      (goto-char (point-max))
      (gnosis-export--insert-thema "222" "basic" "Replacement" nil "Wrong")
      (gnosis-test-draft--refuse))))

(ert-deftest gnosis-test-draft-drift-at-transaction-start ()
  "Recheck after acquiring the write transaction and before saving siblings."
  (gnosis-test-with-db
    (gnosis-test--add-basic-thema "Original" "A" nil nil 111)
    (gnosis-test-draft--with-editor
      (gnosis-edit-thema 111)
      (goto-char (point-min))
      (gnosis-export--insert-thema "NEW" "basic" "Sibling" nil "Answer")
      (let ((execute (symbol-function 'sqlite-execute)))
        (cl-letf (((symbol-function 'sqlite-execute)
                   (lambda (db sql &rest args)
                     (prog1 (apply execute db sql args)
                       (when (equal sql "BEGIN IMMEDIATE")
                         (funcall execute db
                                  "UPDATE themata SET keimenon = 'changed' WHERE id = 111"))))))
          (gnosis-test-draft--refuse))))))

(ert-deftest gnosis-test-draft-study-evidence-is-not-content-drift ()
  "Saving content neither invalidates nor replaces unrelated study evidence."
  (gnosis-test-with-db
    (gnosis-test--add-basic-thema "Original" "A" nil nil 111)
    (gnosis-test-draft--with-editor
      (gnosis-edit-thema 111)
      (gnosis-scheduler-accept-review
       (gnosis-scheduler-event-id) 111 'success 1000000 (gnosis--today-int))
      (gnosis--insert-into 'practice-events '(["draft-practice" 111 "session" 1 1 3]))
      (let ((before (gnosis-test-draft--rows)))
        (call-interactively (key-binding (kbd "C-c C-c")))
        (should (equal before (gnosis-test-draft--rows)))))))

(provide 'gnosis-test-draft-ownership)
;;; gnosis-test-draft-ownership.el ends here

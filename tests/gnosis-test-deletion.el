;;; gnosis-test-deletion.el --- Physical deletion and cancellation -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;;; Commentary:

;; Exercise real deletion writers and dispatch with synthetic, disposable data.

;;; Code:

(require 'ert)
(require 'gnosis-dashboard)
(require 'gnosis-review)
(require 'gnosis-test-helpers)

(defun gnosis-test-deletion--snapshot ()
  "Return all application rows in the disposable database."
  (mapcar (lambda (row)
            (cons (car row)
                  (sqlite-select gnosis-db
                                 (format "SELECT * FROM %s ORDER BY 1"
                                         (car row)))))
          (sqlite-select gnosis-db
                         "SELECT name FROM sqlite_master
                          WHERE type = 'table' ORDER BY name")))

(ert-deftest gnosis-deletion-single-return-and-cancel ()
  "Report completion only after deletion; declining and quitting preserve rows."
  (gnosis-test-with-db
    (let* ((id (gnosis-test--add-basic-thema "Question" "Answer"))
           (before (gnosis-test-deletion--snapshot)))
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) nil)))
        (should-not (gnosis-delete-thema id)))
      (should (equal before (gnosis-test-deletion--snapshot)))
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) (signal 'quit nil))))
        (should (eq 'cancelled (condition-case nil (gnosis-delete-thema id)
                                (quit 'cancelled)))))
      (should (equal before (gnosis-test-deletion--snapshot)))
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
        (should (eq t (gnosis-delete-thema id))))
      (should-not (gnosis-get 'id 'themata `(= id ,id))))))

(ert-deftest gnosis-deletion-bulk-cascades-and-preserves-unrelated ()
  "Delete selected content and evidence, preserving unrelated rows and summaries."
  (gnosis-test-with-db
    (let* ((ids (mapcar (lambda (n) (gnosis-test--add-basic-thema n "Answer"))
                        '("One" "Two" "Keep")))
           (keep (car (last ids)))
           (gnosis-sqlite--max-vars 1))
      (dolist (id ids)
        (let ((event (gnosis-scheduler-accept-review
                      (gnosis-scheduler-event-id) id 'success
                      1000000 (gnosis--today-int))))
          (gnosis--insert-into 'review-voids
                               `([,(format "void-%s" id) ,(plist-get event :event-id)])))
        (gnosis--insert-into 'practice-events
                             `([,(format "practice-%s" id) ,id "session" ,id 1 3]))
        (gnosis--insert-into 'practice-voids
                             `([,(format "void-%s" id) ,(format "practice-%s" id)]))
        (gnosis--insert-into 'thema-links `([,id "source-node"])))
      (gnosis--insert-into 'study-history '(["retained" (:summary "Keep history")]))
      (let ((before (gnosis-test-deletion--snapshot)))
        (should (eq t (gnosis-delete-themata (butlast ids))))
        (dolist (table '("themata" "extras" "thema_tag" "thema_links"
                         "scheduler_baseline" "scheduler_state"
                         "review_events" "practice_events"))
          (let* ((column (cond ((member table '("themata" "extras")) "id")
                               ((equal table "thema_links") "source")
                               (t "thema_id")))
                 (rows (sqlite-select gnosis-db (format "SELECT %s FROM %s" column table))))
            (should (equal (list (list keep)) rows))
            (let ((column-index
                   (cl-position column
                                (mapcar #'cadr
                                        (sqlite-select gnosis-db
                                                       (format "PRAGMA table_info(%s)" table)))
                                :test #'equal)))
              (should (equal (seq-filter (lambda (row) (= keep (nth column-index row)))
                                         (cdr (assoc table before)))
                             (sqlite-select gnosis-db (format "SELECT * FROM %s" table)))))))
        (dolist (table '("review_voids" "practice_voids"))
          (should (equal (seq-filter (lambda (row)
                                      (equal (car row) (format "\"void-%s\"" keep)))
                                    (cdr (assoc table before)))
                         (sqlite-select gnosis-db (format "SELECT * FROM %s" table)))))
        (dolist (table '("study_history" "scheduler_config" "scheduler_active"))
          (should (equal (cdr (assoc table before))
                         (cdr (assoc table (gnosis-test-deletion--snapshot)))))))
      (should-not (sqlite-select gnosis-db "PRAGMA foreign_key_check")))))

(ert-deftest gnosis-deletion-transaction-failure-and-quit ()
  "Roll back earlier child deletes on SQL failure or quit before commit."
  (dolist (fault '(error quit))
    (gnosis-test-with-db
      (let* ((id (gnosis-test--add-basic-thema "Question" "Answer"))
             (before (gnosis-test-deletion--snapshot))
             (execute (symbol-function 'gnosis-sqlite-execute-batch)))
        (cl-letf (((symbol-function 'gnosis-sqlite-execute-batch)
                   (lambda (db sql ids &rest args)
                     (if (string-prefix-p "DELETE FROM themata " sql)
                         (signal fault '("Injected deletion failure"))
                       (apply execute db sql ids args)))))
          (should (eq fault
                      (condition-case nil (gnosis-delete-thema id t)
                        (error 'error) (quit 'quit)))))
        (should (equal before (gnosis-test-deletion--snapshot)))
        (should (gnosis-delete-thema id t))
        (should-not (gnosis-get 'id 'themata `(= id ,id)))))))

(ert-deftest gnosis-deletion-dashboard-binding ()
  "Delete point or marked rows via the dashboard binding, not on cancel."
  (dolist (marked '(nil t))
    (gnosis-test-with-db
      (let* ((ids (list (gnosis-test--add-basic-thema "One" "A")
                        (gnosis-test--add-basic-thema "Two" "B")))
             (buffer (generate-new-buffer " *gnosis-delete-dashboard*"))
             (gnosis-dashboard-buffer-name (buffer-name buffer)))
        (unwind-protect
            (save-window-excursion
              (with-current-buffer buffer (gnosis-dashboard-mode))
              (gnosis-dashboard-output-themata ids)
              (goto-char (point-min))
              (setq gnosis-dashboard--selected-ids (and marked (copy-sequence ids)))
              (let ((before (gnosis-test-deletion--snapshot))
                    (text (buffer-string))
                    (selected (copy-sequence gnosis-dashboard--selected-ids)))
                (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) nil)))
                  (call-interactively (key-binding (kbd "d"))))
                (should (equal before (gnosis-test-deletion--snapshot)))
                (should (equal text (buffer-string)))
                (should (equal selected gnosis-dashboard--selected-ids)))
              (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
                (call-interactively (key-binding (kbd "d"))))
              (should-not gnosis-dashboard--selected-ids)
              (should (= (if marked 0 1) (length tabulated-list-entries)))
              (should (= (if marked 0 1) (length (gnosis-select 'id 'themata)))))
          (when (buffer-live-p buffer) (kill-buffer buffer)))))))

(ert-deftest gnosis-deletion-review-decline-keeps-pending-queue ()
  "Declining deletion returns to actions with the same pending result and queue."
  (gnosis-test-with-db
    (let* ((id (gnosis-test--add-basic-thema "Question" "Answer"))
           (state (gnosis-review-state-create :remaining (list id) :total 1))
           (pending '(:pending unchanged))
           (before (gnosis-test-deletion--snapshot))
           (choices '(?d ?n)))
      (cl-letf (((symbol-function 'gnosis-review--display-thema)
                 (lambda (_) (list "basic" (cons nil pending))))
                ((symbol-function 'read-char-choice)
                 (lambda (&rest _) (or (pop choices) (error "Unexpected prompt"))))
                ((symbol-function 'y-or-n-p) (lambda (&rest _) nil))
                ((symbol-function 'gnosis-review--accept)
                 (lambda (thema success result)
                   (should (= id thema))
                   (should-not success)
                   (should (eq pending result))
                   (should (equal (list id) (gnosis-review-state-remaining state)))
                   (should (equal before (gnosis-test-deletion--snapshot)))
                   (signal 'quit nil))))
        (should (eq 'cancelled
                    (condition-case nil (gnosis-review-process-thema id state)
                      (quit 'cancelled)))))
      (should-not choices)
      (should (equal before (gnosis-test-deletion--snapshot)))
      (should (equal (list id) (gnosis-review-state-remaining state)))
      (should-not (gnosis-review-state-skipped state))
      (should-not (gnosis-review-state-outcomes state)))))

(ert-deftest gnosis-deletion-review-confirmed-skips-without-grade ()
  "Confirmed deletion skips the pending card without a grade or retry."
  (gnosis-test-with-db
    (let* ((id (gnosis-test--add-basic-thema "Question" "Answer"))
           (state (gnosis-review-state-create :remaining (list id) :total 1))
           (register-alist nil))
      (cl-letf (((symbol-function 'gnosis-review--display-thema)
                 (lambda (_) (list "basic" (cons nil nil))))
                ((symbol-function 'read-char-choice) (lambda (&rest _) ?d))
                ((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
        (gnosis-review-process-thema id state))
      (should-not (gnosis-get 'id 'themata `(= id ,id)))
      (should-not (gnosis-review-state-remaining state))
      (should (equal (list id) (gnosis-review-state-skipped state)))
      (should-not (gnosis-review-state-requeued state))
      (should-not (gnosis-review-state-outcomes state))
      (should (= 0 (gnosis-review-state-reviewed state)))
      (should-not (gnosis-select '* 'review-events))
      (should-not (gnosis-select '* 'practice-events)))))

(ert-deftest gnosis-deletion-persistent-review-dispatch ()
  "Decline then accept or delete through real scheduled and practice sessions."
  (dolist (mode '(due practice))
    (dolist (delete '(nil t))
      (gnosis-test-with-db
        (let* ((id (gnosis-test--add-basic-thema "Question" "Answer"))
               (buffer (generate-new-buffer " *gnosis-delete-review*"))
               (gnosis-review-buffer-name (buffer-name buffer))
               (gnosis-monkeytype-enable nil)
               (gnosis-center-content nil)
               (register-alist nil)
               (choices (if delete '(?d ?d) '(?d ?n)))
               (confirmations '(nil t)))
          (unwind-protect
              (save-window-excursion
                (cl-letf (((symbol-function 'gnosis--read-string-with-input-method)
                           (lambda (&rest _) "Answer"))
                          ((symbol-function 'read-char-choice)
                           (lambda (&rest _)
                             (or (pop choices) (error "Unexpected action prompt"))))
                          ((symbol-function 'y-or-n-p)
                           (lambda (&rest _) (pop confirmations))))
                  (let ((state (gnosis-review-loop (list id) mode)))
                    (should-not choices)
                    (should-not (gnosis-review-state-remaining state))
                    (should (= (if delete 0 1) (gnosis-review-state-reviewed state)))
                    (should (equal (and delete (list id))
                                   (gnosis-review-state-skipped state)))
                    (should-not (gnosis-review-state-requeued state))))
                (should (eq (not delete) (not (null (gnosis-get 'id 'themata `(= id ,id))))))
                (should (= (if (and (not delete) (eq mode 'due)) 1 0)
                           (length (gnosis-select '* 'review-events))))
                (should (= (if (and (not delete) (eq mode 'practice)) 1 0)
                           (length (gnosis-select '* 'practice-events))))
                (let ((before (gnosis-test-deletion--snapshot)))
                  (gnosis-sqlite-close gnosis-db)
                  (setq gnosis-db (gnosis-sqlite-open gnosis-test--db-file))
                  (gnosis-db-init)
                  (should (equal before (gnosis-test-deletion--snapshot)))))
            (when (buffer-live-p buffer) (kill-buffer buffer))))))))

(ert-deftest gnosis-deletion-dispatch-failure-preserves-state ()
  "Review and dashboard propagate failed deletion without advancing their views."
  (dolist (caller '(review dashboard))
    (dolist (fault '(error quit))
      (gnosis-test-with-db
        (let* ((id (gnosis-test--add-basic-thema "Question" "Answer"))
               (buffer (generate-new-buffer " *gnosis-delete-fault*"))
               (gnosis-dashboard-buffer-name (buffer-name buffer))
               (state (gnosis-review-state-create :remaining (list id) :total 1))
               (before (gnosis-test-deletion--snapshot))
               (execute (symbol-function 'gnosis-sqlite-execute-batch)))
          (unwind-protect
              (save-window-excursion
                (with-current-buffer buffer (gnosis-dashboard-mode))
                (gnosis-dashboard-output-themata (list id))
                (setq gnosis-dashboard--selected-ids (list id))
                (let ((text (buffer-string)))
                  (cl-letf (((symbol-function 'read-char-choice) (lambda (&rest _) ?d))
                            ((symbol-function 'y-or-n-p) (lambda (&rest _) t))
                            ((symbol-function 'gnosis-review--display-thema)
                             (lambda (_) (list "basic" (cons nil nil))))
                            ((symbol-function 'gnosis-sqlite-execute-batch)
                             (lambda (db sql ids &rest args)
                               (if (string-prefix-p "DELETE FROM themata " sql)
                                   (signal fault '("Injected deletion failure"))
                                 (apply execute db sql ids args)))))
                    (should (eq fault
                                (condition-case nil
                                    (if (eq caller 'review)
                                        (gnosis-review-process-thema id state)
                                      (call-interactively (key-binding (kbd "d"))))
                                  (error 'error) (quit 'quit)))))
                  (should (equal text (buffer-string)))
                  (should (equal (list id) gnosis-dashboard--selected-ids)))
                (should (equal before (gnosis-test-deletion--snapshot)))
                (should (equal (list id) (gnosis-review-state-remaining state)))
                (should-not (gnosis-review-state-skipped state))
                (should-not (gnosis-review-state-outcomes state)))
            (when (buffer-live-p buffer) (kill-buffer buffer))))))))

(provide 'gnosis-test-deletion)
;;; gnosis-test-deletion.el ends here

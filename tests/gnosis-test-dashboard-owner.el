;;; gnosis-test-dashboard-owner.el --- Dashboard command owners -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.
;; Author: Thanos Apollo <public@thanosapollo.org>

;;; Commentary:
;; Refuse stale row commands before input and after recursive input returns.

;;; Code:

(require 'ert)
(require 'gnosis-dashboard)
(require 'gnosis-test-helpers)

(defmacro gnosis-test-dashboard-owner--with-databases (&rest body)
  "Run BODY with colliding rows in two disposable databases and a view."
  (declare (indent 0) (debug t))
  `(gnosis-test-with-db
     (gnosis-test--add-basic-thema "Original" "Answer" '("tag" "TAG") nil 42)
     (gnosis-test--add-basic-thema "Unrelated" "Answer" nil nil 43)
     (let ((original-db gnosis-db)
           (original-dir gnosis-dir))
       (gnosis-test-with-db
         (gnosis-test--add-basic-thema "Successor" "Other" '("tag" "TAG") nil 42)
         (let* ((successor-db gnosis-db)
                (successor-dir gnosis-dir)
                (gnosis-db original-db)
                (gnosis-dir original-dir)
                (buffer (generate-new-buffer " *dashboard-owner*"))
                (gnosis-dashboard-buffer-name (buffer-name buffer))
                (gnosis-dashboard-timer-delay 60))
           (unwind-protect
               (save-window-excursion
                 (with-current-buffer buffer (gnosis-dashboard-mode))
                 ,@body)
             (when (buffer-live-p buffer) (kill-buffer buffer))))))))

(defun gnosis-test-dashboard-owner--snapshot (db)
  "Return content and scheduler rows in DB for preservation assertions."
  (mapcar (lambda (table)
            (gnosis-sqlite-select db (format "SELECT * FROM %s ORDER BY 1, 2" table)))
          '(themata extras thema_tag scheduler_state scheduler_baseline)))

(ert-deftest gnosis-dashboard-owner-stale-commands-refuse-before-input ()
  "Every thema/tag mutation rejects a foreign connection before prompting."
  (gnosis-test-dashboard-owner--with-databases
    (dolist (view '(themata tags))
      (if (eq view 'themata)
          (gnosis-dashboard-output-themata '(42))
        (gnosis-dashboard-output-tags))
      (let ((before (buffer-string))
            (original (gnosis-test-dashboard-owner--snapshot original-db))
            (successor (gnosis-test-dashboard-owner--snapshot successor-db))
            (gnosis-db successor-db)
            (gnosis-dir successor-dir)
            prompted)
        (cl-letf (((symbol-function 'y-or-n-p)
                   (lambda (&rest _) (setq prompted t) t))
                  ((symbol-function 'read-string)
                   (lambda (&rest _) (setq prompted t) "renamed"))
                  ((symbol-function 'completing-read-multiple)
                   (lambda (&rest _) (setq prompted t) '("+new"))))
          (dolist (key (if (eq view 'themata) '("d" "s" "RET" "b" "t")
                        '("d" "s" "r" "R" "C")))
            (ert-info ((format "Stale %s command %s" view key))
              (should-error (call-interactively (local-key-binding (kbd key)))
                            :type 'user-error))))
        (should-not prompted)
        (should (equal before (buffer-string)))
        (should (equal original (gnosis-test-dashboard-owner--snapshot original-db)))
        (should (equal successor (gnosis-test-dashboard-owner--snapshot successor-db)))))))

(ert-deftest gnosis-dashboard-owner-replacement-during-input ()
  "All prompted mutations recheck the view and connection before writing."
  (gnosis-test-dashboard-owner--with-databases
    (dolist (case '((themata "d" confirm) (themata "s" confirm)
                    (themata "t" tags) (themata "b" string)
                    (tags "d" confirm) (tags "s" confirm)
                    (tags "r" string) (tags "R" confirm) (tags "C" confirm)))
      (setq gnosis-db original-db gnosis-dir original-dir)
      (if (eq (car case) 'themata)
          (gnosis-dashboard-output-themata '(42))
        (gnosis-dashboard-output-tags))
      (let ((before (buffer-string))
            (original (gnosis-test-dashboard-owner--snapshot original-db))
            (successor (gnosis-test-dashboard-owner--snapshot successor-db))
            (reads 0)
            replaced)
        (cl-labels ((replace-db ()
                      (setq gnosis-db successor-db gnosis-dir successor-dir
                            replaced t)))
          (cl-letf (((symbol-function 'y-or-n-p)
                     (lambda (&rest _)
                       (when (eq (nth 2 case) 'confirm) (replace-db)) t))
                    ((symbol-function 'read-string)
                     (lambda (&rest _)
                       (when (eq (nth 2 case) 'string) (replace-db))
                       (cl-incf reads)
                       (if (= reads 1) "tag" "renamed")))
                    ((symbol-function 'completing-read-multiple)
                     (lambda (&rest _) (replace-db) '("+new"))))
            (ert-info ((format "Replacement in %S" case))
              (should-error (call-interactively (local-key-binding (kbd (cadr case))))
                            :type 'user-error))))
        (should replaced)
        (should (equal before (buffer-string)))
        (should (equal original (gnosis-test-dashboard-owner--snapshot original-db)))
        (should (equal successor (gnosis-test-dashboard-owner--snapshot successor-db)))))))

(ert-deftest gnosis-dashboard-owner-delete-repurposed-view ()
  "A confirmation cannot authorize a refreshed, repurposed or killed view."
  (dolist (replacement '(refresh mode kill))
    (gnosis-test-dashboard-owner--with-databases
      (gnosis-dashboard-output-themata '(42))
      (let ((original (gnosis-test-dashboard-owner--snapshot original-db))
            successor-text)
        (cl-letf (((symbol-function 'y-or-n-p)
                   (lambda (&rest _)
                     (pcase replacement
                       ('refresh (gnosis-dashboard-output-themata '(43)))
                       ('mode (fundamental-mode)
                              (let ((inhibit-read-only t))
                                (erase-buffer) (insert "Unrelated draft")))
                       ('kill (kill-buffer buffer)))
                     (setq successor-text (buffer-string))
                     t)))
          (should-error (call-interactively (local-key-binding (kbd "d")))
                        :type 'user-error))
        (should (equal successor-text (buffer-string)))
        (should (equal original (gnosis-test-dashboard-owner--snapshot original-db)))))))

(ert-deftest gnosis-dashboard-owner-delete-cancel-and-success ()
  "Cancel preserves rows and selection; confirmed deletion remains physical."
  (gnosis-test-dashboard-owner--with-databases
    (gnosis-dashboard-output-themata '(42 43))
    (call-interactively (local-key-binding (kbd "m")))
    (let ((before (buffer-string))
          (original (gnosis-test-dashboard-owner--snapshot original-db))
          (successor (gnosis-test-dashboard-owner--snapshot successor-db)))
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) nil)))
        (call-interactively (local-key-binding (kbd "d"))))
      (should (equal '(42) gnosis-dashboard--selected-ids))
      (should (equal before (buffer-string)))
      (should (equal original (gnosis-test-dashboard-owner--snapshot original-db)))
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
        (call-interactively (local-key-binding (kbd "d"))))
      (should-not (gnosis-get 'keimenon 'themata '(= id 42)))
      (should (equal "Unrelated" (gnosis-get 'keimenon 'themata '(= id 43))))
      (should (equal '(43) gnosis-dashboard-themata-current-ids))
      (should-not gnosis-dashboard--selected-ids)
      (should (equal successor (gnosis-test-dashboard-owner--snapshot successor-db))))))

(ert-deftest gnosis-dashboard-owner-projection-keeps-producing-database ()
  "Mode hooks cannot relabel original rows as belonging to a new database."
  (gnosis-test-dashboard-owner--with-databases
    (dolist (view '(themata tags))
      (setq gnosis-db original-db gnosis-dir original-dir)
      (let* ((hook (if (eq view 'themata)
                       'gnosis-dashboard-themata-mode-hook
                     'gnosis-dashboard-tags-mode-hook))
             (replace-db (lambda ()
                           (setq gnosis-db successor-db
                                 gnosis-dir successor-dir))))
        (add-hook hook replace-db)
        (unwind-protect
            (if (eq view 'themata)
                (gnosis-dashboard-output-themata '(42))
              (gnosis-dashboard-output-tags))
          (remove-hook hook replace-db)))
      (should (eq gnosis-dashboard--database original-db))
      (should-error (call-interactively (local-key-binding (kbd "d")))
                    :type 'user-error))))

(defun gnosis-test-dashboard-owner--view ()
  "Return the current display and selection for preservation assertions."
  (list (current-buffer) major-mode gnosis-dashboard--load-generation
        (copy-sequence gnosis-dashboard--selected-ids) (buffer-string)))

(defun gnosis-test-dashboard-owner--bulk-link (change &optional at-node)
  "Exercise the real bulk writer with CHANGE during final confirmation.
When AT-NODE is non-nil, replace ownership during node selection instead."
  (gnosis-test-dashboard-owner--with-databases
    (gnosis--insert-into
     'nodes '(["node-1" "disposable.org" "Target" "0" nil nil nil]))
    (gnosis-update 'themata '(= keimenon "Original unselected") '(= id 43))
    (gnosis-dashboard-output-themata '(42 43))
    (call-interactively (local-key-binding (kbd "m")))
    (let ((original (gnosis-test-dashboard-owner--snapshot original-db))
          (successor (gnosis-test-dashboard-owner--snapshot successor-db))
          (links (gnosis-sqlite-select original-db "SELECT * FROM thema_links"))
          (other-links (gnosis-sqlite-select successor-db "SELECT * FROM thema_links"))
          (selected gnosis-dashboard--selected-ids)
          (view (gnosis-test-dashboard-owner--view))
          (confirmations 0)
          result)
      (cl-labels
          ((replace-owner ()
             (pcase change
               ('database (setq gnosis-db successor-db gnosis-dir successor-dir))
               ('refresh (gnosis-dashboard-output-themata '(43)))
               ('mode (fundamental-mode)
                      (let ((inhibit-read-only t))
                        (erase-buffer) (insert "Successor draft")))
               ((or 'kill 'replace)
                (kill-buffer buffer)
                (setq buffer (get-buffer-create gnosis-dashboard-buffer-name))
                (switch-to-buffer buffer)
                (if (eq change 'replace)
                    (progn
                      (gnosis-dashboard-mode)
                      (gnosis-dashboard-output-themata '(43)))
                  (insert "Replacement buffer"))))
             (setq view (gnosis-test-dashboard-owner--view))))
        (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "Original"))
                  ((symbol-function 'gnosis-completing-read)
                   (lambda (&rest _)
                     (when at-node (replace-owner))
                     "Target"))
                  ((symbol-function 'y-or-n-p)
                   (lambda (&rest _)
                     (cl-incf confirmations)
                     (replace-owner)
                     (pcase change
                       ('no nil)
                       ('quit (signal 'quit nil))
                       (_ t)))))
          (setq result
                (condition-case err
                    (progn (call-interactively (local-key-binding (kbd "b"))) 'ok)
                  ((error quit) (car err))))))
      (should (= confirmations (if at-node 0 1)))
      (should (equal selected '(42)))
      (should (equal successor (gnosis-test-dashboard-owner--snapshot successor-db)))
      (should (equal other-links
                     (gnosis-sqlite-select successor-db "SELECT * FROM thema_links")))
      (if (eq change 'success)
          (progn
            (should (eq result 'ok))
            (should (equal "[[id:node-1][Original]]"
                           (gnosis-get 'keimenon 'themata '(= id 42))))
            (should (equal '((42 "node-1"))
                           (gnosis-select '[source dest] 'thema-links)))
            ;; Only the selected keimenon and its link index may change.
            (let ((after (gnosis-test-dashboard-owner--snapshot original-db)))
              (should (equal (cdr original) (cdr after)))
              (should (equal (cdr (car original)) (cdr (car after))))))
        (should (equal original (gnosis-test-dashboard-owner--snapshot original-db)))
        (should (equal links
                       (gnosis-sqlite-select original-db "SELECT * FROM thema_links")))
        (should (equal view (gnosis-test-dashboard-owner--view)))
        (should (eq result (pcase change ('no 'ok) ('quit 'quit) (_ 'user-error))))))))

(ert-deftest gnosis-dashboard-owner-bulk-final-database ()
  "Final confirmation must not overwrite a successor's colliding thema."
  (gnosis-test-dashboard-owner--bulk-link 'database))

(ert-deftest gnosis-dashboard-owner-bulk-final-view ()
  "Final confirmation cannot outlive the initiating view or buffer object."
  (dolist (change '(refresh mode kill replace))
    (ert-info ((format "Final confirmation replacement: %s" change))
      (gnosis-test-dashboard-owner--bulk-link change))))

(ert-deftest gnosis-dashboard-owner-bulk-node-selection ()
  "Node selection must not authorize a stale view or connection."
  (dolist (change '(database refresh mode kill replace))
    (ert-info ((format "Node selection replacement: %s" change))
      (gnosis-test-dashboard-owner--bulk-link change t))))

(ert-deftest gnosis-dashboard-owner-bulk-cancel-and-success ()
  "Final no and quit preserve selection; yes links only selected themata."
  (dolist (change '(no quit success))
    (gnosis-test-dashboard-owner--bulk-link change)))

(provide 'gnosis-test-dashboard-owner)
;;; gnosis-test-dashboard-owner.el ends here

;;; gnosis-test-dashboard-marks.el --- Dashboard mark projection tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Thanos Apollo <public@thanosapollo.org>

;;; Commentary:

;; Exercise native sorting and selected actions with disposable SQLite data.

;;; Code:

(require 'gnosis-test-dashboard-hardening)

(defun gnosis-test-dashboard--marked-ids ()
  "Return visibly highlighted row IDs in display order."
  (save-excursion
    (goto-char (point-min))
    (cl-loop until (eobp)
             when (seq-some (lambda (overlay)
                              (and (overlay-get overlay 'gnosis-mark)
                                   (eq (overlay-get overlay 'face) 'highlight)))
                            (overlays-at (point)))
             collect (tabulated-list-get-id)
             do (forward-line 1))))

(defun gnosis-test-dashboard--assert-marks (ids)
  "Assert that selection and visible highlights both equal the set IDS."
  (should (equal (sort (copy-sequence ids) #'<)
                 (sort (copy-sequence gnosis-dashboard--selected-ids) #'<)))
  (should (equal (sort (copy-sequence ids) #'<)
                 (sort (gnosis-test-dashboard--marked-ids) #'<))))

(ert-deftest gnosis-dashboard-marks-sort-and-append ()
  "Native and fast sorting preserve one, multiple, or all visible marks."
  (gnosis-test-with-db
    (cl-loop for id from 1 to 4
             for question in '("Zulu" "Alpha" "Charlie" "Bravo")
             do (gnosis-test--add-basic-thema question "Answer" nil nil id))
    (dolist (sort-command '(tabulated-list-sort gnosis-tl-sort))
      (dolist (selection '((1) (1 2) all))
        (gnosis-test-dashboard--with-view
          (gnosis-dashboard-output-themata '(1 2 3 4))
          (if (eq selection 'all)
              (call-interactively (local-key-binding (kbd "M")))
            (dolist (id selection)
              (gnosis-dashboard--goto-id id)
              (call-interactively (local-key-binding (kbd "m")))))
          (let ((selected (copy-sequence gnosis-dashboard--selected-ids)))
            (gnosis-test-dashboard--assert-marks selected)
            (should callbacks)
            (dotimes (_ 2)
              (let ((current-prefix-arg 0)) (call-interactively sort-command))
              (gnosis-test-dashboard--assert-marks selected))
            (drain)
            (should (equal '(1 3 4 2) (gnosis-test-dashboard--visible-ids)))
            (gnosis-test-dashboard--assert-marks selected)
            (let ((current-prefix-arg 0)) (call-interactively sort-command))
            (should (equal '(2 4 3 1) (gnosis-test-dashboard--visible-ids)))
            (gnosis-test-dashboard--assert-marks selected)
            ;; Mark all now includes appended rows, even after a full sort.
            (call-interactively (local-key-binding (kbd "M")))
            (let ((current-prefix-arg 0)) (call-interactively sort-command))
            (gnosis-test-dashboard--assert-marks '(1 2 3 4))
            (gnosis-dashboard--goto-id 2)
            (call-interactively (local-key-binding (kbd "u")))
            (gnosis-test-dashboard--assert-marks '(1 3 4))
            (let ((current-prefix-arg 0)) (call-interactively sort-command))
            (gnosis-test-dashboard--assert-marks '(1 3 4))))))))

(ert-deftest gnosis-dashboard-marks-unsorted-append-and-replace ()
  "Appending and replacing rows cannot move marks onto unselected rows."
  (gnosis-test-with-db
    (dolist (id '(1 2 3 4))
      (gnosis-test--add-basic-thema (number-to-string id) "Answer" nil nil id))
    (gnosis-test-dashboard--with-view
      (gnosis-dashboard-output-themata '(1 2 3 4))
      (call-interactively (local-key-binding (kbd "M")))
      (drain)
      (gnosis-test-dashboard--assert-marks '(1 2))
      (gnosis-update 'themata '(= keimenon "Updated") '(= id 1))
      (gnosis-dashboard-update-entry 1)
      (gnosis-test-dashboard--assert-marks '(1 2))
      (gnosis-tl-print t)
      (gnosis-test-dashboard--assert-marks '(1 2))
      (tabulated-list-print t)
      (gnosis-test-dashboard--assert-marks '(1 2))
      (call-interactively (local-key-binding (kbd "g")))
      (drain)
      (gnosis-test-dashboard--assert-marks nil))))

(ert-deftest gnosis-dashboard-marks-sorted-actions ()
  "Delete and suspend change exactly the rows advertised after native sorting."
  (dolist (action '("d" "s"))
    (dolist (selection '((1) (1 2) all))
      (gnosis-test-with-db
        (cl-loop for id from 1 to 4
                 for question in '("Zulu" "Alpha" "Charlie" "Bravo")
                 do (gnosis-test--add-basic-thema question "Answer" nil nil id))
        (gnosis-test-dashboard--with-view
          (gnosis-dashboard-output-themata '(1 2 3 4))
          (if (eq selection 'all)
              (progn (drain) (call-interactively (local-key-binding (kbd "M"))))
            (dolist (id selection)
              (gnosis-dashboard--goto-id id)
              (call-interactively (local-key-binding (kbd "m")))))
          (tabulated-list-sort 0)
          (when (equal action "s") (tabulated-list-sort 0))
          (let ((advertised (gnosis-test-dashboard--marked-ids))
                (selected (copy-sequence gnosis-dashboard--selected-ids)))
            (gnosis-test-dashboard--assert-marks selected)
            (should advertised)
            ;; An unselected point row must not replace the advertised selection.
            (unless (eq selection 'all)
              (drain)
              (gnosis-dashboard--goto-id 4))
            (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
              (call-interactively (local-key-binding (kbd action))))
            (drain)
            (dolist (id '(1 2 3 4))
              (if (equal action "d")
                  (should (eq (not (gnosis-get 'id 'themata `(= id ,id)))
                              (not (null (member id advertised)))))
                (should (= (if (member id advertised) 1 0)
                           (gnosis-get 'suspended 'scheduler-state
                                       `(= thema-id ,id))))))
            (gnosis-test-dashboard--assert-marks nil)))))))

(ert-deftest gnosis-dashboard-marks-canceled-delete ()
  "A canceled or failed delete preserves the sorted advertised selection."
  (gnosis-test-with-db
    (gnosis-test--add-basic-thema "Zulu" "Answer" nil nil 1)
    (gnosis-test--add-basic-thema "Alpha" "Answer" nil nil 2)
    (gnosis-test-dashboard--with-view
      (gnosis-dashboard-output-themata '(1 2))
      (call-interactively (local-key-binding (kbd "m")))
      (tabulated-list-sort 0)
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) nil)))
        (call-interactively (local-key-binding (kbd "d"))))
      (gnosis-test-dashboard--assert-marks '(1))
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
                ((symbol-function 'gnosis-delete-themata)
                 (lambda (&rest _) (error "Injected delete failure"))))
        (should-error (call-interactively (local-key-binding (kbd "d")))))
      (gnosis-test-dashboard--assert-marks '(1))
      (should (equal '(1 2) (gnosis-select 'id 'themata nil t))))))

(ert-deftest gnosis-dashboard-marks-canceled-suspend ()
  "Decline, quit and SQL failure retain sorted marks for a selected retry."
  (dolist (selection '((1) (1 2)))
    (dolist (failure '(decline quit sql-error))
      (dolist (settled '(nil t))
        (gnosis-test-with-db
          (cl-loop for id from 1 to 4
                   for question in '("Zulu" "Alpha" "Charlie" "Bravo")
                   do (gnosis-test--add-basic-thema question "Answer" nil nil id))
          (gnosis-test-dashboard--with-view
            (gnosis-dashboard-output-themata '(1 2 3 4))
            (dolist (id selection)
              (gnosis-dashboard--goto-id id)
              (call-interactively (local-key-binding (kbd "m"))))
            (let ((current-prefix-arg 0))
              (call-interactively (local-key-binding (kbd "S"))))
            (should callbacks)
            (when settled (drain))
            (when (eq failure 'sql-error)
              (gnosis-sqlite-execute
               gnosis-db
               "CREATE TEMP TRIGGER fail_suspend BEFORE UPDATE ON scheduler_state
                BEGIN SELECT RAISE(ABORT, 'Injected suspension SQL failure'); END"))
            (let ((before (gnosis-select '* 'scheduler-state)))
              (cl-letf (((symbol-function 'y-or-n-p)
                         (lambda (&rest _)
                           ;; Settle pending rows while confirmation is active.
                           (drain)
                           (pcase failure
                             ('decline nil)
                             ('quit (signal 'quit nil))
                             (_ t)))))
                (pcase failure
                  ('decline
                   (call-interactively (local-key-binding (kbd "s"))))
                  ('quit
                   (should (eq 'quit
                               (condition-case nil
                                   (call-interactively
                                    (local-key-binding (kbd "s")))
                                 (quit 'quit)))))
                  ('sql-error
                   (should-error
                    (call-interactively (local-key-binding (kbd "s")))))))
              (should (equal before (gnosis-select '* 'scheduler-state)))
              (gnosis-test-dashboard--assert-marks selection)
              (when (eq failure 'sql-error)
                (gnosis-sqlite-execute gnosis-db "DROP TRIGGER fail_suspend"))
              ;; Retry from a different, unselected row using the real writer.
              (gnosis-dashboard--goto-id 4)
              (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
                (call-interactively (local-key-binding (kbd "s"))))
              (drain)
              (dolist (id '(1 2 3 4))
                (should (= (if (member id selection) 1 0)
                           (gnosis-get 'suspended 'scheduler-state
                                       `(= thema-id ,id)))))
              (gnosis-test-dashboard--assert-marks nil))))))))

(ert-deftest gnosis-dashboard-marks-suspend-prefix-and-point ()
  "Single marks toggle; bulk prefix unsuspends; an unmarked action uses point."
  (gnosis-test-with-db
    (dolist (id '(1 2 3))
      (gnosis-test--add-basic-thema (number-to-string id) "Answer" nil nil id))
    (gnosis-test-dashboard--with-view
      (gnosis-dashboard-output-themata '(1 2 3))
      (drain)
      (dolist (case '(((1) nil "Suspend thema? " ((1 1) (2 0) (3 0)))
                      ((1) (4) "Unsuspend thema? " ((1 0) (2 0) (3 0)))
                      ((1 2) nil "Suspend 2 themata? " ((1 1) (2 1) (3 0)))
                      ((1 2) (4) "Unsuspend 2 themata? " ((1 0) (2 0) (3 0)))
                      (nil nil "Suspend thema? " ((1 0) (2 0) (3 1)))
                      (nil nil "Unsuspend thema? " ((1 0) (2 0) (3 0)))))
        (dolist (id (car case))
          (gnosis-dashboard--goto-id id)
          (call-interactively (local-key-binding (kbd "m"))))
        (gnosis-dashboard--goto-id 3)
        (let ((current-prefix-arg (nth 1 case)) prompts)
          (cl-letf (((symbol-function 'y-or-n-p)
                     (lambda (prompt) (push prompt prompts) t)))
            (call-interactively (local-key-binding (kbd "s"))))
          (should (equal (list (nth 2 case)) prompts)))
        (should (equal (nth 3 case)
                       (sort (gnosis-select '[thema-id suspended] 'scheduler-state)
                             (lambda (a b) (< (car a) (car b))))))
        (gnosis-test-dashboard--assert-marks nil)))))

(ert-deftest gnosis-dashboard-marks-tag-sort ()
  "String tag identities retain exact marks after native and fast redraw."
  (gnosis-test-with-db
    (gnosis-test--add-basic-thema "Question" "Answer" '("z" "a" "b"))
    (gnosis-test-dashboard--with-view
      (gnosis-dashboard-output-tags '("z" "a" "b"))
      (gnosis-dashboard--goto-id "z")
      (call-interactively (local-key-binding (kbd "m")))
      (dolist (sort-command '(tabulated-list-sort gnosis-tl-sort))
        (let ((current-prefix-arg 0)) (call-interactively sort-command))
        (should (equal '("z") gnosis-dashboard--selected-ids))
        (should (equal '("z") (gnosis-test-dashboard--marked-ids))))
      (gnosis-dashboard--goto-id "z")
      (call-interactively (local-key-binding (kbd "u")))
      (should-not gnosis-dashboard--selected-ids)
      (should-not (gnosis-test-dashboard--marked-ids)))))

(provide 'gnosis-test-dashboard-marks)
;;; gnosis-test-dashboard-marks.el ends here

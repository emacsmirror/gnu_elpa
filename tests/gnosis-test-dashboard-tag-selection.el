;;; gnosis-test-dashboard-tag-selection.el --- Tag command selection tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Thanos Apollo <public@thanosapollo.org>

;;; Commentary:

;; Preserve logical and visible marks until a tag mutation succeeds.

;;; Code:

(require 'ert)
(require 'gnosis-dashboard)
(require 'gnosis-test-helpers)

(defmacro gnosis-test-tag-selection--with-view (&rest body)
  "Run BODY with two marked tags and an unrelated tag on disposable themata."
  (declare (indent 0) (debug t))
  `(gnosis-test-with-db
     (gnosis-test--add-basic-thema "A" "Answer" '("a") nil 41)
     (gnosis-test--add-basic-thema "B" "Answer" '("b") nil 42)
     (gnosis-test--add-basic-thema "Other" "Answer" '("other") nil 43)
     (let* ((buffer (generate-new-buffer " *gnosis-tag-selection*"))
            (gnosis-dashboard-buffer-name (buffer-name buffer)))
       (unwind-protect
           (save-window-excursion
             (with-current-buffer buffer (gnosis-dashboard-mode))
             (gnosis-dashboard-output-tags '("a" "b" "other"))
             (goto-char (point-min))
             (call-interactively (local-key-binding (kbd "m")))
             (call-interactively (local-key-binding (kbd "m")))
             ,@body)
         (when (buffer-live-p buffer) (kill-buffer buffer))))))

(defun gnosis-test-tag-selection--snapshot ()
  "Return database rows, logical marks, visible marks and displayed text."
  (list
   (gnosis-sqlite-select gnosis-db "SELECT * FROM thema_tag ORDER BY thema_id, tag")
   (gnosis-sqlite-select gnosis-db "SELECT * FROM scheduler_state ORDER BY thema_id")
   (copy-sequence gnosis-dashboard--selected-ids)
   (save-excursion
     (goto-char (point-min))
     (cl-loop until (eobp)
              when (seq-some (lambda (ov) (overlay-get ov 'gnosis-mark))
                             (overlays-at (point)))
              collect (tabulated-list-get-id)
              do (forward-line 1)))
   (buffer-substring-no-properties (point-min) (point-max))
   (copy-sequence gnosis-dashboard-tags-current)))

(ert-deftest gnosis-dashboard-tag-selection-cancel-and-error ()
  "Declines, prompt quits, invalid regexps and write errors preserve marks."
  (dolist (key '("d" "s" "R"))
    (dolist (failure (append '(decline confirm-quit mutation-error)
                             (when (equal key "R")
                               '(pattern-quit replacement-quit invalid-regexp no-match))))
      (ert-info ((format "key=%s failure=%s" key failure))
        (gnosis-test-tag-selection--with-view
          (let ((before (gnosis-test-tag-selection--snapshot))
                (mutation (pcase key
                            ("d" 'gnosis-sqlite-execute-batch)
                            ("s" 'gnosis-toggle-suspend-themata)
                            ("R" 'gnosis--tag-rename-batch)))
                (reads 0))
            (cl-letf (((symbol-function 'read-string)
                       (lambda (&rest _)
                         (setq reads (1+ reads))
                         (when (or (and (= reads 1) (eq failure 'pattern-quit))
                                   (and (= reads 2) (eq failure 'replacement-quit)))
                           (signal 'quit nil))
                         (if (= reads 2) "new_\\&"
                           (pcase failure
                             ('invalid-regexp "[")
                             ('no-match "not_present")
                             (_ "[ab]")))))
                      ((symbol-function 'y-or-n-p)
                       (lambda (&rest _)
                         (when (eq failure 'confirm-quit) (signal 'quit nil))
                         (not (eq failure 'decline)))))
              (let ((invoke (lambda ()
                              (call-interactively (local-key-binding (kbd key))))))
                (if (eq failure 'mutation-error)
                    (cl-letf (((symbol-function mutation)
                               (lambda (&rest _) (error "Injected mutation failure"))))
                      (should-error (funcall invoke) :type 'error))
                  (pcase failure
                    ((or 'confirm-quit 'pattern-quit 'replacement-quit)
                     (should (eq 'quit (condition-case nil (funcall invoke)
                                        (quit 'quit)))))
                    ('invalid-regexp (should-error (funcall invoke) :type 'invalid-regexp))
                    ('no-match (should-error (funcall invoke) :type 'user-error))
                    (_ (funcall invoke))))))
            (should (equal before (gnosis-test-tag-selection--snapshot)))))))))

(ert-deftest gnosis-dashboard-tag-selection-interrupted-write ()
  "Errors and quits after a SQL write roll back data and retain both marks."
  (dolist (key '("d" "s" "R"))
    (dolist (condition '(error quit))
      (ert-info ((format "key=%s condition=%s" key condition))
        (gnosis-test-tag-selection--with-view
          (let ((before (gnosis-test-tag-selection--snapshot))
                (execute (symbol-function 'sqlite-execute))
                (reads 0)
                interrupted)
            (cl-letf (((symbol-function 'read-string)
                       (lambda (&rest _)
                         (if (= (cl-incf reads) 1) "[ab]" "new")))
                      ((symbol-function 'y-or-n-p) (lambda (&rest _) t))
                      ((symbol-function 'sqlite-execute)
                       (lambda (db sql &rest args)
                         (prog1 (apply execute db sql args)
                           (when (and (not interrupted)
                                      (string-match-p
                                       "DELETE FROM thema_tag\\|UPDATE scheduler_state"
                                       sql))
                             (setq interrupted t)
                             (signal condition '("Injected after SQL write")))))))
              (should (eq condition
                          (condition-case err
                              (call-interactively (local-key-binding (kbd key)))
                            ((error quit) (car err))))))
            (should interrupted)
            (should (equal before (gnosis-test-tag-selection--snapshot)))))))))

(ert-deftest gnosis-dashboard-tag-selection-success ()
  "Successful commands affect marked tags, clear both marks and permit refresh."
  (dolist (key '("d" "s" "R"))
    (gnosis-test-tag-selection--with-view
      (let ((reads 0))
        (cl-letf (((symbol-function 'read-string)
                   (lambda (&rest _)
                     (if (= (cl-incf reads) 1) "[ab]" "new_\\&")))
                  ((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
          (call-interactively (local-key-binding (kbd key)))))
      (should-not (nth 2 (gnosis-test-tag-selection--snapshot)))
      (should-not (nth 3 (gnosis-test-tag-selection--snapshot)))
      (should (equal '("other") (gnosis-get-tags-for-ids '(43))))
      (should-not (gnosis-suspended-p 43))
      (pcase key
        ("d" (should-not (gnosis-get-tags-for-ids '(41 42))))
        ("s" (should (gnosis-suspended-p 41))
             (should (gnosis-suspended-p 42))
             ;; The prefix variant remains usable with a fresh selection.
             (goto-char (point-min))
             (call-interactively (local-key-binding (kbd "m")))
             (call-interactively (local-key-binding (kbd "m")))
             (let ((current-prefix-arg '(4)))
               (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
                 (call-interactively (local-key-binding (kbd "s")))))
             (should-not (gnosis-suspended-p 41))
             (should-not (gnosis-suspended-p 42)))
        ("R" (should (equal '("new_a" "new_b")
                            (gnosis-get-tags-for-ids '(41 42))))))
      (call-interactively (local-key-binding (kbd "g")))
      (should-not (nth 2 (gnosis-test-tag-selection--snapshot)))
      (should-not (nth 3 (gnosis-test-tag-selection--snapshot))))))

(provide 'gnosis-test-dashboard-tag-selection)
;;; gnosis-test-dashboard-tag-selection.el ends here

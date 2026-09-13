;;; gnosis-test-interaction-integration.el --- Merged interaction contracts -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.
;; Author: Thanos Apollo <public@thanosapollo.org>

;;; Commentary:
;; Exercise ownership, sorted rendering and mark settlement together.

;;; Code:

(require 'gnosis-test-dashboard-owner)
(require 'gnosis-test-dashboard-marks)
(require 'gnosis-test-dashboard-tag-selection)

(ert-deftest gnosis-interaction-sorted-append-during-confirmation ()
  "Completing the same sorted rendering must not retire its pending command."
  (dolist (key '("s" "d" "b" "t"))
    (gnosis-test-with-db
      (cl-loop for id from 1 to 4
               for question in '("Zulu" "Alpha" "Charlie" "Bravo")
               do (gnosis-test--add-basic-thema question "Answer" nil nil id))
      (gnosis--insert-into 'nodes '(["node-1" "disposable.org" "Target" "0" nil nil nil]))
      (gnosis-test-dashboard--with-view
        (gnosis-dashboard-output-themata '(1 2 3 4))
        (call-interactively (local-key-binding (kbd "m")))
        (tabulated-list-sort 0)
        (should callbacks)
        (let ((prompts 0))
          (cl-letf (((symbol-function 'y-or-n-p)
                     (lambda (&rest _)
                       (cl-incf prompts)
                       (drain)
                       (gnosis-test-dashboard--assert-marks '(1))
                       t))
                    ((symbol-function 'read-string) (lambda (&rest _) "Zulu"))
                    ((symbol-function 'gnosis-completing-read) (lambda (&rest _) "Target"))
                    ((symbol-function 'completing-read-multiple)
                     (lambda (&rest _)
                       (drain)
                       (gnosis-test-dashboard--assert-marks '(1))
                       '("+new"))))
            (call-interactively (local-key-binding (kbd key))))
          (should (= prompts (if (equal key "t") 0 1))))
        (drain)
        (gnosis-test-dashboard--assert-marks nil)
        (pcase key
          ("s" (should (gnosis-suspended-p 1)))
          ("d" (should-not (gnosis-get 'id 'themata '(= id 1))))
          ("b" (should (equal "[[id:node-1][Zulu]]"
                              (gnosis-get 'keimenon 'themata '(= id 1)))))
          ("t" (should (equal '("new" "test") (gnosis-get-tags-for-ids '(1))))))
        (should (equal '(2 3 4) (gnosis-select 'id 'themata '(in id [2 3 4]) t)))
        (should (equal '("test") (gnosis-get-tags-for-ids '(2 3 4))))
        (should-not (gnosis-suspended-p 4))))))

(ert-deftest gnosis-interaction-sorted-selection-owner-replacement ()
  "Stale confirmations cannot write or settle a successor's sorted marks."
  (dolist (view '(themata tags))
    (dolist (key (if (eq view 'themata) '("s" "d") '("s" "d" "R")))
      (dolist (change '(database refresh replace))
        (gnosis-test-dashboard-owner--with-databases
          (cl-labels ((render ()
                        (if (eq view 'themata)
                            (gnosis-dashboard-output-themata '(42 43))
                          (gnosis-dashboard-output-tags))
                        (goto-char (point-min))
                        (call-interactively (local-key-binding (kbd "m")))
                        (tabulated-list-sort 0)))
            (render)
            (let ((original (gnosis-test-dashboard-owner--snapshot original-db))
                  (successor (gnosis-test-dashboard-owner--snapshot successor-db))
                  (reads 0) (prompts 0) after)
              (cl-letf (((symbol-function 'read-string)
                         (lambda (&rest _) (if (= (cl-incf reads) 1) "tag" "new")))
                        ((symbol-function 'y-or-n-p)
                         (lambda (&rest _)
                           (cl-incf prompts)
                           (pcase change
                             ('database (setq gnosis-db successor-db gnosis-dir successor-dir))
                             ('refresh (render))
                             ('replace
                              (kill-buffer buffer)
                              ;; Native acquisition owns the fresh object, not its name.
                              (setq buffer (gnosis-dashboard--buffer))
                              (with-current-buffer buffer (gnosis-dashboard-mode))
                              (render)))
                           (setq after (list (gnosis-test-dashboard-owner--view)
                                             (gnosis-test-dashboard--marked-ids)))
                           t)))
                (should-error (call-interactively (local-key-binding (kbd key)))
                              :type 'user-error))
              (should (= prompts 1))
              (should (equal after (list (gnosis-test-dashboard-owner--view)
                                         (gnosis-test-dashboard--marked-ids))))
              (should (equal original (gnosis-test-dashboard-owner--snapshot original-db)))
              (should (equal successor (gnosis-test-dashboard-owner--snapshot successor-db)))
              ;; Explicit refresh adopts the live connection and a new command owner.
              (call-interactively (local-key-binding (kbd "g")))
              (should (eq gnosis-db gnosis-dashboard--database))
              (should-not gnosis-dashboard--selected-ids)
              (should-not (gnosis-test-dashboard--marked-ids))
              (gnosis-dashboard--command-owner))))))))

(ert-deftest gnosis-interaction-fresh-acquisition-and-refresh ()
  "Fresh native acquisition qualifies rows without accepting a name collision."
  (gnosis-test-with-db
    (gnosis-test--add-basic-thema "Question" "Answer" '("tag") nil 1)
    (let ((gnosis-dashboard-buffer-name (generate-new-buffer-name " *integration*"))
          buffer)
      (unwind-protect
          (save-window-excursion
            (gnosis-dashboard-output-themata '(1))
            (setq buffer (current-buffer))
            (should (eq buffer (gnosis-dashboard--buffer)))
            (call-interactively (local-key-binding (kbd "m")))
            (tabulated-list-sort 0)
            (let ((owner (gnosis-dashboard--command-owner)))
              (call-interactively (local-key-binding (kbd "g")))
              (should-error (gnosis-dashboard--command-owner owner) :type 'user-error))
            (gnosis-test-dashboard--assert-marks nil)
            (gnosis-dashboard-output-tags)
            (should (eq buffer (current-buffer)))
            (gnosis-dashboard--command-owner)
            (fundamental-mode)
            (let ((inhibit-read-only t)) (insert "Unrelated draft"))
            (let ((text (buffer-string)))
              (should-error (gnosis-dashboard-output-themata '(1)) :type 'user-error)
              (should (equal text (buffer-string)))))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(provide 'gnosis-test-interaction-integration)
;;; gnosis-test-interaction-integration.el ends here

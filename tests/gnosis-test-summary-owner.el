;;; gnosis-test-summary-owner.el --- Summary lifetime tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;;; Commentary:
;; Exercise retained summary commands across native buffer repurposing.

;;; Code:

(require 'gnosis-test-study)

(defun gnosis-test-summary-owner-evidence ()
  "Return study evidence and authored thema rows."
  (cons (gnosis-test-study-all-evidence)
        (mapcar (lambda (table) (gnosis-select '* table))
                '(themata extras thema-tag thema-links))))

(defun gnosis-test-summary-owner-repurpose (file detach)
  "Associate the current summary with FILE, optionally DETACH, and replace text."
  (set-visited-file-name file t)
  (when detach (set-visited-file-name nil t))
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert "Unrelated successor text")))

(defun gnosis-test-summary-owner-retained (key)
  "Require retained KEY to refuse associated and detached summaries."
  (dolist (mode '(due practice))
    (dolist (detach '(nil t))
      (gnosis-test-study
        (let* ((id (gnosis-test--add-basic-thema "A" "A"))
               (summary (gnosis-test-study-summary (list id id) mode))
               (file (expand-file-name "source.org" gnosis-dir))
               (before (gnosis-test-summary-owner-evidence)))
          (with-temp-file file (insert "Existing source\n"))
          (with-current-buffer summary
            (let ((command (local-key-binding (kbd key))))
              (gnosis-test-summary-owner-repurpose file detach)
              (should (derived-mode-p 'gnosis-review-summary-mode))
              (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
                        ((symbol-function 'gnosis--read-string-with-input-method)
                         (lambda (&rest _) "A"))
                        ((symbol-function 'read-char-choice) (lambda (&rest _) ?q)))
                (dotimes (_ 2)
                  (should-error (call-interactively command) :type 'user-error)
                  (should (equal before (gnosis-test-summary-owner-evidence)))
                  (should (equal "Unrelated successor text" (buffer-string)))))))
          (should (equal "Existing source\n"
                         (with-temp-buffer
                           (insert-file-contents file) (buffer-string)))))))))

(ert-deftest gnosis-summary-owner-retained-undo ()
  (gnosis-test-summary-owner-retained "u"))

(ert-deftest gnosis-summary-owner-retained-resume ()
  (gnosis-test-summary-owner-retained "r"))

(ert-deftest gnosis-summary-owner-retained-discard ()
  (gnosis-test-summary-owner-retained "d"))

(ert-deftest gnosis-summary-owner-discard-rechecks-after-confirmation ()
  (dolist (mode '(due practice))
    (dolist (mutation '(file mode kill))
      (gnosis-test-study
        (let* ((id (gnosis-test--add-basic-thema "A" "A"))
               (summary (gnosis-test-study-summary (list id id) mode))
               (before (gnosis-test-summary-owner-evidence)))
          (with-current-buffer summary
            (cl-letf (((symbol-function 'y-or-n-p)
                       (lambda (&rest _)
                         (with-current-buffer summary
                           (pcase mutation
                             ('file (gnosis-test-summary-owner-repurpose
                                     (expand-file-name "successor" gnosis-dir) t))
                             ('mode (fundamental-mode)
                                    (let ((inhibit-read-only t))
                                      (erase-buffer) (insert "Successor")))
                             ('kill (kill-buffer summary))))
                         t)))
              (should-error (call-interactively (local-key-binding (kbd "d")))
                            :type 'user-error)))
          (should (equal before (gnosis-test-summary-owner-evidence)))
          (when (buffer-live-p summary)
            (with-current-buffer summary
              (should (equal (if (eq mutation 'file) "Unrelated successor text" "Successor")
                             (buffer-string))))))))))

(ert-deftest gnosis-summary-owner-resume-rechecks-after-setup ()
  (dolist (mode '(due practice))
    (dolist (launch '(nil t))
      (gnosis-test-study
        (let* ((id (gnosis-test--add-basic-thema "A" "A"))
               (_summary (gnosis-test-study-summary (list id id) mode))
               (state (gnosis-review--read-session)))
          (when launch
            (setf (gnosis-review-state-launch-token state) "pending-launch")
            (gnosis-review--save-session state))
          (gnosis-review--show-summary state)
          (let* ((summary (current-buffer))
                 (before (gnosis-test-summary-owner-evidence))
                 (gnosis-review-buffer-name "*Fresh summary resume*")
                 (gnosis-mode-hook
                  (list (lambda ()
                          (with-current-buffer summary
                            (gnosis-test-summary-owner-repurpose
                             (expand-file-name "successor" gnosis-dir) t))))))
            (cl-letf (((symbol-function 'gnosis--read-string-with-input-method)
                       (lambda (&rest _) "A"))
                      ((symbol-function 'read-char-choice) (lambda (&rest _) ?q)))
              (should-error (call-interactively (local-key-binding (kbd "r")))
                            :type 'user-error))
            (should (equal before (gnosis-test-summary-owner-evidence)))
            (with-current-buffer summary
              (should (equal "Unrelated successor text" (buffer-string))))))))))

(ert-deftest gnosis-summary-owner-mode-hook-cannot-reclaim-retired-target ()
  (dolist (mode '(due practice))
    (gnosis-test-study
      (let* ((id (gnosis-test--add-basic-thema "A" "A"))
             (gnosis-review-summary-mode-hook
              (list (lambda ()
                      (gnosis-test-summary-owner-repurpose
                       (expand-file-name "successor" gnosis-dir) t))))
             (summary (gnosis-test-study-summary (list id id) mode))
             (before (gnosis-test-summary-owner-evidence)))
        (with-current-buffer summary
          (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
            (dolist (key '("u" "d" "r"))
              (should-error (call-interactively (local-key-binding (kbd key)))
                            :type 'user-error)))
          (should (equal "Unrelated successor text" (buffer-string))))
        (should (equal before (gnosis-test-summary-owner-evidence)))))))

(ert-deftest gnosis-summary-owner-valid-resume-through-setup ()
  (dolist (mode '(due practice))
    (gnosis-test-study
      (let* ((id (gnosis-test--add-basic-thema "A" "A"))
             (_summary (gnosis-test-study-summary (list id id) mode))
             (state (gnosis-review--read-session))
             (schedule (gnosis-test-study-snapshot))
             (setup-count 0)
             (summary-count 0)
             (gnosis-review-summary-mode-hook
              (list (lambda () (cl-incf summary-count)))))
        (setf (gnosis-review-state-launch-token state) "pending-launch")
        (gnosis-review--save-session state)
        (gnosis-review--show-summary state)
        (let ((gnosis-review-buffer-name "*Fresh valid summary resume*")
              (gnosis-mode-hook
               (list (lambda ()
                       (cl-incf setup-count)
                       ;; A deferred or reentrant resume must not consume the batch.
                       (should-error (gnosis-review-resume) :type 'user-error)))))
          (cl-letf (((symbol-function 'gnosis--read-string-with-input-method)
                     (lambda (&rest _) "A"))
                    ((symbol-function 'read-char-choice) (lambda (&rest _) ?n)))
            (call-interactively (local-key-binding (kbd "r")))))
        (should (= 1 setup-count))
        (should (= 2 summary-count))
        (should (= 2 (gnosis-review-state-reviewed (gnosis-review--read-session))))
        (should-not (gnosis-review-state-launch-token (gnosis-review--read-session)))
        (when (eq mode 'practice)
          (should (equal schedule (gnosis-test-study-snapshot))))))))

(ert-deftest gnosis-summary-owner-ordinary-public-actions ()
  (dolist (mode '(due practice))
    (gnosis-test-study
      (let* ((id (gnosis-test--add-basic-thema "A" "A"))
             (_summary (gnosis-test-study-summary (list id) mode)))
        (with-temp-buffer
          (insert "Ordinary buffer")
          (call-interactively #'gnosis-review-undo)
          (should (= 0 (gnosis-review-state-reviewed (gnosis-review--read-session))))
          (cl-letf (((symbol-function 'gnosis--read-string-with-input-method)
                     (lambda (&rest _) "A"))
                    ((symbol-function 'read-char-choice) (lambda (&rest _) ?n)))
            (save-current-buffer
              (call-interactively #'gnosis-review-resume)))
          (should (= 1 (gnosis-review-state-reviewed (gnosis-review--read-session))))
          (let ((accepted (gnosis-test-study-snapshot)))
            (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
              (call-interactively #'gnosis-review-discard))
            (should-not (gnosis-review--read-session))
            (should (equal accepted (gnosis-test-study-snapshot))))
          (should (equal "Ordinary buffer" (buffer-string))))))))

(ert-deftest gnosis-summary-owner-discard-cancel-preserves-checkpoint ()
  (dolist (mode '(due practice))
    (gnosis-test-study
      (let* ((id (gnosis-test--add-basic-thema "A" "A"))
             (summary (gnosis-test-study-summary (list id id) mode))
             (before (gnosis-test-summary-owner-evidence)))
        (with-current-buffer summary
          (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) nil)))
            (call-interactively (local-key-binding (kbd "d"))))
          (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) (signal 'quit nil))))
            (should (eq 'cancelled
                        (condition-case nil
                            (call-interactively (local-key-binding (kbd "d")))
                          (quit 'cancelled))))))
        (should (equal before (gnosis-test-summary-owner-evidence)))))))

(provide 'gnosis-test-summary-owner)
;;; gnosis-test-summary-owner.el ends here

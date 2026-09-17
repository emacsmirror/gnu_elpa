;;; gnosis-test-study-retired.el --- Early study-day ownership tests -*- lexical-binding: t; -*-

(require 'gnosis-test-study-day)

(ert-deftest gnosis-study-day-initial-mode-body-retirement ()
  "Preserve a successor created by the parent's native pre-mode hook."
  (gnosis-test-study-day
    (let* ((day (gnosis-test-study-day-seed))
           (evidence (gnosis-test-study-day-snapshot))
           (file (expand-file-name "early-hook.org" gnosis-dir))
           (fired nil) view before outcome
           (change-major-mode-hook
            (list (lambda ()
                    (when (and (not fired)
                               (equal (buffer-name) gnosis-study-day-buffer-name))
                      (setq fired t view (current-buffer))
                      (set-visited-file-name file t)
                      (set-visited-file-name nil t)
                      (insert "Early-hook successor λ\n")
                      (setq before (gnosis-test-study-day-buffer-state view)))))))
      (should (gnosis-select '* 'review-events))
      (should (gnosis-select '* 'practice-events))
      (condition-case err (gnosis-study-day day) (error (setq outcome err)))
      (should fired)
      (should (buffer-live-p view))
      (should (equal evidence (gnosis-test-study-day-snapshot)))
      (message "INITIAL-HOOK outcome=%S before=%S after=%S modified=%S"
               outcome before (with-current-buffer view (buffer-string))
               (with-current-buffer view (buffer-modified-p)))
      (should (equal "Early-hook successor λ\n" (car before)))
      (should (equal before (gnosis-test-study-day-buffer-state view)))
      (should (with-current-buffer view (buffer-modified-p)))
      (should (or (eq (car-safe outcome) 'user-error)
                  (not (eq view (current-buffer))))))))

(ert-deftest gnosis-study-day-initial-mode-roundtrip-retirement ()
  "A nested native mode transition must retire initial acquisition."
  (gnosis-test-study-day
    (let* ((day (gnosis-test-study-day-seed))
           (evidence (gnosis-test-study-day-snapshot))
           (fired nil) view before
           (change-major-mode-hook
            (list (lambda ()
                    (when (and (not fired)
                               (equal (buffer-name) gnosis-study-day-buffer-name))
                      (setq fired t view (current-buffer))
                      (text-mode)
                      (gnosis-study-day-mode)
                      (let ((inhibit-read-only t))
                        (insert "Initial-mode successor λ\n"))
                      (setq before (gnosis-test-study-day-buffer-state view)))))))
      (should-error (gnosis-study-day day) :type 'user-error)
      (should fired)
      (should (equal "Initial-mode successor λ\n" (car before)))
      (should (equal before (gnosis-test-study-day-buffer-state view)))
      (should (with-current-buffer view (buffer-modified-p)))
      (should (equal evidence (gnosis-test-study-day-snapshot))))))

(ert-deftest gnosis-study-day-benign-initial-mode-hook ()
  "Run benign pre-mode hooks without retiring an ordinary fresh view."
  (gnosis-test-study-day
    (let* ((day (gnosis-test-study-day-seed))
           (evidence (gnosis-test-study-day-snapshot))
           (calls 0)
           (tail-calls 0)
           (change-major-mode-hook
            (list (lambda ()
                    (when (equal (buffer-name) gnosis-study-day-buffer-name)
                      (cl-incf calls)
                      (with-temp-buffer (text-mode)))
                    t)
                  (lambda ()
                    (when (equal (buffer-name) gnosis-study-day-buffer-name)
                      (cl-incf tail-calls))))))
      (gnosis-study-day day)
      (should (= calls 1))
      (should (= tail-calls 1))
      (should (derived-mode-p 'gnosis-study-day-mode))
      (should buffer-read-only)
      (should-not (buffer-modified-p))
      (should (string-search "Accepted attempts: 2" (buffer-string)))
      (should (string-search "Scheduled: 1 (New: 1)" (buffer-string)))
      (should (string-search "Practice: 1" (buffer-string)))
      (let ((view (current-buffer)))
        (gnosis-study-day 19990101)
        (should (eq view (current-buffer)))
        (gnosis-study-day day)
        (should (eq view (current-buffer)))
        (should (= calls 1)))
      (should (equal evidence (gnosis-test-study-day-snapshot))))))

(ert-deftest gnosis-study-day-initial-successor-mode-state ()
  "Stop the parent's reset before it changes an early successor's locals."
  (gnosis-test-study-day
    (let* ((day (gnosis-test-study-day-seed))
           (evidence (gnosis-test-study-day-snapshot))
           fired view before
           (change-major-mode-hook
            (list (lambda ()
                    (when (and (not fired)
                               (equal (buffer-name) gnosis-study-day-buffer-name))
                      (setq fired t view (current-buffer))
                      (text-mode)
                      (insert "Editable successor λ\n")
                      (setq-local header-line-format "Successor text-mode header")
                      (setq before (gnosis-test-study-day-buffer-state view)))))))
      (should-error (gnosis-study-day day) :type 'user-error)
      (should fired)
      (should (equal before (gnosis-test-study-day-buffer-state view)))
      (should (equal evidence (gnosis-test-study-day-snapshot))))))

(ert-deftest gnosis-study-day-initial-hook-selects-unrelated-draft ()
  "Do not continue native setup or publish ownership in a hook's destination."
  (gnosis-test-study-day
    (let* ((day (gnosis-test-study-day-seed))
           (evidence (gnosis-test-study-day-snapshot))
           (draft (generate-new-buffer " *Early unrelated native draft*"))
           fired before
           (change-major-mode-hook
            (list (lambda ()
                    (when (and (not fired)
                               (equal (buffer-name) gnosis-study-day-buffer-name))
                      (setq fired t)
                      (switch-to-buffer draft))))))
      (with-current-buffer draft
        (text-mode)
        (insert "Unrelated draft λ\n")
        (setq-local header-line-format "Unrelated draft header"))
      (setq before (gnosis-test-study-day-buffer-state draft))
      (should-error (gnosis-study-day day) :type 'user-error)
      (should fired)
      (should (equal before (gnosis-test-study-day-buffer-state draft)))
      (should (equal evidence (gnosis-test-study-day-snapshot))))))

(provide 'gnosis-test-study-retired)
;;; gnosis-test-study-retired.el ends here

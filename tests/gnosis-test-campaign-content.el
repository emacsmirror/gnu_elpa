;;; gnosis-test-campaign-content.el --- Content qualification -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;;; Commentary:
;; Saved physical owners and exact organization receipts.

;;; Code:
(require 'gnosis-test-agent-content)

(defun gnosis-test-campaign-content-seed ()
  "Create content with nonempty scheduled and practice evidence."
  (gnosis-add-thema-fields "basic" "Question α" '("hint") '("answer") ""
                           '("keep") 0 nil "image.png" 101 '("alias"))
  (gnosis-test--add-basic-thema "Other" "other" nil nil 102)
  (gnosis-scheduler-accept-review (gnosis-scheduler-event-id) 101 'success
                                  1780000000000000 (gnosis--today-int))
  (let ((gnosis-agent--launches nil))
    (unwind-protect
        (cl-letf (((symbol-function 'run-with-timer) (lambda (&rest _) nil)))
          (gnosis-agent-start-practice :thema-ids '(101 102) :limit 2)
          (let ((gnosis-review--state (gnosis-review--read-session)))
            (gnosis-review-result 101 t (gnosis-review-algorithm 101 t))))
      (mapc #'gnosis-agent--release (copy-sequence gnosis-agent--launches))))
  (should (gnosis-select '* 'review-events))
  (should (gnosis-select '* 'practice-events)))

(ert-deftest gnosis-campaign-content-preview-value-fidelity ()
  (gnosis-test-with-db
    (gnosis-test-campaign-content-seed)
    (dolist (value '(nil "" "Teaching α\nSecond line"))
      (dolist (fields '(nil (:add-tags ["topic"]) (:add-sources [])
                           (:remove-sources []) (:add-sources nil)))
        (gnosis-update-thema 101 "Question α" '("hint") '("answer")
                             value '("keep") nil)
        (let* ((before (gnosis-agent-content-fetch ["101" "102"]))
               (evidence (gnosis-test-content-evidence))
               (plan (gnosis-test-content-plan
                      (vector (apply #'gnosis-test-content-change "101" fields))))
               (item (aref (plist-get plan :items) 0))
               ;; Explicit source fields retain the content-writer contract:
               ;; empty operations normalize absent teaching text to "".
               (expected (if (or (plist-member fields :add-sources)
                                 (plist-member fields :remove-sources))
                             (or value "") value)))
          (should (equal plan (gnosis-test-content-roundtrip plan)))
          (should (equal expected (plist-get item :parathema)))
          (should (equal before (gnosis-agent-content-fetch ["101" "102"])))
          (let* ((result (gnosis-agent-content-apply (gnosis-test-content-roundtrip plan)))
                 (actual (aref (plist-get result :items) 0)))
            (should (equal result (gnosis-test-content-roundtrip result)))
            (should (equal expected (plist-get actual :parathema)))
            (dolist (key '(:question :hypothesis :answers :accepted-aliases
                          :review-image :rubric :source-ids))
              (should (equal (plist-get actual key)
                             (plist-get (aref (plist-get before :items) 0) key))))
            (let ((gnosis-db (gnosis-sqlite-open gnosis-test--db-file)))
              (unwind-protect
                  (should (equal (plist-get result :items)
                                 (plist-get (gnosis-agent-content-fetch ["101"]) :items)))
                (gnosis-sqlite-close gnosis-db))))
          (should (equal evidence (gnosis-test-content-evidence)))
          (should (equal (aref (plist-get before :items) 1)
                         (aref (plist-get (gnosis-agent-content-fetch ["102"]) :items) 0))))))))

(ert-deftest gnosis-campaign-content-physical-dirty-owners ()
  (gnosis-test-with-db
    (gnosis-test-campaign-content-seed)
    (let* ((gnosis-nodes-dir gnosis-dir)
           (file (expand-file-name "source.org" gnosis-dir))
           (text ":PROPERTIES:\n:ID: source\n:END:\n#+title: Source\nSaved α\n")
           (evidence (gnosis-test-content-evidence)))
      (with-temp-file file (insert text))
      (gnosis-nodes--update-file file)
      (dolist (kind '(hardlink symlink))
        (let ((alias (expand-file-name (format "%s.org" kind) gnosis-dir)))
          (if (eq kind 'hardlink) (add-name-to-file file alias)
            (make-symbolic-link file alias))
          (should (file-equal-p file alias))
          (with-temp-buffer
            (set-visited-file-name alias)
            (insert text)
            (set-buffer-modified-p nil)
            (should (equal text (plist-get (gnosis-agent-content-source "source") :text)))
            (let* ((changes (vector (gnosis-test-content-change
                                     "101" :add-sources [(:id "source" :label "Source α")])))
                   (plan (gnosis-test-content-plan changes))
                   (before (gnosis-agent-content-fetch ["101" "102"])))
              (insert "Private unsaved draft")
              (let ((draft (buffer-string)))
                ;; The exact clean spelling must not hide a dirty alias.
                (with-temp-buffer
                  (set-visited-file-name file t)
                  (insert text)
                  (set-buffer-modified-p nil)
                  (should (equal '(:id "source" :status "dirty" :text nil)
                                 (gnosis-agent-content-source "source")))
                  (should-error (gnosis-test-content-plan changes) :type 'user-error)
                  (should-error (gnosis-agent-content-apply plan) :type 'user-error)
                  (should-not (buffer-modified-p)))
                (should (equal draft (buffer-string)))
                (should (buffer-modified-p)))
              (should (equal before (gnosis-agent-content-fetch ["101" "102"])))
              (erase-buffer)
              (insert text)
              (set-buffer-modified-p nil)
              (gnosis-agent-content-apply plan)
              (should (equal ["source"]
                             (plist-get (aref (plist-get (gnosis-agent-content-fetch ["101"]) :items) 0)
                                        :source-ids)))
              (gnosis-update-thema 101 "Question α" '("hint") '("answer") "" '("keep") nil))
            (set-buffer-modified-p nil))
          (should (equal text (with-temp-buffer (insert-file-contents file) (buffer-string))))))
      (should (equal evidence (gnosis-test-content-evidence)))
      (with-temp-buffer
        (setq-local gnosis-nodes--deleted-file (list file gnosis-db))
        (insert "Detached recovery draft")
        (should (equal "dirty" (plist-get (gnosis-agent-content-source "source") :status)))))))

(provide 'gnosis-test-campaign-content)
;;; gnosis-test-campaign-content.el ends here

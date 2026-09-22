;;; gnosis-test-ordered-groups.el --- Ordered study batches -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

(require 'gnosis-test-agent)

(defun gnosis-test-groups--fixture ()
  "Create overlapping tags, a suspended card and a not-due card."
  (dolist (id '(101 102 103 104 105))
    (gnosis-test--add-basic-thema (format "Question %d" id) "a" nil nil id))
  (gnosis--insert-into 'thema-tag
                      '([103 "amino_acids"] [101 "amino_acids"]
                        [103 "proteins"] [102 "proteins"]
                        [104 "proteins"] [105 "proteins"]))
  (gnosis-update 'scheduler-state '(= suspended 1) '(= thema-id 104))
  (gnosis-update 'scheduler-state `(= due-day ,(1+ (gnosis--today-int))) '(= thema-id 105)))

(ert-deftest gnosis-groups-tags-dispositions-and-boundaries ()
  (dolist (mode '(practice due))
    (gnosis-test-agent
      (gnosis-test-groups--fixture)
      (let* ((before (gnosis-test-agent-snapshot))
             (gnosis-new-themata-limit 0)
             (status (funcall (if (eq mode 'practice) #'gnosis-agent-start-practice-groups
                               #'gnosis-agent-start-review-groups)
                              :tags '("amino_acids" "unknown" "proteins") :limit 10))
             (selection (plist-get status :selection))
             (groups (plist-get selection :groups)))
        (should (equal (plist-get status :selected-ids)
                       (if (eq mode 'practice) [101 103 102 105] [101 103 102])))
        (should (equal (plist-get (aref groups 0) :selected-ids) [101 103]))
        (should (equal (plist-get (aref groups 1) :requested-ids) []))
        (should (equal (plist-get (aref groups 2) :overlap-ids) [103]))
        (should (equal (plist-get selection :excluded-ids)
                       (if (eq mode 'practice) [104] [104 105])))
        (should (equal (plist-get status :mode) (symbol-name mode)))
        (should (eq (plist-get status :schedule-updated) :false))
        (should (equal before (gnosis-test-agent-snapshot)))
        (should (stringp (json-serialize status :false-object :false :null-object nil)))))))

(ert-deftest gnosis-groups-explicit-order-overlap-limit-no-input-mutation ()
  (gnosis-test-agent
    (gnosis-test-groups--fixture)
    (let* ((input '((103 101 103 999) nil (101 105 102)))
           (copy (copy-tree input))
           (status (gnosis-agent-start-practice-groups :thema-groups input :limit 3))
           (groups (plist-get (plist-get status :selection) :groups)))
      (should (equal input copy))
      (should (equal (plist-get status :selected-ids) [103 101 105]))
      (should (equal (plist-get (aref groups 0) :requested-ids) [103 101 103 999]))
      (should (equal (plist-get (aref groups 0) :excluded-ids) [999]))
      (should (equal (plist-get (aref groups 2) :overlap-ids) [101]))
      (should (equal (plist-get (aref groups 2) :omitted-ids) [102])))))

(ert-deftest gnosis-groups-empty-invalid-and-stale-preserve-checkpoint ()
  (dolist (start '(gnosis-agent-start-practice-groups gnosis-agent-start-review-groups))
    (gnosis-test-agent
      (gnosis-test-groups--fixture)
      (gnosis-test-agent-start '(101))
      (gnosis-test-agent-grade nil)
      (let ((before (gnosis-review--session-target))
            (schedule (gnosis-test-agent-snapshot))
            (practice (gnosis-select '* 'practice-events))
            (launches (copy-sequence gnosis-agent--launches)))
        (dolist (args '((:tags ("proteins") :limit 0)
                        (:tags ("amino_acids") :thema-groups ((101)) :limit 1)
                        (:thema-groups (("101")) :limit 1)
                        (:thema-groups ((101 . 102)) :limit 1)
                        (:tags ("" ) :limit 1)))
          (should-error (apply start args) :type 'user-error))
        (let ((empty (funcall start :tags '("unmatched") :limit 2)))
          (should (equal (plist-get empty :status) "completed"))
          (should (equal (plist-get empty :selected-ids) [])))
        (should (equal before (gnosis-review--session-target)))
        (should (equal launches gnosis-agent--launches))
        (should (equal schedule (gnosis-test-agent-snapshot)))
        (should (equal practice (gnosis-select '* 'practice-events)))))))

(ert-deftest gnosis-groups-reopen-resume-native-acceptance-and-retries ()
  (dolist (mode '(practice due))
    (gnosis-test-agent
      (gnosis-test-groups--fixture)
      ;; Keep substantive prior scheduled evidence, on a nonselected item.
      (gnosis-scheduler-accept-review (gnosis-scheduler-event-id) 105 'success
                                      1780000000000000 (gnosis--today-int))
      (let* ((before (gnosis-test-agent-snapshot))
             (status (funcall (if (eq mode 'practice) #'gnosis-agent-start-practice-groups
                               #'gnosis-agent-start-review-groups)
                              :thema-groups '((103 101) (102)) :limit 3))
             (session (plist-get status :session-id))
             (selection (plist-get status :selection))
             (gnosis-practice-retry-distance 2))
        ;; Native runner stops after the first accepted failed answer.
        (cl-letf (((symbol-function 'gnosis--read-string-with-input-method) (lambda (&rest _) "wrong"))
                  ((symbol-function 'gnosis-review--read-action) (lambda (&rest _) ?q)))
          (gnosis-agent--launch (car gnosis-agent--launches)))
        (let ((remaining (plist-get (gnosis-agent-status session) :remaining-ids)))
          (should (equal remaining (if (eq mode 'practice) [101 103 102] [101 102 103])))
          (should (equal selection (plist-get (gnosis-agent-status session) :selection)))
          (gnosis-sqlite-close gnosis-db)
          (setq gnosis-db (gnosis-sqlite-open gnosis-test--db-file))
          (should (equal remaining (plist-get (gnosis-agent-status session) :remaining-ids))))
        ;; Public resume uses retained groups and queue, not current tags.
        (gnosis--delete 'thema-tag)
        (gnosis-agent-resume session)
        (cl-letf (((symbol-function 'gnosis--read-string-with-input-method) (lambda (&rest _) "a"))
                  ((symbol-function 'gnosis-review--read-action) (lambda (&rest _) ?n)))
          (gnosis-agent--launch (car gnosis-agent--launches)))
        (should (equal "completed" (plist-get (gnosis-agent-status session) :status)))
        (should (equal selection (plist-get (gnosis-agent-status session) :selection)))
        (if (eq mode 'practice)
            (progn
              (should (equal before (gnosis-test-agent-snapshot)))
              (should (equal '(103 101 103 102 103)
                             (gnosis-select 'thema-id 'practice-events nil t))))
          (should-not (equal before (gnosis-test-agent-snapshot)))
          (should-not (gnosis-select '* 'practice-events))
          (should (eq t (plist-get (gnosis-agent-status session) :schedule-updated)))
          (should-error (gnosis-agent-results session) :type 'user-error))))))

(provide 'gnosis-test-ordered-groups)
;;; gnosis-test-ordered-groups.el ends here

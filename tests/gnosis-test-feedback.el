;;; gnosis-test-feedback.el --- Post-answer popup contracts -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;;; Commentary:
;; Feedback is ordinary native input; selection never writes a grade itself.

;;; Code:

(require 'ert)
(require 'gnosis-review-test-support)

(ert-deftest gnosis-feedback-labels-are-cached-and-faced ()
  (with-temp-buffer
    (let* ((result '(:preview (:due-day 20260921)))
           (alternate '(:preview (:due-day 20260920)))
           (gnosis-review--feedback
            (list :success t :result result :alternate alternate)))
      (cl-letf (((symbol-function 'gnosis--ensure-db)
                 (lambda () (ert-fail "Rendering queried the database")))
                ((symbol-function 'gnosis-review--override-result)
                 (lambda (&rest _) (ert-fail "Rendering recomputed the preview"))))
        (should (equal (gnosis-review--feedback-next-label) "Next · 2026-09-21"))
        (dolist (success '(t nil))
          (setf (plist-get gnosis-review--feedback :success) success
                (plist-get gnosis-review--feedback :result) (if success result alternate)
                (plist-get gnosis-review--feedback :alternate) (if success alternate result))
          (let ((label (gnosis-review--feedback-override-label)))
            (should (equal label (if success
                                    "Override · Correct · 2026-09-21"
                                  "Override · Incorrect · 2026-09-20")))
            (should (eq (get-text-property (length "Override · ") 'face label)
                        (if success 'success 'error)))
            (should (eq (get-text-property (1- (length label)) 'face label)
                        'keymap-popup-value))))
        (setf (plist-get gnosis-review--feedback :result) '(:mode practice)
              (plist-get gnosis-review--feedback :alternate) '(:mode practice))
        (should (equal (gnosis-review--feedback-next-label) "Next · schedule unchanged"))
        (dolist (success '(t nil))
          (setf (plist-get gnosis-review--feedback :success) success)
          (let ((label (gnosis-review--feedback-override-label)))
            (should (equal label (if success
                                    "Override · Correct · schedule unchanged"
                                  "Override · Incorrect · schedule unchanged")))
            (should (eq (get-text-property (length "Override · ") 'face label)
                        (if success 'success 'error)))))))))

(ert-deftest gnosis-feedback-selection-is-depth-qualified-and-provisional ()
  (with-temp-buffer
    (dolist (command '(gnosis-review-feedback-next gnosis-review-feedback-override
                       gnosis-review-feedback-quit gnosis-review-feedback-edit
                       gnosis-review-feedback-source gnosis-review-feedback-flag
                       gnosis-review-feedback-suspend gnosis-review-feedback-delete))
      (should-error (call-interactively command) :type 'user-error))
    (let ((gnosis-review--feedback
           (list :buffer (current-buffer) :depth 1 :id 222 :result nil :choice nil))
          exited)
      (cl-letf (((symbol-function 'exit-recursive-edit) (lambda () (setq exited t)))
                ((symbol-function 'gnosis-review--accept)
                 (lambda (&rest _) (ert-fail "Selection accepted a grade"))))
        (should-error (gnosis-review-feedback-next) :type 'user-error)
        (should-not exited)
        (cl-letf (((symbol-function 'recursion-depth) (lambda () 1)))
          (dolist (entry '(("n" . ?n) ("o" . ?o) ("q" . ?q) ("e" . ?e)
                           ("v" . ?v) ("f" . ?f) ("s" . ?s) ("d" . ?d)))
            (setq exited nil)
            (call-interactively (keymap-lookup gnosis-review-feedback-mode-map (car entry)))
            (should exited)
            (should (eq (plist-get gnosis-review--feedback :choice) (cdr entry)))))))))

(ert-deftest gnosis-feedback-unwind-preserves-successor-popup-and-context ()
  (save-window-excursion
    (let ((owner (generate-new-buffer " *feedback owner*"))
          (other (generate-new-buffer " *feedback successor*"))
          (successor (list :successor t))
          (context (list :id 222 :success t :result '(:mode practice) :alternate nil)))
      (unwind-protect
          (with-current-buffer owner
            (gnosis-mode)
            (cl-letf (((symbol-function 'recursive-edit)
                       (lambda ()
                         (setq gnosis-review--feedback successor)
                         (switch-to-buffer other)
                         (keymap-popup gnosis-review-map)
                         (signal 'quit nil))))
              (should (eq 'cancelled
                          (condition-case nil (gnosis-review--read-action context)
                            (quit 'cancelled)))))
            (set-buffer owner)
            (should (eq gnosis-review--feedback successor))
            (should gnosis-review-feedback-mode)
            (should (keymap-popup--popup-buffer))
            (should (eq (keymap-popup--active-get (keymap-popup--popup-buffer) :keymap)
                        gnosis-review-map)))
        (keymap-popup-dismiss)
        (kill-buffer owner)
        (kill-buffer other)))))

(ert-deftest gnosis-feedback-preview-selection-retains-captured-facts ()
  (gnosis-test-with-db
    (gnosis-test-content--add "basic")
    (let* ((owner (gnosis-review--setup-buffer '(222) 'due))
           (before (gnosis-test-content--evidence)))
      (unwind-protect
          (with-current-buffer owner
            (let* ((pending (gnosis-review--pending-result 222 t))
                   (context (list :id 222 :success t :result pending :alternate nil)))
              (cl-letf (((symbol-function 'recursive-edit)
                         (lambda ()
                           (let ((alternate (plist-get context :alternate)))
                             (should (eq alternate (plist-get gnosis-review--feedback :alternate)))
                             (dolist (key '(:event-id :reviewed-at-us :review-day :thema-id))
                               (should (equal (plist-get pending key) (plist-get alternate key))))
                             (setf (plist-get gnosis-review--feedback :choice) ?o)))))
                (should (eq (gnosis-review--read-action context) ?o)))
              (should-not gnosis-review-feedback-mode)
              (should-not gnosis-review--feedback)
              (should-not (keymap-popup--popup-buffer))
              (cl-letf (((symbol-function 'gnosis-review--override-result)
                         (lambda (&rest _) (ert-fail "Override discarded the displayed preview"))))
                (should (eq (cdr (gnosis-review-action--override
                                  t 222 pending (plist-get context :alternate)))
                            (plist-get context :alternate))))
              (should (equal before (gnosis-test-content--evidence)))))
        (when (buffer-live-p owner) (kill-buffer owner))))))

(ert-deftest gnosis-feedback-display-callback-preserves-foreign-context ()
  (save-window-excursion
    (let ((owner (generate-new-buffer " *feedback display owner*"))
          (other (generate-new-buffer " *feedback display successor*"))
          (successor (list :successor t))
          (context (list :id 222 :success t :result '(:mode practice) :alternate nil)))
      (unwind-protect
          (with-current-buffer owner
            (gnosis-mode)
            (with-current-buffer other (setq gnosis-review--feedback successor))
            (let* ((backend (keymap-popup-backend-side-window))
                   (show (plist-get backend :show))
                   (keymap-popup-backend
                    (lambda ()
                      (plist-put (copy-sequence backend) :show
                                 (lambda (popup)
                                   (funcall show popup)
                                   (switch-to-buffer other))))))
              (cl-letf (((symbol-function 'recursive-edit)
                         (lambda () (ert-fail "Retired input started"))))
                (should-error (gnosis-review--read-action context) :type 'user-error)))
            (should (equal successor (buffer-local-value 'gnosis-review--feedback other)))
            (should-not (plist-member successor :popup))
            (should-not (keymap-popup--popup-buffer))
            (should-not (buffer-local-value 'gnosis-review-feedback-mode owner)))
        (keymap-popup-dismiss)
        (kill-buffer owner)
        (kill-buffer other)))))

(ert-deftest gnosis-feedback-disable-hook-preserves-successor-context ()
  (save-window-excursion
    (with-temp-buffer
      (gnosis-mode)
      (let* ((successor (list :successor t))
             (context (list :id 222 :success t :result '(:mode practice) :alternate nil))
             (gnosis-review-feedback-mode-hook
              (list (lambda ()
                      (unless gnosis-review-feedback-mode
                        (setq gnosis-review--feedback successor))))))
        (cl-letf (((symbol-function 'recursive-edit)
                   (lambda () (setf (plist-get gnosis-review--feedback :choice) ?n))))
          (should (eq (gnosis-review--read-action context) ?n)))
        (should (eq gnosis-review--feedback successor))
        (should-not (keymap-popup--popup-buffer))))))

(ert-deftest gnosis-feedback-source-reload-preserves-custom-bindings ()
  (let* ((map gnosis-review-feedback-mode-map)
         (help (keymap-lookup map "?"))
         (next (keymap-lookup map "n"))
         (source (file-name-with-extension (locate-library "gnosis-review") "el")))
    (unwind-protect
        (progn
          (keymap-set map "?" #'ignore)
          (keymap-set map "n" #'ignore)
          ;; This test deliberately exercises reevaluation, not test loading.
          (load source nil t t)
          (should (eq map gnosis-review-feedback-mode-map))
          (should (eq (keymap-lookup map "?") #'ignore))
          (should (eq (keymap-lookup map "n") #'ignore)))
      (keymap-set map "?" help)
      (keymap-set map "n" next))))

(provide 'gnosis-test-feedback)
;;; gnosis-test-feedback.el ends here

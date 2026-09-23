;;; gnosis-test-campaign-evaluation.el --- Cancellation ownership tests -*- lexical-binding: t; -*-

(require 'gnosis-test-agent-eval)

(defmacro gnosis-test-campaign-evaluation--with-review (mode &rest body)
  "Run BODY in a disposable evaluator review with MODE."
  (declare (indent 1))
  `(gnosis-test-with-db
     (let* ((id (gnosis-generate-id))
            (gnosis-review-buffer-name "*gnosis-campaign-evaluation*")
            (gnosis-agent-eval-timeout 600))
       (gnosis-add-thema-fields "agent-eval" "Explain α" nil
                                '("Mechanism and consequence") "Teaching" '("test")
                                0 nil nil id nil "Essential: mechanism.")
       (let ((review (gnosis-review--setup-buffer (list id) ,mode)))
         (unwind-protect
             (with-current-buffer review ,@body)
           (when (buffer-live-p review) (kill-buffer review)))))))

(defun gnosis-test-campaign-evaluation--retry (mode synchronous trigger cancellation)
  "Check MODE retry with SYNCHRONOUS completion, TRIGGER and CANCELLATION exit."
  (gnosis-test-campaign-evaluation--with-review mode
    (let* ((before (gnosis-test-eval--evidence))
           (calls 0) old-resolve new-resolve
           (new-result '(:verdict fail :explanation "Newest response failed"))
           (gnosis-agent-eval-function
            (lambda (_request resolve _reject)
              (cl-incf calls)
              (if (= calls 1)
                  (progn
                    (setq old-resolve resolve)
                    (let ((buffer (current-buffer)))
                      (lambda ()
                        (with-current-buffer buffer
                          (gnosis-test-eval--key "C-c C-k")
                          (erase-buffer)
                          (insert "Newest α\nresponse")
                          (gnosis-test-eval--key "C-c C-c"))
                        (pcase cancellation
                          ('error (error "Cancel transport failed"))
                          ('quit (signal 'quit nil))))))
                (setq new-resolve resolve)
                (when synchronous (funcall resolve new-result))
                #'ignore)))
           (result
            (cl-letf (((symbol-function 'recursive-edit)
                       (lambda ()
                         (insert "Old response")
                         (gnosis-test-eval--key "C-c C-c")
                         (pcase trigger
                           ('cancel (gnosis-test-eval--key "C-c C-k"))
                           ('resolve (funcall old-resolve
                                              '(:verdict pass :explanation "Obsolete")))
                           ('timeout
                            (let ((context gnosis-agent-eval--context))
                              (gnosis-agent-eval--settle
                               context (plist-get context :attempt) nil "Timed out"))))
                         (should (= calls 2))
                         (should (equal (buffer-string) "Newest α\nresponse"))
                         (should (equal before (gnosis-test-eval--evidence)))
                         (unless synchronous
                           (should (plist-get gnosis-agent-eval--context :attempt))
                           (should-not (plist-get gnosis-agent-eval--context :result))
                           (should buffer-read-only)
                           (should (string-match-p "Evaluating"
                                                   (overlay-get (plist-get gnosis-agent-eval--context :overlay)
                                                                'after-string)))
                           (funcall new-resolve new-result))
                         (funcall old-resolve '(:verdict pass :explanation "Late obsolete"))
                         (should (equal new-result (plist-get gnosis-agent-eval--context :result)))
                         (should buffer-read-only)
                         (should (string-match-p "Fail: Newest response failed"
                                                 (overlay-get (plist-get gnosis-agent-eval--context :overlay)
                                                              'after-string))))))
              (gnosis-review-agent-eval id))))
      (should-not (car result))
      (when-let* ((encounter (plist-get (cdr result) :encounter)))
        (should (equal (plist-get encounter :response)
                       '(:kind "text" :text "Newest α\nresponse"))))
      (cl-letf (((symbol-function 'gnosis-review--read-action) (lambda (&rest _) ?n)))
        (gnosis-review-actions (car result) id (cdr result)))
      (if (eq mode 'practice)
          (let ((saved (gnosis-get 'data 'practice-encounters)))
            (should (equal (plist-get saved :original-outcome) "failure"))
            (should (equal (plist-get saved :response)
                           '(:kind "text" :text "Newest α\nresponse")))
            (should (equal (plist-get (aref (plist-get saved :coaching) 0) :text)
                           "Newest response failed"))
            (should-not (gnosis-select '* 'review-events)))
        (should (equal (gnosis-select 'rating 'review-events) '((1))))))))

(ert-deftest gnosis-test-campaign-evaluation-reentrant-retry ()
  "Only the newest result survives cancellation, including error and quit."
  (dolist (mode '(practice due))
    (dolist (synchronous '(nil t))
      (dolist (trigger '(resolve cancel timeout))
        (dolist (cancellation '(nil error quit))
          (ert-info ((format "%S" (list mode synchronous trigger cancellation)))
            (gnosis-test-campaign-evaluation--retry mode synchronous trigger cancellation)))))))

(ert-deftest gnosis-test-campaign-evaluation-native-retry ()
  "Exercise retry and acceptance through real recursive input and bindings."
  (skip-unless (not noninteractive))
  (dolist (mode '(practice due))
    (dolist (synchronous '(nil t))
      (gnosis-test-campaign-evaluation--with-review mode
        (let* ((before (gnosis-test-eval--evidence))
               (calls 0) resolve retry failure timer
               (origin (current-buffer))
               ;; Deliberately exceed the former 50ms setup race.
               (gnosis-agent-eval-mode-hook
                (cons (lambda () (sleep-for 0.1)) gnosis-agent-eval-mode-hook))
               (gnosis-agent-eval-function
                (lambda (_request yes _no)
                  (cl-incf calls)
                  (if (= calls 1)
                      (progn
                        (setq resolve yes)
                        (let ((buffer (current-buffer)))
                          (lambda ()
                            (with-current-buffer buffer
                              (execute-kbd-macro (kbd "C-c C-k"))
                              (erase-buffer)
                              (insert "Newest native\nresponse α")
                              (execute-kbd-macro (kbd "C-c C-c"))))))
                    (setq retry yes)
                    (when synchronous
                      (funcall yes '(:verdict fail :explanation "Newest native fail")))
                    #'ignore)))
               (_input
                (setq timer
                      (run-at-time
                       0.01 0.01
                       (lambda ()
                         (let ((context (buffer-local-value
                                         'gnosis-agent-eval--review-context origin)))
                           ;; Rendering can yield.  Only the owned recursive reader
                           ;; may receive input, not a mode hook or display callback.
                           (when (and context
                                      (eq (current-buffer) (plist-get context :buffer))
                                      (eq (window-buffer (selected-window)) (current-buffer))
                                      (= (recursion-depth) (1+ (plist-get context :depth)))
                                      (eq gnosis-agent-eval--context context)
                                      (equal (plist-get context :id) id)
                                      (gnosis-agent-eval--valid-p context))
                             (cancel-timer timer)
                             (condition-case err
                                 (progn
                                   (should (eq major-mode 'gnosis-agent-eval-mode))
                                   (should (> (recursion-depth) 0))
                                   (insert "Original native response")
                                   (execute-kbd-macro (kbd "C-c C-c"))
                                   (funcall resolve '(:verdict pass :explanation "Obsolete native pass"))
                                   (unless synchronous
                                     (should-not (plist-get gnosis-agent-eval--context :result))
                                     (should (plist-get gnosis-agent-eval--context :attempt))
                                     (funcall retry '(:verdict fail :explanation "Newest native fail")))
                                   (funcall resolve '(:verdict pass :explanation "Late native pass"))
                                   (should (equal (plist-get gnosis-agent-eval--context :response)
                                                  "Newest native\nresponse α"))
                                   (should (eq (plist-get (plist-get gnosis-agent-eval--context :result)
                                                          :verdict) 'fail))
                                   (should (equal before (gnosis-test-eval--evidence)))
                                   (setq unread-command-events (listify-key-sequence (kbd "C-c C-c"))))
                               ((error quit)
                                (setq failure err unread-command-events
                                      (listify-key-sequence (kbd "C-g")))))))))))
               (result
                (unwind-protect
                    (condition-case err
                        (with-timeout (10 (ert-fail "Native response input timed out"))
                          (gnosis-review-agent-eval id))
                      ((error quit) (unless failure (setq failure err))))
                  (cancel-timer timer))))
          ;; C-g only unwinds the reader; retain the original timer failure.
          (when failure
            (if (memq 'quit (get (car failure) 'error-conditions))
                (ert-fail (list "Native input quit" failure))
              (signal (car failure) (cdr failure))))
          (should-not (car result))
          (should (= calls 2))
          (setq unread-command-events (listify-key-sequence (kbd "n")))
          (gnosis-review-actions (car result) id (cdr result))
          (if (eq mode 'practice)
              (let ((saved (gnosis-get 'data 'practice-encounters)))
                (should (equal (plist-get saved :original-outcome) "failure"))
                (should (equal (plist-get saved :response)
                               '(:kind "text" :text "Newest native\nresponse α")))
                (should (equal (seq-take before 3)
                               (seq-take (gnosis-test-eval--evidence) 3))))
            (should (equal (gnosis-select 'rating 'review-events) '((1))))))))))

(provide 'gnosis-test-campaign-evaluation)
;;; gnosis-test-campaign-evaluation.el ends here

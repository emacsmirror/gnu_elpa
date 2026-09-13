;;; gnosis-test-cloze-token.el --- Token cloze review -*- lexical-binding: t; -*-

;;; Commentary:
;; Clozes prefer a standalone occurrence when one exists, including a
;; digit that is not part of a decimal fraction, and otherwise keep the
;; legacy first-substring match so existing interior-only cards still
;; hide and re-save.  Review grades stay on disposable data.

;;; Code:
(require 'ert)
(require 'cl-lib)
(require 'gnosis-review)
(require 'gnosis-test-helpers)

(defconst gnosis-test-cloze-token--digit
  "See 122, 2A and the 2 items."
  "Synthetic keimenon whose answer 2 also occurs inside 122 and 2A.")

(defconst gnosis-test-cloze-token--letter
  "The analog has a receptor."
  "Synthetic keimenon whose answer a also occurs inside analog.")

(defconst gnosis-test-cloze-token--unicode
  "The αβ pair has α left."
  "Synthetic keimenon whose answer α also occurs inside αβ.")

(defmacro gnosis-test-cloze-token-study (&rest body)
  "Run BODY with a disposable review buffer and no monkeytype."
  (declare (indent 0) (debug t))
  `(gnosis-test-with-db
     (let ((buffers (buffer-list))
           (gnosis-review-buffer-name "*Gnosis Test Cloze Token*")
           (gnosis-monkeytype-enable nil)
           (gnosis-center-content nil)
           (gnosis-latex-preview nil)
           (register-alist nil))
       (save-window-excursion
         (unwind-protect (progn ,@body)
           (dolist (buf (seq-difference (buffer-list) buffers))
             (when (buffer-live-p buf)
               (with-current-buffer buf (set-buffer-modified-p nil))
               (kill-buffer buf))))))))

(defun gnosis-test-cloze-token--add (keimenon answers &optional hints)
  "Insert a cloze thema for KEIMENON and ANSWERS.  Return its id."
  (let ((hints (or hints (make-list (length answers) "")))
        (id (gnosis-generate-id)))
    (gnosis-add-thema-fields "cloze" keimenon hints answers "" '("test") 0 nil
                             nil id)
    id))

(defun gnosis-test-cloze-token--plain (buffer)
  "Return BUFFER text without properties."
  (with-current-buffer buffer
    (substring-no-properties (buffer-string))))

(defun gnosis-test-cloze-token--face-at (buffer token face)
  "Return non-nil if BUFFER's last character of TOKEN uses FACE."
  (with-current-buffer buffer
    (goto-char (point-min))
    (search-forward token)
    (eq face (get-text-property (1- (point)) 'face))))

(ert-deftest gnosis-test-cloze-token-replace-skips-interior-matches ()
  "Mask the standalone token, not a digit, letter, or letter inside a word."
  (let ((gnosis-latex-preview nil))
    (should (equal "See 122, 2A and the (...) items."
                   (substring-no-properties
                    (gnosis-cloze-create gnosis-test-cloze-token--digit
                                         '("2")))))
    (should (equal "The analog has (...) receptor."
                   (substring-no-properties
                    (gnosis-cloze-create gnosis-test-cloze-token--letter
                                         '("a")))))
    (should (equal "The αβ pair has (...) left."
                   (substring-no-properties
                    (gnosis-cloze-create gnosis-test-cloze-token--unicode
                                         '("α")))))
    (should (equal "See 122, 2A and the 2 items."
                   (substring-no-properties
                    (gnosis-cloze-create "See 122, 2A and the 2 items."
                                         '("mitochondria")))))))

(ert-deftest gnosis-test-cloze-token-replace-multiple-occurrences ()
  "Replace each cloze's first remaining token, including repeated 2."
  (let ((gnosis-latex-preview nil))
    (should (equal "See 122 then (...) plus (...) or (...)."
                   (substring-no-properties
                    (gnosis-cloze-create "See 122 then 2 plus 2 or α."
                                         '("2" "2" "α")))))
    (should (equal "The analog has (...) receptor and (...)."
                   (substring-no-properties
                    (gnosis-cloze-create "The analog has a receptor and b."
                                         '("a" "b")))))))

(ert-deftest gnosis-test-cloze-token-prefers-standalone-over-decimal-fraction ()
  "Mask a real standalone digit rather than a period or comma decimal fraction."
  (let ((gnosis-latex-preview nil))
    (should (equal "See 12.2 and the (...) items."
                   (substring-no-properties
                    (gnosis-cloze--replace "See 12.2 and the 2 items."
                                           '("2") "(...)"))))
    (should (equal "See 12,2 and the (...) items."
                   (substring-no-properties
                    (gnosis-cloze--replace "See 12,2 and the 2 items."
                                           '("2") "(...)"))))
    (should (equal "See 122, 2A and the (...) items."
                   (substring-no-properties
                    (gnosis-cloze--replace gnosis-test-cloze-token--digit
                                           '("2") "(...)"))))
    (should (equal "Count (...)."
                   (substring-no-properties
                    (gnosis-cloze--replace "Count 2." '("2") "(...)"))))
    (should (equal "See (...), plus 12,2 extra."
                   (substring-no-properties
                    (gnosis-cloze--replace "See 2, plus 12,2 extra."
                                           '("2") "(...)"))))
    (should (equal "See a_b and the (...) item."
                   (substring-no-properties
                    (gnosis-cloze--replace "See a_b and the a item."
                                           '("a") "(...)"))))))

(ert-deftest gnosis-test-cloze-token-falls-back-to-legacy-substring ()
  "Keep interior-only needles maskable, including the original multi-character case."
  (let ((gnosis-latex-preview nil))
    (should (equal "A trans(...)er."
                   (substring-no-properties
                    (gnosis-cloze--replace "A transporter." '("port") "(...)"))))
    (should (equal "See 1(...)2, 2A items."
                   (substring-no-properties
                    (gnosis-cloze--replace "See 122, 2A items."
                                           '("2") "(...)"))))
    (should (equal "The (...)nalog has receptors."
                   (substring-no-properties
                    (gnosis-cloze--replace "The analog has receptors."
                                           '("a") "(...)"))))
    (should (equal "The (...)β pair."
                   (substring-no-properties
                    (gnosis-cloze--replace "The αβ pair." '("α") "(...)"))))
    (should (equal "See (...)_b only."
                   (substring-no-properties
                    (gnosis-cloze--replace "See a_b only." '("a") "(...)"))))))

(ert-deftest gnosis-test-cloze-token-check-accepts-legacy-substring ()
  "Saving accepts a standalone match or the legacy interior substring."
  (should (gnosis-cloze-check gnosis-test-cloze-token--digit '("2")))
  (should (gnosis-cloze-check "See 122, 2A items." '("2")))
  (should (gnosis-cloze-check gnosis-test-cloze-token--letter '("a")))
  (should (gnosis-cloze-check "The analog has receptors." '("a")))
  (should (gnosis-cloze-check gnosis-test-cloze-token--unicode '("α")))
  (should (gnosis-cloze-check "The αβ pair." '("α")))
  (should (gnosis-cloze-check "A transporter." '("port")))
  (should (gnosis-cloze-check "The mitochondria is large." '("mitochondria")))
  (should-not (gnosis-cloze-check "See 122, 2A items." '("mitochondria"))))

(ert-deftest gnosis-test-cloze-token-highlight-follows-occurrence-rule ()
  "Correct and false faces use the same standalone-then-substring occurrence."
  (let ((decimal (gnosis-cloze-highlight "See 12.2 and the 2 items."
                                         '("2") 'gnosis-face-correct))
        (legacy (gnosis-cloze-highlight "A transporter."
                                        '("port") 'gnosis-face-false)))
    (with-temp-buffer
      (insert decimal)
      (goto-char (point-min))
      (search-forward "the 2")
      (should (eq 'gnosis-face-correct (get-text-property (1- (point)) 'face)))
      (goto-char (point-min))
      (search-forward "12.2")
      (should-not (eq 'gnosis-face-correct
                      (get-text-property (1- (point)) 'face))))
    (with-temp-buffer
      (insert legacy)
      (goto-char (point-min))
      (search-forward "port")
      (should (eq 'gnosis-face-false (get-text-property (1- (point)) 'face))))))

(ert-deftest gnosis-test-cloze-token-save-reopen-legacy-and-decimal ()
  "Public cloze save and native reopen keep interior-only and decimal cards."
  (dolist (case '(("A transporter." ("port") "A trans(...)er.")
                  ("See 12.2 and the 2 items." ("2")
                   "See 12.2 and the (...) items.")
                  ("See 122, 2A items." ("2") "See 1(...)2, 2A items.")))
    (gnosis-test-cloze-token-study
      (let ((gnosis-save-hook nil)
            (gnosis-review-editing-p nil)
            (gnosis-latex-preview nil)
            (keimenon (nth 0 case))
            (answers (nth 1 case))
            (masked (nth 2 case)))
        (gnosis-add-thema--cloze "NEW" "cloze" keimenon '("") answers
                                 "" '("test") 0 nil)
        (let ((id (gnosis-get 'id 'themata)))
          (should (equal keimenon
                         (gnosis-get 'keimenon 'themata `(= id ,id))))
          (should (equal answers
                         (gnosis-get 'answer 'themata `(= id ,id))))
          (should (equal masked
                         (substring-no-properties
                          (gnosis-cloze-create keimenon answers))))
          (gnosis-edit-thema id)
          (gnosis-save)
          (should (equal (list keimenon answers)
                         (list (gnosis-get 'keimenon 'themata `(= id ,id))
                               (gnosis-get 'answer 'themata `(= id ,id)))))
          (gnosis-edit-thema id)
          (should (string-match-p (regexp-quote keimenon) (buffer-string)))
          (gnosis-edit-quit))))))

(ert-deftest gnosis-test-cloze-token-display-question-and-feedback ()
  "Question blanks and result faces attach to the standalone token."
  (dolist (center '(nil t))
    (with-temp-buffer
      (let ((gnosis-review-buffer-name (buffer-name))
            (gnosis-review--running nil)
            (gnosis-center-content center)
            (gnosis-latex-preview nil)
            (fill-column 80))
        (gnosis-display-cloze-string gnosis-test-cloze-token--digit
                                     '("2") nil nil nil)
        (should (string-match-p "See 122, 2A and the (\\.\\.\\.) items\\."
                                (gnosis-test-cloze-token--plain (current-buffer))))
        (should-not (string-match-p "1(\\.\\.\\.)" (buffer-string)))
        (erase-buffer)
        (gnosis-display-cloze-string gnosis-test-cloze-token--digit
                                     nil nil '("2") nil)
        (should (gnosis-test-cloze-token--face-at
                 (current-buffer) "the 2" 'gnosis-face-correct))
        (goto-char (point-min))
        (search-forward "122")
        (should-not (eq 'gnosis-face-correct
                        (get-text-property (1- (point)) 'face)))
        (erase-buffer)
        (gnosis-display-cloze-string gnosis-test-cloze-token--digit
                                     nil nil nil '("2"))
        (should (gnosis-test-cloze-token--face-at
                 (current-buffer) "the 2" 'gnosis-face-false))
        (goto-char (point-min))
        (search-forward "122")
        (should-not (eq 'gnosis-face-false
                        (get-text-property (1- (point)) 'face)))))))

(defun gnosis-test-cloze-token--review (mode input)
  "Review the digit cloze in MODE with INPUT.  Return (SHOWN SUCCESS EVENTS)."
  (let* ((id (gnosis-test-cloze-token--add
              gnosis-test-cloze-token--digit '("2") '("count")))
         shown success events)
    (cl-letf (((symbol-function 'gnosis--read-string-with-input-method)
               (lambda (&rest _)
                 (setq shown (gnosis-test-cloze-token--plain
                              (get-buffer gnosis-review-buffer-name)))
                 input))
              ((symbol-function 'read-char-choice) (lambda (&rest _) ?n)))
      (let ((state (gnosis-review-loop (list id) mode)))
        (setq success (cdar (gnosis-review-state-outcomes state)))))
    (setq events (append (gnosis-select '* 'review-events)
                         (gnosis-select '* 'practice-events)))
    (list shown success events id)))

(ert-deftest gnosis-test-cloze-token-review-accepts-correct-and-wrong ()
  "Public review hides the token 2 and accepts typed 2 versus a wrong digit."
  (dolist (mode '(practice due))
    (dolist (case '(("2" t) ("9" nil)))
      (gnosis-test-cloze-token-study
        (pcase-let* ((`(,input ,expect) case)
                     (`(,shown ,success ,events ,id)
                      (gnosis-test-cloze-token--review mode input)))
          (should (string-match-p "See 122, 2A and the (count) items\\." shown))
          (should-not (string-match-p "1(\\.\\.\\.)\\|1(count)" shown))
          (should (eq expect success))
          (should events)
          (if (eq mode 'practice)
              (progn
                (should (gnosis-select '* 'practice-events
                                       `(= thema-id ,id)))
                (should-not (gnosis-select '* 'review-events)))
            (progn
              (should (gnosis-select '* 'review-events
                                     `(= thema-id ,id)))
              (should-not (gnosis-select '* 'practice-events)))))))))

(ert-deftest gnosis-test-cloze-token-review-cancel-writes-no-grade ()
  "Cancelling the cloze prompt writes no scheduled or practice grade."
  (dolist (mode '(practice due))
    (gnosis-test-cloze-token-study
      (let* ((id (gnosis-test-cloze-token--add
                  gnosis-test-cloze-token--digit '("2") '("count")))
             (before (list (gnosis-select '* 'review-events)
                           (gnosis-select '* 'practice-events)
                           (gnosis-select '* 'scheduler-state))))
        (cl-letf (((symbol-function 'gnosis--read-string-with-input-method)
                   (lambda (&rest _) (signal 'quit nil))))
          (should (eq 'cancelled
                      (condition-case nil
                          (gnosis-review-loop (list id) mode)
                        (quit 'cancelled)))))
        (should (equal before
                       (list (gnosis-select '* 'review-events)
                             (gnosis-select '* 'practice-events)
                             (gnosis-select '* 'scheduler-state))))
        (should (equal (list id)
                       (gnosis-review-state-remaining
                        (gnosis-review--read-session))))))))

(provide 'gnosis-test-cloze-token)
;;; gnosis-test-cloze-token.el ends here

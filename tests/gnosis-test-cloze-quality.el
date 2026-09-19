;;; gnosis-test-cloze-quality.el --- Occurrence feedback -*- lexical-binding: t; -*-

;;; Commentary:
;; Exercise native cloze evidence and duplicate occurrence ownership.

;;; Code:
(require 'ert)
(require 'gnosis-test-agent)
(require 'gnosis-export-import)

(defun gnosis-test-cloze-quality--create (inline)
  "Create through the native draft binding, reopen SQLite, return its ID."
  (let ((gnosis-save-hook nil) (gnosis-review-editing-p nil))
    (gnosis-add-thema "cloze" inline nil nil "Explanation" '("probe"))
    (with-current-buffer "*Gnosis NEW*"
      (should (eq (key-binding (kbd "C-c C-c")) 'gnosis-save))
      (call-interactively (key-binding (kbd "C-c C-c")))))
  (should-not (get-buffer "*Gnosis NEW*"))
  (gnosis-sqlite-close gnosis-db)
  (setq gnosis-db (gnosis-sqlite-open gnosis-test--db-file))
  (gnosis-db-init)
  (gnosis-get 'id 'themata '(= type "cloze")))

(defun gnosis-test-cloze-quality--faces (text tokens)
  "Read native semantic face at each successive token occurrence."
  (let ((pos 0))
    (mapcar (lambda (token)
              (let ((found (string-match (regexp-quote token) text pos)))
                (should found)
                (setq pos (+ found (length token)))
                (get-text-property found 'face text))) tokens)))

(defun gnosis-test-cloze-quality--evidence ()
  "Return scheduled and practice evidence, excluding session checkpoints."
  (cons (gnosis-test-agent-snapshot)
        (mapcar (lambda (table) (gnosis-select '* table))
                '(practice-events practice-encounters))))

(defun gnosis-test-cloze-quality--journey (inline inputs mode)
  "Use actual native creation, rendering, matching, acceptance and reopen.
Only text input, action choice and fixture timers are substituted."
  (gnosis-test-agent
    ;; Nonempty unrelated history makes preservation more than a zero-row check.
    (gnosis-test--add-basic-thema "Unrelated" "seed" nil nil 111 1)
    (gnosis-scheduler-accept-review (gnosis-scheduler-event-id) 111 'success
                                    1780000000000000 (gnosis--today-int))
    (let* ((id (gnosis-test-cloze-quality--create inline))
           (content (gnosis-review--content-thema id))
           (schedule-before (gnosis-test-agent-snapshot))
           (evidence-before (gnosis-test-cloze-quality--evidence))
           (display (symbol-function 'gnosis-display-cloze-string))
           (encounter (symbol-function 'gnosis-review--encounter))
           (remaining (copy-sequence inputs))
           (gnosis-latex-preview nil)
           (gnosis-string-difference 0)
           renders feedback state accepted response
           (accept (symbol-function 'gnosis-review-result)))
      (cl-letf (((symbol-function 'gnosis-review-result)
                 (lambda (id success result)
                   (setq accepted result)
                   (prog1 (funcall accept id success result)
                     (let ((once (gnosis-test-cloze-quality--evidence)))
                       (funcall accept id success result)
                       (should (equal once (gnosis-test-cloze-quality--evidence)))))))
                ((symbol-function 'gnosis-review--encounter)
                 (lambda (result row input hints tolerance)
                   (setq response input)
                   (funcall encounter result row input hints tolerance)))
                ((symbol-function 'gnosis-display-cloze-string)
                 (lambda (&rest args)
                   (let ((shown (apply display args)))
                     (push (list :arguments (copy-tree args t) :shown (copy-sequence shown)
                                 :text (with-current-buffer gnosis-review--display-buffer
                                         (buffer-string))) renders)
                     shown)))
                ((symbol-function 'gnosis--read-string-with-input-method)
                 (lambda (&rest _)
                   (should (equal evidence-before (gnosis-test-cloze-quality--evidence)))
                   (unless remaining (ert-fail "Unexpected extra input"))
                   (pop remaining)))
                ((symbol-function 'gnosis-review--read-action)
                 (lambda (&rest _)
                   (should (equal evidence-before (gnosis-test-cloze-quality--evidence)))
                   (setq feedback (buffer-string)) ?q)))
        (setq state (if (eq mode 'due)
                        (progn (call-interactively #'gnosis-review-due)
                               (gnosis-review--read-session))
                      (gnosis-review-loop (list id) 'practice))))
      (should-not remaining)
      (should (equal content (gnosis-review--content-thema id)))
      (let* ((table (if (eq mode 'practice) 'practice-events 'review-events))
             (where `(= thema-id ,id))
             (events (gnosis-select '* table where))
             (outcomes (gnosis-select 'rating table where t))
             (practice-result (when (eq mode 'practice)
                                (gnosis-agent-results (gnosis-review-state-session-id state)))))
        (should (= 1 (length events)))
        (if (eq mode 'practice)
            (should (equal schedule-before (gnosis-test-agent-snapshot)))
          (should-not (gnosis-select '* 'practice-events))
          (should (= 1 (gnosis-get 'reps 'scheduler-state `(= thema-id ,id)))))
        (gnosis-sqlite-close gnosis-db)
        (setq gnosis-db (gnosis-sqlite-open gnosis-test--db-file))
        (gnosis-db-init)
        (should (equal events (gnosis-select '* table where)))
        (when (eq mode 'practice)
          (should (equal (plist-get accepted :encounter)
                         (gnosis-get 'data 'practice-encounters
                                     `(= event-id ,(plist-get accepted :event-id))))))
        (when practice-result
          (should (equal practice-result
                         (gnosis-agent-results (gnosis-review-state-session-id state)))))
        (list :mode mode :content content :pending accepted :renders (nreverse renders)
              :feedback feedback :response response
              :outcomes outcomes :practice-result practice-result)))))


(ert-deftest gnosis-test-cloze-quality-occurrence-feedback ()
  "Every duplicate blank owns its mask and feedback through native acceptance."
  (dolist (mode '(due practice))
    (dolist (case '((("wrong") (gnosis-face-false gnosis-face-unanswered) 1)
                    (("α" "wrong") (gnosis-face-correct gnosis-face-false) 1)
                    (("α" "α") (gnosis-face-correct gnosis-face-correct) 3)))
      (let* ((trace (gnosis-test-cloze-quality--journey
                     "{{c1::α}} and {{c1::α}}" (car case) mode))
             (renders (plist-get trace :renders)))
        (should (equal (list (nth 2 case)) (plist-get trace :outcomes)))
        (should (equal (nth 1 case)
                       (gnosis-test-cloze-quality--faces
                        (plist-get trace :feedback) '("α" "α"))))
        (when (cdr (car case))
          (should (string-search "α and (...)" (plist-get (nth 1 renders) :text))))))))

(ert-deftest gnosis-test-cloze-quality-wrong-input-face ()
  "An incorrect literal response is false, independently of blank faces."
  (dolist (mode '(due practice))
    (let* ((literal "wrong α \\ literal")
           (trace (gnosis-test-cloze-quality--journey "{{c1::β}}" (list literal) mode))
           (text (plist-get trace :feedback))
           (pos (string-match (regexp-quote literal) text)))
      (should pos)
      (should (eq 'gnosis-face-false (get-text-property pos 'face text)))
      (should (equal '(1) (plist-get trace :outcomes)))
      (should (eq 'failure (plist-get (plist-get trace :pending) :outcome))))))

(ert-deftest gnosis-test-cloze-quality-shared-answers-and-hints ()
  "Aliased duplicate strings cannot change original index or hint ownership."
  (dolist (mode '(due practice))
    (dolist (hints '(("first") ("first" "middle" "last" "extra")))
      (gnosis-test-agent
        (let* ((a (copy-sequence "α")) (answers (list a "β" a))
               (id 444) (inputs '("β" "α" "α")) responses renders pending)
          (gnosis-add-thema-fields "cloze" "α β α" hints answers "" nil 0 nil nil id)
          (gnosis-sqlite-close gnosis-db)
          (setq gnosis-db (gnosis-sqlite-open gnosis-test--db-file))
          (gnosis-db-init)
          (let* ((stored (gnosis-get 'answer 'themata `(= id ,id)))
                 (schedule (gnosis-test-agent-snapshot))
                 (gnosis-review-buffer-name "*shared cloze*")
                 (buffer (gnosis-review--setup-buffer (list id) mode)))
            (should (eq (nth 0 stored) (nth 2 stored)))
            (unwind-protect
                (with-current-buffer buffer
                  (let ((gnosis-center-content nil))
                    (cl-letf (((symbol-function 'gnosis--read-string-with-input-method)
                               (lambda (&rest _)
                                 (push (buffer-string) renders)
                                 (let ((text (pop inputs)))
                                   (push text responses) text))))
                      (setq pending (cdr (gnosis-review-cloze id)))))
                  (should (equal '(gnosis-face-correct gnosis-face-correct gnosis-face-correct)
                                 (gnosis-test-cloze-quality--faces (buffer-string) '("α" "β" "α"))))
                  (should (string-search (if (nth 2 hints) "α β (last)" "α β (...)")
                                         (car renders)))
                  (gnosis-review-result id t pending)
                  (when (eq mode 'practice)
                    (let* ((encounter (gnosis-get 'data 'practice-encounters `(= event-id ,(plist-get pending :event-id))))
                           (input (plist-get (plist-get encounter :response) :inputs)))
                      (should (equal '(1 0 2) (mapcar (lambda (x) (plist-get x :matched-blank-index)) input)))
                      (should (equal '([0 1 2] [0 2] [2])
                                     (mapcar (lambda (x) (plist-get x :remaining-blank-indices)) input)))
                      (should (equal (vconcat (seq-take hints 3)) (plist-get encounter :hints-shown))))
                    (should (equal schedule (gnosis-test-agent-snapshot)))))
              (kill-buffer buffer))))))))

(ert-deftest gnosis-test-cloze-quality-occurrence-render-context ()
  "Projection retains default matching, literal hints and untouched properties."
  (let* ((text (concat (propertize "prefix " 'face 'bold) "xAx xax suffix"))
         (answers '("xax" "xax"))
         (hint "\\literal α")
         (expected (gnosis-cloze--render text answers '(1) (list nil hint)))
         (table (copy-case-table (standard-case-table))))
    (set-case-syntax-pair ?Q ?x table)
    (with-temp-buffer
      (setq-local case-fold-search nil)
      (let ((search-spaces-regexp "[[:space:]]+"))
        (with-case-table table
          (should (equal-including-properties expected
                                               (gnosis-cloze--render text answers '(1) (list nil hint)))))))
    (should (equal (list hint) (cdr expected)))
    (should (equal (format "prefix xax (%s) suffix" hint) (substring-no-properties (car expected))))
    (should (eq 'bold (get-text-property 0 'face (car expected))))))

(ert-deftest gnosis-test-cloze-quality-overlapping-candidates ()
  "Reject overlapping candidates without skipping a later disjoint blank."
  (dolist (answers '(("ab" "aba") ("αβ" "αβα")))
    (let ((text (apply #'concat answers)))
      (should (equal '(2 . 5)
                     (gnosis-cloze--occurrence text (cadr answers) nil '((0 . 2)))))
      ;; A later standalone occurrence still takes precedence over substrings.
      (should (equal '(6 . 9)
                     (gnosis-cloze--occurrence
                      (concat text " " (cadr answers)) (cadr answers) nil '((0 . 2)))))
      (should (equal "(...)(...)"
                     (substring-no-properties
                      (car (gnosis-cloze--render text answers '(0 1) nil))))))))

(ert-deftest gnosis-test-cloze-quality-adjacent-native-blanks ()
  "Native save/reopen masks adjacent blanks before recall and owns feedback."
  (dolist (answers '(("ab" "aba") ("αβ" "αβα")))
    (dolist (mode '(due practice))
      (dolist (case '(((nil) (nil) ([0 1])
                      (gnosis-face-false gnosis-face-unanswered) 1)
                     ((0 nil) (0 nil) ([0 1] [1])
                      (gnosis-face-correct gnosis-face-false) 1)
                     ((0 1) (0 1) ([0 1] [1])
                      (gnosis-face-correct gnosis-face-correct) 3)
                     ((1 0) (1 0) ([0 1] [0])
                      (gnosis-face-correct gnosis-face-correct) 3)))
        (let* ((inputs (mapcar (lambda (index) (if index (nth index answers) "wrong"))
                              (car case)))
               (trace (gnosis-test-cloze-quality--journey
                       (format "{{c1::%s::first}}{{c1::%s::second}}"
                               (car answers) (cadr answers)) inputs mode))
               (renders (plist-get trace :renders))
               (initial (plist-get (car renders) :text))
               (response (plist-get (plist-get trace :response) :inputs)))
          (should (string-search "(first)(second)" initial))
          (dolist (answer answers) (should-not (string-search answer initial)))
          (should (equal '("first" "second") (plist-get (car renders) :shown)))
          (should (equal '(gnosis-face-cloze gnosis-face-cloze)
                         (gnosis-test-cloze-quality--faces initial '("(first)" "(second)"))))
          (when (cdr inputs)
            (let* ((second (nth 1 renders))
                   (out-of-order (= (caar case) 1))
                   (expected (if out-of-order (concat "(first)" (cadr answers))
                               (concat (car answers) "(second)"))))
              (should (string-search expected (plist-get second :text)))
              (should (equal (list (if out-of-order "first" "second"))
                             (plist-get second :shown)))))
          (should (equal (nth 3 case)
                         (gnosis-test-cloze-quality--faces
                          (plist-get trace :feedback) answers)))
          (should (equal (list (nth 4 case)) (plist-get trace :outcomes)))
          (should (equal inputs (mapcar (lambda (input) (plist-get input :text)) response)))
          (should (equal (nth 1 case)
                         (mapcar (lambda (input) (plist-get input :matched-blank-index)) response)))
          (should (equal (nth 2 case)
                         (mapcar (lambda (input) (plist-get input :remaining-blank-indices)) response)))
          (when (eq mode 'practice)
            (let ((encounter (plist-get (plist-get trace :pending) :encounter)))
              (should (equal ["first" "second"] (plist-get encounter :hints-shown)))
              (should (equal (plist-get trace :response) (plist-get encounter :response))))))))))

(provide 'gnosis-test-cloze-quality)
;;; gnosis-test-cloze-quality.el ends here

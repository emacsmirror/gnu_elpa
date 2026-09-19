;;; gnosis-test-agent-eval.el --- Free-response review tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'gnosis-agent-eval)
(require 'gnosis-export-import)
(require 'gnosis-test-helpers)

(defmacro gnosis-test-eval--with-review (mode &rest body)
  "Run BODY with a disposable agent-eval thema ID and review MODE."
  (declare (indent 1))
  `(gnosis-test-with-db
     (let* ((id (gnosis-generate-id))
            (gnosis-review-buffer-name "*gnosis-eval-test*")
            (gnosis-review-centered nil)
            (gnosis-agent-eval-timeout 600))
       (gnosis-add-thema-fields "agent-eval" "Explain α" '("Optional hint")
                                '("Mechanism\nand consequence") "Teaching" '("test")
                                0 nil nil id nil "Essential: mechanism. Error: reverse causality.")
       (let ((review (gnosis-review--setup-buffer (list id) ,mode)))
         (unwind-protect
             (with-current-buffer review ,@body)
           (when (buffer-live-p review) (kill-buffer review)))))))

(defun gnosis-test-eval--evidence ()
  "Return complete scheduled and practice evidence from this test database."
  (mapcar (lambda (table) (gnosis-select '* table))
          '(scheduler-state scheduler-baseline review-events practice-events study-session)))

(defun gnosis-test-eval--key (key)
  "Invoke KEY through the response buffer's actual binding."
  (call-interactively (key-binding (kbd key))))

(defun gnosis-test-eval--key-face (text key)
  "Assert that KEY in TEXT uses the native key hint face."
  (let ((start (string-match (regexp-quote key) text)))
    (should start)
    (dotimes (offset (length key))
      (let ((face (get-text-property (+ start offset) 'face text)))
        (should (if (listp face) (memq 'help-key-binding face)
                  (eq face 'help-key-binding)))))))

(ert-deftest gnosis-test-eval-header-key-faces ()
  "The response header advertises the actual bindings with native key faces."
  (dolist (rebound '(nil t))
    (let ((gnosis-agent-eval-mode-map (copy-keymap gnosis-agent-eval-mode-map)))
      (when rebound
        (define-key gnosis-agent-eval-mode-map (kbd "C-c C-c") nil)
        (define-key gnosis-agent-eval-mode-map (kbd "C-c C-s")
                    #'gnosis-agent-eval-submit))
      (with-temp-buffer
        (gnosis-agent-eval-mode)
        (gnosis-test-eval--key-face header-line-format (if rebound "C-c C-s" "C-c C-c"))
        (gnosis-test-eval--key-face header-line-format "C-c C-k")
        (gnosis-test-eval--key-face header-line-format "C-g")))))

(ert-deftest gnosis-test-eval-rendered-header-key-faces ()
  "Native header rendering preserves literal bindings and their key faces."
  ;; Batch Emacs returns an empty string from `format-mode-line'.
  (skip-unless (not noninteractive))
  (dolist (key '("C-c C-c" "C-c C-s" "C-c %"))
    (let ((gnosis-agent-eval-mode-map (copy-keymap gnosis-agent-eval-mode-map)))
      (define-key gnosis-agent-eval-mode-map (kbd "C-c C-c") nil)
      (define-key gnosis-agent-eval-mode-map (kbd key) #'gnosis-agent-eval-submit)
      (with-temp-buffer
        (gnosis-agent-eval-mode)
        (should (eq (key-binding (kbd key)) #'gnosis-agent-eval-submit))
        (let ((header (format-mode-line header-line-format nil nil (current-buffer))))
          (should (equal (substring-no-properties header)
                         (concat " Response  " key " Evaluate/continue"
                                 "  C-c C-k Cancel evaluation  C-g Quit")))
          (gnosis-test-eval--key-face header key)
          (gnosis-test-eval--key-face header "C-c C-k")
          (gnosis-test-eval--key-face header "C-g"))))))

(ert-deftest gnosis-test-eval-feedback-key-faces-and-literal-content ()
  "Pending and settled hints keep faces without interpreting learner content."
  (gnosis-test-eval--with-review 'practice
    (let* ((before (gnosis-test-eval--evidence))
           (literal "Ελληνικά 🧠\nC-c C-c \\[gnosis-agent-eval-submit] `literal' 100%")
           resolve reject
           (gnosis-agent-eval-function
            (lambda (_request yes no) (setq resolve yes reject no) #'ignore)))
      (cl-letf (((symbol-function 'recursive-edit)
                 (lambda ()
                   (insert literal)
                   (dolist (outcome '(cancel pass fail ungradable rejection))
                     (gnosis-test-eval--key "C-c C-c")
                     (gnosis-test-eval--key-face
                      (overlay-get (plist-get gnosis-agent-eval--context :overlay) 'after-string)
                      "C-c C-k")
                     (pcase outcome
                       ('cancel (gnosis-test-eval--key "C-c C-k"))
                       ('rejection (with-temp-buffer (funcall reject literal)))
                       (_ (with-temp-buffer
                            (funcall resolve (list :verdict outcome :explanation literal)))))
                     (let* ((display (overlay-get (plist-get gnosis-agent-eval--context :overlay)
                                                  'after-string))
                            (face (pcase outcome ('pass 'success) ('fail 'error) (_ 'warning)))
                            (instruction (substring display (1+ (string-match "\n[^\n]*\n\\'" display)))))
                       (should (eq (get-text-property 2 'face display) face))
                       (gnosis-test-eval--key-face instruction "C-c C-c")
                       (when (memq outcome '(ungradable rejection))
                         (gnosis-test-eval--key-face instruction "C-g"))
                       (unless (eq outcome 'cancel)
                         (let ((start (string-match (regexp-quote literal) display)))
                           (should start)
                           (should (equal (substring-no-properties display start (+ start (length literal)))
                                          literal))
                           (should (eq (get-text-property start 'face display) face))
                           (should (eq (get-text-property (+ start (string-match "C-c C-c" literal))
                                                         'face display) face)))))
                     (should (equal (buffer-string) literal))
                     (should (equal before (gnosis-test-eval--evidence)))
                     ;; Permit the next request after a provisional binary verdict.
                     (when (memq outcome '(pass fail))
                       (gnosis-test-eval--key "C-c C-k")))
                   (gnosis-test-eval--key "C-g"))))
        (should-error (gnosis-review-agent-eval id) :type 'user-error))
      (should (equal before (gnosis-test-eval--evidence))))))

(ert-deftest gnosis-test-eval-settled-hints-respect-rebinding ()
  "Substituted bindings are literal text, not format directives."
  (dolist (key '("C-c C-c" "C-c C-s" "C-c %" "%"))
    (let ((gnosis-agent-eval-mode-map (copy-keymap gnosis-agent-eval-mode-map)))
      (define-key gnosis-agent-eval-mode-map (kbd "C-c C-c") nil)
      (define-key gnosis-agent-eval-mode-map (kbd key) #'gnosis-agent-eval-submit)
      (gnosis-test-eval--with-review 'practice
        (cl-letf (((symbol-function 'recursive-edit)
                   (lambda ()
                     (insert "Answer")
                     (dolist (verdict '(ungradable pass))
                       (let ((gnosis-agent-eval-function
                              (lambda (_request resolve _reject)
                                (funcall resolve (list :verdict verdict :explanation "Feedback"))
                                #'ignore)))
                         (gnosis-test-eval--key key))
                       (let ((display (overlay-get (plist-get gnosis-agent-eval--context :overlay)
                                                    'after-string)))
                         (should (string-match-p (if (eq verdict 'pass)
                                                     "Pass: Feedback" "Not graded: Feedback")
                                                 display))
                         (gnosis-test-eval--key-face display key))))))
          (should (car (gnosis-review-agent-eval id))))))))

(ert-deftest gnosis-test-eval-acceptance-and-override ()
  "Evaluation stays pending until native actions accept, in both study modes."
  (dolist (mode '(due practice))
    (gnosis-test-eval--with-review mode
      (let* ((before (gnosis-test-eval--evidence))
             (cancelled 0) request resolve reject
             (gnosis-agent-eval-function
              (lambda (req yes no)
                (setq request req resolve yes reject no)
                (lambda () (cl-incf cancelled))))
             (result
              (cl-letf (((symbol-function 'recursive-edit)
                         (lambda ()
                           (should (eq major-mode 'gnosis-agent-eval-mode))
                           (insert "My mechanism\nand consequence α")
                           (gnosis-test-eval--key "C-c C-c")
                           (should buffer-read-only)
                           (should-error (gnosis-test-eval--key "C-c C-c") :type 'user-error)
                           (should (equal before (gnosis-test-eval--evidence)))
                           (with-temp-buffer
                             (funcall resolve '(:verdict fail :explanation "Missing mechanism; no incorrect assertion. Add mechanism.")))
                           (should buffer-read-only)
                           (should (equal before (gnosis-test-eval--evidence))))))
                (gnosis-review-agent-eval id))))
        (should (= cancelled 1))
        (should (equal request '(:question "Explain α" :reference-answer "Mechanism\nand consequence"
                                :rubric "Essential: mechanism. Error: reverse causality."
                                :response "My mechanism\nand consequence α")))
        (should-not (car result))
        (should (equal before (gnosis-test-eval--evidence)))
        ;; Existing override preserves the captured content, including rubric.
        (let ((overridden (gnosis-review--override-result (cdr result) t)))
          (should (equal (plist-get overridden :content) (plist-get (cdr result) :content)))
          (cl-letf (((symbol-function 'gnosis-review--read-action) (lambda (&rest _) ?n)))
            (gnosis-review-actions t id overridden)))
        (should (= 1 (length (gnosis-select '* (if (eq mode 'practice) 'practice-events 'review-events)))))
        (when (eq mode 'practice)
          (let ((encounter (gnosis-get 'data 'practice-encounters)))
            (should (equal (plist-get encounter :response)
                           '(:kind "text" :text "My mechanism\nand consequence α")))
            (should (equal (plist-get encounter :match-rule)
                           '(:kind "agent-eval"
                             :rubric "Essential: mechanism. Error: reverse causality.")))
            (should (equal (plist-get encounter :coaching)
                           [(:kind "agent-eval" :verdict "fail"
                             :text "Missing mechanism; no incorrect assertion. Add mechanism.")]))
            (should (equal (plist-get encounter :original-outcome) "failure"))
            (should (equal (plist-get encounter :hints-shown) ["Optional hint"])))
          (should (equal (car before) (gnosis-select '* 'scheduler-state)))
          (should-not (gnosis-select '* 'review-events)))
        (let ((after (gnosis-test-eval--evidence)))
          (funcall resolve '(:verdict pass :explanation "Late"))
          (funcall reject "Late failure")
          (should (equal after (gnosis-test-eval--evidence))))))))

(ert-deftest gnosis-test-eval-narrowed-response-cancel-retry-and-accept ()
  "Narrowing never truncates evaluation or accepted evidence in either mode."
  (dolist (mode '(due practice))
    (gnosis-test-eval--with-review mode
      (let* ((before (gnosis-test-eval--evidence))
             (response "Context α\nMechanism is correct.\nBut consequence is wrong.")
             (cancelled 0) request resolve reject stale
             (gnosis-agent-eval-function
              (lambda (req yes no)
                (setq request req resolve yes reject no)
                (lambda () (cl-incf cancelled))))
             (result
              (cl-letf (((symbol-function 'recursive-edit)
                         (lambda ()
                           (insert response)
                           (goto-char (point-min))
                           (forward-line 1)
                           (narrow-to-region (point) (line-end-position))
                           (let ((bounds (cons (point-min) (point-max))))
                             (dolist (action '(cancel reject accept))
                               (gnosis-test-eval--key "C-c C-c")
                               (should (equal (plist-get request :response) response))
                               (should (equal bounds (cons (point-min) (point-max))))
                               (when stale
                                 (funcall stale '(:verdict pass :explanation "Stale"))
                                 (should-not (plist-get gnosis-agent-eval--context :result)))
                               (pcase action
                                 ('cancel
                                  (setq stale resolve)
                                  (gnosis-test-eval--key "C-c C-k"))
                                 ('reject (funcall reject "Offline"))
                                 ('accept
                                  (funcall resolve '(:verdict fail :explanation "Incorrect consequence; reverse its direction."))))
                               (should (eq (not buffer-read-only) (not (eq action 'accept))))
                               (should (buffer-narrowed-p))
                               (should (equal bounds (cons (point-min) (point-max))))
                               (should (equal (save-restriction (widen) (buffer-string)) response))
                               (should (equal before (gnosis-test-eval--evidence))))))))
                (gnosis-review-agent-eval id))))
        (should (= cancelled 3))
        (should-not (car result))
        (should (equal before (gnosis-test-eval--evidence)))
        (cl-letf (((symbol-function 'gnosis-review--read-action) (lambda (&rest _) ?n)))
          (gnosis-review-actions (car result) id (cdr result)))
        (should (= 1 (length (gnosis-select '* (if (eq mode 'practice) 'practice-events 'review-events)))))
        (if (eq mode 'practice)
            (progn
              (should (equal (seq-take before 3) (seq-take (gnosis-test-eval--evidence) 3)))
              (should (equal (plist-get (gnosis-get 'data 'practice-encounters) :response)
                             (list :kind "text" :text response))))
          (should-not (gnosis-select '* 'practice-events))
          (should-not (equal (car before) (gnosis-select '* 'scheduler-state))))))))

(ert-deftest gnosis-test-eval-import-without-extras ()
  "Absent optional extras preserve rubric and normal study acceptance."
  (gnosis-test-with-db
    (let* ((id (gnosis-generate-id))
           (file (expand-file-name "without-extras.db" gnosis-dir))
           (rubric "Essential: mechanism α.\nError: reverse causality.")
           (gnosis-review-buffer-name "*gnosis-eval-import-test*")
           (gnosis-review-centered nil))
      (gnosis-add-thema-fields
       "agent-eval" "Explain causality" nil '("Mechanism\nand consequence")
       "Teaching" '("test") 0 nil nil id nil rubric)
      (gnosis-export-db file)
      (let ((source (sqlite-open file)))
        (unwind-protect (sqlite-execute source "DELETE FROM extras")
          (sqlite-close source)))
      (dolist (existing '(nil t))
        (dolist (mode '(due practice))
          (gnosis-test-with-db
            (when existing
              (gnosis-add-thema-fields
               "agent-eval" "Old question" nil '("Old reference")
               "Old teaching" '("test") 0 nil nil id nil "Old rubric"))
            (let ((diff (gnosis-import--diff file)))
              (should (equal (mapcar #'car (nth (if existing 1 0) diff))
                             (list id)))
              (gnosis-import--apply-changes
               file (unless existing (list id)) (when existing (list id))
               (nth 2 diff) (nth 3 diff)))
            (gnosis-db--check-schema gnosis-db 11)
            (should-not (gnosis-select '* 'extras `(= id ,id)))
            (should (equal rubric (gnosis-get 'rubric 'themata `(= id ,id))))
            (let ((review (gnosis-review--setup-buffer (list id) mode)))
              (unwind-protect
                  (with-current-buffer review
                    (let* ((before (gnosis-test-eval--evidence))
                           request resolve
                           (gnosis-agent-eval-function
                            (lambda (req yes _no)
                              (setq request req resolve yes)
                              #'ignore))
                           (result
                            (cl-letf (((symbol-function 'recursive-edit)
                                       (lambda ()
                                         (insert "Mechanism\nand consequence α")
                                         (gnosis-test-eval--key "C-c C-c")
                                         (should (equal before (gnosis-test-eval--evidence)))
                                         (funcall resolve '(:verdict pass :explanation "Essentials covered"))
                                         (should (equal before (gnosis-test-eval--evidence))))))
                              (gnosis-review-agent-eval id))))
                      (should (car result))
                      (should (equal request
                                     (list :question "Explain causality"
                                           :reference-answer "Mechanism\nand consequence"
                                           :rubric rubric
                                           :response "Mechanism\nand consequence α")))
                      (should (equal (gnosis-review--content-thema id)
                                     (list (list "agent-eval" "Explain causality" nil
                                                 '("Mechanism\nand consequence")
                                                 nil nil nil rubric))))
                      (should (equal before (gnosis-test-eval--evidence)))
                      (cl-letf (((symbol-function 'gnosis-review--read-action) (lambda (&rest _) ?n)))
                        (gnosis-review-actions (car result) id (cdr result)))
                      (if (eq mode 'practice)
                          (progn
                            (should (equal (seq-take before 3)
                                           (seq-take (gnosis-test-eval--evidence) 3)))
                            (should (= 1 (length (gnosis-select '* 'practice-events)))))
                        (should (= 1 (length (gnosis-select '* 'review-events))))
                        (should-not (equal (car before) (gnosis-select '* 'scheduler-state)))
                        (should-not (gnosis-select '* 'practice-events)))))
                (when (buffer-live-p review) (kill-buffer review))))))))))

(ert-deftest gnosis-test-eval-ungradable-retry-and-late-callback ()
  "Every no-grade outcome preserves multiline input and permits retry."
  (gnosis-test-eval--with-review 'practice
    (let* ((before (gnosis-test-eval--evidence)) resolve reject old-resolve
          (cancels 0)
          (gnosis-agent-eval-function
           (lambda (_request yes no)
             (setq resolve yes reject no)
             (lambda ()
               (cl-incf cancels)
               (funcall yes '(:verdict pass :explanation "Reentrant cancellation callback"))))))
      (cl-letf (((symbol-function 'recursive-edit)
                 (lambda ()
                   (insert "Retained\nresponse")
                   (dolist (result '(nil (:verdict unknown :explanation "?")
                                    (:verdict fail :explanation " ")
                                    (:verdict pass :explanation "ok" :extra t)
                                    (:verdict fail :verdict pass)
                                    (:verdict ungradable :explanation "Ambiguous reference")))
                     (gnosis-test-eval--key "C-c C-c")
                     (funcall resolve result)
                     (should-not buffer-read-only)
                     (should-not (plist-get gnosis-agent-eval--context :result))
                     (should (equal (buffer-string) "Retained\nresponse"))
                     (should (equal before (gnosis-test-eval--evidence))))
                   (gnosis-test-eval--key "C-c C-c")
                   (funcall reject "Offline")
                   (should-not buffer-read-only)
                   (gnosis-test-eval--key "C-c C-c")
                   (setq old-resolve resolve)
                   (gnosis-test-eval--key "C-c C-k")
                   (gnosis-test-eval--key "C-c C-c")
                   (funcall old-resolve '(:verdict pass :explanation "Stale"))
                   (should-not (plist-get gnosis-agent-eval--context :result))
                   (let ((context gnosis-agent-eval--context))
                     (gnosis-agent-eval--settle context (plist-get context :attempt) nil "Evaluation timed out"))
                   (should-not buffer-read-only)
                   (gnosis-test-eval--key "C-c C-c")
                   (funcall resolve '(:verdict pass :explanation "Essentials covered")))))
        (should (car (gnosis-review-agent-eval id))))
      (should (= cancels 10))
      (should (equal before (gnosis-test-eval--evidence))))))

(ert-deftest gnosis-test-eval-reentrant-and-synchronous-completion ()
  "A synchronous callback cannot leave its cancellation handle live."
  (gnosis-test-eval--with-review 'due
    (let* ((cancelled 0)
          (gnosis-agent-eval-function
           (lambda (_request resolve _reject)
             (funcall resolve '(:verdict pass :explanation "Correct"))
             (lambda () (cl-incf cancelled)))))
      (cl-letf (((symbol-function 'recursive-edit)
                 (lambda () (insert "Answer") (gnosis-test-eval--key "C-c C-c"))))
        (should (car (gnosis-review-agent-eval id))))
      (should (= cancelled 1))
      (should-not (gnosis-select '* 'review-events)))))

(ert-deftest gnosis-test-eval-stale-owners ()
  "Database, content, session, buffer and file changes retire evaluation authority."
  (dolist (change '(database content session response-mode response-file review-file))
    (gnosis-test-eval--with-review 'due
      (let* ((before (gnosis-test-eval--evidence)) resolve context
            (gnosis-agent-eval-function
             (lambda (_request yes _no) (setq resolve yes) #'ignore)))
        (cl-letf (((symbol-function 'recursive-edit)
                   (lambda ()
                     (insert "Retained answer")
                     (gnosis-test-eval--key "C-c C-c")
                     (setq context gnosis-agent-eval--context)
                     (pcase change
                       ('database
                        (let ((gnosis-db (gnosis-sqlite-open (expand-file-name "replacement.db" gnosis-dir))))
                          (unwind-protect (funcall resolve '(:verdict pass :explanation "Stale"))
                            (gnosis-sqlite-close gnosis-db))))
                       ('content (gnosis-update 'themata '(= rubric "Changed") `(= id ,id)))
                       ('session (with-current-buffer review (setq gnosis-review--state nil)))
                       ('response-mode (fundamental-mode))
                       ('response-file (set-visited-file-name (expand-file-name "response" gnosis-dir))
                                       (set-visited-file-name nil))
                       ('review-file (with-current-buffer review
                                       (set-visited-file-name (expand-file-name "review" gnosis-dir))
                                       (set-visited-file-name nil))))
                     (unless (eq change 'database)
                       (funcall resolve '(:verdict pass :explanation "Stale")))
                     (should-not (plist-get context :result))
                     ;; Database rebinding is temporary but its callback was discarded.
                     (when (eq change 'database)
                       (setf (plist-get context :retired) t)))))
          (should-error (gnosis-review-agent-eval id) :type 'user-error))
        (should (equal before (gnosis-test-eval--evidence)))
        (when (buffer-live-p (plist-get context :buffer))
          (with-current-buffer (plist-get context :buffer)
            (set-buffer-modified-p nil) (kill-buffer)))))))

(ert-deftest gnosis-test-eval-cancel-quit-no-grade ()
  (gnosis-test-eval--with-review 'due
    (let* ((before (gnosis-test-eval--evidence)) resolve
          (gnosis-agent-eval-function
           (lambda (_request yes _no) (setq resolve yes) #'ignore)))
      (cl-letf (((symbol-function 'recursive-edit)
                 (lambda ()
                   (insert "Unaccepted")
                   (gnosis-test-eval--key "C-c C-c")
                   (gnosis-test-eval--key "C-g")
                   (funcall resolve '(:verdict pass :explanation "Late")))))
        (should-error (gnosis-review-agent-eval id) :type 'user-error))
      (should (equal before (gnosis-test-eval--evidence))))))

(ert-deftest gnosis-test-eval-real-timeout-and-transport-errors ()
  "A real timer, transport error and invalid handle never produce a grade."
  (gnosis-test-eval--with-review 'due
    (let ((gnosis-agent-eval-timeout 0.01)
          (gnosis-agent-eval-function (lambda (&rest _) #'ignore)))
      (cl-letf (((symbol-function 'recursive-edit)
                 (lambda ()
                   (insert "Preserved after timeout")
                   (gnosis-test-eval--key "C-c C-c")
                   (cl-loop repeat 50 while (plist-get gnosis-agent-eval--context :attempt)
                            do (accept-process-output nil 0.01))
                   (should-not (plist-get gnosis-agent-eval--context :attempt))
                   (should-not buffer-read-only)
                   (dolist (evaluator (list (lambda (&rest _) (error "Offline"))
                                           (lambda (&rest _) nil)))
                     (let ((gnosis-agent-eval-function evaluator))
                       (gnosis-test-eval--key "C-c C-c"))
                     (should-not buffer-read-only)
                     (should-not (plist-get gnosis-agent-eval--context :result)))
                   (should (equal (buffer-string) "Preserved after timeout"))
                   (gnosis-test-eval--key "C-g"))))
        (should-error (gnosis-review-agent-eval id) :type 'user-error))
      (should-not (gnosis-select '* 'review-events)))))

(ert-deftest gnosis-test-eval-rubric-drift-before-acceptance ()
  "Changing the rubric after evaluation invalidates acceptance and overrides."
  (gnosis-test-eval--with-review 'due
    (let* ((before (gnosis-test-eval--evidence))
           (gnosis-agent-eval-function
            (lambda (_request resolve _reject)
              (funcall resolve '(:verdict pass :explanation "Covered")) #'ignore))
           (result (cl-letf (((symbol-function 'recursive-edit)
                             (lambda () (insert "Response") (gnosis-test-eval--key "C-c C-c"))))
                     (gnosis-review-agent-eval id))))
      (gnosis-update 'themata '(= rubric "Changed") `(= id ,id))
      (should-error (gnosis-review-result id t (cdr result)) :type 'gnosis-review-content-changed)
      (should-error (gnosis-review-result id nil (gnosis-review--override-result (cdr result) nil))
                    :type 'gnosis-review-content-changed)
      (should (equal before (gnosis-test-eval--evidence))))))

(ert-deftest gnosis-test-eval-setup-callbacks-refuse-before-reader ()
  "Mode and window callbacks cannot lend a successor to native response input."
  (dolist (mode '(due practice))
    (dolist (boundary '(response-mode response-file response-window review-window))
      (gnosis-test-eval--with-review mode
        (let* ((before (gnosis-test-eval--evidence))
               successor entered
               (repurpose
                (lambda ()
                  (setq successor (current-buffer))
                  (set-visited-file-name (expand-file-name "successor" gnosis-dir))
                  (set-visited-file-name nil)
                  (let ((inhibit-read-only t)) (erase-buffer) (insert "Successor draft"))))
               (gnosis-agent-eval-mode-hook
                (when (memq boundary '(response-mode response-file))
                  (list (lambda ()
                          (when (eq boundary 'response-mode) (fundamental-mode))
                          (funcall repurpose)))))
               (pop (symbol-function 'pop-to-buffer)))
          (unwind-protect
              (cl-letf (((symbol-function 'pop-to-buffer)
                         (lambda (buffer &rest args)
                           (prog1 (apply pop buffer args)
                             (when (memq boundary '(response-window review-window))
                               (with-current-buffer (if (eq boundary 'review-window) review buffer)
                                 (funcall repurpose))))))
                        ((symbol-function 'recursive-edit)
                         (lambda () (setq entered t) (error "Entered stale reader"))))
                (should-error (gnosis-review-agent-eval id))
                (should-not entered)
                (should (buffer-live-p successor))
                (should (equal "Successor draft" (with-current-buffer successor (buffer-string))))
                (should (equal before (gnosis-test-eval--evidence))))
            (when (and (buffer-live-p successor) (not (eq successor review)))
              (with-current-buffer successor (set-buffer-modified-p nil))
              (kill-buffer successor))))))))

(ert-deftest gnosis-test-eval-cancellation-callback-preserves-successor ()
  "Cancellation can repurpose the response before settlement resumes."
  (gnosis-test-eval--with-review 'practice
    (let* ((before (gnosis-test-eval--evidence)) successor resolve
           (gnosis-agent-eval-function
            (lambda (_request yes _no)
              (setq resolve yes)
              (let ((buffer (current-buffer)))
                (lambda ()
                  (with-current-buffer buffer
                    (setq successor buffer)
                    (fundamental-mode)
                    (setq buffer-read-only nil)
                    (erase-buffer)
                    (insert "Successor draft")))))))
      (unwind-protect
          (cl-letf (((symbol-function 'recursive-edit)
                     (lambda ()
                       (insert "Original answer")
                       (gnosis-test-eval--key "C-c C-c")
                       (funcall resolve '(:verdict pass :explanation "Old verdict")))))
            (should-error (gnosis-review-agent-eval id))
            (should (buffer-live-p successor))
            (with-current-buffer successor
              (should-not buffer-read-only)
              (should (equal "Successor draft" (buffer-string))))
            (should (equal before (gnosis-test-eval--evidence))))
        (when (buffer-live-p successor)
          (with-current-buffer successor (set-buffer-modified-p nil))
          (kill-buffer successor))))))

(ert-deftest gnosis-test-eval-disconnected-callback-does-not-open-storage ()
  "A late verdict on a disconnected owner must not initialize any database."
  (gnosis-test-eval--with-review 'practice
    (let* ((before (gnosis-test-eval--evidence)) resolve opened
           (gnosis-agent-eval-function
            (lambda (_request yes _no) (setq resolve yes) #'ignore)))
      (cl-letf (((symbol-function 'recursive-edit)
                 (lambda ()
                   (insert "Retained answer")
                   (gnosis-test-eval--key "C-c C-c")
                   (let ((gnosis-db nil))
                     (cl-letf (((symbol-function 'gnosis--ensure-db)
                                (lambda () (setq opened t) (error "Unexpected database open"))))
                       (funcall resolve '(:verdict pass :explanation "Late")))))))
        (should-error (gnosis-review-agent-eval id)))
      (should-not opened)
      (should (equal before (gnosis-test-eval--evidence))))))

(provide 'gnosis-test-agent-eval)
;;; gnosis-test-agent-eval.el ends here

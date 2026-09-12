;;; gnosis-test-review-content.el --- Encounter content guards -*- lexical-binding: t; -*-

;;; Commentary:
;; Native edits preserve answered encounters; unrelated drift cannot grade them.

;;; Code:
(require 'ert)
(require 'gnosis-test-helpers)
(require 'gnosis-review)
(require 'gnosis-export-import)

(defun gnosis-test-content--add (kind)
  "Create a disposable question of KIND."
  (gnosis-add-thema-fields kind "The old answer"
                           (cond ((equal kind "basic") nil)
                                 ((equal kind "cloze") '("hint"))
                                 (t '("old" "new")))
                           '("old") "Explanation" nil 0 nil nil 222))

(defun gnosis-test-content--state (mode)
  "Retain a review state for MODE in the current buffer."
  (setq-local gnosis-review--state
              (gnosis-review-state-create
               :mode mode :persistent-p t :database gnosis-db
               :session-id (gnosis-scheduler-event-id)
               :event-id (gnosis-scheduler-event-id)
               :remaining '(222) :selected '(222) :initial 1 :total 1))
  (gnosis-review--save-session gnosis-review--state))

(defun gnosis-test-content--evidence ()
  "Read all scheduled/practice evidence and session projections."
  (mapcar (lambda (table)
            (gnosis-sqlite-select gnosis-db (concat "SELECT * FROM " table)))
          '("review_events" "practice_events" "scheduler_state"
            "study_session" "study_history")))

(defun gnosis-test-content--answer (kind &optional during-input input)
  "Review KIND with INPUT, calling DURING-INPUT at the input boundary."
  (let ((gnosis-review-buffer-name (buffer-name)))
    (cl-letf (((symbol-function 'gnosis-completing-read)
             (lambda (&rest _) (when during-input (funcall during-input)) (or input "old")))
            ((symbol-function 'gnosis--read-string-with-input-method)
             (lambda (&rest _) (when during-input (funcall during-input)) (or input "old"))))
      (funcall (intern (concat "gnosis-review-" kind)) 222))))

(ert-deftest gnosis-test-content-input-drift ()
  "Each response kind rejects drift before producing a pending grade."
  (dolist (kind '("mcq" "cloze" "mc-cloze"))
    (dolist (mode '(due practice))
      (gnosis-test-with-db
        (gnosis-test-content--add kind)
        (with-temp-buffer
          (gnosis-test-content--state mode)
          (let ((before (gnosis-test-content--evidence))
                (state (copy-tree (gnosis-review--state-data gnosis-review--state))))
            (should-error
             (gnosis-test-content--answer
              kind (lambda () (gnosis-update 'themata '(= answer '("new")) '(= id 222))))
             :type 'user-error)
            (should (equal before (gnosis-test-content--evidence)))
            (should (equal state (gnosis-review--state-data gnosis-review--state)))))))))

(ert-deftest gnosis-test-content-final-drift-and-override ()
  "Final acceptance, including overrides, rejects changed content atomically."
  (dolist (kind '("mcq" "cloze" "mc-cloze"))
    (dolist (mode '(due practice))
      (dolist (override '(nil t))
        (gnosis-test-with-db
          (gnosis-test-content--add kind)
          (with-temp-buffer
            (gnosis-test-content--state mode)
            (let* ((answer (gnosis-test-content--answer kind))
                   (success (if override (not (car answer)) (car answer)))
                   (result (if override (gnosis-review--override-result (cdr answer) success)
                             (cdr answer)))
                   (before (gnosis-test-content--evidence)))
              (gnosis-update 'themata '(= answer '("new")) '(= id 222))
              (should-error (gnosis-review-result 222 success result) :type 'user-error)
              (should (equal before (gnosis-test-content--evidence)))
              (should (equal '(222) (gnosis-review-state-remaining gnosis-review--state))))))))))

(ert-deftest gnosis-test-content-unchanged-retry ()
  "Unchanged answers accept once, including persistent retry after advancement."
  (dolist (kind '("mcq" "cloze" "mc-cloze"))
    (dolist (mode '(due practice))
      (gnosis-test-with-db
        (gnosis-test-content--add kind)
        (with-temp-buffer
          (gnosis-test-content--state mode)
          (let* ((answer (gnosis-test-content--answer kind))
                 (scheduled (gnosis-sqlite-select gnosis-db "SELECT * FROM scheduler_state")))
            (gnosis-review-result 222 (car answer) (cdr answer))
            (let ((once (gnosis-test-content--evidence)))
              (gnosis-review-result 222 (car answer) (cdr answer))
              (should (equal once (gnosis-test-content--evidence))))
            (should-not (gnosis-review-state-remaining gnosis-review--state))
            (when (eq mode 'practice)
              (should (equal scheduled (gnosis-sqlite-select gnosis-db "SELECT * FROM scheduler_state"))))))))))

(defun gnosis-test-content--edit-field (field text)
  "Replace native draft FIELD's body with TEXT, adding absent fields."
  (goto-char (point-min))
  (if (re-search-forward (concat "^\\*\\* " (regexp-quote field) "[ \t]*$") nil t)
      (progn
        (forward-line 1)
        (let ((start (point))
              (end (if (re-search-forward "^\\*\\* " nil t)
                       (line-beginning-position) (point-max))))
          (delete-region start end)
          (goto-char start)
          (insert text "\n\n")))
    (goto-char (point-max))
    (insert "\n** " field "\n" text "\n")))

(defun gnosis-test-content--edit-actions (answer edits &optional final after-edit)
  "Run public review ANSWER through native EDITS and FINAL actions.
EDITS contains field alists, nil for unchanged save, or `cancel'.
Call AFTER-EDIT in the original buffer after each edit.  Return the exact
answer passed to acceptance; also prove editing itself writes no evidence."
  (let* ((origin (current-buffer))
         (gnosis-review-buffer-name (buffer-name))
         (displayed (buffer-string))
         (before (gnosis-test-content--evidence))
         (gnosis-save-hook nil)
         (gnosis-review-editing-p nil)
         (accept (symbol-function 'gnosis-review-result))
         (final (or final '(?n)))
         accepted)
    (unwind-protect
        (cl-letf (((symbol-function 'recursive-edit)
                   (lambda ()
                     (should (derived-mode-p 'gnosis-edit-mode))
                     (let ((edit (pop edits)))
                       (if (eq edit 'cancel)
                           (gnosis-test-content--edit-field "Keimenon" "Uncommitted change")
                         (dolist (field edit)
                           (gnosis-test-content--edit-field (car field) (cdr field))))
                       (cl-letf (((symbol-function 'exit-recursive-edit) #'ignore))
                         (call-interactively
                          (key-binding (kbd (if (eq edit 'cancel) "C-c C-k" "C-c C-c"))))))
                     (set-buffer origin)
                     (when after-edit (funcall after-edit))))
                  ((symbol-function 'read-char-choice)
                   (lambda (&rest _)
                     (should (string-prefix-p (car (split-string displayed "Next review:"))
                                              (buffer-string)))
                     (should (equal before (gnosis-test-content--evidence)))
                     (if edits ?e (or (pop final) (ert-fail "Unexpected action prompt")))))
                  ((symbol-function 'gnosis-review-result)
                   (lambda (id success result)
                     (setq accepted (cons success result))
                     (dolist (key '(:event-id :reviewed-at-us :review-day :content :image :model))
                       (should (equal (plist-get (cdr answer) key) (plist-get result key))))
                     (funcall accept id success result))))
          (catch 'review-loop (gnosis-review-actions (car answer) 222 (cdr answer)))
          (should-not edits)
          (should-not final)
          (should (= (if (car accepted) 3 1)
                     (caar (sqlite-select gnosis-db
                             (concat "SELECT rating FROM "
                                     (if (eq (plist-get (cdr accepted) :mode) 'practice)
                                         "practice_events" "review_events"))))))
          accepted)
      (when (get-buffer "*Gnosis Edit*")
        (with-current-buffer "*Gnosis Edit*" (set-buffer-modified-p nil))
        (kill-buffer "*Gnosis Edit*")))))

(ert-deftest gnosis-test-content-edit-action ()
  "Native e/save/Next or Quit accepts the original answer exactly once."
  (dolist (kind '("basic" "mcq" "cloze" "mc-cloze"))
    (dolist (mode '(due practice))
      (dolist (success '(nil t))
        (dolist (final '(?n ?q))
          (dolist (edit '(cancel nil
                         (("Keimenon" . "The old revised question"))
                         (("Hypothesis" . "type"))
                         (("Keimenon" . "The new answer") ("Answer" . "new"))
                         (("Parathema" . "Revised explanation"))))
            ;; MCQ choices must continue to contain the canonical answer.
            (unless (and (member kind '("mcq" "mc-cloze"))
                         (equal edit '(("Hypothesis" . "type"))))
              (gnosis-test-with-db
                (gnosis-test-content--add kind)
                (when (equal kind "cloze")
                  (sqlite-execute gnosis-db "UPDATE themata SET hypothesis = 'nil' WHERE id = 222"))
                (save-window-excursion
                  (with-temp-buffer
                    (gnosis-test-content--state mode)
                    (let* ((answer (gnosis-test-content--answer kind nil (if success "old" "wrong")))
                           (scheduled (sqlite-select gnosis-db "SELECT * FROM scheduler_state"))
                           (content (gnosis--draft-content gnosis-db 222))
                           (accepted (gnosis-test-content--edit-actions answer (list edit) (list final))))
                      (should (eq success (car accepted)))
                      (should (equal (plist-get (cdr answer) :preview)
                                     (plist-get (cdr accepted) :preview)))
                      (should (= 1 (gnosis-review-state-reviewed gnosis-review--state)))
                      (should (equal (list (cons 222 success))
                                     (gnosis-review-state-outcomes gnosis-review--state)))
                      (should (equal (and (not success) '(222))
                                     (gnosis-review-state-remaining gnosis-review--state)))
                      (let ((once (gnosis-test-content--evidence)))
                        (gnosis-review-result 222 (car accepted) (cdr accepted))
                        (should (equal once (gnosis-test-content--evidence))))
                      (should (= 1 (caar (sqlite-select gnosis-db
                                          (concat "SELECT count(*) FROM "
                                                  (if (eq mode 'practice) "practice_events" "review_events"))))))
                      (when (eq edit 'cancel)
                        (should (equal content (gnosis--draft-content gnosis-db 222))))
                      (dolist (field (unless (eq edit 'cancel) edit))
                        (let ((value (gnosis-get (cdr (assoc (car field)
                                                           '(("Keimenon" . keimenon)
                                                             ("Hypothesis" . hypothesis)
                                                             ("Answer" . answer)
                                                             ("Parathema" . parathema))))
                                                 (if (equal (car field) "Parathema") 'extras 'themata)
                                                 '(= id 222))))
                          (should (equal (cdr field) (if (listp value) (car value) value)))))
                      (when (eq mode 'practice)
                        (should (equal scheduled (sqlite-select gnosis-db "SELECT * FROM scheduler_state")))
                        (should-not (sqlite-select gnosis-db "SELECT * FROM review_events"))))))))))))))

(ert-deftest gnosis-test-content-repeated-edit-and-aliases ()
  "Repeated saves retain captured matching; only explicit override regrades."
  (dolist (mode '(due practice))
    (dolist (success '(nil t))
      (dolist (override '(nil t))
        (gnosis-test-with-db
          (gnosis-test-content--add "basic")
          (save-window-excursion
            (with-temp-buffer
              (gnosis-test-content--state mode)
              (let* ((answer (gnosis-test-content--answer "basic" nil (if success "old" "wrong")))
                     (accepted
                      (gnosis-test-content--edit-actions
                       answer '((("Accepted aliases" . "- wrong"))
                                (("Answer" . "new"))
                                (("Accepted aliases" . ""))
                                cancel nil)
                       (if override '(?o ?n) '(?n)))))
                (should (eq (if override (not success) success) (car accepted)))
                (should (equal '("new") (gnosis-get 'answer 'themata '(= id 222))))
                (should-not (gnosis-get 'accepted-aliases 'themata '(= id 222)))
                (should (= 1 (gnosis-review-state-reviewed gnosis-review--state)))))))))))

(ert-deftest gnosis-test-content-presentation-drift ()
  "Question, choices and explanation are part of the pending content."
  (dolist (kind '("mcq" "cloze" "mc-cloze" "basic"))
    (dolist (mutation '((themata (= keimenon "Changed question"))
                        (themata (= hypothesis '("Changed hint")))
                        (extras (= parathema "Changed explanation"))
                        (extras (= review-image "Changed image"))))
      (gnosis-test-with-db
        (gnosis-test-content--add kind)
        (with-temp-buffer
          (gnosis-test-content--state 'due)
          (let ((answer (gnosis-test-content--answer kind))
                (before (gnosis-test-content--evidence)))
            (gnosis-update (car mutation) (cadr mutation) '(= id 222))
            (should-error (gnosis-review-result 222 (car answer) (cdr answer))
                          :type 'user-error)
            (should (equal before (gnosis-test-content--evidence)))))))))

(ert-deftest gnosis-test-content-encounter-drift ()
  "Input cannot outlive its buffer-local encounter, even in the same database."
  (dolist (kind '("mcq" "cloze" "mc-cloze"))
    (dolist (mode '(due practice))
      (gnosis-test-with-db
        (gnosis-test-content--add kind)
        (with-temp-buffer
          (gnosis-test-content--state mode)
          (let ((before (gnosis-test-content--evidence)))
            (should-error
             (gnosis-test-content--answer
              kind (lambda ()
                     (setq gnosis-review--state
                           (copy-gnosis-review-state gnosis-review--state))))
             :type 'user-error)
            (should (equal before (gnosis-test-content--evidence)))))))))

(ert-deftest gnosis-test-content-cancel-and-restart ()
  "Cancelled input leaves the queue intact and a fresh answer can settle it."
  (dolist (kind '("mcq" "cloze" "mc-cloze"))
    (dolist (mode '(due practice))
      (gnosis-test-with-db
        (gnosis-test-content--add kind)
        (with-temp-buffer
          (gnosis-test-content--state mode)
          (let ((before (gnosis-test-content--evidence)) cancelled)
            (condition-case nil
                (gnosis-test-content--answer kind (lambda () (signal 'quit nil)))
              (quit (setq cancelled t)))
            (should cancelled)
            (should (equal before (gnosis-test-content--evidence)))
            (let ((answer (gnosis-test-content--answer kind)))
              (gnosis-review-result 222 (car answer) (cdr answer)))
            (should-not (gnosis-review-state-remaining gnosis-review--state))))))))

(ert-deftest gnosis-test-content-edit-untrusted-changes ()
  "Native edits never acknowledge foreign content, connections or encounters."
  (dolist (mode '(due practice))
    (dolist (phase '(before-edit before-save after-save))
      (dolist (fault '(content deletion database owner checkpoint successor))
        (gnosis-test-with-db
          (gnosis-test-content--add "basic")
          (save-window-excursion
            (with-temp-buffer
              (gnosis-test-content--state mode)
              (let* ((answer (gnosis-test-content--answer "basic"))
                     (origin (current-buffer))
                     (database gnosis-db)
                     (gnosis-save-hook nil)
                     (choices '(?e ?n))
                     expected
                     (mutate
                      (lambda ()
                        (with-current-buffer origin
                          (pcase fault
                            ('content (gnosis-update 'themata '(= answer '("foreign")) '(= id 222)))
                            ('deletion (gnosis-delete-themata '(222)))
                            ('database (setq gnosis-db (gnosis-sqlite-open gnosis-test--db-file)))
                            ('owner (setq gnosis-review--state (copy-gnosis-review-state gnosis-review--state)))
                            ('checkpoint (setf (gnosis-review-state-event-id gnosis-review--state) "other-attempt"))
                            ('successor
                             (let ((successor (copy-gnosis-review-state gnosis-review--state)))
                               (setf (gnosis-review-state-session-id successor) "successor")
                               (gnosis-review--save-session successor))))
                          (setq expected (gnosis-test-content--evidence))))))
                (unwind-protect
                    (cl-letf (((symbol-function 'read-char-choice) (lambda (&rest _) (pop choices)))
                              ((symbol-function 'y-or-n-p) (lambda (&rest _) nil))
                              ((symbol-function 'recursive-edit)
                               (lambda ()
                                 (gnosis-test-content--edit-field "Keimenon" "Native revision")
                                 (when (eq phase 'before-save) (funcall mutate))
                                 (cl-letf (((symbol-function 'exit-recursive-edit) #'ignore))
                                   (call-interactively (key-binding (kbd "C-c C-c"))))
                                 (set-buffer origin)
                                 (when (eq phase 'after-save) (funcall mutate)))))
                      (when (eq phase 'before-edit) (funcall mutate))
                      (let ((err (should-error
                                  (gnosis-review-actions (car answer) 222 (cdr answer)))))
                        (should (string-match-p "changed\\|outdated\\|stale\\|deleted"
                                                (error-message-string err))))
                      (should expected)
                      (should (equal expected (gnosis-test-content--evidence)))
                      (should-not (sqlite-select gnosis-db "SELECT * FROM review_events"))
                      (should-not (sqlite-select gnosis-db "SELECT * FROM practice_events")))
                  (unless (eq gnosis-db database)
                    (gnosis-sqlite-close gnosis-db)
                    (setq gnosis-db database))
                  (when (get-buffer "*Gnosis Edit*")
                    (with-current-buffer "*Gnosis Edit*" (set-buffer-modified-p nil))
                    (kill-buffer "*Gnosis Edit*")))))))))))

(ert-deftest gnosis-test-content-edit-occurrence-and-rollback ()
  "A replacement draft or failed save cannot acknowledge a pending edit."
  (dolist (mode '(due practice))
    (dolist (fault '(replacement rollback cancel-foreign))
      (gnosis-test-with-db
        (gnosis-test-content--add "basic")
        (save-window-excursion
          (with-temp-buffer
            (gnosis-test-content--state mode)
            (let* ((answer (gnosis-test-content--answer "basic"))
                   (before (gnosis-test-content--evidence))
                   (origin (current-buffer))
                   (gnosis-save-hook nil)
                   (save (symbol-function 'gnosis-save-thema))
                   (choices '(?e ?n)))
              (unwind-protect
                  (cl-letf (((symbol-function 'read-char-choice) (lambda (&rest _) (pop choices)))
                            ((symbol-function 'recursive-edit)
                             (lambda ()
                               (when (eq fault 'replacement)
                                 ;; Reusing the same buffer/ID is still a new native edit.
                                 (set-buffer-modified-p nil)
                                 (gnosis-edit-thema 222))
                               (gnosis-test-content--edit-field "Keimenon" "Native revision")
                               (when (eq fault 'cancel-foreign)
                                 (gnosis-update 'themata '(= answer '("foreign")) '(= id 222)))
                               (cl-letf (((symbol-function 'exit-recursive-edit) #'ignore)
                                         ((symbol-function 'gnosis-save-thema)
                                          (lambda (thema)
                                            (prog1 (funcall save thema)
                                              (when (eq fault 'rollback) (error "Injected save failure"))))))
                                 (call-interactively (key-binding
                                                      (kbd (if (eq fault 'cancel-foreign)
                                                               "C-c C-k" "C-c C-c")))))
                               (set-buffer origin))))
                    (should-error (gnosis-review-actions (car answer) 222 (cdr answer)))
                    (should (equal before (gnosis-test-content--evidence)))
                    (when (eq fault 'rollback)
                      (should (equal "The old answer" (gnosis-get 'keimenon 'themata '(= id 222))))))
                (when (get-buffer "*Gnosis Edit*")
                  (with-current-buffer "*Gnosis Edit*" (set-buffer-modified-p nil))
                  (kill-buffer "*Gnosis Edit*"))))))))))

(ert-deftest gnosis-test-content-edited-alias-applies-next-time ()
  "Adding an alias cannot turn the captured failure into success."
  (dolist (mode '(due practice))
    (gnosis-test-with-db
      (gnosis-test-content--add "basic")
      (save-window-excursion
        (with-temp-buffer
          (gnosis-test-content--state mode)
          (let* ((answer (gnosis-test-content--answer "basic" nil "wrong"))
                 (accepted (gnosis-test-content--edit-actions
                            answer '((("Accepted aliases" . "- wrong"))))))
            (should-not (car accepted))
            ;; The failure is still requeued under the existing retry policy.
            (should (equal '(222) (gnosis-review-state-remaining gnosis-review--state)))
            (should (car (gnosis-test-content--answer "basic" nil "wrong")))))))))

(defun gnosis-test-content--media-answer (kind success)
  "Capture a real KIND review result for SUCCESS, faking only device input."
  (let ((depth 0)
        (gnosis-review-buffer-name (buffer-name)))
    (cl-letf (((symbol-function 'gnosis-model-open) #'gnosis-test-model--canvas)
              ((symbol-function 'gnosis-model--canvas-size) (lambda () 400))
              ((symbol-function 'recursion-depth) (lambda () depth))
              ((symbol-function 'exit-recursive-edit) #'ignore)
              ((symbol-function 'recursive-edit)
               (lambda ()
                 (setq depth 1)
                 (gnosis-test-model--wait-for-preparation)
                 (when (equal kind "model")
                   (gnosis-test-model--review-pick (if success "triangle" "other")))
                 (call-interactively (key-binding (kbd "RET")))))
              ((symbol-function 'gnosis--read-string-with-input-method)
               (lambda (&rest _) (if success "old" "wrong")))
              ((symbol-function 'gnosis-image-input)
               (lambda (&rest _) (list t (if success "left" "right")))))
      (cadr (gnosis-review--display-thema 222)))))

(ert-deftest gnosis-test-content-media-edit-actions ()
  "Image, model and combined owners preserve outcomes but retain stale guards."
  (require 'gnosis-test-image)
  (require 'gnosis-test-model)
  (dolist (kind '("model" "model-name" "image-region" "image-occlusion" "basic"))
    (dolist (mode '(due practice))
      (dolist (success '(nil t))
        (dolist (fault '(nil override content resource owner))
          (gnosis-test-with-db
            (let* ((model (member kind '("model" "model-name")))
                   (reference (if model (gnosis-model-import (gnosis-test-model--scene))
                                (gnosis-image-import (gnosis-test-image--file) gnosis-test-image--regions)))
                   (asset (if model
                              (expand-file-name "triangle.obj"
                                                (file-name-directory (expand-file-name reference (gnosis-assets-root))))
                            (alist-get 'path (gnosis-image-resolve reference))))
                   (hypothesis (pcase kind
                                 ("model" (list reference "0" "0" "1"))
                                 ("model-name" (list reference "triangle" "0" "0" "1"))
                                 ("image-region" (list reference))
                                 ("image-occlusion" (list reference "left" "hide-target"))))
                   (canonical (pcase kind ("model" "triangle") ("image-region" "left") (_ "old"))))
              (gnosis-add-thema-fields
               kind (if (equal kind "basic") (format "Question [[gnosis-image:%s]]" reference) "Question")
               hypothesis (list canonical) "Explanation" nil 0 nil nil 222)
              (save-window-excursion
                (with-temp-buffer
                  (gnosis-mode)
                  (gnosis-test-content--state mode)
                  (cl-letf (((symbol-function 'gnosis-image--decode) #'ignore)
                            ((symbol-function 'gnosis-image-mask) (lambda (&rest _) "[image]"))
                            ((symbol-function 'gnosis-image-format-string) (lambda (text &rest _) text)))
                    (let* ((answer (gnosis-test-content--media-answer kind success))
                           (before (gnosis-test-content--evidence))
                           (scheduled (sqlite-select gnosis-db "SELECT * FROM scheduler_state"))
                           (edits (list '(("Keimenon" . "Future question"))
                                        (list (cons "Answer" (pcase kind
                                                               ("model" "other")
                                                               ("image-region" "right")
                                                               (_ "new")))))))
                      (when (member kind '("basic" "model-name" "image-occlusion"))
                        (setq edits (append edits '((("Accepted aliases" . "- wrong"))))))
                      (should (eq success (car answer)))
                      (if (memq fault '(content resource owner))
                          (progn
                            (should-error
                             (gnosis-test-content--edit-actions
                              answer edits nil
                              (lambda ()
                                (pcase fault
                                  ('content (gnosis-update 'themata '(= keimenon "Foreign question") '(= id 222)))
                                  ('resource (delete-file asset))
                                  ('owner (setq gnosis-review--state (copy-gnosis-review-state gnosis-review--state)))))))
                            (should (equal before (gnosis-test-content--evidence))))
                        (let ((accepted (gnosis-test-content--edit-actions
                                         answer edits (if (eq fault 'override) '(?o ?q) '(?q)))))
                          (should (eq (if (eq fault 'override) (not success) success)
                                      (car accepted)))
                          (should (= 1 (gnosis-review-state-reviewed gnosis-review--state)))
                          (let ((once (gnosis-test-content--evidence)))
                            (gnosis-review-result 222 (car accepted) (cdr accepted))
                            (should (equal once (gnosis-test-content--evidence)))))
                        (when (eq mode 'practice)
                          (should (equal scheduled (sqlite-select gnosis-db "SELECT * FROM scheduler_state")))
                          (should-not (sqlite-select gnosis-db "SELECT * FROM review_events")))))))))))))))

(provide 'gnosis-test-review-content)
;;; gnosis-test-review-content.el ends here

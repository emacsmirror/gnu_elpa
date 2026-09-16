;;; gnosis-test-review-format-owner.el --- Formatting lifetime tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Org hooks are real; terminal media fixtures do not claim decoded pixels.

;;; Code:
(require 'gnosis-test-review-active-owner)

(defun gnosis-test-format--case (mode kind phase mutation &optional native)
  "Exercise MODE, KIND and formatting PHASE across MUTATION.
With NATIVE non-nil, use the real typed reader.  Media attachment and pixels
are substituted explicitly; Org formatting and lifetime hooks remain native."
  (gnosis-test-with-db
    (save-window-excursion
      (gnosis-test-active-owner--seed mode)
      (let* ((id (cond ((member kind '("model" "model-name")) (gnosis-test-model--add))
                       ((member kind '("image-region" "image-occlusion")) (gnosis-test-image--add))
                       (t 222)))
             (gnosis-review-buffer-name "*gnosis-format-owner*")
             (owner (gnosis-review--setup-buffer (list id) mode))
             (positive (memq mutation '(unchanged rename)))
             (formats 0) (inputs 0) (preparations 0)
             (database gnosis-db)
             before fired answer failure draft map successor)
        (unwind-protect
            (progn
              (switch-to-buffer owner)
              (gnosis-test-content--state mode)
              (setf (gnosis-review-state-remaining gnosis-review--state) (list id)
                    (gnosis-review-state-selected gnosis-review--state) (list id))
              (gnosis-review--save-session gnosis-review--state)
              (cond
               ((equal kind "model-name")
                (gnosis-update 'themata '(= type "model-name") `(= id ,id))
                (let ((hypothesis (gnosis-get 'hypothesis 'themata `(= id ,id))))
                  (gnosis-update 'themata
                                 `(= hypothesis ,(cons (car hypothesis) (cons "triangle" (cdr hypothesis))))
                                 `(= id ,id)))
                (gnosis-update 'themata '(= answer '("old")) `(= id ,id)))
               ((equal kind "image-occlusion")
                (gnosis-update 'themata '(= type "image-occlusion") `(= id ,id))
                (gnosis-update 'themata `(= hypothesis ,(append (gnosis-get 'hypothesis 'themata `(= id ,id)) '("left"))) `(= id ,id))
                (gnosis-update 'themata '(= answer '("old")) `(= id ,id)))
               ((= id 222)
                (gnosis-update 'themata `(= type ,(if (equal kind "self-grade") "basic" kind)) '(= id 222))
                (when (member kind '("cloze" "mcq" "mc-cloze"))
                  (gnosis-update 'themata '(= hypothesis '("old" "new")) '(= id 222)))))
              (setq before (gnosis-test-content--evidence))
              (should (nth (if (eq mode 'due) 0 1) before))
              (should (nth 3 before))
              (let* ((gnosis-review-basic-input (if (equal kind "self-grade") 'self-grade 'typed))
                     (org-mode-hook
                      (list (lambda ()
                              (cl-incf formats)
                              (when (= formats (pcase phase
                                                 ('prompt 1)
                                                 ('reveal 2)
                                                 (_ (if (member kind '("cloze" "mc-cloze" "image-occlusion")) 3 2))))
                                (setq fired t)
                                (with-current-buffer owner
                                  (pcase mutation
                                    ('rename (setq draft (gnosis-test-active-owner--rename)))
                                    ((or 'associated 'detached)
                                     (gnosis-test-active-owner--repurpose (eq mutation 'detached)))
                                    ((or 'database 'content 'state 'checkpoint)
                                     (pcase mutation
                                       ('database
                                        (setq gnosis-db (gnosis-sqlite-open
                                                         (expand-file-name "gnosis.db" gnosis-dir))))
                                       ('content (gnosis-update 'themata '(= keimenon "Changed question") `(= id ,id)))
                                       ('state (setf (gnosis-review-state-event-id gnosis-review--state) "replacement-event"))
                                       ('checkpoint
                                        (let ((replacement (copy-gnosis-review-state gnosis-review--state)))
                                          (setf (gnosis-review-state-session-id replacement) "replacement-session")
                                          (gnosis-review--save-session replacement))
                                        (let ((after (gnosis-test-content--evidence)))
                                          (should (equal (seq-take before 3) (seq-take after 3)))
                                          (setq before after))))
                                     (erase-buffer) (insert "Successor unsaved text")
                                     (setq-local header-line-format "Successor header")
                                     (use-local-map (make-sparse-keymap)))
                                    ((or 'mode 'setup)
                                     (let ((state gnosis-review--state))
                                       (if (eq mutation 'mode)
                                           (progn (fundamental-mode) (gnosis-mode))
                                         (gnosis-review--setup-buffer (list id) mode))
                                       (setq gnosis-review--state state))
                                     (erase-buffer) (insert "Successor unsaved text")
                                     (setq-local header-line-format "Successor header")
                                     (use-local-map (make-sparse-keymap))))
                                  (setq successor (or draft owner)
                                        map (with-current-buffer successor (current-local-map))))))))
                     (reader (symbol-function 'gnosis--read-string-with-input-method))
                     (choice-reader (symbol-function 'read-char-choice))
                     (completion-reader (symbol-function 'gnosis-completing-read))
                     (recursive-input (symbol-function 'recursive-edit)))
                (cl-letf (((symbol-function 'gnosis--read-string-with-input-method)
                           (lambda (&rest args) (cl-incf inputs)
                             (if native (apply reader args) "old")))
                          ((symbol-function 'gnosis-completing-read)
                           (lambda (&rest args) (cl-incf inputs)
                             (if native (apply completion-reader args) "old")))
                          ((symbol-function 'read-char-choice)
                           (lambda (prompt &rest args) (cl-incf inputs)
                             (if native (apply choice-reader prompt args)
                               (if (string-prefix-p "Recall first" prompt) ?\s ?y))))
                          ((symbol-function 'gnosis-image--decode) (lambda (_) '(image :type png)))
                          ((symbol-function 'gnosis-image-mask) (lambda (&rest _) "[test mask]"))
                          ((symbol-function 'gnosis-image-input)
                           (lambda (&rest _) (cl-incf inputs) '(nil "left")))
                          ((symbol-function 'gnosis-model-prepare)
                           (lambda (&rest _) (cl-incf preparations) nil))
                          ((symbol-function 'recursive-edit)
                           (lambda ()
                             (cl-incf inputs)
                             (cl-labels ((settle ()
                                           (with-current-buffer owner
                                             (let* ((context gnosis-review--model-context)
                                                    (row (car (gnosis-review--answer-thema id))))
                                               (setf (plist-get context :fields)
                                                     (gnosis-model-fields (nth 0 row) (nth 2 row) (nth 3 row))
                                                     (plist-get context :input) "old"
                                                     (plist-get context :selection) '(:id "triangle" :mesh "triangle" :face 0 :point (0 0 0))
                                                     (plist-get context :view) '(0 -90 1)
                                                     (plist-get context :result) (cons t (gnosis-review-algorithm id t)))))))
                               (if (not native) (settle)
                                 (let ((timer (run-at-time 0.01 nil
                                                           (lambda () (settle) (exit-recursive-edit)))))
                                   (unwind-protect (funcall recursive-input)
                                     (cancel-timer timer))))))))
                  (condition-case err
                      (setq answer (funcall (intern (concat "gnosis-review-" (if (equal kind "self-grade") "basic" kind))) id))
                    (user-error (setq failure err)))))
              (should fired)
              (should (equal before (gnosis-test-content--evidence)))
              (if positive
                  (progn
                    (should-not failure)
                    (should (car answer))
                    (should (> inputs 0))
                    (when draft (gnosis-test-active-owner--draft-unchanged draft map)))
                (should failure)
                (should-not answer)
                (when (eq phase 'prompt) (should (= inputs 0)) (should (= preparations 0)))
                (when (equal kind "self-grade") (should (= inputs (if (eq phase 'prompt) 0 1))))
                (should (equal "Successor unsaved text" (buffer-string)))
                (should (eq map (current-local-map)))
                (should (equal "Successor header" header-line-format))))
          (unless (eq database gnosis-db)
            (gnosis-sqlite-close gnosis-db)
            (setq gnosis-db database))
          (dolist (buffer (list owner draft))
            (when (buffer-live-p buffer)
              (with-current-buffer buffer (set-buffer-modified-p nil))
              (kill-buffer buffer))))))))

(ert-deftest gnosis-format-owner-model-prompt ()
  "Model formatting cannot replace a retired destination or start preparation."
  (dolist (mode '(due practice))
    (gnosis-test-format--case mode "model" 'prompt 'detached)))

(ert-deftest gnosis-format-owner-basic-explanation ()
  "Explanation formatting cannot append to a successor or return a result."
  (dolist (mode '(due practice))
    (gnosis-test-format--case mode "basic" 'explanation 'detached)))

(ert-deftest gnosis-format-owner-native-type-matrix ()
  "Every native type guards prompt, reveal and explanation formatting."
  (dolist (mode '(due practice))
    (dolist (kind '("basic" "self-grade" "mcq" "cloze" "mc-cloze"
                    "model" "model-name" "image-region" "image-occlusion"))
      (dolist (phase (append '(prompt explanation)
                             (when (member kind '("cloze" "mc-cloze" "image-occlusion")) '(reveal))))
        (dolist (mutation '(associated detached mode setup unchanged rename))
          (ert-info ((format "%s %s %s %s" mode kind phase mutation))
            (gnosis-test-format--case mode kind phase mutation)))))))

(ert-deftest gnosis-format-owner-shared-standalone-sinks ()
  "Standalone renderers validate before writing, but allow renamed owners."
  (dolist (renderer '((gnosis-display-keimenon "Prompt")
                      (gnosis-display-basic-answer "Answer" nil "Wrong")
                      (gnosis-display-hint "Hint")
                      (gnosis-display-cloze-user-answer "Wrong")
                      (gnosis-display-correct-answer-mcq "Right" "Wrong")
                      (gnosis-display-parathema "Explanation")
                      (gnosis-display-next-review (2026 9 16) t)))
    (dolist (mutation '(associated detached mode setup unchanged rename))
      (gnosis-test-with-db
        (with-temp-buffer
          (gnosis-mode)
          (let* ((owner (current-buffer))
                 (gnosis-review-buffer-name (buffer-name))
                 (formatter (symbol-function 'gnosis-review--format-string))
                 fired failure draft map)
            (unwind-protect
                (progn
                  (cl-letf (((symbol-function 'gnosis-review--format-string)
                             (lambda (&rest args)
                               (prog1 (apply formatter args)
                                 (unless fired
                                   (setq fired t)
                                   (with-current-buffer owner
                                     (pcase mutation
                                       ('rename (setq draft (gnosis-test-active-owner--rename)))
                                       ((or 'associated 'detached)
                                        (gnosis-test-active-owner--repurpose (eq mutation 'detached)))
                                       ('mode (fundamental-mode) (gnosis-mode))
                                       ('setup (gnosis-review--setup-buffer nil)))
                                     (unless (memq mutation '(associated detached unchanged rename))
                                       (insert "Successor unsaved text")
                                       (setq-local header-line-format "Successor header")
                                       (use-local-map (make-sparse-keymap)))
                                     (setq map (current-local-map))))))))
                    (condition-case err (apply (car renderer) (cdr renderer))
                      (user-error (setq failure err))))
                  (should fired)
                  (if (memq mutation '(unchanged rename))
                      (progn (should-not failure)
                             (should (> (buffer-size) 0))
                             (when draft
                               (gnosis-test-active-owner--draft-unchanged
                                draft (with-current-buffer draft (current-local-map)))))
                    (should failure)
                    (should (equal "Successor unsaved text" (buffer-string)))
                    (should (eq map (current-local-map)))
                    (should (equal "Successor header" header-line-format))))
              (set-buffer-modified-p nil)
              (when (buffer-live-p draft) (kill-buffer draft)))))))))

(ert-deftest gnosis-format-owner-model-preparation-boundaries ()
  "Retired preparation continuations cannot start work, install maps or read."
  (dolist (mode '(due practice))
    (dolist (boundary '(take prepare attach))
      (dolist (mutation '(detached setup))
        (gnosis-test-with-db
          (save-window-excursion
            (gnosis-test-active-owner--seed mode)
            (let* ((id (gnosis-test-model--add))
                   (gnosis-review-buffer-name "*gnosis-format-preparation*")
                   (owner (gnosis-review--setup-buffer (list id) mode))
                   (row (car (gnosis-review--answer-thema id)))
                   (fields (gnosis-model-fields (nth 0 row) (nth 2 row) (nth 3 row)))
                   (before (gnosis-test-content--evidence))
                   prepared entered map)
              (unwind-protect
                  (progn
                    (switch-to-buffer owner)
                    (cl-labels ((retire ()
                                  (if (eq mutation 'detached)
                                      (gnosis-test-active-owner--repurpose t)
                                    (let ((state gnosis-review--state))
                                      (gnosis-review--setup-buffer (list id) mode)
                                      (setq gnosis-review--state state))
                                    (erase-buffer) (insert "Successor unsaved text")
                                    (setq-local header-line-format "Successor header")
                                    (use-local-map (make-sparse-keymap)))
                                  (setq map (current-local-map))))
                      (cl-letf (((symbol-function 'gnosis-review--lookahead-take)
                                 (lambda (_) (when (eq boundary 'take) (retire)) nil))
                                ((symbol-function 'gnosis-model-prepare)
                                 (lambda (_type _hypothesis _answer callback)
                                   (setq prepared t)
                                   (if (eq boundary 'prepare) (retire)
                                     (funcall callback fields nil))
                                   nil))
                                ((symbol-function 'gnosis-model--canvas-size) (lambda () 256))
                                ((symbol-function 'gnosis-model-open) (lambda (&rest _) (retire)))
                                ((symbol-function 'recursive-edit) (lambda () (setq entered t))))
                        (should-error (gnosis-review-model id) :type 'user-error)))
                    (should-not entered)
                    (when (eq boundary 'take) (should-not prepared))
                    (should (equal "Successor unsaved text" (buffer-string)))
                    (should (eq map (current-local-map)))
                    (should (equal "Successor header" header-line-format))
                    (should (equal before (gnosis-test-content--evidence))))
                (with-current-buffer owner (set-buffer-modified-p nil))
                (kill-buffer owner)))))))))

(ert-deftest gnosis-format-owner-image-viewer-callbacks ()
  "The image viewer checks its caller after decode, navigation, mode and render."
  (dolist (boundary '(decode pop mode render))
    (gnosis-test-with-db
      (save-window-excursion
        (let* ((gnosis-review-buffer-name "*gnosis-format-image-viewer*")
               (owner (gnosis-review--setup-buffer nil))
               (check (with-current-buffer owner (gnosis-review--display-validator)))
               entered rendered viewer viewer-owner map)
          (unwind-protect
              (cl-labels ((retire ()
                            (with-current-buffer owner
                              (gnosis-test-active-owner--repurpose t)
                              (setq map (current-local-map)))))
                (let ((buffer-list-update-hook
                       (list (lambda ()
                               (when (and (eq boundary 'pop) (not viewer)
                                          (string-prefix-p "*Gnosis Image*" (buffer-name)))
                                 (setq viewer (current-buffer))
                                 (retire)))))
                      (gnosis-image-mode-hook
                       (list (lambda ()
                               (setq viewer (current-buffer)
                                     viewer-owner gnosis-image--owner)
                               (when (eq boundary 'mode) (retire))))))
                  (cl-letf (((symbol-function 'gnosis-image--decode)
                             (lambda (_) (when (eq boundary 'decode) (retire))))
                            ((symbol-function 'image-type-available-p) (lambda (_) t))
                            ((symbol-function 'gnosis-image--render)
                             (lambda (&rest _)
                               (setq rendered t)
                               (when (eq boundary 'render) (retire))))
                            ((symbol-function 'recursive-edit) (lambda () (setq entered t))))
                    (should-error (gnosis-image-input nil 'region "left" check) :type 'user-error)))
                (should-not entered)
                ;; Capture the viewer before the caller check can unwind it.
                (should-not (buffer-live-p viewer))
                (should-not (cdr viewer-owner))
                (unless (eq boundary 'render) (should-not rendered))
                (with-current-buffer owner
                  (should (equal "Successor unsaved text" (buffer-string)))
                  (should (eq map (current-local-map)))
                  (should (equal "Successor header" header-line-format))))
            (when (buffer-live-p viewer) (kill-buffer viewer))
            (with-current-buffer owner (set-buffer-modified-p nil))
            (kill-buffer owner)))))))

(ert-deftest gnosis-format-owner-standalone-org-capture ()
  "Standalone Org renderers capture the destination before running Org hooks."
  (dolist (kind '(cloze explanation))
    (dolist (mutation '(detached rename))
      (gnosis-test-with-db
        (with-temp-buffer
          (gnosis-mode)
          (let* ((owner (current-buffer))
                 (gnosis-review-buffer-name (buffer-name))
                 draft map failure
                 (org-mode-hook
                  (list (lambda ()
                          (with-current-buffer owner
                            (if (eq mutation 'rename)
                                (setq draft (gnosis-test-active-owner--rename))
                              (gnosis-test-active-owner--repurpose t))
                            (setq map (current-local-map)))))))
            (unwind-protect
                (progn
                  (condition-case err
                      (if (eq kind 'cloze)
                          (with-temp-buffer
                            (gnosis-display-cloze-string "The old answer" '("old") nil nil nil))
                        (gnosis-display-parathema "Explanation"))
                    (user-error (setq failure err)))
                  (if (eq mutation 'rename)
                      (progn (should-not failure)
                             (should (> (buffer-size) 0))
                             (gnosis-test-active-owner--draft-unchanged
                              draft (with-current-buffer draft (current-local-map))))
                    (should failure)
                    (should (equal "Successor unsaved text" (buffer-string)))
                    (should (eq map (current-local-map)))
                    (should (equal "Successor header" header-line-format))))
              (set-buffer-modified-p nil)
              (when (buffer-live-p draft) (kill-buffer draft)))))))))

(ert-deftest gnosis-format-owner-content-and-session ()
  "Formatting cannot adopt a different database, content or checkpoint."
  (dolist (mode '(due practice))
    (dolist (kind '("basic" "self-grade" "mcq" "cloze" "mc-cloze"
                    "model" "model-name" "image-region" "image-occlusion"))
      (dolist (phase '(prompt explanation))
        (dolist (mutation '(database content state checkpoint))
          (ert-info ((format "%s %s %s %s" mode kind phase mutation))
            (gnosis-test-format--case mode kind phase mutation)))))))

(provide 'gnosis-test-review-format-owner)
;;; gnosis-test-review-format-owner.el ends here

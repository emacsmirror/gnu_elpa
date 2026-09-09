;;; gnosis-test-model.el --- Model resource and encounter tests -*- lexical-binding: t; -*-
;;; Commentary:
;; Tiny original triangle fixtures; no third-party anatomy data or renderer.
;;; Code:
(require 'ert)
(require 'gnosis-test-helpers)
(require 'gnosis-model)
(require 'gnosis-review)
(require 'gnosis-export-import)

(defun gnosis-test-model--scene ()
  "Create a tiny licensed source scene in the current disposable directory."
  (let ((dir (expand-file-name "source" gnosis-dir)))
    (make-directory dir t)
    (with-temp-file (expand-file-name "triangle.obj" dir)
      (insert "v 0 0 0\nv 1 0 0\nv 0 1 0\nf 1 2 3\n"))
    (with-temp-file (expand-file-name "scene.json" dir)
      (insert "{\"objects\":[{\"id\":\"triangle\",\"label\":\"Triangle\",\"path\":\"triangle.obj\"},{\"id\":\"other\",\"label\":\"Other triangle\",\"path\":\"triangle.obj\"}],\"initial_view\":[0,-90,1],\"license\":\"CC0; original test geometry\",\"source\":\"Gnosis ERT fixture\"}"))
    (expand-file-name "scene.json" dir)))

(defun gnosis-test-model--add ()
  "Create and return a model thema ID in the disposable environment."
  (let* ((reference (gnosis-model-import (gnosis-test-model--scene)))
         (id (gnosis-generate-id)))
    (gnosis-add-thema-fields "model" "Select triangle" (list reference "0" "-90" "1")
                            '("triangle") "Original geometry" '("test") 0 nil nil id)
    id))

(ert-deftest gnosis-model-author-save-reopen-edit ()
  (gnosis-test-with-db
    (save-window-excursion
      (let ((scene (gnosis-test-model--scene))
            (gnosis-save-hook nil))
        (unwind-protect
            (progn
              (cl-letf (((symbol-function 'read-file-name) (lambda (&rest _) scene))
                        ((symbol-function 'read-string) (lambda (&rest _) ""))
                        ((symbol-function 'completing-read) (lambda (_ choices &rest _) (caar choices)))
                        ((symbol-function 'read-number) (lambda (_ value) value)))
                (call-interactively #'gnosis-add-model-thema))
              (insert "Select the triangle")
              (should (eq (key-binding (kbd "C-c C-a")) #'gnosis-model-attach))
              (should (string-match-p "Resource and starting view" (buffer-string)))
              (should-not (string-match-p "Hypothesis" (buffer-string)))
              (let ((print-length 1) (print-level 1) (print-escape-newlines nil)
                    (print-circle t) (print-gensym t))
                (call-interactively (key-binding (kbd "C-c C-c"))))
              (let* ((id (car (gnosis-select 'id 'themata nil t)))
                     (before (gnosis-select '* 'themata)))
                (should (equal '("0" "-90" "1")
                               (cdr (gnosis-get 'hypothesis 'themata `(= id ,id)))))
                (gnosis-sqlite-close gnosis-db)
                (setq gnosis-db (gnosis-db--open gnosis-dir))
                (should (equal before (gnosis-select '* 'themata)))
                (gnosis-edit-thema id)
                (should (string-match-p "Resource and starting view" (buffer-string)))
                (call-interactively (key-binding (kbd "C-c C-c")))
                (should (equal before (gnosis-select '* 'themata)))
                (gnosis-edit-thema id)
                (goto-char (point-min))
                (search-forward "\n- 0\n- -90\n- 1\n")
                (replace-match "\n- -12.5\n- -45\n- 1.0\n" t t)
                (call-interactively (key-binding (kbd "C-c C-c")))
                (gnosis-sqlite-close gnosis-db)
                (setq gnosis-db (gnosis-db--open gnosis-dir))
                (should (equal '("-12.5" "-45" "1.0")
                               (cdr (gnosis-get 'hypothesis 'themata `(= id ,id)))))
                (gnosis-edit-thema id)
                (should (string-match-p "\n- -12.5\n- -45\n- 1.0\n" (buffer-string)))
                (call-interactively (key-binding (kbd "C-c C-c")))
                (should (equal '("-12.5" "-45" "1.0")
                               (cdr (gnosis-get 'hypothesis 'themata `(= id ,id)))))
                (should (equal "" (gnosis-get 'review-image 'extras `(= id ,id))))))
          (dolist (name '("*Gnosis NEW*" "*Gnosis Edit*"))
            (when (get-buffer name) (kill-buffer name))))))))

(ert-deftest gnosis-model-import-idempotent-and-printer-independent ()
  (gnosis-test-with-db
    (let* ((scene (gnosis-test-model--scene))
           (reference (gnosis-model-import scene))
           (print-length 1) (print-level 1) (print-circle t)
           (json-encoding-pretty-print t) (json-encoding-separator ";"))
      (should (equal reference (gnosis-model-import scene)))
      (should (= 1 (length (directory-files (gnosis-model--root) nil "^[^.].*"))))
      (should (gnosis-model-resolve (list reference "0" "-90" "1") '("triangle"))))))

(ert-deftest gnosis-model-import-copy-failure-quit-and-retry ()
  (gnosis-test-with-db
    (let ((scene (gnosis-test-model--scene)))
      (dolist (condition '(error quit))
        (cl-letf (((symbol-function 'copy-file)
                   (lambda (&rest _) (signal condition '("Injected copy interruption")))))
          (should (eq condition
                      (condition-case err (gnosis-model-import scene)
                        ((error quit) (car err)))))))
      (should-not (directory-files (gnosis-model--root) nil "^[^.].*"))
      (should-not (directory-files (gnosis-model--root) nil "^\\.import-"))
      (should (stringp (gnosis-model-import scene))))))

(ert-deftest gnosis-model-resource-integrity-target-and-save-refusal ()
  (gnosis-test-with-db
    (let* ((id (gnosis-test-model--add))
           (hypothesis (gnosis-get 'hypothesis 'themata `(= id ,id)))
           (before (gnosis-select '* 'themata))
           (file (car (gnosis-model-resolve hypothesis '("triangle")))))
      (should-error (gnosis-model-resolve hypothesis '("absent")))
      (should-error (gnosis-model-resolve '("../scene.json" "0" "0" "1") '("triangle")))
      (with-temp-file (expand-file-name "triangle.obj" (file-name-directory file))
        (insert "changed geometry"))
      (should-error (gnosis-model-resolve hypothesis '("triangle")))
      (should-error (gnosis-update-thema id "Changed" hypothesis '("triangle") "" nil nil))
      (should (equal before (gnosis-select '* 'themata)))
      (should-error (gnosis-add-thema-fields "model" "Bad" hypothesis '("triangle") "" nil 0 nil))
      (should (equal before (gnosis-select '* 'themata)))
      (delete-file file)
      (should-error (gnosis-model-resolve hypothesis '("triangle"))))))

(ert-deftest gnosis-model-manifest-provenance-required-and-preserved ()
  (gnosis-test-with-db
    (let* ((file (gnosis-test-model--scene))
           (scene (gnosis-model--scene file)))
      (with-temp-file file
        (insert (json-encode (assq-delete-all 'source (assq-delete-all 'license scene)))))
      (should-error (gnosis-model-import file))
      (let* ((reference (gnosis-model-import file "CC-BY-SA; credited author" "Source URL"))
             (path (car (gnosis-model-resolve (list reference "0" "0" "1") '("triangle"))))
             (stored (gnosis-model--scene path)))
        (should (equal "CC-BY-SA; credited author" (alist-get 'license stored)))
        (should (equal "Source URL" (alist-get 'source stored)))))))

(ert-deftest gnosis-model-attach-cancel-preserves-draft ()
  (gnosis-test-with-db
    (with-temp-buffer
      (gnosis-edit-mode)
      (gnosis-export--insert-thema "NEW" "model" "Draft question")
      (let ((before (buffer-string)))
        (cl-letf (((symbol-function 'read-file-name) (lambda (&rest _) (signal 'quit nil))))
          (should (eq 'quit (condition-case err (gnosis-model-attach) (quit (car err))))))
        (should (equal before (buffer-string)))))))

(ert-deftest gnosis-model-study-composition-model-only ()
  (gnosis-test-with-db
    (let ((model (gnosis-test-model--add)))
      (gnosis--insert-into 'nodes '(["topic" "fixture.org" "Models" "1" nil nil nil]))
      (gnosis--insert-into 'thema-links `([,model "topic"]))
      (should (equal (list model) (gnosis-study-topic-ids '("topic"))))
      (should (equal (list model) (gnosis-study-topic-ids '("topic") t)))
      (should (equal '(:total 1 :eligible 1 :suspended 0 :new 1 :due 1 :not-due 0)
                     (gnosis-study-composition (list model model))))
      (with-temp-buffer
        (gnosis-study-mode)
        (setq-local gnosis-study--topic "topic")
        (gnosis-study-refresh)
        (should (string-match-p "1 linked, 1 due, 1 new, 0 suspended"
                                header-line-format))))))

(ert-deftest gnosis-model-study-composition-mixed ()
  (gnosis-test-with-db
    (let* ((model (gnosis-test-model--add))
           (due (gnosis-test--add-basic-thema "Due" "Answer"))
           (future (gnosis-test--add-basic-thema "Future" "Answer"))
           (suspended (gnosis-test--add-basic-thema "Suspended" "Answer" nil nil nil 1))
           (ids (list model due future suspended)))
      (gnosis--insert-into 'nodes '(["topic" "fixture.org" "Mixed" "1" nil nil nil]))
      (gnosis--insert-into 'thema-links
                          (mapcar (lambda (id) (vector id "topic")) ids))
      (gnosis-update 'scheduler-state
                     `(= due-day ,(gnosis--date-to-int (gnosis-date 3)))
                     `(= thema-id ,future))
      ;; Imported type spelling must follow the same case-insensitive policy.
      (gnosis-update 'themata '(= type "Model") `(= id ,model))
      (let ((counts (gnosis-study-composition (cons model ids)))
            (selected (gnosis-study-topic-ids '("topic") t)))
        (should (equal (sort (list model due) #'<) (sort selected #'<)))
        (should (= (length selected) (plist-get counts :due)))
        (should (equal '(:total 4 :eligible 3 :suspended 1 :new 3 :due 2 :not-due 1)
                       counts))
        (should (equal (sort (list model due future) #'<)
                       (sort (gnosis-study-topic-ids '("topic")) #'<)))))))

(ert-deftest gnosis-model-due-selection-and-new-limit ()
  (gnosis-test-with-db
    (let* ((model (gnosis-test-model--add))
           (basic (gnosis-test--add-basic-thema "Ordinary" "Answer"))
           (gnosis-new-themata-limit 1))
      (should (gnosis-study-eligible-p model))
      (should (gnosis-review-is-due-p model))
      (should (equal (list (min model basic)) (gnosis-review-get-due-themata)))
      (gnosis-update 'scheduler-state '(= reps 1) `(= thema-id ,model))
      (gnosis-update 'scheduler-state '(= due-day 20000101) `(= thema-id ,model))
      (should (equal (list model) (gnosis-review-get-overdue-themata)))
      (should (= 1 (gnosis-review-count-overdue)))
      (should (= 2 (length (gnosis-review-get-due-themata))))
      (gnosis-update 'scheduler-state '(= suspended 1) `(= thema-id ,model))
      (should-not (gnosis-review-is-due-p model))
      (should-not (gnosis-review-get-overdue-themata))
      (should (= 0 (gnosis-review-count-overdue)))
      (should-not (gnosis-select '* 'practice-events))
      (should-not (gnosis-select '* 'review-events)))))

(ert-deftest gnosis-model-content-exchange-fails-closed ()
  (gnosis-test-with-db
    (let* ((model (gnosis-test-model--add))
           (out (expand-file-name "export.gnosis" gnosis-dir)))
      (with-temp-file out (insert "retained destination"))
      (should-error (gnosis-export-db out))
      (should (equal "retained destination"
                     (with-temp-buffer (insert-file-contents out) (buffer-string))))
      (should-error (gnosis-import--format-version-in-db gnosis-db "main"))
      (gnosis-test--add-basic-thema "Basic" "A" '("ordinary"))
      (gnosis-export-db out '("ordinary"))
      (let ((export (sqlite-open out)))
        (unwind-protect
            (should (= 1 (caar (sqlite-select export "SELECT COUNT(*) FROM themata"))))
          (sqlite-close export)))
      (should (gnosis-get 'id 'themata `(= id ,model))))))

(defun gnosis-test-model--canvas (_path _view)
  "Create a deterministic stand-in for the optional canvas boundary."
  (let ((buffer (generate-new-buffer " *Gnosis test canvas*")))
    (with-current-buffer buffer
      (special-mode)
      (setq-local canvas-3d--process (make-pipe-process :name "gnosis-model-test" :noquery t))
      (setq-local canvas-3d--frame (list :seq 1 :owner canvas-3d--process))
      (setq-local canvas-3d--status "Ready")
      (setq-local canvas-3d--busy nil)
      (setq-local canvas-3d--dirty nil)
      (setq-local canvas-3d--yaw 0)
      (setq-local canvas-3d--pitch -90)
      (setq-local canvas-3d--zoom 1)
      (setq-local canvas-3d-selection-hook nil)
      (add-hook 'kill-buffer-hook
                (lambda () (when (process-live-p canvas-3d--process)
                             (delete-process canvas-3d--process))) nil t))
    (pop-to-buffer buffer)
    buffer))

(defmacro gnosis-test-model--encounter (&rest input)
  "Run INPUT at the real model encounter's recursive input boundary."
  (declare (indent 0) (debug t))
  `(let ((depth 0))
     (cl-letf (((symbol-function 'gnosis-model-open) #'gnosis-test-model--canvas)
               ((symbol-function 'recursion-depth) (lambda () depth))
               ((symbol-function 'exit-recursive-edit) #'ignore)
               ((symbol-function 'recursive-edit)
                (lambda ()
                  (setq depth 1)
                  (progn
                    (setq-local canvas-3d-selected-id "triangle")
                    (gnosis-review--model-selection
                     (list :id "triangle" :frame 1 :owner canvas-3d--process))
                    ,@input))))
       (gnosis-review--display-thema model))))

(ert-deftest gnosis-model-encounter-restores-split-windows ()
  (gnosis-test-with-db
    (save-window-excursion
      (let* ((model (gnosis-test-model--add))
             (gnosis-review-buffer-name " *Gnosis model windows*")
             (buffer (gnosis-review--setup-buffer (list model) 'practice))
             (other (generate-new-buffer " *Gnosis other window*"))
             (body-height (symbol-function 'window-body-height)))
        (unwind-protect
            (progn
              (delete-other-windows)
              (switch-to-buffer buffer)
              (set-window-buffer (split-window-below) other)
              (let ((configuration (current-window-configuration)))
                ;; Batch frames use character units as pixels.  Model a font
                ;; with 40-pixel lines while retaining real window operations.
                (cl-letf (((symbol-function 'window-body-height)
                           (lambda (&optional window pixelwise)
                             (* (if pixelwise 40 1)
                                (funcall body-height window)))))
                  (dotimes (_ 2)
                    (gnosis-test-model--encounter
                      (should (= 2 (length (window-list))))
                      (should (>= (window-body-height nil t) 512))
                      (should (eq (window-buffer (selected-window)) (current-buffer)))
                      (should (get-buffer-window buffer))
                      (should (= (window-start) (point-min)))
                      (gnosis-review-model-submit))
                    (should (compare-window-configurations
                             configuration (current-window-configuration))))
                  (should-error
                   (gnosis-test-model--encounter
                     (gnosis-review-model-cancel)) :type 'user-error)
                  (should (compare-window-configurations
                           configuration (current-window-configuration)))
                  (cl-letf (((symbol-function 'abort-recursive-edit)
                             (lambda () (signal 'quit nil))))
                    (should
                     (condition-case nil
                         (gnosis-test-model--encounter
                           (gnosis-review-model-cancel))
                       (quit t))))
                  (should (compare-window-configurations
                           configuration (current-window-configuration)))
                  (should-error
                   (gnosis-test-model--encounter (error "Renderer failed")))
                  (should (compare-window-configurations
                           configuration (current-window-configuration)))
                  (cl-letf (((symbol-function 'gnosis-model-open)
                             (lambda (&rest _) (error "Backend unavailable"))))
                    (should-error (gnosis-review--display-thema model)))
                  (should (compare-window-configurations
                           configuration (current-window-configuration))))))
          (kill-buffer buffer)
          (kill-buffer other))))))

(ert-deftest gnosis-model-practice-public-encounter-acceptance-no-fsrs ()
  (gnosis-test-with-db
    (save-window-excursion
      (let* ((model (gnosis-test-model--add))
             (gnosis-review-buffer-name " *Gnosis model encounter*")
             (buffer (gnosis-review--setup-buffer (list model) 'practice))
             (before (gnosis-select '* 'scheduler-state)))
        (unwind-protect
            (with-current-buffer buffer
              (let ((state gnosis-review--state))
                (setf (gnosis-review-state-persistent-p state) t
                      (gnosis-review-state-policy state) (gnosis-review-practice-policy))
                (gnosis-review--save-session state)
                (let* ((display (gnosis-test-model--encounter
                                  (should-not (gnosis-select '* 'practice-events))
                                  (gnosis-review-model-submit)
                                  (should-error (gnosis-review-model-submit))))
                       (pair (cadr display)))
                  (should (equal "model" (car display)))
                  (should (car pair))
                  (should (string-match-p "Answer: Triangle" (buffer-string)))
                  (gnosis-review-result model (car pair) (cdr pair))
                  (gnosis-review-result model (car pair) (cdr pair))
                  (should (= 1 (length (gnosis-select '* 'practice-events))))
                  (should-not (gnosis-review-state-remaining state))
                  (should (= 1 (gnosis-review-state-reviewed state)))
                  (should (equal before (gnosis-select '* 'scheduler-state)))
                  (should-not (gnosis-select '* 'review-events)))))
          (kill-buffer buffer))))))

(ert-deftest gnosis-model-submit-rejects-stale-and-inflight-input ()
  (gnosis-test-with-db
    (save-window-excursion
      (let* ((model (gnosis-test-model--add))
             (gnosis-review-buffer-name " *Gnosis model stale*")
             (buffer (gnosis-review--setup-buffer (list model) 'practice)))
        (unwind-protect
            (with-current-buffer buffer
              (should-error
               (gnosis-test-model--encounter
                 (setq canvas-3d--busy t)
                 (should-error (gnosis-review-model-submit))
                 (setq canvas-3d--busy nil canvas-3d--dirty t)
                 (gnosis-review--model-selection
                  (list :id "triangle" :frame 1 :owner canvas-3d--process))
                 (setq canvas-3d--dirty nil)
                 (should-error (gnosis-review-model-submit))
                 (gnosis-review--model-selection
                  (list :id "triangle" :frame 1 :owner canvas-3d--process))
                 (setq canvas-3d--yaw 10)
                 (should-error (gnosis-review-model-submit))
                 (setq canvas-3d--yaw 0)
                 (let ((gnosis-db (gnosis-sqlite-open (expand-file-name "other.db" gnosis-dir))))
                   (unwind-protect (should-error (gnosis-review-model-submit))
                     (gnosis-sqlite-close gnosis-db)))
                 (gnosis-update 'themata '(= keimenon "Changed") `(= id ,model))
                 (should-error (gnosis-review-model-submit))
                 (setf (plist-get gnosis-review--model-context :cancelled) t)
                 (should-error (gnosis-review-model-submit))))
              (should-not (gnosis-select '* 'practice-events))
              (should-not (gnosis-select '* 'review-events)))
          (kill-buffer buffer))))))

(ert-deftest gnosis-model-missing-backend-is-optional ()
  (gnosis-test-with-db
    (let ((gnosis-model-renderer-directory nil)
          (original-require (symbol-function 'require)))
      (cl-letf (((symbol-function 'require)
                 (lambda (feature &rest args)
                   (unless (eq feature 'canvas-3d)
                     (apply original-require feature args)))))
        (should-error (gnosis-model-open "/unused" '(0 0 1)) :type 'user-error))
      (should (gnosis-test--add-basic-thema "Still works" "Yes")))))


(ert-deftest gnosis-model-authoring-pins-before-target-prompts ()
  (gnosis-test-with-db
    (let* ((file (gnosis-test-model--scene))
           (original (gnosis-model-import file)))
      (cl-letf (((symbol-function 'read-file-name) (lambda (&rest _) file))
                ((symbol-function 'read-string) (lambda (&rest _) ""))
                ((symbol-function 'completing-read)
                 (lambda (_ choices &rest _)
                   (with-temp-file (expand-file-name "triangle.obj" (file-name-directory file))
                     (insert "Changed after scene choice"))
                   (caar choices)))
                ((symbol-function 'read-number) (lambda (_ value) value)))
        (let ((fields (gnosis-model--read-fields)))
          (should (equal original (caar fields)))
          (should (apply #'gnosis-model-resolve fields))
          (should-not (gnosis-select '* 'themata)))))))

(ert-deftest gnosis-model-authoring-corrupt-managed-prompt-refuses ()
  (gnosis-test-with-db
    (let* ((file (gnosis-test-model--scene))
           (reference (gnosis-model-import file)))
      (cl-letf (((symbol-function 'read-file-name) (lambda (&rest _) file))
                ((symbol-function 'read-string) (lambda (&rest _) ""))
                ((symbol-function 'completing-read) (lambda (_ choices &rest _) (caar choices)))
                ((symbol-function 'read-number)
                 (lambda (_ value)
                   (with-temp-file (expand-file-name reference (gnosis-model--root)))
                   value)))
        (should-error (gnosis-add-model-thema))
        (should-not (get-buffer "*Gnosis NEW*"))
        (should-not (gnosis-select '* 'themata))))))

(ert-deftest gnosis-model-resource-drift-after-submit-blocks-acceptance ()
  (gnosis-test-with-db
    (save-window-excursion
      (let* ((model (gnosis-test-model--add))
             (gnosis-review-buffer-name " *Gnosis model acceptance*")
             (buffer (gnosis-review--setup-buffer (list model) 'practice)))
        (unwind-protect
            (with-current-buffer buffer
              (let* ((pair (cadr (gnosis-test-model--encounter (gnosis-review-model-submit))))
                     (hypothesis (gnosis-get 'hypothesis 'themata `(= id ,model)))
                     (path (car (gnosis-model-resolve hypothesis '("triangle")))))
                (delete-file path)
                (should-error (gnosis-review-result model (car pair) (cdr pair)))
                (should-not (gnosis-select '* 'practice-events))
                (should-not (gnosis-select '* 'review-events))))
          (kill-buffer buffer))))))

(ert-deftest gnosis-model-renderer-retirement-and-session-change-reject ()
  (gnosis-test-with-db
    (save-window-excursion
      (let* ((model (gnosis-test-model--add))
             (gnosis-review-buffer-name " *Gnosis model retire*")
             (buffer (gnosis-review--setup-buffer (list model) 'practice)))
        (unwind-protect
            (with-current-buffer buffer
              (should-error
               (gnosis-test-model--encounter
                 (gnosis-review--model-check gnosis-review--model-context)
                 (should-not (string-match-p "Triangle" (gnosis-review--model-header)))
                 (delete-process canvas-3d--process)
                 (setq-local canvas-3d--status "Injected renderer failure")
                 (should (string-match-p "Unavailable.*Injected renderer failure"
                                         (gnosis-review--model-header)))
                 (should-error (gnosis-review-model-submit))))
              (should-error
               (gnosis-test-model--encounter
                 (with-current-buffer buffer
                   (setf (gnosis-review-state-event-id gnosis-review--state) "next-encounter"))
                 (should-error (gnosis-review-model-submit))))
              (should-not (gnosis-select '* 'practice-events)))
          (kill-buffer buffer))))))

(ert-deftest gnosis-model-authoring-attach-roundtrip ()
  (gnosis-test-with-db
    (let ((file (gnosis-test-model--scene)))
      (with-temp-buffer
        (gnosis-edit-mode)
        (gnosis-export--insert-thema "NEW" "model" "Preserved question" nil nil "Preserved note")
        (cl-letf (((symbol-function 'read-file-name) (lambda (&rest _) file))
                  ((symbol-function 'read-string) (lambda (&rest _) ""))
                  ((symbol-function 'completing-read) (lambda (_ choices &rest _) (caar choices)))
                  ((symbol-function 'read-number) (lambda (_ value) value)))
          (call-interactively (key-binding (kbd "C-c C-a"))))
        (let ((thema (car (gnosis-export-parse-themata))))
          (should (equal "Preserved question" (nth 2 thema)))
          (should (equal "Preserved note" (nth 5 thema)))
          (should-not (gnosis-save-thema thema))
          (should (equal "model" (gnosis-get 'type 'themata))))))))


(ert-deftest gnosis-model-wrong-selection-feedback-and-real-cancel ()
  (gnosis-test-with-db
    (save-window-excursion
      (let* ((model (gnosis-test-model--add))
             (gnosis-review-buffer-name " *Gnosis model wrong*")
             (buffer (gnosis-review--setup-buffer (list model) 'practice))
             (before (gnosis-select '* 'scheduler-state)))
        (unwind-protect
            (with-current-buffer buffer
              (let* ((display
                      (gnosis-test-model--encounter
                        (setq-local canvas-3d-selected-id "other")
                        (gnosis-review--model-selection
                         (list :id "other" :frame 1 :owner canvas-3d--process))
                        (should-not (string-match-p "Other triangle" (gnosis-review--model-header)))
                        (gnosis-review-model-submit)))
                     (pair (cadr display)))
                (should-not (car pair))
                (should (string-match-p "Answer: Triangle" (buffer-string)))
                (should (string-match-p "Your answer: Other triangle" (buffer-string)))
                (gnosis-review-result model nil (cdr pair))
                (should (= 1 (gnosis-get 'rating 'practice-events)))
                (should (equal before (gnosis-select '* 'scheduler-state)))
                (should-not (gnosis-select '* 'review-events)))
              (cl-letf (((symbol-function 'abort-recursive-edit)
                         (lambda () (signal 'quit nil))))
                (should (eq 'quit
                            (condition-case err
                                (gnosis-test-model--encounter (gnosis-review-model-cancel))
                              (quit (car err))))))
              (should (= 1 (length (gnosis-select '* 'practice-events)))))
          (kill-buffer buffer))))))

(defmacro gnosis-test-model--session (mode &rest body)
  "Run BODY in a disposable persistent model session with MODE."
  (declare (indent 1) (debug t))
  `(gnosis-test-with-db
     (save-window-excursion
       (let* ((model (gnosis-test-model--add))
              (gnosis-review-buffer-name " *Gnosis scheduled model*")
              (buffer (gnosis-review--setup-buffer (list model) ,mode)))
         (unwind-protect
             (with-current-buffer buffer
               (setf (gnosis-review-state-persistent-p gnosis-review--state) t)
               (gnosis-review--save-session gnosis-review--state)
               ,@body)
           (kill-buffer buffer))))))

(ert-deftest gnosis-model-scheduled-native-acceptance-and-replay ()
  (dolist (target '("triangle" "other"))
    (gnosis-test-model--session 'due
      (let* ((before (gnosis-select '* 'scheduler-state))
             (pair (cadr
                    (gnosis-test-model--encounter
                      (setq-local canvas-3d-selected-id target)
                      (gnosis-review--model-selection
                       (list :id target :frame 1 :owner canvas-3d--process))
                      (should (eq (key-binding (kbd "RET")) #'gnosis-review-model-submit))
                      (call-interactively (key-binding (kbd "RET"))))))
             (result (cdr pair)))
        (should (eq (car pair) (equal target "triangle")))
        (should-not (gnosis-select '* 'review-events))
        (should (equal before (gnosis-select '* 'scheduler-state)))
        (cl-letf (((symbol-function 'read-char-choice) (lambda (&rest _) ?n)))
          (should (plist-get (gnosis-review-actions (car pair) model result) :inserted-p)))
        (let ((events (gnosis-select '* 'review-events))
              (state (gnosis-select '* 'scheduler-state))
              (session (gnosis-select '* 'study-session)))
          (should (= 1 (length events)))
          (should (= (if (car pair) 3 1) (gnosis-get 'rating 'review-events)))
          (should (= 1 (gnosis-get 'reps 'scheduler-state)))
          (should (= (if (car pair) 0 1) (gnosis-get 'lapses 'scheduler-state)))
          (should-not (equal before state))
          (should-not (plist-get (gnosis-review-result model (car pair) result) :inserted-p))
          (should (equal events (gnosis-select '* 'review-events)))
          (should (equal state (gnosis-select '* 'scheduler-state)))
          (should (equal session (gnosis-select '* 'study-session)))
          (should-error (gnosis-review-result model (not (car pair)) result))
          (gnosis-scheduler-rebuild-state model 0)
          (should (equal state (gnosis-select '* 'scheduler-state))))
        (should-not (gnosis-select '* 'practice-events))))))

(ert-deftest gnosis-model-pending-override-retains-resource-guard ()
  (gnosis-test-model--session 'practice
    (let* ((pair (cadr (gnosis-test-model--encounter (gnosis-review-model-submit))))
           (result (append (gnosis-review--pending-result model t)
                           (list :model (plist-get (cdr pair) :model))))
           (overridden (gnosis-review--override-result result nil)))
      (should (equal (plist-get result :model) (plist-get overridden :model))))))

(ert-deftest gnosis-model-scheduled-post-submit-drift-fails-closed ()
  (dolist (override '(nil t))
    (dolist (drift '(thema resource database session state mode schedule retention))
      (gnosis-test-model--session 'due
        (let* ((pair (cadr (gnosis-test-model--encounter (gnosis-review-model-submit))))
               (result (if override (gnosis-review--override-result (cdr pair) nil) (cdr pair))))
          (pcase drift
            ('thema (gnosis-update 'themata '(= keimenon "Changed") `(= id ,model)))
            ('resource
             (delete-file (car (gnosis-model-resolve
                                (gnosis-get 'hypothesis 'themata `(= id ,model)) '("triangle")))))
            ('session (setq gnosis-review--state (copy-gnosis-review-state gnosis-review--state)))
            ('state (setf (gnosis-review-state-event-id gnosis-review--state) "next"))
            ('mode (setf (gnosis-review-state-mode gnosis-review--state) 'practice))
            ('schedule (gnosis-update 'scheduler-state '(= lapses 1) `(= thema-id ,model)))
            ('retention (gnosis-scheduler-set-retention 0.85)))
          (let ((before (gnosis-select '* 'scheduler-state)))
            (if (eq drift 'database)
                (let ((gnosis-db (gnosis-sqlite-open gnosis-test--db-file)))
                  (unwind-protect (should-error (gnosis-review-result model (not override) result))
                    (gnosis-sqlite-close gnosis-db)))
              (should-error (gnosis-review-result model (not override) result)))
            (should (equal before (gnosis-select '* 'scheduler-state)))
            (should-not (gnosis-select '* 'review-events))
            (should-not (gnosis-select '* 'practice-events))))))))

(ert-deftest gnosis-model-scheduled-unavailable-or-unsubmitted-never-grades ()
  (dolist (failure '(cancel no-submit backend asset))
    (gnosis-test-model--session 'due
      (let ((before (gnosis-select '* 'scheduler-state)))
        (pcase failure
          ('cancel
           (cl-letf (((symbol-function 'abort-recursive-edit) (lambda () (signal 'quit nil))))
             (should (condition-case nil
                         (gnosis-test-model--encounter (gnosis-review-model-cancel))
                       (quit t)))))
          ('no-submit (should-error (gnosis-test-model--encounter nil)))
          ('backend
           (cl-letf (((symbol-function 'gnosis-model-open) (lambda (&rest _) (user-error "Unavailable"))))
             (should-error (gnosis-review--display-thema model))))
          ('asset
           (delete-file (car (gnosis-model-resolve
                              (gnosis-get 'hypothesis 'themata `(= id ,model)) '("triangle"))))
           (should-error (gnosis-test-model--encounter (gnosis-review-model-submit)))))
        (should (equal before (gnosis-select '* 'scheduler-state)))
        (should-not (gnosis-select '* 'review-events))
        (should-not (gnosis-select '* 'practice-events))))))

(ert-deftest gnosis-model-scheduled-override-binary-policy-and-preview ()
  (dolist (success '(nil t))
    (gnosis-test-model--session 'due
      (let* ((pair (cadr (gnosis-test-model--encounter (gnosis-review-model-submit))))
             (original (cdr pair))
             (result (gnosis-review--override-result original success)))
        (should (equal (plist-get original :event-id) (plist-get result :event-id)))
        (should (equal (plist-get original :model) (plist-get result :model)))
        (should (eq (plist-get original :outcome) 'success))
        (should (equal (if success 3 1) (plist-get (plist-get result :preview) :rating)))
        (should (string-match-p "Next review:" (buffer-string)))
        (should-not (string-match-p "Next review:.*nil" (buffer-string)))
        (should (equal (gnosis-review--result-date original)
                       (gnosis--int-to-date (plist-get (plist-get original :preview) :due-day))))
        (gnosis-review-result model success result)
        (should (= (if success 3 1) (gnosis-get 'rating 'review-events)))
        (should-not (gnosis-select '* 'practice-events))))))

(ert-deftest gnosis-model-nonpersistent-post-submit-owner-drift ()
  (dolist (mode '(due practice))
    (dolist (drift '(buffer state event))
      (gnosis-test-model--session mode
        (setf (gnosis-review-state-persistent-p gnosis-review--state) nil)
        (let* ((before (gnosis-select '* 'scheduler-state))
               (pair (cadr (gnosis-test-model--encounter (gnosis-review-model-submit)))))
          (pcase drift
            ('state (setq gnosis-review--state (copy-gnosis-review-state gnosis-review--state)))
            ('event (setf (gnosis-review-state-event-id gnosis-review--state) "next")))
          (if (eq drift 'buffer)
              (let ((state gnosis-review--state))
                (with-temp-buffer
                  (setq-local gnosis-review--state state)
                  (should-error (gnosis-review-result model t (cdr pair)))))
            (should-error (gnosis-review-result model t (cdr pair))))
          (should (equal before (gnosis-select '* 'scheduler-state)))
          (should-not (gnosis-select '* 'review-events))
          (should-not (gnosis-select '* 'practice-events)))))))

(provide 'gnosis-test-model)
;;; gnosis-test-model.el ends here

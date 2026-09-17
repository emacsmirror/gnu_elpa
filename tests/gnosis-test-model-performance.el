;;; gnosis-test-model-performance.el --- Model preparation regressions -*- lexical-binding: t; -*-

(require 'gnosis-model-test-support)

(ert-deftest gnosis-model-review-does-not-reparse-prepared-geometry ()
  (gnosis-test-with-db
    (save-window-excursion
      (let* ((model (gnosis-test-model--add))
             (buffer (gnosis-review--setup-buffer (list model) 'practice))
             (parse (symbol-function 'gnosis-model--geometry))
             (parses 0))
        (unwind-protect
            (with-current-buffer buffer
              (cl-letf (((symbol-function 'gnosis-model--geometry)
                         (lambda (file) (cl-incf parses) (funcall parse file))))
                (let* ((display (gnosis-test-model--encounter
                                  (gnosis-review-model-submit)))
                       (pair (cadr display)))
                  (gnosis-review-result model (car pair) (cdr pair))
                  ;; All topology preparation belongs to a child, never input,
                  ;; picking, submission, or acceptance in the editor.
                  (should (= 0 parses)))))
          (kill-buffer buffer))))))

(ert-deftest gnosis-model-preparation-validates-topology-once-and-detects-mutation ()
  (gnosis-test-with-db
    (let* ((model (gnosis-test-model--add))
           (row (car (gnosis-review--answer-thema model)))
           (parser (symbol-function 'gnosis-model--geometry))
           (parses 0))
      (cl-letf (((symbol-function 'gnosis-model--geometry)
                 (lambda (file) (cl-incf parses) (funcall parser file))))
        (let* ((fields (gnosis-model-fields (nth 0 row) (nth 2 row) (nth 3 row)))
               (verified (plist-get fields :verified)))
          (should (= parses (length (alist-get 'objects (plist-get verified :manifest)))))
          (should (gnosis-model-check-fields fields))
          (should (= parses (length (plist-get verified :counts))))))
      (cl-letf (((symbol-function 'gnosis-model--geometry)
                 (lambda (file)
                   (prog1 (funcall parser file)
                     (write-region "\n# mutated during validation\n" nil file t 'silent)))))
        (should-error (gnosis-model-fields (nth 0 row) (nth 2 row) (nth 3 row))
                      :type 'user-error)))))

(ert-deftest gnosis-model-loading-public-cancel-retires-child-and-preserves-session ()
  (gnosis-test-with-db
    (save-window-excursion
      (let* ((model (gnosis-test-model--add))
             (buffer (gnosis-review--setup-buffer (list model) 'practice))
             (before (gnosis-select '* 'scheduler-state))
             job context)
        (unwind-protect
            (with-current-buffer buffer
              (dolist (key '("q" "C-g"))
                (let ((depth 0) (map (current-local-map)) (header header-line-format)
                      (snapshot (gnosis-review--state-data gnosis-review--state)))
                  (cl-letf (((symbol-function 'recursion-depth) (lambda () depth))
                            ((symbol-function 'abort-recursive-edit) (lambda () (signal 'quit nil)))
                            ((symbol-function 'recursive-edit)
                             (lambda ()
                               (setq depth 1 context gnosis-review--model-context
                                     job (plist-get context :preparation))
                               (should (process-live-p (plist-get job :process)))
                               (should (integerp (process-id (plist-get job :process))))
                               (should (string-match-p "Model  Loading…" (gnosis-review--model-header)))
                               (let ((case-fold-search nil))
                                 (should-not (string-match-p "Triangle" (buffer-string))))
                               (should-error (gnosis-review-model-submit) :type 'user-error)
                               (call-interactively (key-binding (kbd key))))))
                    (should (condition-case nil (gnosis-review-model model) (quit t))))
                  (should-not gnosis-review--model-context)
                  (should (eq map (current-local-map)))
                  (should (equal header header-line-format))
                  (should (equal snapshot (gnosis-review--state-data gnosis-review--state)))
                  (should-not (process-live-p (plist-get job :process)))
                  (should-not (buffer-live-p (plist-get job :output)))
                  (should-not (buffer-live-p (plist-get job :errors)))
                  ;; A queued completion after cancellation cannot install anything.
                  (gnosis-review--model-prepared context nil "Late result")
                  (should-not gnosis-review--model-context)))
              (should (equal before (gnosis-select '* 'scheduler-state)))
              (should-not (gnosis-select '* 'practice-events))
              (should-not (gnosis-select '* 'review-events)))
          (kill-buffer buffer))))))

(ert-deftest gnosis-model-loading-resource-and-owner-mutations-fail-closed ()
  (dolist (mutation '(asset thema state database mode successor killed))
    (gnosis-test-with-db
      (save-window-excursion
        (let* ((model (gnosis-test-model--add))
               (buffer (gnosis-review--setup-buffer (list model) 'practice))
               (db gnosis-db)
               (row (car (gnosis-review--answer-thema model)))
               (fields (gnosis-model-fields (nth 0 row) (nth 2 row) (nth 3 row)))
               (successor (list :successor t))
               opened context job)
          (unwind-protect
              (with-current-buffer buffer
                (cl-letf (((symbol-function 'gnosis-model-open)
                           (lambda (&rest _) (setq opened t)))
                          ((symbol-function 'recursive-edit)
                           (lambda ()
                             (setq context gnosis-review--model-context
                                   job (plist-get context :preparation))
                             (pcase mutation
                               ('asset (write-region "\n" nil (plist-get fields :scene) t 'silent))
                               ('thema (gnosis-update 'themata '(= keimenon "Changed") `(= id ,model)))
                               ('state (setf (gnosis-review-state-remaining gnosis-review--state) nil))
                               ('database (setq gnosis-db nil))
                               ('mode (fundamental-mode))
                               ('successor (setq gnosis-review--model-context successor))
                               ('killed (kill-buffer buffer)))
                             ;; Model the real child result arriving after the mutation.
                             (gnosis-review--model-prepared context fields nil)
                             (should-not opened)
                             (should-not (plist-get context :fields))
                             (when (eq mutation 'successor)
                               (should (eq successor gnosis-review--model-context))))))
                  (should-error (gnosis-review-model model)))
                (should-not (plist-get context :result)))
            (when (and gnosis-db (not (eq gnosis-db db)))
              (sqlite-close gnosis-db))
            (setq gnosis-db db)
            (gnosis-model-cancel-preparation job)
            (when (buffer-live-p buffer)
              (with-current-buffer buffer (setq gnosis-review--model-context nil))
              (kill-buffer buffer)))
          (should-not opened)
          (should-not (process-live-p (plist-get job :process)))
          (should-not (gnosis-select '* 'practice-events))
          (should-not (gnosis-select '* 'review-events)))))))

(ert-deftest gnosis-model-loading-native-command-loop-remains-cancellable ()
  (skip-unless (memq system-type '(gnu/linux darwin berkeley-unix)))
  (gnosis-test-with-db
    (save-window-excursion
      (let* ((model (gnosis-test-model--add))
             (buffer (gnosis-review--setup-buffer (list model) 'practice))
             (make-child (symbol-function 'make-process)))
        (unwind-protect
            (progn
              (switch-to-buffer buffer)
              (dolist (key '(?q ?\C-g))
                (let ((map (copy-keymap (current-local-map)))
                      (unread-command-events nil)
                      command-ran timed-out job send watchdog)
                  (define-key map "z"
                    (lambda ()
                      (interactive)
                      (setq command-ran t job (plist-get gnosis-review--model-context :preparation)
                            send (run-at-time 0.02 nil
                                              (lambda () (setq unread-command-events (list key)))))))
                  (use-local-map map)
                  (unwind-protect
                      (cl-letf (((symbol-function 'make-process)
                                 (lambda (&rest args)
                                   (let ((process (apply make-child args)))
                                     ;; Hold the real child, not Emacs, at a deterministic
                                     ;; preparation boundary until a native key cancels it.
                                     (when (equal (plist-get args :name) "gnosis-model-prepare")
                                       (signal-process process 'SIGSTOP))
                                     process))))
                        (setq send (run-at-time 0.02 nil
                                                (lambda () (setq unread-command-events (list ?z))))
                              watchdog (run-at-time 3 nil
                                                    (lambda () (setq timed-out t)
                                                      (abort-recursive-edit))))
                        ;; Do not mock recursive-edit or dispatch the commands by hand.
                        (should (condition-case nil (gnosis-review-model model) (quit t)))
                        (should command-ran)
                        (should-not timed-out)
                        (should-not (process-live-p (plist-get job :process)))
                        (should-not gnosis-review--model-context))
                    (when send (cancel-timer send))
                    (when watchdog (cancel-timer watchdog)))))
              (should-not (gnosis-select '* 'practice-events))
              (should-not (gnosis-select '* 'review-events)))
          (when (buffer-live-p buffer) (kill-buffer buffer)))))))

(ert-deftest gnosis-model-prepared-uses-displayed-owner-without-stealing-focus ()
  (dolist (delivery '(other minibuffer hidden cancel successor))
    (gnosis-test-with-db
      (save-window-excursion
        (let* ((model (gnosis-test-model--add))
               (owner (gnosis-review--setup-buffer (list model) 'practice))
               (other (generate-new-buffer " *model-other*"))
               (window (selected-window))
               (row (car (gnosis-review--answer-thema model)))
               (fields (gnosis-model-fields (nth 0 row) (nth 2 row) (nth 3 row)))
               (successor (list :successor t))
               opened context retry)
          (unwind-protect
              (progn
                (switch-to-buffer owner)
                (cl-letf (((symbol-function 'gnosis-model-prepare) (lambda (&rest _) nil))
                          ((symbol-function 'window-body-width)
                           (lambda (&optional win &rest _)
                             (if (eq (or win (selected-window)) window) 800 80)))
                          ((symbol-function 'window-body-height)
                           (lambda (&optional win &rest _)
                             (if (eq (or win (selected-window)) window) 600 24)))
                          ((symbol-function 'frame-char-height) (lambda (&rest _) 16))
                          ((symbol-function 'gnosis-model-open)
                           (lambda (_scene _view size &rest _)
                             (should (eq (selected-window) window))
                             (should (eq (current-buffer) owner))
                             (should
                              (= size (min 568 (max 128
                                                   (- 600 (* (+ 5 (count-lines (point-min) (point-max)))
                                                             16))))))
                             (setq-local canvas-3d--process nil
                                         canvas-3d--image nil
                                         canvas-3d-mode-map (make-sparse-keymap))
                             (setq opened t)))
                          ((symbol-function 'recursive-edit)
                           (lambda ()
                             (setq context gnosis-review--model-context)
                             (let ((focus (if (eq delivery 'minibuffer)
                                              (minibuffer-window)
                                            (split-window window nil 'right))))
                               (unless (eq delivery 'minibuffer)
                                 (set-window-buffer focus other))
                               (select-window focus)
                               (insert "Unrelated input")
                               (when (memq delivery '(hidden cancel successor))
                                 (set-window-buffer window other))
                               (let ((point-before (point))
                                     (text-before (buffer-string))
                                     (owner-point (with-current-buffer owner (point))))
                                 (gnosis-review--model-prepared context fields nil)
                                 (should-not (plist-get context :error))
                                 (when (memq delivery '(hidden cancel successor))
                                   (should-not opened)
                                   (setq retry (plist-get context :display-timer))
                                   (should (timerp retry))
                                   (with-current-buffer owner
                                     (pcase delivery
                                       ('cancel (gnosis-review-model-cancel))
                                       ('successor (setq gnosis-review--model-context successor))))
                                   (set-window-buffer window owner)
                                   ;; Deliver even a cancelled, already queued retry.
                                   (apply (timer--function retry) (timer--args retry)))
                                 (should (eq opened (not (null (memq delivery '(other minibuffer hidden))))))
                                 (when opened (should (eq fields (plist-get context :fields))))
                                 (should (eq (selected-window) focus))
                                 (should (= (point) point-before))
                                 (should (equal (buffer-string) text-before))
                                 (should (= (with-current-buffer owner (point)) owner-point))
                                 (when (eq delivery 'successor)
                                   (should (eq (buffer-local-value 'gnosis-review--model-context owner) successor))))))))
                  (should-error (gnosis-review-model model) :type 'user-error)))
            (when (timerp retry) (cancel-timer retry))
            (when (buffer-live-p owner)
              (with-current-buffer owner (setq gnosis-review--model-context nil))
              (kill-buffer owner))
            (kill-buffer other)))))))

(provide 'gnosis-test-model-performance)
;;; gnosis-test-model-performance.el ends here

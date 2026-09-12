;;; gnosis-test-review-layout.el --- Review window layout tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'gnosis-review)

(defmacro gnosis-test-review-layout--with-buffer (&rest body)
  "Evaluate BODY in an isolated, visible review buffer."
  (declare (indent 0) (debug t))
  `(save-window-excursion
     (delete-other-windows)
     (let* ((buffer (generate-new-buffer " *gnosis-layout-test*"))
            (gnosis-review-buffer-name (buffer-name buffer))
            (gnosis-review--running nil))
       (unwind-protect
           (progn
             (switch-to-buffer buffer)
             (gnosis-mode)
             ,@body)
         (kill-buffer buffer)))))

(defun gnosis-test-review-layout--prefix (window position)
  "Return the review line prefix for WINDOW at POSITION."
  (seq-some (lambda (overlay)
              (when (eq window (overlay-get overlay 'window))
                (overlay-get overlay 'line-prefix)))
            (overlays-at position)))

(ert-deftest gnosis-review-layout-resize-retains-text-and-reading-state ()
  (gnosis-test-review-layout--with-buffer
    (let ((text (concat "Short question\n"
                        (mapconcat #'identity (make-list 30 "wide Ελληνικά") " "))))
      (gnosis-display-keimenon text)
      ;; Initial filling keeps the established readable measure.  Resize
      ;; must not bake in padding or refill this rendered text.
      (should (string-prefix-p "\nShort question\nwide" (buffer-string)))
      (should (seq-every-p (lambda (line) (<= (string-width line) fill-column))
                           (split-string (buffer-string) "\n")))
      (should word-wrap)
      (should-not truncate-lines)

      (let* ((left (selected-window))
             (before (buffer-string))
             (wide-prefix (gnosis-test-review-layout--prefix left 2))
             (right (split-window-right)))
        (set-window-buffer right (current-buffer))
        (goto-char 8)
        (set-mark 5)
        (set-window-point right 12)
        (set-window-start right 2 t)
        (let ((point (point)) (mark (mark))
              (right-point (window-point right))
              (right-start (window-start right)))
          ;; Batch redisplay does not deliver configuration hooks.  Exercise
          ;; the installed callback here; a native lab proves event delivery.
          (run-hooks 'window-configuration-change-hook)
          (should (equal-including-properties before (buffer-string)))
          (should (= point (point)))
          (should (= mark (mark)))
          (should (= right-point (window-point right)))
          (should (= right-start (window-start right)))
          (should (eq left (selected-window)))
          (should-not (equal wide-prefix
                             (gnosis-test-review-layout--prefix left 2)))
          (delete-window right)
          (run-hooks 'window-configuration-change-hook)
          (should (equal wide-prefix
                         (gnosis-test-review-layout--prefix left 2))))))))

(ert-deftest gnosis-review-layout-independent-windows-and-hidden-buffer ()
  (gnosis-test-review-layout--with-buffer
    (gnosis-display-keimenon "Question")
    (let* ((review (current-buffer))
           (left (selected-window))
           (right (split-window-right 30))
           (draft (generate-new-buffer " *gnosis-layout-draft*")))
      (unwind-protect
          (progn
            (set-window-buffer right review)
            (run-hooks 'window-configuration-change-hook)
            (should-not (equal (gnosis-test-review-layout--prefix left 2)
                               (gnosis-test-review-layout--prefix right 2)))
            (set-window-buffer right draft)
            (select-window right)
            (insert "Unsubmitted draft")
            (goto-char 6)
            (set-mark 3)
            (let ((text (buffer-string)) (point (point)) (mark (mark)))
              (window-resize left 5 t)
              (with-current-buffer review
                (run-hooks 'window-configuration-change-hook))
              (should (eq right (selected-window)))
              (should (eq draft (current-buffer)))
              (should (= point (point)))
              (should (= mark (mark)))
              (should (equal text (buffer-string))))
            (set-window-buffer left draft)
            (with-current-buffer review
              (run-hooks 'window-configuration-change-hook)
              (should-not gnosis-review--layout-overlays))
            (set-window-buffer left review)
            (with-current-buffer review
              (run-hooks 'window-configuration-change-hook)
              (should (gnosis-test-review-layout--prefix left 2))))
        (kill-buffer draft)))))

(ert-deftest gnosis-review-layout-preserves-feedback-media-and-encounter ()
  (gnosis-test-review-layout--with-buffer
    (let* ((media (propertize " " 'display '(space :width 5)
                             'gnosis-image-mask '(retained-mask)))
           (state (gnosis-review-state-create :session-id "session"
                                             :event-id "encounter"
                                             :remaining '("thema")))
           (context (list :input "Draft answer" :selection '(target)))
           (gnosis-latex-preview nil))
      (setq gnosis-review--state state)
      (setq-local gnosis-review--model-context context)
      (gnosis-display-cloze-string "The capital is Athens" '("Athens")
                                   nil '("Athens") nil)
      (gnosis-display-hint "Think of Greece")
      (gnosis-display-basic-answer "Athens" nil "Rome")
      (gnosis-display-cloze-user-answer "Athens")
      (gnosis-display-parathema "An explanation")
      (goto-char (point-max))
      (insert "\n" media)
      (let ((before (buffer-string))
            (tick (buffer-chars-modified-tick))
            (undo buffer-undo-list)
            (overlays gnosis-review--layout-overlays))
        ;; Unrelated configuration changes do not rebuild the same layout.
        (run-hooks 'window-configuration-change-hook)
        (should (eq overlays gnosis-review--layout-overlays))
        (split-window-right)
        (cl-letf (((symbol-function 'gnosis--ensure-db)
                   (lambda (&rest _) (ert-fail "Resize touched the database")))
                  ((symbol-function 'read-string)
                   (lambda (&rest _) (ert-fail "Resize restarted input")))
                  ((symbol-function 'gnosis-display-keimenon)
                   (lambda (&rest _) (ert-fail "Resize reset the encounter"))))
          (run-hooks 'window-configuration-change-hook))
        (should (equal-including-properties before (buffer-string)))
        (should (= tick (buffer-chars-modified-tick)))
        (should (eq undo buffer-undo-list))
        (should (eq state gnosis-review--state))
        (should (eq context gnosis-review--model-context))
        (should (equal (plist-get context :input) "Draft answer"))
        (should (text-property-any (point-min) (point-max)
                                   'face 'gnosis-face-correct))
        (should (text-property-any (point-min) (point-max)
                                   'face 'gnosis-face-false))
        (should (equal '(retained-mask)
                       (get-text-property (1- (point-max)) 'gnosis-image-mask)))
        (should-not (gnosis-test-review-layout--prefix
                     (selected-window) (1- (point-max))))))
    ;; The fixture context is not an active model encounter.
    (setq gnosis-review--model-context nil)))

(ert-deftest gnosis-review-layout-disabled-and-mode-cleanup ()
  (gnosis-test-review-layout--with-buffer
    (setq-local gnosis-center-content nil)
    (gnosis-display-keimenon "Question")
    (split-window-right)
    (run-hooks 'window-configuration-change-hook)
    (should-not gnosis-review--layout-overlays)
    (should (string-prefix-p "\nQuestion\n" (buffer-string)))
    (setq-local gnosis-center-content t)
    (run-hooks 'window-configuration-change-hook)
    (should gnosis-review--layout-overlays)
    (let ((overlays gnosis-review--layout-overlays))
      (fundamental-mode)
      (should-not (seq-some #'overlay-buffer overlays))
      (should-not (memq #'gnosis-review--refresh-layout
                        window-configuration-change-hook))
      (should-not (memq #'gnosis-review--refresh-layout
                        after-change-functions)))))

(ert-deftest gnosis-review-layout-wide-frame-public-split-and-resize ()
  (let ((width (frame-width)))
    (unwind-protect
        (progn
          (set-frame-width nil 227)
          (gnosis-test-review-layout--with-buffer
            (gnosis-display-keimenon
             (mapconcat #'identity (make-list 50 "A readable question") " "))
            (let ((before (buffer-string))
                  (left (selected-window))
                  (parameters (window-parameters)))
              (should (<= (apply #'max (mapcar #'string-width
                                               (split-string before "\n")))
                          fill-column))
              (call-interactively #'split-window-right)
              (run-hooks 'window-configuration-change-hook)
              (should (equal parameters (window-parameters left)))
              (should (equal '(nil) (window-margins left)))
              (window-resize left (- 35 (window-total-width left)) t)
              (run-hooks 'window-configuration-change-hook)
              ;; A filled logical line longer than the narrow window has
              ;; no centering prefix: native wrapping gets its full width.
              (should-not (gnosis-test-review-layout--prefix left 2))
              (window-resize left 20 t)
              (run-hooks 'window-configuration-change-hook)
              (delete-other-windows left)
              (run-hooks 'window-configuration-change-hook)
              (should (gnosis-test-review-layout--prefix left 2))
              (should (equal-including-properties before (buffer-string))))))
      (set-frame-width nil width))))

(ert-deftest gnosis-review-layout-narrow-windows-wrap-with-or-without-centering ()
  (dolist (center '(t nil))
    (gnosis-test-review-layout--with-buffer
      (setq-local gnosis-center-content center)
      ;; The default partial-window policy otherwise truncates below 50
      ;; columns even when `truncate-lines' is nil and `word-wrap' is t.
      (setq-local truncate-partial-width-windows 50)
      (gnosis-display-keimenon
       "A question with enough words to require wrapping in a narrow window.")
      (gnosis-display-basic-answer
       "A long answer whose final words must remain visible." t "")
      (goto-char 8)
      (set-mark 4)
      (let ((before (buffer-string))
            (point (point)) (mark (mark))
            (state gnosis-review--state))
        (split-window-right 35)
        (run-hooks 'window-configuration-change-hook)
        (should-not (truncated-partial-width-window-p))
        (should (equal-including-properties before (buffer-string)))
        (should (= point (point)))
        (should (= mark (mark)))
        (should (eq state gnosis-review--state))))))

(provide 'gnosis-test-review-layout)
;;; gnosis-test-review-layout.el ends here

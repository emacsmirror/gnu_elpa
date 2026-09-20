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

(ert-deftest gnosis-review-layout-answer-blocks-start-below-separator ()
  "Center every feedback entry independently of the keimenon separator."
  (dolist (renderer '(basic cloze mcq))
    (gnosis-test-review-layout--with-buffer
      (let ((gnosis-center-content t))
        (gnosis-display-keimenon "Question")
        (pcase renderer
          ('basic (gnosis-display-basic-answer "Athens" nil "Rome"))
          ('cloze (gnosis-display-cloze-user-answer "Rome"))
          ('mcq (gnosis-display-correct-answer-mcq "Athens" "Rome")))
        (dolist (label (pcase renderer
                        ('basic '("Answer:" "Your answer:"))
                        ('cloze '("Your answer:"))
                        ('mcq '("Correct Answer:" "Your answer:"))))
          (goto-char (point-min))
          (search-forward label)
          (should (= (line-beginning-position) (match-beginning 0)))
          (should-not (text-property-not-all (line-beginning-position)
                                             (line-end-position) 'display nil))
          (should (gnosis-test-review-layout--prefix
                   (selected-window) (line-beginning-position))))))))

(ert-deftest gnosis-review-layout-answer-blocks-share-stable-filling ()
  "Fill feedback once like keimenon, retaining authored breaks on resize."
  (dolist (center '(nil t))
    (dolist (renderer '(basic cloze mcq))
      (gnosis-test-review-layout--with-buffer
        (let* ((gnosis-center-content center)
               (fill-column 28)
               (answer "Alpha beta gamma delta epsilon zeta eta\n\nΕλληνικά")
               (snapshot (copy-sequence answer)))
          (gnosis-display-keimenon "Question")
          (pcase renderer
            ('basic (gnosis-display-basic-answer answer nil answer))
            ('cloze (gnosis-display-cloze-user-answer answer))
            ('mcq (gnosis-display-correct-answer-mcq answer answer)))
          (should (string-match-p "\n\nΕλληνικά" (buffer-string)))
          (if center
              (should (seq-every-p (lambda (line) (<= (string-width line) fill-column))
                                   (split-string (buffer-string) "\n")))
            (should (string-match-p (regexp-quote answer) (buffer-string))))
          (let ((text (buffer-string)))
            (split-window-right 35)
            (run-hooks 'window-configuration-change-hook)
            (should (equal-including-properties text (buffer-string))))
          (should (equal-including-properties answer snapshot)))))))

(ert-deftest gnosis-review-layout-answer-blocks-preserve-media-and-literals ()
  "Keep media geometry and literal typed links when formatting feedback."
  (dolist (center '(nil t))
    (gnosis-test-review-layout--with-buffer
      (let* ((gnosis-center-content center)
             (fill-column 100)
             (media (propertize " " 'display '(space :width 5)
                                'gnosis-image-mask '(retained-mask)))
             (answer (concat "Caption\n" media "\n\nTail"))
             (input "[[https://example.org/source][Literal typed link]]")
             (answer-snapshot (copy-sequence answer))
             (input-snapshot (copy-sequence input)))
        (gnosis-display-keimenon "Question")
        (gnosis-display-basic-answer answer nil input)
        (gnosis-display-cloze-user-answer input)
        (should (text-property-any (point-min) (point-max) 'display
                                   (get-text-property 0 'display media)))
        (should (equal '(retained-mask)
                       (get-text-property
                        (text-property-not-all (point-min) (point-max)
                                               'gnosis-image-mask nil)
                        'gnosis-image-mask)))
        (goto-char (point-min))
        (should (search-forward input nil t))
        (should (search-forward input nil t))
        (should (equal-including-properties answer answer-snapshot))
        (should (equal-including-properties input input-snapshot))))))

(ert-deftest gnosis-review-layout-org-citation-retains-inline-typography ()
  "Org subscripts are text, not independently positioned media."
  (gnosis-test-review-layout--with-buffer
    (let* ((gnosis-latex-preview nil)
           (citation "Evidence: lecture_notes_2026.pdf, p. 12")
           (fontified (gnosis-org-format-string citation))
           (position (text-property-not-all 0 (length fontified)
                                            'display nil fontified))
           (display (and position (get-text-property position 'display fontified))))
      (should position)
      (gnosis-display-keimenon "Question")
      (gnosis-display-basic-answer "Answer" nil "Literal_input")
      (gnosis-display-parathema citation)
      (goto-char (point-min))
      (search-forward citation)
      (let ((start (match-beginning 0)))
        (should (equal display (get-text-property (+ start position) 'display)))
        (should (gnosis-test-review-layout--prefix (selected-window) start)))
      (gnosis-display-next-review '(2026 10 1) t)
      (gnosis-display-next-review '(2026 10 2) nil)
      (gnosis-display-next-review '(2026 10 3) t)
      (goto-char (point-min))
      (should (search-forward citation nil t))
      (should (search-forward "Next review:" nil t))
      (should-not (search-forward "Next review:" nil t)))))

(ert-deftest gnosis-review-layout-fills-org-inline-display-prose ()
  "Fill long scientific prose without discarding native subscript properties."
  (let* ((gnosis-center-content t)
         (gnosis-latex-preview nil)
         (fill-column 25)
         (text (gnosis-org-format-string
                "The H_2 molecule has properties discussed in this long explanation.\n\nΕλληνικά"))
         (snapshot (copy-sequence text))
         (result (gnosis-review--format-string text)))
    (should (seq-every-p (lambda (line) (<= (string-width line) fill-column))
                         (split-string result "\n")))
    (should (text-property-not-all 0 (length result) 'display nil result))
    (should (string-suffix-p "\n\nΕλληνικά" result))
    (should (equal-including-properties text snapshot))))

(ert-deftest gnosis-review-layout-explicit-geometry-is-not-prose ()
  "Keep independent blocks intact even without a native display property."
  (gnosis-test-review-layout--with-buffer
    (let* ((fill-column 12)
           (block (propertize "   Renderer owns this entire block   "
                              'gnosis-display-layout 'independent))
           (snapshot (copy-sequence block)))
      (gnosis-display-keimenon (concat "Prose\n" block "\nTail"))
      (goto-char (point-min))
      (search-forward block)
      (should (equal-including-properties
               block (buffer-substring (match-beginning 0) (match-end 0))))
      (should-not (gnosis-test-review-layout--prefix
                   (selected-window) (line-beginning-position)))
      (forward-line 1)
      (should (gnosis-test-review-layout--prefix (selected-window) (point)))
      (should (equal-including-properties block snapshot)))))

(ert-deftest gnosis-review-layout-latex-image-retains-independent-geometry ()
  "Transfer native LaTeX image ownership without classifying subscripts as media."
  (let* ((gnosis-center-content t)
         (gnosis-latex-preview t)
         (fill-column 5)
         (image '(image :type svg :data "disposable test image")))
    ;; Avoid external TeX processes, but exercise the real overlay transfer.
    (cl-letf (((symbol-function 'org-format-latex)
               (lambda (&rest _)
                 (overlay-put (make-overlay (point-min) (point-max))
                              'display image))))
      (let* ((text (gnosis-org-format-string "$x + y$"))
             (result (gnosis-review--format-string text)))
        (should (equal-including-properties text result))
        (should (eq 'independent (get-text-property 0 'gnosis-display-layout result)))
        (should (equal image (get-text-property 0 'display result)))))))

(provide 'gnosis-test-review-layout)
;;; gnosis-test-review-layout.el ends here

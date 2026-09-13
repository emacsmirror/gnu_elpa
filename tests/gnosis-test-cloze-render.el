;;; gnosis-test-cloze-render.el --- Cloze presentation contracts -*- lexical-binding: t; -*-

(require 'ert)
(require 'gnosis-review)

(ert-deftest gnosis-test-cloze-render-substitution-characterization ()
  "Retain sequential first occurrences, literal masks, and whitespace quirks."
  (let ((gnosis-latex-preview nil))
    (dolist (case '(("α β α" ("α") "_" "_ β α")
                    ("α β α" ("α" "α") "_" "_ β _")
                    ("a a" ("a" "a") "aa" "aa aa")
                    ("x[a]. x[a]." ("x[a].") "\\1" "\\1 x[a].")
                    ("A a" ("\"a\"") "_" "_ a")
                    ("left\t α  right" ("\t α  ") "_" "left\t _  right")
                    ("α\n β" ("α\n β") "_" " _")
                    ("   " ("   ") "_" "   _   ")
                    ("α" ("") "_" "_α")
                    ("α" ("absent") "_" "α")))
      (should (equal (nth 3 case)
                     (gnosis-cloze-create (car case) (cadr case) (nth 2 case)))))))

(ert-deftest gnosis-test-cloze-render-input-properties-and-snapshots ()
  "Keep source and mask snapshots and properties outside the replaced span."
  (let* ((gnosis-latex-preview nil)
         (prefix (propertize "*α* " 'help-echo "prefix"))
         (needle (propertize " β " 'help-echo "needle"))
         (text (concat prefix (propertize " β " 'help-echo "source") " end"))
         (mask (propertize "?" 'help-echo "mask" 'face 'warning))
         (snapshots (mapcar #'copy-sequence (list text needle mask)))
         (result (gnosis-cloze-create text (list needle) mask)))
    (should (equal "*α*  ?  end" result))
    (should (equal "prefix" (get-text-property 1 'help-echo result)))
    (should (memq 'bold (ensure-list (get-text-property 1 'face result))))
    (should (equal "needle" (get-text-property 4 'help-echo result)))
    (should (equal "mask" (get-text-property 5 'help-echo result)))
    (should (eq 'gnosis-face-cloze (get-text-property 5 'face result)))
    (cl-mapc (lambda (original snapshot)
               (should (equal-including-properties original snapshot)))
             (list text needle mask) snapshots)
    (should (equal-including-properties
             result (gnosis-cloze-create text (list needle) mask)))))

(ert-deftest gnosis-test-cloze-render-literal-spaces ()
  "Mask the literal space match after a tab near-match, preserving Org output."
  (let* ((gnosis-latex-preview nil)
         (text (propertize "*α* a\tb a b" 'help-echo "source"))
         (clozes (list (propertize "a b" 'help-echo "needle")))
         (mask (propertize "?" 'help-echo "mask" 'face 'warning))
         (snapshots (mapcar #'copy-sequence (list text (car clozes) mask)))
         (expected (let ((search-spaces-regexp nil))
                     (gnosis-cloze-create text clozes mask))))
    (should (equal "*α* a\tb ?" expected))
    (should (memq 'bold (ensure-list (get-text-property 1 'face expected))))
    (should (eq 'gnosis-face-cloze (get-text-property 8 'face expected)))
    (with-temp-buffer
      (insert "caller")
      (goto-char 3)
      (setq-local case-fold-search nil)
      (dolist (spaces '(nil "[ \t]+"))
        (let* ((search-spaces-regexp spaces)
               (result (gnosis-cloze-create text clozes mask)))
          (should (equal-including-properties expected result))
          (should (equal spaces search-spaces-regexp))
          (should-not case-fold-search)
          (should (= 3 (point)))
          (should (equal "caller" (buffer-string)))
          (cl-mapc (lambda (original snapshot)
                     (should (equal-including-properties original snapshot)))
                   (list text (car clozes) mask) snapshots))))))

(ert-deftest gnosis-test-cloze-render-independent-of-caller-search-context ()
  "Retain the temporary fundamental buffer's search and whitespace context."
  (let ((gnosis-latex-preview nil))
    (with-temp-buffer
      (setq-local case-fold-search nil)
      (set-syntax-table (copy-syntax-table))
      (modify-syntax-entry ?x " ")
      (should (equal "_ xax" (gnosis-cloze-create "xAx xax" '("xax") "_"))))))

(ert-deftest gnosis-test-cloze-render-invalid-clozes-do-not-render ()
  "Reject invalid clozes before invoking Org or LaTeX rendering."
  ;; Compiled assertions in Emacs 29 can enter the debugger before a handler.
  (let ((debug-on-error nil) rendered)
    (cl-letf (((symbol-function 'gnosis-org-format-string)
               (lambda (str) (setq rendered t) str)))
      (should-error (gnosis-cloze-create "text" "not a list") :type 'error)
      (should-not rendered))))

(ert-deftest gnosis-test-cloze-render-review-fontifies-and-previews-once ()
  "Use real review insertion, fontification, and exactly one LaTeX boundary."
  (let ((gnosis-latex-preview t)
        (gnosis-center-content nil)
        (previews 0))
    (with-temp-buffer
      (let ((gnosis-review-buffer-name (buffer-name)))
        (cl-letf (((symbol-function 'org-format-latex)
                   (lambda (&rest _)
                     (cl-incf previews)
                     ;; Rendering precedes substitution, including hidden fragments.
                     (should (equal "*α* β $x$" (buffer-string)))
                     (let ((ov (make-overlay (- (point-max) 3) (point-max))))
                       (overlay-put ov 'display 'cloze-test-preview)))))
          (gnosis-display-cloze-string "*α* β $x$" '("β") '("hint") nil nil))
        (should (= previews 1))
        (goto-char (point-min))
        (search-forward "α")
        (should (memq 'bold (ensure-list (get-text-property (1- (point)) 'face))))
        (search-forward "(hint)")
        (should (eq 'gnosis-face-cloze (get-text-property (1- (point)) 'face)))
        (search-forward "$x$")
        (should (eq 'cloze-test-preview (get-text-property (1- (point)) 'display)))))))

(provide 'gnosis-test-cloze-render)
;;; gnosis-test-cloze-render.el ends here

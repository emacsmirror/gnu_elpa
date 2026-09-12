;;; gnosis-test-cloze-newlines.el --- Cloze newline faces -*- lexical-binding: t; -*-

(require 'ert)
(require 'gnosis-review)

(defun gnosis-test-cloze-newlines--assert-faces ()
  "Assert that cloze faces never decorate newlines in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "\n" nil t)
      (dolist (face '(gnosis-face-cloze gnosis-face-correct
                     gnosis-face-false gnosis-face-unanswered))
        (should-not (memq face (ensure-list
                               (get-text-property (1- (point)) 'face))))))))

(ert-deftest gnosis-test-cloze-newlines-input-hints ()
  "Preserve multiline hints while leaving authored and filled breaks plain."
  (dolist (center '(nil t))
    (with-temp-buffer
      (let* ((gnosis-review-buffer-name (buffer-name))
             (gnosis-review--running nil)
             (gnosis-center-content center)
             (gnosis-latex-preview nil)
             (fill-column 24)
             (source "Before Αθήνα\n\nAfter")
             (hint "Alpha beta gamma delta epsilon zeta\n\nΕλληνικά")
             (snapshot (copy-sequence source))
             (hint-snapshot (copy-sequence hint)))
        (gnosis-display-cloze-string source '("Αθήνα") (list hint) nil nil)
        (should (equal (buffer-substring-no-properties (point-min) (point-max))
                       (if center
                           "\nBefore (Alpha beta gamma\ndelta epsilon zeta\n\nΕλληνικά)\n\nAfter\n "
                         "\nBefore (Alpha beta gamma delta epsilon zeta\n\nΕλληνικά)\n\nAfter\n ")))
        (gnosis-test-cloze-newlines--assert-faces)
        (dolist (token '("Alpha" "zeta" "Ελληνικά"))
          (goto-char (point-min))
          (search-forward token)
          (should (eq 'gnosis-face-cloze
                      (get-text-property (1- (point)) 'face))))
        (should (equal-including-properties source snapshot))
        (should (equal-including-properties hint hint-snapshot))))))

(ert-deftest gnosis-test-cloze-newlines-feedback ()
  "Keep result faces on answer tokens, not blank lines or trailing space."
  (dolist (center '(nil t))
    (with-temp-buffer
      (let ((gnosis-review-buffer-name (buffer-name))
            (gnosis-review--running nil)
            (gnosis-center-content center)
            (gnosis-latex-preview nil)
            (fill-column 24))
        (gnosis-display-cloze-string
         "Alpha\n\nBeta\n\nGamma" nil nil '("Alpha") '("Beta" "Gamma"))
        (gnosis-test-cloze-newlines--assert-faces)
        (dolist (pair '(("Alpha" . gnosis-face-correct)
                        ("Beta" . gnosis-face-false)
                        ("Gamma" . gnosis-face-unanswered)))
          (goto-char (point-min))
          (search-forward (car pair))
          (should (eq (cdr pair) (get-text-property (1- (point)) 'face))))
        (dolist (false '(nil t))
          (let* ((answer "Typed\n\nanswer")
                 (snapshot (copy-sequence answer)))
            (gnosis-display-cloze-user-answer answer false)
            (gnosis-test-cloze-newlines--assert-faces)
            (should (string-suffix-p "Typed\n\nanswer\n" (buffer-string)))
            (should (eq (if false 'gnosis-face-false 'gnosis-face-correct)
                        (get-text-property (- (point-max) 2) 'face)))
            (should (equal-including-properties answer snapshot))))))))

(ert-deftest gnosis-test-cloze-newlines-preserve-other-properties ()
  "Remove only inline semantic faces from final display newline characters."
  (let* ((gnosis-center-content nil)
         (source (concat
                  (propertize "Token\n\n" 'face '(gnosis-face-cloze warning)
                              'mouse-face 'mode-line-highlight
                              'help-echo "Retained metadata"
                              'font-lock-face 'bold)
                  (propertize " " 'display '(space :width 5)
                              'gnosis-image-mask '(retained-mask))
                  (propertize "\nOther\n" 'face '(:underline t))))
         (snapshot (copy-sequence source))
         (result (gnosis-review--format-string source)))
    (should (equal source result))
    (should (equal-including-properties source snapshot))
    (should (equal-including-properties (substring source 0 5)
                                       (substring result 0 5)))
    (dolist (position '(5 6))
      (should (equal '(warning) (get-text-property position 'face result)))
      (dolist (property '(mouse-face help-echo font-lock-face))
        (should (equal (get-text-property position property source)
                       (get-text-property position property result)))))
    (should (equal-including-properties (substring source 7)
                                       (substring result 7)))))

(provide 'gnosis-test-cloze-newlines)
;;; gnosis-test-cloze-newlines.el ends here

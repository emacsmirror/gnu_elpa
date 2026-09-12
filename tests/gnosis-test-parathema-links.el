;;; gnosis-test-parathema-links.el --- Parathema link display tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'gnosis-review)

(defun gnosis-test-parathema-links--assert-newlines ()
  "Assert that newlines in the current buffer have no link styling."
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "\n" nil t)
      (let ((position (1- (point))))
        (should-not (memq 'org-link
                          (ensure-list (get-text-property position 'face))))
        (should-not (get-text-property position 'mouse-face))))))

(defun gnosis-test-parathema-links--assert-label (label)
  "Assert that LABEL retains its Org link destination and mouse binding."
  (save-excursion
    (goto-char (point-min))
    (search-forward label)
    (cl-loop for position from (- (point) (length label)) below (point)
             do (should (memq 'org-link
                              (ensure-list (get-text-property position 'face))))
             do (should (eq 'highlight (get-text-property position 'mouse-face)))
             do (should (equal '(:uri "https://example.org/source")
                               (get-text-property position 'htmlize-link)))
             do (should (equal "LINK: https://example.org/source"
                               (get-text-property position 'help-echo)))
             do (should (eq 'org-open-at-mouse
                            (lookup-key (get-text-property position 'keymap)
                                        [mouse-2]))))))

(ert-deftest gnosis-test-parathema-links-filled-newlines ()
  "Wrap real fontified link descriptions without styling empty line areas."
  (dolist (center '(nil t))
    (with-temp-buffer
      (let* ((gnosis-latex-preview nil)
             (source (concat "Before [[https://example.org/source]"
                             "[Alpha beta gamma delta epsilon zeta eta theta]]"
                             " after *bold*."))
             (snapshot (copy-sequence source)))
        (setq-local gnosis-center-content center fill-column 24)
        (gnosis-display-parathema source)
        (should (equal (if center
                           "\nBefore Alpha beta gamma\ndelta epsilon zeta eta\ntheta after *bold*.\n"
                         "\nBefore Alpha beta gamma delta epsilon zeta eta theta after *bold*.\n")
                       (buffer-string)))
        (gnosis-test-parathema-links--assert-newlines)
        (dolist (label '("Alpha" "gamma" "delta" "eta" "theta"))
          (gnosis-test-parathema-links--assert-label label))
        (goto-char (point-min))
        (search-forward "Before")
        (should-not (get-text-property (1- (point)) 'face))
        (should-not (get-text-property (1- (point)) 'keymap))
        (search-forward "bold")
        (should (memq 'bold (ensure-list (get-text-property (1- (point)) 'face))))
        (should-not (get-text-property (1- (point)) 'mouse-face))
        (should (equal-including-properties source snapshot))))))

(ert-deftest gnosis-test-parathema-links-authored-newlines-and-boundaries ()
  "Keep authored breaks within links and blank lines next to plain text."
  (dolist (center '(nil t))
    (dolist (surrounding '("Plain" "[[https://example.org/source][Other]]"))
      (with-temp-buffer
        (let ((gnosis-latex-preview nil))
          (setq-local gnosis-center-content center fill-column 24)
          (gnosis-display-parathema
           (concat surrounding "\n\n"
                   "[[https://example.org/source][Alpha\nbeta]]\n\n"
                   surrounding))
          (should (equal (if (equal surrounding "Plain")
                             "\nPlain\n\nAlpha\nbeta\n\nPlain\n"
                           "\nOther\n\nAlpha\nbeta\n\nOther\n")
                         (buffer-string)))
          (gnosis-test-parathema-links--assert-newlines)
          (gnosis-test-parathema-links--assert-label "Alpha")
          (gnosis-test-parathema-links--assert-label "beta")
          (when (equal surrounding "Plain")
            (goto-char (point-min))
            (search-forward "Plain")
            (should-not (get-text-property (1- (point)) 'face))
            (should-not (get-text-property (1- (point)) 'keymap))))))))

(ert-deftest gnosis-test-parathema-links-preserve-normal-properties ()
  "Change only link styling on newlines, never caller strings or media."
  (dolist (center '(nil t))
    (let* ((gnosis-latex-preview nil)
           (gnosis-center-content center)
           (fill-column 12)
           (link (gnosis-org-format-string
                  "[[https://example.org/source][Alpha beta gamma delta]]"))
           (media (propertize " " 'display '(space :width 5)
                              'gnosis-image-mask '(retained-mask)))
           (source (concat link "\n\n" media "\n"
                           (propertize "ordinary styled text" 'face 'warning)))
           (snapshot (copy-sequence source))
           (result (gnosis-review--format-string source)))
      (should (equal-including-properties source snapshot))
      (with-temp-buffer
        (insert result)
        (gnosis-test-parathema-links--assert-newlines)
        (gnosis-test-parathema-links--assert-label "Alpha")
        (gnosis-test-parathema-links--assert-label "delta")
        (should (text-property-any (point-min) (point-max) 'display
                                   (get-text-property 0 'display media)))
        (goto-char (point-min))
        (search-forward "ordinary")
        (should (eq 'warning (get-text-property (1- (point)) 'face)))
        (should (equal '(retained-mask)
                       (get-text-property
                        (text-property-not-all (point-min) (point-max)
                                               'gnosis-image-mask nil)
                        'gnosis-image-mask)))))))

(ert-deftest gnosis-test-parathema-links-preserve-unrelated-newline-faces ()
  "Retain non-link newline properties, including mixed face lists."
  (let* ((gnosis-center-content nil)
         (source (concat (propertize "Link\n" 'face '(org-link warning)
                                    'mouse-face 'highlight
                                    'help-echo "link metadata")
                         (propertize "Plain\n\n" 'face 'warning
                                     'mouse-face 'mode-line-highlight)
                         (propertize "Other\n" 'face '(bold)
                                     'display '(space :width 2))))
         (snapshot (copy-sequence source))
         (result (gnosis-review--format-string source)))
    (should (equal source result))
    (should (equal-including-properties source snapshot))
    (should (equal '(warning) (get-text-property 4 'face result)))
    (should-not (get-text-property 4 'mouse-face result))
    (should (equal "link metadata" (get-text-property 4 'help-echo result)))
    (should (equal-including-properties (substring source 0 4)
                                       (substring result 0 4)))
    (should (equal-including-properties (substring source 5)
                                       (substring result 5)))))

(provide 'gnosis-test-parathema-links)
;;; gnosis-test-parathema-links.el ends here

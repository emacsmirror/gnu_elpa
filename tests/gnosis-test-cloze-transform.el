;;; gnosis-test-cloze-transform.el --- Independent cloze laws -*- lexical-binding: t; -*-

(require 'ert)
(require 'gnosis-cloze)

(ert-deftest gnosis-test-cloze-transform-composition-and-snapshots ()
  "Compose sequential substitutions without changing retained inputs or results."
  (let* ((text (propertize "α β α" 'help-echo "source"))
         (clozes (list (propertize "α" 'help-echo "needle") "β" "α"))
         (mask (propertize "(?)" 'help-echo "mask"))
         (text-before (copy-sequence text))
         (clozes-before (mapcar #'copy-sequence clozes))
         (mask-before (copy-sequence mask))
         (first (gnosis-cloze--replace text (list (car clozes)) mask))
         (snapshot (copy-sequence first))
         (result (gnosis-cloze--replace first (cdr clozes) mask)))
    (should (equal "(?) β α" first))
    (should (equal "(?) (?) (?)" result))
    (should (equal-including-properties
             result (gnosis-cloze--replace text clozes mask)))
    (should (equal-including-properties first snapshot))
    (should (equal-including-properties text text-before))
    (cl-mapc (lambda (value before) (should (equal-including-properties value before)))
             clozes clozes-before)
    (should (equal-including-properties mask mask-before))
    (should (eq 'gnosis-face-cloze (get-text-property 0 'face result)))
    (should (equal "mask" (get-text-property 0 'help-echo result)))
    ;; Even identity results must not alias the caller's mutable string.
    (dolist (needles '(nil ("absent")))
      (let ((identity (gnosis-cloze--replace text needles mask)))
        (should (equal-including-properties text identity))
        (put-text-property 0 1 'face 'warning identity)
        (should (equal-including-properties text text-before))))))

(ert-deftest gnosis-test-cloze-transform-literal-spaces ()
  "Ignore ambient space expansion without changing inputs or caller settings."
  (let* ((text (propertize "a\tb a b" 'help-echo "source"))
         (clozes (list (propertize "a b" 'help-echo "needle")))
         (mask (propertize "?" 'help-echo "mask" 'face 'warning))
         (snapshots (mapcar #'copy-sequence (list text (car clozes) mask)))
         (expected (concat (substring text 0 4)
                           (propertize mask 'face 'gnosis-face-cloze))))
    (with-temp-buffer
      (insert "caller")
      (goto-char 3)
      (setq-local case-fold-search t)
      (dolist (spaces '(nil "[ \t]+"))
        (let ((search-spaces-regexp spaces)
              (buffers (buffer-list)))
          (string-match "a" "abc")
          (let* ((before (match-data))
                 (result (gnosis-cloze--replace text clozes mask)))
            (should (equal before (match-data)))
            (should (equal buffers (buffer-list)))
            (should (equal-including-properties expected result))
            (should (equal spaces search-spaces-regexp))
            (should case-fold-search)
            (should (= 3 (point)))
            (should (equal "caller" (buffer-string)))
            (cl-mapc (lambda (original snapshot)
                       (should (equal-including-properties original snapshot)))
                     (list text (car clozes) mask) snapshots)))))))

(ert-deftest gnosis-test-cloze-transform-explicit-case-fold ()
  "Use explicit case folding and standard whitespace, not caller buffer state."
  (with-temp-buffer
    (set-syntax-table (copy-syntax-table))
    (modify-syntax-entry ?x " ")
    (setq-local case-fold-search t)
    (let ((syntax (syntax-table)))
      (should (equal "xAx _" (gnosis-cloze--replace "xAx xax" '("xax") "_")))
      (should (equal "_ xax" (gnosis-cloze--replace "xAx xax" '("xax") "_" t)))
      (should (eq syntax (syntax-table)))
      (should case-fold-search))))

(ert-deftest gnosis-test-cloze-transform-preserves-match-and-buffer-state ()
  "Substitute without rendering, changing buffers, or destroying saved matches."
  (with-temp-buffer
    (insert "caller")
    (goto-char 3)
    (string-match "a" "abc")
    (let ((before (match-data)) (buffers (buffer-list)))
      (gnosis-cloze--replace "α β" '("α") "_")
      (should (equal before (match-data)))
      (should (equal buffers (buffer-list)))
      (should (= 3 (point)))
      (should (equal "caller" (buffer-string))))))

(ert-deftest gnosis-test-cloze-transform-literal-hints ()
  "Insert authored hints literally without changing input text or properties."
  (dolist (hint '("\\alpha" "C:\\temp\\file" "\\&" "\\1" "tail\\"
                  "\\\\" "Ελληνικά\\alpha\n\\&"))
    (let* ((source (propertize "Before (...) after" 'help-echo "source"))
           (hints (list (propertize hint 'help-echo "hint")))
           (source-before (copy-sequence source))
           (hint-before (copy-sequence (car hints)))
           (result (gnosis-cloze-add-hints source hints)))
      (should (equal (concat "Before (" hint ") after") result))
      (should (eq 'gnosis-face-cloze (get-text-property 8 'face result)))
      (should (equal "hint" (get-text-property 8 'help-echo result)))
      (should (equal "source" (get-text-property 0 'help-echo result)))
      (should (equal-including-properties source source-before))
      (should (equal-including-properties (car hints) hint-before))))
  ;; A lettered custom mask must not case-convert the replacement.
  (should (equal "(lower\\&)"
                 (gnosis-cloze-add-hints "MASK" '("lower\\&") "MASK")))
  ;; Empty hints consume their own mask; later hints retain their order.
  (should (equal "(...) (...) (...) (...) (\\&) (\\1)"
                 (gnosis-cloze-add-hints
                  "(...) (...) (...) (...) (...) (...)"
                  '(nil "" "nil" "\"\"" "\\&" "\\1")))))

(provide 'gnosis-test-cloze-transform)
;;; gnosis-test-cloze-transform.el ends here

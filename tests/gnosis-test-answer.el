;;; gnosis-test-answer.el --- Explicit answer tests -*- lexical-binding: t; -*-
;; Copyright (C) 2026 Free Software Foundation, Inc.
;;; Commentary:
;; Matching compatibility and lossless independent alias lists.
;;; Code:
(require 'ert)
(require 'gnosis-answer)
(require 'gnosis)

(ert-deftest gnosis-test-answer-existing-verdicts ()
  ;; Fixed verdicts from the pre-alias implementation (f173038f), not a
  ;; second entry point into the new matcher.  Include its short-input rule.
  (dolist (case '(("A" "a" 0 t) ("A" "B" 4 nil)
                  ("ab" "ac" 0 nil) ("ab" "ac" 1 t) ("ab" "ac" 2 nil)
                  ("Athens" "athenz" 1 t) ("α β" "αβ" 0 t)
                  ("\"quoted\"" "quoted" 0 t)
                  ("- literal" "literal" 0 nil) ("- literal" "literal" 1 t)
                  ("" "" 4 t) ("" "a" 4 nil)
                  ("A" "AB" 1 t) ("AB" "A" 1 t)))
    (should (eq (nth 3 case)
                (gnosis-answer-match-p (nth 0 case) (nth 1 case) nil (nth 2 case))))))

(ert-deftest gnosis-test-answer-explicit-aliases ()
  (let* ((aliases '("Humerus" "--flag" "α β"))
         (before (copy-tree aliases)))
    (should (gnosis-answer-match-p "Upper arm bone" "humerus" aliases 0))
    (should (gnosis-answer-match-p "Upper arm bone" "humers" aliases 1))
    (should (gnosis-answer-match-p "Upper arm bone" "αβ" aliases 0))
    (should-not (gnosis-answer-match-p "Upper arm bone" "humerus" nil 0))
    (should-not (gnosis-answer-match-p "A" "b" '("C") 2))
    (should (equal before aliases))))

(ert-deftest gnosis-test-answer-alias-validation ()
  (dolist (bad '("string" ("ok" . "bad") (1) ("") (" \t") ("a\nb") ("a\rb")))
    (should-error (gnosis-answer-validate-aliases bad)))
  (let ((aliases '(" α β " "--flag" "- literal" "-" "[x] literal")))
    (should (eq aliases (gnosis-answer-validate-aliases aliases)))
    (should (equal aliases (gnosis-answer-aliases-from-section
                            (gnosis-answer-aliases-to-section aliases)))))
  (should-not (gnosis-answer-aliases-from-section "\n\n"))
  (should-error (gnosis-answer-aliases-from-section "- one\n  continuation")))

(provide 'gnosis-test-answer)
;;; gnosis-test-answer.el ends here

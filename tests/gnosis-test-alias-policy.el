;;; gnosis-test-alias-policy.el --- Alias eligibility compatibility -*- lexical-binding: t; -*-
;; Copyright (C) 2026 Free Software Foundation, Inc.
;;; Commentary:
;; Characterize both validation boundaries, including their distinct errors.
;;; Code:
(require 'ert)
(require 'gnosis)
(require 'gnosis-export-import)

(ert-deftest gnosis-test-alias-policy-boundary-matrix ()
  ;; Explicit classifications keep this oracle independent of the predicate.
  ;; Alias shape wins over type errors; nil bypasses type/answer eligibility.
  (dolist (type-case '(("basic" eligible) ("BASIC" eligible)
                       ("image-occlusion" eligible) ("Image-Occlusion" eligible)
                       ("model-name" eligible) ("MODEL-NAME" eligible)
                       ("mcq" rejected) ("cloze" rejected)
                       ("mc-cloze" rejected) ("double" rejected)
                       ("model" rejected) ("image-region" rejected)
                       ("unknown" rejected) ("" rejected) (65 rejected)
                       (nil malformed) (basic malformed) (1.5 malformed)
                       (["basic"] malformed) (("basic") malformed)))
    (dolist (answer-case '((("Answer") t) ((" α β ") t)
                           (("first\nsecond") t) (("\rAnswer\r") t)
                           (nil nil) (("") nil) ((" \t\n\r") nil)
                           (("one" "two") nil) ((42) nil) ((nil) nil)
                           ("Answer" nil) (["Answer"] nil) (42 nil)
                           (("Answer" . "tail") nil)))
      (dolist (alias-case '((nil absent) (("Alias") valid)
                            ((" α β " "--flag") valid)
                            ("Alias" malformed) (("Alias" . "tail") malformed)
                            (["Alias"] malformed) ((42) malformed)
                            (("") malformed) ((" \t") malformed)
                            (("a\nb") malformed) (("a\rb") malformed)))
        (let* ((type (car type-case))
               (answer (car answer-case))
               (aliases (car alias-case))
               (row (list 1 type "Q" nil answer "" nil nil aliases))
               (before (copy-tree row t))
               (failure
                (cond
                 ((eq (cadr alias-case) 'malformed)
                  '(user-error "Accepted aliases must be nonempty single-line strings"))
                 ((eq (cadr alias-case) 'absent) nil)
                 ((eq (cadr type-case) 'malformed)
                  (list 'wrong-type-argument 'char-or-string-p type))
                 ((not (and (eq (cadr type-case) 'eligible) (cadr answer-case)))
                  'ineligible))))
          (ert-info ((format "Type %S, answer %S, aliases %S" type answer aliases))
            (dolist (boundary '(authoring import))
              (let* ((expected
                      (if (eq failure 'ineligible)
                          (list 'user-error
                                (if (eq boundary 'authoring)
                                    "Accepted aliases require one canonical typed answer"
                                  "Aliases require one canonical typed answer"))
                        failure))
                     (result
                      (condition-case err
                          (list 'returned
                                (if (eq boundary 'authoring)
                                    (gnosis--validate-accepted-aliases type answer aliases)
                                  (gnosis-import--validate-alias-row row)))
                        (error err))))
                (if expected
                    (should (equal expected result))
                  (should (eq (car result) 'returned))
                  (should (eq (cadr result)
                              (if (eq boundary 'authoring) aliases row)))))
              (should (equal before row)))))))))

(ert-deftest gnosis-test-alias-policy-short-and-improper-rows ()
  ;; The row wrapper owns decoding, not general row-shape validation.
  (dolist (row '(nil (1 nil) (1 "basic" "Q" nil ("Answer"))
                 (1 "basic" "Q" nil ("Answer") "" nil nil ("Alias") . tail)))
    (should (eq row (gnosis-import--validate-alias-row row)))))

(provide 'gnosis-test-alias-policy)
;;; gnosis-test-alias-policy.el ends here

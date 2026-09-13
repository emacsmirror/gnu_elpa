;;; gnosis-test-script-detection.el --- Script detection tests  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Free Software Foundation, Inc.

;; Author: Thanos Apollo <public@thanosapollo.org>

;;; Commentary:

;; Tests for script detection and input method mapping.
;; `gnosis-utils-detect-script' is pure -- no database needed.

;;; Code:

(require 'ert)
(require 'quail)

(require 'gnosis-utils)
(require 'gnosis)

;; ---- Group 1: gnosis-utils-detect-script ----

(ert-deftest gnosis-test-detect-script-greek ()
  "Greek text returns greek script symbol."
  (should (eq 'greek (gnosis-utils-detect-script "ελληνικά"))))

(ert-deftest gnosis-test-detect-script-cyrillic ()
  "Cyrillic text returns cyrillic script symbol."
  (should (eq 'cyrillic (gnosis-utils-detect-script "русский"))))

(ert-deftest gnosis-test-detect-script-latin-only ()
  "Pure Latin text returns nil."
  (should (null (gnosis-utils-detect-script "hello world"))))

(ert-deftest gnosis-test-detect-script-empty-string ()
  "Empty string returns nil."
  (should (null (gnosis-utils-detect-script ""))))

(ert-deftest gnosis-test-detect-script-ascii-only ()
  "ASCII with punctuation and digits returns nil."
  (should (null (gnosis-utils-detect-script "test 123!@#"))))

(ert-deftest gnosis-test-detect-script-mixed-greek-latin ()
  "Mixed text returns the dominant non-Latin script."
  (should (eq 'greek (gnosis-utils-detect-script "the word λόγος means reason"))))

(ert-deftest gnosis-test-detect-script-single-char ()
  "Single non-Latin character is detected."
  (should (eq 'greek (gnosis-utils-detect-script "α"))))

(ert-deftest gnosis-test-detect-script-cjk ()
  "CJK ideographs are detected."
  (let ((script (gnosis-utils-detect-script "漢字")))
    (should script)
    (should (not (eq script 'latin)))))

(ert-deftest gnosis-test-detect-script-spaces-ignored ()
  "Whitespace does not affect detection."
  (should (eq 'greek (gnosis-utils-detect-script "  αβγ  "))))

(ert-deftest gnosis-test-detect-script-numbers-ignored ()
  "Digits (common script) do not affect detection."
  (should (eq 'greek (gnosis-utils-detect-script "42 αβγ"))))

;; ---- Group 2: gnosis-script-input-method-alist lookup ----

(ert-deftest gnosis-test-script-alist-default-has-greek ()
  "Default alist maps greek to the \"greek\" input method."
  (should (equal "greek"
                 (alist-get 'greek gnosis-script-input-method-alist))))

(ert-deftest gnosis-test-script-alist-missing-script ()
  "Unmapped script returns nil from alist."
  (let ((gnosis-script-input-method-alist '((greek . "greek"))))
    (should (null (alist-get 'cyrillic gnosis-script-input-method-alist)))))

(ert-deftest gnosis-test-script-alist-custom-mapping ()
  "Custom alist entries are respected."
  (let ((gnosis-script-input-method-alist
         '((greek . "greek") (cyrillic . "cyrillic-translit"))))
    (should (equal "cyrillic-translit"
                   (alist-get 'cyrillic gnosis-script-input-method-alist)))))

;; ---- Group 3: gnosis--read-string-with-input-method ----
;; Use real Quail activation and restoration, stubbing only the input boundary.

(ert-deftest gnosis-test-read-string-cyrillic-input-method-active ()
  "Honor a custom Cyrillic mapping and inherit it during input."
  (with-temp-buffer
    (let ((gnosis-script-input-method-alist
           '((cyrillic . "cyrillic-translit"))))
      (cl-letf (((symbol-function 'read-string)
                 (lambda (_prompt &optional _init _hist _default inherit)
                   (should inherit)
                   (should (equal current-input-method "cyrillic-translit"))
                   (should (eq input-method-function #'quail-input-method))
                   "typed answer")))
        (should (equal (gnosis--read-string-with-input-method "Answer: " "самолет")
                       "typed answer")))
      (should-not current-input-method))))

(ert-deftest gnosis-test-read-string-unmapped-preserves-input-method ()
  "Latin and unmapped scripts use plain input without replacing the method."
  (dolist (answer '("hello" "самолет"))
    (with-temp-buffer
      (activate-input-method "german-postfix")
      (let ((gnosis-script-input-method-alist '((greek . "greek"))))
        (cl-letf (((symbol-function 'read-string)
                   (lambda (_prompt &optional _init _hist _default inherit)
                     (should-not inherit)
                     (should (equal current-input-method "german-postfix"))
                     "typed answer")))
          (should (equal (gnosis--read-string-with-input-method "Answer: " answer)
                         "typed answer")))
        (should (equal current-input-method "german-postfix"))))))

(ert-deftest gnosis-test-read-string-restores-real-input-method ()
  "Restore nil, same and different Quail methods after return, error or quit."
  (dolist (previous '(nil "greek" "cyrillic-translit"))
    (dolist (outcome '(return error quit))
      (ert-info ((format "Previous %S, outcome %S" previous outcome))
        (with-temp-buffer
          (activate-input-method previous)
          (let ((gnosis-script-input-method-alist '((greek . "greek")))
                result)
            (cl-letf (((symbol-function 'read-string)
                       (lambda (_prompt &optional _init _hist _default inherit)
                         (should inherit)
                         (should (equal current-input-method "greek"))
                         (should (eq input-method-function #'quail-input-method))
                         (if (eq outcome 'return) "typed answer"
                           (signal outcome '("Input interrupted"))))))
              (setq result
                    (condition-case err
                        (gnosis--read-string-with-input-method "Answer: " "α")
                      ((error quit) err))))
            (should (equal result (if (eq outcome 'return) "typed answer"
                                    (list outcome "Input interrupted"))))
            (should (equal current-input-method previous))
            (when previous
              (should (eq input-method-function #'quail-input-method)))))))))

(ert-deftest gnosis-test-read-string-restores-after-activation-failure ()
  "Restore real Quail state even when an activation hook errors or quits."
  (dolist (outcome '(error quit))
    (with-temp-buffer
      (activate-input-method "cyrillic-translit")
      (let ((gnosis-script-input-method-alist '((greek . "greek")))
            (input-method-activate-hook
             (list (lambda ()
                     (when (equal current-input-method "greek")
                       (signal outcome '("Activation interrupted"))))))
            result)
        (cl-letf (((symbol-function 'read-string)
                   (lambda (&rest _) (ert-fail "Input followed failed activation"))))
          (setq result
                (condition-case err
                    (gnosis--read-string-with-input-method "Answer: " "α")
                  ((error quit) err))))
        (should (equal result (list outcome "Activation interrupted")))
        (should (equal current-input-method "cyrillic-translit"))
        (should (eq input-method-function #'quail-input-method))))))

(ert-deftest gnosis-test-read-string-restores-origin-not-current-buffer ()
  "A reader changing buffers must not redirect input-method cleanup."
  (dolist (outcome '(return error quit))
    (with-temp-buffer
      (let ((origin (current-buffer))
            (other (generate-new-buffer " *gnosis-input-other*"))
            (gnosis-script-input-method-alist '((greek . "greek")))
            result)
        (unwind-protect
            (progn
              (activate-input-method "cyrillic-translit")
              (with-current-buffer other (activate-input-method "german-postfix"))
              (cl-letf (((symbol-function 'read-string)
                         (lambda (&rest _)
                           (set-buffer other)
                           (if (eq outcome 'return) "typed answer"
                             (signal outcome '("Input interrupted"))))))
                (setq result
                      (condition-case err
                          (gnosis--read-string-with-input-method "Answer: " "α")
                        ((error quit) err))))
              (should (equal result (if (eq outcome 'return) "typed answer"
                                      (list outcome "Input interrupted"))))
              (with-current-buffer origin
                (should (equal current-input-method "cyrillic-translit"))
                (should (eq input-method-function #'quail-input-method)))
              (with-current-buffer other
                (should (equal current-input-method "german-postfix"))
                (should (eq input-method-function #'quail-input-method))))
          (kill-buffer other))))))

(provide 'gnosis-test-script-detection)

;;; gnosis-test-script-detection.el ends here

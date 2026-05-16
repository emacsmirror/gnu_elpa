;;; minuet-tests.el --- Tests for minuet -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for core Minuet behavior.

;;; Code:

(require 'ert)
(require 'seq)
(load (expand-file-name "test-helper"
                        (file-name-directory
                         (or load-file-name (buffer-file-name))))
      nil t)

(require 'minuet)

(ert-deftest minuet-remove-blank-items-drops-whitespace-only-items ()
  "Blank item filtering removes empty items without trimming valid ones."
  (should (equal (minuet--remove-blank-items '("  indented" "" " \t\n" "tail  "))
                 '("  indented" "tail  "))))

(ert-deftest minuet-filter-text-filters-before-cursor-with-nonzero-length ()
  "A non-zero before-cursor filter trims duplicated prefix text."
  (let ((minuet-before-cursor-filter-length 3)
        (minuet-after-cursor-filter-length 0))
    (should (equal (minuet--filter-text
                    "barxyz"
                    '(:before-cursor "foobar" :after-cursor "nomatch"))
                   "xyz"))))

(ert-deftest minuet-filter-text-filters-after-cursor-with-nonzero-length ()
  "A non-zero after-cursor filter trims duplicated suffix text."
  (let ((minuet-before-cursor-filter-length 0)
        (minuet-after-cursor-filter-length 2))
    (should (equal (minuet--filter-text
                    "xyzqu"
                    '(:before-cursor "nomatch" :after-cursor "quux"))
                   "xyz"))))

(ert-deftest minuet-filter-text-filters-before-and-after-with-nonzero-lengths ()
  "Non-zero before and after filters trim duplicated prefix and suffix text."
  (let ((minuet-before-cursor-filter-length 3)
        (minuet-after-cursor-filter-length 2))
    (should (equal (minuet--filter-text
                    "barxyzqu"
                    '(:before-cursor "foobar" :after-cursor "quux"))
                   "xyz"))))

(ert-deftest minuet-filter-text-trims-whitespace-when-filtering-is-enabled ()
  "Enabled filters trim ITEM before removing duplicated context."
  (let ((minuet-before-cursor-filter-length 3)
        (minuet-after-cursor-filter-length 2))
    (should (equal (minuet--filter-text
                    "  barxyzqu  "
                    '(:before-cursor "foobar" :after-cursor "quux"))
                   "xyz"))))

(ert-deftest minuet-filter-text-accepts-function-filter-lengths ()
  "Function-valued filter lengths are evaluated before filtering."
  (let ((minuet-before-cursor-filter-length (lambda () 3))
        (minuet-after-cursor-filter-length (lambda () 2)))
    (should (equal (minuet--filter-text
                    "barxyzz"
                    '(:before-cursor "foobar" :after-cursor "zzquux"))
                   "xy"))))

(ert-deftest minuet-filter-text-disables-filtering-with-fim-defaults ()
  "Provider-aware FIM defaults disable before and after cursor filtering."
  (let ((minuet-provider 'openai-fim-compatible)
        (minuet-before-cursor-filter-length
         #'minuet--default-before-cursor-filter-length-function)
        (minuet-after-cursor-filter-length
         #'minuet--default-after-cursor-filter-length-function))
    (should (equal (minuet--filter-text
                    "barxyzz"
                    '(:before-cursor "foobar" :after-cursor "zzquux"))
                   "barxyzz"))))

(ert-deftest minuet-filter-text-preserves-item-when-filter-lengths-are-zero ()
  "Disabled filters return ITEM without trimming or other changes."
  (let ((minuet-before-cursor-filter-length 0)
        (minuet-after-cursor-filter-length 0))
    (should (equal (minuet--filter-text
                    "  barxyzz  "
                    '(:before-cursor "foobar" :after-cursor "zzquux"))
                   "  barxyzz  "))))

(provide 'minuet-tests)
;;; minuet-tests.el ends here

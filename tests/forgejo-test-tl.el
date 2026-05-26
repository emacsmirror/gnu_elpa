;;; forgejo-test-tl.el --- Tests for forgejo-tl  -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the fast tabulated-list renderer, especially point
;; restoration when the saved entry has been filtered out.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'forgejo-tl)

(defun forgejo-test-tl--setup (entries)
  "Install ENTRIES in the current buffer with a single Name column."
  (tabulated-list-mode)
  (setq tabulated-list-format [("Name" 20 nil)]
        tabulated-list-entries entries
        tabulated-list-padding 0)
  (tabulated-list-init-header)
  (forgejo-tl-print))

(ert-deftest forgejo-test-tl-remember-pos-found ()
  "Restore point to the saved entry when it still exists."
  (with-temp-buffer
    (forgejo-test-tl--setup
     '((1 ["one"]) (2 ["two"]) (3 ["three"])))
    (goto-char (point-min))
    (forward-line 1)
    (should (equal (tabulated-list-get-id) 2))
    (forgejo-tl-print t)
    (should (equal (tabulated-list-get-id) 2))))

(ert-deftest forgejo-test-tl-remember-pos-filtered-out ()
  "Fall back to the saved line when the saved entry is gone."
  (with-temp-buffer
    (forgejo-test-tl--setup
     '((1 ["one"]) (2 ["two"]) (3 ["three"]) (4 ["four"])))
    (goto-char (point-min))
    (forward-line 1)
    (should (equal (tabulated-list-get-id) 2))
    (setq tabulated-list-entries
          '((1 ["one"]) (3 ["three"]) (4 ["four"])))
    (forgejo-tl-print t)
    ;; Entry 2 is gone; cursor should stay on line 2, now showing 3.
    (should (equal (line-number-at-pos) 2))
    (should (equal (tabulated-list-get-id) 3))))

(ert-deftest forgejo-test-tl-remember-pos-no-id ()
  "Without REMEMBER-POS, point goes to point-min."
  (with-temp-buffer
    (forgejo-test-tl--setup
     '((1 ["one"]) (2 ["two"]) (3 ["three"])))
    (goto-char (point-min))
    (forward-line 2)
    (forgejo-tl-print)
    (should (equal (point) (point-min)))))

(provide 'forgejo-test-tl)
;;; forgejo-test-tl.el ends here

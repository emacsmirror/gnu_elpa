;;; gnosis-test-dashboard-hscroll.el --- Scrolled native tables -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.
;; Author: Thanos Apollo <public@thanosapollo.org>

;;; Commentary:
;; Compare native and fast rows through the same scrolled window and keys.

;;; Code:
(require 'gnosis-test-dashboard-native)

(ert-deftest gnosis-dashboard-native-hscroll ()
  "Native column lookup, movement and width keys agree on scrolled fast rows."
  (gnosis-test-dashboard-native--with-view
    (dolist (case '((native . sort) (bulk . sort) (append . sort) (replace . sort)
                    (native . width) (bulk . width) (append . width) (replace . width)))
      (dolist (text '("Plain ASCII" "Ελληνικά 界 é" "" "界界界界界界界界界界界"))
        (gnosis-dashboard-output-themata '(1))
        (setq tabulated-list-format
              [("ID" 18 t) ("Text" 20 t) ("Tail" 8 t)])
        (setq tabulated-list-entries
              (list (list 1 (vector "1234567890" text "End"))))
        (setq tabulated-list-sort-key nil)
        (tabulated-list-init-header)
        (pcase (car case)
          ('native (tabulated-list-print))
          ('bulk (gnosis-tl-print))
          ('append
           (let ((inhibit-read-only t)) (erase-buffer))
           (gnosis-tl-append-entries tabulated-list-entries))
          ('replace
           (gnosis-tl-print)
           (gnosis-tl-replace-entry 1 (cadar tabulated-list-entries))))
        (goto-char (point-min))
        (move-to-column 8)
        (let ((auto-hscroll-mode nil))
          (set-window-hscroll (selected-window) 5)
          (redisplay t)
          (should (= 5 (window-hscroll)))
          (should (= 8 (current-column)))
          (should (equal "ID" (get-text-property (point) 'tabulated-list-column-name)))
          ;; Each action starts with a cold row, not a native warm-up print.
          (if (eq (cdr case) 'sort)
              (progn
                (execute-kbd-macro (kbd "S"))
                (should (equal '("ID") tabulated-list-sort-key)))
            (execute-kbd-macro (kbd "}"))
            (should (= 19 (cadr (aref tabulated-list-format 0))))
            (should (= 20 (cadr (aref tabulated-list-format 1))))
            (execute-kbd-macro (kbd "{"))
            (should (= 18 (cadr (aref tabulated-list-format 0)))))
          (should (= 1 (tabulated-list-get-id)))
          (execute-kbd-macro (kbd "M-<right>"))
          (should (equal "Text" (get-text-property (point) 'tabulated-list-column-name)))
          (redisplay t)
          (should (= 5 (window-hscroll))))))))

(provide 'gnosis-test-dashboard-hscroll)
;;; gnosis-test-dashboard-hscroll.el ends here

;;; gnosis-test-campaign-dashboard.el --- Collection refresh and retirement -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.
;; Author: Thanos Apollo <public@thanosapollo.org>

;;; Commentary:
;; Regress native collection routes against disposable SQLite data.

;;; Code:
(require 'gnosis-test-dashboard-native)

(ert-deftest gnosis-campaign-dashboard-retired-marks ()
  "Every mark command refuses associated, detached and mode successors."
  (dolist (transition '(associate detach mode))
    (dolist (key '("M" "U" "m" "u"))
      (gnosis-test-dashboard-native--with-view
        (gnosis-dashboard-output-themata '(1 2 3 4 5))
        (let ((command (key-binding (kbd key))))
          (gnosis-test-dashboard-file--draft)
          (pcase transition
            ('detach (set-visited-file-name nil t))
            ('mode (text-mode)))
          (setq-local gnosis-dashboard--selected-ids '(successor))
          (goto-char (+ (point-min) 3))
          (add-text-properties (point-min) (point-max) '(face italic))
          (let ((before (gnosis-test-dashboard-file--snapshot)))
            (should-error
             (if (eq transition 'mode)
                 (call-interactively command)
               (execute-kbd-macro (kbd key)))
             :type 'user-error)
            (should (equal before (gnosis-test-dashboard-file--snapshot)))
            (should (equal-including-properties (car before) (buffer-string))))
          (set-buffer-modified-p nil))))))

(ert-deftest gnosis-campaign-dashboard-revert-pending-data ()
  "Revert reads mutations once and retires old pending rows without losing context."
  (gnosis-test-with-db
    (cl-loop for id from 1 for text in '("Zulu" "Alpha" "Echo" "Bravo")
             do (gnosis-test--add-basic-thema text "Answer" nil nil id))
    (gnosis-test-dashboard--with-view
      (gnosis-dashboard-output-themata '(1 2 3 4))
      (let ((current-prefix-arg nil) (prefix-arg nil))
        (execute-kbd-macro (kbd "M C-0 S")))
      (gnosis-dashboard--goto-id 1)
      (move-to-column 3)
      (should callbacks)
      (with-temp-buffer
        (gnosis-toggle-suspend-themata '(1) 1 t)
        (gnosis-update 'themata '(= keimenon "Aardvark") '(= id 3))
        (gnosis-delete-themata '(2)))
      (revert-buffer nil t)
      (should-not gnosis-dashboard--pending-entries)
      (should (equal '(3 4 1) (gnosis-test-dashboard--visible-ids)))
      (should (equal '(1 3 4) gnosis-dashboard-themata-current-ids))
      (should (= 1 (tabulated-list-get-id)))
      (should (= 3 (current-column)))
      (should (equal '("Keimenon") tabulated-list-sort-key))
      (should (equal "Yes" (aref (tabulated-list-get-entry) 5)))
      (gnosis-test-dashboard--assert-marks '(1))
      (let ((before (buffer-string)))
        (drain)
        (should (equal-including-properties before (buffer-string))))
      (gnosis-tl-sort -1)
      (should (equal '(1 3 4) (gnosis-test-dashboard--visible-ids)))
      (gnosis-test-dashboard--assert-marks '(1)))))

(ert-deftest gnosis-campaign-dashboard-revert-tags ()
  "Native revert refreshes tag counts and excludes removed retained membership."
  (gnosis-test-with-db
    (gnosis-test--add-basic-thema "One" "A" '("one" "gone") nil 1)
    (gnosis-test--add-basic-thema "Two" "A" '("two") nil 2)
    (gnosis-test-dashboard--with-view
      (gnosis-dashboard-output-tags '("one" "gone"))
      (gnosis-dashboard--goto-id "one")
      (move-to-column 3)
      (gnosis-tl-sort 0)
      (with-temp-buffer
        (gnosis--insert-into 'thema-tag '([2 "one"]))
        (gnosis-sqlite-execute gnosis-db "DELETE FROM thema_tag WHERE tag = ?"
                               '("gone")))
      (revert-buffer nil t)
      (should (equal '("one") (gnosis-test-dashboard--visible-ids)))
      (should (equal ["one" "2"] (tabulated-list-get-entry)))
      (should (= 3 (current-column)))
      (should (equal '("Name") tabulated-list-sort-key))
      (execute-kbd-macro (kbd "g"))
      (gnosis-dashboard--goto-id "one")
      (should (equal ["one" "2"] (tabulated-list-get-entry))))))

(ert-deftest gnosis-campaign-dashboard-revert-nodes ()
  "Native revert and g refresh node titles and linked thema counts."
  (gnosis-test-with-db
    (gnosis-test--add-basic-thema "One" "A" nil nil 1)
    (gnosis--insert-into 'nodes '(["a" "a.org" "Old" 0 nil "0" "hash"]
                                 ["b" "b.org" "Other" 0 nil "0" "hash"]))
    (gnosis-test-dashboard--with-view
      (gnosis-dashboard-output-nodes '("a"))
      (gnosis-tl-sort 0)
      (move-to-column 3)
      (with-temp-buffer
        (gnosis-update 'nodes '(= title "New") '(= id "a"))
        (gnosis--insert-into 'thema-links '([1 "a"])))
      (revert-buffer nil t)
      (should (equal '("a") (gnosis-test-dashboard--visible-ids)))
      (should (equal "New" (aref (tabulated-list-get-entry) 0)))
      (should (equal "1" (aref (tabulated-list-get-entry) 3)))
      (should (= 3 (current-column)))
      (should (equal '("Title") tabulated-list-sort-key))
      (execute-kbd-macro (kbd "g"))
      (should (equal "New" (aref (tabulated-list-get-entry) 0)))
      (should (equal "1" (aref (tabulated-list-get-entry) 3)))
      (gnosis-sqlite-execute gnosis-db "DELETE FROM nodes WHERE id = ?" '("a"))
      (dotimes (_ 2)
        (revert-buffer nil t)
        (should-not tabulated-list-entries)
        (should-not gnosis-dashboard-nodes-current-ids)))))

(ert-deftest gnosis-campaign-dashboard-native-sort-delegation ()
  "The compatibility sorter retains original order, column and marks after append."
  (gnosis-test-dashboard-native--with-view
    (gnosis-dashboard-output-themata '(1 2 3 4 5))
    (execute-kbd-macro (kbd "M"))
    (gnosis-dashboard--goto-id 1)
    (move-to-column 3)
    (dotimes (_ 2) (gnosis-tl-sort 0))
    (gnosis-test-dashboard-native--drain)
    (gnosis-tl-sort -1)
    (should (equal '(1 2 3 4 5) (gnosis-test-dashboard--visible-ids)))
    (should (= 1 (tabulated-list-get-id)))
    (should (= 3 (current-column)))
    (gnosis-test-dashboard--assert-marks '(1 2))
    (execute-kbd-macro (kbd "u"))
    (gnosis-test-dashboard--assert-marks '(2))
    (gnosis-tl-print t)
    (tabulated-list-print t)
    (gnosis-test-dashboard--assert-marks '(2))
    (execute-kbd-macro (kbd "U"))
    (gnosis-test-dashboard--assert-marks nil)))

(ert-deftest gnosis-campaign-dashboard-standalone-sort ()
  "Standalone fast tables delegate sorting and original-order capture to Emacs."
  (dolist (count '(1 2))
    (with-temp-buffer
      (tabulated-list-mode)
      (setq tabulated-list-format [("Name" 12 t :pad-right 3) ("Value" 10 t)]
            tabulated-list-entries (list (list 1 ["Zulu" "2"])
                                         (list 2 ["Alpha" "1"])
                                         (list 3 ["Alpha" "3"])))
      (let ((input tabulated-list-entries)
            (before (copy-tree tabulated-list-entries t)))
        (gnosis-tl-print)
        (dotimes (_ count) (gnosis-tl-sort 0))
        (should (equal (if (= count 1) '(2 3 1) '(1 2 3))
                       (gnosis-test-dashboard--visible-ids)))
        (gnosis-tl-sort -1)
        (should (equal '(1 2 3) (gnosis-test-dashboard--visible-ids)))
        (should (equal before input))
        (goto-char (point-min))
        (tabulated-list-next-column)
        (gnosis-tl-sort)
        (should (equal '("Value") tabulated-list-sort-key))
        (should (equal '(2 1 3) (gnosis-test-dashboard--visible-ids)))))))

(ert-deftest gnosis-campaign-dashboard-refresh-failure ()
  "Failed native refresh retains the prior projection and pending callbacks."
  (gnosis-test-with-db
    (dolist (id '(1 2 3))
      (gnosis-test--add-basic-thema (number-to-string id) "A" nil nil id))
    (gnosis-test-dashboard--with-view
      (gnosis-dashboard-output-themata '(1 2 3))
      (let ((before (buffer-string))
            (generation gnosis-dashboard--load-generation))
        (cl-letf (((symbol-function 'gnosis-sqlite-select-batch)
                   (lambda (&rest _) (error "Injected query failure"))))
          (should-error (revert-buffer nil t)))
        (should (equal before (buffer-string)))
        (should (= generation gnosis-dashboard--load-generation))
        (drain)
        (should (equal '(1 2 3) (gnosis-test-dashboard--visible-ids)))))))

(ert-deftest gnosis-campaign-dashboard-refresh-replaced-database ()
  "Explicit refresh can adopt a new database, but not its predecessor's marks."
  (gnosis-test-with-db
    (gnosis-test--add-basic-thema "Old" "A" nil nil 1)
    (gnosis-test-dashboard--with-view
      (gnosis-dashboard-output-themata '(1))
      (execute-kbd-macro (kbd "M"))
      (gnosis-test-with-db
        (gnosis-test--add-basic-thema "New" "A" nil nil 1)
        (revert-buffer nil t)
        (should (equal "New" (aref (tabulated-list-get-entry) 0)))
        (should (eq gnosis-db gnosis-dashboard--database))
        (gnosis-test-dashboard--assert-marks nil)))))

(ert-deftest gnosis-campaign-dashboard-refresh-tag-order ()
  "Revert retains explicit unsorted tag membership order."
  (gnosis-test-with-db
    (gnosis-test--add-basic-thema "Q" "A" '("alpha" "zulu"))
    (gnosis-test-dashboard--with-view
      (gnosis-dashboard-output-tags '("zulu" "alpha"))
      (gnosis-tl-sort 0)
      (revert-buffer nil t)
      (gnosis-tl-sort -1)
      (should (equal '("zulu" "alpha") (gnosis-test-dashboard--visible-ids))))))

(provide 'gnosis-test-campaign-dashboard)
;;; gnosis-test-campaign-dashboard.el ends here

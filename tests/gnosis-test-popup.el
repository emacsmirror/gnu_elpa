;;; gnosis-test-popup.el --- Popup command compatibility tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;;; Commentary:

;; Exercise real popup rendering and public bindings, including source-loaded
;; declarations whose anonymous commands can lose their help descriptions.

;;; Code:

(require 'ert)
(require 'gnosis-dashboard)
(require 'gnosis-review)
(require 'gnosis-test-helpers)

(defmacro gnosis-test-popup-with-buffer (&rest body)
  "Run BODY in an owned buffer and clean up its popup and windows."
  (declare (indent 0) (debug t))
  `(save-window-excursion
     (let* ((buffer (generate-new-buffer " *Gnosis popup test*"))
            (gnosis-dashboard-buffer-name (buffer-name buffer))
            (gnosis-dashboard--history nil)
            (keymap-popup--buffer-name " *Gnosis popup help test*")
            (keymap-popup-persistent nil)
            (keymap-popup-backend #'keymap-popup-backend-side-window))
       (unwind-protect
           (progn
             (switch-to-buffer buffer)
             (gnosis-dashboard-mode)
             ,@body)
         (keymap-popup-dismiss)
         (when (buffer-live-p buffer) (kill-buffer buffer))))))

(defun gnosis-test-popup--visible (map labels)
  "Show MAP and assert that all LABELS appear in its visible help."
  (keymap-popup map)
  (let ((buffer (get-buffer keymap-popup--buffer-name)))
    (should (get-buffer-window buffer))
    (dolist (label labels)
      (should (string-match-p (regexp-quote label)
                              (with-current-buffer buffer (buffer-string)))))))

(defun gnosis-test-popup--key (key)
  "Execute KEY through the command loop, allowing normal C-g cancellation."
  (if (equal key "C-g")
      (condition-case nil
          (execute-kbd-macro (kbd key))
        (quit nil))
    (execute-kbd-macro (kbd key))))

(ert-deftest gnosis-test-popup-nodes-visible ()
  "Nodes help includes all navigation commands and dismisses with C-g."
  (gnosis-test-popup-with-buffer
    (gnosis-test-popup--visible gnosis-dashboard-nodes-map
                               '("View all nodes" "View nodes by tag"
                                 "View isolated nodes"))
    (gnosis-test-popup--key "C-g")
    (should-not (get-buffer-window keymap-popup--buffer-name))))

(ert-deftest gnosis-test-popup-themata-visible ()
  "Themata help keeps its all-themata command."
  (gnosis-test-popup-with-buffer
    (gnosis-test-popup--visible gnosis-dashboard-themata-map
                               '("View all themata" "Search themata"))))

(ert-deftest gnosis-test-popup-maintenance-visible ()
  "Maintenance help keeps both sync commands."
  (gnosis-test-popup-with-buffer
    (gnosis-test-popup--visible gnosis-dashboard-maintenance-map
                               '("Sync nodes" "Rebuild nodes"))))

(ert-deftest gnosis-test-popup-sort-visible ()
  "Sorting help keeps every column command."
  (gnosis-test-popup-with-buffer
    (gnosis-test-popup--visible gnosis-dashboard-nodes-sort-map
                               '("Title" "Links" "Backlinks" "Themata"))))

(ert-deftest gnosis-test-popup-review-visible ()
  "Review help keeps every scheduled-selection command."
  (gnosis-test-popup-with-buffer
    (gnosis-test-popup--visible gnosis-review-map
                               '("Due themata" "Due themata of tag(s)"
                                 "Overdue themata" "Due without overdue"
                                 "All themata of tag(s)"))))

(ert-deftest gnosis-test-popup-node-navigation ()
  "Popup actions reset history, filter isolated nodes, and retain Back."
  (gnosis-test-with-db
    (gnosis--insert-into
     'nodes '(["a" "a.org" "Alpha" 0 nil "0" "a"]
              ["b" "b.org" "Beta" 0 nil "0" "b"]
              ["i" "i.org" "Isolated" 0 nil "0" "i"]))
    (gnosis--insert-into 'node-links '(["a" "b"]))
    (gnosis-test-popup-with-buffer
      (use-local-map gnosis-dashboard-mode-map)
      (setq gnosis-dashboard--history '((:type old)))
      (gnosis-test-popup--key "n")
      (gnosis-test-popup--key "a")
      (should (derived-mode-p 'gnosis-dashboard-nodes-mode))
      (should (= (length tabulated-list-entries) 3))
      (should-not gnosis-dashboard--history)
      (should (string-match-p "Alpha" (buffer-string)))
      (gnosis-test-popup--visible gnosis-dashboard-nodes-map
                                 '("View isolated nodes"))
      (setq gnosis-dashboard--history '((:type old)))
      (gnosis-test-popup--key "i")
      (should (equal gnosis-dashboard-nodes-current-ids '("i")))
      (should (equal (mapcar #'car tabulated-list-entries) '("i")))
      (should (= (length gnosis-dashboard--history) 1))
      (should (eq (plist-get (car gnosis-dashboard--history) :type) 'nodes))
      (gnosis-test-popup--key "q")
      (should (= (length tabulated-list-entries) 3))
      (should-not gnosis-dashboard--history)
      (gnosis-test-popup--key "g")
      (should (= (length tabulated-list-entries) 3)))))

(ert-deftest gnosis-test-popup-themata-navigation ()
  "All-themata dispatch clears old navigation and displays stored questions."
  (gnosis-test-with-db
    (let ((id (gnosis-test--add-basic-thema "Popup question" "Answer")))
      (gnosis-test-popup-with-buffer
        (setq gnosis-dashboard--history '((:type old)))
        (gnosis-test-popup--visible gnosis-dashboard-themata-map
                                   '("View all themata"))
        (gnosis-test-popup--key "a")
        (should (derived-mode-p 'gnosis-dashboard-themata-mode))
        (should (equal gnosis-dashboard-themata-current-ids (list id)))
        (should (string-match-p "Popup question" (buffer-string)))
        (should-not gnosis-dashboard--history)))))

(ert-deftest gnosis-test-popup-sorting-dispatch ()
  "Popup and direct sorting bindings preserve direction and reset point."
  (gnosis-test-popup-with-buffer
    (tabulated-list-mode)
    (setq tabulated-list-format
          [("Title" 12 t) ("Links" 8 gnosis-dashboard-sort-count)
           ("Backlinks" 10 gnosis-dashboard-sort-count)
           ("Themata" 8 gnosis-dashboard-sort-count)]
          tabulated-list-entries
          '(("a" ["Alpha" "1" "3" "2"])
            ("b" ["Beta" "3" "2" "1"])
            ("c" ["Charlie" "2" "1" "3"])))
    (dolist (popup '(nil t))
      (pcase-dolist (`(,key ,column ,reverse ,first)
                    '(("C-t" "Title" nil "a") ("l" "Links" t "b")
                      ("b" "Backlinks" t "a") ("t" "Themata" t "c")))
        (if popup
            (gnosis-test-popup--visible gnosis-dashboard-nodes-sort-map
                                       (list column))
          (use-local-map gnosis-dashboard-nodes-sort-map))
        (goto-char (point-max))
        (gnosis-test-popup--key key)
        (should (equal tabulated-list-sort-key (cons column reverse)))
        (should (= (point) (point-min)))
        (should (equal (tabulated-list-get-id) first))))))

(ert-deftest gnosis-test-popup-rebuild-dispatch ()
  "Rebuild passes the same force flag, while cancellation does nothing."
  (gnosis-test-popup-with-buffer
    (let (calls)
      (cl-letf (((symbol-function 'gnosis-nodes-db-sync)
                 (lambda (&optional force) (push force calls))))
        (gnosis-test-popup--visible gnosis-dashboard-maintenance-map
                                   '("Rebuild nodes"))
        (gnosis-test-popup--key "C-g")
        (should-not calls)
        (gnosis-test-popup--visible gnosis-dashboard-maintenance-map
                                   '("Rebuild nodes"))
        (gnosis-test-popup--key "S")
        (should (equal calls '(t)))))))

(ert-deftest gnosis-test-popup-review-selection-dispatch ()
  "Review keys defer prompts and collection until the review loop calls them."
  (gnosis-test-popup-with-buffer
    (pcase-dolist (`(,key ,selection)
                  '(("d" due) ("t" due-tags) ("o" overdue)
                    ("w" without-overdue) ("T" tags)))
      (let (collector collected prompted)
        (cl-letf (((symbol-function 'gnosis-review-loop)
                   (lambda (fn) (setq collector fn)))
                  ((symbol-function 'gnosis-review--read-selection)
                   (lambda (kind) (setq prompted kind) (list kind "tag")))
                  ((symbol-function 'gnosis-review--selection-ids)
                   (lambda (spec) (setq collected spec) '(42))))
          (keymap-popup gnosis-review-map)
          (gnosis-test-popup--key key)
          (should-not prompted)
          (should-not collected)
          (should (equal (funcall collector) '(42)))
          (if (memq selection '(due-tags tags))
              (progn (should (eq prompted selection))
                     (should (equal collected (list selection "tag"))))
            (should-not prompted)
            (should (equal collected (list selection)))))))))

(provide 'gnosis-test-popup)
;;; gnosis-test-popup.el ends here

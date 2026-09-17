;;; gnosis-test-dashboard-native.el --- Native collection contracts -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.
;; Author: Thanos Apollo <public@thanosapollo.org>

;;; Commentary:
;; Exercise native command dispatch, real progressive timers and retired views.

;;; Code:
(require 'gnosis-test-helpers)
(require 'gnosis-dashboard)
(require 'gnosis-test-dashboard-file-owner)
(require 'gnosis-test-popup)

(defmacro gnosis-test-dashboard-native--with-view (&rest body)
  "Run BODY with disposable data, windows and a fresh dashboard."
  (declare (indent 0) (debug t))
  `(gnosis-test-with-db
     (let ((before (buffer-list))
           (current-prefix-arg nil)
           (prefix-arg nil)
           (gnosis-dashboard-buffer-name " *Gnosis native test*")
           (gnosis-dashboard-render-chunk-size 2)
           (gnosis-dashboard-timer-delay 0.01))
       (unwind-protect
           (save-window-excursion
             (cl-loop for id from 1
                      for text in '("Zulu" "Alpha" "Echo" "Bravo" "Charlie")
                      do (gnosis-test--add-basic-thema text "Answer" nil nil id))
             ,@body)
         (dolist (buffer (seq-difference (buffer-list) before))
           (when (buffer-live-p buffer)
             (with-current-buffer buffer (set-buffer-modified-p nil))
             (kill-buffer buffer)))))))

(defun gnosis-test-dashboard-native--drain ()
  "Deliver real pending timers with a finite wait."
  (cl-loop repeat 100 while gnosis-dashboard--pending-entries
           do (accept-process-output nil 0.02))
  (should-not gnosis-dashboard--pending-entries)
  (should-not gnosis-dashboard--timer))

(defun gnosis-test-dashboard-native--header-event ()
  "Return an event on the real native first-column header."
  (let* ((header (car (last header-line-format)))
         (pos (text-property-not-all 0 (length header)
                                     'tabulated-list-column-name nil header)))
    (list 'mouse-1 (list (selected-window) 'header-line '(0 . 0)
                        0 (cons header pos)))))

(defun gnosis-test-dashboard-native--header-sort (event)
  "Dispatch EVENT through its native header map and command remapping."
  (let* ((object (posn-object (event-start event)))
         (map (get-text-property (cdr object) 'keymap (car object)))
         (command (lookup-key map [header-line mouse-1])))
    (should (eq command 'tabulated-list-col-sort))
    (funcall (or (command-remapping command) command) event)))

(ert-deftest gnosis-dashboard-native-retired-routes ()
  "Native keys, headers and retained reverts preserve successor drafts."
  (dolist (view '(themata tags nodes history))
    (dolist (transition '(associate detach mode))
      (gnosis-test-dashboard-native--with-view
        (pcase view
          ('themata (gnosis-dashboard-output-themata '(1 2 3 4 5)))
          ('tags (gnosis-dashboard-output-tags))
          ('nodes (gnosis-dashboard-output-nodes))
          ('history (gnosis-dashboard-history '((20260101 3 1)))))
        (let ((header (gnosis-test-dashboard-native--header-event))
              (revert revert-buffer-function)
              (commands (mapcar (lambda (key) (key-binding (kbd key)))
                                (if (eq view 'nodes) '("}" "{") '("S" "}" "{")))))
          (gnosis-test-dashboard-file--draft)
          (pcase transition
            ('detach (set-visited-file-name nil t))
            ('mode (text-mode)))
          (let ((before (gnosis-test-dashboard-file--snapshot))
                (format (copy-tree tabulated-list-format t))
                (sort-key (copy-tree tabulated-list-sort-key)))
            (dolist (command commands)
              (let ((current-prefix-arg 0))
                (should-error (call-interactively command) :type 'user-error)))
            (if (eq transition 'mode)
                ;; Core header dispatch is a no-op outside tabulated-list.
                (gnosis-test-dashboard-native--header-sort header)
              (should-error (gnosis-test-dashboard-native--header-sort header)))
            (should-error (funcall revert nil t))
            (unless (eq transition 'mode)
              (should-error (tabulated-list-print t) :type 'user-error))
            (accept-process-output nil 0.03)
            (should (equal-including-properties
                     (car before) (car (gnosis-test-dashboard-file--snapshot))))
            (should (equal before (gnosis-test-dashboard-file--snapshot)))
            (should (equal format tabulated-list-format))
            (should (equal sort-key tabulated-list-sort-key))))))))

(ert-deftest gnosis-dashboard-native-retired-node-sorts ()
  "Named sort keys, popups and retained commands preserve successor state."
  (dolist (transition '(associate detach mode))
    (dolist (route '(key popup retained))
      (dolist (key '("C-t" "l" "b" "t"))
        (ert-info ((format "%s / %s / %s" transition route key))
          (gnosis-test-with-db
            (gnosis-test-popup-with-buffer
              (gnosis-test-popup--nodes)
              (let ((command (lookup-key gnosis-dashboard-nodes-sort-map
                                         (kbd key)))
                    (popup (key-binding (kbd "s"))))
                (gnosis-test-dashboard-file--draft)
                (pcase transition
                  ('detach (set-visited-file-name nil t))
                  ('mode (text-mode)))
                ;; Give the successor nonempty state even after mode reset.
                (setq-local header-line-format
                            (propertize "Successor header λ" 'face 'warning))
                (setq-local tabulated-list-sort-key '("Successor" . t))
                (setq-local gnosis-dashboard--selected-ids '(successor))
                (add-text-properties (point-min) (point-max) '(face italic))
                (goto-char (+ (point-min) 3))
                (let ((before (gnosis-test-dashboard-file--snapshot))
                      (sort-key (copy-tree tabulated-list-sort-key))
                      (format (copy-tree tabulated-list-format t)))
                  (should-error
                   (pcase route
                     ;; Retain the old sort map without restoring its owner.
                     ('key
                      (let ((overriding-local-map gnosis-dashboard-nodes-sort-map))
                        (execute-kbd-macro (kbd key))))
                     ('popup
                      ;; The native binding survives association/detachment;
                      ;; after mode loss exercise its retained launcher.
                      (if (eq transition 'mode)
                          (call-interactively popup)
                        (execute-kbd-macro (kbd "s")))
                      (should (get-buffer-window keymap-popup--buffer-name))
                      (execute-kbd-macro (kbd key)))
                     ('retained (call-interactively command)))
                   :type 'user-error)
                  (should (equal before (gnosis-test-dashboard-file--snapshot)))
                  (should (equal-including-properties (car before) (buffer-string)))
                  (should (equal-including-properties
                           (nth 5 before) header-line-format))
                  (should (equal sort-key tabulated-list-sort-key))
                  (should (equal format tabulated-list-format)))
                (set-buffer-modified-p nil)))))))))

(ert-deftest gnosis-dashboard-native-original-order ()
  "Native original order survives partial sort, delivery, edit and deletion."
  (dolist (early-restore '(nil t))
    (gnosis-test-dashboard-native--with-view
      (gnosis-dashboard-output-themata '(1 2 3 4 5))
      (should (equal '(1 2) (gnosis-test-dashboard--visible-ids)))
      (execute-kbd-macro (kbd "M"))
      (gnosis-dashboard--goto-id 1)
      (move-to-column 3)
      (execute-kbd-macro (kbd "C-0 S C-0 S"))
      (should gnosis-dashboard--pending-entries)
      (when early-restore (execute-kbd-macro (kbd "C-- 1 S")))
      (gnosis-test-dashboard-native--drain)
      (execute-kbd-macro (kbd "C-- 1 S"))
      (should (equal '(1 2 3 4 5) (gnosis-test-dashboard--visible-ids)))
      (should (= 1 (tabulated-list-get-id)))
      (gnosis-test-dashboard--assert-marks '(1 2))
      (gnosis-update 'themata '(= keimenon "Updated") '(= id 3))
      (gnosis-dashboard-update-entry 3)
      (gnosis-delete-themata '(4))
      (gnosis-dashboard--remove-entries '(4))
      (execute-kbd-macro (kbd "S S C-- 1 S"))
      (should (equal '(1 2 3 5) (gnosis-test-dashboard--visible-ids)))
      (should (= 1 (tabulated-list-get-id)))
      (gnosis-test-dashboard--assert-marks '(1 2)))))

(ert-deftest gnosis-dashboard-native-columns-and-refresh ()
  "Cold native sort, movement, header and width commands work on fast rows."
  (gnosis-test-dashboard-native--with-view
    (gnosis-update 'themata '(= keimenon "Άλφα 界 λ") '(= id 1))
    (gnosis-dashboard-output-themata '(1 2 3 4 5))
    (move-to-column 3)
    (should (equal "Keimenon" (get-text-property (point) 'tabulated-list-column-name)))
    (execute-kbd-macro (kbd "M-<right>"))
    (should (equal "Hypothesis" (get-text-property (point) 'tabulated-list-column-name)))
    (execute-kbd-macro (kbd "M-<left> S"))
    (gnosis-test-dashboard-native--drain)
    (gnosis-dashboard--goto-id 5)
    (move-to-column 3)
    (should (equal "Keimenon" (get-text-property (point) 'tabulated-list-column-name)))
    (gnosis-dashboard-update-entry 5)
    (gnosis-dashboard--goto-id 5)
    (move-to-column 3)
    (execute-kbd-macro (kbd "m"))
    (gnosis-dashboard--goto-id 5)
    (move-to-column 3)
    (let ((width (cadr (aref tabulated-list-format 0))))
      (execute-kbd-macro (kbd "}"))
      (should (= (1+ width) (cadr (aref tabulated-list-format 0))))
      (execute-kbd-macro (kbd "{"))
      (should (= width (cadr (aref tabulated-list-format 0)))))
    (gnosis-test-dashboard-native--header-sort
     (gnosis-test-dashboard-native--header-event))
    (should (= 5 (tabulated-list-get-id)))
    (gnosis-test-dashboard--assert-marks '(5))
    (execute-kbd-macro (kbd "g"))
    (gnosis-test-dashboard-native--drain)
    (should (= 5 (length (gnosis-test-dashboard--visible-ids))))))

(ert-deftest gnosis-dashboard-native-history-lifetime ()
  "History refresh and public reopen cannot revive a detached former owner."
  (gnosis-test-dashboard-native--with-view
    (gnosis-study-accept-practice
     (list :mode 'practice :thema-id 1 :session-id "native-session" :attempt 1
           :event-id "native-event" :reviewed-at-us 1789606800000000 :outcome 'success))
    (gnosis-dashboard-history)
    (execute-kbd-macro (kbd "g RET"))
    (with-current-buffer "*Gnosis Practice Evidence*"
      (should (string-search "native-event" (buffer-string))))
    (switch-to-buffer "*Gnosis History*")
    (gnosis-test-dashboard-file--draft)
    (set-visited-file-name nil t)
    (rename-buffer "*Gnosis History*")
    (let ((before (gnosis-test-dashboard-file--snapshot)))
      (should-error (call-interactively #'gnosis-dashboard-history-refresh)
                    :type 'user-error)
      (should-error (gnosis-dashboard-history) :type 'user-error)
      (should (equal before (gnosis-test-dashboard-file--snapshot))))))

(provide 'gnosis-test-dashboard-native)
;;; gnosis-test-dashboard-native.el ends here

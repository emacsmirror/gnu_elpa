;;; forgejo-test-buffer.el --- Tests for forgejo-buffer  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for shared display utilities: state formatting, label
;; colorization, relative time, login extraction, and EWOC node building.

;;; Code:

(require 'ert)
(require 'cl-lib)

(load (expand-file-name "../lisp/forgejo.el"
       (file-name-directory (or load-file-name buffer-file-name))))
(load (expand-file-name "../lisp/forgejo-api.el"
       (file-name-directory (or load-file-name buffer-file-name))))
(load (expand-file-name "../lisp/forgejo-buffer.el"
       (file-name-directory (or load-file-name buffer-file-name))))

;;; Group 1: State formatting

(ert-deftest forgejo-test-buffer-format-state-open ()
  "Open state uses the open face."
  (let ((result (forgejo-buffer--format-state "open")))
    (should (string= result "open"))
    (should (eq (get-text-property 0 'face result) 'forgejo-open-face))))

(ert-deftest forgejo-test-buffer-format-state-closed ()
  "Closed state uses the closed face."
  (let ((result (forgejo-buffer--format-state "closed")))
    (should (eq (get-text-property 0 'face result) 'forgejo-closed-face))))

;;; Group 2: Label formatting

(ert-deftest forgejo-test-buffer-format-labels ()
  "Labels are joined with commas and propertized with readable colors."
  (let* ((labels '(((name . "bug") (color . "d73a4a"))
                   ((name . "help") (color . "0075ca"))))
         (result (forgejo-buffer--format-labels labels)))
    (should (string-match-p "bug" result))
    (should (string-match-p "help" result))
    (should (string-match-p ", " result))
    (should (eq (plist-get (get-text-property 0 'face result) :weight) 'bold))
    (should (plist-get (get-text-property 0 'face result) :foreground))))

(ert-deftest forgejo-test-buffer-format-labels-empty ()
  "Empty labels return empty string."
  (should (string= (forgejo-buffer--format-labels nil) ""))
  (should (string= (forgejo-buffer--format-labels '()) "")))

;;; Group 3: Relative time

(ert-deftest forgejo-test-buffer-relative-time-nil ()
  "Nil or empty time returns empty string."
  (should (string= (forgejo-buffer--relative-time nil) ""))
  (should (string= (forgejo-buffer--relative-time "") "")))

(ert-deftest forgejo-test-buffer-relative-time-old ()
  "Very old time returns a date string."
  (let ((result (forgejo-buffer--relative-time "2020-01-01T00:00:00Z")))
    (should (string-match-p "2020-01-01" result))))

;;; Group 4: Login extraction

(ert-deftest forgejo-test-buffer-login ()
  "Extract login from user alist."
  (should (string= (forgejo-buffer--login '((login . "alice"))) "alice"))
  (should (null (forgejo-buffer--login :null)))
  (should (null (forgejo-buffer--login nil))))

;;; Group 5: EWOC node building

(ert-deftest forgejo-test-buffer-build-nodes ()
  "Build EWOC nodes from issue data and timeline."
  (let* ((issue '((number . 42) (title . "Test") (state . "open")
                  (body . "Description") (user . ((login . "alice")))
                  (labels) (milestone) (comments . 2)
                  (created_at . "2026-01-01T00:00:00Z")))
         (timeline `(((type . "comment") (body . "LGTM")
                      (user . ((login . "bob")))
                      (created_at . "2026-01-02T00:00:00Z"))
                     ((type . "close") (user . ((login . "alice")))
                      (created_at . "2026-01-03T00:00:00Z"))))
         (nodes (forgejo-buffer--build-nodes issue timeline)))
    (should (= (length nodes) 3))
    (should (eq (plist-get (nth 0 nodes) :type) 'header))
    (should (eq (plist-get (nth 1 nodes) :type) 'comment))
    (should (eq (plist-get (nth 2 nodes) :type) 'event))))

(ert-deftest forgejo-test-buffer-build-nodes-empty-timeline ()
  "Issue with no timeline produces only header node."
  (let* ((issue '((number . 1) (title . "Solo") (state . "open")
                  (body . "") (user . ((login . "me")))
                  (labels) (milestone) (comments . 0)
                  (created_at . "2026-01-01T00:00:00Z")))
         (nodes (forgejo-buffer--build-nodes issue nil)))
    (should (= (length nodes) 1))
    (should (eq (plist-get (car nodes) :type) 'header))))

;;; Group 6: Clean body

(ert-deftest forgejo-test-buffer-clean-body ()
  "Strip carriage returns, handle nil and :null."
  (should (string= (forgejo-buffer--clean-body "hello\r\nworld") "hello\nworld"))
  (should (null (forgejo-buffer--clean-body nil)))
  (should (null (forgejo-buffer--clean-body :null)))
  (should (null (forgejo-buffer--clean-body ""))))

(provide 'forgejo-test-buffer)
;;; forgejo-test-buffer.el ends here

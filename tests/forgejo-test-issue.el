;;; forgejo-test-issue.el --- Tests for forgejo-issue  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for issue-specific logic: entry formatting and API param building.

;;; Code:

(require 'ert)
(require 'cl-lib)

(load (expand-file-name "../forgejo.el"
       (file-name-directory (or load-file-name buffer-file-name))))
(load (expand-file-name "../forgejo-tl.el"
       (file-name-directory (or load-file-name buffer-file-name))))
(load (expand-file-name "../forgejo-api.el"
       (file-name-directory (or load-file-name buffer-file-name))))
(load (expand-file-name "../forgejo-buffer.el"
       (file-name-directory (or load-file-name buffer-file-name))))
(load (expand-file-name "../forgejo-issue.el"
       (file-name-directory (or load-file-name buffer-file-name))))

;;; ---- Group 1: Entry conversion ----

(ert-deftest forgejo-test-issue-entries ()
  "Convert API issues to tabulated-list entries."
  (let* ((issues `(((number . 42)
                    (state . "open")
                    (title . "Test issue")
                    (labels . (((name . "bug") (color . "ff0000"))))
                    (user . ((login . "alice")))
                    (updated_at . "2020-06-15T10:00:00Z"))))
         (entries (forgejo-issue--entries issues)))
    (should (= (length entries) 1))
    (let ((entry (car entries)))
      (should (= (car entry) 42))
      (let ((cols (cadr entry)))
        (should (string= (aref cols 0) "42"))
        (should (string= (aref cols 2) "Test issue"))
        (should (string-match-p "alice" (aref cols 4)))))))

;;; ---- Group 2: Build params ----

(ert-deftest forgejo-test-issue-build-params-default ()
  "Default params include type=issues and sort."
  (let ((forgejo-default-sort "recentupdate")
        (forgejo--api-default-limit 30))
    (let ((params (forgejo-issue--build-params nil)))
      (should (assoc "type" params))
      (should (string= (cdr (assoc "type" params)) "issues"))
      (should (assoc "sort" params)))))

(ert-deftest forgejo-test-issue-build-params-with-filters ()
  "Filters are included in params."
  (let ((forgejo-default-sort "recentupdate")
        (forgejo--api-default-limit 30))
    (let ((params (forgejo-issue--build-params
                   '(:state "open" :query "bug" :page 2))))
      (should (string= (cdr (assoc "state" params)) "open"))
      (should (string= (cdr (assoc "q" params)) "bug"))
      (should (string= (cdr (assoc "page" params)) "2")))))

(provide 'forgejo-test-issue)
;;; forgejo-test-issue.el ends here

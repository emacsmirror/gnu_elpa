;;; forgejo-test-pull.el --- Tests for forgejo-pull  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for PR entry formatting and param building.

;;; Code:

(require 'ert)
(require 'cl-lib)

(load (expand-file-name "../lisp/forgejo.el"
       (file-name-directory (or load-file-name buffer-file-name))))
(load (expand-file-name "../lisp/forgejo-tl.el"
       (file-name-directory (or load-file-name buffer-file-name))))
(load (expand-file-name "../lisp/forgejo-api.el"
       (file-name-directory (or load-file-name buffer-file-name))))
(load (expand-file-name "../lisp/forgejo-buffer.el"
       (file-name-directory (or load-file-name buffer-file-name))))
(load (expand-file-name "../lisp/forgejo-pull.el"
       (file-name-directory (or load-file-name buffer-file-name))))

;;; ---- Group 1: PR entry formatting ----

(ert-deftest forgejo-test-pull-entries ()
  "Convert API PR alists to tabulated-list entries."
  (let* ((pulls `(((number . 10)
                   (state . "open")
                   (title . "Add feature X")
                   (labels . (((name . "enhancement") (color . "00ff00"))))
                   (user . ((login . "dev")))
                   (updated_at . "2020-03-10T08:00:00Z"))))
         (entries (forgejo-pull--entries pulls)))
    (should (= (length entries) 1))
    (let ((entry (car entries)))
      (should (= (car entry) 10))
      (let ((cols (cadr entry)))
        (should (string= (aref cols 0) "10"))
        (should (string= (aref cols 2) "Add feature X"))
        (should (string-match-p "dev" (aref cols 4)))))))

;;; ---- Group 2: Build params ----

(ert-deftest forgejo-test-pull-build-params-default ()
  "Default PR params include sort and limit."
  (let ((forgejo-default-sort "recentupdate")
        (forgejo--api-default-limit 30))
    (let ((params (forgejo-pull--build-params nil)))
      (should (assoc "sort" params))
      (should (assoc "limit" params))
      (should-not (assoc "type" params)))))

(ert-deftest forgejo-test-pull-build-params-with-filters ()
  "Filters are included in PR params."
  (let ((forgejo-default-sort "recentupdate")
        (forgejo--api-default-limit 30))
    (let ((params (forgejo-pull--build-params
                   '(:state "closed" :poster "alice" :page 3))))
      (should (string= (cdr (assoc "state" params)) "closed"))
      (should (string= (cdr (assoc "poster" params)) "alice"))
      (should (string= (cdr (assoc "page" params)) "3")))))

(provide 'forgejo-test-pull)
;;; forgejo-test-pull.el ends here

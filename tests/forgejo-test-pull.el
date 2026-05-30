;;; forgejo-test-pull.el --- Tests for forgejo-pull  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for PR entry formatting and param building.

;;; Code:

(require 'ert)
(require 'cl-lib)

(setq forgejo-markdown-mode 'text-mode)
(require 'forgejo-pull)

;;; Group 1: PR entry formatting

(ert-deftest forgejo-test-pull-entries ()
  "Convert API PR alists to tabulated-list entries."
  (let* ((pulls `(((number . 10)
                   (state . "open")
                   (title . "Add feature X")
                   (labels . (((name . "enhancement") (color . "00ff00"))))
                   (user . ((login . "dev")))
                   (updated_at . "2020-03-10T08:00:00Z"))))
         (entries (forgejo-filter-list-entries pulls)))
    (should (= (length entries) 1))
    (let ((entry (car entries)))
      (should (= (car entry) 10))
      (let ((cols (cadr entry)))
        (should (string= (aref cols 0) "10"))
        (should (string= (aref cols 2) "Add feature X"))
        (should (string-match-p "dev" (aref cols 4)))))))

;;; Group 2: Build params

(ert-deftest forgejo-test-pull-build-params-default ()
  "Default PR params include type=pulls, sort, and limit."
  (let ((forgejo-default-sort "recentupdate")
        (forgejo--api-default-limit 30))
    (let ((params (forgejo-pull--build-params nil)))
      (should (assoc "sort" params))
      (should (assoc "limit" params))
      (should (string= (cdr (assoc "type" params)) "pulls")))))

(ert-deftest forgejo-test-pull-build-params-with-filters ()
  "Filters are included in PR params via issues endpoint."
  (let ((forgejo-default-sort "recentupdate")
        (forgejo--api-default-limit 30))
    (let ((params (forgejo-pull--build-params
                   '(:state "closed" :author "alice" :page 3))))
      (should (string= (cdr (assoc "state" params)) "closed"))
      (should (string= (cdr (assoc "created_by" params)) "alice"))
      (should (string= (cdr (assoc "page" params)) "3")))))

;;; Group 3: Sync finalization

(ert-deftest forgejo-test-pull-sync-filtered-force-does-not-close-missing ()
  "Filtered forced syncs must not mark unrelated cached PRs closed."
  (let (close-called sync-called)
    (cl-letf (((symbol-function 'forgejo-api-get) (lambda (&rest _args) nil))
              ((symbol-function 'forgejo-api-get-paged)
               (lambda (_host _endpoint _params _page-callback done-callback)
                 (funcall done-callback '(((number . 1))) '(:total-count 1))))
              ((symbol-function 'forgejo-db-close-missing)
               (lambda (&rest _args) (setq close-called t)))
              ((symbol-function 'forgejo-db-set-sync-time)
               (lambda (&rest _args) (setq sync-called t))))
      (forgejo-pull--sync "https://codeberg.org" "codeberg.org"
                          "owner" "repo" '(:state "open" :labels "bug")
                          " *forgejo-test-missing*" t)
      (should-not close-called)
      (should sync-called))))

(ert-deftest forgejo-test-pull-sync-partial-does-not-finalize ()
  "Partial forced syncs must not close missing PRs or advance sync time."
  (let (close-called sync-called)
    (cl-letf (((symbol-function 'forgejo-api-get) (lambda (&rest _args) nil))
              ((symbol-function 'forgejo-api-get-paged)
               (lambda (_host _endpoint _params _page-callback done-callback)
                 (funcall done-callback '(((number . 1))) '(:partial t))))
              ((symbol-function 'forgejo-db-close-missing)
               (lambda (&rest _args) (setq close-called t)))
              ((symbol-function 'forgejo-db-set-sync-time)
               (lambda (&rest _args) (setq sync-called t))))
      (forgejo-pull--sync "https://codeberg.org" "codeberg.org"
                          "owner" "repo" '(:state "open")
                          " *forgejo-test-missing*" t)
      (should-not close-called)
      (should-not sync-called))))

(provide 'forgejo-test-pull)
;;; forgejo-test-pull.el ends here

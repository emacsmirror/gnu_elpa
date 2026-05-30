;;; forgejo-test-issue.el --- Tests for forgejo-issue  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for issue-specific logic: entry formatting and API param building.

;;; Code:

(require 'ert)
(require 'cl-lib)

(defvar forgejo-markdown-mode)

(setq forgejo-markdown-mode 'text-mode)
(require 'forgejo-issue)

;;; Group 1: Entry conversion

(ert-deftest forgejo-test-issue-entries ()
  "Convert API issues to tabulated-list entries."
  (let* ((issues `(((number . 42)
                    (state . "open")
                    (title . "Test issue")
                    (labels . (((name . "bug") (color . "ff0000"))))
                    (user . ((login . "alice")))
                    (updated_at . "2020-06-15T10:00:00Z"))))
         (entries (forgejo-filter-list-entries issues)))
    (should (= (length entries) 1))
    (let ((entry (car entries)))
      (should (= (car entry) 42))
      (let ((cols (cadr entry)))
        (should (string= (aref cols 0) "42"))
        (should (string= (aref cols 2) "Test issue"))
        (should (string-match-p "alice" (aref cols 4)))))))

;;; Group 2: Build params

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

;;; Group 3: Sync finalization

(ert-deftest forgejo-test-issue-sync-filtered-force-does-not-close-missing ()
  "Filtered forced syncs must not mark unrelated cached issues closed."
  (let (close-called sync-called)
    (cl-letf (((symbol-function 'forgejo-api-get) (lambda (&rest _args) nil))
              ((symbol-function 'forgejo-api-get-paged)
               (lambda (_host _endpoint _params _page-callback done-callback)
                 (funcall done-callback '(((number . 1))) '(:total-count 1))))
              ((symbol-function 'forgejo-db-close-missing)
               (lambda (&rest _args) (setq close-called t)))
              ((symbol-function 'forgejo-db-set-sync-time)
               (lambda (&rest _args) (setq sync-called t))))
      (forgejo-issue--sync "https://codeberg.org" "codeberg.org"
                           "owner" "repo" '(:state "open" :labels "bug")
                           " *forgejo-test-missing*" t)
      (should-not close-called)
      (should sync-called))))

(ert-deftest forgejo-test-issue-sync-partial-does-not-finalize ()
  "Partial forced syncs must not close missing issues or advance sync time."
  (let (close-called sync-called)
    (cl-letf (((symbol-function 'forgejo-api-get) (lambda (&rest _args) nil))
              ((symbol-function 'forgejo-api-get-paged)
               (lambda (_host _endpoint _params _page-callback done-callback)
                 (funcall done-callback '(((number . 1))) '(:partial t))))
              ((symbol-function 'forgejo-db-close-missing)
               (lambda (&rest _args) (setq close-called t)))
              ((symbol-function 'forgejo-db-set-sync-time)
               (lambda (&rest _args) (setq sync-called t))))
      (forgejo-issue--sync "https://codeberg.org" "codeberg.org"
                           "owner" "repo" '(:state "open")
                           " *forgejo-test-missing*" t)
      (should-not close-called)
      (should-not sync-called))))

;;; Group 4: Detail view entry

(defun forgejo-test-issue--issue-alist ()
  "Return an issue alist for detail view tests."
  '((number . 1)
    (title . "Test issue")
    (state . "open")
    (body . "")
    (user . ((login . "alice")))
    (created_at . "2026-01-01T00:00:00Z")
    (updated_at . "2026-01-01T00:00:00Z")))

(defun forgejo-test-issue--comment-alist (id)
  "Return a timeline comment alist with ID."
  `((type . "comment")
    (id . ,id)
    (body . ,(format "Comment %d" id))
    (user . ((login . "commenter")))
    (created_at . "2026-01-01T00:00:00Z")
    (updated_at . "2026-01-01T00:00:00Z")))

(defun forgejo-test-issue--timeline (&rest ids)
  "Return timeline comment events for IDS."
  (mapcar #'forgejo-test-issue--comment-alist ids))

(ert-deftest forgejo-test-issue-view-passes-missing-comment-id-to-sync ()
  (let ((forgejo-repo--host "https://codeberg.org")
        sync-args)
    (unwind-protect
        (cl-letf (((symbol-function 'forgejo-db-get-issue)
                   (lambda (&rest _args) '((number . 1))))
                  ((symbol-function 'forgejo-db-get-timeline)
                   (lambda (&rest _args) nil))
                  ((symbol-function 'forgejo-issue--render-detail)
                   (lambda (buf-name &rest _args) (get-buffer-create buf-name)))
                  ((symbol-function 'forgejo-issue--sync-detail)
                   (lambda (&rest args) (setq sync-args args))))
          (forgejo-issue-view "owner" "repo" 1 99)
          (should (equal sync-args
                         '("codeberg.org" "owner" "repo" 1
                           "*forgejo-issue: owner/repo#1*" nil 99))))
      (when-let* ((buf (get-buffer "*forgejo-issue: owner/repo#1*")))
        (kill-buffer buf)))))

(ert-deftest forgejo-test-issue-view-omits-present-comment-id-from-sync ()
  (let ((forgejo-repo--host "https://codeberg.org")
        sync-args)
    (unwind-protect
        (cl-letf (((symbol-function 'forgejo-db-get-issue)
                   (lambda (&rest _args) '((number . 1))))
                  ((symbol-function 'forgejo-db-get-timeline)
                   (lambda (&rest _args) nil))
                  ((symbol-function 'forgejo-issue--render-detail)
                   (lambda (buf-name &rest _args)
                     (with-current-buffer (get-buffer-create buf-name)
                       (setq forgejo-view--ewoc (ewoc-create #'ignore nil nil t))
                       (ewoc-enter-last forgejo-view--ewoc '(:type comment :id 99))
                       (current-buffer))))
                  ((symbol-function 'forgejo-issue--sync-detail)
                   (lambda (&rest args) (setq sync-args args))))
          (forgejo-issue-view "owner" "repo" 1 99)
          (should (equal sync-args
                         '("codeberg.org" "owner" "repo" 1
                           "*forgejo-issue: owner/repo#1*" nil nil))))
      (when-let* ((buf (get-buffer "*forgejo-issue: owner/repo#1*")))
        (kill-buffer buf)))))

(ert-deftest forgejo-test-issue-view-jumps-existing-buffer-window ()
  (let ((forgejo-repo--host "https://codeberg.org")
        (buf-name "*forgejo-issue: owner/repo#1*")
        (other (get-buffer-create "*forgejo-test-issue-other*")))
    (unwind-protect
        (cl-letf (((symbol-function 'forgejo-db-get-issue)
                   (lambda (&rest _args) (forgejo-test-issue--issue-alist)))
                  ((symbol-function 'forgejo-db-get-timeline)
                   (lambda (&rest _args) (forgejo-test-issue--timeline 10 20)))
                  ((symbol-function 'forgejo-db--row-to-timeline-alist)
                   #'identity)
                  ((symbol-function 'forgejo-buffer--update-reactions)
                   #'ignore)
                  ((symbol-function 'forgejo-issue--sync-detail)
                   #'ignore))
          (forgejo-issue-view "owner" "repo" 1)
          (forgejo-view--goto-comment forgejo-view--ewoc 10)
          (switch-to-buffer other)
          (forgejo-issue-view "owner" "repo" 1 20)
          (should (eql 20 (plist-get (forgejo-view--node-at-point) :id))))
      (when-let* ((buf (get-buffer buf-name)))
        (kill-buffer buf))
      (when (buffer-live-p other)
        (kill-buffer other)))))

(provide 'forgejo-test-issue)
;;; forgejo-test-issue.el ends here

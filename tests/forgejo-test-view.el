;;; forgejo-test-view.el --- Tests for forgejo-view  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for forgejo-view URL parsing.

;;; Code:

(require 'ert)
(require 'cl-lib)

(defvar forgejo-markdown-mode)
(defvar forgejo-repo--host)

(setq forgejo-markdown-mode 'text-mode)
(require 'forgejo-view)

;;; Group 1: Diff keymap

(ert-deftest forgejo-test-view-diff-map-approve-review-binding ()
  (should (eq (keymap-lookup forgejo-view-diff-map "a")
              'forgejo-review-diff-approve))
  (should-not (eq (keymap-lookup forgejo-view-diff-map "R")
                  'forgejo-review-diff-approve))
  (should-not (eq (keymap-lookup forgejo-view-diff-map "A")
                  'forgejo-review-diff-approve)))

;;; Group 2: URL parsing

(ert-deftest forgejo-test-view-parse-issue-url ()
  (should (equal (forgejo-view--parse-forgejo-url
                  "https://codeberg.org/owner/repo/issues/42")
                 '("owner" "repo" 42 nil))))

(ert-deftest forgejo-test-view-parse-pull-url ()
  (should (equal (forgejo-view--parse-forgejo-url
                  "https://codeberg.org/owner/repo/pulls/7")
                 '("owner" "repo" 7 nil))))

(ert-deftest forgejo-test-view-parse-comment-fragment ()
  (should (equal (forgejo-view--parse-forgejo-url
                  "https://codeberg.org/guix/guix/pulls/7104#issuecomment-14913294")
                 '("guix" "guix" 7104 14913294))))

(ert-deftest forgejo-test-view-parse-comment-fragment-trailing-slash ()
  (should (equal (forgejo-view--parse-forgejo-url
                  "https://codeberg.org/o/r/issues/3/#issuecomment-99")
                 '("o" "r" 3 99))))

(ert-deftest forgejo-test-view-parse-non-matching ()
  (should-not (forgejo-view--parse-forgejo-url
               "https://example.com/some/page"))
  (should-not (forgejo-view--parse-forgejo-url nil)))

;;; Group 3: EWOC comment navigation

(defun forgejo-test-view--issue-alist ()
  "Return a minimal issue alist for detail rendering."
  '((number . 1)
    (title . "Issue")
    (state . "open")
    (body . "")
    (user . ((login . "user")))
    (created_at . "2026-01-01T00:00:00Z")
    (updated_at . "2026-01-01T00:00:00Z")))

(defun forgejo-test-view--comment-alist (id)
  "Return a minimal comment timeline event for ID."
  `((id . ,id)
    (type . "comment")
    (body . ,(format "Comment %d" id))
    (user . ((login . "commenter")))
    (created_at . "2026-01-01T00:00:00Z")))

(defun forgejo-test-view--timeline (&rest ids)
  "Return timeline comment events for IDS."
  (mapcar #'forgejo-test-view--comment-alist ids))

(ert-deftest forgejo-test-view-goto-comment-finds-ewoc-node ()
  (with-temp-buffer
    (let ((ewoc (ewoc-create
                 (lambda (node) (insert (format "%S\n" node))) nil nil t)))
      (ewoc-enter-last ewoc '(:type event :id 20))
      (ewoc-enter-last ewoc '(:type comment :id 10))
      (ewoc-enter-last ewoc '(:type comment :id 20))
      (should (forgejo-view--goto-comment ewoc 20))
      (should (equal (ewoc-data (ewoc-locate ewoc))
                     '(:type comment :id 20))))))

(ert-deftest forgejo-test-view-comment-target-for-sync-present ()
  (with-temp-buffer
    (let ((ewoc (ewoc-create #'ignore nil nil t)))
      (ewoc-enter-last ewoc '(:type comment :id 10))
      (should-not (forgejo-view--comment-target-for-sync ewoc 10)))))

(ert-deftest forgejo-test-view-comment-target-for-sync-missing ()
  (with-temp-buffer
    (let ((ewoc (ewoc-create #'ignore nil nil t)))
      (ewoc-enter-last ewoc '(:type comment :id 10))
      (should (eql 20 (forgejo-view--comment-target-for-sync ewoc 20))))))

(ert-deftest forgejo-test-view-comment-target-for-sync-nil-ewoc ()
  (should (eql 20 (forgejo-view--comment-target-for-sync nil 20))))

(ert-deftest forgejo-test-view-render-detail-comment-id-jumps-first-paint ()
  (let ((buf-name "*forgejo-test-first-paint-jump*"))
    (cl-letf (((symbol-function 'forgejo-buffer--update-reactions) #'ignore))
      (let ((buf (forgejo-view--render-detail
                  buf-name "https://codeberg.org" "owner" "repo"
                  (forgejo-test-view--issue-alist)
                  (forgejo-test-view--timeline 10 20)
                  #'fundamental-mode #'ignore #'ignore 20)))
        (unwind-protect
            (with-current-buffer buf
              (should (eql 20 (plist-get (forgejo-view--node-at-point) :id))))
          (when (buffer-live-p buf) (kill-buffer buf)))))))

(ert-deftest forgejo-test-view-render-detail-comment-id-jumps-existing-buffer ()
  (let ((buf-name "*forgejo-test-existing-buffer-jump*"))
    (cl-letf (((symbol-function 'forgejo-buffer--update-reactions) #'ignore))
      (let ((buf (forgejo-view--render-detail
                  buf-name "https://codeberg.org" "owner" "repo"
                  (forgejo-test-view--issue-alist)
                  (forgejo-test-view--timeline 10 20)
                  #'fundamental-mode #'ignore #'ignore)))
        (unwind-protect
            (with-current-buffer
                (forgejo-view--render-detail
                 buf-name "https://codeberg.org" "owner" "repo"
                 (forgejo-test-view--issue-alist)
                 (forgejo-test-view--timeline 10 20)
                 #'fundamental-mode #'ignore #'ignore 20)
              (should (eql 20 (plist-get (forgejo-view--node-at-point) :id))))
          (when (buffer-live-p buf) (kill-buffer buf)))))))

(ert-deftest forgejo-test-view-render-detail-keeps-point-without-comment-id ()
  (let ((buf-name "*forgejo-test-existing-buffer-keep-point*")
        point-before)
    (cl-letf (((symbol-function 'forgejo-buffer--update-reactions) #'ignore))
      (let ((buf (forgejo-view--render-detail
                  buf-name "https://codeberg.org" "owner" "repo"
                  (forgejo-test-view--issue-alist)
                  (forgejo-test-view--timeline 10 20)
                  #'fundamental-mode #'ignore #'ignore)))
        (unwind-protect
            (with-current-buffer buf
              (setq forgejo-view--target-comment-id 20)
              (forgejo-view--goto-comment forgejo-view--ewoc 10)
              (setq point-before (point))
              (forgejo-view--render-detail
               buf-name "https://codeberg.org" "owner" "repo"
               (forgejo-test-view--issue-alist)
               (forgejo-test-view--timeline 10 20)
               #'fundamental-mode #'ignore #'ignore)
              (should (eql point-before (point))))
          (when (buffer-live-p buf) (kill-buffer buf)))))))

(ert-deftest forgejo-test-view-url-target-at-point-includes-comment-id ()
  (with-temp-buffer
    (insert "https://codeberg.org/guix/guix/issues/5125#issuecomment-9233177")
    (goto-char (point-min))
    (should (equal (forgejo-view--url-target-at-point)
                   '("https://codeberg.org" "guix" "guix" 5125 9233177)))))

(ert-deftest forgejo-test-view-follow-link-passes-url-comment-id ()
  (let (called)
    (cl-letf (((symbol-function 'forgejo-view-item)
               (lambda (&rest args)
                 (setq called (cons forgejo-repo--host args)))))
      (with-temp-buffer
        (insert "https://codeberg.org/guix/guix/issues/5125#issuecomment-9233177")
        (goto-char (point-min))
        (forgejo-view-follow-link)
        (should (equal called
                       '("https://codeberg.org" "guix" "guix" 5125 9233177)))))))

(ert-deftest forgejo-test-view-refresh-passes-comment-at-point ()
  (let ((buf-name "*forgejo-test-refresh-comment*")
        called)
    (cl-letf (((symbol-function 'forgejo-buffer--update-reactions) #'ignore))
      (let ((buf (forgejo-view--render-detail
                  buf-name "https://codeberg.org" "owner" "repo"
                  (forgejo-test-view--issue-alist)
                  (forgejo-test-view--timeline 10 20)
                  #'fundamental-mode
                  (lambda (&rest args) (setq called args))
                  #'ignore)))
        (unwind-protect
            (with-current-buffer buf
              (setq forgejo-view--target-comment-id 20)
              (forgejo-view--goto-comment forgejo-view--ewoc 10)
              (forgejo-view-refresh)
              (should (equal called
                             '("codeberg.org" "owner" "repo" 1
                               "*forgejo-test-refresh-comment*" nil 10))))
          (when (buffer-live-p buf) (kill-buffer buf)))))))

(ert-deftest forgejo-test-view-re-render-passes-comment-id ()
  (let ((buf (get-buffer-create "*forgejo-test-re-render-comment*"))
        called)
    (unwind-protect
        (cl-letf (((symbol-function 'forgejo-db-get-issue)
                   (lambda (&rest _args) (forgejo-test-view--issue-alist)))
                  ((symbol-function 'forgejo-db-get-timeline)
                   (lambda (&rest _args) (forgejo-test-view--timeline 10)))
                  ((symbol-function 'forgejo-db--row-to-timeline-alist)
                   #'identity))
          (forgejo-view--re-render
           (buffer-name buf) "https://codeberg.org" "codeberg.org"
           "owner" "repo" 1
           (lambda (&rest args) (setq called args)) nil 10)
          (should (equal called
                         (list (buffer-name buf) "https://codeberg.org"
                               "owner" "repo"
                               (forgejo-test-view--issue-alist)
                               (forgejo-test-view--timeline 10)
                               10))))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest forgejo-test-view-re-render-comment-id-beats-restore-line ()
  (let ((buf (get-buffer-create "*forgejo-test-re-render-precedence*"))
        restored)
    (unwind-protect
        (cl-letf (((symbol-function 'forgejo-db-get-issue)
                   (lambda (&rest _args) (forgejo-test-view--issue-alist)))
                  ((symbol-function 'forgejo-db-get-timeline)
                   (lambda (&rest _args) nil)))
          (with-current-buffer buf
            (erase-buffer)
            (insert "line 1\nline 2\n")
            (goto-char (point-min)))
          (forgejo-view--re-render
           (buffer-name buf) "https://codeberg.org" "codeberg.org"
           "owner" "repo" 1 #'ignore 2 10)
          (with-current-buffer buf
            (setq restored (line-number-at-pos)))
          (should (= restored 1)))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest forgejo-test-view-item-keeps-host-in-async-callback ()
  (let ((forgejo-repo--host "https://alt.example")
        callback seen-host)
    (cl-letf (((symbol-function 'forgejo-db-get-issue)
               (lambda (&rest _args) nil))
              ((symbol-function 'forgejo-api-get)
               (lambda (_host _endpoint _params cb) (setq callback cb)))
              ((symbol-function 'forgejo-db-save-issues) #'ignore)
              ((symbol-function 'forgejo-issue-view)
               (lambda (&rest _args) (setq seen-host forgejo-repo--host))))
      (forgejo-view-item "owner" "repo" 1 99)
      (let ((forgejo-repo--host "https://wrong.example"))
        (funcall callback '((number . 1)) nil))
      (should (equal seen-host "https://alt.example")))))

(provide 'forgejo-test-view)
;;; forgejo-test-view.el ends here

;;; forgejo-test-review.el --- Tests for forgejo-review  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for Forgejo pull review operations.

;;; Code:

(require 'forgejo-test-helper)
(require 'forgejo-review)

;;; Group 1: Diff approval

(ert-deftest forgejo-test-review-diff-approve-posts-review-approval ()
  "Diff approval posts an approval review without line comments."
  (let (posted-host posted-endpoint posted-params posted-body)
    (cl-letf (((symbol-function 'forgejo-api-post)
               (lambda (host endpoint params body callback)
                 (setq posted-host host
                       posted-endpoint endpoint
                       posted-params params
                       posted-body body)
                 (funcall callback nil nil))))
      (with-temp-buffer
        (setq-local forgejo-repo--host "https://codeberg.org")
        (setq-local forgejo-repo--owner "OWNER")
        (setq-local forgejo-repo--name "REPO")
        (setq-local forgejo-diff--pr-number 123)
        (forgejo-review-diff-approve)))
    (should (string= posted-host "https://codeberg.org"))
    (should (string= posted-endpoint "repos/OWNER/REPO/pulls/123/reviews"))
    (should-not posted-params)
    (should (equal posted-body '((event . "APPROVED"))))
    (should-not (assq 'comments posted-body))))

(ert-deftest forgejo-test-review-diff-approve-requires-full-context ()
  "Diff approval requires repo context and an associated PR number."
  (with-temp-buffer
    (setq-local forgejo-repo--owner "OWNER")
    (setq-local forgejo-repo--name "REPO")
    (setq-local forgejo-diff--pr-number 123)
    (should-error (forgejo-review-diff-approve) :type 'user-error)))

(ert-deftest forgejo-test-review-diff-comment-requires-full-context ()
  "Diff comments require repo context and an associated PR number."
  (with-temp-buffer
    (setq-local forgejo-repo--host "https://codeberg.org")
    (setq-local forgejo-repo--name "REPO")
    (setq-local forgejo-diff--pr-number 123)
    (should-error (forgejo-review-diff-comment) :type 'user-error)))

(provide 'forgejo-test-review)
;;; forgejo-test-review.el ends here

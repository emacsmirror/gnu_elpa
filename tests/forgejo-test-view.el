;;; forgejo-test-view.el --- Tests for forgejo-view  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for forgejo-view URL parsing.

;;; Code:

(require 'ert)

(setq forgejo-markdown-mode 'text-mode)
(require 'forgejo-view)

;;; Group 1: URL parsing

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

(provide 'forgejo-test-view)
;;; forgejo-test-view.el ends here

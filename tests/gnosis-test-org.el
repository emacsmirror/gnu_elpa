;;; gnosis-test-org.el --- Tests for gnosis-org.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Free Software Foundation, Inc.

;; Author: Thanos Apollo <public@thanosapollo.org>

;;; Commentary:

;; Tests for the pure org-mode parsing functions in gnosis-org.el.
;; All tests use `with-temp-buffer' + `org-mode' -- no database needed.

;;; Code:

(require 'ert)
(require 'org)

(load (expand-file-name "../gnosis-org.el"
       (file-name-directory (or load-file-name buffer-file-name))))

;;; ---- Group 1: gnosis-org-adjust-title ----

(ert-deftest gnosis-test-org-adjust-title-plain ()
  "Plain text passes through unchanged."
  (should (string= "Hello" (gnosis-org-adjust-title "Hello"))))

(ert-deftest gnosis-test-org-adjust-title-single-link ()
  "Single org link is stripped to its description."
  (should (string= "Emacs"
                    (gnosis-org-adjust-title "[[id:abc-123][Emacs]]"))))

(ert-deftest gnosis-test-org-adjust-title-multiple-links ()
  "Multiple org links are all stripped."
  (should (string= "Emacs and Lisp"
                    (gnosis-org-adjust-title
                     "[[id:a][Emacs]] and [[id:b][Lisp]]"))))

(ert-deftest gnosis-test-org-adjust-title-mixed ()
  "Mixed text and links."
  (should (string= "Learn Emacs today"
                    (gnosis-org-adjust-title
                     "Learn [[id:abc][Emacs]] today"))))

;;; ---- Group 2: gnosis-org--combine-tags ----

(ert-deftest gnosis-test-org-combine-tags-both-nil ()
  "Both nil returns empty list."
  (should (null (gnosis-org--combine-tags nil nil))))

(ert-deftest gnosis-test-org-combine-tags-no-duplicates ()
  "Disjoint lists are merged."
  (let ((result (gnosis-org--combine-tags '("a" "b") '("c"))))
    (should (= 3 (length result)))
    (should (member "a" result))
    (should (member "c" result))))

(ert-deftest gnosis-test-org-combine-tags-dedup ()
  "Duplicate tags are removed."
  (let ((result (gnosis-org--combine-tags '("a" "b") '("b" "c"))))
    (should (= 3 (length result)))))

;;; ---- Group 3: gnosis-org--create-name ----

(ert-deftest gnosis-test-org-create-name-basic ()
  "Basic filename generation: spaces become underscores."
  (let ((name (gnosis-org--create-name "My Title" "%Y")))
    (should (string-match-p "--My_Title\\.org$" name))
    (should-not (string-match-p "\\.gpg" name))))

(ert-deftest gnosis-test-org-create-name-gpg ()
  "GPG suffix is appended when requested."
  (let ((name (gnosis-org--create-name "Test" "%Y" t)))
    (should (string-match-p "\\.org\\.gpg$" name))))

(ert-deftest gnosis-test-org-create-name-hash-stripped ()
  "Hash characters are removed from filename."
  (let ((name (gnosis-org--create-name "My #Title" "%Y")))
    (should-not (string-match-p "#" name))
    (should (string-match-p "My_Title" name))))

;;; ---- Group 4: gnosis-org-get-filetags ----

(ert-deftest gnosis-test-org-get-filetags-present ()
  "Extract filetags from buffer."
  (with-temp-buffer
    (org-mode)
    (insert "#+TITLE: Test\n#+FILETAGS: :foo:bar:\n\nContent\n")
    (let ((tags (gnosis-org-get-filetags)))
      (should (member "foo" tags))
      (should (member "bar" tags)))))

(ert-deftest gnosis-test-org-get-filetags-absent ()
  "No filetags returns nil."
  (with-temp-buffer
    (org-mode)
    (insert "#+TITLE: Test\n\nContent\n")
    (should (null (gnosis-org-get-filetags)))))

;;; ---- Group 5: gnosis-org-get-data--topic ----

(ert-deftest gnosis-test-org-get-data-topic-basic ()
  "Extract title and tags from org buffer."
  (with-temp-buffer
    (org-mode)
    (insert "#+TITLE: My Topic\n#+FILETAGS: :math:\n\nBody\n")
    (let ((result (gnosis-org-get-data--topic)))
      (should (string= "My Topic" (nth 0 result)))
      (should (member "math" (nth 1 result))))))

(ert-deftest gnosis-test-org-get-data-topic-with-id ()
  "Extract ID from property drawer."
  (with-temp-buffer
    (org-mode)
    (insert ":PROPERTIES:\n:ID: abc-123\n:END:\n#+TITLE: Test\n\nBody\n")
    (let ((result (gnosis-org-get-data--topic)))
      (should (string= "Test" (nth 0 result)))
      (should (string= "abc-123" (nth 2 result))))))

(ert-deftest gnosis-test-org-get-data-topic-missing-title ()
  "Missing title signals an error."
  (with-temp-buffer
    (org-mode)
    (insert "#+FILETAGS: :tag:\n\nJust content\n")
    (should-error (gnosis-org-get-data--topic))))

(ert-deftest gnosis-test-org-get-data-topic-link-in-title ()
  "Org link in title is stripped."
  (with-temp-buffer
    (org-mode)
    (insert "#+TITLE: Learn [[id:abc][Emacs]]\n\nBody\n")
    (let ((result (gnosis-org-get-data--topic)))
      (should (string= "Learn Emacs" (nth 0 result))))))

;;; ---- Group 6: gnosis-org--parse-headlines-recursive ----

(ert-deftest gnosis-test-org-parse-headlines-with-id ()
  "Headlines with ID property are collected."
  (with-temp-buffer
    (org-mode)
    (insert "#+TITLE: Root\n\n")
    (insert "* Section\n:PROPERTIES:\n:ID: sec-1\n:END:\n\nContent\n")
    (let* ((parsed (org-element-parse-buffer))
           (headlines (gnosis-org--parse-headlines-recursive
                       parsed nil nil nil)))
      (should (= 1 (length headlines)))
      (should (string= "sec-1" (plist-get (car headlines) :id))))))

(ert-deftest gnosis-test-org-parse-headlines-no-id-skipped ()
  "Headlines without ID are not collected (but children may be)."
  (with-temp-buffer
    (org-mode)
    (insert "#+TITLE: Root\n\n")
    (insert "* No ID Section\n\nContent\n")
    (let* ((parsed (org-element-parse-buffer))
           (headlines (gnosis-org--parse-headlines-recursive
                       parsed nil nil nil)))
      (should (= 0 (length headlines))))))

(ert-deftest gnosis-test-org-parse-headlines-tag-inheritance ()
  "Tags are inherited from parent headlines."
  (with-temp-buffer
    (org-mode)
    (insert "#+TITLE: Root\n\n")
    (insert "* Parent :inherited:\n:PROPERTIES:\n:ID: p-1\n:END:\n\n")
    (insert "** Child :own:\n:PROPERTIES:\n:ID: c-1\n:END:\n\n")
    (let* ((parsed (org-element-parse-buffer))
           (headlines (gnosis-org--parse-headlines-recursive
                       parsed nil nil nil))
           (child (cl-find "c-1" headlines
                           :key (lambda (h) (plist-get h :id))
                           :test #'string=)))
      (should child)
      (should (member "inherited" (plist-get child :tags)))
      (should (member "own" (plist-get child :tags))))))

;;; ---- Group 7: gnosis-org-buffer-data ----

(ert-deftest gnosis-test-org-buffer-data-with-topic-id ()
  "Buffer with file-level ID includes topic entry."
  (with-temp-buffer
    (org-mode)
    (insert ":PROPERTIES:\n:ID: root-1\n:END:\n")
    (insert "#+TITLE: Root Title\n\n")
    (insert "* Section\n:PROPERTIES:\n:ID: sec-1\n:END:\n\n")
    (let ((data (gnosis-org-buffer-data)))
      ;; First entry should be the topic itself
      (should (string= "root-1" (plist-get (car data) :id)))
      (should (string= "Root Title" (plist-get (car data) :title)))
      ;; Second entry is the section
      (should (>= (length data) 2)))))

(ert-deftest gnosis-test-org-buffer-data-without-topic-id ()
  "Buffer without file-level ID returns only headlines."
  (with-temp-buffer
    (org-mode)
    (insert "#+TITLE: No ID\n\n")
    (insert "* Section\n:PROPERTIES:\n:ID: sec-1\n:END:\n\n")
    (let ((data (gnosis-org-buffer-data)))
      ;; No topic entry (no file-level ID), just the headline
      (should (= 1 (length data)))
      (should (string= "sec-1" (plist-get (car data) :id))))))

(provide 'gnosis-test-org)

(ert-run-tests-batch-and-exit)
;;; gnosis-test-org.el ends here

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

(ert-deftest gnosis-test-org-buffer-data-journal-migration ()
  "Single file with file-level ID and multiple dated entries."
  (with-temp-buffer
    (org-mode)
    (insert ":PROPERTIES:\n:ID: journal-root\n:END:\n")
    (insert "#+TITLE: Journal\n\n")
    (insert "* 2026-03-01\n:PROPERTIES:\n:ID: day-01\n:END:\nEntry one.\n\n")
    (insert "* 2026-03-02\n:PROPERTIES:\n:ID: day-02\n:END:\nEntry two.\n\n")
    (insert "* 2026-03-03\n:PROPERTIES:\n:ID: day-03\n:END:\nEntry three.\n\n")
    (let ((data (gnosis-org-buffer-data)))
      ;; topic + 3 headlines = 4 entries
      (should (= 4 (length data)))
      ;; First is the topic
      (should (string= "journal-root" (plist-get (car data) :id)))
      (should (string= "Journal" (plist-get (car data) :title)))
      ;; Each headline has correct master
      (dolist (entry (cdr data))
        (should (string= "journal-root" (plist-get entry :master)))))))

(ert-deftest gnosis-test-org-buffer-data-deeply-nested ()
  "Four levels of nesting produce correct title paths and master chain."
  (with-temp-buffer
    (org-mode)
    (insert ":PROPERTIES:\n:ID: root-id\n:END:\n")
    (insert "#+TITLE: Root\n\n")
    (insert "* L1\n:PROPERTIES:\n:ID: l1-id\n:END:\n\n")
    (insert "** L2\n:PROPERTIES:\n:ID: l2-id\n:END:\n\n")
    (insert "*** L3\n:PROPERTIES:\n:ID: l3-id\n:END:\n\n")
    (let* ((data (gnosis-org-buffer-data))
           (find-entry (lambda (id)
                         (cl-find id data
                                  :key (lambda (e) (plist-get e :id))
                                  :test #'string=))))
      (should (= 4 (length data)))
      ;; Title paths
      (should (string= "Root:L1" (plist-get (funcall find-entry "l1-id") :title)))
      (should (string= "Root:L1:L2" (plist-get (funcall find-entry "l2-id") :title)))
      (should (string= "Root:L1:L2:L3" (plist-get (funcall find-entry "l3-id") :title)))
      ;; Master chain
      (should (string= "root-id" (plist-get (funcall find-entry "l1-id") :master)))
      (should (string= "l1-id" (plist-get (funcall find-entry "l2-id") :master)))
      (should (string= "l2-id" (plist-get (funcall find-entry "l3-id") :master))))))

(ert-deftest gnosis-test-org-buffer-data-mixed-id-siblings ()
  "Siblings without ID are skipped; those with ID are collected."
  (with-temp-buffer
    (org-mode)
    (insert "#+TITLE: Test\n\n")
    (insert "* First\n:PROPERTIES:\n:ID: first-id\n:END:\n\n")
    (insert "* Second (no ID)\nSome content.\n\n")
    (insert "* Third\n:PROPERTIES:\n:ID: third-id\n:END:\n\n")
    (let ((data (gnosis-org-buffer-data)))
      ;; No file-level ID, so just 2 headlines
      (should (= 2 (length data)))
      (should (string= "first-id" (plist-get (car data) :id)))
      (should (string= "third-id" (plist-get (cadr data) :id))))))

(ert-deftest gnosis-test-org-buffer-data-tag-inheritance ()
  "Filetags + headline tags accumulate through nesting."
  (with-temp-buffer
    (org-mode)
    (insert ":PROPERTIES:\n:ID: root-id\n:END:\n")
    (insert "#+TITLE: Math Notes\n#+FILETAGS: :math:\n\n")
    (insert "* Algebra :algebra:\n:PROPERTIES:\n:ID: alg-id\n:END:\n\n")
    (insert "** Proof Techniques :proof:\n:PROPERTIES:\n:ID: proof-id\n:END:\n\n")
    (let* ((data (gnosis-org-buffer-data))
           (proof (cl-find "proof-id" data
                           :key (lambda (e) (plist-get e :id))
                           :test #'string=)))
      (should proof)
      (should (member "math" (plist-get proof :tags)))
      (should (member "algebra" (plist-get proof :tags)))
      (should (member "proof" (plist-get proof :tags))))))

(ert-deftest gnosis-test-org-buffer-data-no-file-id ()
  "No file-level ID: L1 master is 0, L2 master is L1."
  (with-temp-buffer
    (org-mode)
    (insert "#+TITLE: No Root ID\n\n")
    (insert "* Section\n:PROPERTIES:\n:ID: sec-id\n:END:\n\n")
    (insert "** Subsection\n:PROPERTIES:\n:ID: sub-id\n:END:\n\n")
    (let* ((data (gnosis-org-buffer-data))
           (find-entry (lambda (id)
                         (cl-find id data
                                  :key (lambda (e) (plist-get e :id))
                                  :test #'string=))))
      ;; No topic entry
      (should (= 2 (length data)))
      (should (equal 0 (plist-get (funcall find-entry "sec-id") :master)))
      (should (string= "sec-id" (plist-get (funcall find-entry "sub-id") :master))))))

(ert-deftest gnosis-test-org-buffer-data-non-id-parent ()
  "Child under non-ID parent inherits master from nearest ancestor with ID."
  (with-temp-buffer
    (org-mode)
    (insert ":PROPERTIES:\n:ID: file-id\n:END:\n")
    (insert "#+TITLE: Root\n\n")
    (insert "* No ID Parent\nSome text.\n\n")
    (insert "** Child\n:PROPERTIES:\n:ID: child-id\n:END:\n\n")
    (let* ((data (gnosis-org-buffer-data))
           (child (cl-find "child-id" data
                           :key (lambda (e) (plist-get e :id))
                           :test #'string=)))
      ;; topic + child = 2 entries (no-ID parent is skipped)
      (should (= 2 (length data)))
      ;; child's master is the file-level ID, not 0
      (should (string= "file-id" (plist-get child :master))))))

(ert-deftest gnosis-test-org-buffer-data-topic-only ()
  "File-level ID with no headlines returns only the topic entry."
  (with-temp-buffer
    (org-mode)
    (insert ":PROPERTIES:\n:ID: only-id\n:END:\n")
    (insert "#+TITLE: Solo Topic\n\nJust body text.\n")
    (let ((data (gnosis-org-buffer-data)))
      (should (= 1 (length data)))
      (should (string= "only-id" (plist-get (car data) :id)))
      (should (string= "Solo Topic" (plist-get (car data) :title))))))

(ert-deftest gnosis-test-org-buffer-data-link-in-headline ()
  "Org links in headline titles are stripped."
  (with-temp-buffer
    (org-mode)
    (insert "#+TITLE: Root\n\n")
    (insert "* Learn [[id:xyz][Emacs]] Lisp\n:PROPERTIES:\n:ID: hl-id\n:END:\n\n")
    (let* ((data (gnosis-org-buffer-data))
           (entry (car data)))
      (should (string= "Learn Emacs Lisp" (plist-get entry :title))))))

(ert-deftest gnosis-test-org-buffer-data-parallel-branches ()
  "Parallel L1 branches each build independent master chains."
  (with-temp-buffer
    (org-mode)
    (insert ":PROPERTIES:\n:ID: topic-id\n:END:\n")
    (insert "#+TITLE: Parent Topic\n\n")
    (insert "* Branch A\n:PROPERTIES:\n:ID: a1-id\n:END:\n\n")
    (insert "** A Child\n:PROPERTIES:\n:ID: a2-id\n:END:\n\n")
    (insert "*** A Grandchild\n:PROPERTIES:\n:ID: a3-id\n:END:\n\n")
    (insert "* Branch B\n:PROPERTIES:\n:ID: b1-id\n:END:\n\n")
    (insert "** B Child\n:PROPERTIES:\n:ID: b2-id\n:END:\n\n")
    (let* ((data (gnosis-org-buffer-data))
           (find-entry (lambda (id)
                         (cl-find id data
                                  :key (lambda (e) (plist-get e :id))
                                  :test #'string=))))
      ;; topic + 5 headlines = 6
      (should (= 6 (length data)))
      ;; Branch A chain
      (should (string= "topic-id" (plist-get (funcall find-entry "a1-id") :master)))
      (should (string= "a1-id" (plist-get (funcall find-entry "a2-id") :master)))
      (should (string= "a2-id" (plist-get (funcall find-entry "a3-id") :master)))
      ;; Branch B chain
      (should (string= "topic-id" (plist-get (funcall find-entry "b1-id") :master)))
      (should (string= "b1-id" (plist-get (funcall find-entry "b2-id") :master)))
      ;; Title paths are independent
      (should (string= "Parent Topic:Branch A:A Child:A Grandchild"
                        (plist-get (funcall find-entry "a3-id") :title)))
      (should (string= "Parent Topic:Branch B:B Child"
                        (plist-get (funcall find-entry "b2-id") :title))))))

(ert-deftest gnosis-test-org-buffer-data-complex-non-id-intermediates ()
  "Non-ID intermediates at multiple levels skip correctly to nearest ancestor."
  (with-temp-buffer
    (org-mode)
    (insert ":PROPERTIES:\n:ID: topic-id\n:END:\n")
    (insert "#+TITLE: Main Topic\n\n")
    (insert "* Section Without ID\nContent.\n\n")
    (insert "** Subsection With ID\n:PROPERTIES:\n:ID: sub-id\n:END:\n\n")
    (insert "*** Sub-sub With ID\n:PROPERTIES:\n:ID: subsub-id\n:END:\n\n")
    (insert "* Another Section With ID\n:PROPERTIES:\n:ID: sec-id\n:END:\n\n")
    (insert "** Another Sub Without ID\nContent.\n\n")
    (insert "*** Deep With ID\n:PROPERTIES:\n:ID: deep-id\n:END:\n\n")
    (let* ((data (gnosis-org-buffer-data))
           (find-entry (lambda (id)
                         (cl-find id data
                                  :key (lambda (e) (plist-get e :id))
                                  :test #'string=))))
      ;; topic + 4 headlines with IDs = 5
      (should (= 5 (length data)))
      ;; sub links to topic (skipping non-ID L1)
      (should (string= "topic-id" (plist-get (funcall find-entry "sub-id") :master)))
      ;; subsub links to sub
      (should (string= "sub-id" (plist-get (funcall find-entry "subsub-id") :master)))
      ;; deep links to sec (skipping non-ID L2)
      (should (string= "sec-id" (plist-get (funcall find-entry "deep-id") :master))))))

(ert-deftest gnosis-test-org-buffer-data-deep-nesting-10-levels ()
  "Ten levels of nesting parse correctly with proper master chain."
  (with-temp-buffer
    (org-mode)
    (insert ":PROPERTIES:\n:ID: deep-topic\n:END:\n")
    (insert "#+TITLE: Deep Hierarchy\n\n")
    (dotimes (i 10)
      (insert (make-string (1+ i) ?*) " Level " (number-to-string (1+ i)) "\n")
      (insert ":PROPERTIES:\n:ID: level-" (number-to-string (1+ i)) "-id\n:END:\n\n"))
    (let* ((data (gnosis-org-buffer-data))
           (find-entry (lambda (id)
                         (cl-find id data
                                  :key (lambda (e) (plist-get e :id))
                                  :test #'string=))))
      ;; topic + 10 headlines = 11
      (should (= 11 (length data)))
      ;; Deepest level links to level 9
      (should (string= "level-9-id"
                        (plist-get (funcall find-entry "level-10-id") :master)))
      ;; Level 1 links to topic
      (should (string= "deep-topic"
                        (plist-get (funcall find-entry "level-1-id") :master))))))

(ert-deftest gnosis-test-org-buffer-data-done-tasks-mixed ()
  "DONE headlines without IDs are skipped; those with IDs are collected."
  (with-temp-buffer
    (org-mode)
    (insert ":PROPERTIES:\n:ID: topic-id\n:END:\n")
    (insert "#+TITLE: Mixed Headlines\n\n")
    (insert "* Section Without ID\nContent.\n\n")
    (insert "** DONE Task [100%]\n+ [X] Completed item\n\n")
    (insert "** Subsection With ID\n:PROPERTIES:\n:ID: sub-id\n:END:\n\n")
    (insert "*** DONE Another Task Without ID\nMore content.\n\n")
    (insert "*** Sub-sub With ID\n:PROPERTIES:\n:ID: deep-id\n:END:\n\n")
    (let* ((data (gnosis-org-buffer-data))
           (find-entry (lambda (id)
                         (cl-find id data
                                  :key (lambda (e) (plist-get e :id))
                                  :test #'string=))))
      ;; topic + 2 with IDs = 3
      (should (= 3 (length data)))
      ;; sub links to topic (skipping non-ID parent)
      (should (string= "topic-id" (plist-get (funcall find-entry "sub-id") :master)))
      ;; deep links to sub (skipping DONE without ID)
      (should (string= "sub-id" (plist-get (funcall find-entry "deep-id") :master))))))

(ert-deftest gnosis-test-org-buffer-data-empty-filetags ()
  "Empty filetags line produces nil tags, not an error."
  (with-temp-buffer
    (org-mode)
    (insert ":PROPERTIES:\n:ID: journal-id\n:END:\n")
    (insert "#+TITLE: 2026-03-02\n#+FILETAGS:\n\n")
    (insert "* Notes\n* Tasks\n** DONE Exercise [100%]\n+ [X] Completed\n")
    (let ((data (gnosis-org-buffer-data)))
      ;; Only topic (no headlines have IDs)
      (should (= 1 (length data)))
      (should (string= "2026-03-02" (plist-get (car data) :title)))
      (should (string= "journal-id" (plist-get (car data) :id)))
      (should (null (plist-get (car data) :tags))))))

;;; ---- Group 8: gnosis-org-collect-id-links ----

(ert-deftest gnosis-test-org-collect-id-links-between-entries ()
  "ID links between headlines are collected as (target . source) pairs."
  (with-temp-buffer
    (org-mode)
    (insert ":PROPERTIES:\n:ID: file-id\n:END:\n")
    (insert "#+TITLE: Links Test\n\n")
    (insert "* First\n:PROPERTIES:\n:ID: first-id\n:END:\n")
    (insert "See [[id:second-id][Second]].\n\n")
    (insert "* Second\n:PROPERTIES:\n:ID: second-id\n:END:\n")
    (insert "Back to [[id:first-id][First]].\n\n")
    (let ((links (gnosis-org-collect-id-links)))
      (should (= 2 (length links)))
      (should (member '("second-id" . "first-id") links))
      (should (member '("first-id" . "second-id") links)))))

(ert-deftest gnosis-test-org-collect-id-links-multiple-per-section ()
  "Multiple links in different sections are all collected."
  (with-temp-buffer
    (org-mode)
    (insert ":PROPERTIES:\n:ID: main-id\n:END:\n")
    (insert "#+TITLE: Link Test\n\n")
    (insert "* Normal Links\n:PROPERTIES:\n:ID: normal-id\n:END:\n")
    (insert "Regular [[id:target1][Link One]] and [[id:target2][Link Two]].\n\n")
    (insert "* Complex Links\n:PROPERTIES:\n:ID: complex-id\n:END:\n")
    (insert "Links in [[id:target3][different]] formats and [[id:target4][contexts]].\n\n")
    (let ((links (gnosis-org-collect-id-links)))
      (should (>= (length links) 4))
      (should (cl-some (lambda (link) (string= (car link) "target1")) links))
      (should (cl-some (lambda (link) (string= (car link) "target2")) links))
      (should (cl-some (lambda (link) (string= (car link) "target3")) links))
      (should (cl-some (lambda (link) (string= (car link) "target4")) links)))))

(ert-deftest gnosis-test-org-collect-id-links-none ()
  "Headlines with IDs but no id-links return nil."
  (with-temp-buffer
    (org-mode)
    (insert ":PROPERTIES:\n:ID: file-id\n:END:\n")
    (insert "#+TITLE: No Links\n\n")
    (insert "* Section\n:PROPERTIES:\n:ID: sec-id\n:END:\nPlain text.\n\n")
    (let ((links (gnosis-org-collect-id-links)))
      (should (null links)))))

(provide 'gnosis-test-org)

(ert-run-tests-batch-and-exit)
;;; gnosis-test-org.el ends here

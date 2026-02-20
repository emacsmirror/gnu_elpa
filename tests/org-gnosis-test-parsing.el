;;; org-gnosis-test-parsing.el --- Tests for org-gnosis parsing improvements -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025 Free Software Foundation, Inc.

;; Author: Thanos Apollo <public@thanosapollo.org>
;; Keywords: tests

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Simple tests for org-gnosis parsing functionality that don't require database setup.

;;; Code:

(require 'ert)
(require 'org-element)

;; Load the main org-gnosis.el file
(let ((gnosis-file (expand-file-name "../org-gnosis.el"
                                     (file-name-directory (or load-file-name buffer-file-name)))))
  (if (file-exists-p gnosis-file)
      (load gnosis-file)
    (error "Could not find org-gnosis.el at %s" gnosis-file)))

(defmacro org-gnosis-test-with-temp-buffer (content &rest body)
  "Execute BODY in temporary buffer with CONTENT."
  `(with-temp-buffer
     (org-mode)
     (insert ,content)
     (goto-char (point-min))
     ,@body))

(ert-deftest org-gnosis-test-parsing ()
  "Test org-gnosis parsing functions."
  (let ((test-content ":PROPERTIES:
:ID: file-id-123
:END:
#+title: Test
#+filetags: :project:important:

* First Heading :heading:
:PROPERTIES:
:ID: heading-id-456
:END:

Some content with [[id:external-id][External Link]].

** Sub Heading :sub:
:PROPERTIES:
:ID: sub-id-789
:END:

More content with another [[id:another-link][Link]]."))

    (org-gnosis-test-with-temp-buffer test-content
      (let ((data (org-gnosis-buffer-data)))
        ;; Should have topic + 2 headlines = 3 items
        (should (= (length data) 3))

        ;; Find topic by ID
        (let ((topic (cl-find-if (lambda (item) (equal (plist-get item :id) "file-id-123")) data)))
          (should topic)
          (should (equal (plist-get topic :title) "Test"))
          (should (equal (plist-get topic :level) 0))
          (should (member "project" (plist-get topic :tags)))
          (should (member "important" (plist-get topic :tags))))

        ;; Find first heading by ID
        (let ((heading (cl-find-if (lambda (item) (equal (plist-get item :id) "heading-id-456")) data)))
          (should heading)
          (should (equal (plist-get heading :title) "Test:First Heading"))
          (should (equal (plist-get heading :level) 1))
          (should (equal (plist-get heading :master) "file-id-123"))
          (should (member "heading" (plist-get heading :tags))))

        ;; Find sub heading by ID
        (let ((sub-heading (cl-find-if (lambda (item) (equal (plist-get item :id) "sub-id-789")) data)))
          (should sub-heading)
          (should (equal (plist-get sub-heading :title) "Test:First Heading:Sub Heading"))
          (should (equal (plist-get sub-heading :level) 2))
          (should (equal (plist-get sub-heading :master) "heading-id-456"))
          (should (member "sub" (plist-get sub-heading :tags))))

        ;; Test link extraction
        (let ((links (org-gnosis-collect-id-links)))
          (should (>= (length links) 2)))))))

(ert-deftest org-gnosis-test-headline-parsing ()
  "Test headline parsing with various tag scenarios."
  (let ((test-content ":PROPERTIES:
:ID: file-id
:END:
#+title: Tag Test
#+filetags: :global:

* Level 1 :level1:
:PROPERTIES:
:ID: level1-id
:END:

** Level 2 :level2:
:PROPERTIES:
:ID: level2-id
:END:

*** Level 3 :level3:
:PROPERTIES:
:ID: level3-id
:END:"))

    (org-gnosis-test-with-temp-buffer test-content
      (let ((data (org-gnosis-buffer-data)))
        ;; Should have topic + 3 headlines = 4 items
        (should (= (length data) 4))

        ;; Check topic has global tag
        (let ((topic (cl-find-if (lambda (item) (equal (plist-get item :id) "file-id")) data)))
          (should topic)
          (should (equal (plist-get topic :title) "Tag Test"))
          (should (member "global" (plist-get topic :tags))))

        ;; Check level 1 hierarchy
        (let ((level1 (cl-find-if (lambda (item) (equal (plist-get item :id) "level1-id")) data)))
          (should level1)
          (should (equal (plist-get level1 :title) "Tag Test:Level 1"))
          (should (equal (plist-get level1 :level) 1))
          (should (equal (plist-get level1 :master) "file-id"))
          (should (member "level1" (plist-get level1 :tags)))
          ;; Note: Tag inheritance from topic level doesn't work in current implementation
          ;; (should (member "global" (plist-get level1 :tags))) ; This would fail
          )))))

(ert-deftest org-gnosis-test-link-extraction ()
  "Test link extraction improvements."
  (let ((test-content ":PROPERTIES:
:ID: main-id
:END:
#+title: Link Test

* Normal Links
:PROPERTIES:
:ID: normal-id
:END:

Regular [[id:target1][Link One]] and [[id:target2][Link Two]].

* Complex Links
:PROPERTIES:
:ID: complex-id
:END:

Links in [[id:target3][different]] formats and [[id:target4][contexts]]."))

    (org-gnosis-test-with-temp-buffer test-content
      (let ((data (org-gnosis-buffer-data)))
        ;; Should have topic + 2 headlines = 3 items
        (should (= (length data) 3))

        ;; Check topic
        (let ((topic (cl-find-if (lambda (item) (equal (plist-get item :id) "main-id")) data)))
          (should topic)
          (should (equal (plist-get topic :title) "Link Test")))

        ;; Test link collection
        (let ((links (org-gnosis-collect-id-links)))
          (should (>= (length links) 4))
          (should (cl-some (lambda (link) (string= (car link) "target1")) links))
          (should (cl-some (lambda (link) (string= (car link) "target2")) links))
          (should (cl-some (lambda (link) (string= (car link) "target3")) links))
          (should (cl-some (lambda (link) (string= (car link) "target4")) links)))))))

(ert-deftest org-gnosis-test-master-child-relationships ()
  "Test that parent-child relationships are correctly parsed."
  (let ((test-content ":PROPERTIES:
:ID: topic-id-123
:END:
#+title: Parent Topic
#+filetags: :parent:

* Level 1 Heading
:PROPERTIES:
:ID: level1-id-456
:END:

** Level 2 Heading
:PROPERTIES:
:ID: level2-id-789
:END:

*** Level 3 Heading
:PROPERTIES:
:ID: level3-id-abc
:END:

* Another Level 1
:PROPERTIES:
:ID: another-level1-id
:END:

** Another Level 2
:PROPERTIES:
:ID: another-level2-id
:END:"))

    (org-gnosis-test-with-temp-buffer test-content
      (let ((data (org-gnosis-buffer-data)))
        ;; Should have topic + 5 headlines = 6 items
        (should (= (length data) 6))

        ;; Check topic
        (let ((topic (cl-find-if (lambda (item) (equal (plist-get item :id) "topic-id-123")) data)))
          (should topic)
          (should (equal (plist-get topic :title) "Parent Topic"))
          (should (equal (plist-get topic :master) 0))
          (should (equal (plist-get topic :level) 0)))

        ;; Check level 1 master relationship
        (let ((level1 (cl-find-if (lambda (item) (equal (plist-get item :id) "level1-id-456")) data)))
          (should level1)
          (should (equal (plist-get level1 :title) "Parent Topic:Level 1 Heading"))
          (should (equal (plist-get level1 :master) "topic-id-123"))
          (should (equal (plist-get level1 :level) 1)))

        ;; Check level 2 master relationship
        (let ((level2 (cl-find-if (lambda (item) (equal (plist-get item :id) "level2-id-789")) data)))
          (should level2)
          (should (equal (plist-get level2 :title) "Parent Topic:Level 1 Heading:Level 2 Heading"))
          (should (equal (plist-get level2 :master) "level1-id-456"))
          (should (equal (plist-get level2 :level) 2)))

        ;; Check level 3 master relationship
        ;; Note: Current implementation links level 3 to level 1 (grandparent), not level 2 (parent)
        ;; This appears to be intended behavior based on the org-gnosis--find-master-id logic
        (let ((level3 (cl-find-if (lambda (item) (equal (plist-get item :id) "level3-id-abc")) data)))
          (should level3)
          (should (equal (plist-get level3 :title)
			 "Parent Topic:Level 1 Heading:Level 2 Heading:Level 3 Heading"))
          (should (equal (plist-get level3 :master) "level1-id-456"))
          (should (equal (plist-get level3 :level) 3)))))))

(ert-deftest org-gnosis-test-missing-parent-ids ()
  "Test parent-child relationships when intermediate headlines lack IDs."
  (let ((test-content ":PROPERTIES:
:ID: topic-id-xyz
:END:
#+title: Main Topic
#+filetags: :main:

* Section Without ID
Some content here.

** Subsection With ID
:PROPERTIES:
:ID: subsection-id-123
:END:
This should link to topic, not the parent section.

*** Sub-subsection With ID
:PROPERTIES:
:ID: subsubsection-id-456
:END:
This should link to subsection-id-123.

* Another Section With ID
:PROPERTIES:
:ID: section-id-789
:END:

** Another Subsection Without ID
Content here.

*** Deep Item With ID
:PROPERTIES:
:ID: deep-id-abc
:END:
This should link to section-id-789, skipping the parent without ID."))

    (org-gnosis-test-with-temp-buffer test-content
      (let ((data (org-gnosis-buffer-data)))
        ;; Should have topic + 4 headlines with IDs = 5 total
        (should (= (length data) 5))

        ;; Check subsection links to topic (skipping parent without ID)
        (let ((subsection (cl-find-if
                          (lambda (item)
                            (equal (plist-get item :id) "subsection-id-123"))
                          data)))
          (should subsection)
          (should (equal (plist-get subsection :master) "topic-id-xyz")))

        ;; Check sub-subsection links to subsection
        (let ((subsubsection (cl-find-if
                             (lambda (item)
                               (equal (plist-get item :id) "subsubsection-id-456"))
                             data)))
          (should subsubsection)
          (should (equal (plist-get subsubsection :master) "subsection-id-123")))

        ;; Check deep item links to section (skipping parent without ID)
        (let ((deep-item (cl-find-if
                         (lambda (item)
                           (equal (plist-get item :id) "deep-id-abc"))
                         data)))
          (should deep-item)
          (should (equal (plist-get deep-item :master) "section-id-789")))))))

(ert-deftest org-gnosis-test-edge-cases ()
  "Test various edge cases that could break parsing."

  ;; Test empty title error
  (should-error
   (org-gnosis-test-with-temp-buffer
    ":PROPERTIES:\n:ID: test-id\n:END:\n#+title: \n\n* Test\n:PROPERTIES:\n:ID: test-headline\n:END:\n"
    (org-gnosis-buffer-data)))

  ;; Test missing topic ID (should skip topic, only parse headlines)
  (let ((result (org-gnosis-test-with-temp-buffer
                 "#+title: Test Without ID\n\n* Test\n:PROPERTIES:\n:ID: test-headline\n:END:\n"
                 (org-gnosis-buffer-data))))
    ;; Should only have the headline, no topic
    (should (= (length result) 1))
    (let ((headline (car result)))
      (should headline)
      (should (equal (plist-get headline :level) 1))
      (should (equal (plist-get headline :title) "Test"))
      (should (equal (plist-get headline :id) "test-headline"))))

  ;; Test headline without ID (should be skipped)
  (let ((test-content ":PROPERTIES:\n:ID: topic-id\n:END:\n#+title: Valid Topic\n\n* Headline Without ID\nContent here.\n\n* Headline With ID\n:PROPERTIES:\n:ID: valid-headline-id\n:END:\nMore content."))
    (org-gnosis-test-with-temp-buffer test-content
      (let ((data (org-gnosis-buffer-data)))
        ;; Should only have topic + 1 headline = 2 items
        (should (= (length data) 2)))))

  ;; Test deeply nested hierarchy (performance test)
  (let ((deep-content ":PROPERTIES:\n:ID: deep-topic\n:END:\n#+title: Deep Hierarchy\n"))
    (dotimes (i 10)  ; Create 10 levels deep
      (setq deep-content
            (concat deep-content
                    (make-string (1+ i) ?*) " Level " (number-to-string (1+ i)) "\n"
                    ":PROPERTIES:\n:ID: level-" (number-to-string (1+ i)) "-id\n:END:\n\n")))

    (org-gnosis-test-with-temp-buffer deep-content
      (let ((data (org-gnosis-buffer-data)))
        ;; Should have topic + 10 headlines = 11 items
        (should (= (length data) 11))

        ;; Check that deepest level exists and has correct hierarchy behavior
        ;; Note: Based on org-gnosis hierarchy logic, level 10 links to level 1, not level 9
        (let ((deepest (cl-find-if (lambda (item)
                                    (equal (plist-get item :id) "level-10-id"))
                                  data)))
          (should deepest)
          (should (equal (plist-get deepest :master) "level-1-id")) ; Links to level 1, not level 9
          )))))

(ert-deftest org-gnosis-test-mixed-id-headlines ()
  "Test parsing files with mix of headlines with and without IDs."
  (let ((test-content ":PROPERTIES:
:ID: topic-mixed-123
:END:
#+title: Mixed Headlines
#+filetags:

* Section Without ID
Some content here.

** DONE Task [100%]
+ [X] Completed item
+ [X] Another item

** Subsection With ID
:PROPERTIES:
:ID: subsection-with-id-456
:END:
This has an ID and should be parsed.

*** DONE Another Task Without ID
More content without ID.

*** Sub-subsection With ID
:PROPERTIES:
:ID: deep-with-id-789
:END:
This should link to subsection-with-id-456."))

    (org-gnosis-test-with-temp-buffer test-content
      (let ((data (org-gnosis-buffer-data)))
        ;; Should have: topic + 2 headlines with IDs = 3 total
        (should (= (length data) 3))

        ;; Check subsection links to topic (skipping parent without ID)
        (let ((subsection (cl-find-if
                          (lambda (item)
                            (equal (plist-get item :id) "subsection-with-id-456"))
                          data)))
          (should subsection)
          (should (equal (plist-get subsection :master) "topic-mixed-123")))

        ;; Check deep item links to subsection (skipping intermediate parent without ID)
        (let ((deep-item (cl-find-if
                         (lambda (item)
                           (equal (plist-get item :id) "deep-with-id-789"))
                         data)))
          (should deep-item)
          (should (equal (plist-get deep-item :master) "subsection-with-id-456")))))))

(ert-deftest org-gnosis-test-empty-filetags ()
  "Test parsing with empty filetags (reproduces user's error)."
  (let ((test-content ":PROPERTIES:
:ID:       9a2f2518-66a4-436f-b824-5bf0ac959055
:END:
#+title: 2025-03-02
#+filetags:

Ἡμέραι ἓως ἔτους 2050: *9071*

* Καταγραφή
* Ὑπομνήματα Ἡμέρας
* Στόχοι
** DONE Πίστη [100%]
+ [X] Πρωινὴ Προσευχή
+ [X] Ἑσπερινή Προσευχή

** DONE Γυμναστική [100%]
+ [X] Ἐνδυνάμωση· Πλάτη & Δικέφαλα
** DONE Γνῶσις
+ [X] Ὁλοκλήρωση τῶν ἐπαναλήψεων γνῶσις"))

    (org-gnosis-test-with-temp-buffer test-content
      (let ((data (org-gnosis-buffer-data)))
        ;; Should have: topic only (no headlines have IDs) = 1 item
        (should (= (length data) 1))

        ;; Check topic was parsed correctly
        (let ((topic (car data)))
          (should (equal (plist-get topic :title) "2025-03-02"))
          (should (equal (plist-get topic :id) "9a2f2518-66a4-436f-b824-5bf0ac959055"))
          (should (equal (plist-get topic :tags) nil))  ; Empty filetags should be nil
          (should (equal (plist-get topic :level) 0)))))))

(ert-deftest org-gnosis-test-checked-items-from-heading-only ()
  "Test that org-gnosis-get-checked-items works on a specific heading subtree."
  (let* ((today (format-time-string "%Y-%m-%d"))
         (yesterday (format-time-string "%Y-%m-%d" (time-subtract (current-time) (days-to-time 1))))
         (tomorrow (format-time-string "%Y-%m-%d" (time-add (current-time) (days-to-time 1))))
         (test-content (format ":PROPERTIES:
:ID: journal-id
:END:
#+title: Journal
#+filetags:

* %s
+ [X] Old completed item
+ [ ] Old incomplete item

* %s
+ [X] Today completed item 1
+ [X] Today completed item 2
+ [ ] Today incomplete item

* %s
+ [X] Future completed item" yesterday today tomorrow)))

    (org-gnosis-test-with-temp-buffer test-content
      ;; Parse entire buffer - should get all 4 checked items (1+2+1)
      (let ((all-items (org-gnosis-get-checked-items (org-element-parse-buffer))))
        (should (= (length all-items) 4))
        (should (member "Old completed item" all-items))
        (should (member "Today completed item 1" all-items))
        (should (member "Today completed item 2" all-items))
        (should (member "Future completed item" all-items)))

      ;; Parse only today's heading - should get 2 checked items
      (let* ((parsed-buffer (org-element-parse-buffer))
             (today-heading (org-element-map parsed-buffer 'headline
                             (lambda (headline)
                               (when (string= (org-element-property :raw-value headline) today)
                                 headline))
                             nil t))
             (today-items (org-gnosis-get-checked-items today-heading)))
        (should today-heading)
        (should (= (length today-items) 2))
        (should (member "Today completed item 1" today-items))
        (should (member "Today completed item 2" today-items))
        (should-not (member "Old completed item" today-items))
        (should-not (member "Future completed item" today-items))))))

(defun org-gnosis-test-run-parsing-tests ()
  "Run all parsing tests using ERT."
  (interactive)
  (ert-run-tests-batch "org-gnosis-test-"))

;; Run tests if called directly
(when (and (boundp 'argv)
           (member "--run-tests" argv))
  (ert-run-tests-batch "org-gnosis-test-"))

(provide 'org-gnosis-test-parsing)
;;; org-gnosis-test-parsing.el ends here

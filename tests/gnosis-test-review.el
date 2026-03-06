;;; gnosis-test-review.el --- Tests for gnosis-review.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Free Software Foundation, Inc.

;; Author: Thanos Apollo <public@thanosapollo.org>

;;; Commentary:

;; Tests for the scheduling logic in gnosis-review.el.
;; Uses a temporary SQLite database via `gnosis-test-with-db'.

;;; Code:

(require 'ert)
(require 'gnosis)
(require 'gnosis-review)

(load (expand-file-name "gnosis-test-helpers.el"
       (file-name-directory (or load-file-name buffer-file-name))))

;;; ---- Group 1: gnosis-review-is-due-today-p ----

(ert-deftest gnosis-test-review-due-today-past ()
  "Thema with next-rev in the past is due today."
  (gnosis-test-with-db
    (let* ((deck-id (gnosis-test--add-deck "review-test"))
           (id (gnosis-test--add-basic-thema deck-id "Q" "A")))
      ;; Set next-rev to yesterday
      (gnosis-update 'review-log `(= next-rev ',(gnosis-algorithm-date -1))
                     `(= id ,id))
      (should (gnosis-review-is-due-today-p id)))))

(ert-deftest gnosis-test-review-due-today-today ()
  "Thema with next-rev today is due."
  (gnosis-test-with-db
    (let* ((deck-id (gnosis-test--add-deck "review-test"))
           (id (gnosis-test--add-basic-thema deck-id "Q" "A")))
      ;; next-rev defaults to today in gnosis-test--add-basic-thema
      (should (gnosis-review-is-due-today-p id)))))

(ert-deftest gnosis-test-review-due-today-future ()
  "Thema with next-rev in the future is NOT due today."
  (gnosis-test-with-db
    (let* ((deck-id (gnosis-test--add-deck "review-test"))
           (id (gnosis-test--add-basic-thema deck-id "Q" "A")))
      ;; Set next-rev to tomorrow
      (gnosis-update 'review-log `(= next-rev ',(gnosis-algorithm-date 1))
                     `(= id ,id))
      (should-not (gnosis-review-is-due-today-p id)))))

;;; ---- Group 2: gnosis-review-is-due-p ----

(ert-deftest gnosis-test-review-is-due-active ()
  "Active thema due today returns t."
  (gnosis-test-with-db
    (let* ((deck-id (gnosis-test--add-deck "review-test"))
           (id (gnosis-test--add-basic-thema deck-id "Q" "A")))
      (should (gnosis-review-is-due-p id)))))

(ert-deftest gnosis-test-review-is-due-suspended ()
  "Suspended thema due today returns nil."
  (gnosis-test-with-db
    (let* ((deck-id (gnosis-test--add-deck "review-test"))
           (id (gnosis-test--add-basic-thema deck-id "Q" "A" nil nil nil 1)))
      (should-not (gnosis-review-is-due-p id)))))

;;; ---- Group 3: gnosis-review-get--due-themata ----

(ert-deftest gnosis-test-review-get-due-filters-suspended ()
  "Suspended themata are excluded from due list."
  (gnosis-test-with-db
    (let* ((deck-id (gnosis-test--add-deck "review-test"))
           (_id1 (gnosis-test--add-basic-thema deck-id "Q1" "A1"))
           (_id2 (gnosis-test--add-basic-thema deck-id "Q2" "A2" nil nil nil 1)))
      (let ((gnosis-review-new-first nil)
            (gnosis-new-themata-limit nil))
        (let ((due (gnosis-review-get--due-themata)))
          ;; Only 1 should be due (the non-suspended one)
          (should (= 1 (length due))))))))

(ert-deftest gnosis-test-review-get-due-filters-future ()
  "Themata with future next-rev are excluded."
  (gnosis-test-with-db
    (let* ((deck-id (gnosis-test--add-deck "review-test"))
           (id1 (gnosis-test--add-basic-thema deck-id "Q1" "A1"))
           (id2 (gnosis-test--add-basic-thema deck-id "Q2" "A2")))
      ;; Push id2 to future
      (gnosis-update 'review-log `(= next-rev ',(gnosis-algorithm-date 5))
                     `(= id ,id2))
      (let ((gnosis-review-new-first nil)
            (gnosis-new-themata-limit nil))
        (let ((due (gnosis-review-get--due-themata)))
          (should (= 1 (length due)))
          (should (= id1 (caar due))))))))

(ert-deftest gnosis-test-review-get-due-new-first-ordering ()
  "With gnosis-review-new-first, new themata appear before old ones."
  (gnosis-test-with-db
    (let* ((deck-id (gnosis-test--add-deck "review-test"))
           (new-id (gnosis-test--add-basic-thema deck-id "New Q" "A"))
           (old-id (gnosis-test--add-basic-thema deck-id "Old Q" "A")))
      ;; Make old-id look reviewed (n > 0)
      (gnosis-sqlite-execute gnosis-db
        "UPDATE review_log SET n = 5 WHERE id = ?" (list old-id))
      (let ((gnosis-review-new-first t)
            (gnosis-new-themata-limit nil))
        (let* ((due (gnosis-review-get--due-themata))
               (ids (mapcar #'car due)))
          (should (= 2 (length ids)))
          ;; New thema should come first
          (should (= new-id (car ids))))))))

;;; ---- Group 4: gnosis-review-is-thema-new-p ----

(ert-deftest gnosis-test-review-thema-new-p-zero-reviews ()
  "Thema with n=0 is new."
  (gnosis-test-with-db
    (let* ((deck-id (gnosis-test--add-deck "review-test"))
           (id (gnosis-test--add-basic-thema deck-id "Q" "A")))
      (should (gnosis-review-is-thema-new-p id)))))

(ert-deftest gnosis-test-review-thema-new-p-reviewed ()
  "Thema with n>0 is NOT new."
  (gnosis-test-with-db
    (let* ((deck-id (gnosis-test--add-deck "review-test"))
           (id (gnosis-test--add-basic-thema deck-id "Q" "A")))
      ;; Simulate a review
      (gnosis-sqlite-execute gnosis-db
        "UPDATE review_log SET n = 1 WHERE id = ?" (list id))
      (should-not (gnosis-review-is-thema-new-p id)))))

(provide 'gnosis-test-review)

(ert-run-tests-batch-and-exit)
;;; gnosis-test-review.el ends here

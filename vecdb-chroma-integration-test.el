;;; vecdb-chroma-integration-test.el --- Integration tests for vecdb-chroma -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Google LLC
;;
;; Author: Google LLC
;; Maintainer: Google LLC
;; Created: November 2023
;; Modified: November 2023
;; Version: 0.1
;; Keywords: tools, ai
;; Package-Requires: ((emacs "29.1") (vecdb "0.1") (chroma-ext "0.1"))
;;
;; This file is not part of GNU Emacs.
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;     http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
;;
;;; Commentary:
;;
;;  This file contains integration tests for the `vecdb-chroma' package.
;;  It tests the interaction between `vecdb' and the ChromaDB server.
;;
;;  To run these tests manually (e.g., if `eldev test` is problematic):
;;  CHROMA_URL="http://localhost:8000" emacs -batch -l ert -l vecdb-chroma-integration-test.el -f ert-run-tests-batch-and-exit
;;
;;; Code:

(require 'ert)
(require 'vecdb)
(require 'vecdb-chroma)
(require 'cl-lib) ;; For cl-remove-if-not, cl-every

(declare-function chroma-ext--tmp-project-dir "ext-chroma")

;;; Test Suite Definition
(def-ert-suite vecdb-chroma-integration-suite
  "Integration tests for `vecdb-chroma' package."
  :require (vecdb-chroma))

;;; Helper Functions

(defun vecdb-test--chroma-provider ()
  "Initialize and return a `vecdb-chroma-provider' struct.
Reads Chroma URL, tenant, and database from environment variables:
`CHROMA_URL', `CHROMA_TENANT', `CHROMA_DATABASE'.
If `CHROMA_URL' is not set, signals a test skip."
  (let ((url (getenv "CHROMA_URL")))
    (unless url
      (ert-skip "CHROMA_URL environment variable not set."))
    (make-vecdb-chroma-provider
     :url url
     :tenant (getenv "CHROMA_TENANT")
     :database (getenv "CHROMA_DATABASE"))))

(defun vecdb-test--make-collection (name vector-size)
  "Return a `vecdb-collection' struct with NAME and VECTOR-SIZE."
  (make-vecdb-collection :name name :vector-size vector-size))

(defun vecdb-test--make-item (id vector payload)
  "Return a `vecdb-item' struct with ID, VECTOR, and PAYLOAD (plist)."
  (make-vecdb-item :id id :vector vector :payload payload))

(defun vecdb-test--random-vector (dimension)
  "Return a vector of DIMENSION with random float numbers."
  (let ((vec (make-vector dimension 0.0)))
    (dotimes (i dimension)
      (setf (aref vec i) (random 1.0)))
    vec))

(defun vecdb-test--random-string (&optional length)
  "Return a random string of LENGTH (default 8)."
  (let ((s (md5 (format "%s%s" (random) (current-time)))))
    (substring s 0 (or length 8))))

(defmacro with-test-collection ((provider collection-var &key (name (vecdb-test--random-string "test-collection-")) (vector-size 3)) &rest body)
  "Execute BODY with COLLECTION-VAR bound to a new collection.
The collection is created before BODY and deleted afterwards."
  `(let ((,collection-var (vecdb-test--make-collection ,name ,vector-size)))
     (unwind-protect
         (progn
           (vecdb-create ,provider ,collection-var)
           ,@body)
       (vecdb-delete ,provider ,collection-var))))

;;; Test cases

(ert-deftest vecdb-chroma-create-exists-delete-collection-test ()
  "Test `vecdb-create', `vecdb-exists', and `vecdb-delete'."
  {:tags '(vecdb-chroma-integration-suite)}
  (interactive)
  (let ((provider (vecdb-test--chroma-provider))
        (collection-name (vecdb-test--random-string "test-coll-ced-"))
        (collection (vecdb-test--make-collection collection-name 3)))
    (unwind-protect
        (progn
          (should (vecdb-create provider collection))
          (should (vecdb-exists provider collection))
          (should (vecdb-delete provider collection))
          (should-not (vecdb-exists provider collection)))
      ;; Cleanup in case of error during assertions
      (when (vecdb-exists provider collection)
        (vecdb-delete provider collection)))))

(ert-deftest vecdb-chroma-upsert-get-items-test ()
  "Test `vecdb-upsert-items' and `vecdb-get-item'."
  {:tags '(vecdb-chroma-integration-suite)}
  (interactive)
  (let* ((provider (vecdb-test--chroma-provider))
         (collection-name (vecdb-test--random-string "test-coll-ug-"))
         (vector-size 3)
         (items (list
                 (vecdb-test--make-item "item1" (vecdb-test--random-vector vector-size) '(:val 1))
                 (vecdb-test--make-item "item2" (vecdb-test--random-vector vector-size) '(:val 2))
                 (vecdb-test--make-item "item3" (vecdb-test--random-vector vector-size) '(:val 3)))))
    (with-test-collection (provider current-collection :name collection-name :vector-size vector-size)
      (should (vecdb-upsert-items provider current-collection items))
      (dolist (item items)
        (let ((retrieved-item (vecdb-get-item provider current-collection (vecdb-item-id item))))
          (should retrieved-item)
          (should (equal (vecdb-item-id item) (vecdb-item-id retrieved-item)))
          (should (equal (vecdb-item-vector item) (vecdb-item-vector retrieved-item)))
          (should (equal (vecdb-item-payload item) (vecdb-item-payload retrieved-item))))))))

(ert-deftest vecdb-chroma-delete-items-test ()
  "Test `vecdb-delete-items'."
  {:tags '(vecdb-chroma-integration-suite)}
  (interactive)
  (let* ((provider (vecdb-test--chroma-provider))
         (collection-name (vecdb-test--random-string "test-coll-di-"))
         (vector-size 3)
         (item1 (vecdb-test--make-item "item1" (vecdb-test--random-vector vector-size) '(:val 1)))
         (item2 (vecdb-test--make-item "item2" (vecdb-test--random-vector vector-size) '(:val 2)))
         (item3 (vecdb-test--make-item "item3" (vecdb-test--random-vector vector-size) '(:val 3)))
         (items (list item1 item2 item3)))
    (with-test-collection (provider current-collection :name collection-name :vector-size vector-size)
      (vecdb-upsert-items provider current-collection items)
      (should (vecdb-delete-items provider current-collection (list (vecdb-item-id item1) (vecdb-item-id item3))))
      (should-not (vecdb-get-item provider current-collection (vecdb-item-id item1)))
      (should (vecdb-get-item provider current-collection (vecdb-item-id item2)))
      (should-not (vecdb-get-item provider current-collection (vecdb-item-id item3))))))

(ert-deftest vecdb-chroma-search-by-vector-test ()
  "Test `vecdb-search-by-vector'."
  {:tags '(vecdb-chroma-integration-suite)}
  (interactive)
  (let* ((provider (vecdb-test--chroma-provider))
         (collection-name (vecdb-test--random-string "test-coll-sv-"))
         (vector-size 3)
         (item1 (vecdb-test--make-item "item1" (vector 0.1 0.2 0.3) '(:val 1)))
         (item2 (vecdb-test--make-item "item2" (vector 0.4 0.5 0.6) '(:val 2)))
         (item3 (vecdb-test--make-item "item3" (vector 0.7 0.8 0.9) '(:val 3)))
         (items (list item1 item2 item3)))
    (with-test-collection (provider current-collection :name collection-name :vector-size vector-size)
      (vecdb-upsert-items provider current-collection items)
      ;; Search for a vector similar to item2
      (let* ((search-vector (vector 0.41 0.51 0.61))
             (results (vecdb-search-by-vector provider current-collection search-vector 3)))
        (should (= (length results) 3))
        ;; Item2 should be the closest
        (let ((first-result (car results)))
          (should (equal (vecdb-item-id first-result) (vecdb-item-id item2)))
          (should (equal (vecdb-item-vector first-result) (vecdb-item-vector item2)))
          (should (equal (vecdb-item-payload first-result) (vecdb-item-payload item2))))
        ;; Check if all original items are present in results (order might vary beyond the first)
        (should (cl-every (lambda (item)
                            (cl-find-if (lambda (res-item) (equal (vecdb-item-id item) (vecdb-item-id res-item)))
                                        results))
                          items))))))


(ert-deftest vecdb-chroma-search-by-vector-with-filter-test ()
  "Test `vecdb-search-by-vector' with a metadata filter."
  {:tags '(vecdb-chroma-integration-suite)}
  (interactive)
  (let* ((provider (vecdb-test--chroma-provider))
         (collection-name (vecdb-test--random-string "test-coll-svf-"))
         (vector-size 2)
         (item1 (vecdb-test--make-item "itemA1" (vector 0.1 0.2) '(:type "A" :val 1)))
         (item2 (vecdb-test--make-item "itemB1" (vector 0.3 0.4) '(:type "B" :val 2)))
         (item3 (vecdb-test--make-item "itemA2" (vector 0.5 0.6) '(:type "A" :val 3)))
         (items (list item1 item2 item3)))
    (with-test-collection (provider current-collection :name collection-name :vector-size vector-size)
      (vecdb-upsert-items provider current-collection items)
      (let* ((search-vector (vector 0.11 0.21)) ; Closer to itemA1
             ;; Filter for items of type "A"
             (results (vecdb-search-by-vector provider current-collection search-vector 3 '(:type "A"))))
        (should (= (length results) 2))
        (should (cl-every (lambda (item) (equal (getf (vecdb-item-payload item) :type) "A")) results))
        ;; itemA1 should be the first result
        (let ((first-result (car results)))
          (should (equal (vecdb-item-id first-result) (vecdb-item-id item1))))
        ;; Ensure itemB1 is not in the results
        (should-not (cl-find-if (lambda (res-item) (equal (vecdb-item-id res-item) "itemB1")) results))))))


(provide 'vecdb-chroma-integration-test)

;;; vecdb-chroma-integration-test.el ends here

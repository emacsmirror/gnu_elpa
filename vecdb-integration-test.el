;;; vecdb-integration-test.el --- Integration tests for vecdb -*- lexical-binding: t; -*-

;; Copyright (c) 2025  Free Software Foundation, Inc.
;;
;; Author: Andrew Hyatt <ahyatt@gmail.com>
;; Maintainer: Andrew Hyatt <ahyatt@gmail.com>
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
;;  This file contains integration tests for the `vecdb' package,
;;  supporting multiple backends like Chroma and Qdrant.
;;  Tests for specific backends run if corresponding environment variables are set.
;;
;;  To run these tests manually:
;;  1. Ensure the relevant vector database (Chroma and/or Qdrant) is running.
;;  2. Set the necessary environment variables:
;;     For Chroma: CHROMA_URL (e.g., "http://localhost:8000")
;;                 CHROMA_TENANT (optional, defaults to "default")
;;                 CHROMA_DATABASE (optional, defaults to "default")
;;     For Qdrant: QDRANT_URL (e.g., "http://localhost:6333")
;;                 QDRANT_API_KEY (e.g., "your-api-key")
;;     For Postgres: PSQL_DB (should exist already), PSQL_USERNAME, PSQL_PASSWORD (optional)
;;  3. Execute from the command line:
;;     emacs -batch -l ert -l vecdb-integration-test.el -f ert-run-tests-batch-and-exit
;;
;;  The tests for a specific provider will be skipped if its required
;;  environment variables are not set. If no providers are configured,
;;  all tests will be skipped.
;;
;;; Code:

(require 'ert)
(require 'vecdb)
(require 'vecdb-chroma)
(require 'vecdb-qdrant)
(require 'cl-lib) ;; For cl-remove-if-not, cl-every

(declare-function chroma-ext--tmp-project-dir "ext-chroma")

;;; Helper Functions

(defun vecdb-test--get-providers ()
  "Initialize and return a list of available `vecdb-provider' structs.
Reads configuration from environment variables.
Skips tests if no providers are configured."
  (or
   (delq nil
         (list
          ;; ChromaDB Configuration
          (let ((chroma-url (getenv "CHROMA_URL")))
            (when chroma-url
              (make-vecdb-chroma-provider
               :url chroma-url
               :tenant (or (getenv "CHROMA_TENANT") "default")
               :database (or (getenv "CHROMA_DATABASE") "default"))))

          ;; Qdrant Configuration
          (let ((qdrant-url (getenv "QDRANT_URL")))
            (when qdrant-url
              (let ((qdrant-api-key (getenv "QDRANT_API_KEY")))
                (if qdrant-api-key
                    (make-vecdb-qdrant-provider
                     :url qdrant-url
                     :api-key qdrant-api-key)
                  (warn "QDRANT_URL is set, but QDRANT_API_KEY is missing. Qdrant provider will not be configured.")))))

          ;; Postgres Configuration
          (let ((postgres-db (getenv "PSQL_DB"))
                (postgres-username (getenv "PSQL_USERNAME"))
                (postgres-password (getenv "PSQL_PASSWORD")))

            (when postgres-username
              (make-psql-vecdb-provider
               :database postgres-db
               :username postgres-username
               :password postgres-password)))))

   (progn
     (ert-skip "No vector database provider environment variables set. (CHROMA_URL or QDRANT_URL must be set)")
     nil)))

(defmacro vecdb-test--deftest-for-providers (base-name body-function &optional docstring)
  "Define `ert-deftest` forms for BASE-NAME against Chroma and Qdrant providers.
BODY-FUNCTION is a symbol of a function that takes one argument (the provider).
Docstring is optional, a default will be generated.

Each generated test will individually skip if its specific provider
is not found in the list returned by `vecdb-test--get-providers` (which
itself might globally skip if no providers at all are configured)."
  (declare (indent defun))
  (let ((chroma-test-name (intern (format "%s-chroma" base-name)))
        (qdrant-test-name (intern (format "%s-qdrant" base-name)))
        (base-doc (or docstring (format "Test %s for a vector database provider." base-name))))
    `(progn
       (ert-deftest ,chroma-test-name ()
         ,(format "%s (Chroma)" base-doc)
         (interactive) ; Keep interactive for manual runs if desired
         (let ((current-provider (cl-find-if (lambda (p) (eq (type-of p) 'vecdb-chroma-provider))
                                             (vecdb-test--get-providers))))
           (if current-provider
               (funcall ,body-function current-provider)
             (ert-skip (format "Chroma provider not configured for %s" ',chroma-test-name)))))

       (ert-deftest ,qdrant-test-name ()
         ,(format "%s (Qdrant)" base-doc)
         (interactive)
         (let ((current-provider (cl-find-if (lambda (p) (eq (type-of p) 'vecdb-qdrant-provider))
                                             (vecdb-test--get-providers))))
           (if current-provider
               (funcall ,body-function current-provider)
             (ert-skip (format "Qdrant provider not configured for %s" ',qdrant-test-name))))))))

(defmacro with-test-collection (current-provider collection-var collection-name-base options &rest body)
  "Execute BODY with COLLECTION-VAR bound to a new collection.
CURRENT-PROVIDER is the provider instance.
COLLECTION-VAR is the symbol to bind the collection struct to.
COLLECTION-NAME-BASE is the base string for the collection name.
OPTIONS is a plist, e.g., (:vector-size 3).
The full collection name is generated by appending the provider's name.
The collection is created before BODY and deleted afterwards."
  (declare (indent 1) (debug t))
  (let ((full-collection-name (gensym "full-collection-name-"))
        (vector-size-val (gensym "vector-size-"))
        (default-vector-size 3))
    `(let* ((,full-collection-name (format "%s-%s" ,collection-name-base (vecdb-provider-name ,current-provider)))
            (,vector-size-val (or (plist-get ,options :vector-size) ,default-vector-size))
            (,collection-var (make-vecdb-collection :name ,full-collection-name :vector-size ,vector-size-val)))
       (unwind-protect
           (progn
             (vecdb-create ,current-provider ,collection-var)
             ,@body)
         ;; Ensure cleanup even if vecdb-create fails or body errors
         (if (vecdb-exists ,current-provider ,collection-var)
             (vecdb-delete ,current-provider ,collection-var)
           (message "Collection %s did not exist or was already deleted during cleanup." ,full-collection-name))))))

;;; Test cases Body Functions

(defun vecdb-test-create-exists-delete-collection-body (current-provider)
  "Core logic for testing create, exists, and delete collection."
  (let* ((collection-name-base "test-collection-ced")
         (collection-name (format "%s-%s" collection-name-base (vecdb-provider-name current-provider)))
         (collection (make-vecdb-collection :name collection-name :vector-size 3)))
    (unwind-protect
        (progn
          (vecdb-create current-provider collection)
          (should (vecdb-exists current-provider collection))
          (vecdb-delete current-provider collection)
          (should-not (vecdb-exists current-provider collection)))
      ;; Cleanup in case of error during assertions
      (when (vecdb-exists current-provider collection)
        (vecdb-delete current-provider collection)))))

(vecdb-test--deftest-for-providers vecdb-test-create-exists-delete-collection
  #'vecdb-test-create-exists-delete-collection-body
  "Test `vecdb-create', `vecdb-exists', and `vecdb-delete'.")

(defun vecdb-test-upsert-get-delete-items-body (current-provider)
  "Core logic for testing upsert and get items."
  (let* ((collection-name "test-collection-ug")
         (vector-size 3)
         (items (list
                 (make-vecdb-item :id 1 :vector [0 1 2] :payload '(:val 1))
                 (make-vecdb-item :id 2 :vector [0 1 2] :payload '(:val 2))
                 (make-vecdb-item :id 3 :vector [0 1 2] :payload '(:val 3)))))
    (with-test-collection current-provider current-collection collection-name `(:vector-size ,vector-size)
                          (vecdb-upsert-items current-provider current-collection items t)
                          (dolist (item items)
                            (let ((retrieved-item (vecdb-get-item current-provider current-collection (vecdb-item-id item))))
                              (should retrieved-item)
                              (should (equal (vecdb-item-id item) (vecdb-item-id retrieved-item)))
                              ;; We don't test to see if the vector is equal,
                              ;; because it could be normalized.
                              (should (equal (vecdb-item-payload item) (vecdb-item-payload retrieved-item)))))
                          (vecdb-delete-items current-provider current-collection (mapcar #'vecdb-item-id items) t)
                          (dolist (item items)
                            (should-not (vecdb-get-item current-provider current-collection (vecdb-item-id item)))))))

(vecdb-test--deftest-for-providers vecdb-test-upsert-get-delete-items
  #'vecdb-test-upsert-get-delete-items-body
  "Test `vecdb-upsert-items', `vecdb-get-item' and `vecdb-delete-items'.")

(defun vecdb-test-search-by-vector-body (current-provider)
  "Core logic for testing search by vector."
  (let* ((collection-name "test-collection-sv")
         (vector-size 3)
         (item1 (make-vecdb-item :id 1 :vector [0.1 0.2 0.3] :payload '(:val 1)))
         (item2 (make-vecdb-item :id 2 :vector [0.4 0.5 0.6] :payload '(:val 2)))
         (item3 (make-vecdb-item :id 3 :vector [0.7 0.8 0.9] :payload '(:val 3)))
         (items (list item1 item2 item3)))
    (with-test-collection current-provider current-collection collection-name `(:vector-size ,vector-size)
                          (vecdb-upsert-items current-provider current-collection items t)
                          ;; Search for a vector similar to item2
                          (let ((results (vecdb-search-by-vector current-provider current-collection [0.41 0.51 0.61] 3)))
                            (should (= (length results) 3))
                            ;; Item2 should be the closest
                            (let ((first-result (car results)))
                              (should (equal (vecdb-item-id first-result) (vecdb-item-id item2)))
                              (should (equal (vecdb-item-payload first-result) (vecdb-item-payload item2))))
                            ;; Check if all original items are present in results (order might vary beyond the first)
                            (should (cl-every (lambda (item)
                                                (cl-find-if (lambda (res-item) (equal (vecdb-item-id item) (vecdb-item-id res-item)))
                                                            results))
                                              items))))))

(vecdb-test--deftest-for-providers
  vecdb-test-search-by-vector
  #'vecdb-test-search-by-vector-body
  "Test `vecdb-search-by-vector'.")

(provide 'vecdb-integration-test)

;;; vecdb-integration-test.el ends here

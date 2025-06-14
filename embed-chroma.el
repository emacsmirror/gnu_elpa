;;; embed-chroma.el --- An interface to the chroma databases -*- lexical-binding: t; -*-

;; Copyright (c) 2025  Free Software Foundation, Inc.

;; Author: Andrew Hyatt <ahyatt@gmail.com>
;; Homepage: https://github.com/ahyatt/embed-db
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; This package provides an interface to the chroma databases for embed-db.


;;; Code:

(require 'embed-db)
(require 'plz)

(defconst embed-chroma-collection-id-cache
  (make-hash-table :test 'equal)
  "Cache for chroma collection IDs.")

(cl-defstruct (embed-chroma-provider (:include embed-db-provider
                                               (name "chroma")))
  (binary "chroma")
  (url "http://localhost:8000")
  (tenant "default")
  (database "default"))

(defun embed-chroma-call (provider method url-suffix &optional body sync)
  "Make an HTTP request to the Chroma API.
If BODY is provided, it will be sent as the request body.
SYNC indicates whether the request should be synchronous."
  (let ((url (embed-chroma-provider-url provider)))
    (unless url
      (error "Chroma URL is not set"))
    (if sync
        (json-parse-string
         (plz method (concat url url-suffix)
           :headers '(("Content-Type" . "application/json"))
           :body (if body (json-encode body) ""))
         :object-type 'plist)
      (plz method (concat url url-suffix)
        :headers '(("Content-Type" . "application/json"))
        :body (if body (json-encode body) "")
        :then #'ignore))))

(defun embed-chroma-has-tenant-p (provider)
  "Check if the chroma PROVIDER has a tenant."
  (condition-case err
      (embed-chroma-call
       provider
       'get
       (format "/api/v2/tenants/%s" (embed-chroma-provider-tenant provider))
       nil
       t)
    (plz-error (if (eq 404 (plz-response-status (plz-error-response (nth 2 err))))
                   nil
                 (error "Error checking tenant: %s" (plz-error-message err))))))

(defun embed-chroma-has-database-p (provider database)
  "Check if the chroma PROVIDER has a DATABASE."
  (condition-case err
      (embed-chroma-call
       provider
       'get
       (format "/api/v2/tenants/%s/databases/%s"
               (embed-chroma-provider-tenant provider)
               database)
       nil
       t)
    (plz-error (if (eq 404 (plz-response-status (plz-error-response (nth 2 err))))
                   nil
                 (error "Error checking database: %s" (plz-error-message err))))))

(cl-defmethod embed-db-create ((provider embed-chroma-provider)
                               (collection embed-db-collection))
  "Create a new chroma collection."
  (unless (embed-chroma-has-tenant-p provider)
    (embed-chroma-call
     provider
     'post
     "/api/v2/tenants"
     `(("name" . ,(embed-chroma-provider-tenant provider))) t))
  (unless (embed-chroma-has-database-p provider (embed-chroma-provider-database provider))
    (embed-chroma-call
     provider
     'post
     (format "/api/v2/tenants/%s/databases"
             (embed-chroma-provider-tenant provider))
     `(:name ,(embed-chroma-provider-database provider)) t))
  (embed-chroma-call
   provider
   'post
   (format "/api/v2/tenants/%s/databases/%s/collections"
           (embed-chroma-provider-tenant provider)
           (embed-chroma-provider-database provider))
   `(:name ,(embed-db-collection-name collection))
   t))

(cl-defmethod embed-db-delete ((provider embed-chroma-provider)
                               (collection embed-db-collection))
  "Delete a chroma collection."
  (embed-chroma-call
   provider
   'delete
   (format "/api/v2/tenants/%s/databases/%s/collections/%s"
           (embed-chroma-provider-tenant provider)
           (embed-chroma-provider-database provider)
           (embed-db-collection-name collection))
   nil
   t))

(defun embed-chroma-collection-id (provider collection)
  "Get the ID of a chroma COLLECTION in PROVIDER."
  (or (gethash (embed-db-collection-name collection)
               embed-chroma-collection-id-cache)
      (let* ((url (format "/api/v2/tenants/%s/databases/%s/collections/%s"
                          (embed-chroma-provider-tenant provider)
                          (embed-chroma-provider-database provider)
                          (embed-db-collection-name collection)))
             (result (embed-chroma-call provider 'get url nil t)))
        (let ((id (plist-get result :id)))
          (puthash (embed-db-collection-name collection) id
                   embed-chroma-collection-id-cache)
          id))))

(cl-defmethod embed-db-exists ((provider embed-chroma-provider)
                               (collection embed-db-collection))
  "Check if a chroma collection exists."
  (and (embed-chroma-has-tenant-p provider)
       (embed-chroma-has-database-p provider
                                    (embed-chroma-provider-database provider))
       (condition-case nil
           (embed-chroma-call
            provider
            'get
            (format "/api/v2/tenants/%s/databases/%s/collections/%s"
                    (embed-chroma-provider-tenant provider)
                    (embed-chroma-provider-database provider)
                    (embed-chroma-collection-id provider collection))
            nil
            t)
         (plz-error nil))))

(cl-defmethod embed-db-upsert-items ((provider embed-chroma-provider)
                                     (collection embed-db-collection)
                                     items &optional sync)
  "Upsert items into a chroma collection."
  (let ((url (format "/api/v2/tenants/%s/databases/%s/collections/%s/upsert"
                     (embed-chroma-provider-tenant provider)
                     (embed-chroma-provider-database provider)
                     (embed-chroma-collection-id provider collection))))
    (embed-chroma-call
     provider
     'post
     url
     `(:embeddings ,(apply #'vector (mapcar #'embed-db-item-vector items))
                   :ids ,(apply #'vector (mapcar #'embed-db-item-id items))
                   :metadatas ,(apply #'vector (mapcar #'embed-db-item-payload items)))
     sync)))

(cl-defmethod embed-db-get-item ((provider embed-chroma-provider)
                                 (collection embed-db-collection)
                                 item-id)
  "Get a single item from a chroma collection by ITEM-ID."
  (let* ((url (format "/api/v2/tenants/%s/databases/%s/collections/%s/get"
                      (embed-chroma-provider-tenant provider)
                      (embed-chroma-provider-database provider)
                      (embed-chroma-collection-id provider collection)))
         (result (embed-chroma-call provider 'get url
                                    `(:ids ,(vector item-id)
                                           :limit 1)
                                    t)))
    (unless (= (length (plist-get result :items)) 1)
      (error "Expected exactly one item, got %d"
             (length (plist-get result :items)))
      (make-embed-db-item
       :id (aref (plist-get result :ids) 0)
       :vector (aref (plist-get result :embeddings) 0)
       :payload (aref (plist-get result :metadatas) 0)))))

(cl-defmethod embed-db-delete-items ((provider embed-chroma-provider)
                                     (collection embed-db-collection)
                                     item-ids &optional sync)
  "Delete items from a chroma collection by ITEM-IDS."
  (let ((url (format "/api/v2/tenants/%s/databases/%s/collections/%s/delete"
                     (embed-chroma-provider-tenant provider)
                     (embed-chroma-provider-database provider)
                     (embed-chroma-collection-id provider collection))))
    (embed-chroma-call
     provider
     'post
     url
     `(:ids ,(apply #'vector item-ids))
     sync)))

(cl-defmethod embed-db-search-by-vector ((provider embed-chroma-provider)
                                         (collection embed-db-collection)
                                         vector &optional limit)
  "Search for items in a chroma collection by VECTOR."
  (let* ((url (format "/api/v2/tenants/%s/databases/%s/collections/%s/query"
                      (embed-chroma-provider-tenant provider)
                      (embed-chroma-provider-database provider)
                      (embed-chroma-collection-id provider collection)))
         (result (embed-chroma-call provider 'post url
                                    `(:query_embeddings [,vector]
                                                        :n_results ,(or limit 10)
                                                        :include ["embeddings" "metadatas" "distances"])
                                    t)))
    (cl-loop for i from 0 below (length (aref (plist-get result :ids) 0))
             collect (make-embed-db-item ;
                      :id (aref (aref (plist-get result :ids) 0) i)
                      :vector (aref (aref (plist-get result :embeddings) 0) i)
                      :payload (aref (aref (plist-get result :metadatas) 0) i)))))

(provide 'embed-chroma)
;;; embed-chroma.el ends here

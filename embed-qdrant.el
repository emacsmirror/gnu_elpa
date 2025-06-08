;;; embed-qdrant.el --- An interface to the qdrant databases -*- lexical-binding: t; -*-

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
;; This package provides an implementation of embed-db for the qdrant db.

;;; Code:

(require 'embed-db)
(require 'cl-lib)
(require 'json)
(require 'plz)

(cl-defstruct (embed-qdrant-provider (:include embed-db-provider
                                               (name "qdrant")))
  "Provider for the qdrant embedding database."
  (url nil)
  (api-key nil))

(defun embed-qdrant-call (provider method url-suffix &optional body sync)
  "Make an HTTP request to the Qdrant API.
If BODY is provided, it will be sent as the request body.
SYNC indicates whether the request should be synchronous."
  (let ((url (embed-qdrant-provider-url provider))
        (api-key (embed-qdrant-provider-api-key provider)))
    (unless url
      (error "Qdrant URL is not set"))
    (unless api-key
      (error "Qdrant API key is not set"))
    (if sync
        (json-parse-string
         (plz method (concat url url-suffix)
           :headers (append
                     `((api-key . ,api-key))
                     (when body
                       '(("Content-Type" . "application/json"))))
           :body (if body (json-encode body) ""))
         :object-type 'plist)
      (plz method (concat url url-suffix)
        :headers (append
                  `((api-key . ,api-key))
                  (when body
                    '(("Content-Type" . "application/json"))))
        :body (if body (json-encode body) "")
        :then #'ignore))))

(cl-defmethod embed-db-create ((provider embed-qdrant-provider)
                               (collection embed-db-collection))
  "Create a new collection in the qdrant database.

COLLECTION is an `embed-db-collection' object that specifies the
properties of the collection."
  (embed-qdrant-call provider 'put (concat "/collections/" (embed-db-collection-name collection))
                     `(:vectors (:size ,(embed-db-collection-vector-size collection)
                                       :distance "Cosine"))
                     t))

(cl-defmethod embed-db-delete ((provider embed-qdrant-provider)
                               (collection embed-db-collection))
  "Delete a collection from the qdrant database."
  (embed-qdrant-call provider 'delete (concat "/collections/" (embed-db-collection-name collection))
                     nil t))

(cl-defmethod embed-db-exists ((provider embed-qdrant-provider)
                               (collection embed-db-collection))
  "Check if a collection exists in the qdrant database."
  (let ((response (embed-qdrant-call provider 'get (concat "/collections/" (embed-db-collection-name collection)
                                                           "/exists") nil t)))
    (not (eq :false (plist-get (plist-get response :result) :exists)))))

(cl-defmethod embed-db-upsert-items ((provider embed-qdrant-provider)
                                     (collection embed-db-collection)
                                     items &optional sync)
  "Insert items into a collection in the qdrant database."
  (embed-qdrant-call provider 'put
                     (concat "/collections/" (embed-db-collection-name collection) "/points?wait="
                             (if sync "true" "false"))
                     `(:points ,(apply #'vector
                                       (mapcar (lambda (item)
                                                 (append
                                                  `(:id ,(embed-db-item-id item)
                                                        :vector ,(embed-db-item-vector item))
                                                  (when (embed-db-item-payload item)
                                                    `(:payload ,(embed-db-item-payload item))))
                                                 )
                                               items)))
                     t))

(cl-defmethod embed-db-get-item ((provider embed-qdrant-provider)
                                 (collection embed-db-collection)
                                 id)
  "Retrieve an item from a collection in the qdrant database by ID."
  (let ((result (embed-qdrant-call provider 'get
                                   (concat "/collections/" (embed-db-collection-name collection) "/points/" id))))
    (when result
      (let ((point (plist-get result :result)))
        (when point
          (make-embed-db-item :id (plist-get point :id)
                              :vector (plist-get point :vector)
                              :payload (plist-get point :payload)))))))

(cl-defmethod embed-db-delete-items ((provider embed-qdrant-provider)
                                     (collection embed-db-collection)
                                     ids &optional sync)
  "Delete items from a collection in the qdrant database."
  (embed-qdrant-call provider 'post
                     (concat "/collections/" (embed-db-collection-name collection) "/points/delete?wait="
                             (if sync "true" "false"))
                     `(:points ,(apply #'vector ids))
                     nil
                     sync))

(cl-defmethod embed-db-search-by-vector ((provider embed-qdrant-provider)
                                         (collection embed-db-collection)
                                         vector &optional limit)
  "Search for items in a collection in the qdrant database."
  (let ((result (embed-qdrant-call provider 'post
                                   (concat "/collections/" (embed-db-collection-name collection) "/points/query")
                                   `(:query (:nearest ,vector)
                                            :limit ,(or limit 10)
                                            :with_payload t)
                                   t)))
    (mapcar (lambda (point)
              (let ((id (plist-get point :id))
                    (vector (plist-get point :vector))
                    (payload (plist-get point :payload)))
                (make-embed-db-item :id id :vector vector :payload payload)))
            (plist-get (plist-get result :result) :points))))

(provide 'embed-qdrant)
;;; embed-qdrant.el ends here

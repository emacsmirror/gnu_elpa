;;; vecdb-qdrant.el --- An interface to the qdrant databases -*- lexical-binding: t; -*-

;; Copyright (c) 2025  Free Software Foundation, Inc.

;; Author: Andrew Hyatt <ahyatt@gmail.com>
;; Homepage: https://github.com/ahyatt/vecdb
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
;; This package provides an implementation of vecdb for the qdrant db.

;;; Code:

(require 'vecdb)
(require 'cl-lib)
(require 'json)
(require 'plz)

(cl-defstruct (vecdb-qdrant-provider (:include vecdb-provider
                                               (name "qdrant")))
  "Provider for the qdrant embedding database."
  (url nil)
  (api-key nil))

(defun vecdb-qdrant-call (provider method url-suffix &optional body sync)
  "Make an HTTP request to the Qdrant API specified in PROVIDER.
METHOD is the HTTP method of the request (a symbol).
URL-SUFFIX is the path and optional parameters of the request.
If BODY is provided, it will be sent as the request body.
SYNC indicates whether the request should be synchronous."
  (let ((url (vecdb-qdrant-provider-url provider))
        (api-key (vecdb-qdrant-provider-api-key provider)))
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

(cl-defmethod vecdb-create ((provider vecdb-qdrant-provider)
                            (collection vecdb-collection))
  "Create a new COLLECTION of embeddings for PROVIDER."
  (vecdb-qdrant-call provider 'put (concat "/collections/" (vecdb-collection-name collection))
                     `(:vectors (:size ,(vecdb-collection-vector-size collection)
                                       :distance "Cosine"))
                     t))

(cl-defmethod vecdb-delete ((provider vecdb-qdrant-provider)
                            (collection vecdb-collection))
  "Delete a COLLECTION of embeddings in PROVIDER.  This should remove all data."
  (vecdb-qdrant-call provider 'delete (concat "/collections/" (vecdb-collection-name collection))
                     nil t))

(cl-defmethod vecdb-exists ((provider vecdb-qdrant-provider)
                            (collection vecdb-collection))
  "Check if a COLLECTION exists in PROVIDER, return non-nil if it does."
  (let ((response (vecdb-qdrant-call provider 'get (concat "/collections/" (vecdb-collection-name collection)
                                                           "/exists") nil t)))
    (not (eq :false (plist-get (plist-get response :result) :exists)))))

(cl-defmethod vecdb-upsert-items ((provider vecdb-qdrant-provider)
                                  (collection vecdb-collection)
                                  items &optional sync)
  "Upsert a list of `vecdb-item' objects into the COLLECTION with PROVIDER."
  (vecdb-qdrant-call provider 'put
                     (concat "/collections/" (vecdb-collection-name collection) "/points?wait="
                             (if sync "true" "false"))
                     `(:points ,(apply #'vector
                                       (mapcar (lambda (item)
                                                 (append
                                                  `(:id ,(vecdb-item-id item)
                                                        :vector ,(vecdb-item-vector item))
                                                  (when (vecdb-item-payload item)
                                                    `(:payload ,(vecdb-item-payload item)))))
                                               items)))
                     t))

(cl-defmethod vecdb-get-item ((provider vecdb-qdrant-provider)
                              (collection vecdb-collection)
                              id)
  "Get items with ID from the COLLECTION with PROVIDER."
  (let ((result (vecdb-qdrant-call provider 'get
                                   (concat "/collections/" (vecdb-collection-name collection) "/points/" id))))
    (when result
      (let ((point (plist-get result :result)))
        (when point
          (make-vecdb-item :id (plist-get point :id)
                           :vector (plist-get point :vector)
                           :payload (plist-get point :payload)))))))

(cl-defmethod vecdb-delete-items ((provider vecdb-qdrant-provider)
                                  (collection vecdb-collection)
                                  ids &optional sync)
  "Delete items with IDs from the COLLECTION with PROVIDER."
  (vecdb-qdrant-call provider 'post
                     (concat "/collections/" (vecdb-collection-name collection) "/points/delete?wait="
                             (if sync "true" "false"))
                     `(:points ,(apply #'vector ids))
                     nil
                     sync))

(cl-defmethod vecdb-search-by-vector ((provider vecdb-qdrant-provider)
                                      (collection vecdb-collection)
                                      vector &optional limit)
  "Search for items in the COLLECTION with PROVIDER that are similar to VECTOR."
  (let ((result (vecdb-qdrant-call provider 'post
                                   (concat "/collections/" (vecdb-collection-name collection) "/points/query")
                                   `(:query (:nearest ,vector)
                                            :limit ,(or limit 10)
                                            :with_payload t)
                                   t)))
    (mapcar (lambda (point)
              (let ((id (plist-get point :id))
                    (vector (plist-get point :vector))
                    (payload (plist-get point :payload)))
                (make-vecdb-item :id id :vector vector :payload payload)))
            (plist-get (plist-get result :result) :points))))

(provide 'vecdb-qdrant)

;;; vecdb-qdrant.el ends here

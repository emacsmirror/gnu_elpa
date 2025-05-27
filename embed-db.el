;;; embed-db.el --- An interface to embedding databases -*- lexical-binding: t; -*-

;; Copyright (c) 2025  Free Software Foundation, Inc.

;; Author: Andrew Hyatt <ahyatt@gmail.com>
;; Homepage: https://github.com/ahyatt/embed-db
;; Package-Requires: ((emacs "28.1") (plz "0.8"))
;; Package-Version: 0.1
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
;; This package provides an interface to embedding databases, allowing
;; applications to store data and retrieve it via similarity search.

;;; Code

(require 'cl-lib)

(cl-defstruct embed-db-provider
  "A structure representing an embedding database provider."
  name)

(cl-defstruct embed-db-collection
  "A structure representing a collection in an embedding database.

The NAME field is the name of the collection.

The VECTOR-SIZE field is the size of the vectors stored in the
collection.

PAYLOAD-FIELDS is an alist of fields and types (`integer', `string', etc.)
conses, which are sometimes necessary for an initial setup."
  name vector-size payload-fields)

(cl-defstruct embed-db-item
  "A structure representing data to be stored in an embedding database.
An ID may be an integer or a string, and is used to uniquely identify the item."
  id vector payload)

(cl-defgeneric embed-db-create (provider collection)
  "Create a new collection of embeddings."
  (ignore collection)
  (signal 'not-implemented
          (list "embed-db-create not implemented for" (embed-db-provider-name provider))))

(cl-defgeneric embed-db-delete (provider collection)
  "Delete a collection of embeddings. This should remove all data."
  (ignore collection)
  (signal 'not-implemented
          (list "embed-db-delete not implemented for" (embed-db-provider-name provider))))

(cl-defgeneric embed-db-exists (provider collection)
  "Check if a collection exists, return non-nil if it does."
  (ignore collection)
  (signal 'not-implemented
          (list "embed-db-exists not implemented for" (embed-db-provider-name provider))))

(cl-defgeneric embed-db-upsert-items (provider collection data-list &optional sync)
  "Upsert a list of `embed-db-item' objects into the COLLECTION with PROVIDER."
  (ignore collection data-list sync)
  (signal 'not-implemented
          (list "embed-db-upsert not implemented for" (embed-db-provider-name provider))))

(cl-defgeneric embed-db-get-item (provider collection ids &optional sync)
  "Get items with IDs from the COLLECTION with PROVIDER."
  (ignore collection ids sync)
  (signal 'not-implemented
          (list "embed-db-get not implemented for" (embed-db-provider-name provider))))

(cl-defgeneric embed-db-delete-items (provider collection ids &optional sync)
  "Delete items with IDs from the COLLECTION with PROVIDER."
  (ignore collection ids sync)
  (signal 'not-implemented
          (list "embed-db-delete not implemented for" (embed-db-provider-name provider))))

(cl-defgeneric embed-db-search-by-vector (provider collection vector &optional limit)
  "Search for items in the COLLECTION with PROVIDER that are similar to VECTOR."
  (ignore collection vector limit)
  (signal 'not-implemented
          (list "embed-db-search not implemented for" (embed-db-provider-name provider))))


(provide 'embed-db)

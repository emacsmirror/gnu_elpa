;;; vecdb.el --- An interface to vector databases for embeddings -*- lexical-binding: t; -*-

;; Copyright (c) 2025  Free Software Foundation, Inc.

;; Author: Andrew Hyatt <ahyatt@gmail.com>
;; Homepage: https://github.com/ahyatt/vecdb
;; Package-Requires: ((emacs "29.1") (plz "0.8") (pg "0.56"))
;; Package-Version: 0.2.2
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

(require 'cl-lib)

;;; Code:

(cl-defstruct vecdb-provider
  "A structure representing an embedding database provider."
  name)

(cl-defstruct vecdb-collection
  "A structure representing a collection in an embedding database.

The NAME field is the name of the collection.

The VECTOR-SIZE field is the size of the vectors stored in the
collection.

PAYLOAD-FIELDS is an alist of fields and types (`integer', `string', etc.)
conses, which are sometimes necessary for an initial setup."
  name vector-size payload-fields)

(cl-defstruct vecdb-item
  "A structure representing data to be stored in an embedding database.
An ID may be an integer or a string, and is used to uniquely identify the item."
  id vector payload)

(cl-defgeneric vecdb-create (provider collection)
  "Create a new COLLECTION of embeddings for PROVIDER."
  (ignore collection)
  (signal 'not-implemented
          (list "vecdb-create not implemented for" (vecdb-provider-name provider))))

(cl-defgeneric vecdb-delete (provider collection)
  "Delete a COLLECTION of embeddings in PROVIDER.  This should remove all data."
  (ignore collection)
  (signal 'not-implemented
          (list "vecdb-delete not implemented for" (vecdb-provider-name provider))))

(cl-defgeneric vecdb-exists (provider collection)
  "Check if a COLLECTION exists in PROVIDER, return non-nil if it does."
  (ignore collection)
  (signal 'not-implemented
          (list "vecdb-exists not implemented for" (vecdb-provider-name provider))))

(cl-defgeneric vecdb-upsert-items (provider collection data-list &optional sync)
  "Upsert a list of `vecdb-item' objects into the COLLECTION with PROVIDER."
  (ignore collection data-list sync)
  (signal 'not-implemented
          (list "vecdb-upsert not implemented for" (vecdb-provider-name provider))))

(cl-defgeneric vecdb-get-item (provider collection id)
  "Get items with ID from the COLLECTION with PROVIDER.
If the item does not exist, return nil."
  (ignore collection id)
  (signal 'not-implemented
          (list "vecdb-get not implemented for" (vecdb-provider-name provider))))

(cl-defgeneric vecdb-delete-items (provider collection ids &optional sync)
  "Delete items with IDs from the COLLECTION with PROVIDER."
  (ignore collection ids sync)
  (signal 'not-implemented
          (list "vecdb-delete not implemented for" (vecdb-provider-name provider))))

(cl-defgeneric vecdb-search-by-vector (provider collection vector &optional limit)
  "Search for items in the COLLECTION with PROVIDER that are similar to VECTOR."
  (ignore collection vector limit)
  (signal 'not-implemented
          (list "vecdb-search not implemented for" (vecdb-provider-name provider))))


(provide 'vecdb)

;;; vecdb.el ends here

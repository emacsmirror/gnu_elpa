;;; vecdb-psql.el --- An interface to postgres with vector extension -*- lexical-binding: t; -*-

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
;; This package provides an implementation of vecdb for using with postgres.

;;; Code:

(require 'vecdb)
(require 'pg)
(require 'cl-lib)
(require 'map)
(require 'seq)

(cl-defstruct (vecdb-psql-provider (:include vecdb-provider
                                             (name "postgres")))
  "Provider for the vector database.
DBNAME is the database name, which must have been created by the user."
  dbname
  username
  (password "")
  (host "localhost")
  (port 5432))

(defconst vecdb-psql-connection-cache
  (make-hash-table :test 'equal)
  "Cache for database connections by db name.")

(defun vecdb-psql-get-connection (provider)
  "Get a connection to the database specified by PROVIDER."
  (let* ((key (vecdb-psql-provider-dbname provider))
         (connection (gethash key vecdb-psql-connection-cache)))
    (unless connection
      (setq connection
            (pg-connect-plist
             (vecdb-psql-provider-dbname provider)
             (vecdb-psql-provider-username provider)
             :password (vecdb-psql-provider-password provider)
             :host (vecdb-psql-provider-host provider)
             :port (vecdb-psql-provider-port provider)))
      (puthash key connection vecdb-psql-connection-cache))
    connection))

(defun vecdb-psql-table-name (collection-name)
  "Turn COLLECTION-NAME into a safe table name."
  (replace-regexp-in-string "[^a-zA-Z0-9_]" "_" (downcase collection-name)))

(defun vecdb-psql-type (collection-type)
  "Convert COLLECTION-TYPE to a PostgreSQL type string."
  (pcase collection-type
    ('string "TEXT")
    ('integer "INT8")
    ('float "FLOAT")
    (_ (error "Unsupported field type: %s" collection-type))))

(defun vecdb-psql-oid (collection-type)
  "Convert COLLECTION-TYPE to a psql OID."
  (pcase collection-type
    ('string "TEXT")
    ('integer "INT8")
    ('float "FLOAT8")
    (_ (error "Unsupported field type: %s" collection-type))))

(cl-defmethod vecdb-create ((provider vecdb-psql-provider)
                            (collection vecdb-collection))
  "Create COLLECTION in database PROVIDER."
  (pg-vector-setup (vecdb-psql-get-connection provider))
  (pg-exec (vecdb-psql-get-connection provider)
           (format "CREATE EXTENSION IF NOT EXISTS vector;"))
  (pg-exec (vecdb-psql-get-connection provider)
           (format "CREATE TABLE IF NOT EXISTS %s (
                     id INT8 PRIMARY KEY,
                     vector VECTOR(%d) NOT NULL%s
                     %s
                   );"
                   (vecdb-psql-table-name (vecdb-collection-name collection))
                   (vecdb-collection-vector-size collection)
                   (if (vecdb-collection-payload-fields collection) "," "")
                   (mapconcat
                    (lambda (field)
                      (format "\"%s\" %s NULL"
                              (car field)
                              (vecdb-psql-type (cdr field))))
                    (vecdb-collection-payload-fields collection)
                    ", ")))
  (pg-exec (vecdb-psql-get-connection provider)
           (format "CREATE INDEX IF NOT EXISTS %s_embedding_hnsw_idx ON %s USING hnsw (vector vector_cosine_ops)"
                   (vecdb-psql-table-name (vecdb-collection-name collection))
                   (vecdb-psql-table-name (vecdb-collection-name collection))))
  (mapc (lambda (field)
          (pg-exec (vecdb-psql-get-connection provider)
                   (format "CREATE INDEX IF NOT EXISTS \"%s_%s_idx\" ON %s (\"%s\")"
                           (vecdb-psql-table-name (vecdb-collection-name collection))
                           (car field)
                           (vecdb-psql-table-name (vecdb-collection-name collection))
                           (car field))))
        (vecdb-collection-payload-fields collection)))

(cl-defmethod vecdb-delete ((provider vecdb-psql-provider)
                            (collection vecdb-collection))
  "Delete COLLECTION from database PROVIDER."
  (pg-exec (vecdb-psql-get-connection provider)
           (format "DROP TABLE IF EXISTS %s;"
                   (vecdb-psql-table-name (vecdb-collection-name collection)))))

(cl-defmethod vecdb-exists ((provider vecdb-psql-provider)
                            (collection vecdb-collection))
  "Check if the COLLECTION exists in the database specified by PROVIDER."
  (let ((result
         (pg-exec (vecdb-psql-get-connection provider)
                  (format "SELECT EXISTS (
                            SELECT FROM information_schema.tables
                            WHERE table_name = '%s'
                          );"
                          (vecdb-psql-table-name (vecdb-collection-name collection))))))
    (and result
         (equal (caar (pg-result result :tuples)) t))))

(defun vecdb-psql--plist-keys (plist)
  "Return a list of keys from PLIST, as strings with the colon removed."
  (cl-loop for (k _v) on plist by #'cddr
           collect (substring (symbol-name k) 1)))

(cl-defmethod vecdb-upsert-items ((provider vecdb-psql-provider)
                                  (collection vecdb-collection)
                                  data-list &optional _)
  "Upsert items into the COLLECTION in the database PROVIDER.
All items in DATA-LIST must have the same payloads."
  (pg-vector-setup (vecdb-psql-get-connection provider))
  (let ((arg-count 0))
    (funcall #'pg-exec-prepared
             (vecdb-psql-get-connection provider)
             (format "INSERT INTO %s (id, vector%s%s) VALUES %s
                    ON CONFLICT (id) DO UPDATE SET vector = EXCLUDED.vector%s%s;"
                     (vecdb-psql-table-name (vecdb-collection-name collection))
                     (if (vecdb-collection-payload-fields collection) ", " "")
                     ;; We assume every vecdb-item has the same payload structure
                     (mapconcat (lambda (field) (format "\"%s\"" field))
                                (vecdb-psql--plist-keys
                                 (vecdb-item-payload (car data-list)))
                                ", ")
                     (mapconcat (lambda (item)
                                  (format "(%s)"
                                          (string-join (cl-loop for i from 1 below (+ 2 (length (vecdb-item-payload item)))
                                                                do (cl-incf arg-count)
                                                                collect (format "$%d" arg-count))
                                                       ", ")))
                                data-list
                                ", ")
                     (if (vecdb-collection-payload-fields collection) ", " "")
                     (mapconcat
                      (lambda (field)
                        (format "\"%s\" = EXCLUDED.\"%s\"" (car field) (car field)))
                      (vecdb-collection-payload-fields collection)
                      ", "))
             (mapcan (lambda (item)
                       (append
                        (list
                         (cons (vecdb-item-id item) "int8")
                         (cons (vecdb-item-vector item) "vector"))
                        (mapcar (lambda (payload-key)
                                  (cons (plist-get (vecdb-item-payload item) payload-key)
                                        (vecdb-psql-oid (assoc-default
                                                         (intern (substring (symbol-name payload-key) 1))
                                                         (vecdb-collection-payload-fields collection)))))
                                (map-keys (vecdb-item-payload (car data-list))))))
                     data-list))))

(defun vecdb-psql--full-row-to-item (row collection)
  "Convert a full database row ROW into a vecdb-item for COLLECTION."
  (make-vecdb-item
   :id (nth 0 row)
   :vector (nth 1 row)
   :payload
   (flatten-list (cl-loop for field in (vecdb-collection-payload-fields collection)
                          collect
                          (list (intern (format ":%s" (car field)))
                                (nth (+ 2 (cl-position field
                                                       (vecdb-collection-payload-fields collection)
                                                       :test #'equal))
                                     row))))))

(cl-defmethod vecdb-get-item ((provider vecdb-psql-provider)
                              (collection vecdb-collection)
                              id)
  "Get an item from COLLECTION by ID.
PROVIDER specifies the database that the collection is in."
  (let ((result
         (pg-result
          (pg-exec-prepared (vecdb-psql-get-connection provider)
                            (format "SELECT id, vector::vector%s %s FROM %s WHERE id = $1;"
                                    (if (vecdb-collection-payload-fields collection) ", " "")
                                    (mapconcat
                                     (lambda (field)
                                       (format "\"%s\"" (car field)))
                                     (vecdb-collection-payload-fields collection)
                                     ", ")
                                    (vecdb-psql-table-name (vecdb-collection-name collection)))
                            (list (cons id "int8")))
          :tuples)))
    (when result
      (vecdb-psql--full-row-to-item (car result) collection))))

(cl-defmethod vecdb-delete-items ((provider vecdb-psql-provider)
                                  (collection vecdb-collection)
                                  ids &optional _)
  "Delete items from COLLECTION by IDs.
PROVIDER is the database that the collection is in."
  (when ids
    ;; TODO: This should ideally be a prepared statement, but I dont know how to do
    ;; this with psql.
    (pg-exec (vecdb-psql-get-connection provider)
             (format "DELETE FROM %s WHERE id IN (%s);"
                     (vecdb-psql-table-name (vecdb-collection-name collection))
                     (mapconcat #'number-to-string ids ", ")))))

(cl-defmethod vecdb-search-by-vector ((provider vecdb-psql-provider)
                                      (collection vecdb-collection)
                                      vector
                                      &optional limit)
  "Search for items in COLLECTION by VECTOR.
PROVIDER is the database that the collection is in."
  (let ((limit-clause (if limit
                          (format "LIMIT %d" limit)
                        "")))
    (mapcar (lambda (row)
              (vecdb-psql--full-row-to-item row collection))
            (pg-result
             (pg-exec-prepared (vecdb-psql-get-connection provider)
                               (format "SELECT id, vector::vector%s %s FROM %s
                      ORDER BY vector <-> $1 %s;"
                                       (if (vecdb-collection-payload-fields collection) ", " "")
                                       (mapconcat
                                        (lambda (field)
                                          (format "\"%s\"" (car field)))
                                        (vecdb-collection-payload-fields collection)
                                        ", ")
                                       (vecdb-psql-table-name (vecdb-collection-name collection))
                                       limit-clause)
                               (list (cons vector "vector")))
             :tuples))))

(provide 'vecdb-psql)

;;; vecdb-psql.el ends here

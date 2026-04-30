;;; gnosis-custom-values.el --- Per-tag algorithm parameters  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2026  Free Software Foundation, Inc.

;; Author: Thanos Apollo <public@thanosapollo.org>
;; Keywords: extensions

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Lookup and validation of per-tag algorithm parameters
;; (amnesia, epignosis, agnoia, proto, anagnosis, lethe).

;;; Code:

(require 'cl-lib)
(require 'gnosis-db)
(require 'gnosis-algorithm)

(defcustom gnosis-custom-values
  '((:tag "demo" (:proto (1 2) :anagnosis 3 :epignosis 0.5 :agnoia 0.3
			 :amnesia 0.45 :lethe 3)))
  "Custom review values for adjusting gnosis algorithm.

Each entry is a list of (:tag NAME PARAMETERS) where:
- NAME is the tag name string
- PARAMETERS is a plist with keys:
  :proto (list of integers), :anagnosis (integer),
  :epignosis (number), :agnoia (number),
  :amnesia (number 0-1), :lethe (positive integer)"
  :type '(repeat sexp)
  :initialize #'custom-initialize-default
  :set (lambda (symbol value)
         (gnosis-validate-custom-values value)
         (set-default symbol value))
  :group 'gnosis)

(defvar gnosis-custom--valid-values
  '(:proto :anagnosis :epignosis :agnoia :amnesia :lethe))

(defun gnosis-validate-custom-values (new-value)
  "Validate the structure and values of NEW-VALUE for gnosis-custom-values."
  (unless (listp new-value)
    (error "GNOSIS-CUSTOM-VALUES should be a list of entries"))
  (dolist (entry new-value)
    (unless (and (listp entry) (= (length entry) 3)
                 (eq (nth 0 entry) :tag)
                 (stringp (nth 1 entry))
                 (listp (nth 2 entry))) ; Ensure the third element is a plist
      (error
       "Each entry should have a :tag keyword, a string, and a plist of custom values"))
    (let ((proto (plist-get (nth 2 entry) :proto))
          (anagnosis (plist-get (nth 2 entry) :anagnosis))
          (epignosis (plist-get (nth 2 entry) :epignosis))
          (agnoia (plist-get (nth 2 entry) :agnoia))
          (amnesia (plist-get (nth 2 entry) :amnesia))
          (lethe (plist-get (nth 2 entry) :lethe)))
      (unless (and (listp proto) (cl-every #'integerp proto))
        (error "Proto must be a list of integer values"))
      (unless (or (null anagnosis) (integerp anagnosis))
        (error "Anagnosis should be an integer"))
      (unless (or (null epignosis) (numberp epignosis))
        (error "Epignosis should be a number"))
      (unless (or (null agnoia) (numberp agnoia))
        (error "Agnoia should be a number"))
      (unless (or (null amnesia) (and (numberp amnesia) (<= amnesia 1) (>= amnesia 0)))
        (error "Amnesia should be a number between 0 and 1"))
      (unless (or (null lethe) (and (integerp lethe) (> lethe 0)))
        (error "Lethe should be an integer greater than 0")))))

(defvar gnosis--custom-values-ht nil
  "Hash table cache mapping tag strings to their custom value plists.
Built lazily by `gnosis--custom-values-lookup', cleared by the
`gnosis-custom-values' watcher.")

(defun gnosis--build-custom-values-ht (&optional values)
  "Build hash table from VALUES (defaults to `gnosis-custom-values').
Each tag key maps to the merged plist of all matching rules."
  (let ((ht (make-hash-table :test #'equal))
        (rules (or values gnosis-custom-values)))
    (dolist (rule rules)
      (let ((tag (plist-get rule :tag)))
        (when (stringp tag)
          (let ((existing (gethash tag ht))
                (props (nth 2 rule)))
            (puthash tag (append existing props) ht)))))
    ht))

(defun gnosis--custom-values-lookup (tag &optional values)
  "Look up custom values for TAG, using cache when possible.
When VALUES is non-nil (test path), bypasses the cache and
searches VALUES directly."
  (if values
      (gnosis-get-custom-values :tag tag values)
    (unless gnosis--custom-values-ht
      (setq gnosis--custom-values-ht (gnosis--build-custom-values-ht)))
    (let ((plist (gethash tag gnosis--custom-values-ht)))
      (when plist
        (gnosis-get-custom-values--validate plist gnosis-custom--valid-values))
      plist)))

(defun gnosis-custom-values-watcher (symbol new-value _operation _where)
  "Watcher for gnosis custom values.

SYMBOL to watch changes for.
NEW-VALUE is the new value set to the variable.
OPERATION is the type of operation being performed.
WHERE is the buffer or object where the change happens."
  (when (eq symbol 'gnosis-custom-values)
    (setq gnosis--custom-values-ht nil)
    (gnosis-validate-custom-values new-value)))

(add-variable-watcher 'gnosis-custom-values 'gnosis-custom-values-watcher)

;; Validate custom values during review process as well.
(defun gnosis-get-custom-values--validate (plist valid-keywords)
  "Verify that PLIST consists of VALID-KEYWORDS."
  (let ((keys (let (ks)
                (while plist
                  (setq ks (cons (car plist) ks))
                  (setq plist (cddr plist)))
                ks)))
    (let ((invalid-key (cl-find-if (lambda (key) (not (member key valid-keywords))) keys)))
      (if invalid-key
          (error "Invalid custom keyword found in: %s" invalid-key)
        t))))

(defun gnosis-get-custom-values (key search-value &optional values)
  "Return SEARCH-VALUE for KEY from VALUES.

VALUES: Defaults to `gnosis-custom-values'."
  (cl-assert (eq key :tag) nil "Key value must be :tag")
  (cl-assert (stringp search-value) nil "Search-value must be the name of a tag as a string.")
  (let ((results)
	(values (or values gnosis-custom-values)))
    (dolist (rule values)
      (when (and (plist-get rule key)
                 (equal (plist-get rule key) search-value))
        (setq results (append results (nth 2 rule)))))
    (gnosis-get-custom-values--validate results gnosis-custom--valid-values)
    results))

(defun gnosis-get-custom-tag-values (id keyword &optional custom-tags custom-values)
  "Return KEYWORD values for thema ID.
Uses cached hash table lookup when CUSTOM-VALUES is nil."
  (cl-assert (keywordp keyword) nil "keyword must be a keyword!")
  (let ((tags (if id (gnosis-select 'tag 'thema-tag `(= thema-id ,id) t) custom-tags)))
    (cl-loop for tag in tags
	     for val = (plist-get (gnosis--custom-values-lookup tag custom-values) keyword)
	     when val collect val)))

(defun gnosis--get-tag-value (id keyword aggregator &optional custom-tags custom-values)
  "Return aggregated tag value for thema ID and KEYWORD.

AGGREGATOR combines multiple tag values (e.g., #\\='max or #\\='min).
Returns nil when no tags define KEYWORD."
  (let ((vals (gnosis-get-custom-tag-values id keyword custom-tags custom-values)))
    (and vals (apply aggregator vals))))

(defun gnosis-get-thema-custom-value (id keyword aggregator default-var
					 &optional validate-p custom-tags custom-values)
  "Return the custom algorithm value for thema ID and KEYWORD.

Looks up tag values (aggregated with AGGREGATOR), falling back to
DEFAULT-VAR.
When VALIDATE-P is non-nil, signals error if value >= 1."
  (let* ((tag-val (gnosis--get-tag-value id keyword aggregator custom-tags custom-values))
         (val (or tag-val default-var)))
    (when (and validate-p (>= val 1))
      (error "%s value must be lower than 1" keyword))
    val))

;; Named wrappers -- tag variants (used in tests)

(defun gnosis-get-thema-tag-amnesia (id &optional custom-tags custom-values)
  "Return tag amnesia for thema ID."
  (gnosis--get-tag-value id :amnesia #'max custom-tags custom-values))

(defun gnosis-get-thema-tag-epignosis (id &optional custom-tags custom-values)
  "Return tag epignosis for thema ID."
  (gnosis--get-tag-value id :epignosis #'max custom-tags custom-values))

(defun gnosis-get-thema-tag-agnoia (id &optional custom-tags custom-values)
  "Return tag agnoia for thema ID."
  (gnosis--get-tag-value id :agnoia #'max custom-tags custom-values))

(defun gnosis-get-thema-tag-anagnosis (id &optional custom-tags custom-values)
  "Return tag anagnosis for thema ID."
  (gnosis--get-tag-value id :anagnosis #'min custom-tags custom-values))

(defun gnosis-get-thema-tag-lethe (id &optional custom-tags custom-values)
  "Return tag lethe for thema ID."
  (gnosis--get-tag-value id :lethe #'min custom-tags custom-values))

;; Named wrappers -- merged (tag overrides default)

(defun gnosis-get-thema-amnesia (id &optional custom-tags custom-values)
  "Return amnesia value for thema ID."
  (gnosis-get-thema-custom-value id :amnesia #'max gnosis-algorithm-amnesia-value
				 t custom-tags custom-values))

(defun gnosis-get-thema-epignosis (id &optional custom-tags custom-values)
  "Return epignosis value for thema ID."
  (gnosis-get-thema-custom-value id :epignosis #'max gnosis-algorithm-epignosis-value
				 t custom-tags custom-values))

(defun gnosis-get-thema-agnoia (id &optional custom-tags custom-values)
  "Return agnoia value for thema ID."
  (gnosis-get-thema-custom-value id :agnoia #'max gnosis-algorithm-agnoia-value
				 t custom-tags custom-values))

(defun gnosis-proto-max-values (proto-values)
  "Return max values from PROTO-VALUES."
  (if (not (and (listp proto-values) (cl-every #'listp proto-values)))
      proto-values
    (let* ((max-len (apply #'max (mapcar #'length proto-values)))
           (padded-lists (mapcar (lambda (lst)
                                   (append lst (make-list (- max-len (length lst)) 0)))
                                 proto-values)))
      (apply #'cl-mapcar #'max padded-lists))))

(defun gnosis-get-thema-proto (id &optional custom-tags custom-values)
  "Return proto values for thema ID.

CUSTOM-VALUES: Custom values to be used instead.
CUSTOM-TAGS: Custom tags to be used instead."
  (let ((tags-proto (gnosis-get-custom-tag-values id :proto custom-tags custom-values)))
    (if tags-proto (gnosis-proto-max-values tags-proto)
      gnosis-algorithm-proto)))

(defun gnosis-get-thema-anagnosis (id &optional custom-tags custom-values)
  "Return anagnosis value for thema ID."
  (gnosis-get-thema-custom-value id :anagnosis #'min gnosis-algorithm-anagnosis-value
				 nil custom-tags custom-values))

(defun gnosis-get-thema-lethe (id &optional custom-tags custom-values)
  "Return lethe value for thema ID."
  (gnosis-get-thema-custom-value id :lethe #'min gnosis-algorithm-lethe-value
				 nil custom-tags custom-values))

(provide 'gnosis-custom-values)
;;; gnosis-custom-values.el ends here

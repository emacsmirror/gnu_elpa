;;; debbugs-bookmarks.el --- Bookmark support for debbugs  -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Free Software Foundation, Inc.

;; Author: Matthias Meulien <orontee@gmail.com>
;; Keywords: convenience
;; Package: debbugs

;; This file is not part of GNU Emacs.

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

;; This file implements the bookmark interface, so one can bookmark a
;; bug query.

;; Use `bookmark-set' in a Debbugs buffer to set a bookmark for the
;; current query (as described by `debbugs-gnu-current-query').  Then
;; `bookmark-jump' to restore a bookmark.

;;; Code:

(declare-function bookmark-make-record-default
                  "bookmark" (&optional no-file no-context posn))
(declare-function bookmark-prop-get "bookmark" (bookmark prop))
(declare-function bookmark-default-handler "bookmark" (bmk))
(declare-function bookmark-get-bookmark-record "bookmark" (bmk))

(declare-function debbugs-gnu "debbugs-gnu")
(declare-function debbugs-gnu-show-reports "debbugs-gnu")
(declare-function debbugs-org-show-reports "debbugs-org")

(defvar debbugs-gnu-current-buffer)
(defvar debbugs-gnu-current-filter)
(defvar debbugs-gnu-current-query)
(defvar debbugs-gnu-current-suppress)
(defvar debbugs-gnu-current-message)
(defvar debbugs-gnu-local-query)
(defvar debbugs-gnu-local-filter)
(defvar debbugs-gnu-local-suppress)
(defvar debbugs-gnu-local-message)
(defvar debbugs-gnu-show-reports-function)

(defun debbugs-gnu-bookmark-name (query)
  "Candidate for bookmark name.
The name depends on whether QUERY specifies bug identifiers or a phrase.
When a phrase is specified, the subject may override the phrase and
packages if any are mentioned.

Examples of generated names follows:
- Bug #20777
- Bugs #20777, #18338, #38388
- Bugs about \"display\" in emacs package
- Bugs about \"display\" in packages emacs,org
- Bugs with subject \"display\" in packages emacs,org
- Bugs about \"something\" reported by someone@example.org
- Tagged bugs
- Bugs"
  (let* ((org (when (bound-and-true-p debbugs-org-mode) "Org "))
         (bugs (cdr (assq 'bugs query)))
	 (bug-count (length bugs))
	 (bugs-substring
	  (cond
	   ((eq bug-count 0) nil)
	   ((eq bug-count 1) (concat org "Bug #" (int-to-string (car bugs))))
	   ((concat org "Bugs "
                    (string-join
                     (mapcar (lambda (elt) (concat "#" (int-to-string elt)))
                             bugs)
                     ", "))))))
    (if bugs-substring
        bugs-substring
      (let* ((org (when (bound-and-true-p debbugs-org-mode) "Org"))
             (packages (mapcar 'cdr
			       (seq-filter
			        (lambda (elt) (eq (car elt) 'package))
			        query)))
	     (package-count (length packages))
             (packages-token
              (cond
               ((eq package-count 0) nil)
	       ((eq package-count 1) (concat "in " (car packages) " package"))
	       (t (concat "in packages " (string-join packages ",")))))
             (severity (cdr (assq 'severity query)))
             (first-token (if (equal severity "tagged") "Tagged bugs" "Bugs"))
             (subject (cdr (assq 'subject query)))
             (phrase (cdr (assq 'phrase query)))
	     (phrase-token
	      (when phrase
                (if subject
                    (concat "with subject \"" subject "\"")
                  (concat "about \"" phrase "\""))))
             (submitter (cdr (assq 'submitter query)))
             (submitter-token
              (when submitter (concat "reported by " submitter))))
        (string-join (append (seq-filter
                              (lambda (x) x)
                              (list org first-token phrase-token
                                    submitter-token packages-token)))
                     " ")))))

;;;###autoload
(defun debbugs-gnu-bookmark-make-record ()
  "Make record used to bookmark a Debbugs buffer.
This implements the `bookmark-make-record-function' type for
such buffers."
  (let ((bookmark-name (debbugs-gnu-bookmark-name debbugs-gnu-local-query)))
    `(,bookmark-name
      ,@(bookmark-make-record-default 'no-file)
      (filename . nil)
      (handler . debbugs-gnu-bookmark-jump)
      (debbugs-gnu-current-filter . ,debbugs-gnu-local-filter)
      (debbugs-gnu-current-query . ,debbugs-gnu-local-query)
      (debbugs-gnu-current-suppress . ,debbugs-gnu-local-suppress)
      (debbugs-gnu-current-message . ,debbugs-gnu-local-message)
      (debbugs-gnu-show-reports-function
       . ,(if (eq major-mode 'debbugs-gnu-mode)
              #'debbugs-gnu-show-reports #'debbugs-org-show-reports)))))

(put 'debbugs-gnu-bookmark-jump 'bookmark-handler-type "Debbugs")

;;;###autoload
(defun debbugs-gnu-bookmark-jump (bmk)
  "Provide the `bookmark-jump' behavior for a Debbugs buffer.
This implements the `handler' function interface for the record
type returned by `debbugs-gnu-bookmark-make-record'."
  (let* ((debbugs-gnu-current-filter
          (bookmark-prop-get bmk 'debbugs-gnu-current-filter))
	 (debbugs-gnu-current-query
          (bookmark-prop-get bmk 'debbugs-gnu-current-query))
	 (debbugs-gnu-current-suppress
          (bookmark-prop-get bmk 'debbugs-gnu-current-suppress))
         (buf (progn ;; Don't use save-window-excursion (bug#39722)
	        (setq debbugs-gnu-current-message
                      (bookmark-prop-get bmk 'debbugs-gnu-current-message)
                      debbugs-gnu-show-reports-function
                      (bookmark-prop-get
                       bmk 'debbugs-gnu-show-reports-function))
		(debbugs-gnu nil)
                debbugs-gnu-current-buffer)))
    (bookmark-default-handler
     `("" (buffer . ,buf) . ,(bookmark-get-bookmark-record bmk)))))

(provide 'debbugs-bookmarks)
;;; debbugs-bookmarks.el ends here

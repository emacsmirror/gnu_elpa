;;; hugoista.el --- Manage Hugo posts like a barista -*- lexical-binding: t -*-

;; Copyright (C) 2025  Free Software Foundation, Inc.

;; Author: Alexander Adolf <alexander.adolf@condition-alpha.com>
;; Maintainer: Alexander Adolf <alexander.adolf@condition-alpha.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.3") seq)
;; Homepage: https://codeberg.org/c-alpha/hugoista

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
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

;; This package is intended to help in curating and authoring a set of
;; blog post files for a Hugo-generated website.

;; Hugo (https://gohugo.io) is an open-source static site generator.
;; It uses text input files (for example Markdown, Org, AsciiDoc,
;; Pandoc, or reStructuredText) to generate static web pages.  Unlike
;; dynamic web content, static pages do not change their content based
;; on the HTTP request.  Hugo processes a set of such input files in
;; one file-system tree into a corresponding set of HTML files in
;; another file system tree.  These HTML files can then be uploaded to
;; a web server as a website's content.

;; The hugoista package displays the set of those text input files in
;; an input file-system tree, which will be processed into blog posts
;; by Hugo.  It shows them in a list view, grouped by their Hugo
;; publication status (draft, scheduled, published, and expired) along
;; with further Hugo metadata associated with each blog post.  Within
;; each status group, entries can be sorted by date, publication date,
;; expiration date, or title.  The text input file associated with
;; each entry can be visited, and a new text input file for a new blog
;; post can be created using single-key shortcuts.

;; To show such a list of text input files, call the function
;; `hugoista'.  It accepts an optional argument, which is the root
;; directory of a Hugo input file-system tree (that is, the directory
;; where the `hugo.toml' file for the website is).  Each hugoista
;; buffer references it own website input directory.  It is thus
;; possible to have several hugoista buffers open at the same time,
;; each for a different website input directory.  As a convenience for
;; when a single website is to be managed only, the optional argument
;; can be omitted, in which case the directory indicated by
;; `hugoista-site-dir' will be used as the default website input
;; directory.

;; Two operations are provided to act on individual entries.
;; `hugoista-visit-post' (bound to RET by default), and
;; `hugoista-new-post' (bound to N and + by default).  Further
;; operations (for example renaming, or deleting text input files) can
;; conveniently be performed by other built-in facilities, such as for
;; example `dired'.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'tabulated-list)

;;;; Customisation Variables

(defgroup hugoista nil
  "Managing Hugo posts."
  :group 'external)

(defcustom hugoista-hugo-command "hugo"
  "Command to use the Hugo command line interface."
  :type 'string)

(defcustom hugoista-site-dir "~/"
  "Default site directory to be used by `hugoista'."
  :type 'directory
  :local t)

(defcustom hugoista-posts-dir "posts"
  "Hugo content directory containing the blog posts."
  :type 'string
  :local t)

(defcustom hugoista-initial-sort t
  "Sort order for newly created `hugoista' buffers."
  :type '(choice (const :tag "descending" t)
                 (const :tag "ascending" nil)))

;;;; Internal Stuff

(cl-defstruct post-desc
  "Structure containing information about an individual post."
  path slug title date expiry-date publish-date draft permalink kind section)

(defun hugoista--in-quotes-p (pos line)
  "Return t if POS in LINE is within quotes, nil otherwise."
  (let ((quote-count 0)
        (i 0))
    (while (< i pos)
      (when (= (aref line i) ?\")
        (setq quote-count (1+ quote-count)))
      (setq i (1+ i)))
    (= (mod quote-count 2) 1)))

(defun hugoista--next-comma (line start)
  "Find next legitimate comma from START in LINE."
  (let ((pos start))
    (while (and (setq pos (string-match "," line pos))
                (hugoista--in-quotes-p pos line))
      (setq pos (1+ pos)))
    pos))

(defun hugoista--parse-csv-to-list (line)
  "Parse LINE as CSV.

Handles quoted fields with embedded commas and escaped quotes."
  (let ((fields nil)
        (start 0)
        (end (length line)))
    (while (< start end)
      (let* ((next-comma (hugoista--next-comma line start))
             (field-end (or next-comma end))
             (field (substring line start field-end))
             (cleaned-field
              (if (string-match "\\`\"\\(.*\\)\"\\'" field)
                  (replace-regexp-in-string "\"\"" "\""
                    (match-string 1 field))
                field)))
        (push cleaned-field fields)
        (setq start (if next-comma (1+ field-end) end))))
    (nreverse fields)))

(defun hugoista--list-content (&optional restrict)
  "Obtain a list of site content from Hugo.

The set of content returned can optionally be constrained via RESTRICT,
which is a cons of the form (KIND . SECTION).  Here, KIND is a Hugo page
kind, and Section is a Hugo section.  Both are strings.  When RESTRICT
is supplied, only content with matching KIND and SECTION values is
returned.  Setting either of the two to nil, or an empty string acts as
a wildcard matching all values of the respective attribute.

This function calls `hugo list' to obtain a list of site content, and
parses the Hugo output into a grouped list containing `post-desc'
structures, which is returned.  It is a list of the form:
\((drafts (POST ...)) (future (POST ...))
 (published (POST ...)) (expired (POST ...)))
The car of each element is a symbol representing one of the possible
publication states of a Hugo content element.  The cdr is a list of
`post-desc' structs, or nil when there are no matching elements with the
respective publication state."
  (let ((selector-list '(drafts future published expired))
        (filter-kind (or (car restrict) ""))
        (filter-section (or (cdr restrict) ""))
        result)
    (dolist (sel selector-list)
      (let (bol eol members)
        (with-temp-buffer
          (call-process hugoista-hugo-command nil t nil
                        ;; command line args
                        "list" (symbol-name sel))
          (goto-char (point-min))
          ;; skip header line
          (forward-line 1)
          ;; parse all subsequent lines
          (setq bol (pos-bol)
                eol (pos-eol))
          (while (not (= bol eol))
            (seq-let (path slug title date expiry-date publish-date draft
                           permalink kind section)
                (hugoista--parse-csv-to-list (buffer-substring-no-properties bol eol))
              ;; Drafts with a future release date appear in both,
              ;; `hugo list drafts', and `hugo list future'. To avoid
              ;; double appearance in the hugoista buffer, show such
              ;; entries under drafts only.
              (if (or (not (eq sel 'future))
                      (not (string= draft "true")))
                  ;; filter entries based on criteria in RESTRICT;
                  ;; empty value matches all
                  (when (or (and (string-empty-p filter-kind)
                                 (string-empty-p filter-section))
                            (and (string= filter-kind kind)
                                 (string= filter-section section))
                            (and (not (string-empty-p filter-kind))
                                 (string-empty-p filter-section)
                                 (string= filter-kind kind))
                            (and (string-empty-p filter-kind)
                                 (not (string-empty-p filter-section))
                                 (string= filter-section section)))
                    (setq members (push (make-post-desc :path path
                                                        :slug slug
                                                        :title title
                                                        :date date
                                                        :expiry-date expiry-date
                                                        :publish-date publish-date
                                                        :draft draft
                                                        :permalink permalink
                                                        :kind kind
                                                        :section section)
                                        members)))))
            (forward-line 1)
            (setq bol (pos-bol)
                  eol (pos-eol)))
          (setq members (nreverse members)
                result (push (list sel members) result)))))
    (nreverse result)))

(defun hugoista--list-posts ()
  "Obtain a list of post pages from Hugo."
  (hugoista--list-content (cons "page" hugoista-posts-dir)))

(defun hugoista--tabulated-list-groups (content-groups)
  "Produce a structure suitable to set to the variable `tabulated-list-groups'.

CONTENT-GROUPS is a grouped list in the format produced by
`hugoista--list-content'."
  (mapcar (lambda (group)
            (cons (upcase (symbol-name (car group)))
                  (mapcar (lambda (member)
                            (list member
                                  (vector (substring-no-properties
                                           (post-desc-date member) 0 10)
                                          (if (string= (post-desc-date member)
                                                       (post-desc-publish-date member))
                                              ""
                                            (substring-no-properties
                                             (post-desc-publish-date member) 0 10))
                                          (if (string= "0001-01-01T00:00:00Z"
                                                       (post-desc-expiry-date member))
                                              ""
                                            (substring-no-properties
                                             (post-desc-expiry-date member) 0 10))
                                          (post-desc-title member))))
                          (cadr group))))
          content-groups))

(defun hugoista--content-dir ()
  "Query Hugo for the top-level content directory of the site."
  (with-temp-buffer
    (call-process hugoista-hugo-command nil t nil
                  ;; command line args
                  "config")
    (goto-char (point-min))
    (when (re-search-forward "^contentdir[ \t]*=[ \t]*'\\([^']+\\)'$" nil t)
      (match-string-no-properties 1))))

(defun hugoista--hugo-site-dir-p (dir)
  "Test whether DIR is a Hugo site directory."
  (> (length (directory-files dir nil
                              (rx bos
                                  (or "hugo" "config")
                                  "."
                                  (or "toml" (seq "y" (opt "a") "ml"))
                                  eos)
                              t))
     0))

;;;; Interactive Functions

(defun hugoista-reload (&optional _ignore-auto _noconfirm)
  "Refresh the contents of a `hugoista-mode' buffer."
  (interactive)
  (setq tabulated-list-groups
        (hugoista--tabulated-list-groups
         (hugoista--list-posts)))
  (tabulated-list-print t t))

(defun hugoista-visit-post ()
  "Visit the file containing the Hugo post in the current row."
  (interactive)
  (find-file (expand-file-name (post-desc-path (tabulated-list-get-id))
                               hugoista-site-dir)))

(defun hugoista-new-post ()
  "Create a new Hugo post file, querying for the file name."
  (interactive)
  (let* ((content-dir (expand-file-name (hugoista--content-dir)
                                        hugoista-site-dir))
         (posts-dir (file-name-as-directory (expand-file-name hugoista-posts-dir
                                                              content-dir)))
         (new-file (read-file-name "New post file " posts-dir))
         (new-post (file-relative-name new-file content-dir)))
    (with-temp-buffer
      (call-process hugoista-hugo-command nil t nil
                    ;; command line args
                    "new" "content" new-post))
    (hugoista-reload)))

;;;; Tabulated List Derived Major Mode, and Entry Point Listing Function

(defun hugoista--date-less-p (e1 e2)
  "Sorting predicate for the date column in `hugoista-mode'.

E1 and E2 are `post-desc' structs."
  (let ((t1 (date-to-time (post-desc-date (car e1))))
        (t2 (date-to-time (post-desc-date (car e2)))))
    (time-less-p t1 t2)))

(defun hugoista--publish-date-less-p (e1 e2)
  "Sorting predicate for the publication date column in `hugoista-mode'.

E1 and E2 are `post-desc' structs."
  (let ((t1 (date-to-time (post-desc-publish-date (car e1))))
        (t2 (date-to-time (post-desc-publish-date (car e2)))))
    (time-less-p t1 t2)))

(defun hugoista--expiry-date-less-p (e1 e2)
  "Sorting predicate for the expiration date column in `hugoista-mode'.

E1 and E2 are `post-desc' structs."
  (let ((t1 (date-to-time (post-desc-expiry-date (car e1))))
        (t2 (date-to-time (post-desc-expiry-date (car e2)))))
    (time-less-p t1 t2)))

(defvar-keymap hugoista-mode-map
  :doc "Keymap for buffers created by `hugoista'."
  :parent tabulated-list-mode-map
  "RET" #'hugoista-visit-post
  "N"   #'hugoista-new-post
  "+"   #'hugoista-new-post)

(define-derived-mode hugoista-mode tabulated-list-mode "Hugoista"
  "Major mode for buffers made by \\[hugoista]."
  :interactive nil
  (setq tabulated-list-format `[("Posted"   12 ,#'hugoista--date-less-p)
                                ("Releases" 12 ,#'hugoista--publish-date-less-p)
                                ("Expires"  12 ,#'hugoista--expiry-date-less-p)
                                ("Title"     0 t)])
  (setq tabulated-list-padding 3)
  (setq tabulated-list-sort-key '("Posted" . hugoista-initial-sort))
  (tabulated-list-init-header))

;;;###autoload
(defun hugoista (&optional dir)
  "Manage posts on a Hugo-powered site like a barista.

Presents all posts of the Hugo site rooted at DIR (defaulting to
`hugoista-site-dir') in a tabular overview, categorised by post status.
Posts can be acted upon using the following keys:

\\<hugoista-mode-map>
- \\[hugoista-visit-post] visits the file containing the post

- \\[hugoista-new-post] creates a new post, querying for a file name

\\<tabulated-list-mode-map>
The list buffer uses major mode `hugoista-mode', which is derived from
`tabulated-list-mode'.  Thus, all the usual key bindings for tabulated
lists are available in `hugoista-mode-map', such as for instance
\\[revert-buffer] for updating the table contents after one or more of the files have
been modified."
  (interactive)
  (let ((buffer-dir (file-name-as-directory (or dir hugoista-site-dir))))
    (if (not (hugoista--hugo-site-dir-p buffer-dir))
        (message "No Hugo site found at \"%s\"" buffer-dir)
      (if (not (executable-find hugoista-hugo-command))
          (message "Hugo executable \"%s\" not found" hugoista-hugo-command)
        (let ((buf (get-buffer-create
                    (format "*%s*" (abbreviate-file-name buffer-dir)))))
          (set-buffer buf)
          (hugoista-mode)
          (setq hugoista-site-dir buffer-dir
                default-directory buffer-dir)
          (setq-local revert-buffer-function #'hugoista-reload)
          (revert-buffer)
          (switch-to-buffer buf))))))

(provide 'hugoista)
;;; hugoista.el ends here

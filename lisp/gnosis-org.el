;;; gnosis-org.el --- Org-mode parsing for gnosis nodes  -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026  Free Software Foundation, Inc.

;; Author: Thanos Apollo <public@thanosapollo.org>
;; Keywords: extensions

;;; Commentary:

;; Pure org-file parsing functions for gnosis.  No DB dependency.
;; These functions parse org buffers to extract node data, links, and tags.

;;; Code:

(require 'cl-lib)
(require 'org-element)
(require 'subr-x)

(defun gnosis-org-adjust-title (input)
  "Strip org link markup from INPUT, keeping only link descriptions.
Converts [[id:xxx][Description]] to Description."
  (replace-regexp-in-string
   "\\[\\[id:[^]]+\\]\\[\\(.*?\\)\\]\\]"
   "\\1"
   input))

(defun gnosis-org-get-id ()
  "Return the nearest enclosing ID, ignoring narrowing.
Preserve point and restriction; return nil when no heading or root owns an ID."
  (org-with-wide-buffer
   (let ((id (org-id-get)))
     (while (and (not id) (org-up-heading-safe))
       (setq id (org-id-get)))
     (or id
         (progn
           (goto-char (point-min))
           (when (org-before-first-heading-p) (org-id-get)))))))

(defun gnosis-org-collect-id-links (&optional parsed-data)
  "Collect ID links as (target-id . source-id) pairs from PARSED-DATA.
Parse the current buffer when PARSED-DATA is nil.  Each source is the
nearest enclosing ID at the link's beginning."
  (save-excursion
    (org-element-map (or parsed-data (org-element-parse-buffer)) 'link
      (lambda (link)
        (when (equal (org-element-property :type link) "id")
          (goto-char (org-element-property :begin link))
          (when-let* ((source-id (gnosis-org-get-id)))
            (cons (org-element-property :path link) source-id)))))))

(defun gnosis-org--filetag-keywords (&optional parsed-data)
  "Return native FILETAGS keyword elements from PARSED-DATA or the buffer."
  (org-element-map (or parsed-data (org-element-parse-buffer)) 'keyword
    (lambda (keyword)
      (when (equal (org-element-property :key keyword) "FILETAGS")
        keyword))))

(defun gnosis-org-get-filetags (&optional parsed-data)
  "Return unique file tags from all FILETAGS keywords in PARSED-DATA.
Parse the buffer when PARSED-DATA is nil.  Literal block contents are not
keywords.  As in Org, whitespace and colons separate tags."
  (delete-dups
   (mapcan (lambda (keyword)
             (split-string (org-element-property :value keyword)
                           "[ \t\n\r:]+" t))
           (gnosis-org--filetag-keywords parsed-data))))

(defun gnosis-org-get-data--topic (&optional parsed-data)
  "Retrieve the title and ID from the current org buffer or given PARSED-DATA.
Returns (title tags id).  ID will be nil if no file-level ID exists."
  (unless parsed-data
    (setq parsed-data (org-element-parse-buffer)))
  (let* ((id (org-element-map parsed-data 'property-drawer
               (lambda (drawer)
                 (let ((parent (org-element-property :parent drawer)))
                   (when (and parent
                              (eq (org-element-type parent) 'section)
                              (let ((section-parent
                                     (org-element-property
                                      :parent parent)))
                                (eq (org-element-type
                                     section-parent)
                                    'org-data)))
                     (org-element-map
                         (org-element-contents drawer)
                         'node-property
                       (lambda (prop)
                         (when (string= (org-element-property :key prop) "ID")
                           (org-element-property :value prop)))
                       nil t))))
               nil t))
	 (title-raw (org-element-map parsed-data 'keyword
                      (lambda (kw)
                        (when (string= (org-element-property :key kw) "TITLE")
                          (org-element-property :value kw)))
                      nil t))
	 (title (when title-raw (gnosis-org-adjust-title title-raw)))
	 (tags (gnosis-org-get-filetags parsed-data)))
    (unless (and title (not (string-empty-p title)))
      (error "Org buffer must have a non-empty TITLE"))
    (list title tags id)))

(defun gnosis-org--combine-tags (inherited-tags headline-tags)
  "Combine INHERITED-TAGS and HEADLINE-TAGS without changing either list."
  (delete-dups (append inherited-tags headline-tags nil)))

(defun gnosis-org--parse-headlines-recursive
    (element parent-id parent-title parent-tags)
  "Recursively parse headlines from ELEMENT.
ELEMENT can be the parsed-data (org-data) or a headline element.
PARENT-ID is the ID of nearest ancestor with ID (or 0).
PARENT-TITLE is the hierarchical title path (only from ancestors with IDs).
PARENT-TAGS are the inherited tags from ancestors."
  ;; Each callback returns a fresh result spine, never the input Org tree.
  (apply #'nconc
         (org-element-map (org-element-contents element) 'headline
           (lambda (headline)
             (let* ((current-id (org-element-property :ID headline))
                    (title (org-element-property :raw-value headline))
                    (level (org-element-property :level headline))
                    (headline-tags (org-element-property :tags headline))
                    (combined-tags (gnosis-org--combine-tags
                                    parent-tags headline-tags)))
               (if current-id
                   (let* ((clean-title (gnosis-org-adjust-title
                                        (string-trim title)))
                          (full-title (if parent-title
                                          (concat parent-title ":" clean-title)
                                        clean-title))
                          (entry (list :id current-id
                                       :title full-title
                                       :tags combined-tags
                                       :master (or parent-id 0)
                                       :level level)))
                     (cons entry (gnosis-org--parse-headlines-recursive
                                  headline current-id full-title combined-tags)))
                 (gnosis-org--parse-headlines-recursive
                  headline parent-id parent-title combined-tags))))
           nil nil 'headline)))

(defun gnosis-org-buffer-data (&optional data)
  "Parse DATA in current buffer for topics & headlines.
Extracts their ID, tags, and links."
  (let* ((parsed-data (or data (org-element-parse-buffer)))
         (topic-info (gnosis-org-get-data--topic parsed-data))
         (topic-title (nth 0 topic-info))
         (topic-tags (nth 1 topic-info))
         (topic-id (nth 2 topic-info))
         (headlines (gnosis-org--parse-headlines-recursive
                     parsed-data
                     topic-id
                     (when topic-id topic-title)
                     topic-tags)))
    (if topic-id
        (cons (list :title topic-title
                    :id topic-id
                    :tags topic-tags
                    :master 0
                    :level 0)
              headlines)
      headlines)))

(defun gnosis-org-matching-node-ids (query &optional node-ids)
  "Return node IDs whose own Org content matches QUERY.
When NODE-IDS is non-nil, return only IDs in that list."
  (unless (derived-mode-p 'org-mode)
    (org-mode))
  (let (matches)
    (save-excursion
      (goto-char (point-min))
      (while (search-forward query nil t)
        (let ((id (save-excursion
                    (goto-char (match-beginning 0))
                    (gnosis-org-get-id))))
          (when (and id (or (null node-ids) (member id node-ids))
                     (not (member id matches)))
            (push id matches)))))
    (nreverse matches)))

(defun gnosis-org-get-buffer-info ()
  "Parse current buffer for node data, links, and content hash.
The buffer must already be in `org-mode' or contain raw org text.
Returns file data whose last two elements are the link list and
the SHA1 hash of the buffer content."
  (let ((hash (secure-hash 'sha1 (current-buffer))))
    (unless (derived-mode-p 'org-mode)
      (org-mode))
    (org-set-regexps-and-options 'tags-only)
    (let* ((parsed-data (org-element-parse-buffer))
           (data (gnosis-org-buffer-data parsed-data))
           (links (gnosis-org-collect-id-links parsed-data)))
      (append data (list links hash)))))

(defun gnosis-org-get-file-info (filename)
  "Get data for FILENAME.
Returns file data with FILENAME.  The last two elements are
the link list and the SHA1 hash of the file content."
  (with-temp-buffer
    (insert-file-contents filename)
    (gnosis-org-get-buffer-info)))

(defun gnosis-org--file-hash (file)
  "Compute SHA1 hash of FILE content."
  (with-temp-buffer
    (insert-file-contents file)
    (secure-hash 'sha1 (current-buffer))))

(defun gnosis-org--create-name
    (title &optional timestring gpg-p
           default-gpg default-timestring)
  "Create filename for TITLE.
TIMESTRING defaults to DEFAULT-TIMESTRING.  When both are nil,
the filename is just the title without a time prefix.
GPG-P: when non-nil, add .gpg suffix (overrides DEFAULT-GPG)."
  (let ((timestring (or timestring default-timestring))
	(filename (replace-regexp-in-string "#" ""
					    (replace-regexp-in-string " " "_" title)))
	(use-gpg (if (eq gpg-p 'default)
		     default-gpg
		   (or gpg-p default-gpg))))
    (format "%s.org%s"
	    (if timestring
		(format "%s--%s" (format-time-string timestring) filename)
	      filename)
	    (if use-gpg ".gpg" ""))))

(defun gnosis-org-expand-headings (text &optional base-level)
  "Expand heading markers in TEXT relative to BASE-LEVEL.
`{*}' becomes BASE-LEVEL stars, `{**}' becomes BASE-LEVEL+1, etc.
The number of `*' inside braces determines the offset from BASE-LEVEL.
When BASE-LEVEL is nil, auto-detect from current `org-current-level'."
  (let ((base-level (or base-level (1+ (or (org-current-level) 0)))))
    (replace-regexp-in-string
     "{\\(\\*+\\)}"
     (lambda (match)
       (let ((offset (1- (length (match-string 1 match)))))
	 (make-string (+ base-level offset) ?*)))
     text t)))

(provide 'gnosis-org)
;;; gnosis-org.el ends here

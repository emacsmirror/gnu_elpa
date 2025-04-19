;;; denote-markdown.el --- Extensions that better integrate Denote with Markdown -*- lexical-binding: t -*-

;; Copyright (C) 2024-2025  Free Software Foundation, Inc.

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; Maintainer: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://github.com/protesilaos/denote-markdown
;; Version: 0.1.1
;; Package-Requires: ((emacs "28.1") (denote "4.0.0"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Optional extensions for Denote that work specifically with Markdown files.

;;; Code:

(require 'denote)

(defgroup denote-markdown ()
  "Optional extensions for Denote that work specifically with Markdown files."
  :group 'denote
  :link '(info-link "(denote) top")
  :link '(info-link "(denote-markdown) top")
  :link '(url-link :tag "Denote homepage" "https://protesilaos.com/emacs/denote")
  :link '(url-link :tag "Denote Markdown homepage" "https://protesilaos.com/emacs/denote-markdown"))

;;;; Register a new file type

(add-to-list
 'denote-file-types
 '(markdown-obsidian
   :extension ".md"
   :front-matter "# %s\n\n"
   :title-key-regexp "^# "
   :title-value-function identity
   :title-value-reverse-function identity
   :link denote-md-link-format
   :link-in-context-regexp denote-md-link-in-context-regexp))

;;;; Convert links

(defun denote-markdown--get-regexp (type)
  "Return regular expression to match link TYPE.
TYPE is a symbol among `denote', `file', `obsidian', and `reverse-obsidian'."
  (pcase type
    ('denote "(denote:\\(?1:.*?\\))")
    ('file (format "(.*?\\(?1:%s\\).*?)" denote-id-regexp))
    ('obsidian "\\(?2:\\[.*?\\]\\)(denote:\\(?1:.*?\\))")
    ('reverse-obsidian (format "\\(?2:\\[.*?\\(?:%s\\).*?\\]\\)(\\(?1:.*?\\(?:%s\\).*?\\))" denote-id-regexp denote-id-regexp))
    (_ (error "`%s' is an unknown type of link" type))))

;;;###autoload
(defun denote-markdown-convert-links-to-file-paths (&optional absolute)
  "Convert denote: links to file paths.
Ignore all other link types.  Also ignore links that do not
resolve to a file in the variable `denote-directory'.

With optional ABSOLUTE, format paths as absolute, otherwise do them
relative to the variable `denote-directory'."
  (interactive "P" markdown-mode)
  (if (derived-mode-p 'markdown-mode)
      (save-excursion
        (let ((count 0))
          (goto-char (point-min))
          (while (re-search-forward (denote-markdown--get-regexp 'denote) nil :no-error)
            (when-let* ((id (match-string-no-properties 1))
                        (file (save-match-data
                                (if absolute
                                    (denote-get-path-by-id id)
                                  (denote-get-relative-path-by-id id)))))
              (replace-match (format "(%s)" file) :fixed-case :literal)
              (setq count (1+ count))))
          (message "Converted %d `denote:' links to %s paths" count (if absolute "ABSOLUTE" "RELATIVE"))))
    (user-error "The current file is not using Markdown mode")))

;;;###autoload
(defun denote-markdown-convert-links-to-denote-type ()
  "Convert generic file links to denote: links in the current Markdown buffer.
Ignore all other link types.  Also ignore file links that do not point
to a file with a Denote file name.

Also see `denote-markdown-convert-obsidian-links-to-denote-type'."
  (interactive nil markdown-mode)
  (if (derived-mode-p 'markdown-mode)
      (save-excursion
        (let ((count 0))
          (goto-char (point-min))
          (while (re-search-forward (denote-markdown--get-regexp 'file) nil :no-error)
            (let* ((file (match-string-no-properties 1))
                   (id (save-match-data (denote-retrieve-filename-identifier file))))
              (when id
                (replace-match (format "(denote:%s)" id) :fixed-case :literal)
                (setq count (1+ count)))))
          (message "Converted %d file links to `denote:' links" count)))
    (user-error "The current file is not using Markdown mode")))

;;;###autoload
(defun denote-markdown-convert-links-to-obsidian-type ()
  "Convert denote: links to Obsidian-style file paths.
Ignore all other link types.  Also ignore links that do not
resolve to a file in the variable `denote-directory'."
  (interactive nil markdown-mode)
  (if (derived-mode-p 'markdown-mode)
      (save-excursion
        (let ((count 0))
          (goto-char (point-min))
          (while (re-search-forward (denote-markdown--get-regexp 'obsidian) nil :no-error)
            (when-let* ((id (match-string-no-properties 1))
                        (path (save-match-data (denote-get-relative-path-by-id id)))
                        (name (file-name-sans-extension path)))
              (replace-match (format "[%s](%s)" name path) :fixed-case :literal)
              (setq count (1+ count))))
          (message "Converted %d `denote:' links to Obsidian-style format" count)))
    (user-error "The current file is not using Markdown mode")))

;;;###autoload
(defun denote-markdown-convert-obsidian-links-to-denote-type ()
  "Convert Obsidian-style links to denote: links in the current Markdown buffer.
Ignore all other link types.  Also ignore file links that do not point
to a file with a Denote file name.

Also see `denote-markdown-convert-links-to-denote-type'."
  (interactive nil markdown-mode)
  (if (derived-mode-p 'markdown-mode)
      (save-excursion
        (let ((count 0))
          (goto-char (point-min))
          (while (re-search-forward (denote-markdown--get-regexp 'reverse-obsidian) nil :no-error)
            (let ((file nil)
                  (id nil)
                  (description nil))
              (save-match-data
                (setq file (expand-file-name (match-string-no-properties 1) (denote-directory))
                      id (denote-retrieve-filename-identifier file)
                      description (denote-get-link-description file)))
              (when id
                (replace-match (format "[%s](denote:%s)" description id) :fixed-case :literal)
                (setq count (1+ count)))))
          (message "Converted %d Obsidian-style links to `denote:' links" count)))
    (user-error "The current file is not using Markdown mode")))

(provide 'denote-markdown)
;;; denote-markdown.el ends here

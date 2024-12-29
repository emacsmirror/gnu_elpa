;;; denote-search.el --- Search the contents of your notes -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Lucas Quintana

;; Author: Lucas Quintana <lmq10@protonmail.com>
;; Maintainer: Lucas Quintana <lmq10@protonmail.com>
;; URL: https://github.com/lmq-10/denote-search
;; Created: 2024-12-28
;; Keywords: matching
;; Version: 0.2.0
;; Package-Requires: ((emacs "29.1") (denote "3.0"))

;; This program is NOT part of GNU Emacs.

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

;; This package provides a search utility for Denote, the
;; simple-to-use, focused-in-scope, and effective note-taking tool for
;; Emacs.
;;
;; The command `denote-search' is the main point of entry.  It accepts
;; a query, which should be a regular expression, and then searches
;; the contents of all the notes stored in `denote-directory' for it.
;; The results are put in a buffer which allows folding and further
;; filtering; all standard commands offered by Xref are available as
;; well.
;;
;; This package has the same code principles as Denote: to be
;; simple-to-use, focused-in-scope, and effective.  We build upon Xref
;; to be good Emacs citizens, and don't use any dependencies other
;; than Denote and built-in libraries.

;;; Code:

(require 'denote)
(require 'outline)
(require 'time-date)
(require 'xref)

;;;; User options:

(defgroup denote-search ()
  "A simple search utility for Denote."
  :group 'matching
  :link '(info-link "(denote-search) Top"))

(defcustom denote-search-buffer-name "*denote-search*"
  "Name of the buffer created by `denote-search'."
  :group 'denote-search
  :type 'string)

(defcustom denote-search-format-heading-function #'denote-search-extract-title
  "Function used to construct headings in the `denote-search' buffer.

It is called with a single argument, the path to the note file, and it
should always return a string."
  :type 'function)

(defcustom denote-search-untitled-string "[Untitled]"
  "String to use as heading for untitled notes."
  :type 'string)

(defcustom denote-search-help-string "\\<denote-search-mode-map>Refine with \
`\\[denote-search-refine]', exclude files with \
`\\[denote-search-exclude-files]', (only) include certain files \
with `\\[denote-search-only-include-files]'."
  "Help string appended to the header line of `denote-search' buffer.

This gets processed by `substitute-command-keys', so it can contain key
descriptions which get replaced; check the documentation for details.

Once you are familiar with the program, you can safely set this variable
to the void string."
  :type 'string)

(defcustom denote-search-hook nil
  "Normal hook run after finishing a `denote-search'."
  :type 'hook)

;;;; Main variables:

(defvar denote-search-query-history nil
  "Minibuffer history for generic search commands.")

(defvar denote-search-file-regexp-history nil
  "Minibuffer history for commands asking for a file regexp.")

(defvar denote-search--last-files nil
  "Variable holding a list of files matched by the last call to `denote-search'.")

(defvar denote-search--last-query nil
  "Variable holding the QUERY argument of the last call to `denote-search'.")

;;;; Main functions:

(defun denote-search-extract-title (file)
  "Extract note title from FILE front matter.

When no title is found, return title found in FILE name.

When that doesn't work, return `denote-search-untitled-string'.

This is the default function used to format headings in the
`denote-search' buffer.  See `denote-format-heading-function'."
  (or
   (denote-retrieve-title-or-filename file (denote-filetype-heuristics file))
   denote-search-untitled-string))

(defun denote-search-file-regexp-prompt (&optional include)
  "Prompt for a file regexp in the minibuffer.

The prompt assumes the user wants to exclude files, unless INCLUDE is
non-nil."
  (list (read-string
         (if (not include)
             "Exclude file names matching: "
           "Only include file names matching: ")
         nil 'denote-search-file-regexp-history)))

(defun denote-search-query-prompt (&optional focused)
  "Prompt for a search query in the minibuffer.

The prompt assumes a search in all files, unless FOCUSED is non-nil."
  (list (read-string
         (if (not focused)
             "Search (all Denote files): "
           "Search (only files matched last): ")
         nil 'denote-search-query-history)))

(defun denote-search-format-heading-with-keywords (file)
  "Format a heading for FILE with its title and keywords.

Returned heading has the following format:

TITLE  [KEYWORD1, KEYWORD2]

This function is intended to be used as the
`denote-search-format-heading-function'."
  (let ((keywords (denote-retrieve-filename-keywords file))
        (title (denote-search-extract-title file)))
    (if (not keywords)
        title
      (format
       "%s  [%s]"
       title (string-join (string-split keywords "_") ", ")))))

(defun denote-search-set-header-line (query number-of-files time)
  "Set header line for `denote-search' buffer.

QUERY should be a string.  It is assumed to be the search term.

NUMBER-OF-FILES should be an integer.  It is assumed to be the number of
files matching the search.

TIME should be an Emacs timestamp as returned by e.g. `current-time'.
It is assumed to be the exact time when search started.

If `denote-search-help-string' is non-nil, it is appended to the header
line, and any key descriptions within it are replaced using
`substitute-command-keys'."
  (let ((help-string
         (if (or (not denote-search-help-string)
                 (string-blank-p denote-search-help-string))
             ""
           (concat "  " (substitute-command-keys denote-search-help-string)))))
    (setq-local
     header-line-format
     (format
      "Search for ‘%s’ finished in %s (%d files matching).%s"
      query
      (seconds-to-string (float-time (time-subtract (current-time) time)))
      number-of-files help-string))))

;;;###autoload
(defun denote-search (query &optional set)
  "Search QUERY in the content of Denote files.

QUERY should be a regular expression accepted by `xref-search-program',
which see.

The files to search for are those returned by `denote-directory-files'
with a non-nil TEXT-ONLY argument.  When calling the function from Lisp,
however, SET can be a list of files to search instead.  This is mostly
useful for filtering output; see e.g. `denote-search-refine'.

The results are populated in a buffer whose major mode is
`xref--xref-buffer-mode' and where `denote-search-mode-map' is active."
  ;; Some of this is based on `denote-link--prepare-backlinks'
  (interactive (denote-search-query-prompt))
  (let ((now (current-time))
        (inhibit-read-only t)
        (xref-file-name-display 'abs)
        (xref-alist
         (xref--analyze
          (xref-matches-in-files
           query
           (or set (denote-directory-files nil nil :text-only))))))
    (or xref-alist (user-error "No matches for `%s'" query))
    ;; Set internal variables for last set of files and last query
    (setq denote-search--last-files nil)
    (setq denote-search--last-query query)
    (mapc
     (lambda (x)
       (let* ((file-xref (car x))
              (file
               ;; NOTE: Unfortunately, the car of the xref construct is
               ;; not reliable; sometimes it's absolute, sometimes it
               ;; is not
               (if (file-name-absolute-p file-xref)
                   file-xref
                 (xref-location-group
                  (xref-match-item-location (car (last x)))))))
         ;; Add to current set of files
         (push file denote-search--last-files)
         ;; Format heading
         (setf (car x) (funcall denote-search-format-heading-function file))))
     xref-alist)
    (delete-dups denote-search--last-files)
    (with-current-buffer (get-buffer-create denote-search-buffer-name)
      (erase-buffer)
      (xref--insert-xrefs xref-alist)
      (xref--xref-buffer-mode)
      (denote-search-mode)
      (denote-search-set-header-line query (length xref-alist) now)
      (setq-local revert-buffer-function
                  (lambda (_ignore-auto _noconfirm)
                    (denote-search
                     denote-search--last-query
                     denote-search--last-files)))
      (goto-char (point-min)))
    (pop-to-buffer-same-window denote-search-buffer-name)
    (run-hooks 'denote-search-hook)))

(defun denote-search-refine (query)
  "Search QUERY in the content of files which matched the last `denote-search'.

QUERY should be regular expression.

A typical case is to search notes which have two specific terms anywhere
within them, such as \"emacs\" and \"philosophy\".  The user would then
call ‘\\<global-map>\\[denote-search] emacs RET’ in order to search for
emacs, and once the results are populated, they would type
‘\\<denote-search-mode-map>\\[denote-search-refine] philosophy RET’ to
search the term philosophy only in those notes.  This can be done
as many times as wished."
  (interactive (denote-search-query-prompt :focused))
  (denote-search query denote-search--last-files))

(defalias 'denote-search-focused-search 'denote-search-refine)

(defun denote-search-exclude-files (regexp)
  "Exclude files whose name matches REGEXP from current `denote-search' buffer.

This is useful even if you don't know regular expressions, given the
Denote file-naming scheme.  For instance, to exclude notes with the
keyword \"philosophy\" from current search buffer, type
‘\\<denote-search-mode-map>\\[denote-search-exclude-files] _philosophy
RET’.

Internally, this works by generating a new call to `denote-search' with
the same QUERY as the last one, but with a restricted SET gotten from
checking REGEXP against last matched files."
  (interactive (denote-search-file-regexp-prompt))
  (let (final-files)
    (dolist (file denote-search--last-files)
      (unless (string-match regexp file)
        (push file final-files)))
    (if final-files
        (denote-search denote-search--last-query final-files)
      (user-error "No remaining files when applying that filter"))))

(defun denote-search-only-include-files (regexp)
  "Exclude file names not matching REGEXP from current `denote-search' buffer.

See also `denote-search-exlude-files'."
  (interactive (denote-search-file-regexp-prompt :include))
  (let (final-files)
    (dolist (file denote-search--last-files)
      (when (string-match regexp file)
        (push file final-files)))
    (if final-files
        (denote-search denote-search--last-query final-files)
      (user-error "No remaining files when applying that filter"))))

;;;; Keymap and mode definition:

(defvar-keymap denote-search-mode-map
  :doc "Keymap for buffers generated by `denote-search'."
  "a" #'outline-cycle-buffer
  "f" #'denote-search-refine
  "k" #'outline-previous-heading
  "j" #'outline-next-heading
  "o" #'delete-other-windows
  "s" #'denote-search
  "v" #'outline-cycle
  "x" #'denote-search-exclude-files
  "i" #'denote-search-only-include-files)

(define-minor-mode denote-search-mode
  "Minor mode enabled in the buffer generated by `denote-search'.

It takes care of enabling `outline-minor-mode' and setting up the
relevant keymap (`denote-search-mode-map')."
  :interactive nil
  (when denote-search-mode
    (setq-local outline-minor-mode-use-buttons 'in-margins)
    (outline-minor-mode)))


(provide 'denote-search)
;;; denote-search.el ends here

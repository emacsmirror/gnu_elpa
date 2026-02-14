;;; vc-jj.el --- VC backend for the Jujutsu version control system -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025  Free Software Foundation, Inc.

;; Author: Wojciech Siewierski
;;         Rudolf Schlatte <rudi@constantly.at>
;;         Kristoffer Balintona <krisbalintona@gmail.com>
;; URL: https://codeberg.org/emacs-jj-vc/vc-jj.el
;; Version: 0.5
;; Package-Requires: ((emacs "28.1") (compat "29.4"))
;; Keywords: vc tools

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

;; This package provides a backend for vc.el to handle Jujutsu (jj)
;; repositories.  Jujutsu uses a change-centric model that differs in
;; important ways from Git’s commit-centric model.  Vc-jj adapts
;; vc.el’s abstractions to match jj terminology and behavior.
;;
;; Users may see and customize all options by pressing
;;
;;  'M-x customize-group vc-jj RET'
;;
;; and using the Customize menu.

;; STATUS AND LIMITATIONS
;;
;; Vc-jj implements most commonly used vc.el commands (such as
;; `vc-dir', `vc-diff', `vc-print-log', and file state queries), but
;; some operations are not yet supported or behave differently from
;; vc-git due to jj’s model.

;; FILE STRUCTURE
;;
;; After the "Customization" and "Internal Utilities" sections, the
;; organization of this file follows the "BACKEND PROPERTIES" section
;; of the preamble of the 'vc.el' file: each outline heading
;; corresponding to a vc backend method and the contents of each
;; heading relate to implementing that backend method.

;; FEEDBACK AND CONTRIBUTIONS
;;
;; Bug reports and contributions are welcome, especially for
;; unimplemented vc.el backend methods.  The home of this project is
;; found here: https://codeberg.org/emacs-jj-vc/vc-jj.el.  Users may
;; file bug reports in the "Issues" tab and create pull requests in
;; the "Pull requests" tab.
;;
;; Vc-jj prefers jujutsu's terminology and attempts to adhere to it,
;; so we ask contributors to try their best to use the terminology
;; specific to jujutsu as opposed to other version control systems,
;; such as Git.  A brief description of these differences can be found
;; above.

;;; Code:

(require 'compat)
(require 'cl-lib)
(require 'seq)
(require 'vc)
(require 'vc-git)
(require 'log-view)
(require 'log-edit)
(require 'ansi-color)
(require 'iso8601)
(require 'time-date)
(declare-function vc-annotate-convert-time "vc-annotate" (&optional time))

;;; Customization

(defgroup vc-jj nil
  "VC Jujutsu backend."
  :group 'vc)

(defcustom vc-jj-program "jj"
  "Name of the jj executable (excluding any arguments)."
  :type 'string
  :risky t)

(defcustom vc-jj-root-log-format
  (list
   ;; Log format (passed as the template for "jj log")
   "
if(root,
  format_root_commit(self),
  label(if(current_working_copy, \"working_copy\"),
    concat(
      separate(\" \",
        change_id ++ \"​\" ++ change_id.shortest(8).prefix() ++ \"​\" ++ change_id.shortest(8).rest(),
        if(author.name(), author.name(), if(author.email(), author.email().local(), email_placeholder)),
        commit_timestamp(self).format(\"%Y-%m-%d\"),
        bookmarks,
        tags,
        working_copies,
        if(git_head, label(\"git_head\", \"git_head()\")),
        format_short_commit_id(commit_id),
        if(conflict, label(\"conflict\", \"conflict\")),
        if(config(\"ui.show-cryptographic-signatures\").as_boolean(),
          format_short_cryptographic_signature(signature)),
        if(empty, label(\"empty\", \"(empty)\")),
        if(description,
          description.first_line(),
          label(if(empty, \"empty\"), description_placeholder),
        ),
      ) ++ \"\n\",
    ),
  )
)"
   ;; Log entry regexp
   (rx
    line-start
    ;; Graph
    (+? nonl) " "
    ;; Full change ID
    (group (+ (any "K-Zk-z")))
    space
    ;; Visible change ID
    (group (+ (any "K-Zk-z")))
    space
    (group (+ (any "K-Zk-z")))
    " "
    ;; Author
    (group (* nonl)) " "
    ;; Time
    (group (= 4 (any num)) "-" (= 2 (any num)) "-" (= 2 (any num)))
    ;; Tags and  bookmarks
    (group (*? nonl)) " "
    ;; Commit ID
    (group (+ (any hex))) " "
    ;; Special states
    (group (opt "conflict "))
    (group (opt "(empty) "))
    (group (opt "(no description set)"))
    ;; Description
    (* nonl) line-end)
   ;; Font lock keywords
   '((1 '(face nil invisible t))        ; Full change ID
     (2 'log-view-message)              ; Short change ID
     (3 'change-log-list)               ; Rest of Change ID
     (4 'change-log-name)               ; Author name
     (5 'change-log-date)               ; Date
     (6 'vc-jj-log-view-bookmark)       ; Bookmark names
     (7 'vc-jj-log-view-commit)         ; Commit ID
     (8 'vc-conflict-state)             ; Conflict marker
     (9 'change-log-function)           ; No description marker
     (10 'change-log-function)))        ; Revision description
  "JJ log format for `vc-print-root-log'.
This option determines the format and fontification of the JJ Log View
buffer created from `vc-print-root-log'.

It should be a list of the form (FORMAT REGEXP KEYWORDS), where FORMAT
is a format string (a JJ template passed to \"jj log\"), REGEXP is a
regular expression matching a single entry in the \"jj log\" output, and
KEYWORDS is a list of font lock keywords (see
`font-lock-keywords'and `(elisp) Search-based Fontification') for
highlighting the Log View buffer.

REGEXP may define capture groups that KEYWORDS can use to fontify
various regions of the Log View buffer."
  :type '(list string regexp (repeat sexp)))

(defcustom vc-jj-global-switches '("--no-pager" "--color" "never")
  "Global switches to pass to any jj command."
  :type '(choice (const :tag "None" nil)
         (string :tag "Argument String")
         (repeat :tag "Argument List" :value ("") string)))

(defcustom vc-jj-annotate-switches nil
  "String or list of strings specifying switches for \"jj file annotate\".
If nil, use the value of `vc-annotate-switches'.  If t, use no switches."
  :type '(choice (const :tag "Unspecified" nil)
                 (const :tag "None" t)
                 (string :tag "Argument String")
                 (repeat :tag "Argument List" :value ("") string)))

(defcustom vc-jj-checkin-switches nil
  "String or list of strings specifying switches for \"jj commit\".
If nil, use the value of `vc-checkin-switches'.  If t, use no switches."
  :type '(choice (const :tag "Unspecified" nil)
                 (const :tag "None" t)
                 (string :tag "Argument String")
                 (repeat :tag "Argument List" :value ("") string)))

(defcustom vc-jj-diff-switches '("--git")
  "String or list of strings specifying switches for \"jj diff\".
If nil, use the value of `vc-diff-switches'.  If t, use no switches."
  :type '(choice (const :tag "Unspecified" nil)
                 (const :tag "None" t)
                 (string :tag "Argument String")
                 (repeat :tag "Argument List" :value ("") string)))

(defface vc-jj-log-view-commit
  '((t :weight light :inherit (shadow italic)))
  "Face for commit IDs in `vc-jj-log-view-mode' buffers.")

(defface vc-jj-log-view-bookmark
  '((t :weight bold :inherit log-view-message))
  "Face for bookmark names in `vc-jj-log-view-mode' buffers.")

;;; Internal Utilities

;; Note that 'JJ' should come before 'Git' in `vc-handled-backends',
;; since by default a jj repository contains both '.jj' and '.git'
;; directories.

;;;###autoload
(add-to-list 'vc-handled-backends 'JJ)

(defun vc-jj--filename-to-fileset (filename)
  "Convert FILENAME to a JJ fileset expression.
The fileset expression returned is relative to the JJ repository root.

When FILENAME is not inside a JJ repository, throw an error."
  (if-let ((root (vc-jj-root filename))
           (default-directory root))
      (format "root:%S" (file-relative-name filename root))
    (error "File is not inside a JJ repository: %s" filename)))

(defun vc-jj--set-up-process-buffer (buffer root command)
  "Prepare BUFFER for execution of COMMAND in directory ROOT."
  (with-current-buffer buffer
    (vc-run-delayed
      (vc-compilation-mode 'jj)
      (setq-local compile-command (string-join command " "))
      (setq-local compilation-directory root)
      ;; Either set `compilation-buffer-name-function' locally to nil
      ;; or use `compilation-arguments' to set `name-function'.
      ;; See `compilation-buffer-name'.
      (setq-local compilation-arguments
                  (list compile-command nil
                        (lambda (_name-of-mode) buffer)
                        nil))
      (ansi-color-filter-region (point-min) (point-max))))
  (vc-set-async-update buffer))

(defun vc-jj--process-lines (&rest args)
  "Run jj with ARGS, returning its output to stdout as a list of strings.
In contrast to `process-lines', discard output to stderr since jj prints
warnings to stderr even when run with '--quiet'."
  (with-temp-buffer
    (let ((status (apply #'process-file vc-jj-program nil
                         ;; (current-buffer)
                         (list (current-buffer) nil)
                         nil args)))
      (unless (eq status 0)
	(error "'jj' exited with status %s" status))
      (ansi-color-filter-region (point-min) (point-max))
      (goto-char (point-min))
      (let (lines)
        (while (not (eobp))
          (setq lines (cons (buffer-substring-no-properties
                             (line-beginning-position)
                             (line-end-position))
                            lines))
          (forward-line 1))
        (nreverse lines)))))

(defun vc-jj--command-parseable (&rest args)
  "Run jj with ARGS, returning its output as string.

Note: any filenames in ARGS should be converted via
`vc-jj--filename-to-fileset'.

In contrast to `vc-jj--command-dispatched', discard output to stderr so
the output can be safely parsed.  Does not support many of the features
of `vc-jj--command-dispatched', such as async execution and checking of
process status."
  (with-temp-buffer
    (let ((status (apply #'process-file vc-jj-program nil
                         (list (current-buffer) nil)
                         nil args)))
      (unless (eq status 0)
	(error "'jj' exited with status %s" status))
      (ansi-color-filter-region (point-min) (point-max))
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun vc-jj--command-dispatched (buffer okstatus file-or-list &rest flags)
  "Execute `vc-jj-program', notifying the user and checking for errors.
See `vc-do-command' for documentation of the BUFFER, OKSTATUS,
FILE-OR-LIST and FLAGS arguments.

Note: if we need to parse jj's output `vc-jj--command-parseable' should
be used instead of this function, since jj might print warnings to
stderr and1 `vc-do-command' cannot separate output to stdout and stderr."
  (let* ((filesets (mapcar #'vc-jj--filename-to-fileset (ensure-list file-or-list)))
         (global-switches (ensure-list vc-jj-global-switches)))
    (apply #'vc-do-command (or buffer "*vc*") okstatus vc-jj-program
           ;; Note that we pass NIL for FILE-OR-LIST to avoid
           ;; vc-do-command mangling of filenames; we pass the fileset
           ;; expressions in ARGS instead.
           nil
           (append global-switches flags filesets))))

;;; BACKEND PROPERTIES

;;;; revision-granularity
(defun vc-jj-revision-granularity ()
  "Jj implementation of vc property `revision-granularity'."
  'repository)

;;;; update-on-retrieve-tag
(defun vc-jj-update-on-retrieve-tag ()
  "JJ-specific implementation of `update-on-retrieve-tag' property."
  nil)

;;; STATE-QUERYING FUNCTIONS

;;;; registered

;;;###autoload (defun vc-jj-registered (file)
;;;###autoload   "Return non-nil if FILE is registered with jj."
;;;###autoload   (if (and (vc-find-root file ".jj")   ; Short cut.
;;;###autoload            (executable-find "jj"))
;;;###autoload       (progn
;;;###autoload         (load "vc-jj" nil t)
;;;###autoload         (vc-jj-registered file))))
(defun vc-jj-registered (file)
  "Check whether FILE is registered with jj.
Return non-nil when FILE is file tracked by JJ and nil when not."
  (when-let ((default-directory (vc-jj-root file)))
    (vc-jj--process-lines "file" "list" "--"
                          (vc-jj--filename-to-fileset file))))

;;;; state

(defun vc-jj-state (file)
  "JJ implementation of `vc-state' for FILE.
There are several file states recognized by vc (see the docstring of
`vc-state' for the full list).  Several of these are relevant to
jujutsu.  They are:
- Added (new file)
- Removed (deleted file)
- Edited (modified file)
- Up-to-date (unmodified file)
- Conflict (merge conflict)
- Ignored (ignored by repository)

Other VCS backends would also recognize the \"unregistered\" state, but
there is no such state in jj since jj automatically registers new files."
  ;; We can deduce all of the vc states listed above with two
  ;; commands:
  ;;
  ;; - "jj diff --summary FILE" gets us the "added" state (when output
  ;;   starts with "A "), the "removed" state (when output starts with
  ;;   "D ") and "edited" state (when output starts with "M ").  No
  ;;   output is also possible (this could mean the "conflict",
  ;;   "ignored" or "unchanged" state), so we deduce these with the
  ;;   next command:
  ;;
  ;; - "jj file list -T 'conflict' FILE" prints "true" when the file
  ;;   is in "conflict" state, prints an empty string when the file is
  ;;   in "ignored" state, and "false" for any other state -- but most
  ;;   of these are already covered by the previous command, so we
  ;;   deduce "up-to-date".
  (let* ((default-directory (vc-jj-root file))
         (file (vc-jj--filename-to-fileset file))
         (changed (vc-jj--command-parseable "diff" "--summary" "--" file)))
    (cond
     ((string-prefix-p "A " changed) 'added)
     ((string-prefix-p "D " changed) 'removed)
     ((string-prefix-p "M " changed) 'edited)
     (t (let ((conflicted-ignored
               (vc-jj--command-parseable "file" "list" "-T" "conflict" "--"
                                         file)))
          (cond
           ((string= conflicted-ignored "true") 'conflict)
           ((string-empty-p conflicted-ignored) 'ignored)
           ;; If the file hasn't matched anything yet, this leaves
           ;; only one possible state: up-to-date
           ((string= conflicted-ignored "false") 'up-to-date)
           (t
            (warn "VC state of %s is not recognized, assuming up-to-date" file)
            'up-to-date)))))))

;;;; dir-status-file

(defun vc-jj-dir-status-files (dir _files update-function)
  "Calculate a list of (FILE STATE EXTRA) entries for DIR.
Return the result of applying UPDATE-FUNCTION to that list.

For a description of the states relevant to jj, see `vc-jj-state'."
  ;; This function is specified below the STATE-QUERYING FUNCTIONS
  ;; header in the comments at the beginning of vc.el.  The
  ;; specification says the 'dir-status-files' backend function
  ;; returns "a list of lists ... for FILES in DIR", which does not
  ;; say anything about subdirectories.  We follow the example of
  ;; 'vc-git' and return the state of files in subdirectories of DIR
  ;; as well (except for ignored files, since we don't want to cons up
  ;; a list of every file below DIR).
  ;;
  ;; Unfortunately this function needs to do a lot of work:
  ;; - There is no single jj command that gives us all the info we
  ;;   need, so we cannot run asynchronously.
  ;; - jj prints filenames relative to the repository root, while we
  ;;   need them relative to DIR.
  ;;
  ;; TODO: we should use hash tables, since we're doing a lot of set
  ;; operations, which are slow on lists.
  (with-demoted-errors "JJ error during `vc-dir-status-files': %S"
    (let* ((dir (expand-file-name dir))
           (default-directory dir)
           (project-root (vc-jj-root dir))
           (fileset (vc-jj--filename-to-fileset dir))
           (registered-files (vc-jj--process-lines "file" "list" "--" fileset))
           (ignored-files (seq-difference (cl-delete-if #'file-directory-p
                                                        (directory-files dir nil nil t))
                                          registered-files))
           (changed (vc-jj--process-lines "diff" "--summary" "--" fileset))
           (added-files (mapcan (lambda (entry)
                                  (and (string-prefix-p "A " entry)
                                       (list (substring entry 2))))
                                changed))
           (removed-files (mapcan (lambda (entry)
                                    (and (string-prefix-p "D " entry)
                                         (list (substring entry 2))))
                                  changed))
           (edited-files (mapcan (lambda (entry)
                                     (and (string-prefix-p "M " entry)
                                          (list (substring entry 2))))
                                   changed))
           ;; The command below only prints conflicted files in DIR, but
           ;; relative to project-root, hence the dance with
           ;; expand-file-name / file-relative-name
           (conflicted-files (mapcar (lambda (entry)
                                       (file-relative-name (expand-file-name entry project-root) dir))
                                     (vc-jj--process-lines "file" "list"
                                                           "-T" "if(conflict, path ++ \"\\n\")"
                                                           "--" fileset)))
           (up-to-date-files (cl-remove-if (lambda (entry) (or (member entry conflicted-files)
                                                              (member entry edited-files)
                                                              (member entry added-files)
                                                              (member entry ignored-files)))
                                          registered-files))
           (result
            (nconc (mapcar (lambda (entry) (list entry 'conflict)) conflicted-files)
                   (mapcar (lambda (entry) (list entry 'added)) added-files)
                   (mapcar (lambda (entry) (list entry 'removed)) removed-files)
                   (mapcar (lambda (entry) (list entry 'edited)) edited-files)
                   (mapcar (lambda (entry) (list entry 'ignored)) ignored-files)
                   (mapcar (lambda (entry) (list entry 'up-to-date)) up-to-date-files))))
      (funcall update-function result nil))))

;;;; dir-extra-headers

(defun vc-jj-dir-extra-headers (dir)
  "Return extra headers for `vc-dir' when executed inside DIR.

Always add headers for the first line of the description, the change ID,
and the git commit ID of the current change.  If the current change is
named by one or more bookmarks, also add a Bookmarks header.  If the
current change is conflicted, divergent, hidden or immutable, also add a
Status header.  (We do not check for emptiness of the current change
since the user can see that via the list of files below the headers
anyway.)"
  (cl-destructuring-bind
      ;; We have some fixed lines for the current changeset, then at
      ;; the end 5 lines for each parent
      ( change-id change-id-short commit-id commit-id-short
        description bookmarks conflict divergent hidden immutable
        &rest parent-info)
      (let ((default-directory (file-name-as-directory dir)))
        (vc-jj--process-lines "log" "--no-graph" "-r" "@" "-T"
                              "concat(
self.change_id().short(8), \"\\n\",
self.change_id().shortest(), \"\\n\",
self.commit_id().short(8), \"\\n\",
self.commit_id().shortest(), \"\\n\",
description.first_line(), \"\\n\",
bookmarks.join(\",\"), \"\\n\",
self.conflict(), \"\\n\",
self.divergent(), \"\\n\",
self.hidden(), \"\\n\",
self.immutable(), \"\\n\",
parents.map(|c| concat(
  c.change_id().short(8), \"\\n\",
  c.change_id().shortest(), \"\\n\",
  c.commit_id().short(8), \"\\n\",
  c.commit_id().shortest(), \"\\n\",
  c.description().first_line(), \"\\n\"
)))"))
    (cl-labels
        ((str (string &optional face prefix)
           ;; format a string
           (cond ((not face) (propertize string 'face 'vc-dir-header-value))
                 ((not prefix) (propertize string 'face face))
                 (t (concat (propertize prefix 'face 'vc-dir-header-value)
                            (propertize string 'face face)))))
         (info (key description change-id change-id-prefix commit-id commit-id-prefix)
           ;; format a changeset info line
           (let ((change-id-suffix (substring change-id (length change-id-prefix)))
                 (commit-id-suffix (substring commit-id (length commit-id-prefix))))
             (concat
              (str (format "% -11s: " key) 'vc-dir-header)
              ;; There's no vc-dir-header-value-emphasis or similar
              ;; face, so we re-use vc-dir-status-up-to-date to render
              ;; the unique prefix
              " "
              (str change-id-suffix 'vc-dir-status-ignored change-id-prefix)
              " "
              (str commit-id-suffix 'vc-dir-status-ignored commit-id-prefix)
              " "
              (if (string-empty-p description)
                  (str "(no description set)")
                (str description))))))
      (let ((status (concat
                     (and (string= conflict "true") "(conflict)")
                     (and (string= divergent "true") "(divergent)")
                     (and (string= hidden "true") "(hidden)")
                     (and (string= immutable "true") "(immutable)")))
            (parent-keys (cl-loop
                          for (change-id change-id-short commit-id commit-id-short description)
                          in (seq-partition parent-info 5)
                          collect (info "Parent" description
                                        change-id change-id-short
                                        commit-id commit-id-short))))
        (string-join
         (seq-remove
          ;; Remove NIL entries because we get empty lines otherwise
          #'null
          (cl-list*
           (info "Changeset" description change-id change-id-short commit-id commit-id-short)
           (unless (string= bookmarks "")
             (concat (str "Bookmarks  : " 'vc-dir-header) (str bookmarks)))
           (unless (string= status "")
             (concat (str "Status     : " 'vc-dir-header) (str status 'vc-dir-status-warning)))
           parent-keys))
         "\n")))))

;;;; dir-printer

;;;; status-fileinfo-extra

;;;; working-revision

(defun vc-jj-working-revision (file)
  "Return the current change id of the repository containing FILE."
  (when-let* ((default-directory (vc-jj-root file)))
    ;; 'jj log' might print a warning at the start of its output,
    ;; e.g., "Warning: Refused to snapshot some files."  The output we
    ;; want will be printed afterwards.
    (car (last (vc-jj--process-lines "log" "--no-graph"
                                     "-r" "@"
                                     "-T" "change_id ++ \"\\n\"")))))

;;;; checkout-model

(defun vc-jj-checkout-model (_files)
  "JJ-specific implementation of `vc-checkout-model'."
  'implicit)

;;;; mode-line-string

(defun vc-jj-mode-line-string (file)
  "Return a mode line string and tooltip for FILE."
  (pcase-let* ((long-rev (vc-jj-working-revision file))
               (`(,short-rev ,description)
                (vc-jj--process-lines "log" "--no-graph" "-r" long-rev
                                      "-T" "self.change_id().shortest() ++ \"\\n\" ++ description.first_line() ++ \"\\n\""))
               (def-ml (vc-default-mode-line-string 'JJ file))
               (help-echo (get-text-property 0 'help-echo def-ml))
               (face   (get-text-property 0 'face def-ml)))
    ;; See docstring of `vc-default-mode-line-string' for a
    ;; description of the string prefix we extract here
    (propertize (concat (substring def-ml 0 3) short-rev)
                'face face
                'help-echo (concat help-echo
                                   "\nCurrent change: " long-rev
                                   " (" description ")"))))

;;; STATE-CHANGING FUNCTIONS

;;;; create-repo

(defun vc-jj-create-repo ()
  "Create an empty jj repository in the current directory."
  (if current-prefix-arg
      (process-file vc-jj-program nil nil nil "git" "init" "--colocate")
    (process-file vc-jj-program nil nil nil "git" "init")))

;;;; register

(defun vc-jj-register (files &optional _comment)
  "Register FILES into the jj version-control system."
  ;; This is usually a no-op since jj auto-registers all files, so we
  ;; just need to run some jj command so new files are picked up.  We
  ;; run "jj file track" for the case where some of FILES are excluded
  ;; via the "snapshot.auto-track" setting or via git's mechanisms
  ;; such as the .gitignore file.
  (vc-jj--command-dispatched nil 0 files "file" "track" "--"))

;;;; responsible-p

(defalias 'vc-jj-responsible-p #'vc-jj-root)

;;;; receive-file

;;;; unregister

;;;; checkin

(defun vc-jj-checkin (files comment &optional _rev)
  "Run \"jj commit\" with supplied FILES and COMMENT."
  (let* ((comment (car (log-edit-extract-headers () comment)))
         (args (append (vc-switches 'jj 'checkin)
                       (list "commit" "-m" comment))))
    (apply #'vc-jj--command-dispatched nil 0 files args)))

;;;; checkin-patch

;;;; find-revision

(defun vc-jj-find-revision (file rev buffer)
  "Read revision REV of FILE into BUFFER and return the buffer."
  (let ((revision (vc-jj--command-parseable "file" "show" "-r" rev "--" (vc-jj--filename-to-fileset file))))
    (with-current-buffer buffer
      (erase-buffer)
      (insert revision)))
  buffer)

;;;; checkout

(defun vc-jj-checkout (file &optional rev)
  "Restore the contents of FILE to be the same as in change REV.
If REV is not specified, revert the file as with `vc-jj-revert'."
  ;; TODO: check that this does the right thing: if REV is not
  ;; specified, should we revert or leave FILE unchanged?
  (let ((args (append (and rev (list "--from" rev)))))
    (apply #'vc-jj--command-dispatched nil 0 file "restore" args)))

;;;; revert

(defun vc-jj-revert (file &optional _contents-done)
  "Restore FILE to the state from its parent(s), via \"jj restore\"."
  (vc-jj--command-dispatched nil 0 file "restore"))

;;;; merge-file

;;;; merge-branch

;;;; merge-news

;;;; pull

(defvar vc-jj-pull-history nil
  "History variable for `vc-jj-pull'.")

(defun vc-jj-pull (prompt)
  "JJ-specific implementation of `vc-pull'.
If PROMPT is non-nil, prompt for the jj command to run (default is \"jj
git fetch\")."
  (let* ((command (if prompt
                      (split-string-shell-command
                       (read-shell-command
                        (format "jj git fetch command: ")
                        (concat vc-jj-program " git fetch")
                        'vc-jj-pull-history))
                    `(,vc-jj-program "git" "fetch")))
         (jj-program (car command))
         (args (cdr command))
         (root (vc-jj-root default-directory))
         (buffer (format "*vc-jj : %s*" (expand-file-name root))))
    (apply #'vc-do-async-command buffer root jj-program args)
    (vc-jj--set-up-process-buffer buffer root command)))

;;;; push

(defvar vc-jj-push-history nil
  "History variable for `vc-jj-push'.")

(defun vc-jj-push (prompt)
  "JJ-specific implementation of `vc-push'.
If PROMPT is non-nil, prompt for the command to run (default is \"jj git
push\")."
  (let* ((command (if prompt
                      (split-string-shell-command
                       (read-shell-command
                        (format "jj git push command: ")
                        (concat vc-jj-program " git push")
                        'vc-jj-push-history))
                    `(,vc-jj-program "git" "push")))
         (jj-program (car command))
         (args (cdr command))
         (root (vc-jj-root default-directory))
         (buffer (format "*vc-jj : %s*" (expand-file-name root))))
    (apply #'vc-do-async-command buffer root jj-program args)
    (vc-jj--set-up-process-buffer buffer root command)))

;;;; steal-lock

;;;; get-change-comment

(defun vc-jj-get-change-comment (_files rev)
  "Get the change comment of revision REV."
  (vc-jj--command-parseable "log" "--no-graph" "-n" "1"
                            "-r" rev "-T" "description"))

;;;; modify-change-comment

;; TODO: protect immutable changes
(defun vc-jj-modify-change-comment (_files rev comment)
  "Set the change comment of revision REV to COMMENT."
  (let ((comment (car (log-edit-extract-headers () comment))))
    (vc-jj--command-dispatched nil 0 nil "desc" rev "-m" comment "--quiet")))

;;;; mark-resolved

;;;; find-admin-dir

;;; HISTORY FUNCTIONS

;;;; print-log

(defun vc-jj-print-log (files buffer &optional shortlog start-revision limit)
  "Print commit log associated with FILES into specified BUFFER.
If SHORTLOG is non-nil, use a short log format similar to
`vc-jj-root-log-format'.  If START-REVISION is non-nil, it is a string
of the newest revision in the log to show.  If LIMIT is a number, show
no more than this many entries.  If LIMIT is a non-empty string, use it
as a base revision."
  (vc-setup-buffer buffer)
  (let ((inhibit-read-only t)
        (files
         ;; There is a special case when FILES has just the root of
         ;; the project as its only element (e.g., when calling
         ;; `vc-print-root-log').  In this case, we do not specify a
         ;; fileset to jj because doing do would cause the log to only
         ;; show ancestors of START-REVISION (even if the fileset is
         ;; "all()").  This behavior is undesirable in
         ;; `vc-print-root-log' (in a JJ context of bookmarks), since
         ;; users expect to see descendants as well
         (unless (file-equal-p (vc-jj-root (car files)) (car files))
           files))
        (args (append (pcase limit
                        ;; When LIMIT is a number, only show up to
                        ;; that many revisions
                        ((pred numberp)
                         (list "-n" (number-to-string limit)
                               "-r" (concat "::" start-revision)))
                        ;; When LIMIT is a string, it is a revision.
                        ;; In that case, show the revisions between
                        ;; LIMIT and START-REVISION, not including the
                        ;; revision LIMIT.
                        ((pred stringp)
                         (list "-r" (format "%s::%s & ~%s" limit start-revision limit))))
                      (if shortlog
                          (list "-T" (car vc-jj-root-log-format))
                        (list "--no-graph" "-T" "builtin_log_detailed")))))
    (with-current-buffer buffer
      (apply #'vc-jj--command-dispatched buffer 'async files "log" args))))

;;;; log-outgoing

;; (defun vc-jj-log-outgoing (buffer remote-location)
;;   ;; TODO
;;   )

;;;; log-incoming

;; (defun vc-jj-log-incoming (buffer remote-location)
;;   ;; TODO
;;   )

;;;; log-search

;;;; log-view-mode

(defun vc-jj-log-view-restore-position ()
  "Restore the position of point prior to reverting.
When this function is added to `revert-buffer-restore-functions' in a
`vc-jj-log-view-mode' buffer, after reverting the buffer, restore the
position of the point to the revision the point was on prior to
reverting.  If that revision no longer exists, do not move the point."
  (when-let* ((rev (log-view-current-tag)))
    (lambda ()
      (when (re-search-forward rev nil t)
        (goto-char (match-beginning 0))
        (beginning-of-line)
        ;; Report to the user that we've restored the point, otherwise
        ;; they might think the buffer was not reverted or that
        ;; something has gone wrong (because that would likely be the
        ;; case with the default behavior, without this function)
        (message "Buffer reverted and point restored to revision %s"
                 (propertize (vc-jj--command-parseable "show" "--no-patch"
                                                       "-r" rev
                                                       "-T" "change_id.shortest()")
                             'face 'log-view-message))))))

(defun vc-jj--expanded-log-entry (revision)
  "Return a string of the commit details of REVISION.
Called by `log-view-toggle-entry-display' in a JJ Log View buffer."
  (with-temp-buffer
    (vc-jj--command-dispatched
     t 0 nil "log"
     ;; REVISION may be divergent (i.e., several revisions with the
     ;; same change ID).  In those cases, we opt to avoid jj erroring
     ;; via "-r change_id(REVISION)" and show only all the divergent
     ;; commits.  This is preferable to confusing or misinforming the
     ;; user by showing only some of the divergent commits.
     "-r" (format "change_id(%s)" revision)
     "--no-graph" "-T" "builtin_log_detailed")
    (buffer-string)))

(defvar auto-revert-mode)

;; FIXME 2025-12-07: This only reverts the parent buffer, but there
;; more often than not multiple files in a project that could be
;; reverted.  Do we revert those too?  (Note: there is
;; `vc-auto-revert-mode' in Emacs 31.1.)
(defun vc-jj--reload-log-buffers ()
  "Revert the log view buffer and its parent buffer.
Revert the parent buffer of the current log view buffer then revert the
log view buffer.

The \"parent buffer\" of the log view buffer is the buffer referenced by
the variable `vc-parent-buffer'; it is the buffer in which the log view
buffer was created."
  (when (derived-mode-p 'vc-jj-log-view-mode)
    (when vc-parent-buffer
      (with-current-buffer vc-parent-buffer
        ;; It's only necessary to ask when `auto-revert-mode' isn't
        ;; already enabled in the `vc-parent-buffer'.  If it is, then
        ;; it is safe to assume the user wants that buffer reverted
        ;; automatically
        (revert-buffer nil auto-revert-mode)))
    ;; Revert the log view buffer
    (revert-buffer)))

(defun vc-jj-log-view-edit-change ()
  "Edit the jj revision at point.
Call \"jj edit\" on the revision at point."
  (interactive nil vc-jj-log-view-mode)
  (let ((rev (log-view-current-tag)))
    (vc-jj-retrieve-tag nil rev nil)
    (vc-jj--reload-log-buffers)))

(defun vc-jj-log-view-abandon-change ()
  "Abandon the jj revision at point.
Call \"jj abandon\" on the revision at point."
  (interactive nil vc-jj-log-view-mode)
  (let ((rev (log-view-current-tag)))
    (when (y-or-n-p (format "Abandon revision %s?"
                            (propertize
                             (vc-jj--command-parseable "show" "--no-patch"
                                                       "-r" rev
                                                       "-T" "change_id.shortest()")
                             'face 'log-view-message)))
      (vc-jj--command-dispatched nil 0 nil "abandon" rev "--quiet")
      (vc-jj--reload-log-buffers))))

(defun vc-jj-log-view-new-change ()
  "Create a new, empty change on the revision at point.
Call \"jj new\", creating a new revision whose parent is the revision at
point."
  (interactive nil vc-jj-log-view-mode)
  (let ((rev (log-view-current-tag)))
    (vc-jj--command-dispatched nil 0 nil "new" rev "--quiet")
    (vc-jj--reload-log-buffers)))

(defun vc-jj-log-view-bookmark-set ()
  "Set the bookmark of revision at point.
When called in a `vc-jj-log-view-mode' buffer, prompt for a bookmark to
set at the revision at point.  If the bookmark already exists and would
be moved backwards or sideways in the revision history, confirm with the
user first."
  (interactive nil vc-jj-log-view-mode)
  (when (derived-mode-p 'vc-jj-log-view-mode)
    (let* ((target-rev (log-view-current-tag))
           (bookmarks (vc-jj--process-lines "bookmark" "list" "-T" "self.name() ++ \"\n\""))
           (bookmark (completing-read "Move or create bookmark: " bookmarks))
           (new-bookmark-p (not (member bookmark bookmarks)))
           ;; If the bookmark already exists and target-rev is not a
           ;; descendant of the revision that the bookmark is
           ;; currently on, this means that the bookmark will be moved
           ;; sideways or backwards
           (backwards-move-p
            (unless new-bookmark-p
              ;; If `vc-jj--process-lines' returns nil, then that
              ;; means the jj command returned no revisions.  This
              ;; REVSET is the intersection between BOOKMARK, its
              ;; descendants, and TARGET-REV.  That intersection is
              ;; non-empty only when TARGET-REV is BOOKMARK or a
              ;; descendant or BOOKMARK
              (not (vc-jj--process-lines "log" "-r" (format "%s & %s::" target-rev bookmark))))))
      (when backwards-move-p
        (unless (yes-or-no-p
                 (format-prompt "Moving bookmark %s to revision %s would move it either backwards or sideways. Is this okay?"
                                nil bookmark target-rev bookmark))
          (user-error "Aborted moving bookmark %s to revision %s" bookmark target-rev)))
      (vc-jj--command-dispatched nil 0 nil "bookmark" "set" bookmark "-r" target-rev
                                 "--allow-backwards" "--quiet")
      (revert-buffer))))

(defun vc-jj-log-view-bookmark-rename ()
  "Rename a bookmark pointing to the revision at point.
When called in a `vc-jj-log-view-mode' buffer, rename the bookmark
pointing to the revision at point.  If there are multiple bookmarks
pointing to the revision, prompt the user to one of these bookmarks to
rename."
  (interactive nil vc-jj-log-view-mode)
  (when (derived-mode-p 'vc-jj-log-view-mode)
    (let* ((target-rev (log-view-current-tag))
           (bookmarks-at-rev
            (or (vc-jj--process-lines "bookmark" "list" "-r" target-rev
                                      "-T" "if(!self.remote(), self.name() ++ \"\n\")")
                ;; FIXME(KrisB 2025-12-09): Is there a more
                ;; idiomatic/cleaner way to exit with a message than a
                ;; `user-error' in the middle of a let binding?
                (user-error "No bookmarks at %s"
                            (propertize
                             (vc-jj--command-parseable "show" "--no-patch"
                                                       "-r" target-rev
                                                       "-T" "change_id.shortest()")
                             'face 'log-view-message))))
           (bookmark-old
            (if (= 1 (length bookmarks-at-rev))
                (car bookmarks-at-rev)
              (completing-read "Which bookmark to rename? " bookmarks-at-rev)))
           (bookmark-new
            (read-string (format-prompt "Rename %s to" nil bookmark-old))))
      (vc-jj--command-dispatched nil 0 nil "bookmark" "rename" bookmark-old bookmark-new
                                 "--quiet")
      (revert-buffer))))

(defun vc-jj-log-view-bookmark-delete ()
  "Delete bookmark of the revision at point.
When called in a `vc-jj-log-view-mode' buffer, delete the bookmark of
the revision at point.  If there are multiple bookmarks attached to the
revision, prompt the user to choose one or more of these bookmarks to
delete."
  (interactive nil vc-jj-log-view-mode)
  (when (derived-mode-p 'vc-jj-log-view-mode)
    (let* ((rev (log-view-current-tag))
           (revision-bookmarks
            (string-split
             (vc-jj--command-parseable
              "show" "-r" rev "--no-patch"
              "-T" "self.local_bookmarks().map(|b| b.name()) ++ \"\n\"")
             " " t "\n"))
           (bookmarks
            (if (< 1 (length revision-bookmarks))
                (completing-read-multiple "Delete bookmarks: " revision-bookmarks nil t)
              revision-bookmarks)))
      (apply #'vc-jj--command-dispatched nil 0 nil "--quiet" "bookmark" "delete" bookmarks)
      (revert-buffer))))

(defvar vc-jj-log-view-mode-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "r" #'vc-jj-log-view-edit-change)
    (keymap-set map "x" #'vc-jj-log-view-abandon-change)
    (keymap-set map "i" #'vc-jj-log-view-new-change)
    (keymap-set map "b s" #'vc-jj-log-view-bookmark-set)
    (keymap-set map "b r" #'vc-jj-log-view-bookmark-rename)
    (keymap-set map "b D" #'vc-jj-log-view-bookmark-delete)
    map)
  "Keymap for `vc-jj-log-view-mode'.")

(define-derived-mode vc-jj-log-view-mode log-view-mode "JJ-Log-View"
  "Log View mode specific for JJ."
  :keymap vc-jj-log-view-mode-map

  (require 'add-log) ;; We need the faces add-log.
  ;; Don't have file markers, so use impossible regexp.
  (setq-local log-view-file-re regexp-unmatchable)
  (setq-local log-view-per-file-logs nil)
  ;; The `log-view-message-re' is a regexp matching a revision.  Its
  ;; first match group must match the revision number itself
  (setq-local log-view-message-re
              (if (not (memq vc-log-view-type '(long log-search with-diff)))
                  (cadr vc-jj-root-log-format)
                (rx bol "Commit ID: " (1+ (any alnum)) "\n"
                    "Change ID: " (group (1+ (any alpha))))))
  ;; Allow expanding short log entries.
  (when (memq vc-log-view-type '(short log-outgoing log-incoming mergebase))
    (setq truncate-lines t)
    (setq-local log-view-expanded-log-entry-function 'vc-jj--expanded-log-entry))
  ;; Fontify according to regexp capture groups (for "short" format
  ;; log views, the relevant regexp is found in
  ;; `vc-jj-root-log-format')
  (setq-local log-view-font-lock-keywords
              (if (not (memq vc-log-view-type '(long log-search with-diff)))
                  (list (cons (nth 1 vc-jj-root-log-format)
                              (nth 2 vc-jj-root-log-format)))
                `((,log-view-message-re
                   (1 'change-log-acknowledgment))
                  (,(rx bol "Commit ID" (0+ (any space)) ": " (group (1+ (any alnum))) "\n")
                   (1 'vc-jj-log-view-commit))
                  (,(rx bol "Bookmarks" (0+ (any space)) ": " (group (1+ not-newline)) "\n")
                   (1 'vc-jj-log-view-bookmark))
                  (,(rx bol (or "Author" "Committer") (0+ (any space)) ": "
                        ;; Name
                        (group (+? anything))
                        ;; Email (based on `goto-address-mail-regexp')
                        (group " <"
                               (+ (or alnum (any "-=._+")))
                               "@"
                               (+ (seq (+ (or alnum (any "-_"))) "."))
                               (+ alnum)
                               "> ")
                        ;; Date and time
                        "(" (group (+ (or digit space (any "-:")))) ")\n")
                   (1 'change-log-name)
                   (2 'change-log-email)
                   (3 'change-log-date)))))

  (when (boundp 'revert-buffer-restore-functions) ; Emacs 30.1
    (add-hook 'revert-buffer-restore-functions #'vc-jj-log-view-restore-position nil t)))

;;;; show-log-entry

(defun vc-jj-show-log-entry (revision)
  "Move to the log entry for REVISION."
  ;; TODO: check that this works for all forms of log output, or at
  ;; least the predefined ones
  (goto-char (point-min))
  (when (search-forward-regexp
         ;; TODO: reformulate using `rx'
         (concat "^[^|]\\s-+\\(" (regexp-quote revision) "\\)\\s-+")
         nil t)
    (goto-char (match-beginning 1))))

;;;; comment-history

;;;; update-changelog

;;;; diff

(defun vc-jj-diff (files &optional rev1 rev2 buffer _async)
  "Display diffs for FILES between revisions REV1 and REV2.
FILES is a list of file paths.  REV1 and REV2 are the full change IDs of
two revisions.  REV1 is the earlier revision and REV2 is the later
revision.

When BUFFER is non-nil, it is the buffer object or name to insert the
diff into.  Otherwise, when nil, insert the diff into the *vc-diff*
buffer.  If _ASYNC is non-nil, run asynchronously.  This is currently
unsupported."
  ;; TODO: handle async
  (setq buffer (get-buffer-create (or buffer "*vc-diff*"))
        files (mapcar #'vc-jj--filename-to-fileset files))
  (cond
   ((not (or rev1 rev2))
    ;; Use `vc-jj-previous-revision' instead of "@-" because the
    ;; former handles edge cases like e.g. multiple parents
    (setq rev1 (vc-jj-previous-revision nil "@")))
   ((null rev1)
    (setq rev1 "root()")))
  (setq rev2 (or rev2 "@"))
  (let ((inhibit-read-only t)
        ;; When REV1 and REV2 are the same revision, "-f REV1 -t REV2"
        ;; (erroneously) returns an empty diff.  So we check for that
        ;; case and use "-r REV1" instead, which returns the correct
        ;; diff
        (args (append (if (string= rev1 rev2)
                          (list "-r" rev1)
                        (list "-f" rev1 "-t" rev2))
                      (vc-switches 'jj 'diff)
                      (list "--") files)))
    ;; Match `vc-git-diff' by returning the value of
    ;; `vc-jj--command-dispatched'.
    ;;
    ;; Also ensure that (vc-jj--command-dispatched BUFFER ...) is
    ;; called when BUFFER is not the current buffer.  Otherwise,
    ;; BUFFER may accumulate content from previous invocations of
    ;; `vc-jj-diff', because `vc-do-command' (called internally by
    ;; `vc-jj--command-dispatched') only erases BUFFER when BUFFER is
    ;; not the current buffer.  See
    ;; https://codeberg.org/emacs-jj-vc/vc-jj.el/issues/152 for more
    ;; information.
    (prog1
        (apply #'vc-jj--command-dispatched buffer 0 nil "diff" args)
      (with-current-buffer buffer
        (ansi-color-filter-region (point-min) (point-max))))))

;;;; revision-completion-table

(defun vc-jj--revision-annotation-function (elem)
  "Calculate propertized change description from ELEM.
ELEM can be of the form (change-id . description), as produced in
`vc-jj-revision-completion-table', in which case we return the second
element, or can be a change id, in which case we query for the first
line of its description."
  (let ((description
         (if (listp elem)
             (cl-second elem)
           (vc-jj--command-parseable "log" "-r" elem "--no-graph" "-T" "self.description().first_line()"))))
    (format " %s" (propertize description 'face 'completions-annotations))))

(defun vc-jj-revision-completion-table (files)
  "Return a completion table for existing revisions of FILES."
  (let* ((revisions
          (mapcar
           ;; Boldly assuming that jj's change ids won't suddenly change length
           (lambda (line) (list (substring line 0 31) (substring line 32)))
           (apply #'vc-jj--process-lines "log" "--no-graph"
                  "-T" "self.change_id() ++ self.description().first_line() ++ \"\\n\"" "--" files))))
    (lambda (string pred action)
      (if (eq action 'metadata)
          `(metadata . ((display-sort-function . ,#'identity)
                        (annotation-function . ,#'vc-jj--revision-annotation-function)))
        (complete-with-action action revisions string pred)))))

;;;; annotate-command

(defun vc-jj-annotate-command (file buf &optional rev)
  "Fill BUF with per-line change history of FILE at REV."
  (let ((rev (or rev "@"))
        (file (file-relative-name file)))
    ;; Contrary to most other jj commands, 'jj file annotate' takes a
    ;; path instead of a fileset expression, so we append FILE to the
    ;; unprocessed argument list here.
    (apply #'vc-jj--command-dispatched buf 'async nil "file" "annotate"
           (append (vc-switches 'jj 'annotate)
                   (list "-r" rev file)))))

;;;; annotate-time

(defconst vc-jj--annotation-line-prefix-re
  (rx bol
      (group (+ (any "a-z")))           ; change id
      " "
      (group (+? anychar))              ; author username
      (+ " ")
      (group                            ; iso 8601-ish datetime
       (= 4 digit) "-" (= 2 digit) "-" (= 2 digit) " "
       (= 2 digit) ":" (= 2 digit) ":" (= 2 digit))
      (+ " ")
      (group (+ digit))                 ; line number
      ": ")
  ;; TODO: find out if the output changes when the file got renamed
  ;; somewhere in its history
  "Regex for the output of \"jj file annotate\".
The regex matches each line's change information and captures
four groups: change id, author, datetime, line number.")

(defun vc-jj-annotate-time ()
  "Return the time for the annotated line."
  (and-let*
      (((re-search-forward vc-jj--annotation-line-prefix-re nil t))
       (dt (match-string 3))
       (dt (and dt (string-replace " " "T" dt)))
       (decoded (ignore-errors (iso8601-parse dt))))
    (vc-annotate-convert-time
     (encode-time (decoded-time-set-defaults decoded)))))

;;;; annotate-current-time

;;;; annotate-extract-revision-at-line

(defun vc-jj-annotate-extract-revision-at-line ()
  "Return the revision (change id) for the annotated line."
  (save-excursion
    (beginning-of-line)
    (when (looking-at vc-jj--annotation-line-prefix-re)
      (match-string-no-properties 1))))

;;;; region-history

(defun vc-jj-region-history (file buffer lfrom lto)
  "JJ-specific version of `vc-region-history'.
Insert into BUFFER the history (log comments and diffs) of the content
of FILE between lines LFROM and LTO."
  ;; Support for vc-region-history via the git version which works
  ;; fine at least when co-located with git.
  (vc-git-region-history file buffer lfrom lto))

;;;; region-history-mode

(define-derived-mode vc-jj-region-history-mode vc-git-region-history-mode
  "JJ/Git-Region-History")

;;;; mergebase

;;;; last-change

;;; TAG/BRANCH SYSTEM

;;;; create-tag

(defun vc-jj-create-tag (_dir name branchp)
  "Attach tag named NAME to the current revision.
When BRANCHP is non-nil, a bookmark named NAME is created at the current
revision.

Since jujutsu does not support tagging revisions, a nil value of BRANCHP
has no effect."
  (if branchp
      (vc-jj--command-dispatched nil 0 nil "bookmark" "create" name "--quiet")
    (user-error "Setting tags is not supported by jujutsu")))

;;;; retrieve-tag

(defun vc-jj-retrieve-tag (_dir rev _update)
  "Call jj edit on REV inside DIR.
REV is the change ID of a jj revision.

_DIR and _UPDATE are as described in the vc.el specification."
  (vc-jj--command-dispatched nil 0 nil "edit" rev "--quiet"))

;;; MISCELLANEOUS

;;;; make-version-backups-p

;;;; root

(defun vc-jj-root (file)
  "Return the root of the repository containing FILE.
Return NIL if FILE is not in a jj repository."
  (vc-find-root file ".jj"))

;;;; ignore

(defun vc-jj-ignore (file &optional directory remove)
  "Ignore FILE under DIRECTORY.

FILE is a wildcard specification relative to DIRECTORY.
DIRECTORY defaults to `default-directory'.

If REMOVE is non-nil, remove FILE from ignored files instead.

For jj, modify `.gitignore' and call `jj untrack' or `jj track'."
  (vc-default-ignore 'Git file directory remove)
  (let ((default-directory
         (if directory (file-name-as-directory directory)
           default-directory)))
    (vc-jj--command-dispatched nil 0 file "file" (if remove "track" "untrack") "--")))

;;;; ignore-completion-table

;;;; find-ignore-file

(defun vc-jj-find-ignore-file (file)
  "Return the jj ignore file that controls FILE."
  ;; Currently, Jujutsu uses .gitignore files, even for non-colocated
  ;; repos.  See
  ;; https://docs.jj-vcs.dev/latest/working-copy/#ignored-files
  (expand-file-name ".gitignore" (vc-jj-root file)))

;;;; previous-revision

(defun vc-jj-previous-revision (file rev)
  "JJ-specific version of `vc-previous-revision'.
Return the revision number that precedes REV for FILE, or nil if no such
revision exists."
  (if file
      (vc-jj--command-parseable "log" "--no-graph" "--limit" "1"
                                "-r" (format "ancestors(%s) & ~%s" rev rev)
                                "-T" "change_id"
                                "--" (vc-jj--filename-to-fileset file))
    ;; The jj manual states that "for merges, [first_parent] only
    ;; returns the first parent instead of returning all parents";
    ;; given the choice, we do want to return the first parent of a
    ;; merge change.
    (vc-jj--command-parseable "log" "--no-graph"
                              "-r" (concat "first_parent(" rev ")")
                              "-T" "change_id")))

;;;; file-name-changes

;;;; next-revision

(defun vc-jj-next-revision (file rev)
  "JJ-specific version of `vc-next-revision'.
Return the revision that follows REV for FILE, or nil if no such
revision exists."
  (if file
      (vc-jj--command-parseable "log" "--no-graph" "--limit" "1"
                                "-r" (concat "descendants(" rev ")")
                                 "-T" "change_id"
                                 "--" (vc-jj--filename-to-fileset file))
    ;; Note: experimentally, jj (as of 0.35.0) prints children in LIFO
    ;; order (newest child first), but we should not rely on that
    ;; behavior and since none of the children of a change are
    ;; special, we return an arbitrary one.
    (car (vc-jj--process-lines "log" "--no-graph"
                                 "-r" (concat "children(" rev ")")
                                 "-T" "change_id ++ \"\n\""))))

;;;; log-edit-mode

;; Set up `log-edit-mode' to handle jj description files.

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.jjdescription\\'" . log-edit-mode))
;;;###autoload
(add-hook 'vc-log-mode-hook #'vc-jj-ensure-log-edit-callback)
;;;###autoload
(add-hook 'log-edit-mode-hook #'vc-jj-ensure-log-edit-callback)
;;;###autoload
(defun vc-jj-ensure-log-edit-callback ()
  "Set up `log-edit-callback' when editing jj change descriptions."
  (unless (bound-and-true-p log-edit-callback)
    (setq-local log-edit-callback (lambda ()
                                    (interactive)
                                    (save-buffer)
                                    (kill-buffer)))))

;;;; check-headers

;;;; delete-file

(defun vc-jj-delete-file (file)
  "Delete FILE and make sure jj registers the change."
  (when (file-exists-p file)
    (delete-file file)
    (vc-jj--command-dispatched nil 0 nil "status")))

;;;; rename-file

(defun vc-jj-rename-file (old new)
  "Rename file OLD to NEW and make sure jj registers the change."
  (rename-file old new)
  (vc-jj--command-dispatched nil 0 nil "status"))

;;;; find-file-hook

;;;; extra-menu

;;;; extra-dir-menu

;;;; conflicted-files

;;;; repository-url

;;;; prepare-patch

;;;; clone

(defun vc-jj-clone (remote directory rev)
  "Attempt to clone REMOTE repository into DIRECTORY at revision REV.
On failure, return nil.  Upon success, return DIRECTORY."
  (let ((successp (ignore-errors
                    (vc-jj--command-dispatched nil 0 nil "git" "clone" "--colocate" remote directory))))
    (when (and successp rev)
      (let ((default-directory directory))
        (vc-jj--command-dispatched nil 0 nil "new" rev "--quiet")))
    (when successp directory)))

(provide 'vc-jj)
;;; vc-jj.el ends here

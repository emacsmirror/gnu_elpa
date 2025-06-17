;;; vc-jj.el --- VC backend for the Jujutsu version control system -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025  Free Software Foundation, Inc.

;; Author: Wojciech Siewierski
;;         Rudolf Schlatte <rudi@constantly.at>
;; URL: https://codeberg.org/emacs-jj-vc/vc-jj.el
;; Version: 0.3
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

;; A backend for vc.el to handle Jujutsu repositories.

;;; Code:

(require 'compat)
(require 'cl-lib)
(require 'seq)
(require 'vc)
(require 'vc-git)
(require 'ansi-color)
(require 'iso8601)
(require 'time-date)

(add-to-list 'vc-handled-backends 'JJ)

(defun vc-jj-revision-granularity () 'repository)
(defun vc-jj-checkout-model (_files) 'implicit)
(defun vc-jj-update-on-retrieve-tag () nil)

(defgroup vc-jj nil
  "VC Jujutsu backend."
  :group 'vc)

(defcustom vc-jj-program "jj"
  "Name of the jj executable (excluding any arguments)."
  :type 'string
  :risky t)

(defvar vc-jj--log-default-template
  "
if(root,
  format_root_commit(self),
  label(if(current_working_copy, \"working_copy\"),
    concat(
      separate(\" \",
        change_id.shortest(8).prefix() ++ \"​\" ++ change_id.shortest(8).rest(),
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
)
")

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

(defun vc-jj--filename-to-fileset (filename &optional root)
  "Convert FILENAME to a JJ fileset expression.
If ROOT is non-nil, the resulting fileset is relative to the repository
root, otherwise it is relative to the current directory."
  (if root
      (concat "root-file:\""
              (string-replace "\"" "\\\"" (file-relative-name filename root))
              "\"")
    (concat "cwd-file:\""
            (string-replace "\"" "\\\"" (file-relative-name filename)) "\"")))

(defun vc-jj--process-lines (&rest args)
  "Run jj with ARGS, returning its output to stdout as a list of strings.
In contrast to `process-lines', discard output to stderr since jj prints
warnings to stderr even when run with '--quiet'."
  (with-temp-buffer
    (let ((status (apply #'call-process vc-jj-program nil
                         ;; (current-buffer)
                         (list (current-buffer) nil)
                         nil args)))
      (unless (eq status 0)
	(error "'jj' exited with status %s" status))
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
    (let ((status (apply #'call-process vc-jj-program nil
                         (list (current-buffer) nil)
                         nil args)))
      (unless (eq status 0)
	(error "'jj' exited with status %s" status))
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

;;;###autoload (defun vc-jj-registered (file)
;;;###autoload   "Return non-nil if FILE is registered with jj."
;;;###autoload   (if (and (vc-find-root file ".jj")   ; Short cut.
;;;###autoload            (executable-find "jj"))
;;;###autoload       (progn
;;;###autoload         (load "vc-jj" nil t)
;;;###autoload         (vc-jj-registered file))))
(defun vc-jj-registered (file)
  "Check whether FILE is registered with jj."
  (and-let* ((vc-jj-program (executable-find vc-jj-program))
             (default-directory (vc-jj-root file)))
    (with-temp-buffer
      (and (= 0 (call-process vc-jj-program nil (list t nil) nil
                              "file" "list" "--" (vc-jj--filename-to-fileset file)))
           (/= (point-min) (point-max))))))

(defun vc-jj-state (file)
  "JJ implementation of `vc-state' for FILE."
  ;; We need to run two commands for the complete state:
  ;;
  ;; - "jj file list -T 'conflict' FILE" gets us conflicted (output
  ;;   "true"), ignored (no output) or tracked (output "false", but
  ;;   could be added or modified)
  ;;
  ;; - "jj diff --summary FILE" gets us modified (output starts with
  ;;   "M ") or added (output starts with "A "), but no output could
  ;;   be conflicted, ignored or unchanged
  (let* ((default-directory (vc-jj-root file))
         (file (vc-jj--filename-to-fileset file))
         (conflicted-ignored
          (vc-jj--command-parseable "file" "list" "-T" "conflict" "--" file))
         (modified-added
          (vc-jj--command-parseable "diff" "--summary" "--" file)))
    (cond
     ((string-empty-p conflicted-ignored) 'ignored)
     ((string= conflicted-ignored "true") 'conflict)
     ((string-prefix-p "M " modified-added) 'edited)
     ((string-prefix-p "A " modified-added) 'added)
     ((string= conflicted-ignored "false") 'up-to-date)
     (t nil))))

(defun vc-jj-dir-status-files (dir _files update-function)
  "Calculate a list of (FILE STATE EXTRA) entries for DIR.
Return the result of applying UPDATE-FUNCTION to that list."
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
           (registered-files (vc-jj--process-lines "file" "list" "--" dir))
           (ignored-files (seq-difference (cl-delete-if #'file-directory-p
                                                        (directory-files dir nil nil t))
                                          registered-files))
           (changed (vc-jj--process-lines "diff" "--summary" "--" dir))
           (added-files (mapcan (lambda (entry)
                                  (and (string-prefix-p "A " entry)
                                       (list (substring entry 2))))
                                changed))
           (modified-files (mapcan (lambda (entry)
                                     (and (string-prefix-p "M " entry)
                                          (list (substring entry 2))))
                                   changed))
           ;; The command below only prints conflicted files in DIR, but
           ;; relative to project-root, hence the dance with
           ;; expand-file-name / file-relative-name
           (conflicted-files (mapcar (lambda (entry)
                                       (file-relative-name (expand-file-name entry project-root) dir))
                                     (vc-jj--process-lines "file" "list"
                                                           "-T" "if(conflict, path ++ \"\\n\")" "--" dir)))
           (unchanged-files (cl-remove-if (lambda (entry) (or (member entry conflicted-files)
                                                              (member entry modified-files)
                                                              (member entry added-files)
                                                              (member entry ignored-files)))
                                          registered-files))
           (result
            (nconc (mapcar (lambda (entry) (list entry 'conflict)) conflicted-files)
                   (mapcar (lambda (entry) (list entry 'added)) added-files)
                   (mapcar (lambda (entry) (list entry 'edited)) modified-files)
                   (mapcar (lambda (entry) (list entry 'ignored)) ignored-files)
                   (mapcar (lambda (entry) (list entry 'up-to-date)) unchanged-files))))
      (funcall update-function result nil))))

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

(defun vc-jj-working-revision (file)
  "Return the current change id of the repository containing FILE."
  (when-let* ((default-directory (vc-jj-root file)))
    ;; 'jj log' might print a warning at the start of its output,
    ;; e.g., "Warning: Refused to snapshot some files".  The output we
    ;; want will be printed afterwards.
    (car (last (vc-jj--process-lines "log" "--no-graph"
                                     "-r" "@"
                                     "-T" "change_id ++ \"\\n\"")))))

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

(defun vc-jj-create-repo ()
  "Create an empty jj repository in the current directory."
  (if current-prefix-arg
      (call-process vc-jj-program nil nil nil "git" "init" "--colocate")
    (call-process vc-jj-program nil nil nil "git" "init")))

(defun vc-jj-register (files &optional _comment)
  "Register FILES into the jj version-control system."
  ;; This is usually a no-op since jj auto-registers all files, so we
  ;; just need to run some jj command so new files are picked up.  We
  ;; run "jj file track" for the case where some of FILES are excluded
  ;; via the "snapshot.auto-track" setting or via git's mechanisms
  ;; such as the .gitignore file.
  (vc-jj--command-dispatched nil 0 files "file" "track" "--"))

(defun vc-jj-delete-file (file)
  "Delete FILE and make sure jj registers the change."
  (when (file-exists-p file)
    (delete-file file)
    (vc-jj--command-dispatched nil 0 nil "status")))

(defun vc-jj-rename-file (old new)
  "Rename file OLD to NEW and make sure jj registers the change."
  (rename-file old new)
  (vc-jj--command-dispatched nil 0 nil "status"))

(defun vc-jj-checkin (files comment &optional _rev)
  "Run \"jj commit\" with supplied FILES and COMMENT."
  (setq comment (replace-regexp-in-string "\\`Summary: " "" comment))
  (let ((args (append (vc-switches 'jj 'checkin) (list "commit" "-m" comment))))
    (apply #'vc-jj--command-dispatched nil 0 files args)))

(defun vc-jj-find-revision (file rev buffer)
  "Read revision REV of FILE into BUFFER and return the buffer."
  (let ((revision (vc-jj--command-parseable "file" "show" "-r" rev "--" (vc-jj--filename-to-fileset file))))
    (with-current-buffer buffer
      (erase-buffer buffer)
      (insert revision)))
  buffer)

(defun vc-jj-checkout (file &optional rev)
  "Restore the contents of FILE to be the same as in change REV.
If REV is not specified, revert the file as with `vc-jj-revert'."
  ;; TODO: check that this does the right thing: if REV is not
  ;; specified, should we revert or leave FILE unchanged?
  (let ((args (append (and rev (list "--from" rev)))))
    (apply #'vc-jj--command-dispatched nil 0 file "restore" args)))

(defun vc-jj-revert (file &optional _contents-done)
  "Restore FILE to the state from its parent(s), via \"jj restore\"."
  (vc-jj--command-dispatched nil 0 file "restore"))

(defun vc-jj-print-log (files buffer &optional _shortlog start-revision limit)
  "Print commit log associated with FILES into specified BUFFER."
  ;; FIXME: limit can be a revision string, in which case we should
  ;; print revisions between start-revision and limit
  (vc-setup-buffer buffer)
  (let ((inhibit-read-only t)
        (args (append
               (and limit
                    (list "-n" (number-to-string limit)))
               (if start-revision
                 (list "-r" (concat "::" start-revision))
                 (list "-r" "::"))
               (list "-T" vc-jj--log-default-template "--")
               (unless (string-equal (vc-jj-root (car files)) (car files))
                 files))))
    (with-current-buffer buffer
      (apply #'vc-jj--command-dispatched buffer
        'async nil "log" args))))

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

;; (defun vc-jj-log-outgoing (buffer remote-location)
;;   ;; TODO
;;   )
;; (defun vc-jj-log-incoming (buffer remote-location)
;;   ;; TODO
;;   )

(defvar log-view-message-re)
(defvar log-view-file-re)
(defvar log-view-font-lock-keywords)
(defvar log-view-per-file-logs)
(defvar log-view-expanded-log-entry-function)

(defvar vc-jj--logline-re
  (rx
    line-start
    ;; graph
    (+? nonl)
    " "
    ;; change-id
    (group (+ (any "K-Zk-z")))
    space
    (group (+ (any "K-Zk-z")))
    " "
    ;; author
    (group (* nonl))
    " "
    ;; time
    (group
      (= 4 (any num)) "-" (= 2 (any num)) "-" (= 2 (any num)))
      ;; " "
      ;; (= 2 (any num)) ":" (= 2 (any num)) ":" (= 2 (any num)))
    ;; tags, bookmarks, commit-id
    (group
      (*? nonl))

    " "
    ;; commit-id
    (group
      (+ (any hex)))
    " "
    ;; special states
    (? (group (or (: "(empty) ")
                "")))
    (? (group (or (: "(no description set)" (* nonl))
                "")))
    ;; regular description
    (* nonl)
    line-end))

(define-derived-mode vc-jj-log-view-mode log-view-mode "JJ-Log-View"
  (require 'add-log) ;; We need the faces add-log.
  ;; Don't have file markers, so use impossible regexp.
  (setq-local log-view-file-re regexp-unmatchable)
  (setq-local log-view-per-file-logs nil)
  (setq-local log-view-message-re vc-jj--logline-re)
  ;; Allow expanding short log entries.
  (setq truncate-lines t)
  (setq-local log-view-expanded-log-entry-function
    'vc-jj-expanded-log-entry)
  (setq-local log-view-font-lock-keywords
    `((,vc-jj--logline-re
        (1 'log-view-message)
        (2 'change-log-list)
        (3 'change-log-name)
        (4 'change-log-date)
        (5 'change-log-file)
        (6 'change-log-list)
        (7 'change-log-function)
        (8 'change-log-function))))

  (keymap-set vc-jj-log-view-mode-map "r" #'vc-jj-edit-change)
  (keymap-set vc-jj-log-view-mode-map "x" #'vc-jj-abandon-change)
  (keymap-set vc-jj-log-view-mode-map "i" #'vc-jj-new-change))


(defun vc-jj-expanded-log-entry (revision)
  (with-temp-buffer
    (vc-jj--command-dispatched t 0 nil
      "log"
      "-r" revision
      "-T" "builtin_log_detailed"
      "--color" "never"
      "--no-graph"
      "--")
    (buffer-string)))

(defun vc-jj-previous-revision (file rev)
  "JJ-specific version of `vc-previous-revision'."
  ;; TODO: here and in `vc-jj-next-revision', check how (concat
  ;; revision "-") works with merge commits; do we get multiple change
  ;; ids in a multi-line string, and does this break users of these
  ;; functions?
  (if file
      (vc-jj--command-parseable "log" "--no-graph" "-n" "1"
                                "-r" (concat ".." rev "-")
                                "-T" "change_id.shortest()"
                                "--" (vc-jj--filename-to-fileset file))
    (vc-jj--command-parseable "log" "--no-graph" "-n" "1"
                              "-r" (concat rev "-")
                              "-T" "change_id.shortest()"
                              )))

(defun vc-jj-next-revision (file rev)
  "JJ-specific version of `vc-next-revision'."
  (if file
      (vc-jj--command-parseable "log" "--no-graph" "-n" "1"
                                "-r" (format "roots(files(\"%s\") ~ ::%s)" file rev)
                                "-T" "change_id.shortest()")
    (vc-jj--command-parseable "log" "--no-graph" "-n" "1"
                              "-r" (concat rev "+")
                              "-T" "change_id.shortest()")))

(defun vc-jj-get-change-comment (_files rev)
  (vc-jj--command-parseable "log" "--no-graph" "-n" "1"
                            "-r" rev "-T" "description"))

(declare-function log-edit-mode "log-edit" ())
(declare-function log-edit-toggle-header "log-edit" (header value))
(declare-function log-edit-extract-headers "log-edit" (headers string))
(declare-function log-edit--toggle-amend "log-edit" (last-msg-fn))

;; TODO: protect immutable changes
(defun vc-jj-modify-change-comment (_files rev comment)
  (let ((args (log-edit-extract-headers () comment)))
    (vc-jj--command-dispatched nil 0 nil "desc" rev "-m" (pop args) "--quiet")))

(defun vc-jj--reload-log-buffers ()
  (and vc-parent-buffer
    (with-current-buffer vc-parent-buffer
      (revert-buffer)))
  (revert-buffer))

(defun vc-jj-edit-change ()
  (interactive)
  (let ((rev (log-view-current-tag)))
    (vc-jj-retrieve-tag nil rev nil)
    (vc-jj--reload-log-buffers)))

(defun vc-jj-abandon-change ()
  (interactive)
  ;; TODO: should probably ask for confirmation, although this would be
  ;; different from the cli
  (let ((rev (log-view-current-tag)))
    (vc-jj--command-dispatched nil 0 nil "abandon" rev "--quiet")
    (vc-jj--reload-log-buffers)))

(defun vc-jj-new-change ()
  (interactive)
  (let ((rev (log-view-current-tag)))
    (vc-jj--command-dispatched nil 0 nil "new" rev "--quiet")
    (vc-jj--reload-log-buffers)))

(defun vc-jj-root (file)
  "Return the root of the repository containing FILE.
Return NIL if FILE is not in a jj repository."
  ;; `default-directory' must be an absolute directory name ending
  ;; with a slash, so we go to some lengths to guarantee this, even
  ;; for e.g. (vc-jj-root ".")
  (let* ((absolute-file (expand-file-name file))
         (default-directory
          (file-name-as-directory (if (file-directory-p absolute-file)
                                      absolute-file
                                    (file-name-directory absolute-file)))))
    (with-temp-buffer
      (when (= 0 (call-process vc-jj-program nil (list t nil) nil "root"))
        (file-name-as-directory (buffer-substring (point-min) (1- (point-max))))))))

(defalias 'vc-jj-responsible-p #'vc-jj-root)

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

(defun vc-jj-diff (files &optional rev1 rev2 buffer _async)
  "Display diffs for FILES between revisions REV1 and REV2."
  ;; TODO: handle async
  (setq buffer (get-buffer-create (or buffer "*vc-diff*")))
  (cond
   ((not (or rev1 rev2))
    (setq rev1 "@-"))
   ((null rev1)
    (setq rev1 "root()")))
  (setq rev2 (or rev2 "@"))
  (let ((inhibit-read-only t)
        (args (append (vc-switches 'jj 'diff) (list "--") files)))
    (with-current-buffer buffer
      (erase-buffer))
    (apply #'call-process vc-jj-program nil buffer nil "diff" "--from" rev1 "--to" rev2 args)
    (if (seq-some (lambda (line) (string-prefix-p "M " line))
                  (apply #'vc-jj--process-lines "diff" "--summary" "--" files))
        1
      0)))

(defun vc-jj-annotate-command (file buf &optional rev)
  "Fill BUF with per-line commit history of FILE at REV."
  (let ((rev (or rev "@"))
        (file (file-relative-name file)))
    ;; Contrary to most other jj commands, 'jj file annotate' takes a
    ;; path instead of a fileset expression, so we append FILE to the
    ;; unprocessed argument list here.
    (apply #'vc-jj--command-dispatched buf 'async nil "file" "annotate"
           (append (vc-switches 'jj 'annotate)
                   (list "-r" rev file)))))

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
The regex matches each line's commit information and captures
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

(defun vc-jj-annotate-extract-revision-at-line ()
  "Return the revision (change id) for the annotated line."
  (save-excursion
    (beginning-of-line)
    (when (looking-at vc-jj--annotation-line-prefix-re)
      (match-string-no-properties 1))))

(defun vc-jj-revision-completion-table (files)
  (let ((revisions
         (apply #'vc-jj--process-lines "log" "--no-graph"
                "-T" "self.change_id() ++ \"\\n\"" "--" files)))
    (lambda (string pred action)
      (if (eq action 'metadata)
          `(metadata . ((display-sort-function . ,#'identity)))
        (complete-with-action action revisions string pred)))))

(defun vc-jj-retrieve-tag (_dir rev _update)
  "Call jj edit on REV inside DIR.
REV is the change ID of a jj revision.

_DIR and _UPDATE are as described in the vc.el specification."
  (vc-jj--command-dispatched nil 0 nil "edit" rev "--quiet"))

(defvar vc-jj-pull-history nil
  "History variable for `vc-jj-pull'.")

(defun vc-jj-pull (prompt)
  "Pull changes from an upstream repository, invoked via \\[vc-update].
Normally, this runs \"jj git fetch\".  If PROMPT is non-nil, prompt for
the jj command to run."
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

(defvar vc-jj-push-history nil
  "History variable for `vc-jj-push'.")

(defun vc-jj-push (prompt)
  "Push changes to an upstream repository, invoked via \\[vc-push].
Normally, this runs \"jj git push\".  If PROMPT is non-nil, prompt for
the command to run, e.g., the semi-standard \"jj git push -c @-\"."
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

(defun vc-jj--set-up-process-buffer (buffer root command)
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
                        nil))))
  (vc-set-async-update buffer))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.jjdescription\\'" . log-edit-mode))

(provide 'vc-jj)
;;; vc-jj.el ends here

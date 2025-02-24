;;; vc-jj.el --- VC backend for the Jujutsu version control system -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Wojciech Siewierski

;; Author: Wojciech Siewierski
;;         Rudolf Schlatte <rudi@constantly.at>
;; URL: https://codeberg.org/emacs-jj-vc/vc-jj.el
;; Version: 0.1
;; Package-Requires: ((emacs "25.1") (compat "29.4"))
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

(defcustom vc-jj-colorize-log t
  "Control whether to have jj colorize the log."
  :type 'boolean)

(defcustom vc-jj-log-template "builtin_log_compact"
  "The template to use for `vc-print-log'."
  :type '(radio (const "builtin_log_oneline")
                (const "builtin_log_compact")
                (const "builtin_log_comfortable")
                (const "builtin_log_compact_full_description")
                (const "builtin_log_detailed")
                (string :tag "Custom template")))

(defun vc-jj--call-jj (&rest args)
  "Call `vc-jj-program' with ARGS.
Return T if the process exited successfully (with exit status 0),
NIL otherwise."
  (= 0 (apply #'call-process vc-jj-program nil t nil args)))

(defun vc-jj-command (buffer okstatus file-or-list &rest flags)
  ""
  (apply #'vc-do-command (or buffer "*vc*") okstatus vc-jj-program
    file-or-list
    (append (list "--no-pager" "--color" "never")
      flags)))

(defun vc-jj--file-tracked (file)
  (with-temp-buffer
    (and (vc-jj--call-jj "file" "list" "--" file)
         (not (= (point-min) (point-max))))))

(defun vc-jj--file-modified (file)
  (with-temp-buffer
    (and (vc-jj--call-jj "diff" "--summary" "--" file)
         (not (= (point-min) (point-max)))
         (progn (goto-char (point-min)) (looking-at-p "M ")))))

(defun vc-jj--file-added (file)
  (with-temp-buffer
    (and (vc-jj--call-jj "diff" "--summary" "--" file)
         (not (= (point-min) (point-max)))
         (progn (goto-char (point-min)) (looking-at-p "A ")))))

(defun vc-jj--file-conflicted (file)
  (with-temp-buffer
    (and (vc-jj--call-jj "resolve" "--list" "--" file)
         (not (= (point-min) (point-max)))
         (progn (goto-char (point-min)) (looking-at-p file)))))

(defun vc-jj--file-state (file)
  (let ((s (with-output-to-string
             (vc-jj-command standard-output 0 file
               "diff" "--summary" "--" file))))
    (when s
      ;; for [T]racked. returns nothing cleanly if file has no changes
      (if (zerop (length s))
        "T"
        ;; HACK: if the file is not tracked, it doesnt error and still returns
        ;; 0, but prints an error msg, which starts with "Warning...". it wont
        ;; match M or A, which means it's untracked. Ugly, but saves us from
        ;; running multiple commands for every file
        (substring s 0 1)))))

;;;###autoload (defun vc-jj-registered (file)
;;;###autoload   "Return non-nil if FILE is registered with jj."
;;;###autoload   (if (and (vc-find-root file ".jj")   ; Short cut.
;;;###autoload            (executable-find "jj"))
;;;###autoload       (progn
;;;###autoload         (load "vc-jj" nil t)
;;;###autoload         (vc-jj-registered file))))

(defun vc-jj-registered (file)
  (when (executable-find vc-jj-program)
    (unless (not (file-exists-p default-directory))
      (with-demoted-errors "Error: %S"
        (when-let* ((root (vc-jj-root file)))
          (let* ((default-directory root)
                 (relative (file-relative-name file)))
            (vc-jj--file-tracked relative)))))))

(defun vc-jj-state (file)
  "JJ implementation of `vc-state' for FILE."
  (when-let* ((default-directory (vc-jj-root file))
              (relative (file-relative-name file))
              (state (vc-jj--file-state relative)))
    (cond
       ((vc-jj--file-conflicted relative)
        'conflict)
       ((equal "M" state)
        'edited)
       ((equal "A" state)
        'added)
       (state
        'up-to-date)
       (t nil))))

(defun vc-jj-dir-status-files (dir _files update-function)
  "Calculate a list of (FILE STATE EXTRA) entries for DIR.
The list is passed to UPDATE-FUNCTION."
  ;; TODO: could be async!
  (let* ((dir (expand-file-name dir))
         ;; TODO: Instead of the two `mapcan' calls, it should be more
         ;; efficient to write the output to a buffer and then search
         ;; for lines beginning with A or M, pushing them into a list.
         (changed-files (process-lines vc-jj-program "diff" "--summary" "--" dir))
         (added (mapcan (lambda (file) (and (string-prefix-p "A " file)
                                            (list (substring file 2))))
                        changed-files))
         (modified (mapcan (lambda (file) (and (string-prefix-p "M " file)
                                               (list (substring file 2))))
                           changed-files))
         (files (mapcan (lambda (file) (list (substring file 2))) changed-files))
         ;; The output of `jj resolve --list' is a list of file names
         ;; plus a free-text conflict description per line -- rather
         ;; than trying to be fancy and parsing each line (and getting
         ;; bugs with file names with spaces), use `string-prefix-p'
         ;; later.  Note that 'jj resolve' errors when there are no
         ;; conflicts, which is harmless.
         (conflicted (process-lines-ignore-status vc-jj-program "resolve" "--list")))
    (let ((result
           (mapcar
            (lambda (file)
              (let ((vc-state
                     (cond ((seq-find
                             (apply-partially #'string-prefix-p file) conflicted)
                            'conflict)
                           ((member file added) 'added)
                           ((member file modified) 'edited)
                           (t 'up-to-date))))
                (list file vc-state)))
            files)))
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
  (pcase-let* ((default-directory (file-name-as-directory dir))
               (`( ,change-id ,change-id-short ,commit-id ,commit-id-short
                   ,description ,bookmarks ,conflict ,divergent ,hidden ,immutable)
                (process-lines vc-jj-program "log" "--no-graph" "-r" "@" "-T"
                               "concat(
self.change_id().short(), \"\\n\",
self.change_id().shortest(), \"\\n\",
self.commit_id().short(), \"\\n\",
self.commit_id().shortest(), \"\\n\",
description.first_line(), \"\\n\",
bookmarks.join(\",\"), \"\\n\",
self.conflict(), \"\\n\",
self.divergent(), \"\\n\",
self.hidden(), \"\\n\",
self.immutable(), \"\\n\"
)"))
               (status (concat
                        (and (string= conflict "true") "(conflict)")
                        (and (string= divergent "true") "(divergent)")
                        (and (string= hidden "true") "(hidden)")
                        (and (string= immutable "true") "(immutable)")))
               (change-id-suffix (substring change-id (length change-id-short)))
               (commit-id-suffix (substring commit-id (length commit-id-short))))
    (cl-flet ((fmt (key value &optional prefix)
                (concat
                 (propertize (format "% -11s: " key) 'face 'vc-dir-header)
                 ;; there is no header value emphasis face, so we use
                 ;; vc-dir-status-up-to-date for the prefix.
                 (and prefix (propertize prefix 'face 'vc-dir-status-up-to-date))
                 (propertize value 'face 'vc-dir-header-value))))
      (string-join (seq-remove
                    #'null
                    (list
                     (fmt "Description" (if (string= description "") "(no description set)" description))
                     (fmt "Change ID" change-id-suffix change-id-short)
                     (fmt "Commit" commit-id-suffix commit-id-short)
                     (unless (string= bookmarks "") (fmt "Bookmarks" bookmarks))
                     (unless (string= status "")
                       ;; open-code this line instead of adding a
                       ;; `face' parameter to `fmt'
                       (concat
                        (propertize (format "% -11s: " "Status") 'face 'vc-dir-header)
                        (propertize status 'face 'vc-dir-status-warning)))))
                   "\n"))))

(defun vc-jj-working-revision (file)
  "Return the current change id of the repository containing FILE."
  (when-let* ((default-directory (vc-jj-root file)))
    (car (process-lines vc-jj-program "log" "--no-graph"
                        "-r" "@"
                        "-T" "change_id ++ \"\\n\""))))

(defun vc-jj-mode-line-string (file)
  "Return a mode line string and tooltip for FILE."
  (pcase-let* ((long-rev (vc-jj-working-revision file))
               (`(,short-rev ,description)
                (process-lines vc-jj-program "log" "--no-graph" "-r" long-rev
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
  (apply #'vc-jj--call-jj "file" "track" files))

(defun vc-jj-delete-file (file)
  "Delete the file and make sure jj registers the change."
  (when (file-exists-p file)
    (delete-file file)
    (vc-jj--call-jj "status")))

(defun vc-jj-rename-file (old new)
  "Rename file OLD to NEW and make sure jj registers the change."
  (rename-file old new)
  (vc-jj--call-jj "status"))

(defun vc-jj-checkin (files comment &optional _rev)
  "Runs \"jj commit\" with supplied FILES and COMMENT."
  (setq comment (replace-regexp-in-string "\\`Summary: " "" comment))
  (let ((args (append (vc-switches 'jj 'checkin) (list "--") files)))
    (apply #'call-process vc-jj-program nil nil nil "commit" "-m" comment args)))

(defun vc-jj-find-revision (file rev buffer)
  "Read REVISION of FILE into a buffer and return the buffer."
  (call-process vc-jj-program nil buffer nil "file" "show" "-r" rev "--" file))

(defun vc-jj-checkout (file &optional rev)
  "Restore the contents of FILE to be the same as in change REV.
If REV is not specified, revert the file as with `vc-jj-revert'."
  ;; TODO: check that this does the right thing: if REV is not
  ;; specified, should we revert or leave FILE unchanged?
  (let ((args (append (and rev (list "--from" rev))
                      (list "--" file))))
    (apply #'vc-jj--call-jj "restore" args)))

(defun vc-jj-revert (file &optional _contents-done)
  "Restore FILE to the state from its parent(s), via \"jj restore\"."
  (call-process vc-jj-program nil nil nil "restore" "--" file))

(defun vc-jj-print-log (files buffer &optional _shortlog start-revision limit)
  "Print commit log associated with FILES into specified BUFFER."
  ;; FIXME: limit can be a revision string, in which case we should
  ;; print revisions between start-revision and limit
  (let ((inhibit-read-only t)
        (args (append
               (and limit
                    (list "-n" (number-to-string limit)))
               (and start-revision
                    (list "-r" (concat ".." start-revision)))
               (and vc-jj-colorize-log (list "--color" "always"))
               (list "-T" vc-jj-log-template "--")
               files)))
    (with-current-buffer buffer (erase-buffer))
    (apply #'call-process vc-jj-program nil buffer nil "log" args)
    (when vc-jj-colorize-log
      (with-current-buffer buffer
        (ansi-color-apply-on-region (point-min) (point-max)))))
  (goto-char (point-min)))

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
    (vc-jj--call-jj "file" (if remove "track" "untrack") file)))

(defvar vc-jj-diff-switches '("--git"))

(defun vc-jj-diff (files &optional rev1 rev2 buffer _async)
  "Display diffs for FILES between two revisions."
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
    (if (seq-some #'vc-jj--file-modified files)
        1
      0)))

(defun vc-jj-annotate-command (file buf &optional rev)
  "Fill BUF with per-line commit history of FILE at REV."
  (let ((rev (or rev "@"))
        (file (file-relative-name file)))
    (apply #'vc-jj-command buf 'async nil "file" "annotate"
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
         (apply #'process-lines
                vc-jj-program "log" "--no-graph"
                "-T" "self.change_id() ++ \"\\n\"" "--" files)))
    (lambda (string pred action)
      (if (eq action 'metadata)
          `(metadata . ((display-sort-function . ,#'identity)))
        (complete-with-action action revisions string pred)))))


(provide 'vc-jj)
;;; vc-jj.el ends here

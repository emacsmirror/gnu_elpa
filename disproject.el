;;; disproject.el --- Dispatch project commands with Transient  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 aurtzy
;; Copyright (C) 2008-2023 The Magit Project Contributors
;; Copyright (C) 2015-2024 Free Software Foundation, Inc.

;; Author: aurtzy <aurtzy@gmail.com>
;; URL: https://github.com/aurtzy/disproject
;; Keywords: convenience, project
;; Package-Version: 0.3.0

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

;; Disproject is a `transient.el' interface that provides integration with
;; `project.el' for managing and interacting with projects.  It is similar to
;; (and inspired by) the function `project-switch-project', but attempts to make
;; it more convenient than just a Transient-ified version.

;;; Code:

(require 'grep)
(require 'pcase)
(require 'map)
(require 'project)
(require 'transient)

;;;
;;; Macros.
;;;

(defmacro disproject--with-environment (&rest body)
  "Run BODY with `disproject' \"environment\" options set."
  ;; Define variables that determine the environment.
  `(let ((from-directory (or (disproject--root-directory) default-directory))
         (prefer-other-window (disproject--prefer-other-window))
         ;; Only enable envrc if the initial environment has it enabled.
         (enable-envrc (and (bound-and-true-p envrc-mode)
                            (symbol-function 'envrc-mode)))
         ;; HACK: Since `project-external-roots' targets specifically the
         ;; current buffer's major mode - a problem, since we create a temp
         ;; buffer - we make it work by grabbing the function that it's supposed
         ;; to return (i.e. `project-vc-external-roots-function') before
         ;; entering the temp buffer, and then restoring it.  This won't be
         ;; needed once `project.el' supports project-wide external roots.
         (external-roots-function project-vc-external-roots-function))
     (with-temp-buffer
       (let ((default-directory from-directory)
             ;; This handles edge cases with `project' commands.
             (project-current-directory-override from-directory)
             (display-buffer-overriding-action
              (and prefer-other-window '(display-buffer-use-some-window
                                         (inhibit-same-window t))))
             (project-vc-external-roots-function external-roots-function))
         ;; Make sure commands are run in the correct direnv environment
         ;; if envrc-mode is enabled.
         (when enable-envrc (funcall enable-envrc))
         ,@body))))


;;;
;;; Global variables.
;;;

(defgroup disproject nil
  "Transient interface for managing and interacting with projects."
  :group 'convenience
  :group 'project)

(defcustom disproject-compile-suffixes '(("c" "make" "make -k"
                                          :description "Make  %s"))
  "Commands for the `disproject-compile' prefix.

The value should be a list of transient-like specification
entries (KEY IDENTIFIER COMPILE-COMMAND {PROPERTY VALUE} ...).

KEY is the keybind that will be used in the Transient menu.

IDENTIFIER is used in the compilation buffer name.  This should
be unique, but it may be useful to use the same identifier as
another command if one wants certain project compilation commands
as incompatible (only one runs at a given time).

COMPILE-COMMAND is passed to `compile' as the shell command to
run.

Optional properties can be set after COMPILE-COMMAND through
keywords.

:description is the only valid property.  It is used as the
transient command description.  If a \"%s\" is present in the
description, it will be substituted with COMPILE-COMMAND.
Otherwise, if the property is not specified, COMPILE-COMMAND is
used in place of the entire description.

For example, the following may be used as a dir-locals.el value
for `disproject-compile-suffixes' to add \"make -k\" and
\"guile --help\" in a particular project:

  ((\"m\" \"make\"
    \"echo Running make...; make -k\"
    :description \"Make\")
   (\"g\" \"guile-help\"
    \"echo Get some help from Guile...; guile --help\"
    :description \"/Compile/ some help from Guile!\")))"
  :type '(repeat (list (string :tag "Key bind")
                       (string :tag "Identifier")
                       (string :tag "Shell command")
                       (plist :inline t
                              :tag "Properties"
                              :key-type (const :description)
                              :value-type string)))
  :group 'disproject)

(defcustom disproject-find-file-command #'project-find-file
  "The command used for opening a file in a project.

This is called whenever the function `disproject-find-file' is
invoked."
  :type 'function
  :group 'disproject)

(defcustom disproject-find-regexp-command #'project-find-regexp
  "The command used for finding regexp matches in a project.

This is called whenever the function `disproject-find-regexp' is
invoked."
  :type 'function
  :group 'disproject)

(defcustom disproject-or-external-find-file-command
  #'project-or-external-find-file
  "The command used to find a file in a project or its external roots.

This is called whenever the function
`disproject-or-external-find-file' is invoked."
  :type 'function
  :group 'disproject)

(defcustom disproject-or-external-find-regexp-command
  #'project-or-external-find-regexp
  "The command used to find regexp matches in a project or its external roots.

This is called whenever the function
`disproject-or-external-find-file' is invoked."
  :type 'function
  :group 'disproject)

(defcustom disproject-shell-command #'project-eshell
  "The command used for opening a shell in a project.

This is called whenever the function `disproject-shell-command'
is invoked."
  :type 'function
  :group 'disproject)

(defcustom disproject-switch-to-buffer-command #'project-switch-to-buffer
  "The command used for switching project buffers.

This is called whenever the function
`disproject-switch-to-buffer' is invoked."
  :type 'function
  :group 'disproject)


;;;
;;; Prefixes.
;;;

;;;###autoload (autoload 'disproject "disproject")
(transient-define-prefix disproject-dispatch (&optional directory)
  "Dispatch some command for a project.

DIRECTORY is an optional argument that tells `disproject' where
to start searching first for a project directory root; otherwise,
it moves on to `default-directory'.  If no project is found, it
starts the menu anyways to explicitly ask later when a command is
executed or when --root-directory is manually set."
  :refresh-suffixes t
  [:description
   (lambda ()
     (format (propertize "Project: %s" 'face 'transient-heading)
             (if-let ((directory (disproject--root-directory t)))
                 (propertize directory 'face 'transient-value)
               (propertize "None detected" 'face 'transient-inapt-suffix))))
   ("p" "Switch project" disproject:--root-directory)
   ("P" "Switch to active project" disproject:only-active--root-directory)]
  ["Options"
   ("o" "Prefer other window" "--prefer-other-window")]
  ["Commands"
   :pad-keys t
   [("b" "Switch buffer" disproject-switch-to-buffer)
    ("B" "Buffer list" disproject-list-buffers)
    ("c" "Compile" disproject-compile)
    ("D" "Dired" disproject-dired)
    ("k" "Kill buffers" disproject-kill-buffers)]
   [("s" "Shell" disproject-shell)
    ("v" "VC dir" disproject-vc-dir)
    ("!" "Run" disproject-shell-command)
    ("M-x" "Extended command" disproject-execute-extended-command)]
   ["Find..."
    ("f" "File" disproject-find-file)
    ("F" "File (+external)" disproject-or-external-find-file)
    ("g" "Regexp" disproject-find-regexp)
    ("G" "Regexp (+external)" disproject-or-external-find-regexp)]]
  ;; This section should consist of project-dependent commands that could be
  ;; disabled, allowing for it to be completely omitted in the menu if no
  ;; additional commands are applicable.
  ["Additional commands"
   ["Magit"
    ;; Needs :refresh-suffixes t since it depends on infix "--root-directory="
    :if (lambda () (and (featurep 'magit)
                        (funcall (symbol-function 'magit-git-repo-p)
                                 (or (disproject--root-directory t)
                                     default-directory))))
    ("m" "Status" disproject-magit-status)
    ("T" "Todos" disproject-magit-todos-list
     :if (lambda () (featurep 'magit-todos)))]]
  (interactive)
  (transient-setup
   'disproject-dispatch nil nil
   :scope `((root-directory . ,(or directory (disproject--root-directory t))))))

(transient-define-prefix disproject-compile (&optional directory)
  "Dispatch compilation commands.

This prefix can be configured with `disproject-compile-suffixes'."
  ["Compile"
   :class transient-column
   :setup-children disproject-compile--setup-suffixes]
  (interactive)
  (transient-setup
   'disproject-compile nil nil
   :scope `((root-directory . ,(or directory (disproject--root-directory))))))


;;;
;;; Transient state handling.
;;;

(defun disproject--active-projects ()
  "Return a list of active known projects, i.e. those with open buffers."
  (let* ((buffer-list
          ;; Ignore ephemeral buffers
          (seq-filter (lambda (buf)
                        (not (string-prefix-p " " (buffer-name buf))))
                      (buffer-list)))
         (deduplicated-buffer-list
          (cl-remove-duplicates (mapcar
                                 (lambda (buf)
                                   (buffer-local-value 'default-directory buf))
                                 buffer-list)
                                :test #'equal)))
    (seq-mapcat (lambda (directory)
                  (if-let ((project (project-current nil directory)))
                      (list (project-root project))))
                deduplicated-buffer-list)))

(defun disproject--find-root-directory (directory &optional silent)
  "Attempt to find project root directory from DIRECTORY.  May return nil.

A message is printed if no root directory can be found.  SILENT
may be set to a non-nil value to suppress it."
  (if-let ((directory (directory-file-name (file-truename directory)))
           (project (project-current nil directory))
           (root-directory (project-root project)))
      (progn
        (project-remember-project project)
        root-directory)
    (unless silent
      (message "No parent project found for %s"
               directory))
    nil))

(defun disproject--scope (key &optional no-alist?)
  "Get `disproject' scope.

By default, this function assumes that the scope is an alist.
KEY is the key used to get the alist value.  If NO-ALIST? is
non-nil, the scope will be treated as a value of any possible
type and directly returned instead, ignoring KEY."
  ;; Just return nil instead of signaling an error if there is no prefix.
  (if-let (((transient-prefix-object))
           (scope (transient-scope)))
      (if no-alist? scope (alist-get key scope))))

;;;; Infix classes.

(defclass disproject-option-switches (transient-switches)
  ()
  "Class used for a set of switches where exactly one is selected.")

(cl-defmethod transient-infix-read ((obj disproject-option-switches))
  "Cycle through mutually exclusive switch options from OBJ.

This method skips over nil, so exactly one switch of this object
is always selected."
  (let ((choices (mapcar (apply-partially #'format (oref obj argument-format))
                         (oref obj choices))))
    (if-let ((value (oref obj value))
             (next-value (cadr (member value choices))))
        next-value
      (car choices))))

;;;; Infixes.

(transient-define-infix disproject:--root-directory ()
  "Root directory of the project to execute commands from.

Uses `project-prompt-project-dir' to switch project root directories."
  :class transient-option
  :argument "--root-directory="
  ;; Value already shown elsewhere.
  :format " %k %d"
  :init-value (lambda (obj)
                (oset obj value (disproject--scope 'root-directory)))
  :always-read t
  :reader (lambda (&rest _ignore)
            ;; Don't set anything if either of these are nil.
            (if-let ((new-root-directory (disproject--find-root-directory
                                          (project-prompt-project-dir)))
                     (scope (disproject--scope nil t)))
                ;; Update --root-directory in Transient scope to keep it in sync
                (setf (alist-get 'root-directory scope) new-root-directory)
              new-root-directory)))

(transient-define-infix disproject:only-active--root-directory ()
  "Root directory of the project to execute commands from.

This is equivalent to `disproject:--root-directory' and is backed
by the same \"--root-directory=\" argument value, but only shows
active projects when prompting for projects to switch to."
  :class transient-option
  :argument "--root-directory="
  ;; Value already shown elsewhere.
  :format " %k %d"
  :always-read t
  :reader (lambda (&rest _ignore)
            (if-let* ((new-root-directory
                       (completing-read "Select active project: "
                                        (project--file-completion-table
                                         ;; Follow the format of
                                         ;; `project--list'.
                                         (mapcar #'list
                                                 (disproject--active-projects)))
                                        nil
                                        t))
                      (scope (disproject--scope nil t)))
                ;; Update --root-directory in Transient scope to keep it in sync
                (setf (alist-get 'root-directory scope) new-root-directory)
              new-root-directory)))

;;;; Transient state getters.

(defun disproject--prefer-other-window ()
  "Return whether other window should be preferred when displaying buffers."
  (let ((args (transient-args transient-current-command)))
    (and args (transient-arg-value "--prefer-other-window" args))))

(defun disproject--root-directory (&optional no-prompt? directory)
  "Return the project root directory defined in transient arguments.

Prefer searching DIRECTORY for a project root first, if set.
Otherwise, use the current Transient prefix's arguments.  If
those are also not available, try the Transient scope.
`default-directory' is searched if none of these methods find a
root directory.  As a fallback, the function may prompt for a
project to use.

DIRECTORY is used to search for the project, and is preferred if
it is set.

If NO-PROMPT? is non-nil, no prompts will be made if a root
directory can be found, and this function may return nil."
  ;; `project-current' only remembers project when maybe-prompt?  is true, but
  ;; this function will opt to always remember instead so it can show up in
  ;; the "Switch projects" prompt.
  (or (if-let ((directory)
               (project (project-current nil directory)))
          (prog1 (project-root project)
            (project-remember-project project)))
      (if-let ((args (transient-args transient-current-command)))
          (transient-arg-value "--root-directory=" args))
      (disproject--scope 'root-directory)
      (if no-prompt?
          (if-let ((project (project-current nil)))
              (prog1 (project-root project)
                (project-remember-project project)))
        (project-root (project-current t)))))


;;;
;;; Suffixes.
;;;

(transient-define-suffix disproject-dired ()
  "Open Dired in project root."
  (interactive)
  (disproject--with-environment
   (call-interactively #'dired)))

(transient-define-suffix disproject-execute-extended-command ()
  "Execute an extended command in project root."
  (interactive)
  (disproject--with-environment
   (call-interactively #'execute-extended-command)))

(transient-define-suffix disproject-find-file ()
  "Find file in project."
  (interactive)
  (disproject--with-environment
   (call-interactively disproject-find-file-command)))

(transient-define-suffix disproject-find-regexp ()
  "Search project for regexp."
  (interactive)
  (disproject--with-environment
   (call-interactively disproject-find-regexp-command)))

(transient-define-suffix disproject-kill-buffers ()
  "Kill all buffers related to project."
  (interactive)
  (disproject--with-environment
   (call-interactively #'project-kill-buffers)))

(transient-define-suffix disproject-list-buffers ()
  "Display a list of open buffers for project."
  (interactive)
  (disproject--with-environment
   (call-interactively #'project-list-buffers)))

(transient-define-suffix disproject-magit-status ()
  "Open the Magit status buffer for project."
  (interactive)
  (declare-function magit-status-setup-buffer "magit-status")
  (disproject--with-environment
   (magit-status-setup-buffer)))

(transient-define-suffix disproject-magit-todos-list ()
  "Open a `magit-todos-list' buffer for project."
  (interactive)
  (declare-function magit-todos-list-internal "magit-todos")
  (disproject--with-environment
   (magit-todos-list-internal default-directory)))

(transient-define-suffix disproject-or-external-find-file ()
  "Find file in project or external roots."
  (interactive)
  (disproject--with-environment
   (call-interactively disproject-or-external-find-file-command)))

(transient-define-suffix disproject-or-external-find-regexp ()
  "Find regexp in project or external roots."
  (interactive)
  (disproject--with-environment
   (call-interactively disproject-or-external-find-regexp-command)))

(transient-define-suffix disproject-shell ()
  "Start a shell in project."
  (interactive)
  (disproject--with-environment
   (call-interactively disproject-shell-command)))

(transient-define-suffix disproject-shell-command ()
  "Run a shell command asynchronously in a project."
  (interactive)
  (disproject--with-environment
   (call-interactively #'async-shell-command)))

(defun disproject-compile--setup-suffixes (_)
  "Set up suffixes according to `disproject-compile-suffixes'."
  (transient-parse-suffixes
   'disproject-compile
   `(,@(mapcar
        (pcase-lambda (`( ,key ,identifier ,compile-command
                          . ,(map :description)))
          `(,key
            ,(format (or description "%s")
                     (propertize compile-command
                                 'face
                                 'transient-value))
            (lambda ()
              (interactive)
              (disproject--with-environment
               (let* ((compilation-buffer-name-function
                       (lambda (major-mode-name)
                         (project-prefixed-buffer-name
                          (concat ,identifier "-" major-mode-name)))))
                 (compile ,compile-command))))))
        (with-temp-buffer
          (let ((default-directory (disproject--root-directory)))
            (hack-dir-local-variables-non-file-buffer)
            disproject-compile-suffixes)))
     ("!"
      "Alternative command..."
      (lambda ()
        (interactive)
        (disproject--with-environment
         (call-interactively #'compile)))))))

(transient-define-suffix disproject-switch-to-buffer ()
  "Switch to buffer in project."
  (interactive)
  (disproject--with-environment
   (call-interactively disproject-switch-to-buffer-command)))

(transient-define-suffix disproject-vc-dir ()
  "Run VC-Dir in project."
  (interactive)
  (disproject--with-environment
   (call-interactively #'vc-dir)))

(provide 'disproject)
;;; disproject.el ends here

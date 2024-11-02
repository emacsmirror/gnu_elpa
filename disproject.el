;;; disproject.el --- Dispatch project commands with Transient  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 aurtzy
;; Copyright (C) 2008-2023 The Magit Project Contributors
;; Copyright (C) 2015-2024 Free Software Foundation, Inc.

;; Author: aurtzy <aurtzy@gmail.com>
;; URL: https://github.com/aurtzy/disproject
;; Keywords: convenience, files, vc

;; Package-Version: 0.6.0
;; Package-Requires: ((emacs "29.4") (transient "0.7.7"))

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
;; (and inspired by) the function `project-switch-project', but also attempts to
;; improve on its feature set in addition to the use of Transient.

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
  "Run BODY with `disproject' \"environment\" options set.

The \"environment\" consists of the following overrides:

`default-directory', `project-current-directory-override': Set to
the project's root directory.

`display-buffer-overriding-action': Set to display in another
window if \"--prefer-other-window\" is enabled."
  ;; Define variables that determine the environment.
  `(let ((from-directory (disproject--root-directory))
         (prefer-other-window? (disproject--prefer-other-window?))
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
              (and prefer-other-window? '(display-buffer-use-some-window
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

(defcustom disproject-compile-suffixes '(("c" "Make  %s" "make -k"
                                          :identifier "make"))
  "Commands for the `disproject-compile' prefix.

The value should be a list of transient-like specification
entries (KEY DESCRIPTION COMPILE-COMMAND {PROPERTY VALUE} ...).

KEY is the keybind that will be used in the Transient menu.

DESCRIPTION is used as the Transient command description.  If a
\"%s\" is present in the description, it will be substituted with
COMPILE-COMMAND.

COMPILE-COMMAND is passed to `compile' as the shell command to
run.

Optional properties can be set after COMPILE-COMMAND through
keywords.

:identifier is the only valid property.  It defaults to the first
word in the description (or \"default\" if none are found).  This
should be unique as it is used in the compilation buffer name,
but it may be useful to use the same identifier as another
command if one wants certain project compilation commands to be
incompatible (enforcing only one runs at a given time).

For example, the following may be used as a dir-locals.el value
for `disproject-compile-suffixes' to add \"make -k\" and
\"guile --help\" in a particular project:

  ((\"m\" \"Make\"
    \"echo Running make...; make -k\"
    :identifier \"make\")
   (\"g\" \"/Compile/ some help from Guile!\"
    \"echo Get some help from Guile...; guile --help\"
    :identifier \"guile-help\")))"
  :type '(repeat (list (string :tag "Key bind")
                       (string :tag "Description")
                       (string :tag "Shell command")
                       (plist :inline t
                              :tag "Properties"
                              :key-type (const :identifier)
                              :value-type string)))
  :group 'disproject)

(defcustom disproject-custom-suffixes '()
  "Custom project commands for the `disproject-dispatch' prefix.

This is meant to be set per-project, usually via directory-local
variables.

The value should be a suffix command or a group specification as
defined by `transient-parse-suffixes'.

For example, to evaluate some code when \"N\" is pressed,
something like the following may be specified:

  ((\"N\" \"Frobnicate a foo\"
    (lambda ()
      (interactive)
      (message \"Frobnicated a foo!\"))))

Consider wrapping code in the `disproject--with-environment' form
so commands that should respect the menu options can do so (like
preferring other buffer, or running from the project's root
directory)."
  :type 'sexp
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
;;; Prefix handling.
;;;

(defun disproject--setup-scope (&optional write-scope? directory force-init?)
  "Set up Transient scope for a Disproject prefix.

When WRITE-SCOPE? is t, overwrite the current Transient scope
with the return value.

DIRECTORY is passed to `disproject--root-directory' as a
\"preferred search directory\".

FORCE-INIT? is an internal hack.  If it is t, it indicates that
the scope should be set up for a completely new Transient,
ignoring the previous Transient state.

The specifications for the scope returned is an alist with keys
and descriptions of their values as follows:

\\='default-root-directory: the project root directory of
`default-directory' (or the current buffer, in other words).

\\='root-directory: the current project root directory selected
in the Transient menu.

\\='project: the project associated with \\='root-directory.

\\='git-repository?: whether the currently selected project is
a git repository."
  ;; FIXME: Transient scope can contain the previous `disproject-dispatch'
  ;; invocation's root directory (specifically after invoking a suffix), so
  ;; don't use `disproject--root-directory' if FORCE-INIT? is t.  This also
  ;; applies to `disproject-compile', but since this would break scope between
  ;; it and `project-dispatch', this shouldn't used for the sub-prefix (but it's
  ;; less likely to be called on its own anyways, so is probably able to do
  ;; without the hack).
  ;;
  ;; Upstream issue: https://www.github.com/magit/transient/issues/323
  (let* ((default-root-directory
          (if-let* ((project (project-current nil default-directory)))
              (project-root project)))
         (root-directory
          (if force-init?
              default-root-directory
            (disproject--root-directory nil directory)))
         (project (project-current nil root-directory))
         (git-repository?
          (and (featurep 'magit)
               root-directory
               (funcall (symbol-function 'magit-git-repo-p) root-directory)))
         (dir-local-variables (with-temp-buffer
                                (when-let* ((root-directory)
                                            (default-directory root-directory))
                                  (hack-dir-local-variables)
                                  dir-local-variables-alist)))
         (compile-suffixes
          (alist-get 'disproject-compile-suffixes dir-local-variables))
         (custom-suffixes
          (alist-get 'disproject-custom-suffixes dir-local-variables))
         (new-scope
          `((default-root-directory . ,default-root-directory)
            (root-directory . ,root-directory)
            (project . ,project)
            (git-repository? . ,git-repository?)
            (compile-suffixes . ,compile-suffixes)
            (custom-suffixes . ,custom-suffixes))))
    (if-let* ((write-scope?)
              (scope (disproject--scope nil t)))
        (seq-each (pcase-lambda (`(,key . ,value))
                    (setf (alist-get key scope) value))
                  new-scope))
    new-scope))

;;;; Prefixes.

;;;###autoload (autoload 'disproject-dispatch "disproject")
(transient-define-prefix disproject-dispatch (&optional directory)
  "Dispatch some command for a project.

DIRECTORY is an optional argument that tells the function where
to start searching first for a project directory root; otherwise,
it uses the default root directory if available.  If no project
is found, it starts the menu anyways to explicitly ask later when
a command is executed or when invoking one of the switch-project
commands."
  :refresh-suffixes t
  [:description
   (lambda ()
     (format (propertize "Project: %s" 'face 'transient-heading)
             (if-let* ((directory (disproject--root-directory t)))
                 (propertize directory 'face 'transient-value)
               (propertize "None detected" 'face 'transient-inapt-suffix))))
   ("p" "Switch project" disproject-switch-project
    :transient t)
   ("P" "Switch to active project" disproject-switch-project-active
    :transient t)]
  ["Options"
   ("o" "Prefer other window" "--prefer-other-window")]
  ["Commands"
   :pad-keys t
   [("b" "Switch buffer" disproject-switch-to-buffer)
    ("B" "Buffer list" disproject-list-buffers)
    ("c" "Compile" disproject-compile)
    ("D" "Dired" disproject-dired)]
   [("k" "Kill buffers" disproject-kill-buffers)
    ("s" "Shell" disproject-shell)
    ("!" "Run" disproject-shell-command)
    ("M-x" "Extended command" disproject-execute-extended-command)]
   ["Find"
    ("f" "file" disproject-find-file)
    ("F" "file (+external)" disproject-or-external-find-file)
    ("g" "regexp" disproject-find-regexp)
    ("G" "regexp (+external)" disproject-or-external-find-regexp)]]
  ;; This section may contain commands that are dynamically enabled/disabled
  ;; depending on the chosen project.  This requires :refresh-suffixes to be t.
  [["Version control"
    :if (lambda () (nth 1 (disproject--project)))
    ("v d" "Magit dispatch" magit-dispatch
     :if (lambda () (and (featurep 'magit) (disproject--git-repository?)))
     :inapt-if-not disproject--root-directory-is-default?)
    ("v f" "Magit file dispatch" magit-file-dispatch
     :if (lambda () (and (featurep 'magit) (disproject--git-repository?)))
     :inapt-if-not disproject--root-directory-is-default?)
    ("v m" "Magit status" disproject-magit-status
     :if (lambda () (and (featurep 'magit) (disproject--git-repository?))))
    ("v t" "Magit todos" disproject-magit-todos-list
     :if (lambda () (and (featurep 'magit-todos)
                         (disproject--git-repository?))))
    ("v v" "VC dir" disproject-vc-dir)]
   ["Custom commands"
    :class transient-column
    :setup-children disproject--setup-custom-suffixes]]
  [("SPC" "Manage projects" disproject-manage-projects)]
  (interactive)
  (transient-setup
   'disproject-dispatch nil nil
   :scope (disproject--setup-scope nil directory t)))

(transient-define-prefix disproject-compile (&optional directory)
  "Dispatch compilation commands.

This prefix can be configured with `disproject-compile-suffixes'."
  ["Compile"
   :class transient-column
   :setup-children disproject-compile--setup-suffixes]
  (interactive)
  (transient-setup
   'disproject-compile nil nil
   :scope (disproject--setup-scope nil directory)))

(transient-define-prefix disproject-manage-projects (&optional directory)
  "Dispatch commands for managing projects.

DIRECTORY will be searched for the project if passed."
  ["Forget"
   ;; TODO: Could add an option to close buffers of the project to forget.
   ("f p" "a project" disproject-forget-project)
   ("f u" "projects under..." disproject-forget-projects-under)
   ("f z" "zombie projects" disproject-forget-zombie-projects)]
  ["Remember"
   ("r a" "active projects" disproject-remember-projects-active)
   ("r u" "projects under..." disproject-remember-projects-under)]
  (interactive)
  (transient-setup
   'disproject-manage-projects nil nil
   :scope (disproject--setup-scope nil directory)))


;;;
;;; Transient state handling.
;;;

(defun disproject--active-projects ()
  "Return a list of active known projects, i.e. those with open buffers."
  (let* ((buffer-list
          ;; Ignore ephemeral and star buffers
          (match-buffers (lambda (buf)
                           (let ((name (buffer-name buf)))
                             (not (or (string-prefix-p " " name)
                                      (string-prefix-p "*" name)))))))
         (directories
          (cl-remove-duplicates (mapcar
                                 (lambda (buf)
                                   (buffer-local-value 'default-directory buf))
                                 buffer-list)
                                :test #'equal)))
    (cl-remove-duplicates
     (seq-mapcat (lambda (directory)
                   (if-let* ((project (project-current nil directory)))
                       (list project)))
                 directories)
     :test (lambda (p1 p2) (equal (project-root p1) (project-root p2))))))

(defun disproject--scope (key &optional no-alist?)
  "Get `disproject' scope.

By default, this function assumes that the scope is an alist.
KEY is the key used to get the alist value.  If NO-ALIST? is
non-nil, the scope will be treated as a value of any possible
type and directly returned instead, ignoring KEY."
  ;; Just return nil instead of signaling an error if there is no prefix.
  (if-let* (((transient-prefix-object))
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
    (if-let* ((value (oref obj value))
              (next-value (cadr (member value choices))))
        next-value
      (car choices))))

;;;; Infixes.

;;;; Transient state getters.

(defun disproject--compile-suffixes ()
  "Return the `disproject-compile' suffixes for this scope."
  (disproject--scope 'compile-suffixes))

(defun disproject--custom-suffixes ()
  "Return the `disproject-dispatch' custom suffixes for this scope."
  (disproject--scope 'custom-suffixes))

(defun disproject--default-root-directory ()
  "Return the current caller's (the one setting up Transient) root directory."
  (disproject--scope 'default-root-directory))

(defun disproject--git-repository? ()
  "Return if project is a Git repository."
  ;; Index 1 contains the project backend; see
  ;; `project-vc-backend-markers-alist'.
  (eq (nth 1 (disproject--scope 'project)) 'Git))

(defun disproject--prefer-other-window? ()
  "Return whether other window should be preferred when displaying buffers."
  (let ((args (transient-args transient-current-command)))
    (and args (transient-arg-value "--prefer-other-window" args))))

(defun disproject--project ()
  "Return the project from the current Transient scope."
  (disproject--scope 'project))

(defun disproject--root-directory (&optional no-prompt? directory)
  "Return the project root directory defined in transient arguments.

Prefer searching DIRECTORY for a project root first, but only if
it is set.  Otherwise, use the current Transient prefix's
arguments.  If those are also not available, try the Transient
scope.  Fall back to searching default root directory at this
point if DIRECTORY is not set.  The function may prompt for a
project to use if all of these cannot find a project root.

DIRECTORY is used to search for the project, and is prioritized
when it is set.

If NO-PROMPT? is non-nil, no prompts will be made if a root
directory can be found, and this function may return nil."
  ;; `project-current' only remembers project when maybe-prompt?  is true, but
  ;; this function will opt to always remember instead so it can show up in
  ;; the "Switch projects" prompt.
  (let ((find-project-root
         (lambda (no-prompt? dir)
           (if no-prompt?
               (if-let* ((project (project-current nil dir)))
                   (prog1 (project-root project)
                     (project-remember-project project)))
             (project-root (project-current t dir))))))
    (or
     ;; If DIRECTORY is set, prioritize searching it first without prompting.
     (if directory (funcall find-project-root t directory))
     ;; Scope.
     (disproject--scope 'root-directory)
     ;; Use current root directory if DIRECTORY wasn't set.  Prompt as a
     ;; fallback if it is permitted and all else fails.
     (if directory
         (if (not no-prompt?)
             (funcall find-project-root nil directory))
       (let ((directory (disproject--scope 'default-root-directory)))
         (if no-prompt?
             (funcall find-project-root t directory)
           (funcall find-project-root nil directory)))))))

(defun disproject--root-directory-is-default? ()
  "Return whether the project is the same as the default project."
  (equal (disproject--scope 'default-root-directory)
         (disproject--scope 'root-directory)))


;;;
;;; Suffix handling.
;;;

(defun disproject--switch-project (search-directory)
  "Modify the Transient scope to switch to another project.

Look for a valid project root directory in SEARCH-DIRECTORY.  If
one is found, update the root-directory key in Transient scope to
the new directory."
  (if-let* ((directory (disproject--root-directory nil search-directory)))
      (disproject--setup-scope t directory)
    (if directory
        (error "No scope available")
      (message "No parent project found for %s" search-directory))))

;;;; Suffix setup functions.

(defun disproject--setup-custom-suffixes (_)
  "Set up suffixes according to `disproject-custom-suffixes'."
  (transient-parse-suffixes
   'disproject-dispatch
   (disproject--custom-suffixes)))

(defun disproject-compile--setup-suffixes (_)
  "Set up suffixes according to `disproject-compile-suffixes'."
  (transient-parse-suffixes
   'disproject-compile
   `(,@(mapcar
        (pcase-lambda (`( ,key ,description ,compile-command
                          . ,(map :identifier)))
          `(,key
            ,(format description
                     (propertize compile-command 'face 'transient-value))
            (lambda ()
              (interactive)
              (disproject--with-environment
               (let* ((compilation-buffer-name-function
                       (lambda (major-mode-name)
                         (project-prefixed-buffer-name
                          (concat ,(or identifier
                                       (and
                                        (string-match "\\(\\w+\\)" description)
                                        (match-string 1 description))
                                       "default")
                                  "-" major-mode-name)))))
                 (compile ,compile-command))))))
        (disproject--compile-suffixes))
     ("!"
      "Alternative command..."
      (lambda ()
        (interactive)
        (disproject--with-environment
         (let ((compilation-buffer-name-function
                (lambda (major-mode-name)
                  (project-prefixed-buffer-name
                   (concat "default-" major-mode-name)))))
           (call-interactively #'compile))))))))

;;;; Suffixes.

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

(transient-define-suffix disproject-forget-project ()
  "Forget a project."
  (interactive)
  (call-interactively #'project-forget-project))

(transient-define-suffix disproject-forget-projects-under ()
  "Forget projects under a directory."
  (interactive)
  (call-interactively #'project-forget-projects-under))

(transient-define-suffix disproject-forget-zombie-projects ()
  "Forget zombie projects."
  (interactive)
  (call-interactively #'project-forget-zombie-projects))

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

(transient-define-suffix disproject-remember-projects-active ()
  "Remember active projects."
  (interactive)
  (let ((active-projects (disproject--active-projects)))
    (seq-each (lambda (project)
                (project-remember-project project t))
              active-projects)
    (project--write-project-list)))

(transient-define-suffix disproject-remember-projects-under ()
  "Remember projects under a directory."
  (interactive)
  (call-interactively #'project-remember-projects-under))

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

(transient-define-suffix disproject-switch-project ()
  "Switch project to dispatch commands on.

Uses `project-prompt-project-dir' to switch project root
directories."
  (interactive)
  (disproject--switch-project (project-prompt-project-dir)))

(transient-define-suffix disproject-switch-project-active ()
  "Switch to an active project to dispatch commands on.

This is equivalent to `disproject-switch-project' but only shows
active projects when prompting for projects to switch to."
  (interactive)
  (let* ((active-projects (mapcar (lambda (project)
                                    ;; Keep reference to project so we can
                                    ;; project-remember the project chosen.
                                    (cons (project-root project) project))
                                  (disproject--active-projects)))
         ;; `project--file-completion-table' seems to accept any collection as
         ;; defined by `completing-read'.
         (completion-table (project--file-completion-table active-projects))
         (project-directory (completing-read "Select active project: "
                                             completion-table nil t))
         (project (alist-get project-directory active-projects nil nil #'equal)))
    (project-remember-project project)
    (disproject--switch-project project-directory)))

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

;;; disproject.el --- Dispatch project commands with Transient  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 aurtzy
;; Copyright (C) 2008-2023 The Magit Project Contributors
;; Copyright (C) 1985-1987, 1992-2024 Free Software Foundation, Inc.

;; Author: aurtzy <aurtzy@gmail.com>
;; URL: https://github.com/aurtzy/disproject
;; Keywords: convenience, files, vc

;; Version: 1.1.0
;; Package-Requires: ((emacs "29.4") (transient "0.7.8"))

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

(defmacro disproject-with-environment (&rest body)
  "Run BODY with `disproject' \"environment\" options set.

The \"environment\" consists of the following overrides:

`default-directory', `project-current-directory-override': Set to
the project's root directory.

`display-buffer-overriding-action': Set to display in another
window if \"--prefer-other-window\" is enabled."
  (declare (indent 0) (debug t))
  `(let* ((project (disproject--state-project-ensure))
          (from-directory (project-root project))
          (prefer-other-window? (disproject--state-prefer-other-window?))
          ;; Only enable envrc if the initial environment has it enabled.
          (enable-envrc (and (bound-and-true-p envrc-mode)
                             (symbol-function 'envrc-mode)))
          ;; Only enable mise if the initial environment has it enabled.
          (enable-mise (and (bound-and-true-p mise-mode)
                            (symbol-function 'mise-mode)))
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
         ;; Make sure commands are run in the correct mise environment
         ;; if mise-mode is enabled.
         (when enable-mise (funcall enable-mise))
         ,@body))))


;;;
;;; Global variables.
;;;

(defgroup disproject nil
  "Transient interface for managing and interacting with projects."
  :group 'convenience
  :group 'project)

(defgroup disproject-commands nil
  "Customizable Disproject main dispatch commands.

See documentation on `disproject-with-environment' for the
variables it sets according to the menu settings.  These must be
respected (when relevant) by commands.  For example, a command
will not always run in the project root directory if it does not
respect `default-directory' or
`project-current-directory-override', which the macro sets."
  :group 'disproject)

(defcustom disproject-custom-allowed-suffixes '()
  "Allowed values for `disproject-custom-suffixes'."
  :risky t
  :group 'disproject
  :type '(alist :key-type (string :tag "Project path")
                :value-type (repeat (sexp :tag "Custom suffix"))))

(defcustom disproject-custom-suffixes '(("c" "Make"
                                         :command-type compile
                                         :command "make -k"
                                         :identifier "make"))
  "Commands for the `disproject-custom-dispatch' prefix.

The custom dispatch was initially designed for suites of
compilation commands, but it is flexible enough to be used for
any custom command designated for a particular project, like
starting servers or updates.

The value should be a list of transient-like specification
entries (KEY DESCRIPTION {PROPERTY VALUE} ...).

KEY is the key-bind that will be used in the Transient menu.  Key
sequences starting with alphanumeric characters (regexp
\"[a-zA-Z0-9]\") are reserved for the user.

DESCRIPTION is used as the Transient command description.

The following properties are required:

`:command' is an s-expression which is evaluated and used
depending on the command type `:command-type'.

`:command-type' is a symbol that specifies what to do with the
value of `:command'.  It can be any of the following keys:

  bare-call: the value is called as an interactive function from
  the current buffer.  This is the only command type that is not
  automatically run in the environment provided by
  `disproject-with-environment'.

  call: the value will be called as an interactive function.

  compile: the value of `:command' should be a string or an
  interactive function that returns a string that will be passed
  to `compile' as the shell command to run.

When using the \\='bare-call or \\='call command types, consider
using the variable `disproject-process-buffer-name' (available
when evaluating `:command') as the buffer name for processes to
enable tracking e.g. process state.

Some optional properties may be set as well:

`:identifier' is used as part of the buffer name, and should be
unique to the command.  It defaults to the result of applying
`disproject-process-buffer-name' to the value (or first word in
the description if not specified).  Users may choose to set the
same identifier for multiple commands to mark them as
incompatible (only one can run at a given time).  This relies on
commands like `compile' which notify the user that a buffer with
the same name already has a process running.

To illustrate usage of `disproject-custom-suffixes', for
example, the following may be used as a dir-locals.el value for
some project to add \"make -k\" and \"guile --help\" as compile
commands and some custom `find-file' call commands:

  ((\"m\" \"Make\"
    :command-type compile
    :command \"echo Running make...; make -k\"
    :identifier \"make\")
   (\"h\" \"Get help from a program (if it supports --help)\"
    :command-type compile
    :command (lambda (program)
               (interactive \"sProgram: \")
               (concat program \" --help\"))
    :identifier \"guile-help\")
   (\"f\" \"Find a file\"
    :command-type call
    :command #\\='find-file)
   (\"F\" \"Announce the finding a file\"
    :command-type call
    :command (lambda ()
               (interactive)
               (message \"FINDING A FILE!\")
               (call-interactively #\\='find-file))))

This variable is marked safe due to various reasons discussed in
`disproject-custom--suffixes-allowed?'.  Prompts are deferred to
the mentioned function, called when setting up the custom
dispatch menu.  Non-default values must still be explicitly
allowed by the user - this may be unsafe if unconditionally
evaluated."
  :safe #'always
  :type '(repeat (list (string :tag "Key bind")
                       (string :tag "Description")
                       (plist :inline t
                              :tag "Properties"
                              :key-type (choice (const :command-type)
                                                (const :command)
                                                (const :identifier)))))
  :group 'disproject)

(defcustom disproject-find-dir-command #'project-find-dir
  "Command to find a directory in a project.

This is called whenever the function `disproject-find-dir' is
invoked."
  :type 'function
  :group 'disproject
  :group 'disproject-commands)

(defcustom disproject-find-file-command #'project-find-file
  "Command used for opening a file in a project.

This is called whenever the function `disproject-find-file' is
invoked."
  :type 'function
  :group 'disproject
  :group 'disproject-commands)

(defcustom disproject-find-line-command #'disproject-default-find-line
  "Command to find line occurrences in open project buffers.

This is called whenever the function `disproject-find-line' is
invoked."
  :type 'function
  :group 'disproject
  :group 'disproject-commands)

(defcustom disproject-find-regexp-command #'project-find-regexp
  "Command used for finding regexp matches in a project.

This is called whenever the function `disproject-find-regexp' is
invoked."
  :type 'function
  :group 'disproject
  :group 'disproject-commands)

(defcustom disproject-or-external-find-file-command
  #'project-or-external-find-file
  "Command used to find a file in a project or its external roots.

This is called whenever the function
`disproject-or-external-find-file' is invoked."
  :type 'function
  :group 'disproject
  :group 'disproject-commands)

(defcustom disproject-or-external-find-regexp-command
  #'project-or-external-find-regexp
  "Command used to find regexp matches in a project or its external roots.

This is called whenever the function
`disproject-or-external-find-file' is invoked."
  :type 'function
  :group 'disproject
  :group 'disproject-commands)

(defcustom disproject-shell-command #'project-eshell
  "Command used for opening a shell in a project.

This is called whenever the function `disproject-shell-command'
is invoked."
  :type 'function
  :group 'disproject
  :group 'disproject-commands)

(defcustom disproject-switch-to-buffer-command #'project-switch-to-buffer
  "Command used for switching project buffers.

This is called whenever the function
`disproject-switch-to-buffer' is invoked."
  :type 'function
  :group 'disproject
  :group 'disproject-commands)

;;;; Default commands.

(defun disproject-default-find-line (regexp)
  "Find matching line in buffers associated with the current project.

REGEXP is a regular expression used to search for occurrences.

This uses `multi-occur' under the hood."
  (interactive (list (project--read-regexp)))
  (if-let* ((project (project-current)))
      (multi-occur (project-buffers project) regexp)
    (error "No project in current directory: %s" default-directory)))


;;;
;;; Prefix handling.
;;;

(defun disproject--assert-type (variable value)
  "Assert that VALUE matches the type for custom VARIABLE.

A nil value is returned and a warning is signaled if VALUE does
not match the type.  Otherwise, this function returns a non-nil
value."
  (let ((type (get variable 'custom-type)))
    (if (widget-apply (widget-convert type) :match value)
        t
      ;; The rest of the menu should still be usable if a customizable variable
      ;; type is invalid, so only warn user about invalid types.
      (warn "Value `%S' does not match type %s" value type)
      nil)))

(defun disproject-custom--suffixes-allowed? (project custom-suffixes)
  "Return non-nil if CUSTOM-SUFFIXES for PROJECT has been allowed.

CUSTOM-SUFFIXES should follow the specifications from
`disproject-custom-suffixes'.  PROJECT is the project context in
which the suffixes will be checked.

If CUSTOM-SUFFIXES has not been marked as allowed, a prompt will
be made to temporarily or permanently do so.

Due to the tendency for `disproject-custom-suffixes' to be
tweaked or customized, we use this function with limited history
saving to determine if the suffixes are allowed in order to
prevent `safe-local-variable-values' from increasing drastically
in size.

This does not check other projects, because commands may behave
differently depending on the project (e.g. what \"make\" does for
one project can do something else for another).

Saved history of allowed suffixes is currently not implemented.
Only the most recent custom suffixes (i.e. currently used) are
saved."
  ;; TODO: Add some form of saved history; the
  ;; `disproject-custom-allowed-suffixes' alist value is a list type to allow
  ;; for this.  This implementation only saves a single custom-suffixes element
  ;; to the list at the moment.
  (let* ((root-directory (project-root project))
         (project-suffixes-list (alist-get root-directory
                                           disproject-custom-allowed-suffixes
                                           nil nil #'equal)))
    (or (seq-some (lambda (suffixes)
                    (equal suffixes custom-suffixes))
                  project-suffixes-list)
        (let* ((buf (get-buffer-create "*Custom Suffixes*")))
          (with-current-buffer buf
            (erase-buffer)
            (insert "\
This project has modified local custom suffixes, which may be risky.

Allow the custom suffixes?  You can type
y -- to allow the custom suffixes.
n -- to ignore them and use the default custom suffixes.
! -- to permanently allow the custom suffixes."
                    "\n\n")
            ;; TODO: Replace this pretty print with a custom one to give a more
            ;; uniform layout (e.g. new line for each keyword followed by
            ;; value).  This one is particularly funky with the ":" prefix.
            (cl-prettyprint custom-suffixes)
            (setq-local cursor-type nil)
            (set-buffer-modified-p nil)
            (goto-char (point-min)))
          ;; FIXME: If the user presses an invalid key in the menu after
          ;; answering prompt, the menu does not quit until a valid key is
          ;; pressed (e.g. "C-g" doesn't quit it, but "SPC").
          (save-window-excursion
            (pop-to-buffer buf '(display-buffer-at-bottom))
            (prog1
                (pcase (read-char-choice "Type y, n, or !: " '(?y ?n ?!))
                  (?y t)
                  (?n nil)
                  (?! (progn
                        (setf (alist-get root-directory
                                         disproject-custom-allowed-suffixes
                                         nil nil #'equal)
                              (list custom-suffixes))
                        (customize-push-and-save
                         'disproject-custom-allowed-suffixes
                         disproject-custom-allowed-suffixes)
                        t)))
              (quit-window t)))))))

(defun disproject--selected-project-description ()
  "Return a Transient menu headline to indicate the currently selected project."
  ;; TODO: For some reason, `defconst'-ing a predefined group and using it in
  ;; prefixes is an error; probably doing something wrong.  Investigate since
  ;; there are multiple cases of just declaring
  ;; `[:description disproject--selected-project-description ""]'...
  (format (propertize "Project: %s" 'face 'transient-heading)
          (if-let* ((directory (disproject--state-project-root)))
              (propertize directory 'face 'transient-value)
            (propertize "None detected" 'face 'transient-inapt-suffix))))

(defun disproject--setup-scope (&optional overrides write-scope? prompt-keys)
  "Set up Transient scope for a Disproject prefix.

When WRITE-SCOPE? is non-nil, overwrite the current Transient scope
with the return value.

OVERRIDES should be an alist, with optional key-value entries
where the key corresponds to one in the scope that permits being
overridden (see specifications below).  If a corresponding key is
non-nil, the override value will be used instead, ignoring any
current project state.

PROMPT-KEYS should be a list of keys corresponding to the scope
alist.  If a key is present, it is permitted to invoke prompts.
If a tag is not provided, a default will be used instead.  The
following keys respect this argument: \\='custom-suffixes

The specifications for the scope returned is an alist with keys
and descriptions of their values as follows:

\\='default-project: the project belonging to
`default-directory' (or the current buffer, in other words).  Can
be overridden.

\\='project: the currently selected project in the Transient
menu.  Can be overridden.

\\='prefer-other-window?: whether to prefer another window when
displaying buffers.  Can be overridden.

\\='custom-suffixes: suffixes for `disproject-custom-dispatch',
as described `disproject-custom-suffixes'."
  (let* ((maybe-override ; TODO: maybe this should be a macro?
          (lambda (key value)
            (if-let* ((override (assq key overrides)))
                (cdr override)
              value)))
         (default-project
          (funcall
           maybe-override
           'default-project
           (project-current nil default-directory)))
         (project
          (funcall
           maybe-override
           'project
           (or (disproject--state-project) default-project)))
         (prefer-other-window?
          (funcall
           maybe-override
           'prefer-other-window?
           (disproject--state-prefer-other-window?)))
         (dir-local-variables
          (with-temp-buffer
            (when-let* ((project)
                        (default-directory (project-root project)))
              (hack-dir-local-variables)
              dir-local-variables-alist)))
         (custom-suffixes
          (if-let* (((seq-contains-p prompt-keys 'custom-suffixes #'eq))
                    (dir-local-variables)
                    (suffixes (alist-get 'disproject-custom-suffixes
                                         dir-local-variables))
                    ((disproject--assert-type 'disproject-custom-suffixes
                                              suffixes))
                    ((disproject-custom--suffixes-allowed? project suffixes)))
              suffixes
            (default-value 'disproject-custom-suffixes)))
         (new-scope
          `((default-project . ,default-project)
            (project . ,project)
            (prefer-other-window? . ,prefer-other-window?)
            (custom-suffixes . ,custom-suffixes))))
    ;; Remember the project in case it's not in `project-known-project-roots'.
    (when project (project-remember-project project))
    (if-let* ((write-scope?)
              (scope (disproject--scope nil t)))
        (seq-each (pcase-lambda (`(,key . ,value))
                    (setf (alist-get key scope) value))
                  new-scope))
    new-scope))

;;;; Prefixes.

;;;###autoload (autoload 'disproject-dispatch "disproject" nil t)
(transient-define-prefix disproject-dispatch (&optional project)
  "Dispatch some command for a project.

PROJECT is an optional argument that tells the function what to
start with as the selected project.

See Info node `(transient)Modifying Existing Transients' for
information on inserting user-defined suffix commands to this
menu."
  :refresh-suffixes t
  [:description
   disproject--selected-project-description
   ("p" "Switch project" disproject-switch-project
    :transient t)
   ("P" "Switch to active project" disproject-switch-project-active
    :transient t)
   ("C-p" "Manage projects" disproject-manage-projects-dispatch)]
  ["Options"
   ("o" "Prefer other window" "--prefer-other-window")]
  ["Commands"
   :pad-keys t
   [("b" "Switch buffer" disproject-switch-to-buffer)
    ("B" "Buffer list" disproject-list-buffers)
    ("d" "Dired" disproject-dired)
    ("k" "Kill buffers" disproject-kill-buffers)
    ("l" "Dir-locals file" disproject-dir-locals)
    ("s" "Shell" disproject-shell)]
   [("!" "Run" disproject-shell-command)
    ("M-x" "Extended cmd." disproject-execute-extended-command)]
   ["Find"
    ("D" "directory" disproject-find-dir)
    ("f" "file" disproject-find-file)
    ("F" "file (+external)" disproject-or-external-find-file)
    ("g" "regexp" disproject-find-regexp)
    ("G" "regexp (+external)" disproject-or-external-find-regexp)
    ("L" "line occurrence" disproject-find-line)]]
  ;; This section may contain commands that are dynamically enabled/disabled
  ;; depending on the chosen project.  This requires :refresh-suffixes to be t.
  ;;
  ;; FIXME: There is a case where the section doesn't display when it should.
  ;; This is fixed in Transient commit 0ed009491910f5466ad7f95b4576e9dde7156f4e;
  ;; update dependency when version is available.  Upstream issue context:
  ;; https://www.github.com/magit/transient/issues/327
  [["Version control"
    :if (lambda () (nth 1 (disproject--state-project)))
    ("m" "Magit" disproject-magit-commands-dispatch
     :if (lambda () (and (featurep 'magit) (disproject--state-git-repository?))))
    ("v" "VC status" disproject-vc-dir)]]
  [("SPC" "Custom dispatch" disproject-custom-dispatch
    :transient transient--do-replace)]
  (interactive)
  (transient-setup
   'disproject-dispatch nil nil
   :scope (disproject--setup-scope
           `(,@(if project `((project . ,project)) '())))
   ;; XXX: Preserve options in scope if we're coming from another Disproject
   ;; Transient.  `:refresh-suffixes' being true causes the `:init-value'
   ;; function to be called every refresh which messes up --prefer-other-window,
   ;; so that can't be used.
   :value `(,@(if (disproject--state-prefer-other-window?)
                  '("--prefer-other-window")))))

(transient-define-prefix disproject-custom-dispatch (&optional project)
  "Dispatch custom suffix commands.

If non-nil, PROJECT is used as the project to dispatch custom
commands for.

This prefix can be configured with `disproject-custom-suffixes';
see its documentation for more information.

Suffixes have an associated buffer that is tracked for command
process activity; this is shown in the menu in the form of
\"[CHAR]\", where the string is color-coded and CHAR is a single
character.  These characters represent the following states:

  [a]: Command is active.
  [i]: Command is inactive."
  :refresh-suffixes t
  [:description disproject--selected-project-description ""]
  ["Custom suffix commands"
   :class transient-column
   :pad-keys t
   :setup-children disproject-custom--setup-suffixes]
  [("SPC" "Main dispatch" disproject-dispatch
    :transient transient--do-replace)]
  (interactive)
  (transient-setup
   'disproject-custom-dispatch nil nil
   :scope (disproject--setup-scope
           `((project . ,(or project (disproject--state-project-ensure))))
           nil '(custom-suffixes))))

(transient-define-prefix disproject-magit-commands-dispatch ()
  "Dispatch Magit-related commands for a project.

DIRECTORY will be searched for the project if passed.

Some commands may not be available if the selected project is not
the same as the default (current buffer) one."
  [:description disproject--selected-project-description ""]
  ["Magit commands"
   ("d" "Dispatch" magit-dispatch
    :inapt-if-not disproject--state-project-is-default?)
   ("f" "File dispatch" magit-file-dispatch
    :inapt-if-not disproject--state-project-is-default?)
   ("m" "Status" disproject-magit-status)
   ("T" "Todos" disproject-magit-todos-list
    :if (lambda () (featurep 'magit-todos)))]
  (interactive)
  (transient-setup
   'disproject-magit-commands-dispatch nil nil
   :scope (disproject--setup-scope)))

(transient-define-prefix disproject-manage-projects-dispatch (&optional project)
  "Dispatch commands for managing projects.

If PROJECT is non-nil, it overrides the currently selected
project in Transient state (if any)."
  ["New"
   ("n g c" "git clone" magit-clone
    :if (lambda () (featurep 'magit-clone)))
   ("n g c" "git clone" disproject-git-clone-fallback
    :if (lambda () (and (not (featurep 'magit-clone)) (executable-find "git"))))
   ("n g i" "git init" magit-init
    :if (lambda () (featurep 'magit-status)))
   ("n g i" "git init" disproject-git-init-fallback
    :if (lambda () (and (not (featurep 'magit-status)) (executable-find "git"))))]
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
   'disproject-manage-projects-dispatch nil nil
   :scope (disproject--setup-scope
           `(,@(if project `((project . ,project)) '())))))


;;;
;;; Transient state handling.
;;;

(defun disproject--open-projects ()
  "Return a list of projects with open buffers."
  (let* ((buffer-list
          ;; Ignore ephemeral buffers
          (match-buffers (lambda (buf)
                           (not (string-prefix-p " " (buffer-name buf))))))
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
            ;; HACK: Transient commit 4de5812 introduces a change that modifies
            ;; the behavior of `transient-scope' when called without arguments.
            ;; This hack uses the old method of retrieving scope, but ideally we
            ;; should rely on `transient-scope' again.
            (scope (oref (transient-prefix-object) scope)))
      (if no-alist? scope (alist-get key scope))))

;;;; Infix classes.

;;;; Infixes.

;;;; Transient state getters.
;; Functions that query the Transient state should have their names be prefixed
;; with "disproject--state-" to provide unique identifiers that can be searched
;; for.

(defun disproject--state-custom-suffixes ()
  "Return the `disproject-dispatch' custom suffixes for this scope."
  (disproject--scope 'custom-suffixes))

(defun disproject--state-default-project-root ()
  "Return the current caller's (the one setting up Transient) root directory."
  (if-let* ((project (disproject--scope 'default-project)))
      (project-root project)))

(defun disproject--state-git-repository? ()
  "Return if project is a Git repository."
  ;; Index 1 contains the project backend; see
  ;; `project-vc-backend-markers-alist'.
  (eq (nth 1 (disproject--scope 'project)) 'Git))

(defun disproject--state-prefer-other-window? ()
  "Return whether other window should be preferred when displaying buffers."
  (if (eq transient-current-command 'disproject-dispatch)
      (let ((args (transient-args transient-current-command)))
        (and args (transient-arg-value "--prefer-other-window" args)))
    (disproject--scope 'prefer-other-window?)))

(defun disproject--state-project ()
  "Return the project from the current Transient scope."
  (disproject--scope 'project))

(defun disproject--state-project-ensure ()
  "Ensure that there is a selected project and return it.

This checks if there is a selected project in Transient scope,
prompting for the value if needed to meet that expectation.
Sets the Transient state if possible."
  (or (disproject--state-project)
      (if-let* ((directory (project-prompt-project-dir))
                (project (project-current nil directory)))
          (progn
            (disproject--setup-scope `((project . ,project)) t)
            project)
        (error "No project found for directory: %s" directory))))

(defun disproject--state-project-is-default? ()
  "Return whether the selected project is the same as the default project."
  (if-let* ((default-project (disproject--scope 'default-project))
            (project (disproject--scope 'project)))
      (equal (project-root default-project) (project-root project))))

(defun disproject--state-project-root ()
  "Return the selected project's root directory from Transient state."
  (if-let* ((project (disproject--scope 'project)))
      (project-root project)))


;;;
;;; Suffix handling.
;;;

(defun disproject-process-buffer-name (&optional identifier project-dir)
  "Return the selected project's process buffer name associated with IDENTIFIER.

IDENTIFIER is an optional string argument that can be specified
to make the buffer name unique.  If non-nil, \"default\" is used
as the identifier.

PROJECT-DIR is an override value that specifies the project
directory used to construct the buffer name.  If non-nil, it uses
the currently selected project from transient state.

This function is *not* meant to be used like
`project-prefixed-buffer-name', although it is similar in
functionality; the identifier should not be tied to the buffer
mode in any way.  It should be the only means of making a name
unique in the context of a project.  This allows users to track
buffers based on just an identifier and also allow specifying
incompatible commands (e.g. if two commands use the same buffer
name, they should not be allowed to run at the same time)."
  (concat "*"
          (file-name-nondirectory (directory-file-name
                                   (or project-dir
                                       (disproject--state-project-root))))
          "-process|"
          (or identifier "default")
          "*"))

(defun disproject-custom--suffix (spec-entry)
  "Construct and return a suffix to be parsed by `transient-parse-suffixes'.

SPEC-ENTRY is a single entry from the specification described by
`disproject-custom-suffixes'."
  (pcase spec-entry
    (`( ,key ,description
        .
        ,(map :command-type :command :identifier))
     (let* (;; Fall back to description if identifier is not provided.
            ;; Uniqueness is preferred over the name looking nice to prevent
            ;; unintentionally making commands incompatible.
            (identifier (or identifier description))
            (disproject-process-buffer-name (disproject-process-buffer-name
                                             identifier)))
       `(,key
         ,(disproject-custom--suffix-description
           (get-buffer disproject-process-buffer-name)
           description)
         (lambda ()
           (interactive)
           ;; Expose buffer name to the user; see note in
           ;; `disproject-custom-suffixes'.
           (let ((disproject-process-buffer-name ,disproject-process-buffer-name))
             ,(disproject-custom--suffix-command command-type command)
             ;; Auto-refresh menu on command completion
             (when-let* ((buffer (get-buffer disproject-process-buffer-name))
                         (process (get-buffer-process buffer))
                         ((not (advice-function-member-p
                                #'disproject-custom--suffix-refresh-transient
                                (process-sentinel process)))))
               (add-function
                :before (process-sentinel process)
                #'disproject-custom--suffix-refresh-transient)))))))))

(defun disproject-custom--suffix-command (command-type command)
  "Dispatch a command s-expression to be evaluated in a custom suffix.

COMMAND-TYPE is a symbol corresponding to a command type
documented in `disproject-custom-suffixes'.

COMMAND is an yet-to-be-evaluated s-expression which is inserted
appropriately according to the command type."
  (pcase command-type
    ('bare-call
     `(let ((command ,command))
        (cond
         ((commandp command t)
          (call-interactively command))
         (t
          ,(disproject-custom--suffix-command-type-error
            "Not an interactive function" command-type command)))))
    ('call
     `(disproject-with-environment
        (let ((command ,command))
          (cond
           ((commandp command t)
            (call-interactively command))
           (t
            ,(disproject-custom--suffix-command-type-error
              "Not an interactive function" command-type command))))))
    ('compile
     `(disproject-with-environment
        (let* ((compilation-buffer-name-function
                (lambda (&rest _ignore) disproject-process-buffer-name))
               (command ,command))
          (compile (cond
                    ((stringp command)
                     command)
                    ((commandp command t)
                     (let ((result (call-interactively command)))
                       (if (stringp result)
                           result
                         ,(disproject-custom--suffix-command-type-error
                           "Function does not return string"
                           command-type
                           command))))
                    (t
                     ,(disproject-custom--suffix-command-type-error
                       "Not a string or interactive function"
                       command-type
                       command)))))))))

(defun disproject-custom--suffix-command-type-error (message
                                                     command-type
                                                     command)
  "Return an s-expression to evaluate when there is a suffix command type error.

MESSAGE is the message body stating the typing issue.

COMMAND-TYPE is the `:command-type' value declared in the custom
suffix entry specification.

COMMAND is an unevaluated s-expression representing the declared
`:command' value that can be printed as-is for the user."
  `(error "(`%s') %s: %s" ',command-type ,message ',command))

(defun disproject-custom--suffix-description (buffer description)
  "Return an appropriate description for a custom suffix.

BUFFER is the associated custom suffix buffer.  It may be
non-nil.

DESCRIPTION is the custom suffix description as defined by the
user."
  (concat (cond
           ((null buffer)
            "")
           ((get-buffer-process buffer)
            (concat (propertize "[a]" 'face 'transient-enabled-suffix)
                    " "))
           (t
            (concat (propertize "[i]" 'face 'transient-inactive-value)
                    " ")))
          description))

(defun disproject-custom--suffix-refresh-transient (&rest _ignore)
  "Refresh the `disproject-custom-dispatch' transient, if active."
  (when (transient-active-prefix)
    (transient--refresh-transient)))

(defun disproject--switch-project (search-directory)
  "Modify the Transient scope to switch to another project.

Look for a valid project root directory in SEARCH-DIRECTORY.  If
one is found, update the Transient scope to switch the selected
project."
  (if-let* ((project (project-current nil search-directory)))
      (disproject--setup-scope `((project . ,project)) t)
    (error "No parent project found for %s" search-directory)))

;;;; Suffix setup functions.

(defun disproject-custom--setup-suffixes (_)
  "Set up suffixes according to `disproject-custom-suffixes'."
  (transient-parse-suffixes
   'disproject-custom-dispatch
   (mapcar #'disproject-custom--suffix
           `(,@(disproject--state-custom-suffixes)
             ("!" "Alternative compile..."
              :command-type compile
              :command (lambda ()
                         (interactive)
                         (compilation-read-command (eval compile-command))))))))

;;;; Suffixes.

(transient-define-suffix disproject-dired ()
  "Open Dired in project root."
  (interactive)
  (disproject-with-environment
    (call-interactively #'project-dired)))

(transient-define-suffix disproject-dir-locals ()
  "Open `dir-locals-file' in the project root.

If prefix arg is non-nil, open the personal secondary
file (\".dir-locals-2.el\" by default)."
  (interactive)
  (disproject-with-environment
    (find-file (if current-prefix-arg
                   (concat (file-name-base dir-locals-file)
                           "-2."
                           (file-name-extension dir-locals-file))
                 dir-locals-file))))

(transient-define-suffix disproject-execute-extended-command ()
  "Execute an extended command in project root."
  (interactive)
  (disproject-with-environment
    (call-interactively #'execute-extended-command)))

(transient-define-suffix disproject-find-dir ()
  "Find directory in project.

The command used can be customized with
`disproject-find-dir-command'."
  (interactive)
  (disproject-with-environment
    (call-interactively disproject-find-dir-command)))

(transient-define-suffix disproject-find-file ()
  "Find file in project.

The command used can be customized with
`disproject-find-file-command'."
  (interactive)
  (disproject-with-environment
    (call-interactively disproject-find-file-command)))

(transient-define-suffix disproject-find-line ()
  "Find matching line in open project buffers."
  (interactive)
  (disproject-with-environment
    (call-interactively disproject-find-line-command)))

(transient-define-suffix disproject-find-regexp ()
  "Search project for regexp.

The command used can be customized with
`disproject-find-regexp-command'."
  (interactive)
  (disproject-with-environment
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

(transient-define-suffix disproject-git-clone-fallback (repository
                                                        directory
                                                        &optional
                                                        arguments)
  "Git-clone REPOSITORY into DIRECTORY.

ARGUMENTS is \"git clone\" by default.  If prefix arg is non-nil,
prompt to modify ARGUMENTS options.

\" -- <repository> <directory>\" is appended to ARGUMENTS, which
is executed as a shell command.

This command relies on the \"git\" executable being in the
programs path."
  (interactive "GRepository: \nGDirectory: ")
  (let* ((directory (expand-file-name directory))
         (arguments (or arguments
                        (if current-prefix-arg
                            (read-shell-command "git arguments: " "git clone ")
                          "git clone ")))
         (buf-name (disproject-process-buffer-name "git" directory)))
    (async-shell-command (concat arguments " -- " repository " " directory)
                         buf-name)
    (switch-to-buffer-other-window buf-name)
    (setq default-directory directory)))

(transient-define-suffix disproject-git-init-fallback (directory
                                                       &optional
                                                       arguments)
  "Initialize a git repository in DIRECTORY.

ARGUMENTS is \"git init\" by default.  If prefix arg is non-nil,
prompt to modify ARGUMENTS.

\" -- <directory>\" is appended to ARGUMENTS, which is executed
as a shell command.

This command relies on the \"git\" executable being in the
programs path."
  (interactive "GDirectory: ")
  (let* ((directory (expand-file-name directory))
         (arguments (or arguments
                        (if current-prefix-arg
                            (read-shell-command "git arguments: " "git init ")
                          "git init ")))
         (buf-name (disproject-process-buffer-name "git" directory)))
    (async-shell-command (concat arguments " -- " directory) buf-name)
    (switch-to-buffer-other-window buf-name)
    (setq default-directory directory)))

(transient-define-suffix disproject-kill-buffers ()
  "Kill all buffers related to project."
  (interactive)
  (disproject-with-environment
    (call-interactively #'project-kill-buffers)))

(transient-define-suffix disproject-list-buffers ()
  "Display a list of open buffers for project."
  (interactive)
  (disproject-with-environment
    (call-interactively #'project-list-buffers)))

(transient-define-suffix disproject-magit-status ()
  "Open the Magit status buffer for project."
  (interactive)
  (declare-function magit-status-setup-buffer "magit-status")
  (disproject-with-environment
    (magit-status-setup-buffer)))

(transient-define-suffix disproject-magit-todos-list ()
  "Open a `magit-todos-list' buffer for project."
  (interactive)
  (declare-function magit-todos-list-internal "magit-todos")
  (disproject-with-environment
    (magit-todos-list-internal default-directory)))

(transient-define-suffix disproject-or-external-find-file ()
  "Find file in project or external roots.

The command used can be customized with
`disproject-or-external-find-file-command'."
  (interactive)
  (disproject-with-environment
    (call-interactively disproject-or-external-find-file-command)))

(transient-define-suffix disproject-or-external-find-regexp ()
  "Find regexp in project or external roots.

The command used can be customized with
`disproject-or-external-find-regexp-command'."
  (interactive)
  (disproject-with-environment
    (call-interactively disproject-or-external-find-regexp-command)))

(transient-define-suffix disproject-remember-projects-active ()
  "Remember active projects."
  (interactive)
  (when-let* ((open-projects (disproject--open-projects)))
    (seq-each (lambda (project)
                (project-remember-project project t))
              open-projects)
    (project--write-project-list)))

(transient-define-suffix disproject-remember-projects-under ()
  "Remember projects under a directory."
  (interactive)
  (call-interactively #'project-remember-projects-under))

(transient-define-suffix disproject-shell ()
  "Start a shell in project.

The command used can be customized with the variable
`disproject-shell-command'."
  (interactive)
  (disproject-with-environment
    (call-interactively disproject-shell-command)))

(transient-define-suffix disproject-shell-command ()
  "Run a shell command asynchronously in a project."
  (interactive)
  (disproject-with-environment
    (let ((shell-command-buffer-name-async
           (disproject-process-buffer-name "async-shell-command")))
      (call-interactively #'async-shell-command))))

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
  (let* ((open-projects (mapcar #'project-root (disproject--open-projects)))
         ;; `project--file-completion-table' seems to accept any collection as
         ;; defined by `completing-read'.
         (completion-table (project--file-completion-table open-projects))
         (project-directory (completing-read "Select active project: "
                                             completion-table nil t)))
    (disproject--switch-project project-directory)))

(transient-define-suffix disproject-switch-to-buffer ()
  "Switch to buffer in project.

The command used can be customized with
`disproject-switch-to-buffer-command'."
  (interactive)
  (disproject-with-environment
    (call-interactively disproject-switch-to-buffer-command)))

(transient-define-suffix disproject-vc-dir ()
  "Run VC-Dir in project."
  (interactive)
  (disproject-with-environment
    (call-interactively #'vc-dir)))

(provide 'disproject)
;;; disproject.el ends here

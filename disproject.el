;;; disproject.el --- Dispatch project commands with Transient  -*- lexical-binding: t; -*-

;; Copyright (C) 1985-1987, 1992-2026 Free Software Foundation, Inc.

;; Author: Alvin Hsu <aurtzy@gmail.com>
;; Maintainer: Alvin Hsu <aurtzy@gmail.com>
;; URL: https://github.com/aurtzy/disproject
;; Keywords: convenience, files, vc
;; Version: 2.2.0
;; Package-Requires: ((emacs "29.4") (transient "0.9.2"))

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

;; Disproject implements Transient menus for dispatching project-related
;; commands on top of the `project.el' library.  It aims to provide a more
;; featureful version of the `project-switch-project' command, which it is
;; inspired by.  Those who are familiar with Projectile may also find
;; similarities to `projectile-commander'.

;;; Code:


;;;; Initial code.

(require 'cus-edit)
(require 'eieio)
(require 'grep)
(require 'pcase)
(require 'pp)
(require 'map)
(require 'project)
(require 'transient)


;;;; Options.


;;;;; Customization groups.

(defgroup disproject nil
  "Transient interface for managing and interacting with projects."
  :group 'convenience
  :group 'project)

(defgroup disproject-swappable-commands nil
  "Customizable commands in Disproject's main menu.

See documentation on `disproject-with-env-apply' and
`disproject-with-root-apply' for variables that are set according
to transient state (e.g. `default-directory' to the project's
root directory).  These must be respected by custom commands in
order to apply as expected."
  :group 'disproject)


;;;;; Variables.

(defcustom disproject-custom-allowed-suffixes '()
  "Allowed values for `disproject-custom-suffixes'."
  :risky t
  :group 'disproject
  :type '(alist :key-type (string :tag "Project path")
                :value-type (repeat (sexp :tag "Custom suffix"))))

(defcustom disproject-custom-suffixes '(("c" "Make" disproject-compile
                                         :cmd "make -k"
                                         :buffer-id "make"))
  "Commands for the `disproject-custom-dispatch' prefix.

This variable provides a way to declare per-project suffixes like
common compilation commands.  Usually, this is done through
the `dir-locals-file'.

The value should be a list of suffix or group specifications.
See Info node `(transient)Suffix Specifications' and Info
node `(transient)Group Specifications' for documentation on
respective syntax.

Any key sequence starting with alphanumeric characters or
dash (regexp \"[a-zA-Z0-9-]\") is reserved for the user.

For common types of commands like compilation or shell commands,
Disproject provides suffixes with built-in features like process
buffer naming/tracking that can be customized via keyword values
in suffix specifications.  These can be found in:
  `M-x shortdoc disproject-customizable-suffixes'
Alternatively, if more flexibility is needed, see
  `M-x shortdoc disproject-environment'
for functions that set up variables according to transient state.

Some examples of suffixes using some of the various methods
described are provided below.  Note that some examples may use
the `advice' or `advice*' slots, which is an experimental feature
in recent Transient versions.

  ((\"f\" \"Find a file from project\"
    (lambda () (interactive)
      (disproject-with-environment
        (call-interactively #\\='find-file))))
   (\"m\" \"Run make\" disproject-compile
    :cmd \"echo Running make...; make -k\"
    :buffer-id \"make\")
   (\"M\" \"Run make test\" disproject-compile
    :cmd \"echo Running tests...; make test\"
    :buffer-id \"make\")
   (\"S\" \"Start `shell' in project root\" project-shell
    :advice disproject-with-env-apply))

`disproject-custom-suffixes' initially used a custom syntax
provided by Disproject for defining custom suffixes.  This is
still supported, but is discouraged in favor of a solution that
is based on Transient syntax, and may be deprecated/removed at a
later date.  The old syntax is used for suffix specifications
when the `:command-type' keyword is present.  Group
specifications are not supported with this syntax; only the
shorthand may be used.

This variable is marked safe due to various reasons discussed in
`disproject-custom--suffixes-allowed?'.  Prompts are deferred to
the mentioned function for explicit permission by the user - this
may be unsafe if unconditionally evaluated."
  :type '(repeat sexp)
  :group 'disproject)
;;;###autoload(put 'disproject-custom-suffixes 'safe-local-variable #'always)

(defcustom disproject-find-dir-command #'project-find-dir
  "Command to find a directory in a project.

This is called whenever the function `disproject-find-dir' is
invoked."
  :type 'function
  :group 'disproject
  :group 'disproject-swappable-commands)

(defcustom disproject-find-file-command #'project-find-file
  "Command used for opening a file in a project.

This is called whenever the function `disproject-find-file' is
invoked."
  :type 'function
  :group 'disproject
  :group 'disproject-swappable-commands)

(defcustom disproject-find-line-command #'disproject-default-find-line
  "Command to find line occurrences in a project's open buffers.

This is called whenever the function `disproject-find-line' is
invoked."
  :type 'function
  :group 'disproject
  :group 'disproject-swappable-commands)

(defcustom disproject-find-regexp-command #'project-find-regexp
  "Command used for finding regexp matches in a project.

This is called whenever the function `disproject-find-regexp' is
invoked."
  :type 'function
  :group 'disproject
  :group 'disproject-swappable-commands)

(defcustom disproject-find-special-file-suffixes
  '(["Other options"
     (disproject-infix-customize-switch)]
    ["Special files"
     (disproject-find-dir-locals-file)
     (disproject-find-dir-locals-2-file)])
  "Commands for the `disproject-find-special-file-dispatch' prefix.

Its value should be a list of group specifications that is
accepted by `transient-subgroups' (the same as expected in
`transient-define-prefix').  If only one column is needed, the
value may also be a list of suffix specifications as a shorthand.
See Info node `(transient) Group Specifications' and Info
node `(transient) Suffix Specifications' for documentation on
specification forms.

Any key sequence starting with alphanumeric characters or dash
\(regexp \"[a-zA-Z0-9-]\") is reserved for the user.

`disproject-find-special-file' is provided as a convenient way to
create commands for finding special files.  See its documentation
for more information on using it.

An example value is provided below, which adds a
`disproject-find-special-file' command for editing the make file
at the project root (with preferred file name \"Makefile\"), in
addition to the default find-dir-locals-file commands.

  ([\"Options\"
    (disproject-infix-customize-switch)]
   [\"Special files\"
    (disproject-find-dir-locals-file)
    (disproject-find-dir-locals-2-file)
    (\"m\" disproject-find-special-file :file (\"Makefile\"
                                             \"makefile\"
                                             \"GNUmakefile\"))])"
  :type '(repeat sexp)
  :group 'disproject)

(defcustom disproject-or-external-find-file-command
  #'project-or-external-find-file
  "Command used to find a file in a project or its external roots.

This is called whenever the function
`disproject-or-external-find-file' is invoked."
  :type 'function
  :group 'disproject
  :group 'disproject-swappable-commands)

(defcustom disproject-or-external-find-regexp-command
  #'project-or-external-find-regexp
  "Command used to find regexp matches in a project or its external roots.

This is called whenever the function
`disproject-or-external-find-file' is invoked."
  :type 'function
  :group 'disproject
  :group 'disproject-swappable-commands)

(defcustom disproject-shell-command #'project-eshell
  "Command used for opening a shell in a project.

This is called whenever the function `disproject-shell' is invoked."
  :type 'function
  :group 'disproject
  :group 'disproject-swappable-commands)

(defcustom disproject-switch-to-buffer-command #'project-switch-to-buffer
  "Command used for switching project buffers.

This is called whenever the function
`disproject-switch-to-buffer' is invoked."
  :type 'function
  :group 'disproject
  :group 'disproject-swappable-commands)

(defcustom disproject-vc-status-commands '((Git . magit-status)
                                           (nil . project-vc-dir))
  "Alist of entries denoting a VC backend and an associated status command.

BACKEND is a VC backend; see `project-vc-backend-markers-alist'
for recognized keys.  COMMAND is a symbol or function that is
called interactively when the suffix is invoked.

In certain cases, COMMAND may be unbound, or an entry for a
selected project's backend is not present.  In both of these
cases, the entry with a nil backend will be used instead as a
fallback.  A fallback should always be specified.

This is used in the command `disproject-vc-status'."
  :type 'alist
  :group 'disproject
  :group 'disproject-swappable-commands)


;;;; Faces.


;;;; Conditional feature predicates/forms.
;; NOTE: The `disproject-prefix-*' predicates are specifically meant for usage
;; during transient setup of `disproject-prefix' prefixes (or subclasses of).
;; Calling these in any other situation may lead to unexpected/undesired
;; results.

(defun disproject-prefix--feature-flymake? ()
  "Return non-nil if `flymake' is an available library."
  (featurep 'flymake))

(defun disproject-prefix--feature-magit-clone? ()
  "Return non-nil if `magit-clone' is an available library."
  (featurep 'magit-clone))

(defun disproject-prefix--feature-magit-status? ()
  "Return non-nil if `magit-status' is an available library."
  (featurep 'magit-status))

(defun disproject-prefix--git-clone-fallback-apt? ()
  "Return non-nil if the \"git clone\" fallback command should be used."
  (and (not (featurep 'magit-clone)) (executable-find "git")))

(defun disproject-prefix--git-init-fallback-apt? ()
  "Return non-nil if the \"git init\" fallback command should be used."
  (and (not (featurep 'magit-status)) (executable-find "git")))

(defun disproject-prefix--in-default-project? ()
  "Return non-nil if the selected project is also the default project."
  (and-let* ((scope (transient-scope)))
    (disproject-scope-project-is-default? scope)))

(defun disproject-prefix--magit-apt? ()
  "Return non-nil if magit commands are apt to show."
  (and-let* ((_ (featurep 'magit))
             (scope (transient-scope))
             (project (disproject-scope-selected-project scope)))
    (eq 'Git (disproject-project-backend project))))

(defun disproject-prefix--magit-todos-apt? ()
  "Return non-nil if the `magit-todos' is available."
  (locate-library "magit-todos"))

(defun disproject-prefix--customize-dirlocals-apt? ()
  "Return non-nil if `customize-dirlocals' function exists."
  (functionp 'customize-dirlocals))

(defun disproject-prefix--version-control-apt? ()
  "Return non-nil if version control commands are apt to show.

Consider commands apt when no project is selected, since the
state already implies that a prompt will be made to select a
project."
  (if-let* ((project (disproject-scope-selected-project (transient-scope))))
      (disproject-project-backend project)
    t))


;;;; Transient groups.


;;;;; Definitions.

(transient-define-group disproject-global-header-group
  disproject--selected-project-header-group
  disproject--global-options-group)

(transient-define-group disproject--global-options-group
  ["Global options"
   (disproject-display-buffer-action-dispatch)
   ;; Reserve ";" key for other global options to be added in the future.
   ])

(transient-define-group disproject--selected-project-header-group
  [:description disproject--selected-project-description ""])


;;;;; Dynamic group definitions.

;;;;;; Auxiliary.

(defun disproject--group-insert-defaults (group-spec &optional reached-keywords?
                                                     &rest keyword-values)
  "Insert KEYWORD-VALUES as \"default keyword-value pairs\" for GROUP-SPEC.

GROUP-SPEC is a transient group specification, as defined in Info
node `(transient) Group Specifications'.  KEYWORD-VALUES will be
prepended to the keyword-value pairs (or just added, if none are
present), such that they essentially change the default values
associated with the keywords.

REACHED-KEYWORDS? is an internal argument, used for tracking
whether KEYWORD-VALUES have already been added to the beginning
of the keyword-value pairs of a group (they must be at the
beginning, so that it's possible to override them).

For example, if SPEC is:
  [\"Example group\"
   :if (lambda () nil)
   [(\"f\" \"Find file\" find-file)]
   [(\"d\" \"Dired\" dired)]]

and KEYWORD-VALUES is:
  (:if (lambda () t))

the return value will be:
  [\"Example group\"
   :if (lambda () t)
   ;; This overrides the line above.
   :if (lambda () nil)
   [:if (lambda () t)
    (\"f\" \"Find file\" find-file)]
   [:if (lambda () t)
    (\"d\" \"Dired\" dired)]]"
  ;; See (info "(transient) Group Specifications") for matching specifications.
  (pcase-exhaustive group-spec
    ('()
     nil)
    ;; Don't process GROUP-SPEC if it's not a sequence.  As a limitation of
    ;; complexity in implementation, this includes transient group variables,
    ;; which will also not be processed.
    ((pred (not seqp))
     group-spec)
    ;; Optimization: operate on a list-ified GROUP-SPEC, which should avoid
    ;; generating a bunch of vectors from descending in recursion.
    ((pred vectorp)
     (apply #'disproject--group-insert-defaults
            (seq-into group-spec 'list) reached-keywords? keyword-values))
    ;; ELEMENT (group)
    (`(,(and (pred vectorp) group-element) . ,rest-group)
     (vconcat (if reached-keywords? [] keyword-values)
              (vector (apply #'disproject--group-insert-defaults
                             group-element nil keyword-values))
              (apply #'disproject--group-insert-defaults
                     rest-group :reached-keywords keyword-values)))
    ;; ELEMENT (suffix)
    (`(,(and (pred listp) suffix-element) . ,rest-group)
     (vconcat (if reached-keywords? [] keyword-values)
              ;; NOTE: Transient currently doesn't permit subgroup and suffix
              ;; specifications in the same group, but it's possible this could
              ;; change, so keep processing anyways.
              (vector suffix-element)
              (apply #'disproject--group-insert-defaults
                     rest-group :reached-keywords keyword-values)))
    ;; {KEYWORD VALUE}
    (`(,(and (pred keywordp) keyword) ,value . ,rest-group)
     (vconcat (if reached-keywords? [] keyword-values)
              (vector keyword value)
              (apply #'disproject--group-insert-defaults
                     rest-group :reached-keywords keyword-values)))
    ;; {LEVEL} or {DESCRIPTION}.  Also act as a catch-all for any syntax we
    ;; don't need/expect to process in GROUP-SPEC, which can be included as-is.
    (`(,special-optional . ,rest-group)
     (vconcat (vector special-optional)
              (apply #'disproject--group-insert-defaults
                     rest-group reached-keywords? keyword-values)))))

(defun disproject-custom--custom-spec? (spec)
  "Return non-nil if SPEC is considered custom Disproject suffix syntax.

The keyword `:command-type' is used as an indicator that SPEC
should be interpreted as custom syntax.

Note that this essentially makes `:command-type' a reserved
keyword; behavior may be unexpected if it involves a
specification that uses some transient suffix class with this
keyword."
  (and (listp spec) (memq :command-type spec)))

;;;;;; Setup functions.

(defvar disproject-custom--showed-custom-spec-deprecation? nil
  "Whether deprecation message for the custom specification syntax has been shown.")

(defun disproject-custom--setup-suffixes (_)
  "Set up suffixes according to `disproject-custom-suffixes'."
  (let* ((scope (disproject--scope))
         (suffixes (disproject-project-custom-suffixes
                    (disproject-scope-selected-project-ensure scope)))
         ;; DEPRECATED: Remove at least 6 months after release with this notice.
         ;; Since we originally only supported a single column, just convert
         ;; any custom spec suffixes found in a shallow search.
         (suffixes
          (seq-map
           (lambda (spec)
             (if (disproject-custom--custom-spec? spec)
                 (progn
                   (unless disproject-custom--showed-custom-spec-deprecation?
                     (message "\
Defining custom suffixes with Disproject's custom syntax is deprecated;\
 use transient specifications instead\
 (see `disproject-custom-suffixes' for updated documentation).")
                     (setq disproject-custom--showed-custom-spec-deprecation? t))
                   (disproject-custom--suffix spec))
               spec))
           suffixes))
         ;; As a shorthand (and for backwards compatibility), vector brackets
         ;; can be omitted if only one column of suffixes is needed.
         (suffixes (if (nlistp (seq-elt suffixes 0))
                       suffixes
                     `(["Custom suffixes" ,@suffixes])))
         (suffixes (seq-map (lambda (element)
                              (disproject--group-insert-defaults
                               element nil
                               :advice #'disproject-with-env-apply
                               :advice* #'disproject-with-env-apply))
                            suffixes)))
    (transient-parse-suffixes 'disproject-custom-dispatch suffixes)))

(defun disproject--setup-find-special-file-suffixes (_)
  "Set up suffixes according to `disproject-find-special-file-suffixes'."
  (let* ((user-suffixes (if (nlistp (seq-elt
                                     disproject-find-special-file-suffixes 0))
                            disproject-find-special-file-suffixes
                          ;; Shorthand: Allow omission of vector brackets if
                          ;; only a single column needs to be specified.
                          `(["Special files"
                             ,@disproject-find-special-file-suffixes])))
         (suffixes (seq-map (lambda (element)
                              (disproject--group-insert-defaults
                               element nil
                               :advice #'disproject-with-env-apply
                               :advice* #'disproject-with-env-apply))
                            user-suffixes)))
    (transient-parse-suffixes 'disproject-find-special-file-dispatch suffixes)))


;;;; Classes.


;;;;; `disproject-project' class.

(defclass disproject-project ()
  ;; root must be a path to a valid project.  An `initialize-instance' method
  ;; for this class enforces this.
  ((root :reader disproject-project-root
         :initarg :root
         :type string
         :documentation "Project root directory.")
   (instance :reader disproject-project-instance
             :type list
             :documentation "Project `project.el' instance.")
   (backend :reader disproject-project-backend
            :type symbol
            :documentation "Project VC backend.")
   (custom-suffixes :reader disproject-project-custom-suffixes
                    :type list
                    :documentation "\
Project's custom suffixes as described in
`disproject-custom-suffixes'."))
  "Class representing a `project.el' project.

Instances of this class are initialized by providing the `:root'
initialization argument, which is the only argument available.
Other slots depend on this value, and are lazily fetched as
needed when calling readers.  These fetched values are then
cached for subsequent queries.")

(cl-defmethod initialize-instance :after ((obj disproject-project) &rest _slots)
  "Do additional initialization for a `disproject-project' instance.

This enforces that the root is part of a valid project.  If
unbound, `project-current' will be invoked, which may prompt the
user for a path.  If the path does not lead to a valid project,
an error will be signaled.  \"Transient\" project instances are
not considered valid.

If the directory provided is not actually the root directory, the
slot will also be changed to the detected root.

Additionally, the project will be force-remembered as a known
project as a last step of initialization."
  (let* ((provided-root (if (slot-boundp obj 'root)
                            (oref obj root)
                          (project-root (project-current t))))
         (project (oset obj instance (project-current nil provided-root))))
    (unless project
      (error "Root directory does not lead to valid project: %s" provided-root))
    ;; The provided root may be a sub-directory, so we re-set it to the root
    ;; determined by `project.el'.
    (oset obj root (project-root project))
    ;; Remember the project in case it's not in `project-known-project-roots'.
    (project-remember-project project)))

(defun disproject-project-or-nil (&optional directory)
  "Return a `disproject-project' instance for DIRECTORY, or maybe nil.

DIRECTORY is passed to `project-current' to detect a project."
  (and-let* ((project (project-current nil directory)))
    (disproject-project :root (project-root project))))

(cl-defmethod disproject-project-backend ((obj disproject-project))
  "Return the OBJ project backend.

This internally uses `project-try-vc' to determine the backend."
  (if (slot-boundp obj 'backend)
      (oref obj backend)
    ;; XXX: `project-find-functions' (used in `project-current') doesn't enforce
    ;; a project instance format, so we can't expect the VC backend to be in the
    ;; same place in `disproject-project-instance'.  Instead, we specifically
    ;; rely on `project-try-vc' (`project-vc-backend-markers-alist') to get the
    ;; backend.
    (oset obj backend (let* ((root (disproject-project-root obj))
                             (instance (project-try-vc root)))
                        ;; Expect index 1 to be the VC backend of the project
                        ;; instance.
                        (nth 1 instance)))))

(cl-defmethod disproject-project-custom-suffixes ((obj disproject-project))
  "Return the OBJ project custom suffixes."
  (if (slot-boundp obj 'custom-suffixes)
      (oref obj custom-suffixes)
    (oset obj
          custom-suffixes
          (if-let*
              ((project (disproject-project-instance obj))
               (root (disproject-project-root obj))
               (suffixes
                ;; Retrieve custom suffixes without triggering dir-locals
                ;; permissions prompt.
                (alist-get 'disproject-custom-suffixes
                           (with-temp-buffer
                             (let ((default-directory root))
                               (hack-dir-local--get-variables nil)))))
               ((disproject--assert-type 'disproject-custom-suffixes suffixes))
               ((disproject-custom--suffixes-allowed? project suffixes)))
              suffixes
            (default-value 'disproject-custom-suffixes)))))


;;;;; `disproject-scope' class.

(defclass disproject-scope ()
  ((selected-project :initarg :selected-project
                     :accessor disproject-scope-selected-project
                     :initform nil
                     :type (or null disproject-project)
                     :documentation "\
Currently selected project object in the transient menu, if any.
If no value is provided during initialization, the function
`project-current' is used to find one for initializing a
`disproject-project' object.  This slot may be nil.")
   (default-project :reader disproject-scope-default-project
                    :initform nil
                    :type (or null disproject-project)
                    :documentation "\
Project object belonging to `default-directory' at the time of
initialization (or the current buffer, in other words), if any.")
   (display-buffer-action :initarg :display-buffer-action
                          :accessor disproject-scope-display-buffer-action
                          :initform nil
                          :type list
                          :documentation "\
Value to use for `display-buffer-overriding-action' when
executing suffix commands.

If non-nil, the action alist must contain an entry under the
\\='description key that stores a string value describing the
action.  This will be used as a menu indicator to show the active
overriding action."))
  "Class representing a Disproject menu's transient scope.

Objects of this type are intended to be used in a transient
prefix's `:scope' slot, and contains information regarding the
default project, selected project, and other state like option
values that should be shared with other menus.

In Disproject prefixes, this scope object is normally fetched via
the function `disproject--scope'.")

(cl-defmethod initialize-instance :after ((obj disproject-scope) &rest _slots)
  "Do additional initialization for scope OBJ."
  (let* ((default-project-obj
          (disproject-project-or-nil default-directory))
         (selected-project-obj
          ;; `project-current' may read other variables like
          ;; `project-current-directory-override', which may make the initial
          ;; selected project different from the default project (only
          ;; `default-directory' is read for the latter).
          (if-let* ((current-project (project-current))
                    ;; Use `default-project-obj' if the projects happen to be
                    ;; the same so cached values are shared.
                    ((or (not default-project-obj)
                         (not (file-equal-p
                               (project-root current-project)
                               (disproject-project-root default-project-obj))))))
              (disproject-project :root (project-root current-project))
            default-project-obj)))
    (when default-project-obj
      (oset obj default-project default-project-obj))
    (when selected-project-obj
      (oset obj selected-project selected-project-obj))))

(cl-defmethod disproject-scope-selected-project ((obj disproject-scope))
  "Return scope OBJ selected project.  May be nil."
  (if (slot-boundp obj 'selected-project)
      (oref obj selected-project)))

(cl-defmethod disproject-scope-selected-project-ensure ((obj disproject-scope))
  "Return scope OBJ selected project.

If the selected-project slot is nil, a new instance of
`disproject-project' will be initialized and written to the slot.
The user may be prompted for a project."
  (or (disproject-scope-selected-project obj)
      (oset obj selected-project (disproject-project))))

(cl-defmethod disproject-scope-default-project ((obj disproject-scope))
  "Return scope OBJ default project.  May be nil."
  (if (slot-boundp obj 'default-project)
      (oref obj default-project)))

(cl-defmethod disproject-scope-project-is-default? ((obj disproject-scope))
  "Return non-nil if the OBJ scope's selected and default projects are the same."
  (and-let* ((default-project (disproject-scope-default-project obj))
             (selected-project (disproject-scope-selected-project obj)))
    (file-equal-p (disproject-project-root default-project)
                  (disproject-project-root selected-project))))

(cl-defmethod disproject-scope-prefer-other-window? ((obj disproject-scope))
  "Return whether OBJ scope has a `display-buffer' override to prefer other window."
  (pcase (disproject-scope-display-buffer-action obj)
    (`(,(or 'display-buffer-use-some-window
            '(display-buffer-use-some-window . ,_))
       . ,_alist)
     t)))

;; DEPRECATED: Remove after at least 6 months of being in a stable release.
(make-obsolete 'disproject-scope-prefer-other-window?
               'disproject-scope-display-buffer-action
               "2.2.0")

(defvar disproject--environment-scope nil
  "Current disproject scope in `disproject-with-env-apply'.

This is for suffixes that use `disproject-with-env-apply'.  It is
preferred to use this when possible over `disproject--scope' to
avoid cases where there is no scope, causing a new
`disproject-scope' to be initialized on every call and
potentially resulting in duplicate prompts.")

(defun disproject--scope ()
  "Return the scope for a `disproject-prefix' prefix.

It is recommended to bind the return value for reuse instead of
calling this multiple times, as it is possible for multiple
different scope objects to be created (which is not usually desired)."
  ;; TODO: Improve case where callers cannot avoid calling `disproject--scope'
  ;; more than once (perhaps when calling a suffix that also calls scope).
  ;; Maybe document that `disproject-with-environment' should be used so that
  ;; `disproject--environment-scope' is bound?
  (or disproject--environment-scope
      ;; Fall back to initializing scope with `disproject-dispatch' if needed.
      (transient-scope 'disproject-dispatch 'disproject-prefix)))


;;;;; `disproject-process-suffix' class.

(defclass disproject-process-suffix (transient-suffix)
  ((buffer-id :initarg :buffer-id
              :initform nil
              :documentation "\
String.  Unique identifier for the process buffer associated with
this suffix command.

Users may set the same identifier for multiple commands to mark
them as incompatible for a project (only one can run at a given
time).

If the `description' slot of the instance is a string, this slot
will use it as a default value.  Otherwise, the description is
assumed to be a function, and the `default-buffer-id' slot value
will be used instead, since there is no way to guarantee that a
function will always return the same identifier.

The `disproject-process-suffix-buffer-name' method may be used to
retrieve the buffer name associated with this suffix.

Implementations of suffix commands should handle the value as
documented.")
   (display-status? :initarg :display-status?
                    :initform t
                    :documentation "\
Non-nil to display status of associated process buffer.")
   (allow-multiple-buffers? :initarg :allow-multiple-buffers?
                            :initform nil
                            :documentation "\
Non-nil if multiple buffers associated with the buffer ID may be
created.

If allowed, the `display-status?' slot will be ignored, and
status will not be shown.  Suffix command implementations may
also behave differently; for example, executing a command while a
process is running could create a new buffer instead of killing
the existing one.")
   (default-buffer-id :allocation :class
                      :initform "default"
                      :documentation "\
Default string for the `buffer-id' slot when it is nil."))
  "Class for Disproject suffixes that spawn a process.

This provides methods for managing things related to the
associated command's process buffer.")

(defun disproject-process-suffix--buffer-name (buffer-id project-name)
  "Return a buffer name for a process suffix from BUFFER-ID and PROJECT-NAME."
  (concat "*" project-name "-process|" buffer-id "*"))

(cl-defmethod disproject-process-suffix-buffer-name
  ((obj disproject-process-suffix) project-name)
  "Return the OBJ suffix's process buffer name associated with PROJECT-NAME.

PROJECT-NAME is the project name, which will be used to give a
unique namespace to the project's process buffers."
  (disproject-process-suffix--buffer-name
   (or (oref obj buffer-id)
       (let ((description (oref obj description)))
         (and (stringp description) description))
       (oref obj default-buffer-id))
   project-name))

(cl-defmethod transient-format-description ((obj disproject-process-suffix))
  "Format description for OBJ.

If the `display-status?' slot is non-nil and
`allow-multiple-buffers?' is nil, the description will be
formatted with an indicator that describes the associated process
buffer's current status."
  ;; Preserve the transient "bug" warning if there is no description.
  (and-let* ((description (cl-call-next-method)))
    (if-let* (((and (oref obj display-status?)
                    (null (oref obj allow-multiple-buffers?))))
              (project (disproject-scope-selected-project (disproject--scope)))
              (project-name (project-name
                             (disproject-project-instance project)))
              (buf-name (disproject-process-suffix-buffer-name obj project-name))
              (buffer (get-buffer buf-name)))
        (progn
          ;; Refresh transient if process status changes.
          (disproject-add-sentinel-refresh-transient buf-name)
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
      description)))


;;;;; `disproject-shell-command-suffix' class.

(defclass disproject-shell-command-suffix (disproject-process-suffix)
  ((cmd :initarg :cmd
        :initform nil
        :documentation "\
String or interactive function which returns a shell command that
will be used to spawn a process.

If the value is a string, it is used as the shell command.
Otherwise, it should be an interactive function that returns a
string to be used as the command.  A nil value indicates to read
a shell command.

Implementations of suffix commands should handle spawning
processes based on the value.")
   (always-read? :initarg :always-read?
                 :initform nil
                 :documentation "\
Non-nil to always read shell command, even when `cmd' is non-nil.

Implementations of suffix commands should handle reading based
off this value.")
   (default-buffer-id :allocation :class
                      :initform "shell-command"))
  "Class for suffix commands that execute shell commands.")

(cl-defmethod disproject-shell-command-suffix-cmd
  ((obj disproject-shell-command-suffix))
  "Return a string shell command from OBJ.

Use the `cmd' slot of OBJ as the shell command.  If it is a
command, interactively call it and use the return value as the
shell command.

May also return nil if the value of `cmd' is nil.

When unable to convert to a string, throw an error."
  (let ((cmd (oref obj cmd)))
    (cond
     ((null cmd) nil)
     ((stringp cmd) cmd)
     ((commandp cmd t) (call-interactively cmd))
     (t (user-error "Not a string or command: %s" cmd)))))


;;;;; `disproject-compilation-suffix' class.

(defclass disproject-compilation-suffix (disproject-shell-command-suffix)
  ((comint? :initarg :comint?
            :documentation "\
Non-nil to enable Comint mode in the compilation buffer.

Implementations of suffix commands should check this value in
order to conditionally enable the mode.")
   (default-buffer-id :allocation :class
                      :initform "compile"))
  "Class for suffixes that utilize the `compile' command for shell commands.")


;;;;; `disproject-display-buffer-action-suffix' class.

(defclass disproject-display-buffer-action-suffix (transient-suffix)
  ((display-buffer-action :initarg :display-buffer-action
                          :initform nil
                          :documentation "\
Value to be used for `display-buffer-overriding-action'.

If non-nil, the action alist must contain a \\='description entry
describing the action.

Action alists may specify an \\='inhibit-multiple-displays entry,
which will affect action behavior in certain cases as described
in `disproject-with-env'.

Implementations of suffix commands must handle storing this value
somewhere so it can be accessed later and applied; for example,
through a shared object in the transient scope."))
  "Class for suffixes that manage `display-buffer' action settings.")


;;;;; `disproject-find-special-file-suffix' class.

(defclass disproject-find-special-file-suffix (transient-suffix)
  ((file :initarg :file
         :initform "."
         :type (or string (list-of string) function)
         :documentation "The special file's base name.

This slot's value should be a string, list of strings, or a
function that returns a string.

The behavior for each type is as documented:

- String: Literal file name.

- List of strings: The first element will be treated as the
  preferred file name to find when none of the other files in the
  list exist.  The rest are treated as fallbacks (in order) to be
  used if they exist.

- Function: Must return a string as the literal file name to
  find.

By default, the file is the project root (\".\" is relative to
the root directory).")
   (find-file-function :initarg :find-file-function
                       :initform #'find-file
                       :type function
                       :documentation "\
Function that is applied to this object's \\='file slot value to edit it."))
  "Class for convenience suffixes to edit common project files.")

(cl-defmethod disproject-find-special-file-suffix-file
  ((obj disproject-find-special-file-suffix))
  "Return OBJ's \\='file slot value as a string.

If a list of strings is encountered, return the first string
where the file exists from `default-directory'.  Otherwise,
return the first (preferred file) string."
  (let ((value (oref obj file)))
    (pcase-exhaustive value
      ((pred stringp)
       value)
      ((pred listp)
       (when (null value) (error "`file' slot value cannot be an empty list"))
       (or (seq-find #'file-exists-p value)
           (car value)))
      ((pred functionp)
       (funcall value)))))

(cl-defmethod transient-format-description
  ((obj disproject-find-special-file-suffix))
  "Return a formatted description for OBJ.

If the \\='description slot value is nil, format and return the
\\='file slot value as the description.  Otherwise, behave the
same as `transient-suffix' for formatting descriptions."
  (if (oref obj description)
      (cl-call-next-method)
    (if-let* ((scope (disproject--scope))
              (project (disproject-scope-selected-project scope))
              (default-directory (disproject-project-root project))
              (file (disproject-find-special-file-suffix-file obj)))
        (propertize file 'face (if (file-exists-p file)
                                   'transient-value
                                 'transient-inactive-value))
      (propertize "(BUG: no description; no project in scope)" 'face 'error))))


;;;;; `disproject-prefix' class.

(defclass disproject-prefix (transient-prefix) ()
  "General Disproject prefix class.

All prefixes that need to make use of `disproject-scope' as the
scope object should be of this type or inherit from it, as it is
responsible for preserving the scope across menus.")

(cl-defmethod transient-init-scope ((obj disproject-prefix))
  "Initialize transient scope for OBJ.

Inherit the current prefix's scope if it is a `disproject-prefix'
type; otherwise, initialize a new `disproject-scope' scope value
if it hasn't already been initialized."
  (if (cl-typep transient-current-prefix 'disproject-prefix)
      (let ((scope (disproject--scope)))
        (oset obj scope scope))
    ;; This method is also called for situations like returning from a
    ;; sub-prefix, in which case we want to keep the existing scope.
    (unless (oref obj scope)
      (oset obj scope (disproject-scope)))))


;;;;; `disproject--selected-project-prefix' class.

(defclass disproject--selected-project-prefix (disproject-prefix) ()
  "Class for Disproject prefixes that require a project to be selected.")

(cl-defmethod transient-init-scope ((obj disproject--selected-project-prefix))
  "Ensure there is a selected project after initializing scope for OBJ."
  (cl-call-next-method)
  (disproject-scope-selected-project-ensure (oref obj scope)))


;;;;; `disproject--custom-suffixes-prefix' class.

(defclass disproject--custom-suffixes-prefix
  (disproject--selected-project-prefix) ()
  "Class for Disproject prefixes that need to use custom suffixes.")

(cl-defmethod transient-init-scope ((obj disproject--custom-suffixes-prefix))
  "Ensure custom suffixes are loaded after initializing scope for OBJ."
  (cl-call-next-method)
  (disproject-project-custom-suffixes
   (disproject-scope-selected-project-ensure (oref obj scope))))


;;;; Commands.


;;;;; Interactive functions.

;;;;;; Auxiliary.

;;;;;; Definitions.

(defun disproject-default-find-line (regexp)
  "Find matching line in buffers associated with the current project.

REGEXP is a regular expression used to search for occurrences.

This uses `multi-occur' under the hood."
  (interactive (list (project--read-regexp)))
  (if-let* ((project (project-current)))
      (multi-occur (seq-filter (lambda (buf)
                                 (not (string-prefix-p " " (buffer-name buf))))
                               (project-buffers project))
                   regexp)
    (error "No project in current directory: %s" default-directory)))


;;;;; Transient infixes.

;;;;;; Auxiliary.

;;;;;; Definitions.

(transient-define-infix disproject-infix-customize-switch ()
  :description "Use Customize interface"
  :class 'transient-switch
  :shortarg "-c"
  :argument "--customize"
  :if #'disproject-prefix--customize-dirlocals-apt?)


;;;;; Transient suffixes.

;;;;;; Auxiliary.

(defun disproject-add-sentinel-refresh-transient (buffer-name)
  "Add function to a buffer's process sentinel to refresh transient, if active.

BUFFER-NAME is the name of the buffer with a process sentinel the
function will be added to."
  (when-let* ((buffer (get-buffer buffer-name))
              (process (get-buffer-process buffer))
              ((not (advice-function-member-p
                     #'disproject--refresh-transient
                     (process-sentinel process)))))
    (add-function
     :before (process-sentinel process)
     #'disproject--refresh-transient)))

(defun disproject-dir-locals-file ()
  "Return `dir-locals-file'."
  dir-locals-file)

(defun disproject-dir-locals-2-file ()
  "Return the secondary `dir-locals-file'."
  (let ((file-root (file-name-base dir-locals-file))
        (file-extension (file-name-extension dir-locals-file)))
    (concat file-root "-2." file-extension)))

(defun disproject-process-buffer-name (project-dir &optional identifier)
  "Return a project's process buffer name corresponding to IDENTIFIER.

PROJECT-DIR is the project directory, which will be used to give
project buffers a unique namespace.

IDENTIFIER is an optional string argument that can be specified
to make the buffer name unique to the project's process buffers.
If non-nil, \"default\" is used as the identifier.

This function is *not* meant to be used like
`project-prefixed-buffer-name', although it is similar in
functionality; the identifier should not be tied to the buffer
mode in any way.  It should be the only means of making a name
unique in the context of a project.  This allows users to track
buffers based on just an identifier and also allow specifying
incompatible commands (e.g. if two commands use the same buffer
name, they should not be allowed to run at the same time)."
  (concat "*"
          (file-name-nondirectory (directory-file-name project-dir))
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
     (let* ((project (disproject-scope-selected-project-ensure
                      (disproject--scope)))
            ;; Fall back to description if identifier is not provided.
            ;; Uniqueness is preferred over the name looking nice to prevent
            ;; unintentionally making commands incompatible.
            (identifier (or identifier description))
            (disproject-process-buffer-name (disproject-process-buffer-name
                                             (disproject-project-root project)
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
             (disproject-add-sentinel-refresh-transient
              disproject-process-buffer-name))))))))

(defun disproject-custom--suffix-command (command-type command)
  "Dispatch a command s-expression to be evaluated in a custom suffix.

COMMAND-TYPE is a symbol corresponding to a command type
documented in `disproject-custom-suffixes'.

COMMAND is an yet-to-be-evaluated s-expression which is inserted
appropriately according to the command type.

Note that the returned s-expressions may expect
`disproject-process-buffer-name' to be set when evaluated; this
value is expected to be the name of the buffer which processes
will run."
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
                       command)))))))
    ('run
     `(disproject-with-environment
        (let ((shell-command-buffer-name-async disproject-process-buffer-name)
              (command ,command))
          (async-shell-command
           (cond ((stringp command)
                  command)
                 ((commandp command t)
                  (if-let* ((result (call-interactively command))
                            ((stringp result)))
                      result
                    ,(disproject-custom--suffix-command-type-error
                      "Function does not return string"
                      command-type
                      command)))
                 (t
                  ,(disproject-custom--suffix-command-type-error
                    "Not a string or interactive function"
                    command-type
                    command)))))))
    (_
     (display-warning
      'disproject
      (format-message "Custom suffix command type not recognized: `%s'"
                      command-type)))))

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

BUFFER is the associated custom suffix buffer.  It may be nil.

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

(defun disproject--find-dir-locals-file (file)
  "Dispatch a function to edit dir-locals FILE depending on transient state.

This supports `find-file' (default) and `customize-dirlocals';
the latter of which will be used when the \"--customize\" value
is in transient arguments.

Note that `customize-dirlocals' was introduced in Emacs version
30.1.  If the function is not available and the \"--customize\"
value is passed, this function will not do anything."
  (declare-function customize-dirlocals "cus-edit")
  (let* ((args (if transient-current-command
                   (transient-args transient-current-command)))
         (customize? (transient-arg-value "--customize" args)))
    (cond
     (customize?
      (if (functionp 'customize-dirlocals)
          (customize-dirlocals (expand-file-name file))
        (user-error "`customize-dirlocals' is not defined (added in Emacs 30.1)")))
     (t
      (find-file file)))))

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
                   (and-let* ((project (project-current nil directory)))
                     (list project)))
                 directories)
     :test (lambda (p1 p2) (equal (project-root p1) (project-root p2))))))

(defun disproject--refresh-transient (&rest _ignore)
  "Refresh the currently active transient, if available."
  (when (transient-active-prefix)
    (transient--refresh-transient)))

(defun disproject--switch-project (search-directory)
  "Modify the Transient scope to switch to another project.

Look for a valid project root directory in SEARCH-DIRECTORY.  If
one is found, update the Transient scope to switch the selected
project."
  (setf (disproject-scope-selected-project (disproject--scope))
        (disproject-project :root search-directory)))

;;;;;; Suffix environment.

(defvar disproject--environment-buffer-name " disproject-environment"
  "Name of buffer which commands will be run from.")

(defmacro disproject--with-environment-buffer (&rest body)
  "Run BODY in a disproject environment buffer.

This will be an indirect buffer made from the current buffer."
  (declare (indent 0) (debug t))
  (let ((buf-name (make-symbol "buf-name")))
    `(let ((,buf-name (generate-new-buffer-name
                       disproject--environment-buffer-name)))
       (unwind-protect
           (with-current-buffer
               (make-indirect-buffer (current-buffer) ,buf-name t t)
             ,@body)
         (when-let* ((buf (get-buffer ,buf-name)))
           (kill-buffer buf))))))

(defun disproject-with-root-apply (fun &rest args)
  "Apply FUN to ARGS with `default-directory' set to the current project root."
  (let ((default-directory (project-root (project-current t))))
    (apply fun args)))

(defmacro disproject-with-root (&rest body)
  "Run BODY from the current project's root directory.

This is a macro version of `disproject-with-root-apply'."
  (declare (indent 0) (debug t))
  `(disproject-with-root-apply (lambda () ,@body)))

(defun disproject-with-env-apply (fun &rest args)
  "Set up environment from Disproject transient options and apply FUN to ARGS.

In order to prevent side effects from modifying variables in the
current buffer, BODY is run in the context of a temporary
indirect buffer made from the current buffer.

The environment consists of the following overrides:

`project-current-directory-override': Set to the selected
project's root directory.

`display-buffer-overriding-action': Set to the selected
`display-buffer' action override, if any.  If the overriding
action alist has a non-nil entry for
\\='inhibit-multiple-displays, the action will set up such that
it only applies to the first `display-buffer' for a command while
no minibuffer is active.  This may be useful for actions that do
not play well with commands that can display more than one
buffer."
  (let* ((original-command this-command)
         (disproject--environment-scope (disproject--scope))
         (project (disproject-project-instance
                   (disproject-scope-selected-project-ensure
                    disproject--environment-scope)))
         (from-directory (project-root project))
         (old-display-buffer-overriding-action display-buffer-overriding-action)
         (display-buffer-action (disproject-scope-display-buffer-action
                                 disproject--environment-scope))
         (minibuffer-depth (minibuffer-depth))
         ;; Don't use `display-buffer-override-next-command' to avoid having
         ;; to infer type in PRE-FUNCTION.  Also makes it possible to use
         ;; `display-buffer-no-window', which would not have valid values to
         ;; return.
         (display-buffer-override-function
          `(,(lambda (buffer alist)
               (pcase display-buffer-action
                 (`(,functions . ,(and (map ('inhibit-multiple-displays
                                             inhibit-multiple-displays))
                                       action-alist))
                  (unless (or (and inhibit-multiple-displays
                                   (> (minibuffer-depth) minibuffer-depth))
                              ;; HACK: Don't apply display-buffer overrides on
                              ;; prefix menus.
                              (get this-command 'transient--prefix))
                    (prog1 (seq-some (lambda (fun)
                                       (funcall fun buffer
                                                (append action-alist alist)))
                                     (ensure-list functions))
                      (when inhibit-multiple-displays
                        (setq display-buffer-overriding-action
                              old-display-buffer-overriding-action)))))))))
         ;; Only enable envrc if the initial environment has it enabled.
         (enable-envrc (and (bound-and-true-p envrc-mode)
                            (symbol-function 'envrc-mode)))
         ;; Only enable mise if the initial environment has it enabled.
         (enable-mise (and (bound-and-true-p mise-mode)
                           (symbol-function 'mise-mode))))
    (disproject--with-environment-buffer
      ;; Since interactive prompts may occur from having to ensure that a
      ;; project is selected, we re-set `this-command' so
      ;; `transient-suffix-object' can find the current suffix object.
      (let ((this-command original-command)
            (project-current-directory-override from-directory)
            ;; If an override is used and `switch-to-buffer' is called, we
            ;; assume the user explicitly wants it to obey the display action.
            (switch-to-buffer-obey-display-actions (and display-buffer-action t)))
        (disproject-with-root
          (hack-dir-local-variables-non-file-buffer)
          ;; Make sure commands are run in the correct direnv environment if
          ;; envrc-mode is enabled.
          (when enable-envrc (funcall enable-envrc))
          ;; Make sure commands are run in the correct mise environment if
          ;; mise-mode is enabled.
          (when enable-mise (funcall enable-mise)))
        (let ((display-buffer-overriding-action display-buffer-override-function))
          (apply fun args))))))

(defmacro disproject-with-env (&rest body)
  "Run BODY with Disproject transient settings applied.

This is a macro version of `disproject-with-env-apply'; see the
function for documentation on the settings."
  (declare (indent 0) (debug t))
  `(disproject-with-env-apply
    (lambda () ,@body)))

(defun disproject-with-env+root-apply (fun &rest args)
  "Apply FUN to ARGS from project root with Disproject transient settings.

See `disproject-with-env' and `disproject-with-root' for
documentation on settings applied.

Note that the environment is applied first, so project root will
be based on the selected project, not the current project."
  (disproject-with-env (disproject-with-root (apply fun args))))

(defmacro disproject-with-env+root (&rest body)
  "Run BODY with Disproject transient settings applied in selected project root.

This is a macro version of `disproject-with-env+root-apply'; see the
function for documentation on settings applied."
  (declare (indent 0) (debug t))
  `(disproject-with-env+root-apply (lambda () ,@body)))

(defmacro disproject-with-environment (&rest body)
  "Run BODY with Disproject transient settings applied in selected project root.

This is an alias for `disproject-with-env+root'."
  (declare (indent 0) (debug t))
  `(disproject-with-env+root ,@body))

;;;;;; Definitions.

(transient-define-suffix disproject-compile ()
  "Run a shell command with `compile' in project root.

The `cmd' slot value of the current transient suffix object is
used as the shell command.  If nil, prompt with `compile-command'
as an initial value.

With prefix arg or the `always-read?' slot non-nil, always
prompt.

If the `comint?' slot value of the current suffix object is
non-nil, Comint mode will be enabled in the compilation buffer.

See type `disproject-compilation-suffix' for documentation on
transient suffix slots."
  :class disproject-compilation-suffix
  (interactive)
  (disproject-with-root
    (let* ((obj
            (transient-suffix-object))
           (always-read?
            (oref obj always-read?))
           (command
            (if-let* ((cmd (disproject-shell-command-suffix-cmd obj)))
                ;; We don't need to read if `compilation-read-command' is t,
                ;; since the command should already be considered safe from
                ;; `disproject-custom--suffixes-allowed?'.
                (if (or always-read? current-prefix-arg)
                    (compilation-read-command cmd)
                  cmd)
              (let ((cmd (eval compile-command)))
                (if (or compilation-read-command current-prefix-arg)
                    (compilation-read-command cmd)
                  cmd))))
           (comint?
            (if (slot-boundp obj 'comint?)
                (oref obj comint?)
              ;; TODO: Default to a customizable variable when
              ;; the slot is unbound.
              nil))
           (scope
            (disproject--scope))
           (project-name
            (project-name (disproject-project-instance
                           (disproject-scope-selected-project-ensure scope))))
           (buf-name
            (disproject-process-suffix-buffer-name obj project-name))
           (compilation-buffer-name-function
            (cl-constantly buf-name)))
      (compile command comint?))))

(transient-define-suffix disproject-custom-prune-allowed-suffixes ()
  "Prune `disproject-custom-allowed-suffixes' entries.

The variable `disproject-custom-allowed-suffixes' may retain old
allowed custom suffixes from forgotten projects.  This command
provides a convenient means of removing old entries.

Disproject will usually handle calling this for the user when it
matters; for example, at the end of `disproject-forget-*'
commands."
  (interactive)
  (let* ((set-message-function 'set-multi-message)
         (new-suffixes
          (seq-filter (pcase-lambda (`(,project-root . ,_rest))
                        (if (seq-contains-p (project-known-project-roots)
                                            project-root)
                            t
                          (message
                           "disproject: Pruning allowed custom suffixes for unknown project: %s"
                           project-root)
                          nil))
                      disproject-custom-allowed-suffixes)))
    (when (not (equal new-suffixes disproject-custom-allowed-suffixes))
      (customize-save-variable 'disproject-custom-allowed-suffixes new-suffixes))))

(transient-define-suffix disproject-dired ()
  "Open Dired in project root."
  (interactive)
  (disproject-with-root
    (call-interactively #'project-dired)))

(transient-define-suffix disproject-dir-locals ()
  "Open `dir-locals-file' in the project root.

If prefix arg is non-nil, open the personal secondary
file (\".dir-locals-2.el\" by default)."
  (interactive)
  (disproject-with-root
    (find-file (if current-prefix-arg
                   (disproject-dir-locals-2-file)
                 (disproject-dir-locals-file)))))

;; DEPRECATED: Remove at least six months after release with this notice.
(make-obsolete 'disproject-dir-locals 'disproject-find-dir-locals-file "2.2.0")

(transient-define-suffix disproject-display-buffer-action-set ()
  "Set the transient scope's `display-buffer-overriding-action' value.

This does not do anything as a standalone command.  It is
intended to be used in Transient prefixes, where this command may
be specified with a value for the \\='display-buffer-action
slot (of `disproject-display-buffer-action-suffix').  This
enables generating suffix commands that can set the
`display-buffer' overriding action to custom values.

Users may specify a description for the action by adding a
\\='description entry to the action alist.  If this is omitted,
the suffix description will be used instead.

An \\='inhibit-multiple-displays entry may also be specified,
which will affect the behavior of `display-buffer' in certain
cases.  See `disproject-with-env' for more information.

For example, a specification for a suffix command that sets the
override to \"prefer other window\" might look something like so:
  (\"o\" \"other window\" disproject-display-buffer-action-set
   :display-buffer-action (display-buffer-use-some-window
                           (inhibit-same-window . t)
                           ;; Omitting this will cause the suffix description
                           ;; to be used; in this case, \"other window\".
                           (description . \"use other window\")))

Note that this only works in transient prefixes of class
`disproject-prefix'.

See type `disproject-display-buffer-action-suffix' for
documentation on transient suffix slots."
  :class disproject-display-buffer-action-suffix
  :transient #'transient--do-return
  (interactive)
  (and-let* ((scope (disproject--scope))
             (suffix (transient-suffix-object)))
    (setf (disproject-scope-display-buffer-action scope)
          (let ((action (oref suffix display-buffer-action)))
            (pcase-exhaustive action
              ('nil
               nil)
              (`(,_functions . ,(map ('description (pred (not null)))))
               action)
              (`(,functions . ,alist)
               `(,functions . ((description
                                . ,(transient-format-description suffix))
                               ,@alist))))))))

(transient-define-suffix disproject-execute-extended-command ()
  "Execute an extended command in project root."
  (interactive)
  (disproject-with-root
    (call-interactively #'execute-extended-command)))

(transient-define-suffix disproject-find-dir ()
  "Find directory in project.

The command used can be customized with
`disproject-find-dir-command'."
  (interactive)
  (disproject-with-root
    (call-interactively disproject-find-dir-command)))

(transient-define-suffix disproject-find-dir-locals-file ()
  "Find `dir-locals-file' in project root.

If the \"--customize\" option is specified in transient state,
use `customize-dirlocals' to open the file.

The secondary dir-locals file may be accessed with
`disproject-find-dir-locals-2-file'."
  :class disproject-find-special-file-suffix
  :key "l"
  :file #'disproject-dir-locals-file
  :find-file-function #'disproject--find-dir-locals-file
  (interactive)
  (call-interactively #'disproject-find-special-file))

(transient-define-suffix disproject-find-dir-locals-2-file ()
  "Find secondary `dir-locals-file' in project root.

If the \"--customize\" option is specified in transient state,
use `customize-dirlocals' to open the file.

The primary dir-locals file may be accessed with
`disproject-find-dir-locals-file'."
  :class disproject-find-special-file-suffix
  :key "L"
  :file #'disproject-dir-locals-2-file
  :find-file-function #'disproject--find-dir-locals-file
  (interactive)
  (call-interactively #'disproject-find-special-file))

(transient-define-suffix disproject-find-file ()
  "Find file in project.

The command used can be customized with
`disproject-find-file-command'."
  (interactive)
  (disproject-with-root
    (call-interactively disproject-find-file-command)))

(transient-define-suffix disproject-find-line ()
  "Find matching line in open project buffers."
  (interactive)
  (disproject-with-root
    (call-interactively disproject-find-line-command)))

(transient-define-suffix disproject-find-regexp ()
  "Search project for regexp.

The command used can be customized with
`disproject-find-regexp-command'."
  (interactive)
  (disproject-with-root
    (call-interactively disproject-find-regexp-command)))

(transient-define-suffix disproject-find-special-file ()
  "Find a special file in project.

The `file' slot can be overridden in specifications to set a
\"special file\" that this command will find.  By default, this
command will find the project root directory.

`find-file' is the default function used for opening special
files.  This may be customized via the `find-file-function' slot;
the value should be a function that takes a single argument,
being the file to open.

See type `disproject-find-special-file-suffix' for documentation
on transient suffix slots."
  :class disproject-find-special-file-suffix
  (interactive)
  (disproject-with-root
    (let* ((suffix (transient-suffix-object))
           (file (disproject-find-special-file-suffix-file suffix))
           (find-file-function (oref suffix find-file-function)))
      (funcall find-file-function file))))

(transient-define-suffix disproject-flymake-diagnostics ()
  "View the Flymake diagnostics of the project."
  (interactive)
  (declare-function flymake-show-project-diagnostics "flymake")
  (disproject-with-root
    (call-interactively #'flymake-show-project-diagnostics)))

(transient-define-suffix disproject-forget-project ()
  "Forget a project."
  (interactive)
  (let ((set-message-function 'set-multi-message))
    (call-interactively #'project-forget-project)
    (call-interactively #'disproject-custom-prune-allowed-suffixes)))

(transient-define-suffix disproject-forget-projects-under ()
  "Forget projects under a directory."
  (interactive)
  (let ((set-message-function 'set-multi-message))
    (call-interactively #'project-forget-projects-under)
    (call-interactively #'disproject-custom-prune-allowed-suffixes)))

(transient-define-suffix disproject-forget-zombie-projects ()
  "Forget zombie projects."
  (interactive)
  (let ((set-message-function 'set-multi-message))
    (call-interactively #'project-forget-zombie-projects)
    (call-interactively #'disproject-custom-prune-allowed-suffixes)))

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
         (buf-name (disproject-process-buffer-name directory "git")))
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
         (buf-name (disproject-process-buffer-name directory "git")))
    (async-shell-command (concat arguments " -- " directory) buf-name)
    (switch-to-buffer-other-window buf-name)
    (setq default-directory directory)))

(transient-define-suffix disproject-kill-buffers ()
  "Kill all buffers related to project."
  (interactive)
  (disproject-with-root
    (call-interactively #'project-kill-buffers)))

(transient-define-suffix disproject-list-buffers ()
  "Display a list of open buffers for project."
  (interactive)
  (disproject-with-root
    (call-interactively #'project-list-buffers)))

(transient-define-suffix disproject-magit-todos-list ()
  "Open a `magit-todos-list' buffer for project."
  (interactive)
  (declare-function magit-todos-list-internal "magit-todos")
  (disproject-with-root
    (magit-todos-list-internal default-directory)))

(transient-define-suffix disproject-or-external-find-file ()
  "Find file in project or external roots.

The command used can be customized with
`disproject-or-external-find-file-command'."
  (interactive)
  (disproject-with-root
    (call-interactively disproject-or-external-find-file-command)))

(transient-define-suffix disproject-or-external-find-regexp ()
  "Find regexp in project or external roots.

The command used can be customized with
`disproject-or-external-find-regexp-command'."
  (interactive)
  (disproject-with-root
    (call-interactively disproject-or-external-find-regexp-command)))

(transient-define-suffix disproject-query-replace-regexp ()
  "Search project for regexp and `query-replace' matches."
  (interactive)
  (disproject-with-root
    (call-interactively #'project-query-replace-regexp)))

(transient-define-suffix disproject-remember-projects-open ()
  "Remember projects with open buffers."
  (interactive)
  (when-let* ((open-projects (disproject--open-projects)))
    (seq-each (lambda (project)
                (project-remember-project project t))
              open-projects)
    (project--write-project-list)))

(transient-define-suffix disproject-remember-projects-under ()
  "Remember projects under a directory with `project-remember-projects-under'."
  (interactive)
  (call-interactively #'project-remember-projects-under))

(transient-define-suffix disproject-shell ()
  "Start a shell in project.

The command used can be customized with the variable
`disproject-shell-command'."
  (interactive)
  (disproject-with-root
    (call-interactively disproject-shell-command)))

(transient-define-suffix disproject-shell-command ()
  "Run a shell command asynchronously in project root.

The `cmd' slot value of the current transient suffix object is
used as the command.  If nil, prompt for a command to run.

With prefix arg or the `always-read?' slot non-nil, always
prompt.

If the `allow-multiple-buffers?' slot of the current suffix
object is nil, `async-shell-command-buffer' will be set to
\\='confirm-kill-process so the process status can be accurately
reflected.

See type `disproject-shell-command-suffix' for documentation on
transient suffix slots."
  :class disproject-shell-command-suffix
  (interactive)
  (disproject-with-root
    (let* ((scope (disproject--scope))
           (obj (transient-suffix-object))
           (always-read? (oref obj always-read?))
           (command (if-let* ((cmd (disproject-shell-command-suffix-cmd obj)))
                        (if (or always-read? current-prefix-arg)
                            (read-shell-command "Async shell command: " cmd)
                          cmd)
                      (read-shell-command "Async shell command: ")))
           (project-name (project-name
                          (disproject-project-instance
                           (disproject-scope-selected-project-ensure scope))))
           (buf-name (disproject-process-suffix-buffer-name obj project-name))
           (allow-multiple-buffers? (oref obj allow-multiple-buffers?))
           (async-shell-command-buffer (if allow-multiple-buffers?
                                           async-shell-command-buffer
                                         'confirm-kill-process)))
      (async-shell-command command buf-name))))

(transient-define-suffix disproject-switch-project ()
  "Switch project to dispatch commands on.

Uses `project-prompt-project-dir' to switch project root
directories."
  :transient t
  (interactive)
  (disproject--switch-project (project-prompt-project-dir)))

(transient-define-suffix disproject-switch-project-open ()
  "Switch to an open project to dispatch commands on.

This is equivalent to `disproject-switch-project' but only shows
projects with open buffers when prompting for projects to switch
to."
  :transient t
  (interactive)
  (let* ((open-projects (mapcar #'project-root (disproject--open-projects)))
         ;; `project--file-completion-table' seems to accept any collection as
         ;; defined by `completing-read'.
         (completion-table (project--file-completion-table open-projects))
         (project-directory (completing-read "Select open project: "
                                             completion-table nil t)))
    (disproject--switch-project project-directory)))

(transient-define-suffix disproject-switch-to-buffer ()
  "Switch to buffer in project.

The command used can be customized with
`disproject-switch-to-buffer-command'."
  (interactive)
  (disproject-with-root
    (call-interactively disproject-switch-to-buffer-command)))

(transient-define-suffix disproject-synchronous-shell-command ()
  "Run `project-shell-command' in selected project."
  (interactive)
  (disproject-with-root
    (call-interactively #'project-shell-command)))

(transient-define-suffix disproject-vc-status ()
  "Dispatch a VC status command depending on the project backend.

The status command used depends on the variable
`disproject-vc-status-commands' - see its documentation for
configuring this function's behavior.  The chosen function will
be called interactively."
  :description
  (lambda ()
    (concat (if-let* ((project (disproject-scope-selected-project
                                (disproject--scope)))
                      (backend (disproject-project-backend project)))
                ;; The number chosen for this is somewhat arbitrary, but should
                ;; be fine as long as the end result is around the same size as
                ;; the other descriptions in its column.
                (truncate-string-to-width (symbol-name backend) 6 nil nil t)
              "VC")
            " status"))
  (interactive)
  (disproject-with-root
    (let* ((scope (disproject--scope))
           (project (disproject-scope-selected-project-ensure scope))
           (backend (disproject-project-backend project)))
      (unless backend
        (user-error "No backend found for project: %s"
                    (disproject-project-root project)))
      (call-interactively
       (if-let* ((command (alist-get backend disproject-vc-status-commands))
                 ((fboundp command)))
           command
         (alist-get nil disproject-vc-status-commands))))))


;;;;; Transient prefixes.

;;;;;; Auxiliary.

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
      (display-warning
       'disproject
       (format-message "Value: `%S'\nDoes not match type: %s" value type))
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
            (save-excursion
              (let ((pp-max-width 60))
                (pp-emacs-lisp-code custom-suffixes)))
            ;; Do some heuristics to adjust pretty-printing output.
            ;;
            ;; XXX: Some of these heuristics can produce slightly incorrect
            ;; output from substituting forms in strings too.
            (dolist (pair '(;; Separate adjacent brackets with ") (" form, which
                            ;; seems to often be the cause of unexpectedly long
                            ;; lines.
                            (") (" . ")\n(")
                            ("\\] \\[" . "]\n[")
                            ;; Move keywords onto their own lines.
                            ("[[:blank:]]*:\\(\\w\\)" . "\n:\\1")
                            ;; Remove superfluous empty lines.
                            ("\n[\n[:blank:]]*\n" . "\n")
                            ;; Open brackets at the end of a line can be joined
                            ;; with the next line.
                            ("(\n[[:blank:]]*" . "(")
                            ("\\[\n[[:blank:]]*" . "[")))
              (save-excursion
                (pcase-exhaustive pair
                  (`(,regexp . ,replacement)
                   (while (re-search-forward regexp nil t)
                     (replace-match replacement))))))
            ;; Fix indentation from some of the heuristics applied.
            (goto-char (point-max))
            (backward-sexp)
            (indent-pp-sexp)

            (setq-local cursor-type nil)
            (set-buffer-modified-p nil)
            (goto-char (point-min)))
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
  (format (propertize "Project: %s" 'face 'transient-heading)
          (if-let* ((scope (disproject--scope))
                    (project (disproject-scope-selected-project scope))
                    (root (disproject-project-root project)))
              (propertize root 'face 'transient-value)
            (propertize "None detected" 'face 'transient-inapt-suffix))))

;;;;;; Definitions.

;;;###autoload (autoload 'disproject-dispatch "disproject" nil t)
(transient-define-prefix disproject-dispatch ()
  "Dispatch some command for a project.

See Info node `(transient)Modifying Existing Transients' for
information on inserting user-defined suffix commands to this
menu."
  :class disproject-prefix
  :refresh-suffixes t
  [:description
   disproject--selected-project-description
   ("p" "Switch project" disproject-switch-project)
   ("P" "Switch to open project" disproject-switch-project-open)
   ("C-p" "Manage projects" disproject-manage-projects-dispatch)]
  disproject--global-options-group
  ["Main commands"
   [ :advice disproject-with-env-apply
     :advice* disproject-with-env-apply
     ("b" "Switch buffer" disproject-switch-to-buffer)
     ("B" "Buffer list" disproject-list-buffers)
     ("c" "Compile" disproject-compile
      :buffer-id "compile")
     ("d" "Dired" disproject-dired)
     ("k" "Kill buffers" disproject-kill-buffers)
     ("s" "Shell" disproject-shell)
     ("v" disproject-vc-status
      :inapt-if-not disproject-prefix--version-control-apt?)
     ("w" "Diagnostics" disproject-flymake-diagnostics
      :if disproject-prefix--feature-flymake?)]
   [ :advice disproject-with-env-apply
     :advice* disproject-with-env-apply
     ("!" "Shell command" disproject-synchronous-shell-command)
     ("&" "Async shell command" disproject-shell-command
      :buffer-id "shell-command"
      :allow-multiple-buffers? t)
     ("M-x" "Extended command" disproject-execute-extended-command)]
   ["Find"
    :advice disproject-with-env-apply
    :advice* disproject-with-env-apply
    ("D" "directory" disproject-find-dir)
    ("f" "file" disproject-find-file)
    ("F" "file (+external)" disproject-or-external-find-file)
    ("g" "regexp" disproject-find-regexp)
    ("G" "regexp (+external)" disproject-or-external-find-regexp)
    ("r" "regexp and replace" disproject-query-replace-regexp)
    ("l" "special file" disproject-find-special-file-dispatch)
    ("L" "line in buffers" disproject-find-line)
    ("T" "todos" disproject-magit-todos-list
     :if disproject-prefix--magit-todos-apt?)]]
  [ :class transient-row
    :advice disproject-with-env-apply
    :advice* disproject-with-env-apply
    ("SPC" "Custom dispatch" disproject-custom-dispatch
     :transient transient--do-replace)])

(transient-define-prefix disproject-custom-dispatch ()
  "Dispatch custom suffix commands.

This prefix can be configured with `disproject-custom-suffixes';
see its documentation for more information.

Suffixes have an associated buffer that is tracked for command
process activity; this is shown in the menu in the form of
\"[CHAR]\", where the string is color-coded and CHAR is a single
character.  These characters represent the following states:

  [a]: Command is active.
  [i]: Command is inactive."
  :class disproject--custom-suffixes-prefix
  disproject-global-header-group
  [:class transient-subgroups
   :setup-children disproject-custom--setup-suffixes]
  [ :class transient-row
    :advice disproject-with-env-apply
    :advice* disproject-with-env-apply
    ("SPC" "Main dispatch" disproject-dispatch
     :transient transient--do-replace)
    ("!" "Alternative compile" disproject-compile
     :buffer-id "compile")])

(transient-define-prefix disproject-find-special-file-dispatch ()
  "Dispatch command to find a special file in project.

This prefix can be configured with
`disproject-find-special-file-suffixes'; see its documentation
for more information."
  :class disproject--selected-project-prefix
  disproject-global-header-group
  [:class transient-subgroups
   :setup-children disproject--setup-find-special-file-suffixes])

(transient-define-prefix disproject-manage-projects-dispatch ()
  "Dispatch commands for managing projects."
  ["New"
   ("n g c" "git clone" magit-clone
    :if disproject-prefix--feature-magit-clone?)
   ("n g c" "git clone" disproject-git-clone-fallback
    :if disproject-prefix--git-clone-fallback-apt?)
   ("n g i" "git init" magit-init
    :if disproject-prefix--feature-magit-status?)
   ("n g i" "git init" disproject-git-init-fallback
    :if disproject-prefix--git-init-fallback-apt?)]
  ["Forget"
   ;; TODO: Could add an option to close buffers of the project to forget.
   ("f p" "a project" disproject-forget-project)
   ("f u" "projects under..." disproject-forget-projects-under)
   ("f z" "zombie projects" disproject-forget-zombie-projects)]
  ["Remember"
   ("r o" "open projects" disproject-remember-projects-open)
   ("r u" "projects under..." disproject-remember-projects-under)])

(transient-define-prefix disproject-display-buffer-action-dispatch ()
  "Dispatch commands for setting transient `display-buffer' action overrides.

See `disproject-display-buffer-action-set' for documentation on
defining commands that can modify the override state."
  :class disproject-prefix
  ["Display buffer to"
   ("b" "below selected window" disproject-display-buffer-action-set
    :display-buffer-action (display-buffer-below-selected
                            (inhibit-multiple-displays . t)))
   ("B" "bottom of frame" disproject-display-buffer-action-set
    :display-buffer-action (display-buffer-at-bottom))
   ("f" "full frame" disproject-display-buffer-action-set
    :display-buffer-action (display-buffer-full-frame))
   ("F" "new frame" disproject-display-buffer-action-set
    :display-buffer-action (display-buffer-pop-up-frame
                            (inhibit-multiple-displays . t)))
   ("l" "least-recent window" disproject-display-buffer-action-set
    :display-buffer-action (display-buffer-use-least-recent-window))
   ("n" "no window" disproject-display-buffer-action-set
    :display-buffer-action (display-buffer-no-window
                            (allow-no-window . t)
                            ;; If a command has some prompt like a
                            ;; `completing-read', assume the buffer display will
                            ;; be important, but also ephemeral, so let the user
                            ;; see whatever the command wants to show in that
                            ;; case.
                            (inhibit-multiple-displays . t)))
   ("o" "other window" disproject-display-buffer-action-set
    :display-buffer-action (display-buffer-use-some-window
                            (inhibit-same-window . t)))
   ("s" "same window" disproject-display-buffer-action-set
    :display-buffer-action (display-buffer-same-window
                            (inhibit-same-window . nil)))]
  [("," "Clear override" disproject-display-buffer-action-set)])

(transient-augment-suffix disproject-display-buffer-action-dispatch
  :key ","
  :description
  (lambda ()
    (concat "Override buffer display action ("
            (let* ((scope (disproject--scope))
                   (action (disproject-scope-display-buffer-action scope)))
              (pcase-exhaustive action
                ('nil
                 (propertize "none" 'face 'transient-inactive-value))
                (`(,_functions . ,(map ('description (and (pred (not null))
                                                          description))))
                 (propertize description 'face 'transient-active-infix))
                (_
                 (propertize "BUG: display-buffer-action has no description"
                             'face 'error))))
            ")")))


;;;; Short-documentation groups.

(define-short-documentation-group disproject-customizable-suffixes
  "Customizable Disproject suffixes
(Use `C-h o' on symbols for more documentation)"
  (disproject-compile)
  (disproject-display-buffer-action-set)
  (disproject-find-special-file)
  (disproject-shell-command))

(define-short-documentation-group disproject-environment
  "Environments from Disproject transient settings
(Use `C-h o' on symbols for more documentation)"
  (disproject-with-env-apply)
  (disproject-with-env)
  (disproject-with-root-apply)
  (disproject-with-root)
  (disproject-with-env+root-apply)
  (disproject-with-env+root)
  (disproject-with-environment))

(provide 'disproject)
;;; disproject.el ends here

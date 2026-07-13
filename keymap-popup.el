;;; keymap-popup.el --- Described keymaps with popup help  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Free Software Foundation, Inc.

;; Author: Thanos Apollo <public@thanosapollo.org>
;; Version: 0.4.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: convenience
;; URL: https://codeberg.org/thanosapollo/emacs-keymap-popup

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

;; Two macros: `keymap-popup-define' produces a real `defvar-keymap'
;; with embedded descriptions; `keymap-popup-annotate' adds popup
;; descriptions to an existing keymap.  `keymap-popup' displays
;; either as an interactive menu.  One definition, two uses:
;; direct key dispatch and popup help.
;;
;; Rendering is pure: commands mutate state in the user's buffer and the
;; popup re-reads it on refresh.  A compact session plist owns only popup
;; navigation, transient-map, and display lifecycle state.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(defgroup keymap-popup nil
  "Described keymaps with popup help."
  :group 'convenience)

(defcustom keymap-popup-display-action
  '(display-buffer-in-side-window (side . bottom))
  "Display action for the popup buffer.
Only used by `keymap-popup-backend-side-window'.
Common values:
  (display-buffer-in-side-window (side . bottom))  - frame-wide
  (display-buffer-below-selected)                  - current window only"
  :type display-buffer--action-custom-type
  :group 'keymap-popup)

(defcustom keymap-popup-backend #'keymap-popup-backend-side-window
  "Function returning a display backend plist (:show :fit :hide).
Each backend function receives a single buffer argument.
:show displays the buffer, :fit refits after content changes,
:hide removes the display and cleans up."
  :type '(choice (function-item :tag "Side window" keymap-popup-backend-side-window)
                 (function-item :tag "Child frame" keymap-popup-backend-child-frame)
                 (function :tag "Custom backend"))
  :group 'keymap-popup)

(defcustom keymap-popup-child-frame-parameters
  '((no-accept-focus . t)
    (no-focus-on-map . t)
    (min-width . t)
    (min-height . t)
    (border-width . 0)
    (child-frame-border-width . 1)
    (left-fringe . 0)
    (right-fringe . 0)
    (vertical-scroll-bars . nil)
    (horizontal-scroll-bars . nil)
    (menu-bar-lines . 0)
    (tool-bar-lines . 0)
    (tab-bar-lines . 0)
    (no-other-frame . t)
    (no-other-window . t)
    (no-delete-other-windows . t)
    (unsplittable . t)
    (undecorated . t)
    (cursor-type . nil)
    (no-special-glyphs . t)
    (desktop-dont-save . t))
  "Frame parameters for the child-frame backend.
These are merged with runtime parameters (parent-frame, minibuffer,
visibility) when creating the child frame."
  :type '(alist :key-type symbol :value-type sexp)
  :group 'keymap-popup)

(defcustom keymap-popup-buffer-parameters
  '((buffer-read-only . t)
    (cursor-type . nil)
    (mode-line-format
     . (" "
        (:eval (and-let* ((active (plist-get keymap-popup--session :active))
                          (exit-key (plist-get active :exit-key)))
                 (propertize (format " %s " exit-key)
                             'face 'keymap-popup-key)))
        " "
        (:eval (or (plist-get (plist-get keymap-popup--session :active)
                             :resolved-docstring)
                   ""))))
    (header-line-format . nil)
    (tab-line-format . nil)
    (left-margin-width . 1)
    (right-margin-width . 1))
  "Buffer-local parameters applied to the popup buffer.
Each entry is (VARIABLE . VALUE).  Users can remove entries to
keep defaults or change values to customize the popup appearance."
  :type '(alist :key-type symbol :value-type sexp)
  :group 'keymap-popup)

(defcustom keymap-popup-persistent nil
  "When non-nil, the popup stays open after every key press.
All suffix commands execute and refresh the popup in place.
Only the exit key and \\`C-g' dismiss it.
Can also be set per-keymap via the `:persistent' keyword in
`keymap-popup-define' or `keymap-popup-annotate'."
  :type 'boolean
  :group 'keymap-popup)

(defcustom keymap-popup-default-popup-key "h"
  "Default key to open the popup in keymaps defined with `keymap-popup-define'.
Applied by `keymap-popup-define' when :popup-key is not specified.
Read when the definition is loaded, so customize it before loading
packages that define popups.  Not applied by `keymap-popup-annotate'
\(must be given explicitly)."
  :type 'key
  :group 'keymap-popup)

(defcustom keymap-popup-default-exit-key "q"
  "Default key to dismiss the popup.
Used as the fallback when the popup is shown and the keymap stores
no explicit :exit-key, for both `keymap-popup-define' and
`keymap-popup-annotate'."
  :type 'key
  :group 'keymap-popup)

(defconst keymap-popup--buffer-name "*keymap-popup*"
  "Name of the singleton buffer used to display the popup.")

;;; Faces

(defface keymap-popup-key
  '((t :inherit help-key-binding))
  "Face for key bindings in the popup.")

(defface keymap-popup-group-header
  '((t :weight bold))
  "Face for group headers in the popup.")

(defface keymap-popup-value
  '((t :inherit font-lock-string-face :weight bold))
  "Face for switch values in the popup.")

(defface keymap-popup-submenu
  '((t :inherit font-lock-type-face))
  "Face for sub-menu entries in the popup.")

(defface keymap-popup-inapt
  '((t :inherit shadow))
  "Face for inapt (disabled) entries in the popup.")

;;; Keymap metadata

(defun keymap-popup--meta (keymap prop)
  "Get popup metadata PROP from KEYMAP via pseudo-key lookup."
  (let ((val (lookup-key keymap (vector 'keymap-popup prop))))
    ;; lookup-key returns an integer when the vector is a prefix of a
    ;; longer binding rather than an exact match.  Filter that out.
    (and (not (numberp val)) val)))

;; Values are stored via define-key, so t cannot be used as a value
;; (it means "default binding").  Use symbols like 'yes and 'no instead.
(gv-define-setter keymap-popup--meta (val keymap prop)
  `(define-key ,keymap (vector 'keymap-popup ,prop) ,val))

(defun keymap-popup--attach-meta (keymap rows &rest opts)
  "Attach popup descriptions ROWS and metadata OPTS to KEYMAP.
OPTS is a plist accepting :exit-key, :description, and
:persistent; other keys are ignored.  Reattaching replaces all
previous metadata, so omitted options return to their defaults.
:persistent is stored as `yes' or `no' because metadata lives in
`define-key' bindings, where t means \"default binding\".
Returns KEYMAP."
  (let ((persistent (and (plist-member opts :persistent)
                         (if (plist-get opts :persistent) 'yes 'no))))
    (setf (keymap-popup--meta keymap 'descriptions) rows
          (keymap-popup--meta keymap 'exit-key) (plist-get opts :exit-key)
          (keymap-popup--meta keymap 'description) (plist-get opts :description)
          (keymap-popup--meta keymap 'persistent) persistent))
  keymap)

;;; Parsers

(defconst keymap-popup--entry-properties
  '(:if :inapt-if :stay-open :c-u)
  "Properties accepted by popup entries.")

(defconst keymap-popup--group-properties
  '(:if :inapt-if)
  "Properties accepted by popup groups.")

(defun keymap-popup--invalid (context format-string &rest args)
  "Signal an invalid popup declaration in CONTEXT.
FORMAT-STRING and ARGS describe the error."
  (error "keymap-popup: %s: %s"
         context (apply #'format format-string args)))

(defun keymap-popup--parse-properties (properties allowed context)
  "Parse PROPERTIES accepted by ALLOWED for CONTEXT.
The :stay-open property may omit its value."
  (named-let parse ((rest properties) (seen nil))
    (cond
     ((null rest) nil)
     ((not (consp rest))
      (keymap-popup--invalid context "malformed property list"))
     (t
      (let ((property (car rest)))
        (unless (keywordp property)
          (keymap-popup--invalid context "expected property, got %S" property))
        (unless (memq property allowed)
          (keymap-popup--invalid context "unknown property %S" property))
        (when (memq property seen)
          (keymap-popup--invalid context "duplicate property %S" property))
        (if (and (eq property :stay-open)
                 (or (null (cdr rest)) (keywordp (cadr rest))))
            (cons property
                  (cons t (parse (cdr rest) (cons property seen))))
          (unless (cdr rest)
            (keymap-popup--invalid context "property %S needs a value" property))
          (cons property
                (cons (cadr rest)
                      (parse (cddr rest) (cons property seen))))))))))

(defun keymap-popup--function-form-p (value)
  "Return non-nil when VALUE is a quoted or literal function form."
  (and (consp value) (memq (car value) '(closure function lambda))))

(defun keymap-popup--function-designator-p (value)
  "Return non-nil when VALUE is a function object or function symbol."
  (or (functionp value)
      (and (symbolp value)
           (not (memq value '(nil t)))
           (not (keywordp value)))))

(defun keymap-popup--valid-description-p (description runtime)
  "Return non-nil when DESCRIPTION is valid for RUNTIME input."
  (or (stringp description)
      (keymap-popup--function-designator-p description)
      (and (not runtime) (consp description))))

(defun keymap-popup--validate-description (description context runtime)
  "Validate DESCRIPTION for CONTEXT and RUNTIME input."
  (unless (keymap-popup--valid-description-p description runtime)
    (keymap-popup--invalid context
                           "description must be a string or function, got %S"
                           description)))

(defun keymap-popup--valid-function-value-p (value runtime)
  "Return non-nil when VALUE is a function value for RUNTIME input."
  (or (null value)
      (keymap-popup--function-designator-p value)
      (and (not runtime) (consp value))))

(defun keymap-popup--valid-string-value-p (value runtime)
  "Return non-nil when VALUE is a string value for RUNTIME input."
  (or (null value)
      (stringp value)
      (and (not runtime)
           (not (eq value t))
           (not (keywordp value))
           (or (symbolp value) (consp value)))))

(defun keymap-popup--validate-properties (properties context runtime)
  "Validate parsed PROPERTIES for CONTEXT and RUNTIME input."
  (named-let validate ((rest properties))
    (when rest
      (pcase-exhaustive (car rest)
        ((or :if :inapt-if)
         (unless (keymap-popup--valid-function-value-p (cadr rest) runtime)
           (keymap-popup--invalid context "%S needs a function" (car rest))))
        (:stay-open
         (when (and runtime (not (memq (cadr rest) '(nil t))))
           (keymap-popup--invalid context ":stay-open needs a boolean")))
        (:c-u
         (unless (keymap-popup--valid-string-value-p (cadr rest) runtime)
           (keymap-popup--invalid context ":c-u needs a string"))))
      (validate (cddr rest))))
  properties)

(defun keymap-popup--valid-command-p (command runtime)
  "Return non-nil when COMMAND is valid for RUNTIME input."
  (let ((command-symbol (and (symbolp command)
                             (not (memq command '(nil t)))
                             (not (keywordp command)))))
    (if runtime
        (or command-symbol (commandp command))
      (or command-symbol (functionp command) (consp command)))))

(defun keymap-popup--entry (key description type &rest properties)
  "Return a canonical popup entry plist.
KEY, DESCRIPTION, and TYPE identify the entry.  PROPERTIES supplies
the type-specific payload and optional predicates."
  (list :key key
        :description description
        :type type
        :command (plist-get properties :command)
        :variable (plist-get properties :variable)
        :target (plist-get properties :target)
        :if (plist-get properties :if)
        :inapt-if (plist-get properties :inapt-if)
        :stay-open (plist-get properties :stay-open)
        :c-u (plist-get properties :c-u)))

(defun keymap-popup--group (name entries &rest properties)
  "Return a canonical popup group with NAME, ENTRIES, and PROPERTIES."
  (list :name name
        :entries entries
        :if (plist-get properties :if)
        :inapt-if (plist-get properties :inapt-if)))

(defun keymap-popup--validate-key (key mode context)
  "Validate entry KEY for parser MODE and CONTEXT."
  (cond
   ((stringp key)
    (unless (key-valid-p key)
      (keymap-popup--invalid context "invalid key %S" key)))
   ((and (symbolp key) (not (memq key '(nil t))) (not (keywordp key)))
    (when (eq mode 'define)
      (keymap-popup--invalid context "definitions require a key string")))
   (t (keymap-popup--invalid context "invalid key or command %S" key))))

(defun keymap-popup--parse-annotated-entry (key spec context runtime)
  "Parse annotated SPEC for command KEY in CONTEXT.
RUNTIME means values have already been evaluated."
  (let* ((parts (if (or (not (consp spec))
                        (functionp spec)
                        (keymap-popup--function-form-p spec))
                    (list spec)
                  spec))
         (description (car parts))
         (properties (keymap-popup--validate-properties
                      (keymap-popup--parse-properties
                       (cdr parts) keymap-popup--entry-properties context)
                      context runtime)))
    (keymap-popup--validate-description description context runtime)
    (unless (keymap-popup--valid-command-p key runtime)
      (keymap-popup--invalid context "invalid command %S" key))
    (apply #'keymap-popup--entry nil description 'suffix
           :command key properties)))

(defun keymap-popup--parse-keyed-entry (key spec context runtime)
  "Parse keyed SPEC for KEY in CONTEXT.
RUNTIME means values have already been evaluated."
  (unless (and (proper-list-p spec) (>= (length spec) 2))
    (keymap-popup--invalid context
                           "expected (DESCRIPTION COMMAND-OR-TYPE ...)"))
  (let ((description (car spec))
        (kind (cadr spec))
        (payload (cddr spec)))
    (keymap-popup--validate-description description context runtime)
    (pcase kind
      (:switch
       (unless (and (symbolp (car payload))
                    (not (memq (car payload) '(nil t)))
                    (not (keywordp (car payload))))
         (keymap-popup--invalid context "switch needs a variable symbol"))
       (apply #'keymap-popup--entry key description 'switch
              :variable (car payload)
              (keymap-popup--validate-properties
               (keymap-popup--parse-properties
                (cdr payload) keymap-popup--entry-properties context)
               context runtime)))
      (:keymap
       (unless (and (car payload)
                    (not (eq (car payload) t))
                    (not (keywordp (car payload))))
         (keymap-popup--invalid context "submenu needs a keymap"))
       (when (and runtime (not (keymapp (car payload))))
         (keymap-popup--invalid context "submenu target is not a keymap"))
       (apply #'keymap-popup--entry key description 'keymap
              :target (car payload)
              (keymap-popup--validate-properties
               (keymap-popup--parse-properties
                (cdr payload) keymap-popup--entry-properties context)
               context runtime)))
      (_
       (unless (keymap-popup--valid-command-p kind runtime)
         (keymap-popup--invalid context "invalid command %S" kind))
       (apply #'keymap-popup--entry key description 'suffix
              :command kind
              (keymap-popup--validate-properties
               (keymap-popup--parse-properties
                payload keymap-popup--entry-properties context)
               context runtime))))))

(defun keymap-popup--parse-entry (key spec &optional mode)
  "Parse binding SPEC for KEY into a plist.
KEY is a key string for normal entries, or a command symbol for
annotated entries.  SPEC is (DESCRIPTION COMMAND-OR-TYPE &rest PROPS)
for key-based entries, or (DESCRIPTION &rest PROPS) for annotated ones.
MODE is `define', `annotate', or `runtime'; omitted means macro syntax."
  (let* ((mode (or mode 'macro))
         (context (format "entry %S" key))
         (runtime (eq mode 'runtime)))
    (keymap-popup--validate-key key mode context)
    (if (stringp key)
        (keymap-popup--parse-keyed-entry key spec context runtime)
      (keymap-popup--parse-annotated-entry key spec context runtime))))

(defun keymap-popup--split-groups (bindings)
  "Split BINDINGS at :group and :row keywords.
Returns a list of rows, each row a list of (NAME . FLAT-ENTRIES) chunks.
`:group' starts a new group within the current row.
`:row' starts a new row."
  (named-let split ((rest bindings) (name nil) (entries nil)
                    (groups nil) (rows nil))
    (let ((flushed (if entries
                       (cons (cons name (reverse entries)) groups)
                     groups)))
      (cond
       ((null rest)
        (reverse (if flushed (cons (reverse flushed) rows) rows)))
       ((eq (car rest) :row)
        (split (cdr rest) nil nil nil
               (if flushed (cons (reverse flushed) rows) rows)))
       ((eq (car rest) :group)
        (unless (cdr rest)
          (keymap-popup--invalid "bindings" ":group needs a name"))
        (split (cddr rest) (cadr rest) nil flushed rows))
       ((keywordp (car rest))
        (keymap-popup--invalid "bindings" "unknown directive %S" (car rest)))
       ((null (cdr rest))
        (keymap-popup--invalid "bindings" "entry %S needs a specification"
                               (car rest)))
       (t
        (split (cddr rest) name
               (cons (cons (car rest) (cadr rest)) entries)
               groups rows))))))

(defun keymap-popup--parse-group-name (raw &optional mode)
  "Parse RAW group name into (NAME . PROPS).
RAW is a string, a lambda, or a list (NAME :if PRED :inapt-if PRED).
A list whose car is not `lambda' is treated as a name with properties.
MODE controls whether values have already been evaluated."
  (let* ((runtime (eq mode 'runtime))
         (context (format "group %S" raw))
         (specified (and (consp raw)
                         (not (functionp raw))
                         (not (keymap-popup--function-form-p raw))))
         (name (if specified (car raw) raw))
         (properties (and specified
                          (keymap-popup--validate-properties
                           (keymap-popup--parse-properties
                            (cdr raw) keymap-popup--group-properties context)
                           context runtime))))
    (when name
      (keymap-popup--validate-description name context runtime))
    (cons name properties)))

(defun keymap-popup--parse-chunk (chunk &optional mode)
  "Parse CHUNK into a group plist according to parser MODE."
  (let* ((name-props (keymap-popup--parse-group-name (car chunk) mode))
         (name (car name-props))
         (group-props (cdr name-props))
         (entries (cl-loop for (k . v) in (cdr chunk)
                           collect (keymap-popup--parse-entry k v mode))))
    (apply #'keymap-popup--group name entries group-props)))

(defun keymap-popup--parse-bindings (bindings &optional mode)
  "Parse BINDINGS into a list of rows.
Each row is a list of group plists with :name and :entries.
MODE controls whether values have already been evaluated."
  (mapcar (lambda (row)
            (mapcar (lambda (chunk)
                      (keymap-popup--parse-chunk chunk mode))
                    row))
          (keymap-popup--split-groups bindings)))

(defun keymap-popup--if-allows-p (plist)
  "Return non-nil when PLIST has no :if or its :if predicate is non-nil."
  (let ((pred (plist-get plist :if)))
    (or (null pred) (funcall pred))))

(defun keymap-popup--inapt-active-p (plist)
  "Return non-nil when PLIST's :inapt-if predicate is currently active."
  (and-let* ((pred (plist-get plist :inapt-if)))
    (funcall pred)))

(defun keymap-popup--combine-preds (a b)
  "AND-combine zero-arg predicates A and B.  Either may be nil."
  (cond ((null a) b)
        ((null b) a)
        (t (lambda () (and (funcall a) (funcall b))))))

(defun keymap-popup--merge-group-preds (entry group)
  "Return ENTRY with GROUP's :if and :inapt-if AND-merged into it.
The entry is returned unchanged when GROUP has no predicates."
  (let ((g-if (plist-get group :if))
        (g-inapt (plist-get group :inapt-if)))
    (if (not (or g-if g-inapt))
        entry
      (let* ((merged-if (keymap-popup--combine-preds
                         g-if (plist-get entry :if)))
             (merged-inapt (keymap-popup--combine-preds
                            g-inapt (plist-get entry :inapt-if)))
             (r (copy-sequence entry))
             (r (if merged-if (plist-put r :if merged-if) r))
             (r (if merged-inapt (plist-put r :inapt-if merged-inapt) r)))
        r))))

(defun keymap-popup--group-entries-merged (group)
  "Return GROUP's entries with the group's :if/:inapt-if merged in."
  (mapcar (lambda (e) (keymap-popup--merge-group-preds e group))
          (plist-get group :entries)))

(defun keymap-popup--flatten-with-groups (rows)
  "Flatten ROWS into a list of entries with group :if/:inapt-if merged in."
  (mapcan (lambda (row) (mapcan #'keymap-popup--group-entries-merged row))
          rows))

;;; Generated symbol names

(defun keymap-popup--toggle-name (map-name var)
  "Return the symbol naming the toggle command for VAR in MAP-NAME."
  (intern (format "%s--toggle-%s" map-name var)))

(defun keymap-popup--enter-name (map-name target)
  "Return the symbol naming the sub-menu enterer for TARGET in MAP-NAME."
  (intern (format "%s--enter-%s" map-name target)))

(defun keymap-popup--launcher-name (map-name)
  "Return the symbol naming the popup launcher for MAP-NAME."
  (intern (concat (symbol-name map-name) "-popup")))

(defun keymap-popup--entry-command (map-name entry)
  "Return the command to bind in MAP-NAME's keymap for ENTRY.
Switch and keymap entries name generated commands; suffix entries
retain their declared command."
  (pcase-exhaustive (plist-get entry :type)
    ('suffix (plist-get entry :command))
    ('switch (keymap-popup--toggle-name map-name (plist-get entry :variable)))
    ('keymap (keymap-popup--enter-name map-name (plist-get entry :target)))))

;;; Macro helpers

(defun keymap-popup--make-if-filter (pred)
  "Return a `menu-item' :filter exposing the binding only when PRED is non-nil."
  (lambda (b) (and (funcall pred) b)))

(defun keymap-popup--wrap-binding-form (cmd-form if-pred)
  "Wrap CMD-FORM with a `menu-item' :filter when IF-PRED is set.
IF-PRED is a form that evaluates to a zero-arg predicate; a nil
filter result makes the key act as unbound.  `:inapt-if' is not a
keymap concern -- it is enforced by the popup wrapper at keypress
time.  The filter closure is built by a helper in this file so it
remains lexical regardless of the caller."
  (if if-pred
      `(list 'menu-item "" ,cmd-form :filter
             (keymap-popup--make-if-filter ,if-pred))
    cmd-form))

(defun keymap-popup--build-keymap-pairs (map-name entries)
  "Build flat key/command list for `defvar-keymap' from ENTRIES.
MAP-NAME is used to derive generated command names.  Entries with
:if expand to a `menu-item' form with :filter."
  (cl-loop for entry in entries
           for cmd = (keymap-popup--entry-command map-name entry)
           for cmd-form = (if (symbolp cmd) `#',cmd cmd)
           append (list (plist-get entry :key)
                        (keymap-popup--wrap-binding-form
                         cmd-form (plist-get entry :if)))))

(defun keymap-popup--build-entry-form (entry &optional map-name)
  "Build a canonical entry form for ENTRY.
When MAP-NAME is non-nil, switch and keymap entries also store the
generated toggle/enter command as :command, so every entry is
command-addressable (annotate entries have no generated commands,
so their builders pass no MAP-NAME)."
  (let* ((type (plist-get entry :type))
         (command (pcase-exhaustive type
                    ('suffix (plist-get entry :command))
                    ((or 'switch 'keymap)
                     (and map-name
                          (keymap-popup--entry-command map-name entry)))))
         (command-form (and command
                            (if (symbolp command) `#',command command)))
         (variable-form (and (eq type 'switch)
                             `(quote ,(plist-get entry :variable)))))
    `(keymap-popup--entry
      ,(plist-get entry :key)
      ,(plist-get entry :description)
      ',type
      :command ,command-form
      :variable ,variable-form
      :target ,(and (eq type 'keymap) (plist-get entry :target))
      :if ,(plist-get entry :if)
      :inapt-if ,(plist-get entry :inapt-if)
      :stay-open ,(plist-get entry :stay-open)
      :c-u ,(plist-get entry :c-u))))

(defun keymap-popup--build-group-form (group &optional map-name)
  "Build a canonical form for one GROUP plist.
MAP-NAME is passed to `keymap-popup--build-entry-form'."
  `(keymap-popup--group
    ,(plist-get group :name)
    (list ,@(mapcar (lambda (entry)
                      (keymap-popup--build-entry-form entry map-name))
                    (plist-get group :entries)))
    :if ,(plist-get group :if)
    :inapt-if ,(plist-get group :inapt-if)))

(defun keymap-popup--build-descriptions-form (rows &optional map-name)
  "Build a `list' form that constructs descriptions at load time.
ROWS is a list of rows, each row a list of groups.  Uses `list'
calls so lambdas in :if/:inapt-if/:description get compiled.
MAP-NAME is passed down to the entry forms."
  `(list ,@(mapcar (lambda (row)
                     `(list ,@(mapcar (lambda (group)
                                        (keymap-popup--build-group-form
                                         group map-name))
                                      row)))
                   rows)))

(defun keymap-popup--build-switch-forms (map-name entries)
  "Build `defvar-local' + toggle `defun' forms for switch ENTRIES in MAP-NAME."
  (mapcan (lambda (e)
            (let* ((var (plist-get e :variable))
                   (toggle (keymap-popup--toggle-name map-name var)))
              `((defvar-local ,var nil
                  ,(format "Buffer-local switch state.\nToggled by `%s'."
                           toggle))
                (defun ,toggle ()
                  ,(format "Toggle the buffer-local switch.\nSets `%s' in the current buffer." var)
                  (interactive)
                  (setq ,var (not ,var))
                  (message "%s: %s" ',var (if ,var "on" "off"))))))
          entries))

(defun keymap-popup--build-enter-forms (map-name entries)
  "Build sub-menu enter defun forms for keymap ENTRIES in MAP-NAME."
  (mapcar (lambda (e)
            (let* ((target (plist-get e :target))
                   (enter (keymap-popup--enter-name map-name target)))
              `(defun ,enter ()
                 ,(format "Show popup for `%s' (sub-menu of `%s')."
                          target map-name)
                 (interactive)
                 (keymap-popup ,target))))
          entries))

(defun keymap-popup--build-launcher-form (map-name launcher)
  "Build the launcher defun form named LAUNCHER for MAP-NAME."
  `(defun ,launcher ()
     ,(format "Show popup help for `%s'." map-name)
     (interactive)
     (keymap-popup ,map-name)))

(defun keymap-popup--build-declaration-forms (map-name parent entries)
  "Build bare `defvar' forms for MAP-NAME, PARENT, and targets in ENTRIES."
  (mapcar (lambda (symbol) `(defvar ,symbol))
          (seq-uniq
           (append (list map-name)
                   (and parent (symbolp parent) (list parent))
                   (cl-loop for entry in entries
                            for target = (plist-get entry :target)
                            when (and (eq (plist-get entry :type) 'keymap)
                                      target
                                      (symbolp target))
                            collect target)))))

(defun keymap-popup--build-attach-form (map-name rows-form exit-key
                                                 description persistent
                                                 persistent-p)
  "Build the `keymap-popup--attach-meta' call form for MAP-NAME.
ROWS-FORM constructs the descriptions at load time.  Each of
EXIT-KEY and DESCRIPTION is passed only when non-nil.  PERSISTENT
is passed when PERSISTENT-P is non-nil."
  `(keymap-popup--attach-meta ,map-name ,rows-form
                              ,@(and exit-key `(:exit-key ,exit-key))
                              ,@(and description `(:description ,description))
                              ,@(and persistent-p
                                      `(:persistent ,persistent))))

;;; Macro

(defconst keymap-popup--define-options
  '(:popup-key :exit-key :parent :description :persistent)
  "Options accepted by `keymap-popup-define'.")

(defconst keymap-popup--annotate-options
  '(:popup-key :exit-key :description :persistent)
  "Options accepted by `keymap-popup-annotate'.")

(defconst keymap-popup--attach-options
  '(:exit-key :description :persistent)
  "Options accepted by `keymap-popup-attach'.")

(defun keymap-popup--parse-options (values allowed context)
  "Parse leading options from VALUES for CONTEXT.
ALLOWED lists recognized option names.  Return (OPTIONS . REST)."
  (named-let parse ((rest values) (options nil) (seen nil))
    (cond
     ((null rest) (cons (reverse options) nil))
     ((memq (car rest) allowed)
      (unless (cdr rest)
        (keymap-popup--invalid context "option %S needs a value" (car rest)))
      (when (memq (car rest) seen)
        (keymap-popup--invalid context "duplicate option %S" (car rest)))
      (parse (cddr rest)
             (cons (cadr rest) (cons (car rest) options))
             (cons (car rest) seen)))
     ((and (keywordp (car rest))
           (not (memq (car rest) '(:group :row))))
      (keymap-popup--invalid context "unknown option %S" (car rest)))
     (t (cons (reverse options) rest)))))

(defun keymap-popup--validate-key-option (options property context runtime)
  "Validate key PROPERTY in OPTIONS for CONTEXT and RUNTIME input."
  (when-let* ((value (plist-get options property)))
    (cond
     ((stringp value)
      (unless (key-valid-p value)
        (keymap-popup--invalid context "%S is not a valid key" value)))
     ((or runtime
          (memq value '(nil t))
          (keywordp value)
          (not (or (symbolp value) (consp value))))
      (keymap-popup--invalid context "%S needs a key string" property)))))

(defun keymap-popup--validate-options (options context runtime)
  "Validate OPTIONS for CONTEXT and RUNTIME input."
  (keymap-popup--validate-key-option options :popup-key context runtime)
  (keymap-popup--validate-key-option options :exit-key context runtime)
  (when-let* ((description (plist-get options :description)))
    (keymap-popup--validate-description description context runtime))
  (when (and runtime
             (plist-member options :persistent)
             (not (memq (plist-get options :persistent) '(nil t))))
    (keymap-popup--invalid context ":persistent needs a boolean"))
  options)

(defun keymap-popup--extract-macro-opts (body allowed context)
  "Extract options accepted by ALLOWED from macro BODY for CONTEXT.
Return a plist containing :docstring, parsed options, and :bindings."
  (let* ((docstring (and (stringp (car body))
                         (or (null (cadr body))
                             (not (listp (cadr body))))
                         (car body)))
         (rest (if docstring (cdr body) body))
         (result (keymap-popup--parse-options rest allowed context))
         (options (append (list :docstring docstring)
                          (car result)
                          (list :bindings (cdr result)))))
    (keymap-popup--validate-options options context nil)))

;;;###autoload
(defmacro keymap-popup-define (name &rest body)
  "Define NAME as a keymap with embedded descriptions.
BODY is an optional docstring, optional :popup-key KEY (default
per `keymap-popup-default-popup-key'), optional :exit-key KEY
\(default per `keymap-popup-default-exit-key'), optional :parent
KEYMAP, optional :description STRING-OR-FUNCTION, optional
:persistent BOOL, followed by :group keywords and KEY (DESC ...)
pairs."
  (declare (indent 1))
  (let* ((options (keymap-popup--extract-macro-opts
                   body keymap-popup--define-options
                   (format "definition %S" name)))
         (docstring (plist-get options :docstring))
         (popup-key (plist-get options :popup-key))
         (exit-key (plist-get options :exit-key))
         (parent (plist-get options :parent))
         (description (plist-get options :description))
         (persistent (plist-get options :persistent))
         (persistent-p (plist-member options :persistent))
         (bindings (plist-get options :bindings))
         (rows (keymap-popup--parse-bindings bindings 'define))
         (all-entries (keymap-popup--flatten-with-groups rows))
         (switch-entries (cl-loop for entry in all-entries
                                  when (eq (plist-get entry :type) 'switch)
                                  collect entry))
         (keymap-entries (cl-loop for entry in all-entries
                                  when (eq (plist-get entry :type) 'keymap)
                                  collect entry))
         (keymap-pairs (keymap-popup--build-keymap-pairs name all-entries))
         (launcher (keymap-popup--launcher-name name)))
    `(progn
       ,@(keymap-popup--build-declaration-forms name parent all-entries)
       ,@(keymap-popup--build-switch-forms name switch-entries)
       ,@(keymap-popup--build-enter-forms name keymap-entries)
       ,(keymap-popup--build-launcher-form name launcher)
       (defvar-keymap ,name
         ,@(and docstring (list :doc docstring))
         ,@(and parent (list :parent parent))
         ,@keymap-pairs)
       ;; Bound outside `defvar-keymap' so the default key is read at
       ;; load time, not baked in when the macro is byte-compiled.
       (keymap-set ,name ,(or popup-key 'keymap-popup-default-popup-key)
                   #',launcher)
       ,(keymap-popup--build-attach-form
         name (keymap-popup--build-descriptions-form rows name)
         exit-key description persistent persistent-p))))

;;;###autoload
(defmacro keymap-popup-annotate (keymap &rest body)
  "Annotate existing KEYMAP with popup descriptions.
BODY is optional :popup-key KEY, optional :exit-key KEY, optional
:description STRING-OR-FUNCTION, optional :persistent BOOL,
followed by :group keywords and COMMAND-SYMBOL DESCRIPTION pairs.
COMMAND-SYMBOL is a function symbol already bound in the keymap.
DESCRIPTION is a string or (STRING &rest PROPS).

Unlike `keymap-popup-define', no defaults are applied for
:popup-key or :exit-key.  Only explicitly provided values take
effect.  When :exit-key is omitted, the popup falls back to
`keymap-popup-default-exit-key' at display time.

Keys are resolved dynamically via `where-is-internal' at display
time, so the popup always reflects the user's current bindings."
  (declare (indent 1))
  (let* ((options (keymap-popup--extract-macro-opts
                   body keymap-popup--annotate-options "annotation"))
         (popup-key (plist-get options :popup-key))
         (exit-key (plist-get options :exit-key))
         (description (plist-get options :description))
         (persistent (plist-get options :persistent))
         (persistent-p (plist-member options :persistent))
         (bindings (plist-get options :bindings))
         (rows (keymap-popup--parse-bindings bindings 'annotate))
         (launcher (and popup-key (symbolp keymap)
                        (keymap-popup--launcher-name keymap))))
    `(progn
       ,(keymap-popup--build-attach-form
         keymap (keymap-popup--build-descriptions-form rows)
         exit-key description persistent persistent-p)
       ,@(cond
          (launcher
           `(,(keymap-popup--build-launcher-form keymap launcher)
             (keymap-set ,keymap ,popup-key #',launcher)))
          (popup-key
           `((keymap-set ,keymap ,popup-key
                         (lambda () (interactive) (keymap-popup ,keymap)))))))))

;;; Public API

;;;###autoload
(defun keymap-popup-attach (keymap bindings &rest opts)
  "Attach popup descriptions for BINDINGS to KEYMAP at runtime.
BINDINGS is an evaluated list shaped like `keymap-popup-annotate'
BODY: KEY (DESCRIPTION COMMAND . PROPS) or COMMAND-SYMBOL
DESCRIPTION pairs, optionally grouped with :group.  Commands and
predicates are actual objects, not forms.  OPTS is a plist
accepting :exit-key, :description, and :persistent.  Returns
KEYMAP.

Unlike the macros, this attaches descriptions computed from data,
so it suits anonymous keymaps and menus built from runtime lists.
Reattaching replaces prior descriptions and options; omitted
options return to their defaults.  It does not bind keys or define
commands; keyed entries should already be bound in KEYMAP."
  (let ((parsed (keymap-popup--parse-options
                 opts keymap-popup--attach-options "attachment")))
    (when (cdr parsed)
      (keymap-popup--invalid "attachment" "expected option, got %S"
                             (car (cdr parsed))))
    (let ((options (keymap-popup--validate-options
                    (car parsed) "attachment" t)))
      (apply #'keymap-popup--attach-meta keymap
             (keymap-popup--parse-bindings bindings 'runtime)
             options))))

(defun keymap-popup--map-groups (rows fn)
  "Apply FN to each group in ROWS, returning the transformed rows.
FN receives a group plist and returns a new group plist."
  (mapcar (lambda (row) (mapcar fn row)) rows))

(defun keymap-popup--add-entry-to-rows (rows entry group-name)
  "Return ROWS with ENTRY appended to the group named GROUP-NAME.
Falls back to the first group if GROUP-NAME is not found.
If ENTRY's key already appears in ROWS (in any group), the prior
entry is removed first -- matching the replacement semantics of
`keymap-set'."
  (let* ((key (plist-get entry :key))
         (scrubbed (if key
                       (keymap-popup--remove-key-from-rows rows key)
                     rows))
         (target (or (cl-loop for row in scrubbed
                              thereis (cl-loop for g in row
                                               when (equal (plist-get g :name) group-name)
                                               return group-name))
                     (plist-get (caar scrubbed) :name))))
    (keymap-popup--map-groups
     scrubbed
     (lambda (group)
       (if (equal (plist-get group :name) target)
           (plist-put (copy-sequence group) :entries
                      (append (plist-get group :entries) (list entry)))
         group)))))

(defun keymap-popup--remove-key-from-rows (rows key)
  "Return ROWS with entries matching KEY filtered out."
  (keymap-popup--map-groups
   rows
   (lambda (group)
     (plist-put (copy-sequence group) :entries
                (cl-remove-if
                 (lambda (e) (equal (plist-get e :key) key))
                 (plist-get group :entries))))))

;;;###autoload
(defun keymap-popup-add-entry (keymap key description command &optional group)
  "Add KEY binding with DESCRIPTION and COMMAND to KEYMAP.
GROUP is the group name to add to (nil for the first group).
Updates both the keymap and the popup descriptions."
  (let ((descs (keymap-popup--meta keymap 'descriptions)))
    (or descs (user-error "No descriptions in keymap"))
    (let ((entry (keymap-popup--parse-entry
                  key (list description command) 'runtime)))
      (keymap-set keymap key command)
      (setf (keymap-popup--meta keymap 'descriptions)
            (keymap-popup--add-entry-to-rows descs entry group)))))

;;;###autoload
(defun keymap-popup-remove-entry (keymap key)
  "Remove KEY binding from KEYMAP.
Updates both the keymap and the popup descriptions."
  (keymap-set keymap key nil)
  (setf (keymap-popup--meta keymap 'descriptions)
        (keymap-popup--remove-key-from-rows
         (keymap-popup--meta keymap 'descriptions) key)))

;;; Renderer

(defconst keymap-popup--max-description-width 40
  "Maximum visible width of a resolved description, in columns.
Resolved descriptions longer than this are truncated with an
ellipsis.  Authors should keep descriptions short by convention;
this bound exists so dynamic descriptions can't blow out the
column layout.")

(defun keymap-popup--resolve-description (desc)
  "Resolve DESC to a display string.
If DESC is a function, call it; otherwise return as-is.  Collapse
runs of whitespace into single spaces and truncate to
`keymap-popup--max-description-width'."
  (let* ((raw (if (functionp desc) (funcall desc) desc))
         (collapsed (and raw (string-clean-whitespace raw))))
    (and collapsed
         (truncate-string-to-width
          collapsed keymap-popup--max-description-width nil nil t))))

(defun keymap-popup--render-entry (entry &optional prefix-mode key-width)
  "Render ENTRY into a formatted line, or nil if :if hides it.
When PREFIX-MODE is non-nil, entries with :c-u are highlighted and
their :c-u description is shown; other entries are dimmed.
KEY-WIDTH pads the key column for alignment."
  (and (keymap-popup--if-allows-p entry)
       (let* ((inapt (keymap-popup--inapt-active-p entry))
              (raw-desc (keymap-popup--resolve-description
			 (plist-get entry :description)))
              (type (plist-get entry :type))
              (desc (if (eq type 'keymap)
			(propertize raw-desc 'face 'keymap-popup-submenu)
                      raw-desc))
              (c-u-desc (plist-get entry :c-u))
              (raw-key (plist-get entry :key))
              (padded-key (if key-width
                              (string-pad raw-key key-width)
                            raw-key))
              (key-str (propertize padded-key 'face 'keymap-popup-key))
              (value-str (if (eq type 'switch)
                             (propertize
                              (if (symbol-value (plist-get entry :variable))
				  " [on]" " [off]")
                              'face 'keymap-popup-value)
			   ""))
              (c-u-str (and c-u-desc
                            (propertize (format " (%s)" c-u-desc)
                                        'face (if prefix-mode 'warning 'shadow))))
              (line (format "  %s  %s%s%s" key-str desc value-str
                            (or c-u-str ""))))
	 (cond
	  (inapt (propertize line 'face 'keymap-popup-inapt))
	  ((and prefix-mode (not c-u-desc))
           (propertize line 'face 'shadow))
	  (t line)))))

(defun keymap-popup--render-group-lines (group &optional prefix-mode)
  "Render GROUP into a list of lines (strings).
When PREFIX-MODE is non-nil, pass it to entry rendering.
Returns nil if the group is hidden by :if or has no visible entries.
When the group has :inapt-if that returns non-nil, all entries are
rendered with the inapt face."
  (and (keymap-popup--if-allows-p group)
       (let* ((group-inapt (keymap-popup--inapt-active-p group))
              (entries (plist-get group :entries))
              (key-width (cl-loop for entry in entries
				  maximize (length (plist-get entry :key))))
              (header (and-let* ((raw-name (plist-get group :name))
				 (name (keymap-popup--resolve-description raw-name)))
			(propertize name 'face (if group-inapt
                                                   'keymap-popup-inapt
						 'keymap-popup-group-header))))
              (lines (cl-loop for entry in entries
                              when (keymap-popup--render-entry
				    entry prefix-mode key-width)
                              collect it)))
	 (and lines
              (let ((result (if header (cons header lines) lines)))
                (if group-inapt
		    (mapcar (lambda (line) (propertize line 'face 'keymap-popup-inapt))
			    result)
                  result))))))

(defun keymap-popup--column-width (col)
  "Return the max visible width of lines in COL."
  (cl-loop for line in col
           maximize (string-width line)))

(defun keymap-popup--join-columns (columns separator col-widths)
  "Join COLUMNS side by side with SEPARATOR between them.
COL-WIDTHS is a list of minimum widths per column position.
Shorter columns are padded with blank lines."
  (let* ((max-height (cl-loop for col in columns maximize (length col)))
         (padded-cols (cl-mapcar
                       (lambda (col width)
                         (let ((padded (mapcar (lambda (line)
                                                 (string-pad line width))
                                               col))
                               (blanks (make-list (- max-height (length col))
                                                  (make-string width ?\s))))
                           (append padded blanks)))
                       columns col-widths)))
    (cl-loop for row from 0 below max-height
             collect (string-trim-right
                      (mapconcat (lambda (col) (nth row col))
                                 padded-cols
                                 separator)))))

(defun keymap-popup--rows-to-columns (rows &optional prefix-mode)
  "Render each row of ROWS into its list of column line-lists.
When PREFIX-MODE is non-nil, pass it to group rendering.
Returns a list of ((col-lines ...) ...) per row, filtering empty groups."
  (cl-loop for row in rows
           collect (cl-loop for group in row
                            when (keymap-popup--render-group-lines group prefix-mode)
                            collect it)))

(defun keymap-popup--global-col-widths (rendered-rows)
  "Compute max column width per position across all RENDERED-ROWS."
  (let ((max-cols (or (cl-loop for cols in rendered-rows
                               maximize (length cols))
                      0)))
    (cl-loop for i from 0 below max-cols
             collect (cl-loop for cols in rendered-rows
                              when (nth i cols)
                              maximize (keymap-popup--column-width (nth i cols))))))

(defun keymap-popup--render (rows &optional prefix-mode)
  "Render ROWS into a complete popup string.
ROWS is a list of rows, each row a list of groups.
When PREFIX-MODE is non-nil, highlight :c-u entries and dim others.
Column widths are aligned across all rows."
  (let* ((rendered-rows (keymap-popup--rows-to-columns rows prefix-mode))
         (col-widths (keymap-popup--global-col-widths rendered-rows))
         (sections (cl-loop for cols in rendered-rows
                            when cols
                            collect (string-join
                                     (keymap-popup--join-columns
                                      cols "   " col-widths)
                                     "\n"))))
    (concat (string-join sections "\n") "\n")))

;;; Popup state

(defvar-local keymap-popup--session nil
  "Popup session plist owned by the popup buffer.
The :active value is a navigation-state plist; :stack holds parent
states.  Session-wide source, prefix, persistence, backend, and hook
values live beside them.")

(defun keymap-popup--plist-with (plist &rest properties)
  "Return PLIST with PROPERTIES replaced without changing PLIST."
  (named-let replace ((result (copy-sequence plist)) (rest properties))
    (if (null rest)
        result
      (replace (plist-put result (car rest) (cadr rest)) (cddr rest)))))

(defun keymap-popup--session-state (buf)
  "Return BUF's popup session plist."
  (and (buffer-live-p buf)
       (buffer-local-value 'keymap-popup--session buf)))

(defun keymap-popup--session-get (buf property)
  "Return PROPERTY from BUF's popup session."
  (plist-get (keymap-popup--session-state buf) property))

(defun keymap-popup--active-get (buf property)
  "Return PROPERTY from BUF's active popup state."
  (plist-get (keymap-popup--session-get buf :active) property))

(defun keymap-popup--set-session (buf &rest properties)
  "Replace PROPERTIES in BUF's popup session."
  (with-current-buffer buf
    (setq-local keymap-popup--session
                (apply #'keymap-popup--plist-with
                       keymap-popup--session properties))))

(defun keymap-popup--set-active (buf &rest properties)
  "Replace PROPERTIES in BUF's active popup state."
  (let ((active (keymap-popup--session-get buf :active)))
    (keymap-popup--set-session
     buf :active (apply #'keymap-popup--plist-with active properties))))

(defun keymap-popup--make-state (keymap descriptions docstring exit-key)
  "Return state for KEYMAP, DESCRIPTIONS, DOCSTRING, and EXIT-KEY."
  (list :keymap keymap
        :descriptions descriptions
        :docstring docstring
        :resolved-docstring nil
        :exit-key exit-key
        :wrapper-map nil
        :exit-function nil))

;;; Popup display

(defun keymap-popup--dedupe-descriptions (descriptions)
  "Return DESCRIPTIONS with duplicate-key entries removed.
The first occurrence of each key wins; later duplicates are dropped.
Entries with nil :key (annotated entries before key resolution)
are preserved as-is.  Groups and rows that end up empty are removed."
  (let ((seen (make-hash-table :test 'equal)))
    (cl-labels ((keep-entry (e)
                  (let ((k (plist-get e :key)))
                    (cond ((null k) e)
                          ((gethash k seen) nil)
                          (t (puthash k t seen) e))))
                (keep-group (g)
                  (and-let* ((es (seq-keep #'keep-entry (plist-get g :entries))))
                    (plist-put (copy-sequence g) :entries es)))
                (keep-row (r)
                  (seq-keep #'keep-group r)))
	       (seq-keep #'keep-row descriptions))))

(defun keymap-popup--collect-descriptions (keymap)
  "Collect descriptions from KEYMAP and all its parent keymaps.
Walks the native parent chain via `keymap-parent'.  When a key is
bound in both child and parent, the child's entry wins and the
parent's is dropped, matching the dispatch behavior of inherited
keymaps."
  (keymap-popup--dedupe-descriptions
   (cl-loop for map = keymap then (keymap-parent map)
            while map
            when (keymap-popup--meta map 'descriptions)
            append it)))

(defun keymap-popup--find-entry-by-key (descriptions key-str)
  "Find the entry matching KEY-STR in DESCRIPTIONS.
DESCRIPTIONS is a list of rows, each row a list of groups.
Returns the entry plist, or nil."
  (cl-loop for row in descriptions
           thereis (cl-loop for group in row
                            thereis (cl-loop for entry in (plist-get group :entries)
                                             when (equal (plist-get entry :key) key-str)
                                             return entry))))

(defun keymap-popup--keep-popup-p (descriptions key-str)
  "Return non-nil if KEY-STR should keep the popup open in DESCRIPTIONS.
True for switches, stay-open suffixes, and :keymap entries with a
target.  Inapt-key handling is folded into the keep-pred via
`keymap-popup--inapt-key-p'."
  (and-let* ((entry (keymap-popup--find-entry-by-key descriptions key-str)))
    (or (eq (plist-get entry :type) 'switch)
        (plist-get entry :stay-open)
        (and (eq (plist-get entry :type) 'keymap)
             (plist-get entry :target)))))

(defun keymap-popup--inapt-key-p (buf key-str)
  "Return non-nil when KEY-STR is currently inapt in BUF's popup.
Checks both the entry's own :inapt-if and its containing group's;
group predicates are not merged into the descriptions tree.
Returns nil when BUF is dead (the popup already closed)."
  (and (buffer-live-p buf)
       (cl-loop
        for row in (keymap-popup--active-get buf :descriptions)
        thereis (cl-loop
                 for group in row
                 thereis (cl-loop
                          for entry in (plist-get group :entries)
                          when (equal (plist-get entry :key) key-str)
                          return (or (keymap-popup--inapt-active-p entry)
                                     (keymap-popup--inapt-active-p group)))))))

(defun keymap-popup--refresh-buffer (buf descriptions &optional prefix-mode)
  "Re-render popup BUF with DESCRIPTIONS, refit via backend.
PREFIX-MODE toggles prefix argument highlighting."
  (let ((content (keymap-popup--render descriptions prefix-mode)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert content)
        (goto-char (point-min))))
    (when-let* ((fit (plist-get (keymap-popup--session-get buf :backend) :fit)))
      (funcall fit buf))))

(defun keymap-popup--refresh (buf)
  "Re-render popup BUF from its buffer-local state.
Renders in the source buffer's context so `symbol-value' for
switch variables sees the user's buffer-local values.
Resolves the docstring for mode-line display."
  (when (buffer-live-p buf)
    (let* ((session (keymap-popup--session-state buf))
           (active (plist-get session :active))
           (source (plist-get session :source))
           (descriptions (plist-get active :descriptions))
           (docstring (plist-get active :docstring))
           (prefix (plist-get session :prefix-mode)))
      (with-current-buffer (if (buffer-live-p source) source buf)
        (let ((resolved (and docstring
                             (keymap-popup--resolve-description docstring))))
          (keymap-popup--set-active buf :resolved-docstring resolved))
        (keymap-popup--refresh-buffer buf descriptions prefix)))))

(defun keymap-popup--resolve-key (entry keymap)
  "Return ENTRY with :key reflecting KEYMAP's current bindings.
An entry keeps its stored :key while that key still dispatches to
the entry's :command.  When the command has been rebound,
`where-is-internal' supplies the current key; when it finds no
binding, the stored key is kept (this covers lambda commands and
entries hidden by an `:if' filter).  Entries with no stored :key
\(annotated) resolve via `where-is-internal' and are dropped when
unbound.  Resolution searches only KEYMAP and its parents, never
the global map."
  (let ((cmd (plist-get entry :command))
        (stored (plist-get entry :key)))
    (cond
     ((null cmd) entry)
     ((and stored (eq (keymap-lookup keymap stored) cmd)) entry)
     (t (let ((keys (where-is-internal cmd (list keymap) t)))
          (cond
           (keys (plist-put (copy-sequence entry) :key
                            (key-description keys)))
           (stored entry)))))))

(defun keymap-popup--resolve-descriptions (rows keymap)
  "Resolve entry keys in ROWS against KEYMAP's current bindings.
Drops annotated entries whose command has no binding, then removes
duplicates introduced when annotated entries resolve to the same key."
  (keymap-popup--dedupe-descriptions
   (keymap-popup--map-groups
    rows
    (lambda (group)
      (plist-put (copy-sequence group) :entries
                 (cl-loop for entry in (plist-get group :entries)
                          when (keymap-popup--resolve-key entry keymap)
                          collect it))))))

;;; Display backends

(defun keymap-popup--show-side-window (buf)
  "Display BUF in a side window."
  (display-buffer buf (append keymap-popup-display-action
                              '((window-height . fit-window-to-buffer)))))

(defun keymap-popup--fit-side-window (buf)
  "Refit the side window displaying BUF."
  (when-let* ((win (get-buffer-window buf))
              (_ (window-live-p win)))
    (fit-window-to-buffer win)))

(defun keymap-popup--hide-side-window (buf)
  "Delete the side window displaying BUF."
  (when-let* ((win (get-buffer-window buf)))
    (delete-window win)))

(defun keymap-popup--show-child-frame (buf)
  "Display BUF in a child frame centered on the parent.
Frame parameters are taken from `keymap-popup-child-frame-parameters'."
  (let* ((parent (selected-frame))
         ;; Skip window-system init hooks (DnD, terminal face setup,
         ;; user hooks); irrelevant for an ephemeral popup frame.
         (after-make-frame-functions nil)
         (frame (make-frame
                 `((parent-frame . ,parent)
                   (minibuffer . ,(minibuffer-window))
                   (visibility . nil)
                   ,@keymap-popup-child-frame-parameters)))
         (win (frame-root-window frame)))
    (set-window-buffer win buf)
    (set-window-dedicated-p win t)
    (fit-frame-to-buffer frame)
    (let ((x (/ (- (frame-pixel-width parent) (frame-pixel-width frame)) 2))
          (y (/ (- (frame-pixel-height parent) (frame-pixel-height frame)) 2)))
      (set-frame-position frame (max 0 x) (max 0 y)))
    (make-frame-visible frame)
    (redirect-frame-focus frame parent)))

(defun keymap-popup--fit-child-frame (buf)
  "Refit the child frame displaying BUF."
  (when-let* ((win (get-buffer-window buf t))
              (frame (window-frame win))
              (_ (frame-parent frame)))
    (fit-frame-to-buffer frame)))

(defun keymap-popup--hide-child-frame (buf)
  "Delete the child frame displaying BUF."
  (when-let* ((win (get-buffer-window buf t))
              (frame (window-frame win))
              (_ (frame-parent frame)))
    (delete-frame frame)))

(defun keymap-popup-backend-side-window ()
  "Return a side-window display backend."
  (list :show #'keymap-popup--show-side-window
        :fit #'keymap-popup--fit-side-window
        :hide #'keymap-popup--hide-side-window))

(defun keymap-popup-backend-child-frame ()
  "Return a child-frame display backend."
  (list :show #'keymap-popup--show-child-frame
        :fit #'keymap-popup--fit-child-frame
        :hide #'keymap-popup--hide-child-frame))

(defun keymap-popup--prepare-buffer ()
  "Create and configure the popup buffer."
  (let ((buf (get-buffer-create keymap-popup--buffer-name)))
    (with-current-buffer buf
      (pcase-dolist (`(,var . ,val) keymap-popup-buffer-parameters)
        (set (make-local-variable var) val)))
    buf))

;; `internal-{push,pop}-keymap' are the only public path for manipulating
;; `overriding-terminal-local-map'; `set-transient-map' itself uses them.

(defun keymap-popup--suspend ()
  "Suspend the popup's transient map for minibuffer input."
  (when-let* ((buf (get-buffer keymap-popup--buffer-name))
              (map (keymap-popup--active-get buf :wrapper-map)))
    (internal-pop-keymap map 'overriding-terminal-local-map)))

(defun keymap-popup--resume ()
  "Resume the popup's transient map after minibuffer input."
  (when-let* ((buf (get-buffer keymap-popup--buffer-name))
              (map (keymap-popup--active-get buf :wrapper-map)))
    (internal-push-keymap map 'overriding-terminal-local-map)))

(defun keymap-popup--session-exit-functions (session include-active)
  "Return exit functions owned by SESSION.
INCLUDE-ACTIVE includes the active transient map as well as parents."
  (let* ((active (plist-get session :active))
         (states (if include-active
                     (cons active (plist-get session :stack))
                   (plist-get session :stack))))
    (seq-keep (lambda (state)
                (and-let* ((exit (plist-get state :exit-function))
                           ((functionp exit)))
                  exit))
              states)))

(defun keymap-popup--close-session (buf include-active)
  "Deactivate BUF's session maps.
INCLUDE-ACTIVE also deactivates the active transient map."
  (let* ((session (keymap-popup--session-state buf))
         (active (plist-get session :active))
         (exits (keymap-popup--session-exit-functions
                 session include-active)))
    (keymap-popup--set-session
     buf :active (keymap-popup--plist-with active :exit-function nil)
     :stack nil :closing t)
    (mapc #'funcall exits)))

(defun keymap-popup--remove-session-hooks (buf)
  "Remove global hooks owned by BUF's popup session."
  (remove-hook 'minibuffer-setup-hook #'keymap-popup--suspend)
  (remove-hook 'minibuffer-exit-hook #'keymap-popup--resume)
  (when-let* ((hook (keymap-popup--session-get buf :persistent-hook)))
    (remove-hook 'post-command-hook hook)))

(defun keymap-popup--hide-session (buf)
  "Hide the display owned by BUF's popup session."
  (when-let* ((backend (keymap-popup--session-get buf :backend))
              (hide (plist-get backend :hide)))
    (funcall hide buf)))

(defun keymap-popup--kill-buffer-cleanup ()
  "Release the popup session when its buffer is killed directly."
  (let ((buf (current-buffer)))
    (unless (keymap-popup--session-get buf :closing)
      (keymap-popup--close-session buf t)
      (keymap-popup--remove-session-hooks buf)
      (keymap-popup--hide-session buf))))

(defun keymap-popup--teardown (buf)
  "Remove the popup display for BUF and kill it."
  (when (buffer-live-p buf)
    (keymap-popup--close-session buf nil)
    (keymap-popup--remove-session-hooks buf)
    (keymap-popup--hide-session buf)
    (kill-buffer buf)))

(defun keymap-popup--make-keep-pred (buf)
  "Return a keep-pred for `set-transient-map'.
Reads state from BUF.  Consumes the reentering flag on read."
  (lambda ()
    (and (buffer-live-p buf)
         (let* ((session (keymap-popup--session-state buf))
                (active (plist-get session :active))
                (key-str (key-description (this-command-keys-vector))))
           (cond
            ((active-minibuffer-window) t)
            ((plist-get session :reentering)
             (keymap-popup--set-session buf :reentering nil)
             t)
            ((memq this-command
                   '(universal-argument universal-argument-more
                     digit-argument negative-argument
                     keymap-popup--prefix-argument
                     describe-key describe-key-briefly)))
            ((equal key-str (plist-get active :exit-key)) nil)
            ((eq this-command 'keyboard-quit) nil)
            ((keymap-popup--inapt-key-p buf key-str) t)
            ((plist-get session :persistent))
            (t (keymap-popup--keep-popup-p
                (plist-get active :descriptions) key-str)))))))

(defun keymap-popup--make-on-exit (buf)
  "Return an on-exit callback for `set-transient-map' closing BUF.
Pops the sub-menu stack if exit-key or \\`C-g' caused the exit,
otherwise tears down completely."
  (lambda ()
    (when (buffer-live-p buf)
      (let* ((session (keymap-popup--session-state buf))
             (active (plist-get session :active))
             (stack (plist-get session :stack))
             (key-str (key-description (this-command-keys-vector))))
        (unless (plist-get session :closing)
          (if (and stack
                   (member key-str (list (plist-get active :exit-key) "C-g")))
              (progn
                (keymap-popup--set-session
                 buf :active (car stack) :stack (cdr stack)
                 :reentering t :prefix-mode nil)
                (keymap-popup--refresh buf))
            (keymap-popup--teardown buf)))))))

(defun keymap-popup--state-for-keymap (keymap)
  "Derive navigation state from KEYMAP."
  (keymap-popup--make-state
   keymap
   (keymap-popup--resolve-descriptions
    (keymap-popup--collect-descriptions keymap) keymap)
   (keymap-popup--meta keymap 'description)
   (or (keymap-popup--meta keymap 'exit-key)
       keymap-popup-default-exit-key)))

(defun keymap-popup--push-submenu (buf child-keymap)
  "Push current popup state in BUF and activate CHILD-KEYMAP's transient map."
  (let* ((session (keymap-popup--session-state buf))
         (parent (plist-get session :active))
         (child (keymap-popup--state-for-keymap child-keymap)))
    (keymap-popup--set-session
     buf :active child :stack (cons parent (plist-get session :stack))
     :prefix-mode nil)
    (keymap-popup--refresh buf)
    (keymap-popup--activate-transient-map buf)))

(defun keymap-popup--prefix-argument ()
  "Toggle prefix argument mode in the active popup.
When toggling on, activates `universal-argument-map' so that
subsequent digit and `negative-argument' keys refine the prefix."
  (interactive)
  (when-let* ((buf (get-buffer keymap-popup--buffer-name)))
    (let ((enabled (not (keymap-popup--session-get buf :prefix-mode))))
      (keymap-popup--set-session buf :prefix-mode enabled)
      (setq prefix-arg (and enabled '(4)))
      (keymap-popup--refresh buf)
      (when enabled
        (universal-argument--mode)))))

(defun keymap-popup--core-overrides (exit-key)
  "Return alist of core overrides for EXIT-KEY and prefix toggle.
The exit key is bound to `ignore'; the keep-pred exits on the key
itself, so the binding only needs to exist and do nothing."
  (list (cons exit-key #'ignore)
        (cons "C-u" #'keymap-popup--prefix-argument)))

(defun keymap-popup--refuse-inapt (buf)
  "Refuse an inapt key press for the popup in BUF.
Preserves BUF's prefix-mode so \\[universal-argument] is not
consumed; the popup only ever stores (4) (see
`keymap-popup--prefix-argument'), so re-setting that value is
preservation, not approximation."
  (message "Command unavailable")
  (when (and (buffer-live-p buf)
             (keymap-popup--session-get buf :prefix-mode))
    (setq prefix-arg '(4))))

(defun keymap-popup--call-real-binding (keymap key-str)
  "Call KEY-STR's live binding in KEYMAP if it is a command.
Sets `this-command' to the binding so `repeat' and `last-command'
see the real command rather than the wrapper closure.  Return the
binding, which may be nil."
  (let ((cmd (keymap-lookup keymap key-str)))
    (when (commandp cmd)
      (setq this-command cmd)
      (call-interactively cmd))
    cmd))

(defun keymap-popup--consume-prefix (buf)
  "Consume prefix mode owned by popup BUF."
  (when (keymap-popup--session-get buf :prefix-mode)
    (keymap-popup--set-session buf :prefix-mode nil)
    (setq prefix-arg nil)))

(defun keymap-popup--dispatch-entry (keymap entry buf)
  "Dispatch KEYMAP's popup ENTRY for BUF."
  (let ((key (plist-get entry :key)))
    (if (keymap-popup--inapt-key-p buf key)
        (keymap-popup--refuse-inapt buf)
      (pcase-exhaustive (plist-get entry :type)
        ('keymap
         (keymap-popup--push-submenu buf (plist-get entry :target)))
        ('switch
         (when (keymap-popup--call-real-binding keymap key)
           (keymap-popup--consume-prefix buf))
         (keymap-popup--refresh buf))
        ('suffix
         (keymap-popup--call-real-binding keymap key)
         (when (plist-get entry :stay-open)
           (keymap-popup--refresh buf)))))))

(defun keymap-popup--entry-needs-override-p (entry group)
  "Return non-nil when ENTRY in GROUP needs popup dispatch."
  (pcase-exhaustive (plist-get entry :type)
    ((or 'switch 'keymap) t)
    ('suffix (or (plist-get entry :stay-open)
                 (plist-get entry :inapt-if)
                 (plist-get group :inapt-if)))))

(defun keymap-popup--entry-override (keymap entry group buf)
  "Return KEYMAP override for ENTRY in GROUP and BUF, or nil."
  (and-let* ((key (plist-get entry :key))
             ((keymap-popup--entry-needs-override-p entry group)))
    (cons key
          (lambda ()
            (interactive)
            (keymap-popup--dispatch-entry keymap entry buf)))))

(defun keymap-popup--description-overrides (keymap descriptions buf)
  "Return popup entry overrides for KEYMAP, DESCRIPTIONS, and BUF."
  (mapcan
   (lambda (row)
     (mapcan
      (lambda (group)
        (seq-keep (lambda (entry)
                    (keymap-popup--entry-override keymap entry group buf))
                  (plist-get group :entries)))
      row))
   descriptions))

(defun keymap-popup--build-wrapper-map (keymap descriptions buf exit-key)
  "Build wrapper keymap over KEYMAP with DESCRIPTIONS for BUF.
EXIT-KEY and entry handlers layer over KEYMAP.  `:if' enforcement
lives on KEYMAP itself via `menu-item' :filter; `:inapt-if' is
enforced here at keypress time."
  (let* ((map (make-sparse-keymap))
         (overrides (append
                     (keymap-popup--description-overrides
                      keymap descriptions buf)
                     (keymap-popup--core-overrides exit-key))))
    (set-keymap-parent map keymap)
    (pcase-dolist (`(,key . ,cmd) overrides)
      (keymap-set map key cmd))
    map))

(defun keymap-popup-dismiss ()
  "Dismiss the active popup, if any.
Deactivates every transient map in a nested popup session and
removes the popup display."
  (interactive)
  (when-let* ((buf (get-buffer keymap-popup--buffer-name)))
    (keymap-popup--close-session buf t)
    (when (buffer-live-p buf)
      (keymap-popup--remove-session-hooks buf)
      (keymap-popup--hide-session buf)
      (kill-buffer buf))))

(defun keymap-popup--make-session (keymap)
  "Derive a popup session for KEYMAP and the current buffer."
  (list :source (current-buffer)
        :active (keymap-popup--state-for-keymap keymap)
        :stack nil
        :prefix-mode nil
        :reentering nil
        :closing nil
        :persistent (pcase (keymap-popup--meta keymap 'persistent)
                      ('yes t)
                      ('no nil)
                      (_ keymap-popup-persistent))
        :backend (funcall keymap-popup-backend)
        :persistent-hook nil))

(defun keymap-popup--init-session (buf session)
  "Install SESSION in BUF and arrange cleanup on buffer death."
  (with-current-buffer buf
    (setq-local keymap-popup--session session)
    (add-hook 'kill-buffer-hook #'keymap-popup--kill-buffer-cleanup nil t)))

(defun keymap-popup--activate-transient-map (buf)
  "Build and activate the transient map for BUF's active state."
  (let* ((active (keymap-popup--session-get buf :active))
         (wrapper (keymap-popup--build-wrapper-map
                   (plist-get active :keymap)
                   (plist-get active :descriptions)
                   buf
                   (plist-get active :exit-key))))
    (keymap-popup--set-active buf :wrapper-map wrapper)
    (let ((exit-function
           (set-transient-map wrapper
                              (keymap-popup--make-keep-pred buf)
                              (keymap-popup--make-on-exit buf))))
      (when (buffer-live-p buf)
        (keymap-popup--set-active buf :exit-function exit-function)))))

(defun keymap-popup--install-persistent-hook (buf)
  "Install BUF's self-removing persistent refresh hook."
  (let ((hook-fn (make-symbol "keymap-popup--persistent-refresh")))
    (fset hook-fn
          (lambda ()
            (if (buffer-live-p buf)
                (keymap-popup--refresh buf)
              (remove-hook 'post-command-hook hook-fn))))
    (keymap-popup--set-session buf :persistent-hook hook-fn)
    (add-hook 'post-command-hook hook-fn)))

;;;###autoload
(defun keymap-popup (keymap)
  "Show popup help for described KEYMAP.
Activates KEYMAP as a transient map.  Switch keys execute and re-render
without closing.  Inapt keys are refused with \"Command unavailable\"
and keep the popup open; outside a popup, `:inapt-if' does not block
dispatch.  Sub-menu keys push a navigation stack.
\\[universal-argument] toggles prefix mode."
  (or (keymap-popup--meta keymap 'descriptions)
      (user-error "No descriptions in keymap"))
  (when (get-buffer keymap-popup--buffer-name)
    (keymap-popup-dismiss))
  (let ((session (keymap-popup--make-session keymap))
        (buf (keymap-popup--prepare-buffer)))
    (keymap-popup--init-session buf session)
    (keymap-popup--refresh buf)
    (funcall (plist-get (plist-get session :backend) :show) buf)
    (keymap-popup--activate-transient-map buf)
    (add-hook 'minibuffer-setup-hook #'keymap-popup--suspend)
    (add-hook 'minibuffer-exit-hook #'keymap-popup--resume)
    (when (plist-get session :persistent)
      (keymap-popup--install-persistent-hook buf))))

(provide 'keymap-popup)
;;; keymap-popup.el ends here

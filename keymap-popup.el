;;; keymap-popup.el --- Described keymaps with popup help  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Free Software Foundation, Inc.

;; Author: Thanos Apollo <public@thanosapollo.org>
;; Version: 0.3.1
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
;; The popup is a pure renderer: it reads state (buffer-locals, dynamic
;; descriptions) but owns none.  Commands mutate state in the user's
;; buffer; the popup just re-reads it on the next refresh.

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
        (:eval (and keymap-popup--active-exit-key
                    (propertize (format " %s " keymap-popup--active-exit-key)
                                'face 'keymap-popup-key)))
        " "
        (:eval (or keymap-popup--resolved-docstring ""))))
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
Applied automatically by `keymap-popup-define' when :popup-key is
not specified.  Not applied by `keymap-popup-annotate' (must be
given explicitly)."
  :type 'key
  :group 'keymap-popup)

(defcustom keymap-popup-default-exit-key "q"
  "Default key to dismiss the popup.
Applied automatically by `keymap-popup-define' when :exit-key is
not specified.  For `keymap-popup-annotate', used as runtime
fallback when :exit-key is omitted."
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
;; (it means "default binding").  Use symbols like 'yes instead.
(gv-define-setter keymap-popup--meta (val keymap prop)
  `(define-key ,keymap (vector 'keymap-popup ,prop) ,val))

(defun keymap-popup--attach-meta (keymap rows &rest opts)
  "Attach popup descriptions ROWS and metadata OPTS to KEYMAP.
OPTS is a plist accepting :exit-key, :description, and
:persistent; other keys are ignored.  Only non-nil values are
stored.  :persistent is stored as the symbol `yes' because
metadata lives in `define-key' bindings, where t means \"default
binding\".  Returns KEYMAP."
  (setf (keymap-popup--meta keymap 'descriptions) rows)
  (when-let* ((exit-key (plist-get opts :exit-key)))
    (setf (keymap-popup--meta keymap 'exit-key) exit-key))
  (when-let* ((description (plist-get opts :description)))
    (setf (keymap-popup--meta keymap 'description) description))
  (when (plist-get opts :persistent)
    (setf (keymap-popup--meta keymap 'persistent) 'yes))
  keymap)

;;; Parsers

(defun keymap-popup--extract-props (plist)
  "Extract known properties from PLIST.
Recognized keys: :if, :inapt-if, :stay-open, :c-u."
  (cl-loop for (k v) on plist by #'cddr
           when (memq k '(:if :inapt-if :stay-open :c-u))
           append (list k v)))

(defun keymap-popup--parse-entry (key spec)
  "Parse binding SPEC for KEY into a plist.
KEY is a key string for normal entries, or a command symbol for
annotated entries.  SPEC is (DESCRIPTION COMMAND-OR-TYPE &rest PROPS)
for key-based entries, or (DESCRIPTION &rest PROPS) for annotated ones."
  (if (symbolp key)
      ;; Annotated entry: key is a command symbol, spec is (DESC . PROPS),
      ;; any atom, or a bare lambda/function form.
      (let* ((spec (cond ((not (consp spec)) (list spec))
                         ((memq (car-safe spec) '(lambda function)) (list spec))
                         (t spec)))
             (description (car spec))
             (props (cdr spec)))
        `(:key nil :description ,description :type suffix
               :command ,key
               ,@(keymap-popup--extract-props props)))
    ;; Normal entry: key is a string
    (let* ((description (car spec))
           (second (cadr spec))
           (rest (cddr spec)))
      (pcase second
        (:switch
         `(:key ,key :description ,description :type switch
                :variable ,(car rest)
                ,@(keymap-popup--extract-props (cdr rest))))
        (:keymap
         `(:key ,key :description ,description :type keymap
                :target ,(car rest)
                ,@(keymap-popup--extract-props (cdr rest))))
        (_
         `(:key ,key :description ,description :type suffix
                :command ,second
                ,@(keymap-popup--extract-props rest)))))))

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
        (split (cddr rest) (cadr rest) nil flushed rows))
       (t
        (split (cddr rest) name
               (cons (cons (car rest) (cadr rest)) entries)
               groups rows))))))

(defun keymap-popup--parse-group-name (raw)
  "Parse RAW group name into (NAME . PROPS).
RAW is a string, a lambda, or a list (NAME :if PRED :inapt-if PRED).
A list whose car is not `lambda' is treated as a name with properties."
  (if (and (consp raw) (not (eq (car raw) 'lambda)))
      (cons (car raw) (keymap-popup--extract-props (cdr raw)))
    (cons raw nil)))

(defun keymap-popup--parse-chunk (chunk)
  "Parse CHUNK of (NAME . ((KEY . SPEC) ...)) into a group plist."
  (let* ((name-props (keymap-popup--parse-group-name (car chunk)))
         (name (car name-props))
         (group-props (cdr name-props))
         (entries (cl-loop for (k . v) in (cdr chunk)
                           collect (keymap-popup--parse-entry k v))))
    `(:name ,name :entries ,entries ,@group-props)))

(defun keymap-popup--parse-bindings (bindings)
  "Parse BINDINGS into a list of rows.
Each row is a list of group plists with :name and :entries."
  (mapcar (lambda (row) (mapcar #'keymap-popup--parse-chunk row))
          (keymap-popup--split-groups bindings)))

(defun keymap-popup--if-allows-p (plist)
  "Return non-nil when PLIST has no :if, or its :if predicate returns non-nil."
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
Always a symbol; the corresponding `defun' is emitted by
`keymap-popup-define'."
  (pcase-exhaustive (plist-get entry :type)
    ('suffix (plist-get entry :command))
    ('switch (keymap-popup--toggle-name map-name (plist-get entry :variable)))
    ('keymap (keymap-popup--enter-name map-name (plist-get entry :target)))))

;;; Macro helpers

(defun keymap-popup--make-if-filter (pred)
  "Return a `menu-item' :filter exposing the binding only when PRED is non-nil."
  (lambda (b) (and (funcall pred) b)))

(defun keymap-popup--make-inapt-filter (pred)
  "Return a `menu-item' :filter that swaps the binding for the inapt stub.
PRED non-nil reroutes dispatch to `keymap-popup--inapt-stub'."
  (lambda (b) (if (funcall pred) #'keymap-popup--inapt-stub b)))

(defun keymap-popup--make-combined-filter (if-pred inapt-pred)
  "Return a `menu-item' :filter that AND-combines IF-PRED and INAPT-PRED.
IF-PRED nil unbinds the key; INAPT-PRED non-nil reroutes to the inapt stub."
  (lambda (b)
    (cond ((not (funcall if-pred)) nil)
          ((funcall inapt-pred) #'keymap-popup--inapt-stub)
          (t b))))

(defun keymap-popup--wrap-binding-form (cmd-form if-pred inapt-pred)
  "Wrap CMD-FORM with a `menu-item' :filter when IF-PRED or INAPT-PRED is set.
IF-PRED and INAPT-PRED are forms that evaluate to zero-arg predicates.
A nil :if filter result makes the key act as unbound; an active :inapt-if
reroutes dispatch to `keymap-popup--inapt-stub'.  Filter closures are built by
helpers in this file so they remain lexical regardless of the caller."
  (cond
   ((and if-pred inapt-pred)
    `(list 'menu-item "" ,cmd-form :filter
           (keymap-popup--make-combined-filter ,if-pred ,inapt-pred)))
   (if-pred
    `(list 'menu-item "" ,cmd-form :filter
           (keymap-popup--make-if-filter ,if-pred)))
   (inapt-pred
    `(list 'menu-item "" ,cmd-form :filter
           (keymap-popup--make-inapt-filter ,inapt-pred)))
   (t cmd-form)))

(defun keymap-popup--build-keymap-pairs (map-name entries)
  "Build flat key/command list for `defvar-keymap' from ENTRIES.
MAP-NAME is used to derive generated command names.  Entries with
:if or :inapt-if expand to a `menu-item' form with :filter."
  (cl-loop for entry in entries
           for cmd = (keymap-popup--entry-command map-name entry)
           for cmd-form = (if (symbolp cmd) `#',cmd cmd)
           append (list (plist-get entry :key)
                        (keymap-popup--wrap-binding-form
                         cmd-form
                         (plist-get entry :if)
                         (plist-get entry :inapt-if)))))

(defun keymap-popup--build-entry-form (entry)
  "Build a `list' form for a single ENTRY."
  (let* ((type (plist-get entry :type))
         (type-props (pcase-exhaustive type
                       ('suffix (let ((cmd (plist-get entry :command)))
                                  `(:command ,(if (symbolp cmd) `#',cmd cmd)
                                             ,@(and (plist-get entry :stay-open)
                                                    '(:stay-open t)))))
                       ('keymap `(:target ,(plist-get entry :target)))
                       ('switch `(:variable ',(plist-get entry :variable)))))
         (if-pred (plist-get entry :if))
         (inapt-if (plist-get entry :inapt-if)))
    `(list :key ,(plist-get entry :key)
           :description ,(plist-get entry :description)
           :type ',type
           ,@type-props
           ,@(and if-pred (list :if if-pred))
           ,@(and inapt-if (list :inapt-if inapt-if))
           ,@(and-let* ((c-u (plist-get entry :c-u)))
               (list :c-u c-u)))))

(defun keymap-popup--build-group-form (group)
  "Build a `list' form for one GROUP plist."
  (let ((if-pred (plist-get group :if))
        (inapt-if (plist-get group :inapt-if)))
    `(list :name ,(plist-get group :name)
           :entries (list ,@(mapcar #'keymap-popup--build-entry-form
                                    (plist-get group :entries)))
           ,@(and if-pred (list :if if-pred))
           ,@(and inapt-if (list :inapt-if inapt-if)))))

(defun keymap-popup--build-descriptions-form (rows)
  "Build a `list' form that constructs descriptions at load time.
ROWS is a list of rows, each row a list of groups.  Uses `list'
calls so lambdas in :if/:inapt-if/:description get compiled."
  `(list ,@(mapcar (lambda (row)
                     `(list ,@(mapcar #'keymap-popup--build-group-form row)))
                   rows)))

(defun keymap-popup--build-switch-forms (map-name entries)
  "Build `defvar-local' + toggle `defun' forms for switch ENTRIES in MAP-NAME."
  (mapcan (lambda (e)
            (let* ((var (plist-get e :variable))
                   (toggle (keymap-popup--toggle-name map-name var)))
              `((defvar-local ,var nil
                  ,(format "Buffer-local switch state toggled by `%s'."
                           toggle))
                (defun ,toggle ()
                  ,(format "Toggle `%s' in the current buffer." var)
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
                                                 description persistent)
  "Build the `keymap-popup--attach-meta' call form for MAP-NAME.
ROWS-FORM constructs the descriptions at load time.  Each of
EXIT-KEY, DESCRIPTION, PERSISTENT is passed only when non-nil."
  `(keymap-popup--attach-meta ,map-name ,rows-form
                              ,@(and exit-key `(:exit-key ,exit-key))
                              ,@(and description `(:description ,description))
                              ,@(and persistent '(:persistent t))))

;;; Macro

(defun keymap-popup--consume-keywords (rest keywords)
  "Consume KEYWORDS from REST in any order.
Returns (VALUES . REMAINING) where VALUES is a list of extracted
values (nil for absent keywords), ordered as KEYWORDS."
  (named-let collect ((rest rest) (alist nil))
    (if (and rest (memq (car rest) keywords))
        (collect (cddr rest)
                 (cons (cons (car rest) (cadr rest)) alist))
      (cons (mapcar (lambda (kw) (alist-get kw alist)) keywords)
            rest))))

(defun keymap-popup--extract-macro-opts (body)
  "Extract macro options from BODY.
Returns (DOCSTRING POPUP-KEY EXIT-KEY PARENT DESCRIPTION PERSISTENT BINDINGS).
Unspecified keywords yield nil."
  (let* ((docstring (and (stringp (car body))
                         (or (null (cadr body))
                             (not (listp (cadr body))))
                         (car body)))
         (rest (if docstring (cdr body) body))
         (result (keymap-popup--consume-keywords
                  rest '(:popup-key :exit-key :parent :description :persistent))))
    (append (list docstring) (car result) (list (cdr result)))))

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
  (pcase-let* ((`(,docstring ,popup-key ,exit-key ,parent ,description
                             ,persistent ,bindings)
                (keymap-popup--extract-macro-opts body))
               (popup-key (or popup-key keymap-popup-default-popup-key))
               (exit-key (or exit-key keymap-popup-default-exit-key))
               (rows (keymap-popup--parse-bindings bindings))
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
         ,@keymap-pairs
         ,popup-key #',launcher)
       ,(keymap-popup--build-attach-form
         name (keymap-popup--build-descriptions-form rows)
         exit-key description persistent))))

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
  (pcase-let* ((`(,_docstring ,popup-key ,exit-key ,_parent ,description
                              ,persistent ,bindings)
                (keymap-popup--extract-macro-opts body))
               (rows (keymap-popup--parse-bindings bindings))
               (launcher (and popup-key (symbolp keymap)
                              (keymap-popup--launcher-name keymap))))
    `(progn
       ,(keymap-popup--build-attach-form
         keymap (keymap-popup--build-descriptions-form rows)
         exit-key description persistent)
       ,@(cond
          (launcher
           `(,(keymap-popup--build-launcher-form keymap launcher)
             (keymap-set ,keymap ,popup-key #',launcher)))
          (popup-key
           `((keymap-set ,keymap ,popup-key
                         (lambda () (interactive) (keymap-popup ,keymap)))))))))

;;; Public API

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
           (list :name (plist-get group :name)
                 :entries (append (plist-get group :entries) (list entry)))
         group)))))

(defun keymap-popup--remove-key-from-rows (rows key)
  "Return ROWS with entries matching KEY filtered out."
  (keymap-popup--map-groups
   rows
   (lambda (group)
     (list :name (plist-get group :name)
           :entries (cl-remove-if
                     (lambda (e) (equal (plist-get e :key) key))
                     (plist-get group :entries))))))

;;;###autoload
(defun keymap-popup-add-entry (keymap key description command &optional group)
  "Add KEY binding with DESCRIPTION and COMMAND to KEYMAP.
GROUP is the group name to add to (nil for the first group).
Updates both the keymap and the popup descriptions."
  (let ((descs (keymap-popup--meta keymap 'descriptions)))
    (or descs (user-error "No descriptions in keymap"))
    (keymap-set keymap key command)
    (let ((entry (list :key key :description description
                       :type 'suffix :command command)))
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
  (let ((max-cols (cl-loop for cols in rendered-rows
                           maximize (length cols))))
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

(defvar-local keymap-popup--source-buffer nil
  "The buffer from which the popup was invoked.
Switch variables are buffer-local there, so rendering must read
`symbol-value' in that buffer's context.")
(defvar-local keymap-popup--active-keymap nil
  "The currently displayed keymap in the popup.")
(defvar-local keymap-popup--active-descriptions nil
  "Descriptions for the currently active keymap.")
(defvar-local keymap-popup--active-docstring nil
  "Docstring for the currently active keymap.")
(defvar-local keymap-popup--stack nil
  "Stack of parent state plists for sub-menu navigation.")
(defvar-local keymap-popup--prefix-mode nil
  "Non-nil when \\`C-u' prefix mode is active.")
(defvar-local keymap-popup--reentering nil
  "Non-nil when a sub-menu just popped, preventing cascading exit.")
(defvar-local keymap-popup--active-exit-key nil
  "The exit key for the currently active popup.")
(defvar-local keymap-popup--resolved-docstring nil
  "Resolved docstring string for mode-line display.")
(defvar-local keymap-popup--persistent nil
  "Non-nil when this popup instance is in persistent mode.")
(defvar-local keymap-popup--wrapper-map nil
  "The active wrapper keymap on `overriding-terminal-local-map'.")
(defvar-local keymap-popup--display-backend nil
  "The active display backend plist (:show :fit :hide).")

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
`this-command'."
  (and-let* ((entry (keymap-popup--find-entry-by-key descriptions key-str)))
    (or (eq (plist-get entry :type) 'switch)
        (plist-get entry :stay-open)
        (and (eq (plist-get entry :type) 'keymap)
             (plist-get entry :target)))))

(defun keymap-popup--refresh-buffer (buf descriptions &optional prefix-mode)
  "Re-render popup BUF with DESCRIPTIONS, refit via backend.
PREFIX-MODE toggles prefix argument highlighting."
  (let ((content (keymap-popup--render descriptions prefix-mode)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert content)
        (goto-char (point-min))))
    (when-let* ((fit (plist-get (buffer-local-value 'keymap-popup--display-backend buf)
                                :fit)))
      (funcall fit buf))))

(defun keymap-popup--refresh (buf)
  "Re-render popup BUF from its buffer-local state.
Renders in the source buffer's context so `symbol-value' for
switch variables sees the user's buffer-local values.
Resolves the docstring for mode-line display."
  (when (buffer-live-p buf)
    (let ((source (buffer-local-value 'keymap-popup--source-buffer buf))
          (descs (buffer-local-value 'keymap-popup--active-descriptions buf))
          (doc (buffer-local-value 'keymap-popup--active-docstring buf))
          (prefix (buffer-local-value 'keymap-popup--prefix-mode buf)))
      (with-current-buffer (if (buffer-live-p source) source buf)
        (let ((resolved (and doc (keymap-popup--resolve-description doc))))
          (with-current-buffer buf
            (setq-local keymap-popup--resolved-docstring resolved)))
        (keymap-popup--refresh-buffer buf descs prefix)))))

(defun keymap-popup--resolve-key (entry keymap)
  "Resolve ENTRY's :command to a key in KEYMAP.
Returns entry with :key filled in, or nil if unbound."
  (if (plist-get entry :key) entry
    (and-let* ((cmd (plist-get entry :command))
               (keys (where-is-internal cmd keymap t)))
      (plist-put (copy-sequence entry) :key (key-description keys)))))

(defun keymap-popup--resolve-descriptions (rows keymap)
  "Resolve command symbols to keys in ROWS using KEYMAP.
Drops entries whose command has no binding."
  (keymap-popup--map-groups
   rows
   (lambda (group)
     (plist-put (copy-sequence group) :entries
                (cl-loop for entry in (plist-get group :entries)
                         when (keymap-popup--resolve-key entry keymap)
                         collect it)))))

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
              (map (buffer-local-value 'keymap-popup--wrapper-map buf)))
    (internal-pop-keymap map 'overriding-terminal-local-map)))

(defun keymap-popup--resume ()
  "Resume the popup's transient map after minibuffer input."
  (when-let* ((buf (get-buffer keymap-popup--buffer-name))
              (map (buffer-local-value 'keymap-popup--wrapper-map buf)))
    (internal-push-keymap map 'overriding-terminal-local-map)))

(defun keymap-popup--teardown (buf)
  "Remove the popup display for BUF and kill it."
  (when (buffer-live-p buf)
    (remove-hook 'minibuffer-setup-hook #'keymap-popup--suspend)
    (remove-hook 'minibuffer-exit-hook #'keymap-popup--resume)
    (when-let* ((hide (plist-get (buffer-local-value 'keymap-popup--display-backend buf)
                                 :hide)))
      (funcall hide buf))
    (kill-buffer buf)))

(defun keymap-popup--make-keep-pred (buf)
  "Return a keep-pred for `set-transient-map'.
Reads state from BUF.  Consumes the reentering flag on read."
  (lambda ()
    (and (buffer-live-p buf)
         (let ((key-str (key-description (this-command-keys-vector)))
               (exit-key (buffer-local-value 'keymap-popup--active-exit-key buf)))
           (cond
            ((active-minibuffer-window) t)
            ((buffer-local-value 'keymap-popup--reentering buf)
             (with-current-buffer buf
               (setq-local keymap-popup--reentering nil))
             t)
            ((memq this-command
                   '(universal-argument universal-argument-more
					digit-argument negative-argument
					keymap-popup--prefix-argument
					describe-key describe-key-briefly)))
            ((equal key-str exit-key) nil)
            ((eq this-command 'keyboard-quit) nil)
            ((eq this-command 'keymap-popup--inapt-stub) t)
            ((buffer-local-value 'keymap-popup--persistent buf))
            (t (and-let* ((descs (buffer-local-value
                                  'keymap-popup--active-descriptions buf)))
                 (keymap-popup--keep-popup-p descs key-str))))))))

(defun keymap-popup--make-on-exit (buf)
  "Return an on-exit callback for `set-transient-map' closing BUF.
Pops the sub-menu stack if exit-key or \\`C-g' caused the exit,
otherwise tears down completely."
  (lambda ()
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (let ((key-str (key-description (this-command-keys-vector))))
          (if (and keymap-popup--stack
                   (or (equal key-str keymap-popup--active-exit-key)
                       (equal key-str "C-g")))
              (pcase-let ((`(:keymap ,km :descriptions ,descs :docstring ,doc
                                     :exit-key ,ek)
                           (pop keymap-popup--stack)))
                (setq-local keymap-popup--active-keymap km
                            keymap-popup--active-descriptions descs
                            keymap-popup--active-docstring doc
                            keymap-popup--active-exit-key ek
                            keymap-popup--reentering t
                            keymap-popup--prefix-mode nil)
                (keymap-popup--refresh buf))
            (keymap-popup--teardown buf)))))))

(defun keymap-popup--collect-entries (descriptions fn)
  "Collect non-nil results of (FN ENTRY GROUP) across DESCRIPTIONS.
Walks rows, groups, and entries.  FN receives an entry plist and
its parent group plist; non-nil return values are collected."
  (mapcan (lambda (row)
            (mapcan (lambda (group)
                      (mapcan (lambda (entry)
                                (and-let* ((result (funcall fn entry group)))
                                  (list result)))
                              (plist-get group :entries)))
                    row))
          descriptions))

(defun keymap-popup--classify-entries (descriptions)
  "Walk DESCRIPTIONS once, classify entries by type and properties.
Returns plist (:switches KEYS :submenus PAIRS :stay-open KEYS)."
  (let ((entries (keymap-popup--collect-entries
                  descriptions
                  (lambda (entry _group)
                    (and-let* ((key (plist-get entry :key)))
                      (list :key key
                            :type (plist-get entry :type)
                            :target (plist-get entry :target)
                            :stay-open (plist-get entry :stay-open)))))))
    (cl-loop for e in entries
             for type = (plist-get e :type)
             for key = (plist-get e :key)
             when (eq type 'switch) collect key into switches
             when (eq type 'keymap) collect (cons key (plist-get e :target)) into submenus
             when (and (eq type 'suffix) (plist-get e :stay-open))
             collect key into stay-open
             finally return (list :switches switches
                                  :submenus submenus
                                  :stay-open stay-open))))

(defun keymap-popup--push-submenu (buf child-keymap)
  "Push current popup state in BUF and activate CHILD-KEYMAP's transient map."
  (with-current-buffer buf
    (push (list :keymap keymap-popup--active-keymap
                :descriptions keymap-popup--active-descriptions
                :docstring keymap-popup--active-docstring
                :exit-key keymap-popup--active-exit-key)
          keymap-popup--stack)
    (let* ((descs (keymap-popup--resolve-descriptions
                   (keymap-popup--collect-descriptions child-keymap)
                   child-keymap))
           (doc (keymap-popup--meta child-keymap 'description))
           (exit-key (or (keymap-popup--meta child-keymap 'exit-key)
                         keymap-popup-default-exit-key)))
      (setq-local keymap-popup--active-keymap child-keymap
                  keymap-popup--active-descriptions descs
                  keymap-popup--active-docstring doc
                  keymap-popup--active-exit-key exit-key
                  keymap-popup--prefix-mode nil)
      (keymap-popup--refresh buf)
      (keymap-popup--activate-transient-map buf child-keymap descs exit-key))))

(defun keymap-popup--prefix-argument ()
  "Toggle prefix argument mode in the active popup.
When toggling on, activates `universal-argument-map' so that
subsequent digit and `negative-argument' keys refine the prefix."
  (interactive)
  (when-let* ((buf (get-buffer keymap-popup--buffer-name)))
    (with-current-buffer buf
      (setq-local keymap-popup--prefix-mode
                  (not keymap-popup--prefix-mode))
      (setq prefix-arg
            (and keymap-popup--prefix-mode '(4))))
    (keymap-popup--refresh buf)
    (when (buffer-local-value 'keymap-popup--prefix-mode buf)
      (universal-argument--mode))))

(defun keymap-popup--core-overrides (exit-key)
  "Return alist of core overrides for EXIT-KEY and prefix toggle.
The exit key is bound to `ignore'; the keep-pred exits on the key
itself, so the binding only needs to exist and do nothing."
  (list (cons exit-key #'ignore)
        (cons "C-u" #'keymap-popup--prefix-argument)))

(defun keymap-popup--inapt-stub ()
  "Refuse an inapt key press.
Bound dynamically by the menu-item :filter on entries whose
`:inapt-if' predicate is currently non-nil.  Preserves the popup's
prefix-mode state so `\\[universal-argument]' is not consumed."
  (interactive)
  (message "Command unavailable")
  ;; The popup's prefix-mode only ever stores `(4)' (see
  ;; `keymap-popup--prefix-argument'), so re-setting that value is
  ;; preservation, not approximation.
  (when-let* ((buf (get-buffer keymap-popup--buffer-name))
              ((buffer-local-value 'keymap-popup--prefix-mode buf)))
    (setq prefix-arg '(4))))

(defun keymap-popup--submenu-overrides (submenu-pairs buf)
  "Return alist of submenu key overrides from SUBMENU-PAIRS for BUF."
  (mapcar (lambda (pair)
            (cons (car pair)
                  (let ((target (cdr pair)))
                    (lambda () (interactive)
                      (keymap-popup--push-submenu buf target)))))
          submenu-pairs))

(defun keymap-popup--call-real-binding (keymap key-str)
  "Call KEY-STR's live binding in KEYMAP if it is a command.
Return the binding, which may be nil or the inapt stub."
  (let ((cmd (keymap-lookup keymap key-str)))
    (when (commandp cmd)
      (call-interactively cmd))
    cmd))

(defun keymap-popup--switch-overrides (keymap switch-keys buf)
  "Return alist of switch key overrides for KEYMAP's SWITCH-KEYS in BUF.
Wraps the toggle command with prefix-mode consumption.  When the
underlying binding is currently unbound (because an `:if' predicate
is false) or resolves to `keymap-popup--inapt-stub' (because an
`:inapt-if' predicate is active), the toggle is refused and the
prefix-mode is preserved."
  (mapcar (lambda (key-str)
            (cons key-str
                  (lambda () (interactive)
                    (let ((cmd (keymap-popup--call-real-binding keymap key-str)))
                      (unless (or (null cmd)
                                  (eq cmd #'keymap-popup--inapt-stub))
                        (when (buffer-local-value 'keymap-popup--prefix-mode buf)
                          (with-current-buffer buf
                            (setq-local keymap-popup--prefix-mode nil))
                          (setq prefix-arg nil)))
                      (keymap-popup--refresh buf)))))
          switch-keys))

(defun keymap-popup--stay-open-overrides (keymap stay-open-keys buf)
  "Return alist of stay-open suffix overrides for KEYMAP's STAY-OPEN-KEYS in BUF.
Each command executes and refreshes the popup in place."
  (mapcar (lambda (key-str)
            (cons key-str
                  (lambda () (interactive)
                    (keymap-popup--call-real-binding keymap key-str)
                    (keymap-popup--refresh buf))))
          stay-open-keys))

(defun keymap-popup--build-wrapper-map (keymap descriptions buf exit-key)
  "Build wrapper keymap over KEYMAP with DESCRIPTIONS for BUF.
EXIT-KEY and switch/submenu/stay-open handlers layer over KEYMAP.
Predicate enforcement lives on KEYMAP itself via `menu-item' :filter,
so the wrapper carries no inapt logic of its own."
  (let* ((map (make-sparse-keymap))
         (classified (keymap-popup--classify-entries descriptions))
         (overrides (append (keymap-popup--core-overrides exit-key)
                            (keymap-popup--switch-overrides
                             keymap (plist-get classified :switches) buf)
                            (keymap-popup--submenu-overrides
                             (plist-get classified :submenus) buf)
                            (keymap-popup--stay-open-overrides
                             keymap (plist-get classified :stay-open) buf))))
    (set-keymap-parent map keymap)
    (pcase-dolist (`(,key . ,cmd) overrides)
      (keymap-set map key cmd))
    map))

(defun keymap-popup-dismiss ()
  "Dismiss the active popup, if any.
Deactivates the transient map and removes the popup display."
  (interactive)
  (when-let* ((buf (get-buffer keymap-popup--buffer-name))
              (map (buffer-local-value 'keymap-popup--wrapper-map buf)))
    (internal-pop-keymap map 'overriding-terminal-local-map)
    (keymap-popup--teardown buf)))

(defun keymap-popup--session-inputs (keymap)
  "Derive a plist of session inputs from KEYMAP and the current buffer.
Returns (:source BUF :keymap MAP :descriptions D :docstring S
         :exit-key K :persistent P :backend B)."
  (list :source (current-buffer)
        :keymap keymap
        :descriptions (keymap-popup--resolve-descriptions
                       (keymap-popup--collect-descriptions keymap)
                       keymap)
        :docstring (keymap-popup--meta keymap 'description)
        :exit-key (or (keymap-popup--meta keymap 'exit-key)
                      keymap-popup-default-exit-key)
        :persistent (or (keymap-popup--meta keymap 'persistent)
                        keymap-popup-persistent)
        :backend (funcall keymap-popup-backend)))

(defun keymap-popup--init-buffer-state (buf session)
  "Initialize BUF's buffer-local state from SESSION plist."
  (with-current-buffer buf
    (setq-local keymap-popup--source-buffer (plist-get session :source)
                keymap-popup--active-keymap (plist-get session :keymap)
                keymap-popup--active-descriptions (plist-get session :descriptions)
                keymap-popup--active-docstring (plist-get session :docstring)
                keymap-popup--active-exit-key (plist-get session :exit-key)
                keymap-popup--persistent (plist-get session :persistent)
                keymap-popup--display-backend (plist-get session :backend)
                keymap-popup--stack nil
                keymap-popup--prefix-mode nil
                keymap-popup--reentering nil)))

(defun keymap-popup--activate-transient-map (buf keymap descriptions exit-key)
  "Build the wrapper for KEYMAP using DESCRIPTIONS, stash on BUF, activate.
EXIT-KEY is bound in the wrapper to dismiss the popup."
  (let ((wrapper (keymap-popup--build-wrapper-map keymap descriptions buf exit-key)))
    (with-current-buffer buf
      (setq-local keymap-popup--wrapper-map wrapper))
    (set-transient-map wrapper
                       (keymap-popup--make-keep-pred buf)
                       (keymap-popup--make-on-exit buf))))

(defun keymap-popup--install-persistent-hook (buf)
  "Add a self-removing post-command-hook that refreshes BUF until it dies."
  (let ((hook-fn (make-symbol "keymap-popup--persistent-refresh")))
    (fset hook-fn
          (lambda ()
            (if (buffer-live-p buf)
                (keymap-popup--refresh buf)
              (remove-hook 'post-command-hook hook-fn))))
    (add-hook 'post-command-hook hook-fn)))

;;;###autoload
(defun keymap-popup (keymap)
  "Show popup help for described KEYMAP.
Activates KEYMAP as a transient map.  Switch keys execute and re-render
without closing.  Inapt keys signal `Command unavailable' via
`keymap-popup--inapt-stub'.  Sub-menu keys push a navigation stack.
\\[universal-argument] toggles prefix mode."
  (or (keymap-popup--meta keymap 'descriptions)
      (user-error "No descriptions in keymap"))
  (let ((session (keymap-popup--session-inputs keymap))
        (buf (keymap-popup--prepare-buffer)))
    (keymap-popup--init-buffer-state buf session)
    (keymap-popup--refresh buf)
    (funcall (plist-get (plist-get session :backend) :show) buf)
    (keymap-popup--activate-transient-map
     buf keymap
     (plist-get session :descriptions)
     (plist-get session :exit-key))
    (add-hook 'minibuffer-setup-hook #'keymap-popup--suspend)
    (add-hook 'minibuffer-exit-hook #'keymap-popup--resume)
    (when (plist-get session :persistent)
      (keymap-popup--install-persistent-hook buf))))

(provide 'keymap-popup)
;;; keymap-popup.el ends here

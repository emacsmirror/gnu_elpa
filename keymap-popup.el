;;; keymap-popup.el --- Described keymaps with popup help  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Free Software Foundation, Inc.

;; Author: Thanos Apollo
;; Version: 0.2.1
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

;;; Code:

(require 'cl-lib)

(defgroup keymap-popup nil
  "Described keymaps with popup help."
  :group 'convenience)

(defcustom keymap-popup-display-action
  '(display-buffer-in-side-window (side . bottom))
  "Display action for the popup buffer.
Common values:
  (display-buffer-in-side-window (side . bottom))  - frame-wide
  (display-buffer-below-selected)                  - current window only"
  :type 'sexp
  :group 'keymap-popup)

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
  (lookup-key keymap (vector prop)))

(gv-define-setter keymap-popup--meta (val keymap prop)
  `(define-key ,keymap (vector ,prop) ,val))

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
      ;; Annotated entry: key is a command symbol, spec is (DESC . PROPS)
      ;; or a bare string
      (let* ((spec (if (stringp spec) (list spec) spec))
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
  (keymap-popup--split-groups-1 bindings nil nil nil nil))

(defun keymap-popup--split-groups-1 (rest name entries groups rows)
  "Recursive helper for `keymap-popup--split-groups'.
REST is remaining bindings, NAME is current group name, ENTRIES
is accumulated entries (reversed), GROUPS is current row's groups
\(reversed), ROWS is accumulated rows (reversed)."
  (let ((flush-group (if entries
                         (cons (cons name (reverse entries)) groups)
                       groups)))
    (cond
     ((null rest)
      (reverse (if flush-group
                   (cons (reverse flush-group) rows)
                 rows)))
     ((eq (car rest) :row)
      (keymap-popup--split-groups-1
       (cdr rest) nil nil nil
       (if flush-group (cons (reverse flush-group) rows) rows)))
     ((eq (car rest) :group)
      (keymap-popup--split-groups-1
       (cddr rest) (cadr rest) nil flush-group rows))
     (t
      (keymap-popup--split-groups-1
       (cddr rest) name
       (cons (cons (car rest) (cadr rest)) entries)
       groups rows)))))

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
         (entries (mapcar (lambda (pair)
                            (keymap-popup--parse-entry (car pair) (cdr pair)))
                          (cdr chunk))))
    `(:name ,name :entries ,entries ,@group-props)))

(defun keymap-popup--parse-bindings (bindings)
  "Parse BINDINGS into a list of rows.
Each row is a list of group plists with :name and :entries."
  (mapcar (lambda (row) (mapcar #'keymap-popup--parse-chunk row))
          (keymap-popup--split-groups bindings)))

;;; Infix generators

(defun keymap-popup--switch-forms (map-name entry)
  "Return (defvar-local defun) forms for switch ENTRY in MAP-NAME."
  (let* ((variable (plist-get entry :variable))
         (description (plist-get entry :description))
         (fn-name (intern (format "%s--toggle-%s" map-name variable))))
    (list
     `(defvar-local ,variable nil)
     `(defun ,fn-name ()
        ,(format "Toggle %s." description)
        (interactive)
        (setq-local ,variable (not ,variable))
        (message "%s: %s" ,description (if ,variable "on" "off"))))))

(defun keymap-popup--entry-command (map-name entry)
  "Return the command to bind in MAP-NAME's keymap for ENTRY."
  (pcase (plist-get entry :type)
    ('suffix (plist-get entry :command))
    ('switch (intern (format "%s--toggle-%s" map-name (plist-get entry :variable))))
    ('keymap (let ((target (plist-get entry :target)))
               `(lambda () (interactive) (keymap-popup ,target))))))

;;; Macro helpers

(defun keymap-popup--build-keymap-pairs (map-name entries)
  "Build flat key/command list for `defvar-keymap' from ENTRIES.
MAP-NAME is used to derive generated command names."
  (cl-loop for entry in entries
           for cmd = (keymap-popup--entry-command map-name entry)
           append (list (plist-get entry :key)
                        (if (symbolp cmd) `#',cmd cmd))))

(defun keymap-popup--quote-if-needed (form)
  "Quote FORM unless it is a lambda, in which case return as-is."
  (if (and (consp form) (eq (car form) 'lambda))
      form
    `',form))

(defun keymap-popup--build-entry-form (entry)
  "Build a `list' form for a single ENTRY that evaluates lambdas properly."
  (let* ((type (plist-get entry :type))
         (key (plist-get entry :key))
         (desc-form (keymap-popup--quote-if-needed
                     (plist-get entry :description)))
         (type-props (pcase type
                       ('suffix `(:command ,(keymap-popup--quote-if-needed
                                             (plist-get entry :command))
					   ,@(when (plist-get entry :stay-open)
					       '(:stay-open t))))
                       ('keymap `(:target ,(plist-get entry :target)))
                       ('switch `(:variable ',(plist-get entry :variable)))))
         (if-pred (plist-get entry :if))
         (inapt-if (plist-get entry :inapt-if)))
    `(list :key ,key
           :description ,desc-form
           :type ',type
           ,@type-props
           ,@(and if-pred (list :if if-pred))
           ,@(and inapt-if (list :inapt-if inapt-if))
           ,@(and-let* ((c-u (plist-get entry :c-u)))
               (list :c-u c-u)))))

(defun keymap-popup--build-descriptions-form (rows)
  "Build a `list' form that constructs descriptions at load time.
ROWS is a list of rows, each row a list of groups.
Uses list calls so lambdas get compiled."
  `(list ,@(mapcar
            (lambda (row)
              `(list ,@(mapcar
                        (lambda (group)
                          (let ((if-pred (plist-get group :if))
                                (inapt-if (plist-get group :inapt-if)))
                            `(list :name ,(plist-get group :name)
                                   :entries (list ,@(mapcar #'keymap-popup--build-entry-form
                                                            (plist-get group :entries)))
                                   ,@(and if-pred (list :if if-pred))
                                   ,@(and inapt-if (list :inapt-if inapt-if)))))
                        row)))
            rows)))

;;; Macro

(defun keymap-popup--consume-keyword (rest keyword)
  "If REST starts with KEYWORD, return (VALUE . REMAINING), else nil."
  (and (eq (car rest) keyword)
       (cons (cadr rest) (cddr rest))))

(defun keymap-popup--extract-macro-opts (body)
  "Extract macro options from BODY.
Returns (DOCSTRING POPUP-KEY EXIT-KEY PARENT DESCRIPTION BINDINGS).
A string followed by a list is a key binding, not a docstring."
  (let* ((docstring (and (stringp (car body))
                         (or (null (cadr body))
                             (not (listp (cadr body))))
                         (car body)))
         (rest (if docstring (cdr body) body))
         (popup-pair (keymap-popup--consume-keyword rest :popup-key))
         (popup-key (if popup-pair (car popup-pair) "h"))
         (rest (if popup-pair (cdr popup-pair) rest))
         (exit-pair (keymap-popup--consume-keyword rest :exit-key))
         (exit-key (if exit-pair (car exit-pair) ?q))
         (rest (if exit-pair (cdr exit-pair) rest))
         (parent-pair (keymap-popup--consume-keyword rest :parent))
         (parent (when parent-pair (car parent-pair)))
         (rest (if parent-pair (cdr parent-pair) rest))
         (desc-pair (keymap-popup--consume-keyword rest :description))
         (description (when desc-pair (car desc-pair)))
         (bindings (if desc-pair (cdr desc-pair) rest)))
    (list docstring popup-key exit-key parent description bindings)))

;;;###autoload
(defmacro keymap-popup-define (name &rest body)
  "Define NAME as a keymap with embedded descriptions.
BODY is an optional docstring, optional :popup-key KEY (default
\"h\"), optional :exit-key CHAR (default ?q), optional :parent
KEYMAP, optional :description STRING-OR-FUNCTION, followed by
:group keywords and KEY (DESC ...) pairs."
  (declare (indent 1))
  (pcase-let* ((`(,docstring ,popup-key ,exit-key ,parent ,description ,bindings)
                (keymap-popup--extract-macro-opts body))
               (rows (keymap-popup--parse-bindings bindings))
               (all-entries (cl-loop for row in rows
				     append (cl-loop for group in row
						     append (plist-get group :entries))))
               (infix-forms (cl-loop for entry in all-entries
				     append (pcase (plist-get entry :type)
                                              ('switch (keymap-popup--switch-forms name entry))
                                              (_ nil))))
               (keymap-pairs (keymap-popup--build-keymap-pairs name all-entries)))
    `(progn
       ,@infix-forms
       (defvar-keymap ,name
         ,@(when docstring (list :doc docstring))
         ,@(when parent (list :parent parent))
         ,@keymap-pairs
         ,popup-key (lambda () (interactive) (keymap-popup ,name)))
       (setf (keymap-popup--meta ,name 'keymap-popup--descriptions)
             ,(keymap-popup--build-descriptions-form rows))
       (setf (keymap-popup--meta ,name 'keymap-popup--exit-key) ,exit-key)
       ,@(when description
           `((setf (keymap-popup--meta ,name 'keymap-popup--description) ,description))))))

;;;###autoload
(defmacro keymap-popup-annotate (keymap &rest body)
  "Annotate existing KEYMAP with popup descriptions.
BODY is :group keywords and COMMAND-SYMBOL DESCRIPTION pairs.
COMMAND-SYMBOL is a function symbol already bound in the keymap.
DESCRIPTION is a string or (STRING &rest PROPS).
Keys are resolved dynamically via `where-is-internal' at display
time, so the popup always reflects the user's current bindings."
  (declare (indent 1))
  (let ((rows (keymap-popup--parse-bindings body)))
    `(progn
       (setf (keymap-popup--meta ,keymap 'keymap-popup--descriptions)
             ,(keymap-popup--build-descriptions-form rows))
       ;; t is the default binding in keymaps, lookup-key ignores it.
       (setf (keymap-popup--meta ,keymap 'keymap-popup--annotated) 'yes))))

;;; Public API

(defun keymap-popup--map-groups (rows fn)
  "Apply FN to each group in ROWS, returning the transformed rows.
FN receives a group plist and returns a new group plist."
  (mapcar (lambda (row) (mapcar fn row)) rows))

(defun keymap-popup--add-entry-to-rows (rows entry group-name)
  "Return ROWS with ENTRY appended to the group named GROUP-NAME.
Falls back to the first group if GROUP-NAME is not found."
  (let ((target (or (cl-loop for row in rows
                             thereis (cl-loop for g in row
                                              when (equal (plist-get g :name) group-name)
                                              return group-name))
                    (plist-get (caar rows) :name))))
    (keymap-popup--map-groups
     rows
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
  (let ((descs (keymap-popup--meta keymap 'keymap-popup--descriptions)))
    (or descs (user-error "No descriptions in keymap"))
    (keymap-set keymap key command)
    (let ((entry (list :key key :description description
                       :type 'suffix :command command)))
      (setf (keymap-popup--meta keymap 'keymap-popup--descriptions)
            (keymap-popup--add-entry-to-rows descs entry group)))))

;;;###autoload
(defun keymap-popup-remove-entry (keymap key)
  "Remove KEY binding from KEYMAP.
Updates both the keymap and the popup descriptions."
  (keymap-set keymap key nil)
  (setf (keymap-popup--meta keymap 'keymap-popup--descriptions)
        (keymap-popup--remove-key-from-rows
         (keymap-popup--meta keymap 'keymap-popup--descriptions) key)))

;;; Renderer

(defun keymap-popup--resolve-description (desc)
  "If DESC is a function, call it; otherwise return as-is."
  (if (functionp desc) (funcall desc) desc))

(defun keymap-popup--render-entry (entry &optional prefix-mode key-width)
  "Render ENTRY into a formatted line, or nil if :if hides it.
When PREFIX-MODE is non-nil, entries with :c-u are highlighted and
their :c-u description is shown; other entries are dimmed.
KEY-WIDTH pads the key column for alignment."
  (when (or (null (plist-get entry :if))
            (funcall (plist-get entry :if)))
    (let* ((inapt (and-let* ((pred (plist-get entry :inapt-if)))
                    (funcall pred)))
           (raw-desc (keymap-popup--resolve-description
                      (plist-get entry :description)))
           (type (plist-get entry :type))
           (desc (if (eq type 'keymap)
                     (propertize raw-desc 'face 'keymap-popup-submenu)
                   raw-desc))
           (c-u-desc (plist-get entry :c-u))
           (raw-key (plist-get entry :key))
           (padded-key (if key-width
                           (concat raw-key
                                   (make-string (max 0 (- key-width (length raw-key)))
                                                ?\s))
                         raw-key))
           (key-str (propertize padded-key 'face 'keymap-popup-key))
           (value-str (if (eq type 'switch)
                          (propertize
                           (if (symbol-value (plist-get entry :variable))
                               " [on]" " [off]")
                           'face 'keymap-popup-value)
			""))
           (c-u-str (and c-u-desc
                         (if prefix-mode
                             (propertize (format " (%s)" c-u-desc)
                                         'face 'warning)
                           (propertize (format " (%s)" c-u-desc)
                                       'face 'shadow))))
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
  (when (or (null (plist-get group :if))
            (funcall (plist-get group :if)))
    (let* ((group-inapt (and-let* ((pred (plist-get group :inapt-if)))
                          (funcall pred)))
           (entries (plist-get group :entries))
           (key-width (cl-loop for entry in entries
                               maximize (length (plist-get entry :key))))
           (header (and-let* ((raw-name (plist-get group :name))
                              (name (keymap-popup--resolve-description raw-name)))
                     (propertize name 'face (if group-inapt
                                                'keymap-popup-inapt
                                              'keymap-popup-group-header))))
           (lines (cl-loop for entry in entries
                           for line = (keymap-popup--render-entry
                                       entry prefix-mode key-width)
                           when line collect line)))
      (when lines
        (let ((result (if header (cons header lines) lines)))
          (if group-inapt
              (mapcar (lambda (line) (propertize line 'face 'keymap-popup-inapt))
                      result)
            result))))))

(defun keymap-popup--string-width-visible (str)
  "Return the visible width of STR, ignoring text properties."
  (string-width (substring-no-properties str)))

(defun keymap-popup--column-width (col)
  "Return the max visible width of lines in COL."
  (cl-loop for line in col
           maximize (keymap-popup--string-width-visible line)))

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
  (mapcar (lambda (row)
            (cl-loop for group in row
                     when (keymap-popup--render-group-lines group prefix-mode)
                     collect it))
          rows))

(defun keymap-popup--global-col-widths (rendered-rows)
  "Compute max column width per position across all RENDERED-ROWS."
  (let ((max-cols (cl-loop for cols in rendered-rows
                           maximize (length cols))))
    (cl-loop for i from 0 below max-cols
             collect (cl-loop for cols in rendered-rows
                              when (nth i cols)
                              maximize (keymap-popup--column-width (nth i cols))))))

(defun keymap-popup--render (docstring rows &optional prefix-mode)
  "Render DOCSTRING and ROWS into a complete popup string.
ROWS is a list of rows, each row a list of groups.
When PREFIX-MODE is non-nil, highlight :c-u entries and dim others.
Column widths are aligned across all rows."
  (let* ((resolved (when docstring
                     (keymap-popup--resolve-description docstring)))
         (doc (when resolved
                (concat (propertize resolved 'face 'font-lock-doc-face)
                        "\n")))
         (rendered-rows (keymap-popup--rows-to-columns rows prefix-mode))
         (col-widths (keymap-popup--global-col-widths rendered-rows))
         (sections (cl-loop for cols in rendered-rows
                            when cols
                            collect (mapconcat #'identity
                                               (keymap-popup--join-columns
						cols "   " col-widths)
                                               "\n"))))
    (concat doc (string-join sections "\n") "\n")))

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
  "Non-nil when C-u prefix mode is active.")
(defvar-local keymap-popup--reentering nil
  "Non-nil when a sub-menu just popped, preventing cascading exit.")
(defvar-local keymap-popup--hook-fn nil
  "The post-command-hook function for this popup session.")

;;; Popup display

(defun keymap-popup--collect-descriptions (keymap)
  "Collect descriptions from KEYMAP and all its parent keymaps.
Walks the native parent chain via `keymap-parent'."
  (cl-loop for map = keymap then (keymap-parent map)
           while map
           when (keymap-popup--meta map 'keymap-popup--descriptions)
           append it))

(defun keymap-popup--find-entry-by-key (descriptions key-str)
  "Find the entry matching KEY-STR in DESCRIPTIONS.
DESCRIPTIONS is a list of rows, each row a list of groups.
Returns the entry plist, or nil."
  (cl-loop for row in descriptions
           thereis (cl-loop for group in row
                            thereis (cl-loop for entry in (plist-get group :entries)
                                             when (equal (plist-get entry :key) key-str)
                                             return entry))))

(defun keymap-popup--infix-p (descriptions key-str)
  "Return non-nil if KEY-STR maps to a switch entry in DESCRIPTIONS."
  (and-let* ((entry (keymap-popup--find-entry-by-key descriptions key-str)))
    (eq (plist-get entry :type) 'switch)))

(defun keymap-popup--keymap-target (descriptions key-str)
  "Return the target map symbol if KEY-STR is a :keymap entry in DESCRIPTIONS."
  (and-let* ((entry (keymap-popup--find-entry-by-key descriptions key-str))
             (_ (eq (plist-get entry :type) 'keymap)))
    (plist-get entry :target)))

(defun keymap-popup--find-group-for-key (descriptions key-str)
  "Find the group containing KEY-STR in DESCRIPTIONS."
  (cl-loop for row in descriptions
           thereis (cl-loop for group in row
                            when (cl-loop for entry in (plist-get group :entries)
                                          thereis (equal (plist-get entry :key) key-str))
                            return group)))

(defun keymap-popup--inapt-p (descriptions key-str)
  "Return non-nil if KEY-STR is inapt in DESCRIPTIONS.
Checks both group-level and entry-level :inapt-if predicates."
  (or (and-let* ((group (keymap-popup--find-group-for-key descriptions key-str))
                 (pred (plist-get group :inapt-if)))
        (funcall pred))
      (and-let* ((entry (keymap-popup--find-entry-by-key descriptions key-str))
                 (pred (plist-get entry :inapt-if)))
        (funcall pred))))

(defun keymap-popup--stay-open-p (descriptions key-str)
  "Return non-nil if KEY-STR should keep the popup open in DESCRIPTIONS.
True for switches and suffixes with :stay-open."
  (and-let* ((entry (keymap-popup--find-entry-by-key descriptions key-str)))
    (or (eq (plist-get entry :type) 'switch)
        (plist-get entry :stay-open))))

(defun keymap-popup--keep-popup-p (descriptions key-str)
  "Return non-nil if KEY-STR should keep the popup open.
True for switches, inapt keys, :keymap entries, and C-u.
Suffixes with :stay-open dismiss and reopen instead."
  (or (keymap-popup--infix-p descriptions key-str)
      (keymap-popup--inapt-p descriptions key-str)
      (keymap-popup--keymap-target descriptions key-str)
      (equal key-str "C-u")))

(defun keymap-popup--refresh-buffer (buf win descriptions &optional docstring prefix-mode)
  "Re-render popup BUF with DESCRIPTIONS, fit WIN.
DOCSTRING is shown at the top if non-nil.  PREFIX-MODE toggles
prefix argument highlighting."
  (let ((content (keymap-popup--render docstring descriptions prefix-mode)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert content)
        (goto-char (point-min))))
    (when (and win (window-live-p win))
      (fit-window-to-buffer win))))

(defun keymap-popup--refresh (buf)
  "Re-render popup BUF from its buffer-local state.
Renders in the source buffer's context so `symbol-value' for
switch variables sees the user's buffer-local values."
  (when (buffer-live-p buf)
    (let ((source (buffer-local-value 'keymap-popup--source-buffer buf))
          (descs (buffer-local-value 'keymap-popup--active-descriptions buf))
          (doc (buffer-local-value 'keymap-popup--active-docstring buf))
          (prefix (buffer-local-value 'keymap-popup--prefix-mode buf)))
      (with-current-buffer (if (buffer-live-p source) source buf)
        (keymap-popup--refresh-buffer
         buf (get-buffer-window buf) descs doc prefix)))))

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

(defun keymap-popup--prepare-buffer ()
  "Create and configure the popup buffer."
  (let ((buf (get-buffer-create "*keymap-popup*")))
    (with-current-buffer buf
      (setq-local buffer-read-only t
                  cursor-type nil
                  mode-line-format nil))
    buf))

(defun keymap-popup--teardown (buf)
  "Remove the popup window for BUF, clean up hooks, and kill it."
  (when (buffer-live-p buf)
    (when-let* ((fn (buffer-local-value 'keymap-popup--hook-fn buf)))
      (remove-hook 'post-command-hook fn))
    (when-let* ((win (get-buffer-window buf)))
      (delete-window win))
    (kill-buffer buf)))

(defun keymap-popup--make-keep-pred (buf)
  "Return a keep-pred for `set-transient-map'.
Reads state from BUF.  Consumes the reentering flag on read."
  (lambda ()
    (or (when (buffer-local-value 'keymap-popup--reentering buf)
          (with-current-buffer buf
            (setq-local keymap-popup--reentering nil))
          t)
        (and-let* ((keys (this-command-keys-vector))
                   (key-str (key-description keys))
                   (descs (buffer-local-value 'keymap-popup--active-descriptions buf)))
          (keymap-popup--keep-popup-p descs key-str)))))

(defun keymap-popup--make-post-command-fn (buf)
  "Return a post-command-hook function that refreshes BUF.
Removes itself when BUF is killed externally."
  (let ((fn (make-symbol "keymap-popup--post-command")))
    (fset fn
          (lambda ()
            (if (not (buffer-live-p buf))
                (remove-hook 'post-command-hook fn)
              (keymap-popup--refresh buf))))
    fn))

(defun keymap-popup--make-on-exit (buf)
  "Return an on-exit callback for `set-transient-map'.
Pops the sub-menu stack if non-empty, otherwise tears down."
  (lambda ()
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (if keymap-popup--stack
            (pcase-let ((`(:keymap ,km :descriptions ,descs :docstring ,doc)
                         (pop keymap-popup--stack)))
              (setq-local keymap-popup--active-keymap km
                          keymap-popup--active-descriptions descs
                          keymap-popup--active-docstring doc
                          keymap-popup--reentering t
                          keymap-popup--prefix-mode nil)
              (keymap-popup--refresh buf))
          (keymap-popup--teardown buf))))))

(defun keymap-popup--inapt-keys (descriptions)
  "Return a list of key-strings that may be inapt in DESCRIPTIONS.
Includes keys with entry-level :inapt-if and keys in groups with :inapt-if."
  (cl-loop for row in descriptions
           append (cl-loop for group in row
                           for group-pred = (plist-get group :inapt-if)
                           append (cl-loop for entry in (plist-get group :entries)
                                           when (and (plist-get entry :key)
                                                     (or (plist-get entry :inapt-if)
                                                         group-pred))
                                           collect (plist-get entry :key)))))

(defun keymap-popup--stay-open-suffix-keys (descriptions)
  "Return list of key-strings for :stay-open suffix entries in DESCRIPTIONS."
  (cl-loop for row in descriptions
           append (cl-loop for group in row
                           append (cl-loop for entry in (plist-get group :entries)
                                           when (and (plist-get entry :key)
                                                     (eq (plist-get entry :type) 'suffix)
                                                     (plist-get entry :stay-open))
                                           collect (plist-get entry :key)))))

(defun keymap-popup--switch-keys (descriptions)
  "Return list of key-strings for switch entries in DESCRIPTIONS."
  (cl-loop for row in descriptions
           append (cl-loop for group in row
                           append (cl-loop for entry in (plist-get group :entries)
                                           when (eq (plist-get entry :type) 'switch)
                                           collect (plist-get entry :key)))))

(defun keymap-popup--submenu-keys (descriptions)
  "Return an alist of (KEY-STRING . TARGET-KEYMAP) for :keymap entries."
  (cl-loop for row in descriptions
           append (cl-loop for group in row
                           append (cl-loop for entry in (plist-get group :entries)
                                           when (eq (plist-get entry :type) 'keymap)
                                           collect (cons (plist-get entry :key)
                                                         (plist-get entry :target))))))

(defun keymap-popup--push-submenu (buf child-keymap)
  "Push current popup state and activate CHILD-KEYMAP's transient map."
  (with-current-buffer buf
    (push (list :keymap keymap-popup--active-keymap
                :descriptions keymap-popup--active-descriptions
                :docstring keymap-popup--active-docstring)
          keymap-popup--stack)
    (let* ((raw (keymap-popup--collect-descriptions child-keymap))
           (descs (if (keymap-popup--meta child-keymap 'keymap-popup--annotated)
                      (keymap-popup--resolve-descriptions raw child-keymap)
                    raw))
           (doc (keymap-popup--meta child-keymap 'keymap-popup--description))
           (exit-key (or (keymap-popup--meta child-keymap 'keymap-popup--exit-key) ?q)))
      (setq-local keymap-popup--active-keymap child-keymap
                  keymap-popup--active-descriptions descs
                  keymap-popup--active-docstring doc
                  keymap-popup--prefix-mode nil)
      (keymap-popup--refresh buf)
      (set-transient-map
       (keymap-popup--build-wrapper-map child-keymap descs buf exit-key)
       (keymap-popup--make-keep-pred buf)
       (keymap-popup--make-on-exit buf)))))

(defun keymap-popup--core-overrides (buf exit-key)
  "Return alist of core overrides: exit key and C-u prefix toggle."
  (list (cons (key-description (vector exit-key))
              (lambda () (interactive)))
        (cons "C-u"
              (lambda () (interactive)
                (with-current-buffer buf
                  (setq-local keymap-popup--prefix-mode
                              (not keymap-popup--prefix-mode))
                  (setq prefix-arg
                        (when keymap-popup--prefix-mode '(4))))))))

(defun keymap-popup--inapt-overrides (keymap descriptions buf)
  "Return alist of inapt key overrides.
Each command checks the predicate dynamically at invocation time."
  (mapcar (lambda (key-str)
            (cons key-str
                  (lambda () (interactive)
                    (let ((descs (buffer-local-value
                                  'keymap-popup--active-descriptions buf)))
                      (if (keymap-popup--inapt-p descs key-str)
                          (progn
                            (message "Command unavailable")
                            (when (buffer-local-value
                                   'keymap-popup--prefix-mode buf)
                              (setq prefix-arg '(4))))
                        (call-interactively
                         (keymap-lookup keymap key-str)))))))
          (keymap-popup--inapt-keys descriptions)))

(defun keymap-popup--submenu-overrides (descriptions buf)
  "Return alist of submenu key overrides."
  (mapcar (lambda (pair)
            (cons (car pair)
                  (let ((target (cdr pair)))
                    (lambda () (interactive)
                      (keymap-popup--push-submenu buf target)))))
          (keymap-popup--submenu-keys descriptions)))

(defun keymap-popup--switch-overrides (keymap descriptions buf)
  "Return alist of switch key overrides.
Wraps the toggle command with prefix-mode consumption."
  (mapcar (lambda (key-str)
            (cons key-str
                  (lambda () (interactive)
                    (call-interactively (keymap-lookup keymap key-str))
                    (when (buffer-local-value 'keymap-popup--prefix-mode buf)
                      (with-current-buffer buf
                        (setq-local keymap-popup--prefix-mode nil))
                      (setq prefix-arg nil)))))
          (keymap-popup--switch-keys descriptions)))

(defun keymap-popup--stay-open-overrides (keymap descriptions)
  "Return alist of stay-open suffix overrides.
Each command dismisses the popup, executes, and reopens."
  (mapcar (lambda (key-str)
            (cons key-str
                  (lambda () (interactive)
                    (call-interactively (keymap-lookup keymap key-str))
                    (keymap-popup keymap))))
          (keymap-popup--stay-open-suffix-keys descriptions)))

(defun keymap-popup--build-wrapper-map (keymap descriptions buf exit-key)
  "Build wrapper keymap over KEYMAP with all popup overrides."
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map keymap)
    (pcase-dolist (`(,key . ,cmd)
                   (append (keymap-popup--core-overrides buf exit-key)
                           (keymap-popup--switch-overrides keymap descriptions buf)
                           (keymap-popup--inapt-overrides keymap descriptions buf)
                           (keymap-popup--submenu-overrides descriptions buf)
                           (keymap-popup--stay-open-overrides keymap descriptions)))
      (keymap-set map key cmd))
    map))

;;;###autoload
(defun keymap-popup (keymap)
  "Show popup help for described KEYMAP.
Activates KEYMAP as a transient map.  Switch keys execute and
re-render without closing.  Inapt keys are blocked.
Sub-menu keys push a navigation stack.  C-u toggles prefix mode."
  (or (keymap-popup--meta keymap 'keymap-popup--descriptions)
      (user-error "No descriptions in keymap"))
  (let* ((source (current-buffer))
         (buf (keymap-popup--prepare-buffer))
         (raw (keymap-popup--collect-descriptions keymap))
         (descriptions (if (keymap-popup--meta keymap 'keymap-popup--annotated)
                           (keymap-popup--resolve-descriptions raw keymap)
                         raw))
         (docstring (keymap-popup--meta keymap 'keymap-popup--description))
         (exit-key (or (keymap-popup--meta keymap 'keymap-popup--exit-key) ?q))
         (hook-fn (keymap-popup--make-post-command-fn buf)))
    (with-current-buffer buf
      (setq-local keymap-popup--source-buffer source
                  keymap-popup--active-keymap keymap
                  keymap-popup--active-descriptions descriptions
                  keymap-popup--active-docstring docstring
                  keymap-popup--stack nil
                  keymap-popup--prefix-mode nil
                  keymap-popup--reentering nil
                  keymap-popup--hook-fn hook-fn))
    (keymap-popup--refresh buf)
    (display-buffer buf (append keymap-popup-display-action
                                '((window-height . fit-window-to-buffer))))
    (when-let* ((win (get-buffer-window buf)))
      (fit-window-to-buffer win))
    (add-hook 'post-command-hook hook-fn)
    (set-transient-map
     (keymap-popup--build-wrapper-map keymap descriptions buf exit-key)
     (keymap-popup--make-keep-pred buf)
     (keymap-popup--make-on-exit buf))))

(provide 'keymap-popup)
;;; keymap-popup.el ends here

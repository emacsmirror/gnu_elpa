;;; cl-ts-mode.el --- lisp-mode with tree-sitter support -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zach shaftel

;; Author: zach shaftel <zach@shaf.tel>
;; Maintainer: zach shaftel <zach@shaf.tel>
;; Created: May 14, 2026
;; Version: 0.0.1
;; Keywords:
;; URL:
;; Package-Requires: ((emacs "31"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;;  lisp-mode with tree-sitter support

;;; Code:

(require 'treesit)
(eval-when-compile
  (require 'subr-x))

(ts-declare-unavailable-functions)

;; this guard is temporary, just while developing so i can test grammar changes
;; locally before pushing commits. it's much easier to debug the grammar in a
;; buffer than with tree-sitter-cli.
(let* ((this-dir (file-name-directory (or (macroexp-file-name) (buffer-file-name))))
       (repo (if (file-exists-p (expand-file-name "grammars/cl/grammar.js" this-dir))
                 (directory-file-name this-dir)
               "https://codeberg.org/zshaftel/tree-sitter-cl-syntax")))
  (setf (alist-get 'common-lisp ts-language-source-alist)
        `(,repo :source-dir "grammars/cl/src"))
  (setf (alist-get 'cl-format ts-language-source-alist)
        `(,repo :source-dir "grammars/format/src")))

;; not ready yet
;; (when (boundp 'treesit-major-mode-remap-alist)
;;   (add-to-list 'treesit-major-mode-remap-alist
;;                '(lisp-mode . cl-ts-mode)))

(defgroup cl-ts-mode ()
  "Common Lisp mode with tree-sitter support."
  :group 'lisp)

;; most of the inherited faces are arbitrary, it's just to keep the faces
;; consistent with the current theme's color palette.
(defface cl-ts-mode-format-tilde '((t :inherit font-lock-regexp-grouping-backslash))
  "Face for the ~ in format directive")

(defface cl-ts-mode-format-char-parameter '((t :inherit font-lock-delimiter-face))
  "Face for character parameters to format directives.")

(defface cl-ts-mode-format-numeric-parameter '((t :inherit font-lock-number-face))
  "For for numeric parameters to format directives.")

(defface cl-ts-mode-format-arg-parameter
  '((t :inherit cl-ts-mode-format-char-parameter))
  "For V/v parameters to format directives.")

(defface cl-ts-mode-format-remaining-parameter
  '((t :inherit cl-ts-mode-format-numeric-parameter))
  "For # parameters to format directives.")

(defface cl-ts-mode-format-comma '((t :inherit font-lock-punctuation-face))
  "Face for commas that separate parameters to format directives.")

(defface cl-ts-mode-format-at '((t :inherit font-lock-function-name-face))
  "Face for @ modifiers in format directives.")

(defface cl-ts-mode-format-colon '((t :inherit font-lock-builtin-face))
  "Face for : modifiers in format directives.")

(defface cl-ts-mode-format-standalone-directive '((t :inherit font-lock-property-use-face))
  "Face for simple format directive characters like A and S.")

(defface cl-ts-mode-format-paired-directive '((t :weight extra-bold
                                                 :inherit font-lock-regexp-grouping-construct))
  "Face used to highlight paired format directives: ~<~>, ~{~}, ~(~), and ~[~].
If `cl-ts-mode-format-rainbow-delimiters' is non-nil, this face is
merged with the rainbow delimiters face (the latter taking precedence).")

(defface cl-ts-mode-format-skipped-whitespace
  '((default :inherit font-lock-comment-face :weight bold)
    (((supports :underline (:color foreground-color :style dashes)))
     :underline (:color foreground-color :style dashes))
    (((supports :underline (:color foreground-color :style dots)))
     :underline (:color foreground-color :style dots))
    (((supports :strike-through t)) :strike-through t)
    (t :underline t))
  "Face placed over all whitespace ignored by the ~<newline> format directive.")

(defface cl-ts-mode-block-comment-depth-1 '((t :inherit font-lock-comment-face))
  "Face used to highlight block comments nested by 1 level. Top level block
comments are highlighted with `font-lock-comment-face'.")

(defface cl-ts-mode-block-comment-depth-2 '((t :inherit cl-ts-mode-block-comment-depth-1))
  "Face used to highlight block comments nested by 2 levels.")

(defface cl-ts-mode-block-comment-depth-3 '((t :inherit cl-ts-mode-block-comment-depth-2))
  "Face used to highlight block comments nested by 3 levels.")

(defface cl-ts-mode-positive-read-conditional
  '((t :weight extra-bold :inherit success))
  "Face used for #+.")

(defface cl-ts-mode-negative-read-conditional
  '((t :weight extra-bold :inherit error))
  "Face used for #-.")

(defface cl-ts-mode-read-eval
  '((t :weight extra-bold :inherit font-lock-misc-punctuation-face))
  "Face used for #..")

(defface cl-ts-mode-quote
  '((t :weight extra-bold :inherit font-lock-preprocessor-face))
  "Face for \\='.")

(defface cl-ts-mode-sharpquote
  '((t :weight extra-bold :inherit font-lock-constant-face))
  "Face for #\\='.")

(defface cl-ts-mode-quasiquote
  '((t :weight extra-bold :inherit font-lock-string-face))
  "Face for \\=`.")

(defface cl-ts-mode-comma
  '((t :weight extra-bold :inherit font-lock-property-use-face))
  "Face for , (unquote).")

(defface cl-ts-mode-comma-at
  '((t :weight extra-bold
       :inherit (font-lock-string-face cl-ts-mode-comma)))
  "Face for ,@ (unquote splice).")

(defface cl-ts-mode-comma-dot
  '((t :weight extra-bold
       :inherit (font-lock-warning-face cl-ts-mode-comma)))
  "Face for ,. (unquote nconc).")

(defconst cl-ts-mode--block-comment-faces
  [font-lock-comment-face
   cl-ts-mode-block-comment-depth-1
   cl-ts-mode-block-comment-depth-2
   cl-ts-mode-block-comment-depth-3])

(defcustom cl-ts-mode-comment-darken-percentage 15.0
  "Used as the second argument to `color-darken-name' in
`cl-ts-mode-update-comment-faces'."
  :type 'number)

(declare-function color-darken-name "color")

;;;###autoload
(defun cl-ts-mode-update-comment-faces (&optional theme)
  "A function to update the block comment faces by increasing darkness.
Meant to be added to `enable-theme-functions'. The darkening ratio is
controlled by `cl-ts-mode-comment-darken-percentage', or by THEME's
`cl-ts-mode-comment-darken-percentage' symbol property if that's
non-nil."
  (let* ((comment-fore (face-attribute 'font-lock-comment-face :foreground nil t))
         (percentage (or (and theme (get theme 'cl-ts-mode-comment-darken-percentage))
                         cl-ts-mode-comment-darken-percentage)))
    (when (stringp comment-fore)
      (require 'color)
      (let ((i 0))
        (while (< i 3)
          (incf i)
          (thread-last (* percentage i)
            (color-darken-name comment-fore)
            (set-face-attribute (aref cl-ts-mode--block-comment-faces i)
                                nil :foreground)))))))

(defcustom cl-ts-mode-format-rainbow-delimiters (featurep 'rainbow-delimiters)
  "Whether paired format directives like ~[~] are highlighted based on
nesting depth with faces from rainbow-delimiters.el. If this is non-nil
and rainbow-delimiters is not available, a warning will be emitted and
it will be set to nil."
  :type 'boolean)

(defvar cl-ts-mode--enabled-fl-features ())

(defun cl-ts-mode--format-use-rainbow-delimiters-p ()
  (and cl-ts-mode-format-rainbow-delimiters
       (or (boundp 'rainbow-delimiters-pick-face-function)
           (require 'rainbow-delimiters nil t)
           (prog1 nil
             (lwarn 'cl-ts-mode :warning
                    (concat "rainbow-delimiters.el is not available, "
                            "disabling `cl-ts-mode-format-rainbow-delimiters'"))
             (setq-default cl-ts-mode-format-rainbow-delimiters nil)))))

;; FIXME: should this be a defcustom? this is really just for `clparse-mode' to
;; hook into `cl-ts-mode--fontify-one-format-directive'.
(defvar cl-ts-mode-fontify-format-funcall-function ;say that three times fast!
  nil
  "A function called on the function nodes in ~/function/ FORMAT directives.
It can either return a face which will be added to the node, or add the
face itself and return nil.")

(defun cl-ts-mode--fontify-one-format-directive (node &optional paired-depth mismatch-p)
  (defvar rainbow-delimiters-pick-face-function)
  (let ((child (ts-node-child node 0))
        (colonp nil)
        (atp nil))
    (while child
      (when-let* ((face (pcase (ts-node-type child)
                          ('"~" 'cl-ts-mode-format-tilde)
                          ;; "v" is aliased to "V" in the grammar
                          ('"V" 'cl-ts-mode-format-arg-parameter)
                          ('"#" 'cl-ts-mode-format-remaining-parameter)
                          ('"char_parameter" 'cl-ts-mode-format-char-parameter)
                          ('"numeric_parameter" 'cl-ts-mode-format-numeric-parameter)
                          ('"," 'cl-ts-mode-format-comma)
                          ('"@"
                           (setq atp t)
                           'cl-ts-mode-format-at)
                          ('":"
                           (setq colonp t)
                           'cl-ts-mode-format-colon)
                          ('"directive_character"
                           (cond
                             ((eq (char-after (ts-node-start child)) ?\n)
                              (save-excursion
                                (goto-char (ts-node-start child))
                                (when atp (forward-char)) ;first newline kept
                                (let ((start (point)))
                                  (unless colonp ; only the newline was skipped
                                    (skip-chars-forward "\n\t "))
                                  (add-face-text-property start (point)
                                                          'cl-ts-mode-format-skipped-whitespace))))
                             ((not paired-depth) 'cl-ts-mode-format-standalone-directive)
                             ((not (cl-ts-mode--format-use-rainbow-delimiters-p))
                              'cl-ts-mode-format-paired-directive)
                             (t (list (funcall rainbow-delimiters-pick-face-function
                                               paired-depth (not mismatch-p)
                                               (ts-node-start child))
                                      ;; put this behind it to merge the two faces
                                      'cl-ts-mode-format-paired-directive))))
                          ;; ~/
                          ('"interned_symbol"
                           (when cl-ts-mode-fontify-format-funcall-function
                             (funcall cl-ts-mode-fontify-format-funcall-function child))))))
        (add-face-text-property (ts-node-start child)
                                (ts-node-end child)
                                face))
      (setq child (ts-node-next-sibling child)))))

(defun cl-ts-mode--fontify-format-directives (node depth &optional stop)
  ;; "~&~<~;~{~^~}~:>"
  ;; should have a tree like (simplified):
  ;; (string (format_directive `~&`)
  ;;         (format_group start: (format_directive `~<`)
  ;;                       (format_directive `~;`)
  ;;                       (format_group start: (format_directive `~{`)
  ;;                                     (format_directive `~^`)
  ;;                                     end: (format_directive `~}`))
  ;;                       end: (format_directive `~:>`)))
  (while (not (equal node stop))
    (pcase (ts-node-type node)
      ('"format_directive" (cl-ts-mode--fontify-one-format-directive node))
      ('"format_group"
       (let ((start (ts-node-child-by-field-name node "start"))
             (end (ts-node-child-by-field-name node "end")))
         (cl-ts-mode--fontify-one-format-directive start (1+ depth))
         (cl-ts-mode--fontify-format-directives (ts-node-next-sibling start) (1+ depth) end)
         (cl-ts-mode--fontify-one-format-directive end (1+ depth)))))
    (setq node (ts-node-next-sibling node))))

(defun cl-ts-mode--fontify-string (node override start end &rest _)
  (ignore override)
  (when (and (>= (ts-node-start node) start) (<= (ts-node-end node) end))
    ;; (add-face-text-property (ts-node-start node)
    ;;                         (ts-node-end node)
    ;;                         'font-lock-string-face)
    (when (memq 'format-directive cl-ts-mode--enabled-fl-features)
      (cl-ts-mode--fontify-format-directives (ts-node-child node 1) 0))))

(defun cl-ts-mode--fontify-nested-comments (node depth)
  (let ((child (ts-node-child node 0)))
    (while child
      (let ((type (ts-node-type child)))
        (cond
          ((and (memq 'block-comment cl-ts-mode--enabled-fl-features)
                (string= type "nested_comment"))
           (cl-ts-mode--fontify-nested-comments child (1+ depth)))
          ((member type '("#|" "|#"))
           (let ((beg (ts-node-start child))
                 (end (ts-node-end child)))
             (add-face-text-property beg end 'font-lock-comment-delimiter-face)))))
      (setq child (ts-node-next-sibling child)))
    (add-face-text-property (ts-node-start node)
                            (ts-node-end node)
                            (aref cl-ts-mode--block-comment-faces (min depth 3))
                            t)))

(defun cl-ts-mode--fontify-comment (node override start end &rest _)
  (ignore override)
  (let ((nbeg (ts-node-start node))
        (nend (ts-node-end node)))
    (when (and (>= nbeg start) (<= nend end))
      (pcase (ts-node-type node)
        ('"line_comment"
         (goto-char nbeg)
         (add-face-text-property (prog1 (point) (skip-chars-forward ";" nend))
                                 (point) 'font-lock-comment-delimiter-face)
         (add-face-text-property (point) nend 'font-lock-comment-face))
        ('"block_comment" (cl-ts-mode--fontify-nested-comments node 0))))))

(defun cl-ts-mode--fontify-symbol (node override start end &rest _)
  (ignore override)
  (when (and (>= (ts-node-start node) start) (<= (ts-node-end node) end))
    (let ((child (ts-node-child node 0))
          (everything-else-face nil))
      (while child
        (pcase (ts-node-type child)
          ((or '":" '"::")
           (if (eq (ts-node-start node) (ts-node-start child))
               ;; keyword
               (setq everything-else-face 'font-lock-builtin-face)
             (add-face-text-property (ts-node-start node)
                                     (ts-node-start child)
                                     'font-lock-keyword-face))
           (add-face-text-property (ts-node-start child)
                                   (ts-node-end child)
                                   'font-lock-delimiter-face))
          ('"single_escape"
           (add-face-text-property (ts-node-start child)
                                   (ts-node-end child)
                                   'font-lock-escape-face))
          ('"|"                         ;from a recursive call
           (add-face-text-property (ts-node-start child)
                                   (ts-node-end child)
                                   'font-lock-constant-face))
          ('"multiple_escape"
           ;; multi escapes can't be nested, so there will be at most one level
           ;; of recursion, just to highlight single escapes within the
           ;; multi escape
           (cl-ts-mode--fontify-symbol child override start end)))
        (setq child (ts-node-next-sibling child)))
      (when everything-else-face
        (add-face-text-property (ts-node-start node)
                                (ts-node-end node)
                                everything-else-face
                                t)))))

(defface cl-ts-mode-0-bit '((t :inherit success))
  "Face for 0s in #b0 rationals and #*0 bit vectors.")

(defface cl-ts-mode-1-bit '((t :inherit error))
  "Face for 1s in #b1 rationals and #*1 bit vectors.")

(defun cl-ts-mode--fontify-bits (node override start end &rest _)
  (ignore override)
  (when (and (>= (ts-node-start node) start) (<= (ts-node-end node) end))
    (let* ((node-type (ts-node-type node))
           (node-end (ts-node-end node))
           (ratp (equal node-type "rational"))
           (chars ["0" "1"])
           (faces [cl-ts-mode-0-bit cl-ts-mode-1-bit]))
      (goto-char (ts-node-start node))
      (when (cond
              (ratp (looking-at (rx "#" (or (any "Bb")
                                            (seq (* "0") "2" (any "Rr"))))))
              ((equal node-type "bit_vector")
               (looking-at (rx "#" (* "0-9") "*"))))
        (goto-char (match-end 0))
        (while (memq (following-char) '(?0 ?1))
          (let* ((idx (- (following-char) ?0)))
            (add-face-text-property (prog1 (point)
                                      (skip-chars-forward (aref chars idx) node-end))
                                    (point)
                                    (aref faces idx)))
          (and ratp
               (eq (following-char) ?/)
               (< (point) node-end)
               (forward-char)))))))

(defun cl-ts-mode--font-lock-rules ()
  (ts-font-lock-rules
   ;; this is an embedded parser and we don't have range settings here.
   ;; currently this rule will only run if triggered by `clparse-mode'.
   :language 'cl-format
   :feature 'format-directive
   :override 'prepend
   `((format_string) @cl-ts-mode--fontify-string)
   :default-language 'common-lisp
   :feature 'string
   `((string) @font-lock-string-face)
   :feature 'comment
   `([(line_comment) (block_comment)] @cl-ts-mode--fontify-comment)
   :feature 'number
   :override 'prepend
   ;; complexes have a tree like (complex (real) (real)) so their whole and
   ;; imaginary parts will both get the face. highlighting the entire expression
   ;; #C(1/3 99.1) with font-lock-number-face seems weird to me.
   `((real) @font-lock-number-face)
   :feature 'bits
   :override 'prepend
   `([(bit_vector) (rational)] @cl-ts-mode--fontify-bits)
   :feature 'symbol
   :override 'prepend
   `((symbol_tokens [(single_escape) @font-lock-escape-face
                     (multiple_escape "|" @font-lock-constant-face)])
     [ ;; keywords
      (interned_symbol
       !package
       [":" "::"] @font-lock-delimiter-face
       name: (symbol_tokens) @font-lock-builtin-face)
      (uninterned_symbol
       "#:" @font-lock-delimiter-face
       name: (symbol_tokens) @font-lock-builtin-face)
      ;; other symbols
      (interned_symbol
       package: (symbol_tokens) :? @font-lock-keyword-face
       [":" "::"] :? @font-lock-delimiter-face)])
   ;; these next 2 should probably be merged right?
   :feature 'quote
   :override 'prepend
   `((quote      "'"  @cl-ts-mode-quote)
     (sharpquote "#'" @cl-ts-mode-sharpquote)
     (quasiquote "`"  @cl-ts-mode-quasiquote)
     (unquote   [","  @cl-ts-mode-comma
                 ",@" @cl-ts-mode-comma-at
                 ",." @cl-ts-mode-comma-at]))
   :feature 'reader-macros
   :override 'prepend
   `(["#." @cl-ts-mode-read-eval
      "#+" @cl-ts-mode-positive-read-conditional
      "#-" @cl-ts-mode-negative-read-conditional])))

(defconst cl-ts-mode-font-lock-feature-list
  '((string comment)
    (format-directive number)
    (block-comment symbol)
    (quote reader-macros bits)))

;; i might use this stuff for treesit based structural editing/navigation
;; (forward-sexp, up-list, raise-sexp etc.) since the generic treesit
;; implementation doesn't seem to work, tho ideally we get those to work instead
;; of reinventing the wheel.
(defun cl-ts-mode--thing-node-at-pos (valid-result-pred &optional pos while-match)
  (let* ((root (ts-parser-root-node ts-primary-parser))
         (pos (or pos (point)))
         (node root)
         (curpos pos)
         (while-match (or while-match 'sexp))
         (result nil)
         next)
    (while (and (or (and (setq next (ts-node-first-child-for-pos node curpos t))
                         (<= (ts-node-start next) pos (ts-node-end next)))
                    ;; if point is right after a sexp, return that one
                    (and (eq curpos pos)
                         (setq next (ts-node-first-child-for-pos node (decf curpos) t))
                         (<= (ts-node-start next) pos (ts-node-end next))))
                (ts-node-match-p next while-match t))
      (setq node next)
      (when (ts-node-match-p node valid-result-pred t)
        (setq result node)))
    result))

(define-inline cl-ts-mode-sexp-node-at-pos (&optional pos)
  ;; slight KLUDGE: `treesit-node-match-p' expects a function value, not a
  ;; function name. makes sense tho, since `list' is a very common treesit thing
  (inline-quote (cl-ts-mode--thing-node-at-pos (symbol-function 'always) ,pos)))

(define-inline cl-ts-mode-list-node-at-pos (&optional pos)
  (inline-quote (cl-ts-mode--thing-node-at-pos 'list ,pos)))

(define-inline cl-ts-mode-symbol-node-at-pos (&optional pos)
  (inline-quote (cl-ts-mode--thing-node-at-pos 'symbol ,pos)))

(defun cl-ts-mode--compute-features ()
  (let ((n (cond
             ((integerp ts-font-lock-level) ts-font-lock-level)
             ((alist-get 'cl-ts-mode ts-font-lock-level
                         nil nil (lambda (entry mode)
                                   (provided-mode-derived-p mode entry))))
             (t (alist-get t ts-font-lock-level 3))))
        (f cl-ts-mode-font-lock-feature-list)
        (a ()))
    (while (plusp n)
      (setq a (append (pop f) a))
      (decf n))
    a))

;; treesit-parsers-at and co can only find the ones added by
;; `treesit-range-settings'
(defun cl-ts-mode--find-parser-at (pos parser-list)
  (while (and parser-list
              (not (let ((ranges (ts-parser-included-ranges (car parser-list))))
                     (while (and ranges (not (<= (caar ranges) pos (cdar ranges))))
                       (pop ranges))
                     ranges)))
    (pop parser-list))
  (car parser-list))

(defun cl-ts-mode--parsers-in-region (beg end parser-list)
  (let* ((intersection ()))
    (dolist (parser parser-list)
      (pcase-dolist (`(,lo . ,hi) (ts-parser-included-ranges parser))
        (let ((range-lo (max lo beg))
              (range-hi (min end hi)))
          (when (< range-lo range-hi)
            (push (list range-lo range-hi parser) intersection)))))
    ;; reverse because we wanna do the
    (sort intersection :key #'car :in-place t :reverse t)))

(defun cl-ts-mode--format-directive-at-pos (node pos)
  (let ((child (ts-node-first-child-for-pos node pos)))
    (pcase child
      ('nil nil)
      ((app ts-node-type "format_directive") child)
      ((app ts-node-type "format_group")
       (cl-ts-mode--format-directive-at-pos child pos)))))

(defcustom cl-ts-mode-format-indent-auto-escape-eol "~@"
  "Whether a ~<newline> directive is automatically inserted when indenting
format strings.

If non-nil, when indenting a format directive after a plain newline, a
string is added to the end of the preceding line escaping the newline so
that the directive can be indented without affecting the whitespace of
the output."
  :type
  '(choice (const :tag "Don't indent if the newline isn't escaped" nil)
           (const :tag "Always indent regardless of whether it's escaped" t)
           (string :tag "Always add a specific string" :default "~@")
           (cons :tag "Choose based on context"
                 (string :tag "String used inside a ~<~:> directive" :default "~:@_~")
                 (string :tag "String used anywhere else" :default "~@"))))

(defconst cl-ts-mode--format-pprint-logical-block-query
  (ts-query-compile 'cl-format
                    '((format_group
                       end: (format_directive ":" ((directive_character) @cap
                                                   (:eq? @cap ">")))))))

(defcustom cl-ts-mode-format-indent-parent-predicate
  (rx bos "format_group" eos)
  "A predicate to determine where indentation should occur within format
strings. When a node matches this predicate, any text *within* it is
indented relative to it. For format groups, indent relative to the
opening delimiter + `cl-ts-mode-format-group-indent-offset'; if the
predicate matches the entire format string, indent relative to the
opening \" + `cl-ts-mode-format-string-indent-offset'."
  :type
  '(choice (const :tag "Never indent inside format strings" nil)
           (const :tag "Any format group or the entire format string"
                  "\\`format_\\(?:group\\|string\\)\\'")
           (function :tag "A function to match nodes")
           (regexp :tag "A regexp matching the node's type"
                   :default "\\`format_group\\'")))

(defcustom cl-ts-mode-format-indent-align-block-ends t
  "Determines how the start and directives of paired format constructs are aligned.
Applies to ~<~>, ~[~], ~{~} and ~(~) when the closing delimiter is the
first non whitespace character on a line. If non-nil, the directive
characters themselves are aligned. If nil, the ~ that introduces the
directives are aligned.

non-nil:
~:@{~
    ~A~
  ~}

nil:
~:@{~
    ~A~
~}"
  :type 'boolean)

;; FIXME should also offer "~ relative" indentation.
(defcustom cl-ts-mode-format-group-indent-offset 1
  "Additional columns of indentation used when indenting inside paired
format directives, relative to the opening directive. Example:

With value of 2:
  ~{
     ~A
  ~}
With a value of 0:
  ~{
   ~A
  ~}"
  :type 'integer)

(defcustom cl-ts-mode-format-string-indent-offset 1
  "Additional columns of indentation used when indenting relative to the
start of the format string. Example:

With value of 2:
  \"
     ~A\"
With value of 0:
  \"
  ~A\""
  :type 'integer)

(defun cl-ts-mode--format-indent-parent (directive parser lbeg)
  (let ((pred (if (symbolp cl-ts-mode-format-indent-parent-predicate)
                  (indirect-function cl-ts-mode-format-indent-parent-predicate)
                cl-ts-mode-format-indent-parent-predicate)))
    (cond
      ;; already escaped. with a : modifier the whitespace wouldn't actually be
      ;; escaped (only the newline), but i can't think of a good way to handle
      ;; that situation. maybe we should unconditionally skip indenting if
      ;; there's a colon modifier?
      ((and directive (= (ts-node-end directive) lbeg))
       (ts-parent-until directive pred))
      (cl-ts-mode-format-indent-auto-escape-eol
       (let* ((esc (cond
                     ((stringp cl-ts-mode-format-indent-auto-escape-eol)
                      cl-ts-mode-format-indent-auto-escape-eol)
                     ((atom cl-ts-mode-format-indent-auto-escape-eol) nil)
                     ((ts-query-capture
                       parser
                       cl-ts-mode--format-pprint-logical-block-query
                       (1- lbeg) lbeg t)
                      (car cl-ts-mode-format-indent-auto-escape-eol))
                     (t (cdr cl-ts-mode-format-indent-auto-escape-eol)))))
         (if (and directive (null esc))
             (ts-parent-until directive pred t)
           (cl-assert (length= (ts-parser-included-ranges parser) 1))
           ;; FIXME: we shouldn't be adding this if the line is already indented
           (let* ((root (ts-parser-root-node parser))
                  (beg (ts-node-start root))
                  (end (ts-node-end root))
                  (new-end (copy-marker end t)))
             (goto-char lbeg)
             (unwind-protect
                 (save-excursion
                   (goto-char (1- lbeg))
                   (insert esc)
                   (ts-parser-set-included-ranges
                    parser
                    `((,beg . ,(marker-position new-end)))))
               (move-marker new-end nil))
             (thread-first parser
               (ts-parser-root-node)
               (ts-node-descendant-for-range (point) (point) t)
               (ts-parent-until pred t)))))))))

(defun cl-ts-mode--indent-format-line (parser)
  (let* ((lbeg (line-beginning-position))
         (root (ts-parser-root-node parser))
         (end-directive (cl-ts-mode--format-directive-at-pos root (1- lbeg)))
         (parent (cl-ts-mode--format-indent-parent end-directive parser lbeg)))
    (if (not (ts-node-match-p parent cl-ts-mode-format-indent-parent-predicate))
        'noindent
      (indent-line-to
       (save-excursion
         (let ((starter (ts-node-child-by-field-name parent "start"))
               (ender (ts-node-child-by-field-name parent "end")))
           (max
            0
            (cond*
              ;; whole string
              ((null starter)
               (goto-char (ts-node-start parent))
               (+ (current-column) cl-ts-mode-format-string-indent-offset))
              (t (back-to-indentation)) ;non exit clause
              ((/= (ts-node-start ender) (point))
               ;; not indenting the closing directive so indent relative to the
               ;; opener
               (goto-char (ts-node-end starter))
               (+ (1- (current-column)) ;-1 cuz we're after the opener but the
                                        ;offset is relative to the opener
                  cl-ts-mode-format-group-indent-offset))
              (cl-ts-mode-format-indent-align-block-ends
               (goto-char (ts-node-end starter))
               (- (current-column) (- (ts-node-end ender) (ts-node-start ender))))
              (t (goto-char (ts-node-start starter))
                 (current-column))))))))))

(defun cl-ts-mode-maybe-indent-format-line (&optional parser)
  (if-let* ((_ cl-ts-mode-format-indent-parent-predicate)
            (parser-at (or parser (cl-ts-mode--find-parser-at
                                   (point) (ts-parser-list nil 'cl-format t))))
            (_ (> (line-beginning-position)
                  (1+ (ts-node-start (ts-parser-root-node parser-at))))))
      (cl-ts-mode--indent-format-line parser-at)
    'noindent))

(defun cl-ts-mode-indent-region-wrapper (orig beg end)
  (let ((end-marker (copy-marker end t)))
    (unwind-protect
        (prog1 (funcall orig beg end)
          (redisplay)                   ;force clparse to readjust the ranges
          (when-let* ((_ cl-ts-mode-format-indent-parent-predicate)
                      (parser-ranges
                       (thread-last (ts-parser-list nil 'cl-format t)
                         (cl-ts-mode--parsers-in-region beg end-marker))))
            (save-excursion
              (pcase-dolist (`(,beg ,end ,parser) parser-ranges)
                (goto-char end)
                (while (> (point) (1+ beg))
                  (save-excursion (cl-ts-mode-maybe-indent-format-line parser))
                  (beginning-of-line 0))))))
      (move-marker end-marker nil))))

(defcustom cl-ts-mode-indent-format-excluded-commands ()
  "List of commands in which format indentation is suppressed.
When indentation is triggered by a command in this list, indentation of
format directives is not performed."
  :type '(repeat :default (newline-and-indent) function))

(defun cl-ts-mode-indent-line-wrapper (orig)
  (if (memq this-command cl-ts-mode-indent-format-excluded-commands)
      (funcall orig)
    (let ((lindent (cl-ts-mode-maybe-indent-format-line)))
      (if (eq lindent 'noindent)
          (funcall orig)
        lindent))))
(defun cl-ts-mode--extend-fl-region ()
  (defvar font-lock-beg)
  (defvar font-lock-end)
  (let* ((bnode (thread-first (ts-node-at font-lock-beg treesit-primary-parser t)
                  (treesit-parent-until 'sexp t)))
         (changed-beg (and bnode (< (ts-node-start bnode) font-lock-beg)
                           (setq font-lock-beg (ts-node-start bnode)))))
    (if (and bnode (>= (ts-node-end bnode) font-lock-end))
        (or (< font-lock-end (setq font-lock-end (ts-node-end bnode))) ;in case it's =
            changed-beg)
      ;; font-lock-end - 1 cuz the node returned by `ts-node-at' always ends
      ;; after POS, and if a node ends /at/ POS we don't need to extend the
      ;; region
      (let ((enode (ts-node-at (1- font-lock-end) treesit-primary-parser t))
            ignore-it)
        ;; we don't use parent-until here cuz we also wanna stop if we hit BNODE
        (while (not (or (setq ignore-it (null enode))
                        (setq ignore-it (ts-node-eq enode bnode))
                        (ts-node-match-p enode 'sexp)))
          (setq enode (ts-node-parent enode)))
        (or (and (not ignore-it)
                 (> (ts-node-end enode) font-lock-end)
                 (setq font-lock-end (ts-node-end enode)))
            changed-beg)))))

(defconst cl-ts-mode--syntax-propertize-query
  (treesit-query-compile 'common-lisp
                         ;; giving #3r or #x prefix syntax doesn't seem
                         ;; appropriate to me, but should the # on those get
                         ;; symbol syntax?
                         '(([",@" ",." "#+" "#." "#-" "#:" "#C" "#P"]
                            @prefix)
                           ([(bit_vector) (array)] @array)
                           ((multiple_escape) @escape))))

(defun cl-ts-mode-syntax-propertize (start end)
  (pcase-dolist (`(,cap . ,node)
                 (ts-query-capture ts-primary-parser
                                   cl-ts-mode--syntax-propertize-query
                                   start end))
    (pcase cap
      ;; on the prefixes the first char is either , or # which already have
      ;; prefix syntax, so just add the property to the chars following it
      ('prefix (let ((nbeg (1+ (ts-node-start node)))
                     (nend (ts-node-end node)))
                 (when (string= (ts-node-type node) "#S(") (decf nend))
                 ;; treesit-query-capture gives us nodes intersecting with the
                 ;; range, not necessarily fully contained within it, so we
                 ;; still have to check the ranges on each node
                 (and (>= nbeg start)
                      (<= nend end)
                      (put-text-property nbeg nend 'syntax-table
                                         (string-to-syntax "'")))))
      ('array (let* ((contents (ts-node-child node -1))
                     (last-prefix-node (ts-node-prev-sibling contents)))
                (and (>= (1+ (ts-node-start node)) start)
                     (<= (ts-node-end last-prefix-node) end)
                     (put-text-property (1+ (ts-node-start node))
                                        (ts-node-end last-prefix-node)
                                        'syntax-table
                                        (string-to-syntax "'")))))
      ('escape (and (>= (ts-node-start node) start)
                    (<= (ts-node-end node) end)
                    (put-text-property (ts-node-start node)
                                       (ts-node-end node)
                                       'syntax-table
                                       (string-to-syntax "_")))))))

(defconst cl-ts-mode-thing-settings
  `((common-lisp
     (sexp ,(rx bos (or "string"
                        "interned_symbol"
                        "uninterned_symbol"
                        "struct"
                        "list"
                        "vector"
                        "bit_vector"
                        "array"
                        "quote"
                        "sharpquote"
                        "unquote"
                        "quasiquote"
                        "labelled"
                        "reference"
                        "read_eval"
                        "read_conditional"
                        "character"
                        "complex"
                        "rational"
                        "float")
                eos))
     (comment ,(rx "_comment" eos))
     (symbol ,(rx "_symbol" eos))
     ;; this is used by `treesit-major-mode-setup' to set things like
     ;; `forward-list-function', so we set it to everything "list-like"
     (list ,(rx bos (or "list" "vector" "array" "complex" "struct") eos)))))

;;;###autoload
(define-derived-mode cl-ts-mode prog-mode "Lisp"
  "Common Lisp major mode with tree-sitter support."
  (make-local-variable 'font-lock-defaults)
  (let ((font-lock-defaults font-lock-defaults))
    (lisp-mode-variables t t))
  ;; copied from `lisp-mode'
  (setq-local lisp-indent-function #'common-lisp-indent-function)
  (setq-local comment-start-skip
              "\\(\\(^\\|[^\\\n]\\)\\(\\\\\\\\\\)*\\)\\(;+\\|#|\\) *")
  (setq-local comment-end-skip "[ \t]*\\(\\s>\\||#\\)")
  (setq-local font-lock-comment-end-skip "|#")
  (setq imenu-case-fold-search t)
  (when (and (ts-ensure-installed 'common-lisp) (ts-ensure-installed 'cl-format))
    (setq ts-primary-parser (ts-parser-create 'common-lisp))
    (setq ts-font-lock-settings (cl-ts-mode--font-lock-rules))
    (setq ts-font-lock-feature-list cl-ts-mode-font-lock-feature-list)
    (setq-local ts-thing-settings cl-ts-mode-thing-settings)
    ;; these treesit functions don't work right, TODO: look into it. i think
    ;; somebody already mentioned it on emacs-devel.
    (cl-macrolet ((with-saved-vars (vars &body body)
                    (let ((binds ())
                          (restore ()))
                      (dolist (var vars)
                        (let ((s (make-symbol (format "%s-was-local" var))))
                          (push `(,s (prog1 (local-variable-p ',var)
                                       (make-local-variable ',var)))
                                binds)
                          (push `(,var ,var) binds)
                          (push `(unless ,s (kill-local-variable ',var)) restore)))
                      `(let* (,@(nreverse binds))
                         ,@body
                         ,@(nreverse restore)))))
      (with-saved-vars
       (up-list-function
        forward-list-function
        forward-sexp-function
        ;; this one seems to be broken in some other languages too
        forward-comment-function)
       (ts-major-mode-setup)))
    (add-function :around (local 'indent-region-function)
                  #'cl-ts-mode-indent-region-wrapper)
    (add-function :around (local 'indent-line-function)
                  #'cl-ts-mode-indent-line-wrapper)
    (setq-local syntax-propertize-function #'cl-ts-mode-syntax-propertize)
    ;; (make-local-variable 'up-list-function)
    ;; (make-local-variable 'forward-list-function)
    ;; (make-local-variable 'forward-sexp-function)
    ;; (let* ((up-list-function up-list-function))
    ;;   (ts-major-mode-setup))
    ;; (kill-local-variable 'up-list-function)
    ;; (setq-local forward-sexp-function #'treesit-forward-sexp)
    (add-hook 'font-lock-extend-region-functions
              #'cl-ts-mode--extend-fl-region nil t)
    ;; might as well, even if we've already computed it somebody may have decided
    ;; to change their `treesit-font-lock-level'
    (setq-default cl-ts-mode--enabled-fl-features
                  (cl-ts-mode--compute-features))))

(derived-mode-add-parents 'cl-ts-mode '(lisp-mode))

(provide 'cl-ts-mode)
;;; cl-ts-mode.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("ts-" . "treesit-"))
;; End:

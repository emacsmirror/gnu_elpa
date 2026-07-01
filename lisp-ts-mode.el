;;; lisp-ts-mode.el --- lisp-mode with tree-sitter support -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: zach shaftel <zach@shaf.tel>
;; Maintainer: zach shaftel <zach@shaf.tel>
;; Created: May 14, 2026
;; Version: 0.2.0
;; Keywords: lisp, languages, tree-sitter
;; URL: https://codeberg.org/zshaftel/lisp-ts-mode
;; Package-Requires: ((emacs "30.2") cond-star (compat "31"))

;; This program is free software: you can redistribute it and/or modify
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

;;; TODO:

;; - i would love to support "comments" in format strings, of the form
;;     ~0[~;comment~]
;;   we could make `comment-line' insert this directive when point is in a
;;   format string, and use `font-lock-comment-face' on the whole directive

;;; Code:

(require 'treesit)
(eval-when-compile
  (require 'subr-x)
  (require 'cond-star)
  (require 'cl-lib))

(ts-declare-unavailable-functions)

(add-to-list 'ts-language-source-alist
             '(common-lisp
               "https://codeberg.org/zshaftel/tree-sitter-cl-syntax"
               :commit "13ed8fe10e336a55f88b714ddd5a5c21f382232e"
               :source-dir "grammars/cl/src")
             t)

(add-to-list 'ts-language-source-alist
             '(cl-format
               "https://codeberg.org/zshaftel/tree-sitter-cl-syntax"
               :commit "13ed8fe10e336a55f88b714ddd5a5c21f382232e"
               :source-dir "grammars/format/src")
             t)

(when (boundp 'ts-major-mode-remap-alist)
  (add-to-list 'ts-major-mode-remap-alist '(lisp-mode . lisp-ts-mode)))

(defgroup lisp-ts-mode ()
  "Common Lisp major-mode powered by tree-sitter."
  :group 'lisp)

;; most of the inherited faces are arbitrary, it's just to keep the faces
;; consistent with the current theme's color palette.
(defface lisp-ts-mode-format-tilde
  '((t :inherit font-lock-regexp-grouping-backslash))
  "Face for the ~ in format directives.")

(defface lisp-ts-mode-format-char-parameter
  '((t :inherit font-lock-delimiter-face))
  "Face for character parameters to format directives.")

(defface lisp-ts-mode-format-numeric-parameter
  '((t :inherit font-lock-number-face))
  "For for numeric parameters to format directives.")

(defface lisp-ts-mode-format-arg-parameter
  '((t :inherit lisp-ts-mode-format-char-parameter))
  "For V/v parameters to format directives.")

(defface lisp-ts-mode-format-remaining-parameter
  '((t :inherit lisp-ts-mode-format-numeric-parameter))
  "For # parameters to format directives.")

(defface lisp-ts-mode-format-comma '((t :inherit font-lock-punctuation-face))
  "Face for commas that separate parameters to format directives.")

(defface lisp-ts-mode-format-at '((t :inherit font-lock-function-name-face))
  "Face for @ modifiers in format directives.")

(defface lisp-ts-mode-format-colon '((t :inherit font-lock-builtin-face))
  "Face for : modifiers in format directives.")

(defface lisp-ts-mode-format-standalone-directive
  '((t :inherit font-lock-property-use-face))
  "Face for simple format directive characters like A and S.")

(defface lisp-ts-mode-format-paired-directive
  '((t :weight extra-bold
       :inherit font-lock-regexp-grouping-construct))
  "Face used to highlight paired format directives: ~<~>, ~{~}, ~(~), and ~[~].
If `lisp-ts-mode-format-rainbow-delimiters' is non-nil, this face is
merged with the rainbow delimiters face (the latter taking precedence).")

(defface lisp-ts-mode-format-skipped-whitespace
  '((default :inherit font-lock-comment-face :weight bold)
    (((supports :underline (:color foreground-color :style dashes)))
     :underline (:color foreground-color :style dashes))
    (((supports :underline (:color foreground-color :style dots)))
     :underline (:color foreground-color :style dots))
    (((supports :strike-through t)) :strike-through t)
    (t :underline t))
  "Face placed over all whitespace ignored by the ~<newline> format directive.")

(defface lisp-ts-mode-block-comment-depth-1 '((t :inherit font-lock-comment-face))
  "Face used to highlight block comments nested by 1 level.
Top level block comments are highlighted with `font-lock-comment-face'.")

(defface lisp-ts-mode-block-comment-depth-2
  '((t :inherit lisp-ts-mode-block-comment-depth-1))
  "Face used to highlight block comments nested by 2 levels.")

(defface lisp-ts-mode-block-comment-depth-3
  '((t :inherit lisp-ts-mode-block-comment-depth-2))
  "Face used to highlight block comments nested by 3 levels.")

(defface lisp-ts-mode-character-escape
  '((t :weight bold :inherit font-lock-escape-face))
  "Face for #\\ in character literals.")

(defface lisp-ts-mode-character-name
  '((t :inherit font-lock-builtin-face))
  "Face for the names of characters in character literals.")

(defface lisp-ts-mode-positive-read-conditional
  '((t :weight extra-bold :inherit success))
  "Face used for #+.")

(defface lisp-ts-mode-negative-read-conditional
  '((t :weight extra-bold :inherit error))
  "Face used for #-.")

(defface lisp-ts-mode-read-eval
  '((t :weight extra-bold :inherit font-lock-misc-punctuation-face))
  "Face used for #..")

(defface lisp-ts-mode-quote
  '((t :weight extra-bold :inherit font-lock-preprocessor-face))
  "Face for \\='.")

(defface lisp-ts-mode-sharpquote
  '((t :weight extra-bold :inherit font-lock-constant-face))
  "Face for #\\='.")

(defface lisp-ts-mode-quasiquote
  '((t :weight extra-bold :inherit font-lock-string-face))
  "Face for \\=`.")

(defface lisp-ts-mode-comma
  '((t :weight extra-bold :inherit font-lock-property-use-face))
  "Face for , (unquote).")

(defface lisp-ts-mode-comma-at
  '((t :weight extra-bold
       :inherit (font-lock-string-face lisp-ts-mode-comma)))
  "Face for ,@ (unquote splice).")

(defface lisp-ts-mode-comma-dot
  '((t :weight extra-bold
       :inherit (font-lock-warning-face lisp-ts-mode-comma)))
  "Face for ,. (unquote nconc).")

(defconst lisp-ts-mode--block-comment-faces
  [font-lock-comment-face
   lisp-ts-mode-block-comment-depth-1
   lisp-ts-mode-block-comment-depth-2
   lisp-ts-mode-block-comment-depth-3])

(defcustom lisp-ts-mode-comment-darken-percentage 15.0
  "Used as the second argument to `color-darken-name' in
`lisp-ts-mode-update-comment-faces'."
  :type 'number)

(declare-function color-darken-name "color")

;;;###autoload
(defun lisp-ts-mode-update-comment-faces (&optional theme)
  "A function to update the block comment faces by increasing darkness.
Meant to be added to `enable-theme-functions'. The darkening ratio is
controlled by `lisp-ts-mode-comment-darken-percentage', or by THEME's
`lisp-ts-mode-comment-darken-percentage' symbol property if that's
non-nil."
  (let* ((comment-fore (face-attribute 'font-lock-comment-face :foreground nil t))
         ;; FIXME: should we use the theme-plist prop instead? that isn't really
         ;; a user facing api so probly not, but it feels more appropriate
         (percentage (or (and theme (get theme 'lisp-ts-mode-comment-darken-percentage))
                         lisp-ts-mode-comment-darken-percentage)))
    (when (stringp comment-fore)
      (require 'color)
      (dotimes (i 3)
        (thread-last (* percentage (1+ i))
          (color-darken-name comment-fore)
          (set-face-attribute (aref lisp-ts-mode--block-comment-faces (1+ i))
                              nil :foreground))))))

(defcustom lisp-ts-mode-format-rainbow-delimiters (featurep 'rainbow-delimiters)
  "Whether rainbow-delimiters.el faces are used in format string fontification.
When non-nil, `rainbow-delimiters-pick-face-function' is called to
select a face to apply to paired directive characters based on nesting
depth. If this is non-nil and rainbow-delimiters is not available, a
warning will be emitted and it will be set to nil."
  :type 'boolean)

(defun lisp-ts-mode--format-use-rainbow-delimiters-p ()
  "Return non-nil if rainbow-delimiters faces should be use for format directives.
This depends on the value of `lisp-ts-mode-format-rainbow-delimiters'.
If rainbow delimiters isn't available, set it to nil and emit a warning."
  (and lisp-ts-mode-format-rainbow-delimiters
       (or (boundp 'rainbow-delimiters-pick-face-function)
           (require 'rainbow-delimiters nil t)
           (ignore
            (lwarn 'lisp-ts-mode :warning
                   (concat "rainbow-delimiters.el is not available, "
                           "disabling `lisp-ts-mode-format-rainbow-delimiters'"))
            (setq-default lisp-ts-mode-format-rainbow-delimiters nil)))))

;; FIXME: should this be a defcustom? this is really just for `gaudy-cl-mode' to
;; hook into `lisp-ts-mode--fontify-one-format-directive'.
(defvar lisp-ts-mode-fontify-format-funcall-function ;say that three times fast!
  nil
  "A function called on the function nodes in ~/function/ FORMAT directives.
It can either return a face which will be added to the node, or add the
face itself and return nil.")

(defun lisp-ts-mode--fontify-one-format-directive (node &optional paired-depth mismatch-p)
  "Fontify the format_directive treesit NODE.

PAIRED-DEPTH is the current nesting depth of paired directives
containing NODE. MISMATCH-P is non-nil if NODE is a paired directive
without a corresponding opener/closer."
  (defvar rainbow-delimiters-pick-face-function)
  (let ((child (ts-node-child node 0))
        (colonp nil)
        (atp nil))
    (while child
      (when-let* ((face (pcase (ts-node-type child)
                          ("~" 'lisp-ts-mode-format-tilde)
                          ;; "v" is aliased to "V" in the grammar
                          ("V" 'lisp-ts-mode-format-arg-parameter)
                          ("#" 'lisp-ts-mode-format-remaining-parameter)
                          ("char_parameter" 'lisp-ts-mode-format-char-parameter)
                          ("numeric_parameter" 'lisp-ts-mode-format-numeric-parameter)
                          ("," 'lisp-ts-mode-format-comma)
                          ("@"
                           (setq atp t)
                           'lisp-ts-mode-format-at)
                          (":"
                           (setq colonp t)
                           'lisp-ts-mode-format-colon)
                          ("directive_character"
                           (cond*
                             ((eq (char-after (ts-node-start child)) ?\n)
                              (save-excursion
                                (goto-char (ts-node-start child))
                                (when atp (forward-char)) ;first newline kept
                                (let ((start (point)))
                                  (unless colonp ; only the newline was skipped
                                    (skip-chars-forward "\n\t "))
                                  (add-face-text-property
                                   start (point)
                                   'lisp-ts-mode-format-skipped-whitespace)))
                              nil)
                             ((not paired-depth)
                              'lisp-ts-mode-format-standalone-directive)
                             ((not (lisp-ts-mode--format-use-rainbow-delimiters-p))
                              'lisp-ts-mode-format-paired-directive)
                             ((bind-and* (rainbow-face
                                          (funcall rainbow-delimiters-pick-face-function
                                                   paired-depth (not mismatch-p)
                                                   (ts-node-start child))))
                              (list rainbow-face
                                    ;; put this behind it to merge the two faces
                                    'lisp-ts-mode-format-paired-directive))
                             (t 'lisp-ts-mode-format-paired-directive)))
                          ;; ~/
                          ("interned_symbol"
                           (when lisp-ts-mode-fontify-format-funcall-function
                             (funcall lisp-ts-mode-fontify-format-funcall-function child))))))
        (add-face-text-property (ts-node-start child) (ts-node-end child) face))
      (setq child (ts-node-next-sibling child)))))

(defun lisp-ts-mode--fontify-format-directives (node depth &optional stop)
  "Fontify the format_directive nodes starting with NODE.

Fontification is applied to NODE itself and its next siblings. DEPTH is
the nesting depth of paired directives containing NODE. STOP is a
sibling of NODE where iteration will terminate. If STOP is nil, all of
its siblings will be fontified."
  ;; "~&~<~;~{~^~}~:>"
  ;; should have a tree like (simplified):
  ;; (format_string
  ;;   (format_directive `~&`)
  ;;   (format_group
  ;;     start: (format_directive `~<`)
  ;;     (format_directive `~;`)
  ;;     (format_group
  ;;       start: (format_directive `~{`)
  ;;       (format_directive `~^`)
  ;;       end: (format_directive `~}`))
  ;;     end: (format_directive `~:>`)))
  (while (not (equal node stop))
    (pcase (ts-node-type node)
      ("format_directive" (lisp-ts-mode--fontify-one-format-directive node))
      ("format_group"
       (let ((start (ts-node-child-by-field-name node "start"))
             (end (ts-node-child-by-field-name node "end")))
         (lisp-ts-mode--fontify-one-format-directive start (1+ depth))
         (lisp-ts-mode--fontify-format-directives (ts-node-next-sibling start)
                                                  (1+ depth) end)
         (lisp-ts-mode--fontify-one-format-directive end (1+ depth)))))
    (setq node (ts-node-next-sibling node))))

(defun lisp-ts-mode--fontify-format-string (node override start end &rest _)
  "Fontify the format_string treesit NODE.
See `treesit-font-lock-settings' for the meaning of the remaining
arguments."
  (ignore override)
  (and (>= (ts-node-start node) start)
       (<= (ts-node-end node) end)
       (lisp-ts-mode--fontify-format-directives (ts-node-child node 1) 0)))

(defun lisp-ts-mode--fontify-nested-comments (node depth)
  "Fontify the block comment treesit NODE and its children.
DEPTH is the current depth of comment nesting; 0 if NODE is the top
level comment."
  (let ((child (ts-node-child node 0)))
    (while child
      (let ((type (ts-node-type child)))
        (cond
          ((string= type "nested_comment")
           (lisp-ts-mode--fontify-nested-comments child (1+ depth)))
          ((member type '("#|" "|#"))
           (let ((beg (ts-node-start child))
                 (end (ts-node-end child)))
             (add-face-text-property beg end 'font-lock-comment-delimiter-face)))))
      (setq child (ts-node-next-sibling child)))
    (add-face-text-property (ts-node-start node)
                            (ts-node-end node)
                            (aref lisp-ts-mode--block-comment-faces (min depth 3))
                            ;; append to put it behind
                            ;; `font-lock-comment-delimiter-face'
                            t)))

(defun lisp-ts-mode--fontify-comment (node override start end &rest _)
  "Fontify the block_comment or line_comment treesit NODE.
See `treesit-font-lock-settings' for the meaning of the remaining
arguments."
  (ignore override)
  (let ((nbeg (ts-node-start node))
        (nend (ts-node-end node)))
    (when (and (>= nbeg start) (<= nend end))
      (pcase (ts-node-type node)
        ("line_comment"
         (goto-char nbeg)
         (add-face-text-property (prog1 (point) (skip-chars-forward ";" nend))
                                 (point) 'font-lock-comment-delimiter-face)
         (add-face-text-property (point) nend 'font-lock-comment-face))
        ("block_comment" (lisp-ts-mode--fontify-nested-comments node 0))))))

(defface lisp-ts-mode-0-bit '((t :inherit success))
  "Face for 0s in #b0 rationals and #*0 bit vectors.")

(defface lisp-ts-mode-1-bit '((t :inherit error))
  "Face for 1s in #b1 rationals and #*1 bit vectors.")

(defun lisp-ts-mode--fontify-bits (node override start end &rest _)
  "Fontify 0s and 1s in the rational or bit_vector treesit NODE.
See `treesit-font-lock-settings' for the meaning of the remaining
arguments. The faces `lisp-ts-mode-0-bit' and `lisp-ts-mode-1-bit' are
applied to 0s and 1s respectively. For rationals, the faces are only
applied if the number uses the read syntax #b1 or #2r10."
  (ignore override)
  (when (and (>= (ts-node-start node) start) (<= (ts-node-end node) end))
    (let* ((node-type (ts-node-type node))
           (node-end (ts-node-end node))
           (ratp (equal node-type "rational"))
           (chars ["0" "1"])
           (faces [lisp-ts-mode-0-bit lisp-ts-mode-1-bit]))
      (goto-char (ts-node-start node))
      (when (cond
              (ratp (looking-at (rx "#" (or (any "Bb")
                                            (seq (* "0") "2" (any "Rr"))))))
              ((equal node-type "bit_vector")
               (looking-at (rx "#" (* (any "0-9")) "*"))))
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

(defun lisp-ts-mode--fontify-character (node override start end &rest _)
  "Fontify character treesit NODE.
See `treesit-font-lock-settings' for the meaning of the remaining
arguments. The #\\ is given the face `lisp-ts-mode-character-escape',
and the character's name is given the face
`lisp-ts-mode-character-name'."
  (ignore override)
  (when (and (>= (ts-node-start node) start) (<= (ts-node-end node) end))
    (let ((backslash-end (ts-node-end (ts-node-child node 0))))
      (add-face-text-property (ts-node-start node) backslash-end
                              'lisp-ts-mode-character-escape)
      (add-face-text-property backslash-end (ts-node-end node)
                              'lisp-ts-mode-character-name))))

;; as of now this could just be a `defconst', but we might wanna make certain
;; rules like format-directive conditional, so let's keep it a function for
;; forward compatibility.
(defun lisp-ts-mode--font-lock-rules ()
  "Return font-lock rules used for `treesit-font-lock-settings' in `lisp-ts-mode'."
  (ts-font-lock-rules
   ;; this rule will only run if explicitly triggered by `gaudy-cl-mode' or when
   ;; `lisp-ts-format-support-mode' is enabled
   :language 'cl-format
   :feature 'format-directive
   :override 'prepend
   `((format_string) @lisp-ts-mode--fontify-format-string)
   :default-language 'common-lisp
   :feature 'string
   :override 'append
   `((string) @font-lock-string-face)
   :feature 'comment
   `([(line_comment) (block_comment)] @lisp-ts-mode--fontify-comment)
   :feature 'number
   :override 'prepend
   ;; complexes will have their whole and imaginary parts get the face
   ;; separately. highlighting the entire expression #C(1/3 99.1) with
   ;; font-lock-number-face seems weird to me.
   `((real) @font-lock-number-face)
   :feature 'bits
   :override 'prepend
   `([(bit_vector) (rational)] @lisp-ts-mode--fontify-bits)
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
   ;; these next 3 should probably be merged right?
   :feature 'character
   :override 'prepend
   ;; the character's name doesn't create a node so we have to use a function to
   ;; apply the two faces
   `((character) @lisp-ts-mode--fontify-character)
   :feature 'quote
   :override 'prepend
   `((quote      "'"  @lisp-ts-mode-quote)
     (sharpquote "#'" @lisp-ts-mode-sharpquote)
     (quasiquote "`"  @lisp-ts-mode-quasiquote)
     (unquote   [","  @lisp-ts-mode-comma
                 ",@" @lisp-ts-mode-comma-at
                 ",." @lisp-ts-mode-comma-dot]))
   :feature 'reader-macro
   :override 'prepend
   `(["#." @lisp-ts-mode-read-eval
      "#+" @lisp-ts-mode-positive-read-conditional
      "#-" @lisp-ts-mode-negative-read-conditional])))

(defconst lisp-ts-mode--font-lock-feature-list
  '((string comment)
    (number)
    (format-directive symbol)
    (quote reader-macro character bits))
  "`lisp-ts-mode' settings for `treesit-font-lock-feature-list'.")

;; unfortunately the generic treesit implementations of some commands like
;; `forward-sexp' and `up-list' don't seem to play nice with this mode, so we
;; reinvent the wheeel a bit. but FIXME: try to get the native treesit ones to
;; work

;; we need this instead of `treesit-thing-at' to catch nodes directly preceding
;; POS
(defun lisp-ts-mode--thing-node-at-pos (thing &optional pos not-before)
  "Return the node at POS that matches the predicate THING.
See `treesit-thing-settings' for what forms THING can take. POS defaults
to `point'. This function will also consider nodes directly preceding
POS (meaning the node ends *at* POS), unless NOT-BEFORE is non-nil."
  (let* ((root (ts-parser-root-node ts-primary-parser))
         (pos (or pos (point)))
         (node root)
         (curpos pos)
         (last-at-pos nil)
         next)
    (while (or (and (setq next (ts-node-first-child-for-pos node curpos t))
                    (<= (ts-node-start next) pos (ts-node-end next)))
               ;; if point is right after a sexp, return that one
               (and (not not-before) (eq curpos pos)
                    (setq last-at-pos node
                          next
                          (or (ts-node-first-child-for-pos node (decf curpos) t)
                              ;; if `last-at-pos' is gonna be `eq' to node,
                              ;; don't let `treesit-parent-until' traverse
                              ;; the same node to the root twice
                              (setq node nil)))
                    (= (ts-node-end next) pos)))
      (setq node next))
    (or (ts-parent-until node thing t)
        (ts-parent-until last-at-pos thing t))))

(define-inline lisp-ts-mode--sexp-node-at-pos (&optional pos not-before)
  "Return the sexp treesit node at POS.
NOT-BEFORE is the same as in `lisp-ts-mode--thing-node-at-pos'. The
definition of `sexp' is based on its entry in
`lisp-ts-mode-thing-settings'."
  (inline-quote (lisp-ts-mode--thing-node-at-pos 'sexp ,pos ,not-before)))

;; treesit-parsers-at and co can only find the ones with overlays added by
;; `treesit-range-settings', which `gaudy-cl-mode' has to bypass
(defun lisp-ts-mode--find-parser-at (pos parser-list)
  "Find the first treesit parser in PARSER-LIST whose range contains POS."
  (car (any (lambda (parser)
              (any (lambda (range)
                     (<= (car range) pos (cdr range)))
                   (ts-parser-included-ranges parser)))
            parser-list)))

(defun lisp-ts-mode--parsers-in-region (parser-list beg end)
  "Filter PARSER-LIST to those intersecting with the range BEG..END.
The value is a list of lists of the form (LOW HIGH PARSER), where LOW
and HIGH are the boundaries of PARSER's range that intersect with the
range BEG..END. There may be multiple entries for PARSER if it has more
than one included range. The returned list is sorted in ascending order
by LOW."
  (let* ((intersection ()))
    (dolist (parser parser-list)
      (pcase-dolist (`(,lo . ,hi) (ts-parser-included-ranges parser))
        (let ((range-lo (max lo beg))
              (range-hi (min end hi)))
          (when (< range-lo range-hi)
            (push (list range-lo range-hi parser) intersection)))))
    (sort intersection :key #'car :in-place t)))

(defun lisp-ts-mode--parsers-strictly-in-region (parser-list beg end)
  "Filter PARSER-LIST to those whose ranges fall strictly between BEG and END."
  (let* ((filtered ()))
    (dolist (parser parser-list)
      (when (all (lambda (range)
                   (<= beg (car range) (cdr range) end))
                 (ts-parser-included-ranges parser))
        (push parser filtered)))
    (nreverse filtered)))

(defun lisp-ts-mode--format-directive-at-pos (node pos)
  "Find the format_directive descendent of NODE at POS.
This will only consider descendants of NODE, not NODE itself, because
NODE is expected to be the parser's root node."
  (let ((child (ts-node-first-child-for-pos node pos)))
    (pcase child
      ('nil nil)
      ((app ts-node-type "format_directive") child)
      ((app ts-node-type "format_group")
       (lisp-ts-mode--format-directive-at-pos child pos)))))

(defcustom lisp-ts-mode-format-indent-auto-escape-eol "~@"
  "Whether a ~<newline> directive is inserted when indenting format strings.

If non-nil, when indenting a format directive after a plain newline, a
string is automatically added to the end of the preceding line escaping
the newline so that the directive can be indented without affecting the
whitespace of the output."
  :type
  '(choice (const :tag "Don't indent if the newline isn't escaped" nil)
           (const :tag "Always indent regardless of whether it's escaped" t)
           (string :tag "Always add a specific string" :default "~@")
           (cons :tag "Choose based on context"
                 (string :tag "String used inside a ~<~:> directive"
                         :default "~:@_~")
                 (string :tag "String used anywhere else" :default "~@"))))

(defcustom lisp-ts-mode-format-indent-predicate
  (rx bos "format_group" eos)
  "A predicate to determine where indentation should occur within format strings.
Set this to nil to disable format string indentation entirely.

When a node matches this predicate, any text *within* it is indented
relative to it. For format groups, indent relative to the opening
delimiter + `lisp-ts-mode-format-group-indent-offset' (see
`lisp-ts-mode-format-indent-tilde-relative'); if the predicate matches the
entire format string, indent relative to the opening \" +
`lisp-ts-mode-format-string-indent-offset'."
  :type '(choice (const :tag "Never indent inside format strings" nil)
                 (const :tag "Any format group or the entire format string"
                        "\\`format_\\(?:group\\|string\\)\\'")
                 (function :tag "A function to match nodes")
                 (regexp :tag "A regexp matching the node's type"
                         :default "\\`format_group\\'")))

(defcustom lisp-ts-mode-format-indent-tilde-relative nil
  "Determines which column is used as a basis for format string indentation.

If non-nil, indentation is performed relative to the ~ of the starting
directive; otherwise it's relative to the directive character itself ({,
[, ( or <). This affects both the contents of the paired directive, and
the indentation of the closing directive (}, ], ) or >) relative to the
opener when the closer's ~ is the first non whitespace character on a
line. Example (with `lisp-ts-mode-format-group-indent-offset' = 1):

nil:
~:@{~
    ~A~
  ~}

non-nil:
~:@{~
 ~A~
~}"
  :type 'boolean)

(defcustom lisp-ts-mode-format-group-indent-offset 1
  "Additional columns of indentation when indenting inside paired directives.
This is calculated relative to the opening directive (~<, ~(, ~[ or ~{).
Example (with `lisp-ts-mode-format-indent-tilde-relative' = nil):

With value of 2:
  ~{
     ~A
  ~}
With a value of 0:
  ~{
   ~A
  ~}"
  :type 'integer)

(defcustom lisp-ts-mode-format-string-indent-offset 1
  ;; when adjusting the example, remember the \ shifts the " right by one
  "Additional columns of indentation relative to the start of the format string.
This is only used if string-relative indentation is enabled by the value
of `lisp-ts-mode-format-indent-predicate'. Example:

With value of 2:
  \"
    ~A\"
With value of 0:
  \"
  ~A\"

You can set this to the value of `most-negative-fixnum' to always indent
non-nested directives to the start of the line."
  :type `(choice (const :tag "Always flush to the leftmost column"
                        ,most-negative-fixnum)
                 (integer :tag "Offset from string quote")))

(defconst lisp-ts-mode--format-pprint-logical-block-query
  (when (fboundp 'ts-query-compile)
    (ts-query-compile 'cl-format
                      '((format_group
                         end: (format_directive ":" ((directive_character) @cap
                                                     (:eq? @cap ">")))))))
  "A treesit query matching ~<~:> format_group nodes.")

(defun lisp-ts-mode--eol-escape-string-at (parser pos)
  "Return the appropriate string to place at the end of the line preceding POS.
PARSER is a `cl-format' treesit parser who range contains POS. If a
~<newline> directive is already present, or if
`lisp-ts-mode-format-indent-auto-escape-eol' is t, return nil. If there
is no ~<newline> directive present and
`lisp-ts-mode-format-indent-auto-escape-eol' is nil, return the symbol
`noindent'. Otherwise the returned string depends on the value of
`lisp-ts-mode-format-indent-auto-escape-eol' and whether POS is
positioned inside a logical-block format directive (~<~:>)."
  (let ((node (lisp-ts-mode--format-directive-at-pos
               (ts-parser-root-node parser)
               (1- pos))))
    (cond
      ((and node (= (ts-node-end node) pos))
       ;; ends at bol so it has to be a newline directive
       nil)
      ((stringp lisp-ts-mode-format-indent-auto-escape-eol)
       lisp-ts-mode-format-indent-auto-escape-eol)
      ((null lisp-ts-mode-format-indent-auto-escape-eol) 'noindent)
      ((atom lisp-ts-mode-format-indent-auto-escape-eol) nil)
      ((ts-query-capture
        parser
        lisp-ts-mode--format-pprint-logical-block-query
        pos (1+ pos) t)
       (car lisp-ts-mode-format-indent-auto-escape-eol))
      (t (cdr lisp-ts-mode-format-indent-auto-escape-eol)))))

(defun lisp-ts-mode--indent-format-line (parser)
  "Indent the current line, which should start within the range of PARSER.
PARSER is a `cl-format' treesit parser. If there is nothing to indent,
return the symbol `noindent'. If the line is indented or was already
indented, call `back-to-indentation' before returning, but if
indentation is disabled here due to the value of
`lisp-ts-mode-format-indent-predicate', `point' will not move."
  (let* ((lbeg (line-beginning-position))
         (root (ts-parser-root-node parser))
         (nearest-node (ts-node-descendant-for-range root lbeg lbeg))
         (pred lisp-ts-mode-format-indent-predicate)
         (pred (if (and (symbolp pred)
                        (not (ts-thing-defined-p pred 'cl-format)))
                   (indirect-function pred)
                 pred))
         (parent (ts-parent-until nearest-node pred t)))
    ;; if point is at the start of the format group, we'll get the starting
    ;; directive, so get its parent
    (and parent (= (ts-node-start parent)
                   (save-excursion (back-to-indentation) (point)))
         (setq parent (ts-parent-until parent pred)))
    (if (not (and parent (<= (ts-node-start parent)
                             lbeg
                             (ts-node-end parent))))
        'noindent
      (let* ((starter (ts-node-child-by-field-name parent "start"))
             (ender (ts-node-child-by-field-name parent "end"))
             (cur-indent (current-indentation))
             (new-indent
              (save-excursion
                (max
                 0
                 (cond*
                   ((null starter)
                    ;; whole string
                    (goto-char (ts-node-start parent))
                    (+ (current-column) lisp-ts-mode-format-string-indent-offset))
                   (t (back-to-indentation)) ;non exit clause
                   ((/= (ts-node-start ender) (point))
                    ;; not indenting the closing directive so indent relative to
                    ;; the opener
                    (goto-char (if lisp-ts-mode-format-indent-tilde-relative
                                   (ts-node-start starter)
                                 (1- (ts-node-end starter))))
                    (+ (current-column) lisp-ts-mode-format-group-indent-offset))
                   ((not lisp-ts-mode-format-indent-tilde-relative)
                    (goto-char (ts-node-end starter))
                    (- (current-column)
                       (- (ts-node-end ender) (ts-node-start ender))))
                   (t (goto-char (ts-node-start starter))
                      (current-column))))))
             cont)
        (prog1 (if (or (= new-indent cur-indent)
                       ;; this means `lisp-ts-mode-format-indent-auto-escape-eol'
                       ;; is nil and there's no directive already there
                       (eq (setq cont (lisp-ts-mode--eol-escape-string-at parser lbeg))
                           'noindent))
                   'noindent
                 (let* ((old-ranges (ts-parser-included-ranges parser))
                        (_ (cl-assert (length= old-ranges 1)))
                        (pbeg (caar old-ranges))
                        (pend (copy-marker (cdar old-ranges) t)))
                   (indent-line-to new-indent)
                   (when cont
                     (save-excursion
                       (goto-char (1- lbeg))
                       (insert cont)))
                   (thread-last (prog1 (marker-position pend)
                                  (set-marker pend nil))
                     (cons pbeg)
                     (list)
                     (ts-parser-set-included-ranges parser)))
                 nil)
          (back-to-indentation))))))

;; i use this to bind `lisp-ts-mode-format-indent-auto-escape-eol' to plain "~"
;; when `this-command' = `newline-and-indent'.
(defvar lisp-ts-mode-format-indent-function #'lisp-ts-mode--indent-format-line
  "The function called to perform format directive indentation.
It's called with one argument, the treesit parser for the format string,
with point positioned on the line to be indented. If it performs no
indentation, it should return the symbol `noindent'.

`add-function' is meant to be used on this variable to control when and
how indentation is performed.")

(defun lisp-ts-mode-maybe-indent-format-line (&optional parser)
  "Possibly indent the current line if it's within a format string.
If no indentation is performed, return the symbol `noindent'. PARSER is
a `cl-format' treesit parser; if not supplied or nil, one is searched
for at point with `lisp-ts-mode--find-parser-at'."
  (if-let* ((_ lisp-ts-mode-format-indent-predicate)
            (parser-at (or parser (thread-last (ts-parser-list nil 'cl-format t)
                                    (lisp-ts-mode--find-parser-at (point)))))
            (_ (> (line-beginning-position)
                  (1+ (ts-node-start (ts-parser-root-node parser-at))))))
      (funcall lisp-ts-mode-format-indent-function parser-at)
    'noindent))

;; indent-sexp doesn't use indent-region, and instead tries to be smart and
;; skips strings.
(defun lisp-ts-mode-indent-sexp (&optional endpos)
  "A version of `indent-sexp' that forwards to `indent-region'.

`indent-sexp' is remapped to this command in `lisp-ts-mode' so that it
triggers FORMAT string indentation. ENDPOS, if supplied, is the position
where indentation stops, defaulting to the end of the sexp."
  (interactive)
  (let ((inhibit-message t))           ;indent-region is loud, indent-sexp isn't
    (indent-region (save-excursion (backward-prefix-chars) (point))
                   (or endpos (save-excursion (forward-sexp) (point))))))

(defun lisp-ts-mode-indent-line-wrapper (orig)
  "Used in `lisp-ts-mode' as `:around' advice on `indent-line-function'."
  (let ((lindent (lisp-ts-mode-maybe-indent-format-line)))
    (if (eq lindent 'noindent)
        (funcall orig)
      lindent)))

(defun lisp-ts-mode-up-list (arg escape-strings no-syntax-crossing)
  "Used as `up-list-function' in `lisp-ts-mode'."
  (cond
    ((not no-syntax-crossing)
     ;; FIXME handle format directives!
     (up-list-default-function arg escape-strings no-syntax-crossing))
    (t (let* ((pred '(or "\\`string\\'" list))
              (node (lisp-ts-mode--thing-node-at-pos pred))
              (backwards-p (minusp arg))
              (arg (truncate (abs arg)))
              (parent nil))
         (unless (or (= (ts-node-start node) (point))
                     (= (ts-node-end node) (point)))
           (and (equal (ts-node-type node) "string")
                (not escape-strings)
                (plusp arg)
                (error "At top level"))
           (decf arg))
         (while (and (plusp arg)
                     (setq parent (ts-parent-until node 'list)))
           (decf arg)
           (setq node parent))
         (goto-char (if backwards-p
                        (ts-node-start node)
                      (ts-node-end node)))))))

(defun lisp-ts-mode--extend-fl-region ()
  "Added to `font-lock-extend-region-functions' in `lisp-ts-mode'.

Prevents the font-lock region from starting or ending in the middle of
an expression."
  (defvar font-lock-beg)
  (defvar font-lock-end)
  (let* ((beg-sexp (lisp-ts-mode--sexp-node-at-pos font-lock-beg t))
         (change-beg (and beg-sexp
                          (< (ts-node-start beg-sexp)
                             font-lock-beg
                             (ts-node-end beg-sexp)))))
    (when change-beg
      (setq font-lock-beg (ts-node-start beg-sexp)))
    (or (cond*
          ((and beg-sexp (< (ts-node-start beg-sexp)
                            font-lock-end
                            (ts-node-end beg-sexp)))
           (setq font-lock-end (ts-node-end beg-sexp)))
          ((eq (ts-node-end beg-sexp) font-lock-end) nil)
          ((bind* (end-sexp (lisp-ts-mode--sexp-node-at-pos (1- font-lock-end) t))))
          ((and end-sexp (< (ts-node-start end-sexp)
                            font-lock-end
                            (ts-node-end end-sexp)))
           (setq font-lock-end (ts-node-end end-sexp))))
        change-beg)))

(defconst lisp-ts-mode--syntax-propertize-query
  (when (fboundp 'ts-query-compile)
    (ts-query-compile 'common-lisp
                      ;; giving #3r or #x prefix syntax doesn't seem appropriate
                      ;; to me, but should the # on those get symbol syntax?
                      '(([",@" ",." "#+" "#." "#-" "#:" "#C" "#P"] @prefix)
                        ;; FIXME vectors with explicit length
                        ([(bit_vector) (array)] @array)
                        ((multiple_escape) @escape))))
  "A query to match Lisp nodes needed by `lisp-ts-mode-syntax-propertize'.")

(defconst lisp-ts-mode--format-syntax-propertize-query
  (when (fboundp 'ts-query-compile)
    (ts-query-compile 'cl-format '((format_directive) @directive)))
  "A query to match all format directive nodes.
Used by `lisp-ts-mode-syntax-propertize'.")

;; this is neat but some users may not vibe with it. this syntax table is placed
;; on just the format directive characters. it gives the 4 paired directives the
;; appropriate delimiter syntax descriptor, gives / "paired delimiter" syntax
;; (not sure if this is the best choice), and all other characters symbol
;; syntax. the syntax table below is added to the rest of the format string,
;; which just gives it basic syntax (importantly, it gives the paired directive
;; characters symbol syntax so normal parenthesized text doesn't interfere).
;; lastly, we give the tilde, parameters and :@ modifiers a prefix syntax
;; descriptor. all this together means that commands like forward-sexp and
;; down-list will treat format directives as expressions, so eg. you can move
;; across or down into ~{~}, and `show-paren-mode' will highlight the whole
;; directive (which i find extremely useful).
(defconst lisp-ts-mode--format-directive-syntax-table
  (let* ((tab (make-syntax-table)))
    (modify-syntax-entry (cons 0 (max-char)) "_" tab) ;symbol syntax by default
    (modify-syntax-entry ?/ "$" tab)                  ;paired delimiter
    (dolist (leftright '("<>" "()" "[]" "{}"))
      (modify-syntax-entry (aref leftright 0)
                           (concat "(" (substring leftright 1))
                           tab)
      (modify-syntax-entry (aref leftright 1)
                           (concat ")" (substring leftright 0 1))
                           tab))
    tab)
  "Syntax table added via `lisp-ts-mode-syntax-propertize'.
This is applied to format directive characters, like the \"_\" in
\"~:@_\". In that directive, the \"~:@\" is given prefix syntax.")

(defconst lisp-ts-mode--format-string-syntax-table
  (let ((tab (make-syntax-table)))
    (mapc (lambda (ch) (modify-syntax-entry ch "_" tab)) "()[]{}<>")
    tab)
  "Syntax table applied to non-directive parts of format strings by
`lisp-ts-mode-syntax-propertize'.")

(defun lisp-ts-mode-syntax-propertize (start end)
  "Used as `syntax-propertize-function' in `lisp-ts-mode'.
START and END are the boundaries of the region to operate on. In Lisp
code, add prefix syntax to reader macros like #S, #+ etc. In format
strings, add parenthesis syntax to ~<~>, ~[~], ~(~) and ~{~} directives,
and prefix syntax on all characters from the ~ up to the directive
character."
  (pcase-dolist (`(,cap . ,node)
                 (ts-query-capture ts-primary-parser
                                   lisp-ts-mode--syntax-propertize-query
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
      ('array (and (>= (ts-node-start node) start)
                   (save-excursion
                     (goto-char (ts-node-start node))
                     (looking-at (rx "#" (* (any "0-9")) (any "Aa*"))))
                   (<= (match-end 0) end)
                   (put-text-property (match-beginning 0)
                                      (match-end 0)
                                      'syntax-table
                                      (string-to-syntax "'"))))
      ;; the lisp mode syntax table gives | string quote syntax, which is
      ;; annoying because movement commands treat it as a separate sexp when
      ;; scanning
      ('escape (and (>= (ts-node-start node) start)
                    (<= (ts-node-end node) end)
                    (put-text-property (ts-node-start node)
                                       (ts-node-end node)
                                       'syntax-table
                                       (string-to-syntax "_"))))))
  (ts-update-ranges start end)
  (dolist (fmt-parser (thread-first (ts-parser-list nil 'cl-format t)
                        (lisp-ts-mode--parsers-strictly-in-region start end)))
    (let ((root (ts-parser-root-node fmt-parser)))
      ;; FIXME: we're getting called on empty parsers, need to figure out what's
      ;; causing that
      (when (> (1- (ts-node-end root)) (ts-node-start root))
        (put-text-property (1+ (ts-node-start root)) (1- (ts-node-end root))
                           'syntax-table
                           lisp-ts-mode--format-string-syntax-table)
        (dolist (node (ts-query-capture fmt-parser
                                        lisp-ts-mode--format-syntax-propertize-query
                                        start end t))
          (let* ((fn-node (ts-node-child-by-field-name node "function"))
                 (prefix-end (if fn-node
                                 (1- (ts-node-start fn-node))
                               (1- (ts-node-end node)))))
            (put-text-property (ts-node-start node) prefix-end
                               'syntax-table (string-to-syntax "'"))
            (put-text-property prefix-end (ts-node-end node)
                               'syntax-table
                               lisp-ts-mode--format-directive-syntax-table)))))))

(defvar-keymap lisp-ts-mode--mode-line-map
  "<mode-line> <mouse-1>" #'lisp-ts-mode-toggle-comment-style)

(defun lisp-ts-mode-update-modeline ()
  "Update the `lisp-ts-mode' mode name based on the current comment style."
  (setq mode-name
        (concat "Lisp/"
                (propertize comment-start
                            'face 'font-lock-comment-delimiter-face
                            'help-echo (concat "mouse-1: Switch to "
                                               (if (string-empty-p comment-end)
                                                   "#| block comments |#"
                                                 ";; line comments"))
                            'local-map lisp-ts-mode--mode-line-map)))
  (force-mode-line-update))

(defun lisp-ts-mode-toggle-comment-style (&optional arg)
  "Toggle between #| block |# and ;; line comments for comment commands.
With a positive prefix arg, enable block comments; with a negative
prefix arg, enable line comments; zero or unsupplied prefix argument
toggles between them.

When called from Lisp, ARG can be the symbol `block' to enable block
comments, the symbol `line' to enable line comments, or nil to toggle
between them."
  (interactive (cond
                 ((null current-prefix-arg) '(nil))
                 ;; same as c-toggle-comment-style
                 ((minusp (prefix-numeric-value current-prefix-arg)) '(line))
                 (t '(block)))
               lisp-mode                ;nothing ts specific in here so why not
               lisp-ts-mode)
  (unless arg
    (setq arg (if (string-empty-p comment-end) 'block 'line)))
  (if (eq arg 'line)
      (setq-local comment-start ";"
                  comment-end ""
                  comment-continue nil)
    (setq-local comment-start "#|"
                comment-end "|#"
                ;; i would much rather use " |" but it just doesn't handle it
                ;; correctly. sly-lisp-indent-function sometimes errors and when
                ;; it doesn't it ends up indenting like
                ;; #|
                ;; |
                ;; | |#
                ;; which looks terrible
                ;; so FIXME: block comment indentation
                comment-continue "# "))
  (message "Enabled %s %s comments %s" comment-start arg comment-end)
  (when (derived-mode-p 'lisp-ts-mode)
    (lisp-ts-mode-update-modeline)))

(defvar-keymap lisp-ts-mode-map
  :parent lisp-mode-map
  :doc "Keymap for `lisp-ts-mode'."
  "<remap> <indent-sexp>" #'lisp-ts-mode-indent-sexp
  ;; KLUDGE: would prefer C-c C-k to match the key bindings for
  ;; `c[-ts-mode]-toggle-comment-style' but sly (and probably slime) binds
  ;; compile-and-load-file to C-c C-k, which many (including me) have probably
  ;; grown accustomed to
  "C-c k" #'lisp-ts-mode-toggle-comment-style)

(defconst lisp-ts-mode-thing-settings
  `((common-lisp
     (sexp ,(rx bos (or "interned_symbol"
                        "uninterned_symbol"
                        "rational"
                        "float"
                        "complex"
                        "vector"
                        "array"
                        "string"
                        "bit_vector"
                        "list"
                        "character"
                        "quote"
                        "sharpquote"
                        "unquote"
                        "quasiquote"
                        "labelled"
                        "reference"
                        "struct"
                        "read_eval"
                        "read_conditional"
                        "pathname")
                eos))
     ;; i did some benchmarking, these simple patterns seem to give the best
     ;; performance. i figured a \' anchor was a little faster but it's not.
     (comment "_comment")
     (symbol "_symbol")
     ;; this is used by `treesit-major-mode-setup' to set things like
     ;; `forward-list-function', so we set it to everything "list-like"
     (list ,(rx bos (or "list" "vector" "array" "complex" "struct") eos)))
    (cl-format
     (directive "_directive")
     (modifier "[@:]")
     (parameter ,(rx bos (or "#" "V" "char_parameter" "numeric_parameter") eos))))
  "Used as the value of `treesit-thing-settings' in `lisp-ts-mode'.")

;;;###autoload
(defconst lisp-ts-mode-font-lock-ignore-keywords
  `(;; keyword and uninterned symbol highlighting, done by the `symbol' treesit
    ;; feature
    "#:a"
    (pred ,(lambda (kw)
             (and (functionp (car-safe kw))
                  (ignore-errors
                    (with-temp-buffer
                      (set-syntax-table lisp-mode-syntax-table)
                      (insert ":keyword")
                      (goto-char (point-min))
                      (funcall (car kw) (point-max))))))))
  "A recommended list of predicates to use in `font-lock-ignore'.
Example usage:

  (use-package lisp-ts-mode
    :config
    (setf (alist-get \\='lisp-ts-mode font-lock-ignore)
          lisp-ts-mode-font-lock-ignore-keywords))

This should be set before `lisp-ts-mode' is activated.")

;;;###autoload
(define-derived-mode lisp-ts-mode prog-mode "Lisp"
  "Common Lisp major mode with tree-sitter support."
  :after-hook (lisp-ts-mode-update-modeline)
  (lisp-mode-variables t t)
  ;; `lisp-indent-region' is more or less a version of
  ;; `indent-region-line-by-line' that avoids exponential complexity by keeping
  ;; a running ppss. that's nice, but it messes with our format indentation
  ;; stuff by bypassing `indent-line-function'. so TODO: a version of
  ;; `lisp-indent-region' that works with format stuff, but until then, we'll
  ;; stick with the generic brute force method.
  (kill-local-variable 'indent-region-function)
  ;; copied most of this from `lisp-mode'
  (setq-local lisp-indent-function #'common-lisp-indent-function)
  (setq-local comment-start-skip
              "\\(\\(^\\|[^\\\n]\\)\\(\\\\\\\\\\)*\\)\\(;+\\|#|\\) *")
  (setq-local comment-end-skip "[ \t]*\\(\\s>\\||#\\)")
  (setq-local font-lock-comment-end-skip "|#")
  (setq imenu-case-fold-search t)
  (setq-local lisp-fill-paragraphs-as-doc-string nil) ;specifically designed for elisp
  (setq-local comment-quote-nested nil)
  ;; FIXME: we should only prompt for the format grammar when turning on
  ;; `lisp-ts-format-support-mode'
  (when (and (ts-ensure-installed 'common-lisp) (ts-ensure-installed 'cl-format))
    (setq ts-primary-parser (ts-parser-create 'common-lisp))
    (setq ts-font-lock-settings (lisp-ts-mode--font-lock-rules))
    (setq ts-font-lock-feature-list lisp-ts-mode--font-lock-feature-list)
    (setq-local ts-thing-settings lisp-ts-mode-thing-settings)
    (ts-major-mode-setup)
    ;; the treesit versions of these functions don't work right, TODO: look into
    ;; it. i think somebody already mentioned it on emacs-devel.
    (kill-local-variable 'forward-list-function)
    (kill-local-variable 'forward-sexp-function)
    (kill-local-variable 'forward-comment-function)
    (setq font-lock-defaults
          '((lisp-cl-font-lock-keywords
             lisp-cl-font-lock-keywords-1
             lisp-cl-font-lock-keywords-2)
            nil t nil nil
            (font-lock-mark-block-function . mark-defun)
            (font-lock-extra-managed-props . (help-echo))
            (font-lock-fontify-syntactically-function
             . ts-font-lock-fontify-region)))
    (add-function :around (local 'indent-line-function)
                  #'lisp-ts-mode-indent-line-wrapper)
    (setq-local up-list-function #'lisp-ts-mode-up-list)
    (setq-local syntax-propertize-function #'lisp-ts-mode-syntax-propertize)
    (add-hook 'font-lock-extend-region-functions
              #'lisp-ts-mode--extend-fl-region nil t)))

;;;###autoload
(derived-mode-add-parents 'lisp-ts-mode '(lisp-mode))

(defalias 'common-lisp-ts-mode #'lisp-ts-mode)


;;; format grammar stuff, for those who don't use gaudy-cl-mode

;; it really seems like these heinously complex queries are the only exhaustive
;; way to do this, but i'd love to be wrong.
(defun lisp-ts-mode--arg-fields (indices)
  "Return a partial query matching specified patterns at INDICES.

The result starts with an :anchor, so it must be prepended with a
pattern that matches a node. INDICES is an alist of (INDEX . PATTERN)
where INDEX is the index of the argument and PATTERN is a list spliced
into the resulting query at INDEX. INDICES must already be in descending
order. Args at positions not in INDICES are given the wildcard
pattern (_). The resulting partial query will match the arguments in
sequence irrespective of intervening comments. For example, if INDICES
is ((2 . ((string) :?)) (0 . ((list)))) the result would be
  (:anchor (comment) :* :anchor (list)
   :anchor (comment) :* :anchor (_)
   :anchor (comment) :* :anchor (string) :?)"
  (when indices
    (let ((res ())
          (n (caar indices)))
      (while (progn
               (and res (setq res `(:anchor (comment) :* :anchor ,@res)))
               (>= n 0))
        (if (eq (caar indices) n)
            (setq res (append (cdr (pop indices)) res))
          (push '(_) res))
        (decf n))
      res)))

(defun lisp-ts-mode--build-format-query (operator-alist)
  "Construct a treesit sexp query from OPERATOR-ALIST.

The format of OPERATOR-ALIST is described in the documentation of
`lisp-ts-format-support-mode-query'. In addition to the query described
there, the value also always contains an additional query which matches
  :format-control \"~(format string~)\"
to match format strings in initializer lists for simple-conditions."
  (let ((query (list '(((interned_symbol !package ":" name: (_) @_sym)
                        (:eq? @_sym "format-control"))
                       :anchor (comment) :* :anchor
                       (string) @format)))
        (fmt-field
         (lambda (&rest nth-args)
           (let ((alist ()))
             (dolist (a (sort nth-args))
               (push `(,a . ((string) @format ,@(and alist '(:?)))) alist))
             (lisp-ts-mode--arg-fields alist)))))
    (pcase-dolist (`(,n . ,operators) operator-alist)
      (let* ((op-query
              (if (symbolp operators)
                  `((interned_symbol) @_sym (:pred? ,operators @_sym))
                `((interned_symbol name: (symbol_tokens) @_sym)
                  ,(if (eq (proper-list-p operators) 1)
                       `(:eq? @_sym ,(if (symbolp (car operators))
                                         (symbol-name (car operators))
                                       (car operators)))
                     `(:match?
                       @_sym
                       ,(if (stringp operators)
                            operators
                          (concat
                           "\\`"
                           (regexp-opt
                            (mapcar (lambda (op)
                                      (if (stringp op) op (symbol-name op)))
                                    operators))
                           "\\'"))))))))
        (push `(list :anchor (comment) :* :anchor ,op-query
                     ,@(cond ((eq n t) `((string) @format))
                             ((listp n) (apply fmt-field n))
                             (t (funcall fmt-field n))))
              query)))
    (nreverse query)))

(defcustom lisp-ts-format-support-mode-query
  `((0     . ("error" "warn" "formatter" "yes-or-no-p" "y-or-n-p" "break"))
    (1     . ("format" "format-symbol"))
    (2     . ("assert"))
    ((0 1) . ("cerror")))
  "A query used to apply format grammar in  `lisp-ts-format-support-mode'.
This variable must be set through the customize interface or `setopt'.
The value can be a treesit query (used for `treesit-range-rules') or an
alist of (ARG . OPERATOR-MATCH).

OPERATOR-MATCH matches against the head of a list. If OPERATOR-MATCH is
a regexp, it is matched against the name of the operator (excluding any
\"package:\" prefix). A list of symbols or strings is roughly equivalent
to (regexp-opt OPERATOR-MATCH). If OPERATOR-MATCH is a symbol, it is a
unary function which is called on the interned_symbol treesit node and
should return non-nil if that operator is matched by the rule.

ARG can be an integer or list of integers, specifying that the ARGth
argument to the operator is a format string. Otherwise ARG can be t,
meaning all string arguments to the operator are matched.

If you want to simply apply the grammar to all strings you can use the
query \"(string) @format\". Note that this will end up applying to
pathnames, so #p\"~/.emacs.d/\" will be incorrectly highlighted as a
function call format directive."
  :type '(choice (alist :key-type
                        (choice (const :tag "All string arguments" t)
                                (repeat natnum))
                        :value-type
                        (choice (regexp :tag "Match operator names"
                                        :value "format\\|FORMAT")
                                (function :tag "Predicate for the treesit node")
                                (repeat (string :tag "The name of an operator"))))
                 (restricted-sexp :value "(string) @format"
                                  :match-alternatives (ts-query-p)))
  :set (lambda (var val)
         (setf (default-toplevel-value var)
               (cond
                 ((not (fboundp 'ts-query-compile)) val)
                 ((integerp (car-safe (car-safe val)))
                  (ts-query-compile 'common-lisp
                                    (lisp-ts-mode--build-format-query val)))
                 ((ts-compiled-query-p val) val)
                 (t (ts-query-compile 'common-lisp val))))))

(defvar-local lisp-ts-format-support-mode--saved-state nil)

;;;###autoload
(define-minor-mode lisp-ts-format-support-mode
  "Enable embedded format string grammar.
Do not use this mode with `gaudy-cl-mode', as the latter applies the
format grammar automatically."
  :lighter nil
  :interactive (lisp-ts-mode)
  (when (local-variable-p 'lisp-ts-format-support-mode--saved-state)
    (buffer-local-restore-state lisp-ts-format-support-mode--saved-state)
    (kill-local-variable 'lisp-ts-format-support-mode--saved-state))
  (cond*
    ((not lisp-ts-format-support-mode) nil)
    ((not (derived-mode-p 'lisp-ts-mode))
     (lisp-ts-format-support-mode -1)
     (user-error "`lisp-ts-format-support-mode' only works in `lisp-ts-mode'"))
    ((not (ts-ensure-installed 'cl-format))
     (lisp-ts-format-support-mode -1)
     (warn "FORMAT grammar not available, `lisp-ts-format-support-mode' disabled"))
    ((bound-and-true-p gaudy-cl-mode)
     (warn (concat "`gaudy-cl-mode' and `lisp-ts-format-support-mode' "
                   "are redundant and should not be used together"))
     :non-exit)
    (t (setq-local
        lisp-ts-format-support-mode--saved-state
        (buffer-local-set-state
         ts-range-settings
         (append
          ts-range-settings
          (ts-range-rules
           :embed 'cl-format
           :host 'common-lisp
           :local t
           lisp-ts-format-support-mode-query))))
       (syntax-ppss-flush-cache (point-min))
       (font-lock-flush))))

;; declare doesn't work in `define-minor-mode'
(function-put 'lisp-ts-format-support-mode 'completion-predicate
              (lambda (_cmd buf)
                (with-current-buffer buf
                  (not (bound-and-true-p gaudy-cl-mode)))))

;; Local Variables:
;; read-symbol-shorthands: (("ts-" . "treesit-"))
;; End:

(provide 'lisp-ts-mode)
;;; lisp-ts-mode.el ends here

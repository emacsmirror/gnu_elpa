;;; cl-ts-mode.el --- lisp-mode with tree-sitter support -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2026 zach shaftel
;;
;; Author: zach shaftel <zach@shaf.tel>
;; Maintainer: zach shaftel <zach@shaf.tel>
;; Created: May 14, 2026
;; Modified: May 14, 2026
;; Version: 0.0.1
;; Keywords:
;; Homepage:
;; Package-Requires: ((emacs 31.0.50))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  lisp-mode with tree-sitter support
;;
;;; Code:

(require 'treesit)
(eval-when-compile
  (require 'subr-x))
(ts-declare-unavailable-functions)

(setf (alist-get 'common-lisp ts-language-source-alist)
      `(,(file-name-directory (macroexp-file-name))
        :source-dir "grammars/cl/src"))

(defgroup cl-ts-mode ()
  "Common Lisp mode with tree-sitter support."
  :group 'lisp)

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

(defface cl-ts-mode-format-paired-directive '((t :inherit font-lock-regexp-grouping-construct))
  "Face used to highlight paired format directives: ~<~>, ~{~}, ~(~), and ~[~].
If `cl-ts-mode-format-rainbow-delimiters' is non-nil, this face isn't
used.")

(defface cl-ts-mode-format-skipped-whitespace
  '((default :inherit font-lock-comment-face :weight bold)
    (((supports :underline (:color foreground-color :style dots)))
     :underline (:color foreground-color :style dots))
    (((supports :underline (:color foreground-color :style dashes)))
     :underline (:color foreground-color :style dashes))
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
(defun cl-ts-mode-update-comment-faces (&optional _theme)
  "A function to update the block comment faces by increasing darkness.
Meant to be added to `enable-theme-functions'. The darkening ratio is
controlled by `cl-ts-mode-comment-darken-percentage'."
  (let* ((comment-fore (face-attribute 'font-lock-comment-face :foreground nil t)))
    (when (stringp comment-fore)
      (require 'color)
      (let ((i 0))
        (while (< i 3)
          (incf i)
          (thread-last (* cl-ts-mode-comment-darken-percentage i)
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

(defun cl-ts-mode--fontify-one-format-directive (node &optional paired-depth mismatch-p)
  (defvar rainbow-delimiters-pick-face-function)
  (let ((child (ts-node-child node 0)))
    (while child
      (when-let* ((face (pcase (ts-node-type child)
                          ('"~" 'cl-ts-mode-format-tilde)
                          ('"V" 'cl-ts-mode-format-arg-parameter)
                          ('"#" 'cl-ts-mode-format-remaining-parameter)
                          ('"char_parameter" 'cl-ts-mode-format-char-parameter)
                          ('"numeric_parameter" 'cl-ts-mode-format-numeric-parameter)
                          ('"," 'cl-ts-mode-format-comma)
                          ('"@" 'cl-ts-mode-format-at)
                          ('":" 'cl-ts-mode-format-colon)
                          ('"directive_character"
                           (cond
                             ((eq (char-after (ts-node-start child)) ?\n)
                              (save-excursion
                                (goto-char (ts-node-start child))
                                (skip-chars-forward "\n\t ")
                                (add-face-text-property (ts-node-start child) (point)
                                                        'cl-ts-mode-format-skipped-whitespace)))
                             ((not paired-depth) 'cl-ts-mode-format-standalone-directive)
                             ((not (cl-ts-mode--format-use-rainbow-delimiters-p))
                              'cl-ts-mode-format-paired-directive)
                             (t (funcall rainbow-delimiters-pick-face-function
                                         paired-depth (not mismatch-p)
                                         (ts-node-start child))))))))
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
    (add-face-text-property (ts-node-start node)
                            (ts-node-end node)
                            'font-lock-string-face)
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
   :default-language 'common-lisp
   :feature 'string
   `((string) @cl-ts-mode--fontify-string)
   :feature 'comment
   `([(line_comment) (block_comment)] @cl-ts-mode--fontify-comment)
   :feature 'number
   ;; complexes are defined as
   ;; (complex [(float) (rational)] [(float) (rational)])
   ;; so their whole and imaginary parts will both get the face. highlighting
   ;; the entire expression #C(1/3 99.1) with font-lock-number-face seems weird
   ;; to me.
   `([(float) (rational)] @font-lock-number-face)
   :feature 'symbol
   :override 'prepend
   `((symbol_tokens [(single_escape) @font-lock-escape-face
                     (multiple_escape "|" @font-lock-constant-face)])
     [ ;; keywords
      (interned_symbol
       !package
       [":" "::"] @font-lock-delimiter-face
       name: (symbol_tokens) @font-lock-builtin-face)
      ;; other symbols
      (interned_symbol
       package: (symbol_tokens) :? @font-lock-keyword-face
       [":" "::"] :? @font-lock-delimiter-face)])
   :feature 'quote
   `((quote      "'"  @font-lock-preprocessor-face)
     (sharpquote "#'" @font-lock-constant-face)
     (quasiquote "`"  @font-lock-builtin-face)
     (unquote   [","  @font-lock-property-name-face
                 ",@" @font-lock-property-use-face
                 ",." @font-lock-warning-face]))
   :feature 'bits
   :override 'prepend
   `([(bit_vector) (rational)] @clparse--fontify-bits)))

(defconst cl-ts-mode-font-lock-feature-list
  '((string comment)
    (format-directive number)
    (block-comment symbol)
    (quote bits)))

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
      ;; font-lock-end - 1 cuz the node returned by
      ;; `ts-node-at' always ends after POS, and if a node ends
      ;; /at/ POS we don't need to extend the region
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

(defconst cl-ts-mode-thing-settings
  `((common-lisp
     (sexp ,(rx bos (or "string"
                        "interned_symbol"
                        "uninterned_symbol"
                        "struct"
                        "list"
                        "vector"
                        "bool_vector"
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
  (when (ts-ensure-installed 'common-lisp)
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
       (up-list-function forward-list-function forward-sexp-function)
       (ts-major-mode-setup)))
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

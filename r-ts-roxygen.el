;;; r-ts-roxygen.el --- R major mode using tree-sitter -*- lexical-binding: t; -*-


;;;; =========================================================================
;;;; Groups, Custom Variables, General Variables
;;;; =========================================================================
(defgroup r-ts-roxygen nil
  "R's roxygen support for r-ts-mode."
  :group 'r-ts-mode
  :version "30.1")

(defcustom r-ts-mode-roxygen-tags-param
  '("author" "aliases" "concept" "details"
    "example" "examples" "examplesIf"
    "format" "keywords"
    "method" "exportMethod"
    "name" "note" "param"
    "include" "references" "return" "returns"
    "seealso" "source" "docType"
    "title" "TODO" "usage" "import"
    "exportClass" "exportPattern"
    "exportS3Method" "S3method"
    "inherit" "inheritParams" "inheritSection"
    "importFrom" "importClassesFrom"
    "importMethodsFrom" "useDynLib"
    "rawNamespace"
    "rdname" "section" "slot" "description"
    "md" "eval" "evalNamespace" "family")
  "Roxygen tags that require a parameter.
Used to decide highlighting and tag completion."
  :group 'r-ts-roxygen
  :type '(repeat string))

(defcustom r-ts-mode-roxygen-tags-noparam '("export" "noRd")
  "Roxygen tags that can be used without a parameter.
Used to decide highlighting and tag completion."
  :group 'r-ts-roxygen
  :type '(repeat string))

(defconst r-ts-mode-roxygen--initial-regex "^[ \t]*#+'"
  "Regexp matching the start of a roxygen comment line.")

(defconst r-ts-mode-roxygen--param-name-regexp
  "\\(?:\\(?:\\sw\\|\\s_\\)+,?\\)+"
  "Regexp matching a parameter name, including symbols and commas.")


;;;; =========================================================================
;;;; Roxygen Supportive Functions
;;;; =========================================================================
(defun r-ts-mode-roxygen--build-keywords ()
  "Return a font-lock keyword list for roxygen comments.
Pure function — reads only `defcustom' values, produces no side effects."
  `(;; Highlight entire roxygen lines
    (,(concat r-ts-mode-roxygen--initial-regex ".*")
     (0 'font-lock-doc-face prepend))
    ;; Tags that take a parameter
    (,(concat r-ts-mode-roxygen--initial-regex " *\\([@\\]"
              (regexp-opt r-ts-mode-roxygen-tags-param t)
              "\\)\\>")
     (1 'font-lock-keyword-face prepend))
    ;; @param / @importFrom / etc. — highlight the argument name too
    (,(concat r-ts-mode-roxygen--initial-regex " *\\(@"
              (regexp-opt '("param" "importFrom" "importClassesFrom"
                            "importMethodsFrom" "describeIn")
                          'words)
              "\\)\\(?:[ \t]+\\(" r-ts-mode-roxygen--param-name-regexp "\\)\\)")
     (1 'font-lock-keyword-face prepend)
     (3 'font-lock-variable-name-face prepend))
    ;; Tags that take no parameter
    (,(concat "[@\\]" (regexp-opt r-ts-mode-roxygen-tags-noparam t) "\\>")
     (0 'font-lock-variable-name-face prepend))
    ;; Bold the #' prefix itself
    (,(concat r-ts-mode-roxygen--initial-regex)
     (0 'bold prepend))))

(defvar-local r-ts-mode-roxygen--active-keywords nil
  "Font-lock keywords currently installed by `r-ts-mode-roxygen-mode'.")

;; Does not work with ESS active
(defun r-ts-roxygen-complete-tag ()
  "Auto completion for Roxygen tags."
  (let* ((boundaries (bounds-of-thing-at-point 'symbol))
         (beg (car boundaries))
         (end (cdr boundaries)))
    (when (and boundaries
               (save-excursion
                 (goto-char beg)
                 (eq (following-char) ?@)))
      (list (1+ beg)
            end
            (append r-ts-mode-roxygen-tags-param
                    r-ts-mode-roxygen-tags-noparam)
            :exclusive 'no))))

(defun r-ts-mode-roxygen--enable ()
  "Install roxygen font-lock keywords and completion in the current buffer."
  (setq r-ts-mode-roxygen--active-keywords
        (r-ts-mode-roxygen--build-keywords))
  (font-lock-add-keywords nil r-ts-mode-roxygen--active-keywords)
  (add-hook 'completion-at-point-functions
            #'r-ts-roxygen-complete-tag nil t)
  )

(defun r-ts-mode-roxygen--disable ()
  "Remove roxygen font-lock keywords and completion from the current buffer."
  (when r-ts-mode-roxygen--active-keywords
    (font-lock-remove-keywords nil r-ts-mode-roxygen--active-keywords)
    (setq r-ts-mode-roxygen--active-keywords nil))
  (remove-hook 'completion-at-point-functions
               #'r-ts-roxygen-complete-tag t)
  )


;;;; =========================================================================
;;;; Roxygen Minor Mode
;;;; =========================================================================

;;;###autoload
(define-minor-mode r-ts-mode-roxygen-mode
  "Minor mode for roxygen documentation in R buffers."
  :init-value nil
  (if r-ts-mode-roxygen-mode
      (r-ts-mode-roxygen--enable)
    (r-ts-mode-roxygen--disable))
  (when font-lock-mode
    (font-lock-flush)))


(provide 'r-ts-roxygen)
;;; r-ts-roxygen.el ends here

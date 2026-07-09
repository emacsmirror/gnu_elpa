;;; r-ts-mode-tests.el --- ERT tests for r-ts-mode  -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for r-ts-mode.el using Emacs' built-in ERT framework.
;;
;; The package follows a functional style: most functions are pure
;; predicates or accessors over tree-sitter nodes.  Tests are therefore
;; organised into two tiers:
;;
;;   1. Pure / non-interactive functions — tested without a live
;;      tree-sitter parser, using stubs or lightweight buffer fixtures.
;;
;;   2. Tree-sitter integration tests — require the R grammar to be
;;      installed.  They are tagged :ts and are skipped automatically
;;      when the grammar is absent, so the pure-function suite always
;;      runs cleanly in a vanilla CI environment.

;;; Code:

(require 'ert)
(require 'cl-lib)


;;;; ---------------------------------------------------------------------------
;;;; Test utilities
;;;; ---------------------------------------------------------------------------

(defmacro r-ts-test--with-r-buffer (content &rest body)
  "Execute BODY in a temporary buffer containing CONTENT.
If the R tree-sitter grammar is available the buffer is put in
`r-ts-mode'; otherwise it is left in `fundamental-mode' so that
pure-function tests can still run."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (insert ,content)
     (goto-char (point-min))
     (if (treesit-ready-p 'r t)
         (r-ts-mode)
       (fundamental-mode))
     ,@body))

(defmacro r-ts-test--skip-without-grammar ()
  "Skip the current test when the R tree-sitter grammar is unavailable."
  `(skip-unless (treesit-ready-p 'r t)))

(defun r-ts-test--node-at-word (word)
  "Return the tree-sitter node whose text equals WORD, searching from point-min."
  (save-excursion
    (goto-char (point-min))
    (when (search-forward word nil t)
      (treesit-node-at (1- (point))))))


;;;; ---------------------------------------------------------------------------
;;;; Potential tests for setup module
;;;; ---------------------------------------------------------------------------

;;; r-ts-mode--build-r-find-package-command
;; (ert-deftest r-ts-mode-test--build-r-find-package-command/unix ()
;;   "Unix: command ends with single-quoted expression."
;;   (let ((cmd (r-ts-mode--build-r-find-package-command "R")))
;;     (should (string-match-p "find\\.package" cmd))
;;     (should (string-match-p "treesitter\\.r" cmd))
;;     ;; Unix variant uses single quotes around the -e argument
;;     (should (string-match-p "'" cmd))))

;; (ert-deftest r-ts-mode-test--build-r-find-package-command/windows-exe ()
;;   "Windows .exe path: command does NOT contain single quotes."
;;   (let ((cmd (r-ts-mode--build-r-find-package-command "C:/Program Files/R/bin/R.exe")))
;;     (should (string-match-p "find\\.package" cmd))
;;     (should (string-match-p "treesitter\\.r" cmd))
;;     (should-not (string-match-p "'" cmd))))

;; (ert-deftest r-ts-mode-test--build-r-find-package-command/custom-path ()
;;   "Custom R path is embedded in the returned command."
;;   (let ((cmd (r-ts-mode--build-r-find-package-command "/opt/R/4.4/bin/R")))
;;     (should (string-prefix-p "/opt/R/4.4/bin/R" cmd))))

;; ;;; r-ts-mode--parse-r-find-package-output

;; (ert-deftest r-ts-mode-test--parse-r-find-package-output/typical ()
;;   "Typical R output: extracts the path inside double quotes."
;;   (should (equal "/home/user/R/library/treesitter.r"
;;                  (r-ts-mode--parse-r-find-package-output
;;                   "[1] \"/home/user/R/library/treesitter.r\"\n"))))

;; (ert-deftest r-ts-mode-test--parse-r-find-package-output/windows-path ()
;;   "Windows path with backslashes is extracted correctly."
;;   (should (equal "C:/Users/user/R/win-library/4.4/treesitter.r"
;;                  (r-ts-mode--parse-r-find-package-output
;;                   "[1] \"C:/Users/user/R/win-library/4.4/treesitter.r\""))))

;; (ert-deftest r-ts-mode-test--parse-r-find-package-output/error-prefix ()
;;   "Output starting with 'Error' signals an error."
;;   (should-error
;;    (r-ts-mode--parse-r-find-package-output "Error in find.package(...) : ...")
;;    :type 'error))

;; (ert-deftest r-ts-mode-test--parse-r-find-package-output/no-quotes ()
;;   "Output with no quoted path signals an error."
;;   (should-error
;;    (r-ts-mode--parse-r-find-package-output "[1] no-quotes-here\n")
;;    :type 'error))

;; ;;; r-ts-mode--binary-path-unix

;; (ert-deftest r-ts-mode-test--binary-path-unix/structure ()
;;   "Returns path ending in /libs/treesitter.r.so."
;;   (let ((result (r-ts-mode--binary-path-unix "/some/pkg")))
;;     (should (string-equal result "/some/pkg/libs/treesitter.r.so"))))

;; ;;; r-ts-mode--binary-path-win

;; (ert-deftest r-ts-mode-test--binary-path-win/returns-list ()
;;   "Returns a list of candidate .dll paths."
;;   (let ((result (r-ts-mode--binary-path-win "C:/R/treesitter.r")))
;;     (should (listp result))
;;     (should (> (length result) 0))
;;     (should (cl-every (lambda (p) (string-match-p "\\.dll\\'" p)) result))))

;; (ert-deftest r-ts-mode-test--binary-path-win/contains-base-dll ()
;;   "One candidate is the flat treesitter.r.dll."
;;   (let ((result (r-ts-mode--binary-path-win "/pkg")))
;;     (should (member "/pkg/libs/treesitter.r.dll" result))))

;; ;;; r-ts-mode--validate-path-exists

;; (ert-deftest r-ts-mode-test--validate-path-exists/existing ()
;;   "Returns the path when it exists."
;;   (let ((tmp (make-temp-file "r-ts-test-")))
;;     (unwind-protect
;;         (should (equal tmp (r-ts-mode--validate-path-exists tmp)))
;;       (delete-file tmp))))

;; (ert-deftest r-ts-mode-test--validate-path-exists/missing ()
;;   "Signals an error for a non-existent path."
;;   (should-error
;;    (r-ts-mode--validate-path-exists "/this/path/does/not/exist/hopefully")
;;    :type 'error))

;; ;;; r-ts-mode--ensure-directory

;; (ert-deftest r-ts-mode-test--ensure-directory/existing ()
;;   "Does not error on an existing directory."
;;   (let ((tmp (make-temp-file "r-ts-dir-" t)))
;;     (unwind-protect
;;         (should-not (condition-case err
;;                         (progn (r-ts-mode--ensure-directory tmp) nil)
;;                       (error err)))
;;       (delete-directory tmp))))

;; (ert-deftest r-ts-mode-test--ensure-directory/creates-when-allowed ()
;;   "Creates the directory when `r-ts-mode-create-treesitter-dir' is non-nil."
;;   (let* ((tmp-parent (make-temp-file "r-ts-parent-" t))
;;          (new-dir (expand-file-name "subdir" tmp-parent))
;;          (r-ts-mode-create-treesitter-dir t))
;;     (unwind-protect
;;         (progn
;;           (r-ts-mode--ensure-directory new-dir)
;;           (should (file-directory-p new-dir)))
;;       (delete-directory tmp-parent t))))

;; (ert-deftest r-ts-mode-test--ensure-directory/errors-when-forbidden ()
;;   "Signals an error when directory is missing and creation is disabled."
;;   (let ((r-ts-mode-create-treesitter-dir nil))
;;     (should-error
;;      (r-ts-mode--ensure-directory "/nonexistent/path/r-ts-test")
;;      :type 'error)))

;; (ert-deftest r-ts-mode-test--binary-path-unix/trailing-slash-package-path ()
;;   "Handles a package path with trailing slash without double-slash."
;;   (let ((result (r-ts-mode--binary-path-unix "/some/pkg/")))
;;     ;; Should not produce double slashes in the middle
;;     (should-not (string-match-p "//libs" result))))

;; (ert-deftest r-ts-mode-test--binary-path-win/x64-variant-present ()
;;   "The x64 sub-architecture path is among the Windows candidates."
;;   (let ((result (r-ts-mode--binary-path-win "/pkg")))
;;     (should (cl-some (lambda (p) (string-match-p "x64" p)) result))))

;; (ert-deftest r-ts-mode-test--parse-r-find-package-output/extra-whitespace ()
;;   "Handles extra whitespace / newlines around the quoted path."
;;   (should (equal "/path/to/pkg"
;;                  (r-ts-mode--parse-r-find-package-output
;;                   "\n[1] \"/path/to/pkg\"\n"))))


;;;; ---------------------------------------------------------------------------
;;;; Tree-sitter node predicates and accessors
;;;;     These require a live parser — tagged :ts
;;;; ---------------------------------------------------------------------------

;;; r-ts-mode--node-is-fun-def-p

(ert-deftest r-ts-mode-test--node-is-fun-def-p/true ()
  "Returns non-nil for a function-definition assignment node."
  :tags '(:ts)
  (r-ts-test--skip-without-grammar)
  (r-ts-test--with-r-buffer "my_fn <- function(x) x + 1"
    (let ((node (treesit-node-at (point-min))))   ; <-- no pre-jumps
      (while (and node
                  (not (string-equal (treesit-node-type node) "binary_operator")))
        (setq node (treesit-node-parent node)))
      (should node)
      (should (r-ts-mode--node-is-fun-def-p node)))))

(ert-deftest r-ts-mode-test--node-is-fun-def-p/false-for-value-assign ()
  "Returns nil for a plain value assignment."
  :tags '(:ts)
  (r-ts-test--skip-without-grammar)
  (r-ts-test--with-r-buffer "x <- 42"
    (let ((node (treesit-node-parent (treesit-node-at (point-min)))))
      (while (and node
                  (not (string-equal (treesit-node-type node) "binary_operator")))
        (setq node (treesit-node-parent node)))
      (should node)
      (should-not (r-ts-mode--node-is-fun-def-p node)))))

;;; r-ts-mode--node-is-assignment-p

(ert-deftest r-ts-mode-test--node-is-assignment-p/arrow ()
  "Returns non-nil for `<-' assignment."
  :tags '(:ts)
  (r-ts-test--skip-without-grammar)
  (r-ts-test--with-r-buffer "foo <- 1"
    (let ((node (treesit-node-parent (treesit-node-at (point-min)))))
      (while (and node
                  (not (string-equal (treesit-node-type node) "binary_operator")))
        (setq node (treesit-node-parent node)))
      (should (r-ts-mode--node-is-assignment-p node)))))

(ert-deftest r-ts-mode-test--node-is-assignment-p/equals ()
  "Returns non-nil for `=' assignment."
  :tags '(:ts)
  (r-ts-test--skip-without-grammar)
  (r-ts-test--with-r-buffer "foo = 1"
    (let ((node (treesit-node-parent (treesit-node-at (point-min)))))
      (while (and node
                  (not (string-equal (treesit-node-type node) "binary_operator")))
        (setq node (treesit-node-parent node)))
      (should (r-ts-mode--node-is-assignment-p node)))))

(ert-deftest r-ts-mode-test--node-is-assignment-p/pipe-is-not-assignment ()
  "Returns nil for a pipe operator — not an assignment."
  :tags '(:ts)
  (r-ts-test--skip-without-grammar)
  (r-ts-test--with-r-buffer "x |> foo()"
    (let ((node (treesit-node-parent (treesit-node-at (point-min)))))
      (while (and node
                  (not (string-equal (treesit-node-type node) "binary_operator")))
        (setq node (treesit-node-parent node)))
      (should-not (r-ts-mode--node-is-assignment-p node)))))

;;; r-ts-mode--node-is-simple-object-p

(ert-deftest r-ts-mode-test--node-is-simple-object-p/plain-value ()
  "Returns non-nil for a top-level value assignment."
  :tags '(:ts)
  (r-ts-test--skip-without-grammar)
  (r-ts-test--with-r-buffer "my_var <- 99"
    (let ((node (treesit-node-parent (treesit-node-at (point-min)))))
      (while (and node
                  (not (string-equal (treesit-node-type node) "binary_operator")))
        (setq node (treesit-node-parent node)))
      (should (r-ts-mode--node-is-simple-object-p node)))))

(ert-deftest r-ts-mode-test--node-is-simple-object-p/function-def-is-not-simple ()
  "Returns nil for a function definition assignment."
  :tags '(:ts)
  (r-ts-test--skip-without-grammar)
  (r-ts-test--with-r-buffer "my_fn <- function(x) x"
    (let ((node (treesit-node-parent (treesit-node-at (point-min)))))
      (while (and node
                  (not (string-equal (treesit-node-type node) "binary_operator")))
        (setq node (treesit-node-parent node)))
      (should-not (r-ts-mode--node-is-simple-object-p node)))))

;;; r-ts-mode--node-lhs-text

(ert-deftest r-ts-mode-test--node-lhs-text/arrow-assignment ()
  "Returns the LHS identifier text for `<-'."
  :tags '(:ts)
  (r-ts-test--skip-without-grammar)
  (r-ts-test--with-r-buffer "my_var <- 42"
    (let ((node (treesit-node-parent (treesit-node-at (point-min)))))
      (while (and node
                  (not (string-equal (treesit-node-type node) "binary_operator")))
        (setq node (treesit-node-parent node)))
      (should (equal "my_var" (r-ts-mode--node-lhs-text node))))))

(ert-deftest r-ts-mode-test--node-lhs-text/equals-assignment ()
  "Returns the LHS identifier text for `='."
  :tags '(:ts)
  (r-ts-test--skip-without-grammar)
  (r-ts-test--with-r-buffer "result = TRUE"
    (let ((node (treesit-node-parent (treesit-node-at (point-min)))))
      (while (and node
                  (not (string-equal (treesit-node-type node) "binary_operator")))
        (setq node (treesit-node-parent node)))
      (should (equal "result" (r-ts-mode--node-lhs-text node))))))

(ert-deftest r-ts-mode-test--node-lhs-text/non-binary-returns-nil ()
  "Returns nil when called on a non-binary-operator node."
  :tags '(:ts)
  (r-ts-test--skip-without-grammar)
  (r-ts-test--with-r-buffer "42"
    ;; The node at point-min here is a numeric literal, not a binary_operator
    (let ((node (treesit-node-at (point-min))))
      (should-not (r-ts-mode--node-lhs-text node)))))

;;; r-ts-mode--defun-name

(ert-deftest r-ts-mode-test--defun-name/returns-name-for-fun ()
  "Returns the function name for a function definition node."
  :tags '(:ts)
  (r-ts-test--skip-without-grammar)
  (r-ts-test--with-r-buffer "compute_sum <- function(a, b) a + b"
    (let ((node (treesit-node-parent (treesit-node-at (point-min)))))
      (while (and node
                  (not (string-equal (treesit-node-type node) "binary_operator")))
        (setq node (treesit-node-parent node)))
      (should (equal "compute_sum" (r-ts-mode--defun-name node))))))

(ert-deftest r-ts-mode-test--defun-name/nil-for-plain-assignment ()
  "Returns nil for a non-function assignment."
  :tags '(:ts)
  (r-ts-test--skip-without-grammar)
  (r-ts-test--with-r-buffer "x <- 1"
    (let ((node (treesit-node-parent (treesit-node-at (point-min)))))
      (while (and node
                  (not (string-equal (treesit-node-type node) "binary_operator")))
        (setq node (treesit-node-parent node)))
      (should-not (r-ts-mode--defun-name node)))))

;;; r-ts-mode--object-name

(ert-deftest r-ts-mode-test--object-name/returns-name ()
  "Returns the object name for any binary_operator node."
  :tags '(:ts)
  (r-ts-test--skip-without-grammar)
  (r-ts-test--with-r-buffer "my_df <- data.frame()"
    (let ((node (treesit-node-parent (treesit-node-at (point-min)))))
      (while (and node
                  (not (string-equal (treesit-node-type node) "binary_operator")))
        (setq node (treesit-node-parent node)))
      (should (equal "my_df" (r-ts-mode--object-name node))))))

(ert-deftest r-ts-mode-test--object-name/nil-for-non-binary ()
  "Returns nil when node is not a binary_operator."
  :tags '(:ts)
  (r-ts-test--skip-without-grammar)
  (r-ts-test--with-r-buffer "42"
    (let ((node (treesit-node-at (point-min))))
      (should-not (r-ts-mode--object-name node)))))


;;;; ---------------------------------------------------------------------------
;;;; Node navigation utilities
;;;; ---------------------------------------------------------------------------

;;; r-ts-mode--node-ancestor-matching

(ert-deftest r-ts-mode-test--node-ancestor-matching/finds-ancestor ()
  "Walks up and returns the first matching ancestor node."
  :tags '(:ts)
  (r-ts-test--skip-without-grammar)
  (r-ts-test--with-r-buffer "f <- function(x) { x + 1 }"
    ;; Position point inside the braces on `x'
    (search-forward "x + 1")
    (backward-char 2)
    (let* ((node (treesit-node-at (point)))
           (result (r-ts-mode--node-ancestor-matching node "function_definition")))
      (should result)
      (should (string-equal (treesit-node-type result) "function_definition")))))

(ert-deftest r-ts-mode-test--node-ancestor-matching/returns-self-when-matching ()
  "Returns NODE itself when it already matches TYPE."
  :tags '(:ts)
  (r-ts-test--skip-without-grammar)
  (r-ts-test--with-r-buffer "x <- 1"
    (let* ((node (treesit-node-parent (treesit-node-at (point-min)))))
      (while (and node
                  (not (string-equal (treesit-node-type node) "binary_operator")))
        (setq node (treesit-node-parent node)))
      (should (eq node (r-ts-mode--node-ancestor-matching node "binary_operator"))))))

(ert-deftest r-ts-mode-test--node-ancestor-matching/returns-nil-at-root ()
  "Returns nil when no ancestor matches before the program root."
  :tags '(:ts)
  (r-ts-test--skip-without-grammar)
  (r-ts-test--with-r-buffer "x <- 1"
    (let* ((node (treesit-node-at (point-min))))
      ;; "call" won't be found in a bare assignment
      (should-not (r-ts-mode--node-ancestor-matching node "call")))))

;;; r-ts-mode--inside-fun-def-p

(ert-deftest r-ts-mode-test--inside-fun-def-p/inside ()
  "Returns non-nil when point is inside a function body."
  :tags '(:ts)
  (r-ts-test--skip-without-grammar)
  (r-ts-test--with-r-buffer "f <- function(x) { x * 2 }"
    (search-forward "x * 2")
    (backward-char 1)
    (should (r-ts-mode--inside-fun-def-p))))

(ert-deftest r-ts-mode-test--inside-fun-def-p/outside ()
  "Returns nil when point is at top level, not inside any function."
  :tags '(:ts)
  (r-ts-test--skip-without-grammar)
  (r-ts-test--with-r-buffer "x <- 1\ny <- 2"
    (goto-char (point-min))
    ;; At `x', not inside any function definition
    (should-not (r-ts-mode--inside-fun-def-p))))

;;; r-ts-mode--argument-function-name

(ert-deftest r-ts-mode-test--argument-function-name/inside-call ()
  "Returns the enclosing function name when point is in an argument list."
  :tags '(:ts)
  (r-ts-test--skip-without-grammar)
  (r-ts-test--with-r-buffer "mean(x, na.rm = TRUE)"
    (search-forward "na.rm")
    (should (equal "mean" (r-ts-mode--argument-function-name)))))

(ert-deftest r-ts-mode-test--argument-function-name/outside-call ()
  "Returns nil when point is not inside any function call arguments."
  :tags '(:ts)
  (r-ts-test--skip-without-grammar)
  (r-ts-test--with-r-buffer "x <- 1"
    (goto-char (point-min))
    (should-not (r-ts-mode--argument-function-name))))

;;; r-ts-mode--buffer-function-positions

(ert-deftest r-ts-mode-test--buffer-function-positions/single-function ()
  "Returns an alist with one entry for a buffer with one function."
  :tags '(:ts)
  (r-ts-test--skip-without-grammar)
  (r-ts-test--with-r-buffer "add <- function(a, b) a + b"
    (let ((result (r-ts-mode--buffer-function-positions (current-buffer))))
      (should (= 1 (length result)))
      (should (assoc "add" result))
      (should (integerp (cdr (assoc "add" result)))))))

(ert-deftest r-ts-mode-test--buffer-function-positions/multiple-functions ()
  "Returns entries for all function definitions in the buffer."
  :tags '(:ts)
  (r-ts-test--skip-without-grammar)
  (r-ts-test--with-r-buffer "f1 <- function(x) x\nf2 <- function(y) y + 1\n"
    (let ((result (r-ts-mode--buffer-function-positions (current-buffer))))
      (should (= 2 (length result)))
      (should (assoc "f1" result))
      (should (assoc "f2" result)))))

(ert-deftest r-ts-mode-test--buffer-function-positions/no-functions ()
  "Returns nil for a buffer with no function definitions."
  :tags '(:ts)
  (r-ts-test--skip-without-grammar)
  (r-ts-test--with-r-buffer "x <- 1\ny <- 2\n"
    (let ((result (r-ts-mode--buffer-function-positions (current-buffer))))
      (should (null result)))))

(ert-deftest r-ts-mode-test--buffer-function-positions/excludes-plain-objects ()
  "Plain value assignments are not included in function positions."
  :tags '(:ts)
  (r-ts-test--skip-without-grammar)
  (r-ts-test--with-r-buffer "x <- 1\nmy_fn <- function(z) z\n"
    (let ((result (r-ts-mode--buffer-function-positions (current-buffer))))
      (should (= 1 (length result)))
      (should (assoc "my_fn" result))
      (should-not (assoc "x" result)))))

(ert-deftest r-ts-mode-test--buffer-function-positions/accepts-buffer-name ()
  "Accepts a buffer name string as well as a buffer object."
  :tags '(:ts)
  (r-ts-test--skip-without-grammar)
  (r-ts-test--with-r-buffer "f <- function() NULL\n"
    (let* ((buf-name (buffer-name))
           (result (r-ts-mode--buffer-function-positions buf-name)))
      (should (assoc "f" result)))))


;;;; ---------------------------------------------------------------------------
;;;; Public interactive commands (smoke tests)
;;;; ---------------------------------------------------------------------------

;;; r-ts-mode-goto-next-definition / r-ts-mode-goto-previous-definition

(ert-deftest r-ts-mode-test--goto-next-definition/moves-forward ()
  "Point advances past the first definition when calling goto-next."
  :tags '(:ts)
  (r-ts-test--skip-without-grammar)
  (r-ts-test--with-r-buffer "x <- 1\ny <- 2\n"
    (goto-char (point-min))
    (let ((start (point)))
      (r-ts-mode-goto-next-definition)
      (should (> (point) start)))))

(ert-deftest r-ts-mode-test--goto-previous-definition/moves-backward ()
  "Point moves backward to the previous definition."
  :tags '(:ts)
  (r-ts-test--skip-without-grammar)
  (r-ts-test--with-r-buffer "x <- 1\ny <- 2\nTRUE\n"
    (goto-char (point-max))
    (r-ts-mode-goto-previous-definition)
    (let ((after-first-back (point)))
      (r-ts-mode-goto-previous-definition)
      (should (< (point) after-first-back)))))


;;;; ---------------------------------------------------------------------------
;;;; Syntax table spot-checks (no parser required)
;;;; ---------------------------------------------------------------------------
(ert-deftest r-ts-mode-test--syntax-table/hash-is-comment-start ()
  "# has comment-start syntax in r-ts-mode-syntax-table."
  (with-syntax-table r-ts-mode-syntax-table
    (should (eq (char-syntax ?#) ?<))))

(ert-deftest r-ts-mode-test--syntax-table/newline-is-comment-end ()
  "Newline has comment-end syntax (closes # comments)."
  (with-syntax-table r-ts-mode-syntax-table
    (should (eq (char-syntax ?\n) ?>))))

(ert-deftest r-ts-mode-test--syntax-table/plus-is-punctuation ()
  "+ has punctuation (operator) syntax."
  (with-syntax-table r-ts-mode-syntax-table
    (should (eq (char-syntax ?+) ?.))))

(ert-deftest r-ts-mode-test--syntax-table/double-quote-is-string ()
  "Double-quote opens/closes strings."
  (with-syntax-table r-ts-mode-syntax-table
    (should (eq (char-syntax ?\") ?\"))))

(ert-deftest r-ts-mode-test--syntax-table/underscore-is-symbol ()
  "_ has symbol-constituent syntax."
  (with-syntax-table r-ts-mode-syntax-table
    (should (eq (char-syntax ?_) ?_))))


;;;; ---------------------------------------------------------------------------
;;;; Variable and constant sanity checks (no parser required)
;;;; ---------------------------------------------------------------------------

(ert-deftest r-ts-mode-test--operators-list/is-list-of-strings ()
  "`r-ts-mode--operators' is a non-empty list of strings."
  (should (listp r-ts-mode--operators))
  (should (> (length r-ts-mode--operators) 0))
  (should (cl-every #'stringp r-ts-mode--operators)))

(ert-deftest r-ts-mode-test--operators-list/contains-pipe ()
  "Native pipe |> is in the operators list."
  (should (member "|>" r-ts-mode--operators)))

(ert-deftest r-ts-mode-test--operators-list/contains-assignment-ops ()
  "Arrow and equals assignment operators are listed."
  (should (member "<-" r-ts-mode--operators))
  (should (member "=" r-ts-mode--operators)))

(ert-deftest r-ts-mode-test--indent-rules/r-language-key ()
  "`r-ts-mode--indent-rules' is keyed under the symbol `r'."
  (should (assq 'r r-ts-mode--indent-rules)))

(ert-deftest r-ts-mode-test--indent-rules/closing-delimiters-at-zero ()
  "Closing bracket rules have 0 offset (align with parent)."
  (let ((rules (cdr (assq 'r r-ts-mode--indent-rules))))
    ;; Each rule is (PREDICATE ANCHOR OFFSET); find the '}' rule
    (let ((brace-rule (cl-find-if (lambda (r)
                                    (and (listp r)
                                         (equal (car r) '(node-is "}"))))
                                  rules)))
      (should brace-rule)
      (should (= 0 (nth 2 brace-rule))))))

(ert-deftest r-ts-mode-test--indent-level/is-positive-integer ()
  "`r-ts-mode-indent-level' is a positive integer."
  (should (integerp r-ts-mode-indent-level))
  (should (> r-ts-mode-indent-level 0)))

(ert-deftest r-ts-mode-test--imenu-settings/two-categories ()
  "`r-ts-mode--imenu-settings' has exactly two categories."
  (should (= 2 (length r-ts-mode--imenu-settings))))

(ert-deftest r-ts-mode-test--imenu-settings/function-category-exists ()
  "Imenu has a \"Function\" category."
  (should (assoc "Function" r-ts-mode--imenu-settings)))

(ert-deftest r-ts-mode-test--imenu-settings/object-category-exists ()
  "Imenu has an \"Object\" category."
  (should (assoc "Object" r-ts-mode--imenu-settings)))


;;;; ---------------------------------------------------------------------------
;;;; Mode activation smoke test
;;;; ---------------------------------------------------------------------------

(ert-deftest r-ts-mode-test--mode-activates ()
  "r-ts-mode activates without errors when the R grammar is present."
  :tags '(:ts)
  (r-ts-test--skip-without-grammar)
  (r-ts-test--with-r-buffer "x <- 1\n"
    (should (eq major-mode 'r-ts-mode))))

(ert-deftest r-ts-mode-test--mode-sets-indent-rules ()
  "r-ts-mode installs treesit-simple-indent-rules in the buffer."
  :tags '(:ts)
  (r-ts-test--skip-without-grammar)
  (r-ts-test--with-r-buffer "x <- 1\n"
    (should (local-variable-p 'treesit-simple-indent-rules))
    (should treesit-simple-indent-rules)))

(ert-deftest r-ts-mode-test--mode-sets-font-lock-settings ()
  "r-ts-mode installs treesit-font-lock-settings in the buffer."
  :tags '(:ts)
  (r-ts-test--skip-without-grammar)
  (r-ts-test--with-r-buffer "x <- 1\n"
    (should (local-variable-p 'treesit-font-lock-settings))
    (should treesit-font-lock-settings)))

(ert-deftest r-ts-mode-test--alias-R-ts-mode ()
  "R-ts-mode is an alias for r-ts-mode."
  (should (eq (symbol-function 'R-ts-mode) #'r-ts-mode)))


(provide 'r-ts-mode-tests)
;;; r-ts-mode-tests.el ends here

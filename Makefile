## Makefile for r-ts-mode
## Targets: help, build, test, test-pure, test-ts, lint, clean

EMACS      ?= emacs
BATCH       = $(EMACS) --batch --no-site-file --no-site-lisp

## Source files — order matters for byte-compilation (roxygen first)
SOURCES     = r-ts-roxygen.el r-ts-mode.el
COMPILED    = $(SOURCES:.el=.elc)
TEST_FILE   = r-ts-mode-tests.el

## Load flags passed to every Emacs invocation
LOAD_SRC    = $(foreach f,$(SOURCES),--load $(f))
LOAD_ALL    = $(LOAD_SRC) --load $(TEST_FILE)

## ── Phony targets ────────────────────────────────────────────────────────────
.PHONY: help build test test-pure test-ts lint clean

## Default target
all: help

help:
	@echo ""
	@echo "  r-ts-mode — available targets"
	@echo ""
	@echo "  build      Byte-compile all source files"
	@echo "  test       Run the full test suite"
	@echo "  test-pure  Run only pure-function tests (no R grammar required)"
	@echo "  test-ts    Run only tree-sitter integration tests"
	@echo "  lint       Byte-compile with warnings-as-errors (no .elc output)"
	@echo "  clean      Remove byte-compiled files"
	@echo ""
	@echo "  Override the Emacs binary with:  make test EMACS=/path/to/emacs"
	@echo ""

## ── Build ────────────────────────────────────────────────────────────────────
build: $(COMPILED)

## Compile roxygen first since r-ts-mode.el requires it
r-ts-roxygen.elc: r-ts-roxygen.el
	$(BATCH) --load r-ts-roxygen.el \
	         --eval '(byte-compile-file "r-ts-roxygen.el")'

r-ts-mode.elc: r-ts-mode.el r-ts-roxygen.elc
	$(BATCH) --load r-ts-roxygen.el \
	         --load r-ts-mode.el \
	         --eval '(byte-compile-file "r-ts-mode.el")'

## ── Test ─────────────────────────────────────────────────────────────────────

## Run every test (pure + tree-sitter; :ts tests skip silently without grammar)
test:
	$(BATCH) $(LOAD_ALL) \
	         --eval '(ert-run-tests-batch-and-exit t)'

## Run only tests that do NOT carry the :ts tag
test-pure:
	$(BATCH) $(LOAD_ALL) \
	         --eval '(ert-run-tests-batch-and-exit \
	                   (lambda (test) \
	                     (not (memq (quote ts) (ert-test-tags test)))))'

## Run only tree-sitter integration tests
test-ts:
	$(BATCH) $(LOAD_ALL) \
	         --eval '(ert-run-tests-batch-and-exit \
	                   (lambda (test) \
	                     (memq (quote ts) (ert-test-tags test))))'

## ── Lint ─────────────────────────────────────────────────────────────────────
## Byte-compiles to a temp directory so no .elc files are written to the tree.
## Any byte-compiler warning is promoted to an error and fails the target.
lint:
	$(BATCH) \
	  --eval '(setq byte-compile-error-on-warn t)' \
	  --eval '(let ((tmp (make-temp-file "r-ts-lint-" t))) \
	            (dolist (f (list "r-ts-roxygen.el" "r-ts-mode.el")) \
	              (byte-compile-file f) \
	              (let ((elc (concat (file-name-sans-extension f) ".elc"))) \
	                (when (file-exists-p elc) (delete-file elc)))))'

## ── Clean ────────────────────────────────────────────────────────────────────
clean:
	rm -f $(COMPILED)

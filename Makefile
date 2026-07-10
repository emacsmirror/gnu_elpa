## Makefile for r-ts-mode
## Targets: help, build, test, test-pure, test-ts, lint, clean
.POSIX:
EMACS = emacs
BATCH = $(EMACS) -Q --batch -L .
VERSION = 1.0.0

COMPILE = r-ts-roxygen.elc r-ts-mode.elc
TESTS = r-ts-mode-tests.elc

.SUFFIXES: .el .elc
.el.elc:
	$(BATCH) -f batch-byte-compile $<

## ── Phony targets ────────────────────────────────────────────────────────────
.PHONY: help build test lint clean

## Default target
all: help

help:
	@echo ""
	@echo "  r-ts-mode — available targets"
	@echo ""
	@echo "  build      Byte-compile all source files"
	@echo "  test       Run the full test suite"
	@echo "  lint       Byte-compile with warnings-as-errors (no .elc output)"
	@echo "  clean      Remove byte-compiled files"
	@echo ""
	@echo "  Override the Emacs binary with:  make test EMACS=/path/to/emacs"
	@echo ""

## ── Build ────────────────────────────────────────────────────────────────────
build: $(COMPILE)

## Compile test file only after sources are compiled
$(TESTS): $(COMPILE)

test: $(COMPILE) $(TESTS)
	$(BATCH)  -l r-ts-roxygen.elc -l r-ts-mode.elc \
                  -l r-ts-mode-tests.elc -f ert-run-tests-batch-and-exit

lint:
	$(BATCH) --eval '(setq byte-compile-error-on-warn t)' \
	         -f batch-byte-compile r-ts-roxygen.el r-ts-mode.el
	rm -f $(COMPILE)

clean:
	rm -f $(COMPILE) $(TESTS)

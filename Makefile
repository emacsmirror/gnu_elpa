## Makefile for r-ts-mode
.POSIX:
.SUFFIXES: .el .elc

EMACS   = emacs
BATCH   = $(EMACS) -Q --batch -L .
VERSION = 1.1.2

## Files to byte-compile
EL      = r-ts-roxygen.el r-ts-mode.el r-ts-setup.el
COMPILE = $(EL:.el=.elc)
TESTS   = r-ts-mode-tests.elc

## Everything that goes into the release tarball
PKG     = r-ts-mode-pkg.el
DIST    = $(PKG) $(EL) README.org LICENSE

.el.elc:
	$(BATCH) -f batch-byte-compile $<

.PHONY: all help build test lint package clean

all: help

help:
	@echo ""
	@echo "  r-ts-mode — available targets"
	@echo ""
	@echo "  build      Byte-compile all source files"
	@echo "  test       Run the full test suite"
	@echo "  lint       Byte-compile with warnings-as-errors (no .elc output)"
	@echo "  package    Create release tarball r-ts-mode-\$$(VERSION).tar"
	@echo "  clean      Remove byte-compiled files and package directory/tar"
	@echo ""
	@echo "  Override the Emacs binary with:  make test EMACS=/path/to/emacs"
	@echo ""

## ── Build ──
build: $(COMPILE)

## Compile tests only after sources are ready
$(TESTS): $(COMPILE)

## ── Test ───
test: $(COMPILE) $(TESTS)
	$(BATCH) -l r-ts-roxygen.elc -l r-ts-mode.elc \
	         -l r-ts-mode-tests.elc -f ert-run-tests-batch-and-exit

## ── Lint ───
lint:
	$(BATCH) --eval "(progn (add-to-list 'load-path \"~/.emacs.d/straight/build/package-lint/\") (require 'package-lint))" -l $(EL) -f package-lint-batch-and-exit $(EL)

## ── Package ──
package: r-ts-mode-$(VERSION).tar

r-ts-mode-$(VERSION): $(DIST)
	mkdir -p $@
	cp $(DIST) $@/
	touch $@/

r-ts-mode-$(VERSION).tar: r-ts-mode-$(VERSION)
	tar cf $@ r-ts-mode-$(VERSION)/
	rm -rf r-ts-mode-$(VERSION)/

## ── Clean ──
clean:
	rm -rf $(COMPILE) $(TESTS) r-ts-mode-$(VERSION) r-ts-mode-$(VERSION).tar

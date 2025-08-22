SHELL = /bin/sh

SRCDIR = $(shell dirname "$(realpath $(lastword $(MAKEFILE_LIST)))")

DOCDIR = $(SRCDIR)/doc

PKG = disproject

EMACS ?= emacs

EMACS_Q = $(EMACS) --quick

EMACS_BATCH = $(EMACS_Q) --batch

.PHONY: all clean clean-doc doc info texi

all: doc

clean:
	rm *.info

# Build documentation.

MAKEINFO ?= makeinfo

doc: info

texi:
	$(EMACS_BATCH) $(DOCDIR)/$(PKG).org \
		--load ox-texinfo \
		--funcall org-texinfo-export-to-texinfo

info: texi
	$(MAKEINFO) $(DOCDIR)/$(PKG).texi

# Tests (no tests exist, so just do nothing for now).

check:

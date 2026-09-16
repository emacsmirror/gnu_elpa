.POSIX:

NIX := $(shell command -v nix 2>/dev/null)
USE_NIX ?= 1

ENV_MAKE = $(MAKE) --no-print-directory
ifeq ($(USE_NIX),1)
ifeq ($(IN_NIX_SHELL),)
ifneq ($(NIX),)
# A local reference uses Git's tracked files in a checkout, and also works
# in a source archive.  Do not use path: here: it copies ignored files too.
ENV_MAKE = nix develop . --command $(MAKE) --no-print-directory USE_NIX=0
endif
endif
endif

EMACS_CMD ?= emacs
EMACS_OPTS ?= -Q --batch

SRCS = keymap-popup.el
TESTS = $(wildcard tests/*-tests.el)
BATCH = $(EMACS_CMD) $(EMACS_OPTS)

ORG = docs/keymap-popup.org
TEXI = docs/keymap-popup.texi
INFO = docs/keymap-popup.info

.PHONY: all compile do-compile test do-test lint do-lint doc do-doc clean dev do-dev load

all: compile

compile:
	@$(ENV_MAKE) do-compile

do-compile:
	@echo "Compiling $(SRCS)..."
	@$(BATCH) -f batch-byte-compile $(SRCS)

test:
	@$(ENV_MAKE) do-test

do-test:
	@echo "Testing $(TESTS)..."
	@$(BATCH) -L . -L tests -l ert $(foreach src,$(SRCS),-l $(src)) \
	  $(foreach test,$(TESTS),-l $(test)) -f ert-run-tests-batch-and-exit

lint:
	@$(ENV_MAKE) do-lint

do-lint:
	@echo "Running checkdoc..."
	@$(BATCH) --eval '(require (quote checkdoc))' \
	  --eval "(let ((diagnostics nil) (report checkdoc-create-error-function)) \
	    (let ((checkdoc-create-error-function \
	           (lambda (&rest args) (push args diagnostics) (apply report args)))) \
	      (dolist (file (quote ($(foreach src,$(SRCS),\"$(src)\")))) \
	        (checkdoc-file file))) \
	    (when diagnostics (kill-emacs 1)))"
	@echo "Running package-lint..."
	@$(BATCH) --eval '(package-initialize)' \
	  --eval '(require (quote package-lint))' \
	  -f package-lint-batch-and-exit $(SRCS)

doc:
	@$(ENV_MAKE) do-doc

do-doc: $(INFO)

$(INFO): $(ORG)
	@echo "Building $(INFO)..."
	@$(BATCH) --load org \
	  --eval "(with-current-buffer (find-file \"$(ORG)\") (org-texinfo-export-to-info))" \
	  --kill

dev:
	@$(ENV_MAKE) do-dev

do-dev: do-compile do-lint do-test

load:
	@emacsclient --eval "(progn \
	  (add-to-list (quote load-path) \"$(CURDIR)\") \
	  (load-file \"$(CURDIR)/$(SRCS)\"))" > /dev/null
	@printf "\033[32mLoaded keymap-popup into Emacs\033[0m\n"

clean:
	rm -f *.elc $(TEXI) $(INFO)

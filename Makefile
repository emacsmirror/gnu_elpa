.POSIX:

NIX := $(shell command -v nix 2>/dev/null)
USE_NIX ?= 1

ENV_MAKE = $(MAKE) --no-print-directory
ifeq ($(USE_NIX),1)
ifeq ($(IN_NIX_SHELL),)
ifneq ($(NIX),)
ENV_MAKE = nix develop path:. --command $(MAKE) --no-print-directory USE_NIX=0
endif
endif
endif

EMACS_CMD ?= emacs
EMACS_OPTS ?= -Q --batch

SRCS = keymap-popup.el
TESTS = tests/keymap-popup-tests.el
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
	@$(BATCH) -l ert -l $(SRCS) -l $(TESTS) -f ert-run-tests-batch-and-exit

lint:
	@$(ENV_MAKE) do-lint

do-lint:
	@echo "Running checkdoc..."
	@$(BATCH) --eval '(checkdoc-file "$(SRCS)")'
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
	  (when (boundp (quote keymap-popup-display-action)) \
	    (makunbound (quote keymap-popup-display-action))) \
	  (load-file \"$(CURDIR)/$(SRCS)\"))" > /dev/null
	@printf "\033[32mLoaded keymap-popup into Emacs\033[0m\n"

clean:
	rm -f *.elc $(TEXI) $(INFO)

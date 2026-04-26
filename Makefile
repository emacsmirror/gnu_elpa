.POSIX:

ifndef EMACS_CMD
GUIX := $(shell command -v guix 2>/dev/null)
ifdef GUIX
GUIX_SHELL := guix shell --pure -D -f guix.scm emacs-next --
EMACS_CMD := $(GUIX_SHELL) emacs
else
GUIX_SHELL :=
EMACS_CMD := emacs
endif
endif

GUIX_WRAP = $(if $(GUIX_SHELL),$(GUIX_SHELL) $(MAKE) --no-print-directory EMACS_CMD=emacs,$(MAKE) --no-print-directory)

SRCS = keymap-popup.el
TESTS = tests/keymap-popup-tests.el
BATCH = $(EMACS_CMD) -Q --batch

.PHONY: all compile do-compile test do-test lint do-lint clean dev load

all: compile

compile:
	@$(GUIX_WRAP) do-compile

do-compile:
	@echo "Compiling $(SRCS)..."
	@$(BATCH) -f batch-byte-compile $(SRCS)

test:
	@$(GUIX_WRAP) do-test

do-test:
	@echo "Testing $(TESTS)..."
	@$(BATCH) -l ert -l $(SRCS) -l $(TESTS) -f ert-run-tests-batch-and-exit

lint:
	@$(GUIX_WRAP) do-lint

do-lint:
	@echo "Running checkdoc..."
	@$(BATCH) --eval '(checkdoc-file "$(SRCS)")'

dev: compile lint test

load:
	@emacsclient --eval "(progn \
	  (add-to-list (quote load-path) \"$(CURDIR)\") \
	  (when (boundp (quote keymap-popup-display-action)) \
	    (makunbound (quote keymap-popup-display-action))) \
	  (load-file \"$(CURDIR)/$(SRCS)\"))" > /dev/null
	@printf "\033[32mLoaded keymap-popup into Emacs\033[0m\n"

clean:
	rm -f *.elc

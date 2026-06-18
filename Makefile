.POSIX:

NIX := $(shell command -v nix 2>/dev/null)

ENV_MAKE = $(MAKE) --no-print-directory
ifeq ($(FORGEJO_ENV_WRAPPED),)
ifneq ($(NIX),)
ENV_MAKE = nix develop path:$(CURDIR) --command env FORGEJO_ENV_WRAPPED=1 $(MAKE) --no-print-directory
endif
endif

EMACS_CMD ?= emacs

SRCS = lisp/forgejo.el lisp/forgejo-api.el lisp/forgejo-db.el \
       lisp/forgejo-filter.el lisp/forgejo-utils.el \
       lisp/forgejo-buffer.el lisp/forgejo-tl.el lisp/forgejo-view.el \
       lisp/forgejo-repo.el lisp/forgejo-issue.el lisp/forgejo-pull.el \
       lisp/forgejo-vc.el lisp/forgejo-review.el lisp/forgejo-settings.el \
       lisp/forgejo-token.el lisp/forgejo-alert.el lisp/forgejo-watch.el \
       lisp/forgejo-notification.el lisp/forgejo-ol.el

TEST_HELPERS = tests/forgejo-test-helper.el

TESTS = tests/forgejo-test-load.el tests/forgejo-test-api.el \
        tests/forgejo-test-db.el tests/forgejo-test-host.el \
        tests/forgejo-test-buffer.el tests/forgejo-test-filter.el \
        tests/forgejo-test-issue.el tests/forgejo-test-pull.el \
        tests/forgejo-test-review.el tests/forgejo-test-tl.el \
        tests/forgejo-test-vc.el tests/forgejo-test-view.el

SELECTOR ?= t
ERT_OPTS ?=

BATCH = $(EMACS_CMD) -Q --batch -L lisp -L tests

.PHONY: all compile do-compile compile-tests do-compile-tests test do-test lint do-lint clean dev do-dev load test-env

all: compile

compile:
	@$(ENV_MAKE) do-compile

do-compile:
	@for f in $(SRCS); do \
	  echo "Compiling $$f..."; \
	  $(BATCH) -l $$f -f batch-byte-compile $$f || exit 1; \
	done

compile-tests:
	@$(ENV_MAKE) do-compile-tests

do-compile-tests:
	@for f in $(TEST_HELPERS) $(TESTS); do \
	  echo "Compiling $$f..."; \
	  $(BATCH) -l ert -f batch-byte-compile $$f || exit 1; \
	done

test:
	@$(ENV_MAKE) do-compile-tests do-test

do-test:
	@for f in $(TESTS); do \
	  echo "Testing $$f..."; \
	  $(BATCH) -l ert $(ERT_OPTS) -l $$f \
	    --eval '(ert-run-tests-batch-and-exit (quote $(SELECTOR)))' || exit 1; \
	done

lint:
	@$(ENV_MAKE) do-lint

do-lint:
	@echo "Running checkdoc..."
	@for f in $(SRCS); do \
	  $(BATCH) --eval "(checkdoc-file \"$$f\")" || exit 1; \
	done

dev:
	@$(ENV_MAKE) do-dev

do-dev: do-compile do-lint do-compile-tests do-test

load: clean
	@emacsclient --eval "(progn \
	  (add-to-list 'load-path \"$(CURDIR)/lisp\") \
	  (dolist (sym '(forgejo-map forgejo-tl-list-mode-map \
	               forgejo-issue-list-mode-map forgejo-pull-list-mode-map \
	               forgejo-pull-view-mode-map forgejo-issue-view-mode-map \
	               forgejo-repo-search-mode-map forgejo-watch-list-mode-map \
	               forgejo-notification-list-mode-map \
	               forgejo-view-mode-map forgejo-view-diff-map \
	               forgejo-compose-mode-map forgejo-buffer-diff-map \
	               forgejo-vc-map forgejo-buffer-ref-map \
	               forgejo-buffer-commit-map)) \
	    (when (boundp sym) (makunbound sym))))" > /dev/null
	@for f in $(SRCS); do \
	  emacsclient --eval "(load-file \"$(CURDIR)/$$f\")" > /dev/null || \
	    printf "\033[31mFAIL\033[0m $$f\n"; \
	done
	@emacsclient --eval "(dolist (buf (buffer-list)) \
	  (with-current-buffer buf \
	    (cond ((derived-mode-p 'forgejo-issue-list-mode) \
	           (use-local-map forgejo-issue-list-mode-map)) \
	          ((derived-mode-p 'forgejo-pull-list-mode) \
	           (use-local-map forgejo-pull-list-mode-map)) \
	          ((derived-mode-p 'forgejo-pull-view-mode) \
	           (use-local-map forgejo-pull-view-mode-map)) \
	          ((derived-mode-p 'forgejo-issue-view-mode) \
	           (use-local-map forgejo-issue-view-mode-map)) \
	          ((derived-mode-p 'forgejo-notification-list-mode) \
	           (use-local-map forgejo-notification-list-mode-map)) \
	          ((and (derived-mode-p 'diff-mode) \
	                (bound-and-true-p forgejo-diff--pr-number)) \
	           (use-local-map forgejo-view-diff-map)))))" > /dev/null
	@printf "\033[32mLoaded all modules into Emacs\033[0m\n"

test-env:
	@$(BATCH) --eval "(loaddefs-generate \"$(CURDIR)/lisp\" \"$(CURDIR)/lisp/forgejo-autoloads.el\")"
	@echo "Generated autoloads. Starting clean Emacs..."
	@$(EMACS_CMD) -Q -L lisp -l forgejo-autoloads

clean:
	rm -f *.elc lisp/*.elc tests/*.elc lisp/forgejo-autoloads.el

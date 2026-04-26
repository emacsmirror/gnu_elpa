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

SRCS = lisp/forgejo.el lisp/forgejo-api.el lisp/forgejo-db.el \
       lisp/forgejo-filter.el lisp/forgejo-utils.el \
       lisp/forgejo-buffer.el lisp/forgejo-view.el lisp/forgejo-repo.el \
       lisp/forgejo-issue.el lisp/forgejo-pull.el lisp/forgejo-vc.el \
       lisp/forgejo-tl.el lisp/forgejo-review.el lisp/forgejo-settings.el \
       lisp/forgejo-alert.el lisp/forgejo-watch.el lisp/forgejo-transient.el

TESTS = tests/forgejo-test-load.el tests/forgejo-test-api.el \
        tests/forgejo-test-db.el tests/forgejo-test-host.el \
        tests/forgejo-test-buffer.el tests/forgejo-test-filter.el \
        tests/forgejo-test-issue.el tests/forgejo-test-pull.el \
        tests/forgejo-test-vc.el

BATCH = $(EMACS_CMD) -Q --batch -L lisp

.PHONY: all compile do-compile test do-test lint do-lint clean dev load

all: compile

compile:
	@$(GUIX_WRAP) do-compile

do-compile:
	@for f in $(SRCS); do \
	  echo "Compiling $$f..."; \
	  $(BATCH) -l $$f -f batch-byte-compile $$f || exit 1; \
	done

test:
	@$(GUIX_WRAP) do-test

do-test:
	@for f in $(TESTS); do \
	  echo "Testing $$f..."; \
	  $(BATCH) -l ert -l $$f -f ert-run-tests-batch-and-exit || exit 1; \
	done

lint:
	@$(GUIX_WRAP) do-lint

do-lint:
	@echo "Running checkdoc..."
	@for f in $(SRCS); do \
	  $(BATCH) --eval "(checkdoc-file \"$$f\")" || exit 1; \
	done

dev: compile lint test

load: clean
	@emacsclient --eval "(progn \
	  (add-to-list 'load-path \"$(CURDIR)/lisp\") \
	  (dolist (sym '(forgejo-issue-list-mode-map forgejo-pull-list-mode-map \
	               forgejo-pull-view-mode-map forgejo-issue-view-mode-map \
	               forgejo-repo-search-mode-map forgejo-watch-list-mode-map \
	               forgejo-buffer-diff-map \
	               forgejo-buffer-ref-map forgejo-buffer-commit-map)) \
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
	           (use-local-map forgejo-issue-view-mode-map)))))" > /dev/null
	@printf "\033[32mLoaded all modules into Emacs\033[0m\n"

clean:
	rm -f *.elc lisp/*.elc

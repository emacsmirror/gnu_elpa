.POSIX:

EMACS ?= emacs
EMACS_CMD = $(EMACS) -Q --batch -L lisp

SRCS = lisp/forgejo.el lisp/forgejo-api.el lisp/forgejo-db.el \
       lisp/forgejo-utils.el lisp/forgejo-buffer.el lisp/forgejo-repo.el \
       lisp/forgejo-issue.el lisp/forgejo-pull.el lisp/forgejo-vc.el \
       lisp/forgejo-tl.el lisp/forgejo-notification.el lisp/forgejo-transient.el

TESTS = tests/forgejo-test-load.el tests/forgejo-test-api.el \
        tests/forgejo-test-db.el tests/forgejo-test-buffer.el \
        tests/forgejo-test-issue.el tests/forgejo-test-pull.el \
        tests/forgejo-test-vc.el

.PHONY: all compile test lint clean dev load

all: compile

compile:
	@for f in $(SRCS); do \
	  echo "Compiling $$f..."; \
	  $(EMACS_CMD) -l $$f -f batch-byte-compile $$f || exit 1; \
	done

test:
	@for f in $(TESTS); do \
	  echo "Testing $$f..."; \
	  $(EMACS_CMD) -l ert -l $$f -f ert-run-tests-batch-and-exit || exit 1; \
	done

lint:
	@echo "Running checkdoc..."
	@for f in $(SRCS); do \
	  $(EMACS_CMD) --eval "(checkdoc-file \"$$f\")" || exit 1; \
	done

dev: compile lint test

load: clean
	@emacsclient --eval "(progn \
	  (add-to-list 'load-path \"$(CURDIR)/lisp\") \
	  (dolist (sym '(forgejo-issue-list-mode-map forgejo-pull-list-mode-map \
	               forgejo-pull-view-mode-map forgejo-issue-view-mode-map \
	               forgejo-repo-search-mode-map forgejo-notification-list-mode-map \
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

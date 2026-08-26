.POSIX:
.PHONY: all doc autoload autoload-smoke compile lint lint-checkdoc \
	lint-package-lint test check dev load clean

-include local.mk

EMACS ?= emacs
EMACSCLIENT ?= emacsclient
EMACS_OPTS ?= -Q --batch
ENV ?=
EXTRA_LOAD_PATH ?=

LISP_DIR := lisp
TEST_DIR := tests
LOAD_PATH := -L $(LISP_DIR) -L $(TEST_DIR) $(EXTRA_LOAD_PATH)
AUTOLOADS := $(LISP_DIR)/gnosis-autoloads.el
ORG := docs/gnosis.org
TEXI := docs/gnosis.texi
INFO := docs/gnosis.info

MODULES := gnosis-sqlite gnosis-tl gnosis-utils gnosis-org \
	gnosis-algorithm gnosis-fsrs gnosis-cloze gnosis-db gnosis-vc \
	gnosis-tags gnosis-custom-values gnosis-links gnosis-monkeytype \
	gnosis-nodes gnosis-journal gnosis gnosis-review gnosis-dashboard \
	gnosis-export-import gnosis-anki
SOURCES := $(addprefix $(LISP_DIR)/,$(addsuffix .el,$(MODULES)))
PACKAGE_LINT_SOURCES := $(LISP_DIR)/gnosis.el \
	$(filter-out $(LISP_DIR)/gnosis.el,$(SOURCES))

AUTOLOAD_COMMANDS := gnosis gnosis-add-thema gnosis-modeline-mode \
	gnosis-dashboard gnosis-export-db gnosis-import-db gnosis-save \
	gnosis-import-anki gnosis-journal-find gnosis-journal-insert \
	gnosis-journal gnosis-links-check gnosis-links-cleanup gnosis-links-sync \
	gnosis-nodes-delete-file gnosis-nodes-find gnosis-nodes-find-by-tag \
	gnosis-nodes-insert-template gnosis-nodes-insert gnosis-nodes-insert-tags \
	gnosis-nodes-visit-backlinks gnosis-nodes-db-sync \
	gnosis-nodes-db-force-sync gnosis-review gnosis-review-topic \
	gnosis-history-clear gnosis-monkeytype-start gnosis-vc-push gnosis-vc-pull

TESTS := tests/gnosis-test-sqlite.el \
	tests/gnosis-test-autoload-boundary.el \
	tests/gnosis-test-journal-boundary.el \
	tests/gnosis-test-nodes-boundary.el \
	tests/gnosis-test-fsrs.el \
	tests/gnosis-test-algorithm.el \
	tests/gnosis-test-export-import.el \
	tests/gnosis-test-dashboard.el \
	tests/gnosis-test-cloze.el \
	tests/gnosis-test-bulk-link.el \
	tests/gnosis-test-script-detection.el \
	tests/gnosis-test-insert-template.el \
	tests/gnosis-test-isolation.el \
	tests/gnosis-test-links.el \
	tests/gnosis-test-org.el \
	tests/gnosis-test-nodes.el \
	tests/gnosis-test-review.el \
	tests/gnosis-test-journal.el \
	tests/gnosis-test-migration.el \
	tests/gnosis-test-anki.el

all: check

doc: $(ORG)
	$(ENV) $(EMACS) $(EMACS_OPTS) --load org \
		--eval "(with-current-buffer (find-file \"$(ORG)\") \
		  (org-texinfo-export-to-info))"

autoload:
	rm -f $(AUTOLOADS)
	$(ENV) $(EMACS) $(EMACS_OPTS) -L $(LISP_DIR) \
		--eval "(loaddefs-generate \"$(LISP_DIR)\" \"$(AUTOLOADS)\")"

autoload-smoke: autoload
	$(ENV) $(EMACS) $(EMACS_OPTS) -L $(LISP_DIR) \
		-l gnosis-autoloads \
		--eval "(dolist (command '($(AUTOLOAD_COMMANDS))) \
		  (unless (and (autoloadp (symbol-function command)) \
		               (commandp command)) \
		    (error \"Missing command autoload: %S\" command)))"

compile: autoload
	rm -f $(LISP_DIR)/*.elc
	$(ENV) $(EMACS) $(EMACS_OPTS) $(LOAD_PATH) \
		--eval "(defun gnosis--compile-log-warning \
		  (string position fill level) \
		  (unless (and (eq level :warning) \
		               (equal string \"docstring wider than 80 characters\") \
		               (memq byte-compile-current-form \
		                 '(gnosis-dashboard-mode-map--enter-gnosis-dashboard-nodes-map \
		                   gnosis-dashboard-mode-map--enter-gnosis-dashboard-themata-map \
		                   gnosis-dashboard-mode-map--enter-gnosis-dashboard-import-export-map \
		                   gnosis-dashboard-mode-map--enter-gnosis-dashboard-maintenance-map \
		                   gnosis-dashboard-nodes-mode-map--enter-gnosis-dashboard-nodes-search-map \
		                   gnosis-dashboard-nodes-mode-map--enter-gnosis-dashboard-nodes-filter-map \
		                   gnosis-dashboard-nodes-mode-map--enter-gnosis-dashboard-nodes-sort-map))) \
		    (if (eq level :warning) \
		        (error \"%s\" string) \
		      (byte-compile--log-warning-for-byte-compile \
		       string position fill level))))" \
		--eval "(setq byte-compile-error-on-warn nil \
		              byte-compile-log-warning-function \
		              #'gnosis--compile-log-warning \
		              load-prefer-newer t)" \
		-f batch-byte-compile $(SOURCES)

test:
	@set -eu; for file in $(TESTS); do \
		tmp=$$(mktemp -d); \
		trap 'rm -rf "$$tmp"' 0 1 2 3 15; \
		echo "Running $$file..."; \
		HOME="$$tmp/home" XDG_CACHE_HOME="$$tmp/cache" \
		XDG_CONFIG_HOME="$$tmp/config" XDG_DATA_HOME="$$tmp/share" \
		XDG_STATE_HOME="$$tmp/state" GNOSIS_TEST_DIR="$$tmp/gnosis" \
		$(ENV) $(EMACS) $(EMACS_OPTS) $(LOAD_PATH) -l ert \
			--eval="(setq gnosis-dir \
			  (file-name-as-directory (getenv \"GNOSIS_TEST_DIR\")) \
			  gnosis-testing t gnosis-vc-auto-push nil \
			  load-prefer-newer t)" \
			-l "$$file" -f ert-run-tests-batch-and-exit; \
		rm -rf "$$tmp"; trap - 0 1 2 3 15; \
	done

check: compile autoload-smoke test

lint-checkdoc:
	@set -eu; for file in $(SOURCES); do \
		output=$$($(ENV) $(EMACS) $(EMACS_OPTS) -L $(LISP_DIR) \
			--eval="(progn (require 'checkdoc) \
			  (checkdoc-file \"$$file\"))" 2>&1); \
		if test -n "$$output"; then \
			printf '%s\n' "$$output"; exit 1; \
		fi; \
	done

lint-package-lint:
	@set -eu; for file in $(PACKAGE_LINT_SOURCES); do \
		$(ENV) $(EMACS) $(EMACS_OPTS) $(LOAD_PATH) \
			--eval="(package-initialize)" \
			--eval="(dolist (spec '((keymap-popup (0 4 3)) \
			                         (compat (31 0 0 2)))) \
			  (push (list (car spec) \
			    (package-desc-create :name (car spec) \
			      :version (cadr spec) :summary \"Nix dependency\" \
			      :reqs nil :kind 'dir :archive \"nix\")) \
			    package-alist))" \
			--eval="(require 'package-lint)" \
			--eval="(setq package-lint-main-file \
			  \"$(LISP_DIR)/gnosis.el\")" \
			--eval="(unless \
			  (cl-letf (((symbol-function 'package-initialize) #'ignore)) \
			    (package-lint-batch-and-exit-1 (list \"$$file\"))) \
			  (kill-emacs 1))"; \
	done

lint: lint-checkdoc lint-package-lint

dev: lint check

load:
	rm -f $(LISP_DIR)/*.elc
	@$(EMACSCLIENT) -e "(progn \
	  (add-to-list 'load-path \"$(CURDIR)/$(LISP_DIR)\") \
	  (dolist (symbol '(gnosis-dashboard-common-map \
	                   gnosis-dashboard-themata-mode-map \
	                   gnosis-dashboard-tags-mode-map \
	                   gnosis-dashboard-mode-map \
	                   gnosis-dashboard-nodes-mode-map \
	                   gnosis-dashboard-nodes-sort-map \
	                   gnosis-dashboard-nodes-search-map \
	                   gnosis-dashboard-nodes-filter-map \
	                   gnosis-dashboard-nodes-map \
	                   gnosis-dashboard-themata-map \
	                   gnosis-dashboard-import-export-map \
	                   gnosis-dashboard-maintenance-map \
	                   gnosis-import-diff-mode-map \
	                   gnosis-review-map)) \
	    (when (boundp symbol) (makunbound symbol))) \
	  (dolist (file '($(SOURCES))) \
	    (load-file (expand-file-name (symbol-name file) \"$(CURDIR)\"))) \
	  (dolist (buffer (buffer-list)) \
	    (with-current-buffer buffer \
	      (cond ((derived-mode-p 'gnosis-dashboard-themata-mode) \
	             (use-local-map gnosis-dashboard-themata-mode-map)) \
	            ((derived-mode-p 'gnosis-dashboard-tags-mode) \
	             (use-local-map gnosis-dashboard-tags-mode-map)) \
	            ((derived-mode-p 'gnosis-dashboard-nodes-mode) \
	             (use-local-map gnosis-dashboard-nodes-mode-map)) \
	            ((derived-mode-p 'gnosis-dashboard-mode) \
	             (use-local-map gnosis-dashboard-mode-map))))))" \
	  > /dev/null
	@echo "Loaded $(words $(SOURCES)) files."

clean:
	rm -f $(TEXI) $(INFO) $(AUTOLOADS) \
		$(LISP_DIR)/*.elc $(TEST_DIR)/*.elc *-pkg.el*

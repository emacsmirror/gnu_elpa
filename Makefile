.POSIX:

# Only the suite scheduler may fan out.  Serialize public goals and build
# phases even when the caller supplies -j; workers still honor JOBS below.
ifneq ($(MAKECMDGOALS),_test-summary)
.NOTPARALLEL:
endif
.PHONY: all help test-canvas _test-canvas _test-canvas-ert \
	doc autoload autoload-smoke compile lint lint-checkdoc \
	lint-package-lint test check dev load clean \
	_doc _autoload _autoload-smoke _compile _lint _lint-checkdoc \
	_lint-package-lint _test _test-summary _check _dev

-include local.mk

EMACS ?= emacs
EMACSCLIENT ?= emacsclient
EMACS_OPTS ?= -Q --batch
ENV ?=
EXTRA_LOAD_PATH ?=
NIX ?= nix
NIX_FLAGS ?= --no-write-lock-file
GNOSIS_ENV_WRAPPED ?=
JOBS ?= $(shell nproc 2>/dev/null || printf '4')
TEST_RESULTS := .test-results
ERT_REQUIRED_TESTS ?=
CANVAS_PYTHON ?= $(CURDIR)/optional/canvas-3d/.venv/bin/python

LISP_DIR := lisp
TEST_DIR ?= tests
LOAD_PATH := -L $(LISP_DIR) -L $(TEST_DIR) -L tests/tooling $(EXTRA_LOAD_PATH)
AUTOLOADS := $(LISP_DIR)/gnosis-autoloads.el
ORG := docs/gnosis.org
TEXI := docs/gnosis.texi
INFO := docs/gnosis.info

MODULES := gnosis-sqlite gnosis-tl gnosis-utils gnosis-answer gnosis-org \
	gnosis-logical-day gnosis-fsrs gnosis-cloze gnosis-db gnosis-assets gnosis-backup gnosis-model gnosis-image gnosis-lecture gnosis-scheduler gnosis-vc \
	gnosis-tags gnosis-links gnosis-monkeytype \
	gnosis-nodes gnosis-journal gnosis gnosis-study gnosis-review gnosis-agent-eval gnosis-agent-eval-hermes gnosis-agent gnosis-dashboard \
	gnosis-export-import gnosis-anki
SOURCES := $(addprefix $(LISP_DIR)/,$(addsuffix .el,$(MODULES)))
PACKAGE_LINT_SOURCES := $(LISP_DIR)/gnosis.el \
	$(filter-out $(LISP_DIR)/gnosis.el,$(SOURCES))

AUTOLOAD_COMMANDS := gnosis-lecture-attach gnosis-lecture-cancel gnosis gnosis-add-thema gnosis-add-model-thema gnosis-add-model-name-thema gnosis-add-image-thema gnosis-add-thema-from-node \
	gnosis-modeline-mode \
	gnosis-dashboard gnosis-export-db gnosis-import-db gnosis-save \
	gnosis-import-anki gnosis-journal-find gnosis-journal-insert \
	gnosis-journal gnosis-journal-date gnosis-journal-previous \
	gnosis-journal-next gnosis-journal-capture gnosis-journal-add-todo gnosis-journal-insert-template \
	gnosis-journal-insert-task \
	gnosis-journal-complete-task gnosis-journal-study gnosis-study-day \
	gnosis-links-check gnosis-links-cleanup gnosis-links-sync \
	gnosis-nodes-delete-file gnosis-nodes-find gnosis-nodes-find-by-tag \
	gnosis-nodes-insert-template gnosis-nodes-insert gnosis-nodes-insert-tags \
	gnosis-nodes-visit-backlinks gnosis-nodes-db-sync \
	gnosis-nodes-db-force-sync gnosis-review gnosis-review-topic \
	gnosis-practice-topic gnosis-review-due-topic gnosis-study-topic \
	gnosis-study-repair gnosis-study-subtree gnosis-backup-db \
	gnosis-backup-data gnosis-backup-verify gnosis-backup-restore \
	gnosis-review-resume gnosis-review-discard gnosis-review-continue \
	gnosis-review-undo gnosis-scheduler-set-retention gnosis-study-history-audit \
	gnosis-monkeytype-start gnosis-vc-push gnosis-vc-pull

TEST_SUPPORT := $(TEST_DIR)/gnosis-test-db.el \
	$(TEST_DIR)/gnosis-test-helpers.el $(TEST_DIR)/gnosis-test-schema-v8.el
TESTS := $(filter-out $(TEST_SUPPORT), \
	$(wildcard $(TEST_DIR)/gnosis-test-*.el))
TEST_STAMPS := $(patsubst $(TEST_DIR)/%.el,$(TEST_RESULTS)/%.stamp,$(TESTS))

all: check

help:
	@printf '%s\n' 'dev: lint, compile, autoload, core ERT and manual gates' \
		'test: core ERT (JOBS=N, TESTS="tests/gnosis-test-NAME.el")' \
		'test-canvas: opt-in Python/EGL and canvas/model ERT; no installation' \
		'  Set EMACS, CANVAS_PYTHON and EXTRA_LOAD_PATH for prepared tools.' \
		'  Ordinary test/check/dev do not require Python, EGL or native canvas.'

doc autoload autoload-smoke compile lint lint-checkdoc lint-package-lint \
test test-canvas check dev:
	@if test -z "$(GNOSIS_ENV_WRAPPED)" && test -z "$$IN_NIX_SHELL" \
		&& command -v "$(NIX)" >/dev/null 2>&1; then \
		exec "$(NIX)" develop $(NIX_FLAGS) --command \
			$(MAKE) GNOSIS_ENV_WRAPPED=1 _$@; \
	else \
		exec $(MAKE) GNOSIS_ENV_WRAPPED=1 _$@; \
	fi

_doc: $(ORG)
	$(ENV) $(EMACS) $(EMACS_OPTS) --load org \
		--eval "(with-current-buffer (find-file \"$(ORG)\") \
		  (org-texinfo-export-to-info))"

_autoload:
	rm -f $(AUTOLOADS)
	$(ENV) $(EMACS) $(EMACS_OPTS) -L $(LISP_DIR) \
		--eval "(loaddefs-generate \"$(LISP_DIR)\" \"$(AUTOLOADS)\")"

_autoload-smoke: _autoload
	$(ENV) $(EMACS) $(EMACS_OPTS) -L $(LISP_DIR) \
		-l gnosis-autoloads \
		--eval "(dolist (command '($(AUTOLOAD_COMMANDS))) \
		  (unless (and (autoloadp (symbol-function command)) \
		               (commandp command)) \
		    (error \"Missing command autoload: %S\" command)))"

_compile: _autoload
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

_test: _autoload

_test _test-canvas-ert:
	@rm -rf $(TEST_RESULTS)
	@mkdir -p $(TEST_RESULTS)
	@$(MAKE) --no-print-directory -j$(JOBS) -Otarget _test-summary

$(TEST_RESULTS)/%.stamp: $(TEST_DIR)/%.el
	@tmp=$$(mktemp -d); log="$(TEST_RESULTS)/$*.log"; \
	receipt="$(CURDIR)/$(TEST_RESULTS)/$*.receipt"; n=0; status=FAIL; \
	rm -f "$$receipt"; \
	trap 'rm -rf "$$tmp"' 0 1 2 3 15; \
	mkdir -p "$$tmp/home" "$$tmp/cache" "$$tmp/config" \
		"$$tmp/share" "$$tmp/state" "$$tmp/gnosis"; \
	if HOME="$$tmp/home" XDG_CACHE_HOME="$$tmp/cache" \
		XDG_CONFIG_HOME="$$tmp/config" XDG_DATA_HOME="$$tmp/share" \
		XDG_STATE_HOME="$$tmp/state" GNOSIS_TEST_DIR="$$tmp/gnosis" \
		GNOSIS_TEST_RECEIPT="$$receipt" \
		$(ENV) $(EMACS) $(EMACS_OPTS) $(LOAD_PATH) -l ert \
			--eval="(setq gnosis-dir \
			  (file-name-as-directory (getenv \"GNOSIS_TEST_DIR\")) \
			  gnosis-testing t gnosis-vc-auto-push nil \
			  load-prefer-newer t)" \
			--eval="(require 'gnosis-tooling-runner)" \
			--eval="(require '$*)" \
			--eval="(gnosis-tooling-run-tests '($(ERT_REQUIRED_TESTS)))" > "$$log" 2>&1; then \
		exited=0; \
	else \
		exited=1; \
	fi; \
	if test -f "$$receipt" && test "$$(wc -l < "$$receipt")" -eq 1 \
		&& ! LC_ALL=C grep -qvx 'completed [0-9][0-9]*' "$$receipt"; then \
		read completed n < "$$receipt"; \
		if test "$$exited" -eq 0; then status=OK; fi; \
	else \
		printf '%s\n' 'Missing or invalid ERT completion receipt' >> "$$log"; \
	fi; \
	if test "$$status" = OK; then \
		printf '  OK %s (%s tests)\n' "$<" "$${n:-0}"; \
		if test -n "$(ERT_REQUIRED_TESTS)"; then \
			while IFS= read -r line; do printf '%s\n' "$$line"; done < "$$log"; \
		fi; \
		rm -f "$$log"; \
	else \
		printf 'FAIL %s (%s tests)\n' "$<" "$${n:-0}"; \
		while IFS= read -r line; do printf '%s\n' "$$line"; done < "$$log"; \
	fi; \
	printf '%s %s\n' "$$status" "$${n:-0}" > "$@"

_test-summary: $(TEST_STAMPS)
	@total=0; passed=0; failed=0; failed_files=""; \
	for stamp in $(TEST_STAMPS); do \
		read status n < "$$stamp"; total=$$((total + n)); \
		if test "$$status" = FAIL; then \
			failed=$$((failed + 1)); \
			failed_files="$$failed_files $(TEST_DIR)/$$(basename "$$stamp" .stamp).el"; \
		else \
			passed=$$((passed + 1)); \
		fi; \
	done; \
	printf '%s tests across %s files: %s passed, %s failed\n' \
		"$$total" "$(words $(TEST_STAMPS))" "$$passed" "$$failed"; \
	if test "$$failed" -eq 0; then \
		rm -rf $(TEST_RESULTS); \
	else \
		printf 'Failed files:%s\nLogs preserved in $(TEST_RESULTS)/\n' \
			"$$failed_files"; \
	fi; \
	test "$$failed" -eq 0

# Keep native rendering entirely outside the core verification dependency set.
_test-canvas:
	@cd optional/canvas-3d && $(ENV) "$(CANVAS_PYTHON)" -B \
		-m unittest -v test_protocol test_render test_geometry
	@GNOSIS_CANVAS_PYTHON="$(CANVAS_PYTHON)" $(MAKE) GNOSIS_ENV_WRAPPED=1 \
		TEST_DIR=tests/tooling TESTS=tests/tooling/gnosis-tooling-canvas.el \
		EXTRA_LOAD_PATH="$(EXTRA_LOAD_PATH) -L optional/canvas-3d" \
		ERT_REQUIRED_TESTS=canvas-3d-model-translated-highlight-is-pickable _test-canvas-ert

_check: _compile _autoload-smoke _test

_lint-checkdoc:
	@set -eu; for file in $(SOURCES); do \
		output=$$($(ENV) $(EMACS) $(EMACS_OPTS) -L $(LISP_DIR) \
			--eval="(progn (require 'checkdoc) \
			  (checkdoc-file \"$$file\"))" 2>&1); \
		if test -n "$$output"; then \
			printf '%s\n' "$$output"; exit 1; \
		fi; \
	done

_lint-package-lint:
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

_lint: _lint-checkdoc _lint-package-lint

_dev: _lint _check _doc

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
	rm -rf $(TEST_RESULTS)

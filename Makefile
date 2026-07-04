.POSIX:
.PHONY: all doc do-doc compile do-compile test do-test load clean

NIX := $(shell command -v nix 2>/dev/null)

# Public targets re-enter make through the nix dev shell so builds and
# tests run against the flake's isolated dependencies; do-* targets do
# the real work and expect Emacs plus deps on PATH.
ENV_MAKE = $(MAKE) --no-print-directory
ifeq ($(GNOSIS_ENV_WRAPPED),)
ifneq ($(NIX),)
ENV_MAKE = nix develop --no-write-lock-file path:$(CURDIR) --command env GNOSIS_ENV_WRAPPED=1 $(MAKE) --no-print-directory
endif
endif

EMACS ?= emacs
ORG := docs/gnosis.org
TEXI := docs/gnosis.texi
INFO := docs/gnosis.info
LISP_DIR := lisp
TEST_FILES := tests/gnosis-test-sqlite.el \
	tests/gnosis-test-algorithm.el \
	tests/gnosis-test-custom-values.el \
	tests/gnosis-test-export-import.el \
	tests/gnosis-test-dashboard.el \
	tests/gnosis-test-cloze.el \
	tests/gnosis-test-bulk-link.el \
	tests/gnosis-test-script-detection.el \
	tests/gnosis-test-insert-template.el \
	tests/gnosis-test-org.el \
	tests/gnosis-test-nodes.el \
	tests/gnosis-test-review.el \
	tests/gnosis-test-journal.el \
	tests/gnosis-test-migration.el \
	tests/gnosis-test-anki.el \

all: doc

doc:
	@$(ENV_MAKE) do-doc

do-doc: $(ORG)
	$(EMACS) --batch \
	-Q \
	--load org \
	--eval "(with-current-buffer (find-file \"$(ORG)\") (org-texinfo-export-to-info))" \
	--kill

compile:
	@$(ENV_MAKE) do-compile

# docstrings-wide is excluded: keymap-popup-define generates launcher
# docstrings wider than 80 columns.
do-compile:
	rm -f $(LISP_DIR)/*.elc
	$(EMACS) --batch \
	-q \
	--eval "(setq byte-compile-error-on-warn t)" \
	--eval "(setq byte-compile-warnings '(not docstrings-wide))" \
	-L $(LISP_DIR) \
	-f batch-byte-compile $(LISP_DIR)/*.el

test:
	@$(ENV_MAKE) do-compile do-test

do-test:
	rm -f $(LISP_DIR)/*.elc
	@set -e; for f in $(TEST_FILES); do \
		echo "Running $$f..."; \
		$(EMACS) --batch \
		-q \
		--eval "(add-to-list 'load-path \"$(shell pwd)/$(LISP_DIR)\")" \
		--load $$f; \
	done

EL_FILES := gnosis-sqlite.el gnosis-tl.el gnosis-utils.el gnosis-org.el \
	gnosis-algorithm.el gnosis-cloze.el gnosis-db.el gnosis-vc.el \
	gnosis-tags.el gnosis-custom-values.el gnosis-links.el \
	gnosis.el gnosis-nodes.el gnosis-journal.el \
	gnosis-review.el gnosis-dashboard.el gnosis-export-import.el \
	gnosis-anki.el gnosis-monkeytype.el

load:
	rm -f $(LISP_DIR)/*.elc
	@emacsclient -e "(progn \
	  (dolist (sym '(gnosis-dashboard-common-map \
	               gnosis-dashboard-themata-mode-map \
	               gnosis-dashboard-tags-mode-map \
	               gnosis-dashboard-mode-map \
	               gnosis-dashboard-nodes-mode-map \
	               gnosis-dashboard-nodes-sort-map \
	               gnosis-dashboard-nodes-search-map \
	               gnosis-dashboard-nodes-filter-map \
	               gnosis-dashboard-nodes-map \
	               gnosis-dashboard-themata-map \
	               gnosis-import-diff-mode-map \
	               gnosis-review-map)) \
	    (when (boundp sym) (makunbound sym))) \
	  (dolist (sym '(gnosis-dashboard-menu \
	               gnosis-dashboard-themata-mode-menu \
	               gnosis-dashboard-tags-mode-menu \
	               gnosis-dashboard-nodes-mode-menu \
	               gnosis-dashboard-menu-nodes \
	               gnosis-dashboard-menu-themata \
	               gnosis-dashboard-nodes-sort-menu \
	               gnosis-dashboard-nodes-search-menu \
	               gnosis-dashboard-nodes-filter-menu \
	               gnosis-import-diff-menu)) \
	    (when (fboundp sym) (fmakunbound sym))))" > /dev/null
	@for f in $(EL_FILES); do \
		emacsclient -e "(load-file \"$(shell pwd)/$(LISP_DIR)/$$f\")" > /dev/null; \
	done
	@emacsclient -e "(dolist (buf (buffer-list)) \
	  (with-current-buffer buf \
	    (cond ((derived-mode-p 'gnosis-dashboard-themata-mode) \
	           (use-local-map gnosis-dashboard-themata-mode-map)) \
	          ((derived-mode-p 'gnosis-dashboard-tags-mode) \
	           (use-local-map gnosis-dashboard-tags-mode-map)) \
	          ((derived-mode-p 'gnosis-dashboard-nodes-mode) \
	           (use-local-map gnosis-dashboard-nodes-mode-map)) \
	          ((derived-mode-p 'gnosis-dashboard-mode) \
	           (use-local-map gnosis-dashboard-mode-map)))))" > /dev/null
	@echo "Loaded $(words $(EL_FILES)) files."

clean:
	rm -f $(TEXI) $(INFO) $(LISP_DIR)/*.elc *-pkg.el*

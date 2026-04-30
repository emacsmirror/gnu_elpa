.POSIX:
.PHONY: all doc test load clean

EMACS = emacs
GUIX_SHELL ?= guix shell -m manifest.scm --
ORG := docs/gnosis.org
TEXI := docs/gnosis.texi
INFO := docs/gnosis.info
TEST_FILES := tests/gnosis-test-sqlite.el \
	tests/gnosis-test-algorithm.el \
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

doc:	$(ORG)
	$(GUIX_SHELL) $(EMACS) --batch \
	-Q \
	--load org \
	--eval "(with-current-buffer (find-file \"$(ORG)\") (org-texinfo-export-to-info))" \
	--kill


test:
	rm -f *.elc
	@for f in $(TEST_FILES); do \
		echo "Running $$f..."; \
		$(GUIX_SHELL) $(EMACS) --batch \
		-q \
		--eval "(add-to-list 'load-path \"$(shell pwd)\")" \
		--load $$f; \
	done

EL_FILES := gnosis-sqlite.el gnosis-tl.el gnosis-utils.el gnosis-org.el \
	gnosis-algorithm.el gnosis-cloze.el gnosis-db.el gnosis-vc.el \
	gnosis-tags.el gnosis-custom-values.el gnosis-links.el \
	gnosis.el gnosis-nodes.el gnosis-journal.el \
	gnosis-review.el gnosis-dashboard.el gnosis-export-import.el \
	gnosis-anki.el gnosis-monkeytype.el

load:
	rm -f *.elc
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
		emacsclient -e "(load-file \"$(shell pwd)/$$f\")" > /dev/null; \
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
	rm -f $(TEXI) $(INFO) *.elc *-pkg.el*

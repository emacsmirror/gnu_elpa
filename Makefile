.POSIX:
.PHONY: all doc clean
.SUFFIXES: .el .elc

EMACS = emacs
GUIX_SHELL ?= guix shell -m manifest.scm --
ORG := doc/gnosis.org
TEXI := doc/gnosis.texi
INFO := doc/gnosis.info
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
	--eval "(with-current-buffer (find-file \"$(ORG)\") (org-texinfo-export-to-texinfo) (org-texinfo-export-to-info) (save-buffer))" \
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
	gnosis-algorithm.el gnosis.el gnosis-nodes.el gnosis-journal.el \
	gnosis-review.el gnosis-dashboard.el gnosis-export-import.el \
	gnosis-anki.el gnosis-monkeytype.el

load:
	rm -f *.elc
	@for f in $(EL_FILES); do \
		emacsclient -e "(load-file \"$(shell pwd)/$$f\")" > /dev/null; \
	done
	@echo "Loaded $(words $(EL_FILES)) files."

clean:
	rm -f $(TEXI) $(INFO) *.elc *-pkg.el*

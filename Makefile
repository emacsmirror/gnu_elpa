.POSIX:
.PHONY: all doc clean test test-parsing test-db
.SUFFIXES: .el .elc

EMACS = emacs
ORG := doc/org-gnosis.org
TEXI := doc/org-gnosis.texi
INFO := doc/org-gnosis.info

all: doc

doc:	$(ORG)
	$(EMACS) --batch \
	-Q \
	--load org \
	--eval "(with-current-buffer (find-file \"$(ORG)\") (org-texinfo-export-to-texinfo) (org-texinfo-export-to-info) (save-buffer))" \
	--kill

test: test-parsing test-db

test-parsing:
	guix shell -m manifest.scm -- $(EMACS) --batch \
	--eval "(add-to-list 'load-path \".\")" \
	-l ert \
	-l tests/org-gnosis-test-parsing.el \
	-f ert-run-tests-batch-and-exit

test-db:
	guix shell -m manifest.scm -- $(EMACS) --batch \
	--eval "(add-to-list 'load-path \".\")" \
	-l ert \
	-l tests/org-gnosis-test-db.el \
	-f ert-run-tests-batch-and-exit

clean:
	rm -f $(TEXI) $(INFO) *pkg* *autoloads*

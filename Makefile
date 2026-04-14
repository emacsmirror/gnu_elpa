EMACS ?= emacs

.PHONY: compile test clean

compile:
	$(EMACS) --batch -f batch-byte-compile trust-manager.el

test:
	$(EMACS) --batch -L . -l trust-manager-tests.el -f ert-run-tests-batch-and-exit

clean:
	rm -f *.elc

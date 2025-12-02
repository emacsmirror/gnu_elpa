EMACS ?= emacs
TEST_FILE := tests/tex-paren-tests.el

.PHONY: check
check:
	$(EMACS) -Q --batch -L . -l $(TEST_FILE) -f ert-run-tests-batch-and-exit

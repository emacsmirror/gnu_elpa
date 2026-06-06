# -*- indent-tabs-mode: t; -*-

EMACS := emacs
ERT_SELECTOR := (member                               \
		 isbn-tests-list->string              \
		 isbn-tests-string->list              \
		 isbn-tests-valid-p                   \
		 isbn-tests-convert                   \
		 isbn-tests-hyphenation-ranges-update \
		 isbn-tests-hyphenate                 \
)

.PHONY: test
test:
	@$(EMACS) -batch -l ert \
		-l isbn.el -l test/isbn-tests.el \
		-eval '(ert-run-tests-batch-and-exit (quote $(ERT_SELECTOR)))'

# -*- indent-tabs-mode: t; -*-

EMACS := emacs
ERT_SELECTOR := (member                              \
                 isbn-test:list->string              \
                 isbn-test:string->list              \
                 isbn-test:valid-p                   \
                 isbn-test:convert                   \
                 isbn-test:hyphenation-ranges-update \
                 isbn-test:hyphenate                 \
)

.PHONY: test
test:
	@$(EMACS) -batch -l ert \
		-l isbn.el -l test/isbn-test.el \
		-eval '(ert-run-tests-batch-and-exit (quote $(ERT_SELECTOR)))'

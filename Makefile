# Not much to see here.

EMACSBIN = emacs
EMACS = $(EMACSBIN) --batch

test:
	$(EMACS) -L . -l related-file-tests -f ert-run-tests-batch

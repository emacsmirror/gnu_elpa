# Not much to see here.

EMACSBIN = emacs
EMACS = $(EMACSBIN) --batch

test:
	$(EMACS) -L . -l futur-tests -f ert-run-tests-batch

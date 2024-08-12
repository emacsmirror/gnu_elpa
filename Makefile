EMACS ?= emacs

SOURCE=$(wildcard *.el)
TESTSOURCE=$(wildcard test/*.el)
TARGET=$(patsubst %.el,%.elc,$(SOURCE))
TESTTARGET=$(patsubst %.el,%.elc,$(TESTSOURCE))

.PHONY: check clean
.PRECIOUS: %.elc

%.elc: %.el
	@$(EMACS) -Q -batch -L . -f batch-byte-compile $<

build: $(TARGET)

check: build $(TESTTARGET)
	emacs -Q --batch -L . -l $(TESTSOURCE) -f ert-run-tests-batch-and-exit

clean:
	-rm -f $(TARGET) $(TESTTARGET)

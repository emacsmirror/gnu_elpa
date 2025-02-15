EMACS ?= emacs
MAKEINFO ?= makeinfo

SOURCE=$(wildcard *.el)
TESTSOURCE=$(wildcard test/*.el)
TARGET=$(filter-out debbugs-pkg.elc,$(patsubst %.el,%.elc,$(SOURCE)))
TESTTARGET=$(patsubst %.el,%.elc,$(TESTSOURCE))

INFOMANUALS=debbugs.info debbugs-ug.info

.PHONY: all build check clean
.PRECIOUS: %.elc

%.elc: %.el
	@$(EMACS) -Q -batch -L . -f batch-byte-compile $<

%.info: %.texi
	$(MAKEINFO) --error-limit=0 --no-split $< -o $@

all: build doc

doc: $(INFOMANUALS)

build: $(TARGET)

check: build $(TESTTARGET)
	@$(EMACS) -Q --batch -L . -l $(TESTSOURCE) -f ert-run-tests-batch-and-exit

clean:
	-rm -f $(TARGET) $(TESTTARGET) $(INFOMANUALS)

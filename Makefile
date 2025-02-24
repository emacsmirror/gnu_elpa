EMACS ?= emacs
MAKEINFO ?= makeinfo

# regex of tests to run
TESTS=.*

SOURCE=$(wildcard *.el)
TESTSOURCE=$(wildcard test/*.el)
TARGET=$(filter-out debbugs-pkg.elc,$(patsubst %.el,%.elc,$(SOURCE)))
TESTTARGET=$(patsubst %.el,%.elc,$(TESTSOURCE))

INFOMANUALS=debbugs.info debbugs-ug.info

.PHONY: all build check clean checkdoc
.PRECIOUS: %.elc

%.elc: %.el
	@$(EMACS) -Q -batch -L . -f batch-byte-compile $<

test/%.elc: test/%.el
	@$(EMACS) -Q -batch -L . -L ./test -f batch-byte-compile $<

%.info: %.texi
	$(MAKEINFO) --error-limit=0 --no-split $< -o $@

all: build doc

doc: $(INFOMANUALS)

build: $(TARGET)

checkdoc: $(SOURCE) $(TESTSOURCE)
	@$(EMACS) -Q --batch -l resources/debbugs-checkdoc-config.el $(foreach file,$^,"--eval=(checkdoc-file \"$(file)\")")

check: build $(TESTTARGET)
	@$(EMACS) -Q --batch -L . -L ./test $(foreach file,$(TESTSOURCE), -l $(file)) --eval '(ert-run-tests-batch-and-exit "$(TESTS)")'

clean:
	-rm -f $(TARGET) $(TESTTARGET) $(INFOMANUALS)

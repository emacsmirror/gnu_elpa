EMACS ?= emacs -Q -L .

all: autoloads.el minimail.elc

%.elc: %.el
	$(EMACS) --batch -f batch-byte-compile $<

autoloads.el: $(EL_FILES)
	$(EMACS) --batch --eval '(loaddefs-generate "." "autoloads.el")'

clean:
	rm -f autoloads.el *.elc

test: all
	$(EMACS) -batch -l ert -l minimail-tests.el -f ert-run-tests-batch-and-exit

.PHONY: all clean test

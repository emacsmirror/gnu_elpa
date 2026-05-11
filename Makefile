EMACS ?= emacs
HOMEDIR ?= emacs-init/$(EMACS)
BATCH = $(EMACS) --batch --init-directory='$(HOMEDIR)' -f package-initialize

EL = minimail.el
ELC = $(EL:.el=.elc)

all: $(ELC)

$(HOMEDIR):
	mkdir -p '$(HOMEDIR)'
	$(BATCH) --eval '(package-refresh-contents)' \
                 --eval '(package-install-file "minimail.el")'

%.elc: %.el $(HOMEDIR)
	$(BATCH) -f batch-byte-compile $<

clean:
	rm -f $(ELC)

extraclean: clean
	rm -rf emacs-init

test: all
	$(BATCH) -l ert -L . -l minimail-tests.el -f ert-run-tests-batch-and-exit

test-all: all
	$(MAKE) EMACS=emacs-31 clean test
	$(MAKE) EMACS=emacs-30 clean test

interact: $(ELC)
	$(EMACS) --init-directory '$(HOMEDIR)' -l minimail.elc

.PHONY: all clean extraclean test test-all

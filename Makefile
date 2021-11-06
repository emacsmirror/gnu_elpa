EMACS =		emacs

.PHONY: all compile clean

all: compile

compile: algol-mode.elc

clean:
	rm -f *.elc

.SUFFIXES: .el .elc
.el.elc:
	${EMACS} -Q --batch -L . -f batch-byte-compile $<

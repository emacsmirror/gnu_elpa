.POSIX:
.PHONY: all doc clean
.SUFFIXES: .el .elc

EMACS = emacs
ORG := doc/org-gnosis.org
TEXI := doc/org-gnosis.texi
INFO := doc/org-gnosis.info

all: doc

doc:	$(ORG)
	$(EMACS) --batch \
	-Q \
	--load org \
	--eval "(with-current-buffer (find-file \"$(ORG)\") (org-texinfo-export-to-texinfo) (org-texinfo-export-to-info) (save-buffer))" \
	--kill

clean:
	rm -f $(TEXI) $(INFO)

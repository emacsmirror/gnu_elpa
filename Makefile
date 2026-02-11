#### Makefile for the Emacs Lisp Introduction manual

# Copyright (C) 1994-2026  Free Software Foundation, Inc.

# This file is part of GNU Emacs.

# GNU Emacs is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# GNU Emacs is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.


SHELL = /bin/sh

srcdir = .

infodir = .
# Directory with the (customized) texinfo.tex file.
texinfodir = .

INFO_SOURCES = ${srcdir}/emacs-lisp-intro.texi
INFO_SOURCES_ES = ${srcdir}/emacs-lisp-intro-es.texi
# The file name eintr must fit within 5 characters, to allow for
# -NN extensions to fit into DOS 8+3 limits without clashing
INFO_TARGETS = ${infodir}/eintr
DVI_TARGETS = emacs-lisp-intro-es.dvi

PO2TEXI = dotexi.sh
MAKEINFO = makeinfo  --no-split
TEXI2DVI = texi2dvi
TEXI2PDF = texi2pdf
TEXI2HTML = texi2html
PO4ATRANSLATE = po4a-translate
DVIPS = dvips

.SUFFIXES: .dvi .ps .texi

info: $(INFO_TARGETS)

dvi: $(DVI_TARGETS)

${infodir}/eintr: ${INFO_SOURCES_ES}
	cd $(srcdir); $(MAKEINFO) emacs-lisp-intro-es.texi -o $(infodir)/eintr

emacs-lisp-intro-es.texi: ${INFO_SOURCES}
	cd $(srcdir); ./$(PO2TEXI)

emacs-lisp-intro-es.dvi: ${INFO_SOURCES_ES}
	$(TEXI2DVI) -I $(srcdir) -I $(texinfodir) $(srcdir)/emacs-lisp-intro-es.texi

emacs-lisp-intro.dvi: ${INFO_SOURCES}
	$(TEXI2DVI) -I $(srcdir) -I $(texinfodir) $(srcdir)/emacs-lisp-intro.texi

emacs-lisp-intro-es.pdf: ${INFO_SOURCES_ES}
	$(TEXI2PDF) -I $(srcdir) -I $(texinfodir) $(srcdir)/emacs-lisp-intro-es.texi

emacs-lisp-intro.pdf: ${INFO_SOURCES}
	$(TEXI2PDF) -I $(srcdir) -I $(texinfodir) $(srcdir)/emacs-lisp-intro.texi

emacs-lisp-intro-es.html: $(INFO_SOURCES_ES)
	$(TEXI2HTML) $(srcdir)/emacs-lisp-intro-es.texi

emacs-lisp-intro.html: $(INFO_SOURCES_ES)
	$(TEXI2HTML) $(srcdir)/emacs-lisp-intro.texi

.dvi.ps:
	$(DVIPS) $< -o $@

mostlyclean:
	rm -f  *.log *.cp *.fn *.ky *.pg *.vr *.tp *.tmp* *.aux *.cps *.toc

clean: mostlyclean
	rm -f *.dvi *.html *.info emacs-lisp-intro-es.texi emacs-lisp-intro-es.pdf

distclean: clean

maintainer-clean: distclean
	rm -f *.aux *.cps *.fns *.kys *.pgs *.vrs *.toc *.tmp*
	cd $(infodir); rm -f eintr eintr-[1-9]

install: emacs-lisp-intro-es.html emacs-lisp-intro-es.pdf
	mkdir -p $(DESTDIR)/usr/share/doc/elisp-es/html
	mkdir -p $(DESTDIR)/usr/share/doc/elisp-es/pdf
	cp emacs-lisp-intro-es.html $(DESTDIR)/usr/share/doc/elisp-es/html
	cp emacs-lisp-intro-es.pdf $(DESTDIR)/usr/share/doc/elisp-es/pdf

uninstall:
	rm -rf $(DESTDIR)/usr/share/doc/elisp-es/

# Tell versions [3.59,3.63) of GNU make to not export all variables.
# Otherwise a system limit (for SysV at least) may be exceeded.
.NOEXPORT:

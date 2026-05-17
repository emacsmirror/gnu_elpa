# Makefile --- for building the ffs manual

# Copyright (c) 2026 Free Software Foundation, Inc.
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

### Commentary:

# The Makefile is used for exporting ffs's manual from Org to Texinfo,
# and from Texinfo to HTML.

# make all/doc  Generate various formats of the ffs manual from Org
# make clean    Delete generated ffs.texi, ffs.info, ffs.html files

### Code:

.POSIX:
.PHONY: all doc clean
.SUFFIXES: .org .texi .info .html

EMACS = emacs
MAKEINFO = makeinfo
MAKEINFO_html_opts = --html --no-split --css-ref="$(CSS_REF)"
DOC = ffs.texi ffs.info ffs.html

CSS_REF = ../../dek.css?v=20260516

all: doc
doc: $(DOC)
clean:
	$(RM) $(DOC)

.org.texi:
	@echo "Generating $@ from $<"
	@$(EMACS) --batch "$<" \
	  -l 'ox-texinfo' -f org-texinfo-export-to-texinfo

.texi.info:
	@echo "Generating $@ from $<"
	$(MAKEINFO) $<

.texi.html:
	@echo "Generating $@ from $<"
	$(MAKEINFO) $(MAKEINFO_html_opts) $<

ffs.texi: ffs.org
ffs.info: ffs.texi
ffs.html: ffs.texi

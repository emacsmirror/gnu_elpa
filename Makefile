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

# make all/doc  Generate various formats of the ffs manual from Org
# make clean    Delete generated ffs.texi, ffs.info, ffs.html files

### Code:

.POSIX:
.PHONY: all doc clean
.SUFFIXES: .texi .info .html

CSS_REF = ../bandali.css?v=20261022

EMACS = emacs
MAKEINFO = makeinfo
MAKEINFO_html_opts = --html --no-split --css-ref="$(CSS_REF)" $\
  --init-file="ffsinit.pm"
DOC = ffs.info ffs.html

all: doc
doc: $(DOC)
clean:
	$(RM) $(DOC)

.texi.info:
	@echo "Generating $@ from $<"
	$(MAKEINFO) $<

.texi.html:
	@echo "Generating $@ from $<"
	$(MAKEINFO) $(MAKEINFO_html_opts) $<

ffs.info: ffs.texi ffsinit.pm
ffs.html: ffs.texi ffsinit.pm

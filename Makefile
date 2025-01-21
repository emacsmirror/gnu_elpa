# Copyright (C) 2025 Mauro Aranda

# This file is part of cus-abbrev.

# cus-abbrev is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# cus-abbrev is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with cus-abbrev.  If not, see <https://www.gnu.org/licenses/>.

## Programs used.
EMACS = emacs
EMACSFLAGS = -batch -f batch-byte-compile

## Variables (some might not be used right now).
PACKAGE = cus-abbrev
PACKAGE_BUGREPORT = maurooaranda@gmail.com
PACKAGE_NAME = cus-abbrev
PACKAGE_STRING = cus-abbrev 1.0
PACKAGE_TARNAME = cus-abbrev-1.0
PACKAGE_VERSION = 1.0
DISTDIR = $(PACKAGE_TARNAME)
DISTFILES = Makefile cus-abbrev.el

## Targets.

.PHONY: all clean dist

all: cus-abbrev.elc

cus-abbrev.elc: cus-abbrev.el
	$(EMACS) $(EMACSFLAGS) cus-abbrev.el

clean:
	-rm -f cus-abbrev.elc
	-rm -f $(PACKAGE_TARNAME).tar.gz

dist: cus-abbrev.elc
	mkdir --parents $(DISTDIR)
	cp --parents $(DISTFILES) $(DISTDIR)
	tar -cf $(PACKAGE_TARNAME).tar $(DISTDIR)
	rm -R $(DISTDIR)
	gzip $(PACKAGE_TARNAME).tar

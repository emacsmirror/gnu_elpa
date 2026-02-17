# Makefile for the po-mode package of GNU gettext
# Copyright (C) 1995-2026 Free Software Foundation, Inc.
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.


all: po-mode.info po-mode.html


# This directory does not support parallel make.
# So, turn off parallel execution (at least in GNU make >= 4.0).
GNUMAKEFLAGS = -j1

# List of -I options referring to directories that contain texinfo
# sources used by this directory.
TEXINCLUDES =

# The customization variable CHECK_NORMAL_MENU_STRUCTURE is necessary with
# makeinfo versions ≥ 6.8.
MAKEINFO = \
  env LANG= LC_MESSAGES= LC_ALL= LANGUAGE= \
  makeinfo -c CHECK_NORMAL_MENU_STRUCTURE=1
MAKEINFOFLAGS = $(TEXINCLUDES) --no-split

main_TEXINFOS = po-mode.texi
# List of texinfo sources @included by po-mode.texi.
included_TEXINFOS = \
  gpl.texi \
  fdl.texi

# Temporary index files.
INDEXFILES = \
  po-mode.cp po-mode.cps \
  po-mode.ef po-mode.efs \
  po-mode.em po-mode.ems \
  po-mode.ev po-mode.evs


# Documentation in info format.
po-mode.info: $(main_TEXINFOS) $(included_TEXINFOS)
	$(MAKEINFO) $(MAKEINFOFLAGS) $(main_TEXINFOS)

# Documentation in HTML format.
po-mode.html: $(main_TEXINFOS) $(included_TEXINFOS)
	$(MAKEINFO) $(MAKEINFOFLAGS) --html --no-headers $(main_TEXINFOS)

# Documentation in HTML format, from texi2html.
po-mode.t2h.html: $(main_TEXINFOS) $(included_TEXINFOS)
	perl texi2html $(TEXINCLUDES) --output=po-mode.t2h.html -no-sec-nav -no-menu -toc-links -number -monolithic $(main_TEXINFOS)

# Documentation in DVI format (outdated).
po-mode.dvi: $(main_TEXINFOS) $(included_TEXINFOS)
	texi2dvi $(TEXINCLUDES) $(main_TEXINFOS)
	rm -f $(INDEXFILES) po-mode.toc po-mode.aux po-mode.log

# Documentation in Postscript format (outdated).
po-mode.ps: $(main_TEXINFOS) $(included_TEXINFOS)
	texi2any --ps $(TEXINCLUDES) $(main_TEXINFOS)
	rm -f $(INDEXFILES) po-mode.toc po-mode.aux po-mode.log

# Documentation in Portable Document Format.
po-mode.pdf: $(main_TEXINFOS) $(included_TEXINFOS)
	texi2pdf $(TEXINCLUDES) $(main_TEXINFOS)
	rm -f $(INDEXFILES) po-mode.toc po-mode.aux po-mode.log

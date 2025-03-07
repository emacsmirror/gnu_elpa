# Copyright (C) 2024-2025 Free Software Foundation, Inc.

# Author: Morgan Smith <Morgan.J.Smith@outlook.com>
# Package: debbugs
# Keywords: comm, hypermedia, maint

# This file is free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This file is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

### Commentary:

## Some  test targets:
##
## check: re-run all tests.
## filename-tests: re-run tests from test/filename-tests.el(c).

## SELECTOR discrimination (see ERT manual for more possibilities):
##
## SELECTOR='"regexp"': Run all tests which name match "regexp"
## SELECTOR='test-name': Run test with name test-name

### Code:

EMACS ?= emacs
MAKEINFO ?= makeinfo

SOURCE = $(wildcard *.el)
TESTSOURCE = $(wildcard test/*.el)
TARGET = $(filter-out debbugs-pkg.elc,$(patsubst %.el,%.elc,$(SOURCE)))
TESTTARGET = $(patsubst %.el,%.elc,$(TESTSOURCE))

TESTS = $(patsubst test/%.el,%,$(wildcard test/*-tests.el))
SELECTOR ?= (not (tag :unstable))

INFOMANUALS = debbugs.info debbugs-ug.info

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
	@$(EMACS) -Q --batch -l resources/debbugs-checkdoc-config.el \
	  $(foreach file,$^,"--eval=(checkdoc-file \"$(file)\")")

check: $(TESTS)

%-tests: build $(TESTTARGET)
	@$(EMACS) -Q --batch -L . -L ./test -l $@ \
	  --eval '(ert-run-tests-batch-and-exit (quote ${SELECTOR}))'

clean:
	-rm -f $(TARGET) $(TESTTARGET) $(INFOMANUALS)

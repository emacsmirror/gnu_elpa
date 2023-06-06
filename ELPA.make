# For compiling gpr-query Ada code in elpa or devel worktree

#export Standard_Common_Build := Debug

.PHONY : all force

all : build byte-compile autoloads docs

docs : gpr-query.info gpr-query.html

build : config/emacs_gpr_query_config.gpr force
	gprbuild -p -j8 emacs_gpr_query.gpr

install : bin/gpr_query$(EXE_EXT)
	gprinstall -f -p -P emacs_gpr_query.gpr --prefix=~/.local --install-name=gpr_query

ifeq ($(shell uname),Linux)
EMACS_EXE ?= emacs
else ifeq ($(shell uname),Darwin)
EMACS_EXE ?= "/Applications/Emacs.app/Contents/MacOS/Emacs"
else
# windows
# specify uniscribe to workaround weird Windows harfbuzz bug
EMACS_EXE ?= emacs -xrm Emacs.fontBackend:uniscribe
endif

BYTE_COMPILE := "(progn (setq byte-compile-error-on-warn t)(batch-byte-compile))"
byte-compile : byte-compile-clean
	$(EMACS_EXE) -Q -batch -L . -L $(GNAT_COMPILER) -L $(WISI) --eval $(BYTE_COMPILE) *.el

byte-compile-clean :
	rm -f *.elc

autoloads : force
	$(EMACS_EXE) -Q -batch --eval "(progn (setq generated-autoload-file (expand-file-name \"autoloads.el\"))(update-directory-autoloads \".\"))"

%.info : %.texi
	makeinfo $< -o $@

%.html : %.texi
	makeinfo --html --no-split $< -o $@

clean : force
	rm -rf gpr-query.info obj gpr_query$(EXE_EXT)

recursive-clean : force
	gprclean -r -P emacs_gpr_query.gpr

# builds $(ELPA_ROOT)/archive-devel/*, from the last commit, _not_ the
# current workspace Also checks copyright; run elpa/GNUMakefile
# check/<pkg> first if added files.
build-elpa : ../../GNUmakefile force
	rm -rf ../../archive-devel
	make -C ../.. build/gpr-query

config :
	mkdir config

config/emacs_gpr_query_config.gpr : config
	cp emacs_gpr_query_config_devel.gpr config/emacs_gpr_query_config.gpr

# Local Variables:
# eval: (load-file "prj.el")
# End:
# end of file

# For compiling gpr-query Ada code with Alire

ALIRE_RULES_DIR ?= ../wisi
include $(ALIRE_RULES_DIR)/alire_rules.make

install : alire-build
	cp bin/gpr_query$(EXE_EXT) ~/.local/bin

# Local Variables:
# eval: (load-file "prj.el")
# End:

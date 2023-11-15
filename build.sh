#!/bin/sh
# Build executables for gpr mode.
#    build.sh options when not using Alire; <other gprbuild options>
#    e.g. 'build.sh -j0' : use all available processors to compile
#         'build.sh -wn' : treat warnings as warnings.
#         'build.sh -vh' : Verbose output (high verbosity)
#
# See install.sh for install

if type alr; then
    # alr can be installed from https://alire.ada.dev/
    echo "building gpr-mode executables via Alire"

    alr get emacs_gpr_mode~1.0.5
    cd emacs_gpr_mode_*; alr build --release

elif type gprbuild; then
    echo "building gpr-mode executables via gnat compiler"
    
    if [ -d ../wisi-4.3.0 ]; then
        WISI_DIR=`ls -d ../wisi-4.3.*`
    fi

    args=`echo -DELPA="yes" $WISI_DIR/wisi.gpr.gp $WISI_DIR/wisi.gpr`
    echo "gnatprep " $args
    gnatprep $args

    # We don't add WISI_DIR to GPR_PROJECT_PATH because the user may have
    # already set GPR_PROJECT_PATH.

    # Allow running build.sh again, since it often fails the first time.
    #  - Run gprclean, to allow changing compilers and other drastic things

    gprclean -r -P gpr_mode_wisi_parse.gpr -aP$WISI_DIR

    # We don't generate the parser code here (unlike ada-mode),
    # because it is small enough to keep in ELPA
    gprbuild -p -j8 -P gpr_mode_wisi_parse.gpr -aP $WISI_DIR "$@"

else
    echo "neither Alire nor gnat compiler found"
    return 1
fi

# end of file

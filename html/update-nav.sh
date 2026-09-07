#!/bin/sh
# This script updates the navbar in all HTML files

set -e

if [ -z "$1" ] || [ ! -d "$1" ]
then
    >&2 echo "Usage: $0 DIRECTORY [NAVBAR-FILE]"
    exit 1
fi

if [ -n "$2" ]
then
    NAVBAR_FILE="$2"
else
    NAVBAR_FILE="$1/_nav.html"
fi
if [ ! -f "$NAVBAR_FILE" ] || [ ! -r "$NAVBAR_FILE" ]
then
    >&2 echo "Cannot use $NAVBAR-FILE as navbar-file."
    exit 1
fi

find "$1" -name '*.html' -print0 |		\
    xargs -0 -L1 -P "$(nproc)" -- sed -E -i	\
	  -e "/^<nav>$/r $NAVBAR_FILE"		\
	  -e "/^<nav>$/,/^<\/nav>$/d"

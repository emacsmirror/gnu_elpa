#! /usr/bin/env bash
set -eu -o pipefail

## gen-graphics.sh --- Generate screenshots for greenbar    -*- lexical-binding: t -*-

# Copyright (C) 2025  Free Software Foundation, Inc.

# Author: Michael R. Mauger <michael@mauger.com>
# SPDX-License-Identifier: GPL-3.0-or-later

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

# Assumes GNOME Desktop Environment

PROG="$( basename "$0" )"
# shellcheck disable=SC2034 # PDIR not used (remove if actually used)
PDIR="$( cd "$( dirname "$0" )" && pwd )"

if ! type -f emacs >/dev/null; then
    echo >&2 "${PROG}: \`emacs' executable not found"
    exit 2
fi

# ========================================
# Set the defaults for options
ALL=false
DARK=false
LIGHT=false
CMDS=(short long)
BG_MODE=
GB_THEME=

# ========================================
# Array. OPT, OPT: (reqd arg), OPT? (opt arg)
SHORT_OPTIONS=( )
LONG_OPTIONS=( all long short dark light )

# ========================================
# Usage help text
function opt_usage {
    cat >&2 << TAC
usage: ${PROG} OPTIONS | bg-mode gb-theme
Options:
 * --all
      If specified, run the script for all possible values of bg-mode and gb-theme
 * --long
      Use long output lines
 * --short
      Use short output lines
 * --dark
      Use dark background
 * --light
      Use light background

Parameters:
 * bg-mode
      Background Mode. Either \`dark' or \`light'
 * gb-theme
      Greenbar Theme. One of: greenbar, graybar, or rainbow
TAC
    exit 2
}

OPT_SHORT=$( IFS='';  : "${SHORT_OPTIONS[*]}"; : "${_//\?/::}"; : "${_}h";           echo "${_}" )
OPT_LONG=$(  IFS=','; : "${LONG_OPTIONS[*]}";  : "${_//\?/::}"; : "${_}${_:+,}help"; echo "${_}" )

OPTIONS=$( getopt --alternative \
                  --name "${PROG}" \
                  --options "${OPT_SHORT}" \
                  --longoptions "${OPT_LONG}" \
                  -- "$@" )
eval set -- "${OPTIONS}"

# ========================================
# Handle command line options
while true; do
    case ${1} in
        (--all)         ALL=true ;;
        (--dark)        DARK=true ;;
        (--light)       LIGHT=true ;;
        (--long)        CMDS=(long) ;;
        (--short)       CMDS=(short) ;;

        (-h | --help)   opt_usage ;;
        (--)            shift; break ;;
        (*)
            echo >&2 "${PROG}: unrecognized option -- \"-${OPT}\""
            exit 1
            ;;
    esac
    shift
done

# echo "ALL='${ALL}'" "DARK='${DARK}'" "LIGHT='${LIGHT}'" "CMDS=(${CMDS[*]})" "[*]=$*"

# ========================================
# Parse the command line parameters
if ${ALL}; then
    for BG_MODE in light dark; do
        for GB_THEME in greenbar graybar rainbow; do
            for CMD in "${CMDS[@]}"; do
                "$0" "--${CMD}" "${BG_MODE}" "${GB_THEME}"
                sleep 1
            done
        done
    done

    exit

elif ${DARK}; then
    for GB_THEME in greenbar graybar rainbow; do
        for CMD in "${CMDS[@]}"; do
            "$0" "--${CMD}" dark "${GB_THEME}"
            sleep 1
        done
    done

    exit

elif ${LIGHT}; then
    for GB_THEME in greenbar graybar rainbow; do
        for CMD in "${CMDS[@]}"; do
            "$0" "--${CMD}" light "${GB_THEME}"
            sleep 1
        done
    done

    exit

elif (( ${#CMDS[*]} > 1 )); then
    for CMD in "${CMDS[@]}"; do
        "$0" "--${CMD}" "${@}"
        sleep 1
    done

    exit

else
    CMD=${CMDS[0]}
fi

# ========================================
# Parse BG-MODE and GB-THEME
if (( $# != 2 )); then
    echo >&2 "${PROG}: accepts 2 parameters: bg-mode gb-theme"
    exit 1
fi

case $1 in
    (dark | light) BG_MODE=$1 ;;
    *)
        echo >&2 "${PROG}: unrecognized background mode -- \"${1}\""
        exit 1
        ;;
esac

THEME=
case ${BG_MODE} in
    (dark) THEME='tango-dark' ;;
    (light) THEME='tango' ;;
esac

case $2 in
    (greenbar | graybar | rainbow) GB_THEME=$2 ;;
    *)
        echo >&2 "${PROG}: unrecognized greenbar theme -- \"${2}\""
        exit 1
        ;;
esac

NAME="${GB_THEME}-${BG_MODE}-${CMD}"
#

WIDTH=
case ${CMD} in
    (short) WIDTH=80 ;;
    (long)  WIDTH=140 ;;
esac

HEIGHT=
case ${CMD} in
    (short) HEIGHT=14 ;;
    (long)  HEIGHT=40 ;;
esac

FONT=
case ${CMD} in
    (short) FONT='Adwaita Mono 12' ;;
    (long)  FONT='Adwaita Mono 4' ;;
esac

COMINT_CMD=
case ${CMD} in
    (short)
        COMINT_CMD='echo ls -lAF; ls -lAF .git* *.el *.elc\n'
        ;;
    (long)
        COMINT_CMD='echo journalctl -u avahi-daemon; (journalctl -u avahi-daemon | sed -E \"s/[[:xdigit:]]{3,4}([:. ])/####\1/g\" | tail -30 )\n'
        ;;
esac

# ========================================
# Make the script to generate the snapshot
GEN_EL=$( mktemp --tmpdir --suffix .el "${PROG}.XXXXXXXXXX" )
cat >"${GEN_EL}" <<TAC
(set-face-font 'default "${FONT}" nil)
(push '(width . ${WIDTH}) default-frame-alist)
(push '(height . ${HEIGHT}) default-frame-alist)
(push '(background-mode . ${BG_MODE}) default-frame-alist)
(load-theme '${THEME})
(load-file "./greenbar.el")
(setopt greenbar-background-colors '${GB_THEME})

(defun refresh-display ()
  (while (and (not (sit-for 1))
              (accept-process-output nil 1))
    (redisplay)))

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(let* ((fram (make-frame '((name . "GreenBar (${BG_MODE} ${GB_THEME})")
                           (minibuffer . nil))))
       (shel  (shell))
       (proc  (get-buffer-process shel)))
  (select-frame fram)
  (pop-to-buffer shel)
  (delete-other-windows)
  (greenbar-mode +1)
    (refresh-display)
  (comint-send-string proc "${COMINT_CMD}")
    (refresh-display)
  (call-process "gnome-screenshot" nil nil nil
                "--window"
                "--delay" "1"
                "--file" "${PDIR}/${NAME}.png")
  (comint-send-eof)
    (refresh-display)
  (kill-buffer shel)
    (refresh-display)
  (kill-emacs)
  )
TAC

# ========================================
# Produce the snapshot
cd "${PDIR}"
if [[ -e ../greenbar.el ]]; then
    cd ..
fi
emacs --quick --load "${GEN_EL}"

#

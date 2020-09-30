#!/bin/sh
#  SH FILE: graphics-emacs.sh
#
#  Purpose   : Run graphics Emacs asynchronously on specified files.
#  Created   : Tuesday, September 29 2020.
#  Author    : Pierre Rouleau <prouleau001@gmail.com>
#  Time-stamp: <2020-09-29 22:28:41, updated by Pierre Rouleau>
# --------------------------------------------------------------------
emacs --chdir=$(pwd) "$@" 2>/dev/null &
# --------------------------------------------------------------------

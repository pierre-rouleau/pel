#!/bin/sh
# SH FILE: recover-from-inconsistent-mode.sh
#
# Purpose   : Recover Emacs normal mode from an inconsistent fast startup.
# Created   : Thursday, May 15 2025.
# Author    : Pierre Rouleau <prouleau001@gmail.com>
# Time-stamp: <2025-05-15 23:24:28 EDT, updated by Pierre Rouleau>
# ----------------------------------------------------------------------------
# Description
# -----------
#
# A simple shell script that restores Emacs normal mode, after a failed
# attempt to convert it to PEL fast startup mode.

# ----------------------------------------------------------------------------
# Dependencies
# ------------
#
#

# ----------------------------------------------------------------------------
# Code
# ----
#
#
if [ ! -d "$HOME/.emacs.d" ]; then
    printf -- "*** ERROR: Emacs ~/.emacs.d directory does not exist!\n"
    exit 1
fi

cd "$HOME/.emacs.d" || exit 2
if [ -e "pel-fast-startup-init.el" ]; then
    rm "pel-fast-startup-init.el"
fi

if [ ! -L "elpa" ]; then
    printf -- "*** ERROR: ~/.emacs.d/elpa symlink does not exist!\n"
    exit 1
fi
rm elpa
ln -s "$HOME/.emacs.d/elpa-complete/" elpa

# Update the graphics directories if they exist.
if [ -L "elpa-graphics" ]; then
    if [ ! -d "elpa-complete-graphics" ]; then
        printf -- "*** ERROR: ~/.emacs.d/elpa-complete-graphics does not exist!\n"
        exit 1
    fi
    if [ ! -d "elpa-reduced-graphics" ]; then
        printf -- "*** ERROR: ~/.emacs.d/elpa-reduced-graphics does not exist!\n"
        exit 1
    fi
    rm elpa-graphics
    ln -s "$HOME/.emacs.d/elpa-complete-graphics/" elpa-graphics
fi

printf -- "SUCCESS! Emacs normal mode should be re-established!\n"

# ----------------------------------------------------------------------------

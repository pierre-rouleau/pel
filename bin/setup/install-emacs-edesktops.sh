#!/bin/sh
# SH FILE: install-emacs-edesktops.sh
#
# Purpose   : Install the PEL edesktops scripts.
# Created   : Tuesday, January  7 2025.
# Author    : Pierre Rouleau <prouleau001@gmail.com>
# Time-stamp: <2025-01-07 17:03:41 EST, updated by Pierre Rouleau>
# ----------------------------------------------------------------------------
# Module Description
# ------------------
#
# Install all edesktops utility scripts into ~/bin as symlinks.
# Tell user to put ~/bin inside PATH if it's not already.

# ----------------------------------------------------------------------------
# Code
# ----
#
#

script="$(realpath "$0")"
script_dirpath="$(dirname "$script")"
bin_dirpath="$(dirname "$script_dirpath")"

if [ ! -d "$HOME/bin" ]; then
    printf -- "***ERROR: The ~/bin directory does not exist.\n"
    printf -- "   Please create it.\n\n"
    exit 1
fi

check_file()
{
    if [ -e "$1" ]; then
        printf -- "***ERROR: File %s already exists.\n" "$1"
        printf -- "   Was this already installed?\n"
        printf -- "   If not, remove that file or rename it and try again.\n\n"
        exit 1
    fi
}

check_file "$HOME/bin/edesktops"
check_file "$HOME/bin/edesktops-used"
check_file "$HOME/bin/edesktops--lockinfo-for"

install_symlink_for()
{
    ln -s "${bin_dirpath}/$1" "$HOME/bin/$1"  || exit 1
}

install_symlink_for edesktops
install_symlink_for edesktops-used
install_symlink_for edesktops--lockinfo-for

printf -- "SUCCESS!!\nInstallation of the script completed!\They are:\n\n"

ls -l "$HOME/bin/edesktops"
ls -l "$HOME/bin/edesktops-used"
ls -l "$HOME/bin/edesktops--lockinfo-for"

if [ "$(which edesktops)" != "$HOME/bin/edesktops" ]; then
    printf -- "***NEXT STEP:\n"
    printf -- " Please add %s to your PATH.\n\n" "$HOME/bin"
    exit 1
fi

# ----------------------------------------------------------------------------

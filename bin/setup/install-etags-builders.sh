#!/bin/sh
# SH FILE: install-etags-builders.sh
#
# Purpose   : Install PEL etags builder scripts.
# Created   : Thursday, June  6 2024.
# Author    : Pierre Rouleau <prouleau001@gmail.com>
# Time-stamp: <2025-01-07 17:04:13 EST, updated by Pierre Rouleau>
# ----------------------------------------------------------------------------
# Module Description
# ------------------
#
# Install all etags builder scripts into ~/bin as symlinks.
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

check_file "$HOME/bin/etags-autotools"
check_file "$HOME/bin/etags-c"
check_file "$HOME/bin/etags-el"
check_file "$HOME/bin/etags-erl"
check_file "$HOME/bin/etags-go"
check_file "$HOME/bin/etags-lisp"
check_file "$HOME/bin/etags-make"
check_file "$HOME/bin/etags-py"
check_file "$HOME/bin/etags-rs"

install_symlink_for()
{
    ln -s "${bin_dirpath}/$1" "$HOME/bin/$1"  || exit 1
}

install_symlink_for etags-autotools
install_symlink_for etags-c
install_symlink_for etags-el
install_symlink_for etags-erl
install_symlink_for etags-go
install_symlink_for etags-lisp
install_symlink_for etags-make
install_symlink_for etags-py
install_symlink_for etags-rs

printf -- "SUCCESS!!\nInstallation of the script completed!\They are:\n\n"

ls -l "$HOME/bin/etags-autotools"
ls -l "$HOME/bin/etags-c"
ls -l "$HOME/bin/etags-el"
ls -l "$HOME/bin/etags-erl"
ls -l "$HOME/bin/etags-go"
ls -l "$HOME/bin/etags-lisp"
ls -l "$HOME/bin/etags-make"
ls -l "$HOME/bin/etags-py"
ls -l "$HOME/bin/etags-rs"

if [ "$(which etags-c)" != "$HOME/bin/etags-c" ]; then
    printf -- "***NEXT STEP:\n"
    printf -- " Please add %s to your PATH.\n\n" "$HOME/bin"
    exit 1
fi

printf -- "\nFor help on these commands use their --help command line option.\n"

# ----------------------------------------------------------------------------

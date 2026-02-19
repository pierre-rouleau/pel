#!/bin/sh
# SH FILE: install-etags-builders.sh
#
# Purpose   : Install PEL etags builder scripts.
# Created   : Thursday, June  6 2024.
# Author    : Pierre Rouleau <prouleau001@gmail.com>
# Time-stamp: <2026-02-18 18:09:02 EST, updated by Pierre Rouleau>
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

install_file()
{
    # argument: $1 := base name of script to install.
    if [ -e "$HOME/bin/$1" ]; then
        current_target="$(readlink -f "$HOME/bin/$1")"
        required_target="${bin_dirpath}/$1"
        if [ "${current_target}" = "${required_target}" ]; then
            printf -- "***Warning: File %s already exists.\n" "$HOME/bin/$1"
        else
            printf -- "***ERROR  : File %s already exists but links to: %s\n" "$HOME/bin/$1" "${current_target}"
        fi
    else
        ln -s "${bin_dirpath}/$1" "$HOME/bin/$1"  || exit 1
        printf -- "Installed : %s\n" "$(ls -l "${bin_dirpath}/$1")"
    fi
}

install_file "etags-autotools"
install_file "etags-c"
install_file "etags-el"
install_file "etags-elixir"
install_file "etags-erl"
install_file "etags-go"
install_file "etags-javascript"
install_file "etags-lisp"
install_file "etags-make"
install_file "etags-objc"
install_file "etags-perl"
install_file "etags-pike"
install_file "etags-py"
install_file "etags-rs"
install_file "etags-tcl"

if [ "$(command -v etags-c)" != "$HOME/bin/etags-c" ]; then
    printf -- "***NEXT STEP:\n"
    printf -- " Please add %s to your PATH.\n\n" "$HOME/bin"
    exit 1
fi

printf -- "\nFor help on these commands use their --help command line option.\n"

# ----------------------------------------------------------------------------

#!/bin/sh
# SH FILE: install-etags-builders.sh
#
# Purpose   : Install PEL etags builder scripts.
# Created   : Thursday, June  6 2024.
# Author    : Pierre Rouleau <prouleau001@gmail.com>
# Time-stamp: <2025-10-30 10:43:38 EDT, updated by Pierre Rouleau>
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

process_file()
{
    if [ -e "$1" ]; then
        printf -- "***Warning: File %s already exists.\n" "$1"
    else
        ln -s "${bin_dirpath}/$1" "$HOME/bin/$1"  || exit 1
        printf -- "Installed: %s\n" "$(ls -l "$1")"
    fi
}

process_file "$HOME/bin/etags-autotools"
process_file "$HOME/bin/etags-c"
process_file "$HOME/bin/etags-el"
process_file "$HOME/bin/etags-elixir"
process_file "$HOME/bin/etags-erl"
process_file "$HOME/bin/etags-go"
process_file "$HOME/bin/etags-javascript"
process_file "$HOME/bin/etags-lisp"
process_file "$HOME/bin/etags-make"
process_file "$HOME/bin/etags-perl"
process_file "$HOME/bin/etags-pike"
process_file "$HOME/bin/etags-py"
process_file "$HOME/bin/etags-rs"
process_file "$HOME/bin/etags-tcl"

if [ "$(which etags-c)" != "$HOME/bin/etags-c" ]; then
    printf -- "***NEXT STEP:\n"
    printf -- " Please add %s to your PATH.\n\n" "$HOME/bin"
    exit 1
fi

printf -- "\nFor help on these commands use their --help command line option.\n"

# ----------------------------------------------------------------------------

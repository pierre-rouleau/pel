#!/bin/sh
# SH FILE: install-emacs-launcher-scripts.sh
#
# Purpose   : Install important control scripts via symlinks in ~/bin.
# Created   : Tuesday, May 28 2024.
# Author    : Pierre Rouleau <prouleau001@gmail.com>
# Time-stamp: <2026-05-21 15:20:42 EDT, updated by Pierre Rouleau>
# ----------------------------------------------------------------------------
# Module Description
# ------------------
#
# Creates symbolic links from ~/bin to important PEL scripts: e, ge, ec,
# is-emacs-daemon-running and is-pel-in-fast-startup.
#
# Tell user to put ~/bin inside PATH if it's not already there.

# ----------------------------------------------------------------------------
# Dependencies
# ------------
#
# - dirname   (POSIX)
# - ln        (POSIX)
# - ls        (POSIX)

# ----------------------------------------------------------------------------
# Code
# ----
#
# Extract name of executing script using POSIX tools:
#
script_dirpath="$(cd "$(dirname "$0")" && pwd -P)"
script="${script_dirpath}/${0##*/}"
bin_dirpath="$(dirname "$script_dirpath")"

# echo "The script is: ${script}"
# echo "The directory: ${script_dirpath}"
# echo "The bin      : ${bin_dirpath}"

if [ ! -d "$HOME/bin" ]; then
    printf -- "***ERROR: The ~/bin directory does not exist.\n"
    printf -- "   Please create it (or create a symlink to it).\n\n"
    exit 1
fi

# --
# 1. Pre-installation checks:
check_file()
{
    if [ -e "$1" ] || [ -L "$1" ]; then
        printf -- "***ERROR: File %s already exists.\n" "$1"
        printf -- "   Was this already installed?\n"
        printf -- "   If not, remove that file or rename it and try again.\n\n"
        exit 1
    fi
}

check_file "$HOME/bin/e"
check_file "$HOME/bin/ge"
check_file "$HOME/bin/ec"
check_file "$HOME/bin/is-emacs-daemon-running"
check_file "$HOME/bin/is-pel-in-fast-startup"

# --
# 2. Install the symlink:
install_symlink_for()
{
    ln -s "${bin_dirpath}/$1" "$HOME/bin/$1"  || exit 1
}

install_symlink_for e
install_symlink_for ge
install_symlink_for ec
install_symlink_for is-emacs-daemon-running
install_symlink_for is-pel-in-fast-startup

# --
# 3. Success listing:

printf -- "SUCCESS!!\nInstallation of all following scripts completed!\nThey are:\n\n"
ls -l "$HOME/bin/e"
ls -l "$HOME/bin/ge"
ls -l "$HOME/bin/ec"
ls -l "$HOME/bin/is-emacs-daemon-running"
ls -l "$HOME/bin/is-pel-in-fast-startup"

if [ "$(command -v e)" != "$HOME/bin/e" ]; then
    printf -- "***NEXT STEP:\n"
    printf -- " Please add %s to your PATH.\n\n" "$HOME/bin"
    exit 1
fi

printf -- "\nFor help on these commands use their --help command line option.\n"
printf -- "\
   e --help is emacs --help.
   e opens an independent emacs process in terminal mode.
   All other commands print their own help with -h or --help.\n\n"

# ----------------------------------------------------------------------------
#  Local Variables:
#  sh-shell: sh
#  End:

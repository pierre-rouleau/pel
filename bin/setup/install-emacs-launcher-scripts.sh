#!/bin/sh
# SH FILE: install-emacs-launcher-scripts.sh
#
# Purpose   : Install important control scripts via symlinks in ~/bin.
# Created   : Tuesday, May 28 2024.
# Author    : Pierre Rouleau <prouleau001@gmail.com>
# Time-stamp: <2026-10-20 11:51:32 EDT, updated by Pierre Rouleau>
# ----------------------------------------------------------------------------
# Module Description
# ------------------
#
# Creates symbolic links from ~/bin to important PEL scripts: e, ge, ec, lses,
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
pgm_name="${0##*/}"
script_dirpath="$(cd "$(dirname "$0")" && pwd -P)"
script="${script_dirpath}/${pgm_name}"
bin_dirpath="$(dirname "$script_dirpath")"
pel_dirpath="${bin_dirpath%/*}"


print_usage()
{
    printf -- "
%s -- Install the Emacs launchers and tools: e, ge, ec and lses.

 Usage: %s -h | --help

  • Print this help information.

 Usage: %s

  • Install all launcher scripts:
    - Checks the presence of a ~/bin directory.
    - Create symlinks in ~/bin to the PEL/bin scripts,
    - Build lses executable by compiling its C source,
    - Create a symlink in ~/bin to the PEL/c/lses/lses executable.
" "${pgm_name}" "${script}" "${script}"
}

# --
# Handle help request

# Debug only
# echo "The script is: ${script}"
# echo "The directory: ${script_dirpath}"
# echo "The bin      : ${bin_dirpath}"
# echo "The pel      : ${pel_dirpath}"


if [ "$1" = "-h" ] || [ "$1" = "--help" ]; then
    print_usage
    # help requested explicitly: no error.
    exit 0
fi

# ----------------------------------------------------------------------------
# Prompter
# ========

prompt_yn_return=
prompt_yn()
{
    # Arg 1: string prompt
    while true; do
        printf "%s [yn]? " "$1"
        if ! read -r yn; then
            prompt_yn_return=n
            printf '\nCannot read a valid response; aborting.\n' >&2
            return 1
        fi
        case $yn in
            [Yy]* )
                prompt_yn_return=y
                break
                ;;
            [Nn]* )
                prompt_yn_return=n
                break
                ;;
            * ) echo "Please answer yes or no.";;
        esac
    done
}

# ----------------------------------------------------------------------------
# Check Environment

if [ ! -d "$HOME/bin" ]; then
    printf -- "***ERROR: The ~/bin directory does not exist.\n"
    printf -- "   Please create it (or create a symlink to it).\n\n"
    exit 1
fi


# ----------------------------------------------------------------------------
# Proceed


# Install the symlinks
install_symlink_for()
{
    ln -s "${bin_dirpath}/$1" "$HOME/bin/$1"  || exit 1
}


# --
# 1. Pre-installation checks:
install_file()
{
    # Argument 1: file name: e, ge, ec, lses, ...

    if [ -e "$HOME/bin/$1" ] || [ -L "$HOME/bin/$1" ]; then
        printf -- "***INFO: File [%s] already exists.\n" "$HOME/bin/$1"
        printf -- "   Was this already installed?\n"
        prompt_yn "   Continue using this existing symlink" || exit 4
        if [ ! "$prompt_yn_return" = "y" ]; then
            prompt_yn "Delete old [$HOME/bin/$1], and install a new symlink (Ctrl-C aborts)" || exit 4
            if [ "$prompt_yn_return" = "y" ]; then
                rm -f "$HOME/bin/$1"
                install_symlink_for "$1"
            fi
        fi
    else
        install_symlink_for "$1"
    fi
}


install_file "e"
install_file "ge"
install_file "ec"
install_file "is-emacs-daemon-running"
install_file "is-pel-in-fast-startup"

# --
# Build and install lses
#  Use a sub-shell to temporary change current directory
(
    cd "${pel_dirpath}" || exit 2
    pwd
    make c-lses         || exit 2
)
status=$?

if [ "$status" -ne 0 ]; then
    printf -- "*** ERROR: Failed to build executable lses\n"
    exit 2
fi

# Check if lses symlink already exists
# Setup the symlink if needed
#
if [ -e "$HOME/bin/lses" ] || [ -L "$HOME/bin/lses" ]; then
    printf -- "***INFO: File [%s] already exists.\n" "$HOME/bin/lses"
    printf -- "   Was this already installed?\n"
    prompt_yn "   Continue using this existing symlink" || exit 4
    if [ ! "$prompt_yn_return" = "y" ]; then
        prompt_yn "Delete that old file, and install a new symlink" || exit 4
        if [ "$prompt_yn_return" = "y" ]; then
            rm -f "$HOME/bin/lses"
            ln -s "${pel_dirpath}/c/lses/lses" "$HOME/bin/lses"  || exit 1
        fi
    fi
else
    ln -s "${pel_dirpath}/c/lses/lses" "$HOME/bin/lses"  || exit 1
fi

# --
# 3. Success listing:

printf -- "SUCCESS!!\nInstallation of all following scripts completed!\nThey are:\n\n"
ls -l "$HOME/bin/e"
ls -l "$HOME/bin/ge"
ls -l "$HOME/bin/ec"
ls -l "$HOME/bin/lses"
ls -l "$HOME/bin/is-emacs-daemon-running"
ls -l "$HOME/bin/is-pel-in-fast-startup"

if [ "$(command -v e)" != "$HOME/bin/e" ]; then
    printf -- "***NEXT STEP:\n"
    printf -- " Please add %s to your PATH.\n\n" "$HOME/bin"
    exit 1
fi

printf -- "\nFor help on these commands use their -h or --help command line option.\n"
printf -- "\
   e -- --help is emacs --help.
   e opens an independent emacs process in terminal mode.
\n\n"

# ----------------------------------------------------------------------------
#  Local Variables:
#  sh-shell: sh
#  End:

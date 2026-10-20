#!/bin/sh
# SH FILE: install-emacs-launcher-scripts.sh
#
# Purpose   : Install important control scripts via symlinks in ~/bin.
# Created   : Tuesday, May 28 2024.
# Author    : Pierre Rouleau <prouleau001@gmail.com>
# Time-stamp: <2026-10-20 15:55:03 EDT, updated by Pierre Rouleau>
# ----------------------------------------------------------------------------
# Module Description
# ------------------
#
# Creates symbolic links from ~/bin to the PEL scripts e, ge, ec,
# is-emacs-daemon-running and is-pel-in-fast-startup.
#
# Builds lses from C source and creates a link to the compiled executable.
# GNU Make uses its implicit C compilation rule to build lses.
# Install the launcher scripts before attempting this optional build.
# If the build fails, offer the less-powerful bin/lses shell script.
# If the user declines, leave the existing ~/bin/lses entry unchanged.
#
# Tell user to put ~/bin inside PATH if it's not already there.

# ----------------------------------------------------------------------------
# Dependencies
# ------------
#
# - dirname   (POSIX)
# - ln        (POSIX)
# - ls        (POSIX)
# - rm        (POSIX)
# - readlink  (available on Linux and macOS)
# - GNU Make
# - A C99-capable compiler (to build the C implementation of lses)
# - emacsclient from Emacs 26.1 or later in PATH (C lses runtime)

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
%s -- Install the Emacs launchers and tools.

 Commands: e, ge, ec, lses, is-emacs-daemon-running,
           is-pel-in-fast-startup.

 Usage: %s -h | --help

  • Print this help information.

 Usage: %s

  • Install the launchers and tools:
    - Checks the presence of a ~/bin directory.
    - Create symlinks in ~/bin to the PEL/bin scripts,
    - Build lses executable by compiling its C source,
    - Create a symlink in ~/bin to the PEL/c/lses/lses executable,
    - If the build fails, offer the less-powerful PEL/bin/lses shell script.
" "${pgm_name}" "${script}" "${script}"
}

# --
# Check argument, handle help request

# Debug only
# echo "The script is: ${script}"
# echo "The directory: ${script_dirpath}"
# echo "The bin      : ${bin_dirpath}"
# echo "The pel      : ${pel_dirpath}"

if [ "$#" -gt 0 ]; then
    if [ "$#" -eq 1 ]; then
        case "$1" in
            -h|--help)
                print_usage
                exit 0
                ;;
        esac
    fi
    printf '%s: unsupported arguments; use --help.\n' "$pgm_name" >&2
    exit 1
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
    printf -- "***ERROR: The ~/bin directory does not exist.\n"       >&2
    printf -- "   Please create it (or create a symlink to it).\n\n"  >&2
    exit 1
fi

# ----------------------------------------------------------------------------
# Proceed


# Install the symlinks
install_symlink_for()
{
    ln -s "${bin_dirpath}/$1" "$HOME/bin/$1"  || exit 1
}

# Warn before offering to retain an unexpected destination.
warn_if_unexpected_target()
{
    # Argument 1: destination path.
    # Argument 2: expected absolute symlink target.

    # Compare stored target text, not resolved file identity.
    # current_target is global; POSIX sh does not require local variables.
    if [ -L "$1" ]; then
        # Command-substitution assignment preserves readlink's exit status.
        current_target=$(readlink "$1")
        if [ "$?" -ne 0 ]; then
            printf -- '***WARNING: Cannot read the target of symlink [%s].\n' "$1" >&2
            printf -- '   Expected target: [%s]\n' "$2" >&2
        elif [ "$current_target" != "$2" ]; then
            printf -- '***WARNING: Symlink [%s] has an unexpected target.\n' "$1" >&2
            printf -- '   Current target:  [%s]\n' "$current_target" >&2
            printf -- '   Expected target: [%s]\n' "$2" >&2
        fi
    else
        printf -- '***WARNING: Existing entry [%s] is not a symlink.\n' "$1" >&2
        printf -- '   Expected target: [%s]\n' "$2" >&2
    fi
}

# --
# 1. Pre-installation checks:
install_file()
{
    # Argument 1: file name: e, ge, ec, lses, ...

    if [ -e "$HOME/bin/$1" ] || [ -L "$HOME/bin/$1" ]; then
        printf -- "***INFO: File [%s] already exists.\n" "$HOME/bin/$1"
        warn_if_unexpected_target "$HOME/bin/$1" "${bin_dirpath}/$1"
        printf -- "   Was this already installed?\n"
        prompt_yn "   Keep this existing entry" || exit 4
        if [ ! "$prompt_yn_return" = "y" ]; then
            prompt_yn "Delete old [$HOME/bin/$1], and install a new symlink (Ctrl-C aborts)" || exit 4
            if [ "$prompt_yn_return" = "y" ]; then
                rm -f "$HOME/bin/$1" || exit 3
                install_symlink_for "$1"
            else
                printf '   Skipped replacement of %s; existing entry retained.\n' "$HOME/bin/$1"
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
#  Use a subshell to change directory temporarily.
(
    cd "${pel_dirpath}" || exit 2
    pwd
    make c-lses         || exit 2
)
status=$?

lses_target="${pel_dirpath}/c/lses/lses"
if [ "$status" -ne 0 ]; then
    printf -- '***WARNING: Failed to build the lses executable.\n' >&2
    printf -- '   The fallback shell script is less powerful than the C executable.\n' >&2
    prompt_yn "Use the less-powerful PEL/bin/lses shell script instead" || exit 4
    if [ "$prompt_yn_return" != "y" ]; then
        printf 'No changes made to %s.\n' "$HOME/bin/lses" >&2
        exit 2
    fi
    lses_target="${bin_dirpath}/lses"
    if [ ! -f "$lses_target" ] || [ ! -x "$lses_target" ]; then
        printf -- '***ERROR: Fallback script [%s] is missing or not executable.\n' "$lses_target" >&2
        exit 2
    fi
    printf -- '***WARNING: Selected the less-powerful shell implementation: [%s].\n' "$lses_target" >&2
fi

# Check if lses symlink already exists
# Setup the symlink if needed
#
if [ -e "$HOME/bin/lses" ] || [ -L "$HOME/bin/lses" ]; then
    printf -- "***INFO: File [%s] already exists.\n" "$HOME/bin/lses"
    printf '   Selected target: [%s]\n' "$lses_target"
    warn_if_unexpected_target "$HOME/bin/lses" "$lses_target"
    printf -- "   Was this already installed?\n"
    prompt_yn "   Keep this existing entry instead of installing the selected target" || exit 4
    if [ ! "$prompt_yn_return" = "y" ]; then
        prompt_yn "Delete that old file, and install a new symlink" || exit 4
        if [ "$prompt_yn_return" = "y" ]; then
            rm -f "$HOME/bin/lses" || exit 3
            ln -s "$lses_target" "$HOME/bin/lses" || exit 1
        else
            printf '   Skipped replacement of %s; existing entry retained.\n' "$HOME/bin/lses"
        fi
    fi
else
    ln -s "$lses_target" "$HOME/bin/lses" || exit 1
fi

# --
# 3. List current entries:

printf -- "====================================================\n"
printf -- 'Installation processing completed.\n'
printf -- 'Existing entries were retained where requested.\n'
printf -- 'Current files and symlinks:\n\n'
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
   e   :  opens an independent emacs process in terminal mode.
   ge  :  opens an independent emacs process in GUI mode.
   ec  :  open/check Emacs server and client in terminal or GUI mode.
   lses:  list Emacs daemon sockets.

Other scripts are also available:

 is-emacs-daemon-running: report if Emacs daemon is running or not.
 is-pel-in-fast-startup : check and report whether Emacs is using
                          PEL Fast Startup Mode.
\n\n"

# ----------------------------------------------------------------------------
#  Local Variables:
#  sh-shell: sh
#  End:

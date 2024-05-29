#!/bin/sh
# SH FILE: install-scripts.sh
#
# Purpose   : Install the e, ge and ce scripts in ~/bin.
# Created   : Tuesday, May 28 2024.
# Author    : Pierre Rouleau <prouleau001@gmail.com>
# Time-stamp: <2024-05-29 10:22:17 EDT, updated by Pierre Rouleau>
# ----------------------------------------------------------------------------
# Module Description
# ------------------
#
# Creates symbolic links from ~/bin to the e, ge and ce scripts.
# Tell user to put ~/bin inside PATH if it's not already.


# ----------------------------------------------------------------------------
# Code
# ----
#
#
script="$(realpath $0)"
script_dirpath="$(dirname "$script")"
bin_dirpath="$(dirname "$script_dirpath")"

# echo "The script is: ${script}"
# echo "The directory: ${script_dirpath}"
# echo "The bin      : ${bin_dirpath}"

if [ ! -d "$HOME/bin" ]; then
    printf -- "***ERROR: The ~/bin directory does not exist.\n"
    printf -- "   Please create it.\n\n"
    exit 1
fi

check_file()
{
    if [ -e "$1" ]; then
        printf -- "***ERROR: File $1 already exists.\n"
        printf -- "   Was this already installed?\n"
        printf -- "   If not, remove that file or rename it and try again.\n\n"
        exit 1
    fi
}

check_file "$HOME/bin/e"
check_file "$HOME/bin/ge"
check_file "$HOME/bin/ec"

install_symlink_for()
{
    ln -s "${bin_dirpath}/$1" "$HOME/bin/$1"  || exit 1
}

install_symlink_for e
install_symlink_for ge
install_symlink_for ec


printf -- "SUCCESS!!\nInstallation of e, ge and ec scripts completed!\nThey are:\n\n"
ls -l "$HOME/bin/e"
ls -l "$HOME/bin/ge"
ls -l "$HOME/bin/ec"

if [ "$(which e)" != "$HOME/bin/e" ]; then
    printf -- "***NEXT STEP:\n"
    printf -- " Please add %s to your PATH.\n\n" "$HOME/bin"
    exit 1
fi

printf -- "\nFor help on these commands use their --help command line option.\n"
printf -- "\
   e --help is emacs --help.
   e opens an independent emacs process in terminal mode.
   eg and ec have their own help, which are:\n\n"
ge --help
printf -- "\n\n"
ec --help
printf -- "\n\n"

# ----------------------------------------------------------------------------
#  Local Variables:
#  sh-shell: sh
#  End:

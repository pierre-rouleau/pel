# SH FILE: install-eslint.sh
#
# Purpose   : Install eslint Javascript linter.
# Created   : Thursday, October 30 2025.
# Author    : Pierre Rouleau <prouleau001@gmail.com>
# Time-stamp: <2025-10-31 08:51:55 EDT, updated by Pierre Rouleau>
# ----------------------------------------------------------------------------
# Module Description
# ------------------
#
# Automate installation of Eslint following the instructions
# at https://github.com/eslint-org/eslint?tab=readme-ov-file#quickstart-install
#
# First create the ~/bin/javascript/eslint.mjs by downloading it
# from the official location: https://www.eslint.com/eslint.mjs
#
# Then copy the eslint executable script in ~/bin


# ----------------------------------------------------------------------------
# Dependencies
# ------------
#
# - printf, which, ln

# ----------------------------------------------------------------------------
# Code
# ----
#

pgm_name="$(basename "$0")"

# -- Validate environment
if ! which curl > /dev/null ; then
    printf -- "***ERROR: %s requires curl on PATH. It's not. Is it installed?" "$pgm_name"
    exit 1
fi

# -- Use directory names
# The install script is stored in pel/bin/setup
installer_script_pathname="$(realpath "$0")"
install_script_dirpath="$(dirname "$installer_script_pathname")"
# The eslint script is stored inside pel/bin/
script_dirpath="$(dirname "$install_script_dirpath")"

# The ~/bin directory should be in PATH: store the symlink to eslint there.
bin_dir="$HOME/bin"

# -- Check for presence of ~/bin, create it if missing, stop on error.
if [ ! -d "${bin_dir}" ]; then
    if [ ! -e "${bin_dir}" ]; then
        mkdir "${bin_dir}" || exit 1
    else
        printf -- "*** ERROR: %s exists but is not a directory!\n" "${bin_dir}"
        printf -- "           Please rename it to allow creation of a %s directory.\n" "${bin_dir}"
        exit 1
    fi
fi

# -- Create a symlink in ~/bin to the eslint shell script
if [ -h "${bin_dir}/eslint"  ]; then
    current_target="$(readlink -f "${bin_dir}/eslint")"
    required_target="${script_dirpath}/eslint"
    if [ "${current_target}" = "${required_target}" ]; then
        if [ ! -e "${script_dirpath}/eslint" ]; then
            printf -- "***ERROR: the expected script file (%s) is not there!\n" "${script_dirpath}/eslint"
            printf -- "          Check your PEL repo!\n"
            exit 1
        else
            printf -- "***Warning: symlink %s already exists:\n" "$HOME/bin/eslint"
            printf -- " %s\n" "$(ls -l "$(which eslint)")"
        fi
    else
        printf -- "***ERROR  : File %s already exists but links to: %s\n" "$HOME/bin/eslint" "${current_target}"
    fi
else
    if [ -e "${bin_dir}/eslint"  ]; then
        printf -- "***ERROR  : %s is a file, not a symlink.\n" "${bin_dir}/eslint"
        printf -- "            Please check.  Rename or remove and retry.\n"
        exit 1
    else
        if [ -e "${script_dirpath}/eslint" ]; then
            ln -s "${script_dirpath}/eslint" "$HOME/bin/eslint"  || exit 1
            printf -- "Installed : %s\n" "$(ls -l "${script_dirpath}/eslint")"
        else
            printf -- "***ERROR: the expected script file (%s) is not there!\n" "${script_dirpath}/eslint"
            printf -- "          Check your PEL repo!\n"
            exit 1
        fi
    fi
fi




# ----------------------------------------------------------------------------

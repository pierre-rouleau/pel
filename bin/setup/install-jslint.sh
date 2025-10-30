# SH FILE: install-jslint.sh
#
# Purpose   : Install jslint Javascript linter.
# Created   : Thursday, October 30 2025.
# Author    : Pierre Rouleau <prouleau001@gmail.com>
# Time-stamp: <2025-10-30 14:07:55 EDT, updated by Pierre Rouleau>
# ----------------------------------------------------------------------------
# Module Description
# ------------------
#
# Automate installation of JSLint following the instructions
# at https://github.com/jslint-org/jslint?tab=readme-ov-file#quickstart-install
#
# First create the ~/bin/javascript/jslint.mjs by downloading it
# from the official location: https://www.jslint.com/jslint.mjs
#
# Then copy the jslint executable script in ~/bin


# ----------------------------------------------------------------------------
# Dependencies
# ------------
#
# - printf, which, ln
# - curl

# ----------------------------------------------------------------------------
# Code
# ----
#
# Ref: https://github.com/jslint-org/jslint?tab=readme-ov-file#quickstart-install

pgm_name="$(basename "$0")"

# -- Validate environment
if ! which curl > /dev/null ; then
    printf -- "***ERROR: %s requires curl on PATH. It's not. Is it installed?" "$pgm_name"
    exit 1
fi

# -- Use directory names
script="$(realpath "$0")"
install_script_dirpath="$(dirname "$script")"
script_dirpath="$(dirname "$install_script_dirpath")"
bin_dir="$HOME/bin"
bin_js_dir="$HOME/bin/javascript"

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


# -- Check for presence of ~/bin/javascript, create it if missing, stop on error.
cd "$bin_dir" || exit 1
if [ ! -d "${bin_js_dir}" ]; then
    if [ ! -e "${bin_js_dir}" ]; then
        mkdir "${bin_js_dir}" || exit 1
    else
        printf -- "*** ERROR: %s exists but is not a directory!\n" "${bin_js_dir}"
        printf -- "           Please rename it to allow creation of a %s directory.\n" "${bin_js_dir}"
        exit 1
    fi
fi

# -- Copy https://www.jslint.com/jslint.mjs to ~/bin/javascript/jslint.mjs
#    If already present, copy over it: update it to the latest version.
#    Then check if we have one.
if [ -e "${bin_js_dir}/jslint.mjs" ]; then
    printf -- "Updating jslint.mjs ...\n"
else
    printf -- "Getting a copy of jslint.mjs ...\n"
fi
curl -L https://www.jslint.com/jslint.mjs > "${bin_js_dir}/jslint.mjs"
if [ ! -f "${bin_js_dir}/jslint.mjs" ]; then
    printf -- "*** ERROR: failed getting a copy of https://www.jslint.com/jslint.mjs\n"
    exit 1
fi

# -- Create a symlink in ~/bin to the jslint shell script
if [ -h "${bin_dir}/jslint"  ]; then
    current_target="$(readlink -f "${bin_dir}/jslint")"
    required_target="${script_dirpath}/jslint"
    if [ "${current_target}" = "${required_target}" ]; then
        if [ ! -e "${script_dirpath}/jslint" ]; then
            printf -- "***ERROR: the expected script file (%s) is not there!\n" "${script_dirpath}/jslint"
            printf -- "          Check your PEL repo!\n"
            exit 1
        else
            printf -- "***Warning: File %s already exists.\n" "$HOME/bin/$1"
        fi
    else
        printf -- "***ERROR  : File %s already exists but links to: %s\n" "$HOME/bin/$1" "${current_target}"
    fi
else
    if [ -e "${bin_dir}/jslint"  ]; then
        printf -- "***ERROR  : %s is a file, not a symlink.\n" "${bin_dir}/jslint"
        printf -- "            Please check.  Rename or remove and retry.\n"
        exit 1
    else
        if [ -e "${script_dirpath}/jslint" ]; then
            ln -s "${script_dirpath}/jslint" "$HOME/bin/jslint"  || exit 1
            printf -- "Installed : %s\n" "$(ls -l "${script_dirpath}/jslint")"
        else
            printf -- "***ERROR: the expected script file (%s) is not there!\n" "${script_dirpath}/jslint"
            printf -- "          Check your PEL repo!\n"
            exit 1
        fi
    fi
fi




# ----------------------------------------------------------------------------

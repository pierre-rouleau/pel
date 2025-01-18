#!/usr/bin/env bash
# SH FILE: find-perl
#
# Purpose   : TEST FILE Find all Perl files in specified directory tree.
# Created   : Saturday, January 18 2025 (a modified copy of something in my USRHOME)
# Author    : Pierre Rouleau <prouleau001@gmail.com>
# Time-stamp: <2025-01-18 13:20:34 EST, updated by Pierre Rouleau>
# ----------------------------------------------------------------------------
# Module Description
# ------------------
#
#


# ----------------------------------------------------------------------------
# Dependencies
# ------------
#
#


# ----------------------------------------------------------------------------
# Code
# ----
#
#

error_log="find-perl.log.txt"

print_usage()
{
    printf -- "
find-perl: Find and print the name of every Perl file found in directory tree.
           Search for files with  the following extensions:
          - .pl, .pm, .plx, .pls, .xs, .t, .pod, .cgi, .psg, .psgi

 Usage: find-perl -h|--help

  • Print this help information.

 Usage: find-perl {[-p|--with-system-perl] |[-P|--only-system-perl] [DIR, ...]

  • Search and print name of Perl files found in specified directories.
    - If DIR is not specified, search in the current working directory.
  • If -p or --with-system-perl:  search the directories that Perl
    identifies in its @INC variable as well as the other directories.
  • if the -P or --only-system-perl option is identified then ONLY
    search the the directories that Perl identifies in its @INC variable.
    When that option is included the DIR arguments are not allowed.
  • This also finds script files that uses a Perl shebang line.
  • The set of files are the scripts, then the pure Perl files are listed..

 ERRORS: All detected errors are appended to the file %s
         stored locally. The file is never deleted.  You must delete it
         manually first if you want to detect new errors.
" "${error_log}"
}

# --
# Check validity of arguments

if [ "$1" = "-h" ] || [ "$1" = "--help" ]; then
    print_usage
    exit 0
fi

search_other_directories=true
search_perl_system_directories=false
if [ -n "$1" ]; then
    if [ "$1" = "-p" ] || [ "$1" = "--with-system-perl" ]; then
        search_perl_system_directories=true
        shift
    elif [ "$1" = "-P" ] || [ "$1" = "--only-system-perl" ]; then
        search_perl_system_directories=true
        search_other_directories=false
        shift
        if [ -n "$1" ]; then
            printf -- "*** find-perl ERROR: no extra DIR with --only-system-perl.\n"
            printf -- "    Use 'find-perl -h' to get more information.\n"
            exit 1
        fi
    else
        if [ ! -d "$1" ]; then
            printf -- "*** find-perl ERROR: not a valid directory: %s\n" "$1"
            printf -- "    Use 'find-perl -h' to get more information.\n"
            exit 1
        fi
    fi
fi

# --
# Identify the command to use.

if [ -x "$(command -v fdfind)" ]; then
    search_with='fd'
    fd_pgm=fdfind
elif [ -x "$(command -v fd)" ]; then
    search_with='fd'
    fd_pgm=fd
else
    search_with='find'
    case $(uname) in
        Darwin)
            find_path_option='-f'
            ;;
        Linux)
            find_path_option=
            ;;
        *)
            printf -- "find-perl ** Error Operating System, %s, is not supported!\n" "$(uname)"
            exit 1
            ;;
    esac
fi

# -----------------------------------------------------------------------------
# Proceed:
#
# -x : do not search into other disks
# stderr redirected to /dev/null to hide warnings trying to access
# non-accessible directories.

# --
# Identify the directories to process

declare -a searched_directories
searched_directories=()
if [ "$search_other_directories" = "true" ]; then
    if [ -z "$1" ]; then
        searched_directories=( "$(pwd)" )
    else
        searched_directories=( "$@" )
    fi
fi

if [ "$search_perl_system_directories" = "true" ]; then
    # Add any Perl system directory that exist and is not a child of another
    for dname in $(perl -e 'print join("\n", @INC), "\n";' | sort | uniq); do
        found=false
        if [ -d "$dname" ] ; then
            dpath="$(realpath "${dname}")"
            for d in "${searched_directories[@]}"; do
                #shellcheck disable=SC2076 ## match literally the expansion of $d
                if [[ "$dname" =~ "$d" ]]; then
                    found=true
                    # printf -- "subdir: %s\n" $dname
                    break
                fi
            done
            if [ "$found" = "false" ]; then
                searched_directories+=("$dpath")
            fi
        fi
    done
fi

# --
# Search for shell scripts that use Perl: use rg if available, otherwise use grep.
#
# - Exclude perltidy files, Emacs backup and buffers files.

if [ -x "$(command -v rg)" ]; then
    for dname in "${searched_directories[@]}"; do
        rg -g !'*.tdy' -g !'*~' -g !'*#' -Nl "^#! ?.+/perl" "$(realpath "$dname")" 2>> "${error_log}"
    done
else
    # rg is not available; use recursive grep:
    # -l : just list file name
    # -r: recursive
    # -E: extended regular expressions
    # As it might iterate over files that have restricted access: wipe error messages
    for dname in "${searched_directories[@]}"; do
        grep -l --exclude '*.tdy' --exclude '*~' --exclude '*#' -r -E "^#! ?.+/perl" "$(realpath "$dname")" 2>> "${error_log}"
    done
fi

# --
# Search the Perl code files. Use fd if available, otherwise use find.
#
case "$search_with" in
    find)
        find -L                        \
             "${find_path_option}" "${searched_directories[@]}"  \
             \(  -name "*.p[lm]"       \
             -or -name "*.pl[sx]"      \
             -or -name "*.xs"          \
             -or -name "*.t"           \
             -or -name "*.pod"         \
             -or -name "*.cgi"         \
             -or -name "*.psg"         \
             -or -name "*.psgi"        \
             \)  -print 2>> "${error_log}"
        ;;

    fd)
        ${fd_pgm} --absolute-path \
                  --follow           \
                  --type f           \
                  --type l           \
                  -e .pl             \
                  -e .pm             \
                  -e .pls            \
                  -e .plx            \
                  -e .xs             \
                  -e .t              \
                  -e .pod            \
                  -e .cgi            \
                  -e .psg            \
                  -e .psgi           \
                  . "${searched_directories[@]}" 2>> "${error_log}"
        ;;

    *)
        printf -- "etags-perl ** Error: cannot identify find program!\n"
        exit 1
        ;;
esac

# ----------------------------------------------------------------------------
# Local Variables:
# sh-shell: /bin/bash
# End:

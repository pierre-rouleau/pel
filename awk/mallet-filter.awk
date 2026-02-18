# AWK FILE: mallet-filter.awk
#
# Purpose   : Reformat mallet output: generate file:line:column error format.
# Created   : Tuesday, February 17 2026.
# Author    : Pierre Rouleau <prouleau001@gmail.com>
# Time-stamp: <2026-02-18 08:00:42 EST, updated by Pierre Rouleau>
# ------------------------------------------------------------------------------
# Module Description
# ------------------
#
# Usage: gawk -f pel/awk/mallet-filter.awk
#
# This is used by the script ../bin/mallet-4emacs
#
# Replace:
#
# config.lisp
#   85:2     info        Use 'let' instead of 'let*' when bindings don't depend on each other  needless-let*
#   107:15   info        Use 'let' instead of 'let*' when bindings don't depend on each other  needless-let*
#   167:20   info        Use 'let' instead of 'let*' when bindings don't depend on each other  needless-let*
#   179:20   info        Use 'let' instead of 'let*' when bindings don't depend on each other  needless-let*
#   264:21   info        Use 'let' instead of 'let*' when bindings don't depend on each other  needless-let*
#   402:21   info        Use 'let' instead of 'let*' when bindings don't depend on each other  needless-let*
#
# ✗ 6 problems (6 info)
#
# by the following:
#
# - For config.lisp:
# config.lisp:85:2:     info        Use 'let' instead of 'let*' when bindings don't depend on each other  needless-let*
# config.lisp:107:15:   info        Use 'let' instead of 'let*' when bindings don't depend on each other  needless-let*
# config.lisp:167:20:   info        Use 'let' instead of 'let*' when bindings don't depend on each other  needless-let*
# config.lisp:179:20:   info        Use 'let' instead of 'let*' when bindings don't depend on each other  needless-let*
# config.lisp:264:21:   info        Use 'let' instead of 'let*' when bindings don't depend on each other  needless-let*
# config.lisp:402:21:   info        Use 'let' instead of 'let*' when bindings don't depend on each other  needless-let*
#
# ✗ 6 problems (6 info)

# ------------------------------------------------------------------------------
# Dependencies
# ------------
#
# - GNU awk
#
# ------------------------------------------------------------------------------
# Code
# ----
#
#

BEGIN {
    FS = " "; 	  # Field separator
    OFS = " "; 	  # Output Field separator
    line_printed=0
    file_name=""
}

# Identify the file name and remember it in 'file_name':
# The file name must start at the beginning of the line and may hold path
# Unix or Windows path.
# Currently limiting the special characters in file names.
$0 ~ /^[[:alnum:]_.-/\\:]+/ {
    file_name = $0
    printf "- For: %s:\n", $0
    line_printed=1
}

# For lines error lines, print them starting with the file name in the
# following format: file-name:line:column error message
#
$0 ~ /^ +[0-9]+:[0-9]+/ {

    # Remember the line following
    restofline=$0

    # Remove leading spaces from it
    sub(/^[ \t]+/, "", restofline)

    # Then remove the line:column field ($1) from it.
    sub($1, "", restofline)

    # print if with the file name in front of it
    printf "%s:%s: %s\n", file_name, $1, restofline

    # remember it was printed
    line_printed=1
}

$0 ~ / [0-9]+ problems / {
    line_printed = 0
}

$0 ~ /^$/ {
    line_printed = 0
}

# print through anything no previously handled.
line_printed == 0 { print }

# ------------------------------------------------------------------------------

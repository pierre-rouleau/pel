# AWK FILE: rpmlint-fileter.awk
#
# Purpose   : Reformat rpmlint output to allow use in emacs compile-mode buffers.
# Created   : Wednesday, November 20 2024.
# Author    : Pierre Rouleau <prouleau001@gmail.com>
# Time-stamp: <2024-11-21 17:10:58 EST, updated by Pierre Rouleau>
# ----------------------------------------------------------------------------
# Module Description
# ------------------
#
# A filter to convert the sloppy rpmlint output format to something Emacs compile-mode
# can use to identify the error and warning lines.
#
#
# First convert the rpmlint error/warning lines from:
#
# msl-selinux-policy.spec: E: specfile-error error: line 4: Unknown tag: %de fine version 0.0.2
# msl-selinux-policy.spec: E: specfile-error error: query of specfile msl-selinux-policy.spec failed, can't parse
# msl-selinux-policy.spec:8: W: hardcoded-packager-tag MajorSystemLtd Corp.
# msl-selinux-policy.spec: W: invalid-url Source0: msl-selinux-policy-0.0.2.tar.gz
#
# To:
#
# msl-selinux-policy.spec:4: Error: Unknown tag: %de fine version 0.0.2
# msl-selinux-policy.spec:1: Error: query of specfile msl-selinux-policy.spec failed, can't parse
# msl-selinux-policy.spec:8: Warning: hardcoded-packager-tag MajorSystemLtd Corp.
# msl-selinux-policy.spec:1: Warning: invalid-url Source0: msl-selinux-policy-0.0.2.tar.gz

# Required translations:
#
# Field separator: ':'
# Field 1: file-name
# Field 2:
# - may be a number: that's a line number keep it
# - may be E or W: replace E: by Error:, W: by Warning:
#
# Field 3: may be "specfile-error error"  : remove that field.
#
# Field 4: may be "line ..." : extract the line number and place it after the file-name


# ----------------------------------------------------------------------------
# Dependencies
# ------------
#
# - GNU awk

# ----------------------------------------------------------------------------
# Code
# ----
#
#

BEGIN {
    FS = ":"; 	  # Field separator
    OFS = ":"; 	  # Output Field separator
    line_printed=0
}

# For each line, reset the line_printed flag
$0 ~ / .+ / {
    line_printed=0
}

$4 ~ / line [0-9]+/ {
    # Extract the line number
    line_number = gensub(" line ", "", 1, $4)
    # print file name (field 1), line number, separated by colons
    file_name = $1
    printf "%s:%s: ", file_name, line_number

    # print remainder of the line
    # Print the remainder of the line.

    restofline=gensub($1, "", 1, $0 );
    sub($4, "", restofline);
    sub(": E: ", "Error:", restofline);
    sub("specfile-error error::", "", restofline);
    print restofline
    line_printed=1
}

$2 ~ / E/ {
    $2="1"
    $3=" Error"
}

$2 ~ / W/ {
    replacement= $1 ":1"
    restofline=gensub($1, replacement, 1, $0);
    sub(" W:", " Warning:",restofline);
    print restofline
    line_printed=1
}

$3 ~ " E" {
    $3=" Error"
}

$3 ~ " W" {
    $3=" Warning"
}

line_printed == 0 { print }

# ----------------------------------------------------------------------------

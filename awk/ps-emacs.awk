# AWK FILE: ps-emacs.awk
#
# Purpose   : List Emacs processes.
# Created   : Monday, September 29 2025.
# Author    : Pierre Rouleau <prouleau001@gmail.com>
# Time-stamp: <2026-11-01 08:58:01 EST, updated by Pierre Rouleau>
# ------------------------------------------------------------------------------
# Module Description
# ------------------
#
#
# The purpose of this script is to take the output of the command
#
#     ps-list emacs | grep " tty" | grep -v ps-emacs
#
# that prints lines like this:
#
#   501 98197     1   0 Fri10am ttys003    0:36.73 /Users/some/my/bin/emacs --chdir=/Users/some/my/dvp/elisp/veri-kompass
#   501 15680 15679   0  8Sep25 ttys007    1:28.05 emacs -nw .
#
# where the columns are:
# 1   UID  : user ID
# 2   PID  : process ID
# 3   PPID : parent process ID
# 4   C    : CPU utilization of process or thread
# 5   STIME: process start time
# 6   TTY  : controlling terminal
# 7   TIME : total CPU time (user + system) since it started
# 8   CMD  : command
#
# Print each original record whose command field (field 8) is exactly "emacs".
# This comparison is case-sensitive and does not accept executable paths.
#
# This script only filters records. It does not look up a CWD or change
# the record's formatting.
#
# ------------------------------------------------------------------------------
# Dependencies
# ------------
#
# Regular AWK.

# ------------------------------------------------------------------------------
# Code
# ----
#
# Accept emacs, emacs.app, emacsclient, emacs-lucid, emacs-gtk, emacs-pgtk,
# emacs-nox, emacs-x11, emacs-athena, emacs-nextstep, emacs-snapshot, emacs-30
# and other 2x and 3x number, and architecture-suffixed arm64 or x86_64 Emacs
# names with numeric version components separated by underscores or dots.
#
$8 ~ /(^|\/)[eE]macs(\.app|client|-lucid|-p?gtk|-nox|-x11|-athena|-nextstep|-snapshot|-[23][0-9]|-(arm64|x86_64)-[0-9]+([_.][0-9]+)*)?$/  { print $0 }

# ------------------------------------------------------------------------------

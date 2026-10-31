# AWK FILE: ps-emacs.awk
#
# Purpose   : List Emacs processes.
# Created   : Monday, September 29 2025.
# Author    : Pierre Rouleau <prouleau001@gmail.com>
# Time-stamp: <2026-10-30 23:00:03 EDT, updated by Pierre Rouleau>
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
# 3   PPID : Parent process ID
# 4   C    : CPU utilization of process or thread
# 5   STIME: system CPU time
# 6   TTY  : controlling terminal
# 7   TIME : total CPU time (user + system) since it started
# 8   CMD  : command
#
# The script identify emacs processes from the 8th column and print these lines.
#
# To extract the current working directory of a process ID, we use
# lsof and AWK again:
#
#   lsof -a -d cwd -p $PPID -n -Fn | awk '/^n/ {print substr($0,2)}'
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
#
$8 == "emacs"  { print $0 }

# ------------------------------------------------------------------------------

# GAWK FILE: ps-emacs-format.awk
#
# Purpose   : Format Emacs process list to add the CWD to the line.
# Created   : Sunday, September 21 2025.
# Author    : Pierre Rouleau <prouleau001@gmail.com>
# Time-stamp: <2026-10-31 11:17:17 EDT, updated by Pierre Rouleau>
# ------------------------------------------------------------------------------
# Module Description
# ------------------
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
#
# For each matching record, the script passes the PID from field 2 to
# pidcwd and reads one output line. It appends that result to the original
# record, after the "▶︎▶︎▶︎" marker and the "CWD:" label.
#
# The marker separates the process record from the CWD information.
# The caller uses column to align the CWD information across records.
# This script does not change the TTY field.
#
# The CWD lookup uses the Emacs process's PID, not its parent's PID.
# This script delegates the lookup to pidcwd.
#
# ----------------------------------------------------------------------------
# Dependencies
# ------------
#
# - GNU AWK for the |& coprocess operator,
# - pidcwd
#
# ------------------------------------------------------------------------------
# On all lines, extract the process id (from column 2) and use the pidcwd
# program to extract the current working directory of that process.
# print the line and then the current working directory.
$8 ~ "[eE]macs"  {
      command = "pidcwd " $2
      command |& getline path_result
      close(command)
      print $0, "  ▶︎▶︎▶︎ CWD: ", path_result
}

# ------------------------------------------------------------------------------

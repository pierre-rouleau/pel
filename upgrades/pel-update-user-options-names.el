;;; pel-update-user-options-names.el --- Rename the user options names.  -*- lexical-binding: t; -*-

;; Created   : Tuesday, October 28 2025.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2025-10-29 13:44:31 EDT, updated by Pierre Rouleau>

;; This file-path is part of the PEL package.
;; This file-path is not part of GNU Emacs.

;; Copyright (C) 2025  Pierre Rouleau
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; --------------------------------------------------------------------------
;;; Commentary:
;;
;; This file holds the logic required to change the name of several PEL
;; customizable user-mode variables in the Emacs customization files that
;; activates them.
;;
;; This script was written to help PEL users migrating their Emacs
;; customization files, or other non PEL code that uses PEL, following the
;; change done in October 28 2025 on the name of several of the PEL
;; customizable user-options.
;;
;; You can use the internal function directly or use this file as an Emacs
;; batch script.   The upgrade-01 shell script, located in the same directory,
;; eases the invocation of the Emacs script.

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;

;;; --------------------------------------------------------------------------
;;; Code:
;;

(defun pel-rename (old new)
  "Rename all instances of OLD to NEW in the file.
Return change count."
  (let ((word-regexp (format "\\<%s\\>" old))
        (change-count 0))
    (goto-char (point-min))
    (while (re-search-forward word-regexp nil t)
      (setq change-count  (1+ change-count))
      (replace-match new nil t))
    change-count))

(defun pel-update-names ()
  "Perform the following user-option name changes:
  - pel-use-asn1-mode        to pel-use-asn1
  - pel-use-bison-mode       to pel-use-bison
  - pel-use-changelog-mode   to pel-use-changelog
  - pel-use-cmake-mode       to pel-use-cmake
  - pel-use-crontab-mode     to pel-use-crontab
  - pel-use-csv-mode         to pel-use-csv
  - pel-use-cwl-mode         to pel-use-cwl
  - pel-use-diffview-mode    to pel-use-diffview
  - pel-use-dockerfile-mode  to pel-use-dockerfile
  - pel-use-gitignore-mode   to pel-use-gitignore
  - pel-use-hgignore-mode    to pel-use-hgignore
  - pel-use-ijanet-mode      to pel-use-ijanet
  - pel-use-intel-hex-mode   to pel-use-intel-hex
  - pel-use-kconfig-mode     to pel-use-kconfig
  - pel-use-log4j-mode       to pel-use-log4j
  - pel-use-meson-mode       to pel-use-meson
  - pel-use-mscgen-mode      to pel-use-mscgen
  - pel-use-ninja-mode       to pel-use-ninja
  - pel-use-nix-mode         to pel-use-nix
  - pel-use-org-mode         to pel-use-org
  - pel-use-rails-log-mode   to pel-use-rails-log
  - pel-use-rfc-mode         to pel-use-rfc
  - pel-use-rpm-spec-mode    to pel-use-rpm-spec
  - pel-use-rst-mode         to pel-use-rst
  - pel-use-strace-mode      to pel-use-strace
  - pel-use-syslog-mode      to pel-use-syslog
  - pel-use-yaml-mode        to pel-use-yaml
  - pel-use-yang-mode        to pel-use-yang
  - pel-use-x509-mode        to pel-use-x509-modes

Return number of changes."
  (let ((change-count 0))
    (setq change-count
          (+
           (pel-rename "pel-use-asn1-mode"       "pel-use-asn1")
           (pel-rename "pel-use-bison-mode"      "pel-use-bison")
           (pel-rename "pel-use-changelog-mode"  "pel-use-changelog")
           (pel-rename "pel-use-cmake-mode"      "pel-use-cmake")
           (pel-rename "pel-use-crontab-mode"    "pel-use-crontab")
           (pel-rename "pel-use-csv-mode"        "pel-use-csv")
           (pel-rename "pel-use-cwl-mode"        "pel-use-cwl")
           (pel-rename "pel-use-diffview-mode"   "pel-use-diffview")
           (pel-rename "pel-use-dockerfile-mode" "pel-use-dockerfile")
           (pel-rename "pel-use-gitignore-mode"  "pel-use-gitignore")
           (pel-rename "pel-use-hgignore-mode"   "pel-use-hgignore")
           (pel-rename "pel-use-ijanet-mode"     "pel-use-ijanet")
           (pel-rename "pel-use-intel-hex-mode"  "pel-use-intel-hex")
           (pel-rename "pel-use-kconfig-mode"    "pel-use-kconfig")
           (pel-rename "pel-use-log4j-mode"      "pel-use-log4j")
           (pel-rename "pel-use-meson-mode"      "pel-use-meson")
           (pel-rename "pel-use-mscgen-mode"     "pel-use-mscgen")
           (pel-rename "pel-use-ninja-mode"      "pel-use-ninja")
           (pel-rename "pel-use-nix-mode"        "pel-use-nix")
           (pel-rename "pel-use-org-mode"        "pel-use-org")
           (pel-rename "pel-use-rails-log-mode"  "pel-use-rails-log")
           (pel-rename "pel-use-rfc-mode"        "pel-use-rfc")
           (pel-rename "pel-use-rpm-spec-mode"   "pel-use-rpm-spec")
           (pel-rename "pel-use-rst-mode"        "pel-use-rst")
           (pel-rename "pel-use-strace-mode"     "pel-use-strace")
           (pel-rename "pel-use-syslog-mode"     "pel-use-syslog")
           (pel-rename "pel-use-yaml-mode"       "pel-use-yaml")
           (pel-rename "pel-use-yang-mode"       "pel-use-yang")
           (pel-rename "pel-use-x509-mode"       "pel-use-x509-modes")))
    change-count))

(defun pel-update-names-in-file (file-path)
  "Open FILE-PATH, replace all necessary PEL user-options and save.
   After completion print message.
   The function will print a message to the console."
  (with-current-buffer (find-file-noselect file-path)
    (let ((change-count (pel-update-names)))
      (if (= change-count 0)
          (message "No change required in %s" file-path)
        (save-buffer)
        (message "Replaced %d user-options in %s" change-count file-path)))))


(defun pel-update-user-options-names ()
  "Update the name of affected PEL user-options in specified files.

The command line identifies the name of the file identified on the
command line."
  (dolist (arg command-line-args-left)
    (if (string= arg "--pel-help")
        (progn
          (message "Replace PEL user-option names.
Usage:
 emacs --batch -l pel-update-user-options-names.el\
 -f pel-update-user-options-names  <file1> <file2>...

Note: The command can be used multiple times on a file;
      the replacement will only be performed once.
")
          (kill-emacs))
      ;; No help requested, proceed each arg as a file name.
      (when (file-exists-p arg)
        (pel-update-names-in-file arg)))))

;;; --------------------------------------------------------------------------
(provide 'pel-update-user-options-names)

;;; pel-update-user-options-names.el ends here

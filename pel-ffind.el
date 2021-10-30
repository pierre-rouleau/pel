;;; pel-ffind.el --- PEL file find utilities.  -*- lexical-binding: t; -*-

;; Created   : Saturday, October 30 2021.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2021-10-30 19:35:30, updated by Pierre Rouleau>

;; This file is part of the PEL package.
;; This file is not part of GNU Emacs.

;; Copyright (C) 2021  Pierre Rouleau
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
;; This provides the `pel-ffind' function that find files using either the
;; find or fd command line utilities, as identified by the
;; `pel-ffind-executable' user-option.

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'pel--options)                 ; use: `pel-ffind-executable'
(eval-when-compile (require 'subr-x))   ; use: `string-join', `string-trim'

;;; --------------------------------------------------------------------------
;;; Code:
;;

(defvar pel--ffind-fd-path nil
  "Full path of fd executable if found.")
(defvar pel--ffind-find-path nil
  "Full path of find executable if found.")

(defun pel-ffind-command (filename directories)
  "Return a ffind command searching for FILENAME in DIRECTORIES.

FILENAME may be a glob pattern.
The command returned will produce a list of files sorted in lexicographic
order."
  (cond
   ((eq pel-ffind-executable 'fd)
    (unless (or pel--ffind-fd-path
                (setq pel--ffind-fd-path (executable-find "fd")))
      (user-error "pel-ffind-executable is fd, but can't find it!"))
    ;; fd sorts by default.
    (format "%s --type f --color never -g '%s' '%s'"
            pel--ffind-fd-path
            filename
            (string-join (mapcar (function directory-file-name)
                                 directories)
                         " ")))
   ((eq pel-ffind-executable 'find)
    (unless (or pel--ffind-find-path
                (setq pel--ffind-find-path (executable-find "find")))
      (user-error "pel-ffind-executable is find, but can't find it!"))
    ;; find requires the -s option to sort.
    (format "%s -s '%s' -name '%s' -type f"
            pel--ffind-find-path
            (string-join (mapcar (function directory-file-name)
                                 directories)
                         " ")
            filename))))

(defun pel-ffind (filename &optional directories)
  "Search for FILENAME in current directory tree or in DIRECTORIES.

The function searches in the director trees identified by:

- in the DIRECTORIES argument if specified, otherwise
- in the pel-ffind-directories if non nil, otherwise
- in the current directory.

Returns a list of string, each string is the path of a file found.

Uses shell command identified by `pel-ffind-executable'.
Note that fd ignore files identified in the .gitignore, .fdignore
or .ignore file but find does not ignore them."
  (unless directories
    (setq directories (list default-directory)))
  (split-string
   (string-trim
    (shell-command-to-string (pel-ffind-command filename directories)))))

;;; --------------------------------------------------------------------------
(provide 'pel-ffind)

;;; pel-ffind.el ends here

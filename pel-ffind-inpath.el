;;; pel-ffind-inpath.el --- PEL file find searching in specified directories  -*- lexical-binding: t; -*-

;; Created   : Monday, November 29 2021.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2026-03-28 17:20:44 EDT, updated by Pierre Rouleau>

;; This file is part of the PEL package.
;; This file is not part of GNU Emacs.

;; Copyright (C) 2021, 2022, 2026  Pierre Rouleau
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
;; This provides functions to search for file in a set of directories
;; identified in a list.  The list may be specified by PATH-like or
;; INCLUDE-like environment variables.  For example, this is useful to search
;; files like a C compiler would do.
;;
;; This defines:
;; - `pel-ffind-inpath-include' which searches files in the
;;   directories identified by the INCLUDE environment variable or another
;;   similar environment variable.
;; - `pel-ffind-inpath' which searches files in the specified list of
;;   directories.

;;  The code hierarchy is:
;;
;; - `pel-ffind-inpath-include'
;;   - `pel-ffind-inpath'

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'pel--base)            ; use: `pel-list-of'
(require 'seq)                  ; use: `seq-uniq'

;;; --------------------------------------------------------------------------
;;; Code:
;;

(defun pel-ffind-inpath (filename paths)
  "Find FILENAME from the directories identified in PATHS.

FILENAME must be a string that represents a file name.  The
filename must not have an absolute path but may have a partial
relative path.  If FILENAME is an absolute path the function
issues a user-error.

PATHS is either a directory name string or a list of directories; those
directories are searched.  Only files located in the directories listed
are found; this function does *not* perform a directory tree search.

The function returns a list of file names with absolute path.
If several files are found they are returned in the order of the directories
in PATHS.  If nothing is found the function returns nil.

Note that this function is similar to Emacs built-in
`locate-file' except that `locate-file' only returns the first
found file and `pel-ffind-inpath' returns all files found."
  (when (file-name-absolute-p filename)
    (user-error
     "pel-ffind-inpath called with an absolute path in: %s"
     filename))
  (let ((found-fnames nil)
        fname)
    (dolist (dir (pel-list-of paths) (nreverse found-fnames))
      (setq fname (expand-file-name filename dir))
      (when (file-exists-p fname)
        (push fname found-fnames)))))

(defun pel-ffind-inpath-include (filename &optional include-env-var)
  "Find file FILENAME in the directories identified by an environment variable.

FILENAME must not have an absolute path, if it is absolute, the function
issues a user-error.

The function searches in the directories identified by the INCLUDE
environment variable unless another environment variable is
specified by the INCLUDE-ENV-VAR optional argument.

It returns a list of absolute path strings, the name of the found
file or files found in the directories identified by the environment variable.

The function issues a user-error if the specified environment variable
does not exist, has no value, or its value yields no valid directory paths
\(e.g. it contains only path separators)."
  (let* ((envvar-name (or include-env-var "INCLUDE"))
         (envvar-value (getenv envvar-name))
         (paths (when (and envvar-value (not (string= envvar-value "")))
                  ;; - remove empty entries (when 2 path-separators are used)
                  ;; - trim leading and trailing whitespace
                  ;; - remove duplicates
                  (seq-uniq (split-string envvar-value path-separator
                                          'omit-nulls "[[:blank:]]*")))))
    (if paths
        (pel-ffind-inpath filename paths)
      (user-error
       "Content of environment variable %s does not identify any directory"
       envvar-name))))

;;; --------------------------------------------------------------------------
(provide 'pel-ffind-inpath)

;;; pel-ffind-inpath.el ends here

;;; pel-ffind-inpath.el --- PEL file find searching in specified directories.  -*- lexical-binding: t; -*-

;; Created   : Monday, November 29 2021.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2021-11-29 16:34:38, updated by Pierre Rouleau>

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

;;; --------------------------------------------------------------------------
;;; Code:
;;

(defun pel-ffind-inpath (filename path)
  "Find FILENAME from the directories identified in PATH.

FILENAME must be a string that represent a file name.  The
filename must not have a absolute path but may have a partial
relative path.

PATH must be a list of directories.

The function returns a list of file names with absolute path.
If several files are found they are returned in the order of the directories
in the PATH.  If nothing is found the function returns nil.

Note that this function is similar to Emacs built-in
`locate-file' except that `locate-file' only returns the first
found file and `pel-ffind-inpath' returns all files found."
  (let ((found-fnames nil)
        fname)
    (dolist (dir path (reverse found-fnames))
      (setq fname (expand-file-name filename dir))
      (when (file-exists-p fname)
        (push fname found-fnames)))))

(defun pel-ffind-inpath-include (filename &optional include-env-var)
  "Find file FILENAME in the directories identified by the INCLUDE env variable.

The function search in the directories identified by the INCLUDE
environment variable unless another environment variable is
specified by the INCLUDE-ENV-VAR optional argument.

The function issues an error if the specified environment
variable does not exist or it has no value."
  (let ((path (split-string
               (or (getenv (or include-env-var "INCLUDE")) "")
               path-separator)))
    (if path
        (pel-ffind-inpath filename path)
      (user-error
       (format "Environment variable %s does not exist or holds no value"
               (or include-env-var "INCLUDE"))))))

;;; --------------------------------------------------------------------------
(provide 'pel-ffind-inpath)

;;; pel-ffind-inpath.el ends here

;;; pel-ffind.el --- PEL file find utilities.  -*- lexical-binding: t; -*-

;; Created   : Saturday, October 30 2021.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2025-03-02 10:07:36 EST, updated by Pierre Rouleau>

;; This file is part of the PEL package.
;; This file is not part of GNU Emacs.

;; Copyright (C) 2021, 2022, 2024, 2025  Pierre Rouleau
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
;; This provides the functions that find files.
;;
;;  - `pel-generic-find-file' searches a file inside one or several directory
;;    trees.
;;  - `pel-ffind' function that find files using either the
;;    find or fd command line utilities, as identified by the
;;    `pel-ffind-executable' user-option.

;; * `pel-ffind'
;;   - `pel-ffind-command'
;;     - `pel--ffind-dirname-quoted'
;;
;; * `pel-generic-find-file'
;;   - `pel-ffind-project-directory'
;;   - `pel-ffind'

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'pel--base)                    ; use: `pel-system-is-macos-p'
(require 'pel--options)                 ; use: `pel-ffind-executable'
(eval-when-compile (require 'subr-x))   ; use: `string-join', `string-trim'

;;; --------------------------------------------------------------------------
;;; Code:
;;

(defvar pel--ffind-fd-path nil
  "Full path of fd executable if found.")
(defvar pel--ffind-find-path nil
  "Full path of find executable if found.")

(defun pel--ffind-dirname-quoted (dirname)
  "Return DIRNAME in quote and without trailing slash."
  (format "'%s'" (directory-file-name dirname)))

(defun pel-ffind-command (filename directories)
  "Return a ffind command searching for FILENAME in DIRECTORIES.

FILENAME may be a glob pattern.
It may contain a partial directory path.
The command returned will produce a list of files sorted in lexicographic
order.

The VCS ignore capability of fd is not used, so all files are found
whether the VCS is told to ignore them or not."
  (let ((file-basename (file-name-nondirectory filename))
        (dirnames (if (> (length directories) 1)
                      (string-join (mapcar #'pel--ffind-dirname-quoted
                                           directories)
                                   " ")
                    (car directories))))
    (cond
     ;; -- fd is specified
     ((eq pel-ffind-executable 'fd)
      (unless (or pel--ffind-fd-path
                  (setq pel--ffind-fd-path (executable-find "fd")))
        (user-error "pel-ffind-executable is fd, but can't find it!"))
      ;; fd sorts by default.
      (format "%s --type f --color never --no-ignore-vcs -g '%s' %s"
              pel--ffind-fd-path
              file-basename
              dirnames))
     ;; -- 'find is specified
     ((eq pel-ffind-executable 'find)
      (unless (or pel--ffind-find-path
                  (setq pel--ffind-find-path (executable-find "find")))
        (user-error "pel-ffind-executable is find, but can't find it!"))
      ;; on macOS find requires the -s option to sort.
      ;; That option is not supported ;; on Linux
      (let ((sort-option (if pel-system-is-macos-p "-s" "")))
        (format "%s %s %s -name '%s' -type f"
                pel--ffind-find-path
                sort-option
                dirnames
                file-basename)))
     ;; -- explicit command line
     ;; A string with the following keywords replaced:
     ;; {FNAME}    : the base name of the file
     ;; {DIRNAMES} : a space separated list of directory names to search
     ((stringp pel-ffind-executable)
      (let ((cmd pel-ffind-executable))
        (setq cmd (replace-regexp-in-string "{FNAME}"
                                            file-basename
                                            cmd
                                            'fixed-case
                                            'literal))
        (setq cmd (replace-regexp-in-string "{DIRNAMES}"
                                            dirnames
                                            cmd
                                            'fixed-case
                                            'literal))
        cmd)))))

;;-pel-autoload
(defun pel-ffind (filename &optional directories)
  "Search for FILENAME in current or specified DIRECTORIES trees.

The function searches in the directory trees identified by:

- in the DIRECTORIES argument if specified, otherwise
- in the pel-ffind-directories if non nil, otherwise
- in the current directory.

Returns a list of string, each string is the path of a file found.

Uses shell command identified by `pel-ffind-executable'.
The \\='VCS ignore capability\\=' of fd is not used, so all files are found
whether the VCS is told to ignore them or not."
  (unless directories
    (setq directories (list default-directory)))
  (let ((found-files
         (split-string
          (string-trim
           (shell-command-to-string (pel-ffind-command filename
                                                       directories))))))
    ;; When filename has a directory portion it is ignored in the search
    ;; command created by pel-ffind-command otherwise the find or fd search
    ;; fails.  The result might include files that are not inside the
    ;; specified  directory then.  Remove these files from the result.
    (when (and found-files
               (file-name-directory filename))
      (let ((dir-portion (file-name-directory filename)))
        (setq found-files
              (seq-filter (lambda (fname)
                            (string-match dir-portion fname))
                          found-files))))
    found-files))

;; ---------------------------------------------------------------------------

(defun pel-ffind-project-directory (&optional project-root-identifiers)
  "Find and return the project root directory of file in current buffer.

Search project root directory using the identifier files specified in the
`pel-project-root-identifiers' user-option and the ones in
PROJECT-ROOT-IDENTIFIERS list if specified.

Return a directory name expanded and without trailing slash if found,
nil otherwise."
  (let ((identifiers pel-project-root-identifiers)
        (directory nil)
        (found-dir nil))
    ;; make a list of all project root identifiers
    (dolist (fname project-root-identifiers)
      (unless (member fname identifiers)
        (push fname identifiers)))
    ;; search project root from current directory up looking for a
    ;; project root identifier file.  Retain the shortest directory path found
    ;; to allow nested projects and keep the more encompassing one, broadening
    ;; the file search: anyway if more than 1 file found the user will be
    ;; prompted.
    ;;
    (dolist (fname identifiers)
      (setq found-dir (locate-dominating-file default-directory fname))
      (when found-dir
        (if directory
            (when (< (length found-dir) (length directory))
              (setq directory found-dir))
          (setq directory found-dir))))
    ;; Return a directory name expanded and without trailing slash.
    (when directory
      (expand-file-name (directory-file-name directory)))))

;;-pel-autoload
(defun pel-generic-find-file (filename &optional directories)
  "Find a file FILENAME from the project holding the current buffer file.

Return a list of found file names with complete absolute path.
Return nil if nothing found.

All file searches are done in directory *trees* identified by the current
project and optionally by the extra DIRECTORIES.

If DIRECTORIES argument is specified it may be a single directory
path string or a list of directory path strings to search on top
of the current project directory.

The `pel-generic-find-file' is the most generic method to search
for files, can be set as the default value for the global value
of the `pel-filename-at-point-finders' buffer local variable used
by `pel--find-by-finders'.

However, that may not be sufficient for some programming
languages.  In that case you should be using a language specific function.
The languages specific file finders implemented by PEL are:
- For Erlang: `pel-erlang-find-file'.
- For Perl:   `pel-perl-find-file'"
  (let ((candidate-dir (pel-ffind-project-directory)))
    (condition-case nil
        (let* ((searched-directories (if candidate-dir
                                         (cons candidate-dir directories)
                                       directories))
               (uniq-searched-dirs (delete-dups searched-directories)))
          (pel-ffind filename uniq-searched-dirs))
      (user-error nil))))

;;; --------------------------------------------------------------------------
(provide 'pel-ffind)

;;; pel-ffind.el ends here

;;; pel-ffind.el --- PEL file find utilities  -*- lexical-binding: t; -*-

;; Created   : Saturday, October 30 2021.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2026-04-14 10:12:55 EDT, updated by Pierre Rouleau>

;; This file is part of the PEL package.
;; This file is not part of GNU Emacs.

;; Copyright (C) 2021, 2022, 2024-2026  Pierre Rouleau
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
;;  - `pel-ffind' function that find files using either the find or fd command
;;    line utilities, as identified by the `pel-ffind-executable' user-option.
;;  - `pel-generic-find-file' searches a file inside one or several directory
;;    trees.

;; * `pel-generic-find-file'
;;   - `pel-ffind-project-directory'
;;   * `pel-ffind'
;;     - `pel-ffind-command'
;;       - `pel--ffind-select-tool'
;;       - `pel-ffind-dirname-expanded'
;;

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'pel--base)                  ; use: `pel-system-is-macos-p',
;;                                    ;      `pel-major-mode-symbol-value-or'
;;                                    ;      `pel-major-mode-of-file'
;;                                    ;      `pel-language-of'
;;                                    ;      `pel-string-for'
;;                                    ;      `pel-path='
;;                                    ;      `pel-substitute-in-file-name'
(require 'pel--options)               ; use: `pel-ffind-executable'
(eval-when-compile (require 'subr-x)) ; use: `string-join', `string-trim'
(require 'seq)                        ; use: `seq-filter'

;;; --------------------------------------------------------------------------
;;; Code:
;;

(defun pel-ffind-dirname-expanded (dirname)
  "Return DIRNAME in quote, expanded and without trailing slash.
Substitute the ~ with the home directory.
Replace the name of environment variables with their values."
  (format "'%s'" (directory-file-name
                  (expand-file-name
                   (pel-substitute-in-file-name  dirname)))))

;; ---------------------------------------------------------------------------
(defvar pel--ffind-executable nil
  "Adjusted value of `pel-ffind-executable.")

(defvar pel--ffind-path nil
  "Full path of fd/fdfind/find executable used.")

(defun pel--ffind-select-tool ()
  "Select the search tool based on `pel-ffind-executable' choice.
Return a (symbol . string) cons cell with the two values to store in
`pel--ffind-executable' and `pel--ffind-path'."
  (let (choice exe-path)
    (cond
     ((eq pel-ffind-executable 'fd)
      ;; Some system (eg. Debian use fdfind instead of fd, allowing another,
      ;; unrelated fd command: search for fdfind first.
      (setq exe-path (executable-find "fdfind"))
      (unless exe-path
        (setq exe-path (executable-find "fd"))
        (if exe-path
            (setq choice 'fd)
          (display-warning 'pel-ffind
                           "\
pel-ffind-executable requests fd but its not available: using find instead."
                           :warning)
          (setq choice 'find)
          (setq exe-path (executable-find "find")))))
     ((eq pel-ffind-executable 'find)
      (setq choice 'find)
      (setq exe-path (executable-find "find"))))
    (unless exe-path
      (user-error "\
pel--ffind-executable attempts to use %s, but it is not available!" choice))
    (cons choice exe-path)))

(defun pel-ffind-command (filename directories)
  "Return a ffind command searching for FILENAME in DIRECTORIES.

FILENAME may be a glob file pattern, that start with an absolute or relative
directory path.

The directories may start with \"~\" and contain environment variables
prefixed with a '$' character and ending with a non alpha-numeric or
underscore  character.

The returned command will produce a list of files sorted in lexicographic
order.

The VCS-ignore capability of fd is not used, so all files are found
whether the VCS setting (like the .gitignore file) is set to ignore them."
  (let ((file-basename (pel-shell-quote-path-keep-glob
                        (file-name-nondirectory filename)))
        (dirnames (if (> (length directories) 1)
                      (string-join (mapcar #'pel-ffind-dirname-expanded
                                           directories)
                                   " ")
                    (pel-ffind-dirname-expanded (car directories)))))

    ;; Initialize tool selection and its path if not already done.
    (unless pel--ffind-executable
      (let ((choice.exe-path (pel--ffind-select-tool)))
        (setq pel--ffind-executable (car choice.exe-path))
        (setq pel--ffind-path (cdr choice.exe-path))))

    (cond
     ;; -- using fd (or fdfind):
     ((eq pel-ffind-executable 'fd)
      ;; fd sorts by default.  The -g option (for globs) can't handle file
      ;; names that start with a dash: it must be preceded with a backslash.
      (when (pel-string-starts-with-p file-basename "-")
        (setq file-basename (concat "\\" file-basename)))
      (format "%s --type f --color never --no-ignore-vcs -g '%s' %s"
              pel--ffind-path
              file-basename
              dirnames))
     ;;
     ;; -- using find:
     ;; find handles file names that start with a dash properly: no need for
     ;; special escape.
     ((eq pel-ffind-executable 'find)
      ;; On macOS find requires the -s option to sort.
      ;; That option is not supported on Linux
      (let ((sort-option (if pel-system-is-macos-p "-s" "")))
        (format "%s %s %s -name '%s' -type f"
                pel--ffind-path
                sort-option
                dirnames
                file-basename)))
     ;;
     ;; -- using an explicit command line:
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
(defun pel-ffind (fname &optional directories)
  "Search for FNAME in current or specified DIRECTORIES trees.

The function searches in the directory trees identified by
DIRECTORIES argument if specified, otherwise in the current directory.

Returns a list of string, each string is the path of a file found.

Uses shell command identified by `pel-ffind-executable'.
The \\='VCS ignore capability\\=' of fd is not used, so all files are found
whether the VCS is told to ignore them or not."
  (unless directories
    (setq directories (list default-directory)))
  (let ((found-files
         (split-string
          (string-trim
           (shell-command-to-string (pel-ffind-command fname directories)))
          "\n" t)))
    ;; When fname has a directory portion it is ignored in the search
    ;; command created by pel-ffind-command otherwise the find or fd search
    ;; fails.  The result might include files that are not inside the
    ;; specified  directory then.  Remove these files from the result.
    (when (and found-files
               (file-name-directory fname))
      (let ((dir-portion (file-name-directory fname)))
        (setq found-files
              (seq-filter (lambda (fn)
                            (string-match-p (regexp-quote dir-portion) fn))
                          found-files))))
    found-files))

;; ---------------------------------------------------------------------------

(defun pel-ffind-project-directory (&optional project-root-identifiers)
  "Find and return the project root directory of file in current buffer.

Search project root directory using the anchor files specified in the
`pel-project-root-identifiers' user-option and the ones in
PROJECT-ROOT-IDENTIFIERS list if specified.

Unless it finds a restrictive anchor identified by
`pel-project-restricted-root-identifiers', the search continues and if
multiple anchors are found at different tree levels, the
outermost (shortest path) directory is selected.  This broadens the file
search scope when nested projects exist while still allowing a
restricted project scope for some projects.

Return a directory name expanded and without trailing slash if found,
nil otherwise."
  (let ((identifiers pel-project-root-identifiers)
        (directory nil)
        (found-dir nil))
    ;; make a list of all project root identifiers
    (dolist (fname project-root-identifiers)
      (unless (member fname identifiers)
        (push fname identifiers)))
    ;; also search for restricted root identifiers so they can be found and
    ;; trigger the stop.
    (dolist (fname pel-project-restricted-root-identifiers)
      (unless (member fname identifiers)
        (push fname identifiers)))
    ;; search project root from current directory up looking for a
    ;; project root identifier file.  Stops at any restricted anchor if it
    ;; finds one. Otherwise retain the shortest directory path found
    ;; to allow nested projects and keep the more encompassing one, broadening
    ;; the file search: anyway if more than 1 file found the user will be
    ;; prompted.
    ;;
    (catch 'pel-ffind--break
      (dolist (anchor-fname identifiers)
        (setq found-dir (locate-dominating-file default-directory anchor-fname))
        (when found-dir
          ;; stop on any restricted root anchor
          (when (member anchor-fname pel-project-restricted-root-identifiers)
            (setq directory found-dir)
            (throw 'pel-ffind--break nil))
          (if directory
              (when (< (length found-dir) (length directory))
                (setq directory found-dir))
            (setq directory found-dir)))))
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

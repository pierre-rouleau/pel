;;; pel-perl.el --- PEL support for Perl.  -*- lexical-binding: t; -*-

;; Created   : Friday, December 20 2024.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2025-01-11 12:07:00 EST, updated by Pierre Rouleau>

;; This file is part of the PEL package.
;; This file is not part of GNU Emacs.

;; Copyright (C) 2024, 2025  Pierre Rouleau
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
;; PEL extra support for the Perl (perl-5) programming language.

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'pel--base)                    ; use: `pel-filesep'
(require 'pel-ffind)                    ; use: `pel-ffind'
;;; --------------------------------------------------------------------------
;;; Code:
;;

;;-pel-autoload
(defun pel-perl-critic (&optional verbose)
  "Validate the Perl file visited in current buffer with perlcritic.

With optional VERBOSE prefix argument, print extra information:
- Full name of the Policy module that created the violation
- Full diagnostic discussion of each Perl Best Practice (PBP) violation.

Show errors in compilation-mode buffer in a format that allows navigation."
  (interactive "P")
  (if (executable-find "perlcritic")
      (compile
       ;; use a format that can be used by the compile mode to move to the error.
       (format
        (if verbose
            "perlcritic --nocolor --verbose \"%%F:%%l:%%c:\\tSev:%%s, %%C:\\t%%m.\\n  %%P (%%e):\\n%%d\\n\" %s"
          "perlcritic --nocolor --verbose \"%%F:%%l:%%c:\\tSev:%%s:\\t%%m.\\t(%%e)\\n\" %s")
        (shell-quote-argument (buffer-file-name)))
       nil)
    (user-error "Please install perlcritic")))

;; ----
(defun pel-perl-source-directories (&optional directories)
  "Return a list of directories used by Perl as include path.

If DIRECTORIES argument is specified it may be a single directory
path string or a list of directory path strings.  The list of
specified directories is pre-pended to the Perl's @INC path and the list
identified by the `pel-perl-extra-project-root-directories'.

The list returned is that list as long as each identified
directory exists.  Any directory identified that does not exists
is removed from the returned list."
  (let ((perl-dirs
         (split-string
          (string-trim
           (shell-command-to-string
            "perl -e 'print join(\"\\n\", @INC), \"\n\";'"))
          "\n"))
        (extra-dirs (when directories
                      (if (stringp directories)
                          (list directories)
                        directories)))
        (existing-dirs nil))
    (dolist (dirname pel-perl-extra-project-root-directories)
      (push (expand-file-name dirname) extra-dirs))
    (dolist (dirname (append extra-dirs perl-dirs))
      (when (file-directory-p dirname)
        (push dirname existing-dirs)))
    ;; return sorted directory list with all duplicates removed
    (sort (delete-dups existing-dirs) (function string<))))

;;-pel-autoload
(defun pel-perl-show-source-directories ()
  "Display the Perl Source directories."
  (interactive)
  (message "%s" (string-join (pel-perl-source-directories) "\n")))

(defun pel-perl-filepath-for (filepath)
  "Return OS compliant file path for FILEPATH.

FILEPATH may contain Perl specific directory separator such as the
old single quote or the modern :: path separator.
Those are replaced by the OS specific path separator "
  (while (string-match "\\(::\\)\\|'" filepath)
    (setq filepath (replace-match pel-filesep nil :lit filepath)))
  filepath)

(defun pel-perl-filenames-for (filepath)
  "Return a list of possible Perl file pathnames for FILEPATH.

If FILEPATH has a file extension return it untouched inside a list.
Otherwise return a list of file names with the same path: an extension-less
file name and file names with the following extensions:
- .pm
- .pl
- .perl
- .pod
- .Pm
- .Pl
- .Perl
- .Pod
- .PL
- .al
- .ph
"
  (let ((real-filepath (pel-perl-filepath-for filepath))
        (extensions '("pm" "pl" "perl" "pod"
                      "Pm" "Pl" "Perl" "Pod"
                           "PL"
                      "al" "ph")))
    (if (file-name-extension real-filepath)
        (list real-filepath)
      (let ((filepaths nil))
        (dolist (ext extensions (reverse filepaths))
          (push (concat real-filepath "." ext) filepaths))))))

;;-pel-autoload
(defun pel-perl-find-file (filename &optional directories)
  "Find a Perl file FILENAME.

The FILENAME argument may use the Perl :: or single quote separator:
both will be interpreted as the OS directory separator.  The FILENAME
may or may not include the file extension.  The function searches for the file
name as is and with the .pm and .pl file extensions.

The file search is done in directory *trees* identified by Perl's @INC
array variable and optionally by the extra DIRECTORIES.

If DIRECTORIES argument is specified it may be a single directory
path string or a list of directory path strings.

Return a list of found file names with complete absolute path.
Return nil if nothing found.
"
  (let ((file-pathnames nil))
    (dolist (fname (pel-perl-filenames-for filename))
      (setq file-pathnames (append
                            (pel-ffind
                             fname
                             (pel-perl-source-directories directories))
                            file-pathnames)))
    file-pathnames))

;; --
;;-pel-autoload
(defun pel-perl-tidy-ediff ()
  "Run perltidy on the current buffer, start ediff session.

Run perltidy on the current buffer if no area is marked. If an area is marked
run perltidy on the marked area only."
  (interactive)
  (if (and (fboundp 'perl-tidy-ediff-region)
           (fboundp 'perl-tidy-ediff))
      (if (use-region-p)
          (perl-tidy-ediff-region)
        (perl-tidy-ediff))
    (user-error "First et perl-user-perl to HaraldJoerg/cperl-mode!")))

;;; --------------------------------------------------------------------------
(provide 'pel-perl)

;;; pel-perl.el ends here

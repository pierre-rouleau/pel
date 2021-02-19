;;; pel-net.el --- PEL network management utilities.  -*- lexical-binding: t; -*-

;; Created   : Monday, August 31 2020.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2021-02-19 15:58:44, updated by Pierre Rouleau>

;; This file is part of the PEL package.
;; This file is not part of GNU Emacs.

;; Copyright (C) 2020, 2021  Pierre Rouleau
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
;; This defines the functions that download and install Emacs Lisp files into
;; PEL's "utils" utility directory stored in the directory identified by the
;; variable `user-emacs-directory'.  PEL uses these functions to get Emacs
;; files not supported by Elpa compliant sites, but instead stored in secure
;; and well established sites such as GitHub.
;;
;; - `pel-install-file'  downloads and installs one file.
;; - `pel-install-files' downloads and installs one or several files from the
;;   same web site.
;; - `pel-install-github-files' downloads and installs one or several files
;;    from GitHub specified user project branch.
;; - `pel-install-github-file' downloads and installs one file.  That file
;;   may have a name that differs from the URL used to download it.  This is
;;   mostly used when a file name has a character that cannot be part of a URL
;;   and must be encoded differently.

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;

(require 'pel--base)                    ; use: pel-url-join

;;; --------------------------------------------------------------------------
;;; Code:
;;

;;-pel-autoload
(defun pel-install-file (url fname &optional refresh)
  "Download and install a file FNAME from URL into the PEL's utility directory.
Also byte compile that file.
This is the 'utils' sub-directory of the directory identified by
the Emacs variable `user-emacs-directory'.
If this directory does not exist, the function creates it.

If the file already exists in the destination, no download
is done unless REFRESH is non-nil, in which case the function
prompts for confirmation.

The function returns t if the file was
downloaded, nil otherwise.  Permission errors are raised."
  (let ((utils-dirname (expand-file-name "utils" user-emacs-directory)))
    (unless (file-exists-p utils-dirname)
      (make-directory utils-dirname :make-parents-if-needed))
    (let ((target-fname (expand-file-name fname utils-dirname)))
      (when (or (not (file-exists-p target-fname)) refresh)
        (message "Downloading %s\n" url)
        (when (url-copy-file url target-fname refresh)
          (message "Byte compiling it to %s\n" target-fname)
          (byte-compile-file target-fname))))))

;;-pel-autoload
(defun pel-install-files (url-base fnames &optional refresh)
  "Download & install files identified by their URL-BASE and FNAMES.

The URL-BASE is the common URL for the location of all files.

The FNAMES is a file name string or list of file name strings
identifying the name of the file located at that URL-BASE and
also the name of the file save locally into the PEL Emacs 'utils'
directory.  See `pel-install-file' for more info.

If a file already exists in the destination, no download
is done unless REFRESH is non-nil, in which case the function
prompts for confirmation.

The function returns t if the file was
downloaded, nil otherwise.  Permission errors are raised."
  (let ((fnames (if (listp fnames)
                    fnames
                  (list fnames))))
    (dolist (fname fnames)
      (pel-install-file (pel-url-join url-base fname)
                        fname
                        refresh))))

;;-pel-autoload
(defun pel-install-github-files (user-project-branch fnames &optional refresh)
  "Download & install FNAMES from GitHub USER-PROJECT-BRANCH.

- USER-PROJECT-BRANCH is a GitHub user/project/branch name path
  string.  Something like \"pierre-rouleau/pel/master\".
  If a depot file is stored in a depot sub-directory, include the
  path of depot directory inside USER-PROJECT-BRANCH.
- FNAMES is a file name string or list of file names.

If a file already exists in the destination, no download
is done unless REFRESH is non-nil, in which case the function
prompts for confirmation.

The function returns t if the file was
downloaded, nil otherwise.  Permission errors are raised."
  (let ((github-rawfile-url (pel-url-join "https://raw.githubusercontent.com"
                                          user-project-branch)))
    (pel-install-files github-rawfile-url fnames refresh)))

(defun pel-install-github-file (user-project-branch
                                fname &optional url-fname refresh)
  "Download & install FNAME from GitHub USER-PROJECT-BRANCH/URL-FNAME.

- USER-PROJECT-BRANCH is a GitHub user/project/branch name path
  string.  Something like \"pierre-rouleau/pel/master\".
  If a depot file is stored in a depot sub-directory, include the
  path of depot directory inside USER-PROJECT-BRANCH.
- FNAME is the name of the file, with its .el extension.
- URL-FNAME is the name of the file as it appears in the
  URL. This argument is only required when it differs from FNAME.

If a file already exists in the destination, no download
is done unless REFRESH is non-nil, in which case the function
prompts for confirmation.

The function returns t if the file was
downloaded, nil otherwise.  Permission errors are raised."
  (pel-install-file
   (pel-url-join "https://raw.githubusercontent.com"
                 user-project-branch
                 (or url-fname fname))
   fname
   refresh))

;;; --------------------------------------------------------------------------
(provide 'pel-net)

;;; pel-net.el ends here

; LocalWords:  LocalWords github githubusercontent FNAME pierre rouleau utils
; LocalWords:  fname

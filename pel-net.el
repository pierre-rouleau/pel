;;; pel-net.el --- PEL network management utilities.  -*- lexical-binding: t; -*-

;; Created   : Monday, August 31 2020.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2020-08-31 11:22:04, updated by Pierre Rouleau>

;; This file is part of the PEL package.
;; This file is not part of GNU Emacs.

;; Copyright (C) 2020  Pierre Rouleau
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

;;; ----------------------------------------------------------------------------
;;; Commentary:
;;
;; This file defines the function `pel-install-file' that is used to install an
;; Emacs Lisp file taken from a specified URL into the PEL's utils
;; directory. This is mainly used to automatically install a small Emacs Lisp
;; file that is stored in web site that does not support Elpa-compliant
;; protocol.

;;; ----------------------------------------------------------------------------
;;; Dependencies:
;;
;;

;;; ----------------------------------------------------------------------------
;;; Code:
;;

;;-pel-autoload
(defun pel-install-file (url fname &optional refresh)
  "Download and install a file into the PEL's utility directory.
This is the 'utils' sub-directory of the directory identified by
the Emacs variable `user-emacs-directory'.
If this directory does not exitts, the function creates it.
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
        (url-copy-file url target-fname refresh)))))

;;; ----------------------------------------------------------------------------
(provide 'pel-net)

;;; pel-net.el ends here

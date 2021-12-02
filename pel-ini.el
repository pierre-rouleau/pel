;;; pel-ini.el --- PEL support for .INI files.  -*- lexical-binding: t; -*-

;; Created   : Tuesday, November 30 2021.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2021-12-02 13:45:19, updated by Pierre Rouleau>

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
;; This is a thin layer over the `ini.el' package file written by Daniel Ness.
;;  See https://github.com/daniel-ness/ini.el

;; CAUTION: this does not yet support values that are spread across several
;; lines.

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'pel--base)                    ; use: `pel-require'

;;; --------------------------------------------------------------------------
;;; Code:
;;

(defun pel-ini-load (filename)
  "Load a .INI file FILENAME and return corresponding alist."
  (if (and (pel-require 'ini :install-when-missing
                        "pierre-rouleau/ini.el/master" "ini.el")
           (fboundp 'ini-decode))
      (ini-decode filename)
    (error "The ini package is not installed: ini-decode is void.\
  Set pel-use-ini to t.")))

(defun pel-ini-store (alist filename &optional header overwrite)
  "Store the ALIST object into the FILENAME as .INI file format."
  (if (and (pel-require 'ini :install-when-missing
                        "pierre-rouleau/ini.el/master" "ini.el")
           (fboundp 'ini-store))
      (ini-store alist filename header overwrite)
    (error "The ini package is not installed: ini-store is void.\
  Set pel-use-ini to t.")))

;;; --------------------------------------------------------------------------
(provide 'pel-ini)

;;; pel-ini.el ends here

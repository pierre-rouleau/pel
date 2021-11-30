;;; pel-ini.el --- PEL support for .INI files.  -*- lexical-binding: t; -*-

;; Created   : Tuesday, November 30 2021.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2021-11-30 14:14:20, updated by Pierre Rouleau>

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
;; allow compilation with emacs -Q
(require 'ini nil :noerror)
(declare-function ini-decode "ini")
(declare-function ini-encode "ini")

;;; --------------------------------------------------------------------------
;;; Code:
;;

(defun pel-ini-load (filename)
  "Load a .INI file FILENAME and return corresponding alist."
  (ini-decode
   (with-temp-buffer
     (insert-file-contents filename)
     (buffer-string))))

(defun pel-ini-store (alist filename)
  "Store the ALIST object into the FILENAME as .INI file format."
  (with-temp-buffer
    (insert (ini-encode alist))
    (append-to-file (point-min) (point-max) filename)))

;;; --------------------------------------------------------------------------
(provide 'pel-ini)

;;; pel-ini.el ends here

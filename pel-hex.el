;;; pel-hex.el --- Hexadecimal utilities.  -*- lexical-binding: t; -*-

;; Created   : Tuesday, May 17 2022.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2022-05-17 15:47:08 EDT, updated by Pierre Rouleau>

;; This file is part of the PEL package.
;; This file is not part of GNU Emacs.

;; Copyright (C) 2022  Pierre Rouleau
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
;; For now this only holds the `pel-bibyte' function that converts a number
;; of any radix base to a value in gibibyte, mebibyte, kibibyte and byte.
;;  A kibibyte is 1024, mebibyte is 1024 kibibyte, gebibyte is 1024 mebibyte.

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;

;;; --------------------------------------------------------------------------
;;; Code:
;;

(defun pel-bibyte (number)
  "Convert NUMBER to a (gibibyte, mebibyte, kibibyte, byte) counts."
  (let* ((geb    (/ number 1073741824))
         (number (- number (* geb 1073741824)))
         (meb    (/ number 1048576))
         (number (- number (* meb 1048576)))
         (keb    (/ number 1024))
         (number (- number (* keb 1024))))
    (list geb meb keb number)))

;;; --------------------------------------------------------------------------
(provide 'pel-hex)

;;; pel-hex.el ends here

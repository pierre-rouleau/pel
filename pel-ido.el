;;; pel-ido.el --- Ido control.  -*- lexical-binding: t; -*-

;; Created   : Tuesday, February  9 2021.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2021-02-09 14:46:13, updated by Pierre Rouleau>

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
;; This contains a set of simple functions used to control IDO features
;; interactively.


;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
(require 'pel-prompt)

;;; --------------------------------------------------------------------------
;;; Code:
;;

;;-pel-autoload
(defun pel-set-ido-use-fname-at-point (&optional globally)
  "Set Ido's ability to use the filename at point as a starting point."
  (interactive "P")
  (pel-set-user-option "Find file at point"
                       'ido-use-filename-at-point
                       '((?d "Disabled"               nil)
                         (?g "Guess file name"        guess)
                         (?l "Use literal file name"  t))
                       (not globally)))

;;; --------------------------------------------------------------------------
(provide 'pel-ido)

;;; pel-ido.el ends here

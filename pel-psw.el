;;; pel-psw.el --- Popup switcher utilities.  -*- lexical-binding: t; -*-

;; Created   : Monday, October 25 2021.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2021-10-25 14:23:24, updated by Pierre Rouleau>

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
;;  This file hold convenience functions that facilitate the use of
;;  popup-switcher commands.

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;


;;; --------------------------------------------------------------------------
;;; Code:
;;

(defun pel-psw-navigate-files ()
  "Run `psw-navigage-files' in current directory."
  (interactive)
  (if (and  (require 'popup-switcher nil :noerror)
            (fboundp 'psw-navigate-files))
      (psw-navigate-files ".")
    (user-error "popup-switcher not available, set pel-use-popup-switcher on")))

;;; --------------------------------------------------------------------------
(provide 'pel-psw)

;;; pel-psw.el ends here

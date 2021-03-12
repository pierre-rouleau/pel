;;; pel-diff.el --- File diff utilities.  -*- lexical-binding: t; -*-

;; Created   : Friday, March 12 2021.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2021-03-12 10:53:31, updated by Pierre Rouleau>

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
;; This file provides utilities that speed up the use of Emacs EDiff library.

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'pel--base)                    ; use: pel-current-buffer-filename
;;; --------------------------------------------------------------------------
;;; Code:
;;

;;-pel-autoload
(defun pel-ediff-2files ()
  "Run ediff-files on the files of current and other window."
  (interactive)
  (let ((fname-a (pel-current-buffer-filename))
        (fname-b (save-excursion
                   (other-window 1)
                   (pel-current-buffer-filename))))
    (ediff-files fname-a fname-b )))

;;; --------------------------------------------------------------------------
(provide 'pel-diff)

;;; pel-diff.el ends here

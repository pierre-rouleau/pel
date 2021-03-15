;;; pel-diff.el --- File diff utilities.  -*- lexical-binding: t; -*-

;; Created   : Friday, March 12 2021.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2021-03-15 15:37:17, updated by Pierre Rouleau>

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
(require 'pel-window)                   ; use: pel-window-direction-for
;;                                      ;      pel-window-select
;;; --------------------------------------------------------------------------
;;; Code:
;;

;;-pel-autoload
(defun pel-ediff-2files (&optional n)
  "Run ediff-files on the files of current and other window.

If argument N is specified and in the [2,8] range it identifies
the window in the direction corresponding to the cursor numeric keypad:
  -             8 := 'up
  - 4 := 'left  5 := 'current  6 := 'right
  -             2 := 'down "
  ;; Note: ediff-files is available in Emacs -Q
  (interactive "P")
  (let ((fname-a (pel-current-buffer-filename))
        (fname-b (save-excursion
                   (pel-window-select
                    (pel-window-direction-for
                     (abs (prefix-numeric-value n))
                     'other))
                   (pel-current-buffer-filename))))
    (ediff-files fname-a fname-b )))

;;-pel-autoload
(defun pel-ediff-revision ()
  "Run ediff-revision for current file against its last commit version."
  ;; Note: ediff-revision is available in Emacs -Q
  (interactive)
  (ediff-revision (pel-current-buffer-filename)))

;;; --------------------------------------------------------------------------
(provide 'pel-diff)

;;; pel-diff.el ends here

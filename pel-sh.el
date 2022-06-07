;;; pel-sh.el --- PEL shell script support.  -*- lexical-binding: t; -*-

;; Created   : Tuesday, June  7 2022.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2022-06-07 18:40:21 EDT, updated by Pierre Rouleau>

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
;; Early state module with naive implementation of quoting functions.

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;

;;; --------------------------------------------------------------------------
;;; Code:
;;

(defun pel--quote-word (quote-char)
  (if (use-region-p)
      (let ((begin-pos (region-beginning))
            (end-pos (region-end)))
        (goto-char begin-pos)
        (insert quote-char)
        (goto-char end-pos)
        (insert quote-char))
    (insert quote-char)
    (forward-word)
    (insert quote-char)))

;;-pel-autoload
(defun pel-sh-double-quote-word ()
  "Surround word at point or selected area with double quotes."
  (interactive "*")
  (pel--quote-word ?\"))

;;-pel-autoload
(defun pel-sh-single-quote-word ()
  "Surround word at point or selected area with single quotes."
  (interactive "*")
  (pel--quote-word ?\'))

;;-pel-autoload
(defun pel-sh-backtick-quote-word ()
  "Surround word at point or selected area with backtick characters."
  (interactive "*")
  (pel--quote-word ?`))

;;; --------------------------------------------------------------------------
(provide 'pel-sh)

;;; pel-sh.el ends here

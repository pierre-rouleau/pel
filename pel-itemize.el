;;; pel-itemize.el --- Itemize lines.  -*- lexical-binding: t; -*-

;; Created   : Sunday, March 13 2022.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2022-03-13 14:24:36, updated by Pierre Rouleau>

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
;; This provides utility commands to itemize a set of lines; add text to the
;; lines to indicate that each of the items on those lines are items in a list
;; of items.
;;
;; This is is an early state of the mechanism.  For now it only itemize by
;; pre-pending the "- " string.  In the future the code could evolve into
;; providing the ability to insert various strings and perhaps also using a
;; specified function to support itemization of different things, like the
;; creation of C enums or various other concepts.
;;
;; The idea is to provide an easy to use command bound to a key sequence that
;; is used after typing a set of lines using common indentation.  That would
;; speed up typing by processing the lines that have just been typed.

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;

;;; --------------------------------------------------------------------------
;;; Code:
;;

(defun pel-itemize-lines (&optional item-prefix-string)
  "Prepend each of the previous lines with a ITEM-PREFIX-STRING.

If ITEM-PREFIX-STRING is not specified use \"- \".  Only itemize
lines that have the same indentation level as the current point."
  (interactive)
  (save-excursion
    (setq item-prefix-string (or item-prefix-string "- "))
    (let ((target-column (current-column)))
      (while
          (progn
            (forward-line -1)
            (when (and (not (bobp))
                       (not (eolp))
                       (eq (current-indentation) target-column))
              (move-to-column target-column)
              (insert item-prefix-string)
              (left-char 2)
              t))))))

;;; --------------------------------------------------------------------------
(provide 'pel-itemize)

;;; pel-itemize.el ends here

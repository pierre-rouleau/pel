;;; pel-cpp.el --- PEL support for C++ mode.  -*- lexical-binding: t; -*-

;; Created   : Tuesday, November  7 2023.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2023-11-07 13:23:34 EST, updated by Pierre Rouleau>

;; This file is part of the PEL package.
;; This file is not part of GNU Emacs.

;; Copyright (C) 2023  Pierre Rouleau
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
;; A small set of utilities for C++ code.

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'pel--base)                    ; use: `pel-point-in-comment-or-docstring'
;;; --------------------------------------------------------------------------
;;; Code:
;;

(defconst pel--c++-class-visibility-attribute-regexp
  "\\(\\(public\\)\\|\\(protected\\)\\|\\(private\\)\\)[[:blank:]]*:"
  "Regexp to search for C++ class visibility attributes.")

(defun pel--search-class-visibility (direction)
  "Move point to C++ class visibility in specified DIRECTION.

DIRECTION can be up or down."
  (let ((found-pos nil)
        (original-pos (point)))
    (save-excursion
      (while
          (if
              (if (eq direction 'up)
                  (re-search-backward
                   pel--c++-class-visibility-attribute-regexp nil :noerror)
                (re-search-forward
                 pel--c++-class-visibility-attribute-regexp nil :noerror))
              (if (pel-point-in-comment-or-docstring)
                  t
                (setq found-pos (point))
                nil))))
    (if found-pos
        (progn
          (push-mark original-pos)
          (goto-char found-pos))
      (user-error "No C++ class visibility statement found!")))  )

(defun pel-move-up-to-class-visibility ()
  "Move point to previous class visibility keyword.

Push original position if one is found."
  (interactive)
  (pel--search-class-visibility 'up))

(defun pel-move-down-to-class-visibility ()
  "Move point to previous class visibility keyword."
  (interactive)
  (pel--search-class-visibility 'down))

;;; --------------------------------------------------------------------------
(provide 'pel-cpp)

;;; pel-cpp.el ends here

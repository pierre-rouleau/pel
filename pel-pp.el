;;; pel-pp.el --- C preprocessor utilities.  -*- lexical-binding: t; -*-

;; Created   : Tuesday, November  3 2020.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2020-11-05 16:32:39, updated by Pierre Rouleau>

;; This file is part of the PEL package.
;; This file is not part of GNU Emacs.

;; Copyright (C) 2020  Pierre Rouleau
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
;;  This file defines a set of utilities to deal with C preprocessor
;;  directives.

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'pel--base)                    ; use: pel-symbol-on-off-string
;;                                      ;    : pel-symbol-value-or
;;; --------------------------------------------------------------------------
;;; Code:
;;

(defun pel-pp-previous-directive-pos ()
  "Return position of the previous preprocessor directive."
  (save-excursion
    (re-search-backward "^ *#")))

(defun pel-pp-next-directive-pos ()
  "Return position of the next preprocessor directive."
  (save-excursion
    (re-search-forward "^ *#")))

;;-pel-autoload
(defun pel-pp-next-directive ()
  "Move point to next preprocessor directive."
  (interactive)
  (condition-case err
      (goto-char (pel-pp-next-directive-pos))
  (search-failed
   (user-error "No preprocessor directive found after point (%s)" err))))

;;-pel-autoload
(defun pel-pp-prev-directive ()
  "Move point to previous preprocessor directive."
  (interactive)
  (condition-case err
      (goto-char (pel-pp-previous-directive-pos))
    (search-failed
     (user-error "No preprocessor directive found before point (%s)" err))))


;;-pel-autoload
(defun pel-pp-show-state ()
  "Show state of C preprocessor control modes."
  (interactive)
  (message "\
hide-ifdef mode        : %s
ifdef shadowing        : %s
ifdef read-only        : %s
hide-ifdef-env         : %S
hide-ifdef-define-alist: %S"
           (pel-symbol-on-off-string 'hide-ifdef-mode)
           (pel-symbol-on-off-string 'hide-ifdef-shadow)
           (pel-symbol-on-off-string 'hide-ifdef-read-only)
           (pel-symbol-value-or 'hide-ifdef-env)
           (pel-symbol-value-or 'hide-ifdef-define-alist)))


;;; --------------------------------------------------------------------------
(provide 'pel-pp)

;;; pel-pp.el ends here

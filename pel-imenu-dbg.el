;;; pel-imenu-dbg.el --- Tools to debug iMenu support.  -*- lexical-binding: t; -*-

;; Created   : Friday, February 19 2021.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2021-02-19 09:50:35, updated by Pierre Rouleau>

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
;;

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'pel--base)                    ; use: pel-print-in-buffer,
;;                                      ;      pel-insert-symbol-content

;;; --------------------------------------------------------------------------
;;; Code:
;;

(defun pel-imenu-dbg-print-vars ()
  "Print imenu controlling variables for current major mode in *imenu-dbg*."
  (interactive)
  (let ((buffer (current-buffer)))
    (pel-print-in-buffer
     "*imenu-dbg*"
     "imenu control variables"
     (lambda ()
       "Print imenu variables."
       (pel-insert-list-content 'imenu-generic-expression buffer)
       (pel-insert-symbol-content 'imenu-case-fold-search buffer)
       (pel-insert-list-content 'imenu-syntax-alist buffer)))))

;;; --------------------------------------------------------------------------
(provide 'pel-imenu-dbg)

;;; pel-imenu-dbg.el ends here

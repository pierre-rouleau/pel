;;; pel-outline.el --- PEL outline utilities.  -*- lexical-binding: t; -*-

;; Created   : Saturday, October  9 2021.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2021-10-09 11:53:13, updated by Pierre Rouleau>

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

;;-pel-autoload
(defun pel-outline-print-vars ()
  "Print information about variables controlling outline modes.
Information is printed inside the *outline-dbg* buffer."
  (interactive)
  (let ((buffer (current-buffer)))
    (pel-print-in-buffer
     "outline-dbg*"
     "outline modes variables"
     (lambda ()
       "Print outline modes variables"
       (pel-insert-symbol-content 'outline-regexp   buffer :on-same-line)
       (pel-insert-symbol-content 'outline-level    buffer :on-same-line)
       (pel-insert-symbol-content 'outline-blank-line    buffer :on-same-line)
       (pel-insert-symbol-content 'outline-heading-end-regexp buffer
                                  :on-same-line)
       (pel-insert-symbol-content 'outline-isearch-open-invisible-function  buffer
                                  :on-same-line)

       (pel-insert-list-content   'outline-heading-alist       buffer)
       (pel-insert-list-content   'outline-view-change-hook    buffer)
       (pel-insert-list-content   'outline-mode-hook           buffer)
       (pel-insert-list-content   'outline-view-change-hook    buffer)))))

;;; --------------------------------------------------------------------------
(provide 'pel-outline)

;;; pel-outline.el ends here

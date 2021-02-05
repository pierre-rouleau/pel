;;; pel-goto-addr.el --- PEL extensions to goto-addr.  -*- lexical-binding: t; -*-

;; Created   : Friday, February  5 2021.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2021-02-05 14:29:24, updated by Pierre Rouleau>

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

;;; --------------------------------------------------------------------------
;;; Code:
;;

;;-pel-autoload
(defun pel-goto-next-url ()
  "Move point to the end of next URL in buffer."
  (interactive)
  (if (and (require 'goto-addr nil :no-error)
           (boundp  'goto-address-url-regexp))
      (progn
        (re-search-forward goto-address-url-regexp)
        (left-char))
    (error "Can't load goto-addr!")))

;;-pel-autoload
(defun pel-goto-previous-url ()
  "Move point to the beginning of previous URL in buffer."
  (interactive)
    (if (and (require 'goto-addr nil :no-error)
           (boundp  'goto-address-url-regexp))
        (re-search-backward goto-address-url-regexp))
    (error "Can't load goto-addr!"))


;;; --------------------------------------------------------------------------
(provide 'pel-goto-addr)

;;; pel-goto-addr.el ends here

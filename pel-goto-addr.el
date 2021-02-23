;;; pel-goto-addr.el --- PEL extensions to goto-addr.  -*- lexical-binding: t; -*-

;; Created   : Friday, February  5 2021.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2021-02-23 12:36:46, updated by Pierre Rouleau>

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
;; Extension commands for the goto-address-mode.  The two provided commands
;; should be added to the goto-address-mode key-map to ease navigation when
;; this mode is active.

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
           (boundp  'goto-address-url-regexp)
           (boundp  'goto-address-mode)
           (fboundp 'goto-address-mode))
      (progn
        (unless goto-address-mode
          (goto-address-mode 1))
        (re-search-forward goto-address-url-regexp)
        (left-char)) ; move point back over the URL
    (error "Failed loading goto-addr!")))

;;-pel-autoload
(defun pel-goto-previous-url ()
  "Move point to the beginning of previous URL in buffer."
  (interactive)
  (if (and (require 'goto-addr nil :no-error)
           (boundp  'goto-address-url-regexp)
           (boundp  'goto-address-mode)
           (fboundp 'goto-address-mode))
      (progn
        (unless goto-address-mode
          (goto-address-mode 1))
        (re-search-backward goto-address-url-regexp))
    (error "Failed loading goto-addr!")))


;;; --------------------------------------------------------------------------
(provide 'pel-goto-addr)

;;; pel-goto-addr.el ends here

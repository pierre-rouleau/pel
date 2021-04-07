;;; pel-imenu.el --- PEL imenu extensions. -*-lexical-binding: t-*-

;; Copyright (C) 2020, 2021  Pierre Rouleau

;; Author: Pierre Rouleau <prouleau001@gmail.com>

;; This file is part of the PEL package
;; This file is not part of GNU Emacs.

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
;; iMenu control functionality.

;;; --------------------------------------------------------------------------
;;; Dependencies:

;;; --------------------------------------------------------------------------
;;; Code:


;;-pel-autoload
(defun pel-imenu-rescan ()
  "Force immediate imenu rescan."
  (interactive)
  (if (and (require 'imenu nil :no-error)
           (fboundp 'imenu--menubar-select)
           (boundp  'imenu--rescan-item))
      (imenu--menubar-select imenu--rescan-item)
    (user-error "Cannot rescan imenu: \
imenu--menubar-select or imenu--rescan-item missing")))

;; -----------------------------------------------------------------------------
(provide 'pel-imenu)

;;; pel-imenu.el ends here

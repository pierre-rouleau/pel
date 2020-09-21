;;; pel-help.el --- PEL extra help utilities  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Pierre Rouleau

;; Author: Pierre Rouleau (concat "prouleau" "001" "@" "gmail" ".com")

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

;; -----------------------------------------------------------------------------
;;; Commentary:
;;
;; Contains a small set of help providing utility commands.
;;

;; -----------------------------------------------------------------------------
;;; Dependency
(require 'pel--options)       ; use: pel-pdf-fire-url

;;; Code:

;;-pel-autoload
(defun pel-show-kill-ring ()
  "Display content of `kill-ring' in *Help* buffer.
Simple shortcut to invoke `describe-variable' on the `kill-ring' variable."
  (interactive)
  (describe-variable 'kill-ring))

;;-pel-autoload
(defun pel-show-major-mode ()
  (interactive)
  "Display the symbol of the current major mode."
  (message "Major mode: %S" major-mode))

;; -----------------------------------------------------------------------------
(provide 'pel-help)

;;; pel-help.el ends here

;;; pel-custom.el --- PEL custization utilities.  -*- lexical-binding: t; -*-

;; Created   : Wednesday, October 21 2020.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2021-02-20 11:41:05, updated by Pierre Rouleau>

;; This file is part of the PEL package.
;; This file is not part of GNU Emacs.

;; Copyright (C) 2020, 2021  Pierre Rouleau
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
;; This file adds a set of commands that helps customization of Emacs and PEL.

;; Emacs customization browser is very useful and easy to use.  It provides a
;; quick overview of the data organization and it ought to become better
;; known.  The `pel-browse-pel' and `pel-browse-group' commands provide quick
;; access to the PEL customization group and any group respectively.  This
;; last command complements the Emacs `customize-browse' command that
;; unfortunately does not accept any argument.

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'pel--base)

;;; --------------------------------------------------------------------------
;;; Code:
;;

;;-pel-autoload
(defun pel-browse-pel ()
  "Browse the PEL customization group."
  (interactive)
  (pel-require 'cus-edit)
  (customize-browse 'pel))


(defun pel--read-group ()
  "Read and return a group name.
Lazily load file `cus-edit' if needed."
  (pel-require 'cus-edit)
  (if (fboundp 'customize-read-group)
      (customize-read-group)
    (user-error "Cannot load cus-edit!")))

;;-pel-autoload
(defun pel-browse-group (group)
  "Browse the customization tree from a specific GROUP node."
  (interactive (list (pel--read-group)))
  (pel-require 'cus-edit)
  (when (stringp group)
    (if (string-equal "" group)
        (setq group 'emacs)
      (setq group (intern group)))
    (customize-browse group)))

;;-pel-autoload
(defun pel-customize-pel-base-emacs-group (&optional other-window)
  "Open `pel-base-emacs' customization group."
  (interactive "P")
  (customize-group 'pel-base-emacs other-window))

;;; --------------------------------------------------------------------------
(provide 'pel-custom)

;;; pel-custom.el ends here

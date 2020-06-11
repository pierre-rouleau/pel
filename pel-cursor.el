;;; pel-cursor.el --- PEL cursor control  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Pierre Rouleau

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

;; -----------------------------------------------------------------------------

;;; Commentary:
;;
;; This contains a collection of commands to control the cursor attributes:
;; its color and its type (shape).
;;
;; The persistent values are stored inside the following user options, part of
;; the `pel-pkg-for-cursor' group:
;;
;; - `pel-cursor-overwrite-mode-color' : cursor color when in overwrite-mode
;; - `pel-cursor-type-when-mark'       : cursor type when marking.
;;
;; The following is a list of provided commands (*), functions (-) and
;; (old style, self-activated) advice (@):
;;
;; * `pel-customize-cursor'
;; @ `pel--overwrite-mode-change-cursor'
;; * `pel-set-cursor-color'
;; - `pel-set-cursor-type-when-mark'
;; - `pel-set-cursor-type-non-marked'
;;
;; The `pel-set-cursor-color' is the only command.  It can be used interactively
;; to temporary change the color of the cursor.  Otherwise the functionality of
;; this file takes place when the file is loaded by pel-init: it activates the
;; cursor color when the overwrite-mode is toggled and changes the cursor type
;; when the mark is activated or de-activated, all according to the values of
;; the user options.
;;
;; Note that cursor attributes control only work when Emacs run in graphics/X
;; mode, not when it runs in terminal mode.
;; Don't load this file in terminal mode, its functions will not work anyway.

;;; Code:


(require 'pel--options) ; use: pel-cursor-overwrite-mode-color
;;                      ;      pel-cursor-type-when-mark


;; -----------------------------------------------------------------------------
;; Quick access to customization
;; -----------------------------

(defun pel-customize-cursor (&optional other-window)
  "Open the customize buffer to change the `cursor' color.

If OTHER-WINDOW is non-nil, display in another window."
  (interactive "P")
  (customize-face 'cursor other-window))

;; -----------------------------------------------------------------------------
;; Control cursor color for overwrite-mode
;; ---------------------------------------

(defconst pel--default-cursor-color (face-attribute 'cursor :background)
  "Default cursor color.")

(defun pel-set-cursor-color (colorname)
  "Set cursor to specified COLORNAME string.
Ignore the request when color is not a string.
Return the COLOR string on success, nil otherwise.
When used as an interactive command the new cursor color sticks
only until the overwrite-mode is toggled.
To make the color change persist, modify the `cursor' or the
`pel-cursor-overwrite-mode-color' user options."
    (interactive (list (read-color (format "Color name [%s]: "
                                         (face-attribute 'cursor :background)))))
  (when (stringp colorname)
    (set-cursor-color colorname)))

(defadvice overwrite-mode (after pel--overwrite-mode-change-cursor activate)
  "Change cursor color in override-mode to `pel-cursor-overwrite-mode-color'."
  (pel-set-cursor-color
   (if overwrite-mode
       (face-attribute 'pel-cursor-overwrite-mode-color :background)
     pel--default-cursor-color)))

;; -----------------------------------------------------------------------------
;; Control cursor type (shape) for mark active or not
;; --------------------------------------------------

(defconst pel--default-cursor-type cursor-type
  "Default cursor type.")

(defun pel--set-cursor-type-when-mark ()
  (when pel-cursor-type-when-mark
    (setq cursor-type pel-cursor-type-when-mark)))

(defun pel--set-cursor-type-non-marked ()
  (setq cursor-type pel--default-cursor-type))

(add-hook 'activate-mark-hook   'pel--set-cursor-type-when-mark)
(add-hook 'deactivate-mark-hook 'pel--set-cursor-type-non-marked)

;; -----------------------------------------------------------------------------
(provide 'pel-cursor)

;;; pel-cursor.el ends here

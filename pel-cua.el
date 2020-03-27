;;; pel-cua.el --- PEL CUA Mode Additions -*-lexical-binding: t-*-

;; Copyright (C) 2020  Pierre Rouleau

;; Author: Pierre Rouleau <prouleau.swd@gmail.com>

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
;; CAUTION: this file is under development.  Some of it does not work.  The goal
;; is to switch to cua rectangle mode back and forth using a single key binding
;; and without having to explicitly activate the cua-mode aeach time and
;; de-activate it afterward.  The current code does not do that yet.

(require 'cua-rect)

;;; Code:

;; TODO: The following function  does not work.
;;       As soon as the outer function exits, the cua rectangle mode stops.
;;-pel-autoload
(defun pel-cua-rectangle-mark ()
  "Activate  function `cua-rectangle-mark-mode' and function `cua-mode'.
Activate ability to manage cua rectangle area.
Activate ability to use the CUA compliant commands
for copy/cut/paste that can operate on rectangle of text:
- 'Control C': copy,
- 'Control U': cut,
- 'Control V': paste."
  (interactive)
  (cua-mode 1)
  (cua-rectangle-mark-mode t))

;;-pel-autoload
(defun pel-cua-move-rectangle-left ()
  "Move entire cua rectangle overlay (not the content) left.
This is only active once the cua rectangle mode was activated
with `\\[pel-cua-rectangle-mark-mode]'."
  (interactive)
  (if (and
       (boundp 'cua--rectangle)
       cua--rectangle)
      (cua-move-rectangle-left)
    (user-error "Activate cua-rectangle-mark-mode first!")))

;;-pel-autoload
(defun pel-cua-move-rectangle-right ()
  "Move entire cua rectangle overlay (not the content) right.
This is only active once the cua rectangle mode was activated
with `\\[pel-cua-rectangle-mark-mode]'."
  (interactive)
  (if (and
       (boundp 'cua--rectangle)
       cua--rectangle)
      (cua-move-rectangle-right)
    (user-error "Activate cua-rectangle-mark-mode first!")))

;; -----------------------------------------------------------------------------
(provide 'pel-cua)

;;; pel-cua.el ends here

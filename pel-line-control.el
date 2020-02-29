;;; pel-line-control.el --- PEL Line Control Utilities

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
;;
;; Up/Down logical line (visual line mode)

;;; Code:

;; simple is part of Emacs and is loaded even with emacs -Q: no need to try to
;; load faster, just leave for compiler.
(require 'simple)                       ; use: visual-line-mode, next-logical-line,
                                        ;      previous-logical-line

;;-pel-autoload
(defun pel-lc-previous-logical-line ()
  "Move to previous logical or screen line, according to visual line mode.
In visual line mode: move to previous logical line.
Otherwise to previous screen line."
  (interactive)
  (if visual-line-mode
      (previous-logical-line)
    (forward-line -1)))

;;-pel-autoload
(defun pel-lc-next-logical-line ()
  "Move to next logical or screen line, according to visual line mode.
In visual line mode: move to next logical line.
Otherwise to next screen line."
  (interactive)
  (if visual-line-mode
      (next-logical-line)
    (forward-line)))

;;-pel-autoload
(defun pel-toggle-line-col-modes ()
  "Toggle the display of (line, col) in mode line.
If any is currently active, deactivate both.
If none is active, activate both."
  (interactive)
  (let ((activate (if (or
                       column-number-mode
                       line-number-mode)
                      -1
                    1)))
    (column-number-mode activate)
    (line-number-mode activate)))

;; -----------------------------------------------------------------------------
(provide 'pel-line-control)

;;; pel-line-control.el ends here

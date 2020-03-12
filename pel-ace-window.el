;;; pel-ace-window.el --- ace-window extensions

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
;; This
;;  In pel-bindings, the functions of this file are only mapped if the
;;  pel-use-ace-window option is t.

;;
(eval-when-compile
  (require 'ace-window))   ; use: ace-window, ace-swap-window

;;; Code:

;;-pel-autoload
(defun pel-swap-window ()
  "Swap the buffers of 2 windows.
If one window: raise user error.
If two windows: swap their buffers.
If more windows: use `ace-swap-window' to swap with window identified by number."
  (interactive)
  (let ((window-count (length (window-list))))
    (cond ((< window-count 2)
           (user-error "No other window"))
          ((= window-count 2)
           (let ((this-buffer (current-buffer))
                 (this-window (selected-window)))
             (select-window (next-window nil 'no-minibuf))
             (let ((other-buffer (current-buffer))
                   (other-window (selected-window)))
               (display-buffer-same-window this-buffer nil)
               (select-window this-window)
               (display-buffer-same-window other-buffer nil)
               (message "Exchange of %S and %S" this-window other-window)
               )))
          (t (ace-swap-window)))))

;;-pel-autoload
(defun pel-ace-window (&optional arg)
  "Select a window: perform extended `other-window' and `ace-window'.
- Without ARG:
  - If frame has only 2 windows, select the other window.
  - If frame has 3 windows or more, use `ace-window' to display
    window number and prompt for the window number to select.
    If other frame(s) exist, their windows have numbers in the same
    number-space, but they are not visible; they can be used anyway to
    select a window in that frame (Emacs will switch frames).
- With ARG:
  - 0: toggle `aw-ignore-on', which when t ignores the buffers identified by
       `aw-ignored-buffers''.
  - 4 (\\[universal-argument]): swap between selected and current window
  - 16 (\\[universal-argument] \\[universal-argument]) : delete selected window
  - any other (just a negative sign, 1, 2, 3, 5...): prompt
    for window to select.  That is useful even if there is
    1 or 2 windows in current frame and you want to select a
    window in another frame."
  (interactive "P")
  (if (or (display-graphic-p)
          (> (length (window-list)) 2)
          arg)
      (ace-window (prefix-numeric-value arg))
    (other-window 1)))


;; -----------------------------------------------------------------------------
(provide 'pel-ace-window)

;;; pel-ace-window.el ends here

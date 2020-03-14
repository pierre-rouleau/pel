;;; pel-font.el --- PEL Font Management Utilities

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
;; The functions in this file change the font scaling of the windows of all buffers.
;; This can be quite useful, since font scaling is normally done only in the
;; current buffer, even if the current buffer is the minibuffer.  Resetting the
;; font of all buffers is useful when font have been changed in the past and you
;; create a new buffer which takes the default font size.


;;; Code:


;; Scaling font utilities
;; ----------------------
;;
;; Call hierarchy:
;;
;; - pel-font-increase-size-all-buffers
;; - pel-font-decrease-size-all-buffers
;; - pel-font-reset-size-all-buffers
;;   - pel-scale-all-buffers

(defun pel-font-scale-all-buffers (inc &optional all)
  "Scale text of *all* buffers by INC step.
If INC is 0: reset to default font size.
If INC is positive: increase font size.
If INC is negative: decrease font size.
Exclude Minibuffers and Echo Area buffers unless ALL arg is specified."
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (if (or (not (string-match
                    " *Echo Area\\| *Minibuf-"
                    (buffer-name)))
              all)
          (text-scale-increase inc)))))

;;-pel-autoload
(defun pel-font-increase-size-all-buffers (&optional all)
  "Increase the font size of all buffers.
Exclude Minibuffers and Echo Area buffers unless ALL argument is set
either with a \\[universal-argument] prefix or any numeric argument."
  (interactive "P")
  (pel-font-scale-all-buffers 1 all))

;;-pel-autoload
(defun pel-font-decrease-size-all-buffers (&optional all)
  "Decrease font size of all buffers.
Exclude Minibuffers and Echo Area buffers unless ALL argument is set
either with a \\[universal-argument] prefix or any numeric argument."
  (interactive "P")
  (pel-font-scale-all-buffers -1 all))

;;-pel-autoload
(defun pel-font-reset-size-all-buffers (&optional all)
  "Reset the font size of all buffers to the default size.
Exclude Minibuffers and Echo Area buffers unless ALL argument is set
either with a \\[universal-argument] prefix or any numeric argument."
  (interactive "P")
  (pel-font-scale-all-buffers 0 all))

;; -----------------------------------------------------------------------------
(provide 'pel-font)

;;; pel-font.el ends here

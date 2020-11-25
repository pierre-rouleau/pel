;;; pel-face-ut.el --- Face Management Utilities  -*- lexical-binding: t; -*-

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

;;; Commentary:
;;
;; Face management utilities.

;;; Code:
;;

;;-pel-autoload
(defun pel-show-face-at-point ()
  "Display information about face at point."
  (interactive)
  (message "\
- face           : %s
- read-face-name : %s"
           (get-char-property (point) 'face)
           (get-char-property (point) 'read-face-name)))

;; --

(defun pel-face-at-pos-is (pos face)
  "Return t if face at POS is FACE, nil otherwise.
If POS has several faces return t if FACE is one of them, nil otherwise."
  (let ((the-face (get-char-property pos 'face)))
    (if (listp the-face)
        (memq face the-face)
      (eq face the-face))))

(defun pel-pos-of-first-char-with-face-in (face start &optional end)
  "Return the position of the first char with FACE between START and END.
If none is found return nil."
  ;; brute force approach for now.
  (setq end (or end (point-max)))
  (let (found)
    (while (and (not found) (< start end))
      (if  (pel-face-at-pos-is start face)
          (setq found t)
        (setq start (1+ start))))
    (and found start)))

;; -----------------------------------------------------------------------------
(provide 'pel-face-ut)

;;; pel-face-ut.el ends here

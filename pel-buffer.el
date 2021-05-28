;;; pel-buffer.el --- PEL buffer management utilities.  -*- lexical-binding: t; -*-

;; Created   : Thursday, May 27 2021.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2021-05-27 23:40:30, updated by Pierre Rouleau>

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
;; Provide ability to navigate through buffers in sorted order as specified by
;; the built-in bs.el library.

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'bs)                           ; Emacs built-in
;;; --------------------------------------------------------------------------
;;; Code:
;;


(defun pel-bs-next ()
  "Show next buffer in current window."
  (interactive)
  (bs-show nil)
  (bs-down 1)
  (bs-select))

(defun pel-bs-previous ()
  "Show previous buffer in current window."
  (interactive)
  (bs-show nil)
  (bs-up 1)
  (bs-select))

;;; --------------------------------------------------------------------------
(provide 'pel-buffer)

;;; pel-buffer.el ends here

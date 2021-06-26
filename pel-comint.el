;;; pel-comint.el --- PEL comint extension utilities.  -*- lexical-binding: t; -*-

;; Created   : Saturday, June 26 2021.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2021-06-26 18:47:33, updated by Pierre Rouleau>

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
;;  Utilities to deal with comint buffers: mainly REPLs for various
;;  programming languages.


;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'comint)

;;; --------------------------------------------------------------------------
;;; Code:
;;
(defun pel-clear-comint-buffer (buffer-or-name)
  "Clear the content of the comint-compliant BUFFER-OR-NAME."
  (with-current-buffer buffer-or-name
    (let ((comint-buffer-maximum-size 0))
      (comint-truncate-buffer))))

;;; --------------------------------------------------------------------------
(provide 'pel-comint)

;;; pel-comint.el ends here

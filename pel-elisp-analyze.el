;;; pel-elisp-analyze.el --- Analyze Emacs Lisp code.  -*- lexical-binding: t; -*-

;; Created   : Tuesday, December 22 2020.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2025-03-02 10:14:01 EST, updated by Pierre Rouleau>

;; This file is part of the PEL package.
;; This file is not part of GNU Emacs.

;; Copyright (C) 2020, 2021, 2025  Pierre Rouleau
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
;; Convenience tools to perform Emacs Lisp regression tests.

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'pel-lisp)
(require 'ert)

;;; --------------------------------------------------------------------------
;;; Code:
;;

(defun pel-run-ert ()
  "Byte compile and run ERT test on current buffer."
  (interactive)
  (when (pel-byte-compile-file-and-load)
    (ert t)))

;;; --------------------------------------------------------------------------
(provide 'pel-elisp-analyze)

;;; pel-elisp-analyze.el ends here

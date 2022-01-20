;;; pel-seq.el --- Sequence manipulation utilities -*-lexical-binding: t; -*-

;; Copyright (C) 2020, 2021, 2022  Pierre Rouleau

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

;; This file holds (a currently very small) collection of sequence processing
;; utility functions.
;;
;; - `pel-all-fboundp' checks that all function symbols passed as argument are
;;   bounded.

;;; --------------------------------------------------------------------------
;;; Dependencies:

(require 'pel--base)                      ; use: `pel-require'
(pel-require 'seq :install-when-missing)  ; use: `seq-reduce'
(declare-function seq-reduce "seq")

;; ---------------------------------------------------------------------------
;;; Code:
;;-pel-autoload
(defun pel-all-fboundp (&rest funs)
  "Return t if all function symbols in FUNS list are bounded, nil otherwise."
  (seq-reduce (lambda (b1 b2) (and b1 b2))
              (mapcar #'fboundp funs)
              t))

;; -----------------------------------------------------------------------------
(provide 'pel-seq)

;;; pel-seq.el ends here

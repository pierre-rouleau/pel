;;; pel-scheme.el --- PEL support for Scheme and Scheme dialects.  -*- lexical-binding: t; -*-

;; Created   : Saturday, June 26 2021.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2022-03-10 14:06:43, updated by Pierre Rouleau>

;; This file is part of the PEL package.
;; This file is not part of GNU Emacs.

;; Copyright (C) 2021, 2022  Pierre Rouleau
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
;; Utilities to deal with Scheme and Scheme dialects.

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'pel--base)                    ; use: pel-window-direction-for
(require 'pel-comint)                   ; use: pel-clear-comint-buffer
(require 'pel-window)                   ; use: pel-move-to-window
(require 'cmuscheme)                    ; use: run-scheme
;;; --------------------------------------------------------------------------
;;; Code:
;;

;;-pel-autoload
(defun pel-clear-scheme-repl-buffer ()
  "Erase content of the Scheme REPL running under Emacs."
  (interactive)
  (pel-comint-clear-buffer "*scheme*"))

;;-pel-autoload
(defun pel-chez-repl (&optional n)
  "Run the Chez REPL in window specified by N.
By default use the other window. If a numeric argument is specified,
its value correspond to the direction of a numeric keypad:
.      8
.   4     6
.      2
That is:
 - 8: up
 - 4: left
 - 6: right
 - 2: down

0 and 5 identify the current window."
  (interactive "p")
  (pel-move-to-window (pel-window-direction-for n))
  (run-scheme "chez"))

;;-pel-autoload
(defun pel-chibi-repl (&optional n)
  "Run the Chibi REPL in window specified by N.
See `pel-chez-repl' for complete description."
  (interactive "p")
  (pel-move-to-window (pel-window-direction-for n))
  (run-scheme "chibi"))

;;-pel-autoload
(defun pel-chicken-repl (&optional n)
  "Run the Chicken REPL in window specified by N.
See `pel-chez-repl' for complete description."
  (interactive "p")
  (pel-move-to-window (pel-window-direction-for n))
  (run-scheme "chicken"))

;;-pel-autoload
(defun pel-gambit-repl (&optional n)
  "Run the Gambit Scheme REPL in window specified by N.
See `pel-chez-repl' for complete description."
  (interactive "p")
  (pel-move-to-window (pel-window-direction-for n))
  (run-scheme "gsi"))

;;-pel-autoload
(defun pel-gerbil-repl (&optional n)
  "Run the Gerbil REPL in window specified by N.
See `pel-chez-repl' for complete description."
  (interactive "p")
  (pel-move-to-window (pel-window-direction-for n))
  (run-scheme "gxi"))

;;-pel-autoload
(defun pel-guile-repl (&optional n)
  "Run the Guile REPL in window specified by N.
See `pel-chez-repl' for complete description."
  (interactive "p")
  (pel-move-to-window (pel-window-direction-for n))
  (run-scheme "guile"))

;;-pel-autoload
(defun pel-mit-scheme-repl (&optional n)
  "Run the MIT/GNU Scheme REPL in window specified by N.
See `pel-chez-repl' for complete description."
  (interactive "p")
  (pel-move-to-window (pel-window-direction-for n))
  (run-scheme "mit-scheme"))

;;-pel-autoload
(defun pel-racket-repl (&optional n)
  "Run the Racket REPL in window specified by N.
See `pel-chez-repl' for complete description."
  (interactive "p")
  (pel-move-to-window (pel-window-direction-for n))
  (run-scheme "racket"))

;;-pel-autoload
(defun pel-scsh-repl (&optional n)
  "Run the Scsh REPL in window specified by N.
See `pel-chez-repl' for complete description."
  (interactive "p")
  (pel-move-to-window (pel-window-direction-for n))
  (run-scheme "scsh"))

;;; --------------------------------------------------------------------------
(provide 'pel-scheme)

;;; pel-scheme.el ends here

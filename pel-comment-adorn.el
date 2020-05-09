;;; pel-comment-adorn.el --- PEL Commented Adornment  -*- lexical-binding: t; -*-

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

;; -----------------------------------------------------------------------------
;;; Commentary:
;;
;; This file holds a collection of commands that insert commented text inside
;; the current buffer.  It declares 10 commands that underline the current line
;; with a specific reST-level adornment and comment the 2 lines, providing a
;; quick way to create underlined comments in code.
;;
;; Note that these are available even when pel-use-rst is set to t; there is no
;; harm since the pel-rst does not depend on any external package; it use the
;; rst package which is incorporated inside Emacs.
;;
;; Also note that the character selected and the number of levels depends on the
;; adornment style identified by the `pel-rst-adornment-style' user option.  By
;; default PEL uses the CRiSPer style by default which has 12 levels, enough for
;; all commands.
;; The following commands and functions are provided:
;;
;;  * pel-commented-adorn-1
;;  * pel-commented-adorn-2
;;  * pel-commented-adorn-3
;;  * pel-commented-adorn-4
;;  * pel-commented-adorn-5
;;  * pel-commented-adorn-6
;;  * pel-commented-adorn-7
;;  * pel-commented-adorn-8
;;  * pel-commented-adorn-9
;;  * pel-commented-adorn-10
;;    - pel-commented-adorn


;;; Code:

(require 'pel-rst)
(require 'pel-comment)
(require 'pel-mark)

;; --

(defun pel-commented-adorn (level)
  "Adorn a line of text with specified adornment LEVEL and comment it.
Use the current buffer comment style."
  (move-beginning-of-line nil)
  (pel-rst-adorn level)
  (pel-mark-line-down 2)
  (comment-dwim nil)
  (forward-line 1))


;; Declare 10 pel-commented-adorn-X from X=1 to X=10.
;; Note: 1) Declaring commands  via macros makes it hard to debug the functions
;;          but all these do is call the pel-commented-adorn function with an
;;          argument value.  It shortens the code here.
;;       2) None of these commands are auto-loaded here.  The auto-loading is
;;          done just inside pel_keys, via the use-package macro.

(defmacro pel--mk-adorn-cmd (level)
  "Make a `pel-commented-adorn' command for a specific LEVEL."
  (let ((fct (intern (format "pel-commented-adorn-%d" level)))
        (docstring (format "Insert a commented level-%d reST line adornment at point." level)))
    `(progn
       (defun ,fct ()
         ,docstring
         (interactive "*")
         (pel-commented-adorn ,level)))))

(pel--mk-adorn-cmd 1)
(pel--mk-adorn-cmd 2)
(pel--mk-adorn-cmd 3)
(pel--mk-adorn-cmd 4)
(pel--mk-adorn-cmd 5)
(pel--mk-adorn-cmd 6)
(pel--mk-adorn-cmd 7)
(pel--mk-adorn-cmd 8)
(pel--mk-adorn-cmd 9)
(pel--mk-adorn-cmd 10)

; -----------------------------------------------------------------------------
(provide 'pel-comment-adorn)

;;; pel-comment-adorn.el ends here

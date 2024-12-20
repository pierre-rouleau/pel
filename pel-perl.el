;;; pel-perl.el --- PEL support for Perl.  -*- lexical-binding: t; -*-

;; Created   : Friday, December 20 2024.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2024-12-20 17:44:35 EST, updated by Pierre Rouleau>

;; This file is part of the PEL package.
;; This file is not part of GNU Emacs.

;; Copyright (C) 2024  Pierre Rouleau
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
;; PEL extra support for the Perl (perl-5) programming language.

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;

;;; --------------------------------------------------------------------------
;;; Code:
;;

(defun pel-perl-critic ()
  "Validate the Perl file visited in current buffer with perlcritic.

Show errors in compilation-mode buffer."
  (interactive)
  (if (executable-find "perlcritic")
      (compile
       ;; use a format that can be used by the compile mode to move to the error.
       (format "perlcritic --nocolor --verbose \"%%F:%%l:%%c:\\tSev:%%s:\\t%%m.\\t(%%e)\\n\" %s"
               (shell-quote-argument (buffer-file-name)))
       nil)
      (user-error "Please install perlcritic")))

;;; --------------------------------------------------------------------------
(provide 'pel-perl)

;;; pel-perl.el ends here

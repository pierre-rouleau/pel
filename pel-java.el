;;; pel-java.el --- PEL Java support  -*- lexical-binding: t; -*-

;; Created   : Monday, May 11 2026.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2026-05-11 13:05:49 EDT, updated by Pierre Rouleau>

;; This file is part of the PEL package.
;; This file is not part of GNU Emacs.

;; Copyright (C) 2026  Pierre Rouleau
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
;; Minimal extra support for Java.

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;

;;; --------------------------------------------------------------------------
;;; Code:
;;

;;-pel-autoload
(defun pel--java-setup-with-lsp ()
    "Setup Java with language server capability."
    (if (require 'lsp-java nil 'noerror)
        (when (fboundp 'lsp)
          (lsp))
      (display-warning
       'pel-use-java
       "lsp-java not available; skipping Java LSP activation." :error)))

;;; --------------------------------------------------------------------------
(provide 'pel-java)

;;; pel-java.el ends here

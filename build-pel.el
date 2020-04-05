;; build-pel.el --- Byte compile the PEL system -*-lexical-binding: t-*-

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
;;  This file defines `build-pel' command used to byte-compile all PEL files in
;;  required order.

;;; Code:

(defun build-pel ()
  "Byte-compile all PEL files."
  (interactive)
  ;; Add the directory where PEL files are stored to the load path
  ;; assuming that the current directory is where the PEL source code files are
  ;; stored.
  (add-to-list 'load-path (expand-file-name "."))

  ;; First byte-compile the PEL files that have inlined defined with defsubst.
  (byte-compile-file "build-pel.el")
  (byte-compile-file "pel--base.el")
  (byte-compile-file "pel--macros.el")
  (byte-compile-file "pel--options.el")
  ;;
  (byte-compile-file "pel-autocomplete.el")
  (byte-compile-file "pel-bookmark.el")
  (byte-compile-file "pel-ccp.el")
  (byte-compile-file "pel-comment.el")
  (byte-compile-file "pel-commonlisp.el")
  (byte-compile-file "pel-cua.el")
  (byte-compile-file "pel-file.el")
  (byte-compile-file "pel-fill.el")
  (byte-compile-file "pel-font.el")
  (byte-compile-file "pel-frame-control.el")
  (byte-compile-file "pel-highlight.el")
  (byte-compile-file "pel-imenu.el")
  (byte-compile-file "pel-indent.el")
  (byte-compile-file "pel-kbmacros.el")
  (byte-compile-file "pel-line-control.el")
  (byte-compile-file "pel-lisp.el")
  (byte-compile-file "pel-mark.el")
  (byte-compile-file "pel-navigate.el")
  (byte-compile-file "pel-numkpad.el")
  (byte-compile-file "pel-prompt.el")
  (byte-compile-file "pel-register.el")
  (byte-compile-file "pel-rst.el")
  (byte-compile-file "pel-scroll.el")
  (byte-compile-file "pel-search.el")
  (byte-compile-file "pel-speedbar.el")
  (byte-compile-file "pel-spell.el")
  (byte-compile-file "pel-text-insert.el")
  (byte-compile-file "pel-text-transform.el")
  (byte-compile-file "pel-window.el")
  ;;
  (byte-compile-file "pel-autoload.el")

  ;; do no byte-compile pel_keys.el as it would
  ;; force installation of several missing packages
  ;; and would have several warnings.
  ;; (byte-compile-file "pel_keys.el")
  (byte-compile-file "pel.el"))

;; -----------------------------------------------------------------------------
(provide 'build-pel)

;;; build-pel.el ends here

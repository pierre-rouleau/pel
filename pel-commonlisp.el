;;; pel-commonlisp.el --- PEL Common Lisp Support -*-lexical-binding: t-*-

;; Copyright (C) 2020  Pierre Rouleau

;; Author: Pierre Rouleau <prouleau.swd@gmail.com>

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

;;; Code:

;;-pel-autoload
(defun pel-cl-init (use-slime)
  "Initialize Common Lisp editing/development support.
The USE-SLIME argument identifies whether Slime is/will be used.
Set important hooks.
Should be called by the init.el file."
  ;; Common Lisp indentation rules differ from Emacs Lisp indentation rules:
  ;; - for Common Lisp buffers, use common-lisp-indent-function as indenter,
  ;;   replacing the default indenter (which conforms to the Emacs Lisp
  ;;   indentation rules).
  ;; NOTE: this code is already done by slime-setup, so this is therefore
  ;; not required when Slime is used.
  (unless use-slime
    (add-hook 'lisp-mode-hook
              (lambda ()
                (set (make-local-variable lisp-indent-function)
                     'common-lisp-indent-function)))))

;; -----------------------------------------------------------------------------
(provide 'pel-commonlisp)

;;; pel-commonlisp.el ends here

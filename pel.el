;;; pel.el --- Pragmatic Environment Library

;; Copyright (C) 2020  Pierre Rouleau

;; Author: Pierre Rouleau <prouleau.swd@gmail.com>
;; URL: https://github.com/pierre-rouleau/pel
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.1") (use-package "2.4"))
;; Keywords: convenience

;;; Commentary:

;; The PEL package provides a mechanism to load a set of pre-configured
;; Emacs Lisp packages quickly.  This set includes the PEL convenience
;; packages which aim to simplify several operations.  PEL also comes
;; with key-bindings mainly using function, cursor and numeric keypad
;; keys, and mostly leaving Emacs standard key untouched.
;; In this version PEL uses the F2, F6, F11 and F12 keys as key prefixes.

;; PEL relies on Emacs customization system.  PEL activates third party
;; packages through customization, by setting a corresponding 'pel-use-...'
;; variable to t. Once a feature is activated through customization,
;; PEL also provides extra key bindings and in some cases allow dynamic
;; activation and de-activation of external packages.

;; This is an early version of PEL.  It will grow with time, incorporating
;; more Emacs packages to support more editing tasks.

;; However, the goal is to maintain a quick Emacs init time, even as the
;; number of installed package increases, through the extensive use of
;; auto-loading.

;; To use PEL, execute the `pel-init' function.

;; Using PEL I'm able to keep my Emacs init time at about 0.4 seconds for
;; Emacs running in terminal (tty) mode and 0.6 seconds in graphics mode,
;; with all packages currently supported activated and several others in my
;; .emasc/elpa directory (it lists over 100 packages) that I plan to also
;; integare into PEL.

;; PEL also comes with a set of PDF-formatted tables describing Emacs
;; key bindings, including native Emacs keys, third party package keys and
;; key bindings provided by PEL.  The tables are organized by editing
;; concepts and conatain large set of hyperlink to Emacs manual and relevant
;; documentation available on the net.

;; A more descriptive manual is available in the Github home page.

;;; License

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

;;; Code:

;;;###autoload
(defun pel-init ()
  "Initialize PEL, map its keys, autoload its functions.

Only the PEL features activated via the `pel-use-...' customization variables
from the  \"Pel Package Use\" subgroup of the \"Pel\" group are loaded and the
respective PEL keys are mapped.  The others are not.

If you need to activate new features, use \\[customize] to customize variables
inside the \"Pel\" group.  The \"Pel Package Use\" subgroup contains the
customization variables that control PEL activated features.

You can customize PEL feature only after execution of the `pel-init' command.
After a customization change its best to restart Emacs, however if your
modifications simply activate new features, you may be able to simply
re-execute `pel-init' again to activate them."
  (interactive)
  ;; ensure the library name is sorting just before pel.el
  ;; so that package system bu=yte compile it after everything else.
  (load-library "pel-zkeys")
  (message "PEL loaded, PEL keys binding in effect"))

;; -----------------------------------------------------------------------------
(provide 'pel)

;;; pel.el ends here

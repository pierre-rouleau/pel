;;; pel.el --- Pragmatic Environment Library

;; Copyright (C) 2020  Pierre Rouleau

;; Author: Pierre Rouleau <prouleau.swd@gmail.com>
;; URL: https://github.com/pierre-rouleau/pel
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.1") (use-package "2.4"))
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; License:

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

;;; Commentary:

;; The PEL package provides:
;;
;; - Pre-canned configuration of several useful Emacs packages.
;;   Relieving you from having to write code in your Emacs init file.
;; - The configuration is made to load quickly, making extensive
;;   use of auto-loading and deferred loading is used everywhere.
;; - The selection of package and features is done via PEL customization
;;   variables.
;; - Allows dynamic control of several packages with commands.
;;   For example, PEL support both auto-complete and company, providing
;;   commands to activate one mode in a buffer and another mode inside
;;   another or all other buffers directly from command execution and
;;   key strokes.
;; - Key bindings using function key prefixes (F2, F6, F11 and F12)
;;   provide access to many features and help learn what's available.
;; - The which-key package is used and activated by default, allowing
;;   you to see what's available easily.  F11 is the main prefix key
;;   and all prefixes have a meaningful name that starts with the
;;   'pel:' prefix.  F2 and F6 are used as global shortcut prefix keys,
;;   and F12 as a mode-sensitive shortcut prefix key.
;; - Most standard Emacs keys are left untouched.
;; - Documentation in the form of PDF file on several Emacs topics.
;;   Each table provides an overview, command descriptions, related
;;   key bindings  and links to related on-line documents.  The
;;   tables have extensive markup with colours for standard Emacs,
;;   external package, and PEL-provided command and key bindings.
;;
;; PEL relies on Emacs customization system.  PEL activates third party
;; packages through customization, by setting a corresponding 'pel-use-...'
;; variable to t. Once a feature is activated through customization,
;; PEL also provides extra key bindings and in some cases allow dynamic
;; activation and de-activation of external packages.
;;
;; PEL code is written in several files.  The pel.el file holds `pel-init'
;; which initializes PEL, controls auto-loading of all supported packages
;; and builds the key bindings.  There are several other PEL files that
;; are used by that.  But they can also be used independently.  So if you
;; do not want to use PEL key bindings, you can just use some of the PEL
;; modules and provide you own bindings in your own Emacs init file.
;;
;; This is an early version of PEL.  It will grow with time, incorporating
;; more Emacs packages to support more editing tasks.
;;
;; To use the PEL auto-loading of packages and key bindings, put the
;; following code inside your Emacs init.el file:
;;
;;       (require 'pel)
;;       (pel-init)
;;
;; To start PEL interactively, you can also type:
;;
;;      M-x pel-init
;;
;; A more descriptive manual is available in the Github home page.

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

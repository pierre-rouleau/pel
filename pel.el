;;; pel.el --- Pragmatic Environment Library

;; Copyright (C) 2020  Pierre Rouleau

;; Author: Pierre Rouleau <prouleau.swd@gmail.com>
;; URL: https://github.com/pierre-rouleau/pel
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.1") (use-package "2.4"))
;; Keywords: convenience

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

;;; Commentary:

;; Overview
;; --------

;; `pel.el' provides the following features:
;;
;;  - Function key-driven keymap for a large set of commands, which are either
;;    PEL extensions of existing Emacs commands or commands from external
;;    packages that may be activated via customization of the "Pel" group
;;    using the customize command.
;;  - Configuration code to download, install and load external packages
;;    selected by PEL customization.  Packages are autoloaded as much as
;;    possible, providing a quick Emacs init time.
;;  - Several key mappings that will work in Emacs running in graphics mode
;;    but also in terminal (TTY) mode.
;;  - Several enhanced navigation keys similar to what is available to other
;;    editors in the Brief/CRiSP family such as a flexible home and end key
;;    that accept single, double, triple, quadruple strokes depending on the
;;    context.
;;  - Enhanced letter case conversion key mapping allowing the use of a M-c
;;    and M-C to perform downcasing and upcasing as well as capitalization.
;;
;;  Most PEL key bindings are using function key prefixes.  At the moment these
;;  are fixed to the F2, F6, F11 and F12 keys and are not (yet) customizable.
;;

;; PDF Documentation
;; -----------------
;;
;;  PEL key bindings and a very large set of Emacs standard key bindings are
;;  described in a set of PDF files.  The format of these files is something
;;  between the Emacs Reference Card and the full manual.  Each PDF file is
;;  one table on a specific topic with a quick overview of the topic, the list
;;  of key bindings, references to the Emacs commands, description and hyperlinks
;;  to lots of material including the online Emacs manuals, discussions and
;;  other resources.
;;

;; Usage
;; -----

;; To activate the PEL keymaps and allow its customization, execute the
;; following command:

;;    M-x pel-init

;; If you want to activate it on startup, then put the following inside your
;; Emacs init file:

;;    (require 'pel)
;;    (pel-init)

;; Emacs customization must be placed before the execution of (pel) since `pel'
;; uses the `pel-use-...' customization variables to determine what to load
;; and what keys to bind.  If you place your Emacs customization inside a
;; file separate from your init.el, then you should have something like the
;; following inside your init.el file:

;;    (setq custom-file "~/.emacs-custom.el")  ; or any file you like
;;    (load custom-file)
;;    (require 'pel)
;;    (pel)


;; -----------------------------------------------------------------------------
;;; Code:

;; Utilities:

;; To prevent some byte-compiler warnings, the following functions used only
;; in this file are defined at "file scope".


;; --
;; pel-init - loads all of PEL

;;;###autoload
(defun pel-init ()
  "Initialize the PEL system, map its keys, autoload its functions.

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

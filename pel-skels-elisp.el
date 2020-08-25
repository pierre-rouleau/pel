;;; pel-skels-elisp.el --- Tempo skeleton for Emacs Lisp.  -*- lexical-binding: t; -*-
;; Created   : Monday, August 24 2020.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2020-08-25 09:20:17, updated by Pierre Rouleau>

;; This file is part of the PEL package.
;; This file is not part of GNU Emacs.
;;
;; Copyright (C) 2020  Pierre Rouleau
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

;;; ----------------------------------------------------------------------------
;;; Commentary:
;;
;; This file defines functions ('-') and variables ('>') that implement
;; user-option controlled tempo skeletons for Emacs Lisp source code files.
;;
;; The current implementation only provides one skeleton,
;; the `pel-skels-elisp-large-header-skel' that creates the large file header
;; block, bound to the ``<f12> <f12> C-h`` key like the large header templates
;; for other major modes supported by PEL.
;;
;; The function `pel--install-elisp-skel' is called by the function `pel-init'
;; to install the skeleton.
;;
;; The logical hierarchy of the code is the following:
;;
;; - `pel--install-elisp-skel'
;;   > `pel-skels-elisp-large-header-skel'
;;     - `pel-skels-elisp-file-header-block'
;;       - `pel-skel-elisp-package-name-line'
;;         - `pel-elisp-file-package-name'
;;       - `pel-skel-elisp-separator-line'
;;   > `pel--elisp-skels'
;;   > `pel--elisp-skels-keys'
;;

;;; ----------------------------------------------------------------------------
;;; Dependencies:
(require 'pel--base)       ; use: pel-current-buffer-filename
(require 'pel--options)    ; use: pel-elisp-skel-package-name
(require 'pel-prompt)      ; use: pel-prompt-purpose-for
(require 'pel-skels)       ; use: pel-skel-create-comment
;;                         ;      pel-skel-author-comment
;;                         ;      pel-skel-time-stamp
(require 'pel-tempo)       ; use: pel-tempo-install-pel-skel
(require 'pel-text-insert) ; use: pel-separator-line
;;; -----------------------------------------------------------------------------
;;; Code:

(defun pel-elisp-file-package-name ()
  "Return the Elisp package name of the current file.
This is the string that constitute the first word of the filename."
  (upcase
   (car
    (split-string
     (pel-current-buffer-filename :sans-directory :sans-extension)
     "-"))))

(defun pel-skel-elisp-package-name-line ()
  "Return a string with the commented package name note if required.
This is controlled by the user option variable `pel-elisp-skel-package-name'.
If none is required the function returns nil.
If a string is returned  ends with a new line."
  (let ((package-name (cond ((eq pel-elisp-skel-package-name 'extract-from-file-name)
                             (pel-elisp-file-package-name))
                            ((stringp pel-elisp-skel-package-name)
                             pel-elisp-skel-package-name))))
    (when package-name
      (format ";; This file is part of the %s package.\n" package-name))))

(defun pel-skel-elisp-separator-line ()
  "Return a section separator line for Elisp if required.
If prohibited (by customization) returns nil.
Otherwise return a string that ends with a newline."
  (when pel-elisp-skel-use-separators
    (concat (pel-separator-line nil nil ";;;") "\n")))

(defun pel-skels-elisp-file-header-block ()
  "Return a tempo list for a Emacs Lisp file header block."
  (let ((purpose (pel-prompt-purpose-for "File" 'p))
        (fname   (pel-current-buffer-filename :sans-directory))
        (libname (pel-current-buffer-filename :sans-directory :sans-extension)))
    (list
     'l
     ";;; " fname " --- " purpose
     "  -*- lexical-binding: t; -*-\n\n"
     (pel-skel-created-comment ";;")
     (pel-skel-author-comment  ";;")
     (pel-skel-time-stamp pel-elisp-skel-insert-file-timestamp ";;") 'n
     (pel-skel-elisp-package-name-line)
     ";; This file is not part of GNU Emacs.\n\n" ; if that's not the case, let me know!
     (when pel-elisp-skel-with-license
       (list 'l
             (pel-license-text ";;") 'n))
     (pel-separator-line nil nil ";;;") 'n
     ";;; Commentary:" 'n
     ";;\n"
     ";; " 'p 'n 'n
     (pel-skel-elisp-separator-line)
     ";;; Dependencies:" 'n
     ";;\n"
     ";; " 'p 'n 'n
     (pel-skel-elisp-separator-line)
     ";;; Code:" 'n
     ";;\n"
     'p 'n 'n
     (pel-separator-line nil nil ";;;") 'n
     "(provide '" libname ")" 'n 'n
     ";;; " fname " ends here" 'n)))

;; -----------------------------------------------------------------------------
;; Install Emacs Lisp skeletons

(defvar pel-skels-elisp-large-header-skel
  '(o
    (pel-skels-elisp-file-header-block))
  "The skeleton of an Emacs Lisp file header block.
Please see the function `tempo-define-template'.")

(defvar pel--elisp-skels
  '(("File Header" "file-header" pel-skels-elisp-large-header-skel))
  "List of Emacs Lisp tempo skeletons.")

(defvar pel--elisp-skels-keys '(("file-header" . "C-h"))
  "Key mapping for Emacs Lisp skeletons.")

;;-pel-autoload
(defun pel--install-elisp-skel (key-map)
  "Create the Emacs Lisp skeletons and bind them in the KEY-MAP specified.
This function is meant to be called by the function `pel-init' only."
  (pel-tempo-install-pel-skel
   "elisp"
   pel--elisp-skels
   key-map
   pel--elisp-skels-keys
   "elisp"))

;;; ----------------------------------------------------------------------------
(provide 'pel-skels-elisp)

;;; pel-skels-elisp.el ends here

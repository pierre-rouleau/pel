;;; pel-skels-clisp.el --- Tempo skeleton for Common Lisp.  -*- lexical-binding: t; -*-

;; Created   : Tuesday, April 20 2021.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2021-04-21 17:52:05, updated by Pierre Rouleau>

;; This file is part of the PEL package.
;; This file is not part of GNU Emacs.

;; Copyright (C) 2021  Pierre Rouleau
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
;;

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;

(require 'pel--options)
(require 'pel-text-insert)        ; use: pel-separator-line
(require 'pel-prompt)             ; use: pel-prompt-purpose-for
(require 'pel-tempo)              ; use: pel-tempo-install-pel-skel
(require 'pel-skels)              ; use: pel-skel-time-stamp"
;;; --------------------------------------------------------------------------
;;; Code:
;;


;; Code Guideline and Conventions
;;
;; The code generated by the template is attempting to follow these Common
;; Lisp Code Guidelines:
;;
;; - lisp-lang Style Guide:
;;   - URL: https://lisp-lang.org/style-guide
;;
;; - Tutorial on Good Lisp Programming Style, by Peter Novig and Kent Pitman
;;   - URL: http://norvig.com/luv-slides.ps

;; - 100-column maximum width  : adjustable with `pel-clisp-fill-column'
;; - ; for inline comment
;; - ;; for in-function comment
;; - ;;; for between-function comment
;; - ;;;; for section header

;; ---------------------------------------------------------------------------

(defun pel-skel-clisp-separator-line (&optional prefix)
  "Return a section separator line for Common lisp if required.
If prohibited (by customization) returns nil.
Otherwise return a string with specified PREFIX ending with a newline.
If PREFIX is nil \";;;\" is used."
  (when pel-elisp-skel-use-separators
    (concat (pel-separator-line nil nil prefix) "\n")))

(defun pel-skels-clisp-file-header-block ()
  "Return a tempo list for a Common Lisp file header block."
  (let ((purpose (pel-prompt-purpose-for "File" 'p))
        (fname   (pel-current-buffer-filename :sans-directory)))
    (goto-char (point-min)) ; TODO: del this but mod skels to force entry at top.
    (list
     'l
     (when pel-clisp-emacs-filevar-line
       (list
        'l
        ";;;; -*- "
        pel-clisp-emacs-filevar-line
        " -*-\n"
        ";;;;\n"
        (pel-skel-clisp-separator-line ";;;;")
        ";;;;\n"
        )
       )
     ";;;; Common Lisp File: " fname 'n
     ";;;; Purpose         : " purpose 'n
     (pel-skel-created-comment ";;;;" nil "Created         ")
     (pel-skel-author-comment  ";;;;"     "Author          ")
     (pel-skel-time-stamp pel-clisp-skel-insert-file-timestamp ";;;;" nil nil "      ")
     (when pel-clisp-skel-with-license
       (cond
        ((eq pel-clisp-skel-with-license 'license-text)
         (list 'l
               ";;;;\n"
               (pel-license-text ";;;;") 'n))
        ((eq pel-clisp-skel-with-license 'license-line)
         (let ((license-type (or (pel-prompt "License type" 'license-type)
                                 'p)))
           (list 'l
                 ";;;; License Type    : " license-type 'n )))))
     (pel-skel-clisp-separator-line ";;;;")
     ";;;; Commentary:" 'n
     ";;;;\n"
     ";;;; " 'p 'n 'n
     (pel-skel-clisp-separator-line ";;;;")
     ";;;; Dependencies:" 'n
     ";;;;\n"
     ";;;; " 'p 'n 'n
     (pel-skel-clisp-separator-line ";;;;")
     ";;;; Code:" 'n
     ";;;;\n"
     'p 'n 'n
     (pel-skel-clisp-separator-line ";;;;")
     'n)))

;; -----------------------------------------------------------------------------
;; Install Common Lisp skeletons

(defvar pel-skels-clisp-large-header-skel
  '(o
    (pel-skels-clisp-file-header-block))
  "The skeleton of a Common Lisp file header block.
Please see the function `tempo-define-template'.")

(defvar pel--clisp-skels
  '(("File Header" "file-header" pel-skels-clisp-large-header-skel))
  "List of Common  Lisp tempo skeletons.")

(defvar pel--clisp-skels-keys '(("file-header" . "h"))
  "Key mapping for Common Lisp skeletons.")

;;-pel-autoload
(defun pel--install-clisp-skel (key-map)
  "Create the Common Lisp skeletons and bind them in the KEY-MAP specified.
This function is meant to be called by the function `pel-init' only."
  (pel-tempo-install-pel-skel
   "lisp"
   pel--clisp-skels
   key-map
   pel--clisp-skels-keys
   "lisp"))


;;; --------------------------------------------------------------------------
(provide 'pel-skels-clisp)

;;; pel-skels-clisp.el ends here
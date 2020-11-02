;;; pel-skels-generic.el --- Generic code tempo skeletons.  -*- lexical-binding: t; -*-

;; Created   : Sunday, August 30 2020.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2020-11-01 16:39:18, updated by Pierre Rouleau>

;; This file is part of the PEL package.
;; This file is not part of GNU Emacs.

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
;; This file defines tempo skeletons that can be used for all files.  They are
;; not specialized for a specific programming or markup language.  They only
;; adjust the strings used for comments.

;; The only command is `pel--install-generic-skel' which dynamically creates
;; skeletons commands with keyboard mappings for a specific keyboard map.
;;

;;; ----------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'pel--base)       ; use: pel-current-buffer-filename
(require 'pel--options)    ; use: pel-generic-skel-module-header-block-style
(require 'pel--macros)     ; use: pel-append-to
(require 'pel-prompt)      ; use: pel-prompt-purpose-for
(require 'pel-skels)       ; use: pel-skel-create-comment
;;                         ;      pel-skel-author-comment
;;                         ;      pel-skel-time-stamp
;;                         ;      pel-skel-call
(require 'pel-tempo)       ; use: pel-tempo-install-pel-skel
(require 'pel-text-insert) ; use: pel-separator-line

;;; ----------------------------------------------------------------------------
;;; Code:
;;

;; --
;; Utility functions

(defun pel-skels-generic-separator-line ()
  "Return a section separator line for generic template is required.
If prohibited (by customization) returns nil.
Otherwise return a string that ends with a newline."
  (when pel-generic-skel-use-separators
    (concat (pel-separator-line) "\n")))

;; -----------------------------------------------------------------------------
;; File/Module header block

(defun pel-skels-generic-header-module-block (fname cmt-style)
  "Return a tempo list for the header/module comment block.
The arguments are:
- FNAME := string.  the name of the current file without path.
- CMT-STYLE := a list of 3 strings: (cb cc ce)
            - cb : comment begin string
            - cc : comment continuation string
            - ce : comment end string."
  (let* ((purpose  (pel-prompt-purpose-for "File" 'p))
         (cb       (nth 0 cmt-style))
         (cc       (nth 1 cmt-style))
         (ce       (nth 2 cmt-style)))
    (list
     'l
     cb (format " %s FILE: %s\n"
                (upcase (car (split-string (symbol-name major-mode) "-")))
                fname)
     cc 'n
     cc " Purpose   : " purpose 'n
     (pel-skel-created-comment cc :no-new-line)
     (pel-skel-author-comment  cc nil :no-new-line)
     (pel-skel-time-stamp pel-generic-skel-insert-file-timestamp cc)
     (when pel-generic-skel-with-license
       (list 'l
             cc 'n
             (pel-license-text cc)
             cc 'n))
     ce (pel-when-text-in ce 'n)
     (pel-separator-line) 'n)))

(defun pel-skels-generic-file-header-block ()
  "Return a tempo list for a C file header block.
The format of the file header block is adjusted for the supported file types:
the C code file and the C header file.
The file header portion is controlled by the style selected by the
variable `pel-generic-skel-module-header-block-style'."
  (let* ((fname        (pel-current-buffer-filename :sans-directory))
         (cmt-style    (pel-skel-comments-strings))
         (cb           (nth 0 cmt-style))
         (cc           (nth 1 cmt-style))
         (ce           (nth 2 cmt-style)))
    (goto-char (point-min)) ; TODO: del this but mod skels to force entry at top.
    (list
     'l
     ;; insert the top level comment block for the top of the file
     ;; Select the style from `pel-generic-skel-module-header-block-style'
     (if pel-generic-skel-module-header-block-style
         (pel-skel-call 'pel-generic-skel-module-header-block-style
                        'pel-skels-generic-header-module-block/custom
                        fname
                        cmt-style)
       (pel-skels-generic-header-module-block fname cmt-style))
     ;; then add the remainder for either a header file or code file
     (let ((sk (list 'l)))
       (if pel-generic-skel-insert-module-sections
           (progn
             (pel-append-to sk
                            (list
                             cb " Module Description\n"
                             cc " ------------------\n"
                             cc 'n
                             cc " " 'p 'n
                             cc 'n
                             ce 'n (pel-when-text-in ce 'n)))
             (dolist (mtitle pel-generic-skel-module-section-titles)
               (pel-append-to sk
                              (list
                               (pel-skels-generic-separator-line)
                               cb " " mtitle 'n
                               cc " " (make-string (length mtitle) ?-) 'n
                               ce 'n (pel-when-text-in ce 'n)
                               'p 'n 'n))))
         (pel-append-to sk (list
                            'n
                            'p 'n 'n)))
       (pel-append-to sk (list (pel-skels-generic-separator-line)))))))

;; -----------------------------------------------------------------------------
;; Install Emacs Lisp skeletons

(defvar pel-skels-generic-large-header-skel
  '(o
    (pel-skels-generic-file-header-block))
  "The skeleton of a C file header block.")

(defvar pel-skels-generic-function-definition-skel
  '(o
    (pel-skels-generic-function-definition))
  "The skeleton of a C function definition block.")

(defvar pel--generic-skels
  '(("File Header" "file-header" pel-skels-generic-large-header-skel))
  "List of Emacs Lisp tempo skeletons.")

(defvar pel--generic-skels-keys '(("file-header" . "h"))
  "Key mapping for Emacs Lisp skeletons.")

;;-pel-autoload
(defun pel--install-generic-skel (key-map)
  "Create the generic skeletons and bind them in the KEY-MAP specified.
This function is meant to be called by the function `pel-init' only."
  (pel-tempo-install-pel-skel
   "generic"
   pel--generic-skels
   key-map
   pel--generic-skels-keys
   "generic"))

;;; ----------------------------------------------------------------------------
(provide 'pel-skels-generic)

;;; pel-skels-generic.el ends here

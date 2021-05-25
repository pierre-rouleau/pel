;;; pel-skels-rst.el --- Tempo skeletons for reStructuredText  -*- lexical-binding: t; -*-

;; Copyright (C) 2020, 2021  Pierre Rouleau

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
;; This file defines tempo skeletons for reStructuredText file and the function
;; `pel--install-rst=skel' to install them.  That function is only called by the
;; function `pel-init'.

;; -----------------------------------------------------------------------------
;;; Dependencies:

(require 'pel-prompt)       ; use: pel-prompt-title
(require 'pel-skels)        ; use: pel-skel-copyright-comment, pel-time-stamp
(require 'pel-tempo)        ; use: pel-tempo-install-pel-skel
(require 'pel-text-insert)  ; use: pel-insert-line

;; -----------------------------------------------------------------------------
;;; Code:

;;-pel-autoload
(defun pel-skels-rst-title-header ()
  "Return a reStructuredText file header tempo list."
  (let ((title (pel-prompt-title))
        (title-entry nil))
    (if (string= title "")
        (setq title-entry (list 'l
                                "==================" 'n
                                'p
                                "Place TITLE here!!" 'n
                                "==================" 'n 'n))
      (let* ((title-line (make-string (length title) ?=)))
        (setq title-entry (list 'l title-line 'n title 'n title-line 'n 'n))))
    (list
     'l
     title-entry
     ":Home URL: " 'p 'n
     ":Project: " 'p 'n
     (pel-skel-created-comment "" ":Created: ")
     (pel-skel-author-comment  "" ":Author: ")
     (pel-time-stamp ":") 'n
     (pel-skel-copyright-comment "" "" ":Copyright: ©" ":License: " ) ; TODO: add user-option to select
     'p 'n 'n `
     ".. contents::  **Table of Contents**" 'n
     ".. sectnum::" 'n 'n
     (pel-separator-line) 'n 'n 'p
     'n 'n
     (pel-separator-line) 'n)))

;; -----------------------------------------------------------------------------
;; Install reStructuredText skeleton based commands

(defvar pel-skels-rst-large-header-skel
  '(o
    (pel-skels-rst-title-header))
  "The skeleton of a file header.
Please see the function `tempo-define-template'.")

(defvar pel--rst-skels
  '(("File Header"  "file-header" pel-skels-rst-large-header-skel))
  "List of reStructuredText tempo skeletons.")

(defvar pel--rst-skels-keys '(("file-header" . "h"))
  "Key mapping for reStructuredText skeletons.")

;;-pel-autoload
(defun pel--install-rst-skel (key-map)
  "Create the reStructuredText skeletons and bind them in the KEY-MAP specified.
This function is meant to be called by the function `pel-init' only."
  (pel-tempo-install-pel-skel
   "rst"
   pel--rst-skels
   key-map
   pel--rst-skels-keys
   "rst"))

;; -----------------------------------------------------------------------------
(provide 'pel-skels-rst)

;;; pel-skels-rst.el ends here

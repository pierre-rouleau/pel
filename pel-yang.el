;;; pel-yang.el --- Yang-mode extension.  -*- lexical-binding: t; -*-

;; Created   : Thursday, October  7 2021.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2021-10-08 08:12:18, updated by Pierre Rouleau>

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
;; Add some functionality to yang-mode, specifically imenu support.

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'pel--base)
(require 'yang-mode nil :noerror)
(defvar yang-mode-hook)

(require 'outline)

;;; --------------------------------------------------------------------------
;;; Code:
;;


(defun pel-yang-show-onelevel ()
  "Show entry and children in outline mode."
  (interactive)
  (outline-show-entry)
  (outline-show-children))

(defun pel-yang-outline-bindings ()
  "sets shortcut bindings for outline minor mode"
  (interactive)

  ;; binding available in terminal mode and in graphics mode
  (local-set-key (kbd "M-A")        'outline-show-all)
  (local-set-key (kbd "M-*")        'outline-show-subtree)
  (local-set-key (kbd "M-#")        'outline-hide-body)

  (local-set-key (kbd "M-(")   'outline-hide-subtree)
  (local-set-key (kbd "M-)") #'pel-yang-show-onelevel)

  (local-set-key (kbd "<C-M-up>")   'outline-backward-same-level)
  (local-set-key (kbd "<C-M-down>") 'outline-forward-same-level)
  (local-set-key (kbd "<C-up>")     'outline-previous-visible-heading)
  (local-set-key (kbd "<C-down>")   'outline-next-visible-heading))


(defconst pel-yang-id-regexp "[-a-zA-Z0-9_\\.:]*"
  "Regexp for a YANG identifier.")

(defun pel-yang-regexp-for (id)
  "Return a regexp capturing name of ID in group 1."
  (format "^ *%s +\\([-a-zA-Z0-9_\\.:]+\\) *{" id))

(defvar pel-yang-imenu-generic-expression
  (list
   (list "Module"    (pel-yang-regexp-for "module")      1)
   (list "Container" (pel-yang-regexp-for "container")   1)
   (list "Feature"   (pel-yang-regexp-for "feature")     1)
   (list "Grouping"  (pel-yang-regexp-for "grouping")    1)
   (list "List"      (pel-yang-regexp-for "list")        1)
   (list "Leaf"      (pel-yang-regexp-for "leaf")        1)
   (list "Leaf-List" (pel-yang-regexp-for "leaf-list")   1)
   (list "Typedef"   (pel-yang-regexp-for "typedef")     1)
   )
  "PEL imenu support for YANG.")

(defun pel-yang-setup-imenu ()
  "Setup imenu support for yang-mode."

  (setq imenu-generic-expression pel-yang-imenu-generic-expression))

(defun pel-yang-setup-support ()
  "Setup PEL support for yang-mode."
  (pel-yang-setup-imenu)
  (outline-minor-mode)
  (setq outline-regexp
        (concat "^ *" pel-yang-id-regexp " *"
                pel-yang-id-regexp
                " *{"))
  (pel-yang-outline-bindings))

;;; --------------------------------------------------------------------------
(provide 'pel-yang)

;;; pel-yang.el ends here

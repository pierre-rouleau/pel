;;; pel-imenu-dbg.el --- Tools to debug iMenu support.  -*- lexical-binding: t; -*-

;; Created   : Friday, February 19 2021.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2021-04-09 12:58:08, updated by Pierre Rouleau>

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
;; imenu support is not always correct.  The code in this file help
;; investigate imenu support.

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'pel--base)                    ; use: pel-print-in-buffer,
;;                                      ;      pel-insert-symbol-content

;;; --------------------------------------------------------------------------
;;; Code:
;;

(defun pel-imenu-dbg-print-vars ()
  "Print imenu controlling variables for current major mode in *imenu-dbg*."
  (interactive)
  (let ((buffer (current-buffer)))
    (pel-print-in-buffer
     "*imenu-dbg*"
     "imenu control variables"
     (lambda ()
       "Print imenu variables."
       (pel-insert-symbol-content 'imenu-use-markers                  buffer :on-same-line)
       (pel-insert-symbol-content 'imenu-max-item-length              buffer :on-same-line)
       (pel-insert-symbol-content 'imenu-auto-rescan                  buffer :on-same-line)
       (pel-insert-symbol-content 'imenu-auto-rescan-maxout           buffer :on-same-line)
       (pel-insert-symbol-content 'imenu-use-popup-menu               buffer :on-same-line)
       (pel-insert-symbol-content 'imenu-eager-completion-buffer      buffer :on-same-line)
       (pel-insert-symbol-content 'imenu-after-jump-hook              buffer :on-same-line)
       (pel-insert-symbol-content 'imenu-sort-function                buffer :on-same-line)
       (pel-insert-symbol-content 'imenu-max-items                    buffer :on-same-line)
       (pel-insert-symbol-content 'imenu-space-replacement            buffer :on-same-line)
       (pel-insert-symbol-content 'imenu-level-separator              buffer :on-same-line)
       (pel-insert-symbol-content 'imenu-generic-skip-comments-and-strings  buffer :on-same-line)
       (pel-insert-symbol-content 'imenu-case-fold-search             buffer :on-same-line)
       (pel-insert-symbol-content 'imenu-create-index-function        buffer :on-same-line)
       (pel-insert-symbol-content 'imenu-prev-index-position-function buffer :on-same-line)
       (pel-insert-symbol-content 'imenu-extract-index-name-function  buffer :on-same-line)
       (pel-insert-symbol-content 'imenu-name-lookup-function         buffer :on-same-line)
       (pel-insert-symbol-content 'imenu-default-goto-function        buffer :on-same-line)
       (pel-insert-symbol-content 'imenu--cleanup-seen                buffer :on-same-line)
       (pel-insert-symbol-content 'imenu--menubar-keymap              buffer :on-same-line)
       (pel-insert-symbol-content 'imenu-buffer-menubar               buffer :on-same-line)
       (pel-insert-symbol-content 'imenu-menubar-modified-tick        buffer :on-same-line)
       (pel-insert-symbol-content 'imenu-example--function-name-regexp-c buffer :on-same-line)
       (pel-insert-symbol-content 'imenu--rescan-item                  buffer :on-same-line)

       (pel-insert-list-content   'imenu--index-alist-roup            buffer)
       (pel-insert-list-content   'imenu-generic-expression           buffer)
       (pel-insert-list-content   'imenu-syntax-alist                 buffer)
       (pel-insert-list-content   'imenu--last-menubar-index-alist    buffer)
       (pel-insert-list-content   'imenu--history-list                buffer)))))

;;; --------------------------------------------------------------------------
(provide 'pel-imenu-dbg)

;;; pel-imenu-dbg.el ends here

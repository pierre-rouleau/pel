;;; pel-imenu.el --- PEL imenu extensions.

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
;; imenu index menu lists the entries with sub-menu first then the entries
;; without sub-menus after.  So the order of the entries in the menu does
;; not reflect the order of those entries in the file.
;; This is quite annoying when using the menu to list things like the
;; sections of a reStructuredtext file.
;;
;; The following code changes this behavior.
;;
;; Code based on the pdf-tools/pdf-outline code, which I found via the
;; following StackExchange discussion:
;; http://emacs.stackexchange.com/questions/31791/order-of-items-in-imenu?noredirect=1#comment48799_31791
;;
;;
;; The new behaviour is activated at startup.
;; Change the behaviour by executing pel-toggle-imenu-index-follows-order and
;; then rescan the index.

;; BUG:
;; I would like to not have to rescan after executing
;; pel-toggle-imenu-index-follows-order, but the index list is not updated
;; until a rescan is done.  Maybe I have to use hook for that, I have not
;; looked into the code enough to find out.
;;

;; -----------------------------------------------------------------------------
;;; Code:

(require 'pel-base)                     ; use: pel-toggle
(eval-when-compile
  (require 'imenu))                     ; use: imenu--rescan-item,
                                        ;      imenu-max-items,
                                        ;      imenu--split

;; -----------------------------------------------------------------------------

(defvar pel-imenu-index-follows-order-p t
  "Control how imenu index entries are listed:
- nil: the entries with sub-menus are shown at the top (original, standard
       behaviour).
- t:   the entries are shown as an outline: in the exact same other
       as they appear in the buffer/file -- (new default behaviour).")

;;-pel-autoload
(defun pel-toggle-imenu-index-follows-order ()
  "Change the way the imenu index entries are organized."
  (interactive)
  (pel-toggle 'pel-imenu-index-follows-order-p)
  (if (fboundp 'imenu--cleanup)
      (imenu--cleanup)
    (error "Failed toggling menu: imenu--cleanup not found")))

(defun pel-imenu-outline--split-menu (oldfun menulist title)
  "Replacement function for `imenu--split-menu'.
This function does not move sub-menus to the top, to keep buffer's
outline order.  Also it does not call `imenu-sort-function'.
It has two modes, controlled by `pel-imenu-index-follows-order-p';
when that is t it does not change the entries order, but when it is
nil it behaves like the original implementation which is passed
via OLDFUN.  The original two arguments MENULIST and TITLE follow."
  (if (fboundp 'imenu--split)
      (if pel-imenu-index-follows-order-p
          (let ((menulist (copy-sequence menulist))
                keep-at-top)
            (if (memq imenu--rescan-item menulist)
                (setq keep-at-top (list imenu--rescan-item)
                      menulist (delq imenu--rescan-item menulist)))
            (if (> (length menulist) imenu-max-items)
                (setq menulist
                      (mapcar
                       (lambda (menu)
                         (cons (format "From: %s" (caar menu)) menu))
                       (imenu--split menulist imenu-max-items))))
            (cons title
                  (nconc (nreverse keep-at-top) menulist)))
        (funcall oldfun menulist title))
    (error "Internal error: the function imenu--split is unknown")))

;;-pel-autoload
(defun pel-imenu-init ()
  "Initialize the use of imenu by PEL."
  (advice-add 'imenu--split-menu :around #'pel-imenu-outline--split-menu))

;; -----------------------------------------------------------------------------
(provide 'pel-imenu)

;;; pel-imenu.el ends here

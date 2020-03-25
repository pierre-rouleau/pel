;;; pel-search.el --- PEL Search Utilities

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

;; PEL: Search behaviour control functions
;; ---------------------------------------


(require 'isearch)   ; use: search-upper-case.
;;                   ; isearch is part of standard Emacs distribution and is
;;                   ; loaded even by emacs -Q (in emacs 26).

;;; Code:

;;-pel-autoload
(defun pel-toggle-case-fold-search ()
  "Toggle case sensitivity of current buffer searching.
Toggles the value of local variable `case-fold-search'."
  (interactive)
  (setq case-fold-search (not case-fold-search))
  (pel-show-search-case-state))

;;-pel-autoload
(defun pel-toggle-search-upper-case ()
  "Toggle case sensitivity behaviour of yank in search prompt.
Rotates the value of `search-upper-case' to:
- nil       : upper case don't force case sensitivity
- t         : upper case force case sensitivity
- not-yanks : upper case force case sensitivity, and
              lower case text when yank in search minibuffer."
  (interactive)
  (cond
   ((not search-upper-case)             ; search-upper-case = nil -> t
    (progn
      (setq search-upper-case t)
      (message "Uppercase in search forces case sensitivity.")))
   ((equal search-upper-case 'not-yanks) ; search-upper-case = not-yanks -> nil
    (progn
      (setq search-upper-case nil)
      (message "Uppercase in search have no impact.")))
   (t                                  ; search-upper-case = t  -> not-yanks
    (progn
      (setq search-upper-case 'not-yanks)
      (message "Uppercase in search force case sensitivity,\
 lowercased yanks in search minibuffer.")))))

;; --

(defun pel-search-case-state ()
  "Return description of how search is dealing with letter cases.
.
Depends on 2 Emacs (base system) variables:
.
|`case-fold-search'|`search-upper-case'|
+------------------+-------------------+
| nil              | N/A               ==> case sensitive
| t                | nil               ==> case insensitive
| t                | t                 ==> case insensitive,
|                  |                       upper in search forces sensitivity
| t                | not-yanks         ==> case insensitive,
|                  |                       upper in search forces sensitivity,
|                  |                       lowercased yanks in search buffer"
  (if case-fold-search
      (cond ((not search-upper-case) "Case insensitive search.")
            ((equal search-upper-case 'not-yanks)
             "Case insensitive unless uppercase in search,\
 lowercased yanks in search buffer.")
            (t "Case insensitive unless uppercase in search."))
    "Case sensitive search."))

;;-pel-autoload
(defun pel-show-search-case-state ()
  "Describe the search case handling behaviour in the mini-buffer."
  (interactive)
  (message (pel-search-case-state)))

;; -----------------------------------------------------------------------------
(provide 'pel-search)

;;; pel-search.el ends here

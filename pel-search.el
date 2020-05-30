;;; pel-search.el --- PEL Search Utilities -*-lexical-binding: t-*-

;; Copyright (C) 2020  Pierre Rouleau

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
;; This file holds a set of search utilities and function that manages the
;; search tools available to PEL.
;;
;; The following is a list of available commands (*) and functions (-) listed in
;; hierarchical calling order.
;;
;; Search behaviour control functions
;;
;; * `pel-toggle-case-fold-search'
;; * `pel-toggle-search-upper-case'
;; * `pel-show-search-case-state'
;;   - `pel-search-case-state'
;;
;; Search utility
;;
;; * `pel-search-word-from-top'
;;
;; Search Tool Management:
;;
;; * `pel-show-active-search-tool'
;;   - `pel--active-search-tool'
;; * `pel-select-search-tool'
;;   - `pel--search-tools-selection'
;;   - `pel--activated-search-tool'
;;   - `pel-set-search-tool'
;;     - `pel--disable-search-tool'
;;     - `pel--activate-search-tool'

;;

;;; Code:

(require 'isearch)       ; use: search-upper-case.
;;                       ; isearch is part of standard Emacs distribution and is
;;                       ; loaded even by emacs -Q (in emacs 26).
(require 'pel--options)  ; use: pel-use-ansu, pel-use-swiper,
;;                       ;      pel-initial-search-tool
(require 'pel--macros)
(require 'pel-prompt)
(require 'pel-read)  ; use: pel-word-at-point
(require 'pel-window); use pel-window-direction-for

;; -----------------------------------------------------------------------------
;; Search behaviour control functions
;; ----------------------------------

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
;; Search Utilities
;; ----------------

;;-pel-autoload
(defun pel-search-word-from-top (&optional n)
  "Search word at point from top/bottom of buffer in window identified by N.

Grab the word at point from the current window.
Search in the window identified by N:
- If N is negative, search backward from the bottom of the window
  identified by (abs N).  Otherwise search forward from the top
  of the the window identified by N.
- If N is not specified, nil or 1: search in the current window.
- If N is 0:                     : search in other window
- If N in [2,8] range:           : search in window identified by the direction
                                   corresponding to the cursor in a numeric
                                   keypad:
                                   -             8 := 'up
                                   - 4 := 'left  5 := 'current  6 := 'right
                                   -             2 := 'down
- If N is 9 or larger            : search in window below.


Explicitly selecting the minibuffer window, or a non-existing window is not
allowed, and search is done in current window.

Searched word is remembered and can be used again to repeat an interactive
search with \\[isearch-forward] or \\[isearch-backward].
Position before searched word is pushed on the mark ring."
  (interactive "P")
  ;; Use current window if n is not specified.
  ;; Otherwise select direction from numerical argument.
  (push-mark)
  (let* ((n-value (prefix-numeric-value n))
         (do-forward-search (>= n-value 0))
         (direction (pel-window-direction-for
                     (abs n-value) 'current))
         (searched-word (pel-word-at-point)))
    (when (eq direction 'new)
      (setq direction 'current))
    (pel-window-select direction)
    (if do-forward-search
        ;; forward search
        (progn
          (goto-char (point-min))
          (isearch-forward nil :no-recursive-edit))
      ;; backward search
      (goto-char (point-max))
      (isearch-backward nil :no-recursive-edit))
    (isearch-yank-string searched-word)))

;; -----------------------------------------------------------------------------
;; PEL Search Tool Control
;; -----------------------

(defvar pel--search-initialized nil
  "Set to t when search tool management is initialized.
Modified by pel-search code ONLY.")

(defvar pel--active-search-tool nil
  "Search tool currently used.  One of: nil | anzu | swiper.
A nil value means that Emacs standard search is used.")

;; --

(defun pel--activate-search-tool (tool)
  "Activate the specified search TOOL.
The TOOL argument can be any of nil | anzu | swiper."
  (cond ((eq tool nil)
         (global-set-key "\C-s" 'isearch-forward))
        ;;
        ((and (eq tool 'anzu) (fboundp 'global-anzu-mode))
         (global-anzu-mode +1))
        ;;
        ((eq tool 'swiper)
         (global-set-key "\C-s" 'swiper)))
  (setq pel--active-search-tool tool)
  (message "Now searching with %s" (pel--active-search-tool)))

(defun pel--disable-search-tool (tool)
  "Disable currently specified search TOOL.
The TOOL argument can be any of nil | anzu | swiper.
If TOOL is nil, do nothing."
  (cond ((and (eq tool 'anzu) (fboundp 'global-anzu-mode))
         (global-anzu-mode -1))
        ((eq tool 'swiper)
         (global-set-key "\C-s" 'isearch-forward)))
  (setq pel--active-search-tool nil))

;;-pel-autoload
(defun pel-set-search-tool (tool)
  "Activate the specified search TOOL.
The TOOL argument can be any of nil | anzu | swiper.
A nil value corresponds to Emacs default."
  ;; Disable the currently used tool (if any)
  (pel--disable-search-tool (pel--activated-search-tool))
  ;; Activate the requested one
  (pel--activate-search-tool tool))

(defun pel--activated-search-tool()
  "Return search tool currently used.
Return one of: nil | 'anzu | 'swiper
The nil value means that Emacs default is used."
  pel--active-search-tool)

(defun pel--search-tools-selection ()
  "Return a list of (char prompt symbol) of available search tool choices."
  (let ((selection '((?e "Emacs Default" nil))))
    (when pel-use-anzu   (push '(?a "Anzu" anzu) selection))
    (when pel-use-swiper (push '(?s "Swiper" swiper) selection))
    (reverse selection)))

;;-pel-autoload
(defun pel-select-search-tool ()
  "Prompt user for search tool to use."
  (interactive)
  (unless pel--search-initialized
      ;; select the initial search tool from user option.
    (pel-set-search-tool pel-initial-search-tool)
    (setq pel--search-initialized 1))
  ;;
  (pel-select-from "Search tool"
                   (pel--search-tools-selection)
                   (pel--activated-search-tool)
                   #'pel-set-search-tool))

;; --

(defun pel--active-search-tool ()
  "Return a string describing the currently used search tool."
  (if (not pel--active-search-tool)
      "default ISearch"
    (if (eq pel--active-search-tool 'anzu)
        "ISearch and Anzu"
      (if (eq pel--active-search-tool 'swiper)
          "Swiper"
        "??"))))

;;-pel-autoload
(defun pel-show-active-search-tool ()
  "Display the currently used search tool."
  (interactive)
  (unless pel--search-initialized
    ;; select the initial search tool from user option.
    (pel-set-search-tool pel-initial-search-tool)
    (setq pel--search-initialized 1))
  ;;
  (message "Searching with %s" (pel--active-search-tool)))

;; -----------------------------------------------------------------------------
(provide 'pel-search)

;;; pel-search.el ends here

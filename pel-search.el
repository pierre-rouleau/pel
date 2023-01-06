;;; pel-search.el --- PEL Search Utilities -*-lexical-binding: t; -*-

;; Created   Saturday, February 29 2020.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2023-01-06 10:39:25 EST, updated by Pierre Rouleau>

;; This file is part of the PEL package
;; This file is not part of GNU Emacs.

;; Copyright (C) 2020, 2021, 2022, 2023  Pierre Rouleau with some code derived
;;                                 from Mickey Petersen's work
;;                                 (see CREDITS below).
;;
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
;; * `pel-select-search-tool'
;;   - `pel-active-search-tool-str'
;;   - `pel--active-search-regexp-engine'
;;   - `pel--search-tools-selection'
;;   - `pel--activated-search-tool'
;;   - `pel-set-search-tool'
;;     - `pel--disable-search-tool'
;;     - `pel--activate-search-tool'
;;

;;; --------------------------------------------------------------------------
;;; Dependencies:

(require 'isearch)       ; use: search-upper-case.
(eval-when-compile (require 'subr-x))   ; use: split-string, string-join,
;;
;;                       ; isearch is part of standard Emacs distribution and
;;                       ; is loaded even by emacs -Q (in emacs 26).
(require 'pel--base)     ; use: `pel-symbol-on-off-string'
;;                       ;      `pel-capitalize-first-letter'
(require 'pel--options)  ; use: `pel-use-ansu' `pel-use-swiper'
;;                       ;      `pel-initial-search-tool'
(require 'pel--macros)
(require 'pel-prompt)
(require 'pel-read)      ; use: `pel-word-at-point'
(require 'pel-search-regexp)  ; use: `pel-active-search-regexp-engine'
;;                            ;      `pel--search-regexp-initialized'
;;                            ;      `pel-set-search-regexp-engine'
(require 'pel-window)    ; use: `pel-window-direction-for'
;;                       ;      `pel-count-non-dedicated-windows'

;;; --------------------------------------------------------------------------
;;; Code:


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
      (cond ((not search-upper-case) "case insensitive search.")
            ((equal search-upper-case 'not-yanks)
             "case insensitive unless uppercase in search,\
 lowercased yanks in search buffer.")
            (t "case insensitive unless uppercase in search."))
    "case sensitive search."))

(defun pel-search-case-state-str ()
  "Return a string describing search case behaviour."
  (format"\
- Search               : %s
- Case-fold            : %s
- Lax-whitespace       : %s
- Regexp lax-whitespace: %s
- Subword %s, Superword %s"
           (pel-search-case-state)
           (pel-symbol-on-off-string 'case-fold-search)
           (pel-symbol-on-off-string 'isearch-lax-whitespace)
           (pel-symbol-on-off-string 'isearch-regexp-lax-whitespace)
           (pel-symbol-on-off-string 'subword-mode)
           (pel-symbol-on-off-string 'superword-mode)))

;;-pel-autoload
(defun pel-show-search-case-state ()
  "Describe the search case handling behaviour in the mini-buffer."
  (interactive)
  (message "%s" (pel-search-case-state-str)))

;; ---------------------------------------------------------------------------
;; Search Utilities
;; ----------------

(defun pel-user-option-title-string (user-option-string)
  "Convert a user-option name string into its title.

For example, convert \"pel-use-ido\" into
\"Pel Use Ido: \""
  (concat (string-join
           (mapcar
            (function pel-capitalize-first-letter)
            (split-string user-option-string "-"))
           " ")
          ": "))

;;-pel-autoload
(defun pel-search-word-from-top (&optional n)
  "Search text in region or word at point from top/bottom of specified buffer.

If an area is marked used the text in the area as the searched text,
otherwise search for the word at point.

If 1 non-dedicated window: search from top of current buffer.
If 2 non-dedicated windows:
 - with no argument: search from the top of the other window buffer.
 - with argument 3 or 5: search from the top of current window buffer.
If 3 or more non-dedicated windows:
 Search in the window identified by N:
 - If N is negative, search backward from the bottom of the window
   identified by (abs N).  Otherwise search forward from the top
   of the the window identified by N.
 - If N is not specified, nil or 1: search in the current window buffer.
 - If N is 0:                     : search in other window buffer
 - If N is 3                      : search in current window buffer
 - If N in remaining [2,8] range  : search in the buffer of window identified
                                    by the direction corresponding to the
                                    cursor in a numeric keypad:
                                    -             8 := 'up
                                    - 4 := 'left  5 := 'current  6 := 'right
                                    -             2 := 'down
- If N in [10..18] range         : toggle subword mode in the current buffer
                                   to grab the word to search for this search,
                                   and use (N - 10) to identify the window
                                   to perform the search.
- For N in [20..28] range        : toggle superword mode in the current buffer
                                   to grab the word to search for this search,
                                   and use (N - 20) to identify the window
                                   to perform the search.
- For N in [30..38] range        : Activate superword mode for text extraction
                                   in current buffer, then if the target
                                   buffer is using Custom-mode,
                                   convert the searched string into a
                                   customization user-option title and
                                   and use (N - 30) to identify the window
                                   to perform the search.

Explicitly selecting the minibuffer window, or a non-existing window is not
allowed, and search is done in current window.

Searched word is remembered and can be used again to repeat an interactive
search with \\[isearch-forward] or \\[isearch-backward].
Position before searched word is pushed on the mark ring.

When string is not found, point does not move and an error is thrown.
This allows the use of this command in a keyboard macro that will stop on
failure."
  (interactive "P")
  ;; Use current window if n is not specified.
  ;; Otherwise select direction from numerical argument.
  (let ((searched-text))
    (when (use-region-p)
      (setq searched-text (buffer-substring-no-properties
                           (region-beginning)
                           (region-end)))
      (deactivate-mark))
    (push-mark)
    (let* ((original-window   (selected-window))
           (original-pos      (point))
           (n-value           (prefix-numeric-value n))
           (do-forward-search (>= n-value 0))
           (n-abs             (abs n-value))
           (search-user-option-title (>= 38 n-abs 30))
           (mode-to-toggle    (cond ((>= 28 n-abs 20) 'superword-mode)
                                    ((>= 18 n-abs 10) 'subword-mode)
                                    (t nil)))
           (n-abs             (cond ((>= 38 n-abs 30) (- n-abs 30))
                                    ((>= 28 n-abs 20) (- n-abs 20))
                                    ((>= 18 n-abs 10) (- n-abs 10))
                                    (t n-abs)))
           (direction         (if (and pel-search-from-top-in-other
                                       (eq (pel-count-non-dedicated-windows) 2)
                                       (not (member  n-abs '(3 5))))
                                  'other
                                (pel-window-direction-for n-abs 'current))))
      ;; If text to search was not already identified by a marked region,
      ;; grab word at point, optionally alternating the meaning of word
      ;; while grabbing it.
      (ignore-errors
        (unless searched-text
          (when (and search-user-option-title
                     (not superword-mode))
            (setq mode-to-toggle 'superword-mode))
          (when mode-to-toggle
            (pel-toggle-mode mode-to-toggle))
          (setq searched-text (pel-word-at-point))
          (when mode-to-toggle
            (pel-toggle-mode mode-to-toggle))))
      (if searched-text
          (progn
            ;; Move to the destination window
            (when (eq direction 'new)
              (setq direction 'current))
            (pel-window-select direction)
            ;; if requested to search for a user-option title and destination
            ;; window is a buffer operating in Custom-mode, transform the
            ;; searched string into a user-option title
            (when (and search-user-option-title
                       (eq major-mode 'Custom-mode))
              (setq searched-text
                    (pel-user-option-title-string searched-text)))
            ;; Search from the specified buffer end point to move point
            ;; to target
            (condition-case err
                ;; Perform an Isearch to allow remembering the word and
                ;; provide ability to repeat a manual Isearch.
                ;; But also ensure that an error is thrown when the word is
                ;; not found and prevent the Isearch prompt. For the moment I
                ;; have not found any other way than performing a regular
                ;; search first and then re-do it with an Isearch that is
                ;; guaranteed to succeed.  The first regular search is used to
                ;; detect failure and the Isearch is done to set-up the
                ;; ability to call isearch-yank-string: without it, invalid
                ;; highlighting is done in the target window.    This is
                ;; wasteful but it works.  I could dig into the search
                ;; functions and implement an non-prompting Isearch but I'll
                ;; leave it for later. (TODO?)
                (if do-forward-search
                    ;; forward search
                    (progn
                      (goto-char (point-min))
                      (search-forward searched-text)
                      (goto-char (point-min))
                      (isearch-forward nil :no-recursive-edit))
                  ;; backward search
                  (goto-char (point-max))
                  (search-backward searched-text)
                  (goto-char (point-max))
                  (isearch-backward nil :no-recursive-edit))
              ;; on failure move point back to original window and
              ;; issue the error.
              (search-failed
               (select-window original-window)
               (goto-char original-pos)
               (signal (car err) (cdr err))))
            ;; On success, remember what was searched and allow consecutive
            ;; searches on the same word
            (isearch-yank-string searched-text))
        (user-error "No valid word at point to search")))))


(defun pel-search-word-from-top-noerr (n)
  "Function to perform search from top of specified window.

Always return a status value: t on success, nil on error.
Useful to write ELisp code that uses `pel-search-word-from-top'
non interactively."
  (condition-case nil
      (progn
        (pel-search-word-from-top n)
        t)
    (error nil)))

;; ---------------------------------------------------------------------------
;; PEL Search Tool Control
;; -----------------------

(defvar pel--search-initialized nil
  "Set to t when search tool management is initialized.
Modified by pel-search code ONLY.")

(defvar pel--active-search-tool nil
  "Search tool currently used.  One of: nil | anzu | swiper.
A nil value means that Emacs standard search is used.")

;; --

(defun pel-active-search-tool-str ()
  "Return a string describing the currently used search tool."
  (if (not pel--active-search-tool)
      "default ISearch"
    (if (eq pel--active-search-tool 'anzu)
        "ISearch and Anzu"
      (if (eq pel--active-search-tool 'swiper)
          "Swiper"
        "??"))))

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
  (message "Now searching with %s" (pel-active-search-tool-str)))

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
;;-pel-autoload
(defun pel-show-search-status (&optional with-details)
  "Display search status.
Display:
- the name of the search tool used,
- the regular expression tool used,
- the search case settings used.

With non-nil WITH-DETAILS or any prefix argument, displays
more information about available choices."
  (interactive "P")
  (unless pel--search-initialized
    ;; select the initial search tool from user option.
    (pel-set-search-tool pel-initial-search-tool)
    (setq pel--search-initialized 1))
  (unless pel--search-regexp-initialized
    ;; select the initial search regexp engine
    (pel-set-search-regexp-engine pel-initial-regexp-engine)
    (setq pel--search-regexp-initialized 1))
  (message "\
- Searching with %s
- %s
%s"
           (pel-active-search-tool-str)
           (pel-active-search-regexp-engine-str with-details)
           (pel-search-case-state-str)))

;; ---------------------------------------------------------------------------
;; CREDITS : the following code is derived almost verbatim from Mickey
;;           Petersen's excellent set of Emacs articles on
;;           https://www.masteringemacs.org
;; Source: https://www.masteringemacs.org/article/searching-buffers-occur-mode

(defun pel-buffers-matching-mode (mode)
  "Returns a list of buffers where their major-mode is equal to MODE"
  (let ((buffer-mode-matches '()))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (eq mode major-mode)
          (push buf buffer-mode-matches))))
    buffer-mode-matches))


;;-pel-autoload
(defun pel-multi-occur-in-this-mode ()
  "Show all lines matching REGEXP in buffers with this major mode."
  (interactive)
  (multi-occur
   (pel-buffers-matching-mode major-mode)
   (car (occur-read-primary-args))))

;;; --------------------------------------------------------------------------
(provide 'pel-search)

;;; pel-search.el ends here

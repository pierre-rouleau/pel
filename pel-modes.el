;;; pel-modes.el --- Help facilities for major and minor modes.  -*- lexical-binding: t; -*-

;; Created   : Friday, October 24 2025.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2025-10-24 12:22:03 EDT, updated by Pierre Rouleau>

;; This file is part of the PEL package.
;; This file is not part of GNU Emacs.

;; Copyright (C) 2025  Pierre Rouleau
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
;; - pel-startup-<thing to activate at startup>
;; pel-startup-xref-front-end


;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'pel--base)   ; use: `pel-string-starts-with-p'
(require 'pel--options)
(require 'pel-indent)  ; use: `pel-insert-tab-set-width-info'

;;; --------------------------------------------------------------------------
;;; Code:
;;

(defun pel-mode-activating-user-option-p (symbol)
  "Return t when SYMBOL is valid PEL modes-activating user-option, nil otherwise."
  (and (custom-variable-p symbol)
       (pel-string-starts-with-p (symbol-name symbol)
                                 "pel-modes-activating-")))

(defun pel-compare-symbol-names (s1 s2)
  "Return <0 is name of S1 sorts smaller than name of S2."
  (string< (symbol-name s1) (symbol-name s2)))

(defun pel-mode-activating-user-options ()
  "Return a list of available pel-modes-activating- user-options."
  (let ((symbols '()))
    (mapatoms
     (lambda (symbol)
       (when (pel-mode-activating-user-option-p symbol)
         (push symbol symbols))))
    (sort symbols (function pel-compare-symbol-names))))

(defun pel-mode-activates-p (user-option mode)
  "Return t if USER-OPTION activates major MODE.
USER-OPTION is a symbol for one of the pel-modes-activating- user-options.
MODE is a symbol for the specific mode."
  (memq mode (symbol-value user-option)))

(defun pel-insert-minor-mode-activation-info (mode &optional prelim-inserter)
  "Insert text listing each pel-modes-activating user-option.
For each one add whether it activates the specific MODE."
  (insert (propertize "* Minor Mode Activation Control:" 'face 'bold))
  (when prelim-inserter
    (funcall prelim-inserter))
  (insert "

Other user-options can be also used to activate feature for several modes.
The ones activated for this mode show a check-mark to the right.
")
  (dolist (user-option (pel-mode-activating-user-options))
    (pel-insert-symbol-content
     user-option
     nil nil nil nil :no-value)
    (when (pel-mode-activates-p user-option mode)
      (insert "  ✅"))))

;;-pel-autoload
(defun pel-mode-setup-info (&optional append)
  "Print setup information for the current major mode in specialized buffer.
The buffer name is *pel-mode-info*.
If APPEND is non-nil, append to the buffer.

The command adapt to each major mode, inserting information provided
by mode-specialized functions with conventional names that are called
when bound.

If no specialized mode function is defined (bound) then this only prints the
most generic information about the mode."
  (interactive "P")
  (let* ((pel-insert-symbol-content-context-buffer (current-buffer))
         (current-major-mode major-mode)
         (mode-base-symbol (intern (pel-file-type-for major-mode)))
         (pel-use-mode-user-option-symbol (intern (pel-string-with-major-mode
                                                   "pel-use-%s")))
         (major-mode-used-text-fct (intern (pel-string-with-major-mode
                                       "pel-%s-mode-used-text")))
         (minor-mode-info-inserter-fct (intern (pel-string-with-major-mode
                                                "pel--%s-minor-mode-info")))
         (indent-tab-info-inserter-fct (intern (pel-string-with-major-mode
                                                "pel-%s-insert-indent-tab-info")))
         (title (pel-string-with-major-mode "PEL setup for %s mode")))
    (pel-print-in-buffer
     "*pel-mode-info*"
     title
     (lambda ()
       "Print setup info for the major mode."
       (insert (propertize "* Major Mode Control:" 'face 'bold))
       (pel-insert-symbol-content 'major-mode nil :on-same-line :no-button
                                  "major mode currently used")
       (when pel-use-tree-sitter
         (insert (format "\n- %s" (pel-ts-language-grammar-status-for
                                   mode-base-symbol "\n- "))))
       ;; --
       (when (boundp pel-use-mode-user-option-symbol)
         (pel-insert-symbol-content-line
          pel-use-mode-user-option-symbol
          nil
          (when (fboundp major-mode-used-text-fct)
            major-mode-used-text-fct)))
       ;; --
       (insert "\n\n")
       (pel-insert-minor-mode-activation-info
        current-major-mode
        (when (fboundp minor-mode-info-inserter-fct)
          minor-mode-info-inserter-fct))
       (insert "\n\n")
       ;; --
       (if (fboundp indent-tab-info-inserter-fct)
           (progn
             (funcall indent-tab-info-inserter-fct)
             (pel-insert-tab-set-width-info))
         (pel-insert-tab-set-width-info mode-base-symbol)))
     (unless append :clear-buffer)
     :use-help-mode)))

;;; --------------------------------------------------------------------------
(provide 'pel-modes)

;;; pel-modes.el ends here

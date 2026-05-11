;;; pel-v.el --- Support for the V and Verilog languages  -*- lexical-binding: t; -*-

;; Created   : Saturday, May  9 2026.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2026-05-11 13:58:52 EDT, updated by Pierre Rouleau>

;; This file is part of the PEL package.
;; This file is not part of GNU Emacs.

;; Copyright (C) 2026  Pierre Rouleau
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
;; PEL extra support for the V programming language.
;; Supports both v-mode and vlang-mode external packages.
;;
;; Since both V and Verilog have files with the .v option, this also provides
;; the mode detector function `pel-v-or-verilog-mode'.


;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
(require 'pel--base)        ; use: `pel-treesit-language-available-p' etc.
(require 'pel--options)     ; use: `pel-use-v', `pel-v-tab-width',
;;                          ;      `pel-v-use-tabs', `pel-v-activates-minor-modes'
(require 'pel-indent)       ; use: `pel-indent-insert-control-info',
;;                          ;      `pel-indent-control-context'
;;                          ;      `pel-tab-insert-control-info',
;;                          ;      `pel-tab-control-context'
(require 'pel-modes)        ; use: `pel-insert-minor-mode-activation-info'
;;                          ;      `pel-active-minor-modes'
;;                          ;      `pel-insert-list-of-minor-modes'

;;; --------------------------------------------------------------------------
;;; Code:
;;

(declare-function verilog-mode "verilog-mode")
(declare-function verilog-ts-mode "verilog-ts-mode")
(declare-function v-mode       "v-mode")
(declare-function vlang-mode   "vlang-mode")

;;-pel-autoload
(defun pel-v-or-verilog-mode ()
  "Open buffer in V or Verilog specific mode.
If buffer is empty prompt user.
Otherwise check for a Verilog file variable or a endmodule statement
to identify a Verilog file.  Anything else is assumed being V."
  (let ((is-verilog
         (or
          (and
           (or (eq (buffer-size) 0)
               (not (search-forward-regexp "[^ \t\n\r]" nil 'noerror)))
           (y-or-n-p "Create a Verilog (y) or V (n) file?"))
          (save-excursion
            (goto-char (point-min))
            (search-forward-regexp "\\-\\*\\- [Mm]ode: [Vv]erilog[; ]"
                                   nil 'noerror))
          (save-excursion
            (goto-char (point-min))
            (search-forward-regexp "\\<endmodule\\>"
                                   nil 'noerror)))))
    (if is-verilog
        (if (and
             (pel-treesit-language-available-p 'verilog)
             (fboundp 'verilog-ts-mode))
            (verilog-ts-mode)
          (verilog-mode))
      (if (eq pel-use-v 'v-mode)
          (v-mode)
        (vlang-mode)))))


;;-pel-autoload
(defun pel-v-cleanup-auto-mode-alist ()
  "Remove invalid entries for V from auto-mode-alist."
  ;; Remove any other .v rules from the auto-mode-alist that
  ;; might have been added by the loading of the V language mode code.
  ;; remove what v-mode code adds (if present)
  (setq auto-mode-alist (delete '("\\(\\.v?v\\|\\.vsh\\)$" . v-mode)
                                auto-mode-alist))

  ;; remove what vlang-mode adds (if present)
  (setq auto-mode-alist (delete '("\\.v\\'" . vlang-or-verilog-mode)
                                auto-mode-alist)))


;;-pel-autoload
(defun pel-v-mode-used-text (use-v)
  "Description of what USE-V specifies for major mode.
USE-V should be set to `pel-use-v' value used in current buffer."
  (cond
   ((eq use-v 'v-mode)    "use v-mode from MELPA.")
   ((eq use-v 'vlang-mode) "use vlang-mode (experimental, font-lock only).")
   (t "Invalid! Use v-mode or vlang-mode")))

;;-pel-autoload
(defun pel-v-insert-indent-info ()
  "Insert V indentation setup info in current context.
Return a list of generic symbols described."
  (insert "
- Under PEL, V tab/indentation width is controlled by the value of the
  `pel-v-tab-width' user-option.  PEL stores its value inside `tab-width'
  when opening V buffers.")
  (pel-insert-symbol-content-line 'pel-v-tab-width)
  (pel-insert-symbol-content-line 'tab-width)
  ;; Return the list of generic symbols described here.
  '(indent-description-intro
    pel-MM-tab-width
    tab-width))

;;-pel-autoload
(defun pel-v-insert-tab-info ()
  "Insert V hard tab setup info in current context.
Return a list of generic symbols described."
  (insert "
- The hard tab rendering width for V buffers is controlled by
  `pel-v-tab-width' and stored into `tab-width'.
  These do not control the indentation, just the visual width (in columns)
  that Emacs uses to render a hard tab character.
")
  (pel-insert-symbol-content-line 'pel-v-tab-width)
  (pel-insert-symbol-content-line 'tab-width)
  (pel-insert-symbol-content-line 'pel-v-use-tabs
                                  nil #'pel-on-off-string)
  (pel-insert-symbol-content-line 'indent-tabs-mode
                                  nil #'pel-on-off-string)
  ;; Return the list of generic symbols described here.
  '(tab-description-intro
    pel-MM-tab-width
    tab-width
    pel-MM-use-tabs
    indent-tabs-mode))

(defun pel--v-minor-mode-info ()
  "Insert information related to V minor modes."
  (insert "
Automatic activation of minor mode is also controlled by the
following user-options:")
  (pel-insert-list-content 'pel-v-activates-minor-modes
                           nil nil nil :1line))

;;-pel-autoload
(defun pel-v-setup-info (&optional append)
  "Display V language setup information."
  (interactive "P")
  (pel-major-mode-must-be '(v-mode vlang-mode))
  (let ((pel-insert-symbol-content-context-buffer (current-buffer))
        (current-major-mode major-mode)
        (active-modes (pel-active-minor-modes))
        (indent-control-context (pel-indent-control-context))
        (tab-control-context (pel-tab-control-context)))
    (pel-print-in-buffer
     "*pel-v-info*"
     "PEL setup for V programming language"
     (lambda ()
       "Print V setup info."
       (pel-insert-bold "* Major Mode Control:")
       (pel-insert-symbol-content 'major-mode nil :on-same-line nil
                                  "major mode currently used")
       (pel-insert-symbol-content-line 'pel-use-v nil
                                       (function pel-v-mode-used-text))
       (insert "\n\n")
       ;; -- List of minor modes
       (pel-insert-list-of-minor-modes active-modes)
       (insert "\n\n")
       (pel-insert-minor-mode-activation-info current-major-mode
                                              #'pel--v-minor-mode-info)
       ;; --
       (insert "\n\n")
       (pel-indent-insert-control-info indent-control-context)
       (pel-tab-insert-control-info tab-control-context))
     (unless append :clear-buffer)
     :use-help-mode)))

;;; --------------------------------------------------------------------------
(provide 'pel-v)

;;; pel-v.el ends here

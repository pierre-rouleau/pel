;;; pel-smartparens.el --- PEL smartparens specialization.  -*- lexical-binding: t; -*-

;; Created   : Monday, September 20 2021.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2021-09-24 16:53:29, updated by Pierre Rouleau>

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
;; This provides specialized commands for moving across blocks with the
;; guarantee that point will not stop at symbols.  These allow controlling the
;; behaviour of other smartparens commands with `sp-navigate-consider-symbols'
;; while ensuring the behaviour of those commands is stable.

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'pel--base)                    ; use: pel-print-in-buffer,
;;                                      ;      pel-insert-symbol-content
;;; --------------------------------------------------------------------------
;;; Code:
;;

(defvar pel--sp-op-last nil
  "Remember last value of `sp-navigate-consider-symbols'.") ; prevents warnings
;;-pel-autoload
(defun pel-sp-next-sexp (&optional arg)
  "Same as `sp-next-sexp' with `sp-navigate-consider-symbols' forced nil."
  (interactive "^p")
  (when (and (require 'smartparens nil :no-error)
             (fboundp 'sp-next-sexp)
             (boundp 'sp-navigate-consider-symbols))
    (let ((sp-navigate-consider-symbols nil))
      (sp-next-sexp arg)
      (setq pel--sp-op-last sp-navigate-consider-symbols))))

;;-pel-autoload
(defun pel-sp-previous-sexp (&optional arg)
  "Same as `sp-previous-sexp' with `sp-navigate-consider-symbols' forced nil."
  (interactive "^p")
  (when (and (require 'smartparens nil :no-error)
             (fboundp 'sp-previous-sexp)
             (boundp 'sp-navigate-consider-symbols))
    (let ((sp-navigate-consider-symbols nil))
      (sp-previous-sexp arg)
      (setq pel--sp-op-last sp-navigate-consider-symbols))))

;; ---------------------------------------------------------------------------
;;-pel-autoload
(defun pel-smartparens-augment ()
  "Augment the functionality of smartparens commands.

Advice several commands to add ability to display the
string copied or killed."
  ;; The following 2 pel-ccp functions are autoloaded.
  ;; Declare them to prevent byte compiler warnings.
  (declare-function pel-show-copied "pel-ccp")
  (declare-function pel-show-killed "pel-ccp")
  (dolist (fct '(sp-copy-sexp
                 sp-backward-copy-sexp))
    (advice-add fct :after (function pel-show-copied)))

  ;; TODO: add sp-kill-whole-line once there is a way of identifying exactly
  ;;       what has been killed by the last command.
  (dolist (fct '(sp-change-inner
                 sp-change-enclosing
                 sp-kill-sexp
                 sp-backward-kill-sexp))
    (advice-add fct :after (function pel-show-killed))))


;; ---------------------------------------------------------------------------
;;-pel-autoload

(defun pel-smartparens-info ()
  "Print smartparens setup info in *pel-smartparens-info*."
  (interactive)
  (let* ((buffer (current-buffer))
         (show (lambda (symbols)
                 (dolist (symb (if (symbolp symbols)
                                   (list symbols)
                                 symbols))
                   (pel-insert-symbol-content symb buffer :on-same-line)))))


    (pel-print-in-buffer
     "*pel-smartparens-info*"
     "smartparens control variables"
     (lambda ()
       "Print smartparens variables."
       ;;
       (insert "➣ ")
       (pel-insert-url-link "Smartparens Automatic Escaping"
                            "\
https://smartparens.readthedocs.io/en/latest/automatic-escaping.html"
                            ":")
       (λc show '(sp-escape-wrapped-region
                  sp-escape-quotes-after-insert))
       ;;
       (insert "\n\n➣ ")
       (pel-insert-url-link "Smartparens pairs" "\
https://github.com/Fuco1/smartparens/wiki/Pair-management" ":")
       (λc show 'sp-max-pair-length)
       (pel-insert-list-content 'sp-pairs buffer)
       ))))

;;; --------------------------------------------------------------------------
(provide 'pel-smartparens)

;;; pel-smartparens.el ends here

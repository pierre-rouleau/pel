;;; pel-smartparens.el --- PEL smartparens specialization.  -*- lexical-binding: t; -*-

;; Created   : Monday, September 20 2021.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2021-09-29 22:48:12, updated by Pierre Rouleau>

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

(defvar pel--has-smartparens (require 'smartparens nil :noerror)
  "Non-nil when smartparens is available.")


(declare-function sp-local-pair                "smartparens")
(declare-function sp-next-sexp                 "smartparens")
(declare-function sp-previous-sexp             "smartparens")
(declare-function sp-delete-char               "smartparens")
(declare-function sp-backward-delete-char      "smartparens")
(declare-function sp-navigate-consider-symbols "smartparens")
(declare-function sp-in-code-p                 "smartparens")
(declare-function sp-get-enclosing-sexp        "smartparens")

;;; --------------------------------------------------------------------------
;;; Code:
;;

(defvar pel--sp-op-last nil
  "Remember last value of `sp-navigate-consider-symbols'.") ; prevents warnings
;;-pel-autoload
(defun pel-sp-next-sexp (&optional arg)
  "Same as `sp-next-sexp' with `sp-navigate-consider-symbols' forced nil."
  (interactive "^p")
  (when pel--has-smartparens
    (let ((sp-navigate-consider-symbols nil))
      (sp-next-sexp arg)
      (setq pel--sp-op-last sp-navigate-consider-symbols))))

;;-pel-autoload
(defun pel-sp-previous-sexp (&optional arg)
  "Same as `sp-previous-sexp' with `sp-navigate-consider-symbols' forced nil."
  (interactive "^p")
  (when pel--has-smartparens
    (let ((sp-navigate-consider-symbols nil))
      (sp-previous-sexp arg)
      (setq pel--sp-op-last sp-navigate-consider-symbols))))

;; ---------------------------------------------------------------------------
;; Better smartparens delete
;; -------------------------
;;
;; Both smartparens sp-delete-char and sp-backward-delete-char do not delete
;; a marked area.  These function do.

;;-pel-autoload
(defun pel-sp-delete-char (&optional arg)
  "Execute `sp-delete-char' if no area marked, otherwise delete marked area."
  (interactive "P*")
  (if (use-region-p)
      (delete-char 1)
    (sp-delete-char arg)))

;;-pel-autoload
(defun pel-sp-backward-delete-char (&optional arg)
  "Execute `sp-delete-char' if no area marked, otherwise delete marked area."
  (interactive "P*")
  (if (use-region-p)
      (backward-delete-char-untabify 1)
    (sp-backward-delete-char arg)))


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
       (λc show '(sp-autodelete-opening-pair
                  sp-autodelete-pair
                  sp-autodelete-wrap
                  sp-autoinsert-pair
                  sp-autoinsert-quote-if-followed-by-closing-pair
                  sp-autoskip-closing-pair
                  sp-autoskip-opening-pair
                  sp-autowrap-region
                  sp-cancel-autoskip-on-backward-movement
                  sp-echo-match-when-invisible
                  sp-escape-quotes-after-insert
                  sp-escape-wrapped-region
                  sp-highlight-wrap-overlay
                  sp-hybrid-kill-entire-symbol
                  sp-hybrid-kill-excessive-whitespace
                  sp-max-pair-length
                  sp-message-width
                  sp-navigate-close-if-unbalanced
                  sp-navigate-comments-as-sexps
                  sp-navigate-consider-sgml-tags
                  sp-navigate-consider-symbols
                  sp-navigate-interactive-always-progress-point
                  ;; sp-navigate-reindent-after-up
                  sp-navigate-reindent-after-up-in-string
                  ;; sp-navigate-skip-match
                  sp-navigate-use-textmode-stringlike-parser
                  sp-no-reindent-after-kill-indent-line-functions
                  ;; sp-no-reindent-after-kill-modes
                  sp-split-sexp-always-split-as-string
                  sp-successive-kill-preserve-whitespace
                  sp-undo-pairs-separately
                  sp-use-subword
                  sp-wrap-entire-symbol
                  sp-wrap-from-point
                  sp-wrap-repeat-last
                  sp-wrap-respect-direction
                  sp-wrap-show-possible-pairs
                  ))
                  ;; sp-ignore-modes-list
                  ;; sp-c-modes
                  ;; sp-clojure-modes
                  ;; sp-lisp-modes
       (pel-insert-list-content 'sp-pairs buffer)
       (pel-insert-list-content 'sp-comment-string buffer)
       (pel-insert-list-content 'sp-coverride-key-bindings buffer)
       (pel-insert-list-content 'sp-sexp-prefix buffer)
       (pel-insert-list-content 'sp-sexp-suffix buffer) ; ?? for erlang?
       ))))

;; ---------------------------------------------------------------------------
;; Erlang Support for SmartParens
;; ------------------------------
;;
;; TODO:
;; Once complete the following code will probably be migrated into
;; smartparens itself.

;;-pel-autoload
(defun pel-sp-erlang-handler (id action context)
  "Display handler info message for ID, ACTION and CONTEXT."
  (message "pel-sp-erlang-handler: %S %S %S" id action context))


;;-pel-autoload
(defun pel-smartparens-setup-erlang ()
  "Configure smartparens for Erlang.

This must be called within the scope of a erlang-mode buffer."
  (sp-local-pair 'erlang-mode "<<" ">>")

  ;; TODO: fix behaviour of smartparens commands that don't work properly for Erlang.
  ;; (sp-local-pair 'erlang-mode "[" "]"
  ;;                :actions '(insert wrap autoskip navigate)
  ;;                :post-handlers '(pel-sp-erlang-handler))
  )

;;; --------------------------------------------------------------------------
(provide 'pel-smartparens)

;;; pel-smartparens.el ends here

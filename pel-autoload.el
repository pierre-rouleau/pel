;;; pel-autoload.el --- Autoloads PEL -*-lexical-binding: t-*-

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
;; This contains only one function, `pel--autoload-init' which must be called
;; once when the PEL system is initialized.  It sets up the autoloading of all
;; of the PEL functions.  The PEL source code has only *one* function tagged
;; with the official Emacs Lisp autoload cookie: `pel-init'.  All the other
;; PEL functions that can be autoloaded use another form of tag comment used
;; to identify the PEL functions that must be autoloaded once PEL is
;; initialized.  This way the PEL system is autoloaded in 2 steps: first
;; `pel-init' is autoloaded to provide the ability to start it and then
;; `pel-init' calls `pel--autoload-init' to autoload the rest of the PEL
;; system.  Once that is done, the 'pel-autoload feature can be unloaded
;; since it does not contain anything that will be useful, by evaluating the
;; following form:
;;             (unload-feature 'pel-autoload)

;;; Code:

(defun pel--autoload-init ()
  "Intialize the PEL system -- prepare automatic loading of all function."

  (require 'pel--options)

  ;; pel-applescript
  (when (eq system-type 'darwin)
    (dolist (fct '(pel-pel-say-word
                   pel-say-sentence
                   pel-say-paragraph
                   pel-say-region
                   pel-say
                   pel-say-words
                   pel-run-applescript))
    (autoload fct "pel-applescript")))


  ;; pel-autocomplete
  (autoload 'pel-completion-help             "pel-autocomplete")
  (autoload 'pel-complete                    "pel-autocomplete")
  (when (and (boundp 'pel-use-auto-complete) pel-use-auto-complete)
    (autoload 'pel-global-auto-complete-mode "pel-autocomplete")
    (autoload 'pel-auto-complete-mode        "pel-autocomplete"))
  (when (and (boundp 'pel-use-company) pel-use-company)
    (autoload 'pel-global-company-mode       "pel-autocomplete")
    (autoload 'pel-company-mode              "pel-autocomplete"))

  ;; pel-bookmark
  (autoload 'pel-bookmark-in-current-file-p "pel-bookmark")

  ;; pel-ccp
  (autoload 'pel-copy-word-at-point                   "pel-ccp")
  (autoload 'pel-copy-symbol-at-point                 "pel-ccp")
  (autoload 'pel-copy-sentence-at-point               "pel-ccp")
  (autoload 'pel-copy-function-at-point               "pel-ccp")
  (autoload 'pel-copy-sexp-at-point                   "pel-ccp")
  (autoload 'pel-copy-whitespace-at-point             "pel-ccp")
  (autoload 'pel-copy-filename-at-point               "pel-ccp")
  (autoload 'pel-copy-url-at-point                    "pel-ccp")
  (autoload 'pel-copy-list-at-point                   "pel-ccp")
  (autoload 'pel-copy-paragraph-at-point              "pel-ccp")
  (autoload 'pel-copy-paragraph-start                 "pel-ccp")
  (autoload 'pel-copy-paragraph-end                   "pel-ccp")
  (autoload 'pel-copy-line-start                      "pel-ccp")
  (autoload 'pel-copy-line-end                        "pel-ccp")
  (autoload 'pel-copy-char-at-point                   "pel-ccp")
  (autoload 'pel-kill-word-at-point                   "pel-ccp")
  (autoload 'pel-kill-symbol-at-point                 "pel-ccp")
  (autoload 'pel-kill-sentence-at-point               "pel-ccp")
  (autoload 'pel-kill-function-at-point               "pel-ccp")
  (autoload 'pel-kill-sexp-at-point                   "pel-ccp")
  (autoload 'pel-kill-whitespace-at-point             "pel-ccp")
  (autoload 'pel-kill-filename-at-point               "pel-ccp")
  (autoload 'pel-kill-url-at-point                    "pel-ccp")
  (autoload 'pel-kill-list-at-point                   "pel-ccp")
  (autoload 'pel-kill-paragraph-at-point              "pel-ccp")
  (autoload 'pel-kill-char-at-point                   "pel-ccp")
  (autoload 'pel-delete-whole-line                    "pel-ccp")
  (autoload 'pel-kill-or-delete-marked-or-whole-line  "pel-ccp")
  (autoload 'pel-mark-whole-line                      "pel-ccp")
  (autoload 'pel-copy-marked-or-whole-line            "pel-ccp")
  (autoload 'pel-kill-from-beginning-of-line          "pel-ccp")
  (autoload 'pel-delete-to-next-visible               "pel-ccp")

  ;; pel-comment
  (autoload 'pel-comment-start                           "pel-comment")
  (autoload 'pel-comment-middle                          "pel-comment")
  (autoload 'pel-comment-end                             "pel-comment")
  (autoload 'pel-toggle-comment-auto-fill-only-comments  "pel-comment")
  (autoload 'pel-delete-all-comments                     "pel-comment")
  (autoload 'pel-kill-all-comments                       "pel-comment")

  ;; pel-comment-adorn
  ;; Nothing specified here: the control is inside pel_keys.el
  ;; only.

  ;; pel-commonlisp
  (autoload 'pel-cl-init "pel-commonlisp")

  ;; pel-cua
  (autoload 'pel-cua-rectangle-mark        "pel-cua")
  (autoload 'pel-cua-move-rectangle-left   "pel-cua")
  (autoload 'pel-cua-move-rectangle-right  "pel-cua")

  ;; pel-file
  (autoload 'pel-find-file-at-point-in-window  "pel-file")
  (autoload 'pel-show-filename-at-point        "pel-file")

  ;; pel-fill
  (autoload 'pel-auto-fill-only-comments  "pel-fill")
  (autoload 'pel-show-fill-columns        "pel-fill")

  ;; pel-font loading is directly controlled by pel-init
  ;; by the logic inside pel_keys.el

  ;; pel-frame-control
  (autoload 'pel-toggle-frame-fullscreen  "pel-frame-control")
  (autoload 'pel-show-frame-count         "pel-frame-control")
  (autoload 'pel-next-frame               "pel-frame-control")
  (autoload 'pel-previous-frame           "pel-frame-control")

  ;; pel-hideshow.el
  (dolist (fct '(pel-toggle-hide-all
                 pel-toggle-hide-block
                 pel-hide-block
                 pel-show-block
                 pel-hide-all
                 pel-show-all
                 pel-hide-level-1
                 pel-hide-level-2
                 pel-hide-level-3
                 pel-hide-level-4))
    (autoload fct "pel-hideshow"))

  ;; pel-highlight
  (autoload 'pel-set-highlight-color              "pel-highlight")
  (autoload 'pel-toggle-hl-line-sticky            "pel-highlight")
  (autoload 'pel-toggle-show-trailing-whitespace  "pel-highlight")
  (autoload 'pel-toggle-indicate-empty-lines      "pel-highlight")
  (autoload 'pel-toggle-indent-tabs-mode          "pel-highlight")

  ;; pel-imenu
  (autoload 'pel-toggle-imenu-index-follows-order  "pel-imenu")
  (autoload 'pel-imenu-init                        "pel-imenu")

  ;; pel-indent
  (autoload 'pel-insert-c-indent  "pel-indent")
  (autoload 'pel-unindent         "pel-indent")
  (autoload 'pel-indent-rigidly   "pel-indent")

  ;; pel-kbmacros
  (autoload 'pel-kmacro-start-macro-or-insert-counter "pel-kbmacros")
  (autoload 'pel-forget-recorded-keyboard-macro       "pel-kbmacros")

  ;; pel-line-control
  (autoload 'pel-lc-previous-logical-line  "pel-line-control")
  (autoload 'pel-lc-next-logical-line      "pel-line-control")
  (autoload 'pel-toggle-line-col-modes     "pel-line-control")

  ;; pel-lisp
  (autoload 'pel-toggle-lisp-modes           "pel-lisp")
  (autoload 'pel-byte-compile-file-and-load  "pel-lisp")
  (autoload 'pel-lint-elisp-file             "pel-lisp")

  ;; pel-mark
  (autoload 'pel-mark-ring-stats                      "pel-mark")
  (autoload 'pel-popoff-mark-ring                     "pel-mark")
  (autoload 'pel-mark-line-up                         "pel-mark")
  (autoload 'pel-mark-line-down                       "pel-mark")
  (autoload 'pel-push-mark-no-activate                "pel-mark")
  (autoload 'pel-jump-to-mark                         "pel-mark")
  (autoload 'pel-exchange-point-and-mark-no-activate  "pel-mark")

  ;; pel-navigate
  (dolist (fct '(pel-beginning-of-line
                 pel-end-of-line
                 pel-newline-and-indent-below
                 pel-find-thing-at-point
                 pel-show-char-syntax
                 pel-forward-token-start
                 pel-backward-token-start
                 pel-forward-word-start
                 pel-forward-wspace-start
                 pel-backward-wspace-start
                 pel-forward-syntaxchange-start
                 pel-backward-syntaxchange-start
                 pel-next-visible
                 pel-previous-visible
                 pel-home
                 pel-end
                 pel-beginning-of-next-defun
                 pel-end-of-previous-defun))
    (autoload fct "pel-navigate"))

  ;; pel-numkpad
  (autoload 'pel-toggle-mac-numlock  "pel-numkpad")
  (autoload 'pel-show-mac-numlock    "pel-numkpad")
  (autoload 'pel-0                   "pel-numkpad")
  (autoload 'pel-1                   "pel-numkpad")
  (autoload 'pel-2                   "pel-numkpad")
  (autoload 'pel-3                   "pel-numkpad")
  (autoload 'pel-4                   "pel-numkpad")
  (autoload 'pel-5                   "pel-numkpad")
  (autoload 'pel-6                   "pel-numkpad")
  (autoload 'pel-7                   "pel-numkpad")
  (autoload 'pel-8                   "pel-numkpad")
  (autoload 'pel-9                   "pel-numkpad")
  (autoload 'pel-kp-decimal          "pel-numkpad")
  (autoload 'pel-kp-subtract         "pel-numkpad")
  (autoload 'pel-kp-add              "pel-numkpad")

  ;; pel-prompt
  (autoload 'pel-y-n-e-or-l-p "pel-prompt")

  ;; pel-register
  (autoload 'pel-filename-to-register              "pel-register")
  (autoload 'pel-point-to-register                 "pel-register")
  (autoload 'pel-copy-to-register                  "pel-register")
  (autoload 'pel-copy-rectangle-to-register        "pel-register")
  (autoload 'pel-window-configuration-to-register  "pel-register")
  (autoload 'pel-frameset-to-register              "pel-register")
  (autoload 'pel-number-to-register                "pel-register")
  (autoload 'pel-kmacro-to-register                "pel-register")

    ;; pel-read
  (dolist (fct '(pel-word-at-point
                 pel-sentence-at-point
                 pel-paragraph-at-point))
    (autoload fct "pel-read"))

  ;; pel-rst
  (when (and (boundp 'pel-use-rst-mode) pel-use-rst-mode)
    (dolist (fct '(pel-rst-set-ref-bookmark
                   pel-rst-goto-ref-bookmark
                   pel-rst-makelink
                   pel-rst-set-adornment
                   pel-rst-adorn-default
                   pel-rst-adorn-Sphinx-Python
                   pel-rst-adorn-CRiSPer
                   pel-rst-adorn-title
                   pel-rst-adorn-1
                   pel-rst-adorn-2
                   pel-rst-adorn-3
                   pel-rst-adorn-4
                   pel-rst-adorn-5
                   pel-rst-adorn-6
                   pel-rst-adorn-7
                   pel-rst-adorn-8
                   pel-rst-adorn-9
                   pel-rst-adorn-10
                   pel-rst-adorn-refresh
                   pel-rst-adorn-same-level
                   pel-rst-adorn-increase-level
                   pel-rst-adorn-decrease-level
                   pel-rst-bold
                   pel-rst-italic
                   pel-rst-literal
                   pel-rst-interpreted))
      (autoload fct "pel-rst")))

  ;; pel-scroll
  (dolist (fct '(pel-toggle-scroll-sync
                 pel-add-window-to-scroll-sync
                 pel-remove-window-from-scroll-sync
                 pel-scroll-up
                 pel-scroll-down
                 pel-scroll-down-other
                 pel-scroll-up-other))
    (autoload fct "pel-scroll"))

  ;; pel-search
  (autoload 'pel-toggle-case-fold-search   "pel-search")
  (autoload 'pel-toggle-search-upper-case  "pel-search")
  (autoload 'pel-show-search-case-state    "pel-search")

  ;; pel-speedbar
  (when (and (boundp 'pel-use-speedbar) pel-use-speedbar)
    (autoload 'pel-open-close-speedbar             "pel-speedbar")
    (autoload 'pel-toggle-to-speedbar              "pel-speedbar")
    (autoload 'pel-speedbar-toggle-refresh         "pel-speedbar")
    (autoload 'pel-speedbar-refresh                "pel-speedbar")
    (autoload 'pel-speedbar-toggle-show-all-files  "pel-speedbar")
    (autoload 'pel-speedbar-toggle-sorting         "pel-speedbar")
    (when (display-graphic-p)
      (autoload 'pel-speedbar-toggle-images        "pel-speedbar")))

  ;; pel-spell
  (autoload 'pel-spell-init      "pel-spell")
  (autoload 'pel-spell-show-use  "pel-spell")

  ;; pel-text-insert
  (autoload 'pel-insert-line               "pel-text-insert")
  (autoload 'pel-insert-filename           "pel-text-insert")
  (autoload 'pel-insert-current-date-time  "pel-text-insert")
  (autoload 'pel-insert-current-date       "pel-text-insert")
  (autoload 'pel-insert-iso8601-timestamp  "pel-text-insert")

  ;; pel-text-transform
  (autoload 'pel-capitalize-word-or-region  "pel-text-transform")
  (autoload 'pel-upcase-word-or-region      "pel-text-transform")
  (autoload 'pel-downcase-word-or-region    "pel-text-transform")
  (autoload 'pel-toggle-sentence-end        "pel-text-transform")
  (autoload 'pel-show-text-modes            "pel-text-transform")

  ;; pel-undo loading is directly controlled by pel-init
  ;; by the logic inside pel_keys.el

  ;; pel-window
  (dolist (fct '(pel-show-window-previous-buffer
                 pel-switch-to-last-used-buffer
                 pel-show-window-dedicated-status
                 pel-toggle-window-dedicated
                 pel-create-window-down
                 pel-create-window-right
                 pel-create-window-up
                 pel-create-window-left
                 pel-close-window-down
                 pel-close-window-up
                 pel-close-window-left
                 pel-close-window-right
                 pel-split-window-sensibly
                 pel-2-vertical-windows
                 pel-2-horizontal-windows
                 pel-window-valid-for-editing-p
                 pel-window-select
                 pel-other-window-backward
                 pel-show-window-filename-or-buffer-name
                 pel-show-window-sizes
                 pel-other-window))
    (autoload fct "pel-window")))

;; -----------------------------------------------------------------------------
(provide 'pel-autoload)

;;; pel-autoload.el ends here

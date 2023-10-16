;;; pel-autoload.el --- Autoloads PEL -*-lexical-binding: t-*-

;; Copyright (C) 2020, 2021, 2022, 2023  Pierre Rouleau

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

;; When several functions need to be autoloaded, the macro `pel-autoload' is
;; used.  It is passed the name of the file followed by all function name
;; symbols (unquoted - the macro quotes them).  The macro provides a shorter
;; form of writing the expressions.  It also provides a uniform way of writing
;; the expressions to autoload one function or multiple functions from a file.
;; This simplifies maintenance.


;;; --------------------------------------------------------------------------
;;; Code:

;; TODO: find a way to create a macro that generates the autoload for
;; interactive or non-interactive functions by checking what the symbol refers to.

(defmacro pel-autoload-function (fname for: &rest functions)
  "Schedule the autoloading of FNAME for specified FUNCTIONS.
Argument FOR: just a required separator keyword to make code look better."
  (declare (indent 2))
  (ignore for:)
  (if (> (length functions) 1)
      `(dolist (fct (quote (,@functions)))
         (autoload fct ,fname))
    `(autoload (quote ,@functions) ,fname)))

(defmacro pel-autoload (fname for: &rest functions)
  "Schedule the autoloading of FNAME for specified interactive FUNCTIONS.
Argument FOR: just a required separator keyword to make code look better."
  (declare (indent 2))
  (ignore for:)
  (if (> (length functions) 1)
      `(dolist (fct (quote (,@functions)))
         (autoload fct ,fname nil :interactive))
    `(autoload (quote ,@functions) ,fname nil :interactive)))

;; ---------------------------------------------------------------------------

(defun pel--autoload-init ()
  "Intialize the PEL system -- prepare automatic loading of all function."

  (require 'pel--options)

  (pel-autoload "pel-abbrev" for:
    pel-ispell-word-then-abbrev
    pel-abbrev-info)

  (pel-autoload "pel-align" for:
    pel-newline-and-indent-below
    pel-align-info
    pel-toggle-newline-indent-align
    pel-multi-align-regexp)

  (when (eq system-type 'darwin)
    (pel-autoload "pel-applescript" for:
      pel-pel-say-word
      pel-say-sentence
      pel-say-paragraph
      pel-say-region
      pel-say
      pel-say-words)
    (pel-autoload-function "pel-applescript" for:
      pel-run-applescript))


  (pel-autoload "pel-autocomplete" for:
    pel-completion-info
    pel-complete)
  (when (and (boundp 'pel-use-auto-complete) pel-use-auto-complete)
    (pel-autoload "pel-autocomplete" for:
      pel-global-auto-complete-mode
      pel-auto-complete-mode))
  (when (and (boundp 'pel-use-company) pel-use-company)
    (pel-autoload "pel-autocomplete" for:
      pel-global-company-mode
      pel-company-mode))

  (pel-autoload "pel-benchmark" for:
    pel-show-init-time
    pel-log-init-time)

  (pel-autoload "pel-bookmark" for: pel-bookmark-info)
  (pel-autoload-function "pel-bookmark" for: pel-bookmark-in-current-file-p)

  (pel-autoload "pel-browse" for: pel-browse-url)

  (pel-autoload "pel-buffer" for:
    pel-bs-next
    pel-bs-previous
    pel-bs-init
    pel-smb-next
    pel-smb-previous
    pel-show-all-buffers)

  (pel-autoload "pel-c-utils" for:
    pel-c-search-equal-NULL
    pel-c-search-not-equal-NULL
    pel-c-search-equal-true
    pel-c-search-not-equal-true
    pel-c-search-equal-false
    pel-c-search-not-equal-false
    pel-c-search-any-comparison-problem
    pel-c-fix-comparison-problems
    pel-c-search-preproc-if
    pel-c-search-preproc-if-set
    pel-c-fix-preproc-if-problems)

  (pel-autoload-function "pel-cc-find" for:
    pel-cc-find-via-pel-ini
    pel-cc-find-activate-finder-method)
  (pel-autoload "pel-cc-find" for:
    pel-cc-set-file-finder-ini-tool-name)

  (pel-autoload "pel-cc" for
    pel-cc-newline
    pel-cc-mode-info
    pel-cc-set-indent-width)

  (pel-autoload-function "pel-ccp" for:
    pel-show-copied
    pel-show-killed)

  (pel-autoload "pel-ccp" for:
    pel-toggle-show-copy-cut-text
    pel-copy-word-at-point
    pel-copy-symbol-at-point
    pel-copy-sentence-at-point
    pel-copy-function-at-point
    pel-copy-sexp-at-point
    pel-copy-whitespace-at-point
    pel-copy-filename-at-point
    pel-copy-url-at-point
    pel-copy-list-at-point
    pel-copy-paragraph-at-point
    pel-copy-paragraph-start
    pel-copy-paragraph-end
    pel-copy-line-start
    pel-copy-line-end
    pel-copy-char-at-point
    pel-kill-word-at-point
    pel-kill-word-part
    pel-kill-symbol-at-point
    pel-kill-symbol-part
    pel-kill-sentence-at-point
    pel-kill-function-at-point
    pel-kill-sexp-at-point
    pel-kill-whitespace-at-point
    pel-kill-filename-at-point
    pel-kill-url-at-point
    pel-kill-list-at-point
    pel-kill-paragraph-at-point
    pel-kill-char-at-point
    pel-delete-all-empty-lines
    pel-delete-word-at-point
    pel-delete-word-part
    pel-delete-symbol-at-point
    pel-delete-symbol-part
    pel-delete-sentence-at-point
    pel-delete-function-at-point
    pel-delete-sexp-at-point
    pel-delete-whitespace-at-point
    pel-delete-filename-at-point
    pel-delete-url-at-point
    pel-delete-list-at-point
    pel-delete-line
    pel-delete-paragraph-at-point
    pel-backward-delete-paragraph
    pel-delete-paragraph
    pel-delete-sexp
    pel-backward-delete-sexp
    pel-delete-rectangle
    pel-delete-whole-line
    pel-kill-or-delete-marked-or-whole-line
    ;; pel-replace-with-kill  ;; future
    pel-mark-whole-line
    pel-copy-marked-or-whole-line
    pel-kill-from-beginning-of-line
    pel-delete-from-beginning-of-line
    pel-delete-to-next-visible
    pel-kill-word-and-whitespace
    pel-delete-to-eol
    pel-duplicate-line)

  (pel-autoload "pel-comment" for:
    pel-comment-start
    pel-comment-middle
    pel-comment-end
    pel-toggle-comment-auto-fill-only-comments
    pel-delete-all-comments
    pel-kill-all-comments
    pel-comment-show-variables)

  (pel-autoload "pel-c-comment" for:
    pel-c-comment-dwim)

  (pel-autoload "pel-c-preproc" for:
    pel-c-preproc-forward-conditional
    pel-c-preproc-backward-conditional
    pel-c-preproc-outward-forward-conditional
    pel-c-preproc-outward-backward-conditional
    pel-c-preproc-conditionals-occur
    pel-c-preproc-conditionals-multi-occur)

  (pel-autoload "pel-comint" for:
    pel-comint-clear-buffer
    pel-comint-clear-buffer-and-get-prompt)

  ;; pel-comment-adorn
  ;; Nothing specified here: the control is inside pel_keys.el
  ;; only.

  (pel-autoload "pel-commonlisp" for:
    pel-cl-repl
    pel-cl-hyperspec-lookup
    pel-cl-add-symbol-to-imenu)

  (pel-autoload "pel-completion" for:
    pel-select-completion-mode
    pel-show-active-completion-mode
    pel-ido-mode
    pel-set-ido-ubiquitous
    pel-ido-ubiquitous
    pel-flx-ido
    pel-select-ido-geometry)
  (pel-autoload-function "pel-completion" for:
    pel-set-completion-mode)

  (pel-autoload "pel-cua" for:
    pel-cua-rectangle-mark
    pel-cua-move-rectangle-left
    pel-cua-move-rectangle-right)

  ;; pel-cursor: loaded by pel-init. No need for autoloading.

  (pel-autoload "pel-custom" for:
    pel-browse-pel
    pel-browse-group
    pel-customize-pel-base-emacs-group)

  (pel-autoload "pel-diff" for:
    pel-ediff-2files
    pel-ediff-revision
    pel-diff-show-status
    pel-diff-hunk-files-occur
    pel-diff-ignore-whitespace-in-hunks)

  (pel-autoload "pel-elisp" for:
    pel-toggle-paren-in-column-0-is-defun-start
    pel-elisp-set-navigate-target-form
    pel-elisp-beginning-of-next-form
    pel-elisp-beginning-of-next-defun
    pel-elisp-beginning-of-previous-form
    pel-elisp-beginning-of-previous-defun
    pel-elisp-to-name-of-next-form
    pel-elisp-to-name-of-previous-form
    pel-elisp-to-name-of-next-defun
    pel-elisp-to-name-of-previous-defun)

  (pel-autoload "pel-elisp-analyze" for:
    pel-run-ert)

  (pel-autoload "pel-emacs" for:
    pel-emacs-executable
    pel-open-emacs-refcard
    pel-emacs-load-stats
    pel-emacs-mem-stats
    pel-emacs-command-stats)

  (pel-autoload "pel-erlang" for:
    pel-erlang-comma
    pel-erlang-gt
    pel-erlang-newline
    pel-erlang-semicolon
    pel-erlang-period
    pel-erlang-toggle-space-after-comma
    pel-end-of-previous-clause
    pel-beginning-of-next-clause
    pel-previous-erl-function
    pel-next-erl-function
    pel-erlang-backward-sexp
    pel-erlang-forward-sexp
    pel-show-erlang-version
    pel-erlang-toggle-syntax-checker
    pel-erlang-comment-dwim
    pel-erlang-show-xref
    pel-erlang-select-xref
    pel-erlang-find-definitions
    pel-erlang-unwind)
  (pel-autoload-function "pel-erlang" for:
    pel-erlang-root-path
    pel-erlang-version
    pel-erlang-set-dirpath
    pel-erlang-man-parent-rootdir
    pel-erlang-exec-path
    pel-erlang-shell-mode-init
    pel-erlang-setup-electric-key-behaviour
    pel-erlang-setup-erlang-man-dir-root
    pel-erlang-find-file)

  (pel-autoload-function "pel-skels-erlang" for:
    pel--erlang-mode-setup
    pel--install-erlang-skel)

  (pel-autoload "pel-face-ut" for: pel-show-face-at-point)

  (pel-autoload-function "pel-ffind" for:
    pel-ffind
    pel-generic-find-file)

  (pel-autoload "pel-file" for:
    pel-show-filename-at-point
    pel-show-filename-parts-at-point
    pel-find-file-at-point-in-window
    pel-load-visited-file
    pel-open-file-in-other-dir
    pel-open-file-alternate)

  (pel-autoload "pel-filex" for:
    pel-open-in-os-app
    pel-open-buffer-file-in-os-app)

  (pel-autoload "pel-fill" for:
    pel-auto-fill-only-comments
    pel-show-fill-columns)

  ;; pel-font loading is directly controlled by pel-init
  ;; by the logic inside pel_keys.el

  (pel-autoload "pel-frame-control" for:
    pel-toggle-frame-fullscreen
    pel-show-frame-count
    pel-next-frame
    pel-previous-frame)

  (pel-autoload "pel-goto-addr" for:
    pel-goto-next-url
    pel-goto-previous-url)

  (pel-autoload "pel-help" for:
    pel-show-kill-ring
    pel-show-major-mode)

  (pel-autoload-function "pel-hex" for:
    pel-bibyte)

  (pel-autoload "pel-ido" for:
    pel-set-ido-use-fname-at-point)

  (pel-autoload "pel-imenu" for:
    pel-imenu-toggle-follows-order
    pel-imenu-toggle-auto-rescan
    pel-imenu-init)

  (pel-autoload "pel-imenu-dbg" for:
    pel-imenu-print-vars)

  (pel-autoload "pel-imenu-ido" for:
    pel-select-goto-symbol-UI
    pel-goto-symbol
    pel-select-goto-symbol-any-buffer-UI
    pel-goto-symbol-any-buffer
    pel-imenu-toggle-popup
    pel-imenu-toggle-flatten
    pel-show-goto-symbol-settings)

  (pel-autoload "pel-itemize" for:
    pel-itemize-lines)

  (pel-autoload "pel-lsp" for:
    pel-toggle-lsp-log-io
    pel-toggle-lsp-ui-sideline
    pel-toggle-lsp-ui-doc)

  (pel-autoload "pel-key-chord" for:
    pel-key-chord-describe)

  (pel-autoload "pel--keys-macros" for:
    pel-help-pdf
    pel-help-pdf-select
    pel-customize-pel
    pel-customize-library
    pel-help-pdfs-dir
    pel-help-on-completion-input
    pel-help-on-outline)

  (pel-autoload "pel-hideshow" for:
    pel-show-hide-state
    pel-toggle-hide-all
    pel-toggle-hide-block
    pel-hide-block
    pel-show-block
    pel-hide-all
    pel-show-all
    pel-hide-level-1
    pel-hide-level-2
    pel-hide-level-3
    pel-hide-level-4
    pel-hs-hide-block-below-inc
    pel-hs-hide-block-below-dec
    pel-selective-display-column-inc
    pel-selective-display-column-dec
    pel-selective-display-indent-inc
    pel-selective-display-indent-dec
    pel-toggle-hide-indent)

  (pel-autoload "pel-hide-docstring" for:
    pel-hide/show-docstring
    pel-toggle-docstring
    pel-hide/show-all-docstrings
    pel-toggle-all-docstrings)

  (pel-autoload "pel-go" for:
    pel-go-toggle-gofmt-on-buffer-save
    pel-go-setup-info
    pel-go-toggle-syntax-checker)
  (pel-autoload-function "pel-go" for:
    pel-go-gofmt-on-buffer-save)

  (pel-autoload "pel-graphviz-dot" for: pel-render-commented-graphviz-dot)

  (pel-autoload "pel-highlight" for:
    pel-set-highlight-color
    pel-customize-highlight
    pel-toggle-hl-line-sticky
    pel-toggle-show-trailing-whitespace
    pel-toggle-indicate-empty-lines
    pel-toggle-indent-tabs-mode
    pel-highlight-line
    pel-remove-line-highlight)

  (pel-autoload "pel-imenu" for:
    pel-imenu-rescan)

  (pel-autoload "pel-indent" for:
    pel-indent-lines
    pel-unindent-lines
    pel-indent-rigidly)

  (pel-autoload "pel-kbmacros" for:
    pel-kmacro-start-macro-or-insert-counter
    pel-forget-recorded-keyboard-macro
    pel-kmacro-ring-show-status)

  (pel-autoload "pel-lfe" for:
    pel-lfe-eval-buffer)

  (pel-autoload "pel-line-control" for:
    pel-lc-previous-logical-line
    pel-lc-next-logical-line
    pel-toggle-line-col-modes
    pel-insert-line-above)

  (pel-autoload "pel-lisp" for:
    pel-toggle-lisp-modes
    pel-byte-compile-file-and-load
    pel-lint-elisp-file)

  (pel-autoload "pel-make" for:
    pel-make-forward-conditional
    pel-make-backward-conditional
    pel-make-outward-forward-conditional
    pel-make-outward-backward-conditional
    pel-make-next-macro
    pel-make-previous-macro
    makefile-nmake-mode
    pel-make-conditionals-occur)

  (pel-autoload "pel-man" for:
    pel-man-at-point)
  (pel-autoload "pel-mark" for:
    pel-mark-ring-stats
    pel-popoff-mark-ring
    pel-mark-line-up
    pel-mark-line-down
    pel-push-mark-no-activate
    pel-jump-to-mark
    pel-exchange-point-and-mark-no-activate)

  (pel-autoload "pel-navigate" for:
    pel-beginning-of-line
    pel-end-of-line
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
    pel-end-of-previous-defun)

  (pel-autoload "pel-numkpad" for:
    pel-toggle-mac-numlock
    pel-show-mac-numlock
    pel-0
    pel-1
    pel-2
    pel-3
    pel-4
    pel-5
    pel-6
    pel-7
    pel-8
    pel-9
    pel-kp-decimal
    pel-kp-subtract
    pel-kp-add)

  (pel-autoload "pel-open" for:
    pel-open-at-point
    pel-set-open-at-point-dir
    pel-browse-filename-at-point
    pel-open-url-at-point
    pel-show-filemng-status)

  (pel-autoload "pel-outline" for:
    pel-outline-print-vars)

  (pel-autoload "pel-package" for:
    pel-package-info
    pel-package-info-all
    pel-cleanup)
  (pel-autoload-function "pel-package" for:
    pel-install-from-elpa-attic)

  (pel-autoload "pel-pathmng" for: pel-emacs-load-path)

  (pel-autoload "pel-plantuml" for: pel-render-commented-plantuml)

  (pel-autoload "pel-pp" for:
    pel-pp-next-directive
    pel-pp-prev-directive
    pel-pp-show-state)

  (pel-autoload-function "pel-prompt" for:
    pel-y-n-e-or-l-p
    pel-select-from)

  (pel-autoload-function "pel-psw" for:
    pel-psw-navigate-files)

  (pel-autoload "pel-register" for:
    pel-filename-to-register
    pel-point-to-register
    pel-copy-to-register
    pel-copy-rectangle-to-register
    pel-window-configuration-to-register
    pel-frameset-to-register
    pel-number-to-register
    pel-kmacro-to-register)

  (pel-autoload-function "pel-read" for:
    pel-word-at-point
    pel-sentence-at-point
    pel-paragraph-at-point)

  (pel-autoload "pel-regexp" for: pel-insert-regexp)

  (when (and (boundp 'pel-use-rst-mode) pel-use-rst-mode)
    (pel-autoload "pel-rst" for:
      pel-rst-set-underscore-syntax
      pel-rst-set-ref-bookmark
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

  (pel-autoload "pel-scheme" for:
    pel-clear-scheme-repl-buffer
    pel-chez-repl
    pel-chibi-repl
    pel-chicken-repl
    pel-gambit-repl
    pel-gerbil-repl
    pel-guile-repl
    pel-mit-scheme-repl
    pel-racket-repl
    pel-scsh-repl)

  (pel-autoload "pel-scroll" for:
    pel-toggle-scroll-sync
    pel-add-window-to-scroll-sync
    pel-remove-window-from-scroll-sync
    pel-scroll-up
    pel-scroll-down
    pel-scroll-left
    pel-scroll-right
    pel-scroll-down-other
    pel-scroll-up-other
    pel-scroll-down-only-this
    pel-scroll-up-only-this)

  (pel-autoload "pel-search" for:
    pel-toggle-case-fold-search
    pel-toggle-search-upper-case
    pel-show-search-case-state
    pel-search-word-from-top
    pel-select-search-tool
    pel-show-search-status
    pel-multi-occur-in-this-mode
    pel-search-two-spaces
    pel-search-empty-line)
  (pel-autoload-function "pel-search" for:
    pel-set-search-tool)

  (pel-autoload "pel-server" for:
    pel-shutdown-server)

  (pel-autoload-function "pel-seq" for: pel-all-fboundp)

  (pel-autoload "pel-sh" for:
    pel-sh-double-quote-word
    pel-sh-single-quote-word
    pel-sh-backtick-quote-word
    pel-sh-fix-sc2006)

  (pel-autoload "pel-shell" for:
    pel-shell
    pel-shell-previous-prompt
    pel-shell-next-prompt)

  (pel-autoload-function "pel-skels" for:
    pel-date
    pel-time-stamp
    pel-skel-author-comment
    pel-skel-created-comment
    pel-skel-copyright-comment
    pel-skel-insert-license-when)

  (pel-autoload-function "pel-skels-generic" for:
    pel--install-generic-skel)

  (pel-autoload-function "pel-skels-c" for:
    pel--install-c-skel)

  (pel-autoload-function "pel-skels-cpp" for:
    pel--install-c++-skel)

  (pel-autoload-function "pel-skels-clisp" for:
    pel--install-clisp-skel)

  (pel-autoload-function "pel-skels-elisp" for:
    pel--install-elisp-skel)

  (pel-autoload-function "pel-skels-rst" for:
    pel--install-rst-skel)

  (when (and (boundp 'pel-use-speedbar) pel-use-speedbar)
    (pel-autoload "pel-speedbar" for:
      pel-open-close-speedbar
      pel-toggle-to-speedbar
      pel-speedbar-toggle-refresh
      pel-speedbar-refresh
      pel-speedbar-toggle-show-all-files
      pel-speedbar-toggle-sorting
      pel-sr-speedbar-toggle-select-behaviour
      pel-speedbar-focus-current-file
      pel-speedbar-info)
    (when (display-graphic-p)
      (pel-autoload "pel-speedbar" for:
        pel-speedbar-toggle-images)))

  (pel-autoload-function "pel-spell" for:
    pel-spell-init
    pel-spell-init-from-user-option
    pel-spell-maybe-activate-flyspell
    pel-spell-maybe-activate-flyspell-prog)
  (pel-autoload "pel-spell" for:
    pel-spell-show-use
    pel-spell-toggle-prevent-flyspell
    pel-spell-change-dictionary)

  (pel-autoload-function "pel-spell-iedit" for:
    pel-spell-iedit-check-conflict)

  (pel-autoload "pel-syntax" for:
    pel-syntax-at-point)

  (pel-autoload "pel-xref" for:
    pel-xref-toggle-dumb-jump-mode
    pel-xref-toggle-gxref
    pel-xref-toggle-rtags
    pel-xref-select-front-end
    pel-toggle-helm-cscope
    pel-xref-show-status
    pel-xref-find-custom-definition-at-line)
  (pel-autoload-function "pel-xref" for:
    pel-xref-dumb-jump-activate-locally
    pel-xref-gxref-activate
    pel-xref-rtags-activate
    pel-xref-set-front-end
    pel-activate-helm-cscope)

  (pel-autoload "pel-text-insert" for:
    pel-insert-line
    pel-insert-filename
    pel-insert-filename-and-line
    pel-insert-filename-wtilde
    pel-insert-dirname
    pel-insert-dirname-wtilde
    pel-insert-date
    pel-insert-date-wkd
    pel-insert-date-time
    pel-insert-date-wkd-time
    pel-insert-iso-date
    pel-insert-iso-date-wkd
    pel-insert-iso-date-time
    pel-insert-iso-date-wkd-time
    pel-customize-insert-date-time
    pel-insert-todo-note
    pel-customize-todo-note)
  (pel-autoload-function "pel-text-insert" for:
    pel-separator-line)

  (pel-autoload "pel-text-transform" for:
    pel-capitalize-word-or-region
    pel-upcase-word-or-region
    pel-downcase-word-or-region
    pel-toggle-sentence-end
    pel-show-text-modes
    pel-upcase-letter
    pel-downcase-letter)

  ;; pel-undo loading is directly controlled by pel-init
  ;; by the logic inside pel_keys.el

  (pel-autoload "pel-screen" for:
    pel-screen-log-fix-rendering)

  (pel-autoload "pel-setup" for:
    pel-setup-dual-environment
    pel-setup-info-dual-environment
    pel-setup-fast
    pel-setup-normal)
  (pel-autoload-function "pel-setup" for:
    pel-setup-check-dual-environment)

  (when (>= emacs-major-version 27)
    (pel-autoload "pel-setup-27" for:
      pel-setup-with-quickstart
      pel-setup-no-quickstart))

  (pel-autoload "pel-setup-base" for:
    pel-setup-info)

  (pel-autoload "pel-smartparens" for:
    pel-sp-next-sexp
    pel-sp-previous-sexp
    pel-sp-delete-char
    pel-sp-backward-delete-char
    pel-smartparens-augment
    pel-smartparens-info
    pel-sp-forward-symbol
    pel-sp-backward-symbol)
  (pel-autoload-function "pel-smartparens" for:
    pel-sp-erlang-handler
    pel-smartparens-setup-erlang)

  (pel-autoload "pel-vc" for:
    pel-vc-svn-init)

  (pel-autoload "pel-vcs" for:
    pel-vcs-switch-backend
    pel-vcs-toggle-vc-log)

  (pel-autoload "pel-window" for:
    pel-show-window-previous-buffer
    pel-switch-to-last-used-buffer
    pel-show-window-dedicated-status
    pel-toggle-window-dedicated
    pel-count-non-dedicated-windows
    pel-create-window-down
    pel-create-window-right
    pel-create-window-up
    pel-create-window-left
    pel-close-window-down
    pel-close-window-up
    pel-close-window-left
    pel-close-window-right
    pel-close-other-window
    pel-2-vertical-windows
    pel-2-horizontal-windows
    pel-other-window
    pel-other-window-backward
    pel-show-window-filename-or-buffer-name
    pel-show-window-sizes)
  (pel-autoload-function "pel-window" for:
    pel-move-to-window
    pel-split-window-sensibly
    pel-find-window
    pel-window-valid-for-editing-p
    pel-window-select
    pel-window-direction-for)

  (pel-autoload "pel-whitespace" for:
    pel-toggle-delete-trailing-space-on-save
    pel-delete-all-dual-consecutive-blank-lines)
  (pel-autoload-function "pel-whitespace" for:
    pel-delete-trailing-whitespace-if-activated)

  (pel-autoload "pel-xr" for:
    pel-xr-regxp
    pel-xr-at-point
    pel-xr-lint
    pel-xr-lint-at-point)

  (pel-autoload-function "pel-yang" for:
    pel-yang-setup-support))

;; -----------------------------------------------------------------------------
(provide 'pel-autoload)

;;; pel-autoload.el ends here

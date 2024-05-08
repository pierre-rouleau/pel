;;; pel-commonlisp.el --- PEL Common Lisp Support -*-lexical-binding: t-*-

;; Copyright (C) 2020, 2021, 2024  Pierre Rouleau

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

;;;---------------------------------------------------------------------------
;;; Commentary:
;;
;; Environment Agnostic Commands
;; -----------------------------
;;
;; - The `pel-cl-repl' command opens a Common Lisp REPL, or switch to one
;;   already running, using the available technology based on the user-option
;;   selection: SLY, Slime or the bare-bone inferior Lisp mode REPL.
;;
;; - The `pel-cl-hyperspec-lookup' command which invokes the slime or sly
;;   documentation lookup command depending of which is available.
;;
;; Code hierarchy:
;;
;; * `pel-cl-repl'
;;   - `pel-switch-to-window'
;; * `pel-cl-hyperspec-lookup'
;;   - `pel-symbol-at-point'
;;
;;
;; iMenu Extension
;; ---------------
;;
;; The file also provide the `pel-cl-add-symbol-to-imenu' command.
;; The `pel-cl-add-symbol-to-imenu' command adds the Common Lisp symbol at
;; point to the iMenu section allowing that symbol to be detected and show in
;; the iMenu index list under a specified section title.
;;
;; There's not that many programming languages where a defining construct can
;; be defined by user's code.  Common Lisp is one of them.  These will not
;; always be defined ahead of time in a Emacs file or dir-local setting
;; of the `lisp-imenu-generic-expression'.  Therefore it will be useful to
;; have the ability to dynamically add such a symbol when browsing Common Lisp
;; source code that use a DSL where form defining macros are used.
;;
;; It's code hierarchy is:
;;
;; * `pel-cl-add-symbol-to-imenu'
;;   - `pel-symbol-at-point'
;;   - `pel-cl-add-to-imenu'

;; Credit: Drew Adams for nth-elt
;; https://emacs.stackexchange.com/questions/10492/how-to-get-element-number-in-a-list

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
(require 'pel--base)                    ; use: pel-symbol-at-point
;;                                      ;      pel-add-imenu-sections-to
(require 'pel--options)
(require 'pel-prompt)                   ; use: pel-prompt
(require 'pel-window)                   ; use: pel-switch-to-window
;;;---------------------------------------------------------------------------
;;; Code:


;; ---------------------------------------------------------------------------
;; pel-cl-repl
;; -----------


;;-pel-autoload
(defun pel-cl-repl (&optional n)
  "Open or switch to Common-Lisp REPL buffer window.
Use the Common Lisp REPL selected by the PEL user-options:
- SLY when `pel-used-sly' is on and `pel-clisp-ide' is set to sly,
- Slime when `pel-use-slime'is on and `pel-clisp-ide' is set to slime,
- the inferior Lisp mode otherwise.

The behaviour of the command is affected by the optional argument N:
 - with no buffers running REPL:
   > N is nil or absent:  - open REPL in current window
   > N is positive:       - open REPL in other window
   > N is negative:       - create new REPL in current window

 - with 1 or more REPL already running:
   - with 1 buffer already running it:
     - use REPL buffer as target
    - with multiple buffers already running it:
      - prompt user to select the target REPL buffer
        - if selected buffer is inside an opened window: switch to that window
        - if selected buffer is not in an opened window:
          > N is nil or absent:  - open REPL in current window
          > N is positive:       - open REPL in other window
          > N is negative:       - create new REPL in current window."
  (interactive "P")
  (let* ((n               (prefix-numeric-value n))
         (in-other-window (and n (> n 0)))
         (new-repl        (and n (< n 0))))
    (cond
     ;;
     ;; Use Slime
     ((and pel-use-slime
           (eq pel-clisp-ide 'slime))
      (if (fboundp 'slime)
          (when (or new-repl
                    (not (pel-switch-to-window 'slime-repl-mode in-other-window)))
            (slime))
        (user-error "Function slime is unbound")))
     ;;
     ;; Use SLY
     ((and pel-use-sly
           (eq pel-clisp-ide 'sly))
      (if (fboundp 'sly)
          (when (or new-repl
                    (not (pel-switch-to-window 'sly-mrepl-mode in-other-window)))
            (sly))
        (user-error "Function sly is unbound")))
     ;;
     ;; No IDE: use default Common Lisp REPL
     (t
      (if (fboundp 'run-lisp)
          (when (or new-repl
                    (not (pel-switch-to-window 'inferior-lisp-mode in-other-window)))
            (run-lisp nil))
        (user-error "Function run-lisp is unbound"))))))

;; ---------------------------------------------------------------------------

;;-pel-autoload
(defun pel-cl-hyperspec-lookup ()
  "Open Hyperspec documentation for symbol at point.
Use the Slime, SLY or PEL mechanism, whatever is available."
  (interactive)
  (cond
   ;;
   ;; Use Slime
   ((and pel-use-slime
         (eq pel-clisp-ide 'slime))
    (if (fboundp 'slime-documentation-lookup)
        (slime-documentation-lookup)
      (user-error "Function slime-documentation-lookup is unbound")))
   ;;
   ;; Use SLY
   ((and pel-use-sly
         (eq pel-clisp-ide 'sly))
    (if (fboundp 'sly-documentation-lookup)
        (sly-documentation-lookup)
      (user-error "Function sly is unbound")))
   ;;
   ;; No IDE: implement it.
   (t
    (if (and (require 'hyperspec nil :no-error)
             (fboundp 'common-lisp-hyperspec))
        (let ((symbol (or (pel-symbol-at-point)
                          (pel-prompt "Symbol: "
                                      'hyperspec-search))))
          (common-lisp-hyperspec symbol))
      (user-error "Function common-lisp-hyperspec not available!")))))

;; ---------------------------------------------------------------------------
;; Add Common Lisp define macro symbols to iMenu section
;; -----------------------------------------------------
;;

(defun pel-cl-add-to-imenu (symbol-string title)
  "Add SYMBOL-STRING to the imenu under specified TITLE."
  (pel-add-imenu-sections-to
   (list
    (list title
          'lisp-mode-symbol-regexp
          (list symbol-string)))
   'imenu-generic-expression))

;;-pel-autoload
(defun pel-cl-add-symbol-to-imenu ()
  "Add symbol at point to imenu.

Common Lisp macro can define code definition forms similar to
`defun' and friends, effectively creating a Domain Specific
Language.  The DSL symbols may not currently be know to `imenu'
parsing.

You can add new symbols to `imenu' by placing the point over such
a symbol and executing this command.

For example, if the file's code uses a `define-rule' macro like
this:

  (define-rule rule1
    (do-this)
    (do-that)
    (ensure this and that))

  (define-rule secondary
    (ensure something-else))

Place point over `define-rule' and execute the command.
You will be prompt for a title for `define-rule', where
you could enter \"rules\".

Then the `imenu' list will be able to show the \"rule\" and
\"secondary\" under the \"Rules\" iMenu section."
  (interactive)
  (let ((symbol (pel-symbol-at-point)))
    (if symbol
        (let ((title (pel-prompt (format "iMenu section for %s" symbol)
                                 'imenu-section
                                 :capitalize)))
          (when title
            (pel-cl-add-to-imenu symbol title)))
      (user-error "No symbol at point"))))

;;;---------------------------------------------------------------------------
(provide 'pel-commonlisp)

;;; pel-commonlisp.el ends here

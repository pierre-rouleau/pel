;;; pel-commonlisp.el --- PEL Common Lisp Support -*-lexical-binding: t-*-

;; Copyright (C) 2020, 2021  Pierre Rouleau

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
;; This provides the `pel-cl-repl' command.  It opens a Common Lisp REPL, or
;; switch to one already running, using the available technology based on the
;; user-option selection: SLY, Slime or the bare-bone inferior Lisp mode REPL.

;; Credit: Drew Adams for nth-elt
;; https://emacs.stackexchange.com/questions/10492/how-to-get-element-number-in-a-list

;; Code hierarchy:
;;
;; * `pel-cl-repl'
;;   - `pel-switch-to-window'
;;     - `pel-select-buffer'
;;       - `pel-nth-elt'       (credit to Drew Adams for this one)
;;
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
;;   - `pel-cl-add-to-imenu'

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
(require 'pel--base)                    ; use: pel-buffers-in-mode
;;                                      ;      pel-add-imenu-sections-to
(require 'pel--options)
(require 'pel-prompt)
;;;---------------------------------------------------------------------------
;;; Code:


(defun pel-nth-elt (element elements)
  "Return zero-indexed position of ELEMENT in ELEMENTS list, or nil if absent."
  (let ((idx  0))
    (catch 'nth-elt
      (dolist (x elements)
        (when (equal element x) (throw 'nth-elt idx))
        (setq idx (1+ idx)))
      nil)))


(defun pel-select-buffer (mode)
  "Select a buffer in specified MODE.
MODE is a major mode symbol.
Return buffer selected or nil if nothing selected."
  (let ((buffer-candidates (pel-buffers-in-mode mode)))
    (when buffer-candidates
      (if (> (length buffer-candidates) 1)
          ;; many buffers in list - prompt user
          (let* ((choices (mapcar (function buffer-name)
                                  buffer-candidates))
                 (choice (pel-select-string-from "" choices ?1)))
            (when choice
              (nth (pel-nth-elt choice choices) buffer-candidates)))
        ;; 1 buffer in list
        (car buffer-candidates)))))

(defun pel-switch-to-window (mode &optional in-other-window)
  "Switch to window that has a buffer using specified MODE if any.
MODE is a symbol.

- If no buffer uses MODE return nil.
- If one buffer uses MODE, use that buffer.
- If several buffers use MODE, prompt user to identify which one
  to select.

If the buffer is currently displayed in a frame window, select
that window.  Otherwise open the buffer in the current window
unless IN-OTHER-WINDOW is non-nil: then select another window to
open the buffer using the MODE.

Return the selected buffer using the MODE."
(let* ((selected-buffer (pel-select-buffer mode)))
  (when selected-buffer
    (let ((window-to-use (get-buffer-window selected-buffer)))
      (if window-to-use
          (progn
            (select-window window-to-use)
            (switch-to-buffer selected-buffer))
        (if in-other-window
            (switch-to-buffer-other-window selected-buffer)
          (switch-to-buffer selected-buffer)))))))

;; TODO: move the following out into a more generic place
;;       where the concept of creating REPL will be more general
;;       and can be applied to more programming languages.
(defun pel-switch-to-buffer (mode &optional in-other-window)
  "Switch to buffer in specified MODE if any.
MODE is a symbol.

Prompt user with a list with buffer names if there are several
buffer using MODE.

Return buffer selected, return nil if there are none or none selected.
Use the other window if an IN-OTHER-WINDOW argument is specified."
  (let ((selected-buffer (pel-select-buffer mode)))
    (when selected-buffer
      (if in-other-window
          (switch-to-buffer-other-window selected-buffer)
        (switch-to-buffer selected-buffer)))))

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
  (if (and (require 'thingatpt nil :noerror)
           (fboundp 'thing-at-point))
      (let ((symbol (thing-at-point 'symbol :no-properties)))
        (if symbol
            (let ((title (pel-prompt (format "iMenu section for %s" symbol)
                                     'imenu-section
                                     :capitalize)))
              (when title
                (pel-cl-add-to-imenu symbol title)))
          (user-error "No symbol at point")))
    (error "Function thing-at-point not loaded!")))

;;;---------------------------------------------------------------------------
(provide 'pel-commonlisp)

;;; pel-commonlisp.el ends here

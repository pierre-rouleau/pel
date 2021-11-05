;;; pel-spell.el --- PEL Spelling Utilities -*-lexical-binding: t-*-

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

;;; --------------------------------------------------------------------------
;;; Commentary:
;;
;;  Spell checking utilities that detect and display what spell check mode is
;;  active, and initialization code that fixes a problem with Flyspell pop-up
;;  menu when Emacs runs in terminal (TTY) mode.
;;
;;  One of the goal of this file is to avoid loading either Ispell or flyspell
;;  until they are actually required while providing a function that can
;;  configure these utilities: `pel-spell-init'.
;;
;;  The other is to provide a command that can display how spell checking is
;;  configured, even if there are things missing.  `pel-spell-show-use'
;;  attempts to do that.  It handle several types of errors.  It should always
;;  prints useful information and should never raise an error.  It's a bug if
;;  it does.  In that case please report the situation that it does not
;;  handle.
;;
;;  Another feature of this file is to provide the ability to ease spell
;;  checking code debugging by temporary preventing hooks from activating
;;  flyspell-mode or flyspell-prog-mode by providing the
;;  `pel-spell-maybe-activate-flyspell' and
;;  `pel-spell-maybe-activate-flyspell-prog' functions used as hooks.  These
;;  check the state of the variable `pel-spell-prevent-flyspell' which can be
;;  toggled by the `pel-spell-toggle-prevent-flyspell' command.
;;
;;
;;  *Credits*:
;;      Code of pel-spell-flyspell-emacs-popup-textual was taken from
;;      `https://www.emacswiki.org/emacs/FlySpell'.  I renamed it
;;      and defined it lazily when running in terminal mode.
;;
;;
;; *Limitations*:
;;  Extraction of spell programs version string done by the function
;;  `pel-spell-program-version-string' works if the version text is
;;  printed on the first line only.  That works for the followings:
;;  - aspell 0.60.6.1
;;  - Ispell 3.3.0.2
;;  - enchant-2.2.7
;;  - hunspell 1.7.0
;; I have not tested earlier version of these programs.
;;
;;
;; *Use*:
;;
;; To configure Ispell and Flyspell without forcing early loading of the
;; Ispell and flyspell libraries you can write something like the following
;; inside your init file:
;;
;;    (eval-after-load "ispell"
;;       '(when (fboundp 'pel-spell-init)
;;          (pel-spell-init "aspell"
;;                          "~/.emacs.d/.ispell")))
;;
;; A simpler version of the code is used by PEL:
;;
;;     (with-eval-after-load 'ispell (pel-spell-init-from-user-option))
;;

;;; --------------------------------------------------------------------------
;;; Dependencies:

(require 'pel--base)                    ; use: `pel-toggle-and-show'
(require 'pel--macros)
(require 'pel--options)

(eval-when-compile

  (require 'cl-lib)                     ; use: cl-dolist and cl-return

  ;; both flyspell and ispell are loaded lazily if required, but their symbols
  ;; are needed at compilation. Same for popup.
  ;; The following are part of Emacs
  (require 'flyspell)   ; use: flyspell-sort-corrections, flyspell-emacs-popup
  ;;               ispell use: ispell-call-process, ispell-check-version
  ;;                           ispell-local-dictionary, ispell-dictionary,
  ;;                           ispell-program-name, ispell-personal-dictionary
  (require 'ispell)
  (require 'subr-x)                     ; use: inlined: string-trim

  ;; The last is an external package.
  ;; It might not be present on user's system: allow compilation anyhow to
  ;; provide ability to activate lazily.
  ;; from popup: use: popup-menu*
  (require 'popup nil :no-error))

;;; --------------------------------------------------------------------------
;;; Code:

;; Global de-activation of flyspell-mode and flyspell-prog-mode
;; ------------------------------------------------------------

(declare-function pel-spell-iedit-check-conflict "pel-spell-iedit")

;;-pel-autoload
(defun pel-spell-maybe-activate-flyspell ()
  "Activator for flyspell-mode.  Used as a hook function."
  (unless pel-spell-prevent-flyspell
    (flyspell-mode 1)
    (pel-spell-iedit-check-conflict)))

;;-pel-autoload
(defun pel-spell-maybe-activate-flyspell-prog ()
  "Activator for flyspell-prog-mode.  Used as a hook function."
  (unless pel-spell-prevent-flyspell
    (flyspell-prog-mode)
    (pel-spell-iedit-check-conflict)))

;;-pel-autoload
(defun pel-spell-toggle-prevent-flyspell ()
  (interactive)
  "Toggle lock preventing flyspell-mode and flyspell-prog-mode activation."
  (pel-toggle-and-show 'pel-spell-prevent-flyspell))

;; ---------------------------------------------------------------------------
;; General error message

(defconst pel--spell-error-info-msg
  "See the spell-checking.pdf file for more info."
  "It shows where to get information if you have problem setting Ispell support.")

;; ---------------------------------------------------------------------------
;; - `pel-spell-init-from-user-option'
;;   - `pel--spell-select'
;;     * `pel-spell-init'

;;-pel-autoload
(defun pel-spell-init (spell-program-name
                       &optional spell-path personal-dictionary)
  "Initialize spell checking.

- Use specified SPELL-PROGRAM-NAME as the spell checking process.
  A string.  It must be a Ispell compatible program, like:
    \"ispell\", \"aspell\", \"hunspell\", \"enchant\" and the program
  must be found in PATH.   If it is not found, `ispell-program-name'
  is not changed.
- Specify the directory where the program is found in SPELL-PATH
  when that program is not already found in variable `exec-path'.
  To be used, the value must be a string.
  If no path is needed use nil.
  Any other type raises an error.
- Optionally identify the PERSONAL-DICTIONARY to use.

Activates flyspell-mode and fix issues in terminal mode.
When running in terminal mode, the function modifies
`flyspell-emacs-popup' with `pel-spell-flyspell-emacs-popup-textual'
to allow the flyspell pop-up menu to work in terminal mode."
  (require 'ispell)
  (if (not (string-or-null-p spell-path))
      (error "In pel-spell-init, spell-path non-nil argument is not a string"))
  (if spell-path
      (add-to-list 'exec-path spell-path))

  (pel-set-if-non-nil 'ispell-program-name (executable-find spell-program-name))
  (if personal-dictionary
      (setq ispell-personal-dictionary personal-dictionary))
  ;; Activate Flyspell spell-checking in text modes but under full
  ;; control of PEL: `text-mode-hook' normally includes `turn-on-flyspell'
  ;; which activates flyspell in all modes derived from text-mode.  Remove
  ;; that and replace it by `pel-spell-maybe-activate-flyspell' instead.
  (remove-hook 'text-mode-hook 'turn-on-flyspell)
  (add-hook    'text-mode-hook 'pel-spell-maybe-activate-flyspell)
  ;; Same thing for programming modes: Activate spell-checking in comments of
  ;; programming modes but keep control using
  ;; `pel-spell-maybe-activate-flyspell-prog' instead of using
  ;; `flyspell-prog-mode' directory as prog-mode.el does.
  (remove-hook 'text-mode-hook 'flyspell-prog-mode)
  (add-hook 'prog-mode-hook 'pel-spell-maybe-activate-flyspell-prog)
  ;;
  ;; In Terminal mode, Flyspell pop-up menu does not work.
  ;; The following code make it work, but only if the popup
  ;; package is installed.  Code in pel-zkeys.el schedule
  ;; the installation of popup if pel-spell-init is used
  ;; when emacs is used in terminal mode.
  (unless pel-emacs-is-graphic-p
    (declare-function 'popup-menu* "popup")
    (defun pel-spell-flyspell-emacs-popup-textual (_event poss _word)
      "A textual flyspell popup menu."
      (require 'popup nil :no-error)
      (require 'flyspell)
      (let* ((corrects (if flyspell-sort-corrections
                           (sort (car (cdr (cdr poss))) 'string<)
                         (car (cdr (cdr poss)))))
             (cor-menu (if (consp corrects)
                           (mapcar (lambda (correct)
                                     (list correct correct))
                                   corrects)
                         '()))
             (affix (car (cdr (cdr (cdr poss)))))
             show-affix-info
             (base-menu  (let ((save (if (and (consp affix) show-affix-info)
                                         (list
                                          (list
                                           (concat "Save affix: " (car affix))
                                           'save)
                                          '("Accept (session)" session)
                                          '("Accept (buffer)" buffer))
                                       '(("Save word" save)
                                         ("Accept (session)" session)
                                         ("Accept (buffer)" buffer)))))
                           (if (consp cor-menu)
                               (append cor-menu (cons "" save))
                             save)))
             (menu (mapcar
                    (lambda (arg)
                      (if (consp arg) (car arg) arg))
                    base-menu)))
        (if (fboundp 'popup-menu*)
            (cadr (assoc (popup-menu* menu :scroll-bar t) base-menu)))))
    (with-eval-after-load 'flyspell
      (fset 'flyspell-emacs-popup
            'pel-spell-flyspell-emacs-popup-textual))))


(defun pel--spell-select (program dict-path)
  "Use the spell checker PROGRAM, use the dictionary at DICT-PATH.

PROGRAM is the symbol of a variable that holds one of 2 things:

- the name of a ispell-compatible program that must be on the
  PATH available to Emacs when Emacs run, or

- a string representing the absolute file path of the
  ispell-compatible program to use.  This second form is useful
  in Windows when the ispell program is not available on the PATH
  of programs."
  (let* ((var-name       (symbol-name program))
         (program-name   (symbol-value program))
         (path           (file-name-directory program-name))
         (personal-dict  (pel-string-or-nil dict-path)))
    (if (and (not path)
             (not (executable-find program-name)))
        (display-warning 'pel-spell
                         (format "\
The %s user-option identifies %s as your spell checker program.
 However, that program is not found on the PATH known to Emacs!
 - Is `%s` customize value valid?%s
 - PATH seen by Emacs is: %s
 %s"
                                 var-name program-name
                                 var-name
                                 (pel-string-when
                                  (not (getenv pel-shell-detection-envvar))
                                  (format "
 - The '%s' environment variable, selected by `pel-shell-detection-envvar' to
   detect a shell launching of Emacs, is not set in the environment.
   That indicates that Emacs was launched from a GUI application instead
   of a shell.  For this environment you will need to do one of the following:
   - Identify the complete path of the ispell-compatible in
     `pel-spell-check-tool',  or
   - Update the PATH used by Emacs by specifying an extension of PATH
     inside the `pel-gui-process-environment' user-option that includes
     the directory where your ispell-compatible program is located."
                                          pel-shell-detection-envvar))
                                 (getenv "PATH")
                                 pel--spell-error-info-msg)
                         :error)
      (pel-spell-init program-name path personal-dict))))

;;-pel-autoload
(defun pel-spell-init-from-user-option ()
  "Initialize Spell checking.

Use values taken from user option variable `pel-spell-check-tool'
if specified.  If nothing is specified the spell checker is not
selected."
  (when pel-spell-check-tool
    (pel--spell-select 'pel-spell-check-tool
                       (or pel-spell-check-personal-dictionary "~/.ispell"))))

;; ---------------------------------------------------------------------------
;; * `pel-spell-show-use'
;;   - `pel-ispell-program-name'
;;     - `pel-spell-program-version-string'
;;   - `pel-ispell-main-dictionary'
;;   - `pel-ispell-personal-dictionary'

(defun pel-spell-program-version-string ()
  "Return the version string of the spell-check program used."
  (with-temp-buffer
    (if (fboundp 'ispell-call-process)
        (progn
          (ispell-call-process ispell-program-name nil t nil "-v")
          ;; extract the first line of text - that's normally the version.
          (end-of-line)
          (string-trim (buffer-substring-no-properties (point-min) (point))))
      (error "Unable to detect Ispell version"))))

(defun pel-ispell-program-name ()
  "Return string describing the Ispell program name if Ispell is loaded.
Return \"ispell not loaded\" instead."
  (if (and (boundp 'ispell-program-name)
           (fboundp 'ispell-check-version))
      (format "%s (using: %s)"
              ispell-program-name
              (or (ispell-check-version nil)
                  (pel-spell-program-version-string)
                  "?"))
    "ispell is not loaded!"))

(defun pel-ispell-main-dictionary ()
  "Return string describing Ispell main dictionary if Ispell is loaded.
Return \"?\" instead."
  (if (and (boundp 'ispell-local-dictionary)
           (boundp 'ispell-dictionary)
           (boundp 'ispell-program-name))
      (or ispell-local-dictionary
          ispell-dictionary
          (format "%s default dictionary. (using LANG: %s)"
                  ispell-program-name
                  (getenv "LANG")))
    "?"))

(defun pel-ispell-personal-dictionary ()
  "Return string describing Ispell personal dictionary if Ispell is loaded.
Return \"?\" instead."
  (if (and (boundp 'ispell-personal-dictionary)
           (boundp 'ispell-program-name))
      (or ispell-personal-dictionary
          (format "%s default personal dictionary" ispell-program-name))
    "?"))



;; Declare variable and function used in flyspell.el which control flyspell
;; prog mode.
(defvar flyspell-generic-check-word-predicate)
(declare-function flyspell-generic-progmode-verify "flyspell")

(defun pel--spell-flyspell-prog-mode-state ()
  "Return t if flyspell-prog-mode is on, nil otherwise."
  ;; `flyspell-prog-mode' is just a normal function that takes no argument,
  ;; not a minor mode function that takes an argument with a variable that has
  ;; the same name.  To detect if it is on, code must check the sate of a
  ;; variable it uses: see flyspell.el
  (eq
   flyspell-generic-check-word-predicate
   (function flyspell-generic-progmode-verify)))


;;-pel-autoload
(defun pel-spell-show-use ()
  "Display spell checking programs used and current status."
  (interactive)
  (let ((spell-program-name (condition-case nil
                                (pel-ispell-program-name)
                              (error nil)))
        (format-msg (format "%%s\
ispell: %s, flyspell: %s, flyspell-prog: %s. %%s
Spell main dictionary    : %s
Spell personal dictionary: %s
Flyspell prevention lock : %s"
                            (pel-symbol-on-off-string 'ispell-minor-mode)
                            (pel-symbol-on-off-string 'flyspell-mode)
                            (pel-on-off-string
                             (pel--spell-flyspell-prog-mode-state))
                            (pel-ispell-main-dictionary)
                            (pel-ispell-personal-dictionary)
                            (pel-symbol-on-off-string
                             'pel-spell-prevent-flyspell)))
        err-msg
        name-msg)
    (if spell-program-name
        (setq name-msg (format "
Spell check program used : %s"
                               spell-program-name))
      (setq err-msg (format "\
No ispell program detected!  Check PATH and installation.
 %s
Other available information:\n"
                            pel--spell-error-info-msg)))
    (message format-msg
             (pel-string-when err-msg)
             (pel-string-when name-msg))))

;;; --------------------------------------------------------------------------
(provide 'pel-spell)

;;; pel-spell.el ends here

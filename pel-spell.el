;;; pel-spell.el --- PEL Spelling Utilities -*-lexical-binding: t-*-

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
;;  Spell checking utilities that detect and display what spell check mode is
;;  active, and initialization code that fixes a problem with Flyspell pop-up
;;  menu when Emacs runs in terminal (TTY) mode.
;;
;;  One of the goal of this file is to avoid loading either Ispell or flyspell
;;  until they are actually required while providing a function that can
;;  configure these utilities: `pel-spell-init'.  See the Use section below.
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
;; To configure Ispell and Flyspell without forcing early loading of the Ispell
;; and flyspell libraries you can write something like the following inside your
;; init file:
;;
;;    (eval-after-load "ispell"
;;       '(when (fboundp 'pel-spell-init)
;;          (pel-spell-init "aspell"
;;                          "~/.emacs.d/.ispell")))


;; -----------------------------------------------------------------------------
;;; Dependencies:

(require 'pel--base)
(require 'pel--macros)
(require 'pel--options)

(eval-when-compile

  (require 'cl-lib)    ; use: cl-dolist and cl-return

  ;; both flyspell and ispell are loaded lazily if required, but their symbols
  ;; are needed at compilation. Same for popup.
  ;; The following are part of Emacs
  (require 'flyspell)   ; use: flyspell-sort-corrections, flyspell-emacs-popup
  ;;               ispell use: ispell-call-process, ispell-check-version
  ;;                           ispell-local-dictionary, ispell-dictionary,
  ;;                           ispell-program-name, ispell-personal-dictionary
  (require 'ispell)
  (require 'subr-x)     ; use: inlined: string-trim

  ;; The last is an external package.
  ;; It might not be present on user's system: allow
  ;; compilation anyhow to provide ability to activate lazily.
  (require 'popup nil :no-error)  ; use: popup-menu*
  )

;;; ----------------------------------------------------------------------------
;;; Code:

;; - `pel-spell-init-from-user-option'
;;   - `pel--spell-select'
;;     * `pel-spell-init'

;;-pel-autoload
(defun pel-spell-init (spell-program-name
                       &optional spell-path personal-dictionary)
  "Initialize spell checking.
- Use specified SPELL-PROGRAM-NAME as the spell checking process.
  A string.  It must be a Ispell compatible program, like:
    \"ispell\", \"aspell\", \"hunspell\", \"enchant\".
- Specify the directory where the program is found in SPELL-PATH
  when that program is not already found in the `exec-path'.
  To be used, the value must be a string.
  If no path is needed use nil.
  Any other type raises an error.
- Optionally identify the PERSONAL-DICTIONARY to use.
.
Activates flyspell-mode and fix issues in terminal mode.
When running in terminal mode, the function modifies
`flyspell-emacs-popup' with `pel-spell-flyspell-emacs-popup-textual'
to allow the flyspell pop-up menu to work in terminal mode."
  (require 'ispell)
  (if (not (string-or-null-p spell-path))
      (error "In pel-spell-init, spell-path non-nil argument is not a string"))
  (if spell-path
      (add-to-list 'exec-path spell-path))
  (setq ispell-program-name spell-program-name)
  (if personal-dictionary
      (setq ispell-personal-dictionary personal-dictionary))
  ;; Activate Flyspell spell-checking in text modes
  (add-hook 'text-mode-hook 'flyspell-mode)
  ;; Activate spell-checking in comments of programming modes
  (add-hook 'prog-mode-hook 'flyspell-prog-mode)
  ;;
  ;; In Terminal mode, Flyspell pop-up menu does not work.
  ;; The following code make it work, but only if the popup
  ;; package is installed.  Code in pel-zkeys.el schedule
  ;; the installation of popup if pel-spell-init is used
  ;; when emacs is used in terminal mode.
  (unless (display-graphic-p)
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
    (eval-after-load "flyspell"
      '(progn
         (fset 'flyspell-emacs-popup
               'pel-spell-flyspell-emacs-popup-textual)))))


(defun pel--spell-select (program-name dict-path origin)
  "Use the PROGRAM-NAME spell checker, use the dictionary at DICT-PATH."
  (let ((path           (file-name-directory program-name))
        (personal-dict  (pel-string-or-nil dict-path)))
    (if (and (not path)
             (not (executable-find program-name)))
        (display-warning :error
                         (format "Variable %s requires %s.\n\
Is %s customize value valid?
The spell check program %s not found in your PATH!\n\
You may need to install it.\n\
See the spell-checking.pdf file for more info" origin program-name origin program-name))
      (pel-spell-init program-name path personal-dict))))

;;-pel-autoload
(defun pel-spell-init-from-user-option ()
  "Initialize Spell checking.
Use the values taken from user option variable
`pel-spell-check-tool' if specified.  If nothing is specified
the spell checker is not selected."
  (when pel-spell-check-tool
    (pel--spell-select pel-spell-check-tool
                       (or pel-spell-check-personal-dictionary
                           "~/.ispell")
                       "pel-spell-check-tool")))

;; --

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

;; --

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
Return \"ispell not loaded\" instead."
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
Return \"ispell is not loaded\" instead."
  (if (and (boundp 'ispell-personal-dictionary)
           (boundp 'ispell-program-name))
      (or ispell-personal-dictionary
          (format "%s default personal dictionary" ispell-program-name))
    "?"))

;;-pel-autoload
(defun pel-spell-show-use ()
  "Display what spell checking program is being used."
  (interactive)
  (require 'pel--base nil :no-error)         ; use: pel-symbol-on-off-string
  (pel-when-fbound 'pel-symbol-on-off-string
    (message "\
ispell: %s, flyspell: %s.\n\
Spell program used       : %s\n\
Spell main dictionary    : %s\n\
Spell personal dictionary: %s"
             (pel-symbol-on-off-string 'ispell-minor-mode)
             (pel-symbol-on-off-string 'flyspell-mode)
             (pel-ispell-program-name)
             (pel-ispell-main-dictionary)
             (pel-ispell-personal-dictionary)
             )))

;; -----------------------------------------------------------------------------
(provide 'pel-spell)

;;; pel-spell.el ends here

;;; pel_keys.el --- PEL key binding definitions -*-lexical-binding: t; -*-

;; Copyright (C) 2020, 2021, 2022  Pierre Rouleau

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
;; This file defines most PEL key bindings, with some defined by the pel-skels
;; files and by pel__hydra file.
;;
;; This file is *only* loaded by pel-init, nothing else.
;; This way time is only spent when pel-init is executed, not when
;; Emacs starts.
;;
;; Note: the file and feature name has been selected so that the file name
;;       sorts after all other pel files but before pel.el to help ensure
;;       a byte-compilation order when the package system byte-compiles them
;;       during installation.  The use of an underscore in the file name is
;;       a little unusual in the Emacs Lisp world but provides a simple way
;;       to provide proper file name ordering.
;;
;; Declaration of the auto-loaded interactive functions
;; ----------------------------------------------------
;;
;; The code uses the `pel-autoload-file' macro to setup the auto-loading of
;; interactive functions.  The macro generates a call to the `autoload'
;; function and also generates a call to `declare-function' to prevent
;; byte-compiler warnings on code that reference the interactive functions
;; that are autoloaded.  The `declare-function' form does not generate any
;; extra code: it is removed by the byte-compiler, but it prevents the
;; warnings.
;;
;; Some of the defun forms in this file are not top-level forms and are
;; declared inside a conditional form.  For those, the byte-compiler would
;; generate a warning since it cannot guarantee the existence of the
;; functions. Therefore these defun forms are followed by `declare-function'
;; forms.  The logic must therefore be correct to ensure the existence of the
;; function at run-time.

;; Major Mode setup
;; ----------------
;;
;; Each major mode supported by PEL is initialized when its corresponding
;; `pel-use-<mode>' user-option is turned on.
;;
;; Each major mode has a `pel-<mode>-activates-minor-modes' user-option that
;; identifies the minor-modes that must be automatically activated for the
;; major mode. These user-options are accessed via the `pel-config-major-mode'
;; macro.

;; Minor Modes
;; -----------
;;
;; The definition of minor mode symbols must be done prior to the definition
;; of any major mode, since the user may request the automatic activation of a
;; minor mode for a given major mode and the `pel-config-major-mode' macro checks
;; the validity of the minor mode symbols by calling the function
;; `pel--check-minor-modes-in'.  That issues a warning if the minor mode
;; symbol is not loaded, it does not prevent Emacs from starting but the
;; warning message will be annoying. This check is not absolutely necessary
;; but it prevents undetected invalid entries in the customization.

;; Package Installation
;; --------------------
;;
;; The code uses the following macros to control package installation:
;;
;; - `pel-ensure-package'       Installs an elpa-compliant package.
;;
;; - `pel-install-github-file'  Installs a file from source stored in Github.
;;                              Used to install a file that is not an elpa
;;                              complaint package.  The file is stored in the
;;                              PEL utils directory.
;;
;; - `pel-install-github-files' Installs multiple files from source stored in
;;                              Github.  Used to install several files from a
;;                              *package* stored in GitHub that is not
;;                              elpa-compliant.  The files are stored in the
;;                              PEL utils directory.


;; Delayed evaluation
;; ------------------
;;
;; The code uses the following functions to delay execution:
;;
;; - `pel-eval-after-load' : macro that evaluates its BODY after the
;;                           specified FEATURE was loaded.  Useful to
;;                           extend or configure a major or minor mode.
;;                           This macro extends `with-eval-after-load' with
;;                           protection against error and formatted error
;;                           messages.
;;
;; - `with-eval-after-load' does the same with less error handling; it is used
;;                          in cases where less code is part of its BODY.
;;
;;; --------------------------------------------------------------------------
;;; Code:

;;;---------------------------------------------------------------------------
;; Required packages:
(eval-when-compile
  (require 'cl-lib))    ; use: cl-eval-when

(require 'pel--base)    ; use pel-system-is-macos-p
;;                      ;     pel-system-is-windows-p
;;                      ;     pel-emacs-is-a-tty-p
;;                      ;     pel-emacs-is-graphic-p
;;                      ;     pel-toggle
;;                      ;     pel-mode-toggle-arg
;;                      ;     pel-ensure-package
;;                      ;
;;                      ;
;;                      ;
;;                      ;
(require 'pel--macros)  ; use: pel-setq, pel-seq-default
(require 'pel--keys-macros)
(require 'pel--options) ; all `pel-use-...' variables identify what to use.
;;                      ; also defines a set of utility functions to deal with
;;                      ; the options: pel-auto-complete-help

;; ---------------------------------------------------------------------------
;; Setup GUI launched Emacs environment
;; ------------------------------------
(defvar pel--init-called-once nil
  "Remembers that `pel-init' was called. DO NOT MODIFY!")

(unless pel--init-called-once
  (unless (getenv pel-shell-detection-envvar)
    ;; A GUI Emacs is running! Set up its process environment from user-option.
    (when (and (require 'pel-process nil :no-error)
               (fboundp 'pel-process-update-environment-from))
      (pel-process-update-environment-from pel-gui-process-environment)))
    (setq pel--init-called-once t))

;; ---------------------------------------------------------------------------
;; Configure PEL-level autoloading
;; -------------------------------

(unless (fboundp 'pel-build)
  ;; autoload all PEL functions
  (require 'pel-autoload)
  (if (fboundp 'pel--autoload-init)
      (pel--autoload-init)))

;; ---------------------------------------------------------------------------
;; Control Emacs prompting
;; -----------------------
(when pel-prompt-accept-y-n
  ;; Use 'y'/'n' or SPC/DEL instead of 'yes'/'no'
  (fset 'yes-or-no-p 'y-or-n-p))

;; ---------------------------------------------------------------------------
;; Dual Environment Check
;; ----------------------
;;
;; When Emacs is running in graphic mode, verifies if the dual environment has
;; been requested and if so if it is set properly, activating it if necessary
;; and warning user on any remaining issue.  Don't do it in terminal mode
;; because the terminal mode always uses the same files as Emacs usually does.
;; Schedule this check for some time after the initialization to prevent
;; slowing down Emacs startup.
(when (and pel-emacs-is-graphic-p
           (or pel-support-dual-environment
               (bound-and-true-p pel-init-support-dual-environment-p)
               (bound-and-true-p pel-early-init-support-dual-environment-p))
           (require 'pel-setup nil :no-error)
           (fboundp 'pel-setup-check-dual-environment))
  (run-at-time "3 sec" nil (function pel-setup-check-dual-environment)))

;; ---------------------------------------------------------------------------
;; - PEL Modifier keys on different OS
;; -----------------------------------
;;
;; Ideally, mostly used keybindings are available on all platforms and
;; keyboards, with an easy to use layout.
;; The following is describing the use of modifier keys in the PEL system
;; so far.
;;
;; ======== ==== ======== ======== ============== =========== ===========
;; Modifier Symb Explicit Explicit 'darwin (gr)   'darwin (t) 'windows-nt
;;               Key      Key
;; ======== ==== ======== ======== ============== =========== ===========
;; Control  C-   Control  Control  Control        Control     Control
;; Meta     M-                     Option         Option      Alt
;; Shift    S-                     Shift          Shift       Shift
;; Hyper    H-   C-x @ h                          Fn          Menu/App
;; Super    s-   C-x @ s                          Command⌘
;; Alt      A-   C-x @ a  C-x 8
;; ======== ==== ======== ======== ============== =========== ===========

;; Use the macOS Function key (Fn) as emacs Hyper modifier
;;   Note: this does not work on the terminal based emacs,
;;         only on the graphics, Carbon-based emacs.
(when (and pel-system-is-macos-p
           pel-emacs-is-graphic-p)
  (pel-setq ns-function-modifier 'hyper))

;; On Windows, the Ctrl-Alt key combination works with letter keys
;; but does not work with the <right>, <left>, <up> and <down> keys.
;; As a work-around, this maps the apps key (between the right Windows
;; and Ctrl keys) to hyper and ensure that we add an extra binding
;; with hyper for the C-M-arrow keys.
(when pel-system-is-windows-p
  ;; The following do not seem to work:
  ;; - Ref: http://ergoemacs.org/emacs/emacs_hyper_super_keys.html
  ;; (setq w32-pass-lwindow-to-system nil)
  ;; (setq w32-lwindow-modifier 'hyper)     ; Left Windows Key  := hyper
  ;; (setq w32-pass-rwindow-to-system nil)
  ;; (setq w32-rwindow-modifier 'super)     ; Right Windows key := super
  ;; but this works:
  (pel-setq w32-pass-apps-to-system nil)
  (pel-setq w32-apps-modifier 'hyper))      ; Menu/App key      := hyper

;; ---------------------------------------------------------------------------
;; - Preserve negative-argument with C-- and C-_
;; ---------------------------------------------
;;
;; When running Emacs inside a terminal, the key C-- is not available since
;; there is no "Control -" in ASCII.  On macOS, the terminal shells generate
;; the C-_ key when C-- is typed.  Unfortunately C-_ is mapped to `undo' by
;; default, preventing quick access to the negative argument in a Control key
;; chord.  By mapping C-_ to `negative-argument' we solve the problem.
;; We do it in graphics mode also, for consistency.
;;
;; PEL also bind M-_  to `negative-argument' to help accessing it when
;; the Meta key specifiers is used, to maintain typing velocity.
;;
;; PEL reserved key::  (kbd "C-M-_")
;;
;; The last possible combination, C-M-_ is *not* bound to the
;; negative-argument function.  Instead it is *reserved* as a special
;; key sequence to use with a "lossless keyboard input" package such as
;; term-key.el to add ability to bind keys that are normally not accessible
;; in terminal mode.

(defun pel-bind-negative-argument ()
  "Bind 'C-_' and 'M-_' to `negative-argument'.
Done in this function to allow advising libraries that remap these keys."
  (global-set-key (kbd "C-_") 'negative-argument)
  (global-set-key (kbd "M-_") 'negative-argument))

;; apply the binding - the function can be used also later - if needed
;; to advise functions in other libraries.
(pel-bind-negative-argument)

;; ---------------------------------------------------------------------------
;; Inserting empty line above
;; --------------------------
(global-set-key (kbd "M-L") 'pel-insert-line-above)

;; ---------------------------------------------------------------------------
;; Binding for Greek Letter
;; ------------------------

(defconst pel-latin-to-greek
  '(
    ("a" . "α")
    ("b" . "β")
    ("c" . "χ")
    ("d" . "δ")
    ("e" . "ε")
    ("f" . "ϕ")
    ("g" . "γ")
    ("h" . "η")
    ("i" . "ι")
    ("j" . "φ")
    ("k" . "κ")
    ("l" . "λ")
    ("m" . "μ")
    ("n" . "ν")
    ("o" . "ο")
    ("p" . "π")
    ("q" . "θ")
    ("r" . "ρ")
    ("s" . "σ")
    ("t" . "τ")
    ("u" . "υ")
    ("w" . "ω")
    ("x" . "ξ")
    ("y" . "ψ")
    ("z" . "ζ")
    ("A" . "Α")
    ("B" . "Β")
    ("C" . "Χ")
    ("D" . "Δ")
    ("E" . "Ε")
    ("F" . "Φ")
    ("G" . "Γ")
    ("H" . "Η")
    ("I" . "Ι")
    ("J" . "Φ")
    ("K" . "Κ")
    ("L" . "Λ")
    ("M" . "Μ")
    ("N" . "Ν")
    ("O" . "Ο")
    ("P" . "Π")
    ("Q" . "Θ")
    ("R" . "Ρ")
    ("S" . "Σ")
    ("T" . "Τ")
    ("U" . "Υ")
    ("W" . "Ω")
    ("X" . "Ξ")
    ("Y" . "Ψ")
    ("Z" . "Ζ"))
  "Maps latin ASCII letter to the Greek equivalent")

(defun pel-bind-greek-to (prefix)
  "Add bindings for Greek letters under specified PREFIX."
  (dolist (latin.greek pel-latin-to-greek)
    (define-key
      key-translation-map
      (kbd
       (format "%s %s"
               prefix (car latin.greek)))
      (cdr latin.greek))))

(pel-bind-greek-to "<f6> g")
(when pel-activate-f9-for-greek
  (pel-bind-greek-to "<f9>"))

;; ---------------------------------------------------------------------------
;; File format parsing support
;; ---------------------------
(when pel-use-ini
  (cl-eval-when 'load
    (pel-install-github-file "pierre-rouleau/ini.el/master" "ini.el")))
(when pel-use-emacs-toml
  (pel-ensure-package toml from: melpa))
(when pel-use-kconfig-mode
  (pel-install-github-file "delaanthonio/kconfig-mode/master" "kconfig-mode.el")
  (add-to-list 'auto-mode-alist '("\\Kconfig\\'" . kconfig-mode))
  (pel-autoload-file kconfig-mode for: kconfig-mode))

;; ---------------------------------------------------------------------------
;; - Font Control
;; --------------

(when pel-emacs-is-graphic-p
  ;; Activate the all-the-icons package to get nice icons in graphics mode if
  ;; requested.
  ;;
  ;; NOTE: you must install the icons manually by executing:
  ;;       M-x all-the-icons-install-fonts
  ;;
  (when (or pel-use-all-the-icons
            pel-use-all-the-icons-ibuffer
            pel-use-all-the-icons-dired
            pel-use-all-the-icons-ivy
            pel-neotree-font-in-graphics)
    (pel-ensure-package all-the-icons from: melpa))

  (when pel-use-all-the-icons-ibuffer
    (pel-ensure-package  all-the-icons-ibuffer)
    (pel-require-at-load all-the-icons-ibuffer)
    (pel-autoload-file   all-the-icons-ibuffer for:
                         all-the-icons-ibuffer-mode)
    (all-the-icons-ibuffer-mode 1))

  (when pel-use-all-the-icons-dired
    (pel-ensure-package  all-the-icons-dired)
    (pel-require-at-load all-the-icons-dired)
    (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

  (when pel-use-all-the-icons-ivy
    (pel-ensure-package  all-the-icons-ivy)
    (pel-require-at-load all-the-icons-ivy)
    (add-hook 'after-init-hook 'all-the-icons-ivy-setup))

  ;; On macOS, the keys used by the OS are the same as selected here, both in
  ;; GUI mode and in terminal (TTY) mode:
  ;; - In terminal mode: the Terminal.app uses the ⌘ command keys for fond size
  ;;   control (it's not Emacs that acts on them, its the Terminal.app)
  ;; - In graphics mode the same keys handled by Emacs: the Super modifier is
  ;;   assigned to the ⌘ Command key.
  (when pel-system-is-macos-p
    ;; Bind the face-remap commands. The face-remap package
    ;; is part of Emacs standard distribution.
    (global-set-key (kbd "s-=")             #'text-scale-adjust)
    (global-set-key (kbd "s-+")             #'text-scale-adjust)
    (global-set-key (kbd "s--")             #'text-scale-adjust)
    (global-set-key (kbd "s-0")             #'text-scale-adjust)
    ;; Load the pel-font file only as needed.
    ;; Configure the pel-font commands as autoload.
    (pel-autoload-file pel-font for:
                       pel-font-increase-size-all-buffers
                       pel-font-decrease-size-all-buffers
                       pel-font-reset-size-all-buffers)
    (global-set-key (kbd "<s-kp-add>")      'pel-font-increase-size-all-buffers)
    (global-set-key (kbd "<s-kp-subtract>") 'pel-font-decrease-size-all-buffers)
    (global-set-key (kbd "<s-kp-0>")        'pel-font-reset-size-all-buffers)))

;; ---------------------------------------------------------------------------
;; - Buffer navigation
;; -------------------
;; Replace `list-buffer' by the nicer, more flexible and more
;; powerful `ibuffer'. Show in current window, not other one.
(global-set-key "\C-x\C-b" 'ibuffer)

;; ---------------------------------------------------------------------------
;; delight - control mode lighters
;; -------------------------------
(when pel-use-delight
  (pel-ensure-package delight from: melpa)
  (when (and pel-delight-specs
             (fboundp 'delight))
    (delight pel-delight-specs)))
(unless (string= pel-electric-pair-lighter "")
  (add-to-list 'minor-mode-alist
               (list 'electric-pair-mode pel-electric-pair-lighter)))

;; ---------------------------------------------------------------------------
;; ace-link
;; --------
;;
;; ace-link provides quick navigation in info-mode, help-mode, woman-mode,
;; eww-mode, compilation-mode, Custom mode and several other.
;; In these modes, the 'o' key puts a letter to identify the target links.
;; See URL https://github.com/abo-abo/ace-link
;; Delay its loading 1.5 seconds since it's normally not needed right away.
(when pel-use-ace-link
  (pel-ensure-package     ace-link from: melpa)
  (pel-require-after-init ace-link 1.5)
  (pel-eval-after-load    ace-link
    (if (fboundp 'ace-link-setup-default)
        (ace-link-setup-default)
      (display-warning 'pel-use-ace-link
                       "ace-link-setup-default is void"
                       :error))))

;; ---------------------------------------------------------------------------
;; avy: fast tree movement
;; -----------------------
;;
;; The avy package provides quick navigation inside any buffer and across
;; windows. See URL https://github.com/abo-abo/avy
(when pel-use-avy
  (pel-ensure-package avy from: melpa)
  (pel-autoload-file avy for:
                     avy-goto-char
                     avy-goto-char-2
                     avy-goto-char-timer
                     avy-goto-line
                     avy-goto-word-1
                     avy-goto-word-0)
  ;; Since avy uses home row keys for targets, the bindings also use keys
  ;; that are on the home row (at least for the the single key bindings).
  ;; This helps speed the typing.  The meta key is used with some extra
  ;; bindings using the control key in graphics mode (since these keys are
  ;; not available in terminal mode).
  (when pel-emacs-is-graphic-p
    (global-set-key (kbd "C-:")   'avy-goto-char)
    (global-set-key (kbd "C-'")   'avy-goto-char-2))
  (global-set-key (kbd "M-G")     'avy-goto-char)
  (global-set-key (kbd "M-g M-c") 'avy-goto-char)
  (global-set-key (kbd "M-H")     'avy-goto-char-2)
  (global-set-key (kbd "M-g M-j") 'avy-goto-char-2)
  (global-set-key (kbd "M-g f")   'avy-goto-line)
  (global-set-key (kbd "M-g l")   'avy-goto-line)
  (global-set-key (kbd "M-g w")   'avy-goto-word-1)
  (global-set-key (kbd "M-g e")   'avy-goto-word-0))

;; ---------------------------------------------------------------------------
;; Move to imenu symbol using Ido prompting
;; ----------------------------------------

(define-pel-global-prefix pel:cfg-goto (kbd "M-g <f4>"))

(global-set-key (kbd "M-g ?")   'pel-show-goto-symbol-settings)

(global-set-key (kbd "M-g i")   'imenu)
(global-set-key (kbd "M-g M-i") 'imenu)
(global-set-key (kbd "M-g h")   'pel-goto-symbol)
(global-set-key (kbd "M-g M-h") 'pel-goto-symbol)
(define-key pel:cfg-goto "h" 'pel-select-goto-symbol-UI)

(when pel-use-imenu-anywhere
  (pel-ensure-package imenu-anywhere from: melpa)
  (global-set-key (kbd "M-g y")    'pel-goto-symbol-any-buffer)
  (global-set-key (kbd "M-g M-y")  'pel-goto-symbol-any-buffer)
  (define-key pel:cfg-goto "y" 'pel-select-goto-symbol-any-buffer-UI))

;; pel-flimenu-mode is always available it displays info if flimenu-mode
;; is not available
(define-key pel:cfg-goto "f" 'pel-imenu-toggle-flatten)
(define-key pel:cfg-goto "o" 'pel-imenu-toggle-follows-order)
(define-key pel:cfg-goto "p" 'pel-imenu-toggle-popup)
(define-key pel:cfg-goto "R" 'pel-imenu-toggle-auto-rescan)

;; ---------------------------------------------------------------------------
;; PEL Top Level key prefix
;; ------------------------

;; <f11> Global key prefixes used for multiple packages:
(define-pel-global-prefix pel:     (kbd "<f11>"))
(define-pel-global-prefix pel2:    (kbd "<M-f11>"))
(define-pel-global-prefix pel:help (kbd "<f11> ?"))
(define-pel-global-prefix pel:mode (kbd "<f11> <f5>"))

;; ---------------------------------------------------------------------------
;; Dired Extensions
;; ----------------
(define-pel-global-prefix pel:for-dired (kbd "<f11> SPC M-D"))
;;
;; activate the <f12> key binding for dired
(pel--mode-hook-maybe-call
 (lambda ()
   (pel-local-set-f12 'pel:for-dired))
 'dired-mode 'dired-mode-hook)

;; Emacs ls emulation support
;; --------------------------
(when pel-use-emacs-ls-emulation
  (defvar dired-use-ls-dired)           ; prevent byte-compile warnings
  (defvar ls-lisp-use-insert-directory-program) ; ditto
  (setq dired-use-ls-dired nil)
  (require 'ls-lisp)
  (setq ls-lisp-use-insert-directory-program nil))

(when pel-use-undo-tree
  (pel-eval-after-load dired
    ;; Ensure that `dired-undo' is available in Dired buffer.
    (when (boundp 'dired-mode-map)
      (define-key dired-mode-map (kbd "M-u")   'dired-undo)
      (define-key dired-mode-map (kbd "C-x u") 'dired-undo)
      (define-key dired-mode-map (kbd "C-/")   'dired-undo))))

;; Activate extra Dired-x features when requested.
(when pel-use-dired-x
  (add-hook 'dired-load-hook
            (lambda ()
              (load "dired-x" :noerror :nomessage))))

;; Open files with OS-registered applications from Dired
;; -----------------------------------------------------
(defvar dired-mode-map) ; forward declare - dired is loaded early in Emacs
(with-eval-after-load 'dired
  (define-key dired-mode-map "z" 'pel-open-in-os-app))

;; dired-narrow
;; ------------
;; When dired-narrow is used, add <f12> prefix keys to dired-narrow specific
;; commands.

(when pel-use-dired-narrow
  (pel-ensure-package dired-narrow from: melpa)
  (pel-autoload-file dired-narrow for:
                     dired-narrow
                     dired-narrow-regexp
                     dired-narrow-fuzzy)
  ;; dired-narrow commands
  (define-key pel:for-dired "s" 'dired-narrow)
  (define-key pel:for-dired "r" 'dired-narrow-regexp)
  (define-key pel:for-dired "f" 'dired-narrow-fuzzy))

(when pel-use-dired-hide-dotfiles
  (pel-ensure-package dired-hide-dotfiles from: melpa)
  (when (eq pel-use-dired-hide-dotfiles 'hide-dot-files-by-default)
    (declare-function dired-hide-dotfiles-mode "dired-hide-dotfiles")
    (add-hook 'dired-mode-hook (function dired-hide-dotfiles-mode)))
  (with-eval-after-load 'dired
    (define-key dired-mode-map "/" 'dired-hide-dotfiles-mode)))

(when pel-use-dired-git-info
  (pel-ensure-package dired-git-info from: melpa)
  (when (eq pel-use-dired-git-info 'on-for-git-directories)
    (add-hook 'dired-after-readin-hook 'dired-git-info-auto-enable))
  (with-eval-after-load 'dired
    (define-key dired-mode-map ")" 'dired-git-info-mode)))

;; ---------------------------------------------------------------------------
;; - PEL: Window Behaviour & operations
;; ------------------------------------
;;
;; Uses: pel-window, which is autoloaded.
;;
;; Use multiple techniques to control creation, killing and  navigation
;; across windows.  Provide CRiSP-like function-key/cursor operations as
;; well as extra operations via 3 packages: windmove, ace-window and
;; winner-mode.  All Emacs default windows operation key bindings are
;; retained (except for 'C-x o' which is remapped to ace-window).
;; The operations are:
;;
;; - CRiSP-like operations (implemented using windmove and PEL code):
;;   - navigation: F11 cursor     (point where to move point to)
;;   - creation  : F11 C-cursor   (point to where to create window)
;;   - kill      : F11 C-S-cursor (point to window to kill)
;;
;;  - ace-window assigned to 'C-x o', replacing other-window, which
;;    provides a single digit window number in the top left corner of each
;;    window when moving windows.
;;
;; - Use winner-mode (assigned to 'F11 w p' and 'F11 w n') to restore previous
;;   or next window layout.
;;
;; - '<f11> w s' based keys to resize windows with following keys:
;;   - V : increase vertical size
;;   - v : decrease vertical size
;;   - H : increase horizontal size
;;   - h : decrease horizontal size
;;
;;   Since the above are long to type, once you type one, use 'C-x z' to
;;   repeat, typing 'z' subsequently to repeat again, or even easier: <f5>.
;;
;; - Original window kill: 'C-x 0' and 'C-x 1'
;; - Original window split and creation: 'C-x 2' and 'C-x 3'
;;
;; - Mode Activation - cursor window movement: windmove and framemove
;; ------------------------------------------------------------------
;; In graphics mode, more keys are accessible and some of them
;; can be used with cursor keys to help navigation across Emacs
;; windows:
;;  - for Windows: H-left, H-right, H-up and H-down
;;  - for macOS  : s-left, s-right, s-up and s-down.
;;
;; Also, in graphics mode, if a (modified) version of the
;; framemove.el file is available, then we can allow the
;; window move navigation to expand moving away from the
;; current frame into the others surrounding frames.

;; windmove is part of Emacs.  It's loading is controlled by pel-window, where
;; it is used, via autoloading. Here, just identify default keybindings when
;; Emacs is running in graphics mode.  Other keybindings are defined for the
;; pel: keybinding, somewhere else in this file.
(when pel-emacs-is-graphic-p

  ;; In graphics mode provide control over cursor color and type (shape): the
  ;; logic is in the pel-cursor.el file.
  ;; Don't delay loading: it's small enough.
  (require 'pel-cursor)
  (define-key pel: (kbd  "C-c")     'pel-set-cursor-color)

  ;; Add key cursor bindings for windmove when Emacs runs in graphics mode.
  ;; The windmove package is autoloaded via the pel-window file.  It's not
  ;; enough in graphics mode: we need to either set the default bindings (and
  ;; then we force autoload of windmove) or force users to use something else
  ;; of windmove to activate its special binding.  None of this is a good
  ;; solution. So, as a compromise to delay the loading of windmove, just
  ;; defer it for a specific amount of time, and then schedule the setting of
  ;; the special binding when it is actually loaded.
  (pel-require-after-init windmove 1)
  (pel-autoload-file windmove for: windmove-default-keybindings)
  (pel-eval-after-load windmove
    (windmove-default-keybindings (if pel-system-is-macos-p
                                      'super
                                    'hyper)))
  (when pel-use-framemove
    ;; In graphics mode, bindings to go directly to another frame
    ;; without having to move through all intervening windows in current
    ;; frame.
    ;; download and byte-compile framemove if not already present.
    ;; Do it after compiling pel_keys.el, when pel-init load pel_keys.
    (pel-install-github-file "emacsmirror/framemove/master" "framemove.el")
    (pel-autoload-file framemove for:
                       fm-up-frame
                       fm-down-frame
                       fm-left-frame
                       fm-right-frame)
    (pel-eval-after-load framemove
      (when (boundp 'framemove-hook-into-windmove)
        (setq framemove-hook-into-windmove t)))
    (global-set-key  (kbd "ESC <S-up>")    'fm-up-frame)
    (global-set-key  (kbd "ESC <S-down>")  'fm-down-frame)
    (global-set-key  (kbd "ESC <S-right>") 'fm-right-frame)
    (global-set-key  (kbd "ESC <S-left>")  'fm-left-frame)
    (define-key pel: (kbd  "<S-down>")     'fm-down-frame)
    (define-key pel: (kbd  "<S-up>")       'fm-up-frame)
    (define-key pel: (kbd  "<S-left>")     'fm-left-frame)
    (define-key pel: (kbd  "<S-right>")    'fm-right-frame)))

;; - Full Screen and Mouse Control
;; -------------------------------
(define-key pel: (kbd      "<f11>")        'pel-toggle-frame-fullscreen)
(when pel-emacs-is-a-tty-p
  (define-key pel: (kbd    "<f12>")       #'xterm-mouse-mode))

;; - Uniquify: meaningful names when multiple buffers have the same name
;; ---------------------------------------------------------------------
;; Uniquify provides meaningful names for buffers with the same name.
;; The following code snippet evolved from what's available on
;; https://github.com/bbatsov/prelude.
;; uniquify is now part of Emacs distribution.
(when pel-use-uniquify
  (pel-require-at-load uniquify)
  (pel-eval-after-load uniquify
    (pel-setq uniquify-buffer-name-style 'post-forward)
    ;; rationalize buffer after killing uniquified buffer
    (pel-setq uniquify-after-kill-buffer-p t)
    ;; Don't  not uniquify special buffers
    (pel-setq uniquify-ignore-buffers-re "^\\*")))

;; - Use Hippie Expand
;; -------------------
(when pel-use-hippie-expand
  (global-set-key [remap dabbrev-expand] 'hippie-expand)
  ;; Default PEL setup for Hippie Expand is to use DAbbrev *first*
  ;; as this is what most search need, then search in other buffers
  ;; and file names.
  ;; Don't do source code expansion: that will be handled by the
  ;; completion facilities (like completion-at-point, Company mode, etc...)
  (setq hippie-expand-try-functions-list
        (quote
         (try-expand-dabbrev
          try-expand-dabbrev-all-buffers
          try-complete-file-name-partially
          try-complete-file-name))))
;; Other search rules exist, they are:
;; - try-expand-all-abbrevs
;; - try-complete-lisp-symbol-partially
;; - try-complete-lisp-symbol
;; - try-expand-list
;; - try-expand-dabbrev-from-kill
;; - try-expand-line

;; ---------------------------------------------------------------------------
;; - Extra key bindings
;; --==================
;;

;; - Numeric Keypad Keys
;; ---------------------
;;
;; The nummeric keypad has 18 keys. They are shown here:
;;
;;     +==========================+
;;     |Numlock  =     /    *     |
;;     |   7     8     9    -     |
;;     |   4     5     6    +     |
;;     |   1     2     3   Enter  |
;;     |   ---0---     .   Enter  |
;;     +==========================+
;;
;; The numeric keypad keys behave differently when Emacs is running in
;; graphics mode and when it is running in Terminal mode.  Another difference
;; is between the PC-keyboard and the macOS keyboards.  The PC-keyboard has a
;; numlock key, but the macOS keyboards do not: the Numlock is a clear key.
;;
;; PEL attempts to provide the exact same behaviour for these keys in both
;; modes for all environments despite the difference.
;;

;; -- Numlock/Clear
;;
;; For macOS, the top-left key, labelled as "NumLock" above, is detected only
;; in graphics mode and is the [clear] key.  The Terminal mode does not detect
;; that key. So 2 key bindings are done: - [clear] is mapped to
;; pel-toggle-mac-numlock - <f11> # is also mapped to pel-toggle-mac-numlock
;; (see later)

(global-set-key [clear] 'pel-toggle-mac-numlock)

;; -- The keys +, / and *
;;
;; The "=", "/" and "*" keys are untouched, left to self-insert in all cases.
;;

;; -- The numeric keys: 0 to 9
;;
;; The numeric keys are mapped to pel-0 to pel-9 commands.  These commands
;; check the satet of num-locking managed by the pel-numkpad.el to determine
;; how they should behave: in num-lock they just self insert the corresponding
;; digit. When not num-locked, the keys implement other operations: cursor
;; movement, etc...
;;
;; On graphics mode Emacs is able to generate M-<kp-8> when the Meta key is
;; pressed while the keypad 8 is pressed.  That does not work in Terminal mode.

;; On macOS the keypad 0 key registers as kp-0, but on Linux and Windows it
;; registers as <insertchar> when numlock is off.
(if pel-system-is-macos-p
    (global-set-key [kp-0] 'pel-0)
  (global-set-key (kbd "<insertchar>") 'pel-0))
(global-set-key [kp-1] 'pel-1)
(global-set-key [kp-2] 'pel-2)
(global-set-key [kp-3] 'pel-3)
(global-set-key [kp-4] 'pel-4)
(global-set-key [kp-5] 'pel-5)
(global-set-key [kp-6] 'pel-6)
(global-set-key [kp-7] 'pel-7)
(global-set-key [kp-8] 'pel-8)
(global-set-key [kp-9] 'pel-9)

;; On some keyboards, when in non-numlock the key 0 acts as [kp-insert].
;; This is not available on macOS but it is under Windows keyboards.
;; Also under Windows the key '5' when not num-locked is [kp-space].
;; Therefore PEL configures these keys for Windows only,
;; on macOS it does not, instead it relies on the binding of [kp-0] to pel-0
;; and of [kp-5] to pel-5 to handle it.

(when pel-system-is-windows-p
  (global-set-key [kp-insert] 'yank)
  (global-set-key [kp-space] 'recenter-top-bottom))

;; -- The . ([kp-decimal]) key
;;
;; In terminal (TTY) mode, the keypad "." is not recognized
;; as [kp-decimal] but as "M-O n" so adjust the binding accordingly.
;; In both cases, bind the key to pel-kp-decimal which behaves according
;; to the state of num-locking controlled by pel-numkpad.el

(if pel-emacs-is-graphic-p
    (global-set-key [kp-decimal] 'pel-kp-decimal)
  (global-set-key (kbd "M-O n") 'pel-kp-decimal))

;; -- The + key on the numerical keypad
;;
;; That key registers as [kp-add] when Emacs runs in graphics mode, but
;; registers as [kp-separator] when Emacs runs in terminal mode.
;; So PEL binds the appropriate key to pel-kp-add which selects the action
;; according to the state of num-locking controlled by pel-numkpad.el

(if (or pel-emacs-is-graphic-p pel-keypad-+-is-kp-add)
    (global-set-key [kp-add] 'pel-kp-add)
  (global-set-key [kp-separator] 'pel-kp-add))
;; Check for the Meta Keypad+ in terminal mode
(unless pel-emacs-is-graphic-p
  (when pel-keypad-meta+-special-sequence
    (global-set-key (kbd pel-keypad-meta+-special-sequence)
                    'pel-copy-symbol-at-point)))

;; -- The - key on the numeric keypad
;;
;; That key alwyas registers as [kp-subtract], so bind it to the
;; pel-kp-subtract which selects the action according to the state
;; of num-locking controlled by pel-numkpad.el

(global-set-key [kp-subtract] 'pel-kp-subtract)

;; -- Numerical Keypad keys with modifier keys : Copy/Delete/Kill at point
;;
;; The following code controls the following operations:
;; - Copy a `thing` from the buffer to the kill-ring (for later pasting).
;; - Delete a `thing` from the buffer, without storing to the kill-ring.
;; - Kill a `thing` from the buffer: delete it from the buffer and copy
;;   to the kill-ring (for later pasting).
;;
;; The `thing` can be one of the symbols supported by bounds-of-thing-at-point:
;;  word, symbol, list, sexp, defun, line, sentence, whitespace, page,
;;  filename, url, email.
;;
;; The goal of this is to support quick operations with simple key bindings.

(global-set-key [C-kp-add]  'pel-copy-word-at-point)
(global-set-key [M-kp-add]  'pel-copy-symbol-at-point)
(global-set-key (kbd "M-+") 'pel-copy-symbol-at-point)

(global-set-key [C-kp-subtract] 'pel-kill-word-at-point)
(global-set-key [M-kp-subtract] 'pel-kill-symbol-at-point)

;; -- Numerical Keypad keys with modifier keys : navigate sexp/code nesting
;; Cursor-Keys:
;; Use the numeric keypad cursor keys.
;;  On macOS, simulate the numeric keypad using the number keys
;;  on the keypad.
(if pel-system-is-macos-p
    (progn
      (global-set-key [C-kp-4]    #'backward-sexp)
      (global-set-key [C-kp-6]    #'forward-sexp)
      (global-set-key [C-kp-8]    #'backward-up-list)
      (global-set-key [C-kp-2]    #'down-list))
  (global-set-key [C-kp-left]     #'backward-sexp)
  (global-set-key [C-kp-right]    #'forward-sexp)
  (global-set-key [C-kp-up]       #'backward-up-list)
  (global-set-key [C-kp-down]     #'down-list))

;; ---------------------------------------------------------------------------
;; - Navigation control facilities
;; -------------------------------
;; - uses: pel-navigate
;;
;; Remap standard `beginning-of-line' to `pel-beginning-of-line'.  The PEL
;; function combines the functionality of `beginning-of-line' and the function
;; `back-to-indentation' so that the cursor moves to the beginning of line and
;; then moves to the indentation.
(global-set-key (kbd "C-a") 'pel-beginning-of-line)

;; Remap standard `end-of-line' to `pel-end-of-line' for the similar reason:
;; move to the end of line and then to the last non-whitespace in case there
;; is trailing whitespace.
(global-set-key (kbd "C-e") 'pel-end-of-line)

;; Augment word movement by adding M-n to move to the beginning of a word,
;; something that is not provided by the standard Emacs keys; it only has
;; forward-word  (which moves at the end of word forward) and backward-word
;; (which moves backward to the beginning of the word).  The backward-word
;; command is bound to M-b which is just to the left of the M-n key in QWERTY
;; and AZERTY keyboards.
(global-set-key (kbd "M-n")  'pel-forward-word-start)

;; Meta left/right to forward/backward-word as this is needed by term shells
;; and python shells from outside Emacs so we leave it to get the same
;; behaviour in these shells inside Emacs. These leave cursor at end of a
;; word.
(global-set-key (kbd "<M-left>")  #'backward-word)
(global-set-key (kbd "<M-right>") #'forward-word)
(when (and pel-emacs-is-a-tty-p
           pel-map-meta-left-right-to-Y-Z)
  ;; On a TTY:
  ;; map <M-left>  to the ANSI key sequence for M-Y: "\033Y"
  ;; map <M-right> to the ANSI key sequence for M-Z: "\033Z"
  (define-key esc-map "Y" #'backward-word)
  (define-key esc-map "Z" #'forward-word))

;; Control left/right to forward/backward-word but leave point to the
;; beginning of the word instead.
(global-set-key (kbd "<C-right>") 'pel-forward-token-start)
(global-set-key (kbd "<C-left>")  'pel-backward-token-start)

;; - Navigate to beginning/end of line/window/buffer
;; -------------------------------------------------
;;
;; Replacement for the binding of the <home> and <end>
;; bindings (originally bound respectively to 'beginning-of-buffer
;; and to 'end-of-buffer) with an improved CRiSP-like single/multi
;; home/end key implemented by 'pel-home and 'pel-end.
;;
(global-set-key [(home)] 'pel-home)
(global-set-key [(end)]  'pel-end)

;; If you run Emacs under GNU Screen you may experience a change of behaviour
;; of the <end> key that now registers as <select>.  Set
;; `pel-select-key-is-end'  to t to circumvent the problem.
(when pel-select-key-is-end
  (global-set-key (kbd "<select>") 'pel-end))

;; Navigating through Sexp/blocks (not normally bound by Emacs)
(global-set-key (kbd "C-M-]") 'up-list)

;; ---------------------------------------------------------------------------
;; - Cursor Keys
;; -------------

;; ============ ========== ==================== =============================
;; Cursor key   Modifier   Function             Notes
;; ============ ========== ==================== =============================
;; Left         Control    left-word
;; Right        Control    right-word
;; Up           Control    backward-paragraph
;; Down         Control    forward-paragraph
;; kp-Left      Control    backward-sexp
;; kp-Right     Control    forward-sexp
;; kp-Up        Control    backward-up-list
;; kp-Down      Control    down-list
;;
;; Left         Meta       backward-word        Move to word left
;;                                              ignores all non-whitespace
;; Right        Meta       forward-word         Move to word right
;;                                              ignore all non-whitespace
;; Up           Meta       ??
;; Down         Meta       ??
;; kp-Left      Meta       digit-argument
;; kp-Right     Meta       digit-argument
;; kp-Up        Meta       digit-argument
;; kp-Down      Meta       digit-argument
;;
;; Left         Hyper      end
;; Right        Hyper      home
;; Up           Hyper      prior
;; Down         Hyper      next
;; kp-Left      Hyper      pel-4
;; kp-Right     Hyper      pel-6
;; kp-Up        Hyper      pel-8
;; kp-Down      Hyper      pel-2
;;
;; Left         Super      windmove-left        Move cursor to window at left
;; Right        Super      windmove-right       Move cursor to window at right
;; Up           Super      windmove-up          Move cursor to window above
;; Down         Super      windmove-down        Move cursor to window below
;; kp-Left      Super      ??
;; kp-Right     Super      ??
;; kp-Up        Super      ??
;; kp-Down      Super      ??
;;
;; Up           Meta-Super ??
;; Down         Meta-Super ??
;; kp-Left      Meta-Super ??
;; kp-Right     Meta-Super ??
;; kp-Up        Meta-Super ??
;; kp-Down      Meta-Super ??
;; ============ ========== ==================== ==============================

;; ---------------------------------------------------------------------------
;; - Function Keys
;; ---------------
;;
;; The first 4 function keys are used by Emacs and do not support combinations
;; with Control, Meta and Shift in macOS terminal:  I have not found a
;; way to get terminal escape sequences for the first 4 functions keys to work
;; with Emacs running in macOS Terminal.
;; For F5 through F12, the combinations do work and aside from F8 (used by
;; spell and flyspell), these keys are not used by the Emacs packages I use so
;; far.  I use F6 as an extra prefix and assign the other keys to various
;; operations as described below.

;; <f1>  : Emacs help system
;; <f2>  : prefix
;; <f3>  > pel-kmacro-start-macro-or-insert-counter
;; <f4>  : kmacro-end-or-call-macro
;; <f5>  > repeat           (C)                (M) pel-scroll-up
;; <f6>  > pel prefix       (C)                (M) pel-scroll-down
;; <f7>  > pel-hydra-window (C)                (M)
;; <f8>  >                  (C)                (M)
;; <f9>  >                  (C)                (M)
;;                                             (M-S) pel-show-init-time
;; <f10> > menu-bar-open,   (C) buffer-menu-open, (M) toggle-frame-maximized
;; <f11> > pel prefix,      <C-f11>:                   <M-f11>:
;; <f12> > mode-sensitive,  <C-f12>: pel-next-visible, <M-f12>: mode-sensitive

;; - PEL: Protected keyboard-macro definitions
;; -------------------------------------------
(global-set-key (kbd "<f3>") 'pel-kmacro-start-macro-or-insert-counter)

;; ---------------------------------------------------------------------------
;; - Function Keys - <f5>
;; ----------------------
;; Bind repeat to a single key: <f5> and <S-F5>
(global-set-key (kbd "<f5>") 'repeat)
;; <S-f5> is also bound to repeat but also marks.

;; ---------------------------------------------------------------------------
;; - Function Keys - <f6>
;; ----------------------
;;
;; pel:f6 keys:
;;  d f l L
;;  SPC
;;  C-f C-i <backtab> C-n C-p
;;  M-f
;;  C-M-f
;;  <down> <up> <left> <right>
;;  <f12>

(define-pel-global-prefix pel:f6 (kbd "<f6>"))
(define-key pel:f6 "d"  'pel-insert-current-date)
(define-key pel:f6 "D"  'pel-insert-current-date-time)
(define-key pel:f6 "f"  'pel-insert-filename)
(define-key pel:f6 "F"  'pel-insert-filename-and-line)
(define-key pel:f6 (kbd "C-f")  'pel-insert-dirname)
(define-key pel:f6 (kbd "M-f")  'pel-insert-filename-wtilde)
(define-key pel:f6 (kbd "C-M-f")  'pel-insert-dirname-wtilde)
(define-key pel:f6 "l"  'pel-insert-line)
(define-key pel:f6 "t"  'pel-insert-iso8601-timestamp)
(define-key pel:f6 (kbd "<f6>") 'pel-jump-to-mark)
(define-key pel:f6 "6" 'complete-symbol)
(define-key pel:f6 "7" 'info-complete-symbol)
(define-key pel:f6 "T" 'pel-insert-todo-note)

;; Move to the beginning of next function definition (while moving forward)
;;  complements C-M-e and C-M-a
(define-key pel:f6 "d"            'pel-duplicate-line)
(define-key pel:f6 (kbd "<down>") 'pel-beginning-of-next-defun)
(define-key pel:f6 (kbd "<up>")   'beginning-of-defun)
(define-key pel:f6 (kbd "<left>") 'pel-end-of-previous-defun)
(define-key pel:f6 (kbd "<right>")'end-of-defun)
(define-key pel:f6 (kbd "C-n")    'pel-goto-next-url)
(define-key pel:f6 (kbd "C-p")    'pel-goto-previous-url)
(define-key pel:f6 (kbd "SPC")    'pel-tempo-mode)

;; (kbd "<tab>") does not work in terminal mode, it works only in graphics mode
(define-key pel:f6 (kbd "C-i")       'pel-indent-lines)
(define-key pel:f6 (kbd "<backtab>") 'pel-unindent-lines)
;;
;; Install the generic skeletons, 1 second after Emacs starts to reduce
;; Emacs init time.
(run-with-idle-timer 1 nil (function pel--install-generic-skel) pel:f6)

;; ---------------------------------------------------------------------------
;; - Function Keys - <f11>
;; -----------------------
;;
;; See the definition of pel: in the upper portion of this file.

;; --
;; - Function Keys - <f11> top-level prefix keys


(define-key pel: (kbd "M-t") 'pel-set-tab-width)

(defun pel--global-windmove-on (prefix)
  "Bind windmove commands on PREFIX key followed by cursor."
  (global-set-key (kbd (format "%s <up>"        prefix)) 'windmove-up)
  (global-set-key (kbd (format "%s <down>"      prefix)) 'windmove-down)
  (global-set-key (kbd (format "%s <right>"     prefix)) 'windmove-right)
  (global-set-key (kbd (format "%s <left>"      prefix)) 'windmove-left)
  (global-set-key (kbd (format "%s <C-down>"    prefix)) 'pel-create-window-down)
  (global-set-key (kbd (format "%s <C-up>"      prefix)) 'pel-create-window-up)
  (global-set-key (kbd (format "%s <C-left>"    prefix)) 'pel-create-window-left)
  (global-set-key (kbd (format "%s <C-right>"   prefix)) 'pel-create-window-right)
  (global-set-key (kbd (format "%s <C-S-down>"  prefix)) 'pel-close-window-down)
  (global-set-key (kbd (format "%s <C-S-up>"    prefix)) 'pel-close-window-up)
  (global-set-key (kbd (format "%s <C-S-left>"  prefix)) 'pel-close-window-left)
  (global-set-key (kbd (format "%s <C-S-right>" prefix)) 'pel-close-window-right))

(when pel-windmove-on-esc-cursor
  (pel--global-windmove-on "ESC"))
(when pel-windmove-on-f1-cursor
  (pel--global-windmove-on "<f1>"))

(define-key pel: (kbd      "<down>")       'windmove-down)
(define-key pel: (kbd      "<up>")         'windmove-up)
(define-key pel: (kbd      "<left>")       'windmove-left)
(define-key pel: (kbd      "<right>")      'windmove-right)
(define-key pel: (kbd      "<C-down>")     'pel-create-window-down)
(define-key pel: (kbd      "<C-up>")       'pel-create-window-up)
(define-key pel: (kbd      "<C-left>")     'pel-create-window-left)
(define-key pel: (kbd      "<C-right>")    'pel-create-window-right)
(define-key pel: (kbd      "<C-S-down>")   'pel-close-window-down)
(define-key pel: (kbd      "<C-S-up>")     'pel-close-window-up)
(define-key pel: (kbd      "<C-S-left>")   'pel-close-window-left)
(define-key pel: (kbd      "<C-S-right>")  'pel-close-window-right)

(define-key pel:           "#"             'pel-toggle-mac-numlock)
(define-key pel:           "`"            #'overwrite-mode)
(if pel-system-is-macos-p
    (global-set-key  (kbd "ESC <kp-0>")       #'overwrite-mode)
  (global-set-key  (kbd "ESC <insertchar>")   #'overwrite-mode))
(define-key pel: (kbd      "RET")         #'auto-fill-mode)
(define-key pel: (kbd      "<deletechar>") 'c-hungry-delete-forward)
(define-key pel: (kbd      "M-f")          'pel-forward-syntaxchange-start)
(define-key pel: (kbd      "<M-right>")    'pel-forward-syntaxchange-start)
(define-key pel: (kbd      "M-b")          'pel-backward-syntaxchange-start)
(define-key pel: (kbd      "<M-left>")     'pel-backward-syntaxchange-start)
(define-key pel: (kbd      "0")           #'hl-line-mode)
(define-key pel: (kbd      "M-=")          'pel-toggle-show-copy-cut-text)
;; add ``<f11> C-l`` binding to recenter-top-bottom because some modes use C-l
;; as a prefix key and this feature is quite useful.
(define-key pel: (kbd      "C-l")          'recenter-top-bottom)

;; Electric-pair-mode control
(define-key pel: (kbd      "M-e")          'electric-pair-local-mode)

;; ---------------------------------------------------------------------------
;; EditorConfig Support
;; --------------------
(when pel-use-editor-config
  (pel-ensure-package  editorconfig from: melpa)
  (pel-require-at-load editorconfig)
  (pel-eval-after-load editorconfig
    (if (fboundp 'editorconfig-mode)
        (editorconfig-mode 1)
      (user-error "Failed loading editorconfig"))))

;; ---------------------------------------------------------------------------
;; Actions on File Save
;; --------------------
;; As controlled by PEL customized user options.

(when pel-delete-trailing-whitespace
  ;; - Remove trailing whitespaces on file save
  (add-hook 'before-save-hook  'pel-delete-trailing-whitespace-if-activated))
(define-key pel: (kbd "M-W") 'pel-toggle-delete-trailing-space-on-save)

;; -- update time stamp

(defun pel-toggle-update-time-stamp-on-save (&optional globally)
  "Toggle time-stamp update on file save and display current state.
By default change behaviour for local buffer only.
When GLOBALLY argument is non-nil, change it for all buffers for the current
Emacs editing session (the change does not persist across Emacs sessions).
To modify the global state permanently modify the customized value of the
`pel-update-time-stamp' user option via the `pel-pkg-for-filemng'
group customize buffer."
  (interactive "P")
  (pel-toggle-and-show-user-option 'pel-update-time-stamp globally))

(defun pel--update-time-stamp ()
  "Update time stamp if currently active."
  (when pel-update-time-stamp
    (time-stamp)))

(when pel-update-time-stamp
  ;; - Update file timestamp on file same (if any)
  (add-hook 'before-save-hook  'pel--update-time-stamp)
  (define-key pel: (kbd "M-T") 'pel-toggle-update-time-stamp-on-save))

;; -- update copyright notice

(defun pel-toggle-update-copyright-on-save (&optional globally)
  "Toggle copyright update on file save and display current state.
By default change behaviour for local buffer only.
When GLOBALLY argument is non-nil, change it for all buffers for the current
Emacs editing session (the change does not persist across Emacs sessions).
To modify the global state permanently modify the customized value of the
`pel-update-copyright' user option via the `pel-pkg-for-filemng'
group customize buffer."
  (interactive "P")
  (pel-toggle-and-show-user-option 'pel-update-copyright globally))

(defun pel--update-copyright (&optional arg interactivep)
  "Update time stamp if currently active.
With prefix ARG, replace the years in the notice rather than adding
the current year after them.  If necessary, and
‘copyright-current-gpl-version’ is set, any copying permissions
following the copyright are updated as well.
If non-nil, INTERACTIVEP tells the function to behave as when it’s called
interactively."
  (when pel-update-copyright
    (copyright-update arg interactivep)))

(when pel-update-copyright
  ;; Update the copyright notice present in a file
  (add-hook 'before-save-hook  'pel--update-copyright)
  (define-key pel: (kbd "M-@") 'pel-toggle-update-copyright-on-save))

;; -- make script executable

(when pel-make-script-executable
  ;; - Make script file executable on file save
  (add-hook 'after-save-hook
            'executable-make-buffer-file-executable-if-script-p))

;; - iedit - Powerful editing of multiple instances
;; ------------------------------------------------
(when pel-use-iedit
  (defun pel--add-keys-to-iedit-mode ()
    "Add keys that work in terminal mode to iedit-mode key maps."
    (when (boundp 'iedit-lib-keymap)
      (define-key iedit-lib-keymap (kbd "C-c C-a") 'iedit-show/hide-context-lines)
      (define-key iedit-lib-keymap (kbd "C-c C-o") 'iedit-show/hide-occurrence-lines))
    (when (boundp 'iedit-mode-occurrence-keymap)
      (let ((map iedit-mode-occurrence-keymap))
        (define-key map (kbd "<f1> <f2>") 'iedit-help-for-occurrences)
        (define-key map (kbd "M-U")       'pel-redo)
        (define-key map (kbd "<f1> M-c")  'iedit-toggle-case-sensitive)
        (define-key map (kbd "M-c")       'iedit-downcase-occurrences)
        (define-key map (kbd "M-C")       'iedit-upcase-occurrences))))
  (declare-function pel--add-keys-to-iedit-mode "pel_keys")

  (pel-ensure-package iedit from: melpa)
  (pel-autoload-file iedit for: iedit-mode)
  (define-key pel: "e" 'iedit-mode)
  (define-key ctl-x-r-map "\r" 'iedit-rectangle-mode)
  ;; More iedit config - always required.
  (pel-eval-after-load iedit
    (declare-function pel-spell-iedit-check-conflict "pel-spell-iedit")
    (pel-spell-iedit-check-conflict)
    (pel--add-keys-to-iedit-mode))
  (pel-eval-after-load sh-script
    (require 'pel-sh-iedit)
    (add-hook 'sh-mode-hook 'pel-sh-iedit-enhance)))

;; - indent-tools
;; --------------
(when pel-use-indent-tools
  (pel-ensure-package indent-tools from: melpa)
  (pel-autoload-file indent-tools for: indent-tools-hydra/body))

;; - popup-kill-ring
;; -----------------
;; View all kill-ring deletions in a pop-up menu, when M-y is typed.
;; Activate the popup-kill-ring in graphic mode only
;; because it does not seem to work in terminal mode.
;; It uses the pos-tip package.
(when (and pel-use-popup-kill-ring
           pel-emacs-is-graphic-p)
  ;; Note: pos-tip, required by popup-kill-ring is installed
  ;;       when popup-kill-ring is installed.
  (pel-ensure-package popup-kill-ring from: melpa)
  (pel-autoload-file popup-kill-ring for: popup-kill-ring)
  (define-key pel: (kbd "M-y") 'popup-kill-ring))

;; - browse-kill-ring
;; ------------------
(when pel-use-browse-kill-ring
  (pel-ensure-package browse-kill-ring from: melpa)
  (global-set-key "\C-cy" 'browse-kill-ring))

;; - smart-dash
;; ------------
(when pel-use-smart-dash
  (pel-ensure-package smart-dash from: melpa)
  (pel-autoload-file smart-dash for: smart-dash-mode)
  (autoload 'smart-dash-insert-or-overwrite "smart-dash")

  ;; Prevent smart-dash-mode from preventing the <kp-subtract> from being
  ;; able to invoke pel-kp-subtract when an area is marked, retaining the
  ;; capability to copy marked text with <kp-subtract> without loosing
  ;; ability to insert a dash with it (even when the area is marked: just
  ;; toggle the numlock on)
  (declare-function smart-dash-insert-or-overwrite "smart-dash")
  (defun pel--smart-dash-insert-dash-or-kp-subtract ()
    "Insert dash or invoke pel-kp-subtract when area marked."
    (interactive)
    (if (use-region-p)
        (pel-kp-subtract 1)
      (smart-dash-insert-or-overwrite ?-)))
  (declare-function pel--smart-dash-insert-dash-or-kp-subtract "pel_keys")
  (advice-add 'smart-dash-insert-dash
              :override (function pel--smart-dash-insert-dash-or-kp-subtract))

  ;; when we can, activate a red lighter for Smart dash
  (when (and pel-use-delight
             (fboundp 'delight))
    (delight 'smart-dash-mode
             (propertize "-" 'font-lock-face '(:foreground "green"))
             "smart-dash"))

  (pel-add-hook-for
   'pel-modes-activating-smart-dash-mode
   (lambda ()
     (smart-dash-mode 1)
     ;; ensure that the keypad dash is used as pel-kp-subtract
     ;; which either cuts current line or inserts a normal dash.
     (fset 'smart-dash-insert-dash 'pel-kp-subtract))))

;; ---------------------------------------------------------------------------
;; - Display of Regular Expression -- easy-escape
;; ----------------------------------------------
(when pel-use-easy-escape
  (pel-ensure-package easy-escape from: melpa)
  (pel-autoload-file easy-escape for: easy-escape-minor-mode)
  (define-key pel: "\"" 'easy-escape-minor-mode)
  (pel-add-hook-for
   'pel-modes-activating-easy-escape
   (lambda ()
     (easy-escape-minor-mode 1))))

;; ---------------------------------------------------------------------------
;; - Use undo-tree
;; ---------------
;; Use undo-tree which provides undo/redo ability with complete storage and
;; no loss of history even when redo is done.
;; The key bindings use keys similar to Brief & CRiSP keys: M-u, M-U
;; note that the M-u key is normally assigned to upcase-word but M-C
;; (which is bound to pel-upcase-word-or-region) replaces it.
;; Keys:
;; pel-use-undo-tree        nil                   t
;;
;;  - C-x u                 : undo              : undo-tree-undo
;;  - C-/                   : undo              : undo-tree-undo
;;  - M-u                   : undo              : undo-tree-undo
;;  - M-U                                       : undo-tree-redo
;;  - s-z                   : undo              : undo-tree-undo
;;  - s-Z                                       : undo-tree-redo
;;  - <f11> u u             : undo              : undo-tree-undo
;;  - <f11> u r                                 : undo-tree-redo
;;  - <f11> u v                                 : undo-tree-visualize
;;  - <f11> u /                                 : undo-tree-switch-branch
;;  - <f11> u \             : goto-last-change  : goto-last-change

(define-pel-global-prefix pel:undo (kbd "<f11> u"))

(if pel-use-undo-tree
    (progn
      ;; autoload pel-undo if one of the following commands
      ;; are executed - in the case where pel-use-undo-tree is t.
      (pel-autoload-file pel-undo for:
                         pel-undo
                         pel-redo)
      (global-set-key (kbd "C-z")  'pel-undo)
      (when pel-emacs-is-graphic-p
        (global-set-key (kbd  "s-z")   'pel-undo)
        (global-set-key (kbd  "s-Z")   'pel-redo))
      (global-set-key (kbd    "C-x u") 'pel-undo)
      (global-set-key (kbd    "C-/")   'pel-undo)
      (global-set-key (kbd    "M-u")   'pel-undo)
      (global-set-key (kbd    "M-U")   'pel-redo)
      (define-key pel:undo    "u"      'pel-undo)
      (define-key pel:undo    "r"      'pel-redo)

      (pel-ensure-package undo-tree from: gnu)
      (pel-autoload-file undo-tree for:
                         undo-tree-mode
                         global-undo-tree-mode
                         undo-tree-undo
                         undo-tree-redo
                         undo-tree-visualize
                         undo-tree-switch-branch)
      (define-key pel:undo  "v"    'undo-tree-visualize)
      (define-key pel:undo  "x"    'undo-tree-switch-branch)
      ;; The file undo-tree sets the undo-tree-map key-map which
      ;; sets the binding of M-_ and C-_ to `undo-tree-undo' and
      ;; `undo-tree-redo' and therefore changes the setting that PEL
      ;; is promoting when pel-use-undo-tree is set:
      ;; the binding of M-_ and C-_ to `negative-argument'.
      ;; To correct that, we modify the undo-tree-map and install
      ;; the `negative-argument' function after activating undo tree
      ;; globally.
      (pel-eval-after-load undo-tree
        (global-undo-tree-mode)
        (if (boundp 'undo-tree-map)
            (progn
              (define-key undo-tree-map  (kbd "C-_") 'negative-argument)
              (define-key undo-tree-map  (kbd "M-_") 'negative-argument))
          (display-warning 'pel-use-undo-tree
                           "The undo-tree-map variable is not bound, \
can't bind negative-argument to C-_ and M-_"
                           :error))))

  ;; When pel-use-undo-tree is not t, then use standard Emacs undo but
  ;; map to similar keys (except the redo keys: ``<f11> u r`` and ``M-U``)
  (when pel-emacs-is-graphic-p
    (global-set-key (kbd  "s-z")    #'undo))
  (global-set-key (kbd    "C-x u")  #'undo)
  (global-set-key (kbd    "C-/")    #'undo)
  (global-set-key (kbd    "M-u")    #'undo)
  (define-key pel:undo    "u"       #'undo))

;; - Use goto-last-change
;; ----------------------
(when pel-use-goto-last-change
  (pel-ensure-package goto-last-change from: melpa)
  (pel-autoload-file goto-last-change for: goto-last-change)
  (define-key pel:undo "\\" 'goto-last-change))

;; ---------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> <f10>`` : Menu commands
;; Force load of pel-imenu after load of imenu: pel-imenu-init is identified
;; as an autoload, and it configures the imenu system.
;;
;; Used keys:
;; B f F I f i o p r t
;; <f9> <f10>
;; M-b M-f M-F M-g M-P M-r

;; Initialize PEL special imenu handling
(pel-eval-after-load imenu
  (pel-imenu-init))

(define-pel-global-prefix pel:menu (kbd "<f11> <f10>"))
(define-key pel:menu "B"     #'menu-bar-mode)
(define-key pel:menu "I"     #'imenu-add-menubar-index)
(define-key pel:menu "i"     #'imenu)
(define-key pel:menu "r"      'pel-imenu-rescan)
(define-key pel:menu "t"     #'tmm-menubar)
(define-key pel:menu "o"      'pel-imenu-toggle-follows-order)
(define-key pel:menu "f"      'pel-imenu-toggle-flatten)
(define-key pel:menu "p"      'pel-imenu-toggle-popup)
(define-key pel:menu "R"      'pel-imenu-toggle-auto-rescan)

(when pel-use-imenu+
  (pel-install-github-file "emacsmirror/imenu-plus/master"
                           "imenu+.el" "imenu%2B.el")
  (pel-autoload-file imenu+ for:
                     imenup-add-defs-to-menubar)

  (declare-function imenup-add-defs-to-menubar "ext:imenu+")
  (defun pel--setup-imenu+ ()
    "Activate imenu+ support, protecting against error."
    ;; imenu+ will signal an error if a mode that derives from prog-mode
    ;; does not support imenu.  Prevent this error.
    (ignore-errors
      (imenup-add-defs-to-menubar)))
  (declare-function pel--setup-imenu+ "pel_keys")

  (when (fboundp 'imenup-add-defs-to-menubar)
    (add-hook 'prog-mode-hook 'pel--setup-imenu+)))

;; Although imenu-extra is available through MELPA, that package just provide
;; tools that may be used by other PEL code to incorporate symbol generated
;; by modes into imenu.  It has nothing to autoload.  So instead of using
;; use-package to control its download, the code downloads the file in
;; ~/.emacs.d/utils if requested by the user-option.
;; If you want to get a version of the file newer than the one you have, just
;; delete ~/.emacs.d/utils/imenu-extra.el and restart Emacs.
(when pel-use-imenu-extra
  (pel-install-github-file "redguardtoo/imenu-extra/master"
                           "imenu-extra.el"))

(when pel-use-flimenu
  (pel-ensure-package flimenu from: melpa)
  (define-key pel:menu "f" 'flimenu-mode)
  (define-key pel:menu "F" 'flimenu-global-mode))

(when pel-use-popup-imenu
  ;; No key binding - its used by pel-goto-symbol.
  (cl-eval-when 'load
    (pel-install-github-file "pierre-rouleau/popup-imenu/master" "popup-imenu.el")
    ;; install it's external mandatory dependencies
    (pel-ensure-package dash from: melpa)
    (pel-ensure-package popup from: melpa)
    (pel-ensure-package flx-ido from melpa))
  (pel-autoload-file popup-imenu for: popup-imenu))

(when pel-use-popup-switcher
  ;; (pel-ensure-package popup-switcher from: melpa)
  ;; popup-switcher 2.14 has several bugs I fixed in
  ;; https://github.com/kostafey/popup-switcher/pull/20
  ;; Until this is integrated, we'll use my fork.
  (cl-eval-when 'load
    (pel-install-github-file "pierre-rouleau/popup-switcher/master" "popup-switcher.el")
    ;; install it's mandatory external dependencies
    (pel-ensure-package dash from: melpa)
    (pel-ensure-package popup from: melpa))
  ;; autoload when installed in utils
  (pel-autoload-file popup-switcher for:
                     psw-switch-buffer
                     psw-switch-recentf
                     psw-navigate-files
                     psw-switch-function
                     psw-switch-projectile-files
                     psw-switch-projectile-projects))

;; ---------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> <f2>`` : Customization
;;

;; -- Key bindings
;; Set up the key prefixes.
(define-pel-global-prefix pel:cfg           (kbd "<f11> <f2>"))
(define-pel-global-prefix pel:cfg-pel-lang  (kbd "<f11> <f2> SPC"))
(define-pel-global-prefix pel:cfg-emacs     (kbd "<f11> <f2> E"))
(define-pel-global-prefix pel:cfg-pel       (kbd "<f11> <f2> P"))

(define-key pel:cfg "?" 'pel-setup-info-dual-environment)
(define-key pel:cfg (kbd "M-d") 'pel-setup-dual-environment)

(define-key pel:cfg "c" 'customize)
(define-key pel:cfg "g" 'customize-group)
(define-key pel:cfg "o" 'customize-option)
(define-key pel:cfg "B" 'customize-browse)
(define-key pel:cfg "b" 'pel-browse-group)
(define-key pel:cfg "p" 'pel-customize-pel-base-emacs-group)
(define-key pel:cfg "v" 'customize-save-variable)
(define-key pel:cfg "." 'pel-xref-find-custom-definition-at-line)
;;

(define-key pel:cfg-pel "B" 'pel-browse-pel)
(pel--cfg     ""  pel:cfg-pel "!")  ; all of PEL
;; Key bindings to access PEL customization groups quickly,
;; and optionally other related groups

(when pel-emacs-is-graphic-p
  (define-key pel:cfg-emacs (kbd "C-c") 'pel-customize-cursor))

(pel--cfg-pkg "navigation"  pel:cfg-pel "n" avy)
(pel--cfg-pkg "project-mng" pel:cfg-pel (kbd "<f8>"))

;;
(pel--cfg-pkg "lisp" pel:cfg-pel-lang (kbd "M-L") lispy) ; all Lisps

;; --
(pel--cfg-emacs pel:cfg-emacs "m" "man")
(pel--cfg-emacs pel:cfg-emacs "j" "webjump")
(pel--cfg-emacs pel:cfg-emacs "l" "locate")
(pel--cfg-emacs pel:cfg-emacs "u" "browse-url")
(pel--cfg-emacs pel:cfg-emacs "w" "woman")

(defun pel--setup-for-custom ()
  "PEL setup for Custom-mode."
  (pel-local-set-f12-M-f12 'pel:cfg)
  (when pel-bind-m-dot-to-xref-find-custom-definition
    (local-set-key (kbd "M-.") 'pel-xref-find-custom-definition-at-line))
  (local-set-key (kbd "<f11> X .") 'pel-xref-find-custom-definition-at-line))
(add-hook 'Custom-mode-hook 'pel--setup-for-custom)

;; ---------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> M-I`` : Startup Operation Mode

(define-pel-global-prefix pel:startup     (kbd "<f11> M-S"))
(define-key pel:startup "?" 'pel-setup-info)
(define-key pel:startup "f" 'pel-setup-fast)
(define-key pel:startup "n" 'pel-setup-normal)
(define-key pel:startup (kbd "M-?") 'pel-setup-info-dual-environment)
(define-key pel:startup (kbd "M-d") 'pel-setup-dual-environment)
(when pel-emacs-27-or-later-p
  (define-key pel:startup "q" 'pel-setup-with-quickstart)
  (define-key pel:startup (kbd "M-q") 'pel-setup-no-quickstart))

;; ---------------------------------------------------------------------------
;; Input Completion Framework activation
;; -------------------------------------
;;
;; Activate the mode(s) available, activate the initial mode and provide keys
;; to show current mode and to select a different one. The code supports the
;; following completion modes:
;; - Emacs default (tab completion)
;; - Helm mode
;; - Ido mode
;; - Ivy mode
;; - Ivy mode with Counsel

(define-pel-global-prefix pel:completion (kbd "<f11> M-c"))

(defun pel-number-of-available-modes ()
  "Return number of available modes."
  (let ((count 1))
    (dolist (option '(pel-use-ido
                      pel-use-ivy
                      pel-use-counsel
                      pel-use-helm))
      (when (eval option)
        (setq count (1+ count))))
    count))

;; Helm
;; ----
(when pel-use-helm (pel-ensure-package helm from: melpa)
  (pel-autoload-file helm for: helm-mode)
  (pel-eval-after-load helm
    (require 'helm-config)
    (defvar helm-map)                   ; prevent byte-compiler warning
    ;; <tab> or C-i are mapped to helm-select-action.  Use M-C-i to run
    ;; persistent action.
    (define-key helm-map (kbd "M-C-i") 'helm-execute-persistent-action)
    ;;
    (when pel-use-helm-lsp
      (pel-ensure-package helm-lsp from: melpa))))

;; Ido
;; ---
(when pel-use-ido
  ;; The Ido extenders sometime fail on ido-completions being void.
  ;; to prevent that make sure it is auto-loaded.
  (autoload 'ido-completions "ido")

  ;; When pel-use-ido is set, ensure that ido-mode is autoloaded.
  ;; Ido is distributed with Emacs, so no need to provide logic to install it.
  ;; The selection of the auto-completion mode used when Emacs starts
  ;; is done below, with a call to `pel-set-completion-mode'.
  (defvar ido-common-completion-map)    ; prevent byte-compiler warning
  (defvar ido-buffer-completion-map)    ; prevent byte-compiler warning
  (pel-autoload-file ido for: ido-mode)
  (pel-eval-after-load ido
    ;; Add binding to quickly open the input completion PDF
    (define-key ido-common-completion-map (kbd "<f12><f1>")
      'pel-help-on-completion-input)
    ;; Add key bindings for Ido prompts to complement what is available
    ;; because some bindings are hidden by other keys, like the C-c
    ;; binding to ido-toggle-case or C-p in ido-grid-mode.
    (define-key ido-common-completion-map (kbd "<f12>c") 'ido-toggle-case)
    (define-key ido-common-completion-map (kbd "<f12>p") 'ido-toggle-prefix)
    ;; Add a binding to access the command in terminal mode
    (define-key ido-buffer-completion-map (kbd "<f12>b") 'ido-bury-buffer-at-head))

  (when pel-use-idomenu
    (pel-ensure-package idomenu from: melpa))

  (when pel-use-ido-ubiquitous
    (pel-ensure-package ido-completing-read+ from: melpa)
    ;; add autoloading control for it.  The actual loading is controlled
    ;; by the logic inside pel-completion.el
    (pel-autoload-file ido-completing-read+ for:
                       ido-ubiquitous-mode)
    (pel-eval-after-load ido-completing-read+
      (pel-set-ido-ubiquitous)))

  (when pel-use-ido-grid-mode
    (pel-ensure-package ido-grid-mode from: melpa)
    (pel-autoload-file ido-grid-mode for:
                       ido-grid-mode))

  (when pel-use-ido-grid
    (pel-install-github-file "pierre-rouleau/ido-grid.el/master" "ido-grid.el")
    (pel-autoload-file ido-grid for:
                       ido-grid-enable
                       ido-grid-disable))

  (when pel-use-ido-vertical-mode
    (pel-ensure-package ido-vertical-mode from: melpa)
    (pel-autoload-file ido-vertical-mode for:
                       ido-vertical-mode))

  (when pel-use-smex
    (pel-ensure-package smex from: melpa)
    (pel-autoload-file smex for:
                       smex
                       smex-major-mode-commands)
    (global-set-key (kbd "M-x") 'smex)
    (global-set-key (kbd "M-X") 'smex-major-mode-commands)
    (define-key pel: (kbd "M-x") 'execute-extended-command)))

;; ivy
;; ---
(when pel-use-ivy
  (defvar ivy-use-virtual-buffers)      ; prevent byte-compiler warning
  (defvar ivy-count-format)
  (defvar ivy-minibuffer-map)
  (pel-ensure-package ivy from: melpa)
  (pel-autoload-file ivy for: ivy-mode)
  (pel-eval-after-load ivy
    (setq ivy-use-virtual-buffers t
          ivy-count-format "%d/%d ")
    (define-key ivy-minibuffer-map (kbd "<f12><f1>") 'pel-help-on-completion-input)
    (define-key ivy-minibuffer-map (kbd "<f12>c") 'ivy-toggle-case-fold)
    (define-key ivy-minibuffer-map (kbd "C-SPC") 'ivy-restrict-to-matches)
    (when pel-use-avy
      (define-key ivy-minibuffer-map (kbd "M-H") 'ivy-avy)))

  ;; When ivy and avy are used, activate ivy-avy which integrate both.
  (when pel-use-avy
    (pel-ensure-package ivy-avy from: melpa)
    (pel-autoload-file ivy-avy for: ivy-avy))
  ;;
  (when pel-use-counsel
    (pel-ensure-package counsel from: melpa)
    (pel-require-after-init counsel 1)
    (pel-eval-after-load counsel
      (when pel-system-is-linux-p
        (define-key pel: "A" 'counsel-linux-app)))
    (when (and pel-use-ivy-hydra pel-use-hydra)
      (pel-ensure-package ivy-hydra from: melpa))
    ;;
    (when (and pel-system-is-macos-p pel-use-counsel-osx-app)
      (pel-ensure-package counsel-osx-app from: melpa)
      (pel-autoload-file counsel-osx-app for: counsel-osx-app)
      (define-key pel: "A" 'counsel-osx-app)))

  (when pel-use-lsp-ivy
    (pel-ensure-package lsp-ivy from: melpa)
    ;; TODO: create key bindings for:
    ;; lsp-ivy-workspace-symbol - workspace symbols for the current workspace
    ;; lsp-ivy-global-workspace-symbol - workspace symbols from all of the active workspaces.
    ))

;; flx-ido
;; -------
(when (and pel-use-flx
           (or pel-use-ido pel-use-ivy))
  (pel-ensure-package flx-ido from: melpa)
  ;; Note: flx-ido also explicitly requires the flx package
  (pel-autoload-file flx-ido for: flx-ido-mode))

;; All completion
;; --------------
;; If more than 1 completion mode is available, this means that the user may
;; not want to use Emacs default: activate the one selected by customization
;; and install the selection command.
(when (> (pel-number-of-available-modes) 1)
  ;; Of all input-completion packages, Helm takes the longuest time to load.
  ;; Defer loading of pel-completion a little and allow switching only once
  ;; it's loaded.
  (pel-require-after-init pel-completion 1)
  (pel-eval-after-load pel-completion
    ;; after specified delay configure input completion:
    ;; - set the key bindings
    (define-key pel:completion (kbd "<f4>") 'pel-select-completion-mode)
    (define-key pel:completion "?"          'pel-show-active-completion-mode)
    (when pel-use-ido
      (when pel-use-ido-ubiquitous
        (define-key pel:completion (kbd "M-u") 'pel-ido-ubiquitous)
        (define-key pel:cfg-goto   (kbd "M-u") 'pel-ido-ubiquitous))
      (when (or pel-use-ido-grid-mode
                pel-use-ido-vertical-mode)
        (define-key pel:completion (kbd "M-g") 'pel-select-ido-geometry)
        (define-key pel:cfg-goto   (kbd "M-g") 'pel-select-ido-geometry))
      (when pel-use-flx
        (define-key pel:completion (kbd "M-f") 'pel-flx-ido)
        (define-key pel:cfg-goto   (kbd "M-f") 'pel-flx-ido)))
    ;; - then initialize the mode requested
    (pel-set-completion-mode pel-initial-completion-mode :silent)))

;; ---------------------------------------------------------------------------
;; - Project Management - Projectile

(when pel-use-projectile

  (defun pel--start-projectile ()
    "Activate projectile-mode."
    (when (require 'projectile nil :noerror)
      (projectile-mode +1)))

  (when (and (eq pel-use-projectile 'use-from-start)
             (fboundp 'pel--start-projectile))
    (run-with-idle-timer 1 nil (function pel--start-projectile)))

  (pel-ensure-package projectile from: melpa)
  (pel-autoload-file projectile for: projectile-mode)
  (defvar projectile-mode-map)          ; prevent byte-compiler warning
  (defvar projectile-command-map)
  (pel-eval-after-load projectile
    (define-key projectile-mode-map (kbd "<f8>")    'projectile-command-map)
    (define-key projectile-command-map "~"  'projectile-toggle-project-read-only)
    ;; The default Projectile key-map binds ESC to
    ;; projectile-project-buffers-other-buffer this is unfortunate because it
    ;; prevents the use of any function keys in terminal mode since they are
    ;; implemented by ANSI escape sequences.  Unbind ESC and remap
    ;; projectile-project-buffers-other-buffer to a key that is physically
    ;; closely located to Esc on most keyboards: the 1 key.
    (define-key projectile-command-map (kbd "ESC") nil)
    (define-key projectile-command-map "1"
      'projectile-project-buffers-other-buffer)
    (define-key projectile-command-map (kbd "<f1>") 'pel-help-pdf)
    (define-key projectile-command-map (kbd "<f2>") 'pel-customize-pel)
    (define-key projectile-command-map (kbd "<f3>") 'pel-customize-library)
    (when pel-use-popup-switcher
      (define-key projectile-command-map (kbd "M-f") 'psw-switch-projectile-files)
      (define-key projectile-command-map (kbd "M-p") 'psw-switch-projectile-projects)))
  ;; projectile uses both ripgrep and ag.  These are controlled
  ;; independently but ensure that the commands used by projectile are
  ;; identified as the autoloading commands.  See both of these in the Grep
  ;; operation section below.
  (define-pel-global-prefix pel:projectile (kbd "<f11> <f8>"))
  (define-key pel:projectile (kbd "<f8>") 'projectile-mode)
  ;;
  (when pel-use-treemacs-projectile
    (pel-ensure-package treemacs-projectile from: melpa))  )

;; ---------------------------------------------------------------------------
;; Tempo skeleton - a powerful lisp-style templating system
;; Load pel-tempo when programming languages using it are used.
;; See the use of skeletons in the following sections.

(when (or pel-use-c
          pel-use-c++
          pel-use-common-lisp
          pel-use-erlang
          pel-use-rst-mode)
  (pel-autoload-file pel-tempo for: pel-tempo-mode))

;; ---------------------------------------------------------------------------
;; yasnippet - a Texmate-like templating system

(when pel-use-yasnippet

  (defun pel--start-yasnippet ()
    "Activate yasnippet globally."
    (when (and (require 'yasnippet nil :noerror)
               (fboundp 'yas-global-mode))
      (yas-global-mode 1)))
  (declare-function pel--start-yasnippet "pel_keys") ; prevent warning

  (defun pel--start-yasnippet-snippets ()
    "Activate yasnippet and the yasnippet-snippets globally."
    (load-library "yasnippet-snippets")
    (pel--start-yasnippet))
  (declare-function pel--start-yasnippet-snippets "pel_keys")

  (define-pel-global-prefix pel:yasnippet (kbd "<f11> y"))
  (define-key pel:yasnippet "Y"          'yas-global-mode)
  (define-key pel:yasnippet "y"          'yas-minor-mode)
  (define-key pel:yasnippet "?"          'yas-about)
  (define-key pel:yasnippet "t"          'yas-describe-tables)
  (define-key pel:yasnippet "s"          'yas-insert-snippet)
  (define-key pel:yasnippet "n"          'yas-new-snippet)
  (define-key pel:yasnippet "v"          'yas-visit-snippet-file)

  (when pel-use-yasnippet-snippets
    (pel-ensure-package yasnippet-snippets from: melpa)
    (pel-autoload-file yasnippet-snippets for:
                       yas-global-mode
                       yas-minor-mode)
    (when (eq pel-use-yasnippet 'use-from-start)
      (run-with-idle-timer 4 nil (function pel--start-yasnippet-snippets))))

  (pel-ensure-package yasnippet from: melpa)
  (pel-autoload-file yasnippet for:
                     yas-global-mode
                     yas-minor-mode)
  (when (and (not pel-use-yasnippet-snippets)
             (eq pel-use-yasnippet 'use-from-start))
    (run-with-idle-timer 4 nil (function pel--start-yasnippet))))
;; ---------------------------------------------------------------------------
;; Mode Setting Helper Functions
;; -----------------------------
(defun pel--extend-flymake ()
  "Extend the flymake mode."
  (when (boundp 'flymake-mode-map)
    (define-key flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)
    (define-key flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error)))

(defun pel--extend-flycheck ()
  "Extend the flycheck mode."
  (when (boundp 'flycheck-mode-map)
    (define-key flycheck-mode-map (kbd "M-n") 'flycheck-next-error)
    (define-key flycheck-mode-map (kbd "M-p") 'flycheck-previous-error)
    (define-key flycheck-mode-map (kbd "<f12> e") 'flycheck-list-errors)
    ; '/' is same key as '?' without having to hit Shift
    (define-key flycheck-mode-map (kbd "<f12> /") 'flycheck-explain-error-at-point)))


;; ---------------------------------------------------------------------------
;; Global prefixes to specialized prefixes
;; =======================================
;;
;; All PEL specialized prefixes start with <f11> SPC followed by another
;; character. These characters are listed below.
;; Not all of these are implemented yet, but I'm documented the currently
;; reserved character.  This is for reference and planning.
;;
;; Prefix keys:
;; SPC     - prefix for sub keys
;; C-s     - prefix for Scheme implementation
;;
;; .       - APL
;; 4       - M4
;; A       - Ada
;; C       - C++
;; D       - D
;; E       - Elm
;; F       - FORTRAN
;; G       - Groovy
;; H       - shell script modes
;; J       - Java            -              JVM
;; L       - Common Lisp     - Lisp Family
;; M       - Makefile
;; N       - NetRexx
;; O       - Octave
;; P       - Perl
;; R       - REXX
;; S       - Scala           -              JVM
;; T       - Janet           - Lisp Family
;; U       - Ruby
;; W       - awk
;; a       - AppleScript
;; c       - C
;; d       - Dart
;; e       - Erlang          -              BEAM language
;; f       - Forth
;; g       - Go
;; h       - Haskell
;; i       - Javascript
;; j       - Julia
;; k       - Kotlin          -              JVM
;; l       - Emacs Lisp      - Lisp Family
;; n       - Nim
;; o       - OCaml           - ML Family
;; p       - Python
;; r       - Rust
;; s       - Swift
;; t       - TCL
;; u       - Lua
;; v       - V
;; x       - Elixir          -              BEAM Language
;; y       - Pony
;; z       - Zig
;; C-a     - Arc             - Lisp Family
;; C-c     - CMake
;; C-e     - Eiffel
;; C-f     - Fennel          - Lisp Family, Lua
;; C-g
;; C-h     - Hy              - Lisp Family, Python
;; C-i     -
;; C-j     - Clojure         - Lisp Family, JVM
;; C-l     - LFE             - Lisp Family, BEAM language
;; C-m
;; C-o     - Objective-C
;; C-p     - Pike
;; C-r     - ReasonML        - ML Family
;; C-s C-s - Scheme Family   - Lisp Family
;; C-s C-z - Chez            - Lisp & Scheme Family
;; C-s C-i - Chibi           - Lisp & Scheme Family
;; C-s C-k - Chicken         - Lisp & Scheme Family
;; C-s C-b - Gambit          - Lisp & Scheme Family
;; C-s C-e - Gerbil          - Lisp & Scheme Family
;; C-s C-g - Guile           - Lisp & Scheme Family
;; C-s C-m - MIT/GNU Scheme  - Lisp & Scheme Family
;; C-s C-r - Racket          - Lisp & Scheme Family
;; C-s C-h - Scsh            - Lisp & Scheme Family
;; C-u     - Raku
;; M-a     - AsciiDoc
;; M-c     - Common Workspace Language (CWL)
;; M-g     - GraphViz Dot
;; M-k     - Kconfig
;; M-l     - Outline
;; M-m     - Markdown
;; M-o     - OrgMode
;; M-r     - reStructuredText
;; M-y     - YAML
;; M-s     - SQL
;; M-u     - PlantUML
;; M-A     - Alpaca          -              BEAM Language, Functional/ML
;; M-D     - Dired
;; M-G     - Gleam           -              BEAM Language
;; M-H     - Hamler          -              BEAM Language, Functional/ML/Haskell
;; M-M     - MscGen
;; M-P     - Prolog
;; M-Y     - YANG            - Specification definition language
;;
;; SPC C-l - inferior-lfe-mode
;; SPC b   - ibuffer-mode
;; SPC d d - diff-mode
;; SPC d e - ediff-mode
;; SPC d s - smerge-mode
;; SPC v   - vc-dir-mode
;; SPC s   - shell-mode
;; SPC t   - term-mode
;; SPC z   - shell and terminals

;;          - SNMP MIP
;;          - ANS.1
;; ---------------------------------------------------------------------------
;; Syntax Check with Flycheck (if requested)
;; -----------------------------------------
(when pel-use-flycheck
  (pel-ensure-package flycheck from: melpa)
  (pel-autoload-file flycheck for:
                     flycheck-mode
                     flycheck-select-checker)

  (define-pel-global-prefix pel:flycheck (kbd "<f11> !"))
  (define-key pel:flycheck "!"         'flycheck-mode)
  (define-key pel:flycheck (kbd "M-!") 'global-flycheck-mode)
  (pel-eval-after-load flycheck
    (pel--extend-flycheck)))

;; ---------------------------------------------------------------------------
;; Software Build Tool Support
;; ===========================

;; - CMake support
;; ---------------
;; (use-package cmake-mode)

;; - Makefile editing support
;; --------------------------
;; Nothing needs to be installed as make support is built-in Emacs.

(when pel-use-makefile
  (define-pel-global-prefix pel:for-make (kbd "<f11> SPC M"))

  ;; Add support for the make file modes specified by customization
  (dolist (pattern-mode pel-make-mode-alist)
    (add-to-list 'auto-mode-alist pattern-mode))

  (pel-config-major-mode makefile pel:for-make
    (define-key pel:for-make (kbd "<up>")      'makefile-previous-dependency)
    (define-key pel:for-make (kbd "<down>")    'makefile-next-dependency)
    (define-key pel:for-make (kbd "<M-up>")   #'pel-make-previous-macro)
    (define-key pel:for-make (kbd "<M-down>") #'pel-make-next-macro)
    (define-key pel:for-make "."               'completion-at-point)
    (when (boundp 'makefile-mode-map)
      (let ((map makefile-mode-map))
        (define-key map (kbd "<f6> <right>") 'pel-make-forward-conditional)
        (define-key map (kbd "<f6> <left>")  'pel-make-backward-conditional)
        (define-key map (kbd "<f6> <down>")  'pel-make-outward-forward-conditional)
        (define-key map (kbd "<f6> <up>")    'pel-make-outward-backward-conditional)
        (define-key map (kbd "<f6> o")       'pel-make-conditionals-occur)))))

;; - Tup Built Tool Support
;; ------------------------
(when pel-use-tup
  (pel-install-github-file "pierre-rouleau/tup-mode/master" "tup-mode.el")
  (pel-autoload-file tup-mode for: tup-mode)
  (pel-set-auto-mode tup-mode for:
                     "\\.tup\\'"
                     "Tupfile"
                     "tup.config")
  (pel-config-major-mode tup :no-f12-keys))

;; - Nix Package Manager Support
;; -----------------------------
(when pel-use-nix-mode
  (pel-ensure-package nix-mode from: melpa)
  (add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-mode))
  (pel-config-major-mode nix :no-f12-keys))


;; Intel-Hex object file format support
;; ------------------------------------
(when pel-use-intel-hex-mode
  (pel-ensure-package intel-hex-mode from: melpa)
  (add-to-list 'auto-mode-alist '("\\.hex\\'" . intel-hex-mode))
  (add-to-list 'auto-mode-alist '("\\.a90\\'" . intel-hex-mode))
  (add-to-list 'auto-mode-alist '("\\.a43\\'" . intel-hex-mode))
  (add-to-list 'auto-mode-alist '("\\.ihx\\'" . intel-hex-mode))
  (pel-config-major-mode intel-hex :no-f12-keys))

(when pel-use-elf-mode
  (if (executable-find "readelf")
      (progn
        (pel-install-github-file "pierre-rouleau/elf-mode/master" "elf-mode.el")
        ;; Activate elf-mode from file content, not file name:
        ;; for files that begin with 0x7f followed by 'ELF'.
        (add-to-list 'magic-mode-alist (cons "ELF" 'elf-mode))
        (pel-autoload-file elf-mode for: elf-mode))
    (display-warning :warning
                     "pel-use-elf-mode is on, but can't find readelf executable!")))

;; ---------------------------------------------------------------------------
;; - Programming Language Support
;; ==============================

(when (and pel-use-eldoc-box
           pel-emacs-is-graphic-p)
  (pel-ensure-package eldoc-box from: melpa)
  (pel-autoload-file eldoc-box for:
                     eldoc-box-hover-mode
                     eldoc-box-hover-at-point-mode))

;; ---------------------------------------------------------------------------
;; - AppleScript support
(when pel-use-applescript
  ;; the Melpa package does not seemed maintained. Use my copy instead.
  (pel-install-github-files "pierre-rouleau/apples-mode/master"
                            '("apples-mode.el"
                              "apples-mode/comment"
                              "apples-mode/considering"
                              "apples-mode/considering-application-responses"
                              "apples-mode/display-dialog"
                              "apples-mode/if"
                              "apples-mode/ignoring"
                              "apples-mode/ignoring-application-responses"
                              "apples-mode/on"
                              "apples-mode/repeat"
                              "apples-mode/repeat-until"
                              "apples-mode/repeat-while"
                              "apples-mode/repeat-with"
                              "apples-mode/tell-application"
                              "apples-mode/tell-application-to-activate"
                              "apples-mode/try"
                              "apples-mode/using-terms-from-application"
                              "apples-mode/with-timeout-of-seconds"
                              "apples-mode/with-transaction"))
  (pel-autoload-file apples-mode for:
                     apples-mode
                     apples-open-scratch)
  (add-to-list 'auto-mode-alist '("\\.\\(applescript\\|scpt\\)\\'"
                                  . apples-mode))

  (define-pel-global-prefix pel:for-applescript (kbd "<f11> SPC a"))
  (define-key pel:for-applescript "s" 'apples-open-scratch)
  ;;
  ;; activate the <f12> key binding for apples-mode
  (pel--mode-hook-maybe-call
   (lambda ()
     (pel-local-set-f12 'pel:for-applescript))
   'apples-mode 'apples-mode-hook)


  ;; Text narration on macOS
  ;; -----------------------
  ;; HYDRA: pel-∑narrate is at the bottom of this file with all other PEL hydras.
  (when pel-system-is-macos-p
    (pel-autoload-file pel-applescript for:
                       pel-say
                       pel-say-word
                       pel-say-sentence
                       pel-say-paragraph
                       pel-say-region)
    (when (not pel-use-hydra)
        (define-pel-global-prefix pel:narrate (kbd "<f7> <f8>"))
        (define-key pel:narrate "t" 'pel-say)
        (define-key pel:narrate "R" 'pel-say-region)
        (define-key pel:narrate "w" 'pel-say-word)
        (define-key pel:narrate "s" 'pel-say-sentence)
        (define-key pel:narrate "p" 'pel-say-paragraph))))

;; ---------------------------------------------------------------------------
;; C-like programming languages: C, C++
;; ------------------------------------
(when (and pel-use-c-eldoc
           (or pel-use-c
               pel-use-c++))

  ;; TODO: check if main repo is OK.
  ;; c-eldoc is an external package.
  ;; I am waiting for a fix to be incorporated, using my copy with the fix
  ;; incorporated for now.
  (pel-install-github-file "pierre-rouleau/c-eldoc/master" "c-eldoc.el")
  (defun pel-toggle-c-eldoc-mode ()
    "Toggle c-eldoc mode on/off."
    (interactive)
    (unless (boundp 'eldoc-mode)
      (require 'c-eldoc nil :noerror))
    (if eldoc-mode
        (eldoc-mode -1)
      (c-turn-on-eldoc-mode)))
  (declare-function pel-toggle-c-eldoc-mode "pel_keys")

  (pel-autoload-file c-eldoc for: c-turn-on-eldoc-mode)
  (add-hook 'c-mode-hook 'c-turn-on-eldoc-mode))

;; ---------------------------------------------------------------------------
;; Utility function for mapping CC Mode keys : used by C, C++ and D
(when (or pel-use-c
          pel-use-c++
          pel-use-d)

  ;; Autoload cc-cmds for the c-hungry-delete commands.
  ;; Also autoload c-toggle-hungry-state because it is is used for
  ;; CC Mode compliant modes (see later sections of code, below).
  (pel-autoload-file cc-cmds for:
                     c-context-open-line
                     c-fill-paragraph
                     c-hungry-delete-backwards
                     c-hungry-delete-forward
                     c-toggle-auto-newline
                     c-toggle-comment-style
                     c-toggle-electric-state
                     c-toggle-hungry-state
                     c-toggle-syntactic-indentation)

  (defun pel--map-cc-for (prefix setup-prefix guess-prefix &optional c-preproc-prefix c-search-replace-prefix)
    "Map in the PEL keys for CC Mode in the global keymap specified by PREFIX.
If C-PREPROC-PREFIX also bind the keys for C preprocessor related
commands and sub-keys inside that prefix.  If a key must be
assigned to something different for the programming language just
bind it again after this call."
    ;; guess style
    (define-key guess-prefix "g" 'c-guess-buffer-no-install)
    (define-key guess-prefix "B" 'c-guess-buffer)
    (define-key guess-prefix "G" 'c-guess)
    (define-key guess-prefix "I" 'c-guess-install)
    (define-key guess-prefix "R" 'c-guess-region)
    (define-key guess-prefix "?" 'c-guess-view)

    ;; setup - electric mode control
    (define-key setup-prefix "?"         'pel-cc-mode-info)
    (define-key setup-prefix (kbd "C-i") 'pel-cc-set-indent-width)
    (define-key setup-prefix "e"         'c-toggle-electric-state)
    (define-key setup-prefix "s"         'c-set-style) ; interactively select style
    (define-key setup-prefix "i"         'c-toggle-syntactic-indentation)
    (define-key setup-prefix (kbd "M-;") 'c-toggle-comment-style)
    (define-key setup-prefix (kbd "DEL") 'c-toggle-hungry-state)
    (define-key setup-prefix (kbd "M-RET") 'c-toggle-auto-newline)
    (define-key setup-prefix (kbd "RET")   'pel-cc-change-newline-mode)

    ;; electric mode control
    (define-key prefix (kbd "M-b")  #'subword-mode)
    (define-key prefix (kbd "M-p")  #'superword-mode)
    (define-key prefix (kbd "C-o")   'open-line)
    (define-key prefix      "F"      'c-fill-paragraph)
    (define-key prefix      "f"      'c-display-defun-name)
    ;;
    (define-key prefix (kbd "<down>")  'pel-beginning-of-next-defun)
    (define-key prefix (kbd "<up>")    'beginning-of-defun)
    (define-key prefix (kbd "<left>")  'pel-end-of-previous-defun)
    (define-key prefix (kbd "<right>") 'end-of-defun)
    ;;
    (define-key prefix (kbd "M-9")  #'show-paren-mode)
    ;; reserve "." for a command that find the thing at point in C (via CTags?)
    (define-key prefix      ")"      #'check-parens)
    (when pel-use-rainbow-delimiters
      (define-key prefix (kbd "M-r")  'rainbow-delimiters-mode))
    (when c-preproc-prefix
      (define-key prefix (kbd "M-#")  'hide-ifdef-mode)
      (define-key c-preproc-prefix "#" 'c-macro-expand)
      (define-key c-preproc-prefix "H" 'hide-ifdefs)
      (define-key c-preproc-prefix "S" 'show-ifdefs)
      (define-key c-preproc-prefix "h" 'hide-ifdef-block)
      (define-key c-preproc-prefix "s" 'show-ifdef-block)
      (define-key c-preproc-prefix "d" 'hide-ifdef-define)
      (define-key c-preproc-prefix "u" 'hide-ifdef-undef)
      (define-key c-preproc-prefix "D" 'hide-ifdef-set-define-alist)
      (define-key c-preproc-prefix "U" 'hide-ifdef-use-define-alist)
      (define-key c-preproc-prefix "r" 'hide-ifdef-toggle-read-only)
      (define-key c-preproc-prefix "w" 'hide-ifdef-toggle-shadowing)
      (define-key c-preproc-prefix "C" 'hif-clear-all-ifdef-defined)
      (define-key c-preproc-prefix "e" 'hif-evaluate-macro)
      (define-key c-preproc-prefix "n" 'pel-pp-next-directive)
      (define-key c-preproc-prefix "p" 'pel-pp-prev-directive)
      (define-key c-preproc-prefix "?" 'pel-pp-show-state))
    ;; C/C++ specific commands (not provided to D)
    (when c-search-replace-prefix
      (define-key c-search-replace-prefix "n" 'pel-c-search-equal-NULL)
      (define-key c-search-replace-prefix "N" 'pel-c-search-not-equal-NULL)
      (define-key c-search-replace-prefix "f" 'pel-c-search-equal-false)
      (define-key c-search-replace-prefix "F" 'pel-c-search-not-equal-false)
      (define-key c-search-replace-prefix "t" 'pel-c-search-equal-true)
      (define-key c-search-replace-prefix "T" 'pel-c-search-not-equal-true)
      (define-key c-search-replace-prefix "*" 'pel-c-search-any-comparison-problem)
      (define-key c-search-replace-prefix (kbd "C-f") 'pel-c-fix-comparison-problems)
      (define-key c-search-replace-prefix "#" 'pel-c-search-preproc-if)
      (define-key c-search-replace-prefix "0" 'pel-c-search-preproc-if-set)
      (define-key c-search-replace-prefix (kbd "C-p") 'pel-c-fix-preproc-if-problems)))
  (declare-function pel--map-cc-for "pel_keys")

  (defun pel--setup-for-cc ()
    "More setup for CC modes: add c preprocessor hydra and navigation."
    ;; The pel-⅀c-preproc requires Hydra: load it via the `pel--load-hydra'.
    ;; Note that `pel--load-hydra' removes itself and its presence is used as an
    ;; indication.  So we must check for it being bound and we must NOT use a
    ;; `declare-function' for it even if it was conditionally defined (which
    ;; currently is not the case anyway).
    (when (and pel-use-hydra
               (fboundp 'pel--load-hydra))
      (pel--load-hydra :no-request))
    (let ((map (if (eq major-mode 'c-mode)
                   (when (boundp 'c-mode-map) c-mode-map)
                 (when (boundp 'c++-mode-map) c++-mode-map))))
      (when map
        (define-key map (kbd "<f6> <right>") 'pel-c-preproc-forward-conditional)
        (define-key map (kbd "<f6> <left>")  'pel-c-preproc-backward-conditional)
        (define-key map (kbd "<f6> <down>")  'pel-c-preproc-outward-forward-conditional)
        (define-key map (kbd "<f6> <up>")    'pel-c-preproc-outward-backward-conditional)
        (define-key map (kbd "<f6> o")       'pel-c-preproc-conditionals-occur))))

  (declare-function pel--setup-for-cc "pel_keys")

  (defun pel--set-cc-style (mode bracket-style newline-mode)
    "Set the BRACKET-STYLE and NEWLINE-MODE for MODE.
MODE must be a symbol."
    (if (and (require 'cc-vars nil :no-error)
             (boundp 'c-default-style))
        (progn
          (let* ((used-style  (assoc mode c-default-style))
                 (force-style (not
                               (and used-style
                                    (string-equal (cdr used-style) bracket-style)))))
            (when force-style
              (add-to-list 'c-default-style (cons mode bracket-style))
              (c-set-style bracket-style :dont-override-default)))
          ;; Activate CC mode specific local bindings
          (pel-require 'pel-cc)
          (if (boundp 'pel-cc-newline-mode)
              (setq pel-cc-newline-mode newline-mode)
            (error "Failed loading pel-align!"))
          (local-set-key     (kbd "C-o")   'c-context-open-line)
          (local-set-key     (kbd "RET")   'pel-cc-newline))
      (display-warning 'pel--set-cc-style
                       "Problem loading cc-vars!"
                       :error)))
  (declare-function pel--set-cc-style "pel_keys"))

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; - Function Keys - <f11> - Prefix ``<f11> SPC c`` : C programming utilities

;; Note: C editing is always available in Emacs via the CC Mode and the c-mode
;; that is part of Emacs.  All autoloading is already set by Emacs.  The only
;; extra code needed is to add the specialized menu and then activate it,
;; along with the specialized CC Mode minor modes via the c-mode-hook.

(when pel-use-c
  (define-pel-global-prefix pel:for-c         (kbd "<f11> SPC c"))
  (define-pel-global-prefix pel:c-setup       (kbd "<f11> SPC c <f4>"))
  (define-pel-global-prefix pel:c-guess       (kbd "<f11> SPC c <f4> g"))
  (define-pel-global-prefix pel:for-c-preproc (kbd "<f11> SPC c #"))
  (define-pel-global-prefix pel:c-skel        (kbd "<f11> SPC c <f12>"))
  (define-pel-global-prefix pel:c-search-replace (kbd "<f11> SPC c s"))

  (when pel-use-ini
      (define-key pel:c-setup (kbd "<M-f6>") 'pel-cc-set-file-finder-ini-tool-name))
  (when pel-use-plantuml
    (define-key pel:for-c "u" 'pel-render-commented-plantuml))
  (when pel-use-c-eldoc
    (define-pel-global-prefix pel:c-help (kbd "<f11> SPC c ?"))
    (define-key pel:c-help "?" #'man)
    (define-key pel:c-help "e" 'pel-toggle-c-eldoc-mode))
  (pel--map-cc-for pel:for-c pel:c-setup pel:c-guess pel:for-c-preproc pel:c-search-replace)

  (when pel-use-bison-mode
    (pel-ensure-package bison-mode from: melpa)
    ;; the bison-mode file associates: .y -> bison-mode
    ;;                                 .l -> flex-mode
    ;;                                 .jison -> jison-mode
    ;; add missing associations
    (pel-set-auto-mode bison-mode for: "\\.yacc\\'")
    (pel-set-auto-mode flex-mode for:  "\\.lex\\'")
    ;; and add speedbar support when activated
    (when pel-use-speedbar
      (pel-add-speedbar-extension '(".y"
                                    ".yacc"
                                    ".lex"
                                    ".l"
                                    ".jison"))))

  (defvar c-mode-map)        ; declare dynamic: prevent byte-compiler warnings
  (pel-eval-after-load cc-mode
    (pel-config-major-mode c pel:for-c

      (define-key c-mode-map (kbd "M-;") 'pel-c-comment-dwim)

      ;; Configure how to search for a file name from the user-option
      ;; `pel-c-file-finder-method' which may be specified in a
      ;; .dir-local.el file.
      (pel-cc-find-activate-finder-method pel-c-file-finder-method)

      ;; Configure the CC Mode style for C from PEL custom variables
      ;; 1) set the style: it identifies everything
      (pel--set-cc-style 'c-mode pel-c-bracket-style pel-c-newline-mode)
      ;; 2) apply modifications requested by PEL user options.
      ;;    set variables only available in a CC mode - prevent warnings
      (pel-setq-local c-basic-offset pel-c-indent-width)
      ;; 3) set fill-column to PEL specified C's default if specified
      (when pel-c-fill-column
        (setq-local fill-column pel-c-fill-column))
      ;; 4) Set default auto-newline mode as identified by PEL user option
      (c-toggle-auto-newline (pel-mode-toggle-arg pel-cc-auto-newline))
      ;; 5) Configure M-( to put parentheses after a function name.
      (set (make-local-variable 'parens-require-spaces) nil)
      ;; 6) activate mode specific sub-key prefixes in <f12> and <M-f12>
      (pel-local-set-f12-M-f12 'pel:for-c-preproc "#")
      ;; 7) Install language-specific skeletons
      (pel--install-c-skel pel:c-skel)
      ;; 8) extra setup
      (pel--setup-for-cc))))

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; - Function Keys - <f11> - Prefix ``<f11> SPC C`` : C++ programming utilities

;; Note: C++ editing is always available in Emacs via the CC Mode and the
;; c++-mode that is part of Emacs.  All autoloading is already set by Emacs.
;; The only extra code needed is to add the specialized menu and then activate
;; it, along with the specialized CC Mode minor modes via the c++-mode-hook.

(when pel-use-c++
  (define-pel-global-prefix pel:for-c++         (kbd "<f11> SPC C"))
  (define-pel-global-prefix pel:c++-setup       (kbd "<f11> SPC C <f4>"))
  (define-pel-global-prefix pel:c++-guess       (kbd "<f11> SPC C <f4> g"))
  (define-pel-global-prefix pel:for-c++-preproc (kbd "<f11> SPC C #"))
  (define-pel-global-prefix pel:c++-skel        (kbd "<f11> SPC C <f12>"))
  (define-pel-global-prefix pel:c++-search-replace (kbd "<f11> SPC C s"))

  (when pel-use-speedbar
    ;; Add extensions not already covered by default Emacs code
    (pel-add-speedbar-extension '(".cc"
                                  ".C"
                                  ".CC"
                                  ".hh"
                                  ".HH"
                                  ".ii"
                                  ".inl")))
  (pel-set-auto-mode c++-mode for: "\\.inl\\'") ; not supported by default

  (when pel-use-ini
    (define-key pel:c++-setup (kbd "<M-f6>") 'pel-cc-set-file-finder-ini-tool-name))
  (when pel-use-plantuml
    (define-key pel:for-c++ "u" 'pel-render-commented-plantuml))
  (pel--map-cc-for pel:for-c++ pel:c++-setup pel:c++-guess pel:for-c++-preproc pel:c++-search-replace)

  (pel-eval-after-load cc-mode
    (pel-config-major-mode c++ pel:for-c++

      ;; Configure how to search for a file name from the user-option
      ;; `pel-c++-file-finder-method' which may be specified in a
      ;; .dir-local.el file.
      (pel-cc-find-activate-finder-method pel-c++-file-finder-method)

      ;; "Set the environment for editing C++ files."
      ;; Configure the CC Mode style for C++ from PEL custom variables
      ;; 1) set the style: it identifies everything
      (pel--set-cc-style 'c++-mode pel-c++-bracket-style pel-c++-newline-mode)
      ;; 2)  apply modifications requested by PEL user options.
      ;;     set variables only available in a CC mode - prevent warnings
      (pel-setq c-basic-offset pel-c++-indent-width)
      ;; 3) set fill-column to PEL specified C++'s default if specified
      (when pel-c++-fill-column
        (setq fill-column pel-c++-fill-column))
      ;; 4) Set default auto-newline mode as identified by PEL user option
      (c-toggle-auto-newline (pel-mode-toggle-arg pel-cc-auto-newline))
      ;; 5) Configure M-( to put parentheses after a function name.
      (set (make-local-variable 'parens-require-spaces) nil)
      ;; 6) activate mode specific sub-key prefixes in <f12> and <M-f12>
      (pel-local-set-f12-M-f12 'pel:for-c++-preproc "#")
      ;; 7) Install language-specific skeletons
      (pel--install-c++-skel pel:c++-skel)
      ;; 8) extra setup
      (pel--setup-for-cc))))

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; - Function Keys - <f11> - Prefix ``<f11> SPC D`` : D programming utilities

(when pel-use-d
  (define-pel-global-prefix pel:for-d     (kbd "<f11> SPC D"))
  (define-pel-global-prefix pel:d-setup   (kbd "<f11> SPC D <f4>"))
  (define-pel-global-prefix pel:d-guess   (kbd "<f11> SPC D <f4> g"))

  (pel-ensure-package d-mode from: melpa)
  (pel-autoload-file d-mode for: d-mode)
  ;; When opening a D source code file, load the d-mode feature.
  (add-to-list 'auto-mode-alist '("\\.d[i]?\\'" . d-mode))

  ;; Overcome omission bug in d-mode: add support for Speedbar
  (when pel-use-speedbar
    (pel-add-speedbar-extension ".d"))

  ;; Configure commands available on the D key-map.
  (when pel-use-plantuml
    (define-key pel:for-d "u" 'pel-render-commented-plantuml))
  (pel--map-cc-for pel:for-d pel:d-setup pel:d-guess)

  ;; Configure auto-completion based on selection
  ;; There are 2 possibilities
  (when pel-use-d-ac-dcd
    (pel-ensure-package ac-dcd from: melpa)
    (pel-autoload-file ac-dcd for: ac-dcd-setup)
    (pel-eval-after-load d-mode
      (if (and (require 'auto-complete nil :no-error)
               (boundp 'ac-modes))
          (add-to-list 'ac-modes 'd-mode)
        (display-warning 'pel-use-d-ac-dcd
                         "Failed loading auto-complete: \
d-mode not added to ac-modes!"
                         :error)))
    (add-hook 'd-mode-hook 'ac-dcd-setup))

  (when pel-use-d-company-dcd
    (pel-ensure-package company-dcd from: melpa)
    (pel-autoload-file company-dcd for: company-dcd-mode)
    (add-hook 'd-mode-hook 'company-dcd-mode))

  (pel-eval-after-load d-mode
    (pel-config-major-mode d pel:for-d
      ;; "Set the environment for editing D files."
      ;; Configure the CC Mode style for C++ from PEL custom variables
      ;; 1) set the style: it identifies everything
      (pel--set-cc-style 'd-mode pel-d-bracket-style pel-d-newline-mode)
      ;; 2)  apply modifications requested by PEL user options.
      ;;     set variables only available in a CC mode - prevent warnings
      (pel-setq-local c-basic-offset pel-d-indent-width)
      ;; 3) set fill-column to PEL specified D's default if specified
      (when pel-d-fill-column
        (setq-local fill-column pel-d-fill-column))
      ;; 4) Set default auto-newline mode as identified by PEL user option
      (c-toggle-auto-newline (pel-mode-toggle-arg pel-cc-auto-newline))
      ;; Configure M-( to put parentheses after a function name.
      (set (make-local-variable 'parens-require-spaces) nil)
      ;; 7) Install language-specific skeletons
      ;; TODO
      )))

;; ---------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> SPC f`` : Forth programming
(when pel-use-forth
  (pel-ensure-package forth-mode from: melpa)
  (pel-autoload-file forth-mode for:
                     forth-mode
                     forth-block-mode
                     forth-interaction-mode)
  ;; the <f12> key provides access to help and customization
  (define-pel-global-prefix pel:for-forth (kbd "<f11> SPC f"))
  (define-key pel:for-forth  "z"  'run-forth)
  ;; Activate Forth setup.
  (pel-config-major-mode forth pel:for-forth))

;; ---------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> SPC F`` : FORTRAN programming
;; reserved but not implemented.

;; ---------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> SPC g`` : Go programming
(when pel-use-go
  (when pel-use-go-mode
    ;; go-mode installation
    (pel-ensure-package go-mode from: melpa)
    (pel-autoload-file go-mode for: go-mode)

    ;; goflymake package installation - either using flymake or flycheck
    (cl-eval-when 'load
      (when pel-use-goflymake
        ;; goflymake is a mixed package:
        ;; - it has the Go source:  goflymake/main.go  that Go will compile into
        ;;   the executable stored in a directory that should be on your PATH,
        ;; - the emacs lisp go-flymake.el and go-flycheck.el
        ;; To ensure the Emacs Lisp files are available to Emacs regardless of the
        ;; Go project or workspace used, the Emacs Lisp files are stored in PEL
        ;; utility directory.
        ;; TODO: restore dougm URL once he has accepted by pull-request. In the
        ;;       mean-time I'm using my fork that has code fixes.
        (if (memq pel-use-goflymake '(with-flycheck with-flymake))
            (pel-install-github-file "pierre-rouleau/goflymake/master"
                                     (if (eq pel-use-goflymake 'with-flycheck)
                                         "go-flycheck.el"
                                       "go-flymake.el"))
          (display-warning
           'pel-use-goflymake
           (format "Unsupported pel-use-goflymake value: %S"
                   pel-use-goflymake)
           :error))))

    ;; Setup Go
    (add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))
    ;; Overcome omission bug in go-mode: add support for Speedbar
    (when pel-use-speedbar
      (pel-add-speedbar-extension ".go"))

    (define-pel-global-prefix pel:for-go (kbd "<f11> SPC g"))
    (define-key pel:for-go (kbd "M-s") 'pel-go-toggle-gofmt-on-buffer-save)
    (define-key pel:for-go "?"         'pel-go-setup-info)
    (when pel-use-goflymake
      (define-key pel:for-go "!"       'pel-go-toggle-syntax-checker))

    (pel-eval-after-load go-mode
      ;; Set environment for Go programming using go-mode.
      (pel-config-major-mode go pel:for-go
        ;; ensure gofmt is executed before saving file if
        ;; configured to do so
        (when pel-go-run-gofmt-on-buffer-save
          (add-hook 'before-save-hook  'pel-go-gofmt-on-buffer-save))
        ;; Set the display width of hard tabs used in Go source
        ;; as controlled by the user-option
        (setq-local tab-width pel-go-tab-width)
        (when pel-use-goflymake
          ;; Activate flycheck or flymake if requested
          (cond
           ((eq pel-use-goflymake 'with-flycheck) (pel-require 'go-flycheck))
           ((eq pel-use-goflymake 'with-flymake)  (pel-require 'go-flymake))
           (t
            (error "Unsupported pel-use-goflymake value: %S"
                   pel-use-goflymake))))))))

;; ---------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> SPC i`` : Javascript programming
(when pel-use-javascript
  (define-pel-global-prefix pel:for-javascript  (kbd "<f11> SPC i"))
  (add-to-list 'auto-mode-alist (cons "\\.js\\'"
                                      (if (eq pel-use-javascript 'js-mode)
                                          'js-mode
                                        'js2-mode)))
  (cond
   ((eq pel-use-javascript 'js2-mode)
    (pel-ensure-package js2-mode from: melpa)
    (pel-autoload-file js2-mode for: js2-mode)
    (if (version< emacs-version "27.1")
        (progn
          (add-to-list 'auto-mode-alist '("\\.jsx?\\'" . js2-jsx-mode))
          (add-to-list 'interpreter-mode-alist '("node" . js2-jsx-mode)))
      (add-hook 'js-mode-hook 'js2-minor-mode))
    ;; Experimental ...
    (define-key pel:for-javascript "." 'js2-find-node-at-point)
    (define-key pel:for-javascript "?" 'js2-node-name-at-point)
    (define-key pel:for-javascript "j" 'js2-print-json-path)
    (define-key pel:for-javascript (kbd "<right>") 'js2-forward-sws)
    (define-key pel:for-javascript (kbd "<left>") 'js2-backward-sws)
    (define-key pel:for-javascript (kbd "TAB") 'js2-indent-bounce)
    (define-key pel:for-javascript (kbd "<backtab>") 'js2-indent-bounce-backward)
    ;; js2-display-error-list
    ;; js2-error-buffer-mode
    ;; js2-error-buffer-next
    ;; js2-error-buffer-prev and some more...
    (pel-config-major-mode javascript pel:for-javascript))
   ;;
   ((eq pel-use-javascript 'js-mode)
    ;; Use the built-in js.el
    (pel-autoload-file js for: js-mode)
    (pel-config-major-mode js pel:for-javascript))))

;; ---------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> SPC j`` : Julia programming
(when (and pel-use-julia pel-use-vterm)
  ;; 🚧 Experimental: not yet completed.
  ;; For Julia, the julia-snail package uses julia-mode and
  ;; other required package.
  ;; Note that it also requires the vterm package.
  (pel-ensure-package julia-snail from: melpa)
  (pel-autoload-file julia-snail for:
                     julia-mode
                     julia-snail
                     julia-snail-mode)
  (define-pel-global-prefix pel:for-julia (kbd "<f11> SPC j"))
  (define-key pel:for-julia  "z" #'julia-snail)
  (pel-config-major-mode julia pel:for-julia
    (if (fboundp 'julia-snail-mode)
        (julia-snail-mode 1)
      (display-warning 'pel-use-julia
                       "Cannot load julia-nail-mode"
                       :error))))

;; ---------------------------------------------------------------------------
;; Lisp-style programming Languages
;; --------------------------------

(when pel-use-lispy
  (pel-ensure-package lispy from: melpa)

  (defun pel--activate-lispy ()
    "Activate lispy lazily."
    (if (and (require 'pel-lispy nil :no-error)
             (fboundp 'pel-lispy-mode))
        (pel-lispy-mode)
      (display-warning
       'pel-lispy
       "Failed loading pel-lispy which controls the use of lispy within PEL.
  Verify your PEL installation: refer to PEL manual."
       :error)))
  (declare-function pel--activate-lispy "pel_keys")

  ;; Setup activation of Lispy for specified major modes that are allowed.
  (pel-add-hook-for 'pel-modes-activating-lispy
                    #'pel--activate-lispy
                    pel-allowed-modes-for-lispy)

  ;; Control some keys in the Lispy keyboard map.
  (pel-eval-after-load lispy
    ;; Update lispy key-map according to PEL user-options.
    (if (boundp 'lispy-mode-map)
        (unless pel-enable-lispy-meta-return
          (define-key lispy-mode-map (kbd "M-RET") nil))
      (display-warning 'pel-lispy
                       "The lispy-mode-map is not bound.
  Cannot disable lispy-meta-return binding to M-RET!"
                       :error)))

  ;; The pel-lispy file controls the loading of lispy.
  (pel-autoload-file lispy for: lispy-mode)
  (pel-autoload-file pel-lispy for:
                     pel-lispy-mode
                     lispy-describe-inline
                     lispy-arglist-inline))

(defun pel--lisp-languages-map-for (prefix)
  "Map in the PEL keys for Lisp-like mode in the keymap for PREFIX."
  (define-key prefix (kbd "<down>")   'pel-elisp-beginning-of-next-form)
  (define-key prefix (kbd "<up>")     'pel-elisp-beginning-of-previous-form)
  (define-key prefix (kbd "<M-down>") 'pel-elisp-beginning-of-next-defun)
  (define-key prefix (kbd "<M-up>")   'pel-elisp-beginning-of-previous-defun)
  ;;
  (define-key prefix (kbd "<C-down>")   'pel-elisp-to-name-of-next-form)
  (define-key prefix (kbd "<C-up>")     'pel-elisp-to-name-of-previous-form)
  (define-key prefix (kbd "<C-M-down>") 'pel-elisp-to-name-of-next-defun)
  (define-key prefix (kbd "<C-M-up>")   'pel-elisp-to-name-of-previous-defun)
  ;;
  (define-key prefix (kbd "<left>")  'pel-end-of-previous-defun)
  (define-key prefix (kbd "<right>") 'end-of-defun)
  ;;
  (define-key prefix   (kbd "M-9") #'show-paren-mode)
  (define-key prefix   (kbd "M-p") #'superword-mode)
  (define-key prefix   ")"         #'check-parens)
  ;;
  (when pel-use-parinfer
    (define-key prefix (kbd "M-i")  'parinfer-mode)
    (define-key prefix (kbd "M-I")  'parinfer-toggle-mode))
  (when pel-use-rainbow-delimiters
    (define-key prefix (kbd "M-r")  'rainbow-delimiters-mode))
  (define-key prefix   (kbd "M-s") #'semantic-mode)
  (define-key prefix (kbd "M-n") 'pel-elisp-set-navigate-target-form)
  (define-key prefix (kbd "M-N") 'pel-toggle-paren-in-column-0-is-defun-start)
  (when pel-use-lispy
    (define-key prefix (kbd "M-L") 'pel-lispy-mode)
    (define-key prefix "1"         'lispy-describe-inline)
    (define-key prefix "2"         'lispy-arglist-inline)
    (define-key prefix "3"         'lispy-right)
    (define-key prefix "4"         'lispy-x)
    (define-key prefix "7"         'lispy-cursor-down)
    (define-key prefix "8"         'lispy-parens-down)
    (define-key prefix "9"         'lispy-out-forward-newline)
    (define-key prefix (kbd "DEL") 'lispy-kill-at-point)))

;; ---------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> SPC l`` : Emacs Lisp programming

;; - Use parinfer
;; --------------
(when pel-use-parinfer
  ;; parinfer was removed from MELPA, if you have an old copy in your elpa-attic
  ;; set pel-use-parinfer to use-pel-elpa-attic-copy otherwise PEL will extract it
  ;; from the emacsattic.
  (cl-eval-when 'load
    (or (and (eq pel-use-parinfer 'use-pel-elpa-attic-copy)
             (pel-install-from-elpa-attic "parinfer"))
        (pel-install-github-files "emacsattic/parinfer/master" '("parinfer.el"
                                                                 "parinferlib.el"
                                                                 "parinfer-ext.el"))))
  (pel-autoload-file parinfer for:
                     parinfer-mode
                     parinfer-toggle-mode
                     parinfer-auto-fix
                     parinfer-diff))

;; - Use rainbow-delimiters
;; ------------------------
(when pel-use-rainbow-delimiters
  (pel-ensure-package rainbow-delimiters from: melpa)
  (pel-autoload-file rainbow-delimiters for:
                     rainbow-delimiters-mode))

;; rainbow-delimiters-max-face-count identifies max depth where colours
;; are cycled.  Its default value is 9.  That should be more than enough.
;; The color of the parentheses are identified by the user option variables
;; rainbow-delimiters-depth-X-face  (where 'X' is a digit between 1 and
;; 9 included.) Customize these user option variables.

(define-pel-global-prefix pel:for-elisp  (kbd "<f11> SPC l"))
(define-pel-global-prefix pel:elisp-skel (kbd "<f11> SPC l <f12>"))

(define-key pel:for-elisp "z"  'ielm)
(define-key pel:for-elisp "D"  'pel-add-dir-to-loadpath)
(pel--lisp-languages-map-for pel:for-elisp)
(define-key pel:for-elisp  (kbd "M-l")  'pel-toggle-lisp-modes)
;;
(define-key pel:for-elisp   "."  'pel-find-thing-at-point)
(define-key pel:for-elisp   "t"  'pel-run-ert)
(when pel-use-plantuml
  (define-key pel:for-elisp   "u"  'pel-render-commented-plantuml))
(when pel-use-parinfer
  (define-key pel:for-elisp "i" 'parinfer-auto-fix))

(define-pel-global-prefix pel:elisp-help (kbd "<f11> SPC l ?"))
(define-key pel:elisp-help "e" 'eldoc-mode)
(when (and pel-use-eldoc-box
           pel-emacs-is-graphic-p)
  (define-key pel:elisp-help "b" 'eldoc-box-hover-mode)
  (define-key pel:elisp-help "B" 'eldoc-box-hover-at-point-mode))

(define-pel-global-prefix pel:elisp-analyze (kbd "<f11> SPC l a"))
(define-key pel:elisp-analyze ")" #'check-parens)
(when pel-use-parinfer
  (define-key pel:elisp-analyze "D"  'parinfer-diff))
(define-key pel:elisp-analyze   "b"  'pel-lint-elisp-file)
(define-key pel:elisp-analyze   "d" #'checkdoc)
(define-key pel:elisp-analyze   "f" #'elint-file)

(define-pel-global-prefix pel:elisp-compile (kbd "<f11> SPC l c"))
(define-key pel:for-elisp (kbd "M-c") 'pel-byte-compile-file-and-load)
(define-key pel:elisp-compile "b"  'pel-byte-compile-file-and-load)
(define-key pel:elisp-compile "d" #'byte-recompile-directory)
(define-key pel:elisp-compile "f" #'byte-compile-file)
(define-key pel:elisp-compile "a" #'disassemble)

(define-pel-global-prefix pel:elisp-debug (kbd "<f11> SPC l d"))
(define-key pel:elisp-debug "f" #'debug-on-entry)
(define-key pel:elisp-debug "F" #'cancel-debug-on-entry)
(define-key pel:elisp-debug "v" #'debug-on-variable-change)
(define-key pel:elisp-debug "V" #'cancel-debug-on-variable-change)
(define-key pel:elisp-debug "!" #'toggle-debug-on-error)
(define-key pel:elisp-debug ")" #'toggle-debug-on-quit)
(define-key pel:elisp-debug "e"  'edebug-defun)

(define-pel-global-prefix pel:elisp-eval (kbd "<f11> SPC l e"))
(define-key pel:elisp-eval "b" #'eval-buffer)
(define-key pel:elisp-eval "r" #'eval-region)

(define-pel-global-prefix pel:elisp-function (kbd "<f11> SPC l f"))
(define-key pel:elisp-function "n" 'pel-elisp-beginning-of-next-defun)
(define-key pel:elisp-function "p" 'pel-elisp-beginning-of-previous-defun)

(define-pel-global-prefix pel:elisp-lib (kbd "<f11> SPC l l"))
(define-key pel:elisp-lib "f" #'load-file)
(define-key pel:elisp-lib "v"  'pel-load-visited-file)
(define-key pel:elisp-lib "l" #'find-library)  ; Open the elisp library file
(define-key pel:elisp-lib "L" #'load-library)  ; Load an elisp library file.
(define-key pel:elisp-lib "c" #'locate-library)
(define-key pel:elisp-lib "p" #'list-packages)

(when pel-use-macrostep
  (pel-ensure-package macrostep from: melpa)
  (pel-autoload-file macrostep for: macrostep-expand)
  (define-key pel:for-elisp  (kbd "M-m") #'macrostep-expand))

(when pel-use-highlight-defined
  (pel-ensure-package highlight-defined from: melpa)
  (pel-autoload-file highlight-defined for: highlight-defined-mode)
  (define-key pel:for-elisp  (kbd "M-d") 'highlight-defined-mode))

(when pel-use-eros
  (pel-ensure-package eros from: melpa)
  (pel-autoload-file eros for: eros-mode)
  (define-key pel:for-elisp "E" 'eros-mode))

(when pel-use-suggest
  (pel-ensure-package suggest from: melpa)
  (pel-autoload-file suggest for: suggest)
  (define-key pel:for-elisp "S" 'suggest))

;; activate the <f12> key binding for elisp-mode
(pel-check-minor-modes-in pel-elisp-activates-minor-modes)
(pel--mode-hook-maybe-call
 (lambda ()
   (pel-local-set-f12-M-f12 'pel:for-elisp)
   (pel-local-set-f12-M-f12 'pel:elisp-analyze  "a")
   (pel-local-set-f12-M-f12 'pel:elisp-compile  "c")
   (pel-local-set-f12-M-f12 'pel:elisp-debug    "d")
   (pel-local-set-f12-M-f12 'pel:elisp-eval     "e")
   (pel-local-set-f12-M-f12 'pel:elisp-function "f")
   (pel-local-set-f12-M-f12 'pel:elisp-lib      "l")
   (pel--install-elisp-skel pel:elisp-skel)
   (pel-turn-on-local-minor-modes-in 'pel-elisp-activates-minor-modes))
 'emacs-lisp-mode 'emacs-lisp-mode-hook :append)

(when (or pel-use-elisp-refs       ; it will be set if pel-use-helpful
          pel-use-helpful)         ; is set, but it's done below
  (define-pel-global-prefix pel:elisp-refs (kbd "<f11> SPC l r"))
  (define-key pel:elisp-refs "f" 'elisp-refs-function)
  (define-key pel:elisp-refs "m" 'elisp-refs-macro)
  (define-key pel:elisp-refs "v" 'elisp-refs-variable)
  (define-key pel:elisp-refs "s" 'elisp-refs-special)
  (define-key pel:elisp-refs "o" 'elisp-refs-symbol))

;; ---------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> SPC L`` : Common Lisp
(when pel-use-common-lisp
  (defvar pel-lisp-imenu-generic-expression nil
    "Cache copy for the PEL computed imenu index rule for Common Lisp.")

  (when (and pel-inferior-lisp-program
             (boundp 'inferior-lisp-program))
    (setq inferior-lisp-program pel-inferior-lisp-program))

  ;; Install Slime or Sly, not both.
  ;; TODO: enhance PEL package management and options to support
  ;;       selections of packages by choice better.
  (cond
   ;; Use Slime
   ((and pel-use-slime
         (eq pel-clisp-ide 'slime))
    (pel-ensure-package slime from: melpa))
   ;; Use SLY
   ((and pel-use-sly
         (eq pel-clisp-ide 'sly))
    (pel-ensure-package sly from: melpa)))

  ;; Add support for Speedbar listing Common Lisp files:
  (when pel-use-speedbar
    (pel-add-speedbar-extension ".li?sp")
    (dolist (ext-regexp pel-clisp-extra-files)
      (pel-add-speedbar-extension ext-regexp)))
  ;; Add extra Common Lisp file extensions if requested by user
  (dolist (ext-regexp pel-clisp-extra-files)
    (add-to-list 'auto-mode-alist ext-regexp))

  (define-pel-global-prefix pel:for-lisp (kbd "<f11> SPC L"))
  (define-pel-global-prefix pel:lisp-skel (kbd "<f11> SPC L <f12>"))
  ;; (define-pel-global-prefix pel:for-lisp-repl (kbd "<f11> SPC z L")) Future <f12> key right inside the REPL.
  (pel--lisp-languages-map-for pel:for-lisp)
  (when pel-use-plantuml
    (define-key pel:for-lisp "u" 'pel-render-commented-plantuml))
  (define-key pel:for-lisp "z" 'pel-cl-repl)
  (define-key pel:for-lisp "?" 'pel-cl-hyperspec-lookup)

  ;; Enable use of the Common Lisp Hyperspec by setting their location.
  ;; Customize `pel-clisp-hyperspec-root' if you want to use a local copy.
  (pel-setq common-lisp-hyperspec-root
            (pel-expand-url-file-name pel-clisp-hyperspec-root))

  (pel-config-major-mode lisp pel:for-lisp
    (pel-local-set-f12-M-f12 'pel:elisp-function "f")
    (pel--install-clisp-skel pel:lisp-skel)
    ;;
    ;; TODO: Add keys for Common Lisp Skeletons
                                        ;(pel--install-clisp-skel pel:lisp-skel)
    ;;
    ;; Add key that can add symbols for imenu parsing
    (local-set-key (kbd "M-g <f4> .") 'pel-cl-add-symbol-to-imenu)
    ;;
    ;; Common Lisp Style
    ;; Adjust fill-column if specified by user-option
    (when pel-clisp-fill-column
      (setq fill-column pel-clisp-fill-column))
    ;; Ensure that pel-separator-line uses 3 semicolons.
    (setq-local pel-comment-prefix ";;;")
    ;;
    ;; Common Lisp indentation rules differ from Emacs Lisp indentation rules:
    ;; - for Common Lisp buffers, use common-lisp-indent-function as indenter,
    ;;   replacing the default indenter (which conforms to the Emacs Lisp
    ;;   indentation rules).
    ;; NOTE: this code is already done by slime-setup, so this is therefore
    ;; not required when Slime is used.
    (unless pel-use-slime
      (set (make-local-variable 'lisp-indent-function)
           'common-lisp-indent-function))
    ;; When Slime is used and extra slime contributions are identified
    ;; activate them.
    (when (and pel-use-slime
               (listp pel-use-slime)
               (fboundp 'slime-setup))
      (slime-setup pel-use-slime))
    ;; imenu support: add ability to extract more Common Lisp definitions.
    ;; compute it once after a pel-init (instead of on each file opened).
    (when (boundp 'lisp-imenu-generic-expression)
      (when (and (boundp 'lisp-mode-symbol-regexp)
                 (not pel-lisp-imenu-generic-expression))
        (pel-add-imenu-sections-to pel-clisp-define-forms
                                   'lisp-imenu-generic-expression)
        (setq pel-lisp-imenu-generic-expression
              lisp-imenu-generic-expression))
      (setq-local imenu-generic-expression lisp-imenu-generic-expression))))

;; ---------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> SPC C-a`` : Arc
(when pel-use-arc
  (pel-autoload-file arc for: arc-mode)
  (pel-autoload-file inferior-arc for: run-arc)
  (pel-install-github-files "arclanguage/anarki/master/extras"
                            '("arc.el"
                              "inferior-arc.el"))
  ;; associate .arc file with arc-mode
  (add-to-list 'auto-mode-alist '("\\.arc\\'" . arc-mode))
  (when pel-use-speedbar
    (pel-add-speedbar-extension ".arc"))

  (define-pel-global-prefix pel:for-arc (kbd "<f11> SPC C-a"))
  (pel--lisp-languages-map-for pel:for-arc)

  ;; activate the <f12> key binding for arc-mode
  (pel-config-major-mode arc pel:for-arc))

;; ---------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> SPC T`` : Janet
(when pel-use-janet

  ;; Installation
  (when pel-use-janet-mode
    ;; (pel-ensure-package janet-mode from: melpa)
    ;; Use my version of janet-mode: it's ahead of the MELPA available one.
    (pel-install-github-file "pierre-rouleau/janet-mode/master/"
                             "janet-mode.el")
    (pel-autoload-file janet-mode for:
                       janet-mode)
    (add-to-list 'auto-mode-alist '("\\.janet\\'" . janet-mode))
    (add-to-list 'interpreter-mode-alist '("janet" . janet-mode)))

  (when pel-use-ijanet-mode
    (pel-install-github-file "SerialDev/ijanet-mode/master/"
                             "ijanet.el")
    (pel-autoload-file ijanet for:
                       ijanet))
  (when pel-use-inf-janet
    (pel-install-github-file "velkyel/inf-janet/master"
                             "inf-janet.el")
    (pel-autoload-file inf-janet for:
                       inf-janet-mode
                       inf-janet))
  ;; Speedbar support
  ;; TODO: add imenu support to allow detection of forms
  (when pel-use-speedbar
    (pel-add-speedbar-extension ".janet"))

  ;; Key Bindings
  (define-pel-global-prefix pel:for-janet (kbd "<f11> SPC T"))
  (pel--lisp-languages-map-for pel:for-janet)
  (pel-config-major-mode janet pel:for-janet))

;; ---------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> SPC C-j`` : Clojure
(when pel-use-clojure
  ;; Installation
  (pel-ensure-package clojure-mode from: melpa)
  (pel-autoload-file clojure-mode for: clojure-mode)

  (define-pel-global-prefix pel:for-clojure (kbd "<f11> SPC C-j"))
  (pel--lisp-languages-map-for pel:for-clojure)

  (when pel-use-cider
    (pel-ensure-package cider from: melpa)
    (pel-autoload-file cider for:
                       cider-jack-in
                       cider-connect
                       cider-connect-cljs)
    (define-key pel:for-clojure "j" 'cider-jack-in)
    (define-key pel:for-clojure "c" 'cider-connect)
    (define-key pel:for-clojure "C" 'cider-connect-cljs))

  ;; Activate Yasnippets for Clojure if requested.
  ;; Load the package when Yasnippet starts.
  (when (and  pel-use-clojure-snippets
              pel-use-yasnippet)
    (pel-ensure-package clojure-snippets from: melpa)
    (pel-autoload-file clojure-snippets for:
                       yas-global-mode
                       yas-minor-mode))

  (when pel-use-clj-refactor
    (pel-ensure-package clj-refactor from: melpa))

  ;; activate the <f12> key binding for clojure-mode
  (pel-config-major-mode clojure pel:for-clojure
    (when pel-use-clj-refactor
      ;; Activate clj-refactor and optionally Yasnippet
      (if (and (fboundp 'clj-refactor-mode)
               (fboundp 'cljr-add-keybindings-with-prefix))
          (progn
            (clj-refactor-mode 1)
            (when (and pel-use-yasnippet
                       (fboundp 'yas-minor-mode))
              ;; for adding require/use/import statements
              (yas-minor-mode 1))
            ;; This choice of keybinding leaves cider-macroexpand-1 unbound
            (cljr-add-keybindings-with-prefix "C-c C-m"))
        (display-warning 'pel-clojure
                         "clj-refactor not properly loaded"
                         :error)))))

;; ---------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> SPC C-s C-s`` : Scheme Family
;; IMPORTANT:
;; From EMacs implementation point of view Scheme is a language family that
;; includes the following Scheme dialects:
;;                                         - Chez Scheme
;;                                         - Chicken Scheme
;;                                         - Gambit Scheme
;;                                         - Gerbil Scheme
;;                                         - Guile Scheme
;;                                         - Racket Scheme
;;                                         - Scsh Scheme
;; When any of these languages are activated by their corresponding PEL user
;; option, then the pel-use-scheme user-option is also forced on by the logic
;; at the end of pel--option.el.  These activate the ability to activate other
;; packages that can be used with the Scheme dialects.
;;
;; The scheme-mode and its file associations are supported by Emacs. That also
;; supports the Scheme dialects.  If the PEL user-option for the Scheme
;; dialect is not activated but pel-use-scheme is activated, then only the
;; default support provided by Emacs is available for that dialect.
;;
;; To ensure that the explicit Scheme dialect mode is associated with the
;; files, then the logic for the Scheme dialects is done *after* the logic for
;; default Scheme.

(when pel-use-scheme
  ;; Install requested Scheme Family options
  (when pel-use-geiser
    (pel-ensure-package geiser from: melpa)
    (pel-autoload-file geiser for:
                       geiser
                       geiser-mode)
    ;; Geiser extensions
    (when pel-use-macrostep-geiser
      ;; (pel-ensure-package macrostep-geiser from: melpa)
      ;; This has a bug for supporting Emacs prior to Emacs 27.  I submitted a
      ;; fix: https://github.com/nbfalcon/macrostep-geiser/pull/1
      ;; Using my implementation until author integrates my fix.
      ;; TODO: change to melpa above once my fix is integrated?
      (pel-install-github-file "pierre-rouleau/macrostep-geiser/master"
                               "macrostep-geiser.el")
      (pel-autoload-file macrostep-geiser for:
                         macrostep-geiser-setup)
      ;;
      (pel-eval-after-load geiser-mode
        (if (and (require 'macrostep-geiser nil :no-error)
                 (fboundp 'macrostep-geiser-setup))
            (add-hook 'geiser-mode-hook (function macrostep-geiser-setup))
          (display-warning 'pel-use-macrostep-geiser
                           "Can't load macrostep-geiser" :error)))
      (pel-eval-after-load geiser-repl
        (if (and (require 'macrostep-geiser nil :no-error)
                 (fboundp 'macrostep-geiser-setup))
            (add-hook 'geiser-repl-mode-hook (function
                                              macrostep-geiser-setup))
          (display-warning 'pel-use-macrostep-geiser
                           "Can't load macrostep-geiser" :error))))
    (when pel-use-ac-geiser
      (pel-ensure-package ac-geiser from: melpa)
      (add-hook 'geiser-mode-hook 'ac-geiser-setup)
      (add-hook 'geiser-repl-mode-hook 'ac-geiser-setup)
      (pel-eval-after-load auto-complete
        (if (and (require 'geiser nil :no-error)
                 (fboundp 'geiser-repl-mode)
                 (boundp 'ac-modes))
            (add-to-list 'ac-modes (function geiser-repl-mode))
          (display-warning 'pel-use-ac-geiser
                           (format "\
Can't load ac-geiser: geiser-repl-mode: %S"
                                   (if (fboundp 'geiser-repl-mode)
                                       "bound"
                                     "not bound!")) :error))))
    ;; Geiser Scheme implementation extensions
    (when pel-use-geiser-chez
      (pel-ensure-package geiser-chez from: melpa)
      (with-eval-after-load 'geiser-mode
        (require 'geiser-chez)))
    (when pel-use-geiser-chibi
      (pel-ensure-package geiser-chibi from: melpa)
      (with-eval-after-load 'geiser-mode
        (require 'geiser-chibi)))
    (when pel-use-geiser-chicken
      (pel-ensure-package geiser-chicken from: melpa)
      (with-eval-after-load 'geiser-mode
        (require 'geiser-chicken)))
    (when pel-use-geiser-gambit
      (pel-ensure-package geiser-gambit from: melpa)
      (with-eval-after-load 'geiser-mode
        (require 'geiser-gambit)))
    (when pel-use-geiser-guile
      (pel-ensure-package geiser-guile from: melpa)
      (with-eval-after-load 'geiser-mode
        (require 'geiser-guile)))
    (when pel-use-geiser-mit
      (pel-ensure-package geiser-mit from: melpa)
      (with-eval-after-load 'geiser-mode
        (require 'geiser-mit)))
    (when pel-use-geiser-racket
      (pel-ensure-package geiser-racket from: melpa)
      (with-eval-after-load 'geiser-mode
        (require 'geiser-racket))))

  (when pel-use-quack
    ;; I have fixed byte-compiler warnings in quack in a fork of emacsmirror/quack
    ;; Since that repo is read-only I contacted the author and wait for his reply.
    ;; In the mean time, I use my fork.
    (pel-install-github-file "pierre-rouleau/quack/master" "quack.el")
    (pel-autoload-file quack for:
                       quack-kill-current-buffer
                       quack-uncomment-region
                       quack-backward-sexp
                       quack-browse-quack-web-page
                       quack-w3m-browse-url-other-window
                       quack-about
                       quack-dired-pltcollect
                       quack-find-file
                       quack-newline
                       quack-insert-closing-paren
                       quack-insert-closing-bracket
                       quack-insert-opening-paren
                       quack-insert-opening-bracket
                       quack-toggle-lambda
                       quack-tidy-buffer
                       quack-update-srfi-index
                       quack-view-srfi
                       quack-view-manual
                       quack-view-keyword-docs
                       quack-customize
                       quack-set-other-default-program
                       quack-pltfile-mode
                       quack-pltfile-raw
                       quack-pltfile-quit))

  ;; Just activate the <f12> key for Scheme.
  (define-pel-global-prefix pel:for-scheme (kbd "<f11> SPC C-s C-s"))
  (pel--lisp-languages-map-for pel:for-scheme)

  ;; activate the <f12> key binding for scheme-mode
  (pel-config-major-mode scheme pel:for-scheme)

  ;; ---------------------------------------------------------------------------
  ;; - Function Keys - <f11> - Prefix ``<f11> SPC C-s C-z`` : Chez
  (when pel-use-chez
    (define-pel-global-prefix pel:for-chez (kbd "<f11> SPC C-s C-z"))
    (pel--lisp-languages-map-for pel:for-chez)
    (define-key pel:for-chez "z" 'pel-chez-repl)
    (define-key pel:for-chez (kbd "C-l") 'pel-clear-scheme-repl-buffer)
    ;; activate the <f12> key binding for chez-mode
    (pel-config-major-mode chez pel:for-chez))

  ;; ---------------------------------------------------------------------------
  ;; - Function Keys - <f11> - Prefix ``<f11> SPC C-s C-i`` : Chibi

  (when pel-use-chibi
    (define-pel-global-prefix pel:for-chibi (kbd "<f11> SPC C-s C-i"))
    (pel--lisp-languages-map-for pel:for-chibi)
    (define-key pel:for-chibi "z" 'pel-chibi-repl)
    (define-key pel:for-chibi (kbd "C-l") 'pel-clear-scheme-repl-buffer)
    ;; activate the <f12> key binding for chibi-mode
    (pel-config-major-mode chibi pel:for-chibi))

  ;; ---------------------------------------------------------------------------
  ;; - Function Keys - <f11> - Prefix ``<f11> SPC C-s C-k`` : Chicken

  (when pel-use-chicken
    (define-pel-global-prefix pel:for-chicken (kbd "<f11> SPC C-s C-k"))
    (pel--lisp-languages-map-for pel:for-chicken)
    (define-key pel:for-chicken "z" 'pel-chicken-repl)
    (define-key pel:for-chicken (kbd "C-l") 'pel-clear-scheme-repl-buffer)
    ;; activate the <f12> key binding for chicken-mode
    (pel-config-major-mode chicken pel:for-chicken))

  ;; ---------------------------------------------------------------------------
  ;; - Function Keys - <f11> - Prefix ``<f11> SPC C-s C-b`` : Gambit
  ;; Note: PEL ensures that pel-use-gambit is set when pel-use-gerbil is set.
  (when pel-use-gambit

    (defun pel--set-scheme-repl (repl)
      "Set the Scheme REPL program to specified REPL."
      (if (boundp 'scheme-program-name)
          (let ((repl-file-path-name (executable-find repl)))
            (if repl-file-path-name
                (setq scheme-program-name repl-file-path-name)
              (display-warning 'pel--set-scheme-repl
                               (format "Cannot find REPL at: %s" repl)
                               :error)))))

    ;; No package made for this.  Take the code directly from Github
    (pel-install-github-file "pierre-rouleau/gambit/master/misc/" "gambit.el")
    (pel-autoload-file gambit for: gambit-mode gambit-inferior-mode)

    (define-pel-global-prefix pel:for-gambit (kbd "<f11> SPC C-s C-b"))
    (pel--lisp-languages-map-for pel:for-gambit)
    (define-key pel:for-gambit "z" 'pel-gambit-repl)
    (define-key pel:for-gambit (kbd "C-l") 'pel-clear-scheme-repl-buffer)

    ;; activate the <f12> key binding for gambit-mode
    (pel-config-major-mode gambit pel:for-gambit
      ;; Inside Gambit mode, ensure that the Scheme REPL is the Gambit REPL
      ;; unless Gerbil is also used.
      (unless pel-use-gerbil
        (pel--set-scheme-repl pel-gambit-repl))))

  ;; ---------------------------------------------------------------------------
  ;; - Function Keys - <f11> - Prefix ``<f11> SPC C-s C-e`` : Gerbil
  ;; IMPORTANT: this code must be located AFTER the code that supports Gambit.
  (when pel-use-gerbil
    (declare-function pel--set-scheme-repl "pel_keys")
    ;; No package made for this.  Take the code directly from Github
    (pel-install-github-file "vyzo/gerbil/master/etc/" "gerbil-mode.el")
    (pel-autoload-file gerbil-mode for: gerbil-mode)
    ;; In pel--options the code forces pel-use-gambit on when pel-use-gerbil
    ;; is on.

    (add-hook 'inferior-scheme-mode-hook 'gambit-inferior-mode )
    ;; Gerbil files use the same file extension as Scheme: .ss
    ;; Use Emacs file variable to activate the Gerbil mode: place the following
    ;; text on the first line:  ;; -*- Gerbil -*-
    (when pel-use-speedbar
      (pel-add-speedbar-extension '("\\.ss\\'"
                                    "\\.pkg\\'")))

    (define-pel-global-prefix pel:for-gerbil (kbd "<f11> SPC C-s C-e"))
    (pel--lisp-languages-map-for pel:for-gerbil)
    (define-key pel:for-gerbil "z"         'pel-gerbil-repl)
    (define-key pel:for-gerbil (kbd "C-l") 'pel-clear-scheme-repl-buffer)

    ;; activate the <f12> key binding for gerbil-mode
    (pel-config-major-mode gerbil pel:for-gerbil
      ;; Inside Gerbil mode, ensure that the Scheme REPL is the gerbil REPL.
      (pel--set-scheme-repl pel-gerbil-repl)
      ;; Visit identified TAGS files
      (pel-visit-tags pel-gerbil-base-tags)))

  ;; ---------------------------------------------------------------------------
  ;; - Function Keys - <f11> - Prefix ``<f11> SPC C-s C-b`` : Guile
  (when pel-use-guile
    (define-pel-global-prefix pel:for-guile (kbd "<f11> SPC C-s C-g"))
    (pel--lisp-languages-map-for pel:for-guile)
    (define-key pel:for-guile "z" 'pel-guile-repl)
    (define-key pel:for-guile (kbd "C-l") 'pel-clear-scheme-repl-buffer)
    ;; activate the <f12> key binding for guile-mode
    (pel-config-major-mode guile pel:for-guile))

  ;; ---------------------------------------------------------------------------
  ;; - Function Keys - <f11> - Prefix ``<f11> SPC C-s C-m`` : Mit-Scheme
  (when pel-use-mit-scheme
    (define-pel-global-prefix pel:for-mit-scheme (kbd "<f11> SPC C-s C-m"))
    (pel--lisp-languages-map-for pel:for-mit-scheme)
    (define-key pel:for-mit-scheme "z" 'pel-mit-scheme-repl)
    (define-key pel:for-mit-scheme (kbd "C-l") 'pel-clear-scheme-repl-buffer)
    ;; activate the <f12> key binding for mit-scheme-mode
    (pel-config-major-mode mit-scheme pel:for-mit-scheme))

  ;; ---------------------------------------------------------------------------
  ;; - Function Keys - <f11> - Prefix ``<f11> SPC C-s C-r`` : Racket
  ;; IMPORTANT: This must be done *after* the processing of Scheme.  See note in
  ;; the Scheme section.
  (when pel-use-racket
    (pel-ensure-package racket-mode from: melpa)
    (pel-autoload-file racket-mode for: racket-mode)

    (define-pel-global-prefix pel:for-racket (kbd "<f11> SPC C-s C-r"))
    (pel--lisp-languages-map-for pel:for-racket)
    (define-key pel:for-racket "z" 'pel-racket-repl)
    (define-key pel:for-racket (kbd "C-l") 'pel-clear-scheme-repl-buffer)

    ;; The racket-mode is already supported via scheme-mode and the associations
    ;; are present in auto-mode-alist.  Remove them first to ensure proper support.
    (pel-delete-from-auto-mode-alist 'racket-mode)
    (add-to-list 'auto-mode-alist '("\\.rkt[dl]?\\'" . racket-mode))
    ;; Activate Speedbar support
    (when pel-use-speedbar
      (pel-add-speedbar-extension "\\.rkt[dl]?\\'"))
    ;; activate the <f12> key binding for racket-mode
    (pel-config-major-mode racket pel:for-racket))

  ;; ---------------------------------------------------------------------------
  ;; - Function Keys - <f11> - Prefix ``<f11> SPC C-s C-h`` : Scsh
  (when pel-use-scsh
    (define-pel-global-prefix pel:for-scsh (kbd "<f11> SPC C-s C-h"))
    (pel--lisp-languages-map-for pel:for-scsh)
    (define-key pel:for-scsh "z" 'pel-scsh-repl)
    (define-key pel:for-scsh (kbd "C-l") 'pel-clear-scheme-repl-buffer)
    ;; activate the <f12> key binding for scsh-mode
    (pel-config-major-mode scsh pel:for-scsh)))

;; ---------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> SPC e`` : Erlang programming
;; Programming Language Family: BEAM
(when pel-use-erlang
  ;; Installation control
  (pel-ensure-package erlang from: melpa)
  (pel-autoload-file erlang for: erlang-mode)
  (when pel-use-erlstack-mode
    (pel-ensure-package erlstack-mode from: melpa))
  (when pel-use-ivy-erlang-complete
    (pel-ensure-package ivy-erlang-complete from: melpa)
    (when pel-use-company-erlang
      (pel-ensure-package company-erlang from: melpa)))
  (when pel-use-edts
    (pel-ensure-package edts from: melpa)
    (pel-autoload-file edts for: edts-mode))
  (when pel-use-erlang-ls
    ;; install lsp-mode clients
    (pel-ensure-package lsp-mode from: melpa)
    (pel-ensure-package lsp-ui from: melpa))

  ;; Invocation control
  ;; - Identify Erlang files:
  ;;   The ~/.erlang is the Erlang configuration file: allow the file
  ;;   to be located anywhere but currently don't allow something.erlang
  ;;   to be recognized. Is this too harsh?  It might be, OTOH it will
  ;;   allow opening the file in fundamental mode and then choose to activate
  ;;   the erlang mode, just in case we want to modify it without it having
  ;;   an impact on the Erlang process we're trying to modify.
  (pel-set-auto-mode erlang-mode for:
                     "\\.erl?\\'"
                     "\\.hrl?\\'"
                     "[\\/]\\.erlang\\'"
                     "rebar\\.config\\'"
                     "relx\\.config\\'"
                     "sys\\.config\\.src\\'"
                     "sys\\.config\\'"
                     "\\.config\\.src?\\'"
                     "\\.config\\.script?\\'"
                     "\\.app?\\'"
                     "\\.app.src?\\'"
                     "\\Emakefile")

  ;; - Add Speedbar support (no done by erlang.el)
  (when pel-use-speedbar
    (pel-add-speedbar-extension '(".erl"
                                  ".hrl"
                                  ".escript")))

  ;; Bind keys that can be used *before* the erlang.el file is loaded
  ;; and that essentially start the use of Erlang withing Emacs.
  ;; The remaining keys will be mapped after erlang.el is loaded.
  ;; Some of those are only providing the PEL F1, F2 and F3 keys.
  (define-pel-global-prefix pel:for-erlang        (kbd "<f11> SPC e"))
  (define-pel-global-prefix pel:erlang-skel       (kbd "<f11> SPC e <f12>"))
  (define-pel-global-prefix pel:erlang-lsp        (kbd "<f11> SPC e L"))
  (define-pel-global-prefix pel:erlang-lsp-window (kbd "<f11> SPC e w"))
  (define-key pel:for-erlang      "z"         'erlang-shell)
  (define-key pel:for-erlang      "?"         'pel-show-erlang-version)

  ;; Augment the skeletons defined inside erlang.el.
  ;; Do this once - right after erlang.el file is loaded and
  ;; before the erlang-mode executes.
  (advice-add 'erlang-mode :before #'pel--erlang-mode-setup)

  (when (and pel-use-erlang-syntax-check
             (eq pel-use-erlang-syntax-check 'with-flymake))
    ;; When using flymake with Erlang:
    ;; - Remove hook unless user wants to automatically activate
    ;;   flymake on Erlang buffers.
    ;;   - The erlang-flymake.el code hooks flymake-mode to erlang-mode
    ;;     forcing flymake-mode on all Erlang files. Leave it if erlang-mode
    ;;     was identified as a mode to automatically activate syntax checking,
    ;;     otherwise remove the hook.
    (unless (memq 'erlang-mode pel-modes-activating-syntax-check)
      (remove-hook 'erlang-mode-hook #'flymake-mode)))

  ;; BEFORE loading erlang.el
  ;; Set up a wrapper function for erlang.el `erlang-man-dir', allowing it
  ;; to use the Erlang Man directory identified by the
  ;; `pel-erlang-man-parent-rootdir' user-option.  This way it's possible
  ;; to force a different location for the Erlang Man files.
  (pel-erlang-setup-erlang-man-dir-root)

  (pel-eval-after-load erlang
    ;; Set erlang-root-dir from the method identified by the
    ;; `pel-erlang-version-detection-method' user-option
    (if (boundp 'erlang-root-dir)
        (setq erlang-root-dir (pel-erlang-root-path))
      (display-warning 'pel-use-erlang "erlang-root-dir is unbound" :error))
    ;; Optionally add a Erlang Bin directory to the exec-path
    (pel-erlang-set-dirpath (function pel-erlang-exec-path)
                            (lambda (dirpath) (add-to-list 'exec-path
                                                           dirpath)))

    ;; Prevent erlang shell to echo back commands on request.
    (when pel-erlang-shell-prevent-echo
      (add-hook 'erlang-shell-mode-hook 'pel-erlang-shell-mode-init))

    ;;
    (require 'erlang-start)
    (defvar erlang-mode-map) ; declare dynamic: prevent byte-compiler warnings

    (pel-config-major-mode erlang pel:for-erlang
      ;; Activate Erlang setup.
      ;; Set fill-column to Erlang's default if specified
      (when pel-erlang-fill-column
        (setq-local fill-column pel-erlang-fill-column))
      ;;
      (when (and (require 'pel-file nil :noerror)
                 (boundp 'pel-filename-at-point-finders))
        (setq pel-filename-at-point-finders '(pel-erlang-find-file)))
      ;;
      ;; setup the Erlang-specific key bindings
      (pel--install-erlang-skel pel:erlang-skel)
      ;;
      ;; Configure M-( to put parentheses after a function name.
      (set (make-local-variable 'parens-require-spaces) nil)
      ;;
      ;; Setup requested electric key behaviour
      (pel-erlang-setup-electric-key-behaviour)
      ;;

      (define-pel-global-prefix pel:erlang-electric   (kbd "<f11> SPC e M-`"))
      (define-pel-global-prefix pel:erlang-analysis   (kbd "<f11> SPC e a"))
      (define-pel-global-prefix pel:erlang-clause     (kbd "<f11> SPC e c"))
      (define-pel-global-prefix pel:erlang-debug      (kbd "<f11> SPC e d"))
      (define-pel-global-prefix pel:erlang-function   (kbd "<f11> SPC e f"))
      (define-pel-global-prefix pel:erlang-statement  (kbd "<f11> SPC e s"))

      (define-key pel:erlang-electric (kbd "M-,") 'pel-erlang-toggle-space-after-comma)
      (define-key pel:erlang-electric ","         'pel-erlang-comma)
      (define-key pel:erlang-electric ">"         'pel-erlang-gt)
      (define-key pel:erlang-electric (kbd "RET") 'pel-erlang-newline)
      (define-key pel:erlang-electric ";"         'pel-erlang-semicolon)
      (define-key pel:erlang-electric "."         'pel-erlang-period)
      (when pel-use-smart-dash
        (define-key pel:erlang-electric "-"       'smart-dash-mode))

      (when pel-erlang-code-formatter-command
        (define-key pel:for-erlang "R"     'pel-erlang-format-code))
      (when pel-use-plantuml
        (define-key pel:for-erlang "u"     'pel-render-commented-plantuml))
      (when pel-use-rainbow-delimiters
        (define-key pel:for-erlang (kbd "M-r")    'rainbow-delimiters-mode))
      (define-key pel:for-erlang      (kbd "TAB") 'erlang-indent-current-buffer)
      (define-key pel:erlang-function (kbd "TAB") 'erlang-indent-function)
      (define-key pel:erlang-function "N"         'pel-beginning-of-next-defun)
      (define-key pel:erlang-function "P"         'beginning-of-defun)
      (define-key pel:erlang-function "n"         'pel-next-erl-function)
      (define-key pel:for-erlang (kbd "<down>")   'pel-next-erl-function)
      (define-key pel:erlang-function "p"         'pel-previous-erl-function)
      (define-key pel:for-erlang (kbd "<up>")     'pel-previous-erl-function)
      (define-key pel:erlang-clause   (kbd "TAB") 'erlang-indent-clause)
      (define-key pel:erlang-clause   "a"         'erlang-beginning-of-clause)
      (define-key pel:for-erlang (kbd "<M-up>")   'erlang-beginning-of-clause)
      (define-key pel:erlang-clause   "p"         'pel-end-of-previous-clause)
      (define-key pel:for-erlang (kbd "<M-left>") 'pel-end-of-previous-clause)
      (define-key pel:erlang-clause   "n"         'pel-beginning-of-next-clause)
      (define-key pel:for-erlang (kbd "<M-down>") 'pel-beginning-of-next-clause)
      (define-key pel:erlang-clause   "e"         'erlang-end-of-clause)
      (define-key pel:for-erlang (kbd "<M-right>") 'erlang-end-of-clause)
      (define-key pel:erlang-function "m"         'erlang-mark-function)
      (define-key pel:erlang-clause   "m"         'erlang-mark-clause)
      (define-key pel:erlang-statement "a"        'bsckward-sentence)
      (define-key pel:erlang-statement "e"        'forward-sentence)
      (define-key pel:for-erlang (kbd "M-p")     #'superword-mode)
      (define-key pel:for-erlang (kbd "M-9")     #'show-paren-mode)
      (define-key pel:for-erlang (kbd "M-c")      'erlang-compile)
      (define-key pel:for-erlang (kbd "M-d")      'erlang-man-function-no-prompt)
      (define-key pel:for-erlang (kbd "M-D")      'erlang-man-function)
      (when pel-use-ivy-erlang-complete
        (if (and (require 'ivy-erlang-complete nil :noerror)
                 (fboundp 'ivy-erlang-complete-init))
            (progn
              (ivy-erlang-complete-init)
              ;; Ensure same Erlang is used by ivy-erlang-complete
              (if (and (boundp 'erlang-root-dir)
                       (boundp 'ivy-erlang-complete-erlang-root))
                  (setq ivy-erlang-complete-erlang-root
                        (file-name-as-directory erlang-root-dir))
                (display-warning 'pel-use-ivy-erlang-complete
                                 "Can't access ivy-erlang-complete-erlang-root"
                                 :error))
              ;; automatic update completion data after save
              (add-hook 'after-save-hook 'ivy-erlang-complete-reparse)
              ;; Extra key bindings
              (define-key pel:for-erlang "."          'ivy-erlang-complete)
              (define-key pel:for-erlang (kbd "M-h")  'ivy-erlang-complete-show-doc-at-point)
              (define-key pel:for-erlang (kbd "M-e")  'ivy-erlang-set-project-root)
              (define-key pel:for-erlang (kbd "M-?")  'ivy-erlang-complete-find-references)
              (define-key pel:for-erlang (kbd "C-f")  'ivy-erlang-complete-find-spec)
              (define-key pel:for-erlang (kbd "C-o")  'ivy-erlang-complete-find-file)

              ;; Restore the tags-based M-. and M-? to allow PEL controlled
              ;; selection of behaviour for these key bindings in Erlang mode.
              ;; and remap them under C-c prefix to provide direct access to
              ;; the ivy-erlang-complete commands.
              (define-key erlang-mode-map (kbd "M-.") nil)
              (define-key erlang-mode-map (kbd "M-?") nil)
              (define-key erlang-mode-map (kbd "C-c M-.") 'ivy-erlang-complete-find-definition)
              (define-key erlang-mode-map (kbd "C-c M-?") 'ivy-erlang-complete-find-references))
          (display-warning 'pel-use-ivy-erlang-complete
                           "Failed loading ivy-erlang-complete"
                           :error))
        (when (and pel-use-company-erlang
                   (require 'company-erlang nil :noerror)
                   (fboundp 'company-erlang-init))
          (company-erlang-init)))

      ;; Erlang Syntax Checking
      (when pel-use-erlang-syntax-check
        (cond
         ;; when using flymake with Erlang
         ((eq pel-use-erlang-syntax-check 'with-flymake)
          (pel-require 'erlang-flymake :install-when-missing)
          ;; TODO: activate flymake when (memq 'erlang-mode pel-modes-activating-syntax-check)
          (pel--extend-flymake))
         ;;
         ;; when using flycheck with Erlang
         ((eq pel-use-erlang-syntax-check 'with-flycheck)
          (when (memq 'erlang-mode pel-modes-activating-syntax-check)
            ;; Setup flycheck.
            ;; - Note that both flycheck-select-checker and flycheck-mode
            ;;   are autoloaded.  See section: 'Syntax Check with Flycheck'
            (flycheck-select-checker 'erlang-otp)
            (flycheck-mode))))
        ;;
        ;; When any syntax checker is used with Erlang add a key to toggle it
        (define-key pel:for-erlang "!" 'pel-erlang-toggle-syntax-checker))

      ;; Activate EDTS when required.
      (when pel-use-edts
        ;; EDTS does not seem to autoload various commands.  Do it here.
        (pel-autoload "edts-code" for:
          edts-code-compile-and-display
          edts-code-eunit
          edts-code-next-issue
          edts-code-previous-issue)
        (pel-autoload "edts-debug" for:
          edts-debug-sync
          edts-debug-toggle-interpreted
          edts-debug-toggle-breakpoint
          edts-debug-finish
          edts-debug-step-into
          edts-debug-step-over)
        (pel-autoload "edts-debug-list-breakpoint-mode" for:
          edts-debug-list-breakpoints
          edts-debug-list-breakpoint-find-breakpoint
          edts-debug-list-breakpoint-delete-breakpoint)
        (pel-autoload "edts-debug-list-processes-mode" for:
          edts-debug-list-processes
          edts-debug-list-processes-find-processes
          edts-debug-list-processes-attach
          edts-debug-list-processes-continue
          edts-debug-list-processes-finish
          edts-debug-list-processes-step-into
          edts-debug-list-processes-step-over)
        (pel-autoload "edts-dialyzer" for:
          edts-dialyzer-analyze)

        (defun pel--setup-edts ()
          "Set EDTS to work within PEL."
          ;; In PEL M-. and M-, are bound to a PEL command that determine
          ;; what back-end to use.  Prevent EDTS to unbind PEL keys.
          (defvar edts-mode-map)
          (define-key edts-mode-map (kbd "M-.") nil)
          (define-key edts-mode-map (kbd "M-,") nil)
          (define-key edts-mode-map (kbd "C-c C-d M-.") 'edts-find-source-under-point)
          (define-key edts-mode-map (kbd "C-c C-d M-,") 'edts-find-source-unwind))
        (declare-function pel--setup-edts "pel_keys")
        (add-hook 'edts-mode-hook (function pel--setup-edts))

        (defun edts-mode-desktop-restore  (&rest args)
          "Restore EDTS mode desktop with specified ARGS.
        Prevent edts errors from stopping desktop restoration."
          ;; log a message for each restoration to help understand potential failure.
          (message "Desktop: (edts-mode-desktop-restore %S)" args)
          (with-demoted-errors "Desktop: Problem restoring edts-mode: %S"
            (if (fboundp 'edts-mode)
                ;; If EDTS mode is available allow restoration but catch errors.
                ;; If an error is detected, disable EDTS mode because EDTS can fail
                ;; but the mode can still show as active(!).
                (condition-case err
                    (edts-mode 1)
                  (error
                   (progn
                     (display-warning
                      'pel-use-edts
                      (format "Desktop: Error detected activating EDTS mode: %s"
                              (error-message-string err))
                      :error)
                     (edts-mode -1))))
              (display-warning 'pel-use-edts
                               "edts-mode is void.  Is it installed?"
                               :error))))

        ;; Key to start EDTS
        (define-pel-global-prefix pel:erlang-edts  (kbd "<f11> SPC e M-E"))
        (define-key pel:erlang-edts (kbd "M-E")   'edts-mode)
        (when (eq pel-use-edts 'start-automatically)
          (require 'edts-start))
        (pel-eval-after-load edts
          (add-to-list 'desktop-minor-mode-handlers
                       '(edts-mode . edts-mode-desktop-restore))
          (unless (eq pel-use-edts 'start-automatically)
            (require 'edts-start))
          ;; EDTS keys
          ;; The following do not seem to do anything special in Erlang.
          ;; (define-key pel:for-erlang      ">"     'ahs-forward-definition)
          ;; (define-key pel:for-erlang      "<"     'ahs-backward-definition)
          ;;  edts cross reference command keys
          (define-key pel:erlang-edts (kbd "M-m")   'edts-man-setup)
          (define-key pel:for-erlang "w" 'edts-xref-who-calls)
          (define-key pel:for-erlang "W" 'edts-xref-last-who-calls)
          ;;  edts cross reference
          (define-key pel:for-erlang (kbd "M-f") 'edts-find-local-function)
          (define-key pel:for-erlang (kbd "M-g") 'edts-find-global-function)
          ;; edts refactoring
          (define-key pel:for-erlang "r" 'edts-refactor-extract-function)
          ;; edts man page use
          (define-key pel:for-erlang "h" 'edts-show-doc-under-point)
          (define-key pel:for-erlang "H" 'edts-find-doc)
          ;; edts code analysis
          (define-key pel:erlang-analysis "a" 'edts-dialyzer-analyze)
          (define-key pel:erlang-analysis "t" 'edts-code-eunit)
          (define-key pel:erlang-analysis "c" 'edts-code-compile-and-display)
          ;; edts debug
          (define-key pel:erlang-debug "b" 'edts-debug-toggle-breakpoint)
          (define-key pel:erlang-debug "B" 'edts-debug-list-breakpoints)
          (define-key pel:erlang-debug "p" 'edts-debug-list-processes)
          (define-key pel:erlang-debug "i" 'edts-debug-toggle-interpreted)
          (define-key pel:erlang-debug "I" 'edts-debug-list-interpreted)
          ;; edts node
          (define-key pel:erlang-edts "N" 'edts-buffer-node-name)
          (define-key pel:erlang-edts "z" 'edts-shell)
          (define-key pel:erlang-edts "x" 'edts-api-start-server)
          ;; EDTS/(automatic highlight symbol)  features
          (define-key pel:for-erlang "e" 'edts-ahs-edit-current-function)
          (define-key pel:for-erlang "E" 'edts-ahs-edit-buffer)
          (define-key pel:for-erlang "n" 'ahs-forward)
          (define-key pel:for-erlang "p" 'ahs-backward)
          (define-key pel:for-erlang "." 'ahs-back-to-start)))

      (when pel-use-erlang-ls
        ;; Note: by default the lsp-keymap-prefix is s-l, which may not be
        ;;       available.
        ;;       Instead of trying to control it in PEL, since this is a
        ;;       customizable user-option, just customize it to what you need.
        ;;       With PEL, any function key not used by PEL (such as F9) could
        ;;       be used.  Also the C-l key binding is another good candidate
        ;;       since PEL provides the `<f11> C-l` binding to what C-l is
        ;;       normally bound.

        ;; Enable LSP for Erlang files
        (if (fboundp 'lsp)
            (lsp pel-erlang-ls-with-separate-session)
          (display-warning 'pel-use-erlang-ls
                           "lsp is not bound, can't start LS for Erlang!"
                           :error))

        ;; Add key bindings to toggle LSP specific settings
        (define-key pel:erlang-lsp "I" 'pel-toggle-lsp-log-io)
        (define-key pel:erlang-lsp "L" 'pel-toggle-lsp-ui-sideline)
        (define-key pel:erlang-lsp "D" 'pel-toggle-lsp-ui-doc)
        ;; Add key-bindings inside the lsp-keymap if erlang_ls is used
        (when (and (boundp 'lsp-mode-map)
                   (boundp 'lsp-keymap-prefix))
          ;; The F9 key be used for Greek letters and for the
          ;; lsp-keymap-prefix. Check if there is a conflict and report it.
          (when (and pel-activate-f9-for-greek
                     (string= lsp-keymap-prefix "<f9>"))
            (display-warning 'pel-use-erlang-ls
                             "Key prefix conflict detected!
The F9 key is used for 2 prefixes: LSP and Greek letters. Change one.
See lsp-keymap-prefix and pel-activate-f9-for-greek user-options."))
          (define-key lsp-mode-map
            (kbd (format "%s d" lsp-keymap-prefix)) 'lsp-doctor)
          (define-key lsp-mode-map
            (kbd (format "%s L" lsp-keymap-prefix)) 'lsp-workspace-show-log))

        ;; Enable LSP Origami Mode (for folding ranges)
        ;; TODO: this is not Erlang specific but LSP specific: move
        ;;       it to a LSP section once I have set one up.
        (when pel-use-lsp-origami
          (add-hook 'lsp-after-open-hook 'lsp-origami-try-enable)
          (add-hook 'origami-mode-hook 'lsp-origami-mode))
        ;; TODO (when pel-use-helm
        ;;   ;; Provide commands to list workspace symbols:
        ;;   ;; - helm-lsp-workspace-symbol
        ;;   ;; - helm-lsp-global-workspace-symbol
        ;;   )
        ;; Always show diagnostics at the bottom, using 1/3 of available space
        (add-to-list
         'display-buffer-alist
         `(,(rx bos "*Flycheck errors*" eos)
           (display-buffer-reuse-window display-buffer-in-side-window)
           (side            . bottom)
           (reusable-frames . visible)
           (window-height   . 0.33)))
        ;; Force the use of flycheck when LSP is used.
        (setq pel-use-erlang-syntax-check 'with-flycheck)

        ;; Activate commands to open lsp-treemacs windows if it is available
        (when (and pel-use-treemacs pel-use-lsp-treemacs)
          (define-key pel:erlang-lsp-window "e" 'lsp-treemacs-errors-list)
          (define-key pel:erlang-lsp-window "s" 'lsp-treemacs-symbols)
          (define-key pel:erlang-lsp-window "x" 'lsp-treemacs-references)
          (define-key pel:erlang-lsp-window "i" 'lsp-treemacs-implementations)
          (define-key pel:erlang-lsp-window "c" 'lsp-treemacs-call-hierarchy)
          (define-key pel:erlang-lsp-window "t" 'lsp-treemacs-type-hierarchy)))

      ;; Override global keys with PEL specific ones in Erlang mode.
      ;; Use pel-erlang-comment-dwim instead of comment-dwim
      (define-key erlang-mode-map (kbd "M-;") 'pel-erlang-comment-dwim)
      (define-key erlang-mode-map (kbd "M-.") 'pel-erlang-find-definitions)
      (define-key erlang-mode-map (kbd "M-,") 'pel-erlang-unwind))

    ;; Bind Erlang-specific meta-selector commands
    (define-pel-simple-global-prefix pel:erlang-xref-settings (kbd "<f11> SPC e M-."))

    (define-key pel:erlang-xref-settings (kbd "M-.") 'pel-erlang-select-xref)
    (define-key pel:erlang-xref-settings (kbd "M-?") 'pel-erlang-show-xref)
    ))

;; ---------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> SPC x`` : Elixir programming
;; Programming Language Family: BEAM
(when pel-use-elixir
  (pel-ensure-package elixir-mode from: melpa)
  (pel-autoload-file elixir-mode for: elixir-mode)

  (define-pel-global-prefix pel:for-elixir (kbd "<f11> SPC x"))

  (define-key pel:for-elixir (kbd "M-p") #'superword-mode)
  (when pel-use-plantuml
    (define-key pel:for-elixir "u" 'pel-render-commented-plantuml))

  (when pel-use-alchemist
    (pel-ensure-package alchemist from: melpa)
    (pel-autoload-file alchemist for:
                       alchemist-iex-mode
                       alchemist-iex-run)
    (define-key pel:for-elixir "z"         #'alchemist-iex-run))
  (when pel-use-elixir-exunit
    (pel-ensure-package exunit from: melpa)
    (pel-autoload-file exunit for:
                       exunit-mode
                       exunit-rerun
                       exunit-verify-all
                       exunit-verify-all-in-umbrella
                       exunit-verify-single
                       exunit-verify
                       exunit-toggle-file-and-test
                       exunit-toggle-file-and-test-other-window))

  (when pel-use-elixir-lsp
    (pel-ensure-package lsp-elixir from: melpa)
    (pel-autoload-file lsp-elixir for: elixir-mode)
    (add-hook 'elixir-mode-hook 'lsp))

  ;; Activate Elixir setup.
  (pel-config-major-mode elixir pel:for-elixir))

;; ---------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> SPC C-l `` : LFE programming
;; LFE := Lisp Flavoured Erlang
;; Programming Language Family: BEAM, Lisp
(when pel-use-lfe
  (pel-ensure-package lfe-mode from: melpa)
  (require 'lfe-start nil :no-error)    ; autoloads lfe commands
  (when pel-use-speedbar
    (pel-add-speedbar-extension '(".lfe"
                                  ".lfes"
                                  ".lfesh")))

  (define-pel-global-prefix pel:for-lfe (kbd "<f11> SPC C-l"))
  (pel--lisp-languages-map-for pel:for-lfe)
  (define-key pel:for-lfe "[" 'lfe-insert-brackets)
  (define-key pel:for-lfe "z" 'run-lfe)
  (define-key pel:for-lfe (kbd "M-c") 'pel-lfe-eval-buffer)

  ;; Setup the LFE major mode
  (pel-config-major-mode lfe pel:for-lfe
    (when pel-emacs-is-a-tty-p
      (if (boundp 'lfe-mode-map)
          (define-key lfe-mode-map (kbd "M-[") nil)
        (display-warning 'pel-lfe
                         "The lfe-mode-map is not bound.
 Cannot disable the problematic M-[ key.
 Function keys starting with F5 will no work!"
                         :error))))

  ;; Setup the inferior-lfe-mode support
  ;; Add <f12> keys to the LFE shell (no macro yet for that, spell it out)
  ;; TODO simplify this code, integrate the ability to add <f12> key setup
  ;;      to an inferior process mode to the macros I normally use.
  (define-pel-global-prefix pel:for-inferior-lfe (kbd "<f11> SPC SPC C-l"))
  ;; (pel-config-major-mode inferior-lfe pel:for-inferior-lfe)
  (defun pel--setup-for-inferior-lfe ()
    "Activate inferior-lfe setup, take local variables into account."
    (pel-local-set-f12-M-f12 'pel:for-inferior-lfe)
    (pel-turn-on-local-minor-modes-in 'pel-inferior-lfe-activates-minor-modes))
  (declare-function pel--setup-for-inferior-lfe "pel_keys")
  (pel-check-minor-modes-in pel-inferior-lfe-activates-minor-modes)
  (pel--mode-hook-maybe-call
   (function pel--setup-for-inferior-lfe)
   'inferior-lfe-mode 'inferior-lfe-mode-hook))
;; ---------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> SPC M-G `` : Gleam
;; Programming Language Family: BEAM
(when pel-use-gleam
  (when pel-use-gleam-mode
    (pel-install-github-files "pierre-rouleau/gleam-mode/master"
                              "gleam-mode.el")
    (pel-autoload-file gleam-mode for: gleam-mode)
    (add-to-list 'auto-mode-alist '("\\.gleam\\'" . gleam-mode))
    (when pel-use-speedbar
      (pel-add-speedbar-extension ".gleam"))

    (define-pel-global-prefix pel:for-gleam (kbd "<f11> SPC M-G"))
    (pel--lisp-languages-map-for pel:for-gleam)

    ;; Activate GLEAM setup.
    (pel-config-major-mode gleam pel:for-gleam)))

;; ---------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> SPC M-H `` : Hamler
;; Programming Language Family: BEAM, Functional/ML/Haskell
;; Future: hamler-mode is not written yet
;;
;; (when pel-use-hamler
;;   (pel-install-github-files "hamler-lang/hamler-mode/master"
;;                             "hamler-mode.el")
;;   (pel-autoload-file hamler-mode for:
;;                      hamler-mode)
;;   (add-to-list 'auto-mode-alist '("\\.hm\\'" . hamler-mode))
;;   (when pel-use-speedbar
;;     (pel-add-speedbar-extension ".hm"))
;;
;;   (define-pel-global-prefix pel:for-hamler (kbd "<f11> SPC M-H"))
;;   (pel--lisp-languages-map-for pel:for-hamler)
;;
;;   ;; Activate HAMLER setup.
;;   (pel-config-major-mode hamler pel:for-hamler))

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; BEAM programming Language common tools
(when (or pel-use-erlang
          pel-use-elixir
          pel-use-lfe
          pel-use-gleam)
  (when pel-use-flycheck-rebar3
    (pel-install-github-files "joedevivo/flycheck-rebar3/master"
                              "flycheck-rebar3.el")
    (pel-autoload-file flycheck-rebar3 for:
                       flycheck-rebar3-setup)))
;; TODO: test and complete dependency management of flycheck and rebar3

;; ---------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> SPC h`` : Haskell programming
(when pel-use-haskell
  (define-pel-global-prefix pel:for-haskell (kbd "<f11> SPC h"))
  (when pel-use-haskell-mode
    (pel-ensure-package haskell-mode from: melpa)
    (declare-function run-haskell "inf-haskell")
    (define-key pel:for-haskell "z" #'run-haskell))
  (when pel-use-speedbar
    (pel-add-speedbar-extension '(".hs"
                                  ".hsc"
                                  ".gs")))
  ;; the haskell-mode is part of Emacs
  (pel-config-major-mode haskell pel:for-haskell))

;; Using Intero to support Haskell programming language.
;; (add-hook 'haskell-mode-hook 'intero-mode)

;; ---------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> SPC C-h`` : Hy
;; Hy: A Lisp in Python
(when pel-use-hy
  (pel-ensure-package hy-mode from: melpa)
  (pel-autoload-file hy-mode for: hy-mode)

  (define-pel-global-prefix pel:for-hy (kbd "<f11> SPC C-h"))
  (pel--lisp-languages-map-for pel:for-hy)

  ;; activate the <f12> key binding for hy-mode
  (pel-config-major-mode hy pel:for-hy))

;; ---------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> SPC n`` : Nim programming
(when pel-use-nim
  (define-pel-global-prefix pel:for-nim (kbd "<f11> SPC n"))
  (when pel-use-nim-mode
    (pel-ensure-package nim-mode from: melpa))
  (when pel-use-speedbar
    (pel-add-speedbar-extension '(".nim"
                                  ".nims"
                                  ".nimble")))
  ;; the nim-mode is part of Emacs
  (pel-config-major-mode nim pel:for-nim))

;; ---------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> SPC o`` : Ocaml programming
(when pel-use-ocaml
  (define-pel-global-prefix pel:for-ocaml (kbd "<f11> SPC o"))
  (when pel-use-caml-mode
    (pel-ensure-package caml from: melpa))
  (when pel-use-merlin
    (pel-ensure-package merlin from: melpa))
  (when pel-use-tuareg
    (pel-ensure-package tuareg from: melpa))
  (when pel-use-speedbar
    (pel-add-speedbar-extension '(".ml"
                                  ".mli")))
  ;; the ocaml-mode is part of Emacs
  (pel-config-major-mode tuareg pel:for-ocaml))

;; ---------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> SPC p`` : Python programming
(when pel-use-python

  ;; ⚠️  I recommend that you stay away from the external package python-mode.el
  ;; Support for pel-use-external-python-mode was removed because use-package
  ;; will load it and once it is loaded, it conflicts with Emacs built-in
  ;; python.el.

  (when pel-use-elpy
    (pel-ensure-package elpy from: melpa))
  ;; TODO control start of elpy

  ;; Normally, (python-shell-prompt-detect) should evaluate to
  ;; (">>> " "... " "") for Python shell to work properly.
  ;; Under Windows, that is currently not the
  ;; case for my system (but also others as described by
  ;;  https://github.com/jorgenschaefer/elpy/issues/733)
  ;; I investigated and a work-around is to set python-shell-unbuffered to nil.
  ;; So I attempt to do this here to see if that works when launching emacs.
  ;;
  ;; There is another, remaining problem: the "native" python completion does
  ;; not work on Windows because of its lack of proper PTY (pseudo terminal).
  ;; Therefore, in Windows, "python" should be added to the list bounded to
  ;; python-shell-completion-native-disabled-interpreters.
  ;;
  ;; The code below does both for Windows.
  (when pel-system-is-windows-p
    (defvar python-shell-unbuffered)
    (defvar python-shell-completion-native-disabled-interpreters)
    (setq python-shell-unbuffered nil)
    (if (boundp 'python-shell-completion-native-disabled-interpreters)
        (add-to-list
         'python-shell-completion-native-disabled-interpreters "python")
      (setq python-shell-completion-native-disabled-interpreters '("python"))))

  (define-pel-global-prefix pel:for-python (kbd "<f11> SPC p"))
  (define-key pel:for-python    "."        'pel-find-thing-at-point)
  (define-key pel:for-python    "z"        'run-python)
  (define-key pel:for-python (kbd "M-9")  #'show-paren-mode)
  (define-key pel:for-python (kbd "M-p")  #'superword-mode)
  (when pel-use-plantuml
    (define-key pel:for-python    "u"      'pel-render-commented-plantuml))
  (when pel-use-rainbow-delimiters
    (define-key pel:for-python (kbd "M-r") 'rainbow-delimiters-mode))

  ;; lpy-mode: lispy-style modal editing for Python.
  (when pel-use-lpy
    (pel-autoload-file pel-lispy for: pel-lpy-mode)
    (define-key pel:for-python (kbd "M-L") 'pel-lpy-mode)
    (define-key pel:for-python "1"         'lispy-describe-inline)
    (define-key pel:for-python "2"         'lispy-arglist-inline))

  ;; Activate python mode
  (pel-eval-after-load python
    (pel-config-major-mode python pel:for-python
      (when (and pel-use-indent-tools
                 (eq pel-indent-tools-key-bound 'python)
                 (require 'indent-tools nil :noerror)
                 (boundp 'indent-tools-keymap-prefix)
                 (boundp 'python-mode-map))
        (define-key python-mode-map indent-tools-keymap-prefix
          'indent-tools-hydra/body)))))

;; (use-package jedi
;;   :ensure t
;;   :init
;;   (add-hook 'python-mode-hook 'jedi:setup)
;;   (add-hook 'python-mode-hook 'jedi:ac-setup))

;; NOTE: Jedi requires the installation of the backend server with
;; M-x jedi:install-server
;;
;; For this to work, virtualenv must be present!!
;; inside /usr/local/bin/virtualenv

;; ---------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> SPC R`` : REXX programming
(when pel-use-rexx
  ;; Download and byte-compile rexx-mode if not already present.
  ;; See home page: https://github.com/emacsattic/rexx-mode
  ;; Do it after compiling pel_keys.el, when pel-init load pel_keys.
  (pel-install-github-files "pierre-rouleau/rexx-mode/master"
                            '("rexx-mode.el"
                              "rexx-debug.el"))
  (pel-autoload-file rexx-mode for:
                     rexx-mode
                     rexx-goto-next-procedure
                     rexx-goto-previous-procedure)
  ;; set the file extensions
  (pel-set-auto-mode rexx-mode for:
                     "\\.\\(rexx?\\|elx\\|ncomm\\|cpr\\)\\'")

  ;; Set the mode specific key prefix
  (define-pel-global-prefix pel:for-rexx (kbd "<f11> SPC R"))
  (define-key pel:for-rexx (kbd "<down>") 'rexx-goto-next-procedure)
  (define-key pel:for-rexx (kbd "<up>")   'rexx-goto-previous-procedure)

  ;; activate the <f12> key binding for rexx-mode
  (pel-config-major-mode rexx pel:for-rexx))

;; ---------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> SPC N`` : NetRexx programming
(when pel-use-netrexx
  ;; Download netrexx.el directly from GitHub as there is no official support
  ;; by either GNU Elpa or MELPA
  (pel-install-github-file "pierre-rouleau/netrexx-mode/master"
                           "netrexx-mode.el")
  (pel-autoload-file netrexx-mode for: netrexx-mode)
  ;; Set the file extension for NetRexx: ".nrx"
  (pel-set-auto-mode netrexx-mode for: "\\.nrx\\'")

  ;; Set the mode specific key prefix
  (define-pel-global-prefix pel:for-netrexx (kbd "<f11> SPC N"))
  (define-key pel:for-netrexx (kbd "<down>") 'netrexx-next-method)
  (define-key pel:for-netrexx (kbd "<up>")   'netrexx-previous-method)
  (define-key pel:for-netrexx "="            'netrexx-select-current-block)
  (define-key pel:for-netrexx "s"            'netrexx-sanitize-region)
  (define-key pel:for-netrexx ";"            'netrexx-insert-end-comment)
  (define-key pel:for-netrexx "e"          'netrexx-insert-end-comment-region)
  (define-key pel:for-netrexx "j"          'netrexx-insert-javadoc-for-method)

  ;; activate the <f12> key binding for netrexx-mode
  (pel-config-major-mode netrexx pel:for-netrexx))

;; ---------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> SPC P`` : Perl programming
(when pel-use-perl
  (define-pel-global-prefix pel:for-perl (kbd "<f11> SPC P"))
  ;; the perl-mode is part of Emacs
  (pel-config-major-mode perl pel:for-perl
    (when (boundp 'perl-indent-level)
      (setq-local tab-width perl-indent-level))
    (setq-local indent-tabs-mode pel-perl-use-tabs)))

;; ---------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> SPC U`` : Ruby programming
(when pel-use-ruby
  (define-pel-global-prefix pel:for-ruby (kbd "<f11> SPC U"))
  ;; the ruby-mode is part of Emacs
  (pel-config-major-mode ruby pel:for-ruby))

;; ---------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> SPC r`` : Rust programming
(when pel-use-rust
  ;; TODO: only allow one of rust-mode or rustic and determine what
  ;;       must be made available for rustic.  Currently the code assumes
  ;;       that rust-mode is used.
  (when pel-use-rust-mode
    ;; Important rust-mode user-options:
    ;; - rust-format-on-save
    (pel-ensure-package rust-mode from: melpa)
    (pel-autoload-file rust-mode for: rust-mode))
  (when pel-use-rustic
    (pel-ensure-package rustic from: melpa)
    (pel-autoload-file rustic for: rustic))

  ;; flycheck with rust-mode
  (when (and pel-use-rust-mode
             pel-use-flycheck-rust)
    (pel-ensure-package flycheck-rust from: melpa)
    (pel-eval-after-load rust-mode
      (add-hook 'flycheck-mode-hook 'flycheck-rust-setup)
      (pel-eval-after-load flycheck
        (require 'flycheck-rust))))

  (when pel-use-emacs-racer
    (pel-ensure-package racer from: melpa)
    (pel-autoload-file racer for: racer-mode))

  ;; Add Speedbar support for Rust
  (when pel-use-speedbar
    (pel-add-speedbar-extension ".rs"))

  (when pel-use-cargo
    (pel-ensure-package cargo from: melpa)
    (pel-autoload-file cargo for: cargo-minor-mode)
    (pel-eval-after-load cargo
      ;; M-x package-install company
      (add-hook 'rust-mode-hook 'cargo-minor-mode)
      (add-hook 'rust-mode-hook 'racer-mode)
      (add-hook 'racer-mode-hook 'eldoc-mode)
      (when pel-use-company
        (add-hook 'racer-mode-hook 'company-mode))))

  (define-pel-global-prefix pel:for-rust (kbd "<f11> SPC r"))
  (define-key pel:for-rust "c" 'rust-run)
  (define-key pel:for-rust "d" 'rust-dbg-wrap-or-unwrap)
  (define-key pel:for-rust "l" 'rust-run-clippy)

  (pel-config-major-mode rust pel:for-rust

    (setq indent-tabs-mode nil)
    (when (boundp 'rust-indent-offset)
      (setq-local tab-width rust-indent-offset))
    (setq-local indent-tabs-mode pel-rust-use-tabs)
    (when pel-use-cargo
      (if (boundp 'rust-mode-map)
          (define-key rust-mode-map
            (kbd "TAB") 'company-indent-or-complete-common)
        (display-warning 'pel-use-rust
                         "Unbound rust-mode-map!"
                         :error)))))

;; ---------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> SPC H`` : Sh, Unix shell programming
;;  This is for shell programming support: editing shell script files.
;;  For shell/terminal commend line support, see below.
(when pel-use-sh
  (define-pel-global-prefix pel:for-sh (kbd "<f11> SPC H"))
  ;; Shell support, the sh-mode is part of Emacs
  (pel-config-major-mode sh pel:for-sh
    (superword-mode 1)
    (define-key pel:for-sh "\"" 'pel-sh-double-quote-word)
    (define-key pel:for-sh "'"  'pel-sh-single-quote-word)
    (define-key pel:for-sh "`"  'pel-sh-backtick-quote-word))
  (cond
   ;; using flymake
   ((memq pel-use-shellcheck '(flymake-manual
                               flymake-automatic))
    (pel-ensure-package flymake-shellcheck from: melpa)
    (when (eq pel-use-shellcheck 'flymake-automatic)
      (add-hook 'sh-mode-hook 'flymake-shellcheck-load))
    (pel-eval-after-load flymake
      (pel--extend-flymake)))
   ;; using flycheck
   ;; - flycheck support is already extended by default.
   ;; - start it automatically if it was requested by user-option
   ((eq pel-use-shellcheck 'flycheck-automatic)
    (add-hook 'sh-mode-hook 'flycheck-mode))))

;; ---------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> SPC v`` : V programming
;; Experimental 🚧

(when pel-use-v
  (define-pel-global-prefix pel:for-v  (kbd "<f11> SPC v"))
  ;; TODO: V file name extension clashes with Verilog.
  ;;       Need to find a way to read the file content to distinguish them.
  ;;       Perhaps using magic-mode-alist and a regexp or a function that
  ;;       looks for what could be either V or Verilog code.
  ;; TODO: Document purpose of .v, .vv, .vsh files
  ;;       (ie. find where it's described)
  (add-to-list 'auto-mode-alist (cons "\\.\\(v?v\\|vsh\\)\\'"
                                      (if (eq pel-use-v 'v-mode)
                                          'v-mode
                                        'vlang-mode)))
  (cond
   ((eq pel-use-v 'v-mode)
    ;; TODO: since v-mode uses a hydra, PEL will
    ;; cause a warning when a V file is opened before f7 is typed.
    (pel-ensure-package v-mode from: melpa)
    (pel-autoload-file v-mode for: v-mode)
    (define-key pel:for-v (kbd "C-f") 'v-format-buffer)
    (define-key pel:for-v (kbd "<f10>") 'v-menu)
    (pel-config-major-mode v pel:for-v))

   ((eq pel-use-v 'vlang-mode)
    ;; vlang-mode is experimental: only provides font-locking
    ;; use, not on MELPA: download directly from github.
    (pel-install-github-file "pierre-rouleau/vlang-mode/master"
                             "vlang-mode.el")
    (pel-autoload-file vlang-mode for: vlang-mode))))

;; ---------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> SPC SPC z`` : shells and terminals
(define-pel-global-prefix pel:for-shell-terminals (kbd "<f11> SPC SPC z"))

;; ---------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> SPC SPC s`` : shell-mode
;;   This is for shell/terminal support.  For editing shell script file see above.
(define-pel-global-prefix pel:for-shell (kbd "<f11> SPC SPC s"))

(pel-eval-after-load shell
  ;; TODO: pel-config-major-mode does not work properly with shell-mode
  ;;       so I'm putting the code manually until pel-config-major-mode
  ;;       is fixed.
  (defun pel--setup-for-shell ()
    "Activate PEL setup for shell-mode."
    (pel-local-set-f12-M-f12 'pel:for-shell)
    (local-set-key (kbd "C-c M-o") 'pel-comint-clear-buffer-and-get-prompt)
    (define-key pel:for-shell "c" 'pel-comint-clear-buffer-and-get-prompt)
    (define-key pel:for-shell "r" 'shell-resync-dirs)
    (define-key pel:for-shell (kbd "<up>")   'pel-shell-previous-prompt)
    (define-key pel:for-shell (kbd "<down>") 'pel-shell-next-prompt)

    (pel-turn-on-local-minor-modes-in 'pel-shell-activates-minor-modes))
  (declare-function pel--setup-for-shell "pel_keys")

  (pel-check-minor-modes-in pel-shell-activates-minor-modes)
  (pel--mode-hook-maybe-call
   (function pel--setup-for-shell)
   'shell-mode 'shell-mode-hook))

;; ------------------------------
;; Telnet support
(pel-eval-after-load telnet
  (when (boundp 'telnet-mode-map)
    (define-key telnet-mode-map "\t" 'completion-at-point)))

;; ---------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> SPC SPC t`` : term-mode
(define-pel-global-prefix pel:for-term (kbd "<f11> SPC SPC t"))

(pel-eval-after-load term
  ;; TODO: pel-config-major-mode does not work properly with term-mode
  ;;       so I'm putting the code manually until pel-config-major-mode
  ;;       is fixed.
  (defun pel--setup-for-term ()
    "Activate PEL setup for term-mode."
    (pel-local-set-f12-M-f12 'pel:for-term)
    (pel-turn-on-local-minor-modes-in 'pel-term-activates-minor-modes))
  (declare-function pel--setup-for-term "pel_keys")

  (pel-check-minor-modes-in pel-term-activates-minor-modes)
  (pel--mode-hook-maybe-call
   (function pel--setup-for-term)
   'term-mode 'term-mode-hook))

;; ---------------------------------------------------------------------------
;; Markup Language Support
;; --=====================

;; AsciiDoc support
;; ----------------

(when pel-use-asciidoc
  (pel-ensure-package adoc-mode from: melpa)
  (pel-autoload-file adoc-mode for: adoc-mode)
  (pel-set-auto-mode adoc-mode for: "\\.adoc\\'")
  (pel-config-major-mode adoc :no-f12-keys))

;; ---------------------------------------------------------------------------
;; Outline Mode
;; ------------
(define-pel-global-prefix pel:for-outline-mode (kbd "<f11> SPC M-l"))
(define-key pel: (kbd      "M-l")          'outline-minor-mode)
(define-key pel: (kbd      "M-L")          'outline-mode)
;; The following provides the F12 key in Outline mode.
(pel-eval-after-load outline

  (defvar outline-minor-mode-map)

  (defun pel--setup-outline-minor-mode ()
    "Add PEL <f2> key bindings for outline minor mode."

    (define-key outline-minor-mode-map (kbd "<f2> <f1>") 'pel-help-on-outline)
    (define-key outline-minor-mode-map (kbd "<f2> ?")    'pel-outline-print-vars)

    (define-key outline-minor-mode-map (kbd "<f2> a") 'outline-show-all)
    (define-key outline-minor-mode-map (kbd "<f2> k") 'outline-show-branches)
    (define-key outline-minor-mode-map (kbd "<f2> i") 'outline-show-children)
    (define-key outline-minor-mode-map (kbd "<f2> e") 'outline-show-entry)
    (define-key outline-minor-mode-map (kbd "<f2> s") 'outline-show-subtree)

    (define-key outline-minor-mode-map (kbd "<f2> t") 'outline-hide-body)
    (define-key outline-minor-mode-map (kbd "<f2> c") 'outline-hide-entry)
    (define-key outline-minor-mode-map (kbd "<f2> d") 'outline-hide-subtree)
    (define-key outline-minor-mode-map (kbd "<f2> l") 'outline-hide-leaves)
    (define-key outline-minor-mode-map (kbd "<f2> q") 'outline-hide-sublevels)
    (define-key outline-minor-mode-map (kbd "<f2> o") 'outline-hide-other)

    (define-key outline-minor-mode-map (kbd "<f2> b") 'outline-backward-same-level)
    (define-key outline-minor-mode-map (kbd "<f2> f") 'outline-forward-same-level)
    (define-key outline-minor-mode-map (kbd "<f2> p") 'outline-previous-visible-heading)
    (define-key outline-minor-mode-map (kbd "<f2> n") 'outline-next-visible-heading)
    (define-key outline-minor-mode-map (kbd "<f2> u") 'outline-up-heading)

    (define-key outline-minor-mode-map (kbd "<f2> .") 'outline-mark-subtree)
    (define-key outline-minor-mode-map (kbd "<f2> RET") 'outline-insert-heading)

    (define-key outline-minor-mode-map (kbd "<f2> v") 'outline-move-subtree-down)
    (define-key outline-minor-mode-map (kbd "<f2> ^") 'outline-move-subtree-up)
    (define-key outline-minor-mode-map (kbd "<f2> [") 'outline-promote)
    (define-key outline-minor-mode-map (kbd "<f2> ]") 'outline-demote))
  (declare-function pel--setup-outline-minor-mode "pel_keys")

  (add-hook 'outline-minor-mode-hook (function pel--setup-outline-minor-mode)))

(when pel-use-outshine
  (pel-ensure-package outshine from: melpa))

;; ---------------------------------------------------------------------------
;; Org-Mode Support
;; ----------------

(when pel-use-org-mode
  (define-pel-global-prefix pel:for-org-mode   (kbd "<f11> SPC M-o"))
  ;; (define-pel-global-prefix pel:org-mode-setup (kbd "<f11> SPC M-o <f4>"))

  ;; Org-Mode activation, as suggested by
  ;; https://orgmode.org/manual/Activation.html#Activation :
  (pel-autoload-file org for:
                     org-mode
                     org-indent-mode
                     org-store-link
                     org-agenda
                     org-capture
                     org-switchb)
  (global-set-key "\C-cl" 'org-store-link)
  (global-set-key "\C-ca" 'org-agenda)
  (global-set-key "\C-cc" 'org-capture)
  (global-set-key "\C-cb" 'org-switchb)

  (define-key pel:for-org-mode (kbd "TAB") 'org-indent-mode)

  (pel-eval-after-load org
    (defvar pel-org-electric-pairs '((?\* . ?\*)
                                     (?/ . ?/)
                                     (?= . ?=)
                                     (?\_ . ?\_)
                                     (?~ . ?~)
                                     (?+ . ?+))
      "Electric pairs for org-mode markup.")

    ;; Activate specialized C-a and C-e in Org-Mode.
    (pel-setq org-special-ctrl-a/e t)
    ;; Activate timestamp log for DONE tasks
    (pel-setq org-log-done 'time)
    ;; Add the "IN-PROGRESS" in the list of TODO states
    (pel-setq org-todo-keywords
              (quote ((sequence "TODO" "IN-PROGRESS" "DONE"))))
    ;;
    (when pel-windmove-on-esc-cursor
      ;; Remove Esc down/up/left/right mapping to org-meta...
      ;; to allow the key bindings to be used for windmove operations.
      (when (boundp 'org-mode-map)
        (define-key org-mode-map (kbd "ESC <up>") nil)
        (define-key org-mode-map (kbd "ESC <down>") nil)
        (define-key org-mode-map (kbd "ESC <right>") nil)
        (define-key org-mode-map (kbd "ESC <left>") nil)
        (when (and pel-emacs-is-a-tty-p
                   pel-map-meta-left-right-to-Y-Z)
          ;; On a TTY:
          ;; map <M-left>  to the ANSI key sequence for M-Y: "\033Y"
          ;; map <M-right> to the ANSI key sequence for M-Z: "\033Z"
          (define-key org-mode-map (kbd "M-Y") 'org-metaleft)
          (define-key org-mode-map (kbd "M-Z") 'org-metaright))))

    (pel-config-major-mode org pel:for-org-mode
      ;; Use the cleaner outline view mode.
      (if (fboundp 'org-indent-mode)
          (org-indent-mode 1)
        (display-warning 'pel-use-org-mode
                         "Unbound org-indent-mode"
                         :error))
      (when (and pel-use-imenu+
                 (fboundp 'imenup-add-defs-to-menubar))
        (imenup-add-defs-to-menubar))
      ;; Activate Electric markup keys if requisted
      (when pel-org-use-electric-markup
        (defvar electric-pair-pairs)      ; defined in elec-pair
        (defvar electric-pair-text-pairs) ; defined in elec-pair
        (electric-pair-local-mode 1)
        (setq-local electric-pair-pairs
                    (append electric-pair-pairs pel-org-electric-pairs))
        (setq-local electric-pair-text-pairs electric-pair-pairs)))))

;; ---------------------------------------------------------------------------
;; XML Support
;; -----------
(when pel-use-osx-plist
  ;; Early support - no major mode - TODO: investigate enhancement.
  (pel-ensure-package osx-plist from: melpa))
;; ---------------------------------------------------------------------------
;; YAML Support
;; ------------

(when pel-use-yaml-mode
  (define-pel-global-prefix pel:for-yaml (kbd "<f11> SPC M-y"))
  (pel-ensure-package yaml-mode from: melpa)
  (pel-autoload-file yaml-mode for: yaml-mode)
  ;; .yml and .yaml for YAML
  ;; .eyaml for encrypted (see hiera, puppet)
  ;; .raml for RESTful API Modeling Language
  (pel-set-auto-mode yaml-mode for: "\\.\\(e?ya?\\|ra\\)ml\\'")

  (pel-config-major-mode yaml pel:for-yaml))

(when pel-use-cwl-mode
  (define-pel-global-prefix pel:for-cwl (kbd "<f11> SPC M-c"))
  (pel-ensure-package cwl-mode from: melpa)
  ;; .cwl files are associated with cwl-mode
  (pel-set-auto-mode cwl-mode for: "\\.cwl\\'")
  (pel-config-major-mode cwl pel:for-cwl))

;; ---------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> SPC M-m`` : Markdown
(when pel-use-markdown
  (define-pel-global-prefix pel:for-markdown (kbd "<f11> SPC M-m"))
  (define-pel-global-prefix pel:for-markdown-preview (kbd "<f11> SPC M-m M-p"))

  (when pel-use-edit-indirect
    (pel-ensure-package edit-indirect from: melpa))
  (when pel-use-grip-mode
    (pel-ensure-package grip-mode from: melpa)
    (define-key pel:for-markdown-preview "g" 'grip-mode))
  (when pel-use-markdown-mode
    (pel-ensure-package markdown-mode from: melpa))
  (when pel-use-impatient-showdown
    (pel-ensure-package impatient-showdown from: melpa)
    (define-key pel:for-markdown-preview "i" 'impatient-showdown-mode))
  (when pel-use-markdown-preview-eww
    (pel-ensure-package markdown-preview-eww from: melpa))
  (when pel-use-markdown-preview-mode
    (pel-ensure-package markdown-preview-mode from: melpa)
    (define-key pel:for-markdown-preview "p" 'markdown-preview-mode))
  (when pel-use-markdown-toc
    (pel-ensure-package markdown-toc from: melpa)
    (define-pel-global-prefix pel:for-markdown-toc (kbd "<f11> SPC M-m M-t"))
    (define-key pel:for-markdown-toc (kbd "M-t") 'markdown-toc-generate-toc)
    (define-key pel:for-markdown-toc (kbd "M-r") 'markdown-toc-generate-or-refresh-toc)
    (define-key pel:for-markdown-toc (kbd "M-d") 'markdown-toc-delete-toc))
  (when pel-use-vmd-mode
    (define-key pel:for-markdown-preview "v" 'vmd-mode)
    (pel-ensure-package vmd-mode from: melpa))
  (when pel-use-remark-mode
    (pel-ensure-package remark-mode from: melpa)
    (define-key pel:for-markdown-preview "r" 'remark-mode))

  (define-key pel:for-markdown (kbd "M--") 'pel-itemize-lines)
  (define-key pel:for-markdown "}" 'markdown-forward-block)
  (define-key pel:for-markdown "{" 'markdown-backward-block)
  (define-key pel:for-markdown (kbd "<right>") 'end-of-defun)
  (define-key pel:for-markdown (kbd "<down>")  'pel-beginning-of-next-defun)
  (define-key pel:for-markdown (kbd "<up>")   'beginning-of-defun)
  (define-key pel:for-markdown (kbd "<left>") 'beginning-of-defun)
  (define-key pel:for-markdown "b" 'markdown-insert-bold)
  (define-key pel:for-markdown "i" 'markdown-insert-italic)
  (define-key pel:for-markdown "c" 'markdown-insert-code)
  (define-key pel:for-markdown "C" 'markdown-insert-gfm-code-block)
  (define-key pel:for-markdown "f" 'markdown-insert-footnote)
  (define-key pel:for-markdown "F" 'markdown-insert-foldable-block)
  (define-key pel:for-markdown "k" 'markdown-insert-kbd)
  (define-key pel:for-markdown "p" 'markdown-insert-pre)
  (define-key pel:for-markdown "P" 'markdown-pre-region)
  (define-key pel:for-markdown "q" 'markdown-insert-blockquote)
  (define-key pel:for-markdown "Q" 'markdown-blockquote-region)
  (define-key pel:for-markdown "s" 'markdown-insert-strike-through)
  (define-key pel:for-markdown "t" 'markdown-insert-table)
  (define-key pel:for-markdown "u" 'markdown-insert-uri)
  (define-key pel:for-markdown "w" 'markdown-insert-wiki-link)
  (define-key pel:for-markdown "[" 'markdown-insert-gfm-checkbox)
  (define-key pel:for-markdown "l" 'markdown-insert-list-item)
  (define-key pel:for-markdown "L" 'markdown-insert-link)
  (define-key pel:for-markdown "I" 'markdown-insert-image)
  (define-key pel:for-markdown "-" 'markdown-insert-hr)
  (define-key pel:for-markdown "h" 'markdown-insert-header-dwim)
  (define-key pel:for-markdown "H" 'markdown-insert-header-setext-dwim)
  (define-key pel:for-markdown "!" 'markdown-insert-header-setext-1)
  (define-key pel:for-markdown "@" 'markdown-insert-header-setext-2)
  (define-key pel:for-markdown "1" 'markdown-insert-header-atx-1)
  (define-key pel:for-markdown "2" 'markdown-insert-header-atx-2)
  (define-key pel:for-markdown "3" 'markdown-insert-header-atx-3)
  (define-key pel:for-markdown "4" 'markdown-insert-header-atx-4)
  (define-key pel:for-markdown "5" 'markdown-insert-header-atx-5)
  (define-key pel:for-markdown "6" 'markdown-insert-header-atx-6)
  (define-key pel:for-markdown-preview "l" 'markdown-live-preview-mode)

  (when pel-use-speedbar
    (pel-add-speedbar-extension '(".md"
                                  ".markdown"
                                  ".mkd"
                                  ".mdown"
                                  ".mkdn"
                                  ".mdwn")))

  (pel-config-major-mode markdown pel:for-markdown
    (when (eq pel-use-markdown-toc 'update-toc-on-save)
      (defun pel-markdown-toc-refresh ()
        "Update the table of content if present."
        (if (and (require 'markdown-toc nil :no-error)
                 (fboundp 'markdown-toc--toc-already-present-p)
                 (fboundp 'markdown-toc-generate-toc))
            (when (markdown-toc--toc-already-present-p)
              (markdown-toc-generate-toc t))
          (display-warning 'pel-markdown-toc-refresh
                           "Cannot refresh table of content - missing elements!"
                           :error)))
      (declare-function pel-markdown-toc-refresh "pel_keys")
      ;; hook just for the markdown buffers
      (add-hook 'before-save-hook
                (function pel-markdown-toc-refresh)
                nil :local))))

;; ---------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> SPC M-r`` : reSTucturedText
(when pel-use-rst-mode
  ;; Nothing to install, rst-mode is built in Emacs

  (when pel-use-speedbar
    (pel-add-speedbar-extension '(".rst" ".stxt" ".rst.txt")))

  (define-pel-global-prefix pel:for-reST (kbd "<f11> SPC M-r"))
  (define-pel-global-prefix pel:rst-skel (kbd "<f11> SPC M-r <f12>"))

  ;; Add .stxt to the accepted file extensions for rst-mode
  ;; to the ones that are normally used: .rst and .rest
  (add-to-list 'auto-mode-alist '("\\.stxt\\'"  . rst-mode))
  ;; Also add .rst.txt to the accepted extensions for rst-mode.
  ;; This allows reStructuredText files to be rendered by tools
  ;; that can read normal text files.
  (add-to-list 'auto-mode-alist '("\\.rst.txt\\'"  . rst-mode))

  (define-key pel:for-reST (kbd "M--") 'pel-itemize-lines)
  (define-key pel:for-reST "." 'pel-rst-makelink)
  (define-key pel:for-reST "g" 'pel-rst-goto-ref-bookmark)
  (define-key pel:for-reST "s" 'pel-rst-set-ref-bookmark)
  ;;
  (define-key pel:for-reST "t" 'pel-rst-adorn-title)
  (define-key pel:for-reST "1" 'pel-rst-adorn-1)
  (define-key pel:for-reST "2" 'pel-rst-adorn-2)
  (define-key pel:for-reST "3" 'pel-rst-adorn-3)
  (define-key pel:for-reST "4" 'pel-rst-adorn-4)
  (define-key pel:for-reST "5" 'pel-rst-adorn-5)
  (define-key pel:for-reST "6" 'pel-rst-adorn-6)
  (define-key pel:for-reST "7" 'pel-rst-adorn-7)
  (define-key pel:for-reST "8" 'pel-rst-adorn-8)
  (define-key pel:for-reST "9" 'pel-rst-adorn-9)
  (define-key pel:for-reST "0" 'pel-rst-adorn-10)
  ;;
  (define-key pel:for-reST "=" 'pel-rst-adorn-same-level)
  (define-key pel:for-reST "+" 'pel-rst-adorn-increase-level)
  (define-key pel:for-reST "-" 'pel-rst-adorn-decrease-level)
  (define-key pel:for-reST "r" 'pel-rst-adorn-refresh)
  ;;
  (define-key pel:for-reST "n" 'rst-forward-section)
  (define-key pel:for-reST (kbd "<down>") 'rst-forward-section)
  (define-key pel:for-reST "p" 'rst-backward-section)
  (define-key pel:for-reST (kbd "<up>") 'rst-backward-section)
  ;;
  (define-key pel:for-reST "b" 'pel-rst-bold)
  (define-key pel:for-reST "i" 'pel-rst-italic)
  (define-key pel:for-reST "l" 'pel-rst-literal)
  (define-key pel:for-reST "`" 'pel-rst-interpreted)
  ;; experimental: TODO: may want to execute its constituents
  ;;                     when superword-mode is activated and de-activated or
  ;;                     when navigation functions are executed
  ;;                     (`forward-word' and `backward-word') in
  ;;                     reStructuredText when superword-mode is active.
  ;;                     For now just set underscore syntax according to the
  ;;                     presence of the minor mode.
  (define-key pel:for-reST "_" 'pel-rst-set-underscore-syntax)

  ;;
  (when pel-use-plantuml
    (define-key pel:for-reST  "u" 'pel-render-commented-plantuml))
  ;;
  (define-pel-global-prefix pel:rst-adorn-style (kbd "<f11> SPC M-r A"))
  (define-key pel:rst-adorn-style "d" 'pel-rst-adorn-default)
  (define-key pel:rst-adorn-style "S" 'pel-rst-adorn-Sphinx-Python)
  (define-key pel:rst-adorn-style "C" 'pel-rst-adorn-CRiSPer)

  (pel-eval-after-load rst
    (pel-config-major-mode rst pel:for-reST
      (pel--install-rst-skel pel:rst-skel)
      (when (and pel-use-imenu+
                 (fboundp 'imenup-add-defs-to-menubar))
        (imenup-add-defs-to-menubar)))))

;; ---------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> SPC M-g`` : Graphviz Dot
(when pel-use-graphviz-dot
  (pel-ensure-package graphviz-dot-mode from: melpa)
  (pel-autoload-file graphviz-dot-mode for: graphviz-dot-mode)

  ;; Global bindings for Graphviz-Dot
  (define-key pel:mode (kbd "M-g") 'graphviz-dot-mode)
  (define-key pel:mode "G"         'pel-render-commented-graphviz-dot)

  ;; Graphviz-Dot specific mode keys
  (define-pel-global-prefix pel:for-graphviz-dot (kbd "<f11> SPC M-g"))
  (define-key pel:for-graphviz-dot "c" 'compile)
  (define-key pel:for-graphviz-dot "p" 'graphviz-dot-preview)
  (define-key pel:for-graphviz-dot (kbd "TAB") 'graphviz-dot-indent-graph)

  (pel-config-major-mode graphviz-dot pel:for-graphviz-dot))

;; ---------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> SPC M-M`` : MscGen
(when pel-use-mscgen-mode
  (pel-install-github-file "thomsten/mscgen-mode/master"
                           "mscgen-mode.el")
  (pel-autoload-file mscgen-mode for:
                     mscgen-mode)
  ;; set the file extensions
  (pel-set-auto-mode mscgen-mode for:
                     "\\.\\(msc?\\|msc.txt\\)\\'")
  ;; Add speedbar support
  (when pel-use-speedbar
    (pel-add-speedbar-extension '(".msc" ".msc.txt")))

  ;; Add Key bindings
  (define-pel-global-prefix pel:for-mscgen (kbd "<f11> SPC M-M"))
  (define-key pel:for-mscgen "c" 'mscgen-compile)
  (define-key pel:for-mscgen "l" 'mscgen-insert-label-at-point)

  (pel-eval-after-load mscgen-mode
    (pel-config-major-mode mscgen pel:for-mscgen)))

;; ---------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> SPC M-u`` : PlantUML
(when pel-use-plantuml

  (pel-ensure-package plantuml-mode from: melpa)
  (pel-autoload-file plantuml-mode for:
                     plantuml-mode
                     plantuml-download-jar
                     plantuml-set-exec-mode)
  (when pel-use-flycheck-plantuml
    (pel-ensure-package flycheck-plantuml from: melpa))

  ;; keys inside pel:draw (see below)
  (define-pel-global-prefix pel:plantuml (kbd "<f11> D u"))
  (define-key pel:plantuml "u"         'plantuml-mode)
  (define-key pel:plantuml (kbd "M-d") 'plantuml-download-jar)
  (define-key pel:plantuml (kbd "M-x") 'plantuml-set-exec-mode)
  (define-key pel:plantuml "p"         'pel-render-commented-plantuml)

  ;; keys for PlantUML mode buffer
  (define-pel-global-prefix pel:for-plantuml (kbd "<f11> SPC M-u"))
  (define-key pel:for-plantuml (kbd "M-d")  'plantuml-enable-debug)
  (define-key pel:for-plantuml (kbd "M-D")  'plantuml-disable-debug)
  (define-key pel:for-plantuml "o"          'plantuml-set-output-type)
  (define-key pel:for-plantuml "b"          'plantuml-preview-buffer)
  (define-key pel:for-plantuml "r"          'plantuml-preview-region)
  (define-key pel:for-plantuml "c"          'plantuml-preview-current-block)
  (define-key pel:for-plantuml "p"          'plantuml-preview)
  (define-key pel:for-plantuml "/"          'plantuml-complete-symbol)
  (define-key pel:for-plantuml (kbd "TAB")  'plantuml-indent-line)

  (pel-config-major-mode plantuml pel:for-plantuml
    ;; Configure plantuml default execution mode according to PEL's selection.
    (if (boundp 'plantuml-default-exec-mode)
        (setq plantuml-default-exec-mode (if (eq pel-use-plantuml 'server)
                                             'server
                                           'jar))
      (display-warning 'pel-use-plantuml
                       "Unbound plantuml-default-exec-mode!"
                       :error))
    (pel-eval-after-load flycheck
      (require 'flycheck-plantuml)
      (declare-function flycheck-plantuml-setup "flycheck-plantuml")
      (flycheck-plantuml-setup))))

;; ---------------------------------------------------------------------------
;; Specification/Interface definition languages support
;; ----------------------------------------------------
;;
;; Early support.  No PDF nor major-mode specific key additional bindings yet.

(when pel-use-asn1-mode
  (pel-ensure-package asn1-mode from: melpa))


;; ---------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> SPC M-Y`` : YANG

(when pel-use-yang-mode
  ;; Installation control
  (pel-ensure-package yang-mode from: melpa)

  (define-pel-global-prefix pel:for-yang (kbd "<f11> SPC M-Y"))

  (when pel-use-speedbar
    (pel-add-speedbar-extension ".yang"))

  (pel-eval-after-load yang-mode
    (pel-config-major-mode yang pel:for-yang
      (pel-yang-setup-support))))

;; ---------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> =`` : Copy commands

(define-pel-global-prefix pel:copy (kbd "<f11> ="))
(define-key pel:copy " "  'pel-copy-whitespace-at-point)
(define-key pel:copy "("  'pel-copy-list-at-point)
(define-key pel:copy "."  'pel-copy-symbol-at-point)
(define-key pel:copy "a"  'pel-copy-line-start)
(define-key pel:copy "b"  'pel-copy-paragraph-start)
(define-key pel:copy "c"  'pel-copy-char-at-point)
(define-key pel:copy "e"  'pel-copy-line-end)
(define-key pel:copy "F"  'pel-copy-filename-at-point)
(define-key pel:copy "f"  'pel-copy-function-at-point)
(define-key pel:copy "H"  'pel-copy-paragraph-at-point)
(define-key pel:copy "h"  'pel-copy-paragraph-end)
(define-key pel:copy "l"  'pel-copy-marked-or-whole-line)
(define-key pel:copy "="  'pel-copy-marked-or-whole-line)
(define-key pel:copy "r" #'copy-rectangle-as-kill)
(define-key pel:copy "s"  'pel-copy-sentence-at-point)
(define-key pel:copy "u"  'pel-copy-url-at-point)
(define-key pel:copy "w"  'pel-copy-word-at-point)
(define-key pel:copy "x"  'pel-copy-sexp-at-point)
;;
(global-set-key (kbd "<f11> +") 'pel-copy-marked-or-whole-line)
(global-set-key (kbd "<f11> <kp-add>") 'pel-copy-marked-or-whole-line)
(global-set-key (kbd "M-w") 'pel-copy-marked-or-whole-line)
                                        ; replaces kill-ring-save

;; ---------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> -`` : Kill commands
;; - Function Keys - <f11> - Prefix ``<f11> DEL`` : delete commands
(define-pel-global-prefix pel:kill   (kbd "<f11> -"))
(define-pel-global-prefix pel:delete (kbd "<f11> DEL"))

(define-key pel:delete (kbd "DEL") 'c-hungry-delete-backwards)

(define-key pel:kill   " "  'pel-kill-whitespace-at-point)
(define-key pel:delete " "  'pel-delete-whitespace-at-point)
(define-key pel:kill   "c"  'pel-kill-char-at-point)
;; DEL
(define-key pel:kill   "w"  'pel-kill-word-at-point)
(define-key pel:delete "w"  'pel-delete-word-at-point)
(define-key pel:kill   "q"  'pel-kill-word-part)
(define-key pel:delete "q"  'pel-delete-word-part)
(define-key pel:kill   "."  'pel-kill-symbol-at-point)
(define-key pel:delete "."  'pel-delete-symbol-at-point)
(define-key pel:kill   ","  'pel-kill-symbol-part)
(define-key pel:delete ","  'pel-delete-symbol-part)
(define-key pel:kill   "a"  'pel-kill-from-beginning-of-line)
(define-key pel:delete "a"  'pel-delete-from-beginning-of-line)
(define-key pel:kill   "e" #'kill-line) ; also C-k
(define-key pel:delete "e"  'pel-delete-line)
(define-key pel:kill   "s"  'pel-kill-sentence-at-point)
(define-key pel:delete "s"  'pel-delete-sentence-at-point)
(define-key pel:kill   "b" #'backward-kill-paragraph)
(define-key pel:delete "b"  'pel-backward-delete-paragraph)
(define-key pel:kill   "h" #'kill-paragraph)
(define-key pel:delete "h"  'pel-delete-paragraph)
(define-key pel:kill   "H"  'pel-kill-paragraph-at-point)
(define-key pel:delete "H"  'pel-delete-paragraph-at-point)
(define-key pel:kill   "x"  'pel-kill-sexp-at-point)
(define-key pel:delete "x"  'pel-delete-sexp-at-point)
(define-key pel:kill   "[" #'backward-kill-sexp)
(define-key pel:delete "["  'pel-backward-delete-sexp)
(define-key pel:kill   "]" #'kill-sexp)
(define-key pel:delete "]"  'pel-delete-sexp)
(define-key pel:kill   "("  'pel-kill-list-at-point)
(define-key pel:delete "("  'pel-delete-list-at-point)
(define-key pel:kill   "f"  'pel-kill-function-at-point)
(define-key pel:delete "f"  'pel-delete-function-at-point)
(define-key pel:kill   "F"  'pel-kill-filename-at-point)
(define-key pel:delete "F"  'pel-delete-filename-at-point)
(define-key pel:kill   "u"  'pel-kill-url-at-point)
(define-key pel:delete "u"  'pel-delete-url-at-point)
(define-key pel:kill   "r" #'kill-rectangle)
(define-key pel:delete "r"  'pel-delete-rectangle)
(define-key pel:kill   ";"  'pel-kill-all-comments)
(define-key pel:delete ";"  'pel-delete-all-comments)
(define-key pel:delete (kbd "M-SPC")  'pel-delete-all-empty-lines)

(define-key pel:delete "*" #'delete-duplicate-lines)
(define-key pel:kill   "l"  'pel-kill-or-delete-marked-or-whole-line)

(when pel-emacs-is-graphic-p
  (global-set-key (kbd "s-x") 'pel-kill-or-delete-marked-or-whole-line)
  (global-set-key (kbd "C-K") 'pel-delete-line))
(global-set-key (kbd "C-w")     'pel-kill-or-delete-marked-or-whole-line)
;; Note: also assigned via pel-kp-subtact

(global-set-key "\C-\\"         'pel-kill-from-beginning-of-line)

;; - Kill beginning of line
;; ------------------------
;; Note: the following binding does NOT work in Terminal.
(global-set-key [(meta delete)] 'kill-line)

;; - Delete whitespace between point and next non-whitespace
;; ---------------------------------------------------------
(global-set-key [(control shift delete)] #'kill-word)
(global-set-key [(control delete)]        'pel-delete-to-next-visible)
;; - Delete next word(s) and whitespace following it
;; -------------------------------------------------
(global-set-key (kbd "M-D")               'pel-kill-word-and-whitespace)

;; ---------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> ,`` : auto-completion

(when pel-use-auto-complete
  ;; Defer loading of auto-complete using its autoload that will be
  ;; trigerred when the one of the pel-auto-complete-mode or
  ;; pel-global-auto-complete-mode is executed.
  (pel-ensure-package auto-complete from: melpa)
  (pel-autoload-file auto-complete for:
                     auto-complete-mode
                     global-auto-complete-mode))

(when pel-use-company
  ;; Defer-load company.el via the autoload company-mode and
  ;; global-autoload-mode are called by one of the pel functions.
  (pel-ensure-package company from: melpa)
  (pel-autoload-file company for:
                     company-mode
                     global-company-mode))

(define-pel-global-prefix pel:auto-completion (kbd "<f11> ,"))
(define-key pel:auto-completion   "?"   'pel-completion-help)
(when pel-use-auto-complete
  (define-key pel:auto-completion "A"  'pel-global-auto-complete-mode)
  (define-key pel:auto-completion "a"  'pel-auto-complete-mode))
(when pel-use-company
  (define-key pel:auto-completion "C"  'pel-global-company-mode)
  (define-key pel:auto-completion "c"  'pel-company-mode))
(when (or pel-use-auto-complete pel-use-company)
  (define-key pel:auto-completion ","  'pel-complete)
  ;; TODO: when no autocompletion is active,
  ;;       re-install the binding that was present
  ;;       at the moment it was turned-on.
  (global-set-key (kbd "M-l") 'pel-complete))
;; (add-hook 'after-init-hook 'global-company-mode)

;; ---------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> .`` : mark commands

(define-pel-global-prefix pel:mark (kbd "<f11> ."))
(define-key pel:mark       " "         'pel-push-mark-no-activate)
(define-key pel:mark       "`"         'pel-jump-to-mark)
(define-key pel:mark       "."        #'exchange-point-and-mark)
(define-key pel:mark       ","         'pel-exchange-point-and-mark-no-activate)
(define-key pel:mark       "?"         'pel-mark-ring-stats)
(define-key pel:mark (kbd  "DEL")      'pel-popoff-mark-ring)
(define-key pel:mark (kbd  "<up>")     'pel-mark-line-up)
(define-key pel:mark (kbd  "<down>")   'pel-mark-line-down)
(define-key pel:mark       "M"        #'transient-mark-mode)
(define-key pel:mark       "b"        #'mark-whole-buffer)
(define-key pel:mark       "g"        #'pop-global-mark)
(define-key pel:mark       "h"        #'mark-paragraph)
(define-key pel:mark       "p"        #'mark-page)
(define-key pel:mark       "r"         'pel-cua-rectangle-mark)
(define-key pel:mark       "s"        #'set-mark-command)
(define-key pel:mark       "w"        #'mark-word)
(define-key pel:mark       "x"        #'mark-sexp)
;;
(global-set-key (kbd "M-`")            'pel-jump-to-mark)
(global-set-key (kbd "M-S-<up>")       'pel-mark-line-up)
(global-set-key (kbd "M-S-<down>")     'pel-mark-line-down)

(when pel-use-expand-region
  (pel-ensure-package expand-region from: melpa)
  (pel-autoload-file expand-region for: er/expand-region)
  (define-key pel:mark     "="  'er/expand-region)
  (global-set-key   (kbd "M-=") 'er/expand-region))

;; ---------------------------------------------------------------------------
;; CUA mode setup
;; Activate the ability to use cua-rectangle-mark-mode without using
;; the CUA re-binding of C-c, C-v, C-x and C-z.

;; TODO: Keep this? Create a (persistent?) Hydra for this?
;;       Fix the pel-cua-rectangle-mark??
(global-set-key (kbd "<f11> R") #'cua-rectangle-mark-mode)
(global-set-key (kbd "<f11> [")  'pel-cua-move-rectangle-left)
(global-set-key (kbd "<f11> ]")  'pel-cua-move-rectangle-right)

;; ---------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> ;`` : comment commands

(define-pel-global-prefix pel:comment (kbd "<f11> ;"))
;;
(define-key pel:comment "A"    'pel-toggle-comment-auto-fill-only-comments)
(define-key pel:comment "B"           #'comment-box)
(define-key pel:comment (kbd  "DEL")   'pel-delete-all-comments)
(define-key pel:comment "-"            'pel-kill-all-comments)
(define-key pel:comment "k"           #'comment-kill)
(define-key pel:comment "l"           #'comment-line)
(define-key pel:comment "b"            'pel-comment-start)
(define-key pel:comment "m"            'pel-comment-middle)
(define-key pel:comment "e"            'pel-comment-end)
(define-key pel:comment "u"            'uncomment-region)
(define-key pel:comment "D"            'pel-toggle-all-docstrings)
(define-key pel:comment "\""           'pel-hide/show-all-docstrings)
(define-key pel:comment "d"            'pel-toggle-docstring)
(define-key pel:comment "'"            'pel-hide/show-docstring)
(define-key pel:comment "?"            'pel-comment-show-variables)

(when pel-use-hide-comnt
  ;; Download and byte-compile hide-comnt.el if its not present
  ;; Do it after compiling pel_keys.el, when pel-init load pel_keys.
  (pel-install-github-file "emacsmirror/hide-comnt/master" "hide-comnt.el")
  (pel-autoload-file hide-comnt for:
                     hide/show-comments
                     hide/show-comments-toggle)
  (define-key pel:comment ";" 'hide/show-comments-toggle)
  (define-key pel:comment ":" 'hide/show-comments))

;; ---------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> M-/`` : Hide/Show commands
(define-pel-global-prefix pel:hide-show (kbd "<f11> M-/"))

(define-key pel:hide-show (kbd "M-/") 'pel-toggle-hide-indent)

(when pel-use-hide-lines
  (pel-ensure-package hide-lines from: melpa)
  (global-set-key (kbd "C-c /") 'hide-lines)
  (define-key pel:hide-show "h"         'hide-lines)
  (define-key pel:hide-show (kbd "M-h") 'hide-lines-matching)
  (define-key pel:hide-show (kbd "M-o") 'hide-lines-not-matching)
  (define-key pel:hide-show (kbd "M-s") 'hide-lines-show-all)
  (define-key pel:hide-show "b"         'hide-blocks)
  (define-key pel:hide-show (kbd "M-b") 'hide-blocks-matching)
  (define-key pel:hide-show (kbd "M-p") 'hide-blocks-not-matching))

(when pel-use-origami
  (pel-install-github-files "pierre-rouleau/origami.el/master"
                            ;; download/compile origami-parsers first because
                            ;; it is required by origami.el.
                            '("origami-parsers.el"
                              "origami.el"))
  (pel-autoload-file origami for:
                     origami-mode
                     global-origami-mode)

  (when pel-use-lsp-origami
    (pel-ensure-package lsp-origami from:  melpa))

  ;; TODO: find why using M-o for origami-mode prevents the F11 M-/ F1, F2 and F3
  ;;       keys from working work properly.
  (define-key pel:hide-show "o" 'origami-mode)
  (define-key pel:hide-show "O" 'global-origami-mode)
  (defun pel--activate-origami ()
    "Activate origami-mode key map bindings."
    (when (boundp 'origami-mode-map)
      (define-key origami-mode-map (kbd "M-/ M-o") 'origami-open-node)
      (define-key origami-mode-map (kbd "M-/ O")   'origami-open-node-recursively)
      (define-key origami-mode-map (kbd "M-/ M-s") 'origami-show-node)
      (define-key origami-mode-map (kbd "M-/ M-c") 'origami-close-node)
      (define-key origami-mode-map (kbd "M-/ C")   'origami-close-node-recursively)
      (define-key origami-mode-map (kbd "M-/ M-t") 'origami-toggle-node)
      (define-key origami-mode-map (kbd "M-/ M->") 'origami-forward-toggle-node)
      (define-key origami-mode-map (kbd "M-/ TAB")  'origami-recursively-toggle-node)
      (define-key origami-mode-map (kbd "M-/ M-O") 'origami-open-all-nodes)
      (define-key origami-mode-map (kbd "M-/ M-C") 'origami-close-all-nodes)
      (define-key origami-mode-map (kbd "M-/ M-T") 'origami-toggle-all-nodes)
      (define-key origami-mode-map (kbd "M-/ M-.") 'origami-show-only-node)
      (define-key origami-mode-map (kbd "M-/ M-p") 'origami-previous-fold)
      (define-key origami-mode-map (kbd "M-/ M-n") 'origami-next-fold)
      (define-key origami-mode-map (kbd "M-/ f")   'origami-forward-fold)
      (define-key origami-mode-map (kbd "M-/ M-f") 'origami-forward-fold-same-level)
      (define-key origami-mode-map (kbd "M-/ M-b") 'origami-backward-fold-same-level)
      (define-key origami-mode-map (kbd "M-/ M-u") 'origami-undo)
      (define-key origami-mode-map (kbd "M-/ M-U") 'origami-redo)
      (define-key origami-mode-map (kbd "M-/ R")   'origami-reset)
      (when pel-use-hippie-expand
        (define-key origami-mode-map (kbd "M-/ M-/") 'hippie-expand))
      (when pel-use-hide-comnt
        (define-key origami-mode-map (kbd "M-/ M-;") 'hide/show-comments-toggle))
      (define-key origami-mode-map (kbd "M-/ M-d")   'pel-toggle-docstring)
      (define-key origami-mode-map (kbd "M-/ M-D")   'pel-toggle-all-docstrings))
    (declare-function pel--activate-origami "pel_keys"))

  (add-hook 'origami-mode-hook (function pel--activate-origami)))

;; ---------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> ?`` : Help /apropos/info commands

;; pel:help prefix is defined at the beginning of the <f11> section to allow
;; insertion of help under that prefix, later when logic dictates that
;; appropriate functionality is available.
;;
;; To help keep track what keay are used, the list of key under the pel:help
;; prefix are shown below.
;;
;;   Used `pel:help' keys:  . ? A a b c d e f i k m p P s S w X

(define-key pel:help "." 'pel-mark-ring-stats)
(define-key pel:help "m"  #'man)
(define-key pel:help "w"  #'woman)
(define-key pel:help "?"  'pel-show-major-mode)
(define-key pel:help "$"  'pel-spell-show-use)
(define-key pel:help "f"  'which-function-mode)
(define-key pel:help "P"  'pel-help-pdfs-dir)
(define-key pel:help "p"  'pel-help-pdf-select)
(define-key pel:     "p"  'pel-help-pdf-select) ; faster key for help

(global-set-key (kbd "<M-f8>")   #'man)
(global-set-key (kbd "<M-S-f8>") #'woman)

(pel-autoload-file pel-help for:
                   pel-show-kill-ring
                   pel-show-major-mode)

(when pel-use-ascii-table
  (pel-ensure-package ascii-table from: melpa)
  (pel-autoload-file ascii-table for: ascii-table)
  (define-key pel:help "A" 'ascii-table))

(when pel-use-helpful
  (setq pel-use-elisp-refs t)           ; helpful uses elisp-refs
  (pel-ensure-package helpful from: melpa)
  (define-pel-global-prefix pel:helpful (kbd "<f1> <f2>"))
  (define-key pel:helpful "a" 'helpful-callable)
  (define-key pel:helpful "f" 'helpful-function)
  (define-key pel:helpful "m" 'helpful-macro)
  (define-key pel:helpful "c" 'helpful-command)
  (define-key pel:helpful "k" 'helpful-key)
  (define-key pel:helpful "v" 'helpful-variable)
  (define-key pel:helpful "." 'helpful-at-point)
  (define-key pel:helpful "o" 'helpful-symbol)
  (when (and pel-use-helpful-with-counsel
             pel-use-counsel)
    (pel-setq counsel-describe-function-function 'helpful-callable)
    (pel-setq counsel-describe-variable-function 'helpful-variable)))

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; - Function Keys - <f11> - Prefix ``<f11> ? a`` : Help Apropos commands

(define-pel-global-prefix pel:apropos (kbd "<f11> ? a"))
(define-key pel:apropos "a"  #'apropos)
(define-key pel:apropos "c"  #'apropos-command)
(define-key pel:apropos "d"  #'apropos-documentation)
(define-key pel:apropos "L"  #'apropos-library)
(define-key pel:apropos "l"  #'apropos-local-variable)
(define-key pel:apropos "o"  #'apropos-user-option)
(define-key pel:apropos "u"  #'apropos-value)
(define-key pel:apropos "v"  #'apropos-variable)

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; - Function Keys - <f11> - Prefix ``<f11> ? b`` : Emacs Bug Db Browsing

(when pel-use-debbugs
  (define-pel-global-prefix pel:emacs-bugs (kbd "<f11> ? b"))
  (pel-ensure-package debbugs from: gnu)
  (define-key pel:emacs-bugs "a" 'debbugs-gnu)
  (define-key pel:emacs-bugs "s" 'debbugs-gnu-search)
  (define-key pel:emacs-bugs "u" 'debbugs-gnu-usertags)
  (define-key pel:emacs-bugs "p" 'debbugs-gnu-patches)
  (define-key pel:emacs-bugs "b" 'debbugs-gnu-bugs)
  (define-key pel:emacs-bugs "t" 'debbugs-gnu-tagged)
  (define-key pel:emacs-bugs "A" 'debbugs-org)
  (define-key pel:emacs-bugs "S" 'debbugs-org-search)
  (define-key pel:emacs-bugs "P" 'debbugs-org-patches)
  (define-key pel:emacs-bugs "B" 'debbugs-org-bugs)
  (define-key pel:emacs-bugs "T" 'debbugs-org-tagged))

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; - Function Keys - <f11> - Prefix ``<f11> ? i`` : Help Info commands

(define-pel-global-prefix pel:info (kbd "<f11> ? i"))
(define-key pel:info "a"  #'info-apropos)
(define-key pel:info "i"  #'info)
(define-key pel:info "m"  #'info-display-manual)

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; - Function Keys - <f11> - Prefix ``<f11> ? d`` : Describe

(defun pel-show-buffer-file-encoding ()
  "Show coding system of file in current buffer."
  (interactive)
  (describe-symbol 'buffer-file-coding-system))

(define-pel-global-prefix pel:describe (kbd "<f11> ? d"))
(define-key pel:describe "$"  'pel-spell-show-use)
(define-key pel:describe "a"  'pel-show-face-at-point)
(define-key pel:describe "c" #'list-colors-display)
(define-key pel:describe "e" #'pel-show-buffer-file-encoding)
(define-key pel:describe "C" #'describe-coding-system)
(define-key pel:describe "F" #'list-faces-display)
(define-key pel:describe "f"  'pel-show-window-filename-or-buffer-name)
(define-key pel:describe "H" #'list-command-history)
(define-key pel:describe "i" #'list-input-methods)
(define-key pel:describe "k"  'pel-show-kill-ring)
(define-key pel:describe "l" #'what-line)
(define-key pel:describe "p" #'what-cursor-position)
(define-key pel:describe "s"  'pel-show-char-syntax)
(define-key pel:describe "w"  'pel-show-window-sizes)

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; - Function Keys - <f11> - Prefix ``<f11> ? e`` : Emacs info
;; Used keys:
;; P
;; c l m p r s t u v x
;; C-p
;; M-S
;; <f2>

(define-pel-global-prefix pel:emacs (kbd "<f11> ? e"))
(define-key pel:emacs (kbd "C-p") #'list-processes)
(define-key pel:emacs "c"  'pel-emacs-command-stats)
(define-key pel:emacs "i"  'pel-imenu-print-vars)
(define-key pel:emacs "o"  'pel-outline-print-vars)
(define-key pel:emacs "r"  'pel-open-emacs-refcard)
(define-key pel:emacs "s" #'list-load-path-shadows)
(define-key pel:emacs "t"  'pel-show-init-time)
(define-key pel:emacs "u" #'emacs-uptime)
(define-key pel:emacs "v" #'emacs-version)
(define-key pel:emacs "x"  'pel-emacs-executable)
(define-key pel:emacs "?"  'pel-package-info)
(define-key pel:emacs "."  'pel-syntax-at-point)
(define-key pel:emacs (kbd "M-S") 'pel-setup-info)
(define-key pel:emacs (kbd "<f2>") 'pel-setup-info-dual-environment)

(global-set-key (kbd "<M-S-f9>")  'pel-show-init-time)

(pel-autoload-file pel-emacs for:
                   pel-emacs-load-stats
                   pel-emacs-mem-stats)
(define-key pel:emacs "l"  'pel-emacs-load-stats)
(define-key pel:emacs "m"  'pel-emacs-mem-stats)

(pel-autoload-file pel-pathmng for: pel-show-load-path)
(define-key pel:emacs "p" 'pel-emacs-load-path)

;; Profiling support: esup
(when (and pel-use-esup
           pel-emacs-is-graphic-p)
  (pel-ensure-package esup from: melpa)
  (pel-autoload-file esup for: esup)
  (define-key pel:emacs "P"  'esup))

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; - Function Keys - <f11> - Prefix ``<f11> ? k`` : Info on Keys

(define-pel-global-prefix pel:keys (kbd "<f11> ? k"))
(define-key pel:keys "#"  'pel-show-mac-numlock)
(define-key pel:keys "l" #'view-lossage)
(define-key pel:keys "m" #'describe-mode)

(when pel-use-free-keys
  (pel-ensure-package free-keys from: melpa)
  (pel-autoload-file free-keys for: free-keys)
  (define-key pel:keys "f" #'free-keys))

(when pel-use-bind-key
  (pel-ensure-package bind-key from: melpa)
  (pel-autoload-file bind-key for: describe-personal-keybindings)
  (define-key pel:keys "b" #'describe-personal-keybindings))

(when pel-use-which-key
  ;; List key completions: help show the f11 bindings.
  ;; When requested, delay a little to speed init time.
  ;; Note that "<f11> ? k k" will execute autoloaded
  ;; command which-key-show-major-mode which will force
  ;; loading and ensure the key mode if it's not already loaded.
  (pel-ensure-package     which-key from: melpa)
  (pel-require-after-init which-key 1)
  (pel-autoload-file which-key for:
                     which-key-mode
                     which-key-show-major-mode)
  (define-key pel:keys  "K"  'which-key-mode)
  (define-key pel:keys  "k"  'which-key-show-major-mode)
  (pel-eval-after-load which-key
    (which-key-mode 1)))

;; ---------------------------------------------------------------------------
;; Keycast and logging
;; -------------------
(when pel-use-keycast
  (pel-ensure-package keycast from: melpa)
  (pel-autoload-file keycast for: keycast-mode)
  (define-key pel:keys  "c"  'keycast-mode))

(when pel-use-command-log-mode
  (define-pel-global-prefix pel:command-log (kbd "<f11> ? k c"))
  (pel-install-github-file "pierre-rouleau/command-log-mode/master"
                           "command-log-mode.el")
  (pel-autoload-file command-log-mode for:
                     command-log-mode
                     global-command-log-mode)
  (define-key pel:command-log "c" 'command-log-mode)
  (define-key pel:command-log "C" 'global-command-log-mode)
  (pel-eval-after-load command-log-mode
    (define-key pel:command-log "o" 'clm/open-command-log-buffer)
    (define-key pel:command-log "." 'clm/close-command-log-buffer)
    (define-key pel:command-log "/" 'clm/toggle-log-all)))

(when pel-use-interaction-log-mode
  (define-pel-global-prefix pel:interaction-log (kbd "<f11> ? k i"))
  (pel-ensure-package interaction-log from: melpa)
  (defun pel-interaction-log-buffer ()
    "Show interaction log buffer."
    (interactive)
    (when (boundp 'ilog-buffer-name)
      (display-buffer ilog-buffer-name)))
  (pel-autoload-file interaction-log for:
                     interaction-log-mode)
  (define-key pel:interaction-log "i" 'interaction-log-mode)
  (define-key pel:interaction-log "b" 'pel-interaction-log-buffer)
  (define-key pel:interaction-log "f" 'ilog-show-in-new-frame)
  (define-key pel:interaction-log "n" 'ilog-toggle-display-buffer-names)
  (define-key pel:interaction-log "v" 'ilog-toggle-view))

;; -----------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> $`` : Spell Check

;; popup is used in Terminal mode for spell check menu,
;; and must be available when pel-spell-init is called.
(when pel-emacs-is-a-tty-p
  (pel-ensure-package popup from: melpa)
  (pel-autoload-file popup for: pel-spell-init))

(define-pel-global-prefix pel:spell (kbd "<f11> $"))
;;
(autoload 'ispell-check-version "ispell")
(pel-eval-after-load ispell (pel-spell-init-from-user-option))

(define-key pel:spell "." #'ispell)
(define-key pel:spell ";" #'ispell-comments-and-strings)
(define-key pel:spell "?"  'pel-spell-show-use)
(define-key pel:spell "D"  'pel-spell-change-dictionary)
(define-key pel:spell "F" #'flyspell-mode)
(define-key pel:spell "K" #'ispell-kill-ispell)
(define-key pel:spell "P" #'flyspell-prog-mode)
(define-key pel:spell "b" #'ispell-buffer)
(define-key pel:spell "m" #'ispell-message)
(define-key pel:spell "r" #'ispell-region)
(define-key pel:spell "v" #'ispell-check-version)
(define-key pel:spell "$" 'flyspell-correct-word-before-point)
(define-key pel:spell (kbd "M-f") 'pel-spell-toggle-prevent-flyspell)

(defun pel-flyspell-auto-correct-previous-word (position)
  "Flyspell previous word from POSITION if flyspell mode is active.
See `flyspell-auto-correct-previous-word' for more info."
  (interactive "d")
  (if (and (fboundp 'flyspell-auto-correct-previous-word)
           (or (and (boundp 'flyspell-mode) flyspell-mode)
               (and (boundp 'flyspell-prog-mode) flyspell-prog-mode)))
      (flyspell-auto-correct-previous-word position)
    (user-error "Activate flyspell first!")))
(define-key pel: "\\"  'pel-flyspell-auto-correct-previous-word)

;;Activate Flyspell for modes identified by PEL customization
;;
(declare-function pel-spell-maybe-activate-flyspell "pel-spell")
(pel-add-hook-for
 'pel-modes-activating-flyspell-mode
 (function pel-spell-maybe-activate-flyspell))

(declare-function pel-spell-maybe-activate-flyspell-prog "pel-spell")
(pel-add-hook-for
 'pel-modes-activating-flyspell-prog-mode
 (function pel-spell-maybe-activate-flyspell-prog))

;; Text Translation
(when (and pel-use-go-translate
           pel-emacs-27-or-later-p)
  (pel-ensure-package go-translate from: melpa))

;; ---------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> '`` : bookmark commands

(define-pel-global-prefix pel:bookMark (kbd "<f11> '"))
(define-key pel:bookMark "b" #'bookmark-jump)
(define-key pel:bookMark "B" #'bookmark-jump-other-window)
(define-key pel:bookMark "d" #'bookmark-delete)
(define-key pel:bookMark "F" #'bookmark-insert)
(define-key pel:bookMark "f" #'bookmark-insert-location)
(define-key pel:bookMark "L" #'bookmark-load)
(define-key pel:bookMark "l" #'bookmark-bmenu-list)
(define-key pel:bookMark "m" #'bookmark-set)
(define-key pel:bookMark "M" #'bookmark-set-no-overwrite)
(define-key pel:bookMark "r" #'bookmark-rename)
(define-key pel:bookMark "s" #'bookmark-save)
(define-key pel:bookMark "w" #'bookmark-write)


;; Visible Bookmark (bm.el)
;; ------------------------
;; configure bm package to be loaded only on first use.
(when pel-use-bm
  (pel-ensure-package bm from: melpa)
  (pel-autoload-file bm for:
                     bm-next
                     bm-previous
                     bm-show-all
                     bm-toggle
                     bm-buffer-save
                     bm-buffer-restore)
  (global-set-key (kbd "<f2> <f2>")  'bm-next)
  (define-key pel:bookMark "'"  'bm-toggle) ; toggle visible bookmark
  (define-key pel:bookMark "?"  'bm-show-all)
  (define-key pel:bookMark "n"  'bm-next)
  (define-key pel:bookMark "p"  'bm-previous)

  ;; Ensure that bm restores bookmark when it loads.
  (defvar bm-restore-repository-on-load) ;  Prevent byte-compiler warnings
  (setq bm-restore-repository-on-load t)

  (pel-eval-after-load bm
    ;;  Prevent lint warnings using empty defvar
    ;; Allow cross-buffer 'next'
    (pel-setq bm-cycle-all-buffers t)

    ;; where to store persistent files
    (pel-setq bm-repository-file "~/.emacs.d/bm-repository")

    ;; save bookmarks
    (pel-setq-default bm-buffer-persistence t)

    ;; Loading the repository from file when on start up.
    (add-hook 'after-init-hook 'bm-repository-load)

    ;; Saving bookmarks
    (add-hook 'kill-buffer-hook 'bm-buffer-save)

    ;; Saving the repository to file on exit.
    ;; kill-buffer-hook is not called when Emacs is killed, so we
    ;; must save all bookmarks first.
    (add-hook 'kill-emacs-hook
              (lambda nil
                (when (and (fboundp 'bm-buffer-save-all)
                           (fboundp 'bm-repository-save))
                  (bm-buffer-save-all)
                  (bm-repository-save))))

    ;; The `after-save-hook' is not necessary to use to achieve persistence,
    ;; but it makes the bookmark data in repository more in sync with the file
    ;; state.
    (add-hook 'after-save-hook 'bm-buffer-save)

    ;; Restoring bookmarks
    (add-hook 'find-file-hook    'bm-buffer-restore)
    (add-hook 'after-revert-hook 'bm-buffer-restore)

    ;; The `after-revert-hook' is not necessary to use to achieve persistence,
    ;; but it makes the bookmark data in repository more in sync with the file
    ;; state. This hook might cause trouble when using packages
    ;; that automatically reverts the buffer (like vc after a check-in).
    ;; This can easily be avoided if the package provides a hook that is
    ;; called before the buffer is reverted (like `vc-before-checkin-hook').
    ;; Then new bookmarks can be saved before the buffer is reverted.
    ;; Make sure bookmarks is saved before check-in (and revert-buffer)
    (add-hook 'vc-before-checkin-hook 'bm-buffer-save)))

;; ---------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> <tab>`` : indentation

;; More powerful indent-rigidly: pel-indent-rigidly
;; ------------------------------------------------
;; the pel-indent-rigidly does the same as the original command
;; but also allow indenting the current line even if nothing is marked.
(global-set-key [remap indent-rigidly] 'pel-indent-rigidly)

;; Add indented line below
(global-set-key  (kbd "<M-RET>") 'pel-newline-and-indent-below)
(define-key pel: (kbd "<M-RET>") 'pel-toggle-newline-indent-align)
;; See ``<f11> t a ?`` to show the state.

(define-pel-global-prefix pel:indent (kbd "<f11> TAB"))
(define-key pel:indent "r"            #'indent-relative)
(define-key pel:indent "c"             'pel-indent-lines)
(define-key pel:indent "C"             'pel-unindent-lines)
(define-key pel:indent (kbd "TAB")     'pel-indent-rigidly)
(define-key pel:indent (kbd "<RET>")   'pel-newline-and-indent-below)

(global-set-key (kbd "<backtab>") 'pel-unindent-lines)

(when pel-use-indent-tools
  (define-key pel:indent ">" 'indent-tools-hydra/body)

  (when (eq pel-indent-tools-key-bound 'globally)
    ;; load indent-tools to map its key globally.  Delay it because we need
    ;; it loaded to read the key prefix identified by customization.
    (run-with-idle-timer
     1 nil
     (lambda ()
       (when (and
              (require 'indent-tools nil :noerror)
              (boundp 'indent-tools-keymap-prefix))
         (global-set-key indent-tools-keymap-prefix 'indent-tools-hydra/body))))))

(when pel-use-smart-shift
  (pel-ensure-package smart-shift from: melpa)
  (define-key pel:indent "s" 'smart-shift-mode)
  (define-key pel:indent "S" 'global-smart-shift-mode)

  (defun pel--setup-smart-shift ()
    "Set key bindings for smart-shift."
    (defvar smart-shift-mode-map)
    (let ((map smart-shift-mode-map))
      (when pel-smart-shift-keybinding
        (cond
         ((eq pel-smart-shift-keybinding 'control-cursor)
          (define-key map (kbd "C-c <C-left>") 'smart-shift-left)
          (define-key map (kbd "C-c <C-right>") 'smart-shift-right)
          (define-key map (kbd "C-c <C-up>") 'smart-shift-up)
          (define-key map (kbd "C-c <C-down>") 'smart-shift-down))
         ((eq pel-smart-shift-keybinding 'f9)
          (define-key map (kbd "<f9> <left>") 'smart-shift-left)
          (define-key map (kbd "<f9> <right>") 'smart-shift-right)
          (define-key map (kbd "<f9> <up>") 'smart-shift-up)
          (define-key map (kbd "<f9> <down>") 'smart-shift-down))))))
  (declare-function pel--setup-smart-shift "pel_keys")

  (add-hook 'smart-shift-mode-hook (function pel--setup-smart-shift)))
;; ---------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> |`` : Windows scroll lock commands
;;
;; - Scrolling up & down without moving point
;; ------------------------------------------
;; - uses: pel-scroll
;;
;; Cursor-Keys:
;; - Implement a simple/fast single line scrolling that
;;   also support the dual window scroll lock.
;;   Assigned to multiple key-chords to make it easy to use
;;   in multiple situations:
;;   - Meta up/down
;;   - Meta f5/f62 in org-mode, since Meta up/down do something else.

;; scroll text up: toward small line number
(global-set-key (kbd "<M-down>")   'pel-scroll-up)
(global-set-key (kbd "<M-S-f5>")   'pel-scroll-up-other)
;; scroll text down: toward large line number
(global-set-key (kbd "<M-up>")    'pel-scroll-down)
(global-set-key (kbd "<M-S-f6>")  'pel-scroll-down-other)
;; and with the mouse in terminal mode
(when pel-emacs-is-a-tty-p
  ;; activate mouse-based scrolling
  (global-set-key (kbd "<mouse-4>") 'pel-scroll-down)
  (global-set-key (kbd "<mouse-5>") 'pel-scroll-up))

(define-pel-global-prefix pel:scroll (kbd "<f11> |"))
;;
(define-key pel:scroll "|"  'pel-toggle-scroll-sync)
(define-key pel:scroll "+"  'pel-add-window-to-scroll-sync)
(define-key pel:scroll "-"  'pel-remove-window-from-scroll-sync)
(define-key pel:scroll (kbd "<up>")   'pel-scroll-down-only-this)
(define-key pel:scroll (kbd "<down>") 'pel-scroll-up-only-this)
(define-key pel:scroll (kbd "<left>") 'pel-scroll-right)
(define-key pel:scroll (kbd "<right>") 'pel-scroll-left)
(define-key pel:scroll "{" 'pel-scroll-right)
(define-key pel:scroll "}" 'pel-scroll-left)
(define-key pel:scroll "a" #'scroll-all-mode)
(define-key pel:scroll "f" #'follow-mode)
(define-key pel:scroll "l" #'scroll-lock-mode)
(define-key pel:scroll "t"  'auto-revert-tail-mode)


(when pel-use-smooth-scrolling
  (pel-ensure-package smooth-scrolling from: melpa)
  (pel-autoload-file smooth-scrolling for: smooth-scrolling-mode)
  (define-key pel:scroll "s" 'smooth-scrolling-mode)
  (pel-eval-after-load smooth-scrolling
    (smooth-scrolling-mode 1))
  ;; activate smooth scrolling after startup
  (pel-require-after-init smooth-scrolling 2))

;; ---------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> a`` : abbreviations

(define-pel-global-prefix pel:abbrev (kbd "<f11> a"))

(defun pel--activate-abbrev-mode ()
  "Activate abbrev-mode."
  (define-key pel:abbrev "a" #'abbrev-mode)
  (pel-add-hook-for 'pel-modes-activating-abbrev-mode 'abbrev-mode)
  (when pel--cached-abbrev-file-name
    (setq abbrev-file-name pel--cached-abbrev-file-name)
    (quietly-read-abbrev-file nil))
  ;; If files were on the command line when Emacs started buffers may
  ;; have been opened before this function runs.  Make sure all opened
  ;; buffers that should have abbrev-mode active have it active.
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (memq major-mode pel-modes-activating-abbrev-mode)
        (abbrev-mode 1)))))

(defun pel-extract-abbrev-definitions (&optional arg)
  "Read abbreviations/expansion data from current abbrev definition buffer.
First prompt user to confirm.
With argument ARG , eliminate all abbrev definitions except
the ones defined from the buffer now."
  (interactive "P")
  (if (yes-or-no-p "Read abbreviations/expansion definition data from current buffer? ")
      (define-abbrevs arg)
    (message "Nothing done.")))

(define-key pel:abbrev "X"  #'pel-extract-abbrev-definitions)
(define-key pel:       "/"  #'expand-abbrev)
(define-key pel:abbrev "e"  #'expand-abbrev)
(define-key pel:abbrev "E"  #'expand-region-abbrevs)
(define-key pel:abbrev "g"  #'add-global-abbrev)
(define-key pel:abbrev "G"  #'define-global-abbrev)
(define-key pel:abbrev "L"  #'define-mode-abbrev)
(define-key pel:abbrev "i"  #'insert-abbrevs)
(define-key pel:abbrev "l"  #'add-mode-abbrev)
(define-key pel:abbrev (kbd "M-l")  #'list-abbrevs)
(define-key pel:abbrev (kbd "M-e")  #'edit-abbrevs)
(define-key pel:abbrev "r"  #'read-abbrev-file)
(define-key pel:abbrev "s"  #'write-abbrev-file)
(define-key pel:abbrev "u"  #'unexpand-abbrev)
(define-key pel:abbrev "$"   'pel-ispell-word-then-abbrev)
(define-key pel: (kbd "M-$") 'pel-ispell-word-then-abbrev)

(if pel--cached-abbrev-file-name
    ;; If PEL is informed to delay load the abbreviation file
    ;; do it silently 2 seconds of idle later.
    (run-at-time "2 sec" nil (function pel--activate-abbrev-mode))
  ;; Otherwise do it right away.
  (pel--activate-abbrev-mode))

;; ---------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> b`` : buffer commands
;; Used keys:
;;   - [ ]
;;   C-c
;;   M-a M-p
;;   ?
;;   I K R S U V X
;;   a b c f h i k l n p r s v x

(define-pel-global-prefix pel:buffer (kbd "<f11> b"))
(define-key pel:buffer "-"  #'ruler-mode)
(define-key pel:buffer "a"   'pel-show-all-buffers)
(define-key pel:buffer "c"  #'clone-buffer)
(define-key pel:buffer "k"  #'kill-current-buffer)
(define-key pel:buffer "l"   'pel-switch-to-last-used-buffer)
(define-key pel:buffer "n"  #'next-buffer)
(define-key pel:buffer "?"   'pel-show-window-previous-buffer)
(define-key pel:buffer "p"  #'previous-buffer)
(define-key pel:buffer "r"  #'read-only-mode)
(define-key pel:buffer "s"   'bs-show)
(define-key pel:buffer "S"   'bs-customize)
(define-key pel:buffer "v"  #'view-mode)
(define-key pel:buffer "V"  #'view-buffer)
(define-key pel:buffer "R"  #'rename-buffer)
(define-key pel:buffer "U"  #'rename-uniquely)
(define-key pel:buffer (kbd "M-a")  #'append-to-buffer)
(define-key pel:buffer (kbd "M-p")  #'prepend-to-buffer)
(define-key pel:buffer (kbd "C-c")  #'copy-to-buffer)
(define-key pel:buffer "i"  #'insert-buffer)
(define-key pel:buffer "f"  #'append-to-file)
(define-key pel:buffer (kbd "M-x") 'hexl-mode)
(define-key pel:buffer "."  'pel-bs-next)
(define-key pel:buffer ","  'pel-bs-previous)
(define-key pel:buffer "]"  'pel-smb-next)
(define-key pel:buffer "["  'pel-smb-previous)

;; Add key bindings inside the Buffer Selection Mode.
(add-hook 'bs-mode-hook 'pel-bs-init)

(when pel-use-popup-switcher
  (define-key pel:buffer "b" 'psw-switch-buffer))
;; Reserved            "h"  highlight prefix
;; Reserved            "I"  indirect buffer prefix
;; Reserved            "x"   (see declarations below with pel-use-nhexl-mode)
;; Reserved            "X"

(when pel-use-iflipb
  ;; All key bindings are inside the pel-∑buffer Hydra
  (pel-ensure-package iflipb from: melpa))

;; ibuffer-mode support
;; Provide <f12> <f1> and <f12><f3> in ibuffer-mode
;; TODO simplify this code
(define-pel-global-prefix pel:for-ibuffer (kbd "<f11> SPC SPC b"))
(defun pel--setup-for-ibuffer ()
  "Activate ibuffer setup, take local variables into account."
  (pel-local-set-f12-M-f12 'pel:for-ibuffer))
(declare-function pel--setup-for-ibuffer "pel_keys")
(pel--mode-hook-maybe-call
 (function pel--setup-for-ibuffer)
 'ibuffer-mode 'ibuffer-mode-hook)

;; diff-mode support
;; Provide <f12> <f1>, <f12> <f2> and <f12><f3> keys for diff-mode
(define-pel-global-prefix pel:for-diff-mode       (kbd "<f11> SPC SPC d d"))
(define-pel-global-prefix pel:for-diff-mode-setup (kbd "<f11> SPC SPC d d <f4>"))
(define-key pel:for-diff-mode-setup "?" 'pel-diff-show-status)

(defun pel--setup-for-diff-mode ()
  "Activate diff-mode setup, take local variables into account."
  (pel-local-set-f12-M-f12 'pel:for-diff-mode)
  (when (boundp 'diff-mode-map)
      (let ((map diff-mode-map))
        (define-key map (kbd "<f6> <down>") 'diff-file-next)
        (define-key map (kbd "<f6> <up>")   'diff-file-prev)
        (define-key map (kbd "<f6> o")      'pel-diff-hunk-files-occur))))
(declare-function pel--setup-for-diff-mode "pel_keys")
(pel--mode-hook-maybe-call
 (function pel--setup-for-diff-mode)
 'diff-mode 'diff-mode-hook)

;; ediff-mode support
;; Provide <f12> <f1>, <f12> <f2> and <f12><f3> keys for ediff-mode
;; NOTE: the required code is inside the pel-diff.el file because
;;       the hook needs to be passed to the ediff-files function,
;;       which is a good thing: it helps speed up Emacs startup.

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; - Function Keys - <f11> - Prefix ``<f11> b I`` : Indirect buffer commands

(define-pel-global-prefix pel:indirect-buffer (kbd "<f11> b I"))
(define-key pel:indirect-buffer "c"  #'clone-indirect-buffer)
(define-key pel:indirect-buffer "m"  #'make-indirect-buffer)
(define-key pel:indirect-buffer "w"  #'clone-indirect-buffer-other-window)

;; ---------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> B`` : Browse commands
;; pel:diff is defined later but used here.
(define-pel-global-prefix pel:diff (kbd "<f11> d"))

(when (or pel-use-neotree
          pel-use-ztree
          pel-use-treemacs
          pel-use-rfc-mode)
  (define-pel-global-prefix pel:browse (kbd "<f11> B"))

  (when pel-use-treemacs
    ;; Prevent byte-compiler warnings with forward declarations
    (defvar treemacs-mode-map)
    (declare-function lsp-treemacs-sync-mode "lsp-treemacs")

    (defun pel--setup-treemacs ()
      "Setup treemacs."
      (define-key treemacs-mode-map (kbd "M-?")
        'treemacs-advanced-helpful-hydra)
      (when pel-use-lsp-treemacs
        (lsp-treemacs-sync-mode 1)))
    (declare-function pel--setup-treemacs "pel_keys")

    (pel-ensure-package treemacs from: melpa)
    (pel-autoload-file treemacs for: treemacs)
    (when pel-use-lsp-treemacs
      (pel-ensure-package lsp-treemacs from: melpa))

    (define-key pel:browse  "T" 'treemacs)
    (add-hook 'treemacs-mode-hook (function pel--setup-treemacs))
    ;; TODO: either drop the following or provide support for emacs-winum
    ;; (with-eval-after-load 'winum
    ;;   (when (boundp 'winum-keymap)
    ;;     (define-key winum-keymap (kbd "<f9>") 'treemacs-select-window)))
    )

  (when pel-use-neotree
    (define-pel-global-prefix pel:neotree (kbd "<f11> B N"))
    (pel-ensure-package neotree from: melpa)
    (pel-autoload-file neotree for:
                       neotree-dir
                       neotree-find
                       neotree-toggle)
    (define-key pel:neotree  "D" 'neotree-dir)
    (define-key pel:neotree  "F" 'neotree-find)
    (define-key pel:neotree  "N" 'neotree-toggle)

    (defvar neo-theme)                  ; prevent byte-compiler warning
    (pel-eval-after-load neotree
      ;; TODO: should we not have PEL customization for neo-theme instead?
      (if pel-emacs-is-graphic-p
          (when pel-neotree-font-in-graphics
            (setq neo-theme 'icons))
        (when pel-neotree-font-in-terminal
          (setq neo-theme 'arrow)))
      (define-key pel:neotree  "S" 'neotree-show)
      (define-key pel:neotree  "H" 'neotree-hide)))

  (when pel-use-ztree
    ;; The ztree package has 3 files: ztree.el, ztree-dir.el and ztree-diff.el
    ;; The ztree.el has no other code than requiring the other 2 files.
    ;; Therefore the code ensures the ztree is loaded for the commands,
    ;; which essentially gets both of these files loaded.
    (pel-ensure-package ztree from: melpa)
    (pel-autoload-file ztree for:
                       ztree-dir
                       ztree-diff)
    (define-key pel:browse "Z" 'ztree-dir)
    (define-key pel:diff "z" 'ztree-diff)

    ;; ztree uses variables instead of defcustom forms for its configuration
    ;; variables. PEL provides customization user-options instead, until ztree
    ;; code uses customization.
    (defvar ztree-dir-move-focus)       ; prevent byte-compiler warning
    (defvar ztree-dir-filter-list)
    (defvar ztree-dir-show-filtered-files)
    (pel-eval-after-load ztree-dir
      (setq ztree-dir-move-focus pel-ztree-dir-move-focus)
      (when pel-ztree-dir-filter-list
        (setq-default ztree-dir-filter-list
                      (append pel-ztree-dir-filter-list
                              ztree-dir-filter-list)))
      (setq-default ztree-dir-show-filtered-files
                    pel-ztree-dir-show-filtered-files)))

  (when pel-use-rfc-mode
    (pel-ensure-package rfc-mode from: melpa)
    (define-key pel:browse  "r" 'rfc-mode-read)
    (define-key pel:browse  "R" 'rfc-mode-browse)))

;; ---------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> c`` : count things

(define-pel-global-prefix pel:count (kbd "<f11> c"))
(define-key pel:count "m" #'count-matches)
(define-key pel:count "p" #'count-lines-page)
(define-key pel:count "W" #'count-words-region)
(define-key pel:count "w" #'count-words)

;; ---------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> C`` : clipboard commands
(when pel-emacs-is-graphic-p
  (define-pel-global-prefix pel:clipboard (kbd "<f11> C"))
  (define-key pel:clipboard "c" #'clipboard-kill-ring-save)
  (define-key pel:clipboard "x" #'clipboard-kill-region)
  (define-key pel:clipboard "v" #'clipboard-yank))

;; ---------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> d`` : diff commands
(define-key pel:diff "f"  'diff)
(define-key pel:diff "b"  'diff-buffer-with-file)
(define-key pel:diff "k"  'diff-backup)
(define-key pel:diff "w"  'compare-windows)
(define-key pel:diff "2"  'pel-ediff-2files)
(define-key pel:diff "r"  'pel-ediff-revision)

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; - Function Keys - <f11> - Prefix ``<f11> d e`` : ediff commands
(define-pel-global-prefix pel:ediff (kbd "<f11> d e"))
(define-key pel:ediff "?"  'ediff-documentation)

(define-pel-global-prefix pel:ediff-buffer (kbd "<f11> d e b"))
(define-key pel:ediff-buffer "f"  'ediff-current-file)
(define-key pel:ediff-buffer "b"  'ediff-buffers)
(define-key pel:ediff-buffer "3"  'ediff-buffers3)

(define-pel-global-prefix pel:ediff-files (kbd "<f11> d e f"))
(define-key pel:ediff-files "k"  'ediff-backup)
(define-key pel:ediff-files "f"  'ediff-files)
(define-key pel:ediff-files "3"  'ediff-files3)
(define-key pel:ediff-files "r"  'ediff-revision)

(define-pel-global-prefix pel:ediff-dirs (kbd "<f11> d e d"))
(define-key pel:ediff-dirs "d"  'ediff-directories)
(define-key pel:ediff-dirs "3"  'ediff-directories3)
(define-key pel:ediff-dirs "r"  'edir-revisions)

(define-pel-global-prefix pel:ediff-windows (kbd "<f11> d e w"))
(define-key pel:ediff-windows "w"  'ediff-windows-wordwise)
(define-key pel:ediff-windows "l"  'ediff-windows-linewise)

(define-pel-global-prefix pel:ediff-regions (kbd "<f11> d e r"))
(define-key pel:ediff-regions "w"  'ediff-regions-wordwise)
(define-key pel:ediff-regions "l"  'ediff-regions-linewise)

(define-pel-global-prefix pel:ediff-patch (kbd "<f11> d e p"))
(define-key pel:ediff-patch "f"  'epatch)
(define-key pel:ediff-patch "b"  'epatch-buffer)

(define-pel-global-prefix pel:ediff-merge (kbd "<f11> d e m"))
(define-key pel:ediff-merge "f"  'ediff-merge)
(define-key pel:ediff-merge "F"  'ediff-merge-with-ancestor)
(define-key pel:ediff-merge "b"  'ediff-merge-buffers)
(define-key pel:ediff-merge "B"  'ediff-merge-buffers-with-ancestor)
(define-key pel:ediff-merge "d"  'edir-merge-revisions)
(define-key pel:ediff-merge "D"  'edir-merge-revisions-with-ancestor)
(define-key pel:ediff-merge "c"  'edirs-merge)
(define-key pel:ediff-merge "C"  'edirs-merge-with-ancestor)
(define-key pel:ediff-merge "r"  'ediff-merge-revisions)
(define-key pel:ediff-merge "R"  'ediff-merge-revisions-with-ancestor)

(define-pel-global-prefix pel:ediff-registry (kbd "<f11> d e R"))

(define-key pel:ediff-registry "R"  'eregistry)
(define-key pel:ediff-registry "s"  'ediff-show-registry)
(define-key pel:ediff-registry "f"  'ediff-toggle-multiframe)
(when pel-emacs-is-graphic-p
  (define-key pel:ediff-registry "t"  'ediff-toggle-use-tollbar))
(define-key pel:ediff-registry "v"  'ediff-revert-buffers-then-recompute-diffs)

(when pel-use-smerge
  (define-key pel:diff "s" 'smerge-start-session)

  (defun pel--smerge-setup ()
    "Activate PEL setup in smerge-mode when entering smerge-mode."
    (when (boundp 'smerge-mode-map)
      ;; Activate the special PEL key bindings under the <f6> key
      (let ((map smerge-mode-map))
        ;; TO-DO : define a prefix name that is available in the
        ;;        `smerge-mode-map' only to help describe the prefix in
        ;;        which-key display of prefix.
        ;; (define-prefix-command 'pel:for-smerge map)
        ;; (local-set-key (kbd "<f6> s") 'pel:for-smerge)

        (define-key map (kbd "<f6> s s")    'smerge-start-session)
        (define-key map (kbd "<f6> s n")    'smerge-next)
        (define-key map (kbd "<f6> s p")    'smerge-prev)

        (define-key map (kbd "<f6> s M-c")  'smerge-auto-combine)
        (define-key map (kbd "<f6> s C")    'smerge-combine-with-next)
        (define-key map (kbd "<f6> s >")    'smerge-diff-base-lower)
        (define-key map (kbd "<f6> s <")    'smerge-diff-base-upper)
        (define-key map (kbd "<f6> s =")    'smerge-diff-upper-lower)
        (define-key map (kbd "<f6> s e")    'smerge-ediff)

        (define-key map (kbd "<f6> s a")    'smerge-keep-all)
        (define-key map (kbd "<f6> s b")    'smerge-keep-base)
        (define-key map (kbd "<f6> s \C-m") 'smerge-keep-current)
        (define-key map (kbd "<f6> s l")    'smerge-keep-lower)
        (define-key map (kbd "<f6> s u")    'smerge-keep-upper)

        (define-key map (kbd "<f6> s M-k")  'smerge-kill-current)
        (define-key map (kbd "<f6> s M-C")  'smerge-makeup-conflict)
        (define-key map (kbd "<f6> s m")    'smerge-popup-context-menu)
        (define-key map (kbd "<f6> s R")    'smerge-refine)
        (define-key map (kbd "<f6> s r")    'smerge-resolve)
        (define-key map (kbd "<f6> s M-r")  'smerge-resolve-all)
        (define-key map (kbd "<f6> s M-s")  'smerge-swap))))

  ;; Activate PEL smerge setup when smerge-mode starts on a buffer.
  (add-hook 'smerge-mode-hook 'pel--smerge-setup)

  (when (eq pel-use-smerge 'auto)
    ;; Following function originally taken from smerge-mode.el
    ;; and then modified for PEL key bindings.
    (defun sm-try-smerge ()
      "Activate smerge-mode automatically when file is diff3-annotated."
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward "^<<<<<<< " nil t)
          (smerge-mode 1))))
    (add-hook 'find-file-hook 'sm-try-smerge t)))

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; - Function Keys - <f11> - Prefix ``<f11> d p`` : patch commands
(define-pel-global-prefix pel:patch (kbd "<f11> d p"))
(define-key pel:patch "f"  'epatch)
(define-key pel:patch "b"  'epatch-buffer)

;; ---------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> D`` : draw commands

(define-pel-global-prefix pel:draw (kbd "<f11> D"))
(define-key pel:draw "a"  'artist-mode)       ; toggle artist-mode
(define-key pel:draw "p"  'picture-mode)      ; activate picture-mode

;; ---------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> f`` : File operations

(defun pel-auto-revert-set-timer ()
  "Execute `auto-revert-set-timer' if the auto-revert  mode is active."
  (interactive)
  (declare-function auto-revert-set-timer "autorevert") ; prevent warning
  (defvar auto-revert-interval)                         ; prevent warning
  (if (fboundp 'auto-revert-set-timer)
      (progn
        (auto-revert-set-timer)
        (message
         "auto-revert-mode %d second timer set/cancelled"
         auto-revert-interval))
    (user-error
     "Activate auto-revert-mode before attempting to set/cancel its timer")))

(declare-function find-grep "grep")
(define-pel-global-prefix pel:file (kbd "<f11> f"))
(define-pel-global-prefix pel2:file (kbd "<M-f11> M-f"))
;; Used keys in <f11> f:
;; . / ?
;; F I L O W
;; a d f g h i j l n o p r t u v w
;; M-. M-/ M-d M-l M-t M-u M-x
(define-key pel:file  (kbd "M-d") 'pel-open-file-in-other-dir)
(define-key pel2:file (kbd "M-d") 'pel-open-file-in-other-dir)
(define-key pel:file "f" #'find-file)
(define-key pel2:file (kbd "M-f") #'find-file)
(define-key pel:file "I" #'insert-file-literally)
(define-key pel:file "O" #'find-file-read-only-other-window)
(define-key pel:file "L" (if pel-use-counsel 'counsel-locate #'locate))
(define-key pel:file "W" #'append-to-file)
(define-key pel:file "d" #'find-dired)
(define-key pel:file "F"  'pel-open-buffer-file-in-os-app)
(define-key pel:file "g" #'find-grep)
(define-key pel:file "h" #'find-grep-dired)
(define-key pel:file "i" #'insert-file)
(define-key pel:file "l" #'find-lisp-find-dired)
(define-key pel:file "n" #'find-name-dired)
(define-key pel:file "o" #'find-file-other-window)
(define-key pel:file "t" #'time-stamp)
(define-key pel:file (kbd "M-t") #'time-stamp-toggle-active)
(define-key pel:file "w" #'write-region)
(define-key pel:file (kbd "M-x") 'hexl-find-file)
(define-key pel:file (kbd "M-l") 'find-file-literally)
(define-key pel:file "?" #'pel-show-filemng-status)
(when pel-use-popup-switcher
  (define-key pel:file (kbd "M-f") 'pel-psw-navigate-files))
(unless pel-system-is-windows-p
  (pel-autoload-file pel-sudo-edit for: pel-edit-as-root)
  (define-key pel:file "R" 'pel-edit-as-root))


;; - Open recent file
;; ------------------
(when pel-use-recentf
  (define-pel-global-prefix pel:recent-file (kbd "<f11> f M-r"))
  ;; recentf is built-in Emacs, but its not loaded by default.
  ;; It must be available soon to be able to work properly.
  ;; So don't defer it.
  ;; Its loading will impact init time by a small amount to load the
  ;; tree-widget but it's the price to pay to use the feature.
  (pel-autoload-file recentf for: recentf-mode)
  (pel-autoload-file pel-file-recent for:
                     pel-find-recent-file
                     pel-show-recentf-function
                     pel-select-recentf-function)
  (pel-eval-after-load recentf
    (recentf-mode 1)
    (define-key pel:recent-file (kbd "M-e") 'recentf-edit-list)
    (define-key pel:recent-file (kbd "M-r") 'pel-find-recent-file)
    (define-key pel:recent-file (kbd "M-?") 'pel-show-recentf-function)
    (define-key pel:recent-file (kbd "M-R") 'pel-select-recentf-function))
  (pel-require-at-load recentf))

;; - Open file at point
;; --------------------
(global-set-key (kbd "<M-f6>") 'pel-open-at-point)
(define-key pel:file "."    'pel-open-at-point)
(define-key pel:file ";"    'pel-set-open-at-point-dir)
(define-key pel:file (kbd "M-.") 'pel-set-ido-use-fname-at-point)
(define-key pel:file "/"    'pel-browse-filename-at-point)
(define-key pel:file (kbd "M-/") 'browse-url-at-point)
(define-key pel:file (kbd "M-u") 'pel-open-url-at-point)
(global-set-key "\C-cj"    'webjump)
(define-key pel:file "j"   'webjump)
;; By default, `pel-open-at-point' searches file in the current project
;; if it does not find the file from its name.
(setq pel-filename-at-point-finders '(pel-generic-find-file))

;; - Revert
;; -------
(define-pel-global-prefix pel:file-revert (kbd "<f11> f r"))
(define-key pel:file-revert "a" #'auto-revert-mode)
(define-key pel:file-revert " "  'pel-auto-revert-set-timer) ; cancel/restart the timer
(define-key pel:file-revert "f" #'revert-buffer)
(define-key pel:file-revert "t" #'auto-revert-tail-mode)

;; Navigating URL: goto-address-mode
;; ---------------------------------
;; PEL provides the ability to open URL with several commands listed above.
;; Emacs also provides the goto-address-mode, which is also included in the
;; binding PEL activates.
(define-key pel:file "u" 'goto-address-mode)
(define-key pel:file "U" 'goto-address-prog-mode)


(defun pel--augment-goto-addr-map ()
  "Add more key sequences to goto-addr key map."
  (if (boundp 'goto-address-highlight-keymap)
      (progn
        ;; Add 3 more key sequences active when point is on a URL button
        (define-key goto-address-highlight-keymap (kbd "C-c C-f") 'pel-open-url-at-point)
        (define-key goto-address-highlight-keymap (kbd "C-c C-n") 'pel-goto-next-url)
        (define-key goto-address-highlight-keymap (kbd "C-c C-p") 'pel-goto-previous-url))
    (error "goto-address-highlight-keymap not defined!")))

;; activate the extra keys for goto-addr-mode
(pel--mode-hook-maybe-call  (function pel--augment-goto-addr-map)
                            'goto-address-mode 'goto-address-mode-hook)

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; - Function Keys - <f11> - Prefix ``<f11> f a`` : Find File At Point (ffap)

(when pel-use-ffap
  ;; ffap is built-in Emacs.  When requested by PEL customization provide key
  ;; bindings to the autoloaded commands.  ffap's default key bindings overlap
  ;; with several other Emacs key bindings, so PEL provides a user-option to
  ;; determine whether those will also be used (when pel-use-ffap is set to
  ;; 'ffap-bindings).

  (pel-autoload-file ffap for:
                     ffap
                     ffap-read-only
                     ffap-alternate-file
                     ffap-other-window
                     ffap-other-frame
                     ffap-read-only-other-window
                     ffap-read-only-other-frame
                     dired-at-point
                     ffap-dired-other-window
                     ffap-dired-other-frame
                     ffap-list-directory
                     ffap-menu
                     ffap-bindings)

  (define-pel-global-prefix pel:ffap (kbd "<f11> f a"))
  (define-key pel:ffap  "p"     #'ffap) ; find-file-at-point
  (define-key pel:ffap  "P"     #'ffap-read-only)
  (define-key pel:ffap  "v"     #'ffap-alternate-file)
  (define-key pel:ffap  "w"     #'ffap-other-window)
  (define-key pel:ffap  "f"     #'ffap-other-frame)
  (define-key pel:ffap  "W"     #'ffap-read-only-other-window)
  (define-key pel:ffap  "F"     #'ffap-read-only-other-frame)
  (define-key pel:ffap  "d"     #'dired-at-point)
  (define-key pel:ffap  "D"     #'ffap-dired-other-window)
  (define-key pel:ffap  (kbd "M-d") #'ffap-dired-other-frame)
  (define-key pel:ffap  "l"     #'ffap-list-directory)
  (define-key pel:ffap  "m"     #'ffap-menu)

  (defun pel--activate-ffap-bindings ()
    "Activate ffap standard key bindings."
    (ffap-bindings))
  (declare-function pel--activate-ffap-bindings "pel_keys")

  (when (eq pel-use-ffap 'ffap-bindings)
    (run-with-idle-timer 1 nil (function pel--activate-ffap-bindings))))

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; - Project Management - find-file-in-project

;; CAUTION: This package needs major tuning!  It takes forever searching for a
;;          project.  For the moment, Projectile is MUCH better!

(define-pel-global-prefix pel:ffip (kbd "<f11> f p"))

(when pel-use-find-file-in-project
  (pel-install-github-file "pierre-rouleau/find-file-in-project/master"
                           "find-file-in-project.el")
  (autoload 'find-file-in-project "find-file-in-project")
  (define-key pel:ffip "p" 'find-file-in-project))

;; Use my fork of this project until my contribution to support Emacs Customization
;; is merged in. See https://github.com/redguardtoo/find-file-in-project/pull/126
;; (use-package find-file-in-project
;;   :ensure t
;;   :pin melpa
;;   :commands find-file-in-project)

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; - Function Keys - <f11> - Prefix ``<f11> f v`` : File variables

(define-pel-global-prefix pel:filevar (kbd "<f11> f v"))
(define-key pel:filevar "="  #'add-file-local-variable-prop-line)
(define-key pel:filevar "-"  #'delete-file-local-variable-prop-line)
(define-key pel:filevar "c"  #'copy-dir-locals-to-file-locals-prop-line)

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; - Function Keys - <f11> - Prefix ``<f11> f v D`` : Directory File variables

(define-pel-global-prefix pel:dirvar (kbd "<f11> f v D"))
(define-key pel:dirvar "="  #'add-dir-local-variable)
(define-key pel:dirvar "-"  #'delete-dir-local-variable)
(define-key pel:dirvar "C"  #'copy-file-locals-to-dir-locals)

(when pel-use-log-support
  (when pel-use-logview
    (pel-ensure-package logview from: melpa))
  (when pel-use-log4j-mode
    (pel-ensure-package log4j-mode from: melpa))
  (when pel-use-rails-log-mode
    (pel-ensure-package rails-log-mode from: melpa))
  (when pel-use-syslog-mode
    (pel-ensure-package syslog-mode from: melpa))
  (when pel-use-vlf
    (pel-ensure-package vlf from: melpa)))

;; ---------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> F`` : Frame operations

(define-pel-global-prefix pel:frame (kbd "<f11> F"))
(define-key pel:frame "?"   'pel-show-frame-count)
(define-key pel:frame "0"  #'delete-frame)
(define-key pel:frame "1"  #'delete-other-frames)
(define-key pel:frame "2"  #'make-frame-command)
(define-key pel:frame "b"  #'display-buffer-other-frame)
(define-key pel:frame "d"  #'dired-other-frame)
(define-key pel:frame "f"  #'find-file-other-frame)
(define-key pel:frame "n"   'pel-next-frame)
(define-key pel:frame "o"  #'other-frame)
(define-key pel:frame "O"  #'switch-to-buffer-other-frame)
(define-key pel:frame "p"   'pel-previous-frame)
(define-key pel:frame "r"  #'find-file-read-only-other-frame)
(when pel-emacs-is-graphic-p
  (require 'menu-bar nil :noerror) ; feature loaded in emacs -Q
  (define-key pel:frame "F" 'menu-set-font))

;; ---------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> g`` : Grep operations

(define-pel-global-prefix pel:grep (kbd "<f11> g"))
(declare-function kill-grep "grep")
(define-key pel:grep      "f"         #'find-grep)
(define-key pel:grep      "g"         #'grep)
(define-key pel:grep      "k"         #'kill-grep)
(define-key pel:grep      "l"         #'lgrep)
(define-key pel:grep      "r"         #'rgrep)  ; execute recursive grep
(define-key pel:grep      "z"         #'zrgrep)
(define-key pel:grep      "1"          'first-error)

(when pel-use-wgrep
  ;; autoload wgrep-change-to-wgrep-mode in case its requested by ag-mode
  (autoload 'wgrep-change-to-wgrep-mode "wgrep" nil :interactive)
  (pel-ensure-package wgrep from: melpa)
  (pel-eval-after-load grep
    (when (boundp 'grep-mode-map)
      (define-key grep-mode-map  (kbd "C-x C-q") 'wgrep-change-to-wgrep-mode)))
  (pel-eval-after-load wgrep
    (when (boundp 'wgrep-mode-map)
      (define-key wgrep-mode-map (kbd "C-c C-c") 'wgrep-finish-edit)
      (define-key wgrep-mode-map (kbd "C-c C-s") 'wgrep-save-all-buffers))))

;; ripgrep - a faster grep easier to use than grep.
;; 2 packages support ripgrep: rg.el and ripgrep.el
;; Install rg.el and install ripgrep.el if projectile is used.
(when  pel-use-ripgrep
  ;; rg.el
  (pel-ensure-package rg from: melpa)
  (pel-autoload-file rg for:
                     rg
                     rg-literal
                     rg-menu
                     rg-enable-default-bindings)
  (pel-eval-after-load rg
    (rg-enable-default-bindings))
  (define-key pel:grep  "t"     'rg-literal)
  (define-key pel:grep  "i"     'rg)
  (define-key pel:grep  "m"     'rg-menu)
  (global-set-key (kbd "C-c s") 'rg-menu)

  ;; ripgrep.el
  (when pel-use-projectile
    (pel-ensure-package ripgrep from: melpa)
    (pel-autoload-file ripgrep for: ripgrep-regexp))
  ;; wgrep support
  (when pel-use-wgrep
    (pel-eval-after-load rg
      (when (boundp 'rg-mode-map)
        '(define-key rg-mode-map  (kbd "C-x C-q") 'wgrep-change-to-wgrep-mode)))))

;; ag
;; --

(when pel-use-ag
  (pel-ensure-package ag from: melpa)
  (pel-autoload-file ag for:
                     ag
                     ag-dired
                     ag/kill-process
                     ag-files
                     ag-regexp
                     ag-project
                     ag-project-files
                     ag-project-regexp
                     ag-dired-regexp
                     ag-project-dired-regexp
                     ag-kill-buffers
                     ag-kill-other-buffers)
  (define-pel-global-prefix pel:ag (kbd "<f11> g a"))
  (define-key pel:ag  "a"        'ag)
  (define-key pel:ag  "x"        'ag-regexp)
  (define-key pel:ag  "f"        'ag-files)

  (define-pel-global-prefix pel:ag-project   (kbd "<f11> g a p"))
  (define-key pel:ag-project "f" 'ag-project-files)
  (define-key pel:ag-project "p" 'ag-project)
  (define-key pel:ag-project "x" 'ag-project-regexp)

  (define-pel-global-prefix pel:ag-dired (kbd "<f11> g a d"))
  (define-key pel:ag-dired  "d"  'ag-dired)
  (define-key pel:ag-dired  "x"  'ag-dired-regexp)
  (define-key pel:ag-dired  "f"  'ag-project-dired-regexp)

  (define-pel-global-prefix pel:ag-kill (kbd "<f11> g a k"))
  (define-key pel:ag-kill  "a"   'ag-kill-buffers)
  (define-key pel:ag-kill  "o"   'ag-kill-other-buffers)
  (define-key pel:ag-kill  "p"   'ag/kill-process)
  ;; wgrep support
  (when pel-use-wgrep
    (pel-eval-after-load ag
      (when (boundp 'ag-mode-map)
        '(define-key ag-mode-map  (kbd "C-x C-q") 'wgrep-change-to-wgrep-mode)))))

(when pel-use-deadgrep
  (pel-ensure-package deadgrep from: melpa)
  (define-key pel:grep  "d"     'deadgrep))


;; ---------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> h`` : highlight commands

(define-pel-global-prefix pel:highlight (kbd "<f11> h"))

;; Keys used in pel:highlight:
;; (  - . \ |
;; C F G H L R
;; a c f h i l p r s u w
;; M-c M-i

(when pel-use-auto-highlight-symbol
  (pel-ensure-package auto-highlight-symbol from: melpa)
  (pel-autoload-file auto-highlight-symbol for:
                     auto-highlight-symbol-mode
                     global-auto-highlight-symbol-mode)
  (define-key pel:highlight "a" 'auto-highlight-symbol-mode))

(when pel-use-rainbow-mode
  (pel-ensure-package rainbow-mode from: melpa)
  (pel-autoload-file rainbow-mode for: rainbow-mode)
  (define-key pel:highlight (kbd "M-r") 'rainbow-mode))

(defun pel-hi-lock-find-patterns ()
  "Execute hi-lock-find-patterns when `hi-lock-mode' is active."
  (interactive)
  (declare-function hi-lock-find-patterns "hi-lock")
  (if (fboundp 'hi-lock-find-patterns)
      (hi-lock-find-patterns)
    (user-error "Turn hi-lock-mode on first")))

(define-key pel:highlight      "0" #'hl-line-mode)
(define-key pel:highlight      "(" #'show-paren-mode)
(define-key pel:highlight      "."  #'highlight-symbol-at-point)         ; M-s h .
(define-key pel:highlight (kbd "M-c") #'highlight-changes-mode)
(define-key pel:highlight      "c"   'pel-set-highlight-color)
(define-key pel:highlight (kbd "<f4>") 'pel-customize-highlight)
(define-key pel:highlight      "F"  #'font-lock-mode)
(define-key pel:highlight      "f"   'pel-hi-lock-find-patterns)
(define-key pel:highlight (kbd "M-L") #'global-hi-lock-mode)
(define-key pel:highlight      "L"  #'hi-lock-mode)
(define-key pel:highlight      "l"  #'highlight-lines-matching-regexp)   ; M-s h l
(define-key pel:highlight      "p"  #'highlight-phrase)                  ; M-s h p
(when pel-use-rainbow-delimiters
  (define-key pel:highlight (kbd "M-(")  'rainbow-delimiters-mode))
(define-key pel:highlight      "r"  #'highlight-regexp)                  ; M-s h r
(define-key pel:highlight      "s"   'pel-toggle-hl-line-sticky)
(define-key pel:highlight      "u"  #'unhighlight-regexp)                ; M-s h u
(define-key pel:highlight      "w"  #'hi-lock-write-interactive-patterns)
(define-key pel:highlight      "h" 'pel-highlight-line)
(define-key pel:highlight (kbd "M-H") 'pel-remove-line-highlight)

;;
(when pel-use-highlight-indentation
  (pel-ensure-package highlight-indentation from: melpa)
  (pel-autoload-file highlight-indentation for:
                     highlight-indentation-mode
                     highlight-indentation-current-column-mode)
  (define-key pel:highlight (kbd "M-i") 'highlight-indentation-mode)
  (define-key pel:highlight (kbd "M-c") 'highlight-indentation-current-column-mode))
;;
(when pel-use-iedit
  (define-key pel:highlight "i" 'iedit-mode))

(when pel-use-vline
  ;; download and byte-compile vline if not already present
  ;; Do it after compiling pel_keys.el, when pel-init load pel_keys.
  (pel-install-github-file "emacsmirror/vline/master" "vline.el")
  (pel-autoload-file vline for: vline-mode)
  (define-key pel:highlight "|"  'vline-mode)
  (define-key pel:mode      "|"  'vline-mode)
  (define-key pel:          "9"  'vline-mode))

(when (and (version< emacs-version "27.1")
           pel-use-fill-column-indicator)
  (pel-ensure-package fill-column-indicator from: melpa)
  (pel-autoload-file fill-column-indicator for: fci-mode)
  (define-key pel:highlight "\\" 'fci-mode)
  (define-key pel:mode      "\\"  'fci-mode)
  (define-key pel:          "8"  'fci-mode))
;; For Emacs 27.1 & later use the built-in display-fill-column-indicator-mode.
(unless (version< emacs-version "27.1")
  (define-key pel:highlight "\\" 'display-fill-column-indicator-mode)
  (define-key pel:          "8"  'display-fill-column-indicator-mode)
  (define-key pel:mode      "\\" 'display-fill-column-indicator-mode))

;; -----------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> i`` : Insert text operations

;; Used keys in <f11> i:
;; -
;;   D       L
;; c d f     l t
;; M-c M-f
;;     C-M-f
(define-pel-global-prefix pel:insert (kbd "<f11> i"))
(define-key pel:insert   "c" 'copyright)
(define-key pel:insert (kbd "M-c") 'copyright-update)
(define-key pel:insert   "d" 'pel-insert-current-date)
(define-key pel:insert   "D" 'pel-insert-current-date-time)
(define-key pel:insert   "f" 'pel-insert-filename)
(define-key pel:insert   "F" 'pel-insert-filename-and-line)
(define-key pel:insert (kbd "C-f")  'pel-insert-dirname)
(define-key pel:insert (kbd "M-f")  'pel-insert-filename-wtilde)
(define-key pel:insert (kbd "C-M-f")  'pel-insert-dirname-wtilde)
(define-key pel:insert   "l" 'pel-insert-line)
(define-key pel:insert   "t" 'pel-insert-iso8601-timestamp)
(define-key pel:insert   "T" 'pel-insert-todo-note)

(when (or pel-use-lice
          (eq pel-c-skel-with-license t)
          (eq pel-c++-skel-with-license t)
          (eq pel-clisp-skel-with-license t)
          (eq pel-elisp-skel-with-license t)
          (eq pel-erlang-skel-with-license t))
  (pel-ensure-package lice from: melpa)
  (pel-autoload-file lice for: lice)
  (define-key pel:insert "L" 'lice)
  (define-key pel:f6 "L" 'lice))

(when pel-use-smart-dash
  (define-key pel:insert "-" 'smart-dash-mode))
(when pel-use-smartparens
  (pel-ensure-package smartparens from: melpa)
  (define-pel-global-prefix pel:smartparens (kbd "<f11> ("))

  (define-key pel:smartparens "("         'smartparens-mode)
  (define-key pel:smartparens ")"         'smartparens-strict-mode)
  (define-key pel:smartparens (kbd "M-(") 'smartparens-global-mode)
  (define-key pel:smartparens (kbd "M-)") 'smartparens-global-strict-mode)
  (define-key pel:smartparens "?"         'sp-cheat-sheet)

  ;; Bindings recommended by smartparens author for navigation.  These mostly
  ;; match the behaviour of standard Emacs navigation command bindings.
  ;; However the author uses C-S- combination which is not available in
  ;; terminal mode.  PEL changes some of these bindings to ensure that all
  ;; bindings are available in terminal mode.
  ;;
  ;; sp-forward-sexp (&optional arg)                 ;; C-M-f
  ;; sp-backward-sexp (&optional arg)                ;; C-M-b
  ;; sp-down-sexp (&optional arg)                    ;; C-M-d
  ;; sp-backward-down-sexp (&optional arg)           ;; C-M-a --> C-M-z
  ;; sp-up-sexp (&optional arg)                      ;; C-M-e --> C-M-]
  ;; sp-backward-up-sexp (&optional arg)             ;; C-M-u
  ;; sp-next-sexp (&optional arg)                    ;; C-M-n
  ;; sp-previous-sexp (&optional arg)                ;; C-M-p
  ;; sp-beginning-of-sexp (&optional arg)            ;; C-S-d --> C-M-a
  ;; sp-end-of-sexp (&optional arg)                  ;; C-S-a --> C-M-e
  (with-eval-after-load 'smartparens
    ;; Augment functionality of smartparens with PEL support to display
    ;; string just copied or killed.
    (pel-smartparens-augment)

    ;; bind keys that are not autoloaded
    (define-key pel:smartparens (kbd "M-?") 'sp-describe-system)
    (define-key pel:smartparens "i"         'pel-smartparens-info)

    (defvar smartparens-mode-map)       ; quiet byte-compiler
    (declare-function sp-local-pair "smartparens")
    (unless (eq major-mode 'erlang-mode)
      (define-key smartparens-mode-map (kbd "C-M-a")      'sp-beginning-of-sexp)
      (define-key smartparens-mode-map (kbd "C-M-e")      'sp-end-of-sexp)
      (define-key smartparens-mode-map (kbd "C-M-n")      'sp-next-sexp)
      (define-key smartparens-mode-map (kbd "C-M-p")      'sp-previous-sexp)
      (define-key smartparens-mode-map (kbd "<M-f7> n")   'sp-next-sexp)
      (define-key smartparens-mode-map (kbd "<M-f7> p")   'sp-previous-sexp)
      (define-key smartparens-mode-map (kbd "<M-f7> T")   'sp-transpose-hybrid-sexp)
      (define-key smartparens-mode-map (kbd "<M-f7> }")   'sp-add-to-previous-sexp)
      (define-key smartparens-mode-map (kbd "<M-f7> M->") 'sp-slurp-hybrid-sexp))

    (when (eq major-mode 'erlang-mode)
      ;; Activate Erlang-specific support
      (pel-smartparens-setup-erlang)

      ;; and Erlang specific key bindings
      (define-key smartparens-mode-map [remap delete-forward-char] 'pel-sp-delete-char)
      (define-key smartparens-mode-map [remap backward-delete-char-untabify] 'pel-sp-backward-delete-char)
      (define-key smartparens-mode-map (kbd "<M-f7> n")   'pel-sp-next-sexp)
      (define-key smartparens-mode-map (kbd "<M-f7> p")   'pel-sp-previous-sexp)
      (define-key smartparens-mode-map (kbd "<M-f7> }")   'pel-sp-add-to-previous-sexp))

    (define-key smartparens-mode-map (kbd "<M-f7> M-n")   'sp-narrow-to-sexp)
    (define-key smartparens-mode-map (kbd "<M-f7> f")     'sp-forward-sexp)
    (define-key smartparens-mode-map (kbd "C-M-f")        'sp-forward-sexp)
    (define-key smartparens-mode-map (kbd "<M-f7> b")     'sp-backward-sexp)
    (define-key smartparens-mode-map (kbd "C-M-b")        'sp-backward-sexp)

    (define-key smartparens-mode-map (kbd "<M-f7> F")     'sp-forward-parallel-sexp)
    (define-key smartparens-mode-map (kbd "<M-f7> B")     'sp-backward-parallel-sexp)
    (define-key smartparens-mode-map (kbd "<M-f7> d")     'sp-down-sexp)
    (define-key smartparens-mode-map (kbd "C-M-d")        'sp-down-sexp)
    (define-key smartparens-mode-map (kbd "<M-f7> z")     'sp-backward-down-sexp)
    (define-key smartparens-mode-map (kbd "C-M-z")        'sp-backward-down-sexp)
    (define-key smartparens-mode-map (kbd "<M-f7> a")     'sp-beginning-of-sexp)
    (define-key smartparens-mode-map (kbd "<M-f7> e")     'sp-end-of-sexp)
    (define-key smartparens-mode-map (kbd "<M-f7> j")     'sp-beginning-of-next-sexp)
    (define-key smartparens-mode-map (kbd "<M-f7> k")     'sp-beginning-of-previous-sexp)
    (define-key smartparens-mode-map (kbd "<M-f7> N")     'sp-end-of-next-sexp)
    (define-key smartparens-mode-map (kbd "<M-f7> K")     'sp-end-of-previous-sexp)
    (define-key smartparens-mode-map (kbd "C-M-]")        'sp-up-sexp)
    (define-key smartparens-mode-map (kbd "<M-f7> ]")     'sp-up-sexp)
    (define-key smartparens-mode-map (kbd "<M-f7> u")     'sp-backward-up-sexp)
    (define-key smartparens-mode-map (kbd "C-M-u")        'sp-backward-up-sexp)
    (define-key smartparens-mode-map (kbd "<M-f7> SPC n") 'sp-skip-forward-to-symbol)
    (define-key smartparens-mode-map (kbd "<M-f7> SPC m") 'pel-sp-forward-symbol)
    (define-key smartparens-mode-map (kbd "<M-f7> SPC p") 'pel-sp-backward-symbol)
    (define-key smartparens-mode-map (kbd "<M-f7> SPC .") 'sp-forward-whitespace)
    (define-key smartparens-mode-map (kbd "<M-f7> SPC ,") 'sp-backward-whitespace)
    (define-key smartparens-mode-map (kbd "<M-f7> =")     'sp-copy-sexp)
    (define-key smartparens-mode-map (kbd "<M-f7> M-=")   'sp-backward-copy-sexp)
    (define-key smartparens-mode-map (kbd "<M-f7> c")     'sp-clone-sexp)
    (define-key smartparens-mode-map (kbd "<M-f7> t")     'sp-transpose-sexp)
    (define-key smartparens-mode-map (kbd "<M-f7> s")     'sp-push-hybrid-sexp)
    (define-key smartparens-mode-map (kbd "<M-f7> >")     'sp-forward-slurp-sexp)
    (define-key smartparens-mode-map (kbd "<M-f7> <")     'sp-backward-slurp-sexp)
    (define-key smartparens-mode-map (kbd "<M-f7> {")     'sp-add-to-next-sexp)
    (define-key smartparens-mode-map (kbd "<M-f7> /")     'sp-forward-barf-sexp)
    (define-key smartparens-mode-map (kbd "<M-f7> M-/")   'sp-backward-barf-sexp)
    (define-key smartparens-mode-map (kbd "<M-f7> r")     'sp-rewrap-sexp)
    (define-key smartparens-mode-map (kbd "<M-f7> w")     'sp-swap-enclosing-sexp)
    (define-key smartparens-mode-map (kbd "<M-f7> U")     'sp-unwrap-sexp)
    (define-key smartparens-mode-map (kbd "<M-f7> W")     'sp-backward-unwrap-sexp)
    (define-key smartparens-mode-map (kbd "<M-f7> C")     'sp-convolute-sexp)
    (define-key smartparens-mode-map (kbd "<M-f7> A")     'sp-absorb-sexp)
    (define-key smartparens-mode-map (kbd "<M-f7> E")     'sp-emit-sexp)
    ;; (define-key smartparens-mode-map (kbd "<M-f7> ")     'sp-extract-before-sexp)
    ;; (define-key smartparens-mode-map (kbd "<M-f7> ")     'sp-extract-after-sexp)
    (define-key smartparens-mode-map (kbd "<M-f7> |")     'sp-split-sexp)
    (define-key smartparens-mode-map (kbd "<M-f7> J")     'sp-join-sexp)
    (define-key smartparens-mode-map (kbd "<M-f7> <deletechar>")  'sp-change-inner)
    (define-key smartparens-mode-map (kbd "<M-f7> - n")   'sp-change-inner)
    (define-key smartparens-mode-map (kbd "<M-f7> - .")   'sp-change-enclosing)
    (define-key smartparens-mode-map (kbd "<M-f7> l l")   'sp-splice-sexp)
    (define-key smartparens-mode-map (kbd "<M-f7> l [")   'sp-splice-sexp-killing-backward)
    (define-key smartparens-mode-map (kbd "<M-f7> l ]")   'sp-splice-sexp-killing-forward)
    (define-key smartparens-mode-map (kbd "<M-f7> l o")   'sp-splice-sexp-killing-around)
    (define-key smartparens-mode-map (kbd "<M-f7> - ]")   'sp-kill-sexp)
    (define-key smartparens-mode-map (kbd "<M-f7> - [")   'sp-backward-kill-sexp)
    (define-key smartparens-mode-map (kbd "<M-f7> - }")   'sp-kill-hybrid-sexp)
    (define-key smartparens-mode-map (kbd "<M-f7> - l")   'sp-kill-whole-line)
    (define-key smartparens-mode-map (kbd "<M-f7> DEL -") 'sp-delete-region)
    (define-key smartparens-mode-map (kbd "<M-f7> - -")   'sp-kill-region)
    (define-key smartparens-mode-map (kbd "<M-f7> DEL n") 'sp-delete-char)
    (define-key smartparens-mode-map (kbd "<M-f7> DEL p") 'sp-backward-delete-char)
    (define-key smartparens-mode-map (kbd "<M-f7> DEL v") 'sp-backward-delete-word)
    (define-key smartparens-mode-map (kbd "<M-f7> DEL w") 'sp-delete-word)
    (define-key smartparens-mode-map (kbd "<M-f7> - v")   'sp-backward-kill-word)
    (define-key smartparens-mode-map (kbd "<M-f7> - w")   'sp-kill-word)
    (define-key smartparens-mode-map (kbd "<M-f7> DEL a") 'sp-backward-delete-symbol)
    (define-key smartparens-mode-map (kbd "<M-f7> DEL s") 'sp-delete-symbol)
    (define-key smartparens-mode-map (kbd "<M-f7> - a")   'sp-backward-kill-symbol)
    (define-key smartparens-mode-map (kbd "<M-f7> - s")   'sp-kill-symbol)
    (define-key smartparens-mode-map (kbd "<M-f7> . n")   'sp-select-next-thing)
    (define-key smartparens-mode-map (kbd "<M-f7> . p")   'sp-select-previous-thing)
    (define-key smartparens-mode-map (kbd "<M-f7> . N")   'sp-select-next-thing-exchange)
    (define-key smartparens-mode-map (kbd "<M-f7> . P")   'sp-select-previous-thing-exchange)
    (define-key smartparens-mode-map (kbd "<M-f7> . .")   'sp-mark-sexp)
    (define-key smartparens-mode-map (kbd "<M-f7> TAB")   'sp-indent-adjust-sexp)
    (define-key smartparens-mode-map (kbd "<M-f7> <backtab>") 'sp-dedent-adjust-sexp)))

;; -----------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> k`` : Keyboard macro operations

(define-pel-global-prefix pel:kbmacro (kbd "<f11> k"))
(define-key pel:kbmacro "k"   'pel-forget-recorded-keyboard-macro)
(define-key pel:kbmacro "i"  #'insert-kbd-macro)

(when pel-use-centimacro
  ;; Until abo-abo integrates my pull-request that fixes the bugs
  ;; I'll support this package via my copy of the file from my fork
  ;; of the original project at https://github.com/abo-abo/centimacro
  (pel-install-github-file "pierre-rouleau/centimacro/master"
                           "centimacro.el")
  (pel-autoload-file centimacro for:
                     centi-assign
                     centi-summary
                     centi-restore-all)
  (global-set-key (kbd pel-centi-assign-key) 'centi-assign)
  (define-key pel:kbmacro "="          'centi-assign)
  (define-key pel:kbmacro "?"          'centi-summary)
  (define-key pel:kbmacro (kbd "DEL")  'centi-restore-all)
  (pel-eval-after-load centimacro
    ;; Restore PEL's binding of <f5> to `repeat' despite centimacro's default
    ;; customization which binds <f5> to centi-assign.  PEL provides the
    ;; `pel-centi-assign-key' which identifies the key binding for the
    ;; `centi-assign' command.
    (global-set-key (kbd "<f5>") 'repeat)))

(when pel-use-elmacro
  (define-pel-global-prefix pel:elmacro (kbd "<f11> k l"))
  (pel-ensure-package elmacro from: melpa)
  (pel-autoload-file elmacro for: elmacro-mode)
  (define-key pel:elmacro "l" 'elmacro-mode)
  (pel-eval-after-load elmacro
    (define-key pel:elmacro "m" 'elmacro-show-last-macro)
    (define-key pel:elmacro "c" 'elmacro-show-last-commands)
    (define-key pel:elmacro (kbd "DEL") 'elmacro-clear-command-history)))

(when pel-use-emacros
  (define-pel-global-prefix pel:emacros (kbd "<f11> k e"))
  (pel-install-github-file "pierre-rouleau/emacros/master" "emacros.el")
  (pel-autoload-file emacros for:
                     emacros-load-macros
                     emacros-show-macros
                     emacros-show-macro-names
                     emacros-name-last-kbd-macro-add)
  (add-hook 'find-file-hook 'emacros-load-macros)
  (define-key pel:emacros "L" 'emacros-load-macros)
  (define-key pel:emacros "?" 'emacros-show-macros)
  (define-key pel:emacros "/" 'emacros-show-macro-names)
  (define-key pel:emacros "=" 'emacros-name-last-kbd-macro-add)
  (pel-eval-after-load emacros
    (global-set-key "\C-ce" 'emacros-execute-named-macro)
    (global-set-key "\C-cx" 'emacros-auto-execute-named-macro)
    (define-key pel:emacros "e" 'emacros-execute-named-macro)
    (define-key pel: (kbd "<f4>") 'emacros-execute-named-macro)
    (define-key pel:emacros "R" 'emacros-refresh-macros)
    (define-key pel:emacros "r" 'emacros-rename-macro)
    (define-key pel:emacros "m" 'emacros-move-macro)
    (define-key pel:emacros (kbd "DEL") 'emacros-remove-macro)))

;; ---------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> l`` : Line control commands

(define-pel-global-prefix pel:linectrl (kbd "<f11> l"))
(define-key pel:linectrl (kbd "<up>")    'pel-lc-previous-logical-line)
(define-key pel:linectrl (kbd "<down>")  'pel-lc-next-logical-line)
(define-key pel:linectrl "c"             'pel-toggle-line-col-modes)
(define-key pel:linectrl "l"            (if (version< emacs-version "26.1")
                                            'linum-mode
                                          'display-line-numbers-mode))
(define-key pel:linectrl "t"            #'toggle-truncate-lines)
(define-key pel:linectrl "v"            #'visual-line-mode)

;; ---------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> m`` : Multiple Cursors

(when pel-use-multiple-cursors

  (define-pel-global-prefix pel:mcursors (kbd "<f11> m"))
  (pel-ensure-package multiple-cursors from: melpa)
  (pel-autoload-file multiple-cursors for:
                     mc/edit-lines
                     mc/mark-next-like-this
                     mc/mark-previous-like-this
                     mc/mark-all-like-this)
  ;; guessing from point
  (define-key pel:mcursors "m" 'mc/mark-all-like-this-dwim)
  (define-key pel:mcursors (kbd "M-m") 'mc/mark-all-dwim)
  (define-key pel:mcursors "." 'mc/mark-more-like-this-extended)
  ;; lines
  (define-key pel:mcursors "l" 'mc/edit-lines)
  (define-key pel:mcursors (kbd "C-a") 'mc/edit-beginnings-of-lines)
  (define-key pel:mcursors (kbd "C-e") 'mc/edit-ends-of-lines)
  ;; all like this
  (define-key pel:mcursors "a" 'mc/mark-all-like-this)
  (define-key pel:mcursors (kbd "C-M-a") 'mc/mark-all-like-this-in-defun)
  (define-key pel:mcursors "?" 'mc/mark-all-in-region)
  ;; like this: next and previous, mark, unmark, skip & extended
  (define-key pel:mcursors "n" 'mc/mark-next-like-this)
  (define-key pel:mcursors "p" 'mc/mark-previous-like-this)
  (define-key pel:mcursors "N" 'mc/unmark-next-like-this)
  (define-key pel:mcursors "P" 'mc/unmark-previous-like-this)
  (define-key pel:mcursors (kbd "M-n") 'mc/skip-to-next-like-this)
  (define-key pel:mcursors (kbd "M-p") 'mc/skip-to-previous-like-this)
  ;; word
  (define-key pel:mcursors "w" 'mc/mark-next-word-like-this)
  (define-key pel:mcursors (kbd "M-w") 'mc/mark-next-like-this-word)
  (define-key pel:mcursors "W" 'mc/mark-previous-word-like-this)
  (define-key pel:mcursors (kbd "M-W") 'mc/mark-previous-like-this-word)
  (define-key pel:mcursors (kbd "C-w") 'mc/mark-all-words-like-this)
  (define-key pel:mcursors (kbd "C-M-w") 'mc/mark-all-words-like-this-in-defun)
  ;; symbol
  (define-key pel:mcursors "s" 'mc/mark-next-symbol-like-this)
  (define-key pel:mcursors (kbd "M-s") 'mc/mark-next-like-this-symbol)
  (define-key pel:mcursors "S" 'mc/mark-previous-symbol-like-this)
  (define-key pel:mcursors (kbd "M-S") 'mc/mark-previous-like-this-symbol)
  (define-key pel:mcursors (kbd "C-s") 'mc/mark-all-symbols-like-this)
  (define-key pel:mcursors (kbd "C-M-s") 'mc/mark-all-symbols-like-this-in-defun)
  ;; special
  (define-key pel:mcursors "c" 'set-rectangular-region-anchor)
  (define-key pel:mcursors "t" 'mc/mark-sgml-tag-pair)
  (define-key pel:mcursors "0" 'mc/insert-numbers)
  (define-key pel:mcursors "A" 'mc/insert-letters)
  (define-key pel:mcursors "o" 'mc/sort-regions)
  (define-key pel:mcursors "O" 'mc/reverse-regions)
  (define-key pel:mcursors "|" 'mc/vertical-align-with-space)
  ;; TODO: put key in mc/keymap
  (autoload 'mc-hide-unmatched-lines-mode "mc-hide-unmatched-line-mode")
  (define-key pel:mcursors (kbd "M-/") 'mc-hide-unmatched-lines-mode)

  (when pel-use-iedit
    (define-key pel:mcursors "i" 'iedit-mode)))

;; ---------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> o`` : ordering (sorting)

(define-pel-global-prefix pel:order (kbd "<f11> o"))
(define-key pel:order "l" #'sort-lines)
(define-key pel:order "p" #'sort-paragraphs)
(define-key pel:order "/" #'sort-pages )
(define-key pel:order "," #'sort-fields)
(define-key pel:order "n" #'sort-numeric-fields)
(define-key pel:order "c" #'sort-columns)
(define-key pel:order "x" #'sort-regexp-fields)
(define-key pel:order "r" #'reverse-region)
(define-key pel:order "F"  'pel-toggle-sort-fold-case)

(defun pel-toggle-sort-fold-case ()
  "Toggle the sort case sensitivity."
  (interactive)
  (message "Sort is now case %s."
           (if (pel-toggle 'sort-fold-case)
               "insensitive"
             "sensitive (the default)")))

;; -----------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> r`` : Register commands

(define-pel-global-prefix pel:register (kbd "<f11> r"))
(define-key pel:register "l"    #'list-registers)
(define-key pel:register "v"    #'view-register)
;;
(define-key pel:register " "     'pel-point-to-register)
(define-key pel:register "+"    #'increment-register)
(define-key pel:register "F"     'pel-filename-to-register)
(define-key pel:register "f"     'pel-frameset-to-register)
(define-key pel:register "k"     'pel-kmacro-to-register)
(define-key pel:register "n"     'pel-number-to-register)
(define-key pel:register "r"     'pel-copy-rectangle-to-register)
(define-key pel:register "s"     'pel-copy-to-register)
(define-key pel:register "w"     'pel-window-configuration-to-register)
;;
(define-key pel:register ","    #'prepend-to-register)
(define-key pel:register "."    #'append-to-register)
;;
(define-key pel:register "j"    #'jump-to-register)
(define-key pel:register "i"    #'insert-register)

;; -----------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> s`` : Search/Replace  commands

(define-pel-global-prefix pel:search-replace (kbd "<f11> s"))
(define-key pel:search-replace "."  'pel-search-word-from-top)
(define-key pel:search-replace "b" #'search-backward)
(define-key pel:search-replace "f" #'search-forward)
(define-key pel:search-replace "o" #'multi-occur)
(define-key pel:search-replace "O" #'multi-occur-in-matching-buffers)
(define-key pel:search-replace "r" #'replace-string)
(define-key pel:search-replace "?"  'pel-show-search-status)
;; "S" reserved

(define-pel-global-prefix pel:search-word (kbd "<f11> s w"))
(define-key pel:search-word "f"  #'word-search-forward)
(define-key pel:search-word "F"  #'word-search-forward-lax)
(define-key pel:search-word "b"  #'word-search-backward)
(define-key pel:search-word "B"  #'word-search-backward-lax)
(define-key pel:search-word "i"  #'isearch-forward-word)

(define-pel-global-prefix pel:search-mode (kbd "<f11> s m"))
(define-key pel:search-mode "?"   'pel-show-search-case-state)
(define-key pel:search-mode "f"   'pel-toggle-case-fold-search)
(define-key pel:search-mode "u"   'pel-toggle-search-upper-case)
(define-key pel:search-mode "l"  #'isearch-toggle-lax-whitespace)

;; --
;; Search Tool Control

(when pel-use-anzu
  (pel-ensure-package anzu from: melpa)
  (pel-autoload-file anzu for: global-anzu-mode)
  (when (eq pel-initial-search-tool 'anzu)
    (pel-after-startup-do (global-anzu-mode +1))))

(when pel-use-cexp
  ;; download and byte-compile cexp if not already present
  ;; Do it after compiling pel_keys.el, when pel-init load pel_keys.
  (pel-install-github-files "TobiasZawada/cexp/master" '("cexp.el"
                                                         "cexp-test.el"))
  (pel-autoload-file cexp for: cexp-search-forward)
  (define-key pel:search-replace "c" 'cexp-search-forward))

(when pel-use-swiper
  (pel-ensure-package swiper from: melpa)
  (pel-autoload-file swiper for: swiper)
  (when (eq pel-initial-search-tool 'swiper)
    (global-set-key "\C-s" 'swiper)))

(defun pel-number-of-available-search-tools ()
  "Return the number of available search tools."
  (let ((count 1))
    (dolist (option '(pel-use-anzu
                      pel-use-swiper))
      (when (eval option)
        (setq count (1+ count))))
    count))

(when (> (pel-number-of-available-search-tools) 1)
  (pel-autoload-file pel-search for:
                     pel-select-search-tool)
  (define-key pel:search-replace "s" 'pel-select-search-tool)
  (define-key pel:help           "s" 'pel-show-search-status))

;; --
;; Regular Expression Builder

(defun pel-reb-re-syntax ()
  "Customize reb-re-syntax: Regular Expression Builder syntax."
  (interactive)
  (customize-option 'reb-re-syntax))

(define-pel-global-prefix pel:regexp (kbd "<f11> s x"))
(define-key pel:regexp      " "   'pel-insert-regexp)
(define-key pel:regexp      "b"  #'re-search-backward)
(global-set-key      (kbd "M-R") #'re-search-backward)
(define-key pel:regexp      "f"  #'re-search-forward)
(define-key pel:regexp      "q"  #'query-replace-regexp)
(define-key pel:regexp      "r"  #'replace-regexp)
(define-key pel:regexp      "i"  #'isearch-query-replace-regexp)
(define-key pel:regexp      "B"  #'re-builder)
;; add it here because C-M-% cannot be typed in terminal mode
(define-key pel:regexp  (kbd "M-B")  'pel-reb-re-syntax)
(when pel-bind-keys-for-regexp
  (define-key global-map (kbd "C-c r") 'replace-regexp)
  (define-key global-map (kbd "C-c q") 'query-replace-regexp)
  (define-key global-map (kbd "C-c Q") 'isearch-query-replace-regexp))
;;

;; --
;; Other Regular Expression support

(when pel-use-regex-tool
  (defun pel-select-regex-tool-backend ()
    "Select regexp-tool backend via customization."
    (interactive)
    (customize-option 'regex-tool-backend))

  (pel-ensure-package regex-tool from: melpa)
  (pel-autoload-file regex-tool for: regex-tool)
  (define-key pel:regexp "T" 'regex-tool)
  (pel-eval-after-load regex-tool
    (when (boundp 'regex-tool-mode-map)
      (define-key
        regex-tool-mode-map (kbd "C-c <f2>")
        'pel-select-regex-tool-backend))))

(when pel-use-pcre2el
  (pel-ensure-package pcre2el from: melpa)
  (pel-autoload-file pcre2el for:
                     rxt-mode
                     pcre-mode)
  (define-key pel:regexp "p" 'rxt-mode)
  (define-key pel:regexp "P" 'pcre-mode))

(when pel-use-visual-regexp
  (pel-ensure-package visual-regexp from: melpa)
  (pel-autoload-file visual-regexp for:
                     vr/replace
                     vr/query-replace
                     vr/mc-mark)
  (define-key pel:regexp "R" 'vr/replace)
  (define-key pel:regexp "Q" 'vr/query-replace)
  (when pel-use-multiple-cursors
    (define-key pel:regexp "M" 'vr/mc-mark))

  (pel-autoload-file pel-search-regexp for:
                     pel-select-search-regexp-engine
                     pel-replace-regexp
                     pel-query-replace-regexp)
  (define-key pel:search-replace "S" 'pel-select-search-regexp-engine)
  ;; replace some already bound keys
  (define-key pel:regexp "r" 'pel-replace-regexp)
  (define-key pel:regexp "q" 'pel-query-replace-regexp)
  (when pel-bind-keys-for-regexp
    (define-key global-map (kbd "C-c r") 'pel-replace-regexp)
    (define-key global-map (kbd "C-c q") 'pel-query-replace-regexp)
    (when pel-use-multiple-cursors
      (define-key global-map (kbd "C-c m") 'vr/mc-mark))))

(when pel-use-visual-regexp-steroids
  (pel-ensure-package visual-regexp-steroids from: melpa)
  (pel-autoload-file visual-regexp-steroids for:
                     vr/select-replace
                     vr/select-query-replace
                     vr/select-mc-mark
                     vr/isearch-forward
                     vr/isearch-backward)
  (define-key pel:regexp (kbd "M-r") 'vr/select-replace)
  (define-key pel:regexp (kbd "M-q") 'vr/select-query-replace)
  (define-key pel:regexp (kbd "M-m") 'vr/select-mc-mark)
  (define-key pel:regexp (kbd "C-s") 'vr/isearch-forward)
  (define-key pel:regexp (kbd "C-r") 'vr/isearch-backward))

;; Choices:
;; - Standard Emacs Search/Replace
;; - Visual Regexp
;; - Visual Regexp - emacs plain
;; - Visual Regexp - emacs
;; - Visual Regexp - pcre2el
;; - Visual Regexp - python

;; - - -
;; xr-lint
;; xr-pp
;; xr - Emacs regexp parser and analyzer

(when pel-use-xr
  (pel-ensure-package xr from: gnu)
  (pel-autoload-file xr for:
                     xr-pp
                     xr-lint)
  (pel-autoload-file pel-xr for:
                     pel-xr-at-point
                     pel-xr-regxp
                     pel-xr-lint
                     pel-xr-lint-at-point)
  (define-key pel:regexp "x" 'pel-xr-at-point)
  (define-key pel:regexp "X" 'pel-xr-regxp)
  (define-key pel:regexp "l" 'pel-xr-lint-at-point)
  (define-key pel:regexp "L" 'pel-xr-lint))

;; - - -
;; relint

(when pel-use-relint
  ;; provide access to the command via search/regexp and elisp/analyze
  (define-pel-global-prefix pel:regxp-lint (kbd "<f11> s x M-l"))
  (define-pel-global-prefix pel:elisp-regxp-lint (kbd "<f11> SPC l a l"))

  (pel-ensure-package relint from: gnu)
  (pel-autoload-file relint for:
                     relint-current-buffer
                     relint-file
                     relint-directory)
  (define-key pel:regxp-lint "b" 'relint-current-buffer)
  (define-key pel:regxp-lint "f" 'relint-file)
  (define-key pel:regxp-lint "d" 'relint-directory)
  (define-key pel:elisp-regxp-lint "b" 'relint-current-buffer)
  (define-key pel:elisp-regxp-lint "f" 'relint-file)
  (define-key pel:elisp-regxp-lint "d" 'relint-directory))

;; -----------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> M-s`` : Speedbar/SR-Speedbar commands

(when pel-use-speedbar
  ;; Install sr-speedbar from my GitHub depot instead of from Melpa,
  ;; because Melpa follows the older version in emacsmirror. I'm keeping
  ;; min closer to what's available in the emacswiki.
  (pel-install-github-file "pierre-rouleau/sr-speedbar/master"
                           "sr-speedbar.el")
  (pel-autoload-file sr-speedbar for:
                     sr-speedbar-toggle
                     sr-speedbar-window-p)
  (define-pel-global-prefix pel:speedbar (kbd "<f11> M-s"))
  (define-key pel:speedbar "?"          'pel-speedbar-info)
  (define-key pel:speedbar (kbd "M-s")  'pel-open-close-speedbar)
  (define-key pel:speedbar (kbd "M-b")  'pel-sr-speedbar-toggle-select-behaviour)
  (define-key pel:speedbar (kbd "M-t")  'pel-toggle-to-speedbar)
  (define-key pel:speedbar (kbd "M-R")  'pel-speedbar-toggle-refresh)
  (define-key pel:speedbar (kbd "M-r")  'pel-speedbar-refresh)
  (define-key pel:speedbar (kbd "M-a")  'pel-speedbar-toggle-show-all-files)
  (define-key pel:speedbar (kbd "M-o")  'pel-speedbar-toggle-sorting)
  (define-key pel:speedbar (kbd "M-f")  'pel-speedbar-focus-current-file)
  ;; (define-key pel:speedbar "e"  #'speedbar-toggle-etags)
  (when pel-emacs-is-graphic-p
    (define-key pel:speedbar (kbd "M-i") 'pel-speedbar-toggle-images))

  (defun pel--sr-speedbar-setup()
    "Setup the sr-speedbar hooks."
    ;; Remove sr-speedbar hooks that switch back to the speedbar buffer
    (remove-hook 'speedbar-before-visiting-file-hook 'sr-speedbar-before-visiting-file-hook)
    (remove-hook 'speedbar-before-visiting-tag-hook  'sr-speedbar-before-visiting-tag-hook)
    (remove-hook 'speedbar-visiting-file-hook        'sr-speedbar-visiting-file-hook)
    (remove-hook 'speedbar-visiting-tag-hook         'sr-speedbar-visiting-tag-hook)
    ;; Instead add hooks to a command can controls the behaviour
    (add-hook 'speedbar-visiting-file-hook         'pel-sr-speedbar-visiting-control t)
    (add-hook 'speedbar-visiting-tag-hook
              'pel-sr-speedbar-visiting-control t))
  (declare-function pel--sr-speedbar-setup "pel_keys")

  (pel-eval-after-load sr-speedbar
    (advice-add 'sr-speedbar-open :after (function pel--sr-speedbar-setup)))

  ;; Note: when pel-use-projectile-speedbar is active
  ;;       then pel-use-speedbar is also active, as imposed by
  ;;       the end of pel--options.el.
  (when pel-use-projectile-speedbar
    (pel-ensure-package projectile-speedbar from: melpa)
    (pel-autoload-file projectile-speedbar for:
                       projectile-speedbar-open-current-buffer-in-tree
                       projectile-speedbar-toggle)
    (pel-eval-after-load projectile
      (define-key projectile-command-map (kbd "M-s")
        'projectile-speedbar-open-current-buffer-in-tree))))

;; ---------------------------------------------------------------------------
;; - Function Keys - <f11> -        ``<f11> T`` : Directory Tree
;; The <f11> T key is assigned to ztree-dir when pel-use-ztree is t.
;; See the code above.

;; ---------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> t`` : Text control commands

(define-pel-global-prefix pel:text (kbd "<f11> t"))
(define-key pel:text "c"   'pel-capitalize-word-or-region)
(define-key pel:text "l"   'pel-downcase-word-or-region)
(define-key pel:text "u"   'pel-upcase-word-or-region)
(define-key pel:text "i"  #'toggle-input-method)
(define-key pel:text "I"  #'set-input-method)
(define-key pel:text "o"  #'overwrite-mode)
;; RESERVED          "O"  #'nhexl-overwrite-only-mode)
(define-key pel:text "p"  #'picture-mode)
;;

;; TODO: investigate the following:
;; The following does not work.  I don't know why yet.
;; (defun pel-toggle-nhexl ()
;;   "Toggle buffer between normal text and nhexl/nibble mode."
;;   (interactive)
;;   (if (and (boundp 'nhexl-mode) nhexl-mode)
;;       (progn
;;         ;; Disabling (commented-out code) does not work.  Why?
;;         ;(nhexl-overwrite-only-mode nil)
;;         ;(nhexl-nibble-edit-mode)
;;         ;(nhexl-mode)
;;         (user-error "To deactivate nhexl modes,\
;;  issue the following 2 commands:\
;; \n M-x nhexl-mode\
;; \n M-x nhexl-nibble-edit-mode"))
;;     (progn
;;       (nhexl-mode t)
;;       (nhexl-nibble-edit-mode t)
;;       (message "Activating nhexl nibble mode"))))

(when pel-use-nhexl-mode
  (pel-ensure-package nhexl-mode from: gnu)
  (pel-autoload-file nhexl-mode for:
                     nhexl-mode
                     nhexl-nibble-edit-mode
                     nhexl-overwrite-only-mode)
  (define-key pel:text   "O"  #'nhexl-overwrite-only-mode)
  (define-key pel:buffer "x"  #'nhexl-mode)
  ;; Toggle nibble editing (mainly useful in nhexl mode,
  ;; but can also be used in normal mode to enter character
  ;; in hexadecimal easily)
  (define-key pel:buffer "X"  #'nhexl-nibble-edit-mode))

;; - Optimized keys for Case Conversion
;; ------------------------------------
;; Take advantage of the Shift key with Meta for these operations,
;; since the original Emacs commands break the visible region anyway
;; so we don't have to worry about stealing the Shift key when we want
;; to use one of the case conversion command.
(global-set-key (kbd "M-c")   'pel-downcase-word-or-region)
(global-set-key (kbd "M-C")   'pel-upcase-word-or-region)
(global-set-key (kbd "M-T")   'pel-capitalize-word-or-region)

;; The following does not work and I don;t know why
;; (defun pel-toggle-overwrite ()
;;   "Toggle the overwrite mode.  Support normal and nhexl modes."
;;   (interactive)
;;   (if (and (boundp 'nhexl-mode) nhexl-mode)
;;       (if nhexl-overwrite-only-mode
;;           (nhexl-overwrite-only-mode nil)
;;         (nhexl-overwrite-only-mode t))
;;     (progn
;;       (overwrite-mode)
;;       (message "Toggled overwrite in text mode".))))

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; - Function Keys - <f11> - Prefix ``<f11> t a``: Text align

(define-pel-global-prefix pel:align (kbd "<f11> t a"))
(define-key pel:align "a" #'align)
(define-key pel:align "c" #'align-current)
(define-key pel:align "e" #'align-entire)
(define-key pel:align "l" #'align-newline-and-indent)
(define-key pel2: (kbd "M-a")  #'align-regexp)
(define-key pel2: (kbd "M-A")  'pel-multi-align-regexp)
(define-key pel:align "r" #'align-regexp)
(define-key pel:align "h" #'align-highlight-rule)
(define-key pel:align "H" #'align-unhighlight-rule)
(define-key pel:align "?"  'pel-show-if-newline-aligns)


;; - Alias for align-regexp: ar
;; ----------------------------
;; Use M-x ar to align a region with a regular expression.
(defalias 'ar #'align-regexp)

(pel-add-hook-for
 'pel-modes-activating-align-on-return
 (lambda ()
   (defvar pel-newline-does-align)      ;forward declare
   (setq pel-newline-does-align t)))

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; - Function Keys - <f11> - Prefix ``<f11> t e``: Enriched Text
(define-pel-global-prefix pel:enriched-text (kbd "<f11> t e"))

(define-key pel:enriched-text "e" #'enriched-mode)

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; - Function Keys - <f11> - Prefix ``<f11> t f``: Text fill
;;
(define-pel-global-prefix pel:fill (kbd "<f11> t f"))
(define-key pel:fill "?"    'pel-show-fill-columns)
(define-key pel:fill "c"   #'set-fill-column)
(define-key pel:fill "."   #'set-fill-prefix)
(define-key pel:fill "a"   #'auto-fill-mode)
(define-key pel:fill ";"    'pel-auto-fill-only-comments)
(define-key pel:fill "f"   #'refill-mode)
(define-key pel:fill "l"   #'lisp-fill-paragraph)
(define-key pel:fill "i"   #'fill-individual-paragraphs)
(define-key pel:fill "n"   #'fill-nonuniform-paragraphs)
(define-key pel:fill "p"   #'fill-paragraph)
(define-key pel:fill "r"   #'fill-region)
(define-key pel:fill "q"   #'fill-region-as-paragraph)

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; - Function Keys - <f11> - Prefix ``<f11> t j``: Text justification
;;
(define-pel-global-prefix pel:justification (kbd "<f11> t j"))
(define-key pel:justification "b" #'set-justification-full) ;'b' for "both-side"
(define-key pel:justification "c" #'set-justification-center)
(define-key pel:justification "l" #'set-justification-left)
(define-key pel:justification "u" #'set-justification-none) ; un-justify
(define-key pel:justification "r" #'set-justification-right)

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; - Function Keys - <f11> - Prefix ``<f11> t m``: Text word modes
;;
(define-pel-global-prefix pel:textmodes (kbd "<f11> t m"))
(define-key pel:textmodes "'" #'electric-quote-local-mode)
(define-key pel:textmodes "?"  'pel-show-text-modes)
(define-key pel:textmodes "b" #'subword-mode)
(define-key pel:textmodes "d" #'delete-selection-mode)
(define-key pel:textmodes "g" #'glasses-mode)
(define-key pel:textmodes "p" #'superword-mode)
(define-key pel:textmodes "s"  'pel-toggle-sentence-end)
(define-key pel:textmodes "v" #'visible-mode)
(define-key pel:textmodes "w" #'whitespace-mode)
(define-key pel:textmodes "e" #'enriched-mode)

(pel-add-hook-for 'pel-modes-activating-superword-mode
                  (lambda ()
                    (superword-mode 1)))
(pel-add-hook-for 'pel-modes-activating-subword-mode
                  (lambda ()
                    (subword-mode 1)))
(pel-add-hook-for 'pel-modes-activating-glasses-mode
                  (lambda ()
                    (glasses-mode 1)))
(pel-add-hook-for 'pel-modes-activating-auto-fill-mode
                  (lambda ()
                    (auto-fill-mode 1)))
(pel-add-hook-for 'pel-modes-activating-whitespace-mode
                  (lambda ()
                    (whitespace-mode 1)))
(pel-add-hook-for 'pel-modes-activating-electric-quote-local-mode
                  (lambda ()
                    (electric-quote-local-mode 1)))

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; Function Keys - <f11> - Prefix ``<f11> t t``: Text transpose commands
;;
(define-pel-global-prefix pel:text-transpose (kbd "<f11> t t"))
(define-key pel:text-transpose "c"  #'transpose-chars)
(define-key pel:text-transpose "w"  #'transpose-words)
(define-key pel:text-transpose "l"  #'transpose-lines)
(define-key pel:text-transpose "s"  #'transpose-sentences)
(define-key pel:text-transpose "p"  #'transpose-paragraphs)
(define-key pel:text-transpose "x"  #'transpose-sexps)
;; TODO: pel-transpose-functions, pel-transpose-classes,
;;       pel-transpose-statements, pel-transpose-clauses

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; - Function Keys - <f11> - Prefix ``<f11> t w`` : Text whitespace commands
;;
(define-pel-global-prefix pel:text-whitespace (kbd "<f11> t w"))
(define-key pel:text-whitespace " "         #'untabify)
(define-key pel:text-whitespace (kbd "TAB") #'tabify)
(define-key pel:text-whitespace "I"          'pel-toggle-indent-tabs-mode)
(define-key pel:text-whitespace "."         #'cycle-spacing)
(define-key pel:text-whitespace "c"         #'whitespace-cleanup)
(define-key pel:text-whitespace "e"          'pel-toggle-indicate-empty-lines)
(define-key pel:text-whitespace "m"         #'whitespace-mode)
(define-key pel:text-whitespace "o"         #'whitespace-toggle-options)
(define-key pel:text-whitespace "T"        'pel-toggle-show-trailing-whitespace)
(define-key pel:text-whitespace "t"         #'delete-trailing-whitespace)
(define-key pel:text-whitespace (kbd "M-W") 'pel-toggle-delete-trailing-space-on-save)

;; ---------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> T`` : Time-Tracking
(define-pel-global-prefix pel:time (kbd "<f11> T"))
(when pel-use-timeclock
  (define-key pel:time "W"         'display-time-world)
  (define-key pel:time (kbd "M-d") 'timeclock-mode-line-display)
  ;; add standard key bindings.
  (define-key ctl-x-map "ti" 'timeclock-in)
  (define-key ctl-x-map "to" 'timeclock-out)
  (define-key ctl-x-map "tc" 'timeclock-change)
  (define-key ctl-x-map "tr" 'timeclock-reread-log)
  (define-key ctl-x-map "tu" 'timeclock-update-mode-line)
  (define-key ctl-x-map "tw" 'timeclock-when-to-leave-string)

  (when pel-use-timeclock-timelog
    (pel-install-github-file "pierre-rouleau/timelog/master" "timelog.el")
    (pel-autoload-file timelog for:
                       timelog-summarize-day
                       timelog-summarize-today
                       timelog-summarize-month
                       timelog-summarize-range
                       timelog-summarize-each-day-in-range
                       timelog-current-project
                       timelog-workday-elapsed
                       timelog-open-file)
    (define-key ctl-x-map "tld" 'timelog-summarize-day)
    (define-key ctl-x-map "tlt" 'timelog-summarize-today)
    (define-key ctl-x-map "tlm" 'timelog-summarize-month)
    (define-key ctl-x-map "tlr" 'timelog-summarize-range)
    (define-key ctl-x-map "tlD" 'timelog-summarize-each-day-in-range)
    (define-key ctl-x-map "tlp" 'timelog-current-project)
    (define-key ctl-x-map "tle" 'timelog-workday-elapsed)
    (define-key ctl-x-map "tlf" 'timelog-open-file)))

;; (when pel-use-chronometrist
;;   (pel-ensure-package chronometrist from: melpa-stable))

;; ---------------------
;; Time Management tools
(when pel-use-tzc
  (pel-ensure-package tzc from melpa))

;; ---------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> v`` : VCS operations
;;
(define-pel-global-prefix pel:vcs (kbd "<f11> v"))
(define-key pel:vcs "v"  'vc-dir)
(define-key pel:vcs "s"  'pel-vcs-switch-backend)
(define-key pel:vcs "l"  'pel-vcs-toggle-vc-log)

;; ----------------
;; Git support
(when pel-use-magit
  (define-pel-global-prefix pel:vcs-magit (kbd "<f11> v g"))

  ;; Magit delays its key bindings after Emacs initialization to avoid adding
  ;; bindings to commands that already have one.  PEL circumvents this to add
  ;; one key binding under pel:vcs so that familiar Magit key bindings are
  ;; available while also providing a PEL VCS binding indicating that Magit is
  ;; available: it also delays it using the after-init-hook *and* appends to
  ;; ensure that this hook will be executed after Magit bind its keys.
  (defun pel--cfg-magit-keys ()
    "Delayed Magit configuration - add extra key bindings to Magit."
    (define-key pel:vcs-magit "s" 'magit-status))

  (pel-ensure-package magit from: melpa)
  (pel-autoload-file magit for:
                     magit
                     magit-status)
  (add-hook 'after-init-hook 'pel--cfg-magit-keys :append)

  ;;
  (when pel-use-treemacs-magit
    (pel-ensure-package treemacs-magit from: melpa)))

(when pel-use-gitignore-mode
  (pel-ensure-package gitattributes-mode from: melpa)
  (pel-ensure-package gitconfig-mode from: melpa)
  (pel-ensure-package gitignore-mode from: melpa))

;; ----------------
;; Mercurial Support
(when pel-use-monky
  (define-pel-global-prefix pel:vcs-monky (kbd "<f11> v m"))
  (pel-ensure-package monky from: melpa)
  (pel-autoload-file monky for: monky-status)
  (define-key pel:vcs-monky "s"  'monky-status)
  (define-key pel:vcs-monky "b"  'monky-blame-current-file))

(when pel-use-hgignore-mode
  ;; Install & compile hgignore-mode if requested.  No key assignment;
  ;; the package installation will activate the file name association
  ;; and the auto-loading.
  (pel-ensure-package hgignore-mode from: melpa))

;; ----------------
;; Perforce Support
;; [:todo 2022-03-22, by Pierre Rouleau: Complete Perforce support, delay
;; load, add F11 and F12 key support, ensure that both work together, document.]
(when pel-use-perforce
  (when (memq pel-use-perforce '(both vc-p4))
    (pel-install-file
     "https://swarm.workshop.perforce.com/downloads/guest/magnus_henoch/vc-p4/p4-lowlevel.el"
     "p4-lowlevel.el")
    (pel-install-file
     "https://swarm.workshop.perforce.com/downloads/guest/magnus_henoch/vc-p4/vc-p4.el"
     "vc-p4.el")
    (require 'vc-p4))
  (when (memq pel-use-perforce '(both p4))
    (pel-ensure-package p4 from: melpa)
    (require 'p4))
  (when (and (eq pel-use-perforce 'both)
             (boundp 'p4-do-find-file))
    (setq p4-do-find-file nil)))

;; ----------------
;; Subversion Support
(when pel-use-dsvn
  (pel-ensure-package dsvn from: melpa))

(when pel-use-psvn
  (pel-install-file
   "https://raw.githubusercontent.com/pierre-rouleau/psvn/main/psvn.el"
   "psvn.el"))

;; PEL extends the vc package for subversion by dynamically adding the
;; the --verbose switch to `vc-svn-global-switches' when
;; `pel-vcs-svn-verbose-log' is turned on.  This is activated right after the
;; vc-svn package is loaded

(pel-eval-after-load vc-svn
  (pel-vc-svn-init))

;; ----------------
;; vc-dir-mode support
;; Provide <f12> <f1>, <f12><f2> and <f12><f3> in vc-dir-mode
;; TODO simplify this code
(define-pel-global-prefix pel:for-vc-dir (kbd "<f11> SPC SPC v"))
(defun pel--setup-for-vc-dir ()
  "Activate vc-dir setup, take local variables into account."
  (pel-local-set-f12-M-f12 'pel:for-vc-dir)

  (when (boundp 'vc-dir-mode-map)
    (pel-autoload-file pel-vc for:
                       pel-vc-ignore-marked
                       pel-vc-dir-hide-state)
    (define-key vc-dir-mode-map "!" 'pel-vc-ignore-marked)
    (define-key vc-dir-mode-map "H" 'pel-vc-dir-hide-state)))

(declare-function pel--setup-for-vc-dir "pel_keys")
  (pel--mode-hook-maybe-call
   (function pel--setup-for-vc-dir)
   'vc-dir-mode 'vc-dir-mode-hook)

;; ---------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> M-d`` : Mode line commands
(define-pel-global-prefix pel:mode-line (kbd "<f11> M-d"))

(when pel-modeline-display-time
  (run-at-time "2 sec" nil (function display-time)))


(defun pel-toggle-time-display ()
  "Toggle display of time on the mode line."
  (interactive)
  (declare-function display-time-mode "time")
  (call-interactively (function display-time-mode)))

(define-key pel:mode-line "f"  'which-function-mode)
(define-key pel:mode-line "t" #'pel-toggle-time-display)
(define-key pel:mode-line "c"  'column-number-mode)

(when pel-use-show-point-mode
  (cl-eval-when 'load
    (pel-install-github-file "dmgerman/predictive/master"
                             "show-point-mode.el")
    (pel-autoload "show-point-mode" for: show-point-mode))
  (define-key pel:mode-line "p"  'show-point-mode))

;; ---------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> w`` : Windows operations
;; Use the global local winner-mode, but don't use its key bindings;
;; use some in the '<f11> w' group:

;; Used: # B O S b d f h k m n o p r s v x
;;
(define-pel-global-prefix pel:window (kbd "<f11> w"))
(define-key pel:window    "w"   'pel-close-other-window)
(define-key pel:window    "B"  #'switch-to-buffer-other-window)
(define-key pel:window    "O"   'pel-other-window-backward)
(define-key pel:window    "b"  #'display-buffer)
(define-key pel:window    "f"  #'follow-mode)
(define-key pel:window    "v"   'pel-2-vertical-windows)
(define-key pel:window    "h"   'pel-2-horizontal-windows)
;; reserved:
;; - S: session
;; - d: dedicated windows
;; - k: ace-window
;; - o: ace-window
;; - m: ace-window
;; - x: ace-window
;; - n: winner
;; - p: winner
;; - r: windresize
;; - s: window size operations

;; --
(when pel-use-ace-window
  (pel-ensure-package ace-window from: melpa)
  (pel-autoload-file ace-window for:
                     ace-window
                     ace-swap-window
                     ace-delete-window
                     ace-delete-other-windows
                     ace-window-display-mode)
  ;; move cursor to other window - 'C-x o' is normally mapped to
  ;; this function, but PEL remap it.
  (define-key pel:window  "o"  'pel-other-window)
  (define-key pel:window  "k"  'ace-delete-window)
  ;; old version of ace-window had ace-maximize-window
  ;; but newer version obsoleted that and now use ace-delete-other-windows
  ;; If you do not have it, upgrade ace-window.

  (define-key pel:mode-line "#" 'ace-window-display-mode)
  (define-key pel:window  "#"  'ace-window-display-mode)
  (define-key pel:window  "m"  'ace-delete-other-windows)
  (define-key pel:window  "x"  'ace-swap-window)

  ;; Replace other-window, bound to 'C-x o', to ace-window
  ;; and make the font larger - in graphics mode.
  ;; So `C-x o` shows a window number in top left corner, unless
  ;; there's only 2 windows and if with frames, in terminal mode,
  ;; the argument does not request it.
  (global-set-key [remap other-window] 'ace-window)

  (when pel-emacs-is-graphic-p
    (pel-eval-after-load ace-window
      (custom-set-faces
       '(aw-leading-char-face
         ((t (:inherit ace-jump-face-forward :height 3.0))))))))

;; --
;; TODO: change to use a hook before the function split-window is called and
;;       remove the hook once it is executed if that's possible.
;; The winner package should ideally be loaded just before the first
;; call to any of the window split function is called, which is: when the
;; `split-window' from window.el is called.  To do that, we'd need to use a
;; hook.  For now, we just defer the loading with a timer so it does not get
;; loaded right when Emacs is starting.
;; I tried using advice to solve the problem but it failed.
;; I need a better understanding of the advice mechanism.

(when pel-use-winner
  ;; winner is built-in Emacs.
  (pel-require-after-init winner 1)
  (pel-autoload-file winner for:
                     winner-mode
                     winner-undo
                     winner-redo)
  (define-key pel:window    "n"  'winner-redo) ; next window arrangement
  (define-key pel:window    "p"  'winner-undo) ; previous window arrangement
  (defvar winner-dont-bind-my-keys)            ; prevent byte-compiler warning
  (pel-eval-after-load winner
    ;; winner-mode default bindings use the Shift cursor keys,
    ;; this conflict with org-mode, so use the '<f11> w' bindings
    ;; instead.
    (setq winner-dont-bind-my-keys t)
    ;; turn on the global minor mode
    (winner-mode t)))

(when pel-use-windresize
  (pel-ensure-package windresize from: gnu)
  (pel-autoload-file windresize for: windresize)
  (define-key pel:window "r" 'windresize))

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; - Function Keys - <f11> - Prefix ``<f11> w d`` : Windows dedicated operations
;;
(define-pel-global-prefix pel:window-dedicated (kbd "<f11> w d"))
(define-key pel:window-dedicated "d" 'pel-toggle-window-dedicated)
(define-key pel:window-dedicated "?" 'pel-show-window-dedicated-status)

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; - Function Keys - <f11> - Prefix ``<f11> w s`` : Window size operations
;;

(defun pel--global-resize-window-on (prefix)
  "Bind window resize commands on PREFIX followed by cursor."
  (global-set-key (kbd (format "%s <kp-5>"    prefix)) #'balance-windows)
  (global-set-key (kbd (format "%s <M-up>"    prefix)) #'enlarge-window)
  (global-set-key (kbd (format "%s <M-down>"  prefix)) #'shrink-window)
  (global-set-key (kbd (format "%s <M-right>" prefix)) #'enlarge-window-horizontally)
  (global-set-key (kbd (format "%s <M-left>"  prefix)) #'shrink-window-horizontally))

(when pel-windmove-on-esc-cursor
  (pel--global-resize-window-on "ESC"))
(when pel-windmove-on-f1-cursor
  (pel--global-resize-window-on "<f1>"))

(define-pel-global-prefix pel:window-size (kbd "<f11> w s"))
(define-key pel:window-size "-" #'shrink-window-if-larger-than-buffer)
(define-key pel:window-size "=" #'balance-windows)
(define-key pel:window-size "V" #'enlarge-window)
(define-key pel:window-size "v" #'shrink-window)
(define-key pel:window-size "H" #'enlarge-window-horizontally)
(define-key pel:window-size "h" #'shrink-window-horizontally)

;; -----------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> S`` : Session operations
;;
;; desktop can be used alone or used with either desktop-registry or desktop+
;; The following code control the auto-loading of the 3 modules and creation of
;; key bindings for these 3 packages: the key bindings are set according to what
;; package is used and loaded.

(when pel-use-desktop
  (define-pel-global-prefix pel:session (kbd "<f11> S"))
  ;;
  (defun pel-desktop-show ()
    "Display name of currently used desktop if any."
    (interactive)
    (if (boundp 'desktop-dirname)
        (message "Last loaded desktop: %s" desktop-dirname)
      (user-error "No desktop is currently loaded.")))

  (pel-autoload-file desktop for:
                     desktop-save
                     desktop-read
                     desktop-save-mode
                     desktop-change-dir
                     desktop-revert
                     desktop-clear)
  (if (eq pel-use-desktop 'with-desktop+)
      (define-key pel:session "?" 'pel-desktop-show)
    (define-key pel:session (kbd "M-s") 'desktop-save-mode)
    (define-key pel:session "S"         'desktop-save)
    (define-key pel:session "L"         'desktop-read)
    (define-key pel:session "c"         'desktop-clear)
    (define-key pel:session "d"         'desktop-change-dir)
    (define-key pel:session "r"         'desktop-revert))
  ;;
  ;; When Emacs runs in Terminal (TTY) mode, desktop does not restore the
  ;; window layout, because desktop-restoring-frameset-p returns nil in
  ;; terminal mode.  One way to add the functionality would be to advice that
  ;; function or to explicitly restore the frameset data via a hook.
  ;; That's what we do.
  (defvar desktop-saved-frameset)       ; prevent byte-compiler warning
  (defvar desktop-restore-reuses-frames)
  (defvar desktop-restore-in-current-display)
  (when pel-emacs-is-a-tty-p
    (add-hook 'desktop-after-read-hook
              (lambda ()
                (frameset-restore
                 desktop-saved-frameset
                 :reuse-frames (eq desktop-restore-reuses-frames t)
                 :cleanup-frames (not (eq desktop-restore-reuses-frames 'keep))
                 :force-display desktop-restore-in-current-display
                 :force-onscreen nil))))

  (unless (eq pel-use-desktop 'with-desktop+)
    ;; desktop+ autoloaded logic advices of the desktop functions.
    ;; Since that autoloading might already be done if desktop+ is installed
    ;; these advices are already done even if the user does not want to use
    ;; desktop+ and they will prevent proper operation of desktop alone.
    ;; Remove these advices to allow proper access of the desktop.el
    ;; functions.
    ;; Compatible with feature+ version 0.1.1, package-version: 20170107.2132
    (when (fboundp 'desktop+--advice--desktop-save)
      (advice-remove 'desktop-save
                     #'desktop+--advice--desktop-save))
    (when (fboundp 'desktop+--advice--desktop-restore-frameset)
      (advice-remove 'desktop-restore-frameset
                     #'desktop+--advice--desktop-restore-frameset)))
  (cond
   ;; -- Using with-desktop-auto-save-mode
   ((eq pel-use-desktop 'with-desktop-automatic)
    (desktop-save-mode 1))
   ;; -- Using desktop-registry
   ((memq pel-use-desktop '(with-desktop-registry
                            with-desktop-registry-automatic))
    (when (eq pel-use-desktop 'with-desktop-registry-automatic)
      (desktop-save-mode 1))
    (define-pel-global-prefix pel:session-registry (kbd "<f11> S R"))
    (pel-ensure-package desktop-registry from: melpa)
    (pel-autoload-file desktop-registry for:
                       desktop-registry-change-desktop
                       desktop-registry-remove-desktop
                       desktop-registry-rename-desktop
                       desktop-registry-add-directory
                       desktop-registry-add-current-desktop
                       desktop-registry-list-desktops)
    (define-key pel:session-registry "l" 'desktop-registry-list-desktops)
    (define-key pel:session-registry "o" 'desktop-registry-change-desktop)
    (define-key pel:session-registry "d" 'desktop-registry-remove-desktop)
    (define-key pel:session-registry "R" 'desktop-registry-rename-desktop)
    (define-key pel:session-registry "a" 'desktop-registry-add-directory)
    (define-key pel:session-registry "A" 'desktop-registry-add-current-desktop))
   ;; -- Using desktop+
   ((eq pel-use-desktop 'with-desktop+)
    (pel-ensure-package desktop+ from: melpa)
    (pel-autoload-file desktop+ for:
                       desktop+-create
                       desktop+-load
                       desktop+-create-auto
                       desktop+-load-auto)
    (define-key pel:session "s" 'desktop+-create)
    (define-key pel:session "l" 'desktop+-load)
    (define-key pel:session "S" 'desktop+-create-auto)
    (define-key pel:session "L" 'desktop+-load-auto))))

;; -----------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> z`` : Process & shells execution
;;
(define-pel-global-prefix pel:execute (kbd "<f11> z"))
(define-pel-global-prefix pel:repl    (kbd "<f11> z r"))
;; Used keys:
;; ? a e f i j l p r s t v x
;; L
;; M-g M-l

(declare-function eshell "eshell")

(define-key pel:execute    "?" #'list-processes)
;; Terminals
(define-key pel:execute    "a" #'ansi-term)
(define-key pel:execute    "e" #'eshell)
(define-key pel:execute    "s" 'pel-shell)
(define-key pel:execute    "t" #'term)
(define-key pel:execute    "l" #'ielm)
;; support for the extremely fast/nice libvterm-based vterm shell.
(when pel-use-vterm
  (pel-ensure-package vterm from: melpa)
  (pel-autoload-file vterm for: vterm)
  (define-key pel:execute "v" 'vterm))

;; Programming Language REPL.  Key used is the same as their f11 SPC key.
(when pel-use-common-lisp  (define-key pel:repl  "L" #'pel-cl-repl))
(when pel-use-forth        (define-key pel:repl  "f"  'run-forth))
(when pel-use-haskell
  (declare-function run-haskell "inf-haskell")
  (define-key pel:repl  "h" #'run-haskell))
(when pel-use-julia        (define-key pel:repl  "j" #'julia-snail))

(when pel-use-python       (define-key pel:repl  "p" #'run-python))
(when pel-use-erlang       (define-key pel:repl  "e"  'erlang-shell))
(when (and pel-use-elixir
           pel-use-alchemist)
                           (define-key pel:repl  "x"  #'alchemist-iex-run))
(when (and pel-use-ocaml
           pel-use-tuareg)
  (declare-function run-ocaml "tuareg")
  (define-key pel:repl  "o"  #'run-ocaml))

(when pel-use-arc          (define-key pel:repl (kbd "C-a") #'run-arc))
(when pel-use-lfe          (define-key pel:repl (kbd "C-l")  'run-lfe))
;; - Scheme dialects
(when pel-use-chez         (define-key pel:repl (kbd "C-z") #'pel-chez-repl))
(when pel-use-chibi        (define-key pel:repl (kbd "C-i") #'pel-chibi-repl))
(when pel-use-chicken      (define-key pel:repl (kbd "C-k") #'pel-chicken-repl))
(when pel-use-gambit       (define-key pel:repl (kbd "C-b") #'pel-gambit-repl))
(when pel-use-gerbil       (define-key pel:repl (kbd "C-e") #'pel-gerbil-repl))
(when pel-use-guile        (define-key pel:repl (kbd "C-g") #'pel-guile-repl))
(when pel-use-mit-scheme   (define-key pel:repl (kbd "C-m") #'pel-mit-scheme-repl))
(when pel-use-racket       (define-key pel:repl (kbd "C-r") #'pel-racket-repl))
(when pel-use-scsh         (define-key pel:repl (kbd "C-h") #'pel-scsh-repl))

;; -----------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> X`` : Xref utilities
;;
(define-pel-global-prefix pel:xref          (kbd "<f11> X"))
(define-pel-global-prefix pel:xref-backend  (kbd "<f11> X B"))

(define-key pel:xref "?" #'pel-xref-show-status)
(define-key pel:help "X" #'pel-xref-show-status) ; pel:help key

(define-key pel:xref "a" #'xref-find-apropos)
(define-key pel:xref "c"  'pel-xref-find-custom-definition-at-line)
(define-key pel:xref "t"  'visit-tags-table)
(define-key pel:xref "s"  'tags-search)
(define-key pel:xref "n"  'tags-loop-continue)
(define-key pel:xref "l"  'list-tags)
(define-key pel:xref "f"  'next-file)
(define-key pel:xref "r"  'tags-query-replace)
(define-key pel:xref (kbd "M-r")  'xref-query-replace-in-results)
(define-key pel:xref "1"  'first-error)
(define-key pel:xref "F"  'pel-xref-select-front-end)

(define-key pel:xref-backend "T"  'xref-etags-mode)

;; See Customization section for the key binding of the function
;; `pel-xref-find-custom-definition-at-line' .

;; Installation of work around for Emacs bug 44494
;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=44494
;; TODO: qualify this with emacs version as soon as a version of Emacs fixes
;; the bug.
(add-hook 'xref-etags-mode-hook (function
                                 (lambda () (load "pel-etags" :no-error))))

;; ggtags
(when pel-use-ggtags
  (pel-ensure-package ggtags from: melpa)
  (pel-autoload-file ggtags for: ggtags-mode)

  ;; Provide a key to quickly enable or disable ggtags-mode.
  (define-key pel:xref-backend "G" 'ggtags-mode)

  ;; ggtags has its own key map to which we need to add a key binding.
  (defun pel--ggtags-setup ()
    "Activate PEL extra key bindings for ggtags."
    (when (boundp 'ggtags-mode-map)
      (let ((map ggtags-mode-map))
        (define-key map (kbd "C-c M-.") 'ggtags-find-tag-regexp))))

  ;; Activate PEL ggtags setup when ggatgs-mode starts on a buffer.
  (add-hook 'ggtags-mode-hook
            (function pel--ggtags-setup))

  ;; Activate ggtags mode automatically on modes requested by user customization
  (pel-add-hook-for
   'pel-modes-activating-ggtags
   (lambda ()
     (ggtags-mode 1))))

;; cscope
(when pel-use-xcscope
  (define-pel-global-prefix pel:cscope (kbd "<f11> X C"))
  (pel-ensure-package xcscope from: melpa)
  (pel-autoload-file xcscope for: cscope-minor-mode)
  (define-key pel:cscope "C" 'cscope-minor-mode)
  ;; schedule activation of cscope minor mode for selected ones
  (pel-add-hook-for 'pel-modes-activating-cscope
                    'cscope-minor-mode))
(when pel-use-helm-cscope
  (pel-ensure-package helm-cscope from: melpa)
  (pel-autoload-file helm-cscope for: helm-cscope-mode)
  (define-key pel:cscope "H" 'pel-toggle-helm-cscope)
  (add-hook 'helm-cscope-mode-hook 'pel-activate-helm-cscope)
  (pel-add-hook-for 'pel-modes-activating-helm-cscope
                    'pel-activate-helm-cscope))

;; dumb-jump
(when pel-use-dumb-jump
  (pel-ensure-package dumb-jump from: melpa)
  (pel-autoload-file dumb-jump for: pel-xref-toggle-dumb-jump-mode)
  ;; pel-xref-toggle-dumb-jump-mode sets up the xref-backend-functions
  ;; to use dumb-jump as the backend for xref, and use its key bindings.
  (define-key pel:xref-backend "D" 'pel-xref-toggle-dumb-jump-mode)
  ;; schedule activation for requested major modes.
  (pel-add-hook-for 'pel-modes-activating-dumb-jump
                    'pel-xref-dumb-jump-activate-locally))

;; gxref
(when pel-use-gxref
  (pel-ensure-package gxref from: melpa)
  (pel-autoload-file gxref for: xref-show-xrefs-function)
  (define-key pel:xref-backend "g" 'pel-xref-toggle-gxref)
  (pel-add-hook-for 'pel-modes-activating-gxref
                    'pel-xref-gxref-activate))

;; jtags
(when pel-use-jtags
  (pel-ensure-package jtags from: melpa))

;; rtags
(when pel-use-rtags-xref
  (pel-ensure-package rtags-xref from: melpa)
  (pel-autoload-file rtags-xref for: rtags-xref-enable)
  (define-key pel:xref-backend "R" 'pel-xref-toggle-rtags)
  (when (eq pel-use-rtags-xref 'use-from-start)
    (pel-xref-rtags-activate)))

;; ivy-xref
(when pel-use-ivy-xref
  (pel-ensure-package ivy-xref from: melpa)
  (pel-autoload-file ivy-xref for: ivy-xref-show-xrefs))

;; helm-xref
(when pel-use-helm-xref
  (pel-ensure-package helm-xref from: melpa)
  (if pel-emacs-27-or-later-p
      (pel-autoload-file helm-xref for:
                         helm-xref-show-xrefs-27
                         helm-xref-show-defs-27)
    (pel-autoload-file helm-xref for: helm-xref-show-xrefs)))

;; Activate xref front end 1 second after startup.
(run-with-idle-timer
 1 nil
 (function pel-xref-set-front-end) 'select-from-customization)


;; eopengrok
(when pel-use-eopengrok
  (pel-ensure-package eopengrok from: melpa)
  (pel-autoload-file eopengrok for:
                        eopengrok-mode
                        eopengrok-create-index
                        eopengrok-create-index-with-enable-projects
                        eopengrok-find-definition
                        eopengrok-find-file
                        eopengrok-find-reference
                        eopengrok-find-text
                        eopengrok-find-history
                        eopengrok-find-custom
                        eopengrok-resume)
  (define-pel-global-prefix pel:opengrok (kbd "<f11> X O"))
  (define-key pel:opengrok "i" 'eopengrok-create-index)
  (define-key pel:opengrok "I" 'eopengrok-create-index-with-enable-projects)
  (define-key pel:opengrok "d" 'eopengrok-find-definition)
  (define-key pel:opengrok "f" 'eopengrok-find-file)
  (define-key pel:opengrok "s" 'eopengrok-find-reference)
  (define-key pel:opengrok "t" 'eopengrok-find-text)
  (define-key pel:opengrok "h" 'eopengrok-find-history)
  (define-key pel:opengrok "c" 'eopengrok-find-custom)
  (define-key pel:opengrok "b" 'eopengrok-resume))

;; -----------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> _`` : Underlining commands

(pel-autoload-file pel-comment-adorn for:
                   pel-commented-adorn-1
                   pel-commented-adorn-2
                   pel-commented-adorn-3
                   pel-commented-adorn-4
                   pel-commented-adorn-5
                   pel-commented-adorn-6
                   pel-commented-adorn-7
                   pel-commented-adorn-8
                   pel-commented-adorn-9
                   pel-commented-adorn-10)
(define-pel-global-prefix pel:underline (kbd "<f11> _"))
(define-key pel:underline "1" 'pel-commented-adorn-1)
(define-key pel:underline "2" 'pel-commented-adorn-2)
(define-key pel:underline "3" 'pel-commented-adorn-3)
(define-key pel:underline "4" 'pel-commented-adorn-4)
(define-key pel:underline "5" 'pel-commented-adorn-5)
(define-key pel:underline "6" 'pel-commented-adorn-6)
(define-key pel:underline "7" 'pel-commented-adorn-7)
(define-key pel:underline "8" 'pel-commented-adorn-8)
(define-key pel:underline "9" 'pel-commented-adorn-9)
(define-key pel:underline "0" 'pel-commented-adorn-10)

;; -----------------------------------------------------------------------------
;; Key-Chord Mode
;; ==============

(define-pel-global-prefix pel:mode-key-chord (kbd "<f11> <f5> k"))

(when pel-use-key-chord
  (defun pel--start-key-chord-mode ()
    "Activate key-chord mode if it can be loaded."
    (when (require 'key-chord nil :noerror)
      (key-chord-mode 1)))
  (declare-function pel--start-key-chord-mode "pel_keys")

  (define-key pel:mode-key-chord  "?" 'pel-key-chord-describe)
  (define-key pel:keys (kbd "M-K")    'pel-key-chord-describe)

  ;; The key-seq is only activated once key-chord is activated.
  ;; Both must be active for key-seq to be used.  When both are
  ;; set PEL gives priority to key-seq.
  (when pel-use-key-seq
    (pel-ensure-package key-seq from: melpa))

  (pel-ensure-package key-chord from: melpa)
  (pel-autoload-file key-chord for: key-chord-mode)
  (define-key pel:mode-key-chord "k" 'key-chord-mode)
  (pel-eval-after-load key-chord
    (when (and (require 'pel-key-chord nil :noerror)
               (fboundp 'pel-activate-all-key-chords))
      (when pel-use-key-seq
        (require 'key-seq nil :noerror))
      (pel-activate-all-key-chords)))

  (when (eq pel-use-key-chord 'use-from-start)
    (run-with-idle-timer 1 nil (function pel--start-key-chord-mode))))

;; ---------------------------------------------------------------------------
;; Set Automatic Modes for specified file associations
;; ---------------------------------------------------
;;
;; Activate the list identified by the `pel-auto-mode-alist' user option
(dolist (pattern-mode pel-auto-mode-alist)
  (let ((pattern (car pattern-mode))
        (mode    (cadr pattern-mode)))
    (if (fboundp mode)
        (add-to-list 'auto-mode-alist (cons pattern mode))
      (display-warning
       'pel-mode-association
       (format "Cannot associate file pattern %S to mode %S: %s is not bound!
      Please modify pel-auto-mode-alist.
      To edit the user option, type:   <f11> <f2> o pel-auto-mode-alist RET"
               pattern mode mode)
       :error))))

;; ---------------------------------------------------------------------------
;; Activate global minor modes
;; ---------------------------
;;
;; Once initialization is completed, activate the requested global minor
;; modes.

(defun pel--cfg-global-minor-modes ()
  "Activate global minor modes."
  (pel-check-minor-modes-in           pel-activates-global-minor-modes)
  (pel-turn-on-global-minor-modes-in 'pel-activates-global-minor-modes))

(add-hook 'after-init-hook 'pel--cfg-global-minor-modes :append)

;; ---------------------------------------------------------------------------
;; PEL Hydras Control
;; ------------------

(when pel-use-hydra
  ;; PEL's code for Hydra control is located in the file pel__hydra.el.
  ;; The PEL Hydra code defines several Hydras, using the defhydra macro.
  ;; The defhydra macro is only available once the hydra package is installed.
  ;; Therefore, PEL's hydra code can't be byte-compiled until the hydra
  ;; package is installed.
  ;;
  ;; However, we want to be able to byte-compile all of PEL's code even when
  ;; some external packages are not installed.  After all the whole point of
  ;; PEL is to control installation of external package via customization and
  ;; ensure that we byte-compile everything to increase speed and catch as
  ;; many coding errors as possible.
  ;;
  ;; To resolve this problem, all PEL's hydra definition code that must be
  ;; executed during Emacs initialization is stored inside a separate file:
  ;; pel__hydra.el.  PEL's make file does not byte compile pel__hydra.el.  The
  ;; pel__hydra.el is byte compiled by the code here, when the user-option
  ;; `pel-use-hydra' is turned on, and after it installs and loads the hydra
  ;; package.
  ;;
  ;; This way, if the user does not want to use PEL's hydra facilities and
  ;; leaves `pel-use-hydra' turned off, the hydra package is not installed at
  ;; compilation nor at run time.  Later, if the user wants to use PEL's hydra
  ;; and turns `pel-use-hydra' on, then one of the following action will
  ;; install the hydra package, byte-compile PEL's hydra code and provide its
  ;; full functionality.  This will occur on one of the following actions:
  ;;
  ;; - PEL is built with make,
  ;; - Emacs is re-started,
  ;; - Within Emacs, the user issues a `pel-init' command.

  ;; The hydra external library is however not loaded immediately.
  ;; The pel__hydra file is also not loaded immediately.
  ;; Why?  To reduce Emacs initialization time.
  ;; How is it loaded then?  From PEL's perspective it is controlled by the
  ;; use of the ``<f7>`` key.  The ``<f7>`` key is the key prefix for all PEL
  ;; hydras.  But right after Emacs starts, PEL configures that key to execute
  ;; the special interactive function `pel--load-hydra' which loads
  ;; `pel__hydra' and unbinds itself.  The `pel__hydra' file loads the `hydra'
  ;; library and defines the PEL hydras which use the ``<f7>`` key as their
  ;; prefixes.
  ;;
  ;; This way the `hydra' library is loaded lazily: the first time the user
  ;; press a PEL hydra key or when another package requires the `hydra'
  ;; external library, whatever comes first.

  ;; --
  (defun pel--load-hydra (&optional dont-simulate)
    "Define the PEL Hydra key sequences.

This is available once: this function destroys itself!
The presence of this function indicates that the PEL Hydras
have not yet been loaded.

This call simulates a F7 prefix key unless DONT-SIMULATE is non-nil."
    (interactive)
    ;; remove the temporary global binding to f7
    (global-unset-key (kbd "<f7>"))
    (load-library "pel__hydra")
    (unless dont-simulate
      ;; simulate f7 again so the user sees what he expects
      (setq unread-command-events (listify-key-sequence (kbd "<f7>"))))
    ;; then get rid of this function.
    (fmakunbound 'pel--load-hydra))
  (declare-function pel--load-hydra "pel_keys")

  ;; Bind the Hydra activation key
  (global-set-key (kbd "<f7>") 'pel--load-hydra)

  ;; --
  ;; Install the hydra external package if it is not already installed.
  ;; Then provide some auto-loading.
  (pel-ensure-package hydra from: melpa)
  (pel-autoload-file hydra for: defhydra)

  ;; Byte-compile pel__hydra.el if needed
  ;; TODO: in fast startup mode the elpa packages are not activated until
  ;;       after processing of command line arguments, and is not done in batch mode.
  ;;       This unfortunately means that hydra is not available for batch-mode byte-compilation
  ;;       when PEL/Emacs is operating in fast startup mode.
  (unless (bound-and-true-p pel-running-in-fast-startup-p)
    (let* ((current-directory (file-name-directory load-file-name))
           (el-filename (expand-file-name "pel__hydra.el" current-directory)))
      (pel-byte-compile-if-needed el-filename))))

;; ---------------------------------------------------------------------------
(provide 'pel_keys)

;;; pel_keys.el ends here

; LocalWords:  EditorConfig

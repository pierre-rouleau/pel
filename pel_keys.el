;;; pel_keys.el --- PEL key binding definitions -*-lexical-binding: t-*-

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

;; This file contains *all* PEL key bindings.
;; This file is *only* loaded by pel-init, nothing else.
;; This way the time is only spent when pel-init is executed, not when
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
    (ace-link-setup-default)))

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

(defun pel--install-undo-in-dired ()
  "Ensure that `dired-undo' is available in Dired buffer."
  (when (boundp 'dired-mode-map)
    (define-key dired-mode-map (kbd "M-u")   'dired-undo)
    (define-key dired-mode-map (kbd "C-x u") 'dired-undo)
    (define-key dired-mode-map (kbd "C-/")   'dired-undo)))

(when pel-use-undo-tree
    (eval-after-load "dired"
      '(pel--install-undo-in-dired)))

;; Activate extra Dired-x features when requested.
(when pel-use-dired-x
  (add-hook 'dired-load-hook
            (lambda ()
              (load "dired-x" :noerror :nomessage))))

;; Open files with OS-registered applications from Dired
;; -----------------------------------------------------
(defvar dired-mode-map) ; forward declare - dired is loaded early in Emacs
(eval-after-load "dired"
  '(define-key dired-mode-map "z" 'pel-open-in-os-app))

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
    (cl-eval-when 'load
      (pel-install-github-file "emacsmirror/framemove/master" "framemove.el"))
    (pel-autoload-file framemove for:
                       fm-up-frame
                       fm-down-frame
                       fm-right-frame
                       fm-left-frame
                       fm-down-frame
                       fm-up-frame
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
;; is between the PC-keyaboard and the macOS keyboards.  The PC-keyboard has a
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

(global-set-key [kp-0] 'pel-0)
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

(if pel-emacs-is-graphic-p
    (global-set-key [kp-add] 'pel-kp-add)
  (global-set-key [kp-separator] 'pel-kp-add))

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
(global-set-key (kbd "<M-right>") #'forward-word)
(global-set-key (kbd "<M-left>")  #'backward-word)

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

;; Navigating through Sexp/blocks (not normally bound by Emacs)
(global-set-key (kbd "C-M-]") 'up-list)

;; ---------------------------------------------------------------------------
;; Conflict Checking
;; -----------------

;; - Flyspell and iedit
;;   -----------------
;;
;; flyspell binds the key identified by the user option
;; `flyspell-auto-correct-binding' to the command
;; `flyspell-auto-correct-previous-word' .
;; By default the key is (kbd "C-;").
;; There's 2 problems:
;; 1) that key is not available in terminal mode for such a useful command,
;; 2) iedit-mode default key is also (kbd "C-;") as identified by its user
;;    option `iedit-toggle-key-default'.  iedit-mode checks if something else
;;    is mapping it but flyspell is activated lazily per mode, so iedit check
;;    never detects the conflicting binding.  There's no easy automatic
;;    solution for this, aside from checking for the conflict here and
;;    changing one of the user options.  Remember that user option variables
;;    are only available when their relative feature (file) has been loaded,
;;    which means that the check must be done when flyspell-mode is activated.
;;    The code below perform the check when the modes are activated.

(defvar flyspell-auto-correct-binding)  ; prevent lint warnings

(defun pel--check-flyspell-iedit-conflict ()
  "Check for key binding conflict between flyspell and iedit.
Warn user if necessary."
  (when (and (boundp 'iedit-toggle-key-default)
             (boundp 'flyspell-auto-correct-binding)
             (string= (key-description iedit-toggle-key-default)
                      (key-description flyspell-auto-correct-binding)))
    (display-warning
     'pel-keys
     (format "Both iedit and flyspell bind functions to \"%s\"!\n\
To use this key, change the key selected in one of the following \n\
user options:\n\
- `iedit-toggle-key-default'
- `flyspell-auto-correct-binding'

Then save your changes."
             (key-description flyspell-auto-correct-binding)))))

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
;;  d f l n p L
;;  SPC
;;  C-i <backtab> C-n C-p
;;  <down> <up> <left> <right>
;;  <f12>

(define-pel-global-prefix pel:f6 (kbd "<f6>"))
(define-key pel:f6 "l"  'pel-insert-line)
(define-key pel:f6 "f"  'pel-insert-filename)

;; Move to the beginning of next function definition (while moving forward)
;;  complements C-M-e and C-M-a
(define-key pel:f6 "d"            'pel-duplicate-line)
(define-key pel:f6 "n"            'pel-beginning-of-next-defun)
(define-key pel:f6 (kbd "<down>") 'pel-beginning-of-next-defun)
(define-key pel:f6 "p"            'beginning-of-defun)
(define-key pel:f6 (kbd "<up>")   'beginning-of-defun)
(define-key pel:f6 (kbd "<left>") 'pel-end-of-previous-defun)
(define-key pel:f6 (kbd "<right>")'end-of-defun)
(define-key pel:f6 (kbd "C-n")    'pel-goto-next-url)
(define-key pel:f6 (kbd "C-p")    'pel-goto-previous-url)

;; (kbd "<tab>") does not work in terminal mode, it works only in graphics mode
(define-key pel:f6 (kbd "C-i")       'pel-indent-lines)
(define-key pel:f6 (kbd "<backtab>") 'pel-unindent-lines)
;;
;; Install the generic skeletons, 2 seconds after Emacs starts to reduce
;; Emacs init time.
(run-with-idle-timer 2 nil (function pel--install-generic-skel) pel:f6)

;; ---------------------------------------------------------------------------
;; - Function Keys - <f11>
;; -----------------------
;;
;; See the definition of pel: in the upper portion of this file.

;; --
;; - Function Keys - <f11> top-level prefix keys


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
(global-set-key  (kbd "ESC <kp-0>")       #'overwrite-mode)
(define-key pel: (kbd      "RET")         #'auto-fill-mode)
(define-key pel: (kbd      "<deletechar>") 'c-hungry-delete-forward)
(define-key pel: (kbd      "M-f")          'pel-forward-syntaxchange-start)
(define-key pel: (kbd      "<M-right>")    'pel-forward-syntaxchange-start)
(define-key pel: (kbd      "M-b")          'pel-backward-syntaxchange-start)
(define-key pel: (kbd      "<M-left>")     'pel-backward-syntaxchange-start)
(define-key pel: (kbd      "0")           #'hl-line-mode)
(define-key pel: (kbd      "M-=")          'pel-toggle-show-copy-cut-text)

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

;; -- delete trailing whitespace

(defun pel-toggle-delete-trailing-space-on-save (&optional globally)
  "Toggle deletion of trailing spaces on file save and display current state.
By default change behaviour for local buffer only.
When GLOBALLY argument is non-nil, change it for all buffers for the current
Emacs editing session (the change does not persist across Emacs sessions).
To modify the global state permanently modify the customized value of the
`pel-delete-trailing-whitespace' user option via the `pel-pkg-for-filemng'
group customize buffer."
  (interactive "P")
  (pel-toggle-and-show-user-option 'pel-delete-trailing-whitespace globally))

(defun pel--delete-trailing-whitespace (&optional start end)
  "Delete trailing whitespace if currently active."
  (when pel-delete-trailing-whitespace
    (delete-trailing-whitespace start end)))

(when pel-delete-trailing-whitespace
  ;; - Remove trailing whitespaces on file save
  (add-hook 'before-save-hook  'pel--delete-trailing-whitespace)
  (define-key pel: (kbd "M-W") 'pel-toggle-delete-trailing-space-on-save))

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
(when (or pel-use-iedit pel-use-lispy)
  (defun pel--add-keys-to-iedit-mode ()
    "Add keys that work in terminal mode to iedit-mode key maps."
    (when (boundp 'iedit-lib-keymap)
      (define-key iedit-lib-keymap (kbd "<f1> C-a") 'iedit-show/hide-context-lines)
      (define-key iedit-lib-keymap (kbd "<f1> C-o") 'iedit-show/hide-occurrence-lines))
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
  (pel-eval-after-load iedit-mode
    (pel--check-flyspell-iedit-conflict)
    (pel--add-keys-to-iedit-mode)))

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

;; - smart-dash
;; ------------
(when pel-use-smart-dash
  (pel-ensure-package smart-dash from: melpa)
  (pel-autoload-file smart-dash for: smart-dash-mode)
  (define-key pel: (kbd "M--") 'smart-dash-mode)
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
(eval-after-load 'imenu
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
  (cl-eval-when 'load
    (pel-install-github-file "emacsmirror/emacswiki.org/master"
                             "imenu+.el" "imenu%2B.el"))
  (pel-autoload-file imenu+ for:
                     imenup-add-defs-to-menubar)
  (when (fboundp 'imenup-add-defs-to-menubar)
    (add-hook 'prog-mode-hook 'imenup-add-defs-to-menubar)))

;; Although imenu-extra is available through MELPA, that package just provide
;; tools that may be used by other PEL code to incorporate symbol generated
;; by modes into imenu.  It has nothing to autoload.  So instead of using
;; use-package to control its download, the code downloads the file in
;; ~/.emacs.d/utils if requested by the user-option.
;; If you want to get a version of the file newer than the one you have, just
;; delete ~/.emacs.d/utils/imenu-extra.el and restart Emacs.
(when pel-use-imenu-extra
    (cl-eval-when 'load
      (pel-install-github-file "redguardtoo/imenu-extra/master"
                               "imenu-extra.el")))

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

(define-key pel:cfg "c" 'customize)
(define-key pel:cfg "g" 'customize-group)
(define-key pel:cfg "o" 'customize-option)
(define-key pel:cfg "B" 'customize-browse)
(define-key pel:cfg "b" 'pel-browse-group)
(define-key pel:cfg "p" 'pel-customize-pel-base-emacs-group)
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
(when (or pel-use-helm
          pel-use-helm-xref
          (and pel-use-xcscope pel-use-helm-cscope))
  (pel-ensure-package helm from: melpa)
  (pel-autoload-file helm for: helm-mode)
  (pel-eval-after-load helm
    (require 'helm-config)
    (defvar helm-map)                   ; prevent byte-compiler warning
    ;; <tab> or C-i are mapped to helm-select-action.  Use M-C-i to run
    ;; persistent action.
    (define-key helm-map (kbd "M-C-i") 'helm-execute-persistent-action)))

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
(when (or pel-use-ivy
          pel-use-ivy-xref)
  (defvar ivy-use-virtual-buffers)       ; prevent byte-compiler warning
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
    ;;
    (when (and pel-system-is-macos-p pel-use-counsel-osx-app)
      (pel-ensure-package counsel-osx-app from: melpa)
      (pel-autoload-file counsel-osx-app for: counsel-osx-app)
      (define-key pel: "A" 'counsel-osx-app))))

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

(when (or pel-use-projectile
          pel-use-projectile-speedbar)

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
  (define-key pel:projectile (kbd "<f8>") 'projectile-mode))

;; ---------------------------------------------------------------------------
;; Tempo skeleton - a powerful lisp-style templating system
;; Load pel-tempo when programming languages using it are used.
;; See the use of skeletons in the following sections.

(when (or pel-use-erlang
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
;; Global prefixes to specialized prefixes
;; =======================================
;;
;; All PEL specialized prefixes start with <f11> SPC followed by another
;; character. These characters are listed below.
;; Not all of these are implemented yet, but I'm documented the currently
;; reserved character.  This is for reference and planning.
;;
;; 1   - APL
;; A   - Ada
;; C   - C++
;; D   - D
;; E   - Elm
;; F   - FORTRAN
;; G   - Groovy
;; J   - Java            -              JVM
;; L   - Common Lisp     - Lisp Family
;; M   - Makefile
;; N   - NetRexx
;; P   - Perl
;; R   - REXX
;; S   - Scala           -              JVM
;; U   - Ruby
;; a   - AppleScript
;; c   - C
;; d   - Dart
;; e   - Erlang          -              BEAM language
;; f   - Forth
;; g   - Go
;; h   - Haskell
;; i   - Javascript
;; j   - Julia
;; k   - Kotlin          -              JVM
;; l   - Emacs Lisp      - Lisp Family
;; n   - Nim
;; o   - OCaml
;; p   - Python
;; r   - Rust
;; s   - Swift
;; t   - TCL
;; u   - Lua
;; v   - V
;; x   - Elixir          -              BEAM Language
;; z   - Zig
;; C-a - Arc             - Lisp Family
;; C-c - CMake
;; C-e - Eiffel
;; C-f - Fennel          - Lisp Family, Lua
;; C-g - Prolog
;; C-j - Clojure         - Lisp Family, JVM
;; C-g - Gerbil          - Lisp & Scheme Families
;; C-h - Hy              - Lisp Family, Python
;; C-l - LFE             - Lisp Family, BEAM language
;; C-m - ReasonML
;; C-o - Objective-C
;; C-p - Pike
;; C-r - Racket          - Lisp Family
;; C-s - Scheme          - Lisp Family
;; C-u - Raku
;; M-a - AsciiDoc
;; M-g - GraphViz Dot
;; M-m - Markdown
;; M-o - OrgMode
;; M-r - reStructuredText
;; M-s - SQL
;; M-u - PlantUML
;; M-D - Dired

;; ---------------------------------------------------------------------------
;; Syntax Check with Flycheck (if requested)
;; -----------------------------------------
(when (or (eq pel-use-erlang-syntax-check 'with-flycheck)
          (eq pel-use-goflymake 'with-flycheck))
  (pel-ensure-package flycheck from: melpa)
  (pel-autoload-file flycheck for:
                     flycheck-mode
                     flycheck-select-checker))

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

  ;; Support for nmake - additions to what is supported by make-mode
  (declare-function makefile-nmake-mode "pel-make")
  (add-to-list 'auto-mode-alist '("\\.mak\\'" . makefile-nmake-mode))

  (pel-setup-major-mode makefile pel:for-make
    (define-key pel:for-make (kbd "<up>")      'makefile-previous-dependency)
    (define-key pel:for-make (kbd "<down>")    'makefile-next-dependency)
    (define-key pel:for-make (kbd "<M-up>")   #'pel-make-previous-macro)
    (define-key pel:for-make (kbd "<M-down>") #'pel-make-next-macro)
    (define-key pel:for-make "."               'completion-at-point)))

;; - Tup Built Tool Support
;; ------------------------
(when pel-use-tup
  (cl-eval-when 'load
    (pel-install-github-file "pierre-rouleau/tup-mode/master" "tup-mode.el"))
  (pel-autoload-file tup-mode for: tup-mode)
  (pel-set-auto-mode tup-mode for:
                     "\\.tup\\'"
                     "Tupfile"
                     "tup.config")
  (pel-setup-major-mode tup :no-f12-keys))

;; - Nix Package Manager Support
;; -----------------------------
(when pel-use-nix-mode
  (pel-ensure-package nix-mode from: melpa)
  (add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-mode))
  (pel-setup-major-mode nix :no-f12-keys))

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
  (cl-eval-when 'load
    (pel-install-github-file "pierre-rouleau/apples-mode/master"
                             "apples-mode.el"))
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
  (cl-eval-when 'load
    (pel-install-github-file "pierre-rouleau/c-eldoc/master" "c-eldoc.el"))

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

  (defun pel--map-cc-for (prefix &optional c-preproc-prefix)
    "Map in the PEL keys for CC Mode in the global keymap specified by PREFIX.
If C-PREPROC-PREFIX also bind the keys for C preprocessor related
commands and sub-keys inside that prefix.  If a key must be
assigned to something different for the programming language just
bind it again after this call."
    ;; electric mode control
    (define-key prefix (kbd "M-?")   'pel-cc-mode-info)
    (define-key prefix (kbd "M-s")   'c-set-style) ; interactively select style
    (define-key prefix (kbd "M-;")   'c-toggle-comment-style)
    (define-key prefix (kbd "M-e")   'c-toggle-electric-state)
    (define-key prefix (kbd "RET")   'pel-cc-change-newline-mode)
    (define-key prefix (kbd "M-RET") 'c-toggle-auto-newline)
    (define-key prefix (kbd "M-DEL") 'c-toggle-hungry-state)
    (define-key prefix (kbd "M-b")  #'subword-mode)
    (define-key prefix (kbd "M-p")  #'superword-mode)
    (define-key prefix (kbd "M-i")   'c-toggle-syntactic-indentation)
    (define-key prefix (kbd "C-o")   'open-line)
    (define-key prefix      "F"      'c-fill-paragraph)
    (define-key prefix      "f"      'c-display-defun-name)
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
      (define-key c-preproc-prefix "?" 'pel-pp-show-state)))

  (defun pel--setup-for-cc ()
    "More setup for CC modes: add c preprocessor hydra."
    ;; The pel-⅀c-preproc requires Hydra: load it via the `pel--load-hydra'.
    ;; Note that `pel--load-hydra' removes itself and its presence is used as an
    ;; indication.  So we must check for it being bound and we must NOT use a
    ;; `declare-function' for it even if it was conditionally defined (which
    ;; currently is not the case anyway).
    (when (and pel-use-hydra
               (fboundp 'pel--load-hydra))
      (pel--load-hydra :no-request)))

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
                       :error))))

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; - Function Keys - <f11> - Prefix ``<f11> SPC c`` : C programming utilities

;; Note: C editing is always available in Emacs via the CC Mode and the c-mode
;; that is part of Emacs.  All autoloading is already set by Emacs.  The only
;; extra code needed is to add the specialized menu and then activate it,
;; along with the specialized CC Mode minor modes via the c-mode-hook.

(when pel-use-c
  (define-pel-global-prefix pel:for-c         (kbd "<f11> SPC c"))
  (define-pel-global-prefix pel:for-c-preproc (kbd "<f11> SPC c #"))
  (define-pel-global-prefix pel:c-skel        (kbd "<f11> SPC c <f12>"))

  (when pel-use-plantuml
    (define-key pel:for-c "u" 'pel-render-commented-plantuml))
  (when pel-use-c-eldoc
    (define-pel-global-prefix pel:c-help (kbd "<f11> SPC c ?"))
    (define-key pel:c-help "e" 'pel-toggle-c-eldoc-mode))
  (pel--map-cc-for pel:for-c pel:for-c-preproc)

  (pel-setup-major-mode c pel:for-c
    ;; Configure the CC Mode style for C from PEL custom variables
    ;; 1) set the style: it identifies everything
    (pel--set-cc-style 'c-mode pel-c-bracket-style pel-c-newline-mode)
    ;; 2) apply modifications requested by PEL user options.
    ;; 2a) set variables always available in Emacs
    (setq tab-width          pel-c-tab-width
          indent-tabs-mode   pel-c-use-tabs)
    ;; 2b) set variables only available in a CC mode - prevent warnings
    (pel-setq c-basic-offset pel-c-indent-width)
    ;; 3) set fill-column to PEL specified C's default if specified
    (when pel-c-fill-column
      (setq fill-column pel-c-fill-column))
    ;; 4) Set default auto-newline mode as identified by PEL user option
    (c-toggle-auto-newline (pel-mode-toggle-arg pel-cc-auto-newline))
    ;; 5) Configure M-( to put parentheses after a function name.
    (set (make-local-variable 'parens-require-spaces) nil)
    ;; 6) activate mode specific sub-key prefixes in <f12> and <M-f12>
    (pel-local-set-f12-M-f12 'pel:for-c-preproc "#")
    ;; 7) Install language-specific skeletons
    (pel--install-c-skel      pel:c-skel)
    ;; 8) extra setup
    (pel--setup-for-cc)))

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; - Function Keys - <f11> - Prefix ``<f11> SPC C`` : C++ programming utilities

;; Note: C++ editing is always available in Emacs via the CC Mode and the
;; c++-mode that is part of Emacs.  All autoloading is already set by Emacs.
;; The only extra code needed is to add the specialized menu and then activate
;; it, along with the specialized CC Mode minor modes via the c++-mode-hook.

(when pel-use-c++
  (define-pel-global-prefix pel:for-c++         (kbd "<f11> SPC C"))
  (define-pel-global-prefix pel:for-c++-preproc (kbd "<f11> SPC C #"))

  (when pel-use-plantuml
    (define-key pel:for-c++ "u" 'pel-render-commented-plantuml))
  (pel--map-cc-for pel:for-c++ pel:for-c++-preproc)

  (pel-setup-major-mode c++ pel:for-c++
    ;; "Set the environment for editing C++ files."
    ;; Configure the CC Mode style for C++ from PEL custom variables
    ;; 1) set the style: it identifies everything
    (pel--set-cc-style 'c++-mode pel-c++-bracket-style pel-c++-newline-mode)
    ;; 2)  apply modifications requested by PEL user options.
    ;; 2a) set variables always available in Emacs
    (setq tab-width          pel-c++-tab-width
          indent-tabs-mode   pel-c++-use-tabs)
    ;; 2b) set variables only available in a CC mode - prevent warnings
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
    ;; TODO
    ;; 8) extra setup
    (pel--setup-for-cc)))

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; - Function Keys - <f11> - Prefix ``<f11> SPC D`` : D programming utilities

(when pel-use-d
  (define-pel-global-prefix pel:for-d (kbd "<f11> SPC D"))

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
  (pel--map-cc-for pel:for-d)

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

  (pel-setup-major-mode d pel:for-d
    ;; "Set the environment for editing D files."
    ;; Configure the CC Mode style for C++ from PEL custom variables
    ;; 1) set the style: it identifies everything
    (pel--set-cc-style 'd-mode pel-d-bracket-style pel-d-newline-mode)
    ;; 2)  apply modifications requested by PEL user options.
    ;; 2a) set variables always available in Emacs
    (setq tab-width          pel-d-tab-width
          indent-tabs-mode   pel-d-use-tabs)
    ;; 2b) set variables only available in a CC mode - prevent warnings
    (pel-setq c-basic-offset pel-d-indent-width)
    ;; 3) set fill-column to PEL specified D's default if specified
    (when pel-d-fill-column
      (setq fill-column pel-d-fill-column))
    ;; 4) Set default auto-newline mode as identified by PEL user option
    (c-toggle-auto-newline (pel-mode-toggle-arg pel-cc-auto-newline))
    ;; Configure M-( to put parentheses after a function name.
    (set (make-local-variable 'parens-require-spaces) nil)
    ;; 7) Install language-specific skeletons
    ;; TODO
    ))

;; ---------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> SPC e`` : Erlang programming

(when pel-use-erlang
  (define-pel-global-prefix pel:for-erlang (kbd "<f11> SPC e"))
  (define-pel-global-prefix pel:erlang-function (kbd "<f11> SPC e f"))
  (define-pel-global-prefix pel:erlang-clause   (kbd "<f11> SPC e c"))
  (define-pel-global-prefix pel:erlang-analysis (kbd "<f11> SPC e a"))
  (define-pel-global-prefix pel:erlang-debug    (kbd "<f11> SPC e d"))
  (define-pel-global-prefix pel:erlang-skel     (kbd "<f11> SPC e <f12>"))

  (pel-ensure-package erlang from: melpa)
  (pel-autoload-file erlang for: erlang-mode)

  (pel-set-auto-mode erlang-mode for:
                     "\\.erl?$"
                     "\\.hrl?$"
                     "rebar\\.config$"
                     "relx\\.config$"
                     "sys\\.config\\.src$"
                     "sys\\.config$"
                     "\\.config\\.src?$"
                     "\\.config\\.script?$"
                     "\\.app?$"
                     "\\.app.src?$"
                     "\\Emakefile")

  ;; Overcome omission bug in erlang-mode: add support for Speedbar
  (when pel-use-speedbar
    (pel-add-speedbar-extension '(".erl"
                                  ".hrl"
                                  ".escript")))

  (when pel-erlang-shell-prevent-echo
    ;; Prevent erlang shell to echo back commands.
    (add-hook 'erlang-shell-mode-hook 'pel-erlang-shell-mode-init))


  ;; Augment the skeletons defined inside erlang.el.
  ;; Do this once - right after erlang.el file is loaded and
  ;; before the erlang-mode executes.
  (advice-add 'erlang-mode :before #'pel--erlang-mode-setup)

  ;; bind other erlang keys
  (define-key pel:for-erlang      "?"         'erlang-version)
  (define-key pel:erlang-function "N"         'pel-beginning-of-next-defun)
  (define-key pel:erlang-function "P"         'beginning-of-defun)
  (define-key pel:erlang-function "n"         'pel-next-erl-function)
  (define-key pel:for-erlang (kbd "<down>")   'pel-next-erl-function)
  (define-key pel:erlang-function "p"         'pel-previous-erl-function)
  (define-key pel:for-erlang (kbd "<up>")     'pel-previous-erl-function)
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
  (define-key pel:for-erlang (kbd "M-p")      #'superword-mode)
  (define-key pel:for-erlang (kbd "M-9")      #'show-paren-mode)
  (define-key pel:for-erlang (kbd "M-c")      'erlang-compile)
  (when pel-use-rainbow-delimiters
    (define-key pel:for-erlang (kbd "M-r")    'rainbow-delimiters-mode))
  (when pel-use-plantuml
    (define-key pel:for-erlang "u"     'pel-render-commented-plantuml))

  (pel-eval-after-load erlang
    ;; TODO: do we want to set erlang-root-dir, which is a user-option?
    (if (boundp 'erlang-root-dir)
        (unless erlang-root-dir
          (setq erlang-root-dir (expand-file-name pel-erlang-rootdir)))
      (display-warning 'pel-use-erlang
                       "erlang-root-dir is void, can't set it!"
                       :error))
    (when (file-exists-p pel-erlang-exec-path)
      (add-to-list 'exec-path pel-erlang-exec-path) )
    ;;
    (require 'erlang-start)
    (when pel-use-edts
      (pel-ensure-package edts from: melpa)
      (pel-autoload-file edts for: edts-mode)

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
            (display-warning
             'pel-use-edts
             "edts-mode is void.  Is it installed?"
             :error))))

      ;; Key to start EDTS
      (define-key pel:for-erlang      (kbd "M-SPC")   'edts-mode)
      (when pel-activate-edts-automatically
        (require 'edts-start))
      (pel-eval-after-load edts
        (add-to-list 'desktop-minor-mode-handlers
                     '(edts-mode . edts-mode-desktop-restore))
        (unless pel-activate-edts-automatically
          (require 'edts-start))
        ;; EDTS keys
        ;; The following do not seem to do anything special in Erlang.
        ;; (define-key pel:for-erlang      ">"     'ahs-forward-definition)
        ;; (define-key pel:for-erlang      "<"     'ahs-backward-definition)
        ;;  edts cross reference command keys
        (define-key pel:for-erlang "w" 'edts-xref-who-calls)
        (define-key pel:for-erlang "W" 'edts-xref-last-who-calls)
        ;;  edts cross reference
        (define-key pel:for-erlang (kbd "M-f") 'edts-find-local-function)
        (define-key pel:for-erlang (kbd "M-g") 'edts-find-global-function)
        ;; edts refactoring
        (define-key pel:for-erlang "r" 'edts-refactor-extract-function)
        ;; edts man page use
        (define-key pel:for-erlang "`" 'edts-man-setup)
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
        (define-key pel:for-erlang "N" 'edts-buffer-node-name)
        (define-key pel:for-erlang "x" 'edts-shell)
        (define-key pel:for-erlang "X" 'edts-api-start-server)
        ;; EDTS/(automatic highlight symbol)  features
        (define-key pel:for-erlang "e" 'edts-ahs-edit-current-function)
        (define-key pel:for-erlang "E" 'edts-ahs-edit-buffer)
        (define-key pel:for-erlang "n" 'ahs-forward)
        (define-key pel:for-erlang "p" 'ahs-backward)
        (define-key pel:for-erlang "." 'ahs-back-to-start)))

    (when pel-use-erlang-syntax-check
      (cond
       ;; when using flymake with Erlang
       ((eq pel-use-erlang-syntax-check 'with-flymake)
        (pel-require 'erlang-flymake :install-when-missing)
        ;; The erlang-flymake.el code hooks flymake-mode to erlang-mode
        ;; forcing flymake-mode on all Erlang files. Leave it if erlang-mode
        ;; was identified as a mode to automatically activate syntax checking,
        ;; otherwise remove the hook.
        (unless (memq 'erlang-mode pel-modes-activating-syntax-check)
          (remove-hook 'erlang-mode-hook #'flymake-mode))
        (when (boundp 'flymake-mode-map)
          (define-key flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)
          (define-key flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error)))

       ;; when using flycheck with Erlang
       ((eq pel-use-erlang-syntax-check 'with-flycheck)

        (defun pel--erlang-setup-for-flycheck ()
          "Setup flycheck."
          ;; Note that both flycheck-select-checker and flycheck-mode
          ;; are autoloaded.  See section: 'Syntax Check with Flycheck'
          (flycheck-select-checker 'erlang-otp)
          (flycheck-mode))
        (declare-function pel--erlang-setup-for-flycheck "pel_keys")

        ;; (flycheck-define-checker erlang-otp
        ;;   "An Erlang syntax checker using the Erlang interpreter."
        ;;   :command ("erlc" "-o" temporary-directory "-Wall"
        ;;             "-I" "../include" "-I" "../../include"
        ;;             "-I" "../../../include" source)
        ;;   :error-patterns
        ;;   ((warning line-start (file-name) ":" line ": Warning:"
        ;;             (message) line-end)
        ;;    (error line-start (file-name) ":" line ": " (message) line-end)))

        (when (memq 'erlang-mode pel-modes-activating-syntax-check)
          (add-hook 'erlang-mode-hook #'pel--erlang-setup-for-flycheck))))

      ;; When any syntax checker is used with Erlang add a key to toggle it
      (define-key pel:for-erlang "!" 'pel-erlang-toggle-syntax-checker)))

  (pel-setup-major-mode erlang pel:for-erlang
    ;; "Activate Erlang setup."
    ;; set fill-column to Erlang's default if specified
    (when pel-erlang-fill-column
      (setq fill-column pel-erlang-fill-column))
    ;; setup the Erlang-specific key bindings
    (pel--install-erlang-skel pel:erlang-skel)
    ;; Configure M-( to put parentheses after a function name.
    (set (make-local-variable 'parens-require-spaces) nil)))

;; ---------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> SPC x`` : Elixir programming
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
                       alchemist-iex-run))
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
  (pel-setup-major-mode elixir pel:for-elixir))

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

  ;; Activate Forth setup.
  (pel-setup-major-mode forth pel:for-forth))

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
    (define-key pel:for-go (kbd "M-t") 'pel-go-set-tab-width)
    (define-key pel:for-go (kbd "M-s") 'pel-go-toggle-gofmt-on-buffer-save)
    (define-key pel:for-go "?"         'pel-go-setup-info)
    (when pel-use-goflymake
      (define-key pel:for-go "!"       'pel-go-toggle-syntax-checker))

    ;; Set environment for Go programming using go-mode.
    (pel-setup-major-mode go pel:for-go
      ;; ensure gofmt is executed before saving file if
      ;; configured to do so
      (when pel-go-run-gofmt-on-buffer-save
        (add-hook 'before-save-hook  'pel-go-gofmt-on-buffer-save))
      ;; Set the display width of hard tabs used in Go source
      ;; as controlled by the user-option
      (setq tab-width pel-go-tab-width)
      (when pel-use-goflymake
        ;; Activate flycheck or flymake if requested
        (cond
         ((eq pel-use-goflymake 'with-flycheck) (pel-require 'go-flycheck))
         ((eq pel-use-goflymake 'with-flymake)  (pel-require 'go-flymake))
         (t
          (error "Unsupported pel-use-goflymake value: %S"
                 pel-use-goflymake)))))))

;; ---------------------------------------------------------------------------
;; - Programming Style: Haskell Support
;; ------------------------------------
;;
;; Using Intero to support Haskell programming language.
;; Installed it via the list-packages.
;; ; (add-hook 'haskell-mode-hook 'intero-mode)

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
    (pel-setup-major-mode javascript pel:for-javascript))
   ;;
   ((eq pel-use-javascript 'js-mode)
    ;; Use the built-in js.el
    (pel-autoload-file js for: js-mode)
    (pel-setup-major-mode js pel:for-javascript))))

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

  (pel-setup-major-mode julia pel:for-julia
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

  (defun pel--update-lispy-keymap ()
    "Update lispy key-map according to PEL user-options."
    (if (boundp 'lispy-mode-map)
        (unless pel-enable-lispy-meta-return
          (define-key lispy-mode-map (kbd "M-RET") nil))
      (display-warning
       'pel-lispy
       "The lispy-mode-map is not bound.
  Cannot disable lispy-meta-return binding to M-RET!"
       :error)))
  (declare-function pel--update-lispy-keymap "pel_keys")

  ;; Setup activation of Lispy for specified major modes that are allowed.
  (pel-add-hook-for 'pel-modes-activating-lispy
                    #'pel--activate-lispy
                    pel-allowed-modes-for-lispy)

  ;; Control some keys in the Lispy keyboard map.
  (eval-after-load 'lispy
    '(pel--update-lispy-keymap))

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

(define-key pel:for-elisp "R"  'ielm)
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

;;
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
   (pel-turn-on-minor-modes-in pel-elisp-activates-minor-modes))
 'emacs-lisp-mode 'emacs-lisp-mode-hook :append)

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

  (pel-setup-major-mode lisp pel:for-lisp
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
  ;; Emacs support extracted from the anarki implementation from its
  ;; Github project folder.  However, since the code there is old and
  ;; has bugs, I used my fork until my fixes have been incorporated.
  ;; See: https://github.com/arclanguage/anarki/pull/194
  (cl-eval-when 'load
    (pel-install-github-files "pierre-rouleau/anarki/master/extras"
                              '("arc.el"
                                "inferior-arc.el")))

  ;; associate .arc file with arc-mode
  (add-to-list 'auto-mode-alist '("\\.arc\\'" . arc-mode))

  (define-pel-global-prefix pel:for-arc (kbd "<f11> SPC C-a"))
  (pel--lisp-languages-map-for pel:for-arc)

  ;; activate the <f12> key binding for arc-mode
  (pel-setup-major-mode arc pel:for-arc))

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
  (pel-setup-major-mode clojure pel:for-clojure
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
;; - Function Keys - <f11> - Prefix ``<f11> SPC C-r`` : Racket
(when pel-use-racket
  (pel-ensure-package racket-mode from: melpa)
  (pel-autoload-file racket-mode for: racket-mode)

  (define-pel-global-prefix pel:for-racket (kbd "<f11> SPC C-r"))
  (pel--lisp-languages-map-for pel:for-racket)

  ;; activate the <f12> key binding for racket-mode
  (pel-setup-major-mode racket pel:for-racket))

;; ---------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> SPC C-s`` : Scheme
(when pel-use-scheme
  ;; Note: scheme-mode and its file associations are supported by Emacs.

  ;; Install requested options
  (when pel-use-geiser
    (pel-ensure-package geiser from: melpa)
    (pel-autoload-file geiser for:
                       geiser
                       geiser-mode))

  (when pel-use-quack
    ;; I have fixed byte-compiler warnings in quack in a fork of emacsmirror/quack
    ;; Since that repo is read-only I contacted the author and wait for his reply.
    ;; In the mean time, I use my fork.
    (cl-eval-when 'load
      (pel-install-github-file "pierre-rouleau/quack/master" "quack.el"))
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
  (define-pel-global-prefix pel:for-scheme (kbd "<f11> SPC C-s"))
  (pel--lisp-languages-map-for pel:for-scheme)

  ;; activate the <f12> key binding for scheme-mode
  (pel-setup-major-mode scheme pel:for-scheme))

;; ---------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> SPC C-g`` : Gerbil
(when pel-use-gerbil
  ;; No package made for this.  Take the code directly from Github
  (cl-eval-when 'load
    (pel-install-github-file "vyzo/gerbil/master/etc/" "gerbil-mode.el"))
  (pel-autoload-file gerbil-mode for: gerbil-mode)

  (define-pel-global-prefix pel:for-gerbil (kbd "<f11> SPC C-g"))
  (pel--lisp-languages-map-for pel:for-gerbil)

  ;; activate the <f12> key binding for gerbil-mode
  (pel-setup-major-mode gerbil pel:for-gerbil))

;; ---------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> SPC C-l `` : LFE programming
;; LFE := Lisp Flavoured Erlang

;; Note: the pel:execute has the run-lfe (in the code below.)
(when pel-use-lfe
  (pel-ensure-package lfe-mode from: melpa)
  (pel-autoload-file lfe-mode for:
                     lfe-mode
                     inferior-lfe
                     run-lfe)
  (when pel-use-speedbar
    (pel-add-speedbar-extension '(".lfe"
                                  ".lfes"
                                  ".lfesh")))

  (define-pel-global-prefix pel:for-lfe (kbd "<f11> SPC C-l"))
  (pel--lisp-languages-map-for pel:for-lfe)
  (define-key pel:for-lfe "[" 'lfe-insert-brackets)
  (define-key pel:for-lfe "z" 'run-lfe)
  (define-key pel:for-lfe (kbd "M-c") 'pel-lfe-eval-buffer)

  ;; Activate LFE setup.
  (pel-setup-major-mode lfe pel:for-lfe
    (when pel-emacs-is-a-tty-p
      (if (boundp 'lfe-mode-map)
          (define-key lfe-mode-map (kbd "M-[") nil)
        (display-warning 'pel-lfe
                         "The lfe-mode-map is not bound.
 Cannot disable the problematic M-[ key.
 Function keys starting with F5 will no work!"
                         :error)))))

;; ---------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> SPC C-h`` : Hy
;; Hy: A Lisp in Python
(when pel-use-hy
  (pel-ensure-package hy-mode from: melpa)
  (pel-autoload-file hy-mode for: hy-mode)

  (define-pel-global-prefix pel:for-hy (kbd "<f11> SPC C-h"))
  (pel--lisp-languages-map-for pel:for-hy)

  ;; activate the <f12> key binding for hy-mode
  (pel-setup-major-mode hy pel:for-hy))

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
  (pel-setup-major-mode python pel:for-python
    (setq tab-width pel-python-tab-width)))

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
  (cl-eval-when 'load
    (pel-install-github-files "pierre-rouleau/rexx-mode/master"
                              '("rexx-mode.el"
                                "rexx-debug.el")))

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
  (pel-setup-major-mode rexx pel:for-rexx))

;; ---------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> SPC N`` : NetRexx programming
(when pel-use-netrexx
  ;; Download netrexx.el directly from GitHub as there is no official support
  ;; by either GNU Elpa or MELPA
  (cl-eval-when 'load
    (pel-install-github-file "pierre-rouleau/netrexx-mode/master"
                             "netrexx-mode.el"))

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
  (pel-setup-major-mode netrexx pel:for-netrexx))

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
        (add-hook 'racer-mode-hook 'company-mode)))
    )

  (define-pel-global-prefix pel:for-rust (kbd "<f11> SPC r"))
  (define-key pel:for-rust "c" 'rust-run)
  (define-key pel:for-rust "d" 'rust-dbg-wrap-or-unwrap)
  (define-key pel:for-rust "l" 'rust-run-clippy)

  (pel-setup-major-mode rust pel:for-rust
    (setq indent-tabs-mode nil)
    (when pel-use-cargo
      (if (boundp 'rust-mode-map)
          (define-key rust-mode-map
            (kbd "TAB") 'company-indent-or-complete-common)
        (display-warning 'pel-use-rust
                         "Unbound rust-mode-map!"
                         :error)))))

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
    (pel-setup-major-mode v pel:for-v))

   ((eq pel-use-v 'vlang-mode)
    ;; vlang-mode is experimental: only provides font-locking
    ;; use, not on MELPA: download directly from github.
    (cl-eval-when 'load
      (pel-install-github-file "pierre-rouleau/vlang-mode/master"
                               "vlang-mode.el"))
    (pel-autoload-file vlang-mode for: vlang-mode))))

;; ---------------------------------------------------------------------------
;; Markup Language Support
;; --=====================

;; AsciiDoc support
;; ----------------

(when pel-use-asciidoc
  (pel-ensure-package adoc-mode from: melpa)
  (pel-autoload-file adoc-mode for: adoc-mode)
  (pel-set-auto-mode adoc-mode for: "\\.adoc\\'")

  (pel-setup-major-mode adoc :no-f12-keys))

;; ---------------------------------------------------------------------------
;; Org-Mode Support
;; ----------------

(when pel-use-org-mode
  (define-pel-global-prefix pel:for-org-mode (kbd "<f11> SPC M-o"))

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
  ;; Activate specialized C-a and C-e in Org-Mode.
  (pel-setq org-special-ctrl-a/e t)
  ;; Activate timestamp log for DONE tasks
  (pel-setq org-log-done 'time)
  ;; Add the "IN-PROGRESS" in the list of TODO states
  (pel-setq org-todo-keywords
            (quote ((sequence "TODO" "IN-PROGRESS" "DONE"))))

  (pel-setup-major-mode org pel:for-org-mode
    ;; Use the cleaner outline view mode.
    (if (fboundp 'org-indent-mode)
        (org-indent-mode 1)
      (display-warning 'pel-use-org-mode
                       "Unbound org-indent-mode"
                       :error))
    (when (and pel-use-imenu+
               (fboundp 'imenup-add-defs-to-menubar))
      (imenup-add-defs-to-menubar))))

;; ---------------------------------------------------------------------------
;; YAML Support
;; ------------

(when pel-use-yaml-mode
  (pel-ensure-package yaml-mode from: melpa)
  (pel-autoload-file yaml-mode for: yaml-mode)
  (pel-set-auto-mode yaml-mode for: "\\.yml\\'")

  (pel-setup-major-mode yaml :no-f12-keys))

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

  (pel-setup-major-mode markdown pel:for-markdown
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
    (pel-add-speedbar-extension '(".rst" ".stxt")))

  (define-pel-global-prefix pel:for-reST (kbd "<f11> SPC M-r"))
  (define-pel-global-prefix pel:rst-skel (kbd "<f11> SPC M-r <f12>"))

  ;; Add .stxt to the accepted file extensions for rst-mode
  ;; to the ones that are normally used: .rst and .rest
  (add-to-list 'auto-mode-alist '("\\.stxt\\'"  . rst-mode))

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
  ;;
  (when pel-use-plantuml
    (define-key pel:for-reST  "u" 'pel-render-commented-plantuml))
  ;;
  (define-pel-global-prefix pel:rst-adorn-style (kbd "<f11> SPC M-r A"))
  (define-key pel:rst-adorn-style "d" 'pel-rst-adorn-default)
  (define-key pel:rst-adorn-style "S" 'pel-rst-adorn-Sphinx-Python)
  (define-key pel:rst-adorn-style "C" 'pel-rst-adorn-CRiSPer)

  (pel-setup-major-mode rst pel:for-reST
    (setq tab-width    pel-rst-tab-width)
    (pel--install-rst-skel pel:rst-skel)
    (when (and pel-use-imenu+
               (fboundp 'imenup-add-defs-to-menubar))
      (imenup-add-defs-to-menubar))))

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

  (pel-setup-major-mode graphviz-dot pel:for-graphviz-dot))

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


  (pel-setup-major-mode plantuml pel:for-plantuml
    ;; Configure plantuml default execution mode according to PEL's selection.
    (if (boundp 'plantuml-default-exec-mode)
        (setq plantuml-default-exec-mode (if (eq pel-use-plantuml 'server)
                                             'server
                                           'jar))
      (display-warning 'pel-use-plantuml
                       "Unbound plantuml-default-exec-mode!"
                       :error))
    (with-eval-after-load 'flycheck
      (require 'flycheck-plantuml)
      (declare-function flycheck-plantuml-setup "flycheck-plantuml")
      (flycheck-plantuml-setup))))

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
(define-key pel:copy "r" #'copy-rectangle-as-kill)
(define-key pel:copy "s"  'pel-copy-sentence-at-point)
(define-key pel:copy "u"  'pel-copy-url-at-point)
(define-key pel:copy "w"  'pel-copy-word-at-point)
(define-key pel:copy "x"  'pel-copy-sexp-at-point)
;;
(global-set-key (kbd "<f11> +") 'pel-copy-marked-or-whole-line)
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
(define-key pel:comment "d"            'pel-toggle-all-docstrings)
(define-key pel:comment "D"            'pel-hide/show-all-docstrings)
(define-key pel:comment "'"            'pel-toggle-docstring)
(define-key pel:comment "\""           'pel-hide/show-docstring)
(define-key pel:comment "?"            'pel-comment-show-variables)

(when pel-use-hide-comnt
  ;; Download and byte-compile hide-comnt.el if its not present
  ;; Do it after compiling pel_keys.el, when pel-init load pel_keys.
  (cl-eval-when 'load
    (pel-install-github-file "emacsmirror/hide-comnt/master" "hide-comnt.el"))

  (pel-autoload-file hide-comnt for:
                     hide/show-comments
                     hide/show-comments-toggle)
  (define-key pel:comment ";" 'hide/show-comments-toggle)
  (define-key pel:comment ":" 'hide/show-comments))

;; ---------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> ?`` : Help /apropos/info commands

;; pel:help prefix is defined at the beginning of the <f11> section to allow
;; insertion of help under that prefix, later when logic dictates that
;; appropriate functionality is available.
;;
;; To help keep track what keay are used, the list of key under the pel:help
;; prefix are shown below.
;;
;;   Used `pel:help' keys:  . ? A a c d e f i k m p P s S w X

(define-key pel:help "." 'pel-mark-ring-stats)
(define-key pel:help "m"  #'man)
(define-key pel:help "w"  #'woman)
(define-key pel:help "?"  'pel-show-major-mode)
(define-key pel:help "f"  'which-function-mode)
(define-key pel:help "p"  'pel-help-pdf-select)
(define-key pel:help "P"  'pel-help-pdfs-dir)

(pel-autoload-file pel-help for:
                   pel-show-kill-ring
                   pel-show-major-mode)

(when pel-use-ascii-table
  (pel-ensure-package ascii-table from: melpa)
  (pel-autoload-file ascii-table for: ascii-table)
  (define-key pel:help "A" 'ascii-table))

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
;; - Function Keys - <f11> - Prefix ``<f11> ? i`` : Help Info commands

(define-pel-global-prefix pel:info (kbd "<f11> ? i"))
(define-key pel:info "a"  #'info-apropos)
(define-key pel:info "i"  #'info)
(define-key pel:info "m"  #'info-display-manual)

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; - Function Keys - <f11> - Prefix ``<f11> ? d`` : Describe

(define-pel-global-prefix pel:describe (kbd "<f11> ? d"))
(define-key pel:describe "$"  'pel-spell-show-use)
(define-key pel:describe "a"  'pel-show-face-at-point)
(define-key pel:describe "c" #'list-colors-display)
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

(define-pel-global-prefix pel:emacs (kbd "<f11> ? e"))
(define-key pel:emacs (kbd "C-p") #'list-processes)
(define-key pel:emacs "c"  'pel-emacs-command-stats)
(define-key pel:emacs "i"  'pel-imenu-dbg-print-vars)
(define-key pel:emacs "r"  'pel-open-emacs-refcard)
(define-key pel:emacs "s" #'list-load-path-shadows)
(define-key pel:emacs "t"  'pel-show-init-time)
(define-key pel:emacs "u" #'emacs-uptime)
(define-key pel:emacs "v" #'emacs-version)
(define-key pel:emacs "x"  'pel-emacs-executable)
(define-key pel:emacs "?"  'pel-package-info)

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

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
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
  (cl-eval-when 'load
    (pel-install-github-file "pierre-rouleau/command-log-mode/master"
                             "command-log-mode.el"))
  (pel-autoload-file command-log-mode for:
                     command-log-mode
                     global-command-log-mode)
  (define-key pel:command-log "c" 'command-log-mode)
  (define-key pel:command-log "C" 'global-command-log-mode)
  (pel-eval-after-load command-log-mode
    (define-key pel:command-log "o" 'clm/open-command-log-buffer)
    (define-key pel:command-log "." 'clm/close-command-log-buffer)
    (define-key pel:command-log "/" 'clm/toggle-log-all)))

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
(eval-after-load "ispell" '(pel-spell-init-from-user-option))

(define-key pel:spell "." #'ispell)
(define-key pel:spell ";" #'ispell-comments-and-strings)
(define-key pel:spell "?"  'pel-spell-show-use)
(define-key pel:spell "D" #'ispell-change-dictionary)
(define-key pel:spell "F" #'flyspell-mode)
(define-key pel:spell "K" #'ispell-kill-ispell)
(define-key pel:spell "P" #'flyspell-prog-mode)
(define-key pel:spell "b" #'ispell-buffer)
(define-key pel:spell "m" #'ispell-message)
(define-key pel:spell "r" #'ispell-region)
(define-key pel:spell "v" #'ispell-check-version)

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
(pel-add-hook-for
 'pel-modes-activating-flyspell-mode
 (lambda ()
   (flyspell-mode 1)
   (pel--check-flyspell-iedit-conflict)))

(pel-add-hook-for
 'pel-modes-activating-flyspell-prog-mode
 (lambda ()
   (flyspell-prog-mode)
   (pel--check-flyspell-iedit-conflict)))

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
                     bm-toggle
                     bm-buffer-save
                     bm-buffer-restore)
  (global-set-key (kbd "<f2>")  'bm-next)
  (define-key pel:bookMark "'"  'bm-toggle) ; toggle visible bookmark
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
(global-set-key (kbd "<M-f5>")     'pel-scroll-up)
(global-set-key (kbd "<M-S-f5>")   'pel-scroll-up-other)
;; scroll text down: toward large line number
(global-set-key (kbd "<M-up>")    'pel-scroll-down)
(global-set-key (kbd "<M-f6>")    'pel-scroll-down)
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
(define-key pel:scroll "a" #'scroll-all-mode)
(define-key pel:scroll "f" #'follow-mode)
(define-key pel:scroll "l" #'scroll-lock-mode)

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
(define-key pel:abbrev "D"  'pel-define-abbrevs)
(define-key pel:       "/" #'expand-abbrev)
(define-key pel:abbrev "e" #'expand-abbrev)
(define-key pel:abbrev "E" #'expand-region-abbrevs)
(define-key pel:abbrev "g" #'add-global-abbrev)
(define-key pel:abbrev "i" #'insert-abbrevs)
(define-key pel:abbrev "l" #'add-mode-abbrev)
(define-key pel:abbrev "L" #'list-abbrevs)
(define-key pel:abbrev "M" #'edit-abbrevs)
(define-key pel:abbrev "r" #'read-abbrev-file)
(define-key pel:abbrev "s" #'write-abbrev-file)
(define-key pel:abbrev "u" #'unexpand-abbrev)

(if pel--cached-abbrev-file-name
    ;; If PEL is informed to delay load the abbreviation file
    ;; do it silently 2 seconds of idle later.
    (run-at-time "2 sec" nil
                 (lambda ()
                   (progn
                     (define-key pel:abbrev "a" #'abbrev-mode)
                     (setq abbrev-file-name pel--cached-abbrev-file-name)
                     (quietly-read-abbrev-file nil))))
  (define-key pel:abbrev "a" #'abbrev-mode))


(defun pel-define-abbrevs (&optional arg)
  "Read abbreviations from current buffer after confirming with user.
With argument ARG , eliminate all abbrev definitions except
the ones defined from the buffer now."
  (interactive "P")
  (if (yes-or-no-p "Read abbreviations from current buffer? ")
      (define-abbrevs arg)
    (message "Nothing done.")))

;; ---------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> b`` : buffer commands
;; Used keys:
;;   -
;;   C-c
;;   M-a M-p
;;   I R U X
;;   b c f h i k l n p r v x

(define-pel-global-prefix pel:buffer (kbd "<f11> b"))
(define-key pel:buffer "-"  #'ruler-mode)
(define-key pel:buffer "c"  #'clone-buffer)
(define-key pel:buffer "k"  #'kill-current-buffer)
(define-key pel:buffer "l"   'pel-switch-to-last-used-buffer)
(define-key pel:buffer "n"  #'next-buffer)
(define-key pel:buffer "P"   'pel-show-window-previous-buffer)
(define-key pel:buffer "p"  #'previous-buffer)
(define-key pel:buffer "r"  #'read-only-mode)
(define-key pel:buffer "v"  #'view-buffer)
(define-key pel:buffer "R"  #'rename-buffer)
(define-key pel:buffer "U"  #'rename-uniquely)
(define-key pel:buffer (kbd "M-a")  #'append-to-buffer)
(define-key pel:buffer (kbd "M-p")  #'prepend-to-buffer)
(define-key pel:buffer (kbd "C-c")  #'copy-to-buffer)
(define-key pel:buffer "i"  #'insert-buffer)
(define-key pel:buffer "f"  #'append-to-file)
(define-key pel:buffer (kbd "M-x") 'hexl-mode)
(when pel-use-popup-switcher
  (define-key pel:buffer "b" 'psw-switch-buffer))
;; Reserved            "h"  highlight prefix
;; Reserved            "I"  indirect buffer prefix
;; Reserved            "x"   (see declarations below with pel-use-nhexl-mode)
;; Reserved            "X"

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
          pel-use-treemacs)
  (define-pel-global-prefix pel:browse (kbd "<f11> B"))

  (when pel-use-treemacs
    (pel-ensure-package treemacs from: melpa)
    (pel-autoload-file treemacs for: treemacs)
    (define-key pel:browse  "T" 'treemacs)
      (with-eval-after-load 'winum
        (when (boundp 'winum-keymap)
          (define-key winum-keymap (kbd "<f9>") 'treemacs-select-window))))

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
                    pel-ztree-dir-show-filtered-files))))

;; ---------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> c`` : count things

(define-pel-global-prefix pel:count (kbd "<f11> c"))
(define-key pel:count "m" #'count-matches)
(define-key pel:count "p" #'count-lines-page)
(define-key pel:count "W" #'count-words-region)
(define-key pel:count "w" #'count-words)

;; ---------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> C`` : clipboard commands

(define-pel-global-prefix pel:clipboard (kbd "<f11> C"))
(define-key pel:clipboard "c" #'clipboard-kill-ring-save)
(define-key pel:clipboard "x" #'clipboard-kill-region)
(define-key pel:clipboard "v" #'clipboard-yank)

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
(define-key pel:ediff "R"  'eregistry)

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
(define-key pel:ediff-dirs "d"  'edirs)
(define-key pel:ediff-dirs "3"  'edirs3)
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

(defun pel-show-buffer-file-encoding ()
  "Show coding system of file in current buffer."
  (interactive)
  (describe-symbol 'buffer-file-coding-system))

(declare-function find-grep "grep")
(define-pel-global-prefix pel:file (kbd "<f11> f"))
;; Used keys in <f11> f:
;; . / ?
;; F I L O W
;; a d f g h i j l n o p r t u v w
;; C-f
;; M-. M-/ M-l M-t M-u M-x
(define-key pel: (kbd "C-f") 'find-file)
(define-key pel:file "I" #'insert-file-literally)
(define-key pel:file "O" #'find-file-read-only-other-window)
(define-key pel:file "L" #'locate)
(define-key pel:file "W" #'append-to-file)
(define-key pel:file "d" #'find-dired)
(define-key pel:file "F"  'pel-open-in-os-app)
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
(define-key pel:file "?" #'pel-show-buffer-file-encoding)
(when pel-use-popup-switcher
  (define-key pel:file "f" 'psw-navigate-files))

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
(global-set-key (kbd "C-^") 'pel-open-at-point)
(define-key pel:file "."    'pel-open-at-point)
(define-key pel:file (kbd "M-.") 'pel-set-ido-use-fname-at-point)
(define-key pel:file "/"    'pel-browse-filename-at-point)
(define-key pel:file (kbd "M-/") 'browse-url-at-point)
(define-key pel:file (kbd "M-u") 'pel-open-url-at-point)
(global-set-key "\C-cj"    'webjump)
(define-key pel:file "j"   'webjump)

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
  (cl-eval-when 'load
    (pel-install-github-file "pierre-rouleau/find-file-in-project/master"
                             "find-file-in-project.el")
    (autoload 'find-file-in-project "find-file-in-project"))
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
    (pel-autoload-file ripgrep for: ripgrep-regexp)))

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
  (define-key pel:ag-kill  "p"   'ag/kill-process))

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
  (define-key pel:highlight "c" 'rainbow-mode)) ; use c for color

(defun pel-hi-lock-find-patterns ()
  "Execute hi-lock-find-patterns when `hi-lock-mode' is active."
  (interactive)
  (declare-function hi-lock-find-patterns "hi-lock")
  (if (fboundp 'hi-lock-find-patterns)
      (hi-lock-find-patterns)
    (user-error "Turn hi-lock-mode on first")))

(define-key pel:highlight      "-" #'hl-line-mode)
(define-key pel:highlight      "(" #'show-paren-mode)
(define-key pel:highlight      "."  #'highlight-symbol-at-point)
(define-key pel:highlight      "C"  #'highlight-changes-mode)
(define-key pel:highlight      "h"   'pel-set-highlight-color)
(define-key pel:highlight      "H"   'pel-customize-highlight)
(define-key pel:highlight      "F"  #'font-lock-mode)
(define-key pel:highlight      "f"   'pel-hi-lock-find-patterns)
(define-key pel:highlight      "G"  #'global-hi-lock-mode)
(define-key pel:highlight      "L"  #'hi-lock-mode)
(define-key pel:highlight      "l"  #'highlight-lines-matching-regexp)
(define-key pel:highlight      "p"  #'highlight-phrase)
(when pel-use-rainbow-delimiters
  (define-key pel:highlight    "R"  'rainbow-delimiters-mode))
(define-key pel:highlight      "r"  #'highlight-regexp)
(define-key pel:highlight      "s"   'pel-toggle-hl-line-sticky)
(define-key pel:highlight      "u"  #'unhighlight-regexp)
(define-key pel:highlight      "w"  #'hi-lock-write-interactive-patterns)
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
  (cl-eval-when 'load
    (pel-install-github-file "emacsmirror/vline/master" "vline.el"))
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
;;   D   L
;; c d f l t
;; M-c
(define-pel-global-prefix pel:insert (kbd "<f11> i"))
(define-key pel:insert   "c" 'copyright)
(define-key pel:insert (kbd "M-c") 'copyright-update)
(define-key pel:insert   "d" 'pel-insert-current-date)
(define-key pel:insert   "D" 'pel-insert-current-date-time)
(define-key pel:insert   "f" 'pel-insert-filename)
(define-key pel:insert   "l" 'pel-insert-line)
(define-key pel:insert   "t" 'pel-insert-iso8601-timestamp)

(when (or pel-use-lice
          pel-c-skel-with-license
          pel-clisp-skel-with-license
          pel-elisp-skel-with-license
          pel-erlang-skel-with-license)
  (pel-ensure-package lice from: melpa)
  (pel-autoload-file lice for: lice)
  (define-key pel:insert "L" 'lice)
  (define-key pel:f6 "L" 'lice))

;; -----------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> k`` : Keyboard macro operations

(define-pel-global-prefix pel:kbmacro (kbd "<f11> k"))
(define-key pel:kbmacro "k"   'pel-forget-recorded-keyboard-macro)
(define-key pel:kbmacro "i"  #'insert-kbd-macro)

(when pel-use-centimacro
  ;; Until abo-abo integrates my pull-request that fixes the bugs
  ;; I'll support this package via my copy of the file from my fork
  ;; of the original project at https://github.com/abo-abo/centimacro
  (cl-eval-when 'load
    (pel-install-github-file "pierre-rouleau/centimacro/master"
                             "centimacro.el"))
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
  (cl-eval-when 'load
    (pel-install-github-file "pierre-rouleau/emacros/master" "emacros.el"))
  (pel-autoload-file emacros for:
                     emacros-load-macros
                     emacros-show-macros
                     emacros-show-macro-names)
  (add-hook 'find-file-hook 'emacros-load-macros)
  (global-set-key "\C-ce" 'emacros-execute-named-macro)
  (global-set-key "\C-cx" 'emacros-auto-execute-named-macro)
  (define-key pel:emacros "=" 'emacros-name-last-kbd-macro-add)
  (define-key pel:emacros "e" 'emacros-execute-named-macro)
  (define-key pel: (kbd "<f4>") 'emacros-execute-named-macro)
  (define-key pel:emacros "?" 'emacros-show-macros)
  (define-key pel:emacros "/" 'emacros-show-macro-names)
  (define-key pel:emacros "L" 'emacros-load-macros)
  (define-key pel:emacros "R" 'emacros-refresh-macros)
  (define-key pel:emacros "r" 'emacros-rename-macro)
  (define-key pel:emacros "m" 'emacros-move-macro)
  (define-key pel:emacros (kbd "DEL") 'emacros-remove-macro))

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

(when (or pel-use-multiple-cursors
          pel-use-iedit
          pel-use-lispy)
  (define-pel-global-prefix pel:mcursors (kbd "<f11> m"))
  (when pel-use-multiple-cursors
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
    (define-key pel:mcursors (kbd "M-/") 'mc-hide-unmatched-lines-mode))

  (when (or pel-use-iedit pel-use-lispy)
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
    (global-anzu-mode +1)))

(when pel-use-cexp
  ;; download and byte-compile cexp if not already present
  ;; Do it after compiling pel_keys.el, when pel-init load pel_keys.
  (cl-eval-when 'load
    (pel-install-github-file "TobiasZawada/cexp/master" "cexp.el"))
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
                     pel-select-search-tool
                     pel-show-active-search-tool)
  (define-key pel:search-replace "s" 'pel-select-search-tool)
  (define-key pel:help           "s" 'pel-show-active-search-tool))

;; --
;; Regular Expression Builder

(defun pel-reb-re-syntax ()
  "Customize reb-re-syntax: Regular Expression Builder syntax."
  (interactive)
  (customize-option 'reb-re-syntax))

(define-pel-global-prefix pel:regexp (kbd "<f11> s x"))
(define-key pel:regexp      " "   'pel-insert-regexp)
(define-key pel:regexp      "b"  #'re-search-backward)
(define-key pel:regexp      "f"  #'re-search-forward)
(define-key pel:regexp      "q"  #'query-replace-regexp) ; maybe replaced below
(define-key pel:regexp      "r"  #'replace-regexp)       ; maybe replaced below
(define-key pel:regexp      "B"  #'re-builder)
;; add it here because C-M-% cannot be typed in terminal mode
(define-key pel:regexp  (kbd "M-B")  'pel-reb-re-syntax)
(when pel-bind-keys-for-regexp
  ; both maybe replaced below
  (define-key global-map (kbd "C-c r") 'replace-regexp)
  (define-key global-map (kbd "C-c q") 'query-replace-regexp))
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
    (define-key pel:regexp "M" 'vr/mc-mark)))

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

(when (or pel-use-visual-regexp pel-use-visual-regexp-steroids)
  (pel-autoload-file pel-search-regexp for:
                     pel-select-search-regexp-engine
                     pel-show-active-search-regexp-engine
                     pel-replace-regexp
                     pel-query-replace-regexp)
  (define-key pel:search-replace "S" 'pel-select-search-regexp-engine)
  (define-key pel:help   "S" 'pel-show-active-search-regexp-engine)
  ;; replace some already bound keys
  (define-key pel:regexp "r" 'pel-replace-regexp)
  (define-key pel:regexp "q" 'pel-query-replace-regexp)
  (when pel-bind-keys-for-regexp
    (define-key global-map (kbd "C-c r") 'pel-replace-regexp)
    (define-key global-map (kbd "C-c q") 'pel-query-replace-regexp)
    (when pel-use-multiple-cursors
      (define-key global-map (kbd "C-c m") 'vr/mc-mark))))

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

(when (or pel-use-speedbar
          pel-use-projectile-speedbar)

  ;; Install sr-speedbar from my GitHub depot instead of from Melpa,
  ;; because Melpa follows the older version in emacsmirror. I'm keeping
  ;; min closer to what's available in the emacswiki.
  (cl-eval-when 'load
    (pel-install-github-file "pierre-rouleau/sr-speedbar/master"
                             "sr-speedbar.el"))

  (pel-autoload-file sr-speedbar for:
                     sr-speedbar-toggle
                     sr-speedbar-window-p)
  (define-pel-global-prefix pel:speedbar (kbd "<f11> M-s"))
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

  (with-eval-after-load 'sr-speedbar
    (advice-add 'sr-speedbar-open :after (function pel--sr-speedbar-setup)))

  (when pel-use-projectile-speedbar
    (pel-ensure-package projectile-speedbar from: melpa)
    (pel-autoload-file projectile-speedbar for:
                       projectile-speedbar-open-current-buffer-in-tree
                       projectile-speedbar-toggle)
    (with-eval-after-load 'projectile
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
(define-key pel:justification "n" #'set-justification-none)
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

(pel-add-hook-for
 'pel-modes-activating-superword-mode
 (lambda ()
   (superword-mode 1)))
(pel-add-hook-for
     'pel-modes-activating-subword-mode
     (lambda ()
       (subword-mode 1)))

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

;; ---------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> v`` : VCS operations
;;
(define-pel-global-prefix pel:vcs (kbd "<f11> v"))
(define-key pel:vcs "v"  'vc-dir)

;; Git support
(when pel-use-magit
  (pel-ensure-package magit from: melpa)
  (pel-autoload-file magit for:
                     magit
                     magit-status)
  (define-key pel:vcs "g"  'magit-status))

;; Mercurial Support
(when pel-use-monky
  (pel-ensure-package monky from: melpa)
  (pel-autoload-file monky for: monky-status)
  (define-key pel:vcs "m"  'monky-status))

(when pel-use-hgignore-mode
  ;; Install & compile hgignore-mode if requested.  No key assignment;
  ;; the package installation will activate the file name association
  ;; and the auto-loading.
  (pel-ensure-package hgignore-mode from: melpa))

;; ---------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> w`` : Windows operations
;; Use the global local winner-mode, but don't use its key bindings;
;; use some in the '<f11> w' group:

;; Used: # B O S b d f h k m n o p r s v x
;;
(define-pel-global-prefix pel:window (kbd "<f11> w"))
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
  (pel-autoload-file desktop for:
                     desktop-save
                     desktop-read
                     desktop-save-mode
                     desktop-change-dir
                     desktop-revert
                     desktop-clear)
  (unless (eq pel-use-desktop 'with-desktop+)
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
;; Used keys:
;; ? a e f i j l p r s t v x
;; L

(declare-function eshell "eshell")

(define-key pel:execute    "?" #'list-processes)
(when pel-use-common-lisp
  (define-key pel:execute  "L" #'pel-cl-repl))
(define-key pel:execute    "a" #'ansi-term)
(define-key pel:execute    "e" #'eshell)
(when pel-use-forth
  (define-key pel:execute  "f" 'run-forth))
(when pel-use-julia
  (define-key pel:execute  "j"  'julia-snail))
(define-key pel:execute    "l" #'ielm)
(when pel-use-lfe
  (define-key pel:execute  (kbd "C-l")  'run-lfe))
(when pel-use-python
  (define-key pel:execute  "p" #'run-python))
(when pel-use-erlang
  (define-key pel:execute  "r"  'erlang-shell))
(when (and pel-use-elixir pel-use-alchemist)
  (define-key pel:execute  "x"  'alchemist-iex-run))
(define-key pel:execute    "s" #'shell)
(define-key pel:execute    "t" #'term)

;; support for the extremely fast/nice libvterm-based vterm shell.
(when pel-use-vterm
  (pel-ensure-package vterm from: melpa)
  (pel-autoload-file vterm for: vterm)
  (define-key pel:execute "v" 'vterm))

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

;; Installation of work around for Emacs bug 44494
;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=44494
;; TODO: qualify this with emacs version as soon as a version of Emacs fixes
;; the bug.
(add-hook 'xref-etags-mode-hook (function
                                 (lambda () (load "pel-etags" :no-error))))


(defun pel--setup-for-custom ()
  "PEL setup for Custom-mode."
  (when pel-bind-m-dot-to-xref-find-custom-definition
    (local-set-key (kbd "M-.") 'pel-xref-find-custom-definition-at-line))
  (local-set-key (kbd "<f11> X .") 'pel-xref-find-custom-definition-at-line))
(add-hook 'Custom-mode-hook 'pel--setup-for-custom)

;; ggtags
(when pel-use-ggtags
  (pel-ensure-package ggtags from: melpa)
  (pel-autoload-file ggtags for: ggtags-mode)
  ;; ggtags has its own key map which has all we need.
  ;; just provide a key to quickly enable or disable ggtags-mode.
  (define-key pel:xref-backend "G" 'ggtags-mode)
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
                    'cscope-minor-mode)

  (when pel-use-helm-cscope
    (pel-ensure-package helm-cscope from: melpa)
    (pel-autoload-file helm-cscope for: helm-cscope-mode)
    (define-key pel:cscope "H" 'pel-toggle-helm-cscope)
    (add-hook 'helm-cscope-mode-hook 'pel-activate-helm-cscope)
    (pel-add-hook-for 'pel-modes-activating-helm-cscope
                      'pel-activate-helm-cscope)))

;; dumb-jump
(when pel-use-dumb-jump
  (pel-ensure-package dumb-jump from: melpa)
  (pel-autoload-file dumb-jump for: pel-xref-toggle-dumb-jump-mode)
  ;; pel-xref-toggle-dumb-jump-mode sets up the xref-backend-functions
  ;; to use dumb-jump as the backend for xref, and use its key bindings.
  (define-key pel:xref-backend "D" 'pel-xref-toggle-dumb-jump-mode)
  ;; schedule activation for requested major modes.
  (pel-add-hook-for 'pel-modes-activating-dumb-jump
                    'pel-xref-dumb-jump-activate))

;; gxref
(when pel-use-gxref
  (pel-ensure-package gxref from: melpa)
  (pel-autoload-file gxref for: xref-show-xrefs-function)
  (define-key pel:xref-backend "g" 'pel-xref-toggle-gxref)
  (pel-add-hook-for 'pel-modes-activating-gxref
                    'pel-xref-gxref-activate))

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
  (if (< emacs-major-version 27)
      (pel-autoload-file helm-xref for: helm-xref-show-xrefs)
    (pel-autoload-file helm-xref for:
                       helm-xref-show-xrefs-27
                       helm-xref-show-defs-27)))

(when (or pel-use-ivy-xref
          pel-use-helm-xref)
  (run-with-idle-timer
   1 nil
   (function pel-xref-set-front-end) pel-startup-xref-front-end))

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
  (let* ((current-directory (file-name-directory load-file-name))
         (el-filename (expand-file-name "pel__hydra.el" current-directory)))
    (pel-byte-compile-if-needed el-filename)))

;; ---------------------------------------------------------------------------
(provide 'pel_keys)

;;; pel_keys.el ends here

; LocalWords:  EditorConfig

;;; pel_keys.el --- PEL key binding definitions -*-lexical-binding: t-*-

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
;;       This file is similar to what would be located inside a init.el file.
;;       It is, however, byte-compiled and linted to check for error.
;;       All use-package forms would generate warnings normally.
;;       To prevent that the code uses a cl-eval-when 'compile form
;;       to require external packages *only* when compiling, not at load time.
;;       During execution the external packages are only loaded lazily,
;;       when they are required, not before.


;;; Code:

(eval-when-compile
  (require 'cl-lib))                       ; use: cl-eval-when

;; Utilities

(defun pel--mode-hook-maybe-call (fct mode hook &optional append)
  "Use FCT as the MODE HOOK and call it if buffer is currently in that MODE.
The function FCT is added at the beginning of the hook list unless the
optional argument APPEND is non-nil, in which case it is added at the end."
  (add-hook hook fct append)
  (if (eq major-mode mode)
      (funcall fct)))

(defmacro define-pel-global-prefix (prefix key)
  "Define a PREFIX KEY for the global key map."
  `(progn
     ;; declare the prefix variable to avoid compiler warnings.
     (defvar ,prefix)
     ;; define the prefix key as a global prefix
     (define-prefix-command (quote ,prefix))
     (global-set-key ,key (quote ,prefix))))

(defmacro pel-setq  (sym val)
  "Set a symbol SYM to specified value VAL and prevent warning."
  `(progn
     ;; declare the symbol to prevent lint warning
     (defvar ,sym)
     ;; now set the symbol to the specified value
     (setq ,sym ,val)))


(defmacro pel-setq-default  (sym val)
  "Set a symbol SYM to specified default value VAL and prevent warning."
  `(progn
     ;; declare the symbol to prevent lint warning
     (defvar ,sym)
     ;; now set the symbol to the specified value
     (setq-default ,sym ,val)))

;; -----------------------------------------------------------------------------
;; Required packages:
(require 'pel--options) ; all `pel-use-...' variables identify what to use.
;;                      ; also defines a set of utility functions to deal with
;;                      ; the options: pel-auto-complete-help

(unless (fboundp 'pel-build)
  ;; autoload all PEL functions
  (require 'pel-autoload)
  (if (fboundp 'pel--autoload-init)
      (pel--autoload-init)))

;; -----------------------------------------------------------------------------
;; utilities
;; ---------

;; - Bootstrap `use-package' if needed
;; -----------------------------------
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; - Use popup-kill-ring
;; ---------------------
;; View all kill-ring deletions in a pop-up menu, when
;; M-y is typed.
;; Activate the popup-kill-ring in graphic mode only
;; because it does not seem to work in terminal mode.
;; It uses the pos-tip package.
;; the 2 packages manually.
(when (and pel-use-popup-kill-ring
           (display-graphic-p))

  (cl-eval-when 'compile (require 'popup-kill-ring))
  (use-package popup-kill-ring
    ;; Note: pos-tip, required by popup-kill-ring is installed
    ;;       when popup-kill-ring is installed (and loaded by
    ;;       it too).
    :ensure t
    :pin melpa
    :commands popup-kill-ring))

;; -----------------------------------------------------------------------------
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
(when (and (eq system-type 'darwin)
           (display-graphic-p))
  (pel-setq ns-function-modifier 'hyper))

;; On Windows, the Ctrl-Alt key combination works with letter keys
;; but does not work with the <right>, <left>, <up> and <down> keys.
;; As a work-around, this maps the apps key (between the right Windows
;; and Ctrl keys) to hyper and ensure that we add an extra binding
;; with hyper for the C-M-arrow keys.
(when (eq system-type 'windows-nt)
  ;; The following do not seem to work:
  ;; - Ref: http://ergoemacs.org/emacs/emacs_hyper_super_keys.html
  ;; (setq w32-pass-lwindow-to-system nil)
  ;; (setq w32-lwindow-modifier 'hyper)     ; Left Windows Key  := hyper
  ;; (setq w32-pass-rwindow-to-system nil)
  ;; (setq w32-rwindow-modifier 'super)     ; Right Windows key := super
  ;; but this works:
  (pel-setq w32-pass-apps-to-system nil)
  (pel-setq w32-apps-modifier 'hyper))      ; Menu/App key      := hyper

;; -----------------------------------------------------------------------------
;; - Preserve negative-argument with C-- and C-_
;; ---------------------------------------------
;;
;; When running Emacs inside a terminal, the key C-- is not available since
;; there is no "Control -" in ASCII.  On macOS, the terminal shells generate
;; the C-_ key when C-- is typed.  Unfortunately C-_ is mapped to `undo' by
;; default, preventing quick access to the negative argument in a Control key
;; chord.  By mapping C-_ to `negative-argument' we solve the problem.
;; We do it in graphics mode also, for consistency.
(global-set-key (kbd "C-_") 'negative-argument)

;; Also bind M-_  to `negative-argument' to help accessing it when
;; the Meta key specifiers is used, to maintain typing velocity.
(global-set-key (kbd "M-_") 'negative-argument)

;; Reserved key::  (kbd "C-M-_")
;;
;; The last possible combination, C-M-_ is *not* bound to the
;; negative-argument function.  Instead it is *reserved* as a special
;; key sequence to use with a "lossless keyboard input" package such as
;; term-key.el to add ability to bind keys that are normally not accessible
;; in terminal mode.

;; -----------------------------------------------------------------------------
;; - Font Control
;; --------------
;; On macOS, the keys used by the OS are the same as selected here, both in
;; GUI mode and in terminal (TTY) mode:
;; - In terminal mode: the Terminal.app uses the ⌘ command keys for fond size
;;   control (it's not Emacs that acts on them, its the Terminal.app)
;; - In graphics mode the same keys handled by Emacs: the Super modifier is
;;   assigned to the ⌘ Command key.

(when (and (eq system-type 'darwin)
           (display-graphic-p))

  ;; Bind the face-remap commands. The face-remap package
  ;; is part of Emacs standard distribution.
  (global-set-key (kbd "s-=")             #'text-scale-adjust)
  (global-set-key (kbd "s-+")             #'text-scale-adjust)
  (global-set-key (kbd "s--")             #'text-scale-adjust)
  (global-set-key (kbd "s-0")             #'text-scale-adjust)

  ;; Load the pel-font file only as needed.
  ;; Configure the pel-font commands as autoload.
  ;; although ther is no such package, use the macro to set up
  ;; the delayed autoloads and key bindings.
  (cl-eval-when 'compile (require 'pel-font))
  (use-package pel-font
    ;; autoload it when one of the following commands is used.
    :commands (pel-font-increase-size-all-buffers
               pel-font-decrease-size-all-buffers
               pel-font-reset-size-all-buffers)


    ;; run following command before package is loaded to
    ;; activate the autoload.
    :init
    (global-set-key (kbd "<s-kp-add>")
                    #'pel-font-increase-size-all-buffers)
    (global-set-key (kbd "<s-kp-subtract>")
                    #'pel-font-decrease-size-all-buffers)
    (global-set-key (kbd "<s-kp-0>")        #'pel-font-reset-size-all-buffers)))

;; -----------------------------------------------------------------------------
;; - Buffer navigation
;; -------------------
;; Replace `list-buffer' by the nicer, more flexible and more
;; powerful `ibuffer'. Show in current window, not other one.
(global-set-key "\C-x\C-b" 'ibuffer)

;; dired-narrow
;; ------------
;; When dired-narrow is used, add <f12> prefix keys to dired-narrow specific
;; commands.

(when pel-use-dired-narrow
  (cl-eval-when 'compile (require 'dired-narrow))
  (use-package dired-narrow
    ;; dired-narrow is an external package.
    ;; Ensure it's installed via MELPA
    :ensure t
    :pin melpa

    ;; autoload it when one of the following commands is used.
    :commands (dired-narrow
               dired-narrow-regexp
               dired-narrow-fuzzy)

    ;; run following command before package is loaded to
    ;; activate the autoload.
    :init
    (defvar pel:for-dired-narrow)
    (define-prefix-command 'pel:for-dired-narrow)
    ;;
    (define-key pel:for-dired-narrow "s" #'dired-narrow)
    (define-key pel:for-dired-narrow "r" #'dired-narrow-regexp)
    (define-key pel:for-dired-narrow "f" #'dired-narrow-fuzzy)
    ;;
    (pel--mode-hook-maybe-call
     '(lambda ()
        (local-set-key (kbd "<f12>") 'pel:for-dired-narrow))
     'dired-mode 'dired-mode-hook)))

;; -----------------------------------------------------------------------------
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
;;  - for macOS : s-left, s-right, s-up and s-down.
;;
;; Also, in graphics mode, if a (modified) version of the
;; framemove.el file is available, then we can allow the
;; window move navigation to expand moving away from the
;; current frame into the others surrounding frames.

;; windmove is part of Emacs.  It's loading is controlled by pel-window, where
;; it is used, via autoloading. Here, just identify default keybindings when
;; Emacs is running in graphics mode.  Other keybindings are defined for the
;; pel: keybinding, somewhere else in this file.
(when (display-graphic-p)
  (cl-eval-when 'compile (require 'windmove))
  (use-package windmove
    ;; specify defer: we don't want to require windmove here since it is
    ;; autoloaded via the pel-window file.  However, when Emacs is running in
    ;; graphics mode, we need to either set the default bindings (and then we
    ;; force autoload of windmove) or force users to use something else of
    ;; windmove to activate its special binding.  None of this is a good
    ;; solution. So, as a compromise to delay the loading of windmove, just
    ;; defer it for a specific amount of time, and then schedule the setting of
    ;; the special binding when it is actually loaded.
    :defer 5
    :config
    (declare-function windmove-default-keybindings "windmove")
    (windmove-default-keybindings (if (eq system-type 'darwin)
                                      'super
                                    'hyper))))
(when pel-use-framemove
  (cl-eval-when 'compile (require 'framemove))
  (when (display-graphic-p)
    (use-package framemove
      :defer 3
      :config
      ;; Note: modified framemove:
      ;;  (replaced "remove-if-not" by "cl-remove-if-not")
      (when (boundp 'framemove-hook-into-windmove)
        (setq framemove-hook-into-windmove t)))))

;; Uniquify: meaningful names when multiple buffers have the same name
;; -------------------------------------------------------------------
;; Uniquify provides meaningful names for buffers with the same name.
;; The following code snippet evolved from what's available on
;; https://github.com/bbatsov/prelude.
;; uniquify is now part of Emacs distribution.
(when pel-use-uniquify
  (cl-eval-when 'compile (require 'uniquify))
  (use-package uniquify
    :config
    (pel-setq uniquify-buffer-name-style 'post-forward)
    ;; rationalize buffer after killing uniquified buffer
    (pel-setq uniquify-after-kill-buffer-p t)
    ;; Don't  not uniquify  special buffers
    (pel-setq uniquify-ignore-buffers-re "^\\*")))

;; - Use IDO-mode
;; --------------
;; Provides suggestions at prompts for file names,
;; buffer names, etc...
;; Notes:
;; - When IDO gets in the way, type C-f at IDO prompt to
;;   enter the path and file name without suggestions.
;; - Use C-j when you want to force using what is
;;   and don't want to request a proposed value.
;; IDO is now part of Emacs distribution.
(when pel-use-ido-mode
  (ido-mode 1)
  (pel-setq ido-everywhere t)
  (pel-setq ido-enable-flex-matching t)
  ;; don't require confirmation when creating new buffers
  ;; with C-x b
  (pel-setq ido-create-new-buffer 'always))

;; - Use Hippie Expand
;; -------------------
(when pel-use-hippie-expand
  (global-set-key [remap dabbrev-expand] 'hippie-expand)
  ;; I want Hippie Expand to use DAbbrev *first* as this is what most of
  ;; my search require, then I want to search in the file names.  I don't
  ;; really need lisp code expansion since I already have that provided by
  ;; Company mode that will pop-up a menu. I might need straight abbreviations
  ;; or some dictionary completion, so I may need to add try-expand-all-abbrevs.
  (setq hippie-expand-try-functions-list
        (quote
         (try-expand-dabbrev
          try-expand-dabbrev-all-buffers
          try-complete-file-name-partially
          try-complete-file-name))))
;; Other search rules exist, but I am not using them.
;; They are:
;; - try-expand-all-abbrevs
;; - try-complete-lisp-symbol-partially
;; - try-complete-lisp-symbol
;; - try-expand-list
;; - try-expand-dabbrev-from-kill
;; - try-expand-line

;; -----------------------------------------------------------------------------
;; Visible Bookmark (bm.el)
;; ------------------------
(when pel-use-bm
  ;; configure bm package to be loaded only on first use.
  (cl-eval-when 'compile (require 'bm))
  (use-package bm
    :ensure t
    :pin melpa

    :init
    ;;  Prevent lint warnings using empty defvar
    (defvar bm-restore-repository-on-load)
    ;; Ensure that bm restores bookmark when it loads.
    (setq bm-restore-repository-on-load t)

    :config
    ;;  Prevent lint warnings using empty defvar
    ;; Allow cross-buffer 'next'
    (pel-setq bm-cycle-all-buffers t)

    ;; where to store persistent files
    (pel-setq bm-repository-file "~/.emacs.d/bm-repository")

    ;; save bookmarks
    (pel-setq-default bm-buffer-persistence t)

    ;; Loading the repository from file when on start up.
    (add-hook 'after-init-hook 'bm-repository-load)

    ;; prevent byte-compiler warnings
    (declare-function bm-buffer-save      "bm")
    (declare-function bm-buffer-save-all  "bm")
    (declare-function bm-repository-save  "bm")
    (declare-function bm-buffer-restore   "bm")

    ;; Saving bookmarks
    (add-hook 'kill-buffer-hook 'bm-buffer-save)

    ;; Saving the repository to file when on exit.
    ;; kill-buffer-hook is not called when Emacs is killed, so we
    ;; must save all bookmarks first.
    (add-hook 'kill-emacs-hook '(lambda nil
                                  (bm-buffer-save-all)
                                  (bm-repository-save)))

    ;; The `after-save-hook' is not necessary to use to achieve persistence,
    ;; but it makes the bookmark data in repository more in sync with the file
    ;; state.
    (add-hook 'after-save-hook 'bm-buffer-save)

    ;; Restoring bookmarks
    (add-hook 'find-file-hooks   'bm-buffer-restore)
    (add-hook 'after-revert-hook 'bm-buffer-restore)

    ;; The `after-revert-hook' is not necessary to use to achieve persistence,
    ;; but it makes the bookmark data in repository more in sync with the file
    ;; state. This hook might cause trouble when using packages
    ;; that automatically reverts the buffer (like vc after a check-in).
    ;; This can easily be avoided if the package provides a hook that is
    ;; called before the buffer is reverted (like `vc-before-checkin-hook').
    ;; Then new bookmarks can be saved before the buffer is reverted.
    ;; Make sure bookmarks is saved before check-in (and revert-buffer)
    (add-hook 'vc-before-checkin-hook 'bm-buffer-save)

    ;; TODO?: find a better binding?
    ;; A non conflicting, allowing function key to be used as prefix?
    (global-set-key (kbd "<f2>")   'bm-next)
    ))

;; -----------------------------------------------------------------------------
;; Markup Language Support
;; --=====================

;; --------------------------
;; - Lightweight markup modes
;; --------------------------
;; (use-package markdown-mode)

;; -----------------------------
;; - Programming Style: Org-Mode
;; -----------------------------
(when pel-use-org-mode
  ;; Org-Mode activation (as suggested by
  ;; https://orgmode.org/manual/Activation.html#Activation ):
  (cl-eval-when 'compile (require 'org))
  (use-package org
    :commands (org-mode
               org-indent-mode
               org-store-link
               org-agenda
               org-capture
               org-switchb)
    :init
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
    ;; Use the cleaner outline view mode.
    (add-hook 'org-mode-hook 'org-indent-mode)))

;; -------------------------------------
;; - Programming Style: reStructuredText
;; -------------------------------------
(when pel-use-rst-mode
  ;; - Add .stxt to the accepted file extensions for rst-mode (reStructuredText)
  ;; ---------------------------------------------------------------------------
  ;; The conventions for reStructuredText is normally .rst and .rest
  ;; Adding the .stxt file extension for reStructuredText.
  (setq auto-mode-alist
        (append '(("\\.stxt\\'"  . rst-mode)) auto-mode-alist)))

;; -----------------------------------------------------------------------------
;; - Programming Language Support
;; --============================

;; (when (and pel-use-eldoc-box
;;            (display-graphic-p))
;;   (use-package eldoc-box
;;     :require t))

;; C-like programming languages: C, C++
;; ------------------------------------
(when pel-use-c-eldoc
  (cl-eval-when 'compile (require 'c-eldoc))
  (use-package c-eldoc
    ;; c-eldoc is an external package.
    ;; Ensure it's installed via MELPA
    :ensure t
    :pin melpa

    ;; autoload it when one of the following commands is used.
    :commands c-turn-on-eldoc-mode

    ;; run following command before package is loaded to
    ;; activate the autoload.
    :init
    (add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)))

(when pel-use-cc-vars
  (cl-eval-when 'compile (require 'cc-vars))
  (use-package cc-vars
    :config
    ;; Using bsd/allman style but with 3 spaces per tabs.
    ;; TODO: it's value is 'set-from-style' ??  Need to investigate.
    (pel-setq-default c-basic-offset 3)))

;; ---------------
;; - CMake support
;; ---------------
;; (use-package cmake-mode)

;; ---------------------
;; - Common Lisp support
;; ---------------------
(when pel-use-common-lisp
  (cl-eval-when 'compile (require 'slime))
  (use-package slime
    :ensure t
    :pin melpa
    :defer t)

  (when (fboundp 'pel-cl-init)
    (pel-cl-init :slime-is-used))
  ;; TODO: generalize this code, allowing customization
  ;; also provide ability to install the Hyperspec locally again through
  ;; customization.
  ;; A  copy of the latest version (version 7) of the LispWorks Hyperspec
  ;; is at:
  ;; http://ftp.lispworks.com/pub/software_tools/reference/HyperSpec-7-0.tar.gz
  ;; as identified by the LispWorks Download page
  ;; (http://www.lispworks.com/documentation/common-lisp.html).
  ;; The following code is the default,
  ;; symlinks can be use to point to the real location.
  (pel-setq common-lisp-hyperspec-root
            (concat "file://"
                    (expand-file-name "~/docs/HyperSpec/"))))

;; -------------------------------
;; - Programming Style: Emacs Lisp
;; -------------------------------
(when pel-use-esup
  (cl-eval-when 'compile (require 'esup))
  (use-package esup
    ;; esup is an external package:
    ;; ensure it's installed from MELPA if not available.
    :ensure t
    :pin melpa
    :commands esup))

;; -----------------------------------
;; - Programming Style: Erlang Support
;; -----------------------------------
;; TODO: erlang flymake does not seem to work.
;; erlang MAN also does not work.
;; I do not know why.  Need to learn more.
;; (when pel-use-erlang
;;   ;; TODO: make the following a customization
;;   ;; that either accepts the path or an
;;   ;; environment variable to get it from,
;;   ;; and support multiple versions of Erlang.
;;   (setq erlang-root-dir
;;         (expand-file-name
;;           "~/Library/Application Support/ErlangInstaller/21.1/erts-10.1"))
;;   (use-package erlang
;;     :defer 3
;;     :commands erlang-mode
;;     :init
;;     (require 'erlang-start)
;;     :config
;;     (when pel-use-erlang-flymake
;;       (require 'erlang-flymake))))

;; ; TODO: complete this
;; (when pel-use-edts
;;   (use-package edts
;;     :defer t))))

;; ------------------------------------
;; - Programming Style: Haskell Support
;; ------------------------------------
;;
;; Using Intero to support Haskell programming language.
;; Installed it via the list-packages.
;; ; (add-hook 'haskell-mode-hook 'intero-mode)

;; -----------------------------------
;; - Programming Style: Python Support
;; -----------------------------------
(when pel-use-python                    ; TODO: complete this
  (cl-eval-when 'compile (require 'elpy))
  (use-package elpy
    :defer t)
  ;; Normally, (python-shell-prompt-detect) should
  ;; evaluate to (">>> " "... " "")
  ;; for Python shell to work properly.
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
  (when (eq system-type 'windows-nt)
    (defvar python-shell-unbuffered)
    (defvar python-shell-completion-native-disabled-interpreters)
    (setq python-shell-unbuffered nil)
    (if (boundp 'python-shell-completion-native-disabled-interpreters)
        (add-to-list
         'python-shell-completion-native-disabled-interpreters "python")
      (setq python-shell-completion-native-disabled-interpreters '("python")))))

;; - Programming Style: Rust & Cargo Support
;; -----------------------------------------
(when pel-use-rust                      ; TODO: complete this
  (cl-eval-when 'compile (require 'racer))
  (use-package racer
    :ensure t
    :pin melpa
    :commands racer-mode)

  (cl-eval-when 'compile (require 'rust-mode))
  (use-package rust-mode
    :ensure t
    :pin melpa
    :commands rust-mode)

  (cl-eval-when 'compile (require 'cargo))
  (use-package cargo
    :ensure t
    :pin melpa
    :commands cargo-minor-mode
    :config
    ;; M-x package-install rust-mode
    ;; M-x package-install cargo
    ;; M-x package-install racer
    ;; M-x package-install company
    (add-hook 'rust-mode-hook  'cargo-minor-mode)
    (add-hook 'rust-mode-hook  'racer-mode)
    (add-hook 'racer-mode-hook 'eldoc-mode)
    (when pel-use-company
      (add-hook 'racer-mode-hook 'company-mode))
    (define-key rust-mode-map (kbd "TAB") 'company-indent-or-complete-common)))

;; -----------------------------------------------------------------------------
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
;; The numeric keypad keys behave differently when Emacs is running in graphics
;; mode and when it is running in Terminal mode.  Another difference is between
;; the PC-keyaboard and the macOS keyboards.  The PC-keyboard has a numlock key,
;; but the macOS keyboards do not: the Numlock is a clear key.
;;
;; PEL attempts to provide the exact same behaviour for these keys in both modes
;; for all environments despite the difference.
;;

;; -- Numlock/Clear
;;
;; For macOS, the top-left key, labelled as "NumLock" above, is detected only in
;; graphics mode and is the [clear] key.  The Terminal mode does not detect that
;; key. So 2 key bindings are done:
;; - [clear] is mapped to pel-toggle-mac-numlock
;; - <f11> # is also mapped to pel-toggle-mac-numlock (see later)

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

(when (eq system-type 'windows-nt)
  (global-set-key [kp-insert] 'yank)
  (global-set-key [kp-space] 'recenter-top-bottom))

;; -- The . ([kp-decimal]) key
;;
;; In terminal (TTY) mode, the keypad "." is not recognized
;; as [kp-decimal] but as "M-O n" so adjust the binding accordingly.
;; In both cases, bind the key to pel-kp-decimal which behaves according
;; to the state of num-locking controlled by pel-numkpad.el

(if (display-graphic-p)
    (global-set-key [kp-decimal] 'pel-kp-decimal)
  (global-set-key (kbd "M-O n") 'pel-kp-decimal))

;; -- The + key on the numerical keypad
;;
;; That key registers as [kp-add] when Emacs runs in graphics mode, but
;; registers as [kp-separator] when Emacs runs in terminal mode.
;; So PEL binds the appropriate key to pel-kp-add which selects the action
;; according to the state of num-locking controlled by pel-numkpad.el

(if (display-graphic-p)
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
(if (eq system-type 'darwin)
    (progn
      (global-set-key [C-kp-4]    #'backward-sexp)
      (global-set-key [C-kp-6]    #'forward-sexp)
      (global-set-key [C-kp-8]    #'backward-up-list)
      (global-set-key [C-kp-2]    #'down-list))
  (global-set-key [C-kp-left]     #'backward-sexp)
  (global-set-key [C-kp-right]    #'forward-sexp)
  (global-set-key [C-kp-up]       #'backward-up-list)
  (global-set-key [C-kp-down]     #'down-list))

;; -----------------------------------------------------------------------------
;; - Navigation control facilities
;; -------------------------------
;; - uses: pel-navigate
;;
;; Remap standard 'beginning-of-line' to `pel-beginning-of-line'.  The PEL
;; function combines the functionality of `beginning-of-line' and the function
;; `back-to-indentation'.
(global-set-key (kbd "C-a") 'pel-beginning-of-line)

;; Augment word movement by adding M-n to move to the beginning of a word,
;; something that is not provided by the standard Emacs keys; it only has
;; forward-word  (which moves at the end of word forward) and backward-word
;; (which moves backward to the beginning of the word).  The backward-word
;; command is bound to M-b which is just to the left of the M-n key in QWERTY
;; and AZERTY keyboards.
(global-set-key (kbd "M-n")  'pel-forward-word-start)

;; Meta left/right to forward/backward-word as this is needed by term shells
;; and python shells from outside Emacs so we leave it to get the same behaviour
;; in these shells inside Emacs. These leave cursor at end of a word.
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

;; -----------------------------------------------------------------------------
;; - Cursor Keys
;; -------------

;; ============ ========== ===================== ==============================
;; Cursor key   Modifier   Function              Notes
;; ============ ========== ===================== ==============================
;; Left         Control    left-word
;; Right        Control    right-word
;; Up           Control    backward-paragraph
;; Down         Control    forward-paragraph
;; kp-Left      Control    backward-sexp
;; kp-Right     Control    forward-sexp
;; kp-Up        Control    backward-up-list
;; kp-Down      Control    down-list
;;
;; Left         Meta       backward-word         Move to word left
;;                                               ignores all non-whitespace
;; Right        Meta       forward-word          Move to word right
;;                                               ignore all non-whitespace
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
;; Left         Super      windmove-left         Move cursor to window at left
;; Right        Super      windmove-right        Move cursor to window at right
;; Up           Super      windmove-up           Move cursor to window above
;; Down         Super      windmove-down         Move cursor to window below
;; kp-Left      Super      ??
;; kp-Right     Super      ??
;; kp-Up        Super      ??
;; kp-Down      Super      ??
;;
;; Left         Meta-Super pel-previous-visible  Move to word left
;;                                               ignores all non-whitespace
;; Right        Meta-Super pel-next-visible      Move to word right
;;                                               ignore all non-whitespace
;; Up           Meta-Super ??
;; Down         Meta-Super ??
;; kp-Left      Meta-Super ??
;; kp-Right     Meta-Super ??
;; kp-Up        Meta-Super ??
;; kp-Down      Meta-Super ??
;; ============ ========== ===================== ==============================

;; -----------------------------------------------------------------------------
;; - Function Keys
;; ---------------
;;
;; The first 4 function keys are used by Emacs and do not support combinations
;; with Control, Meta and Shift in macOS terminal (at least I have not found a
;; way to get terminal escape sequences for the first 4 functions keys to work
;; with Emacs running in macOS Terminal.
;; For F5 through F12, the combinations do work and aside from F8 (used by spell
;; and flyspell), these keys are not used by the Emacs packages I use so far.
;; I use F6 as an extra prefix and assign the other keys to various operations
;; as described below.

;; <f1>  : Emacs help system
;; <f2>  : prefix
;; <f3>  > pel-kmacro-start-macro-or-insert-counter
;; <f4>  : kmacro-end-or-call-macro
;; <f5>  > repeat
;; <f6>  > pel prefix
;; <f7>  >                        (C)                   (M)
;; <f8>  >                        (C)                   (M)
;; <f9>  >                        (C)                   (M)
;; <f10> > menu-bar-open,       (C) buffer-menu-open, (M) toggle-frame-maximized
;; <f11> > pel prefix,  <C-f11>: pel-previous-visible,  <M-f11>: pel-scroll-up
;; <f12> > unused,      <C-f12>: pel-next-visible,      <M-f12>: pel-scroll-down

;; - PEL: Protected keyboard-macro definitions
;; -------------------------------------------
(global-set-key (kbd "<f3>") 'pel-kmacro-start-macro-or-insert-counter)

;; -----------------------------------------------------------------------------
;; - Function Keys - <f5>
;; ----------------------
;; Bind repeat to a single key: <f5> and <S-F5>
(global-set-key (kbd "<f5>") 'repeat)
;; <S-f5> is also bound to repeat but also marks.

;; -----------------------------------------------------------------------------
;; - Function Keys - <f6>
;; ----------------------
;;

(define-pel-global-prefix pel:f6 (kbd "<f6>"))
(define-key pel:f6 "l"  'pel-insert-line)
(define-key pel:f6 "F"  'pel-insert-filename)

;; Move to the beginning of next function definition (while moving forward)
;;  complements C-M-e and C-M-a
(define-key pel:f6 "n"           'pel-beginning-of-next-defun)
(define-key pel:f6 "p"           'beginning-of-defun)

;; (kbd "<tab>") does not work in terminal mode, it works only in graphics mode
(define-key pel:f6 (kbd "C-i")       'pel-insert-c-indent)
(define-key pel:f6 (kbd "<backtab>") 'pel-unindent)
;;

;; -----------------------------------------------------------------------------
;; - Function Keys - <f11>
;; -----------------------

;; --
;; - Function Keys - <f11> top-level prefix keys

(define-pel-global-prefix pel: (kbd "<f11>"))
(define-key pel:           "#"             'pel-toggle-mac-numlock)
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
(define-key pel: (kbd      "<M-right>")    'pel-forward-syntaxchange-start)
(define-key pel: (kbd      "<M-left>")     'pel-backward-syntaxchange-start)
(define-key pel: (kbd      "0")           #'hl-line-mode)
(when (and pel-use-popup-kill-ring
           (display-graphic-p))
  (define-key pel:         "y"            #'popup-kill-ring))

;; In graphics mode, bindings to go directly to another frame
;; without having to move through all intervening windows in current
;; frame.
(when (display-graphic-p)
  (when pel-use-framemove
    (define-key pel: (kbd  "<S-down>")     'fm-down-frame)
    (define-key pel: (kbd  "<S-up>")       'fm-up-frame)
    (define-key pel: (kbd  "<S-left>")     'fm-left-frame)
    (define-key pel: (kbd  "<S-right>")    'fm-right-frame)))
;;
(define-key pel: (kbd      "<f11>")        'pel-toggle-frame-fullscreen)
(unless (display-graphic-p)
  (define-key pel: (kbd    "<f12>")       #'xterm-mouse-mode))
;;

;; Bind <f11>/<f12> key-chords as extension of down/up cursors.
;; These are keyboard mnemonics for lower and higher volume
;; respectively, on macOS keyboards.
;; TODO: keep or remove the following now that we have the identified
;;       corresponding:  pel-backward-token-start and
;;                    :  pel-forward-token-start
(global-set-key (kbd "<C-f11>") 'pel-previous-visible) ;
(global-set-key (kbd "<C-f12>") 'pel-next-visible)

;; -----------------------------------------------------------------------------
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
      (cl-eval-when 'compile (require 'undo-tree))
      (use-package undo-tree
        :ensure t
        :pin gnu
        :commands (undo-tree-undo
                   undo-tree-redo
                   undo-tree-visualize
                   undo-tree-switch-branch)
        :init
        ;; PEL doesn't call (global-undo-tree-mode) to preserve the
        ;; binding of C-- and C-_ to negative-argument.  Instead,
        ;; create explicit bindings to the keys for the undo.
        (global-set-key (kbd "C-z")  'undo-tree-undo)
        (when (display-graphic-p)
          (global-set-key (kbd  "s-z")    #'undo-tree-undo)
          (global-set-key (kbd  "s-Z")    #'undo-tree-redo))
        (global-set-key (kbd    "C-x u")  #'undo-tree-undo)
        (global-set-key (kbd    "C-/")    #'undo-tree-undo)
        (global-set-key (kbd    "M-u")    #'undo-tree-undo)
        (global-set-key (kbd    "M-U")    #'undo-tree-redo)

        (define-key pel:undo    "u"       #'undo-tree-undo)
        (define-key pel:undo    "r"       #'undo-tree-redo)
        (define-key pel:undo    "v"       #'undo-tree-visualize)
        (define-key pel:undo    "x"       #'undo-tree-switch-branch)))

  ;; When pel-use-undo-tree is not t, then use standard Emacs undo but
  ;; map to similar keys (except the ``<f11> u``)
  (when (display-graphic-p)
    (global-set-key (kbd  "s-z")    #'undo))
  (global-set-key (kbd    "C-x u")  #'undo)
  (global-set-key (kbd    "C-/")    #'undo)
  (global-set-key (kbd    "M-u")    #'undo)
  (define-key pel:undo    "u"       #'undo))

;; - Use goto-last-change
;; ----------------------
(when pel-use-goto-last-change
  (cl-eval-when 'compile (require 'goto-last-change))
  (use-package goto-last-change
    :ensure t
    :pin melpa
    :commands goto-last-change)
  (define-key pel:undo "\\"  #'goto-last-change))

;; -----------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> <f10>`` : Menu commands
;; Force load of pel-imenu after load of imenu: pel-imenu-init is identified as
;; an autoload, and it configures the imenu system.

(eval-after-load 'imenu
  (when (fboundp 'pel-imenu-init)
    (pel-imenu-init)))

(define-pel-global-prefix pel:menu (kbd "<f11> <f10>"))
(define-key pel:menu "B"     #'menu-bar-mode)
(define-key pel:menu "I"     #'imenu-add-menubar-index)
(define-key pel:menu "i"     #'imenu)
(define-key pel:menu "o"      'pel-toggle-imenu-index-follows-order)
(define-key pel:menu "t"     #'tmm-menubar)

;; -----------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> SPC`` : C programming utilities

(define-pel-global-prefix pel:for-C (kbd "<f11> SPC c"))
(define-key pel:for-C    "." 'pel-find-thing-at-point)
(define-key pel:for-C    "(" #'show-paren-mode)
(define-key pel:for-C    ")" #'check-parens)
(when pel-use-rainbow-delimiters
  (define-key pel:for-C  "R"  'rainbow-delimiters-mode))
;;
(pel--mode-hook-maybe-call
 '(lambda ()
    (local-set-key (kbd "<f12>") 'pel:for-C))
 'c-mode 'c-mode-hook)

;; -----------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> SPC`` : C++ programming utilities

(define-pel-global-prefix pel:for-C++ (kbd "<f11> SPC C"))
(define-key pel:for-C++      "."  'pel-find-thing-at-point)
(define-key pel:for-C++      "(" #'show-paren-mode)
(define-key pel:for-C++      ")" #'check-parens)
(when pel-use-rainbow-delimiters
  (define-key pel:for-C++    "R"  'rainbow-delimiters-mode))
;;
(pel--mode-hook-maybe-call
 '(lambda ()
    (local-set-key (kbd "<f12>") 'pel:for-C++))
 'c++-mode 'c++-mode-hook)

;; -----------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> SPC`` : Erlang programming utilities
(when pel-use-erlang
  (define-pel-global-prefix pel:for-erlang (kbd "<f11> SPC e"))
  ;;
  (pel--mode-hook-maybe-call
   '(lambda ()
      (local-set-key (kbd "<f12>") 'pel:for-erlang))
   'erlang-mode 'erlang-mode-hook))

;; ----------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> SPC`` : Emacs Lisp programming

;; - Use parinfer
;; --------------
(when pel-use-parinfer
  (cl-eval-when 'compile (require 'parinfer))
  (use-package parinfer
    :ensure t
    :pin melpa
    :commands (parinfer-mode
               parinfer-toggle-mode
               parinfer-auto-fix
               parinfer-diff)))

;; - Use rainbow-delimiters
;; ------------------------
(when pel-use-rainbow-delimiters
  (cl-eval-when 'compile (require 'rainbow-delimiters))
  (use-package rainbow-delimiters
    :ensure t
    :pin melpa
    :commands rainbow-delimiters-mode))
;; rainbow-delimiters-max-face-count 9
;;  rainbow-delimiters-max-face-count identifies max depth where colours
;; are cycled;
;; it's default value is 9.  That should be more than enough.
;; The color of the parentheses are identified by the variables
;; rainbow-delimiters-depth-X-face  (where 'X' is a digit between 1 and
;; 9 included.)
;; These variables should be defined inside the init.el file or
;; customization data file.

;; -----------------------------------------------------------------------------

(define-pel-global-prefix pel:for-elisp (kbd "<f11> SPC l"))
;;
(define-key pel:for-elisp   "."  'pel-find-thing-at-point)
(define-key pel:for-elisp   "D" #'toggle-debug-on-error)
(when pel-use-parinfer
  (define-key pel:for-elisp "i" 'parinfer-auto-fix))

(define-pel-global-prefix pel:elisp-analyze (kbd "<f11> SPC l a"))
(define-key pel:elisp-analyze ")" #'check-parens)
(when pel-use-parinfer
  (define-key pel:elisp-analyze "D"  'parinfer-diff))
(define-key pel:elisp-analyze   "b"  'pel-lint-elisp-file)
(define-key pel:elisp-analyze   "d" #'checkdoc)
(define-key pel:elisp-analyze   "f" #'elint-file)

(define-pel-global-prefix pel:elisp-compile (kbd "<f11> SPC l c"))
(define-key pel:elisp-compile "b"  'pel-byte-compile-file-and-load)
(define-key pel:elisp-compile "d" #'byte-recompile-directory)
(define-key pel:elisp-compile "f" #'byte-compile-file)

(define-pel-global-prefix pel:elisp-debug (kbd "<f11> SPC l d"))
(define-key pel:elisp-debug "d" #'debug-on-entry)
(define-key pel:elisp-debug "D" #'cancel-debug-on-entry)
(define-key pel:elisp-debug "!" #'toggle-debug-on-error)
(define-key pel:elisp-debug ")" #'toggle-debug-on-quit)
(define-key pel:elisp-debug "e" #'edebug-defun)

(define-pel-global-prefix pel:elisp-eval (kbd "<f11> SPC l e"))
(define-key pel:elisp-eval "b" #'eval-buffer)
(define-key pel:elisp-eval "f" #'load-file)
(define-key pel:elisp-eval "r" #'eval-region)

(define-pel-global-prefix pel:elisp-function (kbd "<f11> SPC l f"))
(define-key pel:elisp-function "n" 'pel-beginning-of-next-defun)
(define-key pel:elisp-function "p" 'beginning-of-defun)

(define-pel-global-prefix pel:elisp-lib (kbd "<f11> SPC l l"))
(define-key pel:elisp-lib "L" #'load-library)  ; Load an elisp file.
(define-key pel:elisp-lib "l" #'find-library)  ; Open the elisp library file
(define-key pel:elisp-lib "c" #'locate-library)
(define-key pel:elisp-lib "p" #'list-packages)

(define-pel-global-prefix pel:elisp-mode (kbd "<f11> SPC l m"))
(define-key pel:elisp-mode      "(" #'show-paren-mode)
(define-key pel:elisp-mode      "L"  'pel-toggle-lisp-modes)
(when pel-use-parinfer
  (define-key pel:elisp-mode    "I"  'parinfer-mode)
  (define-key pel:elisp-mode    "i"  'parinfer-toggle-mode))
(when pel-use-rainbow-delimiters
  (define-key pel:elisp-mode    "R"  'rainbow-delimiters-mode))
(define-key pel:elisp-mode      "s" #'semantic-mode)

(when pel-use-macrostep
  (cl-eval-when 'compile (require 'macrostep))
  (use-package macrostep
    :ensure t
    :pin melpa
    :commands macrostep-expand
    :init
    (define-key pel:elisp-mode    "m" #'macrostep-expand)))

(when pel-use-highlight-defined
  (cl-eval-when 'compile (require 'highlight-defined))
  (use-package highlight-defined
    :ensure t
    :pin melpa
    :commands highlight-defined-mode
    :init
    (define-key pel:elisp-mode  "d" 'highlight-defined-mode)))

;; Schedule the context sensitive menu
(pel--mode-hook-maybe-call
 '(lambda ()
    (local-set-key (kbd "<f12>")   'pel:for-elisp)
    (local-set-key (kbd "<f12> a") 'pel:elisp-analyze)
    (local-set-key (kbd "<f12> c") 'pel:elisp-compile)
    (local-set-key (kbd "<f12> d") 'pel:elisp-debug)
    (local-set-key (kbd "<f12> e") 'pel:elisp-eval)
    (local-set-key (kbd "<f12> f") 'pel:elisp-function)
    (local-set-key (kbd "<f12> l") 'pel:elisp-lib)
    (local-set-key (kbd "<f12> m") 'pel:elisp-mode))
 'emacs-lisp-mode 'emacs-lisp-mode-hook :append)

;; -----------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> SPC`` : (Common) Lisp programming
(when pel-use-common-lisp
  (define-pel-global-prefix pel:for-lisp (kbd "<f11> SPC L"))
  (define-key pel:for-lisp    "(" #'show-paren-mode)
  (define-key pel:for-lisp    ")" #'check-parens)
  (when pel-use-parinfer
    (define-key pel:for-lisp  "I"  'parinfer-mode)
    (define-key pel:for-lisp  "J"  'parinfer-toggle-mode))
  (define-key pel:for-lisp    "m"  'pel-toggle-lisp-modes)
  (when pel-use-rainbow-delimiters
    (define-key pel:for-lisp  "R"  'rainbow-delimiters-mode))
  (define-key pel:for-lisp    "S" #'semantic-mode)
  ;;
  (pel--mode-hook-maybe-call
   '(lambda ()
      (local-set-key (kbd "<f12>") 'pel:for-lisp))
   'lisp-mode 'lisp-mode-hook))

;; -----------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> SPC`` : Python programming utilities
(when pel-use-python
  (define-pel-global-prefix pel:for-python (kbd "<f11> SPC p"))
  (define-key pel:for-python    "."  'pel-find-thing-at-point)
  (define-key pel:for-python    "(" #'show-paren-mode)
  (when pel-use-rainbow-delimiters
    (define-key pel:for-python  "R"  'rainbow-delimiters-mode))
  ;;
  (pel--mode-hook-maybe-call
   '(lambda ()
      (local-set-key (kbd "<f12>")  'pel:for-python))
   'python-mode 'python-mode-hook))

;; -----------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> SPC`` : reSTucturedText
(when pel-use-rst-mode
  (define-pel-global-prefix pel:for-reST (kbd "<f11> SPC r"))
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
  (define-key pel:for-reST "p" 'rst-backward-section)
  ;;
  (define-pel-global-prefix pel:rst-adorn-style (kbd "<f11> SPC r A"))
  (define-key pel:rst-adorn-style "d" 'pel-rst-adorn-default)
  (define-key pel:rst-adorn-style "S" 'pel-rst-adorn-Sphinx-Python)
  (define-key pel:rst-adorn-style "C" 'pel-rst-adorn-CRiSPer)
  ;;
  (pel--mode-hook-maybe-call
   '(lambda ()
      (local-set-key (kbd "<f12>") 'pel:for-reST))
   'rst-mode 'rst-mode-hook))

;; -----------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> SPC`` : Graphviz Dot
(when pel-use-graphviz-dot
  (cl-eval-when 'compile (require 'graphviz-dot-mode))
  (use-package graphviz-dot-mode
    :ensure t
    :pin melpa
    :commands graphviz-dot-mode)

  (define-pel-global-prefix pel:for-graphviz-dot (kbd "<f11> SPC g"))
  (define-key pel:for-graphviz-dot "c" 'compile)
  ;;
  (pel--mode-hook-maybe-call
   '(lambda ()
      (local-set-key (kbd "<f12">) 'pel:for-graphviz-dot))
   'graphviz-dot-mode 'graphviz-dot-mode-hook))

;; -----------------------------------------------------------------------------
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

;; -----------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> -`` : Kill commands

(define-pel-global-prefix pel:kill (kbd "<f11> -"))
(define-key pel:kill " "  'pel-kill-whitespace-at-point)
(define-key pel:kill "("  'pel-kill-list-at-point)
(define-key pel:kill "*" #'delete-duplicate-lines)
(define-key pel:kill "."  'pel-kill-symbol-at-point)
(define-key pel:kill ";"  'pel-kill-all-comments)
(define-key pel:kill "a"  'pel-kill-from-beginning-of-line)
(define-key pel:kill "b" #'backward-kill-paragraph)
(define-key pel:kill "c"  'pel-kill-char-at-point)
(define-key pel:kill "e" #'kill-line)
(define-key pel:kill "F"  'pel-kill-filename-at-point)
(define-key pel:kill "f"  'pel-kill-function-at-point)
(define-key pel:kill "H"  'pel-kill-paragraph-at-point)
(define-key pel:kill "h" #'kill-paragraph)
(define-key pel:kill "l"  'pel-kill-or-delete-marked-or-whole-line)
(define-key pel:kill "r" #'kill-rectangle)
(define-key pel:kill "s"  'pel-kill-sentence-at-point)
(define-key pel:kill "u"  'pel-kill-url-at-point)
(define-key pel:kill "w"  'pel-kill-word-at-point)
(define-key pel:kill "x"  'pel-kill-sexp-at-point)
(define-key pel:kill "]" #'kill-sexp)
(define-key pel:kill "[" #'backward-kill-sexp)

(if (display-graphic-p)
    (global-set-key (kbd "s-x") 'pel-kill-or-delete-marked-or-whole-line))
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

;; -----------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> ,`` : auto-completion

(when pel-use-auto-complete
  ;; Defer loading of auto-complete using its autoload that will be
  ;; trigerred when the one of the pel-auto-complete-mode or
  ;; pel-global-auto-complete-mode is executed.
  (cl-eval-when 'compile (require 'auto-complete))
  (use-package auto-complete
    :ensure t
    :pin melpa
    :commands (auto-complete-mode global-auto-complete-mode)))

(when pel-use-company
  ;; Defer-load company.el via the autoload company-mode and
  ;; global-autoload-mode are called by one of the pel functions.
  (cl-eval-when 'compile (require 'company))
  (use-package company
    :ensure t
    :pin melpa
    :commands (company-mode global-company-mode)))

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

;; -----------------------------------------------------------------------------
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
(define-key pel:mark       "h"        #'mark-paragraph)
(define-key pel:mark       "p"        #'mark-page)
(define-key pel:mark       "r"         'pel-cua-rectangle-mark)
(define-key pel:mark       "w"        #'mark-word)
(define-key pel:mark       "x"        #'mark-sexp)
;;
(global-set-key (kbd "M-`")            'pel-jump-to-mark)
(global-set-key (kbd "M-S-<up>")       'pel-mark-line-up)
(global-set-key (kbd "M-S-<down>")     'pel-mark-line-down)

(when pel-use-expand-region
  (cl-eval-when 'compile (require 'expand-region))
  (use-package expand-region
    :ensure t
    :pin melpa
    :commands er/expand-region
    :init
    (define-key pel:mark     "="  'er/expand-region)
    (global-set-key   (kbd "M-=") 'er/expand-region)))

;; -----------------------------------------------------------------------------
;; CUA mode setup
;; Activate the ability to use cua-rectangle-mark-mode without using
;; the CUA re-binding of C_c, C-v, C-x and C-z.

;; TODO: Keep this?  Fix the pel-cua-rectangle-mark??
(global-set-key (kbd "S-<f11>") #'cua-rectangle-mark-mode)
(global-set-key (kbd "<f11> [")  'pel-cua-move-rectangle-left)
(global-set-key (kbd "<f11> ]")  'pel-cua-move-rectangle-right)

;; -----------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> ;`` : comment commands

(define-pel-global-prefix pel:comment (kbd "<f11> ;"))
;;
(define-key pel:comment "A"    'pel-toggle-comment-auto-fill-only-comments)
(define-key pel:comment "B"           #'comment-box)
(define-key pel:comment (kbd  "DEL")   'pel-delete-all-comments)
(define-key pel:comment "k"           #'comment-kill)
(define-key pel:comment "l"           #'comment-line)
(define-key pel:comment "b"            'pel-comment-start)
(define-key pel:comment "m"            'pel-comment-middle)
(define-key pel:comment "e"            'pel-comment-end)
(define-key pel:comment "u"            'uncomment-region)

;; -----------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> ?`` : Help /apropos/info commands

(define-pel-global-prefix pel:help (kbd "<f11> ?"))
(define-key pel:help "m"  #'man)
(define-key pel:help "M"  #'woman)

;; -----------------------------------------------------------------------------
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

;; -----------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> ? i`` : Help Info commands

(define-pel-global-prefix pel:info (kbd "<f11> ? i"))
(define-key pel:info "a"  #'info-apropos)
(define-key pel:info "i"  #'info)
(define-key pel:info "m"  #'info-display-manual)

;; -----------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> ? d`` : Describe

(define-pel-global-prefix pel:describe (kbd "<f11> ? d"))
(define-key pel:describe "$"  'pel-spell-show-use)
(define-key pel:describe "c" #'list-colors-display)
(define-key pel:describe "F" #'list-faces-display)
(define-key pel:describe "f"  'pel-show-window-filename-or-buffer-name)
(define-key pel:describe "H" #'list-command-history)
(define-key pel:describe "i" #'list-input-methods)
(define-key pel:describe "k"  'pel-show-kill-ring)
(define-key pel:describe "l" #'what-line)
(define-key pel:describe "p" #'what-cursor-position)
(define-key pel:describe "s"  'pel-show-char-syntax)
(define-key pel:describe "w"  'pel-show-window-sizes)

(defun pel-show-kill-ring ()
  "Display content of `kill-ring' in *Help* buffer.
Simple shortcut to invoke `describe-variable' on the `kill-ring' variable."
  (interactive)
  (describe-variable 'kill-ring))

;; -----------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> ? e`` : Emacs info

(defun pel-emacs-load-stats ()
  "Display number of loaded files & features."
  (interactive)
  (message "\
# loaded files: %d
# features    : %d"
           (length load-history)
           (length features)))

(define-pel-global-prefix pel:emacs (kbd "<f11> ? e"))
(define-key pel:emacs "l"  'pel-emacs-load-stats)
(define-key pel:emacs "s" #'list-load-path-shadows)
(define-key pel:emacs "t" #'emacs-init-time)
(define-key pel:emacs "v" #'emacs-version)

;; -----------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> ? k`` : Info on Keys
(when pel-use-free-keys
  (cl-eval-when 'compile (require 'free-keys))
  (use-package free-keys
    :ensure t
    :pin melpa
    :commands free-keys))                       ; for <f11> ? k f

(when pel-use-bind-key
  (cl-eval-when 'compile (require 'bind-key))
  (use-package bind-key
    :ensure t
    :pin melpa
    :commands describe-personal-keybindings))   ; for <f11> ? k b

(when pel-use-which-key
  (cl-eval-when 'compile (require 'which-key))
  (use-package which-key                  ; for <f11> ? k k
    ;; List key completions: help show the f11 bindings.
    ;; When requested, delay a little to speed init time.
    ;; Note that "<f11> ? k k" will execute autoloaded
    ;; command which-key-show-major-mode which will force
    ;; loading and ensure the key mode if it's not already loaded.
    :ensure t
    :pin melpa
    :defer 1
    :commands which-key-mode
    :config
    (declare-function which-key-mode "which-key")
    (which-key-mode)))


(define-pel-global-prefix pel:keys (kbd "<f11> ? k"))
(define-key pel:keys    "#"  'pel-show-mac-numlock)
(when pel-use-bind-key
  (define-key pel:keys  "b" #'describe-personal-keybindings))
(when pel-use-free-keys
  (define-key pel:keys  "f" #'free-keys))
(when pel-use-which-key
  (define-key pel:keys  "k"  'which-key-show-major-mode))
(define-key pel:keys    "l" #'view-lossage)
(define-key pel:keys    "m" #'describe-mode)
;;

;; -----------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> $`` : Spell Check

;; popup is used in Terminal mode for spell check menu,
;; and must be available when pel-spell-init is called.
(unless (display-graphic-p)
  (cl-eval-when 'compile (require 'popup))
  (use-package popup
    :ensure t
    :pin melpa-stable
    :commands pel-spell-init))


(define-pel-global-prefix pel:spell (kbd "<f11> $"))
;;
(autoload 'ispell-check-version "ispell")

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

;; -----------------------------------------------------------------------------
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
(when pel-use-bm
  (define-key pel:bookMark "t"  'bm-toggle) ; toggle visible bookmark
  (define-key pel:bookMark "n"  'bm-next)
  (define-key pel:bookMark "p"  'bm-previous))

;; -----------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> <tab>`` : indentation

;; More powerful indent-rigidly: pel-indent-rigidly
;; ------------------------------------------------
;; the pel-indent-rigidly does the same as the original command
;; but also allow indenting the current line even if nothing is marked.
(global-set-key [remap indent-rigidly] 'pel-indent-rigidly)

;; Add indented line below
(global-set-key (kbd "<M-RET>") 'pel-newline-and-indent-below)

(define-pel-global-prefix pel:indent (kbd "<f11> TAB"))
(define-key pel:indent "r"            #'indent-relative)
(define-key pel:indent "c"             'pel-insert-c-indent)
(define-key pel:indent "C"             'pel-unindent)
(define-key pel:indent (kbd "TAB")     'pel-indent-rigidly)
(define-key pel:indent (kbd "<RET>")   'pel-newline-and-indent-below)

(global-set-key (kbd "<backtab>") 'pel-unindent)

;; -----------------------------------------------------------------------------
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
;;   - Meta f11/f12 in org-mode, since Meta up/down do something else.

;; scroll text up: toward small line number
(global-set-key (kbd "<M-down>")  'pel-scroll-up)
(global-set-key (kbd "<M-f11>")   'pel-scroll-up)
;; scroll text down: toward large line number
(global-set-key (kbd "<M-up>")    'pel-scroll-down)
(global-set-key (kbd "<M-f12>")   'pel-scroll-down)

(define-pel-global-prefix pel:scroll (kbd "<f11> |"))
;;
(define-key pel:scroll "|"  'pel-toggle-scroll-sync)
(define-key pel:scroll "+"  'pel-add-window-to-scroll-sync)
(define-key pel:scroll "-"  'pel-remove-window-from-scroll-sync)
(define-key pel:scroll "a" #'scroll-all-mode)
(define-key pel:scroll "f" #'follow-mode)
(define-key pel:scroll "l" #'scroll-lock-mode)

(when pel-use-smooth-scrolling
  (cl-eval-when 'compile (require 'smooth-scrolling))
  (use-package smooth-scrolling
    :ensure t
    :pin melpa
    :defer 2
    :init
    (if (fboundp 'smooth-scrolling-mode)
        (define-key pel:scroll "s" 'smooth-scrolling-mode))
    :config
    (if (fboundp 'smooth-scrolling-mode)
        (smooth-scrolling-mode 1))))

;; -----------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> a`` : abbreviations

(define-pel-global-prefix pel:abbrev (kbd "<f11> a"))
(define-key pel:abbrev "d"  'pel-define-abbrevs)
(define-key pel:abbrev "e" #'expand-abbrev)
(define-key pel:abbrev "E" #'expand-region-abbrevs)
(define-key pel:abbrev "i" #'insert-abbrevs)
(define-key pel:abbrev "l" #'list-abbrevs)
(define-key pel:abbrev "m" #'edit-abbrevs)
(define-key pel:abbrev "r" #'read-abbrev-file)
(define-key pel:abbrev "s" #'write-abbrev-file)
(define-key pel:abbrev "u" #'unexpand-abbrev)

(defun pel-define-abbrevs (&optional arg)
  "Read abbreviations from current buffer after confirming with user.
With argument ARG , eliminate all abbrev definitions except
the ones defined from the buffer now."
  (interactive "P")
  (if (yes-or-no-p "Read abbreviations from current buffer? ")
      (define-abbrevs arg)
    (message "Nothing done.")))

;; -----------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> b`` : buffer commands

(define-pel-global-prefix pel:buffer (kbd "<f11> b"))
(define-key pel:buffer "-"  #'ruler-mode)
(define-key pel:buffer "c"  #'clone-buffer)
(define-key pel:buffer "i"  #'insert-file)
(define-key pel:buffer "k"  #'kill-current-buffer)
(define-key pel:buffer "l"   'pel-switch-to-last-used-buffer)
(define-key pel:buffer "n"  #'next-buffer)
(define-key pel:buffer "P"   'pel-show-window-previous-buffer)
(define-key pel:buffer "p"  #'previous-buffer)
(define-key pel:buffer "r"  #'read-only-mode)
(define-key pel:buffer "v"  #'view-buffer)
;; Reserved            "x"   (see declarations below with pel-use-nhexl-mode)
;; Reserved            "X"

;; -----------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> b h`` : buffer highlighting commands

(defun pel-hi-lock-find-patterns ()
  "Execute hi-lock-find-patterns when `hi-lock-mode' is active."
  (interactive)
  (declare-function hi-lock-find-patterns "hi-lock")
  (if (fboundp 'hi-lock-find-patterns)
      (hi-lock-find-patterns)
    (user-error "Turn hi-lock-mode on first")))

(define-pel-global-prefix pel:highlight (kbd "<f11> b h"))
(define-key pel:highlight "-" #'hl-line-mode)
(define-key pel:highlight "(" #'show-paren-mode)

(define-key pel:highlight      "."  #'highlight-symbol-at-point)
(define-key pel:highlight      "C"  #'highlight-changes-mode)
(define-key pel:highlight      "c"   'pel-set-highlight-color)
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
(when pel-use-vline
  (define-key pel:highlight    "v"  'vline-mode))
(define-key pel:highlight      "w"  #'hi-lock-write-interactive-patterns)
;;

;; -----------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> b I`` : Indirect buffer commands

(define-pel-global-prefix pel:indirect-buffer (kbd "<f11> b I"))
(define-key pel:indirect-buffer "c"  #'clone-indirect-buffer)
(define-key pel:indirect-buffer "m"  #'make-indirect-buffer)
(define-key pel:indirect-buffer "w"  #'clone-indirect-buffer-other-window)
;;

;; -----------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> c`` : count things

(define-pel-global-prefix pel:count (kbd "<f11> c"))
(define-key pel:count "m" #'count-matches)
(define-key pel:count "p" #'count-lines-page)
(define-key pel:count "W" #'count-words-region)
(define-key pel:count "w" #'count-words)
;;

;; -----------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> C`` : clipboard commands

(define-pel-global-prefix pel:clipboard (kbd "<f11> C"))
(define-key pel:clipboard "c" #'clipboard-kill-ring-save)
(define-key pel:clipboard "x" #'clipboard-kill-region)
(define-key pel:clipboard "v" #'clipboard-yank)
;;

;; -----------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> d`` : draw commands

(define-pel-global-prefix pel:draw (kbd "<f11> d"))
(define-key pel:draw "a"  'artist-mode)       ; toggle artist-mode
(define-key pel:draw "p"  'picture-mode)      ; activate picture-mode

;; -----------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> f`` : File operations

(defun pel-auto-revert-set-timer ()
  "Execute auto-revert-set-timer if `auto-revert-mode' is active."
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
(define-key pel:file "A" #'auto-revert-mode)
(define-key pel:file " "  'pel-auto-revert-set-timer) ; cancel/restart the timer
(define-key pel:file "d" #'find-dired)
(define-key pel:file "g" #'find-grep)
(define-key pel:file "h" #'find-grep-dired)
(define-key pel:file "l" #'find-lisp-find-dired)
(define-key pel:file "n" #'find-name-dired)
(define-key pel:file "o" #'find-file-other-window)
(define-key pel:file "r" #'find-file-read-only-other-window)
(define-key pel:file "R" #'revert-buffer)
(define-key pel:file "T" #'auto-revert-tail-mode)
(define-key pel:file "t" #'time-stamp)
;;

;; - Open file at point
;; --------------------
(global-set-key (kbd "C-^") 'pel-find-file-at-point-in-window)

;; -----------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> f v`` : File variables

(define-pel-global-prefix pel:filevar (kbd "<f11> f v"))
(define-key pel:filevar "="  #'add-file-local-variable-prop-line)
(define-key pel:filevar "-"  #'delete-file-local-variable-prop-line)
(define-key pel:filevar "c"  #'copy-dir-locals-to-file-locals-prop-line)
;;

;; -----------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> f v D`` : Directory File variables

(define-pel-global-prefix pel:dirvar (kbd "<f11> f v D"))
(define-key pel:dirvar "="  #'add-dir-local-variable)
(define-key pel:dirvar "-"  #'delete-dir-local-variable)
(define-key pel:dirvar "C"  #'copy-file-locals-to-dir-locals)
;;

;; -----------------------------------------------------------------------------
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
;;

;; -----------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> g`` : Grep operations

(define-pel-global-prefix pel:grep (kbd "<f11> g"))
(declare-function kill-grep "grep")
(define-key pel:grep      "f"  #'find-grep)
(define-key pel:grep      "g"  #'grep)
(define-key pel:grep      "k"  #'kill-grep)
(define-key pel:grep      "l"  #'lgrep)
(define-key pel:grep      "r"  #'rgrep)  ; execute recursive grep
(define-key pel:grep      "z"  #'zrgrep)
;;

;; ripgrep - a faster grep easier to use than grep.
(when  pel-use-ripgrep
  (cl-eval-when 'compile (require 'rg))
  (use-package rg
    :ensure t
    :pin melpa
    :commands (rg rg-literal)
    :init
    (define-key pel:grep  "I"  'rg-literal)
    (define-key pel:grep  "i"  'rg)
    :config
    (declare-function rg-enable-default-bindings "rg")
    (rg-enable-default-bindings)))

;; -----------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> i`` : Insert text operations

(define-pel-global-prefix pel:insert (kbd "<f11> i"))
(define-key pel:insert   "C" 'copyright)
(define-key pel:insert   "d" 'pel-insert-current-date)
(define-key pel:insert   "D" 'pel-insert-current-date-time)
(define-key pel:insert   "F" 'pel-insert-filename)
(define-key pel:insert   "l" 'pel-insert-line)
(define-key pel:insert   "t" 'pel-insert-iso8601-timestamp)
(when pel-use-lice
  (cl-eval-when 'compile (require 'lice))
  (use-package lice
    :ensure t
    :pin melpa
    :commands lice
    :init
    (define-key pel:insert "L" 'lice)
    (define-key pel:f6 "L" 'lice)))

;; -----------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> k`` : Keyboard macro operations

(define-pel-global-prefix pel:kbmacro (kbd "<f11> k"))
(define-key pel:kbmacro "k"   'pel-forget-recorded-keyboard-macro)
(define-key pel:kbmacro "i"  #'insert-kbd-macro)

;; -----------------------------------------------------------------------------
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

;; -----------------------------------------------------------------------------
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
(define-key pel:search-replace "o" #'multi-occur)
(define-key pel:search-replace "O" #'multi-occur-in-matching-buffers)
(define-key pel:search-replace "r" #'replace-string)

(define-pel-global-prefix pel:search-lax (kbd "<f11> s l"))
(define-key pel:search-lax "w"  #'word-search-forward-lax)
(define-key pel:search-lax "W"  #'word-search-backward-lax)

(define-pel-global-prefix pel:search-mode (kbd "<f11> s m"))
(define-key pel:search-mode "?"   'pel-show-search-case-state)
(define-key pel:search-mode "f"   'pel-toggle-case-fold-search)
(define-key pel:search-mode "u"   'pel-toggle-search-upper-case)
(define-key pel:search-mode "l"  #'isearch-toggle-lax-whitespace)

;; -----------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> s x`` : Search Regexp commands

(define-pel-global-prefix pel:regexp (kbd "<f11> s x"))
;; add it here because C-M-% cannot be typed in terminal mode
(define-key pel:regexp      "q"  #'query-replace-regexp)
(define-key pel:regexp      "r"  #'replace-regexp)
;;

;; Regular expressions: Re-Builder.  Activate the 'string syntax so we only
;; need one backslash instead of the two required by the 'read syntax.
;; To activate Re-Builder, do:  M-x re-builder
(when pel-use-re-builder
  ;; re-builder is part of standard Emacs distribution.
  (cl-eval-when 'compile (require 're-builder))
  (use-package re-builder
    ;; autoload it when one of the following commands is used.
    :commands re-builder

    ;; run following command before package is loaded to
    ;; activate the autoload.
    :init
    (define-key pel:regexp    "b"  #'re-builder)
    (pel-setq reb-re-syntax 'string)))

;; -----------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> S`` : Speedbar/SR-Speedbar commands

(when pel-use-speedbar
  (cl-eval-when 'compile (require 'sr-speedbar))
  (use-package sr-speedbar
    :ensure t
    :pin melpa
    :commands (sr-speedbar-toggle
               sr-speedbar-window-p))


  (define-pel-global-prefix pel:speedbar (kbd "<f11> S"))
  (define-key pel:speedbar "S"  'pel-open-close-speedbar)
  (define-key pel:speedbar "."  'pel-toggle-to-speedbar)
  (define-key pel:speedbar "R"  'pel-speedbar-toggle-refresh)
  (define-key pel:speedbar "r"  'pel-speedbar-refresh)
  (define-key pel:speedbar "a"  'pel-speedbar-toggle-show-all-files)
  (define-key pel:speedbar "o"  'pel-speedbar-toggle-sorting)
  ;; (define-key pel:speedbar "e"  #'speedbar-toggle-etags)
  (when (display-graphic-p)
    (define-key pel:speedbar "i" 'pel-speedbar-toggle-images)))

;; -----------------------------------------------------------------------------
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
  (cl-eval-when 'compile (require 'nhexl-mode))
  (use-package nhexl-mode
    :ensure t
    :pin gnu
    :commands (nhexl-mode
               nhexl-nibble-edit-mode
               nhexl-overwrite-only-mode)
    :init
    (define-key pel:text   "O"  #'nhexl-overwrite-only-mode)

    (define-key pel:buffer "x"  #'nhexl-mode)
    ;; Toggle nibble editing (mainly useful in nhexl mode,
    ;; but can also be used in normal mode to enter character
    ;; in hexadecimal easily)
    (define-key pel:buffer "X"  #'nhexl-nibble-edit-mode)))


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

;; -----------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> t a``: Text align

(define-pel-global-prefix pel:align (kbd "<f11> t a"))
(define-key pel:align "a" #'align)
(define-key pel:align "c" #'align-current)
(define-key pel:align "e" #'align-entire)
(define-key pel:align "l" #'align-newline-and-indent)
(define-key pel:align "r" #'align-regexp)

;; - Alias for align-regexp: ar
;; ----------------------------
;; Use M-x ar to align a region with a regular expression.
(defalias 'ar #'align-regexp)

;; -----------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> t f``: Text fill
;;
(define-pel-global-prefix pel:fill (kbd "<f11> t f"))
(define-key pel:fill "?"    'pel-show-fill-columns)
(define-key pel:fill ";"   #'fill-comment-paragraph)
(define-key pel:fill "A"   #'auto-fill-mode)
(define-key pel:fill "C"    'pel-auto-fill-only-comments)
(define-key pel:fill "f"   #'refill-mode)
(define-key pel:fill "L"   #'lisp-fill-paragraph)
(define-key pel:fill "i"   #'fill-individual-paragraphs)
(define-key pel:fill "n"   #'fill-nonuniform-paragraphs)
(define-key pel:fill "p"   #'fill-paragraph)
(define-key pel:fill "r"   #'fill-region)
(define-key pel:fill "R"   #'fill-region-as-paragraph)

;; -----------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> t j``: Text justification
;;
(define-pel-global-prefix pel:justification (kbd "<f11> t j"))
(define-key pel:justification "b" #'set-justification-full) ;'b' for "both-side"
(define-key pel:justification "c" #'set-justification-center)
(define-key pel:justification "l" #'set-justification-left)
(define-key pel:justification "n" #'set-justification-none)
(define-key pel:justification "r" #'set-justification-right)

;; -----------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> t m``: Text word modes
;;
(define-pel-global-prefix pel:textmodes (kbd "<f11> t m"))
(define-key pel:textmodes "'" #'electric-quote-local-mode)
(define-key pel:textmodes "?"  'pel-show-text-modes)
(define-key pel:textmodes "b" #'subword-mode)
(define-key pel:textmodes "d" #'delete-selection-mode)
(define-key pel:textmodes "p" #'superword-mode)
(define-key pel:textmodes "r" #'enriched-mode)
(define-key pel:textmodes "s"  'pel-toggle-sentence-end)
(define-key pel:textmodes "v" #'visible-mode)

;; -----------------------------------------------------------------------------
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

;; -----------------------------------------------------------------------------
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

;; -----------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> w`` : Windows operations
;; Use the global local winner-mode, but don't use its key bindings;
;; use some in the '<f11> w' group:

;;
(define-pel-global-prefix pel:window (kbd "<f11> w"))
(define-key pel:window    "B"  #'switch-to-buffer-other-window)
(define-key pel:window    "O"   'pel-other-window-backward)
(define-key pel:window    "b"  #'display-buffer)
(define-key pel:window    "f"  #'follow-mode)
(define-key pel:window    "v"   'pel-2-vertical-windows)
(define-key pel:window    "h"   'pel-2-horizontal-windows)
;; reserved: k, o, m, n, p, x


(when pel-use-ace-window
  (cl-eval-when 'compile (require 'ace-window))
  (use-package ace-window
    :ensure t
    :pin melpa

    :commands (ace-window
               ace-swap-window
               ace-delete-window
               ace-delete-other-windows)

    :init
    ;; move cursor to other window - 'C-x o' is normally mapped to
    ;; this function, but PEL remap it.
    (define-key pel:window  "o"  'pel-other-window)
    (define-key pel:window  "k"  'ace-delete-window)
    ;; old version of ace-window had ace-maximize-window
    ;; but newer version obsoleted that and now use ace-delete-other-windows
    ;; If you do not have it, upgrade ace-window.

    (define-key pel:window  "m"  'ace-delete-other-windows)
    (define-key pel:window  "x"  'ace-swap-window)

    ;; Replace other-window, bound to 'C-x o', to ace-window
    ;; and make the font larger - in graphics mode.
    ;; So `C-x o` shows a window number in top left corner, unless
    ;; there's only 2 windows and if with frames, in terminal mode,
    ;; the argument does not request it.
    (global-set-key [remap other-window] 'ace-window)

    :config
    (custom-set-faces
     '(aw-leading-char-face
       ((t (:inherit ace-jump-face-forward :height 3.0)))))))

;; TODO: change to use a hook before the function split-window is called and
;;       remove the hook once it is executed if that's possible.
;; The winner package should ideally be loaded just before the first
;; call to any of the window split function is called, which is: when the
;; `split-window' from window.el is called.  To do that, we'd need to use a
;; hook.  For now, we just defer the loading with a timer so it does not get
;; loaded right when Emacs is starting.
(when pel-use-winner
  (cl-eval-when 'compile (require 'winner))
  (use-package winner
    :defer 2
    :commands (winner-undo winner-redo)

    :init
    (define-key pel:window    "n"  'winner-redo)   ; next window arrangement
    (define-key pel:window    "p"  'winner-undo)   ; previous window arrangement

    :config
    ;; winner-mode default bindings use the Shift cursor keys,
    ;; this conflict with org-mode, so use the '<f11> w' bindings
    ;; instead.
    (setq winner-dont-bind-my-keys t)
    ;; turn on the global minor mode
    (declare-function winner-mode "winner")
    (winner-mode t)))

;; -----------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> w d`` : Windows dedicated operations
;;
(define-pel-global-prefix pel:window-dedicated (kbd "<f11> w d"))
(define-key pel:window-dedicated "d" 'pel-toggle-window-dedicated)
(define-key pel:window-dedicated "?" 'pel-show-window-dedicated-status)

;; -----------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> w s`` : Window size operations
;;
(define-pel-global-prefix pel:window-size (kbd "<f11> w s"))
(define-key pel:window-size "=" #'balance-windows)
(define-key pel:window-size "-" #'shrink-window-if-larger-than-buffer)
(define-key pel:window-size "V" #'enlarge-window)
(define-key pel:window-size "v" #'shrink-window)
(define-key pel:window-size "H" #'enlarge-window-horizontally)
(define-key pel:window-size "h" #'shrink-window-horizontally)

;; -----------------------------------------------------------------------------
;; - Function Keys - <f11> - Prefix ``<f11> x`` : Process eXecution utilities
;;
(define-pel-global-prefix pel:eXecute (kbd "<f11> x"))

(declare-function eshell "eshell")

(define-key pel:eXecute    "a" #'ansi-term)
(define-key pel:eXecute    "e" #'eshell)
(define-key pel:eXecute    "i" #'ielm)
(define-key pel:eXecute    "m" #'man)
(when pel-use-python
  (define-key pel:eXecute  "p" #'run-python))
(when pel-use-erlang
  (define-key pel:eXecute  "r"  'erlang-shell))
(define-key pel:eXecute    "s" #'shell)
(define-key pel:eXecute    "t" #'term)
(define-key pel:eXecute    "w" #'woman)

;; -----------------------------------------------------------------------------
(provide 'pel_keys)

;;; pel_keys.el ends here

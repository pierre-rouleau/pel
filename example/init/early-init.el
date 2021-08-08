;; -*- lexical-binding: t; -*-
;;
;; Emacs >= 27 support the `package-quickstart' feature which speeds-up
;; Emacs startup time by building the autoloads for all elpa external
;; packages ahead of time in a previous Emacs session.

;; The Emacs quick start mechanism is activated by the presence of a
;; early-init.el file in the user-emacs-directory.  The early-init.el
;; file is loaded very early in the startup process, before graphical
;; elements are initialized and before the package manager is
;; initialized.
;;
;; The following variables must be initialized in early-init.el:
;;
;; - `package-quickstart' must be set to t to activate the package
;;   quickstart mechanism.  Its documentation states that it can be
;;   customized, but the customized value is read too late in the
;;   process, therefore you should avoid modifying its value through
;;   customization.
;; - `package-user-dir': If you need to modify `package-user-dir' when
;;   the package quickstart is used in normal startup mode, then the
;;   value that differ from the default must be set inside early-init.el
;;
;; - `package-load-list': By default this is set to '(all) to specify
;;    that `package-initialize' should load the latest installed version
;;    of all packages. If you need to modify this behaviour when the
;;    package quickstart is used, set the value inside the early-init.el


;; Inform later code that package quickstart is being used.
(setq package-quickstart t)

;; Activate PEL's fast startup if environment was setup by `pel-setup-fast'.
(let ((fast-startup-setup-fname (expand-file-name "pel-setup-package-builtin-versions.el"
                                                  user-emacs-directory)))
  (when (file-exists-p fast-startup-setup-fname)
    (load (file-name-sans-extension fast-startup-setup-fname) :noerror)
    (pel-fast-startup-set-builtins)
    ;; Remember Emacs is running in PEL's fast startup mode.
    (setq pel-running-with-bundled-packages t)))

;; Init option A: independent customization for TTY & graphic modes.
;; Separate elpa directory for Emacs in graphics mode and Emacs in TTY mode.
;; Use ~/.emacs.d/elpa in TTY mode, use ~/.emacs.d/elpa-graphics in graphics mode
;; Inside early-init.el the function `display-graphic-p' does not return t for
;; Emacs running in graphics mode, so instead I use a shell script to start Emacs in
;; graphics mode and set the PEL_EMACS_IN_GRAPHICS environment variable to "1"
;; inside that shell script otherwise do not define the variable.
;;
;; To activate init option A for Emacs 27+ you must use a specialized shell
;; that sets the PEL_EMACS_IN_GRAPHICS environment variable for Emacs used
;; in graphics mode and don't set it for Emacs running in TTY mode.
(when (getenv "PEL_EMACS_IN_GRAPHICS")
  (setq package-user-dir (locate-user-emacs-file "elpa-graphics")))

;; ---------------------------------------------------------------------------

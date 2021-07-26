;; -*- lexical-binding: t; -*-
;;
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
(if (getenv "PEL_EMACS_IN_GRAPHICS")
    (progn
      (setq package-user-dir (locate-user-emacs-file "elpa-graphics"))
      (setq custom-file      (expand-file-name "emacs-customization-graphics.el"
                                               user-emacs-directory)))
  (setq custom-file (expand-file-name "emacs-customization.el"
                                      user-emacs-directory)))

;; ---------------------------------------------------------------------------

;; -*- lexical-binding: t; -*-
;;
;; Activate PEL's fast startup if environment was setup by `pel-setup-fast'.
(let ((fast-startup-setup-fname (expand-file-name "pel-setup-package-builtin-versions.el"
                                                  user-emacs-directory)))
  (when (file-exists-p fast-startup-setup-fname)
    (load (file-name-sans-extension fast-startup-setup-fname) :noerror)
    (pel-fast-startup-set-builtins)
    ;; Remember Emacs is running in PEL's fast startup mode.
    (setq pel-running-with-bundled-packages t))))

;; Separate elpa directory for Emacs in graphics mode and Emacs in TTY mode.
;; Use ~/.emacs.d/elpa in TTY mode, use ~/.emacs.d/elpa-graphics in graphics mode
;; Inside early-init.el the function `display-graphic-p' does not return t for
;; Emacs running in graphics mode, so instead I use a shell script to start Emacs in
;; graphics mode and set the PEL_EMACS_IN_GRAPHICS environment variable to "1"
;; inside that shell script.
(when (getenv "PEL_EMACS_IN_GRAPHICS")
  (setq package-user-dir (locate-user-emacs-file "elpa-graphics"))
  (setq custom-file      "~/.emacs.d/emacs-customization-graphics.el"))

;; ---------------------------------------------------------------------------

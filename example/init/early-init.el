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
;;
;; Note that inside early-init.el Emacs has not initialized its graphics
;; support code and the function `display-graphic-p' is not yet available.
;; This is why we must resort to environment variables to detect if Emacs is
;; running in graphics mode or in terminal/TTY mode.

;; ---------------------------------------------------------------------------
;; PEL OPTION A: independent customization for TTY & graphic modes.
;; ----------------------------------------------------------------
;;
;; If you want to support independent customization of Emacs running in
;; terminal/TTY and Emacs running in graphics mode you MUST set:
;;
;;  1) Set `pel-support-dual-independent-customization-p' to t below.
;;  2) Set `PEL_EMACS_IN_GRAPHICS' environment variable to 1 in
;;     a script that launch the graphics-mode Emacs from a shell.
;;
;; If you do not want to support independent customization of Emacs, leave
;; `pel-support-dual-independent-customization-p' set to nil.
;;
(defconst pel-support-dual-independent-customization-p nil
  "When t PEL uses 2 custom files: one for TTY and one for graphic mode.")

;; ---------------------------------------------------------------------------
;; PEL OPTION B: Detection of GUI-launched graphics Emacs.
;; -------------------------------------------------------
;;
;; Identify an environment variable whose presence detects that Emacs is
;; launched from a shell.  This should have the **SAME** value as what the
;; `pel-shell-detection-envvar' user option defined in pel--options.el has.
;;
;; The default is "_", the environment variable that Bash uses to identify the
;; name of the executable that launched it.  This environment variable is not
;; part of the process environment when Emacs is launched from a GUI program
;; such as macOS Finder.
;;
;; Change this value when using another shell or when running on other
;; operating system such as Windows. If you cannot find a suitable environment
;; variable that is defined when Emacs is launched, then define an environment
;; variable that will be present in all instances of your shell but not inside
;; the OS process environment. For instance you could use the environment name
;; "PEL_SHELL".
;;
(defconst pel-shell-detection-envvar "_"
  "Name of envvar used to detect that Emacs was launched by a shell.
A temporary value for user-option defined in pel--options.el")

;; ---------------------------------------------------------------------------
;; The following code does not require editing.

(defvar pel-force-graphics-specific-custom-file-p
  (and pel-support-dual-independent-customization-p
       (or (string-equal (getenv "PEL_EMACS_IN_GRAPHICS") "1")
           (not (getenv pel-shell-detection-envvar))))
  "Force independent graphics mode customization.")

;; Request use of the package quickstart feature.
(setq package-quickstart t)

;; ----
;; If Emacs is running in Graphics mode with dual independent customization
;; then ensure that package quickstart activation uses the graphics-specific
;; files.
(when pel-force-graphics-specific-custom-file-p

  (defun pel--package-activate-all-ei (original-fct)
    "Force use of controlled package-user-dir during package initialize."
    (let ((package-user-dir        (file-truename
                                    (locate-user-emacs-file
                                     "elpa-graphics")))
          (package-quickstart-file (file-truename
                                    (locate-user-emacs-file
                                     "package-quickstart-graphics.el")))
          (custom-file             (file-truename
                                    (locate-user-emacs-file
                                     "emacs-customization-graphics.el"))))
      (funcall original-fct)))
  (declare-function pel--package-activate-all-ei "early-init")

  (advice-add 'package-activate-all :around #'pel--package-activate-all-ei))

;; --
;; Activate PEL's fast startup if environment was setup by `pel-setup-fast'.
(let ((fast-startup-setup-fname (expand-file-name "pel-fast-startup-init.el"
                                                  user-emacs-directory)))
  (when (file-exists-p fast-startup-setup-fname)
    (load (file-name-sans-extension fast-startup-setup-fname) :noerror)
    (pel-fast-startup-init pel-force-graphics-specific-custom-file-p
                           :from-early-init)
    ;; Remember Emacs is running in PEL's fast startup mode.
    (setq pel-running-in-fast-startup-p t)))

;; ---------------------------------------------------------------------------

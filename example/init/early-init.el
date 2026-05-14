;;; early-init.el --- Early PEL feature control.  -*- lexical-binding: t; -*-
;;

;;; --------------------------------------------------------------------------
;;; Commentary:
;;
;; Emacs >= 27 supports the early-init.el file.  Emacs loads and executes the
;; content of the early-init.el file early in its startup process at a moment
;; where Emacs package manager and graphics support is not yet initialized. In
;; early-init the function `display-graphic-p' is not available.
;;
;; On Emacs >= 27 the package manager controlled by package.el is initialized
;; after the execution of early-init.el and before the execution of init.el.
;;
;; Therefore, on Emacs >= 27, PEL needs to execute code inside early-init.el
;; to control some of its features (listed below).
;;
;; Since Emacs has not initialized its graphics support code and the function
;; `display-graphic-p' is not yet available when early-init.el is executed,
;; the code must resort to environment variables to detect if Emacs is
;; running in graphics mode or in terminal/TTY mode.  The code uses the
;; presence of the "PEL_EMACS_IN_GRAPHICS" environment variable set to "1 to
;; identify the graphics mode when Emacs is launched from a shell and the
;; absence of an environment variable identified by the
;; `pel-early-init-shell-detection-envvar' when Emacs is launched from a GUI
;; application launcher.
;;
;; The code of this file does not load any Emacs Lisp file, it only uses what
;; is already available: the Emacs Lisp forms implemented in C and the ones
;; that are normally bundled in the Emacs dump.  This includes the files
;; subrl.el.
;;
;;
;; PEL Feature Control
;; -------------------
;;
;; 4 PEL features are controlled by early-init:
;;
;; - Package quickstart:
;;     Package quickstart, available independently of the other features.
;;     - Activated by `pel-early-init-support-package-quickstart-p' by PEL commands.
;;     - Controls the following Emacs variables:
;;       - `package-quickstart'
;;
;; - Dual environment:
;;     Whether PEL uses and supports two independent environments: one for
;;     Emacs running in terminal/TTY mode and the other running in graphics
;;     mode. Each environment uses a customization file and a set of package
;;     directories.  When the dual environment is not used only one
;;     customization file and a package directory set is used for both modes.
;;     When dual environment is used each mode uses its own set.
;;     - Activated by `pel-early-init-support-dual-environment-p' by PEL commands.
;;     - Controls the following Emacs variables:
;;       - `package-quickstart-file'
;;       - `package-user-dir'
;;       - `custom-file'
;;
;; - GUI launched Emacs:
;;     Defines the name of an environment variable whose absence indicates
;;     that Emacs was launched from a GUI file manager application like
;;     Windows Explorer, macOS Finder, or the ones available on Linux and
;;     other operating systems.  By default the "_" environment variable used
;;     by Bash is used, but another one must be used when other shells are
;;     used.  If your shell does not have such environment variable, use
;;     something like "PEL_SHELL" and define it inside your shell
;;     initialization file (something like ~/.bashrc or ~/.bash_profile).
;;     - Identified in early-init.el by `pel-early-init-shell-detection-envvar',
;;       and set by PEL command to the same value as the
;;       `pel-shell-detection-envvar' user-option.
;;
;; - PEL fast startup:
;;   Whether PEL activates the fast startup mode.
;;   - This is detected by the presence of the file "pel-fast-startup-init.el"
;;     in the user Emacs directory.
;;

;;; --------------------------------------------------------------------------
;;; Code:
;;

;; ---------------------------------------------------------------------------
;; User configuration
;; ==================
;;
;; PEL uses Emacs configuration files identified in the Emacs `custom-file'
;; user-option and appends the "-graphics" suffix to its name when dual
;; environment is used and Emacs runs in graphics mode. PEL is able to
;; automate this for almost all types of settings EXCEPT when dual environment
;; is used with package quickstart enabled.
;;
;; When package quickstart is used, the name of the customization file *must*
;; be known by this code, running in early-init.el.  However when
;; early-init.el is running code does not have access to the customization
;; user-options values; they are not yet loaded!
;;
;; To work around this limitation the value of the customization file is
;; stored inside the `pel-early-init-custom-file' variable.  The PEL's default
;; is identified.  If you want to use another customization file you must do
;; the following:
;;
;; - Make a copy of PEL's example/init/early-init.el somewhere.
;; - Identify the name of that file inside the `pel-early-init-template'
;;   user-option and save the customization inside the customization file you
;;   want to use.
;; - Open your copy of this early-init.el file and modify the value of the
;;   `pel-early-init-custom-file' variable to reflect the name of your Emacs
;;   customization file.

(defconst pel-early-init-custom-file "~/.emacs.d/emacs-customization.el"
  "Value of `custom-file' used by early-init.el code.
If you want to use some other file, please modify the initialized value.")

;; ---------------------------------------------------------------------------
;; PEL Controlled values
;; ---------------------
;;
;; The following 3 defconst forms are controlled by the function
;; `pel--update-early-init' used by PEL commands code. Therefore you do not
;; need to edit this file manually. The value MUST remain at the end of the
;; line for each of these forms.  The docstring MUST start on the next line.

(defconst pel-early-init-support-package-quickstart-p nil
  "Whether PEL supports package quickstart.")

(defconst pel-early-init-support-dual-environment-p nil
  "When t PEL uses 2 custom files: one for TTY and one for graphic mode.")

(defconst pel-early-init-shell-detection-envvar "_"
  "Name of envvar used to detect that Emacs was launched by a shell.
The value should be the same as `pel-shell-detection-envvar' user-variable
defined in pel--options.el")

(defconst pel-early-init-support-gc-boost-p nil
  "When t, raise GC threshold during startup and restore it afterwards.")

(defconst pel-early-init-suppress-file-name-handler-p nil
  "When t, disable `file-name-handler-alist' during startup and restore it after.")

;; ---------------------------------------------------------------------------
;; The code below this line does not require editing.
;; ==================================================

(defconst pel-early-init-file-version "0.3"
  "Version of PEL early-init.el. Verified by pel-setup logic. Do NOT change.")

(defconst pel-force-graphic-specific-custom-file-p
  (and pel-early-init-support-dual-environment-p
       (or (string-equal (getenv "PEL_EMACS_IN_GRAPHICS") "1")
           (not (getenv pel-early-init-shell-detection-envvar))))
  "Force independent graphics mode customization.")

;; ---------------------------------------------------------------------------
;; GC Threshold Boost During Startup
;; ==================================
;; Temporarily raise the GC threshold so Emacs does not pause for garbage
;; collection while loading the hundreds of Lisp files that make up the
;; initial load.
;; The original values are restored via `emacs-startup-hook' so that normal
;; interactive GC behaviour is not affected after initialization completes.
;;
;; This block must appear before any package activation or fast-startup work
;; to ensure the boost is in effect for the entire startup sequence.
;;
;; The saved values (pel--ei-gc-cons-threshold-saved /
;; pel--ei-gc-cons-percentage-saved) capture whatever the Emacs C layer
;; initialised those variables to, so the restore is always correct even
;; across Emacs versions that may ship different defaults.

(when pel-early-init-support-gc-boost-p
  (defvar pel--ei-gc-cons-threshold-saved gc-cons-threshold
    "GC cons threshold saved before startup boost; restored after init.")
  (defvar pel--ei-gc-cons-percentage-saved gc-cons-percentage
    "GC cons percentage saved before startup boost; restored after init.")
  (setq gc-cons-threshold most-positive-fixnum
        gc-cons-percentage 0.6)
  (add-hook 'emacs-startup-hook
            (lambda ()
              (setq gc-cons-threshold pel--ei-gc-cons-threshold-saved
                    gc-cons-percentage pel--ei-gc-cons-percentage-saved))))

;; ---------------------------------------------------------------------------
;; Suppress ``file-name-handler-alist`` During Startup
;; ===================================================
;;
;; Every `load'/`require' call consults `file-name-handler-alist' to check for
;; special file handlers (remote files, .gz compression, TRAMP, etc.).
;; Disabling it during startup reduces overhead; it should be restored
;; afterwards.  With a large number of files loaded at startup, the savings
;; are meaningful.
;;
;; This is safe because PEL loads only local, non-compressed .el/.elc files
;; from the bundle or elpa directories during startup; no remote or
;; compressed file access is needed at that point.
;;
;; This block is placed before any package activation and fast-startup code
;; so that the handler suppression is in effect for the entire startup
;; sequence.

(when pel-early-init-suppress-file-name-handler-p
  (defvar pel--startup-file-name-handler-alist file-name-handler-alist)
  (setq file-name-handler-alist nil)

  (add-hook 'emacs-startup-hook
            (lambda ()
              (setq file-name-handler-alist
                    pel--startup-file-name-handler-alist))))

;; ---------------------------------------------------------------------------

(defun pel--graphic-file-name (fname)
  "Appends \"-graphics\" to the end of a .el, .elc or extension less FNAME.
Also expands to the file true name, replacing symlinks by what they point to."
  ;; use only functions implemented in C
  (let ((ext (substring fname -3)))
    (file-truename
     (cond
      ((string-match "-graphics" fname) fname)
      ((string-equal ext ".el") (concat (substring fname 0 -3) "-graphics.el"))
      ((string-equal ext "elc") (concat (substring fname 0 -4) "-graphics.elc"))
      (t                        (concat fname "-graphics"))))))

;; If Emacs is running in Graphics mode with dual environment (independent
;; customization and Elpa packages for terminal/TTY and graphics mode), then
;; ensure that:
;; - `package-user-dir' is adjusted to use the -graphics directory
;;   so that `load-path' gets packages from the -graphics directory.
;; -
;; package quickstart activation uses the graphics-specific files.

(when pel-force-graphic-specific-custom-file-p

  (defvar pel--ei-package-quickstart-file nil
    "Copy of `package-quickstart-file' used.  Set by `pel--ei-package-activate-all'.
For debugging and to quiet byte-compiler warning.")

  ;; - Ensure that `package-user-dir' is adjusted to the -graphics directory
  ;;   so that `load-path' gets packages from the -graphics directory.
  (defun pel--ei-pkg-load-all-descriptors (original-fct)
    "Execute ORIGINAL-FCT with a controlled value of `package-user-dir'."
    (let ((package-user-dir (pel--graphic-file-name package-user-dir)))
      (funcall original-fct)))
  (declare-function pel--ei-pkg-load-all-descriptors "early-init")

  (advice-add 'package-load-all-descriptors
              :around (function pel--ei-pkg-load-all-descriptors))

  ;; Package Quickstart Support
  ;; ==========================
  ;; - When package quickstart is required:
  ;;   - set `package-quickstart',
  ;;   - set `package-user-dir', `package-quickstart-file' and `custom-file'
  ;;     to their -graphics equivalent during execution of the function
  ;;     `package-activate-all'
  ;;
  (defvar package-quickstart)           ; prevent byte-compiler warning
  (when pel-early-init-support-package-quickstart-p
    (setq package-quickstart t)

    (defun pel--ei-package-activate-all (original-fct)
      "Force use of controlled package-user-dir during package initialize."
      (setq package-user-dir (pel--graphic-file-name package-user-dir))
      (setq package-quickstart-file (pel--graphic-file-name
                                     package-quickstart-file))
      (setq custom-file (pel--graphic-file-name pel-early-init-custom-file))
      (setq pel--ei-package-quickstart-file package-quickstart-file)
      (funcall original-fct))
    (declare-function pel--ei-package-activate-all "early-init")

    (advice-add 'package-activate-all
                :around (function pel--ei-package-activate-all))))

;; ---------------------------------------------------------------------------
;; Fast Startup Support
;; ====================
;; No predicate is required for this as it checks for the presence of a file.
;;
;; Activate PEL's fast startup if environment was setup by `pel-setup-fast'.
;;
;; NOTE: for debugging startup if there is a problem, you probably will
;;       want to remove the :nomessage argument in the call to load.
;;
(let ((fast-startup-setup-fname (expand-file-name "pel-fast-startup-init.el"
                                                  user-emacs-directory)))
  (when (file-exists-p fast-startup-setup-fname)
    ;; Attempt to load the 'pel-fast-startup-init.el' file that was built
    ;; dynamically by PEL.  Identify running in fast startup mode only on
    ;; success.
    (when (load (file-name-sans-extension fast-startup-setup-fname)
                :noerror :nomessage)
      (declare-function pel-fast-startup-init "pel-fast-startup-init"
                        (&optional force-graphics using-package-quickstart))
      (pel-fast-startup-init pel-force-graphic-specific-custom-file-p
                             pel-early-init-support-package-quickstart-p)
      ;; Remember Emacs is running in PEL's fast startup mode.
      (setq pel-running-in-fast-startup-p t))))

;;; --------------------------------------------------------------------------

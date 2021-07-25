;; -*-lexical-binding: t; -*-
;;; ---Example init.el file ---------------------------------------------------
;;
;; With:
;;      - PEL,
;;      - delayed abbreviation,
;;      - fast load,
;;      - In graphics mode:
;;        - visible-bell instead of beep on error
;;        - display buffer and and file full filepath on frame title bar
;;        - speedbar without images
;;        - using adwaita theme
;;      - benchmark of emacs startup
;;      - Emacs starts without splash screen
;;      - Byte-compilable, using lexical binding
;;      - Support fast-startup.
;;
;; For a little speedup, byte-compile this file.
;;
;; -----------------------------------------------------------------------------
;;
;; 0: Identify where your PEL source code files are located.
;;
(defconst pel-home-dirpath (expand-file-name "~/projects/pel")
  "Directory where PEL source files are stored.")

;; To speed up Emacs init, prevent checking for file handling association and
;; prevent garbage collection.
;;
;; First, up until the complete initialization is done and until pel-init is
;; completed, prevent file extension processing by setting the C source code
;; defined variable `file-name-handler-alist' to nil.
;;
;; Then prevent garbage collection during startup by maxing-out the garbage
;; collection control variables `gc-cons-threshold' and `gc-cons-percentage'.
;; These are restored after handling the command line arguments (but not when
;; batch mode is used) using the `emacs-startup-hook'.

(let ((file-name-handler-alist nil)
      (fast-startup-setup-fname nil))

  (setq gc-cons-threshold most-positive-fixnum
        gc-cons-percentage 0.6)

  ;; Setup Benchmark Measurement
  ;; ---------------------------
  ;; CAUTION:
  ;;          - Copy the following files downloaded from MELPA into your PEL
  ;;            utility directory which is normally  ~/.emacs.d/utils:
  ;;                - benchmark-init-modes.el and .elc
  ;;                - benchmark-init.el and .elc
  ;;            - Do not copy the benchmark-init-autoloads.el and the
  ;;              nor the benchmark-init-pkg.el file.
  ;;              They are not needed for PEL.
  ;;          - Use to measure startup time and development of your init,
  ;;            comment this code once you'require ') happy and want to start
  ;;            a little faster.
  ;;
  (require 'benchmark-init
           (expand-file-name "~/.emacs.d/utils/benchmark-init"))
  (add-hook 'after-init-hook 'benchmark-init/deactivate)

  ;; Remember if Emacs is running in PEL's fast startup mode.
  ;; --------------------------------------------------------
  (setq fast-startup-setup-fname
        (expand-file-name "pel-setup-package-builtin-versions.el"
                          user-emacs-directory))
  (setq pel-running-with-bundled-packages
        (file-exists-p fast-startup-setup-fname))

  ;; Define function to activate package.el Elpa-compliant Package Management
  ;; ------------------------------------------------------------------------
  (defun pel--init-package-support ()
    "Configure package.el support."
    (if (< emacs-major-version 27)
        ;; Emacs prior to 27
        ;; -----------------
        (progn
          ;; Separate elpa directory for Emacs in graphics mode and Emacs
          ;; in TTY mode.
          ;; Use ~/.emacs.d/elpa in TTY mode,
          ;; use ~/.emacs.d/elpa-graphics in graphics mode
          (when (display-graphic-p)
            (setq package-user-dir (locate-user-emacs-file "elpa-graphics")))

          ;; Activate the MELPA package manager
          ;;    (see http://melpa.org/#/getting-started)
          (require 'package)
          ;; By default Emacs enable packages *after* init is loaded.
          ;; The code later explicitly calls package-initialize to do it
          ;; right away to allow installing packages.
          ;; We need to set package-enable-at-startup to nil
          ;; to prevent Emacs from doing it again at the end of init.
          (setq package-enable-at-startup nil)

          ;; Emacs 26.2 has a bug ( https://debbugs.gnu.org/34341 ) that
          ;; prevents gnutls downloads (such as the ones from GNU Elpa
          ;; for packages). The following work-around solves the problem.
          (if (version= emacs-version "26.2")
              (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

          (let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                              (not (gnutls-available-p))))
                 (proto (if no-ssl "http" "https")))
            ;; Add MELPA Stable before MELPA: some packages are only on MELPA
            ;; but several store their stable packages on MELPA Stable and
            ;; the unstable ones in MELPA.
            (add-to-list 'package-archives
                         (cons "melpa"
                               (concat proto "://melpa.org/packages/"))
                         t)
            (add-to-list 'package-archives
                         (cons "melpa-stable"
                               (concat proto "://stable.melpa.org/packages/"))
                         t)
            (when (< emacs-major-version 24)
              ;; For important compatibility libraries like cl-lib
              (add-to-list 'package-archives
                           (cons "gnu"
                                 (concat proto "://elpa.gnu.org/packages/")))))
          (package-initialize))

      ;; Emacs 27 or later.
      ;; ------------------
      ;; Emacs >= 27 support the `package-quickstart' feature which
      ;; speeds-ups Emacs startup time.  This is a user-option which must be
      ;; activated manually.
      ;; When package-quickstart is non-nil, Emacs 27 supports the early-init
      ;; initialization file in the user-emacs-directory (normally ~/.emacs.d).
      ;;
      ;; - early-init.el  : loaded very early in the startup process before
      ;;                    graphical elements are initialized and before the
      ;;                    package manager is initialized.  The following
      ;;                    variables should be set in early-init.el:
      ;;                    - `package-load-list'
      ;;                    - `package-user-dir'
      (unless (boundp 'package-quickstart)
        (setq package-quickstart nil))
      (unless package-quickstart
        ;; Activate PEL's fast startup if environment was setup by `pel-setup-fast'.
        (let ((fast-startup-setup-fname (expand-file-name "pel-setup-package-builtin-versions.el"
                                                          user-emacs-directory)))
          (when (file-exists-p fast-startup-setup-fname)
            (load (file-name-sans-extension fast-startup-setup-fname) :noerror)
            (pel-fast-startup-set-builtins)
            ;; Remember Emacs is running in PEL's fast startup mode.
            (setq pel-running-with-bundled-packages t))))
      (require 'package)
      (add-to-list 'package-archives (cons "melpa" "https://melpa.org/packages/") t)
      (add-to-list 'package-archives (cons "melpa-stable" "https://stable.melpa.org/packages/") t)
      (package-initialize)))
  (declare-function 'pel--init-package-support "init")

  ;; Either delay package.el (on fast startup) or initialize it now (for
  ;; normal operation mode).  In both cases, schedule restoration of garbage
  ;; collector to prevent Emacs from stalling/stuttering on large memory loads.

  (add-hook 'emacs-startup-hook
            (lambda ()
              ;; set GC threshold to 16 MBytes as opposed to 800,000
              (setq gc-cons-threshold 16777216
                    gc-cons-percentage 0.1)))

  (if pel-running-with-bundled-packages
      (progn
        (load (file-name-sans-extension fast-startup-setup-fname) :noerror)
        (pel-fast-startup-set-builtins)
        (add-hook 'emacs-startup-hook (function pel--init-package-support)))
    (pel--init-package-support))

  ;; ---------------------------------------------------------------------------
  ;; 2: Delay loading of abbreviation definitions
  ;;     Disable loading the abbreviation file during Emacs initialization.
  ;;     To do this: save and replace the content of the variable that holds
  ;;     the file name of the abbreviation list with the name of a file
  ;;     that does not exists.
  ;;     Pass the original name to pel-init later to initialize properly.
  ;;
  (setq pel--abbrev-file-name abbrev-file-name)
  (setq abbrev-file-name "~/abbrev_defs-invalid") ; use non-existing file name

  ;; ---------------------------------------------------------------------------
  ;; 3: Add pel to Emacs load-path
  ;;    Identify the directory where you stored pel.
  (add-to-list 'load-path pel-home-dirpath)

  ;; ---------------------------------------------------------------------------
  ;; 4: Add utils to Emacs load-path
  (add-to-list 'load-path (expand-file-name "~/.emacs.d/utils"))

  ;; ---------------------------------------------------------------------------
  ;; 4.1 - Standard Emacs behaviour control
  ;;
  ;; - Emacs startup behaviour
  ;; -------------------------
  ;;
  ;; Do not display the splash screen.  Same as emacs -Q
  (setq inhibit-startup-screen t)

  ;; Don't display the start help in minibuffer, at least for me.
  ;;   Replace YOUR_USER_NAME by your systems' login user name in the line
  ;;   below and un-comment it.
  ;; (setq inhibit-startup-echo-area-message "YOUR_USER_NAME")

  ;; - Configure Graphics Mode Display
  ;; ---------------------------------
  (when (display-graphic-p)
    ;; - Increase frame real-estate: no toolbar
    ;; (tool-bar-mode -1)

    ;; - Visual bell in graphics mode:
    ;;  - In macOS terminal, the bell is already set to silence and
    ;;    visual blinking feedback; so for macOS terminal, we keep the
    ;;    normal emacs bell (it won't make any sound under terminal;
    ;;    it will blink the terminal screen)
    ;;  - When using a graphical emacs, the beeping sound is annoying so
    ;;    the following code disables it and activates a visual bell.
    (setq ring-bell-function nil)
    (setq visible-bell t)

    ;; - Set the theme to: adwaita .  Replace with what you prefer.
    ;;   Tested Themes: theme-name, face-background-highlight, flicker evaluation
    ;;    - adwaita:    "#FFFFFF", flickers a little, not horizontally, nice light theme.pp
    ;;    - tango-dark: "#000000", flickers a lot.  Nice dark theme.
    ;;    - tango:                 flickers too.  Not as nice as adwaita.
    ;;    - leuven:                flickers more than adwaita.  Background is too white. adwaita is nicer.
    ;;    - tsdh-light:            flickers a little, comparable to adwaita. But background is too white.
    ;;    - whiteboard:            flickers a little comparable to adwaita.
    ;;    - wombat:                background is too dark
    (load-theme 'adwaita)

    ;; - Set fringe display of buffer boundaries
    ;; -----------------------------------------
    ;; Add display of buffer boundary in the right fringe column
    (setq-default indicate-buffer-boundaries 'right)

    ;; - Display buffer (full filepath) on frame title bar
    ;; ---------------------------------------------------
    (setq-default frame-title-format "%b (%f)")

    ;; - Configure Speedbar
    ;; --------------------
    ;; The speedbar icons used in graphics mode are ancient
    ;; looking. Using ASCII characters instead is nicer.
    (setq speedbar-use-images nil))

  ;; ---------------------------------------------------------------------------
  ;; 5: Store Emacs customization inside a separate file
  ;;    If you already have a (custom-set-variables ...) form
  ;;    in your init.el, move it into this new file.
  (setq custom-file "~/.emacs.d/emacs-customization.el")
  (load custom-file)

  ;; ---------------------------------------------------------------------------
  ;; 6: Start PEL
  ;; - At first leave this commented out.
  ;; - Activate the code Once you have successfully built PEL once
  (require 'pel)
  (pel-init pel--abbrev-file-name))
;;; ---- end of init.el -------------------------------------------------------

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
;;      - Support fast-startup.
;;      - Reduce the number of forms and prefer using the ones that are
;;        implemented in C.
;;      - Byte-compilable, using lexical binding. For a little speedup,
;;        byte-compile this file.  If you do make sure you byte compile it
;;        after each modification!
;;
;; NOTE: this code has OPTIONS you must identify.  See the following options:
;;
;; - Option A: whether you want to use one custom file for Emacs running in
;;             terminal (TTY) and graphic mode, or two independent custom
;;             file, one for each mode. By default only one is used.
;;
;; - Option B: whether you want to activate the benchmark-init feature to
;;             measure time spent by various features during initialization.
;;
;; - Option C: whether Emacs displays its startup message for current user.
;; - Option D: whether Emacs toolbar is displayed or not.
;; -----------------------------------------------------------------------------
;;
;; Section 0: Constant definitions
;; ===============================
(defconst pel-home-dirpath-name (expand-file-name "~/projects/pel")
  "Directory where PEL source files are stored.")

(defconst pel-fast-startup-setup-fname (expand-file-name
                                  "pel-setup-package-builtin-versions.el"
                                  user-emacs-directory)
  "Elisp file whose presence indicates PEL must run in fast startup mode.")

;; OPTION A: independent customization for TTY and graphic mode.
;; - Set to t if you want to use a different customization file for TTY
;;   and graphic mode.
(defconst pel-use-graphic-specific-custom-file-p nil
  "When t PEL uses 2 custom files: one for TTY and one for graphic mode.")


;; Section 1: Control package.el and garbage collection
;; ====================================================
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
;;
(let ((file-name-handler-alist nil))
  (setq gc-cons-threshold   most-positive-fixnum
        gc-cons-percentage  0.6
        ;; Remember if Emacs is running in PEL's fast startup mode.
        pel-running-with-bundled-packages
        (file-exists-p pel-fast-startup-setup-fname))

  ;; OPTION B: Setup Benchmark Measurement
  ;; -------------------------------------
  ;; Load benchmark-init as early as possible using its file name explicitly
  ;; so it can be used to benchmark the complete package loading mechanism.
  ;; CAUTION:
  ;;          - First install benchmark-init from MELPA with package.el
  ;;            commands when Emacs is running in normal configuration mode.
  ;;          - Copy the following files downloaded from MELPA into your PEL
  ;;            utility directory which is normally  ~/.emacs.d/utils:
  ;;                - benchmark-init-modes.el and .elc
  ;;                - benchmark-init.el and .elc
  ;;            - Do not copy the benchmark-init-autoloads.el and the
  ;;              nor the benchmark-init-pkg.el file.
  ;;              They are not needed for PEL.
  ;;          - Use to measure startup time and development of your init,
  ;;            comment this code after your investigation and want to start
  ;;            a little faster.
  ;;
  ;; To use, un-comment the following 3 lines of code:
  ;; (require 'benchmark-init
  ;;          (expand-file-name "~/.emacs.d/utils/benchmark-init"))
  ;; (add-hook 'after-init-hook 'benchmark-init/deactivate)

  ;; Define function to activate package.el Elpa-compliant Package Management
  ;; ------------------------------------------------------------------------
  (defun pel--init-package-support ()
    "Configure package.el support."
    (if (< emacs-major-version 27)
        ;; Emacs prior to 27
        ;; -----------------
        (progn
          ;; If requested by option A, separate elpa directory for Emacs
          ;; in graphics mode and Emacs in TTY mode:
          ;; - Use ~/.emacs.d/elpa in TTY mode,
          ;; - use ~/.emacs.d/elpa-graphics in graphics mode
          (when (and pel-use-graphic-specific-custom-file-p
                     (display-graphic-p))
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
          ;; It causes a byte-compiler warning but you can ignore it.
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
      (require 'package)
      (add-to-list 'package-archives
                   (cons "melpa" "https://melpa.org/packages/")
                   t)
      (add-to-list 'package-archives
                   (cons "melpa-stable" "https://stable.melpa.org/packages/")
                   t)
      (package-initialize)))
  (declare-function 'pel--init-package-support "init")

  ;; Schedule restoration of garbage collector normal values once Emacs
  ;; startup is completed to prevent Emacs from stalling/stuttering on large
  ;; memory loads.
  ;;
  (add-hook 'emacs-startup-hook
            (lambda ()
              ;; set GC threshold to 16 MBytes as opposed to 800,000
              (setq gc-cons-threshold 16777216
                    gc-cons-percentage 0.1)))

  ;; When PEL fast-start mode is requested:
  ;; - the function `pel-fast-startup-set-builtins' must be called before
  ;;   `pel--init-package-support' is called.
  ;;   - For Emacs 27+ using package-quickstart it was already called inside
  ;;     the early-init file, otherwise call it here.
  ;; - delay execution of `pel--init-package-support' after processing of
  ;;   command line argument (and therefore after Emacs initialization) to
  ;;   speed it up.  PEL does not need package.el feature in fast-startup
  ;;   mode because in that mode it never attempts to install a missing
  ;;   external package.
  ;; When PEL is not in fast-startup mode then it is important to call
  ;; `pel--init-package-support' immediately because PEL does need package.el
  ;; features to download and install any missing external packages requested
  ;; by PEL user options.
  ;;
  (if pel-running-with-bundled-packages
      (progn
        (when (or (< emacs-major-version 27)
                  (null (boundp 'package-quickstart))
                  (null package-quickstart)) ; ignore invalid warning here
          (load (file-name-sans-extension pel-fast-startup-setup-fname)
                :noerror)
          (pel-fast-startup-set-builtins))
        (add-hook 'emacs-startup-hook (function pel--init-package-support)))
    (pel--init-package-support))


  ;; -------------------------------------------------------------------------
  ;; Section 2: Delay loading of abbreviation definitions
  ;; ====================================================
  ;;
  ;; Disable the loading the abbreviation file during Emacs initialization
  ;; because this can take a long time, depending on its size.
  ;; To do this: save and replace the content of the variable that holds the
  ;; file name of the abbreviation list with the name of a non-existing file.
  ;; Pass the original name to pel-init, it will initialize it properly later.
  ;;
  (setq pel--abbrev-file-name abbrev-file-name
        abbrev-file-name      "~/abbrev_defs-invalid")


  ;; -------------------------------------------------------------------------
  ;; Section 3: Prepare load-path for PEL
  ;; ====================================
  ;;
  ;; Add 2 directories to `load-path': the directory where PEL source code is
  ;; located, and the directory where PEL stores the Elisp files taken from
  ;; non-Elpa repo sites.  Ideally these would all be inside one directory but
  ;; they are managed differently.  To help their management they are stored
  ;; in 2 different directories.
  ;;
  ;; Use `push' instead of `add-to-list': it's a little faster.

  (push (expand-file-name "utils" user-emacs-directory) load-path)
  (push pel-home-dirpath-name load-path)


  ;; -------------------------------------------------------------------------
  ;; Section 4: Standard Emacs behaviour control
  ;; ===========================================
  ;;
  ;; - Emacs startup behaviour
  ;; -------------------------
  ;;
  ;; Do not display the splash screen.  Same as emacs -Q
  (setq inhibit-startup-screen t)

  ;; OPTION C:  Don't display the start help in minibuffer, at least for me.
  ;; This variable is treated specially.  Don't group its setting with others.
  ;;   Replace YOUR_USER_NAME by your systems' login user name in the line
  ;;   below and un-comment it:
  ;; (eval '(setq inhibit-startup-echo-area-message "YOUR_USER_NAME"))

  ;; - Configure Graphics Mode Display
  ;; ---------------------------------
  (when (display-graphic-p)
    ;; OPTION D: Increase frame real-estate: no toolbar
    ;; (tool-bar-mode -1)

    ;; - Visual bell in graphics mode:
    ;;  - In macOS terminal, the bell is already set to silence and
    ;;    visual blinking feedback; so for macOS terminal, we keep the
    ;;    normal emacs bell (it won't make any sound under terminal;
    ;;    it will blink the terminal screen)
    ;;  - When using a graphical emacs, the beeping sound is annoying so
    ;;    the following code disables it and activates a visual bell.
    (setq ring-bell-function nil
          visible-bell       t
          ;; - Configure Speedbar:
          ;;   The speedbar icons used in graphics mode are ancient
          ;;   looking. Using ASCII characters instead is nicer.
          speedbar-use-images nil)

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
    (setq-default indicate-buffer-boundaries 'right
                  ;; - Display buffer (full filepath) on frame title bar.
                  frame-title-format         "%b (%f)"))


  ;; ---------------------------------------------------------------------------
  ;; Section 5: Load customization file
  ;; ==================================
  ;;
  ;; PEL does not let customization data go inside the init.el file, it uses
  ;; a separate customization file, or two if you elected to use two as
  ;; specified in OPTION A (one for terminal mode and one for graphic mode).
  ;;
  ;; PEL uses the names specified in the code below: emacs-customization.el
  ;; and potentially also emacs-customization-graphics.el.  If you prefer a
  ;; different name, change the names in the code below.
  ;;
  ;; For Emacs 27 and later, an extra step is required if you decide to use 2
  ;; different files: you must also ensure that the environment variable
  ;; PEL_EMACS_IN_GRAPHICS is defined for the shell that launches Emacs in
  ;; graphics mode.
  ;;
  ;; If you already have a (custom-set-variables ...) form in your current
  ;; init.el, move it into this or these new files.

  (setq custom-file (expand-file-name
                     (if (and pel-use-graphic-specific-custom-file-p
                              (display-graphic-p)
                              (or (< emacs-major-version 27)
                                  (getenv "PEL_EMACS_IN_GRAPHICS")))
                         "emacs-customization-graphics.el"
                       "emacs-customization.el")
                     user-emacs-directory))
  (load (file-name-sans-extension custom-file))

  ;; -------------------------------------------------------------------------
  ;; Section 6: Start PEL
  ;; ====================
  ;; - Perform PEL initialization.
  ;;   Set PEL key bindings. In normal operation mode it will also install
  ;;   and configure the external packages that have been selected by the PEL
  ;;   user-options.  They all have a name that start with `pel-use-'.
  ;; - Since PEL user-option variables are stored in the customization data
  ;;   in the (custom-set-variables) form, this code *must* be done after the
  ;;   identification of the `custom-file' and after the loading of that file.
  (require 'pel)
  (pel-init pel--abbrev-file-name))

;;; ---- end of init.el ------------------------------------------------------

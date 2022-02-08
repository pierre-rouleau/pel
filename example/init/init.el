;; -*-lexical-binding: t; -*-
;;; ---Example of PEL-compatible init.el file --------------------------------
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
;;        after each modification otherwise you will experience a slow
;;        startup!

;; ---------------------------------------------------------------------------
;; User configuration
;; ==================
;;
;; This file has Options that must be edited manually and some that
;; are edited automatically by PEL.  Do not edit the value of the variables
;; edited automatically, PEL commands will do it.  Just edit the values of the
;; variables identified for manual editing.
;;
;;
;; - Edited automatically by PEL:
;;
;;   - Option a is edited automatically by `pel--update-emacs-init',
;;     when the user executes `pel-setup-dual-environment'.
;;       - You must edit the other options manually (search for OPTION).
;;
;; - Variables that must be edited manually:
;;
;;   - OPTION B: name of the PEL directory.
;;
;;   - OPTION C: whether you want to activate the benchmark-init feature to
;;               measure time spent by various features during initialization.
;;
;;   - OPTION D: whether Emacs displays its startup message for current user.
;;   - OPTION E: whether graphical Emacs toolbar is displayed or not and
;;               other options.
;;   - OPTION F: Extra user-code.
;;   - OPTION G: Activate commands that may be confusing for new Emacs users.
;;
;;   Locate these manually edited variable by searching for "OPTION".
;; ---------------------------------------------------------------------------
;;
;; Section 0: PEL Edited Variable Definitions
;; ==========================================
;;
;; Option a: PEL controlled value -- updated by the function
;;           `pel--set-dual-environment-in-emacs-init'.
;;           Support for dual environment with Independent customization and
;;           package directories for Emacs running in terminal/TTY and graphic
;;           mode.

(defconst pel-init-support-dual-environment-p nil
  "Whether PEL uses dual custom & package files for TTY and graphic mode.")

;; ---------------------------------------------------------------------------
;; Section 0.1: Fixed Function Definitions
;; =======================================
;;
;; The following forms do not need editing, except the ones that are
;; explicitly identified as being an option.

;; OPTION B:  if PEL is stored somewhere else change the following value.
(defconst pel-home-dirpath-name (expand-file-name "~/projects/pel")
  "Directory where PEL Emacs Lisp source files are stored.")

;; --
(defconst pel-init-file-version "0.2"
  "Version of PEL init.el. Verified by pel-setup logic. Do NOT change.")

(defconst pel-emacs-is-graphic-p (display-graphic-p)
  "Predicate: t when Emacs is running in graphics mode, nil otherwise.")

(defconst pel-fast-startup-init-fname (expand-file-name
                                  "pel-fast-startup-init.el"
                                  user-emacs-directory)
  "Elisp file whose presence indicates PEL must run in fast startup mode.")

(defconst pel-running-in-fast-startup-p (file-exists-p
                                         pel-fast-startup-init-fname)
  "Non-nil when PEL runs in fast startup mode, nil otherwise.")

(defvar pel-package-user-dir-original nil
  "When set, it is the dirpath of the `package-user-dir' symlink.

The function `pel--init-package-support' stores the original
value of dirpath here and updates the value of `package-user-dir'
to control the format of the entries placed inside the
`load-path'.

PEL logic transforms `package-user-dir' to make it point to the
elpa-complete or elpa-reduced directory (or the graphics
equivalent in dual environment mode).  Having access to the
original value will allow `pel-setup-fast' to create the symlink
the very first time it is called when a user transforms its
original Emacs directory into a directory that supports PEL
startup mode management.")


;; Section 1 : Utility function definition
;; =======================================
;;
;; The pel--graphic-file-name translates a file name to the graphics
;; specific name and expands any symlink to their target.  It is implemented
;; using only functions implemented in C and Elisp always available early.
;; The code ONLY handle names ending .el or .elc, or without extension.
;; It does not handle other extensions, but its unlikely that Emacs related
;; directories have different extensions (in the context of this function
;; calls).
(defun pel--graphic-file-name (fname)
  "Appends \"-graphics\" to the end of a .el, .elc or extension less FNAME.
Also expands to the file true name, replacing symlinks by what they point to."
  ;; use only functions implemented in C or elisp available early
  (let ((ext (substring fname -3)))
    (file-truename
     (cond
      ((string-match "-graphics" fname) fname)
      ((string-equal ext ".el") (concat (substring fname 0 -3) "-graphics.el"))
      ((string-equal ext "elc") (concat (substring fname 0 -4) "-graphics.elc"))
      (t                        (concat fname "-graphics"))))))

;; The `pel--pkg-activate-all' function is used only in Emacs >= 27
;; when package-quickstart is used and under some condition.  It should
;; be defined only for those conditions but the byte compiler generates
;; end-of file warning if that is done.  So for now it's defined here.
;;
;; pel--package-quickstart-file prevent byte-compiler warnings & help debug.
(defvar pel--package-quickstart-file nil
  "Copy of package-quickstart-file used in `pel--pkg-activate-all'.")

(defun pel--pkg-activate-all (original-package-activate-all)
  "Use a graphics specific file for `package-activate-all'."
  (if (boundp 'package-quickstart-file)
      (let ((package-quickstart-file
             (pel--graphic-file-name package-quickstart-file)))
        (setq pel--package-quickstart-file package-quickstart-file)
        (funcall original-package-activate-all))
    (message "WARNING: package-quickstart-file is unbound in\
 advised function attempting to control its name.")))


;; Section 2: Control package.el and garbage collection
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
        gc-cons-percentage  0.6)

  ;; OPTION C: Setup Benchmark Measurement
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
    (require 'package)
    ;;
    ;; Remember the original `package-user-dir' user-option before we modify
    ;; it.   PEL needs to know if the original one was a real directory or a
    ;; symlink.  We need to remember it because the code below modifies
    ;; `package-user-dir' to ensure that `load-path' entries are true
    ;; directory names, not accessed via symlinks that can be changed by a
    ;; separate Emacs process running PEL.
    ;;
    (unless pel-package-user-dir-original
      (setq pel-package-user-dir-original package-user-dir))

    ;;
    ;; In the code that follows, as well as inside early-init.el, PEL also
    ;; sets the `package-user-dir' dynamic value (not the user-option value we
    ;; see inside this function) to a true directory name, following all
    ;; symbolic links to their target, before package functions like
    ;; `package-initialize' is called. This that `load-path' entries are true
    ;; directory names, fully expanded (to symlink target if any symlink is
    ;; used) to ensure continued validity of a process `load-path' even if the
    ;; "~/.emacs.d/elpa" is a symlink and its target is changed by another
    ;; process.

    (if (< emacs-major-version 27)
        ;; Emacs prior to 27
        ;; -----------------
        (progn
          ;; If requested by option A, separate elpa directory for Emacs
          ;; in graphics mode and Emacs in TTY mode:
          ;; - Use ~/.emacs.d/elpa in TTY mode,
          ;; - use ~/.emacs.d/elpa-graphics in graphics mode
          ;; Use setq to ensure that package-user-dir stays like this after
          ;; execution of package-init.
          ;; Also ensure package-user-dir is expanded to true file names and
          ;; not left to what could be a symlink.
          (setq package-user-dir (if (and pel-init-support-dual-environment-p
                                          pel-emacs-is-graphic-p)
                                     (pel--graphic-file-name package-user-dir)
                                   (file-truename package-user-dir)))

          ;; By default Emacs enable packages *after* init is loaded.
          ;; The code later explicitly calls package-initialize to do it
          ;; right away to allow installing packages.
          ;; We need to set package-enable-at-startup to nil
          ;; to prevent Emacs from doing it again at the end of init.
          (setq package-enable-at-startup nil)

          ;; Emacs 26.1 and 26.2 have a bug (https://debbugs.gnu.org/34341)
          ;; that prevents gnutls downloads (such as the ones from GNU Elpa
          ;; for packages). The following work-around solves the problem.  It
          ;; causes a byte-compiler warning but you can ignore it.
          (defvar gnutls-algorithm-priority) ; prevent byte-compiler warning
          (if (member emacs-version '("26.1" "26.2"))
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
      (add-to-list 'package-archives
                   (cons "melpa" "https://melpa.org/packages/")
                   t)
      (add-to-list 'package-archives
                   (cons "melpa-stable" "https://stable.melpa.org/packages/")
                   t)
      (if (and pel-emacs-is-graphic-p
               pel-init-support-dual-environment-p)
          ;; In forced graphics mode: use a graphics mode-specific Elpa
          ;; directory and package quickstart file.
          (progn
            (setq package-user-dir (pel--graphic-file-name package-user-dir))
            (advice-add 'package-activate-all :around #'pel--pkg-activate-all)
            (unwind-protect
                (package-initialize)
              (advice-remove 'package-activate-all #'pel--pkg-activate-all)))
        ;; In terminal/TTY or graphics mode that use the same customization
        ;; file (the usual case for Emacs):
        ;;    No change to Elpa directory nor to `package-quickstart-file',
        ;;    however ensure that the `load-path' uses the true names of
        ;;    directories; expand potential symlinks to their target to
        ;;    guarantee their validity if another Emacs/PEL process switches
        ;;    the startup mode and modifies the target of the symlink.
        (progn
          (setq package-user-dir (file-truename package-user-dir))
          (package-initialize)))))
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
  ;; - the function `pel-fast-startup-init' must be called before
  ;;   `pel--init-package-support' is called.
  ;;   - For Emacs 27+ using package-quickstart it was already called inside
  ;;     the early-init file, otherwise call it here.
  ;; - delay execution of `pel--init-package-support' after processing of
  ;;   command line argument (and therefore after Emacs initialization) to
  ;;   speed it up.  PEL does not need package.el feature in fast-startup
  ;;   mode because in that mode it never attempts to install a missing
  ;;   external package.
  ;; When PEL is not in fast-startup mode then it is important to call
  ;; `pel--init-package-support' before calling `pel-init' because PEL uses
  ;; package.el ;; features to download and install any missing external
  ;; packages requested by PEL user options.  That call is done right before
  ;; `pel-init' below, after loading the customization file.
  ;;
  ;; NOTE: remove the :nomessage argument to load to help debug a startup
  ;; problem if one occurs.
  ;;
  (defvar package-quickstart) ; declared only to prevent byte-compiler warning.
  (when pel-running-in-fast-startup-p
    ;; Start fast startup for: - Emacs < 27
    ;;                         - Emacs >= 27 when package quickstart is not used.
    ;;                                       when used, early-init starts it.
    (when (or (< emacs-major-version 27)
              (null (boundp 'package-quickstart))
              (not package-quickstart))
      (if (and (load (file-name-sans-extension pel-fast-startup-init-fname)
                     :noerror :nomessage)
               (fboundp 'pel-fast-startup-init))
          (pel-fast-startup-init (and pel-init-support-dual-environment-p
                                      pel-emacs-is-graphic-p))
        (message "WARNING: Failed loading pel-fast-startup-init\
 from user-emacs-directory")))
    (add-hook 'emacs-startup-hook (function pel--init-package-support)))


  ;; -------------------------------------------------------------------------
  ;; Section 3: Delay loading of abbreviation definitions
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
  ;; Section 4: Standard Emacs behaviour control
  ;; ===========================================
  ;;
  ;; - Emacs startup behaviour
  ;; -------------------------
  ;;
  ;; Do not display the splash screen.  Same as emacs -Q
  (setq inhibit-startup-screen t)

  ;; OPTION D: Don't display Emacs startup help message, at least for me.
  ;; This variable is treated specially.  Don't group its setting with others.
  ;;   Replace YOUR_USER_NAME by your systems' login user name in the line
  ;;   below and un-comment it:
  ;; (eval '(setq inhibit-startup-echo-area-message "YOUR_USER_NAME"))

  ;; - Configure Graphics Mode Display
  ;; ---------------------------------
  (when pel-emacs-is-graphic-p
    ;; OPTION E: Increase frame real-estate: no toolbar
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

  ;; - Tab Control
  ;; -------------
  ;; Set tabs to 4 space characters by default instead of the
  ;; emacs 8 position using hard/literal tab.
  ;; (setq-default tab-width 4)

  ;; The fill-column should be set inside the directory inside
  ;; a .dir-local.el file instead of init to provide the most flexible setup.
  ;; (setq-default indent-tabs-mode nil)

  ;; -------------------------------------------------------------------------
  ;; Section 5: Load customization file
  ;; ==================================
  ;;
  ;; PEL does not let customization data go inside the init.el file, it uses
  ;; a separate customization file, or two if you elected to use two as
  ;; specified in Option A (one for terminal mode and one for graphic mode).
  ;;
  ;; PEL uses the names specified in the code below: emacs-customization.el
  ;; and potentially also emacs-customization-graphics.el.  If you prefer a
  ;; different name, change the names in the code below.
  ;;
  ;; If you already have a (custom-set-variables ...) form in your current
  ;; init.el, move it into this or these new files.
  ;;
  (setq custom-file (expand-file-name
                     (if (and pel-init-support-dual-environment-p
                              pel-emacs-is-graphic-p)
                         "emacs-customization-graphics.el"
                       "emacs-customization.el")
                     user-emacs-directory))
  ;; Attempt to load the selected customization file.  If not found, create an
  ;; empty one to allow Emacs to start, but display a warning message
  ;; describing the error.
  (condition-case err
      (load (file-name-sans-extension custom-file))
    (file-missing
     (progn
       (let ((utils-dirpath (expand-file-name "utils" user-emacs-directory))
             (abbrev-defs   (expand-file-name "abbrev_defs"
                                              user-emacs-directory)))
         (unless (file-exists-p custom-file)
           (with-temp-buffer (write-file custom-file)))
         (unless (file-exists-p abbrev-defs)
           (with-temp-buffer (write-file abbrev-defs)))
         (unless (file-exists-p utils-dirpath)
           (make-directory utils-dirpath)))
       (display-warning
        'init
        (format "ERROR loading customization file %s.
 Detected error: %S
 Created an empty customization instead to allow Emacs and PEL to start
 and install the few packages that it always installs: popup and which-key.
 - It's normal and expected if you never created %s, otherwise investigate."
                custom-file err custom-file)
        :error))))

  ;; -------------------------------------------------------------------------
  ;; Section 6: in normal startup mode initialize package
  ;; ====================================================
  ;;
  ;; In normal startup mode initialize package.el before calling `pel-init'
  ;; because `pel-init' uses package.el features.  This calls `package-init'
  ;; which will process all local packages and will grow the `load-path'
  ;; accordingly.
  ;;
  (unless pel-running-in-fast-startup-p
    (pel--init-package-support))

  ;; -------------------------------------------------------------------------
  ;; Section 7: Prepare load-path for PEL
  ;; ====================================
  ;;
  ;; Add 2 directories to `load-path': the directory where PEL source code is
  ;; located, and the directory where PEL stores the Elisp files taken from
  ;; non-Elpa repo sites.  Ideally these would all be inside one directory but
  ;; they are managed differently.  To help their management they are stored
  ;; in 2 different directories.
  ;;
  ;; Use `push' instead of `add-to-list': it's a little faster.  However,
  ;; in normal startup mode it must be done *after* the call to the function
  ;; `pel--init-package-support' otherwise the startup will show down
  ;; noticeably: PEL's code is used extensively in the startup, so it's better
  ;; to have it at the beginning of a possibly very long `load-path'!
  ;;
  (push (expand-file-name "utils" user-emacs-directory) load-path)
  (push pel-home-dirpath-name load-path)

  ;; ------------------------------------------------------------------------
  ;; Section 8: Extra User-Specific Logic
  ;; ====================================
  ;;
  ;; If you absolutely need to define values or run some code at
  ;; initialization time that PEL does not already handle and that you cannot
  ;; control via Emacs customization system, write your code here before the
  ;; code in section 9 where PEL is initialized.
  ;;
  ;; OPTION F: Write extra code here if absolutely needed.
  ;;           Keep it short and fast.
  ;;           Make sure it byte-compiles without warnings.

  ;; -------------------------------------------------------------------------
  ;; Section 9: Start PEL
  ;; ====================
  ;; - Perform PEL initialization.
  ;;   Set PEL key bindings. In normal operation mode it will also install
  ;;   and configure the external packages that have been selected by the PEL
  ;;   user-options.  They all have a name that start with `pel-use-'.
  ;; - Since PEL user-option variables are stored in the customization data
  ;;   in the (custom-set-variables) form, this code *must* be done after the
  ;;   identification of the `custom-file' and after the loading of that file.
  (require 'pel)
  (pel-init pel--abbrev-file-name)

  ;; -------------------------------------------------------------------------
  ;; Section 10: Activate some of the *confusing* commands
  ;; =====================================================
  ;;
  ;; Some of Emacs commands are disabled by default because they are
  ;; considered confusing for new users.  The following comments contain code
  ;; that activate some of them.
  ;;
  ;; Emacs will add code after the form when you get Emacs to activate some,
  ;; put that code inside the form to take advantage of the extra little bit
  ;; of speed.
  ;;
  ;; OPTION G:
  ;;
  ;; - In Dired-mode, when selecting a directory with e, f or RET, Emacs
  ;;   creates a new buffer.  It also has a binding for a which opens inside
  ;;   the same buffer.
  ;; (put 'dired-find-alternate-file 'disabled nil)

  ;; - C-x n n : narrows the rest of the buffer.
  ;; (put 'narrow-to-region 'disabled nil)

  ;; - C-x C-l: downcases a complete region
  ;; (put 'downcase-region  'disabled nil)

  ;; - C-x C-n: set-goal-column
  ;; (put 'set-goal-column 'disabled nil)

  ;; - C-x < : scroll-left
  ;; (put 'scroll-left 'disabled nil)
  )

;;; ---- end of init.el ------------------------------------------------------

;;; ---Example init.el file -- For PEL, delayed abbrev, fast load, benchmark---
;;
;; -----------------------------------------------------------------------------
;; To speed up Emacs init, prevent checking for file handling association and
;; prevent garbage collection.  The following 2 lines reduce by about .2
;; seconds loading time in terminal mode.
(let ((file-name-handler-alist nil)
      (gc-cons-threshold most-positive-fixnum))

  ;; ---------------------------------------------------------------------------
  ;; Setup Benchmark Measurement
  ;; ---------------------------
  ;; Load benchmark right away using the file name explicitly so we can use it
  ;; to benchmark the complete package loading mechanism.
  ;; CAUTION: Modify the path when a new version is available.
  (require 'benchmark-init
           (expand-file-name
            "~/.emacs.d/elpa/benchmark-init-20150905.938/benchmark-init"))
  (add-hook 'after-init-hook 'benchmark-init/deactivate)

  ;; ---------------------------------------------------------------------------
  ;; 1: Setup package sources: MELPA, MELPA-STABLE and a local mypelpa
  (when (>= emacs-major-version 24)
    (require 'package)
    (setq package-enable-at-startup nil)
    (if (version=  emacs-version "26.2")
        (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))
    (let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                        (not (gnutls-available-p))))
           (proto (if no-ssl "http" "https")))
      (add-to-list 'package-archives
                   (cons "melpa" (concat proto "://melpa.org/packages/")) t)
      (add-to-list 'package-archives
                   (cons "melpa-stable"
                         (concat proto "://stable.melpa.org/packages/")) t)
      (add-to-list 'package-archives
                   (cons "mypelpa"
                         (expand-file-name "~/projects/pel/pelpa/")) t))
    (package-initialize))

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
  (add-to-list 'load-path (expand-file-name "~/projects/pel"))

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
  ;; (setq inhibit-startup-screen t)

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
  (pel-init pel--abbrev-file-name)

)
;;; ---- end of init.el -------------------------------------------------------

;;; -*-no-byte-compile: t; -*-
;;; ---Example init.el file -- Used for first installation----Step 0-----------
;;
;; 0: Identify where your PEL source code files are located.
;;
(defconst pel-home-dirpath (expand-file-name "~/projects/pel")
  "Directory where PEL source files are stored.")

;; 1: Setup additional package sources: MELPA, MELPA-STABLE.
;;    By default Emacs only identifies the gnu archive located at
;;    URL "https://elpa.gnu.org/packages/".
;;    Add the MELPA archives as they provide more packages.
(when (>= emacs-major-version 24)
  (if (< emacs-major-version 27)
      ;; Emacs prior to 27
      (progn
        (require 'package)
        (setq package-enable-at-startup nil)
        (if (version= emacs-version "26.2")
            (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))
        (let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                            (not (gnutls-available-p))))
               (proto (if no-ssl "http" "https")))
          (add-to-list 'package-archives
                       (cons "melpa" (concat proto "://melpa.org/packages/")) t)
          (add-to-list 'package-archives
                       (cons "melpa-stable"
                             (concat proto "://stable.melpa.org/packages/")) t))
        (package-initialize))
    ;; Emacs 27 or later support the `package-quickstart' feature which
    ;; speeds-ups Emacs startup time.  This is a user-option which must be
    ;; activated manually. The package feature is already set at this point.
    ;; When package-quickstart is customized to t, Emacs 27 support 2 initialization
    ;; files in the user-emacs-directory (which often is ~/.emacs.d), these are:
    ;;
    ;; - early-init.el  : loaded very early in the startup process before
    ;;                    graphical elements are initialized and before the
    ;;                    package manager is initialized.  The following
    ;;                    variables should be set in early-init.el:
    ;;                    - `package-load-list'
    ;;                    - `package-user-dir'
    ;; When package-quickstart is t, there's no need to call package-initialize.
    (add-to-list 'package-archives (cons "melpa" "https://melpa.org/packages/") t)
    (add-to-list 'package-archives (cons "melpa-stable" "https://stable.melpa.org/packages/") t)
    (unless package-quickstart
      (package-initialize))))

;; 2: Delay loading of abbreviation definitions
;;     Disable loading the abbreviation file during Emacs initialization.
;;     To do this: save and replace the content of the variable that holds
;;     the file name of the abbreviation list with the name of a file
;;     that does not exists.
;;     Pass the original name to pel-init later to initialize properly.
;;
;; (setq pel--abbrev-file-name abbrev-file-name)
;; (setq abbrev-file-name "~/abbrev_defs-invalid") ; use non-existing file name

;; 3: Add pel to Emacs load-path
;;    Identify the directory where you stored pel.
(add-to-list 'load-path pel-home-dirpath)

;; 4: Add utils to Emacs load-path
(add-to-list 'load-path (expand-file-name "~/.emacs.d/utils"))

;; 5: Store Emacs customization inside a separate file
;;    If you already have a (custom-set-variables ...) form
;;    in your init.el, move it into this new file.
(setq custom-file "~/.emacs.d/emacs-customization.el")
(load custom-file)

;; 6: Start PEL
;; - At first leave this commented out.
;; - Activate the code Once you have successfully built PEL once
;; (require 'pel)
;; (pel-init)  ; or later->; (pel-init pel--abbrev-file-name)

;;; ---- end of init.el -------------------------------------------------------

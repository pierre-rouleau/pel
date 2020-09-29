;;; ---Example init.el file ---------------------------------------------------
;;
;; With:
;;      - PEL,
;;      - delayed abbreviation.
;;
;;
;; DO THIS BEFORE USING THIS FILE:
;;
;; - create an empty file: ~/.emacs.d/abbrev_defs:
;;
;;      do ->  touch ~/.emacs.d/abbrev_defs
;;
;;
;; -----------------------------------------------------------------------------
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

;; 2: Delay loading of abbreviation definitions
;;     Disable loading the abbreviation file during Emacs initialization.
;;     To do this: save and replace the content of the variable that holds
;;     the file name of the abbreviation list with the name of a file
;;     that does not exists.
;;     Pass the original name to pel-init later to initialize properly.
;;
(setq pel--abbrev-file-name abbrev-file-name)
(setq abbrev-file-name "~/abbrev_defs-invalid") ; use non-existing file name

;; 3: Add pel to Emacs load-path
;;    Identify the directory where you stored pel.
(add-to-list 'load-path (expand-file-name "~/projects/pel"))

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
(require 'pel)
(pel-init pel--abbrev-file-name)

;;; ---- end of init.el -------------------------------------------------------

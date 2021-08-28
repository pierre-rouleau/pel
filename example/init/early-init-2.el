;; -*-lexical-binding: t; -*-
;; ---Example early-init.el --------------------------------------------------
;;
;; With PEL's independent custom file for terminal/TTY and graphics mode
;; the Emacs running in graphics mode must also use the '-graphics' instance
;; of the Elpa package directory.  This is done by setting the variable
;; `package-user-dir' to the value selected by customization and appending
;; "-graphics" to its name.
;;
;; For Emacs 27 and later, Emacs loads the packages early, before init.el is
;; processed.  The following code force the package.el logic to use the proper
;; package user directory when Emacs is running in graphics mode.
;;
;; When early-init.el is run, Emacs has not yet initialized its graphics
;; subsystem and the function `display-graphic-p' is not available.
;; Therefore the graphics mode must be detected by the presence of an
;; environment variable, PEL_EMACS_IN_GRAPHICS, set to "1".

(when (string-equal (getenv "PEL_EMACS_IN_GRAPHICS") "1")
  ;; Ensure that `package-load-all-descriptors' uses the graphics-specific
  ;; directory when forcing use of graphics specific files.  This is the case
  ;; when Emacs runs in graphics mode and PEL dual independent customization
  ;; feature is enabled.

  ;; The pel--graphic-file-name translates a file name to the graphics
  ;; specific name
  (defun pel--graphic-file-name (fname)
    "Appends '-graphics' to the end of a .el, .elc or extension less FNAME."
    ;; use only functions implemented in C
    (let ((ext (substring fname -3)))
      (cond
       ((string-match "-graphics" fname) fname)
       ((string-equal ext ".el") (concat (substring fname 0 -3) "-graphics.el"))
       ((string-equal ext "elc") (concat (substring fname 0 -4) "-graphics.elc"))
       (t                        (concat fname "-graphics")))))

  ;; extra check to ensure that user does want to use dual custom files.
  ;; If you know you want it, delete this file-exist-p check because it slows
  ;; down startup a little.  It's only there because PEL provides this
  ;; early-init file as a default in Emacs >= 27.
  (when (file-exists-p (pel--graphic-file-name custom-file))
    (defun pel--pkg-load-all-descriptors (original-fct)
      "Execute ORIGINAL-FCT with a controlled value of `package-user-dir'."
      (let ((package-user-dir (pel--graphic-file-name package-user-dir)))
        (funcall original-fct)))

    (advice-add
     'package-load-all-descriptors
     :around (function pel--pkg-load-all-descriptors))))

;; ---------------------------------------------------------------------------

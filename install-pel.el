;;; install-pel.el --- Install PEL locally -*-lexical-binding: t-*-

;;; Commentary:
;;
;; This file provides tools used to install the PEL package.
;; The commands are meant to be executed from the command line via PEL's
;; Makefile.

(require 'package-x)

(require 'pel--base)


;;; Code:

(defun upload-pel-to-local-archive ()
  "Upload the distributable PEL package tar file to pelpa.
pelpa is the local elpa-compliant Emacs package archive directory.
It should have been created by a call to `pel-create-pelpa'."
  (interactive)
  (let  ((package-archive-upload-base
          (expand-file-name "./pelpa")))
    (if (fboundp 'package-upload-file)
        (package-upload-file
         (expand-file-name
          (format "./out/pel-%s.tar" (pel-version))))
      (error
       "Unknown function: package-upload-file (should be part of package-x)"))))

;; -----------------------------------------------------------------------------
(provide 'install-pel)

;;; install-pel.el ends here

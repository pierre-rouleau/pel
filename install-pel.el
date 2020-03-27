;;; install-pel.el --- Install PEL locally -*-lexical-binding: t-*-

;;; Commentary:
;;

(require 'package-x)

(require 'pel--base)


;;; Code:

(defun upload-pel-to-local-archive ()
  "Upload the distributable PEL package tar file to the local elpa mirror package."
  (interactive)
  (let  ((package-archive-upload-base
          (expand-file-name "~/mirrors/emacs/elpa-mirror")))
    (if (fboundp 'package-upload-file)
        (package-upload-file
         (expand-file-name (format "~/dev/elisp/pel/out/pel-%s.tar" (pel-version))))
      (error "Unknown function: package-upload-file (should be part of package-x)"))))

;; -----------------------------------------------------------------------------
(provide 'install-pel)

;;; install-pel.el ends here

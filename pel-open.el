;;; pel-open.el --- Open file dispatcher

;;; Commentary:
;;
;; This file defines functions that perform operations that depend on the buffer
;; major mode, the face at point and other criteria and which invoke functions
;; in other PEL files.  These functions will evolve over time and will
;; incorporate more functionality in various modes, allowing a multi-purpose
;; function to be bound globally to a single key sequence or key-chord.

(defun pel-open-at-point (&optional n noerror)
  "Open the file or mode-specific reference at point.
If there is no target issue a user-error unless NOERROR is non-nil.
In that case just return nil.
Optionally identify a window to open a file reference with the argument N.
See `pel-find-file-at-point-in-window' for more information. "
  (interactive "P")
  (if (and (eq major-mode 'rst-mode)
           (require 'pel-rst nil :noerror)
           (fboundp 'pel-at-rst-reference-p)
           (fboundp 'pel-rst-open-target)
           (pel-at-rst-reference-p))
      (pel-rst-open-target n)
    (if (and (require 'pel-file nil :noerror)
             (fboundp 'pel-find-file-at-point-in-window))
        (pel-find-file-at-point-in-window n)
      (unless noerror
        (user-error "Cannot load pel-file!")))))

;;-pel-autoload
(defun pel-browse-filename-at-point ()
  "Open the filename at point in the system's browser."
  (interactive)
  (pel-open-at-point 9))   ; n:=9 to force using a browser

;; -----------------------------------------------------------------------------
(provide 'pel-open)

;;; pel-open.el ends here

;;; pel-open.el --- Open file dispatcher  -*- lexical-binding: t; -*-

;; Copyright (C) 2020, 2022  Pierre Rouleau

;; Author: Pierre Rouleau <prouleau001@gmail.com>

;; This file is part of the PEL package
;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; -----------------------------------------------------------------------------
;;; Commentary:
;;
;; This file defines functions that perform operations that depend on the buffer
;; major mode, the face at point and other criteria and which invoke functions
;; in other PEL files.  These functions will evolve over time and will
;; incorporate more functionality in various modes, allowing a multi-purpose
;; function to be bound globally to a single key sequence or key-chord.

;; -----------------------------------------------------------------------------
;;; Dependencies:

(require 'pel--base)        ; uses: pel-current-buffer-filename

;; -----------------------------------------------------------------------------
;;; Code:

(defun pel-open-at-point (&optional n noerror)
  "Open the file or mode-specific reference at point.
If there is no target issue a `user-error' unless NOERROR is non-nil.
In that case just return nil.
Optionally identify a window to open a file reference with the argument N.
See `pel-find-file-at-point-in-window' for more information."
  (interactive "P")
  ;; It's possible the file visited by the current buffer is located in a
  ;; directory that is not the current directory; the user might have
  ;; forcibly changed the current working directory for example.
  ;; Temporary force the current working directory to the directory holding
  ;; the file visited by the current buffer to ensure we can access the proper
  ;; relative file path.
  (let ((original-cwd default-directory))
    (unwind-protect
        (progn
          (when (buffer-file-name)
            (cd (file-name-directory (pel-current-buffer-filename))))
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
      (cd original-cwd))))

;;-pel-autoload
(defun pel-browse-filename-at-point ()
  "Open the filename at point in the system's browser."
  (interactive)
  (pel-open-at-point 9))   ; n:=9 to force using a browser

;;-pel-autoload
(defun pel-open-url-at-point ()
  "Open the URL at point in a local buffer.
Copy the content of the URL into a temporary file, then open that file."
  (interactive)
  (if (and (require 'pel-file nil :no-error)
           (fboundp 'pel-filename-parts-at-point))
      (let* ((type.url (pel-filename-parts-at-point))
             (url      (cdr type.url)))
        (if url
            (if (and (require 'url-handlers nil :no-error)
                     (fboundp 'url-copy-file))
                (let ((filename (make-temp-file "pel-open-url")))
                  (url-copy-file url filename :ok-if-already-exists)
                  (find-file filename))
              (error "Can't load url-handler!"))
          (user-error "No valid URL at point!")))
    (error "Can't load pel-file!")))

;; -----------------------------------------------------------------------------
(provide 'pel-open)

;;; pel-open.el ends here

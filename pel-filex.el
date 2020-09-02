;;; pel-filex.el --- Open files in external applications.  -*- lexical-binding: t; -*-

;; Created   : Wednesday, September  2 2020.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2020-09-02 13:11:32, updated by Pierre Rouleau>

;; This file is part of the PEL package.
;; This file is not part of GNU Emacs.

;; Copyright (C) 2020  Pierre Rouleau
;;
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

;;; ----------------------------------------------------------------------------
;;; Commentary:
;;
;; This file implements the command `pel-open-in-app' which can open the
;; file currently visited in the current buffer or all files marked in a Dired
;; buffer.

;;; Credit:
;;
;;  Idea to create a command instead of just a function for Dired came from
;;  after stumbling on Xah Lee's "Emacs's: Open File in External App" at
;;  URL http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html
;;  The code `pel-open-in-app' is strongly inspired from Xah Lee's code and
;;  builds on it adding extra validation and using symbols comparison instead of
;;  strings.

;;; ----------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'pel--base)                    ; use: pel-current-buffer-filename
;;; ----------------------------------------------------------------------------
;;; Code:
;;

;;-pel-autoload
(defun pel-open-in-os-app (&optional fname)
  "Open file with FNAME name using the default OS application.
If FNAME is not specified or nil, then act depending on
current major mode:
- in Dired buffers: open all marked files
- in other buffers visiting a file: open the current file.

Issue an error when issued on a buffer that is not visiting a file.

Supports the following OS: macOS, Linux, Windows.
If you need support for another OS, please contact me."
  (interactive)
  (let* ((filenames (if fname
                       (list fname)
                     (if (eq major-mode 'dired-mode)
                         (when (fboundp 'dired-get-marked-files)
                           (dired-get-marked-files))
                       (list (pel-current-buffer-filename)))
                     ))
         (file-count (length filenames))
         (ok         (if (and filenames (<= file-count 10))
                         t
                       (yes-or-no-p (format
                                     "Open all %d files?" file-count)))))
      (when ok
        (cond
         ((eq system-type 'darwin)
          (dolist (filename filenames)
            (start-process "default-app" nil "open" filename)))

         ;; For Linux
         ((eq system-type 'gnu/linux)
          ;; use a pipe
          (let ((process-connection-type nil))
            (dolist (filename filenames)
              (start-process "" nil "xdg-open" filename))))

         ;; For Windows
         ((and (eq system-type 'windows-nt)
               (fboundp 'w32-shell-execute))
          (dolist (filename filenames)
            (w32-shell-execute "open" filename)))

         (t
          (user-error
           "Sorry, your OS is not supported. Please request it!"))))))

;;; ----------------------------------------------------------------------------
(provide 'pel-filex)

;;; pel-filex.el ends here

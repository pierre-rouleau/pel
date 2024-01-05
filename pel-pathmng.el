;;; pel-pathmng.el --- Path management utilities. -*-lexical-binding: t-*-

;; Copyright (C) 2020, 2024  Pierre Rouleau

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
;; Utilities to manage path, and in particular the Emacs `load-path'.
;;
;; With package managers such as MELPA you end up with a lot of packages in the
;; system and each directory is placed inside the `load-path'.  Being able to
;; quickly see what's in this list with one directory per line is nicer than the
;; long list (potentially wrapped around).
;;
;; The `pel-emacs-load-path' command opens up a buffer that lists each directory
;; in the `load-path' and display an overall word count message.  If one
;; installs a new package then running the command will create an updated list
;; in new buffer which can be diffed with the old one to quickly confirm the
;; additions.

(require 'display-line-numbers)         ; use: display-line-numbers-mode
(require 'simple)                       ; use: count-words
(require 'pel-window)                   ; use: pel-window-direction-for,
;;                                      ;      pel-window-select


;;; Code:

;;-pel-autoload
(defun pel-emacs-load-path (&optional n)
  "Show the current `load-path' inside a new *load-path* buffer.
Open the buffer in the current window or the one identified by N,
with the display-line-number-mode on.
The buffer is NOT committed to a file.
If a buffer with the name *load-path* already exists, creates a new
buffer name that contains the string *load-path*.

By default `pel-emacs-load-path' opens the buffer in the current window.
Use the N argument to specify a different window.
- If N is negative : create a new window
- If N is 0:                 : open buffer in other window
- If N in [2,8] range:       : open buffer in window identified by the direction
                               corresponding to the cursor in a numeric
                               keypad:
                               -             8 := \\='up
                               - 4 := \\='left  5 := \\='current  6 := \\='right
                               -             2 := \\='down
- If N is 9 or larger        : open buffer in window below."
  (interactive "P")
  (let ((direction (pel-window-direction-for (prefix-numeric-value n) 'current))
         bufname)
    (if (pel-window-select direction)
        (progn
          (setq bufname (generate-new-buffer "*load-path*"))
          (with-current-buffer bufname
            (dolist (pathname load-path)
              (insert (format "%s\n" pathname))))
          (switch-to-buffer bufname)
          (goto-char (point-min))
          (display-line-numbers-mode 1))
      (user-error "Invalid window!"))))

;; -----------------------------------------------------------------------------
(provide 'pel-pathmng)

;;; pel-pathmng.el ends here

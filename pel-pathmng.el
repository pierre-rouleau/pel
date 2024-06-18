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
;; in the `load-path', the number of entries and the list of directory roots
;; it has.
;;
;; If one installs a new package then running the command will create an
;; updated list in new buffer which can be diffed with the old one to quickly
;; confirm the additions.

;; Call hierarchy
;;
;; * `pel-emacs-load-path'
;;   - `pel-emacs-roots-in-loadpath'
;;     - `pel-emacs-roots-in'
;;       - `pel-paths-path-in-common'
;;       - `pel-paths-set-common-parent'
;;         - `pel-paths-common-parent'
;;

;; ---------------------------------------------------------------------------
;;
(require 'display-line-numbers)         ; use: `display-line-numbers-mode'
(require 'simple)                       ; use: `count-words'
(require 'pel--base)                    ; use: `pel-filesep'
(require 'pel-window)                   ; use: `pel-window-direction-for',
;;                                      ;      `pel-window-select'
(eval-when-compile
  (require 'subr-x)                     ;use `string-join'
  )


;; ---------------------------------------------------------------------------
;;; Code:

;; ---------------------------------------------------------------------------

(defun pel--paths-excluded-loadpath-here ()
  "Return a list of directory file names that are common on this system."
  (let* ((paths  (list (directory-file-name
                        (expand-file-name user-emacs-directory))
                       (getenv "HOME"))))
    (cond (pel-system-is-macos-p (if (file-exists-p "/opt/homebrew")
                                     (progn
                                       (push "/opt/homebrew" paths)
                                       (push "/opt/homebrew/share" paths)
                                       (push "/opt/homebrew/Cellar" paths)
                                       )
                                   (when (file-exists-p "/usr/local/Cellar")
                                     (push "/usr/local" paths)
                                     (push "/usr/local/share" paths)
                                     (push "/usr/local/Cellar" paths))))
          (pel-system-is-linux-p (progn
                                   (push "/usr/local/share" paths)
                                   (push "/usr/local/share/emacs" paths))))
    (reverse paths)))

(defconst pel-paths-excluded-loadpath (pel--paths-excluded-loadpath-here)
  "List of directories that are parent of important Emacs directories.")

(defun pel-paths-common-parent (dir-a dir-b)
  "Return the common parent directory, if any, of DIR-A and DIR-B.

Return a string that is the file name format of the common
directory path (does not end with path separator).
Return nil if no parent in common."
  (let* ((dirs-a (split-string dir-a pel-filesep))
         (dirs-b (split-string dir-b pel-filesep))
         (common '())
         (skip    nil)
         (dirb    nil)
         (common-paths (dolist (dira dirs-a (reverse common))
                         (unless skip
                           (setq dirb (car dirs-b))
                           (setq dirs-b (cdr dirs-b))
                           (if (string-equal dira dirb)
                               (push dira common)
                             (setq skip t))))))
    ;; ignore / being common (since it's always the case
    (when (cdr common-paths)
      (string-join common-paths pel-filesep))))

(defun pel-paths-set-common-parent (dir-a dirs &optional excluded)
  "Return the common parent directory, if any, of DIR-A and any of DIRS.

DIRS is a list of directories.

If EXCLUDED is specified, it's a list of directories
that should not be considered common.  The HOME directory
is often placed in that list.

Return a string that is the file name format of the common
directory path (does not end with path separator).
Return nil if no parent in common."
  (let ((the-common-dirpath nil)
        (common-dirpath nil))
    (dolist (dir dirs)
      (setq common-dirpath (pel-paths-common-parent dir dir-a))
      (when (and common-dirpath
                 (not (member common-dirpath excluded)))
        (if the-common-dirpath
            (when (> (length the-common-dirpath)
                     (length common-dirpath))
              (setq the-common-dirpath common-dirpath)))
        (setq the-common-dirpath common-dirpath))
      )
    the-common-dirpath))

(defun pel-paths-path-in-common (dir-a dirs &optional excluded)
  "Return DIRS element that has a common path with DIR-A.

If EXCLUDED is specified, it's a list of directories
that should not be considered common.  The HOME directory
is often placed in that list.

Return a string that is the file name format of the common
directory path (does not end with path separator).
Return nil if no parent in common."
  (let ((the-common-dirname nil)
        (dir nil))
    (while (and (not the-common-dirname)
                dirs)
      (setq dir (car dirs))
      (setq dirs (cdr dirs))
      (when (pel-paths-common-parent dir dir-a)
        (unless (member (pel-paths-common-parent dir dir-a) excluded)
          (setq the-common-dirname dir))))
    the-common-dirname))


(defun pel-emacs-roots-in (directories &optional excluded)
  "Return a list of the directory roots listed in load-path"
  (let ((roots (list (car directories))))
    (dolist (dir (cdr directories) (reverse roots))
      ;; dir has common-path in common with something inside roots
      ;; It's possible that we placed something in roots that is a
      ;; child of what is common-path.  If this is the case it must be
      ;; removed from roots and common-path must be put instead.
      ;; If there's no child common-path must be also placed in roots.
      (let ((common-path (pel-paths-set-common-parent dir roots excluded)))
        (if common-path
            (let ((item-in-common (pel-paths-path-in-common dir roots excluded )))
              (when item-in-common
                (setq roots  (remove item-in-common roots)))
              (push common-path roots))
          (push dir roots))))))


(defun pel-emacs-roots-in-loadpath ()
  "Return a list of root directories in load-path."
  (pel-emacs-roots-in load-path pel-paths-excluded-loadpath))


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
            (insert "----------------\n")
            (insert (format  "load-path: %d directories:\n" (length load-path)))
            (dolist (pathname load-path)
              (insert (format "%s\n" pathname)))
            (insert "----------------\n")
            (let ((load-path-roots (pel-emacs-roots-in-loadpath)))
              (insert (format "load-path has the following %d roots:\n" (length load-path-roots)))
              (dolist (pathname load-path-roots)
                (insert (format "%s\n" pathname)))))
          (switch-to-buffer bufname)    ; OK to call switch-to-buffer: the
                                        ; command is interactive and
                                        ; specifically requests to open a NEW
                                        ; buffer in a *specified* window.
          (goto-char (point-min))
          (display-line-numbers-mode 1))
      (user-error "Invalid window!"))))

;; -----------------------------------------------------------------------------
(provide 'pel-pathmng)

;;; pel-pathmng.el ends here

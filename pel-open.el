;;; pel-open.el --- Open file dispatcher  -*- lexical-binding: t; -*-

;; Copyright (C) 2020, 2022, 2024, 2025, 2026  Pierre Rouleau

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

;; ---------------------------------------------------------------------------
;;; Commentary:
;;
;; This file holds a set of file opening functions.
;;
;;
;;
;; Directory Scope Control of `pel-open-at-point' file searches
;;  * `pel-set-open-at-point-dir-home'
;;
;; Open File at Point
;;  * `pel-browse-filename-at-point'
;;    * `pel-open-at-point'
;;  * `pel-open-url-at-point'
;;
;; Display File Open Behaviour
;;  * `pel-show-filemng-status'
;;    - `pel--open-file-at-point-dir-string-for'

;; ---------------------------------------------------------------------------
;;; Dependencies:

(require 'pel--base)        ; use: `pel-current-buffer-filename'
(require 'pel--options)     ; use: `pel-open-file-at-point-dir-home'
(require 'pel-prompt)       ; use: `pel-select-from'
(require 'pel-ido)          ; use: `pel-ido-use-fname-at-point-string-for'
(require 'pel-ffind)        ; indirectly use: `pel-generic-find-file'
(require 'pel-file)         ; use: `pel-find-file-at-point-in-window'

;; ---------------------------------------------------------------------------
;;; Code:

;;* Directory Scope Control of `pel-open-at-point' file searches
;;  ============================================================

(defvar-local pel--open-file-at-point-dir-home pel-open-file-at-point-dir-home
  "Root directory from where `pel-open-at-point' opens file.
You can change its buffer-local value with `pel-set-open-at-point-dir-home'.
Can be one of the following:
- nil     : use parent directory of currently visited file, the default.
            If buffer is not visiting a file, then use the buffer's current
            working directory.
- \\='cwd    : use buffer's current working directory
- a string: the name of a specific directory.")

;;-pel-autoload
(defun pel-set-open-at-point-dir-home ()
  "Set the behaviour of `pel-open-at-point' in current buffer.

Select how it determines the directory from which a relative file
name is built. Select one of the following methods:

- Use visited file parent directory (the default).
- Use buffer's current working directory.
- Use a specified directory."
  (interactive)
  (let ((choice
         (pel-select-from "pel-open-at-point base directory"
                          '((?f "File's parent directory" nil)
                            (?b "Buffer's current working directory" cwd)
                            (?s "Specific directory" str)))))
    (when (eq choice 'str)
      (setq choice (expand-file-name
                    (read-directory-name "pel-open-at-point base directory:"
                                         nil
                                         nil
                                         t))))
    (setq pel--open-file-at-point-dir-home choice)))

;; ---------------------------------------------------------------------------
;;* Open File at Point
;;  ==================

(defvar-local pel--open-file-method nil
  "Mode specific file name at point opening function.

A function that extracts a file name in a way that is specific to the mode and
opens it.

When non-nil in a buffer, this function is used by `pel-open-at-point'
in that buffer to extract the file name at point and open it. The
function must take one argument, N, the same as `open-open-at-point'
first argument. It must return non-nil if the function did take the
responsibility to identify and open the file at point and nil if it
declined the responsibility and wants to let open by the standard way
`pel-open-a-point' opens the file.

When nil, `pel-open-at-point' uses the default mechanism: it calls
`pel-find-file-at-point-in-window' to open the file.")

;;-pel-autoload
(defun pel-open-at-point (&optional n)
  "Open the file or mode-specific reference at point.

Extract the target file name from text at point and attempt to find a file
with that name to open.

Optionally identify a window to open a file reference with the argument N.
Opens the file with `pel-find-file-at-point-in-window' unless the buffer
has a non-nil value for `pel-find-file-at-point-in-window' that identifies
another function that does open the file.

Also see `pel-open-file-at-point-dir-home', used by the function when not
using language specialized features.

The user options controlling this are part of the `pel-file-finding'
customization group."
  (interactive "P")
  ;; It's possible the file visited by the current buffer is located in a
  ;; directory that is not the current directory; the user might have
  ;; forcibly changed the current working directory for example.
  ;; Temporary force the current working directory to the directory holding
  ;; the file visited by the current buffer to ensure we can access the proper
  ;; relative file path. In some modes we are more prone to want to keep that
  ;; original working directory (like in reST mode); in that case restore it.
  ;;
  (let ((original-cwd default-directory))
    (condition-case the-error
        (progn
          ;; First change current directory if necessary to open file from
          ;; a different context.
          (cond
           ;; If visiting a file and `pel--open-file-at-point-dir-home' does
           ;; not impose a directory, use current's file parent directory.
           ((and (not pel--open-file-at-point-dir-home)
                 buffer-file-truename)
            (cd (file-name-directory (pel-current-buffer-filename))))
           ;;
           ;; if `pel--open-file-at-point-dir-home' impose a valid directory
           ;; then use that.
           ((and (stringp pel--open-file-at-point-dir-home)
                 (file-exists-p pel--open-file-at-point-dir-home))
            (cd pel--open-file-at-point-dir-home))
           ;; otherwise use current working directory as base.
           )

          ;; Open the file using the mode specific method if it exists and
          ;; does open the file, otherwise use the standard method.
          (unless (and pel--open-file-method
                       (funcall pel--open-file-method n))
            (pel-find-file-at-point-in-window n)))
      (error
       ;; On error: go back to original directory and inform the user.
       (cd original-cwd)
       (user-error "ERROR: %s" (cadr the-error))))))

;;-pel-autoload
(defun pel-browse-filename-at-point ()
  "Open the filename at point in the system's browser."
  (interactive)
  (pel-open-at-point 9))   ; n:=9 to force using a browser

;;-pel-autoload
(defun pel-open-url-at-point ()
  "Open the content of URL at point in a local buffer.
Copy the content of the URL into a temporary file, then open that file."
  (interactive)
  (if (and (require 'pel-file nil :noerror)
           (fboundp 'pel-filename-parts-at-point))
      (let* ((type.url (pel-filename-parts-at-point))
             (url      (cdr type.url)))
        (if url
            (if (and (require 'url-handlers nil :noerror)
                     (fboundp 'url-copy-file))
                (let ((filename (make-temp-file "pel-open-url")))
                  (url-copy-file url filename :ok-if-already-exists)
                  (find-file filename))
              (error "Can't load url-handler!"))
          (user-error "No valid URL at point!")))
    (error "Can't load pel-file!")))

;; ---------------------------------------------------------------------------
;;* Display File Open Behaviour
;;  ===========================

(defun pel--open-file-at-point-dir-string-for (value)
  "Return a description of local VALUE of `pel--open-file-at-point-dir-home'.
VALUE may also be taken from the `pel-open-file-at-point-dir-home' option."
  (cond
   ((not value)
    (if buffer-file-truename
        (format
         "use file's parent directory: %s"
         (file-name-directory (pel-current-buffer-filename)))
      (format "use buffer's current working directory: %s"
              default-directory)))
   ((eq value 'cwd)
    (format
     "use current working directory: %s"
     default-directory))
   ((stringp value)
    (format "use: %s" value))
   (t
    "INVALID!")))

;;-pel-autoload
(defun pel-show-filemng-status ()
  "Display status of various file management controls."
  (interactive)
  (message "\
- In: %s:
- File encoding                             : %s
- pel-open-at-point relative path resolution: %s
- ido-use-filename-at-point                 : %s, ido-use-url-at-point : %s"
           (or  (pel-current-buffer-filename nil nil :noerror)
                (format "buffer %s" (current-buffer)))
           buffer-file-coding-system
           (pel--open-file-at-point-dir-string-for
            pel--open-file-at-point-dir-home)
           (if (boundp 'ido-use-filename-at-point)
               (pel-ido-use-fname-at-point-string-for
                ido-use-filename-at-point)
             "Not loaded")
           (if (boundp 'ido-use-url-at-point)
               (pel-ido-use-url-at-point-string-for
                ido-use-url-at-point)
             "Not loaded")))

;; ---------------------------------------------------------------------------
(provide 'pel-open)

;;; pel-open.el ends here

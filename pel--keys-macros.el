;;; pel--keys-macros.el --- Key binding macros used by pel_keys.  -*- lexical-binding: t; -*-

;; Created   : Tuesday, September  1 2020.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2020-09-17 22:14:30, updated by Pierre Rouleau>

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
;; The functions and macros defined in this file are used by pel_keys.el to
;; create specialized key bindings and to manipulate customization groups.

;; The following lists the functions ('-'), and macros ('@') provided
;; and their calling hierarchy:
;;
;; @ `pel--cfg'
;;    - `pel-prefixed'
;; @ `pel--cfg-pkg'
;;    - `pel--customize-groups'
;;       - `pel--group-isin-libfile'
;;       - `pel--isa-custom-group-p'

;;; ----------------------------------------------------------------------------
;;; Dependencies:
;;
;;

;;; ----------------------------------------------------------------------------
;;; Code:
;;

;; -- Macros
;; To have a name show up in which-key 'menu', a named function is required,
;; otherwise all we see is 'prefix' which is not meaningful.
;; The macros help simplify/reduce the lines of code used to create the
;; key bindings for PEL configuration.
;;

(defun pel-prefixed (str &optional prefix)
  "Return the STR string prefixed with PREFIX (or space) if not empty.
Pass empty string unchanged."
  (if (string= str "")
      ""
    (format "%s%s"
            (or prefix " ")
            str)))

(defmacro pel--cfg (pel-group prefix key)
  "Define a function and key binding to customize specified PEL-GROUP mapped to PREFIX KEY."
  (let ((fct (intern (format "pel-cfg%s" (pel-prefixed pel-group "-"))))
        (group (intern (format "pel%s" (pel-prefixed pel-group "-"))))
        (docstring (format "Customize PEL%s support.\n\
If OTHER-WINDOW is non-nil (use \\[universal-argument]), \
display in other window." (pel-prefixed
                           (capitalize pel-group)))))
    `(progn
       ;; first declare the function
       (defun ,fct (&optional other-window)
         ,docstring
         (interactive "P")
         (customize-group (quote ,group) other-window))
       ;; then define the global key
       (define-key ,prefix ,key (quote ,fct)))))

;; --

(defun pel--isa-custom-group-p (group-name)
  "Return t if GROUP-NAME string is the name of an existing customize group."
  (let (custom-groups)
    (mapatoms (lambda (symbol)
                (when (or (and (get symbol 'custom-loads)
                               (not (get symbol 'custom-autoload)))
                          (get symbol 'custom-group))
                  (push (symbol-name symbol) custom-groups))))
    (not (null (member group-name custom-groups)))))

(defun pel--group-isin-libfile (group)
  "Return non-nil if customize GROUP is defined in an accessible Emacs Lisp file.
Return the path to the source file containing the group.
GROUP must be a string.
Return nil otherwise."
  (let ((file-path (locate-library group)))
    (when file-path
      ;; get the source code file, not the byte-compiled version
      (let ((file-path  (concat (file-name-sans-extension file-path) ".el")))
        (when (file-exists-p file-path)
          (with-temp-buffer
            (insert-file-contents file-path)
            (goto-char (point-min))
            (when (re-search-forward (concat
                                      "^ *?(defgroup +?"
                                      group
                                      " ")
                                     nil
                                     :noerror)
              file-path)))))))

(defun pel--customize-groups (group-list)
  "Utility: customize all groups named in the GROUP-LIST if they exist."
  (dolist (grp group-list)
    (if (pel--isa-custom-group-p grp)
        (customize-group grp t)
      (let ((file-path (pel--group-isin-libfile grp)))
        (when file-path
          (let ((library-name (file-name-base file-path)))
            (when (y-or-n-p
                   (format
                    "Group %s is from a non loaded %s. Load it first? "
                    grp
                    library-name))
              (when (load-library library-name)
                (customize-group grp t)))))))))

(defmacro pel--cfg-pkg (pel-group prefix key &rest other-groups)
  "Define a function and key binding to customize specified PEL-GROUP.
The PEL-GROUP is mapped to PREFIX KEY.  Optionally, OTHER-GROUPS
is one or several other customization groups that will also be
opened when the command is invoked with a prefix argument."
  (let ((fct (intern (format "pel-cfg-pkg-%s" pel-group)))
        (group (intern (format "pel-pkg-for-%s" pel-group)))
        (docstring (format "Customize PEL %s support.\n\
If OTHER-WINDOW is non-nil (use \\[universal-argument]), \
display in other window and open the related group(s) that exist."
                           (capitalize pel-group))))
    `(progn
       ;; first declare the function
       (defun ,fct (&optional other-window)
         ,docstring
         (interactive "P")
         (customize-group (quote ,group) other-window)
         (when (and other-window (quote ,other-groups))
           (pel--customize-groups (quote ,other-groups))))
       ;; then define the global key
       (define-key ,prefix ,key (quote ,fct)))))

;;; ----------------------------------------------------------------------------
(provide 'pel--keys-macros)

;;; pel--keys-macros.el ends here

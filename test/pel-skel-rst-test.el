;;; pel-skel-rst-test.el --- Test the reStructuredText Skeletons.  -*- lexical-binding: t; -*-

;; Created   : Tuesday, May 25 2021.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2021-05-25 14:41:34, updated by Pierre Rouleau>

;; This file is part of the PEL package.
;; This file is not part of GNU Emacs.

;; Copyright (C) 2021  Pierre Rouleau
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

;;; --------------------------------------------------------------------------
;;; Commentary:
;;
;;

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'pel--options)
(require 'pel-ert)
(require 'cl-lib)
;;; --------------------------------------------------------------------------
;;; Code:
;;
(defun pel--rst-all-sk-file-header (directory file-base-name &optional license-name)
  "Generate all supported layouts of the Emacs Lisp templates in DIRECTORY.
Each generated file name use the FILE-NAME-BASE as the base name,
then an extra number patter is added to it to create the file name."
  (let ((base-name (expand-file-name file-base-name directory))
        file-name
        pel-rst-skel-insert-file-timestamp (f2 -1)
        pel-rst-skel-with-license          (f3 -1)
        (license-name                        (or license-name "gpl-3.0")))
    (cl-letf (((symbol-function 'read-from-minibuffer)
               (lambda (prompt &optional
                               _initial-content
                               _keymap
                               _read
                               _hist
                               _default-value
                               _inherit-input-method)
                 (message "The prompt is: %s" prompt)
                 (if (string= prompt "Title: ")
                     "The title of this file"
                   license-name))))
      (cl-letf (((symbol-function 'lice:read-license)
                 (lambda ()
                   license-name)))
        (setq f2 -1)
        (dolist (pel-rst-skel-insert-file-timestamp
                 (get 'pel-rst-skel-insert-file-timestamp :choices))
          (cl-incf f2)
          (setq f3 -1)
          (dolist (pel-rst-skel-with-license
                   (get 'pel-rst-skel-with-license :choices))
            (cl-incf f3)
            (setq file-name (format "%s-%d-%d.rst" base-name f2 f3))
            (message "Testing with %s: %S, %S"
                     file-name
                     pel-rst-skel-insert-file-timestamp
                     pel-rst-skel-with-license)
            (find-file file-name)
            (erase-buffer)
            (rst-mode)
            (pel-rst-file-header)
            (write-file file-name)
            (kill-buffer)))))))

(defun pel--rst-all-sk-files ()
  "Generate Emacs Lisp skeleton template files with all possible combinations.
Store them inside the pel/test-result/templates/rst directory."
  (interactive)
  (let ((dir-name (expand-file-name
                   "test-result/templates/rst" (file-name-directory (locate-library "pel--options")))))
    (pel--rst-all-sk-file-header dir-name "rst" "mit")))

;;; --------------------------------------------------------------------------
(provide 'pel-skel-rst-test)

;;; pel-skel-rst-test.el ends here

;;; pel-skel-clisp-test.el --- Test the Common Lisp Skeletons.  -*- lexical-binding: t; -*-

;; Created   : Saturday, May 22 2021.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2021-05-23 01:51:59, updated by Pierre Rouleau>

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

(defun pel--clisp-all-sk-file-header (directory file-base-name &optional license-name)
  "Generate all supported layouts of the Emacs Lisp templates in DIRECTORY.
Each generated file name use the FILE-NAME-BASE as the base name,
then an extra number patter is added to it to create the file name."
  (let ((base-name (expand-file-name file-base-name directory))
        file-name
        pel-clisp-skel-use-separators        (f1 -1)
        pel-clisp-skel-insert-file-timestamp (f2 -1)
        pel-clisp-skel-with-license          (f3 -1)
        pel-clisp-skel-package-name          (f4 -1)
        pel-clisp-emacs-filevar-line         (f5 -1)
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
                 (if (string= prompt "File purpose: ")
                     "testing the template"
                   license-name))))
      (cl-letf (((symbol-function 'lice:read-license)
                 (lambda ()
                   license-name)))
        (dolist (pel-clisp-skel-use-separators
                 (get 'pel-clisp-skel-use-separators :choices))
          (progn
            (cl-incf f1)
            (setq f2 -1)
            (dolist (pel-clisp-skel-insert-file-timestamp
                     (get 'pel-clisp-skel-insert-file-timestamp :choices))
              (progn
                (cl-incf f2)
                (setq f3 -1)
                (dolist (pel-clisp-skel-with-license
                         (get 'pel-clisp-skel-with-license :choices))
                  (progn
                    (cl-incf f3)
                    (setq f4 -1)
                    (dolist (pel-clisp-skel-package-name
                             (get 'pel-clisp-skel-package-name :choices))
                      (progn
                        (cl-incf f4)
                        (setq f5 -1)
                        (dolist (pel-clisp-emacs-filevar-line
                                 (get 'pel-clisp-emacs-filevar-line :choices))
                          (cl-incf f5)
                          (setq file-name (format "%s-%d-%d-%d-%d-%d.l" base-name f1 f2 f3 f4 f5))
                          (message "Testing with %s: %S, %S, %S, %S, %S"
                                   file-name
                                   pel-clisp-skel-use-separators
                                   pel-clisp-skel-insert-file-timestamp
                                   pel-clisp-skel-with-license
                                   pel-clisp-skel-package-name
                                   pel-clisp-emacs-filevar-line)
                          (find-file file-name)
                          (erase-buffer)
                          (lisp-mode)
                          (pel-lisp-file-header)
                          (insert (format "[
;;;; This text inside the square brackets is normally NOT generated by the template!
;;;; It is here only to show the value of the user-options used to generate the content.
;;;; This file was generated by the test function: pel--clisp-all-sk-file-header
;;;; This file was generated with the following user-options values:
;;;; - pel-clisp-skel-use-separators        : %S
;;;; - pel-clisp-skel-insert-file-timestamp : %S
;;;; - pel-clisp-skel-with-license          : %S
;;;; - pel-clisp-skel-package-name          : %S
;;;; - pel-clisp-emacs-filevar-line         : %S
;;;;  ]"
                                          pel-clisp-skel-use-separators
                                          pel-clisp-skel-insert-file-timestamp
                                          pel-clisp-skel-with-license
                                          pel-clisp-skel-package-name
                                          pel-clisp-emacs-filevar-line))
                          (write-file file-name)
                          (kill-buffer))))))))))))))

(defun pel--clisp-all-sk-files ()
  "Generate Emacs Lisp skeleton template files with all possible combinations.
Store them inside the pel/example/templates/clisp directory."
  (interactive)
  (let ((dir-name (expand-file-name
                   "example/templates/clisp" (file-name-directory (locate-library "pel--options")))))
    (pel--clisp-all-sk-file-header dir-name "clisp" "mit")))

;;; --------------------------------------------------------------------------
(provide 'pel-skel-clisp-test)

;;; pel-skel-clisp-test.el ends here
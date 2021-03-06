;;; pel-skel-c-test.el --- Test the Emacs C Skeletons.  -*- lexical-binding: t; -*-

;; Created   : Sunday, May 23 2021.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2021-05-28 10:40:41, updated by Pierre Rouleau>

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
(defun pel--c-all-sk-file-header (directory file-base-name extension &optional license-name)
  "Generate all supported layouts of the Emacs  templates in DIRECTORY.
Each generated file name use the FILE-NAME-BASE as the base name,
then an extra number patter is added to it to create the file name."
  (let ((base-name (expand-file-name file-base-name directory))
        file-name
        pel-c-skel-use-separators        (f1 -1)
        pel-c-skel-insert-file-timestamp (f2 -1)
        pel-c-skel-with-license          (f3 -1)
        pel-c-skel-cfile-section-titles
        pel-c-skel-hfile-section-titles  (f4 -1)
        (section-titles-symbol (if (string= extension "c")
                                   'pel-c-skel-cfile-section-titles
                                 'pel-c-skel-hfile-section-titles))
        pel-c-skel-doc-markup            (f5 -1)
        pel-c-skel-comment-with-2stars   (f6 -1)
        pel-c-skel-use-include-guards    (f7 -1)
        (license-name (or license-name "gpl-3.0")))
    (cl-letf (((symbol-function 'read-from-minibuffer)
               (lambda (prompt &optional
                               _initial-content
                               _keymap
                               _read
                               _hist
                               _default-value
                               _inherit-input-method)
                 ;; (message "The prompt is: %s" prompt)
                 (if (string= prompt "File purpose: ")
                     "testing the template"
                   license-name))))
      (cl-letf (((symbol-function 'lice:read-license)
                 (lambda ()
                   license-name)))
        (dolist (pel-c-skel-use-separators
                 (get 'pel-c-skel-use-separators :choices))
          (cl-incf f1)
          (setq f2 -1)
          (dolist (pel-c-skel-insert-file-timestamp
                   (get 'pel-c-skel-insert-file-timestamp :choices))
            (cl-incf f2)
            (setq f3 -1)
            (dolist (pel-c-skel-with-license
                     (get 'pel-c-skel-with-license :choices))
              (cl-incf f3)
              (setq f4 -1)
              (dolist (section-titles
                       (get section-titles-symbol :choices))
                (set section-titles-symbol section-titles)
                (cl-incf f4)
                (setq f5 -1)
                (dolist (pel-c-skel-doc-markup
                         (get 'pel-c-skel-doc-markup :choices))
                  (cl-incf f5)
                  (setq f6 -1)
                  (dolist (pel-c-skel-comment-with-2stars
                           (get 'pel-c-skel-comment-with-2stars :choices))
                    (cl-incf f6)
                    (setq f7 -1)
                    (dolist (pel-c-skel-use-include-guards
                             (get 'pel-c-skel-use-include-guards :choices))
                      (cl-incf f7)
                      (setq file-name (format "%s_%d_%d_%d_%d_%d_%d_%d.%s" base-name f1 f2 f3 f4 f5 f6 f7 extension))
                      ;; (message "Testing with %s: %S, %S, %S, %S, %S, %S, %S"
                      ;;          file-name
                      ;;          pel-c-skel-use-separators
                      ;;          pel-c-skel-insert-file-timestamp
                      ;;          pel-c-skel-with-license
                      ;;          section-titles
                      ;;          pel-c-skel-doc-markup
                      ;;          pel-c-skel-comment-with-2stars
                      ;;          pel-c-skel-use-include-guards)
                      (find-file file-name)
                      (erase-buffer)
                      (c-mode)
                      (pel-c-file-header) ; defined dynamically
                      ;; (insert (format "/*[
                      ;; **   This text inside this comment in square brackets is normally NOT generated by the template!
                      ;; **   It is here only to show the value of the user-options used to generate the content.
                      ;; **   This file was generated by the test function: pel--c-all-sk-file-header
                      ;; **   This file was generated with the following user-options values:
                      ;; **   - pel-c-skel-use-separators        : %S
                      ;; **   - pel-c-skel-insert-file-timestamp : %S
                      ;; **   - pel-c-skel-with-license          : %S
                      ;; **   - %-32s : %S
                      ;; **   - pel-c-skel-doc-markup            : %S
                      ;; **   - pel-c-skel-comment-with-2stars   : %S
                      ;; **    ]*/"
                      ;;                                     pel-c-skel-use-separators
                      ;;                                     pel-c-skel-insert-file-timestamp
                      ;;                                     pel-c-skel-with-license
                      ;;                                     section-titles-symbol
                      ;;                                     section-titles
                      ;;                                     pel-c-skel-doc-markup
                      ;;                                     pel-c-skel-comment-with-2stars))
                      (write-file file-name)
                      (kill-buffer))))))))))))

(defun pel--c-all-sk-files ()
  "Generate Emacs skeleton template files with all possible combinations.
Store them inside the pel/test-result/templates/c/files directory."
  (interactive)
  (let ((dir-name (expand-file-name
                   "test-result/templates/c/files" (file-name-directory (locate-library "pel--options")))))
    (pel--c-all-sk-file-header dir-name "example" "c" "mit")
    (pel--c-all-sk-file-header dir-name "example" "h" "mit")))

;;; --------------------------------------------------------------------------
(provide 'pel-skel-c-test)

;;; pel-skel-c-test.el ends here

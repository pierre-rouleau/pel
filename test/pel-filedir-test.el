;;; pel-filedir-test.el --- Test pel-filedir.  -*- lexical-binding: t; -*-

;; Created   : Thursday, February 26 2026.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2026-02-26 23:09:57 EST, updated by Pierre Rouleau>

;; This file is part of the PEL package.
;; This file is not part of GNU Emacs.

;; Copyright (C) 2026  Pierre Rouleau
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
(require 'pel-filedir)                  ; tested file
(require 'ert)

;;; --------------------------------------------------------------------------
;;; Code:
;;

(ert-deftest pel-dpath-of-test ()
  "Test `pel-dpath-of'."

  ;; Without a list of sub-directories return normalized the parent directory
  (should (string= (pel-dpath-of ".") (expand-file-name ".")))
  (should-not (string= (pel-dpath-of ".") "."))

  ;; With a list of sub-directories, they are concatenated to the parent
  (let ((sep (substring (file-name-as-directory "a") -1))) ; for Unix: "/"
                                                           ; for Windows: "\\"

    (let ((root-dir (format "%sroot" sep)))
      ;; The end of the string is not a directory separator
      (should (string= (pel-dpath-of root-dir)  root-dir))

      ;; The parent directory can end with or without a sep, it does not have
      ;; an impact on the returned string.
      ;; - without: /root
      (should (string= (pel-dpath-of root-dir "a" "b")
                       (string-join (list root-dir "a" "b") sep )))
      ;; - with: /root/
      (should (string= (pel-dpath-of (format "%s%s" root-dir sep) "a" "b")
                       (string-join (list root-dir "a" "b") sep ))))))

;;; --------------------------------------------------------------------------
(provide 'pel-filedir-test)

;;; pel-filedir-test.el ends here

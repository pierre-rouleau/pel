;;; pel-comp.el --- Native compilation support  -*- lexical-binding: t; -*-

;; Created   : Wednesday, September 17 2025.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2026-03-21 16:28:12 EDT, updated by Pierre Rouleau>

;; This file is part of the PEL package.
;; This file is not part of GNU Emacs.

;; Copyright (C) 2025, 2026  Pierre Rouleau
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
;; PEL specific support for Emacs that supports native compilation.
;;
;; The module provides the `pel-native-compile-util' function that performs a
;; native compile of an .el file stored in PEL utils directory.
;;
;; The code hierarchy:
;;
;; - `pel-native-compile-util'
;;   - `pel-comp-eln-file-for-util'

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
;; - Emacs built in functions
;; - PEL utils directory

;;; --------------------------------------------------------------------------
;;; Code:
;;

(defun pel-comp-eln-file-for-util (fname)
  "Return the full path of the .eln file for .el util FNAME.
FNAME must be a file base name with the .el extension.
It signals a `user-error' when native compilation is not available on this
instance of Emacs."
  (if (and (fboundp 'comp-el-to-eln-rel-filename)
           (boundp  'comp-native-version-dir))
      (progn
        (unless (string-suffix-p ".el" fname)
          (user-error "File must have the .el extension: %s" fname))
        (let* ((util-dpathname (expand-file-name "utils" user-emacs-directory))
               (bname (comp-el-to-eln-rel-filename
                       (expand-file-name fname util-dpathname)))
               (eln-dirpathname
                (expand-file-name comp-native-version-dir
                                  (expand-file-name "eln-cache"
                                                    user-emacs-directory))))
          (expand-file-name bname eln-dirpathname)))
    (user-error "This Emacs is not built with native-compile support")))


(defun pel-native-compile-util (fname)
  "Native compile the util .el FNAME if necessary.
FNAME must be a file base name with the .el extension.
This returns:
- t when native compilation of the file was initiated,
- nil when native compilation was not started because `native-compile-async'
  is not available,
- \\='eln-present if it was already present and its time stamp is newer
  than the timestamp of the .el file.
It signals a `user-error' when native compilation is not available on this
instance of Emacs."
  (if (featurep 'native-compile)
      (let* ((util-dpathname (expand-file-name "utils" user-emacs-directory))
             (el-fpathname (expand-file-name fname util-dpathname))
             (eln-fpathname (pel-comp-eln-file-for-util fname)))

        (unless (file-exists-p el-fpathname)
          (user-error "Source file not found: %s" el-fpathname))

        (if (and (file-exists-p eln-fpathname)
                 (file-newer-than-file-p eln-fpathname el-fpathname))
            'eln-present
          (progn
            (require 'comp-run)
            (when (fboundp 'native-compile-async)
              (message "Native compile %s --> %s" el-fpathname eln-fpathname)
              (native-compile-async el-fpathname)
              t))))
    (user-error "This Emacs is not built with native-compile support")))

;;; --------------------------------------------------------------------------
(provide 'pel-comp)

;;; pel-comp.el ends here

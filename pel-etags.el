;;; pel-etags.el --- Etags support for compressed files.  -*- lexical-binding: t; -*-

;; Created   : Friday, November  6 2020.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2025-03-20 17:18:40 EDT, updated by Pierre Rouleau>

;; This file is part of the PEL package.
;; This file is not part of GNU Emacs.

;; Copyright (C) 2020, 2024, 2025  Pierre Rouleau
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
;; This file is a temporary work-around for the etags implementation of the
;; xref-location-marker method.  The original code is not capable of handling
;; references to symbol defined in compressed files (.el.gz files for
;; example).

;; I reported this problem in the following bug report:
;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=44494

;; As a work-around I proposed the modification to xref-location-marker which
;; is able to return the name of the .el.gz file.

;; To use this, simply load it after etags has been loaded by using the
;; following code in your Emacs init file (PEL does it inside pel-init):
;;
;; (add-hook 'xref-etags-mode-hook (function
;;                                  (lambda () (load "pel-etags" :no-error))))

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'xref)
(require 'etags)
(require 'eieio)                        ; use: `with-slots'

;;; --------------------------------------------------------------------------
;;; Code:
;;

(defun pel-file-or-compressed-file-for (fname)
  "Return the valid file name for FNAME.
Check if FNAME is an existing file name, if not
try FNAME appended with the following compression extensions:
- \".gz\", the extension of compressed files created by gzip
- \".bz2\", the extension for compressed files created by bzip2
- \".xz\", the extension for compressed files created by xz
- \".lzma\", the extension for compressed files created by xz.

Return the file that exists or nil if nothing found."
  (let ((fpath nil))
    (cl-dolist (ext '(""
                      ".gz"
                      ".bz2"
                      ".xz"
                      ".lzma"))
      (setq fpath (concat fname ext))
      (when (file-exists-p fpath)
        (cl-return fpath)))))

(cl-defmethod xref-location-marker ((l xref-etags-location))
  (with-no-warnings                     ; In previous versions of Emacs slot
                                        ; tag-info and file is undefined.
    (with-slots (tag-info file) l
      (let (buffer
            (fname (pel-file-or-compressed-file-for file)))
        (if fname
            (setq buffer (find-file-noselect fname))
          (user-error "file %s (or .gz, .bz2, .xz, .lzma) does not exist" file))
        (with-current-buffer buffer
          (save-excursion
            (etags-goto-tag-location tag-info)
            (point-marker)))))))

;;; --------------------------------------------------------------------------
(provide 'pel-etags)

;;; pel-etags.el ends here

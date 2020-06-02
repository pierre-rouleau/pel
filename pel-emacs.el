;;; pel-emacs.el --- Emacs stats utilities -*- lexical-binding: t -*-

;; Copyright (C) 2020  Pierre Rouleau

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

;;; Commentary:


;;; Code:

;;-pel-autoload
(defun pel-emacs-load-stats ()
  "Display number of loaded files & features."
  (interactive)
  (message "\
# loaded files: %d
# features    : %d"
           (length load-history)
           (length features)))


;;-pel-autoload
(defun pel-emacs-mem-stats ()
  "Display Emacs memory statistics inside an *emacs-mem-stats* buffer."
  (interactive)
  (let ((bufname (generate-new-buffer "*emacs-mem-stats*")))
    (with-current-buffer bufname
      (insert
       (format "\
On %s:
 - cons-cells-consed  : %19d
 - floats-consed      : %19d
 - vector-cells-consed: %19d
 - symbols-consed     : %19d
 - string-chars-consed: %19d
 - misc-objects-consed: %19d
 - intervals-consed   : %19d
 - strings-consed     : %19d"
               (format-time-string "%A, %B %d, %Y @ %T")
               cons-cells-consed
               floats-consed
               vector-cells-consed
               symbols-consed
               string-chars-consed
               misc-objects-consed
               intervals-consed
               strings-consed)))
    (switch-to-buffer bufname)
    (goto-char (point-min))))

;; -----------------------------------------------------------------------------
(provide 'pel-emacs)

;;; pel-emacs.el ends here

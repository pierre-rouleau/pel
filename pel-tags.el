;;; pel-tags.el --- Ctags/ETags support -*-lexical-binding: t-*-

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

;; -----------------------------------------------------------------------------
;;; Commentary:
;;
;;  Under development - will be unstable for a little while.

;;; Code:

(require 'pel--base)

;; -----------------------------------------------------------------------------


(defun pel-show-etags-mode-status ()
  "Show the status of the Xref Etags mode in current buffer."
  (interactive)
  (message
   "\
- Xref Etags mode: %s
- tags-file-name : %s
- tags-table-list: %S"
   (pel-symbol-on-off-string 'xref-etags-mode)
   tags-file-name
   tags-table-list))



;; -----------------------------------------------------------------------------
(provide 'pel-tags)

;;; pel-tags.el ends here

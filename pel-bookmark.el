;;; pel-bookmark.el --- PEL bookmark utilities -*-lexical-binding: t-*-

;; Copyright (C) 2020, 2023, 2025  Pierre Rouleau

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

(require 'bookmark)                     ; uses: bookmark-get-bookmark
(require 'pel--base)                    ; uses: pel-current-buffer-filename

;;; Code:

;;-pel-autoload
(defun pel-bookmark-in-current-file-p (bookmark_name)
  "Return non-nil if BOOKMARK_NAME exists and points to currently edited file.
Return nil otherwise."
  (let  ((bookmark_filename (cdr (cadr (bookmark-get-bookmark
                                        bookmark_name
                                        :noerror)))))
    (and bookmark_filename (equal (file-truename bookmark_filename)
                                  (pel-current-buffer-filename)))))

;;-pel-autoload
(defun pel-bookmark-info (&optional append)
  "Display information about bookmark control in specialized buffer.

Clear previous buffer content unless optional APPEND argument is
non-nil, in which case it appends to the previous report."
  (interactive "P")
  (let ((pel-insert-symbol-content-context-buffer (current-buffer)))
    (pel-print-in-buffer
     "*pel-bookmark-info*"
     "Baookmark Control"
     (lambda ()
       (pel-insert-symbol-content-line 'pel-use-bm)
       (insert "\n")
       (pel-insert-symbol-content-line 'bookmark-default-file))
     (unless append :clear-buffer)
     :use-help-mode)))

;;-pel-autoload
(defun pel-bookmark-set-file (&optional globally)
  "Change Emacs bookmark file used in current buffer.
Change it globally if GLOBALLY optional argument is set.
This does not persist, the default is `bookmark-default-file' user-option."
  (interactive "P")
  (let ((bookmark-filename (read-file-name "Use Bookmark File: ")))
    (if globally
        (setq bookmark-default-file bookmark-filename)
      (setq-local bookmark-default-file bookmark-filename))))

;; -----------------------------------------------------------------------------
(provide 'pel-bookmark)

;;; pel-bookmark.el ends here

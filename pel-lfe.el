;;; pel-lfe.el --- LFE support.  -*- lexical-binding: t; -*-

;; Created   : Wednesday, May  5 2021.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2021-05-05 16:01:01, updated by Pierre Rouleau>

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
;;  Preliminary support for LFE - Lisp Flavored Erlang.

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;  Currently loads LFE support lazily.
(require 'pel-comment)                 ; use: pel-delete-all-comments
;;; --------------------------------------------------------------------------
;;; Code:
;;

;;-pel-autoload
(defun pel-lfe-eval-buffer (&optional and-go)
  "Send the complete buffer to the inferior LFE process.
Start the inferior LFE process if it's not already running.
Switch to the LFE buffer afterwards when AND-GO argument is non-nil."
  (interactive "P")
  (if (and (require 'inferior-lfe nil :no-error)
           (require 'lfe-mode     nil :no-error)
           (fboundp 'lfe-mode)
           (fboundp 'lfe-eval-region)
           (fboundp 'inferior-lfe))
      (progn
        (unless (get-buffer "*inferior-lfe*")
          (let ((original-window (selected-window)))
            (inferior-lfe nil)
            (select-window original-window)))
        (let ((original-buffer (current-buffer)))
          (with-temp-buffer
            (insert-buffer-substring original-buffer)
            (lfe-mode)
            (pel-delete-all-comments)
            (lfe-eval-region (point-min) (point-max) and-go))))
    (user-error "Can't load LFE support!")))

;;; --------------------------------------------------------------------------
(provide 'pel-lfe)

;;; pel-lfe.el ends here

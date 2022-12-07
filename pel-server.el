;;; pel-server.el --- Emacs Server/Daemon support.  -*- lexical-binding: t; -*-

;; Created   : Wednesday, December  7 2022.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2022-12-07 18:20:36 EST, updated by Pierre Rouleau>

;; This file is part of the PEL package.
;; This file is not part of GNU Emacs.

;; Copyright (C) 2022  Pierre Rouleau
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
;; Utilities to help when using Emacs server.

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;

;;; --------------------------------------------------------------------------
;;; Code:
;;

(defun pel-shutdown-server ()
  "First confirm then prompt to save buffers then stop/kill Emacs server."
  (interactive)
  (unless (and (fboundp 'server-running-p)
               (server-running-p))
    (user-error
     "This Emacs session is not using an Emacs Server! Nothing done"))
  (when
      (y-or-n-p "Shut Emacs Server down and close all its clients?")
    (save-some-buffers)
    (kill-emacs)))

;;; --------------------------------------------------------------------------
(provide 'pel-server)

;;; pel-server.el ends here

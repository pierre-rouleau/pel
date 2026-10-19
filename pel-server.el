;;; pel-server.el --- Emacs Server/Daemon support.  -*- lexical-binding: t; -*-

;; Created   : Wednesday, December  7 2022.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2026-10-19 15:39:04 EDT, updated by Pierre Rouleau>

;; This file is part of the PEL package.
;; This file is not part of GNU Emacs.

;; Copyright (C) 2022, 2026  Pierre Rouleau
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
(require 'pel--options)   ; use: `pel-use-easysession'
;;; --------------------------------------------------------------------------
;;; Code:
;;

(defun pel-shutdown-server ()
  "Prompt to shutdown the Emacs server and all clients.
Save easy-session if active."
  (interactive)
  (unless (and (fboundp 'server-running-p)
               (server-running-p))
    (user-error
     "This Emacs session is not using an Emacs Server! Nothing done"))

  ;; Proceed after confirmation
  (if (yes-or-no-p "Shut down the Emacs server and kill all clients?")
      (progn
        (when (and (featurep 'easysession)
                   (boundp 'easysession--current-session-name)
                   easysession--current-session-name)
          ;; Prompt to save the active session
          (when (yes-or-no-p (format "Save active session '%s'? "
                                     easysession--current-session-name))
            (if (fboundp 'easysession-save)
                (easysession-save)
              (display-warning 'pel-shutdown-server
                               "Can't save easy-session:\
 `easysession-save' function not found."
                               :error))))

        ;; Proceed with killing the server and clients
        (save-buffers-kill-emacs))
    (message "Shutdown cancelled.")))


;;; --------------------------------------------------------------------------
(provide 'pel-server)

;;; pel-server.el ends here

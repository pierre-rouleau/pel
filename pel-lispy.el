;;; pel-lispy.el --- Lispy loader with PEL hydra support.  -*- lexical-binding: t; -*-

;; Created   : Monday, September 14 2020.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2021-03-06 13:27:52, updated by Pierre Rouleau>

;; This file is part of the PEL package.
;; This file is not part of GNU Emacs.

;; Copyright (C) 2020, 2021  Pierre Rouleau
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

;;; ----------------------------------------------------------------------------
;;; Commentary:
;;
;; PEL Hydras all use the same prefix key: the F7 key.
;; PEL does not identify F7 as a prefix key until Hydra is loaded.
;; The hydra package is loaded on demand, only when a key requests it.  That key
;; is also set to F7.  At first F7 is bound to `pel--load-hydra'.  The role of
;; that function is to load hydra, which creates new key bindings that use F7 as
;; a prefix key, then simulate a new F7 key event to simulate the wanted f7
;; prefix as if it was always available and then destroys the `pel--load-hydra'
;; function to prevent it from interfering with the binding.
;;
;; This works fine as long as no external package attempts to load hydra before
;; the above `pel--load-hydra' had a chance to run.
;;
;; This pel-lispy module exists solely to handle this problem since lispy loads
;; hydra. All it does is run `pel--load-hydra' if it is still available and then
;; load lispy. This prevents the generation of a warning that would have shown
;; up otherwise.

;; -----------------------------------------------------------------------------
;;; Dependencies:
;;
;; The function `pel--load-hydra' is defined in pel_keys.el, which has been
;; loaded by the time pel-lispy is loaded.  Since we need to check if
;; `pel--load-hydra' still exists to call it, the `fboundp' check does what is
;; needed.

(when (fboundp 'pel--load-hydra)
  (pel--load-hydra :no-request)
  (pel-require 'lispy :install-when-missing))

;;; ----------------------------------------------------------------------------
;;; Code:
;;


(defun pel-lispy-mode ()
  "Activate (then toggle) lispy mode.
This PEL function acts as a proxy to the real function
`lispy-mode' to ensure that the PEL setup is taken into account."
  (interactive)
  ;; If Hydra setup was not completed, complete it and then load lispy
  (if (fboundp 'pel--load-hydra)
      (progn
        ;; Set up PEL Hydra, removing the global F7 key.
        (pel--load-hydra :no-request)
        ;; then install lispy if it is not already installed
        (when (fboundp 'package-installed-p)
          (unless (package-installed-p 'lispy)
            (package-refresh-contents)
            (package-install 'lispy)))
        ;; load lispy
        (load "lispy")
        ;; and activate the lispy-mode
        (if (fboundp 'lispy-mode)
            (lispy-mode 1)
          (error "Failed to load lispy!")))
    ;; All other times, just toggle the lispy-mode
    (if (and (require 'lispy nil :no-error)
             (fboundp 'lispy-mode)
             (boundp  'lispy-mode))
        (lispy-mode (if lispy-mode -1 1))
      (error "Failed to toggle lispy!"))))

(defun pel-lpy-mode ()
  "Activate (then toggle) lpy mode.
This PEL function acts as a proxy to the real function
`lpy-mode' to ensure that the PEL setup is taken into account."
  (interactive)
  ;; If Hydra setup was not completed, complete it and then load lpy
  (if (fboundp 'pel--load-hydra)
      (progn
        ;; Set up PEL Hydra, removing the global F7 key.
        (pel--load-hydra :no-request)
        ;; then install lpy if it is not already installed
        (when (fboundp 'package-installed-p)
          (unless (package-installed-p 'lpy)
            (package-refresh-contents)
            (package-install 'lpy)))
        ;; load lpy
        (load "lpy")
        ;; and activate the lpy-mode
        (if (fboundp 'lpy-mode)
            (lpy-mode 1)
          (error "Failed to load lpy!")))
    ;; All other times, just toggle the lpy-mode
    (if (and (fboundp 'lpy-mode)
             (boundp  'lpy-mode))
        (lpy-mode (if lpy-mode -1 1))
      (error "Failed to load lpy!"))))

;;; ----------------------------------------------------------------------------
(provide 'pel-lispy)

;;; pel-lispy.el ends here

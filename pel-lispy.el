;;; pel-lispy.el --- Lispy loader with PEL hydra support.  -*- lexical-binding: t; -*-

;; Created   : Monday, September 14 2020.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2021-03-20 10:05:19, updated by Pierre Rouleau>

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
;; loaded by the time pel-lispy is loaded.  The existence of that function is
;; also used to indicate whether the `hydra' external package is loaded: the
;; function is bound when the `hydra' library has not yet been loaded.
;; The `lispy' library uses the `hydra' library.  Under normal circumstances,
;; when `lispy' is installed its required `hydra' library is also
;; installed.
;;
;; However, PEL supports the ability to turn off features via customization
;; and it would be possible to leave `pel-use-lispy' on and turn
;; `pel-use-hydra' off which could mean that the `hydra' package could be
;; removed while lispy remains.  Under these condition a ``(require 'lispy)``
;; form may fail.
;;
;; To prevent any failure, the code ensures the presence of the `hydra'
;; package.

(require 'pel--base)
(require 'pel--options)

;;; ----------------------------------------------------------------------------
;;; Code:
;;

;; lispy mode
;; ----------

(defun pel--get-hydra-and-lispy ()
  "Ensure Hydra and lispy are available, install them if necessary.
Return t when lispy is loaded, nil otherwise"
  (when (or (not pel-use-hydra)
            (fboundp 'pel--load-hydra))
    ;; The hydra system is not yet loaded.
    ;; Prevent the possibility that the external `hydra' package is missing:
    ;; install it if it is missing.
    (pel-ensure-package hydra from: melpa)
    ;; If PEL is configured to use the Hydra package, set it up.
    (when (fboundp 'pel--load-hydra)
      (pel--load-hydra :dont-simulate-the-f7-key)))

  ;; Now ensure lispy is installed and loaded.
  (pel-ensure-package lispy from: melpa)
  (require 'lispy nil :no-error))

(defun pel-lispy-mode ()
  "Activate or toggle lispy mode.
This PEL function acts as a proxy to the real function
`lispy-mode' to ensure that the PEL setup is taken into account."
  (interactive)
  (if  (or (not (boundp 'lispy-mode))
           (not (fboundp 'lispy-mode)))
      ;; lispy is not loaded
      ;; ensure that both Hydra and Lispy packages are available.
      ;; then activate it
      (if (and (pel--get-hydra-and-lispy)
               (fboundp 'lispy-mode))
          (lispy-mode 1)
        (error "Failed to activate Lispy, was Hydra removed? Try again or restart Emacs."))
    ;; lispy is already loaded.
    ;; It might never have been called and the variable may not
    ;; yet be bound though.
    (declare-function lispy-mode "lispy")
    (if (boundp 'lispy-mode)
        ;; if the variable is bound, toggle lispy mode
        (lispy-mode (if lispy-mode -1 1))
      ;; otherwise just activate lispy mode
      (lispy-mode 1))))

;; lpy mode
;; --------

(defun pel--get-hydra-lispy-and-lpy ()
  "Ensure Hydra, lispy and lpy are available, install them if necessary.
Return t when lpy is loaded, nil otherwise"
  (when (pel--get-hydra-and-lispy)
    ;; ensure that the lpy package is installed and loaded
    (pel-ensure-package lpy from: melpa)
    (require 'lpy nil :no-error)))

(defun pel-lpy-mode ()
  "Activate or toggle lpy mode.
This PEL function acts as a proxy to the real function
`lpy-mode' to ensure that the PEL setup is taken into account."
  (interactive)
  (if  (not (fboundp 'lpy-mode))
      ;; lpy is not loaded
      ;; ensure that both Hydra and Lispy packages are available.
      ;; then activate it
      (if (and (pel--get-hydra-lispy-and-lpy)
               (fboundp 'lpy-mode))
          (lpy-mode 1)
        (error "Failed to activate Lpy!"))
    ;; lpy is already loaded.
    ;; It might never have been called and the variable may not
    ;; yet be bound though.
    (declare-function lpy-mode "lpy")
    (if (boundp 'lpy-mode)
        ;; if the variable is bound, toggle lpy mode
        (lpy-mode (if lpy-mode -1 1))
      ;; otherwise just activate lpy mode
      (lpy-mode 1))))

;;; ----------------------------------------------------------------------------
(provide 'pel-lispy)

;;; pel-lispy.el ends here

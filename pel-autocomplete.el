;;; pel-autocomplete.el --- PEL auto-completion support -*-lexical-binding: t-*-

;; Copyright (C) 2020, 2021, 2023, 2024, 2025  Pierre Rouleau

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

;;; --------------------------------------------------------------------------
;;; Commentary:
;;
;; This file holds code that manages the use of various auto-completion
;; facilities for Emacs.  At this point it supports the following:
;;
;; - auto-complete-mode
;; - company-mode
;;
;; The features are:
;;
;; - On a given buffer, or globally, don't allow both modes active at the same
;;   time as they have not been designed to be used together.
;; - Provide the user to see what is activeand the commands to activate or
;;   de-activate  one of them, as long as the other is not active.
;; - Support the modes customization variable, allowing the user to selct the
;;   behaviour of these modes to the customization variables of these modes, as
;;   well as the PEL customization variables used to activate one of them, but
;;   not both.
;; - The customization variables known as this code is written are:
;;   - PEL customization variables:
;;     - pel-use-auto-complete
;;     - pel-use-company
;;   - auto-complete variables:
;;     - global-auto-complete-mode
;;     - auto-complete-mode
;;   - company variables:
;;     - global-company-mode
;;     - company-mode

;;; --------------------------------------------------------------------------
;;; Dependencies:
(require 'pel--base)       ; use: pel-symbol-text
;;                         ;      pel-option-mode-state
;;                         ;      pel-symbol-on-off-string
(require 'pel--macros)     ; use: pel-when-bound, pel-when-fbound
(require 'pel--options)


(eval-when-compile
  ;; need macros from pel--base
  ;; The following 2 packages are loaded lazily when required:
  ;; their autoload is configured by pel.el.
  ;; Here we just need it to verify functions and variables
  ;; at byte-compile time.  However, allow user to byte compile\
  ;; the file even if the packages are not installed.
  (require 'auto-complete nil :noerror)
  (require 'company nil :noerror))

;;; --------------------------------------------------------------------------
;;; Code:


;; ---------------------------------------------------------------------------
;; Utilities

(defun pel--action-for (arg current-state)
  "Return the new state of a mode from ARG and CURRENT-STATE.
Where:
- ARG:           nil: toggle, >=0: enable, <0 : disable.
- CURRENT-STATE: nil: disabled, otherwise enabled.

Return: enable or disable"
  (cond ((null arg) (if current-state 'disable 'enable))
        ((>= arg 0) 'enable)
        (t          'disable)))


;; For both auto-complete-mode and company-mode:
;; ARG : nil | positive --> enable
;;       negative       --> disable
(defun pel--arg-for-action (action)
  "Return integer corresponding to ACTION.
1 for enable, -1 for disable."
  (if (eq action 'enable)
      1
    -1))

;; -----------------------------------------------------------------------------
;; Auto-Complete Support
;; =====================
;;
;; Auto-Complete Initialization
;; ----------------------------
;;
;; TODO: update for finer control once PEL has explicit support for various
;;       programming languages.
;; The pel--setup-auto-complete function currently contains the bare bone
;; initialization code for auto-complete, the code that is often shown on how to
;; set it up.  This is the default setup. Eventually this code should be
;; modified to support better customization.
;;
(defun pel--setup-auto-complete ()
  "Initialize `auto-complete'."
  ;; Write extra support here
  (pel-when-fbound 'ac-config-default
    (ac-config-default)))

;; --
;; Utilities: return state of auto-complete variables that may be unbound

(defun pel--auto-complete-mode-p ()
  "Return t if variable `auto-complete-mode' is loaded and on, nil otherwise."
  (bound-and-true-p auto-complete-mode))

(defun pel--global-auto-complete-mode-p ()
  "Return t if variable `global-auto-complete-mode' is loaded and on.
Return nil otherwise."
  (bound-and-true-p global-auto-complete-mode))

;; --
;; Utilities: turn auto-complete modes on

(defun pel--global-auto-complete-mode-on ()
  "Turn option `global-auto-complete-mode' ON.
On first call, also configure it."
  (if (not (featurep 'auto-complete))
      (pel--setup-auto-complete))
  (pel-when-fbound 'global-auto-complete-mode
    (global-auto-complete-mode t)))

(defun pel--auto-complete-mode-on ()
  "Turn option `auto-complete-mode' ON.
On first call, also configure it according to its customization."
  (let ((is-first-call (not (featurep 'auto-complete)))
        ;; remember the way global autocomplete mode was
        ;; before calling its setup (in case it sets global mode on)
        (global-ac   (pel--global-auto-complete-mode-p)))
    (pel-when-fbound 'auto-complete-mode
      (auto-complete-mode 1))
    (if is-first-call
        (progn
          (pel--setup-auto-complete)
          ;; When the customization variable is requires global
          ;; activation, turn global activation on.
          (pel-when-fbound 'global-auto-complete-mode
            (if global-ac
                (global-auto-complete-mode t)
              ;; otherwise ensure it's off
              (global-auto-complete-mode -1)))))))

;; -----------------------------------------------------------------------------
;; Company Mode Support
;; ====================
;;
;; Company Mode Initialization
;; ---------------------------
;; TODO: update for finer control once PEL has explicit support for various
;;       programming languages.
(defun pel--setup-company ()
  "Initialize Company Mode."
  ;; Write extra support here
  ;; variables affecting automatic completion:
  ;; - company-idle-delay
  ;; - company-minimum-prefix-length
  (if (and (boundp 'company-tooltip-align-annotations)
           (boundp 'company-show-numbers))
      (progn
        (setq company-tooltip-align-annotations t)
        (setq company-show-numbers t))
    (error "Package company not loaded")))

;; --
;; Utilities: return state of company-mode variables that may be unbound

(defun pel--company-mode-p ()
  "Return t if variable `company-mode' is loaded and on, nil otherwise."
  (bound-and-true-p company-mode))

(defun pel--global-company-mode-p ()
  "Return t if variable `global-company-mode' is loaded and on, nil otherwise."
  (bound-and-true-p global-company-mode))

;; --
;; Utilities: turn Company Modes on

(defun pel--global-company-mode-on ()
  "Turn option `global-company-mode' ON.
On first call, also configure it."
  (if (not (featurep 'company))
      (pel--setup-company))
  (pel-when-fbound 'global-company-mode
    (global-company-mode t)))

(defun pel--company-mode-on ()
  "Turn option `company-mode' ON.
On first call, also configure it according to its customization."
  (let ((is-first-call (not (featurep 'company)))
        ;; remember the way global company mode was
        ;; before calling its setup (in case it sets global mode on)
        (global-cm   (pel--global-company-mode-p)))
    (pel-when-fbound 'company-mode
      (company-mode 1))
    (if is-first-call
        (progn
          (pel--setup-company)
          ;; When the customization variable is requires global
          ;; activation, turn global activation on.
          (pel-when-fbound 'global-company-mode
            (if global-cm
                (global-company-mode t)
              ;; otherwise it's off
              (global-company-mode -1)))))))

;; --
;; PEL Auto Complete Commands

;;-pel-autoload
(defun pel-global-auto-complete-mode (&optional arg)
  "Toggle Global Auto Complete mode when ARG is nil.
If ARG is positive: activate it, otherwise de-activate it.
Does not allow activation if Company Mode is active."
  (interactive "P")
  (let ((action (pel--action-for arg (pel--global-auto-complete-mode-p))))
    (if (and (eq action 'enable)
             (pel--company-mode-p))
        (user-error "First turn company-mode off!")
      ;; Prevent byte-compiler warning.  PEL won't invoke this command  if
      ;; it's not already loaded and bound, as controlled by pel_keys.el
      (pel-when-fbound 'global-auto-complete-mode
        (global-auto-complete-mode (pel--arg-for-action action))))))

;;-pel-autoload
(defun pel-auto-complete-mode (&optional arg)
  "Toggle buffer's Auto Complete mode when ARG is nil.
If ARG >= 0: activate it, otherwise de-activate it.
Does not allow activation if Company Mode is active."
  (interactive "P")
  (let ((action (pel--action-for arg (pel--auto-complete-mode-p))))
    (if (and (eq action 'enable)
             (pel--company-mode-p))
        (user-error "First turn company-mode off!")
      ;; Prevent byte-compiler warning.  PEL won't invoke this command  if
      ;; it's not already loaded and bound, as controlled by pel_keys.el
      (pel-when-fbound 'auto-complete-mode
        (auto-complete-mode (pel--arg-for-action action))))))

;; --
;; PEL Company Mode Commands

;;-pel-autoload
(defun pel-global-company-mode (&optional arg)
  "Toggle Global Company mode when ARG is nil.
If ARG is positive: activate it, otherwise de-activate it.
Does not allow activation if Auto Complete Mode is active."
  (interactive "P")
  (let ((action (pel--action-for arg (pel--global-company-mode-p))))
    (if (and (eq action 'enable)
             (pel--auto-complete-mode-p))
        (user-error "First turn auto-complete-mode off!")
      ;; Prevent byte-compiler warning.  PEL won't invoke this command  if
      ;; it's not already loaded and bound, as controlled by pel_keys.el
      (pel-when-fbound 'global-company-mode
        (global-company-mode (pel--arg-for-action action))))))

;;-pel-autoload
(defun pel-company-mode (&optional arg)
  "Toggle buffer's Company Mode when ARG is nil.
If ARG >= 0: activate it, otherwise de-activate it.
Does not allow activation if Auto Complete Mode is active."
  (interactive "P")
  (let ((action (pel--action-for arg (pel--company-mode-p))))
    (if (and (eq action 'enable)
             (pel--auto-complete-mode-p))
        (user-error "First turn auto-complete-mode off!")
      ;; Prevent byte-compiler warning.  PEL won't invoke this command  if
      ;; it's not already loaded and bound, as controlled by pel_keys.el
      (pel-when-fbound 'company-mode
        (company-mode (pel--arg-for-action action))))))

;; --
;; PEL Generic Automatic Completion Commands

;;-pel-autoload
(defun pel-completion-info (&optional append)
  "Display information about available auto-completion in specialized buffer.

Show which one is enabled via customization, and show current
activation state.

Clear previous buffer content unless optional APPEND argument is
non-nil, in which case it appends to the previous report."
  (interactive "P")
  (let ((pel-insert-symbol-content-context-buffer (current-buffer)))
    (pel-print-in-buffer
     "*pel-autocomplete-info*"
     "Auto-Completion Control"
     (lambda ()
       (insert
        (format "\
Auto-completion package state:
%s
- auto-complete-mode                      : %s
- global-auto-complete-mode               : %s
- company-mode                            : %s
- global-company-mode                     : %s"
                (if pel-emacs-30-or-later-p
                    (format "\
- completion-preview-mode                 : %s"
                            (pel-minor-mode-state 'completion-preview-mode ))

                  "")
                (pel-option-mode-state 'auto-complete-mode 'pel-use-auto-complete)
                (pel-symbol-on-off-string 'global-auto-complete-mode)
                (pel-option-mode-state 'company-mode 'pel-use-company)
                (pel-symbol-on-off-string 'global-company-mode)))
       (insert "\n\nCustomization:")
       (when pel-emacs-30-or-later-p
         (pel-insert-symbol-content-line 'completion-preview-minimum-symbol-length)
         (pel-insert-symbol-content-line 'completion-preview-idle-delay))
       (pel-insert-symbol-content-line 'pel-use-auto-complete)
       (pel-insert-symbol-content-line 'pel-use-company)
       (insert "\n")
       (pel-insert-symbol-content-line 'auto-complete-mode)
       (pel-insert-symbol-content-line 'global-auto-complete-mode)
       (pel-insert-symbol-content-line 'company-mode)
       (pel-insert-symbol-content-line 'global-company-mode)

       (insert "\n\nEmacs Built-in completion:")
       (pel-insert-symbol-content-line 'completion-at-point-functions)
       (pel-insert-symbol-content-line 'completion-styles)
       (pel-insert-symbol-content-line 'completion-category-overrides)
       (pel-insert-symbol-content-line 'completion-extra-properties)
       (pel-insert-symbol-content-line 'completion-styles-alist))
     (unless append :clear-buffer)
     :use-help-mode)))

;;-pel-autoload
(defun pel-complete ()
  "Start auto-completion for text at point.
Use the currently active auto-completion system."
  (interactive)
  (cond ((pel--auto-complete-mode-p) (pel-when-fbound 'auto-complete
                                       (auto-complete)))
        ((pel--company-mode-p)       (pel-when-fbound 'company-complete
                                       (company-complete)))
        (t (user-error "First activate a completion system \
with: <f11> , [aAcC]"))))

;; -----------------------------------------------------------------------------
(provide 'pel-autocomplete)

;;; pel-autocomplete.el ends here

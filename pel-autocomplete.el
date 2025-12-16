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
;; - corfu
;;
;; The features are:
;;
;; - On a given buffer, or globally, only allow one of these mode active; they
;;   are mutually exclusive minor modes with global minor mode support.
;; - Provide the user to see what is active and the commands to activate or
;;   de-activate  one of them, as long as the other is not active.
;; - Support the modes customization variable, allowing the user to select the
;;   behaviour of these modes to the customization variables of these modes,
;;   as well as the PEL customization variables used to activate one of them,
;;   but not both.
;; - The customization variables are:
;;   - PEL customization variables:
;;     - `pel-use-auto-complete'
;;     - `pel-use-company'
;;     - `pel-use-corfu' (with the associated `pel-use-corfu-terminal')
;;
;;   - auto-complete variables:
;;     - `global-auto-complete-mode'
;;     - `auto-complete-mode'
;;   - company variables:
;;     - `global-company-mode'
;;     - `company-mode'
;;   - corfu variables:
;;     - `corfu-mode'
;;     - `global-corfu-mode'

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
  (require 'company nil :noerror)
  (require 'corfu nil :noerror)
  (require 'corfu-terminal nil :noerror))

;;; --------------------------------------------------------------------------
;;; Code:

;;* Utilities
;;  ---------

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

(defun pel--autocomplete-active-mode ()
  "Return symbol of currently active auto-completion mode is any.
If none is active, return nil."
  (cond
   ((bound-and-true-p global-company-mode) 'global-company-mode)
   ((bound-and-true-p company-mode) 'company-mode)
   ((bound-and-true-p global-auto-complete-mode) 'global-auto-complete-mode)
   ((bound-and-true-p auto-complete-mode) 'auto-complete-mode)
   ((bound-and-true-p global-corfu-mode) 'global-corfu-mode)
   ((bound-and-true-p corfu-mode) 'corfu-mode)
   (t nil)))
;; -----------------------------------------------------------------------------
;;* Auto-Complete Support
;;  =====================
;;
;;** Auto-Complete Initialization
;;   ----------------------------
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
;;* Company Mode Support
;;  ====================
;;
;;** Company Mode Initialization
;;   ---------------------------
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
  (let ((active-mode (pel--autocomplete-active-mode))
        (action (pel--action-for arg (pel--global-auto-complete-mode-p))))
    (if (and (eq action 'enable)
             active-mode)
        (user-error "First turn %s off!" active-mode)
      (pel-when-fbound 'global-auto-complete-mode
        (global-auto-complete-mode (pel--arg-for-action action))
        (message (pel-value-on-off-text "global-auto-complete"
                                        (eq action 'enable)))))))

;;-pel-autoload
(defun pel-auto-complete-mode (&optional arg)
  "Toggle buffer's Auto Complete mode when ARG is nil.
If ARG >= 0: activate it, otherwise de-activate it.
Does not allow activation if Company Mode is active."
  (interactive "P")
  (let ((active-mode (pel--autocomplete-active-mode))
        (action (pel--action-for arg (pel--auto-complete-mode-p))))
    (if (and (eq action 'enable)
             active-mode)
        (user-error "First turn %s off!" active-mode)
      (pel-when-fbound 'auto-complete-mode
        (auto-complete-mode (pel--arg-for-action action))
        (message (pel-value-on-off-text "auto-complete"
                                        (eq action 'enable)))))))

;; --
;; PEL Company Mode Commands

;;-pel-autoload
(defun pel-global-company-mode (&optional arg)
  "Toggle Global Company mode when ARG is nil.
If ARG is positive: activate it, otherwise de-activate it.
Does not allow activation if Auto Complete Mode is active."
  (interactive "P")
  (let ((active-mode (pel--autocomplete-active-mode))
        (action (pel--action-for arg (pel--global-company-mode-p))))
    (if (and (eq action 'enable)
             active-mode)
        (user-error "First turn %s off!" active-mode)
      (pel-when-fbound 'global-company-mode
        (global-company-mode (pel--arg-for-action action))
        (message (pel-symbol-text 'global-company-mode))))))

;;-pel-autoload
(defun pel-company-mode (&optional arg)
  "Toggle buffer's Company Mode when ARG is nil.
If ARG >= 0: activate it, otherwise de-activate it.
Does not allow activation if Auto Complete Mode is active."
  (interactive "P")
  (let ((active-mode (pel--autocomplete-active-mode))
        (action (pel--action-for arg (pel--company-mode-p))))
    (if (and (eq action 'enable)
             active-mode)
        (user-error "First turn %s off!" active-mode)
      (pel-when-fbound 'company-mode
        (company-mode (pel--arg-for-action action))
        (message (pel-symbol-text 'company-mode))))))

;; ---------------------------------------------------------------------------
;;* Corfu Support
;;  =============

(defun pel--corfu-mode-p ()
  "Return t if variable `corfu-mode' is loaded and on, nil otherwise."
  (if (and pel-use-corfu-terminal
           (not pel-emacs-is-graphic-p))
      (bound-and-true-p corfu-terminal-mode)
    (bound-and-true-p corfu-mode)))

(defun pel--global-corfu-mode-p ()
  "Return t if variable `global-corfu-mode' is loaded and on.
Return nil otherwise."
  (if (and pel-use-corfu-terminal
           (not pel-emacs-is-graphic-p))
      ;; corfu-terminal-mode is a global minor mode
      (bound-and-true-p corfu-terminal-mode)
    (bound-and-true-p global-corfu-mode)))

;;-pel-autoload
(defun pel-global-corfu-mode (&optional arg)
  "Toggle Global Corfu mode when ARG is nil.
If ARG is positive: activate it, otherwise de-activate it.
Does not allow activation if another completion is active."
  (interactive "P")
  (let ((active-mode (pel--autocomplete-active-mode))
        (action (pel--action-for arg (pel--global-corfu-mode-p))))
    (if (and (eq action 'enable)
             active-mode)
        (user-error "First turn %s off!" active-mode)
      (if (and pel-use-corfu-terminal
               (not pel-emacs-is-graphic-p))
          ;; corfu-terminal-mode is a global minor mode!
          (pel-when-fbound 'corfu-terminal-mode
            (corfu-terminal-mode (pel--arg-for-action action))
            (message (pel-value-on-off-text "corfu-terminal"
                                            (eq action 'enable))))
        (pel-when-fbound 'global-corfu-mode
          (global-corfu-mode (pel--arg-for-action action))
          (message (pel-value-on-off-text "global-corfu"
                                          (eq action 'enable))))))))


;;-pel-autoload
(defun pel-corfu-mode (&optional arg)
  "Toggle buffer's Corfu mode when ARG is nil.
If ARG >= 0: activate it, otherwise de-activate it.
Does not allow activation if another completion Mode is active."
  (interactive "P")
  (let ((active-mode (pel--autocomplete-active-mode))
        (action (pel--action-for arg (pel--corfu-mode-p))))
    (message "active-mode: %s, action: %s" active-mode action)
    (if (and (eq action 'enable)
             active-mode)
        (user-error "First turn %s off!" active-mode)
      (if (and pel-use-corfu-terminal
               (not pel-emacs-is-graphic-p))
          ;; corfu-terminal-mode is a global minor mode!
          (pel-when-fbound 'corfu-terminal-mode
            (corfu-terminal-mode (pel--arg-for-action action))
            (message (pel-value-on-off-text "corfu-terminal"
                                            (eq action 'enable))))
        (pel-when-fbound 'corfu-mode
          (corfu-mode (pel--arg-for-action action))
          (message (pel-value-on-off-text "corfu"
                                          (eq action 'enable))))))))

;; ---------------------------------------------------------------------------
;;* PEL Generic Automatic Completion Commands
;;  =========================================
;;
;;  * pel-complete
;;    - pel--auto-complete-mode-p
;;    - auto-complete
;;    - pel--company-mode-p
;;    - company-complete
;;  * pel-completion-info
;;    - pel--built-in-minor-mode-state-string

(defun pel--built-in-minor-mode-state-string (mode)
  "Built in minor MODE (a symbol) state string."
  (pel-minor-mode-state mode))

(defun pel-insert-mode-state-and-use (mode use-symbol)
  "Insert information about MODE symbol and its USE-SYMBOL activator."
  (pel-insert-symbol-content-line mode
                                  nil
                                  (lambda (m)
                                    (pel-minor-mode-state m use-symbol))))

(defun pel-insert-mode-state (mode)
  "Insert information about MODE symbol only."
  (pel-insert-symbol-content-line mode
                                  nil
                                  (when (boundp mode)
                                    #'pel-symbol-on-off-string)))

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
       (pel-insert-bold "***Auto-completion package state:")
       (insert "\n* built-in:")
       (pel-insert-symbol-content-line 'completion-preview-mode nil
                                       #'pel--built-in-minor-mode-state-string)
       (insert "\n\n* External:")
       (pel-insert-mode-state-and-use  'auto-complete-mode
                                       'pel-use-auto-complete)

       (pel-insert-mode-state          'global-auto-complete-mode)
       (insert "\n")
       (pel-insert-mode-state-and-use  'company-mode 'pel-use-company)
       (pel-insert-mode-state          'global-company-mode)
       (insert "\n")
       (pel-insert-mode-state-and-use  'corfu-mode 'pel-use-corfu)
       (pel-insert-mode-state          'global-corfu-mode)
       (when (and pel-use-corfu-terminal
                  (not pel-emacs-is-graphic-p))
         (pel-insert-mode-state        'corfu-terminal-mode))

       (pel-insert-bold "\n\n****Customization:")
       (insert "\n* Activation:")
       (pel-insert-symbol-content-line 'pel-use-auto-complete)
       (pel-insert-symbol-content-line 'pel-use-company)
       (pel-insert-symbol-content-line 'pel-use-corfu)
       (insert "\n\n* Control:")
       (when pel-emacs-30-or-later-p
         (pel-insert-symbol-content-line 'completion-preview-minimum-symbol-length)
         (pel-insert-symbol-content-line 'completion-preview-idle-delay))
       (pel-insert-symbol-content-line 'completion-at-point-functions)
       (pel-insert-symbol-content-line 'completion-styles)
       (pel-insert-list-content 'completion-category-overrides
                                nil nil nil
                                :on-sameline)
       (pel-insert-list-content 'completion-extra-properties
                                nil nil nil
                                :on-sameline)
       (pel-insert-list-content 'completion-styles-alist
                                nil nil nil
                                :on-sameline))
     (unless append :clear-buffer)
     :use-help-mode)))

;;-pel-autoload
(defun pel-complete ()
  "Start auto-completion for text at point.
Use the currently active auto-completion system."
  (interactive)
  (cond ((pel--auto-complete-mode-p)
         (pel-when-fbound 'auto-complete (auto-complete)))
        ((pel--company-mode-p)
         (pel-when-fbound 'company-complete (company-complete)))
        ((pel--corfu-mode-p)
         (pel-when-fbound 'corfu-complete (corfu-complete)))
        (t (user-error "First activate a completion system \
with: <f11> , [aAcCuU]"))))

;; ---------------------------------------------------------------------------
(defvar pel--autocomplete-initialized nil
  "Set to t when auto-complete tool management is initialized.
Modified by pel-search code ONLY.")

(defvar pel--active-search-tool nil
  "Search tool currently used.  One of: nil | anzu | swiper.
A nil value means that Emacs standard search is used.")

;;-pel-autoload
;; (defun pel-select-autocomplete ()
;;   "Prompt user for auto-complete tool to use."
;;   (interactive)
;;   (unless pel--autocomplete-initialized
;;     ;; select the initial search tool from user option.
;;     (pel-set-search-tool pel-initial-search-tool)
;;     (setq pel--autocomplete-initialized 1))
;;   ;;
;;   (pel-select-from "Search tool"
;;                    (pel--search-tools-selection)
;;                    (pel--activated-search-tool)
;;                    #'pel-set-search-tool))
;; -----------------------------------------------------------------------------
(provide 'pel-autocomplete)

;;; pel-autocomplete.el ends here

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
(require 'pel-prompt)      ; use: `pel-select-from'

;;; --------------------------------------------------------------------------
;;; Code:

;; -----------------------------------------------------------------------------
;;* Auto-Complete Support
;;  =====================
;;
(defvar-local pel--used-auto-completion-tool nil
  "Auto-completion tool currently used in the buffer.")

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

;; --
;; State of auto-complete-mode
(defun pel--auto-complete-mode-p ()
  "Return t `auto-complete-mode' is loaded and on, nil otherwise."
  (bound-and-true-p auto-complete-mode))

(defun pel--global-auto-complete-mode-p ()
  "Return t if `global-auto-complete-mode' is loaded and on.
Return nil otherwise."
  (bound-and-true-p global-auto-complete-mode))

;; --
;; Turn auto-complete modes off
(defun pel--global-auto-complete-mode-off ()
  "Turn `global-auto-complete-mode' OFF"
  (pel-when-fbound 'global-auto-complete-mode
    (global-auto-complete-mode -1)))

(defun pel--auto-complete-mode-off ()
  "Turn `auto-complete-mode' OFF"
  (pel-when-fbound 'auto-complete-mode
    (auto-complete-mode -1)))

;; --
;; Turn auto-complete modes on

(defun pel--setup-auto-complete ()
  "Initialize `auto-complete'."
  ;; [:todo 2025-12-17, by Pierre Rouleau: Clarify what ac-config-default does
  ;; (it's not documented) and determine if something is needed for each
  ;; programming major mode]
  (if pel-use-auto-complete
      (progn
        (require 'auto-complete nil :noerror)
        (pel-when-fbound 'ac-config-default
          (ac-config-default)))
    (display-warning 'pel-ensure-package "\
Trying to use auto-complete while pel-use-auto-complete user-option is off."
                     :error)))

(defun pel--global-auto-complete-mode-on ()
  "Turn `global-auto-complete-mode' ON.
On first call, also configure it."
  (pel--setup-auto-complete)
  (pel-when-fbound 'global-auto-complete-mode
    (global-auto-complete-mode 1)))

(defun pel--auto-complete-mode-on ()
  "Turn `auto-complete-mode' ON.
On first call, also configure it."
  (pel--setup-auto-complete)
  (pel-when-fbound 'auto-complete-mode
    (auto-complete-mode 1)))

;;-pel-autoload
(defun pel-auto-complete-mode (arg)
  "Activate or deactivate auto-complete completion engine in buffer.
Activate when ARG is nil, t or positive, deactivate when it is negative."
  (interactive "P")
  (cond
   ((or (eq arg t)
        (> (prefix-numeric-value arg) 0))
    (pel-select-auto-completion 'auto-complete))
   ((< (prefix-numeric-value arg) 0)
    (pel-autocomplete--disable)
    (setq-local pel--used-auto-completion-tool nil))
   (t (error "Invalid Arg: %s" arg))))

;; -----------------------------------------------------------------------------
;;* Company Mode Support
;;  ====================
;;
;;** Company Mode Initialization
;;   ---------------------------
;; TODO: update for finer control once PEL has explicit support for various
;;       programming languages.

;; --
;; State of company-mode

(defun pel--company-mode-p ()
  "Return t `company-mode' is loaded and on, nil otherwise."
  (bound-and-true-p company-mode))

(defun pel--global-company-mode-p ()
  "Return t if `global-company-mode' is loaded and on, nil otherwise."
  (bound-and-true-p global-company-mode))

;; --
;; Turn company-mode off
(defun pel--global-company-mode-off ()
  "Turn `global-company-mode' OFF"
  (pel-when-fbound 'global-company-mode
    (global-company-mode -1)))

(defun pel--company-mode-off ()
  "Turn `company-mode' OFF"
  (pel-when-fbound 'company-mode
    (company-mode -1)))

;; --
;; Turn company-mode on
(defun pel--setup-company ()
  "Initialize Company Mode."
  (if pel-use-company
      (progn
        (require 'company nil :noerror)
        (if (and (boundp 'company-tooltip-align-annotations)
                 (boundp 'company-show-numbers))
            (progn
              (setq company-tooltip-align-annotations t)
              (setq company-show-numbers t))
          (error "Package company not loaded")))
    (display-warning 'pel-ensure-package "\
Trying to use company while pel-use-company user-option is off." :error)))

(defun pel--global-company-mode-on ()
  "Turn `global-company-mode' ON.
On first call, also configure it."
  (if (not (featurep 'company))
      (pel--setup-company))
  (pel-when-fbound 'global-company-mode
    (global-company-mode t)))

(defun pel--company-mode-on ()
  "Turn `company-mode' ON.
On first call, also configure it."
  (if (not (featurep 'company))
      (pel--setup-company))
  (pel-when-fbound 'company-mode
    (company-mode t)))

;;-pel-autoload
(defun pel-company-mode (arg)
  "Activate or deactivate company completion engine in buffer.
Activate when ARG is t or positive, deactivate when it is negative."
  (interactive "P")
  (cond
   ((or (eq arg t)
        (> (prefix-numeric-value arg) 0))
    (pel-select-auto-completion 'company))
   ((< (prefix-numeric-value arg) 0)
    (pel-autocomplete--disable)
    (setq-local pel--used-auto-completion-tool nil))
   (t (error "Invalid Arg: %s" arg))))

;; ---------------------------------------------------------------------------
;;* Corfu Support
;;  =============

;; --
;; State of corfu-mode
(defun pel--corfu-mode-p ()
  "Return t if variable `corfu-mode' is loaded and on, nil otherwise.
On systems that must also use `corfu-terminal-mode' also check if it is on."
  (if (and pel-use-corfu-terminal
           (not pel-emacs-is-graphic-p))
      (and (bound-and-true-p corfu-terminal-mode)
           (bound-and-true-p corfu-mode))
    (bound-and-true-p corfu-mode)))

(defun pel--global-corfu-mode-p ()
  "Return t if variable `global-corfu-mode' is loaded and on, nil otherwise.
On systems that must also use `corfu-terminal-mode' also check if it is on."
  (if (and pel-use-corfu-terminal
           (not pel-emacs-is-graphic-p))
      ;; corfu-terminal-mode is a global minor mode
      (bound-and-true-p corfu-terminal-mode)
    (bound-and-true-p global-corfu-mode)))

;; --
;; Turn corfu modes off
(defun pel--global-corfu-mode-off ()
  "Turn `global-corfu-mode' OFF"
  (pel-when-fbound 'global-corfu-mode
    (global-corfu-mode -1))
  ;; corfu-terminal-mode is a global minor mode
  (when (and pel-use-corfu-terminal
             (not pel-emacs-is-graphic-p)
             (bound-and-true-p corfu-terminal-mode)
             (fboundp 'corfu-terminal-mode))
    (corfu-terminal-mode -1)))

(defun pel--corfu-mode-off ()
  "Turn `corfu-mode' OFF"
  (pel-when-fbound 'corfu-mode
    (corfu-mode -1))
  ;; corfu-terminal-mode is a global minor mode
  (when (and pel-use-corfu-terminal
             (not pel-emacs-is-graphic-p)
             (bound-and-true-p corfu-terminal-mode)
             (fboundp 'corfu-terminal-mode))
    (corfu-terminal-mode -1)))

;; --
;; Turn corfu modes on
(defun pel--setup-corfu ()
  "Initialize `corfu'."
  (if  pel-use-corfu
      (progn
        (require 'corfu nil :noerror)
        (when (and pel-use-corfu-terminal
                   (not pel-emacs-is-graphic-p))
          (require 'corfu-terminal nil :noerror)))
    (display-warning 'pel-ensure-package "\
Trying to use corfu while pel-use-corfu user-option is off." :error)))

(defun pel--set-corfu-features ()
  "Set features used by corfu modes."
  (when (boundp 'corfu-cycle)
    (setq-local corfu-cycle t))
  (when (boundp 'corfu-auto)
    (setq-local corfu-auto t)))

(defun pel--corfu-terminal-maybe ()
  "If required activate corfu-terminal mode."
  (when (and pel-use-corfu-terminal
             (not pel-emacs-is-graphic-p)
             (fboundp 'corfu-terminal-mode))
    (corfu-terminal-mode 1)))

(defun pel--global-corfu-mode-on ()
  "Turn `global-corfu-mode' ON.
On first call, also configure it."
  (unless (featurep 'corfu)
      (pel--setup-corfu))
  (if (fboundp 'global-corfu-mode)
      (progn
        (pel--set-corfu-features)
        (global-corfu-mode 1)
        (pel--corfu-terminal-maybe))
    (user-error "global-corfu-mode not bound.  Is it loaded?")))

(defun pel--corfu-mode-on ()
  "Turn `corfu-mode' ON.
On first call, also configure it."
  (unless (featurep 'corfu)
      (pel--setup-corfu))
  (if (fboundp 'corfu-mode)
      (progn
        (pel--set-corfu-features)
        (corfu-mode 1)
        (pel--corfu-terminal-maybe))
    (user-error "corfu-mode not bound.  Is it loaded?")))

;;-pel-autoload
(defun pel-corfu-mode (arg)
  "Activate or deactivate corfu completion engine in buffer.
Activate when ARG is t or positive, deactivate when it is negative."
  (interactive "P")
  (message "pel-corfu-mode %S" arg)
  (cond
   ((or (eq arg t)
        (> (prefix-numeric-value arg) 0))
    (message "--> select corfu")
    (pel-select-auto-completion 'corfu))
   ((< (prefix-numeric-value arg) 0)
    (pel-autocomplete--disable)
    (setq-local pel--used-auto-completion-tool nil))
   (t (error "Invalid Arg: %s" arg))))

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

(defun pel-insert-list (list-var)
  "Insert information about LIST-VAR symbol with items on separate lines."
  (pel-insert-list-content list-var nil nil nil :on-sameline))

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
       (when pel-emacs-29-or-later-p
         (insert "\n")
         (pel-insert-mode-state-and-use  'corfu-mode 'pel-use-corfu)
         (pel-insert-mode-state          'global-corfu-mode)
         (when (and pel-use-corfu-terminal
                    (not pel-emacs-is-graphic-p))
           (pel-insert-mode-state        'corfu-terminal-mode)))

       (pel-insert-bold "\n\n****Customization:")
       (insert "\n* Activation:")
       (pel-insert-symbol-content-line 'pel-use-auto-complete)
       (pel-insert-symbol-content-line 'pel-use-company)
       (when pel-emacs-29-or-later-p
         (pel-insert-symbol-content-line 'pel-use-corfu))

       (pel-insert-bold "\n\n****Built-in completion control:")
       (pel-insert-bold "\n- Note: ")
       (insert "More user-options in Minibuffer customization group.")
       (pel-insert-bold "\n- Completion mechanism control:")
       (when pel-emacs-29-or-later-p
         (insert "\n   (Identify orderless or corfu-prescient here:)"))
       (pel-insert-symbol-content-line 'completion-styles)
       (pel-insert-list  'completion-category-overrides)
       (when pel-emacs-31-or-later-p
         (pel-insert-symbol-content-line 'completion-pcm-leading-wildcard))
       (pel-insert-symbol-content-line 'completion-auto-help)
       (pel-insert-symbol-content-line 'completion-cycle-threshold)
       (pel-insert-symbol-content-line 'completions-format)
       (pel-insert-symbol-content-line 'read-file-name-completion-ignore-case)
       (pel-insert-list  'completion-at-point-functions)
       (pel-insert-list  'completion-extra-properties)
       (pel-insert-list  'completion-styles-alist)

       (pel-insert-bold "\n- On Emacs 28 and later:")
       (pel-insert-symbol-content-line 'completions-group)
       (pel-insert-symbol-content-line 'completions-group-sort)
       (pel-insert-symbol-content-line 'completions-group-format)
       (pel-insert-symbol-content-line 'completions-detailed)

       (pel-insert-bold "\n- On Emacs 29 and later:")
       (pel-insert-list  'inhibit-message-regexps)
       (pel-insert-list  'set-message-functions)
       (pel-insert-symbol-content-line 'completions-header-format)
       (pel-insert-symbol-content-line 'completions-max-height)

       (pel-insert-bold "\n- On Emacs 30 and later:")
       (pel-insert-list  'minibuffer-regexp-prompts)
       (pel-insert-symbol-content-line 'completions-sort)
       (pel-insert-symbol-content-line 'completion-auto-deselect)
       (pel-insert-symbol-content-line 'minibuffer-visible-completions)

       (when pel-emacs-30-or-later-p
         (pel-insert-bold "\n\n****Completion Preview control (Emacs >= 30):")
         (pel-insert-symbol-content-line 'completion-preview-minimum-symbol-length)
         (pel-insert-symbol-content-line 'completion-preview-idle-delay))

       (pel-insert-bold "\n\n****Auto-complete customization:")
       (pel-insert-symbol-content-line 'global-auto-complete-mode)
       (pel-insert-symbol-content-line 'ac-use-quick-help)
       (pel-insert-symbol-content-line 'ac-quick-help-delay)
       (pel-insert-symbol-content-line 'ac-quick-help-height)
       (pel-insert-symbol-content-line 'ac-quick-help-timer)
       (pel-insert-symbol-content-line 'ac-dwim)
       (pel-insert-symbol-content-line 'ac-trigger-key)
       (pel-insert-list                'ac-trigger-commands)
       (pel-insert-symbol-content-line 'ac-dwim-enable)
       (pel-insert-symbol-content-line 'ac-use-comphist)
       (pel-insert-symbol-content-line 'ac-use-fuzzy)
       (pel-insert-symbol-content-line 'ac-stop-flymake-on-completing)
       (pel-insert-symbol-content-line 'ac-use-dictionary-as-stop-words)
       (pel-insert-list  'ac-stop-words)
       (pel-insert-list  'ac-modes)

       (pel-insert-bold "\n\n****Company customization:")
       (pel-insert-symbol-content-line 'company-idle-delay)
       (pel-insert-symbol-content-line 'company-minimum-prefix-length)

       (pel-insert-bold "\n\n****Corfu customization:")
       (pel-insert-symbol-content-line 'corfu-cycle)
       (pel-insert-symbol-content-line 'corfu-auto)
       (pel-insert-symbol-content-line 'corfu-auto-prefix)
       (pel-insert-symbol-content-line 'corfu-auto-delay)
       (pel-insert-symbol-content-line 'corfu-quit-at-boundary)
       (pel-insert-symbol-content-line 'corfu-echo-documentation)
       (pel-insert-symbol-content-line 'corfu-preview-current)
       (pel-insert-symbol-content-line 'corfu-preselect-first)
       (pel-insert-symbol-content-line 'corfu-history-mode))
     (unless append :clear-buffer)
     :use-help-mode)))

;; ---------------------------------------------------------------------------
;;* Overall Mode Selection Control
;;  ==============================

(defvar pel--globally-used-auto-completion-tool nil
  "Auto-completion tool currently used globally.
The possible values are:
- nil : for emacs-builtin
- auto-complete
- company
- corfu.")

(defun pel-autocomplete--disable (&optional globally)
  "Disable currently active auto-completion.
Disable it in the current buffer unless GLOBALLY is non-nil, in that case
disable it in all buffers.
This affects the auto-complete, company and corfu modes.  Nothing else.
Issues an error if there are any state inconsistency."
  ;; Deal with worst case: disable all that are enabled.
  (if globally
      ;; globally
      (progn
        (when (pel--global-auto-complete-mode-p) (pel--global-auto-complete-mode-off))
        (when (pel--global-company-mode-p)       (pel--global-company-mode-off))
        (when (pel--global-corfu-mode-p)         (pel--global-corfu-mode-off)))
    ;; locally
    (when (pel--auto-complete-mode-p) (pel--auto-complete-mode-off))
    (when (pel--company-mode-p)       (pel--company-mode-off))
    (when (pel--corfu-mode-p)         (pel--corfu-mode-off))))

(defvar pel--select-auto-complete-tool-globally nil
  "Allow global selection.")

(defun pel-select-auto-completion (tool &optional select-globally)
  "Activate the auto-completion TOOL.
Update `pel--used-auto-completion-tool'.
Issues an error if there are any state inconsistency."
  ;; First disable previously used auto-completion tool if any.
  (let* ((globally (or select-globally pel--select-auto-complete-tool-globally))
         (scope-str (if globally "Global" "Local"))
         (new-used-auto-completion-tool nil))
    (pel-autocomplete--disable globally)

    ;; Then activate the new auto-completion tool.
    (setq new-used-auto-completion-tool
          (cond
           ((eq tool 'auto-complete)
            (if globally
                (pel--global-auto-complete-mode-on)
              (pel--auto-complete-mode-on))
            'auto-complete)
           ;;
           ((eq tool 'company)
            (if globally
                (pel--global-company-mode-on)
              (pel--company-mode-on))
            'company)
           ;;
           ((eq tool 'corfu)
            (if globally
                (pel--global-corfu-mode-on)
              (pel--corfu-mode-on))
            'corfu)
           ;;
           ((eq tool 'emacs-builtin)
            nil)
           ;;
           (t (error "Invalid auto-completion tool: %S" tool))))
    (if globally
        (setq pel--globally-used-auto-completion-tool
              new-used-auto-completion-tool)
      (setq-local pel--used-auto-completion-tool
                  new-used-auto-completion-tool))
    (if new-used-auto-completion-tool
        (message "%s auto-completion now uses %s." scope-str
                 new-used-auto-completion-tool)
      (message "%sly using Emacs built-in auto-completion." scope-str))))

(defun pel--autocompletion-tools-selection ()
  "Return a list of (char prompt symbol) for available auto-completion tools."
  (let ((selection '((?e "Emacs built-in" emacs-builtin))))
    (when pel-use-auto-complete
      (push '(?a "Auto-complete" auto-complete) selection))
    (when pel-use-company
      (push '(?c "Company" company) selection))
    (when (and pel-use-corfu
               pel-emacs-29-or-later-p)
      (push '(?u "corfU" corfu) selection))
    (setq selection (reverse selection))
    (or selection
        (user-error "None activated!  Turn on at least one of the following:
 pel-use-auto-complete, pel-use-company and pel-use-corfu."))))


;;-pel-autoload
(defun pel-select-auto-complete-tool (&optional globally)
  "Prompt user to select auto-complete tool to use.
With optional GLOBALLY, select the tool for all buffers."
  (interactive "P")
  ;; set value for `pel-select-auto-completion' call
  (let ((pel--select-auto-complete-tool-globally globally))
    (pel-select-from (format "%s auto-completion tool"
                             (if globally
                                 "Global"
                               "Local"))
                     (pel--autocompletion-tools-selection)
                     (if globally
                         (or pel--globally-used-auto-completion-tool 'emacs-builtin)
                       (or pel--used-auto-completion-tool 'emacs-builtin))
                     #'pel-select-auto-completion
                     nil)))


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
         (if (fboundp 'corfu-complete)
             (progn (corfu-complete))
           (user-error "Function not bound: %s" 'corfu-complete))
         ;; (pel-when-fbound 'corfu-complete (corfu-complete))
         )
        (t (user-error "\
First activate a completion system with: <f11> , s"))))

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

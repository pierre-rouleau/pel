;;; pel-completion.el --- Input Completion Control.  -*- lexical-binding: t; -*-

;; Created   Wednesday, May 20 2020.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2021-02-21 16:41:03, updated by Pierre Rouleau>

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
;; Emacs supports several text completion frameworks that provide completion
;; at various prompting commands like find-file, switch-to-buffer, etc...
;; Emacs has its own builtin completion, where you use tab key to expand and
;; show the completions.  Others are available: Ido, Ivy, Ivy with Counsel,
;; Helm.
;;
;; Each one of these help in different situations.  I often use Ido but when
;; looking for a list of updated packages in the package list buffer I use Ivy
;; to quickly look at the list of packages that have updates.
;;
;; This file holds the logic to dynamically switch from using one completion
;; mode to another.  This allows one to install all the completion modes and
;; then activate the one best suited for the current task.
;;
;; PEL has customization variables to identify the available modes in the
;; pel-pkg-for-completion customization group.  Set the
;; `pel-initial-completion-mode' to select the completion mode to use when
;; Emacs starts.
;;
;; Later use the `pel-select-completion-mode' command to select another mode.
;;
;; At any time you can use `pel-show-active-completion-mode' to display which
;; mode is currently used.
;;
;; The following is a list of available commands (*) and functions (-) listed
;; in hierarchical calling order.  All function/commands with a name that
;; start with 'pel-' are 'public'.  The functions with a name staring with
;; 'pel--' are 'private' and should not be called from outside this file.
;; The function prefixed with --> is called from inside `pel-init'.
;;
;; When Ido mode completion is selected, several Ido extension package are
;; supported and activated if requested by the configurationon startup and on
;; the latest requested setting later.  The settings can be changed by the
;; user when one of the available commands are executed.
;;
;;
;; Top Level Complement Mode Management
;; ------------------------------------
;;
;; * `pel-select-completion-mode'
;;   - `pel--completion-mode-selection'
;;   - `pel-activated-completion-mode'
;; --> `pel-set-completion-mode'
;;      - `pel-activated-completion-mode'
;;      - `pel--available-completion-mode-mask'
;;      - `pel--completion-mode-symbol-for-mask'
;;      - `pel--activate-completion-mode'
;;         - `pel--start/stop'   -- of Ido, Ivy and Helm.
;;            |
;;            |
;;            |   Ido Mode Management
;;            |   -------------------
;;            |
;;            +--> `pel--ido-mode-silently'
;;                  * `pel-ido-mode'
;;                    |
;;                    |
;;                    |     Ido flx Mode Management
;;                    |     -----------------------
;;                    |
;;                    +---> * `pel-flx-ido'
;;                    |        - `pel--set-flx-ido'
;;                    |        - `pel--flx-ido-state'
;;                    |
;;                    |     Control Ido Ubiquitous Mode
;;                    |     ---------------------------
;;                    |
;; -------------------|---> - `pel-set-ido-ubiquitous'
;;                    +---> * `pel-ido-ubiquitous'
;;                    |        - `pel--set-ido-ubiquitous'
;;                    |        - `pel--ido-ubiquitous-state'
;;                    |
;;                    |     Ido Prompt Geometry Management
;;                    |     ------------------------------
;;                    |
;;                    +---> * `pel-select-ido-geometry'
;;                             - `pel--ido-geometry-selection'
;;                             - `pel--activated-ido-geometry-symbol'
;;                             - `pel-set-ido-geometry'
;;                                - `pel--set-ido-grid'
;;                                - `pel--set-ido-vertical'
;;                                * `pel-show-active-completion-mode'
;;                                   - `pel-activated-completion-mode'
;;                                   - `pel-activated-completion-mode-name'
;;                                   - `pel-activated-ido-geometry'
;;                                      - `pel--activated-ido-geometry-symbol'
;;                                   - `pel--ido-ubiquitous-state'
;;
;;      * `pel-show-active-completion-mode'
;;

;;; --------------------------------------------------------------------------
;;; Dependencies:

(require 'pel--base)
(require 'pel--macros)
(require 'pel--options)
(require 'pel-prompt)
(require 'pel-seq)
(eval-when-compile
  (require 'cl-macs))                   ; use: cl-case.

;;; --------------------------------------------------------------------------
;;; Code:

;; --
;; Ido Prompt Geometry Management
;; ------------------------------
;;
;; * `pel-select-ido-geometry'                 - in pel-autoload.el
;;    - `pel--ido-geometry-selection'
;;    - `pel--activated-ido-geometry-symbol'
;;    - `pel-set-ido-geometry'
;;       - `pel--set-ido-grid'
;;       - `pel--set-ido-vertical'
;;       * `pel-show-active-completion-mode'   - in pel-autoload.el
;;          - `pel-activated-completion-mode'
;;          - `pel-activated-completion-mode-name'
;;          - `pel-activated-ido-geometry'
;;             - `pel--activated-ido-geometry-symbol'
;;          - `pel--ido-ubiquitous-state'

(defun pel--set-ido-grid (state)
  "Set the ido-grid-mode to specified STATE.
State can be one of:
- 'emacs-default : ido mode is used, but grid is off
- 'grid-collapsed
- 'grid-expanded.
- 'vertical"
  (if (require 'ido nil :no-error)
      (progn
        (pel-turn-mode-on-when-off ido-mode)
        (when (and pel-use-ido-vertical-mode
                   (featurep 'ido-vertical-mode)
                   (boundp  'ido-vertical-mode)
                   (fboundp 'ido-vertical-mode))
          (pel-turn-mode-off-when-on ido-vertical-mode))
        (when (and pel-use-ido-grid-mode
                   (require 'ido-grid-mode nil :no-error)
                   (featurep 'ido-grid-mode)
                   (boundp  'ido-grid-mode)
                   (fboundp 'ido-grid-mode)
                   (boundp  'ido-grid-mode-start-collapsed))
          (cond
           ((eq state 'off)
            (pel-turn-mode-off-when-on ido-grid-mode))
           ;;
           ((eq state 'grid-collapsed)
            (pel-turn-mode-off-when-on ido-grid-mode)
            (setq ido-grid-mode-start-collapsed t)
            (ido-grid-mode 1))
           ;;
           ((eq state 'grid-expanded)
            (pel-turn-mode-off-when-on ido-grid-mode)
            (setq ido-grid-mode-start-collapsed nil)
            (ido-grid-mode 1))
           (t (user-error "Invalid ido grid state request: %S" state)))))
    (error "Cannot load required Ido mode!")))

(defun pel--set-ido-vertical ()
  "Set the ido-vertical mode on."
  (if (require 'ido nil :no-error)
      (progn
        (pel-turn-mode-on-when-off ido-mode)
        (when (and pel-use-ido-grid-mode
                   (featurep 'ido-grid-mode)
                   (boundp  'ido-grid-mode)
                   (fboundp 'ido-grid-mode))
          (pel-turn-mode-off-when-on ido-grid-mode))
        (if (and pel-use-ido-vertical-mode
                 (require 'ido-vertical-mode nil :no-error)
                 (featurep 'ido-vertical-mode)
                 (boundp  'ido-vertical-mode)
                 (fboundp 'ido-vertical-mode))
            (pel-turn-mode-on-when-off ido-vertical-mode)
          (user-error "Cannot activate ido-vertical-mode")))
    (error "Cannot load required Ido mode!")))

(defun pel--activated-ido-geometry-symbol ()
  "Return a symbol describing Ido currently used prompt geometry."
    (if (and pel-use-ido-vertical-mode
           (featurep 'ido-vertical-mode)
           (boundp 'ido-vertical-mode)
           ido-vertical-mode)
        'vertical
      (if (and pel-use-ido-grid-mode
               (featurep 'ido-grid-mode)
               (boundp  'ido-grid-mode)
               ido-grid-mode
               (boundp  'ido-grid-mode-start-collapsed))
          (if ido-grid-mode-start-collapsed
              'grid-collapsed
            'grid-expanded)
        'emacs-default)))

(defconst pel--ido-geometry-names-alist
  '((nil            . "default linear")
    (emacs-default  . "default-linear")
    (grid-collapsed . "grid mode, starts collapsed: expand with tab")
    (grid-expanded  . "grid mode, starts already expanded")
    (vertical       . "vertical mode"))
  "Association list of (symbol . string) for the Ido geometry.")

(defun pel-activated-ido-geometry ()
  "Return a string describing Ido currently used prompt geometry."
  (cdr (assoc (pel--activated-ido-geometry-symbol)
              pel--ido-geometry-names-alist)))

(defvar pel--ido-geometry pel-initial-ido-geometry
  "Currently selected Ido geometry.")

(defun pel-set-ido-geometry (geometry &optional silent now)
  "Set the Ido prompt GEOMETRY. Display description unless SILENT requested.
Identify that its a change when NOW argument is specified.
This assumes that Ido mode is currently activated."
  (cond
   ((eq geometry 'emacs-default)   (pel--set-ido-grid 'off))
   ((eq geometry 'grid-collapsed)  (pel--set-ido-grid 'grid-collapsed))
   ((eq geometry 'grid-expanded)   (pel--set-ido-grid 'grid-expanded))
   ((eq geometry 'vertical)        (pel--set-ido-vertical))
   (t (user-error "Non-supported Ido geometry selected: %S" geometry)))
  (unless silent
    (pel-show-active-completion-mode now)))

(defun pel--ido-geometry-selection ()
  "Return a list of (char prompt symbol) of available Ido geometry choices."
  (let ((selection '((?e "Emacs default - linear" emacs-default))))
    (when pel-use-ido-grid-mode
      (push '(?c "grid - Collapsed" grid-collapsed) selection)
      (push '(?x "grid - eXpanded"  grid-expanded)  selection))
    (when pel-use-ido-vertical-mode
      (push '(?v "vertical" vertical) selection))
    (reverse selection)))

;;-pel-autoload
(defun pel-select-ido-geometry ()
  "Select Ido presentation geometry."
  (interactive)
  (let ((selected-geometry (pel-select-from
                            "Ido prompt geometry"
                            (pel--ido-geometry-selection)
                            (pel--activated-ido-geometry-symbol)
                            nil
                            "default - linear")))
    (when selected-geometry
      (pel-set-ido-geometry selected-geometry nil :now))))

;; --
;; Ido flx Mode Management
;; -----------------------
;;
;; * `pel-flx-ido'            - in pel-autoload.el
;;    - `pel--set-flx-ido'
;;    - `pel--flx-ido-state'

(defvar pel--use-flx-with-ido (eq pel-use-flx 'use-from-start)
  "Whether flx-ido is used with Ido.")

(defun pel--flx-ido-state ()
  "Return a string describing the sate of the `flx-ido-mode'."
  (pel-on-off-string pel--use-flx-with-ido))

(defvar pel--ido-was-using-faces nil
  "Caches `ido-use-faces' while flx-ido is used.") ; TODO find better way

(defvar ido-use-faces)                  ; prevent byte-compiler warning

(defun pel--set-flx-ido (activate)
  "ACTIVATE or deactivate fuzzy flx engine with Ido.
Return t when it is activated, nil otherwise.

Constraint: Ido must be active when this is called to activate flx-ido."
  (if activate
      ;; activating flx-ido
      (if (and (require 'flx-ido nil :no-error)
               (featurep 'flx-ido)
               (boundp   'flx-ido-mode)
               (fboundp  'flx-ido-mode)
               ;; at this point Ido should have been loaded and activated
               (boundp   'ido-enable-flex-matching)
               (boundp   'ido-use-faces))
          (progn
            (when (not flx-ido-mode)
              (flx-ido-mode 1)
              ;; disable ido faces to see flx highlights.
              (setq ido-enable-flex-matching t)
              (setq pel--ido-was-using-faces ido-use-faces)
              (setq ido-use-faces nil)
              (setq pel--use-flx-with-ido flx-ido-mode))
            t )
        (user-error "Failed loading ido-flx!"))
    ;; Deactivate flx-ido
    (when (and (featurep 'flx-ido)
               (boundp   'flx-ido-mode)
               (fboundp  'flx-ido-mode)
               flx-ido-mode)
       (flx-ido-mode -1)
       (setq ido-use-faces pel--ido-was-using-faces)
       (setq pel--use-flx-with-ido flx-ido-mode)
       nil)))

;; auto-loaded via use-package in pel_keys: no need for this unless
;; flx-ido is used.
(defun pel-flx-ido (&optional activate silent)
  "Activate, deactivate or toggle the `flx-ido' completion.

Argument:
-  ACTIVATE:
 - absent, 0 or nil: toggle Flx IDO completion.
 - > 0             : activate Flx IDO completion.
 - < 0             : deactivate Flx IDO completion.

Display new state unless SILENT."
  (interactive "P")
  (let* ((current-state (and (boundp 'flx-ido-mode)
                             flx-ido-mode))
         (action (pel-action-for activate current-state))
         (msg "is already"))
    (when action
      (pel--set-flx-ido (eq action 'activate))
      (setq msg "mode now"))
    (unless silent
      (message "Flx Ido %s: %s" msg (pel--flx-ido-state)))))

;; --
;; Control Ido Ubiquitous Mode
;; ---------------------------
;;
;; * `pel-ido-ubiquitous'           - in pel-autoload.el
;;    - `pel--set-ido-ubiquitous'
;;    - `pel--ido-ubiquitous-state'

(defvar pel--use-ido-ubiquitous  (eq pel-use-ido-ubiquitous 'use-from-start)
  "Whether Ido Ubiquitous is currently used.")

(defconst pel--ido-ubiquitous-whitelist '(describe-symbol
                                          describe-function
                                          describe-variable
                                          describe-input-method
                                          describe-language-environment
                                          describe-package
                                          info-lookup-symbol
                                          where-is
                                          pel-help-pdf-select
                                          imenu)
  "List of function symbols that must use Ido via Ido Ubiquitous.")

(defun pel-set-ido-ubiquitous ()
  "Set Ido Ubiquitous - ensure that some commands use Ido."
  (when (boundp 'ido-cr+-function-whitelist)
    (dolist (fct pel--ido-ubiquitous-whitelist)
      (unless (memq fct ido-cr+-function-whitelist)
        (push fct ido-cr+-function-whitelist)))))

(defun pel--ido-ubiquitous-state ()
  "Return a string describing the state of `ido-ubiquitous-mode'."
  (pel-on-off-string pel--use-ido-ubiquitous))

(defun pel--set-ido-ubiquitous (activate)
  "Activate or de-activate ubiquitous IDO according to argument ACTIVATE.

Constraint:
- Ido must be active when this is called to activate ido-ubiquitous."
  (if activate
      (if (and (require 'ido-completing-read+ nil :no-error)
               (boundp  'ido-ubiquitous-mode)
               (fboundp 'ido-ubiquitous-mode))
          (when (not ido-ubiquitous-mode)
            (ido-ubiquitous-mode 1)
            (setq pel--use-ido-ubiquitous ido-ubiquitous-mode))
        (user-error "Failed loading ido-completing-read+"))
    (when (and (boundp  'ido-ubiquitous-mode)
               (fboundp 'ido-ubiquitous-mode)
               ido-ubiquitous-mode)
      (ido-ubiquitous-mode -1)
      (setq pel--use-ido-ubiquitous ido-ubiquitous-mode))))

;;-pel-autoload
(defun pel-ido-ubiquitous (&optional activate silent)
  "Activate, deactivate or toggle the `ido-ubiquitous-mode'.

Argument:
-  ACTIVATE:
 - absent, 0 or nil: toggle IDO Ubiquitous mode.
 - > 0             : activate IDO Ubiquitous mode.
 - < 0             : deactivate IDO Ubiquitous mode.

Display new state unless SILENT."
  (interactive "P")
  (let* ((current-state (and (boundp 'ido-ubiquitous-mode)
                             ido-ubiquitous-mode))
         (action (pel-action-for activate current-state))
         (msg "is already"))
    (when action
      (pel--set-ido-ubiquitous (eq action 'activate))
      (setq msg "mode now"))
    (unless silent
      (message "Ido Ubiquitous %s: %s" msg (pel--ido-ubiquitous-state)))))

;; --
;; Ido Mode Management
;; -------------------
;;
;; - `pel--ido-mode-silently'
;;    * `pel-ido-mode'

;;-pel-autoload
(defun pel-ido-mode (&optional activate silent)
  "Activate, deactivate or toggle use of the IDO mode.

Argument:
-  ACTIVATE:
 - absent, 0 or nil: toggle IDO mode.
 - > 0             : activate IDO mode.
 - < 0             : deactivate IDO mode.

Print new state unless SILENT.

Also activate/deactivate the IDO extensions:
- ido-flx
- ido-ubiquitous
- Ido modified prompt geography:
  - ido grid mode
  - ido vertical mode."
  (interactive "P")
  (if (and (require 'ido nil :noerror)
           (fboundp 'ido-everywhere)
           (boundp 'ido-enable-flex-matching))
      ;; Identify action: activate or deactivate?
      (let ((action (pel-action-for activate ido-mode)))
        (cond
         ;; Activate
         ((eq action 'activate)
          ;;
          ;; - activate Ido
          (ido-mode 1)
          (ido-everywhere 1)
          (setq ido-enable-flex-matching t)
          ;; don't require confirmation when creating new buffers
          ;; with C-x b
          (pel-setq ido-create-new-buffer 'always)
          ;; - activate flx-ido if needed
          (when pel--use-flx-with-ido
            (pel-flx-ido 1 :silent))
          ;; - activate ido-ubiquitous if needed
          (when pel--use-ido-ubiquitous
            (pel-ido-ubiquitous 1 :silent))
          ;; - set ido geometry
          (pel-set-ido-geometry pel--ido-geometry :silent))
         ;;
         ;; Deactivate
         ((eq action 'deactivate)
          ;; - deactivate extended IDO geometry: use emacs-default
          (pel-set-ido-geometry 'emacs-default :silent)
          ;; - deactivate ido-ubiquitous
          (pel-ido-ubiquitous -1 :silent)
          ;; - deactivate flx-ido
          (pel--set-flx-ido nil)
          ;; - deactivate Ido
          (ido-mode -1)
          (ido-everywhere -1)
          (setq ido-enable-flex-matching nil)))
        (unless silent
          (pel-show-active-completion-mode :now)))
    (user-error "IDO mode is not available! Please install it first")))

(defun pel--ido-mode-silently (&optional activate)
    "Activate, deactivate or toggle use of the IDO mode silently.

Argument:
-  ACTIVATE:
 - absent, 0 or nil: toggle IDO mode.
 - > 0             : activate IDO mode.
 - < 0             : deactivate IDO mode."
    (pel-ido-mode activate :silent))

;; ---------------------------------------------------------------------------
;; Top Level Complement Mode Management
;; ------------------------------------
;;
;; * `pel-select-completion-mode'
;;   - `pel--completion-mode-selection'
;;   - `pel-activated-completion-mode'
;;   > `pel-set-completion-mode'
;;      - `pel-activated-completion-mode'
;;      - `pel--available-completion-mode-mask'
;;      - `pel--completion-mode-symbol-for-mask'
;;      - `pel--activate-completion-mode'
;;         - `pel--start/stop'
;;      * `pel-show-active-completion-mode'


(defun pel--available-completion-mode-mask ()
  "Return bit mask corresponding to the encoding of completion modes available.
The completion modes available is taken from the following user options:
- `pel-use-helm'
- `pel-use-ido'
- `pel-use-ivy'
- `pel-use-counsel'
The bit layout corresponds to the values of pel-USE-{IDO|IVY|COUNSEL|HELM}."
  (let ((mask 0))
    (when pel-use-ido
      (setq mask pel-USE-IDO))
    (when pel-use-ivy
      (setq mask (logior mask pel-USE-IVY)))
    (when pel-use-counsel
      (setq mask (logior mask pel-USE-COUNSEL)))
    (when pel-use-helm
      (setq mask (logior mask pel-USE-HELM)))
    mask))

(defun pel--completion-mode-symbol-for-mask (mask)
  "Return the symbol corresponding to the bit MASK.
It can return nil | 'ido | 'ido/helm | 'ivy | 'ivy/counsel | 'helm"
  (cond ((pel-all-bitset-p mask pel-USE-IDO) 'ido)
        ((pel-all-bitset-p mask pel-USE-IDO pel-USE-HELM) 'ido/helm)
        ((pel-all-bitset-p mask pel-USE-IVY pel-USE-COUNSEL) 'ivy/counsel)
        ((pel-all-bitset-p mask pel-USE-IVY) 'ivy)
        ((pel-all-bitset-p mask pel-USE-HELM) 'helm)
        (t nil)))

;; --

(defun pel-activated-completion-mode ()
  "Return input completion engine currently used.
Return one of:  nil | 'ido | 'ido/helm | 'ivy | 'ivy/counsel | 'helm
The nil value means that Emacs default is used."
  (if (bound-and-true-p counsel-mode)
      'ivy/counsel
    (if (bound-and-true-p ivy-mode)
        'ivy
      (if (bound-and-true-p ido-mode)
          (if (bound-and-true-p helm-mode)
              'ido/helm
            'ido)
        (if (bound-and-true-p helm-mode)
            'helm
          nil)))))

(defun pel--start/stop (start &rest mode-funs)
  "START or stop all modes by calling their MODE-FUNS.
To start set START to t.  To stop: set it nil.
When starting, start the modes in order of functions in the argument list.
When stopping, use the reverse order."
  (let ((mode-arg (if start 1 -1))
        (funs     (if start mode-funs (reverse mode-funs))))
    (mapcar
     (lambda (fct) (funcall fct mode-arg))
     funs)))

(defmacro pel-map-helm (key start-helm helm-cmd other-cmd)
  "Map KEY to HELM-CMD when START-HELM otherwise to OTHER-CMD."
  `(global-set-key ,key (if ,start-helm ,helm-cmd ,other-cmd)))

(defun pel--activate-completion-mode (mode start)
  "START or stop specified completion MODE to a NEWSTATE.
- MODE must be one of: nil | 'ido | 'ido/helm | 'ivy | 'ivy/counsel | 'helm
  If nil, nothing is done.
- START is non-nil to activate, nil to de-activate."
  (let (chg-helm)
    (cond ((eq mode 'ido)
           (pel--start/stop start 'pel--ido-mode-silently))
          ;;
          ((eq mode 'ido/helm)
           (setq chg-helm t)
           (if (fboundp 'helm-mode)
               (pel--start/stop start 'ido-mode 'helm-mode)
             (error "The helm-mode command is not bound!")))
          ;;
          ((eq mode 'ivy)
           (if (fboundp 'ivy-mode)
               (pel--start/stop start 'ivy-mode)
             (error "The ivy-mode command is not bound!")))
          ;;
          ((eq mode 'ivy/counsel)
           (if (pel-all-fboundp 'ivy-mode 'counsel-mode)
               (pel--start/stop start 'ivy-mode 'counsel-mode)
             (error "The ivy-mode or counsel-mode command is not bound!")))
          ;;
          ((eq mode 'helm)
           (setq chg-helm t)
           (if (fboundp 'helm-mode)
               (pel--start/stop start 'helm-mode)
             (error "The helm-mode command is not bound!")))
          ;;
          ;; mode:= nil - do nothing
          ((not mode) t)
          ;;
          ;; otherwise mode is invalid
          (t (user-error "Invalid mode: %s" mode)))
    ;; When entering or leaving Helm mode, configure extra keys
    ;; to the Helm mode.  For Ido/Helm leave the ones Ido configure to the Ido
    ;; binding and use Heml on the other.
    ;; When leaving Helm, re-establish vanilla Emacs bindings, the mode to
    ;; activate other modes will change them if they need to change.
    (when chg-helm
      ;; for helm and ido/helm modes:
      ;;
      ;; Set the helm prefix to "C-c h" instead of the default "C-x c" because
      ;; it is too close to "C-x C-c", which quits Emacs.
      ;; It must be set globally, because `helm-command-prefix-key' cannot be
      ;; changed once `helm-config' is loaded.  It is delayed required in
      ;; pel_keys. Remove that prefix when exiting helm mode.
      (if start
          (progn
            (global-unset-key (kbd "C-x c"))
            (global-set-key (kbd "C-c h") 'helm-command-prefix))
        (global-unset-key (kbd "C-c h")))
      ;; Add/remove extra bindings
      (pel-map-helm (kbd "M-x") start 'helm-M-x 'execute-extended-command)

      ;; for helm mode only (not ido/helm)
      (when (eq mode 'helm)
        (pel-map-helm (kbd "C-x C-f") start 'helm-find-files 'find-file)
        (pel-map-helm (kbd "C-x b")   start 'helm-mini 'switch-to-buffer)))))

;;-pel-autoload
(defun pel-set-completion-mode (requested &optional silent)
  "Activate the requested completion mode (if allowed by configuration).
The REQUESTED is nil or one of: 'emacs-default, 'ido, 'ivy or 'ivy/counsel.
A nil value for REQUESTED corresponds to Emacs default.
If the REQUESTED mode is currently not supported by the pel-use-..
option variable then the request is ignored.
Display a message describing what mode was actually activated.

When Ido mode is used, activate or deactivate the Ido mode
extensions when Ido is activated or deactivated.

Print message describing active mode unless SILENT argument is non-nil."
  (let* ((current-mode (pel-activated-completion-mode))
         (requested-mask (cond ((eq requested 'ido) pel-USE-IDO)
                               ((eq requested 'ido/helm) (logior
                                                          pel-USE-IDO
                                                          pel-USE-HELM))
                               ((eq requested 'ivy) pel-USE-IVY)
                               ((eq requested 'ivy/counsel) (logior
                                                             pel-USE-IVY
                                                             pel-USE-COUNSEL))
                               ((eq requested 'helm) pel-USE-HELM)
                               ((or (eq requested 'emacs-default)
                                    (not requested))
                                0)
                               (t (error "Invalid requested argument \
(%S) passed to pel-set-completion-mode" requested))))
         (allowed-mask (logand requested-mask
                               (pel--available-completion-mode-mask)))
         (new-mode (pel--completion-mode-symbol-for-mask allowed-mask)))
    ;; perform the operation:
    ;; 1: turn off active mode (if any)
    ;;    - then turn off current mode, returning to Emacs default completion
    (pel--activate-completion-mode current-mode nil)
    ;; 2: then activate new one (if any)
    ;;    - activate the new mode
    (pel--activate-completion-mode new-mode t)
    ;; 3: display the new state of completion mode
    (unless silent
      (pel-show-active-completion-mode :now))))

(defun pel--completion-mode-selection ()
  "Return a list of (char prompt symbol) of available completion choices."
  (let ((selection '((?e "Emacs Default" emacs-default))))
    (when pel-use-helm    (push '(?h "Helm" helm)
                                selection))
    (when pel-use-ido     (push '(?d "Ido" ido)
                                selection))
    (when (and pel-use-helm
               pel-use-ido) (push '(?H "Ido/Helm" ido/helm)
                                  selection))
    (when pel-use-ivy     (push '(?v "Ivy" ivy)
                                 selection))
    (when pel-use-counsel (push '(?c "Ivy/Counsel" ivy/counsel)
                                selection))
    (reverse selection)))


;;-pel-autoload
(defun pel-select-completion-mode ()
  "Prompt user for completion mode to activate."
  (interactive)
  (pel-select-from "Completion mode"
                   (pel--completion-mode-selection)
                   (pel-activated-completion-mode)
                   #'pel-set-completion-mode
                   'emacs-default))

;; --

(defconst pel--completion-mode-names-alist
  '(
    (nil           . "Emacs default")
    (emacs-default . "Emacs default")
    (ivy/counsel   . "Ivy/Counsel")
    (ivy           . "Ivy")
    (ido           . "Ido")
    (ido/helm      . "Ido/Helm")
    (helm          . "Helm"))
  "Association list of (symbol . string) for completion mode")

(defun pel-activated-completion-mode-name ()
  "Return string with name of currently used completion MODE."
  (cdr (assoc (pel-activated-completion-mode)
              pel--completion-mode-names-alist)))

;;-pel-autoload
(defun pel-show-active-completion-mode (&optional now)
  "Display the completion mode currently used.
If NOW is non-nil, message starts with \"Now\"
otherwise it starts with \"Currently\"."
  (interactive)
  (let ((current-mode      (pel-activated-completion-mode))
        (current-mode-name (pel-activated-completion-mode-name)))
  (message "%s using:\n- %s completion mode%s."
           (if now "Now" "Currently")
           current-mode-name
           (if (memq current-mode '(ido ido/helm))
               (format "
  - Ido prompt geometry: %s
  - Ido Ubiquitous mode: %s
  - flx-ido        mode: %s"
                       (pel-activated-ido-geometry)
                       (pel--ido-ubiquitous-state)
                       (pel-on-off-string pel--use-flx-with-ido))
             ""))))

;;; --------------------------------------------------------------------------
(provide 'pel-completion)

;;; pel-completion.el ends here

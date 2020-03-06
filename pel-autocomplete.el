;;; pel-autocomplete.el --- PEL autocomplete packages support

;;; Commentary:
;;
;; This file holds code that manages the use of various auto-completion
;; facilities for Emacs.  At this pint it supports the following:
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


;;; Code:

(eval-when-compile
  ;; the following 2 packages are loaded lazily when required:
  ;; their autoload is configured by pel.el.
  ;; Here wwe just need it to verify functions ast compilation.
  (require 'auto-complete)
  (require 'company))


;; --
;; Auto-Complete Initialization
;; ----------------------------
;;
;; TODO: update for finer control once PEL has explicit support for various
;;       programming languages.
;; The pel--setup-auto-complete function currently contains the bare bone
;; initialization code for auto-complete, the code that is often shown on how to
;; set it up.  This is the default setup. Eventually this code should be
;; modified to support better customizations.
;;
(defun pel--setup-auto-complete ()
  "Initialize `auto-complete'."
  ;; Write extra support here
  (ac-config-default))

;; --
;; Company Mode Intialization
;; --------------------------
;; TODO: update for finer control once PEL has explicit support for various
;;       programming languages.
(defun pel--setup-company ()
  "Initialize Company Mode."
  ;; Write extra support here
  ;; variables affecting automatic completion:
  ;; - company-idle-delay
  ;; - company-minimum-prefix-length
  (setq company-tooltip-align-annotations t)
  (setq company-show-numbers t))


;; --
;; Utilities: return state of auto-complete variables that may be unbound

(defun pel--auto-complete-mode-p ()
  "Return t if variable `auto-complete-mode' is loaded and on, nil otherwise."
  (and (boundp 'auto-complete-mode)
	   auto-complete-mode))

(defun pel--global-auto-complete-mode-p ()
  "Return t if variable `global-auto-complete-mode' is loaded and on, nil otherwise."
  (and (boundp 'global-auto-complete-mode)
	   global-auto-complete-mode))

;; --
;; Utilities: turn auto-complete modes on

(defun pel--global-auto-complete-mode-on ()
  "Turn option `global-auto-complete-mode' ON.
On first call, also configure it."
  (if (not (featurep 'auto-complete))
	  (pel--setup-auto-complete))
  (global-auto-complete-mode t))

(defun pel--auto-complete-mode-on ()
  "Turn option `auto-complete-mode' ON.
On first call, also configure it according to its customization."
  (let ((is-first-call (not (featurep 'auto-complete)))
		;; remember the way global autocomplete mode was
		;; before calling its setup (in case it sets global mode on)
		(global-ac   (pel--global-auto-complete-mode-p)))
	(auto-complete-mode 1)
	(if is-first-call
		(progn
		  (pel--setup-auto-complete)
		  ;; When the customization variable is requires global
		  ;; activation, turn global activation on.
		  (if global-ac
			  (global-auto-complete-mode t)
			;; otherwise ensure it's off
			(global-auto-complete-mode -1))))))

;; --
;; Utlities: return state of company-mode variables that may be unbound

(defun pel--company-mode-p ()
  "Return t if variable `company-mode' is loaded and on, nil otherwise."
  (and (boundp 'company-mode)
	   company-mode))

(defun pel--global-company-mode-p ()
  "Return t if variable `global-company-mode' is loaded and on, nil otherwise."
  (and (boundp 'global-company-mode)
	   global-company-mode))

;; --
;; Utilities: turn Company Modes on

(defun pel--global-company-mode-on ()
  "Turn option `global-company-mode' ON.
On first call, also configure it."
  (if (not (featurep 'company))
	  (pel--setup-company))
  (global-company-mode t))

(defun pel--company-mode-on ()
  "Turn option `company-mode' ON.
On first call, also configure it according to its customization."
  (let ((is-first-call (not (featurep 'company)))
		;; remember the way global company mode was
		;; before calling its setup (in case it sets global mode on)
		(global-cm   (pel--global-company-mode-p)))
	(company-mode 1)
	(if is-first-call
		(progn
		  (pel--setup-company)
		  ;; When the customization variable is requires global
		  ;; activation, turn global activation on.
		  (if global-cm
			  (global-company-mode t)
			;; otherwise it's off
			(global-company-mode -1))))))

;; --
;; PEL Auto Complete Commands

;;-pel-autoload
(defun pel-global-auto-complete-mode (&optional arg)
  "Toggle Global Auto Complete mode when ARG is nil.
If ARG is positive: activate it, otherwise de-activate it.
Does not allow activation if Company Mode is active."
  (interactive "P")
  (if (null arg)
	  ;; toggle mode
	  (if (pel--global-auto-complete-mode-p)
		  (progn
			(global-auto-complete-mode 0))
		(if (pel--company-mode-p)
			(user-error "First turn company-mode off!")
		  (pel--global-auto-complete-mode-on)))
	;; activate/deactivate according to arg value.
	(if (< (prefix-numeric-value arg) 0)
		(global-auto-complete-mode 0)
	  (if (pel--company-mode-p)
		  (user-error "First turn company-mode off!")
		(pel--global-auto-complete-mode-on)))))

;;-pel-autoload
(defun pel-auto-complete-mode (&optional arg)
  "Toggle buffer's Auto Complete mode when ARG is nil.
If ARG is positive: activate it, otherwise de-activate it.
Does not allow activation if Company Mode is active."
  (interactive "P")
  (if (null arg)
	  ;; toggle mode
	  (if (pel--auto-complete-mode-p)
		  (progn
			(auto-complete-mode 0))
		(if (pel--company-mode-p)
			(user-error "First turn company-mode off!")
		  (pel--auto-complete-mode-on)))
	;; activate/deactivate according to arg value.
	(if (< (prefix-numeric-value arg) 0)
		(auto-complete-mode 0)
	  (if (pel--company-mode-p)
		  (user-error "First turn company-mode off!")
		(pel--auto-complete-mode-on)))))


;; --
;; PEL Company Mode Commands

;;-pel-autoload
(defun pel-global-company-mode (&optional arg)
  "Toggle Global Company mode when ARG is nil.
If ARG is positive: activate it, otherwise de-activate it.
Does not allow activation if Auto Complete Mode is active."
  (interactive "P")
  (if (null arg)
	  ;; toggle mode
	  (if (pel--global-company-mode-p)
		  (progn
			(global-company-mode 0))
		(if (pel--auto-complete-mode-p)
			(user-error "First turn auto-complete-mode off!")
		  (pel--global-company-mode-on)))
	;; activate/deactivate according to arg value.
	(if (< (prefix-numeric-value arg) 0)
		(global-company-mode 0)
	  (if (pel--auto-complete-mode-p)
		  (user-error "First turn auto-complete-mode off!")
		(pel--global-company-mode-on)))))

;;-pel-autoload
(defun pel-company-mode (&optional arg)
  "Toggle buffer's Company Mode when ARG is nil.
If ARG is positive: activate it, otherwise de-activate it."
  (interactive "P")
  (if (null arg)
	  ;; toggle mode
	  (if (pel--company-mode-p)
		  (company-mode 0)
		(if (pel--auto-complete-mode-p)
			(user-error "First turn auto-complete-mode off!")
		  (company-mode 1)))
	;; activate/deactivate according to arg value.
	(if (< (prefix-numeric-value arg) 0)
		(company-mode 0)
	  (if (pel--auto-complete-mode-p)
		  (user-error "First turn auto-complete-mode off!")
		(company-mode 1)))))

;; --
;; PEL Generic Automatic Completion Commands

;;-pel-autoload
(defun pel-completion-help ()
	"Display information about available auto-completion.
Shows which one is enabled via customization and their current activation state."
	(interactive)
	(require 'pel-base)
	(message "\
Auto-completion package state:
- auto-complete-mode       : %s
- global-auto-complete-mode: %s
- company-mode             : %s
- global-company-mode      : %s"
			 (pel-option-mode-state pel-use-auto-complete 'auto-complete-mode)
			 (pel-symbol-on-off-string 'global-auto-complete-mode)
			 (pel-option-mode-state pel-use-company 'company-mode)
			 (pel-symbol-on-off-string 'global-company-mode)))

;;-pel-autoload
(defun pel-complete ()
  "Start auto-completion for text at point.
Use the currently active auto-completion system."
  (interactive)
  (cond ((pel--auto-complete-mode-p) (auto-complete))
		((pel--company-mode-p)       (company-complete))
		(t (error "No auto completion system active! Please activate one first!"))))

;; -----------------------------------------------------------------------------
(provide 'pel-autocomplete)

;;; pel-autocomplete.el ends here

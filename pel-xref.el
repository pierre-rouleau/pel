;;; pel-xref.el --- xref cross referencing utilities -*-lexical-binding: t-*-

;; Copyright (C) 2020  Pierre Rouleau

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
;;  This file contains PEL's xref and tools integration logic.
;;  The goals of this logic is to provide that ability to select and use
;;  various cross referencing tools, including tools that integrate with the
;;  Emacs xref and tools that do not integrate with xref.  With the provided
;;  logic and commands, the user can activate the cross reference engine that
;;  best fits the task at hand and can change the xref back-end as well as its
;;  front-end.
;;
;; This supports:
;;
;; - dumb-jump
;; - ggtags
;; - xref:
;;   - xref back-end:
;;     - xref controlled access to GNU Global gtags, Universal Ctags, ctags, etags
;;     - gxref
;;     - xref-etags
;;     - rtags
;;   - xref front-end:
;;     - xref
;;     - helm-xref
;;     - ivy-xref
;;
;; The available commands (*) and functions (-) are listed in call hierarchy
;; order:

;; * `pel-xref-show-status'
;;    - `pel-xref-functions-hook-str'
;;    - `pel-xref-helm-xref-state-str'
;;    - `pel-xref-ivy-xref-state-str'
;; * `pel-xref-select-front-end'
;;    - `pel--xref-front-end-selection'
;;    - `pel-xref-set-front-end'
;; * `pel-xref-toggle-gxref'
;;    - `pel-xref-gxref-active-p'
;;    - `pel-xref-gxref-state-str'
;;    - `pel-xref-gxref-activate'
;; * `pel-xref-toggle-dumb-jump-mode'
;;    - `pel-xref-dumb-jump-active-p'
;;    - `pel-xref-dumb-jump-mode-state-str'
;;    - `pel-xref-dumb-jump-activate'
;; * `pel-xref-toggle-rtags'
;;    - `pel-xref-rtags-active-p'
;;    - `pel-xref-rtags-state-str'
;;    - `pel-xref-rtags-activate'
;;
;;    - `pel-xref-function-hook-local-p'


;;; --------------------------------------------------------------------------
;;; Code:

(require 'pel--base)
(require 'pel--options)
(require 'pel-prompt)                   ; use pel-select-from

;; ---------------------------------------------------------------------------
;; Utilities
;; =========

(defsubst pel-xref-function-hook-local-p (function-hook)
  "Return non-nil if the FUNCTION-HOOK list is a local hook, nil if global."
  (memq t function-hook))

;; ---------------------------------------------------------------------------
;; xref back-ends
;; ==============

;; xref back-end: dumb-jump
;; ------------------------

;;-pel-autoload
(defun pel-xref-dumb-jump-activate ()
  "Activate dumb-jump."
  (pel-require 'xref)
  (pel-require 'dumb-jump)
  ;; In some major modes the hooks are done locally
  ;; in others they are done globally: update the proper one.
  (add-hook 'xref-backend-functions
            'dumb-jump-xref-activate
            nil
            (pel-xref-function-hook-local-p xref-backend-functions)))

(defun pel-xref-dumb-jump-active-p ()
  "Return non-nil when dumb-jump is active, nil otherwise."
  (pel-require 'xref)
  (and (featurep 'dumb-jump)
       (memq 'dumb-jump-xref-activate xref-backend-functions)))

(defun pel-xref-dumb-jump-mode-state-str ()
  "Return a string describing the dumb-jump mode status."
  (pel-require 'xref)
  (let ((state-str (if (featurep 'dumb-jump)
                       (pel-on-off-string (pel-xref-dumb-jump-active-p))
                     (if pel-use-dumb-jump
                         "Available but off."
                       "Not available. Activate pel-use-dumb-jump first!"))))
    (format "%s%s"
            state-str
            (pel-activated-in-str pel-modes-activating-dumb-jump))))

;;-pel-autoload
(defun pel-xref-toggle-dumb-jump-mode ()
  "Activate/deactivate dumb-jump mode."
  (interactive)
  (pel-require 'xref)
  (if (pel-xref-dumb-jump-active-p)
      (remove-hook 'xref-backend-functions
                   'dumb-jump-xref-activate
                   (pel-xref-function-hook-local-p xref-backend-functions))
    (pel-xref-dumb-jump-activate))
  (message "dumb-jump now: %s" (pel-xref-dumb-jump-mode-state-str)))


;; xref back-end: gxref
;; --------------------

;;-pel-autoload
(defun pel-xref-gxref-activate ()
  "Activate the gxref xref back-end for the current major mode."
  (pel-require 'xref)
  (pel-require 'gxref)
  (add-hook 'xref-backend-functions
            'gxref-xref-backend
            nil
            (pel-xref-function-hook-local-p xref-backend-functions)))

(defun pel-xref-gxref-active-p ()
  "Return non-nil when gxref back-end is active, nil otherwise."
  (pel-require 'xref)
  (and (featurep 'gxref)
       (memq 'gxref-xref-backend xref-backend-functions)))


(defun pel-xref-gxref-state-str ()
  "Return a string describing the gxref xref back-end use."
  (pel-require 'xref)
  (let ((state-str   (if (featurep 'gxref)
                         (pel-on-off-string (pel-xref-gxref-active-p))
                       (if pel-use-gxref
                           "Available but off."
                         "Not available. Activate pel-use-gxref first!"))))
    (format "%s%s"
            state-str
            (pel-activated-in-str pel-modes-activating-gxref))))

;;-pel-autoload
(defun pel-xref-toggle-gxref ()
  "Toggle activation of the gxref xref-back-end for the current major mode."
  (interactive)
  (pel-require 'xref)
  (if (pel-xref-gxref-active-p)
      (remove-hook 'xref-backend-functions
                   'gxref-xref-backend
                   (pel-xref-function-hook-local-p xref-backend-functions))
    (pel-xref-gxref-activate))
  (message "gxref xref back-end now: %s" (pel-xref-gxref-state-str)))

;; xref back-end: rtags
;; --------------------
;; For C modes only

(defvar c-mode-common-hook)             ; forward declaration to prevent
                                        ; byte-compiler warning

;;-pel-autoload
(defun pel-xref-rtags-activate ()
  "Activate the rtags-xref xref back-end for C modes."
  (pel-require 'rtags-xref)
  (add-hook 'c-mode-common-hook 'rtags-xref-enable))

(defun pel-xref-rtags-active-p ()
  "Return non-nil when rtags-xref is active, nil otherwise."
  (pel-require 'xref)
  (and (featurep 'rtags-xref)
       (memq 'rtags-xref-enable c-mode-common-hook)))

(defun pel-xref-rtags-state-str ()
  "Return a string describing the state of rtags."
  (if (and (featurep 'rtags-xref)
           (featurep 'rtags))
      (pel-on-off-string (pel-xref-rtags-active-p))
    (if pel-use-rtags-xref
        "Available but off."
      "Not available. Activate pel-use-rtags-xref first!")))

;;-pel-autoload
(defun pel-xref-toggle-rtags ()
  "Toggle activation of the rtags xref-back-end for C modes."
  (interactive)
  (if (pel-xref-rtags-active-p)
      (remove-hook 'c-mode-common-hook
                   'rtags-xref-enable)
    (pel-xref-rtags-activate))
  (message "rtags xref back-end now: %s" (pel-xref-rtags-state-str)))

;; ---------------------------------------------------------------------------
;; xref font-ends
;; ==============

(defvar pel--xref-front-end-used-tool nil
  "Identifies the currently used xref front-end tool.
One of: nil | xref | ivy-xref | helm-xref
where: nil := Emacs xref default (not initialized).")

;; forward references to prevent warnings
(defvar xref-show-xrefs-function)
(defvar xref-show-definitions-function) ;only in Emacs >= 27

(defun pel-xref-ivy-xref-state-str ()
  "Return the ivy-xref state representation string."
  (if (featurep 'ivy-xref)
      (pel-on-off-string (eq xref-show-xrefs-function 'ivy-xref-show-xrefs))
    (if pel-use-ivy-xref
        "Available but off."
      "Not available. Activate pel-use-ivy-xref first!")))

(defun pel-xref-helm-xref-state-str ()
  "Return the helm-xref state representation string."
  (if (featurep 'helm-xref)
      (pel-on-off-string
       (eq xref-show-xrefs-function (if (< emacs-major-version 27)
                                        'helm-xref-show-xrefs
                                      'helm-xref-show-xrefs-27)))
    (if pel-use-helm-xref
        "Available but off."
      "Not available. Activate pel-use-helm-xref first!")))

(defun pel--xref-front-end-selection ()
  "Return a list of (char prompt symbol) of available xref back-ends."
  (let ((selection '((?x "xref buffer" xref))))
    (when pel-use-ivy-xref   (push '(?i "ivy-xref"  ivy-xref)  selection))
    (when pel-use-helm-xref  (push '(?h "helm-xref" helm-xref) selection))
    (reverse selection)))

;;-pel-autoload
(defun pel-xref-set-front-end (front-end)
  "Activate the xref FRONT-END specified.
FRONT-END must be one of:
- nil | 'xref : Emacs default xref front end
- 'ivy-xref   : use ivy-xref front end
- 'helm-xref  : use helm-xref front end."
  (cond
   ;; ivy-xref
   ((eq front-end 'ivy-xref)
    (pel-require 'ivy-xref)
    (if (fboundp 'ivy-xref-show-xrefs)
        (progn
          (setq xref-show-xrefs-function 'ivy-xref-show-xrefs)
          (when (>= emacs-major-version 27)
            (setq xref-show-definitions-function 'ivy-xref-show-defs))
          (setq pel--xref-front-end-used-tool  'ivy-xref))
      (user-error "Despite trying, ivy-xref is not loaded!")))
   ;; helm-xref
   ((eq front-end 'helm-xref)
    (pel-require 'helm-xref)
    (if (< emacs-major-version 27)
        (if (fboundp 'helm-xref-show-xrefs)
            (progn
              (setq xref-show-xrefs-function 'helm-xref-show-xrefs)
              (setq pel--xref-front-end-used-tool  'helm-xref))
          (user-error "Despite trying, helm-xref is not loaded!"))
      (progn
        (if (and (fboundp 'helm-xref-show-xrefs-27)
                 (fboundp 'helm-xref-show-defs-27))
            (progn
              (setq xref-show-xrefs-function       'helm-xref-show-xrefs-27)
              (setq xref-show-definitions-function 'helm-xref-show-defs-27)
              (setq pel--xref-front-end-used-tool  'helm-xref))
          (user-error "Despite trying, helm-xref is not loaded!")))))
   ;; Emacs default xref
   (t
    (if (and (require 'xref nil :noerror)
             (fboundp 'xref--show-xref-buffer))
        (progn
          (setq xref-show-xrefs-function 'xref--show-xref-buffer)
          (setq pel--xref-front-end-used-tool 'xref))
      (user-error "Cannot load xref!")))))

;;-pel-autoload
(defun pel-xref-select-front-end ()
  "Select the xref front-end."
  (interactive)
  (let ((original-value pel--xref-front-end-used-tool))
    (pel-select-from "xref front-end"
                     (pel--xref-front-end-selection)
                     pel--xref-front-end-used-tool
                     #'pel-xref-set-front-end)
    (message "xref front-end %s: %s"
             (if (eq pel--xref-front-end-used-tool original-value)
                 "remains"
               "is now")
             (symbol-name pel--xref-front-end-used-tool))))

;; ---------------------------------------------------------------------------
;; CScope Support
;; --------------
(defvar-local pel--helm-cscope-keys-active nil
  "Set to non-nil when helm-cscope keys are active in current buffer.
Nil otherwise.
Only set by pel-activate-helm-cscope-keys & pel-deactivate-helm-cscope-keys.")

(defvar-local pel--helm-cscope-toggling-mode nil
  "Set to t during pel-toggle-helm-scope execution ONLY.
Used as a protection to what seems to be a bug in helm-cscope-mode.")

(defun pel-activate-helm-cscope-keys ()
  "Activates helm-cscope keys for current buffer."
  (pel-require 'helm-cscope)
  (local-set-key (kbd "M-.") 'helm-cscope-find-global-definition)
  (local-set-key (kbd "M-@") 'helm-cscope-find-calling-this-function)
  (local-set-key (kbd "M-s") 'helm-cscope-find-this-symbol)
  (local-set-key (kbd "M-,") 'helm-cscope-pop-mark)
  (setq pel--helm-cscope-keys-active t))

(defun pel-deactivate-helm-cscope-keys ()
  "Activates helm-cscope keys for current buffer."
  (local-unset-key (kbd "M-."))
  (local-unset-key (kbd "M-@"))
  (local-unset-key (kbd "M-s"))
  (local-unset-key (kbd "M-,"))
  (setq pel--helm-cscope-keys-active nil))

;;-pel-autoload
(defun pel-activate-helm-cscope ()
  "Activate helm-cscope-mode.
Don't do anything if pel--toggling-helm-cscope is t.
Done to prevent call to pel-activate-helm-cscope-keys when
trying to turn the mode off.
That is required by a strange behaviour by helm-scope-mode which
calls the hook function even when trying to disable the mode."
  (unless pel--helm-cscope-toggling-mode
    (pel-activate-helm-cscope-keys)))

;;-pel-autoload
(defun pel-toggle-helm-cscope ()
  "Toggle helm-cscope-mode and its key bindings in current buffer.

The helm-cscope-mode is a complement to the cscope-mode: it adds a
set of key bindings that use Helm to show the results.
The keys are:
  - M-. : `helm-cscope-find-global-definition'
  - M-@ : `helm-cscope-find-calling-this-function'
  - M-s : `helm-cscope-find-this-symbol'
  - M-, : `helm-cscope-pop-mark'"
  (interactive)
  (pel-require 'helm-cscope)
  (if (fboundp 'helm-cscope-mode)
      (progn
        (if pel--helm-cscope-keys-active
            (pel-deactivate-helm-cscope-keys)
          (pel-activate-helm-cscope-keys))
        (let ((pel--helm-cscope-toggling-mode t))
          (helm-cscope-mode (if pel--helm-cscope-keys-active
                                1
                              -1))))
    (user-error "helm-cscope-mode unknown!"))
  (message "helm-cscope now: %s, its keys: %s"
           (pel-symbol-on-off-string 'helm-cscope-mode)
           (pel-on-off-string pel--helm-cscope-keys-active)))

;; ---------------------------------------------------------------------------

(defun pel-xref-functions-hook-str (function-hook)
  "Return representation of the FUNCTION-HOOK with local/global mention."
  (format "%S -- %s" function-hook (if (pel-xref-function-hook-local-p function-hook)
                                       "local"
                                     "global")))

;;-pel-autoload
(defun pel-xref-show-status ()
  "Show the mode status of Xref back-ends in current buffer."
  (interactive)
  (message
   "\
- dumb-jump-mode           : %s
- ggtags-mode              : %s
- xref-backend-functions   : %s
 - xref-etags mode         : %s
  - tags-file-name         : %s
  - tags-table-list        : %S
 - gxref                   : %s
 - rtags-xref (for C/C++)  : %s
- xref-show-xrefs-function : %s
  - ivy-xref               : %s
  - helm-xref              : %s
- cscope-minor-mode        : %s
  - helm-cscope-mode       : %s
  - helm-scope key bindings: %s"
   (pel-xref-dumb-jump-mode-state-str)
   (pel-option-mode-state 'ggtags-mode 'pel-use-ggtags)
   (pel-xref-functions-hook-str xref-backend-functions)
   (pel-symbol-on-off-string 'xref-etags-mode)
   tags-file-name
   tags-table-list
   (pel-xref-gxref-state-str)
   (pel-xref-rtags-state-str)
   xref-show-xrefs-function
   (pel-xref-ivy-xref-state-str)
   (pel-xref-helm-xref-state-str)
   (pel-option-mode-state 'cscope-minor-mode
                          'pel-use-xcscope
                          pel-modes-activating-cscope)
   (pel-symbol-on-off-string 'helm-cscope-mode nil nil "not loaded")
   (pel-on-off-string pel--helm-cscope-keys-active)))

;;; --------------------------------------------------------------------------
(provide 'pel-xref)

;;; pel-xref.el ends here

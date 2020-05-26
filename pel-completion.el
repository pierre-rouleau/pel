;;; pel-completion.el --- Completion Mode Control  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;;  Emacs supports several text completion frameworks that provide completion at
;;  various prompting commands like find-file, switch-to-buffer, etc...
;;  Emacs has its own builtin completion, where you use tab key to expand and
;;  show the completions.  Others are available: Ido, Ivy, Ivy with Counsel,
;;  Helm.
;;
;;  Each one of these help in different situations.  I often use Ido but when
;;  looking for a list of updated packages in the package list buffer I use Ivy
;;  to quickly look at the list of packages that have updates.
;;
;;  This file holds the logic to dynamically switch from using one completion
;;  mode to another.  This allows one to install all the completion modes and
;;  then activate the one best suited for the current task.
;;
;;  PEL has customization variables to identify the available modes in the
;;  pel-pkg-for-completion customization group.
;;  Set the `pel-initial-completion-mode' to select the completion mode
;;  to use when Emacs starts.
;;
;;  Later use the `pel-select-completion-mode' command to select another mode.
;;
;;  At any time you can use `pel-show-active-completion-mode' to display which
;;  mode is currently used.
;;
;; The following is a list of available commands (*) and functions (-) listed in
;; hierarchical calling order.  All function/commands with a name that start
;; with 'pel-' are 'public'.  The functions with a name staring with 'pel--' are
;;'private' and should not be called from outside this file.
;;
;; * `pel-select-completion-mode'
;;   - `pel--completion-mode-selection'
;;   - `pel-set-completion-mode'
;;     - `pel--activate-completion-mode'
;;       - `pel--start/stop'
;;     * `pel-show-active-completion-mode'
;;       - `pel-activated-completion-mode-name'
;;         - `pel-activated-completion-mode'
;;     - `pel--completion-mode-symbol-for-mask'
;;     - `pel--available-completion-mode-mask'
;; * `pel-ido-mode'
;;
;;; Code:
(require 'pel--base)
(require 'pel--macros)
(require 'pel--options)
(require 'pel-prompt)
(require 'pel-seq)
(eval-when-compile
  (require 'cl-macs))                   ; use: cl-case.
;; --

;;-pel-autoload
(defun pel-ido-mode (&optional activate)
  "ACTIVATE/deactivate/toggle use of the IDO mode.
If ACTIVATE is absent or nil toggle the IDO mode.
If ACTIVATE is positive, activate the IDO mode.
Otherwise, de-activate the IDO mode."
  (interactive "P")
  (if (and (require 'ido nil :noerror)
           (fboundp 'ido-everywhere)
           (boundp 'ido-enable-flex-matching))
      (let ((action (cond ((not activate) (if ido-mode 'deactivate 'activate))
                          ((> activate 0) (if ido-mode nil 'activate))
                          (t              (if ido-mode 'deactivate nil)))))
        (cond ((eq action 'activate)
               (progn
                 (ido-mode 1)
                 (ido-everywhere 1)
                 (setq ido-enable-flex-matching t)
                 ;; don't require confirmation when creating new buffers with C-x b
                 (pel-setq ido-create-new-buffer 'always)))
              ((eq action 'deactivate)
               (ido-mode -1)
               (ido-everywhere -1)
               (setq ido-enable-flex-matching nil))))
    (user-error "IDO mode is not available! Please install it first")))

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

;;-pel-autoload
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


;;-pel-autoload
(defun pel-activated-completion-mode-name ()
  "Return string with name of currently used completion MODE."
  (cl-case (pel-activated-completion-mode)
    (nil "Emacs default")
    (ido "Ido")
    (ido/helm "Ido/Helm")
    (ivy "Ivy")
    (ivy/counsel "Ivy/Counsel")
    (helm "Helm")
    (t "??")))

;;-pel-autoload
(defun pel-show-active-completion-mode (&optional now)
  "Display the completion mode currently used.
If NOW is non-nil message starts with \"Now\"
otherwise it starts with \"Currently\"."
  (interactive)
  (message "%s using %s completion mode."
           (if now "Now" "Currently")
           (pel-activated-completion-mode-name)))


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
- START is non-nil to activate , nil to de-activate."
  (let (chg-helm)
    (cond ((eq mode 'ido) (pel--start/stop start 'ido-mode))
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
        (pel-map-helm (kbd "C-x b")   start 'helm-mini 'switch-to-buffer)
        ))))



;;-pel-autoload
(defun pel-set-completion-mode (requested)
  "Activate the requested completion mode (if allowed by configuration).
The REQUESTED is nil or one of: 'ido, 'ivy or 'ivy/counsel.
A nil value for REQUESTED corresponds to Emacs default.
If the REQUESTED mode is currently not supported by the pel-use-..
option variable then the request is ignored.
Display a message describing what mode was actually activated."
  (let* ((requested-mask (cond ((eq requested 'ido) pel-USE-IDO)
                               ((eq requested 'ido/helm) (logior
                                                          pel-USE-IDO
                                                          pel-USE-HELM))
                               ((eq requested 'ivy) pel-USE-IVY)
                               ((eq requested 'ivy/counsel) (logior
                                                             pel-USE-IVY
                                                             pel-USE-COUNSEL))
                               ((eq requested 'helm) pel-USE-HELM)
                               ((not requested) 0)))
         (allowed-mask (logand requested-mask
                               (pel--available-completion-mode-mask)))
         (new-mode (pel--completion-mode-symbol-for-mask allowed-mask)))
    ;; perform the operation: turn off active mode (if any)
    (pel--activate-completion-mode (pel-activated-completion-mode) nil)
    ;; then activate new one (if any)
    (pel--activate-completion-mode new-mode t)
    ;; and display the new state of completion mode
    (pel-show-active-completion-mode :now)))

(defun pel--completion-mode-selection ()
  "Return a list of (char prompt symbol) of available completion choices."
  (let ((selection '((?e "Emacs Default" nil))))
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
                   #'pel-set-completion-mode))

;; -----------------------------------------------------------------------------
(provide 'pel-completion)

;;; pel-completion.el ends here

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
;;; Code:
(require 'pel--base)
(require 'pel--options)


;; --

(defun pel--activated-completion-mode ()
  "Return Completion engine currently used.
Return one of:  nil | 'ido | 'ivy | 'ivy/counsel"
  (if (and (boundp 'counsel-mode) counsel-mode)
      'ivy/counsel
    (if (and (boundp 'ivy-mode) ivy-mode)
        'ivy
      (if (and (boundp 'ido-mode) ido-mode)
          'ido
        nil))))

(defun pel--available-completion-mode-mask ()
  "Return bit mask corresponding to the encoding of completion modes available.
The completion modes available is taken from the following user options:
- `pel-use-ido'
- `pel-use-ivy'
- `pel-use-counsel'
The bit layout corresponds to the values of pel-USE-{IDO|IVY|COUNSEL}."
  (let ((mask 0))
    (when pel-use-ido
      (setq mask pel-USE-IDO))
    (when pel-use-ivy
      (setq mask (logior mask pel-USE-IVY)))
    (when pel-use-counsel
      (setq mask (logior mask pel-USE-COUNSEL)))
    mask))

(defun pel--completion-mode-symbol-for-mask (mask)
  "Return the symbol corresponding to the bit MASK.
It can return nil | 'ido | 'ivy | 'ivy/counsel"
  (cond ((pel-all-bitset-p mask pel-USE-IDO) 'ido)
        ((pel-all-bitset-p mask pel-USE-IVY pel-USE-COUNSEL) 'ivy/counsel)
        ((pel-all-bitset-p mask pel-USE-IVY) 'ivy)
        (t nil)))

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

(defun pel--activate-completion-mode (mode newstate)
  "Activate/deactivate specified completion MODE to a NEWSTATE.
- MODE must be one of: nil | 'ido | 'ivy | 'ivy/counsel
  If nil, nothing is done.
- NEWSTATE is either one of: t | nil. t:= activate, nil:= deactivate."
  (let* ((activate newstate)
         (action (if newstate 1 -1)))
    (cond ((eq mode 'ido) (pel-ido-mode action))
          ;;
          ((eq mode 'ivy)
           (if (fboundp 'ivy-mode)
               (ivy-mode action)
             (error "The ivy-mode command not bound.  Please report problem")))
          ;;
          ;; activate ivy then counsel, deactivate counsel then ivy.
          ((eq mode 'ivy/counsel)
           (if (and (fboundp 'ivy-mode)
                    (fboundp 'counsel-mode))
               (if activate
                   (progn
                     (ivy-mode action)
                     (counsel-mode action))
                 (counsel-mode action)
                 (ivy-mode action))
             (error "The ivy-mode or counsel-mode command is not bound. \
Please report this problem")))
          ;;
          ;; mode:= nil - do nothing
          ((not mode) t)
          ;;
          ;; otherwise mode is invalid
          (t (user-error "Invalid mode: %s" mode)))))

(defun pel-show-active-completion-mode ()
  "Display the completion mode currently used."
  (interactive)
  (message "Now using %s completion mode."
           (or (pel--activated-completion-mode) "Emacs' default")))

(defun pel-set-completion-mode (requested)
  "Activate the requested completion mode (if allowed by configuration).
The REQUESTED is nil or one of: 'ido, 'ivy or 'ivy/counsel.
If the REQUESTED mode is currently not supported by the pel-use-..
option variable then the request is ignored.
Display a message describing what mode was actually activated."
  (let* ((requested-mask (cond ((eq requested 'ido) pel-USE-IDO)
                               ((eq requested 'ivy) pel-USE-IVY)
                               ((eq requested 'ivy/counsel) (logior
                                                             pel-USE-IVY
                                                             pel-USE-COUNSEL))
                               ((not requested) 0)))
         (allowed-mask (logand requested-mask
                               (pel--available-completion-mode-mask)))
         (new-mode (pel--completion-mode-symbol-for-mask allowed-mask)))
    ;; perform the operation: turn off active mode (if any)
    (pel--activate-completion-mode (pel--activated-completion-mode) nil)
    ;; then activate new one (if any)
    (pel--activate-completion-mode new-mode t)
    ;; and display the new state of completion mode
    (pel-show-active-completion-mode)))

(defun pel--completion-mode-selection ()
  "Return a list of (char prompt symbol) of available completion choices."
  (let ((selection '((?e "Emacs Default" nil))))
    (when pel-use-ido     (push '(?d "Ido" ido)
                                 selection))
    (when pel-use-ivy     (push '(?v "Ivy" ivy)
                                 selection))
    (when pel-use-counsel (push '(?c "Ivy/Counsel" ivy/counsel)
                                 selection))
    (reverse selection)))

(defun pel--prompt-for (selection)
  "Return a prompt string for the available completion mode SELECTION.
SELECTION is a list of (char prompt completion-symbol)"
  (format "Completion mode: %s."
          (mapconcat (lambda (elt)
                       (format "%c: %s"
                               (car elt) (cadr elt)))
                     selection
                     ", ")))

(defun pel-select-completion-mode ()
  "Prompt user for completion mode to activate."
  (interactive)
  (let* ((selection (pel--completion-mode-selection))
         (prompt    (pel--prompt-for selection))
         (chars     (mapcar #'car selection))
         (choice    (read-char-choice prompt chars))
         (requested-mode (nth 2 (assoc choice selection))))
    (pel-set-completion-mode requested-mode)))

;; -----------------------------------------------------------------------------
(provide 'pel-completion)

;;; pel-completion.el ends here

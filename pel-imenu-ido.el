;;; pel-imenu-ido.el --- Navigation over imenu symbols with Ido prompting.  -*- lexical-binding: nil; -*-

;; Original Authors : shjk, updated by Matt Keller and Vergard Oye
;; Evolution in PEL:  Pierre Rouleau
;; Time-stamp: <2021-02-21 22:33:15, updated by Pierre Rouleau>

;; This file is an evolution of the single pel-goto-symbol function
;; taken from https://www.emacswiki.org/emacs/ImenuMode#h5o-14
;; written by shjk (updated by MattKeller to handle overlays as “positions”;
;; updated by VegardOye (to set the mark before jumping).

;; This file is part of the PEL package.
;; This file is not part of GNU Emacs.

;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;;; --------------------------------------------------------------------------
;;; Commentary:
;;
;;
;; The code in this file implement commands used to moved quickly to item
;; definition statement identified by the imenu system.  The basic imenu
;; command provides a plain Emacs-default completion prompt.
;;
;; The functions here provide better input completion mechanism, as provided
;; by Ido, Ido extension libraries, Ivy and Helm.  The two main entry point
;; commands are:
;;
;;  - `pel-goto-symbol' which provides the ability to jump point to a item
;;    definition into the current buffer.
;;  - `pel-imenu-anywhere' which provides the same ability but for all buffers
;;    of the same major modes: it uses and extends the imenu-anywhere package.
;;
;; The input completion mechanism used by these two commands are selected in
;; with their respective user-option variables:
;;
;;  - `pel-initial-goto-symbol-completion-mode' sets the completion mode used
;;    initially by `pel-goto-symbol'.  It can be changed dynamically during an
;;    editing session with the command `pel-goto-symbol-select-completion'.
;;  - `pel-use-imenu-anywhere' activates the use of imenu-anywhere and set the
;;    initial completion mode used by `pel-imenu-anywhere'. It can be changed
;;    dynamically during an editing session with the command
;;    `pel-imenu-anywhere-select-completion'.
;;
;; The `pel-popup-imenu' command disregards the current input completion
;; selection and imenu configuration: it pops up a menu with the imenu items
;; around the location of the cursor in the current window.

;;
;; The following is a list of available commands (*) and functions (-) listed
;; in hierarchical calling order.  All function/commands with a name that
;; start with 'pel-' are 'public'.  The functions with a name staring with
;; 'pel--' are 'private' and should not be called from outside this file.
;;
;;     * `pel-goto-symbol'
;;        - `pel--goto-symbol'
;;           - `pel---goto-symbol'   (recursive)
;;              * `pel-popup-imenu'
;;
;;     * `pel-imenu-anywhere'
;;
;;     * `pel-goto-symbol-select-completion'
;;        - `pel--goto-symbol-completion-mode-selection'
;;
;;     * `pel-imenu-anywhere-select-completion'
;;       - `pel--imenu-anywhere-completion-mode-selection'
;;



;; Note that the current implementation of the recursive function
;; `pel---goto-symbol' depends on dynamic binding.  It's the only function in
;; the file that requires dynamic binding; all others can use lexical binding.
;; For now the entire file is set to use dynamic scoping because of that
;; function.  This function is an evolution of the code copied from
;; https://www.emacswiki.org/emacs/ImenuMode#h5o-14
;;
;; TODO: convert this code to semantic binding to allow better optimization.
;;
;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;

;; The current (original) implementation uses dynamic binding and recursion.
;; The following forms prevent byte compiler warnings.

(eval-when-compile
  (require 'cl-lib))                    ; use: cl-eval-when
(cl-eval-when 'compile (require 'ido   nil  :no-error))
(cl-eval-when 'compile (require 'imenu nil  :no-error))

(defvar imenu--index-alist)             ; prevent compiler warning
(defvar imenu--rescan-item)             ; prevent compiler warning
(defvar selected-symbol)                ; prevent compiler warning
(defvar symbol-names)                   ; prevent compiler warning
(defvar name-and-pos)                   ; prevent compiler warning

(require 'pel--options)       ; use: pel-goto-symbol-completion-function
;;                            ;      pel-use-ivy
(require 'pel-prompt)         ; use: pel-select-symbol-from
(require 'pel-completion)     ; use: pel--use-ido-ubiquitous
;;; --------------------------------------------------------------------------
;;; Code:
;;

(defconst pel--goto-completion-mode-names-alist
  '(
    (emacs-default . "Emacs default")
    (ivy           . "Ivy")
    (ido           . "Ido")
    (helm          . "Helm"))
  "Association list of (symbol . string) for completion mode")

(defvar pel--goto-symbol-completion-mode
  pel-initial-goto-symbol-completion-mode
  "Completing read function used by the command function `pel-goto-symbol'.
The following functions can be used:
- ido, the default
- ivy, available when `pel-use-ivy' is t.

This variable is set by the function `pel-goto-symbol' and used
by the function `pel---goto-symbol'.")

(defun pel--goto-symbol-completion-mode-selection ()
  "Return a list of (char string symbol) of available completion modes."
  (let ((selection '()))
    ;; only support Ido and Ivy for this now.
    (when pel-use-ido  (push '(?d "Ido" ido) selection))
    (when pel-use-ivy  (push '(?i "Ivy" ivy) selection))
    (push '(?m "pop-up imenu" popup-imenu) selection)
    (reverse selection)))

;;-pel-autoload
(defun pel-goto-symbol-select-completion ()
  "Select completion system for function `pel-goto-symbol'."
  (interactive)
  (let* ((prompt-msg "Completion mode for pel-goto-symbol")
         (selected-mode (pel-select-from
                         prompt-msg
                         (pel--goto-symbol-completion-mode-selection)
                         pel--goto-symbol-completion-mode)))
    (when selected-mode
      (setq pel--goto-symbol-completion-mode selected-mode)
      (message "%s now set to %s" prompt-msg
               (or (cdr (assoc selected-mode
                               pel--goto-completion-mode-names-alist))
                   (cdr (assoc selected-mode
                               '((popup-imenu "iMenu pop-up menu")))))))))

;; --

(defvar pel--goto-symbol-completion-function nil
  "Internal selection.  Set by `pel-goto-symbol'.
Do not modify.")

;;-pel-autoload
(defun pel-popup-imenu (&optional _prompt _choices)
  "Open pop-up imenu.

Can be used by the user, but can also be called internally
by `pel---goto-symbol' as one of the functions identified by the variable
`pel--goto-symbol-completion-function', it must accept the _PROMPT and
_CHOICES arguments for compatibility.  It, however, ignores them."
  (interactive)
  (if (and (require 'imenu nil :no-error)
           (boundp  'imenu-use-popup-menu)
           `(fboundp 'imenu))
      (let ((imenu-use-popup-menu t))
        (call-interactively (function imenu)))
    (user-error "Required imenu library is not loaded")))

(defun pel---goto-symbol (&optional symbol-list)
  "Internal prompt for symbol from SYMBOL-LIST.

Refresh imenu and jump to the location of selected symbol.
Supports completion method specified by caller."
  (unless (featurep 'imenu)
    (require 'imenu nil t))
  (cond
   ((not symbol-list)
    (let ((ido-mode ido-mode)
          name-and-pos symbol-names position)
      (while (progn
               (imenu--cleanup)
               (setq imenu--index-alist nil)
               (pel---goto-symbol (imenu--make-index-alist))
               (setq selected-symbol
                     (funcall pel--goto-symbol-completion-function
                              "Symbol? " symbol-names))
               (string= (car imenu--rescan-item) selected-symbol)))
      (unless (and (boundp 'mark-active) mark-active)
        (push-mark nil t nil))
      (setq position (cdr (assoc selected-symbol name-and-pos)))
      (cond
       ((overlayp position)
        (goto-char (overlay-start position)))
       (t
        (goto-char position)))))
   ;;
   ((listp symbol-list)
    (dolist (symbol symbol-list)
      (let (name position)
        (cond
         ((and (listp symbol) (imenu--subalist-p symbol))
          (pel---goto-symbol symbol))
         ((listp symbol)
          (setq name (car symbol))
          (setq position (cdr symbol)))
         ((stringp symbol)
          (setq name symbol)
          (setq position
                (get-text-property 1 'org-imenu-marker symbol))))
        (unless (or (null position) (null name)
                    (string= (car imenu--rescan-item) name))
          (add-to-list 'symbol-names name)
          (add-to-list 'name-and-pos (cons name position))))))))

(defun pel--goto-symbol ()
  "Utility: call `pel---goto-symbol' unless Ido with Ubiquitous is requested.
In that case use `imenu'."
  ;; If Ivy or Ido with Ido Ubiquitous is selected use the
  ;; imenu command instead of this because imenu provides access to the
  ;; hierarchical item selection, and this function does not do that.
  ;; Use the function if ivy was selected or if Ido is selected without the
  ;; availability of Ido Ubiquitous.
  (if (and (require 'imenu nil :no-error)
           (fboundp 'imenu)
           (or (eq pel--goto-symbol-completion-mode 'ivy)
               (and (eq pel--goto-symbol-completion-mode 'ido)
                    pel--use-ido-ubiquitous)))
      (call-interactively (function imenu))
    ;; use the local implementation when Ido without Ido Ubiquitous is
    ;; requested because Ido is not applied to imenu unless Ido Ubiquitous is
    ;; used and we'd end up with plain Emacs completion.
    (pel---goto-symbol)))

;;-pel-autoload
(defun pel-goto-symbol ()
  "Prompt using Ido for imenu symbol and move point to it.

Refresh imenu and jump to a place in the buffer using one of the following
completion mechanisms:
- Ido
- Ivy

as selected by user-option variable `pel-goto-symbol-completion-function'
and the availability of ivy.

If Ivy is selected by the user option variable `pel-use-ivy' is
nil, then ido is still used."
  (interactive)
    ;; Define the completion mechanism and then invoke the command
    (let* ((pel--goto-symbol-completion-function
            (cond
             ((eq pel--goto-symbol-completion-mode 'ido)
              (function ido-completing-read))
             ((eq pel--goto-symbol-completion-mode 'ivy)
              (if (and (require 'ivy nil :no-error)
                       (fboundp 'ivy-completing-read))
                  (function ivy-completing-read)
                (user-error "Ivy is not available!")))
             ((eq pel--goto-symbol-completion-mode 'popup-imenu)
              (function pel-popup-imenu)))))
      ;; when moving, push xref marker to allow coming back
      (when (and (require 'xref)
                 (fboundp 'xref-push-marker-stack))
        (xref-push-marker-stack))
      (pel--goto-symbol)))

;; --
(defvar pel--imenu-anywhere-method pel-use-imenu-anywhere
  "Identifies whether imenu-anywhere is used and which completion to use.")

(defun pel--imenu-anywhere-completion-mode-selection ()
  "Return a list of (char string symbol) of available completion modes."
  (let ((selection '((?e "Emacs-default" emacs-default))))
    ;; only support Ido and Ivy for this now.
    (when pel-use-ido  (push '(?d "Ido"  ido)  selection))
    (when pel-use-ivy  (push '(?v "Ivy"  ivy)  selection))
    (when pel-use-helm (push '(?h "Helm" helm) selection))
    (reverse selection)))

;;-pel-autoload
(defun pel-imenu-anywhere-select-completion ()
  "Select completion system for function `pel-goto-symbol'."
  (interactive)
  (let* ((prompt-msg "Completion mode for imenu-anywhere")
         (selected-mode (pel-select-from
                        prompt-msg
                        (pel--imenu-anywhere-completion-mode-selection)
                        pel--imenu-anywhere-method)))
    (when selected-mode
      (setq pel--imenu-anywhere-method selected-mode)
      (message "%s now set to %s" prompt-msg
               (cdr (assoc selected-mode
                           pel--goto-completion-mode-names-alist))))))

;; --
;;-pel-autoload
(defun pel-imenu-anywhere ()
  "Go to imenu tag defined in all reachable buffers.
See `imenu-anywhere' for more information.
This function uses the completion method selected by
`pel-use-imenu-anywhere' and any changes requested by executing the
command `pel-imenu-anywhere--select-completion'."
  (interactive)
  (if (and (require 'imenu-anywhere nil :no-error)
           (fboundp 'imenu-anywhere)
           (fboundp 'ido-imenu-anywhere)
           (fboundp 'ivy-imenu-anywhere)
           (fboundp 'helm-imenu-anywhere))
      (cond
       ((eq pel--imenu-anywhere-method 'emacs-default) (imenu-anywhere))
       ((eq pel--imenu-anywhere-method 'ido)           (ido-imenu-anywhere))
       ((eq pel--imenu-anywhere-method 'ivy)           (ivy-imenu-anywhere))
       ((eq pel--imenu-anywhere-method 'helm)          (helm-imenu-anywhere))
       (t (error "Invalid pel--imenu-anywhere-method value: %S"
                 pel--imenu-anywhere-method)))
    (user-error "The package imenu-anywhere is not available. \
Please set pel-use-imenu-anywhere")))

;;; --------------------------------------------------------------------------
(provide 'pel-imenu-ido)

;;; pel-imenu-ido.el ends here

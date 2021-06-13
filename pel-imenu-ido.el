;;; pel-imenu-ido.el --- Navigation over imenu symbols with Ido prompting.  -*-lexical-binding: t-*-

;; Original Authors : shjk, updated by Matt Keller and Vergard Oye
;; Evolution in PEL:  Pierre Rouleau
;; Time-stamp: <2021-06-13 11:19:29, updated by Pierre Rouleau>

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
;; definition statement identified by the imenu system. It provides two action
;; commands:
;;
;;     * `pel-goto-symbol'
;;     * `pel-goto-symbol-any-buffer'
;;
;; The `pel-goto-symbol' allows moving to the symbol defined in the current
;; buffer, while `pel-goto-symbol-any-buffer' provides the ability to move to the
;; symbol defined in any of the currently opened buffers.
;;
;; Both commands use Emacs imenu system to parse and identify the target
;; symbols in each buffer, according to the major mode of the buffer.
;;
;; For both of these commands, several user interfaces are available for user
;; input. These include simple tab-based user input completion, Ido input
;; completion with or without multiple enhancements, Ivy/Counsel drop-down menu
;; completion, Helm-based completion, popup menu with embedded interactive
;; search and flex matching.
;;
;; You can select the user interface to use for each of these 2 commands,
;; independently from each other.
;;
;; The user interface used at the beginning of the Emacs editing session is
;; identified by a PEL user-option customization variable.
;;
;; During an Emacs editing session you can modify the user interface used by
;; each command by invoking the related configuration command.
;;
;; The commands that select the user-interface configurations are:
;;
;;     * `pel-select-goto-symbol-UI'
;;     * `pel-select-goto-symbol-any-buffer-UI'
;;
;; To see what interface are currently active use the following status display
;; command:
;;
;;     * `pel-show-goto-symbol-settings'
;;
;;

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'pel--base)
(require 'pel--options)       ; use: pel-goto-symbol-completion-function
;;                            ;      pel-use-ivy
(require 'pel-prompt)         ; use: pel-select-symbol-from
(require 'pel-completion)     ; use: pel--use-ido-ubiquitous
;;                            ;      pel--ido-ubiquitous-state
;;                            ;      pel-ido-completion-settings-string
;;; --------------------------------------------------------------------------
;;; Code:
;;

;; imenu-based pel-goto-symbol with selectable UI
;; ----------------------------------------------
;;
;; The `pel-goto-symbol' command moves point to a buffer location where a
;; selected symbol is defined.  Several user interface methods are supported
;; and made available according to what PEL user-option is turned on.
;;
;; On startup `pel-initial-goto-symbol-UI' holds the user interface method
;; used initially by `pel-goto-symbol'.  It is stored into the variable
;; `pel--goto-symbol-UI' which later can be changed dynamically during an
;; editing session by the command `pel-select-goto-symbol-UI'.
;;
;; The potentially available user interface methods are identified in the
;; `pel--goto-symbol-UI-alist' defconst.  Each list inside that can identify
;; availability requirements: symbols that must be non-nil for the
;; corresponding UI to be available.
;;
;; The `pel-select-goto-symbol-UI' command is called to change the UI used. It
;; builds the selection of available UI methods by calling the
;; `pel--ui-selection' function with `pel--goto-symbol-UI-alist', using the
;; `pel--ui-entry-available-p' predicate to filter out unavailable UI
;; methods.  Then it stores the new value into `pel--goto-symbol-UI'.
;;
;; The `pel-goto-symbol' command uses this value to determine what function to
;; call.
;;
;; Call tree:
;;
;; * `pel-select-goto-symbol-UI'
;;   - `pel--ui-selection'
;;     - `pel--ui-entry-available-p'
;;   - `pel--goto-symbol-ui-name'
;;
;; * `pel-goto-symbol'
;;


(defconst pel--goto-symbol-UI-alist
  '(
    (emacs-default  ?e "Emacs default, imenu" t)
    (ido            ?d "Ido"            pel-use-ido-ubiquitous)
    (ivy            ?v "Ivy"            pel-use-ivy pel-use-counsel)
    (helm           ?h "helm"           pel-use-helm)
    (popup-imenu    ?p "popup-imenu"    pel-use-popup-imenu)
    (popup-switcher ?s "popup-switcher" pel-use-popup-switcher))
  "Availability of UI `pel-goto-symbol'.
Maps UI symbol name to its specs: (key description requirement)")

(defvar pel--goto-symbol-UI   pel-initial-goto-symbol-UI
  "User interface mode used by the function `pel-goto-symbol'.
The possible values are the same as the choices for
`pel-initial-goto-symbol-UI'.")

(defun pel--ui-entry-available-p (entry)
  "Return non-nil if the UI type specified by ENTRY is available.

ENTRY is a list with a minimum of 4 elements.
The fourth and all successive elements identify requirements
for the entry.  They are either t or a symbol.
The element is available when all of the requirement symbols
are t or have a non-nil symbol value.

The function return nil if any of the requirement is not met,
t if they are all met."
  (let ((allowed t))
    (dolist (requirement (nthcdr 3 entry))
      (setq allowed (and allowed
                         (or (eq requirement t)
                             (symbol-value requirement)))))
    allowed))

(defun pel--ui-selection (ui-alist)
  "Return list of (char string symbol) of available UI modes from UI-ALIST."
  (let ((selection '()))
    (dolist (elt ui-alist)
      (when (pel--ui-entry-available-p elt)
        ;; create a selection entry: (letter description symbol)
        (let ((entry '()))
          (push (car elt) entry)        ; symbol
          (push (nth 2 elt) entry)      ; description
          (push (nth 1 elt) entry)      ; letter
          ;; put it in the selection list
          (push entry selection))))
    ;; sort the selection lists in order of their choice letter
    (sort selection (lambda (e1 e2) (< (car e1) (car e2))))))

(defun pel--goto-symbol-ui-name (&optional selected-mode)
  "Return the name of the goto symbol completion currently used."
  (let ((selected-mode (or selected-mode
                           pel--goto-symbol-UI)))
    (caddr (assoc selected-mode
                  pel--goto-symbol-UI-alist))))

;;-pel-autoload
(defun pel-select-goto-symbol-UI ()
  "Select completion system for function `pel-goto-symbol'."
  (interactive)
  (let* ((prompt-msg "pel-goto-symbol UI")
         (selected-mode (pel-select-from
                         prompt-msg
                         (pel--ui-selection
                          pel--goto-symbol-UI-alist)
                         pel--goto-symbol-UI)))
    (when selected-mode
      (setq pel--goto-symbol-UI selected-mode)
      (message "%s now set to %s"
               prompt-msg
               (pel--goto-symbol-ui-name)))))

;;-pel-autoload
(defun pel-goto-symbol ()
  "Move to imenu detected symbol in current buffer.
Prompt using the user interface currently active.
It is identified by the `pel-initial-goto-symbol-UI' user-option and
can be modified by the `pel-select-goto-symbol-UI' command.

Ido for imenu symbol and move point to it.

Refresh imenu and jump to a place in the buffer using one of the following
completion mechanisms:
- Ido
- Ivy

as selected by user-option variable `pel-goto-symbol-completion-function'
and the availability of ivy.

If Ivy is selected by the user option variable `pel-use-ivy' is
nil, then ido is still used."
  (interactive)
  ;; Assume the operation will result in moving point,
  ;; so push xref marker to allow coming back
  ;; TODO: find a way to detect quit from some of these functions to
  ;;       cleanup extra entries placed on the xref stack
  (when (and (require 'xref)
             (fboundp 'xref-push-marker-stack))
    (xref-push-marker-stack))
  ;; Execute the command corresponding to the UI selected.
  (cond
   ((eq pel--goto-symbol-UI 'emacs-default)
    ;; call imenu interactively so it can prompt user
    (call-interactively (function imenu)))
   ;;
   ((eq pel--goto-symbol-UI 'ido)
    (if (and pel-use-idomenu
             (fboundp 'idomenu))
        (idomenu)
      (user-error "idomenu is not available!")))
   ;;
   ((eq pel--goto-symbol-UI 'ivy)
    (if (and pel-use-counsel
             (fboundp 'counsel-imenu))
        (counsel-imenu)
      (user-error "counsel-imenu is not available!")))
   ;;
   ((eq pel--goto-symbol-UI 'helm)
    (if (and pel-use-helm
             (fboundp 'helm-imenu))
        (helm-imenu)
      (user-error "helm-imenu is not available!")))
   ;;
   ((eq pel--goto-symbol-UI 'popup-imenu)
    (if (and pel-use-popup-imenu
             (fboundp 'popup-imenu))
        ;; TODO: this external package seems to only work once
        ;;       investigate and fix it
        (popup-imenu)
      (user-error "popup-imenu is not available!")))
   ;;
   ((eq pel--goto-symbol-UI 'popup-switcher)
    (if (and pel-use-popup-switcher
             (fboundp 'psw-switch-function))
        ;; TODO: this external package has 2 major bugs:
        ;;        1) it fails to detect the target after the first use
        ;;        2) it leaks highlighting when flex was used.
        ;;        Fix it.
        (psw-switch-function)
      (user-error "psw-switch-function is not available!")))
   ;;
   (t (error "Invalid pel--goto-symbol-completion-mode: %S"
             pel--goto-symbol-UI))))

;; ---------------------------------------------------------------------------
;; Goto Any Buffer
;; ---------------
;;
;; This mechanism uses the imenu-anywhere package.
;;
;; The `pel-use-imenu-anywhere' user-option activates the use of
;; imenu-anywhere to search symbols in all currently opened buffers and jump
;; to the selected one.  When it is active PEL provides the following 2
;; commands:
;;
;; * `pel-goto-symbol-any-buffer'
;; * `pel-select-goto-symbol-any-buffer-UI'
;;
;; PEL supports iMenu Anywhere with several input completion mechanisms:
;;   - Emacs default
;;   - Ido
;;   - Ivy
;;   - Helm
;;
;; The `pel-use-imenu-anywhere' user-option when non-nil, identifies the
;; input completion method used when Emacs starts.  Later the user can modify
;; what is used in the current editing session by executing the function
;; `pel-select-goto-symbol-any-buffer-UI'.
;;
;; The selection is stored inside the variable `pel--imenu-anywhere-method'
;; and then used by the `pel-goto-symbol-any-buffer'.
;;
;; The `pel-goto-symbol-any-buffer' function requires the imenu-anywhere external
;; library lazily and verifies if it is available.
;;
;; Call tree:
;;
;; * `pel-select-goto-symbol-any-buffer-UI'
;;   - `pel--ui-selection'
;;   - `pel--goto-any-buffer-ui-name'
;; * `pel-goto-symbol-any-buffer'
;;

(defconst pel--goto-any-buffer-symbol-UI-alist
  '(
    (emacs-default  ?e "Emacs default, imenu" t)
    (ido            ?d "Ido"            pel-use-ido)
    (ivy            ?v "Ivy"            pel-use-ivy)
    (helm           ?h "helm"           pel-use-helm))
  "Availability of UI for `pel-goto-symbol-any-buffer'.
Maps UI symbol name to its specs: (key description requirement)")

(defvar pel--imenu-anywhere-method pel-use-imenu-anywhere
  "Identifies whether imenu-anywhere is used and which completion to use.")

(defun pel--goto-any-buffer-ui-name (&optional selected-mode)
  "Return completion name used by goto-any-buffer."
  (let ((selected-mode (or selected-mode pel--imenu-anywhere-method)))
    (caddr (assoc selected-mode
                pel--goto-symbol-UI-alist))))

;;-pel-autoload
(defun pel-select-goto-symbol-any-buffer-UI ()
  "Select completion system for function `pel-goto-symbol-any-buffer'."
  (interactive)
  (let* ((prompt-msg "Completion mode for pel-goto-symbol-any-buffer")
         (selected-mode (pel-select-from
                         prompt-msg
                         (pel--ui-selection
                          pel--goto-any-buffer-symbol-UI-alist)
                         pel--imenu-anywhere-method)))
    (when selected-mode
      (setq pel--imenu-anywhere-method selected-mode)
      (message "%s now set to %s" prompt-msg
               (pel--goto-any-buffer-ui-name)))))

;; --
;;-pel-autoload
(defun pel-goto-symbol-any-buffer ()
  "Go to imenu tag defined in all reachable buffers.
See `imenu-anywhere' for more information.
This function uses the completion method selected by
`pel-use-imenu-anywhere' and any changes requested by executing the
command `pel-select-goto-symbol-any-buffer-UI'."
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
Please turn pel-use-imenu-anywhere on")))

;; ---------------------------------------------------------------------------
;; iMenu : dynamic selection of popup menu VS. completion buffer
;; -------------------------------------------------------------

;;-pel-autoload
(defun pel-imenu-toggle-popup (&optional in-current-buffer)
  "Toggle the use of pop-up menu versus completion buffer for imenu.
By default this applies to imenu issued in all buffers,
but with the IN-CURRENT-BUFFER argument set the change applies only
to the current buffer."
  (interactive "P")
  (if (require 'imenu nil :no-error)
      (pel-toggle-and-show-user-option 'imenu-use-popup-menu
                                       (not in-current-buffer)
                                       "use pop-up menu"
                                       "use completion buffer")
    (user-error "Failed loading imenu")))

;; iMenu flatten lists : flimenu-mode access
;; -----------------------------------------

;;-pel-autoload
(defun pel-imenu-toggle-flatten ()
  "Toggle between a hierarchical and a flat imenu."
  (interactive)
  (if (fboundp 'flimenu-mode)
      (pel-toggle-mode-and-show 'flimenu-mode
                                "on:  imenu is flat"
                                "off: imenu is hierarchical")
    (user-error
     "flimenu-mode is not available: pel-use-flimode is %s"
     (pel-on-off-string pel-use-flimenu))))

;; ---------------------------------------------------------------------------
;; goto symbol/any-buffer help
;; ---------------------------
;;
;; The `pel-show-goto-symbol-settings' command prints the settings used by the
;; 2 action commands:
;;
;;     * `pel-goto-symbol'
;;     * `pel-goto-symbol-any-buffer'
;;
;; It prints the settings used by each command, and also prints the setting of
;; the Ido completion, when Ido completion is used by one of the commands.
;;
;; Call tree:
;;
;; * `pel-show-goto-symbol-settings'
;;   - `pel--goto-symbol-ui-name'
;;   - `pel--goto-any-buffer-ui-name'

(defun pel-show-goto-symbol-settings ()
  "Display current settings used by the goto symbol commands."
  (interactive)
  (message "\
pel-goto-symbol            UI  (M-g <f4> h)   is: %s
pel-goto-symbol-any-buffer UI  (M-g <f4> y)   is: %s%s
- Ido requires: Ido Ubiquitous (M-g <f4> M-u) is: %s
- flx-ido  (fuzzy matching)    (M-g <f4> M-f) is: %s
- iMenu lists are %s.%s%s
- iMenu+ support is: %s
- Semantic mode  is: %s"
           (pel--goto-symbol-ui-name)
           (pel--goto-any-buffer-ui-name)
           (pel-string-for
            (when (boundp 'imenu-use-popup-menu)
              (format "\n- iMenu UI is: %s" (if imenu-use-popup-menu
                                                "pop-up menu"
                                              "completion buffer"))))
           (pel--ido-ubiquitous-state)
           (pel-on-off-string pel--use-flx-with-ido)
           (if (and (boundp 'flimenu-mode)
                    flimenu-mode)
               (format
                "flat (but may be split if longer than %s entries)"
                (if (boundp 'imenu-max-items)
                    imenu-max-items
                  "some number of"))
             "hierarchical")
           (pel-string-for
            (when (or (eq 'ido pel--goto-symbol-UI)
                      (eq 'ido pel--imenu-anywhere-method))
              (format
               "\n- Ido uses:\n%s"
               (pel-ido-completion-settings-string "  - "))))
           (pel-string-for
            (when (and (or (eq 'popup-switcher pel--goto-symbol-UI)
                           (eq 'popup-switcher pel--imenu-anywhere-method))
                       (boundp 'psw-use-flx)
                       (boundp 'psw-popup-menu-max-length))
              (format "\n- popup menu:\n  - can display %d lines.%s"
                      psw-popup-menu-max-length
                      (pel-string-when
                       psw-use-flx
                       "\n  - supports 'flx' fuzzy engine."))))
           (pel-symbol-on-off-string 'pel-use-imenu+ "on, which impacts all Ido-based prompts" nil "off")
           (pel-symbol-on-off-string 'semantic-mode nil nil "not loaded")))

;;; --------------------------------------------------------------------------
(provide 'pel-imenu-ido)

;;; pel-imenu-ido.el ends here

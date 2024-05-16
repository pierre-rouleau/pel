;;; pel__hydra.el --- PEL Hydra control.  -*- lexical-binding: t; -*-

;; Created   : Friday, March 19 2021.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2024-05-16 18:44:56 EDT, updated by Pierre Rouleau>

;; This file is part of the PEL package.
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
;; This file defines a set of Hydras for PEL.

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'pel--base)
(require 'pel--options)
(require 'hydra)
;;; --------------------------------------------------------------------------
;;; Code:
;;

;; Hydra Definitions
;; =================
;;
;; All PEL Hydras are invoked via the key <f7>.  The key typed right after f7
;; determines what Hydra will be used. Therefore try to limit using the same
;; keys inside the various PEL Hydras otherwise there won't be many keys to
;; identify the exact Hydra to use.  For the moment most top left keys are
;; used by the Window Hydra (pel-∑wnd), the other PEL Hydras use a secondary
;; prefix key.
;;
;; Hydra auto-loading is controlled by the <f7> key. At first that key is
;; mapped to execute `pel--load-hydra'. That function breaks this binding,
;; load the hydra library, triggering the configuration of all PEL Hydras via
;; the ```use-package hydra`` call.  Then it simulates a second <f7> key event
;; to get the effect the user expects and then removes itself from Emacs.
;;
;;
;; Byte-compilation
;; ----------------
;;
;; The byte compilation of this file is NOT controlled by the Makefile.
;; Instead it is dynamically byte-compiled by the logic inside the *end*
;; of pel_keys.el.
;; Therefore *all* symbols interned by pel_keys.el are available for use
;; in this file here.  Do not try to byte compile pel__hydra.el to detect
;; undefined symbol.


;; TODO:
;; - Might want to place the different hydras inside their own files and allow
;;   users to map them to some other bindings by using map references instead
;;   of having them hard coded like they are now.

;; PEL variables
(defvar pel--cache-for-hydra-is-helpful nil)
(defvar pel--cache-for-hydra-is-helpful-filled nil)

;; Declarations of Hydra variables to prevent byte-compiler warnings
(defvar hydra-is-helpful)

(defun pel--cache-hydra-is-helpful ()
  "Store hydra-is-helpful user option."
  (unless pel--cache-for-hydra-is-helpful-filled
    (setq pel--cache-for-hydra-is-helpful hydra-is-helpful)
    (setq pel--cache-for-hydra-is-helpful-filled t)))
(declare-function pel--cache-hydra-is-helpful "pel_keys")

(defun pel--restore-hydra-is-helpful ()
  "Restore the value of hydra-is-helpful user option."
  (when pel--cache-for-hydra-is-helpful-filled
    (setq hydra-is-helpful pel--cache-for-hydra-is-helpful)
    (setq pel--cache-for-hydra-is-helpful-filled nil)))
(declare-function pel--restore-hydra-is-helpful "pel_keys")

(defun pel-toggle-hydra-hint ()
  "Toggle display of the current hydra hint."
  (interactive)
  (message (if (pel-toggle 'hydra-is-helpful)
               "Showing Hydra Hint"
             "Hiding Hint")))

;; (defun pel-customize-hydra-is-helpful ()
;;   (interactive)
;;   (let ((help-key-fct-name (symbol-name (key-binding "?"))))
;;     (message "help-key-fct-name: %s" help-key-fct-name)
;;     (when (pel-string-starts-with-p help-key-fct-name "pel-∑")
;;       (let* ((the-hydra-name (substring help-key-fct-name 9))
;;              (the-hydra-exit-fct-name (format "pel-∑%s/nil" the-hydra-name)))
;;         (message "Would need to execute %s" the-hydra-exit-fct-name))))
;;   ;; (customize-option 'hydra-is-helpful)
;;   )

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; PEL Hydra: Greek Letters : activated with <f7> <f6> <f6>

(defmacro pel--define-greek-command (greek)
  "Define a command with a GREEK name that insert that GREEK letter."
  (let ((function-name (intern greek))
        (docstring (format "Insert %s." greek)))
    `(defun ,function-name nil
       ,docstring
       (interactive)
       (insert ,greek))))

(when pel-activate-hydra-for-greek

  ;; Define Greek letter commands used by the pel-∑gr Hydra defined inside
  ;; pel__hydra.el.  Code should really be located in there, but placing it
  ;; here prevents byte compiler warnings.
  ;; Note: pel--options.el ensures that `pel-use-hydra' is t when
  ;;       `pel-activate-hydra-for-greek' is t.

  (pel--define-greek-command "α") (declare-function α "pel_keys")
  (pel--define-greek-command "β") (declare-function β "pel_keys")
  (pel--define-greek-command "χ") (declare-function χ "pel_keys")
  (pel--define-greek-command "δ") (declare-function δ "pel_keys")
  (pel--define-greek-command "ε") (declare-function ε "pel_keys")
  (pel--define-greek-command "ϕ") (declare-function ϕ "pel_keys")
  (pel--define-greek-command "γ") (declare-function γ "pel_keys")
  (pel--define-greek-command "η") (declare-function η "pel_keys")
  (pel--define-greek-command "ι") (declare-function ι "pel_keys")
  (pel--define-greek-command "φ") (declare-function φ "pel_keys")
  (pel--define-greek-command "κ") (declare-function κ "pel_keys")
  (pel--define-greek-command "λ") (declare-function λ "pel_keys")
  (pel--define-greek-command "μ") (declare-function μ "pel_keys")
  (pel--define-greek-command "ν") (declare-function ν "pel_keys")
  (pel--define-greek-command "ο") (declare-function ο "pel_keys")
  (pel--define-greek-command "π") (declare-function π "pel_keys")
  (pel--define-greek-command "θ") (declare-function θ "pel_keys")
  (pel--define-greek-command "ρ") (declare-function ρ "pel_keys")
  (pel--define-greek-command "σ") (declare-function σ "pel_keys")
  (pel--define-greek-command "τ") (declare-function τ "pel_keys")
  (pel--define-greek-command "υ") (declare-function υ "pel_keys")
  (pel--define-greek-command "ω") (declare-function ω "pel_keys")
  (pel--define-greek-command "ξ") (declare-function ξ "pel_keys")
  (pel--define-greek-command "ψ") (declare-function ψ "pel_keys")
  (pel--define-greek-command "ζ") (declare-function ζ "pel_keys")
  (pel--define-greek-command "Α") (declare-function Α "pel_keys")
  (pel--define-greek-command "Β") (declare-function Β "pel_keys")
  (pel--define-greek-command "Χ") (declare-function Χ "pel_keys")
  (pel--define-greek-command "Δ") (declare-function Δ "pel_keys")
  (pel--define-greek-command "Ε") (declare-function Ε "pel_keys")
  (pel--define-greek-command "Φ") (declare-function Φ "pel_keys")
  (pel--define-greek-command "Γ") (declare-function Γ "pel_keys")
  (pel--define-greek-command "Η") (declare-function Η "pel_keys")
  (pel--define-greek-command "Ι") (declare-function Ι "pel_keys")
  (pel--define-greek-command "Φ") (declare-function Φ "pel_keys")
  (pel--define-greek-command "Κ") (declare-function Κ "pel_keys")
  (pel--define-greek-command "Λ") (declare-function Λ "pel_keys")
  (pel--define-greek-command "Μ") (declare-function Μ "pel_keys")
  (pel--define-greek-command "Ν") (declare-function Ν "pel_keys")
  (pel--define-greek-command "Ο") (declare-function Ο "pel_keys")
  (pel--define-greek-command "Π") (declare-function Π "pel_keys")
  (pel--define-greek-command "Θ") (declare-function Θ "pel_keys")
  (pel--define-greek-command "Ρ") (declare-function Ρ "pel_keys")
  (pel--define-greek-command "Σ") (declare-function Σ "pel_keys")
  (pel--define-greek-command "Τ") (declare-function Τ "pel_keys")
  (pel--define-greek-command "Υ") (declare-function Υ "pel_keys")
  (pel--define-greek-command "Ω") (declare-function Ω "pel_keys")
  (pel--define-greek-command "Ξ") (declare-function Ξ "pel_keys")
  (pel--define-greek-command "Ψ") (declare-function Ψ "pel_keys")
  (pel--define-greek-command "Ζ") (declare-function Ζ "pel_keys")

  ;; Note: Hydra docstrings are generated to include the base hydra name and
  ;; the key map.  I would like to use pel-∑greek as the hydra name but that
  ;; would generate a docstring first line that is longer than 80 characters.
  (defhydra pel-∑gr (global-map "<f7> <f6> <f6>"
                                :base-map (make-sparse-keymap)
                                :foreign-keys run)
    ""
    ("M-a"  α "α")
    ("M-b"  β "β")
    ("M-c"  χ "χ")
    ("M-d"  δ "δ")
    ("M-e"  ε "ε")
    ("M-f"  ϕ "ϕ")
    ("M-g"  γ "γ")
    ("M-h"  η "η")
    ("M-i"  ι "ι")
    ("M-j"  φ "φ")
    ("M-k"  κ "κ")
    ("M-l"  λ "λ")
    ("M-m"  μ "μ")
    ("M-n"  ν "ν")
    ("M-o"  ο "ο")
    ("M-p"  π "π")
    ("M-q"  θ "θ")
    ("M-r"  ρ "ρ")
    ("M-s"  σ "σ")
    ("M-t"  τ "τ")
    ("M-u"  υ "υ")
    ("M-w"  ω "ω")
    ("M-x"  ξ "ξ")
    ("M-y"  ψ "ψ")
    ("M-z"  ζ "ζ")
    ("M-A"  Α "Α")
    ("M-B"  Β "Β")
    ("M-C"  Χ "Χ")
    ("M-D"  Δ "Δ")
    ("M-E"  Ε "Ε")
    ("M-F"  Φ "Φ")
    ("M-G"  Γ "Γ")
    ("M-H"  Η "Η")
    ("M-I"  Ι "Ι")
    ("M-J"  Φ "Φ")
    ("M-K"  Κ "Κ")
    ("M-L"  Λ "Λ")
    ("M-M"  Μ "Μ")
    ("M-N"  Ν "Ν")
    ("M-O"  Ο "Ο")
    ("M-P"  Π "Π")
    ("M-Q"  Θ "Θ")
    ("M-R"  Ρ "Ρ")
    ("M-S"  Σ "Σ")
    ("M-T"  Τ "Τ")
    ("M-U"  Υ "Υ")
    ("M-W"  Ω "Ω")
    ("M-X"  Ξ "Ξ")
    ("M-Y"  Ψ "Ψ")
    ("M-Z"  Ζ "Ζ")
    ("<f7>" nil "cancel" )))

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; PEL HYDRA: Narrate : activated with <f7> <f8>
(when (and pel-use-applescript pel-system-is-macos-p)
  (pel-declare-file pel-applescript defines:
                    pel-say-word
                    pel-say-sentence
                    pel-say-paragraph
                    pel-say-region
                    pel-say)
  (pel-declare-file pel-navigate defines: pel-forward-word-start)

  (defun pel-say-last-word ()
    "Narrate last word."
    (interactive)
    (backward-word)
    (pel-say-word))
  (declare-function pel-say-last-word "pel__hydra")

  (defun pel-nxt-sentence ()
    "Move to next sentence."
    (interactive)
    (forward-sentence)
    (pel-forward-word-start))
  (declare-function pel-nxt-sentence "pel__hydra")

  (defhydra pel-∑say (global-map "<f7> <f8>" :foreign-keys run)
    ""
    ("w"     pel-say-word             "word"              :column "Read")
    ("s"     pel-say-sentence         "sentence"          :column "Read")
    ("p"     pel-say-paragraph        "paragraph"         :column "Read")
    ("R"     pel-say-region           "region"            :column "Read")
    ("r"     pel-say-last-word        "last word"         :column "Repeat")
    ("t"     pel-say                  "at prompt"         :column "Type")
    ("b"     backward-word            "previous word"     :column "Move to")
    ("n"     pel-forward-word-start   "next word"         :column "Move to")
    ("B"     backward-sentence        "previous sentence" :column "Move to")
    ("N"     pel-nxt-sentence         "next sentence"     :column "Move to")
    ("<f7>" nil                       "cancel"            :column "End")))

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; PEL HYDRA: Buffer Navigation/Management

(when (not pel-use-iflipb)
  (defun iflipb-next-buffer ()
    "Warning stub."
    (user-error "Unavailable - set pel-use-iflipb to activate!"))
  (declare-function iflipb-next-buffer "pel__hydra")

  (defun iflipb-previous-buffer ()
    "Warning stub."
    (user-error "Unavailable - set pel-use-iflipb to activate!"))
  (declare-function iflipb-previous-buffer "pel__hydra"))

(defhydra pel-∑buffer (global-map "<f7> <f9>"
                                  :foreign-keys run)
  ""
  ("M-n"     next-buffer             "next"      :column "Buffer")
  ("M-p"     previous-buffer         "prev"      :column "Buffer")
  ("M-l"     pel-switch-to-last-used-buffer "last" :column "Buffer")
  ("M-v"     view-mode               "view"      :column "Buffer")


  ("M-."     pel-bs-next             "next"      :column "Buffer Selection")
  ("M-,"     pel-bs-previous         "prev"      :column "Buffer Selection")

  ("<f9>"    iflipb-next-buffer      "next"      :column "Flip")
  ("S-<f9>"  iflipb-previous-buffer  "prev"      :column "Flip")
  ("M-k"     iflipb-kill-buffer      "kill"      :column "Flip")

  ("]"       pel-smb-next            "next"      :column "Same Mode")
  ("["       pel-smb-previous        "previous"  :column "Same Mode")

  ("?"       pel-toggle-hydra-hint   "hint"      :column "Other")
  ("<f7>"    nil                     "cancel"    :column "Other"))

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; PEL HYDRA: Window Management
;; The hydra includes functions that may not be available
;; provide dummy stubs for them if necessary.
(when (not pel-use-winner)
  (defun winner-redo ()
    "Warning stub."
    (user-error "Unavailable - set pel-use-winner to t to activate!"))
  (declare-function winner-redo "pel__hydra")

  (defun winner-undo ()
    "Warning stub."
    (user-error "Unavailable - set pel-use-winner to t to activate!"))
  (declare-function winner-undo "pel__hydra"))

(when (not pel-use-ace-window)
  (defun ace-window ()
    "Warning stub."
    (user-error "Unavailable - set pel-ace-window to t to activate!"))
  (declare-function ace-window "pel__hydra")

  (defun ace-swap-window ()
    "Warning stub."
    (user-error "Unavailable - set pel-ace-window to t to activate!"))
  (declare-function ace-swap-window "pel__hydra"))

(defun pel--goldratio ()
  "Toggle `golden-ratio-mode' when available"
  (interactive)
  (if (and (fboundp 'golden-ratio-mode))
      (golden-ratio-mode 'toggle)
    (user-error "Unavailable - activate it via pel-use-golden-ratio")))

(defun pel-∑-customize-hint ()
  "Customize hydra from the window hydra.

CAUTION: the hydra is still active!"
  (interactive)
  (customize-option 'hydra-is-helpful)
  (message "Turn hydra off to use standard keys!"))


(defhydra pel-∑wnd (global-map "<f7>"
                               :pre  (pel--cache-hydra-is-helpful)
                               :post (pel--restore-hydra-is-helpful)
                               :foreign-keys run)
  ""

  ("/ 8"         pel-split-root-window-top    "root⬆️"        :column "SplitF")
  ("/ 2"         pel-split-root-window-bottom "root⬇️"        :column "SplitF")
  ("/ 4"         pel-split-root-window-left   "root⬅️"        :column "SplitF")
  ("/ 6"         pel-split-root-window-right  "root➡️"        :column "SplitF")
  ("\\ 8"        pel-buf-in-side-win-top      "side⬆️"        :column "SplitF")
  ("\\ 2"        pel-buf-in-side-win-bottom   "side⬇️"        :column "SplitF")
  ("\\ 4"        pel-buf-in-side-win-left     "side⬅️"        :column "SplitF")
  ("\\ 6"        pel-buf-in-side-win-right    "side➡️"        :column "SplitF")
  ("2"           split-window-below           "-"            :column "SplitW")
  ("3"           split-window-right           "|"            :column "SplitW")
  ("C-<up>"      pel-create-window-up         "⬆️"            :column "SplitW")
  ("C-<down>"    pel-create-window-down       "⬇️"            :column "SplitW")
  ("C-<left>"    pel-create-window-left       "⬅️"            :column "SplitW")
  ("C-<right>"   pel-create-window-right      "➡️"            :column "SplitW")
  ("s"           pel-toggle-window-size-fixed "fix size"     :column "Layout")
  ("n"           winner-redo                  "next layout"  :column "Layout")
  ("p"           winner-undo                  "last layout"  :column "Layout")
  ("x"           ace-swap-window              "swap with.#"  :column "Layout")
  ("M-v"         pel-2-vertical-windows       "flip vert."   :column "Layout")
  ("M-h"         pel-2-horizontal-windows     "flip horiz."  :column "Layout")
  ("g"           pel--goldratio               "gold ratio"   :column "Layout")
  ("<up>"        windmove-up                  "⬆️"            :column "Move")
  ("<down>"      windmove-down                "⬇️"            :column "Move")
  ("<left>"      windmove-left                "⬅️"            :column "Move")
  ("<right>"     windmove-right               "➡️"            :column "Move")
  ("o"           other-window                 "other"        :column "Move")
  ("#"           ace-window                   "to #"         :column "Move")
  ("="           balance-windows              "balance"      :column "Resize")
  ("V"           enlarge-window               "taller"       :column "Resize")
  ("v"           shrink-window                "shorter"      :column "Resize")
  ("H"           enlarge-window-horizontally  "wider"        :column "Resize")
  ("h"           shrink-window-horizontally   "narrower"     :column "Resize")
  ("."           fit-window-to-buffer         "fit2buf"      :column "Resize")
  ("-"           shrink-window-if-larger-than-buffer "shrink" :column "Resize")
  ("0"           delete-window                "this"         :column "Close")
  ("O"           pel-close-other-window       "other"        :column "Close")
  ("1"           delete-other-windows         "others"       :column "Close")
  ("C-S-<up>"    pel-close-window-up          "above"        :column "Close")
  ("C-S-<down>"  pel-close-window-down        "below"        :column "Close")
  ("C-S-<left>"  pel-close-window-left        "left"         :column "Close")
  ("C-S-<right>" pel-close-window-right       "right"        :column "Close")
  ("K"           kill-buffer-and-window       "kill buf/win" :column "Buffer")
  ("k"           pel-kill-current-buffer      "kill buffer"  :column "Buffer")
  ("b"           next-buffer                  "next buffer"  :column "Buffer")
  ("B"           previous-buffer              "prev buffer"  :column "Buffer")
  ("5"           recenter-top-bottom          "recenter"     :column "Buffer")
  ("<M-up>"      pel-scroll-down              "scroll down"  :column "Other")
  ("<M-down>"    pel-scroll-up                "scroll up"    :column "Other")
  ("f"           follow-mode                  "follow-mode"  :column "Other")
  ("I"           pel-show-window-info         "info"         :column "Other")
  ("M-?"         pel-∑-customize-hint         "hint cfg"     :column "Other")
  ("?"           pel-toggle-hydra-hint        "hint"         :column "Other")
  ("q"           quit-window                  "quit"         :column "Other")
  ("<f7>"        nil                          "cancel"       :column "Other"))


;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; PEL HYDRA: Window Info, Dedicate, Purpose

(defun pel--wininfo-up ()
  "Move to window up, show it's info."
  (interactive)
  (windmove-up)
  (pel-show-window-info))

(defun pel--wininfo-down ()
  "Move to window down, show it's info."
  (interactive)
  (windmove-down)
  (pel-show-window-info))

(defun pel--wininfo-left ()
  "Move to window left, show it's info."
  (interactive)
  (windmove-left)
  (pel-show-window-info))

(defun pel--wininfo-right ()
  "Move to window right, show it's info."
  (interactive)
  (windmove-right)
  (pel-show-window-info))

(defun pel--ptwpd ()
  "Toggle window purpose and print window info."
  (interactive)
  (if pel-use-window-purpose
      (if (and (fboundp 'purpose-toggle-window-purpose-dedicated)
               (boundp 'purpose-mode)
               (fboundp 'purpose-mode))
          (progn
            (unless purpose-mode
              (purpose-mode 1))
            (purpose-toggle-window-purpose-dedicated)
            (pel-show-window-info))
        (user-error "window-purpose not available."))
    (user-error "Please activate pel-use-window-purpose first!")))

(defun pel--ptbpd ()
  "Toggle window purpose and print window info."
  (interactive)
  (if pel-use-window-purpose
      (if (and (fboundp 'purpose-toggle-window-buffer-dedicated)
               (boundp 'purpose-mode)
               (fboundp 'purpose-mode))
          (progn
            (unless purpose-mode
              (purpose-mode 1))
            (purpose-toggle-window-buffer-dedicated)
            (pel-show-window-info))
        (user-error "window-purpose not available."))
    (user-error "Please activate pel-use-window-purpose first!")))

(defun pel--purpose-mode (&optional arg)
  "Provide access to purpose mode if present."
  (interactive "P")
  (if (fboundp 'purpose-mode)
      (purpose-mode arg)
    (user-error "Please activate pel-use-window-purpose first!")))

(defun pel--twd ()
  "Toggle window dedicated and print window info"
  (interactive)
  (pel-toggle-window-dedicated)
  (pel-show-window-info))

(defhydra pel-∑winInfo (global-map "<f7> M-i"
                                   :pre (pel--cache-hydra-is-helpful)
                                   :post (pel--restore-hydra-is-helpful)
                                   :foreign-keys nil)
  ""
  ("M-d"         pel--twd                  "window"               :column "Dedicate")
  ("M-b"         pel--ptbpd                "buffer purpose"       :column "Dedicate")
  ("M-w"         pel--ptwpd                "window purpose"       :column "Dedicate")
  ("M-i"         pel-show-window-info      "current"              :column "Window info")
  ("<up>"        pel--wininfo-up           "⬆️"                    :column "Window info")
  ("<down>"      pel--wininfo-down         "⬇️"                    :column "Window info")
  ("<left>"      pel--wininfo-left         "⬅️"                    :column "Window info")
  ("<right>"     pel--wininfo-right        "➡️"                    :column "Window info")
  ("M-p"         pel--purpose-mode         "toggle purpose mode"  :column "Other")
  ("?"           pel-toggle-hydra-hint     "hint"                 :column "Other")
  ("<f7>"        nil                       "cancel"               :column "Other")
  )
;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; PEL HYDRA: Scroll
(defhydra pel-∑scroll (global-map "C-<f7>"
                                  :pre  (pel--cache-hydra-is-helpful)
                                  :post (pel--restore-hydra-is-helpful)
                                  :foreign-keys run)
  ""
  ("C-<up>"      pel-scroll-down             "up"     :column "scroll")
  ("C-<down>"    pel-scroll-up               "down"   :column "scroll")
  ("C-<left>"    pel-scroll-right            "left"   :column "scroll")
  ("C-<right>"   pel-scroll-left             "right"  :column "scroll")
  ("M-?"         pel-∑-customize-hint        "hint cfg"  :column "Other")
  ("?"           pel-toggle-hydra-hint       "hint"   :column "Other")
  ("<C-f7>"      nil                         "cancel" :column "Other"))

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; PEL HYDRA: Hide/Show

(pel-autoload-file hideshow for: hs-minor-mode)
(pel-autoload-file pel-hideshow for:
                   pel-show-hide-state
                   pel-toggle-hide-all
                   pel-toggle-hide-block
                   pel-hide-all
                   pel-hide-block
                   pel-show-all
                   pel-show-block
                   pel-hide-level-1
                   pel-hide-level-2
                   pel-hide-level-3
                   pel-hide-level-4
                   pel-hs-hide-block-below-inc
                   pel-hs-hide-block-below-dec)

(defhydra pel-∑hideshow (global-map "<f7> /"
                                    :foreign-keys run)
  "Hide/Show:"
  ("/" hs-minor-mode               "Toggle hs mode" :column "State")
  ("?" pel-show-hide-state         "info")
  ("a" pel-toggle-hide-all         "all"    :column "Hide/Show")
  ("b" pel-toggle-hide-block       "block")
  ("H" pel-hide-all                "all"    :column "Hide")
  ("h" pel-hide-block              "block")
  ("S" pel-show-all                "all"    :column "Show")
  ("s" pel-show-block              "block")
  ("1" pel-hide-level-1            ">= 1"  :column "Hide levels")
  ("2" pel-hide-level-2            ">= 2")
  ("3" pel-hide-level-3            ">= 3")
  ("4" pel-hide-level-4            ">= 4")
  (">" pel-hs-hide-block-below-inc "+1" :column "Hide levels:")
  ("<" pel-hs-hide-block-below-dec "-1")
  ("<f7>" nil                      "cancel" :column "End"))

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; PEL HYDRA: C preprocessor

(when pel-use-c
  (pel-autoload-file hideif for:
                     hide-ifdef-mode
                     hide-ifdef-toggle-shadowing
                     hide-ifdef-toggle-read-only
                     hide-ifdefs
                     show-ifdefs
                     hide-ifdef-block
                     show-ifdef-block
                     hif-evaluate-macro
                     hide-ifdef-define
                     hide-ifdef-undef
                     hide-ifdef-use-define-alist
                     hide-ifdef-set-define-alist
                     hif-clear-all-ifdef-defined)

  (pel-declare-file pel-pp declares:
                    pel-pp-next-directive
                    pel-pp-prev-directive)

  (pel-autoload-file cc-cmds for:
                     c-backward-conditional
                     c-forward-conditional
                     c-up-conditional)
  (defvar pel:for-c)
  (defhydra pel-∑c (pel:for-c "<f7>"  :foreign-keys run)
    "C preprocessor"
    ("n"    pel-pp-next-directive       "next"           :column "Move to")
    ("p"    pel-pp-prev-directive       "prev"           :column "Move to")
    ("C-p"  c-backward-conditional      "begin"          :column "Move to")
    ("C-n"  c-forward-conditional       "end"            :column "Move to")
    ("C-u"  c-up-conditional            "up"             :column "Move to")
    ("#"    hide-ifdef-mode             "toggle mode"    :column "Hide")
    ("W"    hide-ifdef-toggle-shadowing "toggle shadow"  :column "Hide")
    ("R"    hide-ifdef-toggle-read-only "toggle RO"      :column "Hide")
    ("H"    hide-ifdefs                 "hide"           :column "Hide")
    ("S"    show-ifdefs                 "show"           :column "Hide")
    ("h"    hide-ifdef-block            "hide block"     :column "Hide")
    ("s"    show-ifdef-block            "show block"     :column "Hide")
    ("e"    hif-evaluate-macro          "evaluate"       :column "# Vars")
    ("d"    hide-ifdef-define           "define"         :column "# Vars")
    ("u"    hide-ifdef-undef            "undef"          :column "# Vars")
    ("U"    hide-ifdef-use-define-alist "Use list"       :column "# Vars")
    ("D"    hide-ifdef-set-define-alist "Save list"      :column "# Vars")
    ("C"    hif-clear-all-ifdef-defined "Clear all"      :column "# Vars")
    ("?"    pel-pp-show-state           "Show state"     :column "Other")
    ("<f7>" nil                         "cancel"         :column "Other")))

(when pel-use-c++
  ;; TODO: Find a way to inject the above hydra into the C++ key prefix instead
  ;;       of having to duplicate it as it is done below because now this
  ;;       produces twice as many hydra functions: one set for C and another set
  ;;       of identical functions for C++.  This wastes time and memory space.
  ;;
  ;; PEL HYDRA: C preprocessor for C++
  (defvar pel:for-c++)
  (defhydra pel-∑c++ (pel:for-c++ "<f7>"  :foreign-keys run)
    "C preprocessor"
    ("n"    pel-pp-next-directive       "next"           :column "Move to")
    ("p"    pel-pp-prev-directive       "prev"           :column "Move to")
    ("C-p"  c-backward-conditional      "begin"          :column "Move to")
    ("C-n"  c-forward-conditional       "end"            :column "Move to")
    ("C-u"  c-up-conditional            "up"             :column "Move to")
    ("#"    hide-ifdef-mode             "toggle mode"    :column "Hide")
    ("W"    hide-ifdef-toggle-shadowing "toggle shadow"  :column "Hide")
    ("R"    hide-ifdef-toggle-read-only "toggle RO"      :column "Hide")
    ("H"    hide-ifdefs                 "hide"           :column "Hide")
    ("S"    show-ifdefs                 "show"           :column "Hide")
    ("h"    hide-ifdef-block            "hide block"     :column "Hide")
    ("s"    show-ifdef-block            "show block"     :column "Hide")
    ("e"    hif-evaluate-macro          "evaluate"       :column "# Vars")
    ("d"    hide-ifdef-define           "define"         :column "# Vars")
    ("u"    hide-ifdef-undef            "undef"          :column "# Vars")
    ("U"    hide-ifdef-use-define-alist "Use list"       :column "# Vars")
    ("D"    hide-ifdef-set-define-alist "Save list"      :column "# Vars")
    ("C"    hif-clear-all-ifdef-defined "Clear all"      :column "# Vars")
    ("?"    pel-pp-show-state           "Show state"     :column "Other")
    ("<f7>" nil                         "cancel"         :column "Other")))

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; PEL HYDRA: Selective Display
;; Hide text based on indentation by column or indentation level.

(defun pel--maybe-vline-mode ()
  "Use the vertical line mode when available."
  (interactive)
  (if (and pel-use-vline
           (require 'vline nil :noerror)
           (boundp 'vline-mode)
           (fboundp 'vline-mode))
      (progn
        (vline-mode (if vline-mode -1 +1))
        (move-to-column (if selective-display
                            (max 0 (- selective-display 1))
                          0)))
    (user-error "Command vline-mode is not available.  \
Customize pel-use-vline to t!")))

;; -------
;; Define functions (instead of placing lambdas inside the hydra)
;; to prevent warnings about excessive docstring width.

(defun pel--sd-uc ()
  "Selective display un-hide column."
  (interactive)
  (set-selective-display nil))

(defun pel--sd-h1 ()
  "Selective display hide at column 1."
  (interactive)
  (set-selective-display 1))



;; pel Hydra hide-indent : renamed to reduce size
(defhydra pel-∑hi (global-map "<f7> C-x $" :foreign-keys run)
  "Selective Display"
  ("<right>" pel-selective-display-column-inc "+1"        :column "By Column")
  ("<left>"  pel-selective-display-column-dec "-1"        :column "By Column")
  ("0"       pel--sd-uc                       "unhide"    :column "By Column")
  ("1"       pel--sd-h1                       "hide at 1" :column "By Column")
  ("S-<right>"
   pel-selective-display-indent-inc        "+indent" :column "By Indent")
  ("S-<left>"
   pel-selective-display-indent-dec        "-indent" :column "By Indent")
  ("|"      pel--maybe-vline-mode "rightmost visible limit" :column "Show")
  ("<f7>"      nil                           "cancel"      :column "End"))

;;; --------------------------------------------------------------------------
(provide 'pel__hydra)

;;; pel__hydra.el ends here

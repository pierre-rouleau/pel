;;; pel-key-chord.el --- PEL Key-Chord Support. -*-lexical-binding: t-*-

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

;; -----------------------------------------------------------------------------
;;; Commentary:
;;
;; This file provides code that manages the creation of key-chords via Emacs
;; customization UI.

;; -----------------------------------------------------------------------------
;;; Code:

(require 'pel--base)     ; use: pel-concat-strings-in-list
(require 'pel--options)  ; use: pel-key-chords

(defun pel--kcs-define-global (type keys action)
  "Map a global key-chord or key-seq as identified by the arguments.
- TYPE is either key-chord or key-seq
- keys is a string of 2 keys
- action is a string or function identifying the action.
This executes a `key-seq-define-global' if TYPE is
key-seq and key-seq-define-global is bound, otherwise
it executes a `key-chord-define-global'.
Return t if done, nil otherwise."
  (if (and (eq type 'key-seq)
           (require 'key-seq nil :noerror)
           (fboundp 'key-seq-define-global))
      (progn
        (key-seq-define-global keys action)
        t)
    (if (and (require 'key-chord nil :noerror)
             (fboundp 'key-chord-define-global))
        (progn
          (key-chord-define-global keys action)
          t)
      (lwarn 'pel-key-chords :warning
             "Unable to activate global pel-key-chords entry: %S %S %S"
             type keys action)
      nil)))

(defun pel--kcs-define (type mode keys action)
  "Map a mode-specific key-chord or key-seq as identified by the arguments.
- TYPE is either key-chord or key-seq
- MODE, a symbol, identifies the mode. It must be loaded.
- keys is a string of 2 keys
- action is a string or function identifying the action.
This executes a `key-seq-define' if TYPE is
key-seq and key-seq-define is bound, otherwise
it executes a `key-chord-define'.
Return t if done, nil otherwise."
  (if (and (eq type 'key-seq)
           (require 'key-seq nil :noerror)
           (fboundp 'key-seq-define))
      (progn
        (key-seq-define mode keys action)
        t)
    (if (and (require 'key-chord nil :noerror)
             (fboundp 'key-chord-define))
        (progn
          (key-chord-define mode keys action)
          t)
      (lwarn 'pel-key-chords :warning
             "Unable to activate mode-specific pel-key-chords entry: %S %S %S %S"
             type mode keys action)
      nil)))

(defun pel-activate-key-chord-from-spec (key-chord-spec)
  "Activate the KEY-CHORD-SPEC.
The KEY-CHORD-SPEC must be 5 element list like the following:
(mode:     symbol
 fname:    string
 key-type: key-chord|key-seq
 key:      string
 action:   string|function)
The list element are:
1. A symbol: either 'global or the name of a mode where the key-chord
   or key-seq will be active.
2. A file name which is loaded to activate the mode and which will be
   used as the trigger to activate the corresponding key-chord or key-seq
   if the mode is not bound when the function is called.
   This may also be an empty string, in which case the key-cord/key-seq
   is defined when Emacs starts as long as the symbol identifies an already
   loaded and bounded mode.
   This is ignored for global key-chord and key-seq.
3. A key-type symbol, identifying either key-chord or key-seq.
   - When key-type is key-chord the key map is updated with
     `key-chord-define-global' when symbol is global, and with
     `key-chord-define' for mode-specific chords, scheduled when
     the identified file is loaded.
   - When key-type is key-seq, the key binding is done with
     `key-seq-define-global' and `key-seq-define' instead.
4. A string of 2 characters: the keys that identify the key-chord
   or key-seq.
5. The action to execute when the key-chord/key-seq is typed.
   This is one of:
   - a function or lambda
   - A string describing the key sequence to execute.

Return one of:
- t if a global or bound mode key-chord/key-seq definition
  succeeded.
- A list with the same values as the key-chord-spec argument, untouched.
  This means that the mode is currently not bounded and the definition must
  be deferred."
  (let ((kc-mode  (car key-chord-spec))
        (kc-type  (nth 2 key-chord-spec))
        (kc       (nth 3 key-chord-spec))
        (kc-exec  (nth 4 key-chord-spec))
        done)
    (if (eq kc-mode 'global)
        (setq done (pel--kcs-define-global kc-type kc kc-exec))
      (let ((kc-mode-map (pel-map-symbol-for kc-mode)))
        (when (and (fboundp kc-mode)
                   (boundp kc-mode-map)
                   (keymapp (symbol-value kc-mode-map)))
          (setq done (pel--kcs-define kc-type (symbol-value kc-mode-map) kc
                                      kc-exec)))))
    (if done t key-chord-spec)))

(defun pel-activate-key-chords-in (key-chords-spec)
  "Activate all global key-chords in KEY-CHORDS-SPEC.
Return an alist of (mode . fname) for which the activation must be deferred."
  (let ((deferred-modes ())) ; alist of (mode . fname) that must be deferred
    (dolist (spec key-chords-spec)
      (let ((activation-result (pel-activate-key-chord-from-spec spec)))
        ;; activation-result := (mode-symbol
        ;;                       fname
        ;;                       key-type
        ;;                       key-string
        ;;                       action-string|function)
        (unless (eq activation-result t)
          (let ((mode    (car activation-result))
                (fname   (cadr activation-result)))
            (unless (assoc mode deferred-modes)
              (setq deferred-modes (cons (cons mode fname) deferred-modes)))))))
    deferred-modes))

(defun pel--activate-deferred-key-chords ()
  "Activates deferred key-chord(s).  Display a message to help follow."
  (message "Activating deferred key-chord.")
  (pel-activate-key-chords-in pel-key-chords))

(defun pel-activate-all-key-chords ()
  "Activate all key-chords defined in `pel-key-chords'."
  (let ((deferred-modes-alist (pel-activate-key-chords-in pel-key-chords)))
    (dolist (mode-fname deferred-modes-alist)
      (let ((mode  (car mode-fname))
            (fname (cdr mode-fname)))
        (message "deferring %s, via loading %s" mode fname)
        ;; for deferral, just re-execute the complete interpretation of
        ;; pel-key-chords.  This way if a change occurred in it, it will
        ;; be activated as soon as possible.
        (eval-after-load fname
          '(pel--activate-deferred-key-chords))))))

;; -----------------------------------------------------------------------------
(provide 'pel-key-chord)

;;; pel-key-chord.el ends here

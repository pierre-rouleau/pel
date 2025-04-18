;;; pel-prompt.el --- PEL Prompt Utilities -*- lexical-binding: t; -*-

;; Created   : Saturday, February 29 2020.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2025-03-14 13:57:57 EDT, updated by Pierre Rouleau>

;; This file is part of the PEL package
;; This file is not part of GNU Emacs.

;; Copyright (C) 2020, 2021, 2022, 2024, 2025  Pierre Rouleau
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
;; This file defines functions that prompt the user.
;;
;; The file defines the following functions:
;;
;; - `pel-y-n-e-or-l-p'
;; - `pel-set-user-option'
;; - `pel-select-string-from'
;; - `pel-select-symbol-from'
;;   - `pel-select-from'
;;     - `pel--prompt-for'
;;       - `pel--var-value-description'
;; - `pel-prompt-purpose-for'
;; - `pel-prompt-function'
;; - `pel-prompt-args'
;; - `pel-prompt'
;; - `pel-prompt-with-completion'
;; - `pel-prompt-title'
;;
;; The `pel-y-n-e-or-l-p' function is a minor modification of the Emacs'
;; y-or-n-p.  It has the ability to type "e" or "E" as an answer to
;; identify an edit-replacement action and "l" or "L" to request searching
;; a library file.  It also modified the return type so it can return one of
;; 4 outcomes: yes, no, edit or findlib.
;;
;; The `pel-select-from' function provides a selection of choices to select from,
;; providing a quick interactive selection of choices.  It is used by the
;; following higher level functions:
;; - `pel-set-user-option' sets a global or buffer local value for the
;;    specified user-option variable.
;; - `pel-select-string-from'
;; - `pel-select-symbol-from'
;;
;; The `pel-prompt-purpose-for' function prompts for the purpose of a specific
;; item (file or function) and maintains a prompt history for each item.
;; The `pel-prompt-function' prompts for a function name.  It also maintains a
;; prompt history for the functions created in each major mode.
;; The `pel-prompt-args' prompts for function arguments.  It maintains a prompt
;; history for the function arguments created in each major mode.
;; These prompt histories do not persist when Emacs is stopped.
;;
;; The `pel-prompt' is a generic prompt with history.
;; `pel-prompt-with-completion' is also generic, with prompt history but also
;; provides a completion from the choices given in a collection list.
;;
;; The `pel-prompt-title' is a specialized prompt for a title string.

;;; --------------------------------------------------------------------------
;;; Dependencies
(require 'pel--base)                  ; use: `pel-capitalize-first-letter'
;;                                    ;      `pel-end-text-with-period'
(require 'pel--options)               ; use `pel-prompt-read-method'
(eval-when-compile (require 'subr-x)) ; use: `string-trim'
(eval-when-compile
  (require 'cl-lib)                   ; use: `cl-dolist' and `cl-return'
  (require 'cl-macs))                 ; use: `cl-case'.


;;; Code:

(defvar pel-prompt-map
  (let ((map (make-sparse-keymap)))
    (define-key map "y" 'yes)
    (define-key map "Y" 'yes)
    (define-key map "n" 'no)
    (define-key map "N" 'no)
    (define-key map "e" 'edit)
    (define-key map "E" 'edit)
    (define-key map "l" 'findlib)
    (define-key map "L" 'findlib)
    (define-key map "\C-g" 'no)
    (define-key map "\C-]" 'no)
    (define-key map "\e" 'no)
    (define-key map [escape] 'no)
    map)
  "Keymap of responses to PEL prompts.
The \"bindings\" in this map are not commands; they are answers.
The valid answers include `yes', `no', `edit' and `findlib'.

This keymap is used by `pel-y-n-e-or-l-p'")

;;-pel-autoload
(defun pel-y-n-e-or-l-p (prompt)
  "Ask user a \"y, n or e\" question.
Return:
- \\='yes     if the answer is \"y\",
- \\='no      if it is \"n\",
- \\='edit    if it is \"e\", or
- \\='findlib if it is \"!\".
PROMPT is the string to display to ask the question.
It should end in a space;
`pel-y-n-e-or-l-p' adds \"(y, n, e or l) \" to it.
.
No confirmation of the answer is requested; a single character is
enough.
.
Under a windowing system a dialog box will be used if `last-nonmenu-event'
is nil and `use-dialog-box' is non-nil."
  ;; ¡Beware! when I tried to edebug this code, Emacs got into a weird state
  ;; where all the keys were unbound (i.e. it somehow got triggered
  ;; within read-key, apparently).  I had to kill it.
  (let ((answer 'first-answer)
        (clarification
         "Please answer one of these letters: \
y/Y (yes), n/N (no), e/E (edit), l/L (library).\n")
        ;; set padded lambda to pad prompt.
        (padded (lambda (prompt &optional dialog)
                  (let ((l (length prompt)))
                    (concat prompt
                            (if (or
                                 (zerop l)
                                 (eq ?\s (aref prompt (1- l))))
                                "" " ")
                            (if dialog "" "(y, n, e or l) "))))))
    (cond
     ;; in non-interactive mode simple prompt/read from minibuffer.
     (noninteractive
      (setq prompt (funcall padded prompt))
      (let ((temp-prompt prompt))
        (while (not (memq answer '(act skip edit-replacement automatic)))
          (let ((str (read-string temp-prompt)))
            (cond ((member str '("y" "Y")) (setq answer 'yes))
                  ((member str '("n" "N")) (setq answer 'no))
                  ((member str '("e" "E")) (setq answer 'edit))
                  ((member str '("l" "L")) (setq answer 'findlib))
                  (t (setq temp-prompt (concat clarification prompt))))))))
     ;; for mouse events, use a pop-up dialog.
     ((and (display-popup-menus-p)
           last-input-event             ; not during startup
           (listp last-nonmenu-event)   ; last event was a mouse event
           use-dialog-box)
      (setq prompt (funcall padded prompt t)
            answer (x-popup-dialog t `(,prompt
                                       ("Yes"          . yes)
                                       ("No"           . no)
                                       ("Edit"         . edit)
                                       ("Find Libfile" . findlib)))
            ))
     ;; In all other cases use the echo area.
     ;; Ensure window in frame is visible.
     (t
      (setq prompt (funcall padded prompt))
      (while
          (let* ((first-prompt '(first-answer))
                 (key
                  (let ((cursor-in-echo-area t))
                    (when minibuffer-auto-raise
                      (raise-frame (window-frame (minibuffer-window))))
                    (read-key (propertize (if (memq answer first-prompt)
                                              prompt
                                            (concat clarification prompt))
                                          'face 'minibuffer-prompt)))))
            (setq answer (lookup-key pel-prompt-map (vector key) t))
            (cond
             ((memq answer '(yes no edit findlib)) nil)
             (t t)))
        (ding)
        (discard-input))
      ))
    ;; Check final answer.
    ;;  In interactive display message (prompt followed with y or n)
    ;;  Return 'yes, 'no, 'edit or 'findlib
    (let ((retl
           (cond
            ((eq answer 'yes)       '(yes  . ?y))
            ((eq answer 'no)        '(no   . ?n))
            ((eq answer 'edit)      '(edit . ?e))
            ((eq answer 'findlib)   '(findlib . ?l))
            (t nil))))
      (unless noninteractive
        (message "%s%c" prompt (cdr retl)))
      (car retl))))

;; -----------------------------------------------------------------------------
;; Prompt for choices from a list for choices
;; ------------------------------------------
;;
;; - `pel-select-from'
;;   - `pel--prompt-for'
;;     - `pel--var-value-description'

(defun pel--var-value-description (value selection)
  "Return string for the specific VALUE given the SELECTION.

- SELECTION argument is a list of choices.
Each choice is a list of 3 elements:
  - A character, presented to the user to select the corresponding choice.
  - A string, describing the choice
  - A value, returned or used for the choice."
  (cl-dolist (char.string.value selection)
    (when (eq value (nth 2 char.string.value))
      (cl-return (nth 1 char.string.value)))))

(defun pel--prompt-for (title selection &optional current nil-value)
  "Return a prompt string with TITLE for SELECTION.
SELECTION is a list of (char prompt value).
CURRENT optionally identifies the currently used value.  It may hold
the spacial value :no-current-value to prevent display of current value.

NIL-VALUE optional string identifies meaning for a nil value.  It is
required when the USER-OPTION may be set to the same thing by one
value in the SELECTION but also by the nil value, and that nil
value is not part of the SELECTION."
  (format "%s%s: %s."
          title
          (if (eq current :no-current-value)
              "Select"
            (let ((initial-value (or (pel--var-value-description current
                                                                 selection)
                                     nil-value)))
              (if initial-value
                  (format " [%s]. Select" initial-value)
                "")))
          (mapconcat (lambda (elt)
                       (format "%c: %s"
                               (car elt) (cadr elt)))
                     selection
                     ", ")))

;; pel-autoload
(defun pel-select-from (title selection &optional current-value action nil-value)
  "Prompt user with a TITLE for a SELECTION of choices.
It optionally displays the CURRENT-VALUE in the prompt and
also optionally calls the ACTION function passing selected value.
The prompt list the choices in the order of SELECTION list.
The SELECTION argument is a list of choices.
Each choice is a list of 3 elements:
  - A character, presented to the user to select the corresponding choice.
  - A string, describing the choice
  - A value, returned or used for the choice.
If ACTION is nil or if the choice is the same as CURRENT-VALUE,
`pel-select-from' returns the selected value, otherwise it
returns the value returned by (ACTION selected-value) evaluation.

NIL-VALUE optional string identifies a meaning for a nil value.  It is
required when the USER-OPTION may be set to the same thing by one
value in the SELECTION but also by the nil value, and that nil
value is not part of the SELECTION."
  (let* ((prompt    (pel--prompt-for title selection current-value nil-value))
         (chars     (mapcar #'car selection))
         (choice    (read-char-choice prompt chars))
         (requested-value (nth 2 (assoc choice selection))))
    (if (and action
             (not (equal requested-value current-value)))
        (funcall action requested-value)
      requested-value)))

(defun pel-select-symbol-from (title symbols &optional first-idx)
  "Prompt with a TITLE to select from a set of SYMBOLS.
The list of choices use index characters starting at 1 unless
FIRST-IDX is provided: a character.
Return the selected symbol.
TITLE   := a prompt string.  It should end with a space.
SYMBOLS := a list of symbols."
  (let* ((choices '())
         (idx     (or first-idx ?1))
         (choice (pel-select-from
                  title
                  (dolist (symbol symbols (reverse choices))
                    (push (list                 ; each list entry must have:
                           idx                  ; a selector character
                           (symbol-name symbol) ; a descriptive string
                           symbol)              ; the value to return
                          choices)
                    (setq idx (1+ idx))))))
    ;; clear echo area and return choice
    (message nil)
    choice))

(defun pel-select-string-from (title strings &optional first-idx)
  "Prompt with a TITLE to select from a set of STRINGS.
The list of choices use index characters starting at 1 unless
FIRST-IDX is provided: a character.
Return the selected symbol.
TITLE   := a prompt string.  It should end with a space.
STRINGS := a list of strings."
  (let* ((choices '())
         (idx     (or first-idx ?1))
         (choice  (pel-select-from
                   title
                   (dolist (string strings (reverse choices))
                     (push (list                 ; each list entry must have:
                            idx                  ; a selector character
                            string               ; a descriptive string
                            string)              ; the value to return
                           choices)
                     (setq idx (1+ idx)))
                   :no-current-value)))
    ;; clear echo area and return choice
    (message nil)
    choice))

(defun pel-prompt-select-read (prompt strings)
  "Display PROMPT to request selection of one of the STRINGS.

Return the selected string or nil on escape.

The prompt mechanism is using the back-end selected by
`pel-prompt-read-method' user-option."
  (cl-case pel-prompt-read-method
    ((nil)
     (pel-select-string-from prompt strings ?0))
    ((ivy)
     (if  (and (require 'ivy nil :noerror)
               (fboundp 'ivy-read))
         (ivy-read prompt strings)
       (user-error "Can't load ivy")))))

;; ---------------------------------------------------------------------------
;; Set user-option from selected choices

(defun pel-set-user-option (prompt user-option selection &optional locally nil-value)
  "PROMPT to set the value of USER-OPTION to one of the SELECTION.

- USER-OPTION is a user-option symbol.  It must be bound.
- SELECTION argument is a list of choices.
Each choice is a list of 3 elements:
  - A character, presented to the user to select the corresponding choice.
  - A string, describing the choice
  - A value, returned or used for the choice.

Affects Emacs globally unless the LOCALLY argument is non-nil, in which case a
buffer local variable is created for that user-option and the change value
only affects the current buffer.

NIL-VALUE optionally identify a meaning for a nil value.  It is
required when the USER-OPTION may be set to the same thing by one
value in the SELECTION but also by the nil value, and that nil
value is not part of the SELECTION."
  (let ((var-name  (symbol-name user-option))
        (new-value (pel-select-from prompt
                                    selection
                                    (symbol-value user-option)
                                    nil
                                    nil-value)))
    (when locally
      (with-current-buffer (current-buffer)
        (make-local-variable user-option)))
    (set user-option new-value)
    (message "%s now set to %s"
             var-name
             (pel--var-value-description
              new-value
              selection))))

;; ---------------------------------------------------------------------------
;; Prompt for purpose and function names

(defun pel-prompt-purpose-for (item &optional default)
  "Prompt for ITEM purpose and return adjusted user input string.
The returned string is trimmed, its first letter is capitalized
and is terminated by a period.
Holds an independent prompt history for each ITEM.
If the user hit returns and does not enter anything at the prompt
the function returns an empty string if DEFAULT is nil, otherwise it
returns DEFAULT"
  (let ((history-symbol (intern
                         (format
                          "pel-prompt-history-for-purpose-%s" item)))
        (prompt-text    (format "%s purpose: " item)))
    (pel-use-or
     (string-trim
      (read-from-minibuffer prompt-text nil nil nil history-symbol))
     (function pel-hastext)
     default
     (function pel-capitalize-first-letter)
     (function pel-end-text-with-period))))

(defun pel-prompt-class (&optional transform-class)
  "Prompt for class and return potentially transformed input string.
If TRANSFORM-CLASS is non-nil it must be a function that accepts
the class var-name string and return it transformed or nil if the class
var-name is not acceptable.
Holds an independent class prompt history for each major mode."
  (let ((history-symbol (intern
                         (format
                          "pel-prompt-history-for-class-%s" major-mode)))
        (fname          nil))
    (while (not fname)
      (setq fname
            (string-trim
             (read-from-minibuffer "Class name: " nil nil nil history-symbol)))
      (when transform-class
        (setq fname (funcall transform-class fname))))
    fname))

(defun pel-prompt-function (&optional transform-function)
  "Prompt for function and return potentially transformed input string.
If TRANSFORM-FUNCTION is non-nil it must be a function that accepts
the function var-name string and return it transformed or nil if the function
var-name is not acceptable.
Holds an independent function prompt history for each major mode."
  (let ((history-symbol (intern
                         (format
                          "pel-prompt-history-for-function-%s" major-mode)))
        (fname          nil))
    (while (not fname)
      (setq fname
            (string-trim
             (read-from-minibuffer "Function name: " nil nil nil history-symbol)))
      (when transform-function
        (setq fname (funcall transform-function fname))))
    fname))

(defun pel-prompt-args (&optional transform-function)
  "Prompt for argument(s) and return potentially transformed input string.
If TRANSFORM-FUNCTION is non-nil it must be a function that accepts
the function var-name string and return it transformed or nil if the function
var-name is not acceptable.
Holds an independent function prompt history for each major mode."
  (let ((history-symbol (intern
                         (format
                          "pel-prompt-history-for-args-%s" major-mode)))
        (args          nil))
    (while (not args)
      (setq args
            (string-trim
             (read-from-minibuffer "Function arg(s): " nil nil nil history-symbol)))
      (when transform-function
        (setq args (funcall transform-function args))))
    args))

;; ---------------------------------------------------------------------------
;; Generic prompt

(defun pel-prompt (prompt &optional scope capitalize)
  "Generic PROMPT for string.

Optionally identify a SCOPE symbol for the prompt history.
If it is specified the prompt has its own history for each major mode,
otherwise it has no history.

Return entered string, optionally capitalized if CAPITALIZE is non-nil."
  (let* ((history-symbol (when scope
                           (intern
                            (format "pel-prompt-%s-%s" scope major-mode))))
         (text (string-trim
                (read-from-minibuffer (format "%s: " prompt)
                                      nil nil nil history-symbol))))
    (if capitalize
        (pel-capitalize-first-letter text)
      text)))


(defun pel-prompt-with-completion (prompt collection &optional scope)
  "Generic PROMPT for string with tab-completion from COLLECTION.

COLLECTION must be a list of strings.

Optionally identify a SCOPE symbol for the prompt history.
If it is specified the prompt has its own history for each major mode,
otherwise it has no history."
  (completing-read prompt collection
                   nil nil nil
                   (when scope
                     (intern (format
                              "pel-prompt-%s-%s"
                              scope
                              major-mode)))))

;; ---------------------------------------------------------------------------
;; Title prompt

(defun pel-prompt-title (&optional with-full-stop)
  "Prompt and return a title string.
Returned string has the first letter capitalized.
If WITH-FULL-STOP is non-nil a period is added if user did not enter one."
  (let ((title (pel-prompt "Title" 'title :capitalize)))
    (if with-full-stop
        (pel-end-text-with-period title)
      title)))

;; -----------------------------------------------------------------------------
(provide 'pel-prompt)

;;; pel-prompt.el ends here

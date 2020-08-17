;;; pel-prompt.el --- PEL Prompt Utilities -*-lexical-binding: t-*-

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
;; This file defines functions that prompt the user.
;;
;; The file defines the following functions:
;;
;; - `pel-y-n-e-or-l-p'
;; - `pel-select-from'
;;   - `pel--prompt-for'
;;
;;
;; The `pel-y-n-e-or-l-p' function is a minor modification of the Emacs'
;; y-or-n-p.  It has the ability to type "e" or "E" as an answer to
;; identify an edit-replacement action and "l" or "L" to request searching
;; a library file. It also modified the return type so it can return one of
;; 4 outcomes: yes, no, edit or findlib.
;;
;; The `pel-select-from' function provides a selection of choices to select from,
;; providing a quick interactive selection of choices.


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
- 'yes     if the answer is \"y\",
- 'no      if it is \"n\",
- 'edit    if it is \"e\", or
- 'findlib if it is \"!\".
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
;;

(defun pel--prompt-for (title selection &optional current)
  "Return a prompt string with TITLE for  SELECTION.
SELECTION is a list of (char prompt value).
CURRENT optionally identifies the currently used value."
  (format "%s%s. Select: %s."
          title
          (let ((text "?"))
            (format
             " [%s]"
             (dolist (choice selection text)
               (when (equal (nth 2 choice) current)
                 (setq text (nth 1 choice))))))
          (mapconcat (lambda (elt)
                       (format "%c: %s"
                               (car elt) (cadr elt)))
                     selection
                     ", ")))

(defun pel-select-from (title selection &optional current-value action)
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
returns the value returned by (ACTION selected-value) evaluation."
  (let* ((prompt    (pel--prompt-for title selection current-value))
         (chars     (mapcar #'car selection))
         (choice    (read-char-choice prompt chars))
         (requested-value (nth 2 (assoc choice selection))))
    (if (and action
             (not (equal requested-value current-value)))
        (funcall action requested-value)
      requested-value)))

;; -----------------------------------------------------------------------------
(provide 'pel-prompt)

;;; pel-prompt.el ends here

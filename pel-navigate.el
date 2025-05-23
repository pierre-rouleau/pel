;;; pel-navigate.el --- PEL Navigation Support -*-lexical-binding: t-*-

;; Copyright (C) 2020, 2023, 2024, 2025  Pierre Rouleau

;; Author: Pierre Rouleau <prouleau001@gmail.com>

;; This file is not part of GNU Emacs.

;;; License:

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


;;; Commentary:
;;
;; This file contains a collection of navigation commands that complement the
;; standard Emacs navigation commands.
;;
;;  - `pel-to-forward-space' moves point forward to the first whitespace character
;;     following non-whitespace characters, and `pel-to-backward-space' moves
;;     point backward after the last white-space character before non-whitespace.
;;  - `pel-beginning-of-line' is meant to replace `beginning-of-line' as it
;;    does the same and extends it: if point is already at the beginning of
;;    the line then it moves it to the first non-whitespace character.
;;  - `pel-end-of-line' does the same for end of line, ending at the
;;    complete end or the last non-whitespace character, before the trailing
;;    spaces.
;;    - Note that calling `pel-end-of-line' followed by
;;      `pel-beginning-of-line' the point is guaranteed to be at column 0.
;;      Also, calling `pel-beginning-of-line' followed by `pel-end-of-line'
;;      ensures that point is located at the real end of the line.
;;
;;  - `pel-find-thing-at-point' provides a search capability without the need
;;    for a tag database but it is limited in what it can find.  It's a poor
;;    man cross reference.
;;  - `pel-show-char-syntax' shows the character syntax of the character at
;;    point.
;;  - `pel-forward-token-start' and `pel-backward-to-start' move forward or
;;    backward to the beginning of a text semantic token as defined by Emacs
;;    character syntax for the current buffer.

;;  - `pel-forward-word-start' moves point to the beginning of next word.
;;    This complements what's already available in standard Emacs:
;;    `forward-word' and `backward-word'.
;;  - `pel-forward-syntaxchange-start' and `pel-backward-syntaxchange-start'
;;    move point forward or backward to the character syntax change character.
;;    This can be useful to debug syntax characters for a specific mode.
;;  - `pel-next-visible' and `pel-previous-visible' move point to the next or
;;    previous visible (non whitespace) character.
;;  - `pel-home' and `pel-end' implement a quick, multi-hit movement to the
;;    beginning or end of the current field, line, window and buffer.
;;    These commands are similar to the home and end CRiSP/Brief commands.
;;    They also support the multiple window scroll sync provided by the
;;    `pel-scroll' commands.
;;  - `pel-beginning-of-next-defun' move point to the beginning of the
;;    next function definition.  This complements `beginning-of-defun' which
;;    only reaches the same location by moving backwards.


;;; Code:

(require 'pel--base)                    ; use: pel--n-funcall-to
;;                                      ;      pel-toggle-and-show
(require 'pel-scroll)                   ; use: pel-in-scroll-sync
(require 'subword)                      ; use: superword-mode

;; ---------------------------------------------------------------------------
;; [:todo 2025-01-17, by Pierre Rouleau: Consolidate with pel-next-visible and
;; pel-previous-visible and reduce the number of available commands while
;; providing the best control.  There's overlap between these 2 sets of
;; functions and the difference of behavious issubtle depending of the major
;; mode of the buffer.]

;; Move to next space
(defun pel-to-forward-space ()
  "Move point to the next space character."
  (interactive "^")
  (let ((pos (point)))
    (forward-whitespace 1)
    (forward-whitespace -1)
    ;; if point was already on whitespace on inside several spaces, it did not
    ;; move forward, so move to the beginning of next non-whitespace and try again.
    (when (>= pos (point))
      (forward-word 1)
      (backward-word 1)
      (forward-whitespace 1)
      (forward-whitespace -1))))

(defun pel-to-backward-space ()
  "Move point to the previous space character."
  (interactive "^")
  (let ((pos (point)))
    (forward-whitespace -1)
    (forward-whitespace 1)
    ;; if point was already on whitespace on inside several spaces, it did not
    ;; move backward, so move to the end of previous non-whitespace and try
    ;; again.
    (when (<= pos (point))
      (backward-word 1)
      (forward-word 1)
      (forward-whitespace -1)
      (forward-whitespace 1))))

;; ---------------------------------------------------------------------------
;; Smart Beginning of line
;; -----------------------

;;-pel-autoload
(defun pel-beginning-of-line (&optional n)
  "Move point to beginning of current line or N lines away.
If N is not specified, or 0: move to beginning of current line.
- For the current line, if point is already at the beginning of the
  line, then move the first non-whitespace (to indentation) if any,
  otherwise don't move.
If N > 0:  move to the beginning of N line forward.
If N < 0:  move to the beginning of (abs N) lines backward.
This combines the functionality of `move-beginning-of-line'
and `back-to-indentation' with a more logical meaning for N."
  (interactive "^P")
  (let ((n (if (not n)
               0
             (prefix-numeric-value n))))
    (if (< n 0)
        (progn
          (move-end-of-line (1+ n))
          (setq n 1))
      (setq n (1+ n)))
    (if (and (eq n 1)
             (bolp))
        (back-to-indentation)
      (move-beginning-of-line n))))


;; Smart end of line
;; -----------------

;;-pel-autoload
(defun pel-end-of-line (&optional n)
  "Move point to end of current line or N lines away.

If N is not specified, or 0: move to the end of the current line.
- For the current line, if point is already at the end of the line, then
  move the last non-whitespace character if any (otherwise don't move).
If N > 0:  move to the end of N line forward.
If N < 0:  move to the end of (abs N) lines backward."
  (interactive "^P")
  (let ((n (if (not n)
               0
             (prefix-numeric-value n))))
    (if (< n 0)
        (progn
          (move-end-of-line (1+ n))
          (setq n 1))
      (setq n (1+ n)))
    (if (and (eq n 1)
             (eolp))
        (progn
          (let ((pos (point)))
            (forward-line 0)
            (skip-chars-forward " \t")
            (unless (looking-at "\n")
              (goto-char pos)
              (re-search-backward "[^ \t\r\n]" nil t)
              (right-char))))
      (move-end-of-line n))))

;; ---------------------------------------------------------------------------
;; Navigate across code using symbols at point
;; -------------------------------------------

;;-pel-autoload
(defun pel-find-thing-at-point (&optional in-other-window)
  "Find source code of function or variable at point.
Open in current window unless a \\[universal-argument] prefix is
supplied as IN-OTHER-WINDOW in which case it opens inside the other window."
  (interactive "P")
  (let ((symb (function-called-at-point)))
    (if symb
        (if (consp in-other-window)
            (find-function-other-window symb)
          (find-function symb))
      (if (consp in-other-window)
          (find-variable-other-window symb))
      (find-variable symb))))

;; ---------------------------------------------------------------------------
;; Description Utilities
;; ---------------------

;;-pel-autoload
(defun pel-show-char-syntax ()
  "Display a message showing the character syntax of character at point."
  (interactive)
  (message "Char syntax of '%c' at point is: '%c'"
           (char-after)
           (char-syntax (char-after))))

;; ---------------------------------------------------------------------------
;; Navigate over tokens: words/symbols and over whitespace.
;; --------------------------------------------------------
;; This section provides two main commands:
;;
;; - pel-forward-token-start
;; - pel-backward-token-start
;;
;; These move point to the beginning of the next or previous
;; token. The token is either
;; - a word (made of word characters or symbol characters)
;; - a punctuation or symbol following whitespace.
;;
;; Both of the functions accept the special arguments N.
;; A positive N performs the operation N times.
;; A negative N preforms the reverse operation N times.
;;
;; Implementation call tree:
;;
;; * pel-forward-token-start
;; * pel-backward-token-start
;;   - pel--forward-token-start
;;   - pel--backward-token-start
;;     - pel-at-token-start-p
;;       - pel-at-word-or-symbol-start-p
;;         - pel-is-word-or-symbol-char
;;       - pel-at-wspace-end-p
;;       - pel-at-operator-before-wspace-p
;;         - pel-char-next
;;         - pel-is-operator-char
;;         - pel-is-whitespace-or-newline
;;

;; --
(defun pel-is-word-or-symbol-char (ch)
  "Return non-nil value if CH is a word or symbol char table, nil otherwise.
The syntax table of the current buffer is used to check.
.
The returned non-nil value is a list where the head is
the syntax type code for `ch'."
  (memq (char-syntax ch) '(?w ?_)))

(defun pel-at-word-or-symbol-start-p ()
  "Return t if point is located at symbol boundary character."
  (and (pel-is-word-or-symbol-char (char-after))
       (not (pel-is-word-or-symbol-char (char-before)))))

;; --
(defun pel-at-wspace-end-p ()
  "Return t if point is located at end of whitespace."
  (and (= (char-syntax (char-before))  ?\s)
       (not (= (char-syntax (char-after)) ?\s))))

(defun pel-char-next ()
  "Return character next after point."
  (char-after (+ (point) 1)))

(defun pel-is-operator-char (ch)
  "Return non-nil if CH is an operator character, nil otherwise.
The syntax table of the current buffer is used to check.
.
The returned non-nil value is a list where the head is
the syntax type code for `ch'."
  (memq (char-syntax ch) '(?. ?\" ?_)))

;; --
(defun pel-is-whitespace-or-newline (ch)
  "Return non-nil if CH is a whitespace or newline character, nil otherwise.
The syntax table of the current buffer is used to check.
.
The returned non-nil value is a list where the head is
the syntax type code for `ch'."
  (memq (char-syntax ch) '(?\s ?>)))

(defun pel-at-operator-before-wspace-p ()
  "Return t if point is located at the end of an operator before whitespace."
  (and (pel-is-operator-char (char-after))
       (pel-is-whitespace-or-newline (pel-char-next))))

;; --
;; Point status predicates
(defun pel-at-token-start-p ()
  "Return t if point is located a word/symbol or whitespace boundary."
  (or
   (pel-at-word-or-symbol-start-p)
   (pel-at-wspace-end-p)
   (pel-at-operator-before-wspace-p)))

;; --
;; utility functions called by the top level ones
(defun pel--forward-token-start ()
  "Move forward to the start of the next token."
  (while (progn (forward-char) (not (pel-at-token-start-p)))))

(defun pel--backward-token-start ()
  "Move backward to the start of the previous token."
  (while (progn (backward-char) (not (pel-at-token-start-p)))))

;; --
;;-pel-autoload
(defun pel-forward-token-start (&optional n)
  "Move forward to the start of the next token.
.
A token being identified by:
- any word (with all characters allowed by syntax table)
- punctuation
- first character after whitespace.
.
Move over whitespace but stop at comments, operators,
punctuation.
.
Argument N is a numeric argument identifying the number
of times the operation is done.  If N is negative the
move is reversed (and goes backward)."
  (interactive "^P")
  (pel-n-funcall-to (prefix-numeric-value n)
                    'pel--forward-token-start
                    'pel--backward-token-start))

;;-pel-autoload
(defun pel-backward-token-start (&optional n)
  "Move backward to the start of the previous token.
Argument N is a numeric argument identifying the number
of times the operation is done.  If N is negative the
move is reversed (and goes forward).
.
See `pel-forward-token-start' for details."
  (interactive "^P")
  (pel-n-funcall-to (prefix-numeric-value n)
                    'pel--backward-token-start
                    'pel--forward-token-start))

;; ---------------------------------------------------------------------------
;; Move to the beginning of next word
;; -----------------------------------
;;
;; The standard forward-word moves point at the end of next word, and
;; backward-word moves point to the beginning of the previous word. There is
;; no command that moves point forward to the beginning of next word. That's
;; what pel-forward-word-start does.  It's behaviour is affected by the
;; current value of superword-mode.

;; Implementation call tree:
;; * pel-forward-word-start
;;   - pel-at-word-boundary-p

(defun pel-at-word-boundary-p ()
  "Return t if point is located at a word boundary character."
  (let ((syntax-chrs (if superword-mode
                         '(?w ?_)
                       '(?w)))
        (c-after (char-after)))
    (or (not c-after)
        (and (memq (char-syntax c-after) syntax-chrs)
             (not (memq (char-syntax (char-before)) syntax-chrs))))))

;; NOTE: Another possible implementation of the function above.
;; Has the same behaviour AFAIK.
;; (defun pel-at-word-boundary-p ()
;;   "Return t if point is located at a word boundary character."
;;   ;; The syntax class integer for a word is: 2
;;   ;; see: info (elisp)Syntax Table internals
;;   (and (= (syntax-class (syntax-after (point))) 2)
;;        (not (= (syntax-class (syntax-after (1- (point)))) 2))))

;;-pel-autoload
(defun pel-forward-word-start ()
  "Move point forward to beginning of next word.
Supports command `superword-mode' but not the command `subword-mode'.
On reaching end of buffer, stop and signal error."
  (interactive "^")
  (while (progn (forward-char) (not (pel-at-word-boundary-p)))))

;; ---------------------------------------------------------------------------
;; Navigate to end of whitespace
;; -----------------------------
;;

;;-pel-autoload
(defun pel-forward-wspace-start ()
  "Forward: stop at end of whitespace."
  (interactive)
  (while (progn (forward-char) (not (pel-at-wspace-end-p)))))

;;-pel-autoload
(defun pel-backward-wspace-start ()
  "Backward: stop at end of whitespace."
  (interactive)
  (while (progn (backward-char) (not (pel-at-wspace-end-p)))))

;; ---------------------------------------------------------------------------
;; Navigate to change of character syntax
;; --------------------------------------
;; The following commands help investigate the syntactic elements of an Emacs
;; major mode by moving point to the next or previous character syntax change
;; location.

;; Implementation call tree:
;;
;; * pel-forward-syntaxchange-start
;; * pel-backward-syntaxchange-start
;;   - pel-at-syntax-change-p
;;

(defun pel-at-syntax-change-p ()
  "Return t if point is located at a boundary between 2 character syntax."
  (not (= (char-syntax (char-after)) (char-syntax (char-before)))))

;;-pel-autoload
(defun pel-forward-syntaxchange-start ()
  "Move point forward: stop at beginning of character syntax change."
  (interactive "^")
  (while (progn (forward-char) (not (pel-at-syntax-change-p)))))

;;-pel-autoload
(defun pel-backward-syntaxchange-start ()
  "Move point backward: stop at beginning of character syntax change."
  (interactive "^")
  (while (progn (backward-char) (not (pel-at-syntax-change-p)))))

;; ---------------------------------------------------------------------------
;; Moving to next/previous visible
;; -------------------------------
;; Move to previous/next non-whitespace
;; They handle:
;;  - the shift-select-mode using the interactive "^",
;;  - prefix numbers for multiple execution.

;;-pel-autoload
(defun pel-next-visible (&optional n)
  "Move point to the next non-whitespace character.
- N defaults to 1.
  Use a larger integer value to jump over several whitespace groups.
- Handles `shift-select-mode' so when the Shift key is pressed, the movement
  extends the marked region."
  (interactive "^P")
  (call-interactively (function pel-forward-token-start) t (vector n)))

;;-pel-autoload
(defun pel-previous-visible (&optional n)
  "Move point just after the previous non-whitespace character.
- N defaults to 1.
  Use a larger integer value to jump over several whitespace groups.
- Handles `shift-select-mode' so when the Shift key is pressed, the movement
  extends the marked region."
  (interactive "^P")
  (call-interactively (function pel-backward-token-start) t (vector n)))

;; ---------------------------------------------------------------------------
;; Navigate to beginning/end of line/window/buffer
;; -----------------------------------------------
;; These functions operate according to current position,
;; In a way that is somewhat similar to how CRiSP manages
;; home and end keys:
;;
;; - home: moves point to the beginning of the field/line/window/buffer
;;         depending on the current point location.
;; - end : moves point to the end of field/line/window/buffer
;;         depending on the current point location.

(defun pel--home ()
  "Move to the beginning of field/line/window/buffer.
Return number of lines scrolled."
  (if (not (bolp))
      ;; If point not at beginning of line, use (beginning-of-line) that is
      ;; constrained by fields (such as prompts in interactive buffers like
      ;; IELM).
      ;; If point did not move, it's at the beginning of a field, so move
      ;; to the real beginning of line with (forward-line 0): the forward-line
      ;; function is not constrained by fields.
      (let ((p1 (point))
            p2)
        (beginning-of-line)
        (setq p2 (point))
        (if (= p1 p2)
            (forward-line 0))
        0)
    ;; else - point is already at beginning of line: if point is not at the
    ;; top of window then move to it. Otherwise move to the top of buffer.
    (if (/= (point) (window-start))
        (progn
          (goto-char (window-start))
          0)
      (let ((start-point (point)))
        (goto-char (point-min))
        (- (count-lines start-point (point)))))))

;;-pel-autoload
(defun pel-home (&optional arg)
  "Move to beginning of field/line/window/buffer.
Move point according to current point position:
   - If field exists and point not at its beginning: move to its beginning.
   - If point not at beginning of line: move it to the beginning of the line.
   - If point not on first line of window: move to first line/column of window.
   - If at first line of window: move to first line/column of buffer.
- If the buffer is narrowed, this command uses the start of the accessible part
  of the buffer.
Before moving, push mark at previous position, unless either a
\\[universal-argument] prefix is supplied as ARG, or Transient Mark mode is
enabled and the mark is active.
If `pel-in-scroll-sync' is non-nil, the synced window is scrolled the same
number of lines.


*Warning*: Do NOT use `pel-home' in elisp programs.
           Use (`goto-char' (`point-min')) instead.
           `pel-home' is only meant to be used interactively."
  (interactive "^P")
  (unless (eq last-repeatable-command 'pel-home)
    (or (consp arg)
        (region-active-p)
        (push-mark)))
  (let ((excursion-line-count (pel--home)))
    (when pel-in-scroll-sync
      ;; pel-scroll not required at file level to prevent
      ;; loading it if no scroll operation performed: so
      ;; declare the function to prevent byte-compiler
      ;; warning.  CAUTION: make sure the function names are OK!
      (declare-function pel-scroll-down-all-insync "pel-scroll"
                        (including-current &optional n))
      (pel-scroll-down-all-insync
       nil
       (abs excursion-line-count)))))

;; --

(defun pel--end ()
  "Move to the end of field/line/window/buffer.
Return number of lines scrolled."
  (if (not (eolp))
      ;; not at end of line, could be inside a field.
      (let ((p1 (point))
            p2)
        ;; try moving to end of line
        (end-of-line)
        (setq p2 (point))
        (when (= p1 p2)
          ;; point did not move: it's at the end of a field
          ;; force moving right (unconstrained by field) and check again
          (right-char)
          (unless (eolp)
            (end-of-line)))
        0)
    ;; cannot get exact value of end of window, check if within 150 chars
    (if (> (abs (- (point) (window-end))) 150)
        (progn
          (move-to-window-line -1)
          (end-of-line)
          0)
      (let ((start-point (point)))
        (goto-char (point-max))
        (count-lines start-point (point))))))

;;-pel-autoload
(defun pel-end (&optional arg)
  "Move to end of field/line/window/buffer.
Move point according to current point position:
   - If field exist and point not at end of field: move to end of field.
   - If point not at end of line: move to end of line.
   - If point not on last line of window: move to last line/column of window.
   - If point not on last line of buffer: move to last line/column of buffer.
- If the buffer is narrowed, this command uses the end of the accessible part
  of the buffer.
Before moving, push mark at previous position, unless either a
\\[universal-argument] prefix is supplied as ARG, or Transient Mark mode is
enabled and the mark is active.
If `pel-in-scroll-sync' is non-nil, the synced window is scrolled the same
number of lines.

*Warning*: Do NOT use `pel-end' in elisp programs.
           Use (`goto-char' (`point-min')) instead.
           `pel-end' is only meant to be used interactively."
  (interactive "^P")
  (unless (eq last-repeatable-command 'pel-end)
    (or (consp arg)
        (region-active-p)
        (push-mark)))
  (let ((excursion-line-count (pel--end)))
    (when pel-in-scroll-sync
      ;; pel-scroll not required at file level to prevent
      ;; loading it if no scroll operation performed: so
      ;; declare the function to prevent byte-compiler
      ;; warning.  CAUTION: make sure the function names are OK!
      (declare-function pel-scroll-up-all-insync "pel-scroll"
                        (including-current &optional n))
      (pel-scroll-up-all-insync
       nil
       (abs excursion-line-count)))))

;; ---------------------------------------------------------------------------
;; Navigate across function definitions
;; ------------------------------------

;; Debugging helpers
;; (defun pel-n() (interactive) (pel--end-of-defun))
;; (defun pel-p() (interactive) (pel--beginning-of-defun))
;; (defun pel-b() (interactive) (message "%s" (pel-beginning-of-next-defun)))

;; --
;; Mode aware navigation functions

(defun pel--beginning-of-defun (&optional arg)
  "Call major mode specific function to move to beginning of defun, with ARG.
Supports:  `c-beginning-of-defun' and `beginning-of-defun'."
  (if (and (memq major-mode '(c-mode c++-mode))
           (require 'cc-cmds nil :noerror)
           (fboundp 'c-beginning-of-defun))
      (c-beginning-of-defun arg)
    (beginning-of-defun arg)))

(defun pel--end-of-defun (&optional arg)
  "Call major mode specific function to move to end of defun, with ARG.
Supports: `c-end-of-defun' and `end-of-defun'."
  (if (and (memq major-mode '(c-mode c++-mode))
           (require 'cc-cmds nil :noerror)
           (fboundp 'c-end-of-defun))
      (c-end-of-defun arg)
    (end-of-defun arg)))

;; --

(defun pel--maybe-to-first-defun-after (start-pos)
  "Maybe move point to first defun definition located after START_POS.
Return point location; if found it is larger than START-POS."
  (let (current-pos
        tentative-final-pos)
    (while
        (progn
          (setq tentative-final-pos (point))
          (pel--beginning-of-defun)
          (setq current-pos (point))
          (and (> current-pos start-pos)
               (/= current-pos tentative-final-pos))))
    (goto-char tentative-final-pos)
    tentative-final-pos))

;;-pel-autoload
(defun pel-beginning-of-next-defun (&optional silent dont-push-mark)
  "Move forward to the beginning of the next function/method/class definition.
If not found, don\\='t move point and beep unless SILENT is non-nil.
Return non-nil if found, nil if not found.
The actual value returned when location is found provides information about
the context; when the location was found the function returns one of:
- \\='was-outside          : when point was outside any function before the call,
- \\='was-inside           : when point was inside a function before the call,
- \\='was-outside-class/m  : when point was outside but moved to new class or
                          to its first method.
When the location is found, `pel-beginning-of-next-defun' pushes the start
position on the mark ring unless DONT-PUSH-MARK argument is non-nil.

KNOWN LIMITATIONS:
- Moves to statements after last function definition in file even though
  there are not function definition (just statements or empty space).
- Fails detecting and moving to next C++ template function in some cases."
  (interactive "^")
  (let* ((start-pos (point))
         (final-pos start-pos))
    (prog1
        (save-excursion
          ;; Try simple case: move to end then back to beginning
          ;; that works when point is before a function definition.
          (pel--end-of-defun)
          (pel--beginning-of-defun)
          ;; Now check if that was the case
          (let ((current-pos (point))
                tentative-final-pos)
            (if (> current-pos start-pos)
                ;; Check if point moved into the beginning of the last method
                ;; of the next class by checking if we can go to the beginning
                ;; of a defun that is before point but after start-pos.
                (let ((potential-final-pos (pel--maybe-to-first-defun-after
                                            start-pos)))
                  (if (= potential-final-pos current-pos)
                      (progn
                        (setq final-pos current-pos)
                        'was-outside)                    ; found right away
                    (setq final-pos potential-final-pos)
                    'was-outside-class/m))

              ;; If start-point was already inside a defun/function/method
              ;; body then point will move back to the beginning of the
              ;; current defun. If that's the case we have to try again by
              ;; going to end defun twice and then go back up to the beginning
              ;; of the defun.
              ;; It's also possible that end-of-defun moves point to the end
              ;; of the last method of a class so we have to go back several
              ;; times to go back to the beginning of the very first method or
              ;; class definition that is after where we started from.
              (pel--end-of-defun)
              (end-of-defun)
              (setq tentative-final-pos (pel--maybe-to-first-defun-after
                                         start-pos))
              (if (and (/= tentative-final-pos start-pos)
                       (not (eobp)))
                  (progn
                    (setq final-pos tentative-final-pos)
                    'was-inside)                    ; found
                ;; not found
                (setq final-pos start-pos)
                (when (not silent)
                  (beep))
                nil))))
      (when (/= final-pos start-pos)
        (unless dont-push-mark
          (push-mark start-pos))
        (goto-char final-pos)))))

;; --

;;-pel-autoload
(defun pel-end-of-previous-defun (&optional silent dont-push-mark)
  "Move backward to the end of the previous function/method/class definition.

- If location is found:
  - push the start position on the mark ring unless DONT-PUSH-MARK argument
    is non-nil,
  - return t.
- If location is not found:
  - don't move point,
  - beep unless SILENT is non-nil,
  - return nil.

KNOWN LIMITATIONS:
- Does not always handle nested functions/methods properly where it
  stays in the current location."
  (interactive "^")
  (let* ((start-pos (point))
         (final-pos start-pos)
         (attempt-cnt 0)
         (searching t)
         found)
    (save-excursion
      (while (and searching (< attempt-cnt 3))
        (setq attempt-cnt (1+ attempt-cnt))
        (save-excursion
          (beginning-of-defun attempt-cnt)
          (end-of-defun)
          (let ((current-pos (point)))
            (when (< current-pos start-pos)
              (setq final-pos current-pos)
              (setq searching nil)
              (setq found t))))))
    (if found
        (progn
          (unless dont-push-mark
            (push-mark start-pos))
          (goto-char final-pos)
          t)
      (when (not silent)
        (beep))
      nil)))

;;; --------------------------------------------------------------------------
(provide 'pel-navigate)

;;; pel-navigate.el ends here

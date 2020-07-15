;;; pel--base.el --- PEL base utilities. -*-lexical-binding: t-*-

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
;; A loosely coupled collection of simple utilities used by other PEL
;; which exist simply to simplify the PEL code.
;;
;; The following is a list of available commands (*) and functions (-) listed in
;; hierarchical calling order.
;;
;; PEL version:
;; * `pel-version'
;;
;; Environment Querying functions:
;;  - `pel-used-major-mode-of'
;;  - `pel-current-buffer-filename'
;;
;; Check for Zero:
;;  - `pel-!0'
;;
;; Bitwise Operations:
;;  - `pel-all-bitset-p'
;;
;; String checks:
;;  - `pel-whitespace-in-str-p'
;;
;; Pluralizer:
;;  - `pel-count-string'
;;
;; String generation utilities:
;;  - `pel-option-mode-state'
;;  - `pel-symbol-text'
;;    - `pel-symbol-on-off-string'
;;      - `pel-on-off-string'
;;  - `pel-yes-no-string'
;;
;; Operations on sequences:
;;  - `pel-concat-strings-in-list'
;;
;; Toggle a local mode:
;;  - `pel-toggle-mode'
;;
;; Symbol processing
;;  - `pel-hook-symbol-for'
;;  - `pel-map-symbol-for'
;;
;; Basic functions working with values and variables:
;;  - `pel-toggle-and-show'
;;    - `pel-toggle'
;;  - `pel-val-or-default'
;;
;; Argument converter:
;;  - `pel-multiplier'
;;
;; Iteration helpers:
;;  - `pel-dec'
;;  - `pel-inc'
;;
;; Text at point:
;;  - `pel-at-lowercase-p'
;;  - `pel-at-uppercase-p'
;;    - `pel-at-letter-p'
;;  - `pel-chars-at-point'
;;
;; Calling functions:
;; - `pel-n-funcall-to'
;;
;;  Moving Point:
;;  - `pel-goto-position'
;;  - `pel-goto-line'
;;
;; Identifying region:
;; - `pel-region-for'
;;

;;; Code:

;; -----------------------------------------------------------------------------
;; Constants
;; ---------
;;
;; Note: The following symbols have nothing to do with PEL and could
;;       have a name that starts with 'system' but I'm not doing it
;;       to provide name space isolation in case Emacs declares these
;;       in the future.

(defconst pel-system-is-macos-p
  (eq system-type 'darwin)
  "Predicate: t if running under a macOS Operating System, nil otherwise.")

(defconst pel-system-is-windows-p
  (memq system-type '(windows-nt ms-dos))
  "Predicate: t if running under a Windows Operating System, nil otherwise.")

(defconst pel-can-display-special-chars-p
  (and (eq system-type 'darwin)
       (not (display-graphic-p)))
  "Predicate: t if Emacs can properly show Unicode characters like 👍 or 👎.")

;; -----------------------------------------------------------------------------
;; PEL version
;; ===========

(defun pel-version (&optional insert)
  "Display and return PEL package version string.
Optionally insert it at point if INSERT is non-nil."
  (interactive "P")
  (let ((version "0.1.1"))
    (if insert
        (insert version))
    (message "PEL version: %s" version)
    version))

;; -----------------------------------------------------------------------------
;; Environment Querying functions:
;; ------------------------------
;;
;; The following functions provide information about the Emacs environment.

(defun pel-used-major-mode-of (&optional buffer-or-name)
  "Return the major mode symbol of the specified BUFFER-OR-NAME.
If not specified (or nil) return the major mode of the current buffer."
  (if buffer-or-name
      (with-current-buffer buffer-or-name
        major-mode)
    major-mode))

(defun pel-current-buffer-filename (&optional sans-directory)
  "Return current buffer's filename string.
Return a filename with full path unless SANS-DIRECTORY is non-nil.
Return nil if current buffer does not visit a file."
  (if buffer-file-truename
      (let ((fn (expand-file-name buffer-file-truename)))
        (if sans-directory
            (file-name-nondirectory fn)
          fn))
    (user-error "No file in buffer %s" (buffer-name))))

;; -----------------------------------------------------------------------------
;; Check for Zero
;; --------------
;; In Lisp, nil is the only 'false' value.  Even 0 is an equivalent to 'true'.
;; The following inline help checking for a zero-value result.
;; If I find something similar native in Emacs I'll use and remove this one.
(defsubst pel-!0 (v)
  "Return nil if 0, t otherwise."
  (not (equal v 0)))

;; -----------------------------------------------------------------------------
;; Bitwise Operations
;; ------------------
(defun pel-all-bitset-p (value &rest bits)
  "Return t when all and only those BITS are set in VALUE, nil otherwise."
  (let ((bitmask 0))
    (dolist (bit bits bitmask)
      (setq bitmask (logior bitmask bit)))
    (equal 0 (logxor value bitmask))))

;; -----------------------------------------------------------------------------
;; String checks
;; -------------

(defun pel-whitespace-in-str-p (text)
  "Return non-nil if any whitespace character is inside TEXT, nil otherwise.
The index of the first whitespace character is returned when one is present."
  (string-match "[ \t\n\r]" text))

;; -----------------------------------------------------------------------------
;; - Pluralizer
;; ------------

(defun pel-count-string (n singular &optional plural)
  "Return a formatted string for N in SINGULAR or PLURAL.
If N is 0 or 1, use the singular form.
If N > 2: use the PLURAL form if specified,
          otherwise use SINGULAR with a 's' suffix."
  (if (> n 1)
      (format "%d %s" n
              (or plural
                  (format "%ss" singular)))
    (format "%d %s" n singular)))

;; -----------------------------------------------------------------------------
;; String generation utilities
;; ---------------------------
;;
;; Call hierarchy:
;;  - `pel-option-mode-state'
;;  - `pel-symbol-text'
;;    - `pel-symbol-on-off-string'
;;      - `pel-on-off-string'
;;  - `pel-yes-no-string'

(defun pel-on-off-string (boolean &optional on-string off-string)
  "Return \"off\" for nil, \"on\" for non-nil BOOLEAN argument.
If ON-STRING and OFF-STRING arguments are specified use them as the
on/off value, otherwise use \"on\" and \"off\"."
  (if boolean
      (or on-string "on")
    (or off-string "off")))

(defun pel-symbol-on-off-string (symbol &optional on-string off-string void-string)
  "Return representation of symbold value and whether it is bound.
When SYMBOL is not bound: return VOID-STRING or \"void\" if it's nil,
When it is bound, return:
- the OFF-STRING or \"off\" for nil,
- the ON-STRING or \"on\" for SYMBOL boolean value."
  (if (boundp symbol)
      (pel-on-off-string (eval symbol) on-string off-string)
    (or void-string "void")))

(defun pel-symbol-text (symbol &optional on-string off-string)
  "Return a string with an interpretation of SYMBOL value.
If symbol is not bound: show \"void\".
If symbol is set to t: show ON-STRING if defined, \"t\" otherwise.
If symbol is nil: show OFF-STRING if defined, \"nil\" otherwise."
  (format "%s is now: %s"
          symbol
          (pel-symbol-on-off-string symbol on-string off-string)))

(defun pel-yes-no-string (test &optional true-string false-string)
  "Return TRUE-STRING when boolean TEST is non-nil, otherwise FALSE_STRING.
By default or when these arguments are nil:
- TRUE_STRING is \"yes\" and
- FALSE_STRING is \"no\"."
  (if test
      (or true-string "yes")
    (or false-string "no")))

(defun pel-option-mode-state (option mode)
  "Return description string for OPTION and its MODE.
OPTION is the value of the specifed option,
MODE is the mode symbol."
  (if option
      (if (boundp mode)
          (format "available and %s"
                  (pel-symbol-on-off-string mode))
        "available but not loaded, use a command to load it")
    "not available"))

;; -----------------------------------------------------------------------------
;; Operations on sequences
;; -----------------------

(defun pel-concat-strings-in-list (list)
  "Return the concatenation of all strings in the LIST of strings."
  (let ((acc "")
        elem)
    (while list
      (setq elem (car list))
      (setq list (cdr list))
      (setq acc (concat acc elem)))
    acc))

;; Note: - another way would be:  (mapconcat 'identity list "")
;;       - the advantage would be to be able to perform transformation if the
;;       - elements of the list are not string, and to inject a separator.

(defun pel-cons-alist-at (alist key val)
  "Prepend VAL to ALIST of list members at KEY.
If ALIST has no KEY, create an entry for KEY with (VAL) as KEY value.
REQUIREMENT:  ALIST must be a non-empty list.
SIDE EFFECT:  ALIST is modified destructively.
Return new value of ALIST.

Usage Example:

ELISP> (setq al '((one (\"..\" \"[..]\"))))
((one
  (\"..\" \"[..]\")))

ELISP> al
((one
  (\"..\" \"[..]\")))

ELISP> (pel-cons-alist-at al 'two '(\"aa\" \"[aa]\"))
((one
  (\"..\" \"[..]\"))
 (two
  (\"aa\" \"[aa]\")))

ELISP> al
((one
  (\"..\" \"[..]\"))
 (two
  (\"aa\" \"[aa]\")))

ELISP> (pel-cons-alist-at al 'tree '(\"AA\" \"[AA]\"))
((one
  (\"..\" \"[..]\"))
 (two
  (\"aa\" \"[aa]\"))
 (tree
  (\"AA\" \"[AA]\")))

ELISP> (pel-cons-alist-at al 'one '(\",,\" \"[,,]\"))
((one
  (\",,\" \"[,,]\")
  (\"..\" \"[..]\"))
 (two
  (\"aa\" \"[aa]\"))
 (tree
  (\"AA\" \"[AA]\")))

ELISP> (pel-cons-alist-at al 'tree '(\"BB\" \"[BB]\" and-something-else))
((one
  (\",,\" \"[,,]\")
  (\"..\" \"[..]\"))
 (two
  (\"aa\" \"[aa]\"))
 (tree
  (\"BB\" \"[BB]\" and-something-else)
  (\"AA\" \"[AA]\")))

ELISP> al
((one
  (\",,\" \"[,,]\")
  (\"..\" \"[..]\"))
 (two
  (\"aa\" \"[aa]\"))
 (tree
  (\"BB\" \"[BB]\" and-something-else)
  (\"AA\" \"[AA]\")))"
  (if (length alist)
      (let* ((value (assq key alist))
             (new-value (cons val (cdr value))))
        (if value
            (progn
              (setcdr (assq key alist) new-value)
              alist)
          (nconc alist (list (list key val)))))
    (error "pel-cons-alist-at given an empty ALIST argument!")))

;; -----------------------------------------------------------------------------
;; Toggle a local mode
;; -------------------

(defun pel-toggle-mode (mode)
  "Toggle the specified MODE (a symbol).
Return the new state of the mode: t if active, nil otherwise."
  (unless (symbolp mode)
    (error "Nothing done: pel-toggle-mode expects a symbol as argument"))
  (funcall (symbol-function mode) (if (symbol-value mode) -1 1)))

;; -----------------------------------------------------------------------------
;; Symbol processing
;; -----------------

(defun pel-hook-symbol-for (mode)
  "Return the hook symbol for the specified MODE symbol."
  (intern (format "%s-hook" (symbol-name mode))))


(defun pel-map-symbol-for (mode)
  "Return the map symbol for the specified MODE symbol."
    (intern (format "%s-map" (symbol-name mode))))

;; -----------------------------------------------------------------------------
;; Basic functions working with values and variables
;; -------------------------------------------------
;;
;; To toggle the value of variable that would have
;; the hypothetical name is-acceptable we can use
;; the following calls:
;;                     (pel-toggle 'is-acceptable)
;;                     (pel-toggle-and-show 'is-acceptable)
;;
;; Notice the required quoting.

(defun pel-toggle (symbol)
  "Toggle value of SYMBOL from nil to/from t. Return SYMBOL's new value.
For example, to toggle the value of a variable  named isok,
the caller must pass it quoted.
Return the new SYMBOL value.
The function issue an error if the argument is not a symbol."
  (if (symbolp symbol)
      (set symbol (not (eval symbol)))
    (error "Nothing done: pel-toggle expects a symbol as argument")))

(defun pel-toggle-and-show (symbol &optional on-string off-string)
  "Toggle value of SYMBOL from nil to/from t, and show it's new value.
If ON-STRING and OFF-STRING arguments are specified use them as the
on/off value, otherwise use \"on\" and \"off\".
For example, to toggle the value of a variable  named isok,
the caller must pass it quoted.
The function issue an error if the argument is not a symbol."
  (message "%s is now %s"
           symbol
           (pel-on-off-string
            (pel-toggle symbol) on-string off-string)))

(defun pel-val-or-default (val default)
  "Return VAL if not nil otherwise return DEFAULT."
  (or val default))

;; -----------------------------------------------------------------------------
;; Argument converter
;; ------------------

(defun pel-multiplier (positive)
  "Return a positive value 1 if POSITIVE is non-nill, -1 otherwise."
  (if positive 1 -1))

(defalias 'pel-mode-toggle-arg 'pel-multiplier
  "Convert a boolean value to the value required by mode toggling functions.")

;; -----------------------------------------------------------------------------
;; Iteration helpers
;; -----------------
;;
;; The following 2 functions can be used as the while loop test form, both to
;; decrement or increment a loop variable and to control termination of the
;; loop.
;;
;; For example, the following code snippet inserts 10 lines of text, identifying
;; line 1 to line 10:
;;
;; (let ((cnt 0))
;;   (while (pel-inc 'cnt 10)
;;     (insert (format "\nLine %d" cnt))))
;;
;; Note that these functions take a symbol, allowing in place increment or
;; decrement.

(defun pel-dec (n &optional step floor)
  "Decrement symbol N by STEP (defaults to 1) down to FLOOR if specified.
N can be a symbol.  In that case it's value is updated.
If FLOOR is specified that's the smallest allowed value for N.
If N is a symbol, return nil if it cannot be decremented, otherwise
return the new value.
If N is a value, always return the new N value, even if it cannot be
decremented and then returns the same value N."
  (let* ((is-symbol  (symbolp n))
         (n-val      (if is-symbol (eval n) n))
         (step       (or step 1))
         (too-small  (and floor (< n-val (+ floor step))))
         (new-val    (if too-small n-val
                       (- n-val step)))
         (retval     (if (and too-small is-symbol)
                         nil
                       new-val)))
    (if is-symbol
        (set n new-val))
    retval))


(defun pel-inc (n &optional ceiling)
  "Increment symbol N up to CEILING.
Return N when it's smaller than CEILING.
Return nil if symbol N value is CEILING or larger."
  (let ((oldvalue (eval n)))
    (if (< oldvalue (or ceiling most-positive-fixnum))
        (set n (1+ oldvalue)))))

;; -----------------------------------------------------------------------------
;; Text at point
;; -------------
;;
;; The following functions extract string information from the text at point.
;; The hierarchy of these function is show here:
;;
;; - `pel-at-lowercase-p'
;; - `pel-at-uppercase-p'
;;   - `pel-at-letter-p'
;; - `pel-chars-at-point'
;;

(defun pel-chars-at-point (n)
  "Return the string of N characters at point."
  (save-excursion
    (let ((str ""))
      (while (and (> n 0) (not (eobp)))
        (setq str (concat str (char-to-string (char-after))))
        (setq n (1- n))
        (right-char))
      str)))

(defun pel-at-letter-p ()
  "Return non-nil if point is located over a letter character, nil otherwise.
Letters include all characters considered as letters by [[:alpha:]]"
  (looking-at-p "[[:alpha:]]"))

(defun pel-at-lowercase-p (&optional exact pos byword backward)
  "Return non-nil if point is located at lowercase letter, nil otherwise.
By default, the function checks the case of the first letter found,
starting to look at point and then forward until one letter character
is found.  However,
- if EXACT is non-nil, the function only checks the exact current
  point position,
- if POS is non-nil, it starts checking from that position instead,
- if BYWORD is non-nil, ignore BACKWARD and check the case of the first
  letter of the word offset by the number BYWORD: back BYWORD word(s)
  if BYWORD is negative, forward BYWORD word(s) if BYWORD is positive.
  If BYWORD is 0, do not move point.
- if BACKWARD is non-nil, it checks moving by character backward instead."
  (let ((case-fold-search nil)
        (step (if backward -1 1)))
    (save-excursion
      (unless exact
        (when pos
          (goto-char pos))
        (when byword
          (setq step 1)
          (forward-word byword))
        (while (not (pel-at-letter-p))
          (right-char step)))
      (looking-at-p "[[:lower:]]"))))

(defun pel-at-uppercase-p (&optional n exact backward pos)
  "Return non-nil if point is located at uppercase letter, nil otherwise.
If N is specified, return non-nil if point is located at the beginning
of a sequence of N uppercase letters.
By default, the function starts checking the case of the first letter found,
looking at point and then forward until one letter character is found.
However, if EXACT is non-nil, the function only checks from the exact
current point position.
The function checks the characters to the right of point unless
the BACKWARD argument is non-nil.
By default, `pel-at-lowercase-p' checks the case of the characters
starting from point.  if POS is non-nil, it starts from that position
instead."
  (or n (setq n 1))
  (let ((case-fold-search nil)
        (all-upper t)
        (step (if backward -1 1)))
    (save-excursion
      (unless exact
        (if pos
            (goto-char pos))
        (while (not (pel-at-letter-p))
          (right-char step)))
      (while (and all-upper (> n 0))
        (setq n (1- n))
        (unless (looking-at-p "[[:upper:]]")
          (setq all-upper nil))
        (right-char step)))
    all-upper))

;; -----------------------------------------------------------------------------
;; Calling functions
;; -----------------

(defun pel-n-funcall-to (n pos-fct neg-fct)
  "Call one of arity-0 functions (abs N) times.
If N >= 0: call POS-FCT N times.
If N <  0: call NEG-FCT (abs N) times.
Return nil."
  (if (>= n 0)
      (dotimes (_i n)
        (funcall pos-fct))
    (dotimes (_i (abs n))
      (funcall neg-fct))))

;; -----------------------------------------------------------------------------
;; Moving Point
;; ------------
;;
;; - `pel-goto-position'
;;   - `pel-goto-line'
;;

(defun pel-goto-line (line)
  "Move point to the beginning of LINE, an integer 1 or larger."
  (unless (> line 0)
    (error "Specified line is too small: %d" line))
  (goto-char 1)
  (forward-line (1- line)))

(defun pel-goto-position (line &optional column)
  "Move point to specified LINE, COLUMN.  Any can be nil."
  (progn
    (if line
        (pel-goto-line line))
    (if column
        (move-to-column column))))

;; -----------------------------------------------------------------------------
;; Identifying region:
;; - `pel-region-for'
;;

(defun pel-region-for (start-str end-str &optional pos)
  "Return the position of the beginning of delimited region.
The delimiters are START-STR and END-STR.
Search at POS if specified, otherwise search around point.
Include whole lines.
Return a (start . end) cons cell if found, otherwise return nil."
  (setq pos (or pos (point)))
  (save-excursion
    (let (beg end)
      (when (search-backward start-str nil :noerror)
        (beginning-of-line 1)
        (setq beg (point))
        (when (search-forward end-str nil :noerror)
          (end-of-line 1)
          (setq end (point))
          (cons beg end))))))


;; -----------------------------------------------------------------------------
(provide 'pel--base)

;;; pel--base.el ends here

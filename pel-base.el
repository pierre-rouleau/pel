;;; pel-base.el --- PEL package basic utility functions.

;; Copyright (C) 2020  Pierre Rouleau

;; Author: Pierre Rouleau <prouleau.swd@gmail.com>

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

;;; Code:

;; Utility macros
;; --------------

(defmacro while-n (count cond &rest body)
  "Bounded while: execute BODY a maximum of COUNT times, while COND is true."
  (let ((tmpvar (make-symbol "i")))
    `(let ((,tmpvar ,count))
       (while (and (> ,tmpvar 0) ,cond)
         ,@body
         (decf ,tmpvar)))))

;; -----------------------------------------------------------------------------
;; Environment Querying function
;; -----------------------------
;;
;; The following functions provide information about the Emacs environment.

(defun pel-running-under-windows-p ()
  "Return non-nil if running under DOS or Windows, nil otherwise."
  (memq system-type '(windows-nt ms-dos)))

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
If N > 2: use the PLURAL form if specified, otherwise use SINGULAR with a 's' suffix."
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
;;  - pel-symbol-on-off-string  (inline)
;;    - pel-on-off-string       (inline)
;;  - pel-yes-no-string

(defsubst pel-on-off-string (boolean &optional on-string off-string)
  "Return \"off\" for nil, \"on\" for non-nil BOOLEAN argument.
If ON-STRING and OFF-STRING arguments are specified use them as the
on/off value, otherwise use \"on\" and \"off\"."
  (if boolean
	  (or on-string "on")
	(or off-string "off")))

(defsubst pel-symbol-on-off-string (symbol &optional on-string off-string)
  "Return representation of symbold value and whether it is bound.
Return \"void\" when SYMBOL is not bound,
the OFF-STRING or \"off\" for nil,
the ON-STRING or \"on\" for SYMBOL boolean value."
  (if (boundp symbol)
      (pel-on-off-string (eval symbol) on-string off-string)
    "void"))

(defun pel-yes-no-string (test &optional true-string false-string)
  "Return TRUE-STRING when boolean TEST is non-nil, otherwise FALSE_STRING.
By default or when they are nil, TRUE_STRING is \"yes\" and FALSE_STRING is \"no\"."
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
  (if val
      val
    default))

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
;;   (while (pel-bounded++ 'cnt 10)
;;     (insert (format "\nLine %d" cnt))))
;;
;; Note that these functions take a symbol, allowing in place increment or
;; decrement.

(defun pel-bounded-- (n &optional floor)
  "Decrement symbol N down to FLOOR (0 by default).
Return N when it's larger than FLOOR.
Return nil if symbol N value is FLOOR or smaller."
  (let ((oldvalue (eval n)))
    (if (> oldvalue (or floor 0))
        (set n (1- oldvalue)))))

(defun pel-bounded++ (n ceiling)
  "Increment symbol N up to CEILING.
Return N when it's smaller than CEILING.
Return nil if symbol N value is CEILING or larger."
  (let ((oldvalue (eval n)))
    (if (< oldvalue ceiling)
        (set n (1+ oldvalue)))))

;; -----------------------------------------------------------------------------
;; Text at point
;; -------------
;;
;; The following functions extract string information from the text at point.
;; The hierarchy of these function is show here:
;;
;; - pel-at-lowercase-p
;; - pel-at-uppercase-p
;;   - pel-at-letter-p
;; - pel-chars-at-point
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

(defun pel-at-lowercase-p (&optional exact backward pos)
  "Return non-nil if point is located at lowercase letter, nil otherwise.
By default, the function checks the case of the first letter found,
looking at point and then forward until one letter character is found.
However, if EXACT is non-nil, the function only checks the exact current
point position.
The function checks the characters to the right of point unless
the BACKWARD argument is non-nil.
By default, `pel-at-lowercase-p' checks the case of the characters
starting from point.  if POS is non-nil, it starts from that position
instead."
  (let ((case-fold-search nil)
        (step (if backward -1 1)))
    (save-excursion
      (unless exact
        (if pos
            (goto-char pos))
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

(defun pel--n-funcall-to (n pos-fct neg-fct)
  "Call one of arity-0 functions (abs N) times.
If N >= 0: call POS-FCT N times.
If N <  0: call NEG-FCT (abs N) times.
Return nil."
  (if (>= n 0)
      (dotimes (i n)
        (funcall pos-fct))
    (dotimes (i (abs n))
      (funcall neg-fct))))

;; -----------------------------------------------------------------------------
;; Moving Point
;; ------------
;;
;; - pel-goto-position
;;   - pel-goto-line
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
(provide 'pel-base)

;;; pel-base.el ends here

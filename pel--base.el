;;; pel--base.el --- PEL base utilities. -*-lexical-binding: t-*-

;; Copyright (C) 2020, 2021  Pierre Rouleau

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

;; ---------------------------------------------------------------------------
;;; Commentary:
;;
;; A loosely coupled collection of simple utilities used by other PEL
;; which exist simply to simplify the PEL code.
;;
;; The following is a list of available commands (*) and functions (-) listed
;; in hierarchical calling order.
;;
;; PEL version:
;; * `pel-version'
;;
;; Environment Querying functions:
;;  - `pel-used-major-mode-of'
;;  - `pel-current-buffer-filename'
;;  - `pel-current-buffer-file-extension'
;;
;; Emacs Lisp Development support:
;; - `pel-add-dir-to-loadpath'
;;
;; Check for Zero:
;;  - `pel-!0'
;;
;; Bitwise Operations:
;;  - `pel-all-bitset-p'
;;
;; String checks:
;;  - `pel-whitespace-in-str-p'
;;  - `pel-ends-with-space-p'
;;  - `pel-starts-with-space-p'
;;
;; Pluralizer:
;;  - `pel-count-string'
;;
;; String generation utilities:
;;  - `pel-option-mode-state'
;;    - `pel-activated-in-str'
;;  - `pel-symbol-value-or'
;;  - `pel-symbol-text'
;;    - `pel-symbol-on-off-string'
;;      - `pel-on-off-string'
;;  - `pel-yes-no-string'
;;
;; String transformation utilities:
;; - `pel-capitalize-first-letter'
;; - `pel-end-text-with-period'
;; - `pel-hastext'
;; - `pel-when-text-in'
;; - `pel-string-spread'
;; - `pel-list-str'
;; - `pel-title-case-to-dash-separated'
;; - `pel-grp-regex'

;; Value check:
;; - `pel-use-or'
;;
;; Operations on sequences:
;;  - `pel-concat-strings-in-list'
;;
;; Lazy loading:
;; - `pel-require'
;;
;; Toggle a local mode:
;;  - `pel-toggle-mode'
;;
;; Symbol processing
;;  - `pel-hook-symbol-for'
;;  - `pel-map-symbol-for'
;;
;; Hook control
;;  - `pel-add-hook-for'
;;
;; Basic functions working with values and variables:
;;  - `pel-toggle-and-show-user-option'
;;    - `pel-toggle-and-show'
;;      - `pel-toggle'
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
;; Insert or overwrite text
;; - `pel-insert-or-overwrite'
;;
;; Extract text from buffer
;; - `pel-text-from-beginning-of-line'
;;
;; Check text from buffer
;; - `pel-line-has-only-whitespace-p'
;;
;; File Path processing
;; - `pel-parent-dirpath'
;; - `pel-expand-url-file-name'
;;
;; Print in dedicated buffer
;; - `pel-print-in-buffer'
;;
;; Code parsing support
;; - `pel-point-in-comment-or-docstring'
;;
;; Tab width control
;; - `pel-set-tab-width'

;; ---------------------------------------------------------------------------
;;; Dependencies:
                ;; subr (always loaded) ; use: called-interactively-p
(eval-when-compile (require 'subr-x))   ; use: split-string, string-join

;; ---------------------------------------------------------------------------
;;; Code:

;; ---------------------------------------------------------------------------
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

(defconst pel-system-is-linux-p
  (eq system-type 'gnu/linux)
  "Predicate: t if running under a Linux Operating System, nil otherwise.")

(defconst pel-system-is-windows-p
  (memq system-type '(windows-nt ms-dos))
  "Predicate: t if running under a Windows Operating System, nil otherwise.")

(defconst pel-can-display-special-chars-p
  (and (eq system-type 'darwin)
       (not (display-graphic-p)))
  "Predicate: t if Emacs can properly show Unicode characters like 👍 or 👎.")
;; TODO: add ability to install unicode fonts and take it into account.

;; ---------------------------------------------------------------------------
;; PEL version
;; ===========

(defun pel-version (&optional insert)
  "Display and return PEL package version string.
Optionally insert it at point if INSERT is non-nil."
  (interactive "P")
  (let ((version "0.3.1"))
    (if insert
        (insert version))
    (message "PEL version: %s" version)
    version))

;; ---------------------------------------------------------------------------
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

(defun pel-current-buffer-filename
    (&optional sans-directory sans-extension no-error)
  "Return current buffer's filename string.
Return a filename with full path unless SANS-DIRECTORY is non-nil.
If SANS-EXTENSION is non-nil exclude the extension, otherwise include it.
Issue a user error if current buffer does not visit a file, unless the
optional NO-ERROR argument is non-nil, in which case it returns nil."
  (if buffer-file-truename
      (let ((fn (expand-file-name buffer-file-truename)))
        (when sans-extension
            (setq fn (file-name-sans-extension fn)))
        (if sans-directory
            (file-name-nondirectory fn)
          fn))
    (if no-error
        nil
      (user-error "Buffer %s is not visiting a file!" (buffer-name)))))

(defun pel-current-buffer-file-extension (&optional with-period)
  "Return the extension of the current buffer's file.

By default, the returned value excludes the period that starts
the extension, but if the optional argument WITH-PERIOD is
non-nil, the period is included in the value and in that case, if
FILENAME has no extension the returned value is \"\".

See the function `file-name-extension' for details on how this
treats files with no extension or file names that ends with a
period.  Issue a user error if current buffer does not visit a
file."
  (if buffer-file-truename
      (file-name-extension buffer-file-truename with-period)
    (user-error "No file in buffer %s" (buffer-name))))

;; ---------------------------------------------------------------------------
;; Emacs Lisp Development Support
;; ------------------------------

(defun pel-add-dir-to-loadpath (dir)
  "Add a directory to Emacs variable `load-path' if not already in the list.
Interactively display the number of directories in the list and whether
the operation succeeded or not.
Return non-nil if it was added, nil otherwise."
  (interactive "DDir: ")
  (let* ((original-length (length load-path))
         (new-dir         (directory-file-name (expand-file-name dir)))
         (new-length      (length (add-to-list 'load-path new-dir))))
    (when (called-interactively-p 'interactive)
      (message "load-path: %d directories.  %s was %s"
               new-length
               new-dir
               (if (= new-length original-length)
                   " already in the list, nothing new added!"
                 "added.")))
    (= new-length original-length)))

;; ---------------------------------------------------------------------------
;; Check for Zero
;; --------------
;; In Lisp, nil is the only 'false' value.  Even 0 is an equivalent to 'true'.
;; The following inline help checking for a zero-value result.
;; If I find something similar native in Emacs I'll use and remove this one.
(defsubst pel-!0 (v)
  "Return nil if V is 0, t otherwise."
  (not (equal v 0)))

;; ---------------------------------------------------------------------------
;; Bitwise Operations
;; ------------------
(defun pel-all-bitset-p (value &rest bits)
  "Return t when all and only those BITS are set in VALUE, nil otherwise."
  (let ((bitmask 0))
    (dolist (bit bits bitmask)
      (setq bitmask (logior bitmask bit)))
    (equal 0 (logxor value bitmask))))

;; ---------------------------------------------------------------------------
;; String checks
;; -------------

(defun pel-whitespace-in-str-p (text)
  "Return non-nil if any whitespace character is inside TEXT, nil otherwise.
The index of the first whitespace character is returned when one is present."
  (string-match "[ \t\n\r]" text))

(defun pel-ends-with-space-p (text)
  "Return t if TEXT ends with a space character, nil otherwise."
  (let ((len (length text)))
    (when (> len 0)
      (string= (substring text (- len 1) len) " "))))

(defun pel-starts-with-space-p (text)
  "Return t if TEXT has space character(s) at beginning, nil otherwise."
  (when (> (length text) 0)
    (string= (substring text 0 1) " ")))

(defun pel-string-starts-with (text prefix &optional ignore-case)
  "Return t if TEXT string does start with PREFIX string, nil otherwise.
Ignore case differences if IGNORE-CASE is non-nil."
  (eq t (compare-strings prefix nil nil
                         text nil (length prefix)
                         ignore-case)))

;; ---------------------------------------------------------------------------
;; - Pluralizer
;; ------------

(defun pel-count-string (n singular &optional plural)
  "Return a formatted string for N in SINGULAR or PLURAL.
If N is 0 or 1, use the singular form.
If N > 2: use the PLURAL form if specified,
          otherwise use SINGULAR with a 's' suffix."
  (if (> n 1)
      (format "%d %s" n (or plural
                            (format "%ss" singular)))
    (format "%d %s" n singular)))

;; ---------------------------------------------------------------------------
;; String generation utilities
;; ---------------------------
;;
;; Call hierarchy:
;;  - `pel-option-mode-state'
;;    - `pel-activated-in-str'
;;  - `pel-symbol-value-or'
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
  "Return representation of SYMBOL value and whether it is bound.
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

(defun pel-symbol-value-or (symbol &optional replacement formatter)
  "Return SYMBOL value if non void, otherwise its REPLACEMENT.

If SYMBOL is void and there is no REPLACEMENT return a string
created by (format \"unknown - %S is not loaded\" symbol).
If SYMBOL is void and replacement is :nil-for-void, return nil.
If SYMBOL is bound and FORMATTER is non nil it's a function that
takes the symbol and returns a string."
  (if (boundp symbol)
      (if formatter
          (funcall formatter symbol)
        (symbol-value symbol))
    (if replacement
        (if (eq replacement :nil-for-void)
            nil
          replacement)
      (format "unknown - %S is not loaded" symbol))))

(defun pel-yes-no-string (test &optional true-string false-string)
  "Return TRUE-STRING when boolean TEST is non-nil, otherwise FALSE_STRING.
By default or when these arguments are nil:
- TRUE_STRING is \"yes\" and
- FALSE_STRING is \"no\"."
  (if test
      (or true-string "yes")
    (or false-string "no")))

(defun pel-activated-in-str (activated-in)
  "Return a string describing ACTIVATED-IN list.
Return an empty string if ACTIVATED-IN is nil.
Otherwise return a string start starts with \" Auto-loaded in: \"
followed by the elements of ACTIVATED-IN separated by commas."
  (if activated-in
      (format " Auto-loaded in: %s"
              (pel-list-str activated-in))
    ""))

(defun pel-option-mode-state (mode user-option &optional activated-in)
  "Return description of MODE status controlled by USER_OPTION.
USER-OPTION is a symbol.  A non-nil value of that symbol
identifies whether the mode is made available, nil that it is not made
available and most probably not loaded.
MODE is the mode symbol, indicating whether the mode is active or not.
If ACTIVATED-IN is specified that's the list of major modes where MODE
is automatically activated; this is included in the description."
  (let ((autoloaded-str (pel-activated-in-str activated-in)))
  (if (boundp user-option)
      (if (eval user-option)
          (if (boundp mode)
              (format "Available %s.%s"
                      (pel-symbol-on-off-string mode
                                                "and on"
                                                "but off")
                      autoloaded-str)
            (format "Available but not loaded, use a command to load it.%s"
                    autoloaded-str))
        (format "Not available. Activate %s first.%s"
                (symbol-name user-option)
                autoloaded-str))
    (format "%s symbol unknown" (symbol-name user-option)))))

;; ---------------------------------------------------------------------------
;; String transformation utilities:
;; - `pel-capitalize-first-letter'
;; - `pel-end-text-with-period'
;; - `pel-hastext'
;; - `pel-when-text-in'
;; - `pel-string-spread'
;; - `pel-list-str'

(defun pel-capitalize-first-letter (text)
  "Return TEXT with first character up-cased, all other unchanged.
Return empty string if no input string."
  (if (> (length text) 0)
      (concat (upcase (substring text 0 1)) (substring text 1))
    ""))

(defun pel-end-text-with-period (text)
  "Append a period character to TEXT if none is present.
Return empty string if TEXT is the empty string."
  (if (> (length text) 0)
    (if (string= (substring text -1) ".")
        text
      (concat text "."))
    ""))

(defun pel-hastext (string)
  "Return t if STRING hold text, nil otherwise."
  (not (string= string "")))

(defun pel-when-text-in (string value)
  "Return VALUE if STRING is a non-empty string.
Otherwise return nil."
  (unless (string= string "")
    value))

(defun pel-string-or-nil (string)
  "Return a non-empty STRING unchanged, nil if string is empty."
  (if (string= string "")
      nil
    string))

(defun pel-string-spread (string &optional separator)
  "Return STRING with characters separated by SEPARATOR.
SEPARATOR is a string and defaults to a single space.

Example:

    ELISP> (pel-string-spread \"abcdef\")
    \"a b c d e f\"
    ELISP> (pel-string-spread \"abcdef\" \".\")
    \"a.b.c.d.e.f\"
    ELISP> (pel-string-spread \"abcdef\" \"--\")
    \"a--b--c--d--e--f\"
    ELISP>"
  (string-join (cdr (butlast (split-string string "")))
               (or separator " ")))

(defun pel-list-str (list)
  "Return a string representation of a LIST using comma separator."
  (string-join (mapcar (function symbol-name)
                       list)
               ", "))

(defun pel-title-case-to-dash-separated (text)
  "Return dash-separated lowercase for space-separated title-case words TEXT.

Use this function to transform the symbol presentation in Customize buffers
to the real name of Emacs Lisp user option variables.

For example:

ELISP> (pel-title-case-to-dash-separated \"Pdb Track Stack From Shell P\")
\"pdb-track-stack-from-shell-p\"
ELISP> (pel-title-case-to-dash-separated \"Py  Execute Use Temp File P\")
\"py--execute-use-temp-file-p\"
ELISP> "
  (string-join
   (mapcar (function downcase) (split-string text " "))
   "-"))


(defun pel-grp-regex (text &optional tail)
  "Return string with TEXT inside regexp group.

For STR, the returned string is \"\\\\(STR\\\\)\",
unless TAIL is specified, in which case tail is appended
after the closing parenthesis."
  (let ((str (format "\\(%s\\)" text)))
    (if tail
        (concat str tail)
      str)))

;; ---------------------------------------------------------------------------
;; Value check
;; -----------

(defun pel-use-or (value check-function alternative &rest transform-functions)
  "Return VALUE if (CHECK-FUNCTION VALUE) is non-nil, else return ALTERNATIVE.
If there are any TRANSFORM-FUNCTIONS return a transformed VALUE by calling the
first function with VALUE as argument and then the second, etc...
For example, if t1, t2 and t3 are specified , the returned value is the result
of the following call sequence:

\(t1 (t2 (t3 VALUE))

Example:

    ELISP> (pel-use-or \"abc\" (function pel-hastext) \"empty!\")
    \"abc\"
    ELISP> (pel-use-or \"\" (function pel-hastext) \"empty!\")
    \"empty!\"
    ELISP>

And with transformation functions:

    ELISP> (pel-use-or \"abc\" (function pel-hastext) 0
                       (function pel-capitalize-first-letter)
                       (function pel-end-text-with-period))
    \"Abc.\"
    ELISP> (pel-use-or \"\" (function pel-hastext) 0
                       (function pel-capitalize-first-letter)
                       (function pel-end-text-with-period))
    0 (#o0, #x0, ?\C-@)
    ELISP>"
  (if (funcall check-function value)
      (dolist (transform-fun transform-functions value)
        (setq value (funcall transform-fun value)))
    alternative))

;; ---------------------------------------------------------------------------
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
    (error "Call to pel-cons-alist-at given an empty ALIST argument!")))

;; ---------------------------------------------------------------------------
;; Lazy loading:
;; - `pel-require'

(defun pel-require (feature &optional package)
  "Load FEATURE if not already loaded, optionally try to install PACKAGE.

FEATURE is a symbol.

If optional PACKAGE is non-nil is specified and FEATURE is not loaded,
try to install the specified package if it is not already available
and try checking for the presence of FEATURE again, with the same behaviour.

The specified package is specified by the PACKAGE argument. It can be either:

- The special symbol `:install-when-missing' to indicate that the package to
  install has the same name as the FEATURE.
- Another symbol that identifies the name of the required package.

Issue a user-error on failure.
Otherwise return the loading state of the FEATURE."
  (unless (featurep feature)
    (let ((feature-is-loaded (require feature nil :noerror)))
      (unless feature-is-loaded
        ;; required failed - if package specified try installing it
        ;; when not already present
        (if (and package
                 (fboundp 'package-installed-p)
                 (fboundp 'package-install))
            (let ((package (if (eq package :install-when-missing)
                               feature
                             package)))
              (unless (package-installed-p package)
                (package-install package)
                (require feature nil :noerror)
                (unless (featurep feature)
                  (user-error
                   "Failed loading %s even after installing package %s!"
                   feature package))))
          ;; no package specified, required failed
          (user-error "Failed loading %s. %s!"
                      feature
                      (if package
                          "Cannot load package.el"
                        "No specified package"))))))
  (featurep feature))

;; ---------------------------------------------------------------------------
;; Toggle a local mode
;; -------------------

(defun pel-toggle-mode (mode)
  "Toggle the specified MODE (a symbol).
Return the new state of the mode: t if active, nil otherwise."
  (unless (symbolp mode)
    (error "Nothing done: pel-toggle-mode expects a symbol as argument"))
  (funcall (symbol-function mode) (if (symbol-value mode) -1 1)))

;; ---------------------------------------------------------------------------
;; Symbol processing
;; -----------------

(defun pel-hook-symbol-for (mode)
  "Return the hook symbol for the specified MODE symbol."
  (intern (format "%s-hook" (symbol-name mode))))


(defun pel-map-symbol-for (mode)
  "Return the map symbol for the specified MODE symbol."
    (intern (format "%s-map" (symbol-name mode))))

;; ---------------------------------------------------------------------------
;; Hook control
;; ------------

(defun pel-add-hook-for (modes-list-symbol func)
  "Add the FUNC hook to all modes listed in the MODES-LIST-SYMBOL."
    (dolist (mode (eval modes-list-symbol))
      (if (and mode              ; make sure the mode is a valid symbol
               (symbolp mode))   ; TODO: find better ways to detect major mode
          (add-hook (pel-hook-symbol-for mode)
                    func)
        (display-warning
         'pel-mode-hooks
         (format "Invalid mode %s in the list %s.\n\
Change its customized value with ``M-x customize %s``"
                 mode
                 modes-list-symbol
                 modes-list-symbol)
         :error))))

;;----------------------------------------------------------------------------
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
  (pel-toggle symbol)
  (message (pel-symbol-text symbol on-string off-string)))


(defun pel-toggle-and-show-user-option (user-option &optional globally)
  "Toggle the behaviour of USER-OPTION for current buffer or GLOBALLY.
Display the new state.
USER-OPTION must be a variable symbol."
  (unless globally
    (with-current-buffer (current-buffer)
      (unless (local-variable-p user-option)
        (make-local-variable user-option))))
    (pel-toggle-and-show user-option))

(defun pel-val-or-default (val default)
  "Return VAL if not nil otherwise return DEFAULT."
  (or val default))

;; ---------------------------------------------------------------------------
;; Argument converter
;; ------------------

(defun pel-multiplier (positive)
  "Return a positive value 1 if POSITIVE is non-nill, -1 otherwise."
  (if positive 1 -1))

(defalias 'pel-mode-toggle-arg 'pel-multiplier
  "Convert a boolean value to the value required by mode toggling functions.")

;; ---------------------------------------------------------------------------
;; Iteration helpers
;; -----------------
;;
;; The following 2 functions can be used as the while loop test form, both to
;; decrement or increment a loop variable and to control termination of the
;; loop.
;;
;; For example, the following code snippet inserts 10 lines of text,
;; identifying line 1 to line 10:
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

;; ---------------------------------------------------------------------------
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

;; ---------------------------------------------------------------------------
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

;; ---------------------------------------------------------------------------
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

;; ---------------------------------------------------------------------------
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

;; ---------------------------------------------------------------------------
;; Insert or overwrite text
;; - `pel-insert-or-overwrite'
;;

(defun pel-insert-or-overwrite (text)
  "Insert or overwrite TEXT depending of variable `overwrite-mode' status.
TEST can be a single character or a string.
Multi-byte characters are handled properly."
  (when overwrite-mode
    (if (stringp text)
        (delete-char (length text))
      (delete-char 1)))
  (insert text))

;; ---------------------------------------------------------------------------
;; Extract text from buffer
;; - `pel-text-from-beginning-of-line'

(defun pel-text-from-beginning-of-line (&optional with-properties)
  "Return text string between beginning of line and point.
If WITH-PROPERTIES is non-nil the returned value includes the text properties,
otherwise it does not."
  (let ((begin (line-beginning-position))
        (end   (point)))
    (if with-properties
        (buffer-substring begin end)
      (buffer-substring-no-properties begin end))))


;; ---------------------------------------------------------------------------
;; Check text from buffer
;; - `pel-line-has-only-whitespace-p'

(defun pel-line-has-only-whitespace-p (&optional pos)
  "Return non-nil if current line (or line at POS) contain only whitespace.
Return nil otherwise.
Whitespace characters are specified by the syntax table of the
current major mode."
  (save-excursion
    (goto-char (or pos (point)))
    (beginning-of-line)
    (= (progn
         (skip-syntax-forward " ")
         (point))
       (point-at-eol))))

;; ---------------------------------------------------------------------------
;; File Path processing
;; --------------------

(defsubst pel-parent-dirpath (pathname)
  "Return parent directory of PATHNAME.

For example:

        ELISP> (pel-parent-dirpath \"/usr/local/Cellar/emacs/26.3/\")
        \"/usr/local/Cellar/emacs/\"
        ELISP> (pel-parent-dirpath \"/usr/local/Cellar/emacs/26.3\")
        \"/usr/local/Cellar/emacs/\"
        ELISP>

which shows that the presence of a trailing slash has no impact."
  (file-name-directory (directory-file-name pathname)))


(defun pel-expand-url-file-name (url)
  "Expand and return file URL.
Return other URL untouched.

Example:

   ELISP> (pel-expand-url-file-name \"file://~/docs/HyperSpec/\")
   \"file:///Users/roup/docs/HyperSpec/\"
   ELISP> (pel-expand-url-file-name \"http://www.lispworks.com\")
   \"http://www.lispworks.com\"
   ELISP>"
  (if (eq 0 (string-match "file://" url))
      (concat "file://" (expand-file-name (substring url 7)))
    url))

;; ---------------------------------------------------------------------------
;; Print in dedicated buffer
;; -------------------------

(defun pel-print-in-buffer (bufname title text)
  "Print TITLE than TEXT inside specified buffer BUFNAME."
  (let ((current-buffer-name (buffer-name))
        (outbuf (get-buffer-create bufname)))
    (with-current-buffer outbuf
      (goto-char (point-max))
      (insert (format "----- %s from %s:\n%s\n\n"
                      title
                      current-buffer-name
                      text)))
    ;; display the end part of the buffer showing comment variables
    ;; move the last line of text to the bottom line of the window
    (with-selected-window (display-buffer outbuf)
      (goto-char (- (point-max) 2))  ; last 2 chars are '\n'
      (recenter -1))))

;; ---------------------------------------------------------------------------
;; Code Parsing Support
;; --------------------

(defun pel-point-in-comment-or-docstring (&optional move-fct)
  "Return position of start of comment or docstring surrounding point.
Return nil when point is outside comment and docstring.
If MOVE-FCT is specified, call it before checking the state of point."
  (save-excursion
    (when move-fct
      (funcall move-fct))
    (nth 8 (parse-partial-sexp (point-min) (point)))))

;; ---------------------------------------------------------------------------
;; Control Tab Width
;; -----------------

(defun pel-set-tab-width (n)
  "Change the tab width used in current buffer.

The change is temporary and affects the current buffer only.
Return the new tab-width or nil if unchanged."
  (interactive "nNew tab-width: ")
  (while (not (and (< n 9) (> n 1)))
    (setq n  (read-number "Enter valid tab-width in 2-8 range: " tab-width)))
  (when (not (= n tab-width))
    (message "Changed buffer's tab-width from %d to %d" tab-width n)
    (setq tab-width n)))

;;;---------------------------------------------------------------------------
(provide 'pel--base)

;;; pel--base.el ends here

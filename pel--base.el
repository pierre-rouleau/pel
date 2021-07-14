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

;;; ---------------------------------------------------------------------------
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
;;  - `pel-buffers-in-mode'
;;    - `pel-major-mode-of'
;;  - `pel-current-buffer-filename'
;;  - `pel-current-buffer-file-extension'
;;
;; Emacs Lisp Development support:
;; - `pel-add-dir-to-loadpath'
;;
;; Base predicates:
;; - `pel-expression-p'
;;
;; Check for Zero:
;;  - `pel-!0'
;;
;; Bitwise Operations:
;;  - `pel-all-bitset-p'
;;
;; String predicates:
;;  - `pel-whitespace-in-str-p'
;;  - `pel-ends-with-space-p'
;;  - `pel-starts-with-space-p'
;;  - `pel-string-ends-with-p'
;;  - `pel-string-starts-with-p'
;;  - `pel-lowercase-p'
;;  - `pel-uppercase-p'
;;
;; Pluralizer:
;;  - `pel-count-string'
;;    - `pel-plural-of'
;;
;; Symbol value extraction
;; - `pel-symbol-value'
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
;; - `pel-string-or-nil'
;; - `pel-string-for'
;; - `pel-string-when'
;; - `pel-string-spread'
;; - `pel-list-str'
;; - `pel-title-case-to-dash-separated'
;; - `pel-grp-regex'
;;
;; Value check:
;; - `pel-use-or'
;;
;; Operations on sequences:
;;  - `pel-concat-strings-in-list'
;;
;; Operation on auto-mode-alist
;;  - `pel-delete-from-auto-mode-alist'
;;
;; Lazy loading and package installation:
;; - `pel-require-at-load-deferred'
;; - `pel-require-at-load'
;; - `pel-require'
;;   - `pel-package-installed-p'
;;   - `pel-package-install'
;; - `pel-require-after-init'
;; - `pel-eval-after-load'
;; - `pel-set-auto-mode'
;; - `pel-autoload-file'
;; - `pel-declare-file'
;; - `pel-ensure-package'
;;   - `pel-ensure-pkg'
;;     - `pel--package-ensure-elpa'
;;       - `pel--package-install'
;;
;; Mode argument interpretation
;; -  `pel-action-for'
;;
;; Toggle a local mode:
;;  - `pel-toggle-mode-and-show'
;;    - `pel-toggle-mode'
;;      - `pel-autoload-p'
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
;; Swap 2 values:
;; - `pel-swap'
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
;; - `pel-path-strip'
;; - `pel-url-join'
;; - `pel-url-location'
;; - `pel-same-fname-p'
;;   - `pel-normalize-fname'
;; - `pel-symlink-points-to-p'
;;
;; Insertion of text in current buffer
;; - `pel-insert-symbol-content'
;; - `pel-insert-list-content'
;;
;; Print in dedicated buffer
;; - `pel-print-in-buffer'
;;
;; Code parsing support
;; - `pel-point-in-comment-or-docstring'
;;
;; Tab width control
;; - `pel-set-tab-width'
;;
;; Speedbar Support
;; - `pel-add-speedbar-extension'
;;
;; Byte Compilation
;; - `pel-byte-compile-if-needed'
;;   - `pel-modtime-of'
;;
;; Imenu Utilities
;; - `pel-add-imenu-sections-to'
;;
;; Tags support
;; - `pel-visit-tags'
;;
;;; --------------------------------------------------------------------------
;;; Dependencies:
;; subr (always loaded) ; use: called-interactively-p
(eval-when-compile (require 'subr-x))   ; use: split-string, string-join,
;;                                      ;      string-trim

;;; --------------------------------------------------------------------------
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

(defconst pel-emacs-is-graphic-p (display-graphic-p)
  "Predicate: t when Emacs is running in graphics mode, nil otherwise.")

(defconst pel-emacs-is-a-tty-p (not pel-emacs-is-graphic-p)
  "Predicate: t when Emacs is running in TTY mode, nil otherwise.")

(defconst pel-can-display-special-chars-p  (and (eq system-type 'darwin)
                                                pel-emacs-is-a-tty-p)
  "Predicate: t if Emacs can properly show Unicode characters like 👍 or 👎.")
;; TODO: add ability to install unicode fonts and take it into account.

;; ---------------------------------------------------------------------------
;; Code Style Buffer Local Variables
;; ---------------------------------

(defvar-local pel-comment-prefix nil
  "String identifying the comment start. Set by specific modes only.")

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
;; Support for future Emacs versions
;; ---------------------------------

(when (version< emacs-version "28")
  ;; the following function is available in Emacs 28, as part of macroexp
  ;; TODO: check if this file must be required in Emacs 28
  (defun macroexp-file-name ()
    "Return the name of the file from which the code comes.
Returns nil when we do not know.
A non-nil result is expected to be reliable when called from a macro in order
to find the file in which the macro's call was found, and it should be
reliable as well when used at the top-level of a file.
Other uses risk returning non-nil value that point to the wrong file."
    ;; `eval-buffer' binds `current-load-list' but not `load-file-name',
    ;; so prefer using it over using `load-file-name'.
    (let ((file (car (last current-load-list))))
      (or (if (stringp file) file)
          (bound-and-true-p byte-compile-current-file)))))

;; (declare-function macroexp-file-name (if (version< emacs-version "28")
;;                                          "pel--base"
;;                                        "macroexp"))

;; ---------------------------------------------------------------------------
;; Environment Querying functions:
;; ------------------------------
;;
;; The following functions provide information about the Emacs environment.

(defun pel-major-mode-of (&optional buffer-or-name)
  "Return the major mode symbol of the specified BUFFER-OR-NAME.
If not specified (or nil) return the major mode of the current buffer."
  (if buffer-or-name
      (with-current-buffer buffer-or-name
        major-mode)
    major-mode))

(defun pel-buffers-in-mode (wanted-major-mode)
  "Return a list of buffers with specified WANTED-MAJOR-MODE, nil if none open.
WANTED-MODE is a symbol."
  (let ((buffers-in-wanted-mode '()))
    (dolist (buffer (buffer-list) (reverse buffers-in-wanted-mode))
      (with-current-buffer buffer
        (when (eq major-mode wanted-major-mode)
          (push buffer buffers-in-wanted-mode))))))

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
  "Add directory DIR to Emacs variable `load-path' if not already in the list.
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
;; Base predicates
;; ---------------
;;
;; I looked for the following predicate function and did not find it.
;; If there is something like this already, let me know.

(defun pel-expression-p (val)
  "Return non-nil if VAL is an expression, nil if it is a value.
Return nil for t and nil.
Return t for 'some-symbols or '(some expressions), nothing else.
Meant to be used to identify code that is quoted (for delayed
code execution)."
  (and (not (eq val t))
       (not (eq val nil))
       (or (symbolp val)
           (consp val))))

(defun pel-user-option-p (symbol)
  "Return t when SYMBOL is a valid PEL User-option, nil otherwise."
  (and (custom-variable-p symbol)
       (eq t (compare-strings "pel-use-" nil nil
                              (symbol-name symbol) 0 8))))

;; ---------------------------------------------------------------------------
;; Check for Zero
;; --------------
;; In Lisp, nil is the only 'false' value.  Even 0 is an equivalent to 'true'.
;; The following inline help checking for a zero-value result.
;; If I find something similar native in Emacs I'll use and remove this one.
(defsubst pel-!0 (v)
  "Return nil if V is 0, t otherwise."
  (not (zerop v)))

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
;; String predicates
;; -----------------

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

(defun pel-string-ends-with-p (text suffix &optional ignore-case)
  "Return t if TEXT string does end with SUFFIX string, nil otherwise.
Ignore case differences if IGNORE-CASE is non-nil."
  (let ((text-len (length text))
        (suffix-len (length suffix)))
    (and (>= text-len suffix-len)
         (eq t (compare-strings suffix nil nil
                                text (- text-len suffix-len) nil
                                ignore-case)))))

(defun pel-string-starts-with-p (text prefix &optional ignore-case)
  "Return t if TEXT string does start with PREFIX string, nil otherwise.
Ignore case differences if IGNORE-CASE is non-nil."
  (eq t (compare-strings prefix nil nil
                         text nil (length prefix)
                         ignore-case)))

(defun pel-lowercase-p (string)
  "Return t if all characters in STRING are lowercase, nil otherwise."
  (let ((case-fold-search nil))
    (not (string-match-p "[[:upper:]]" string))))

(defun pel-uppercase-p (string)
  "Return t if all characters in STRING are uppercase, nil otherwise."
  (let ((case-fold-search nil))
    (not (string-match-p "[[:lower:]]" string))))

(defun pel-alnum-p (string)
  "Return t if all characters in STRING are letters or digits, nil otherwise."
  (let ((case-fold-search nil))
    (and (not (string-match-p "[[:punct:]]" string))
         (not (string-match-p "[[:space:]]" string))
         (not (string-match-p "[[:cntrl:]]" string)))))

;; ---------------------------------------------------------------------------
;; - Pluralizer
;; ------------

(defun pel-plural-of (word)
  "Return the plural of the specified word.
Does not handle all of English, it handles the following types:
  - class   -> classes
  - tomato  -> tomatoes
  - sky     -> skies
  - calf    -> calves
  - command -> commands"
  (let ((last-letter (substring-no-properties word -1)))
    (cond
     ;; class -> classes.  tomato -> tomatoes
     ((member last-letter '("s" "o"))
      (concat word "es"))
     ;; sky -> skies
     ((string= last-letter "y")
      (concat (substring-no-properties word 0 -1) "ies"))
     ;; calf -> calves
     ((string= last-letter "f")
      (concat (substring-no-properties word 0 -1) "ves"))
     ;; command -> commands
     (t (concat word "s")))))

(defun pel-count-string (n singular &optional plural)
  "Return a formatted string for N in SINGULAR form or PLURAL form.
If N is 0 or 1, use the singular form.
If N > 2: use the PLURAL form if specified,
          otherwise use `pel-plural-of' to compute the plural
          form of SINGULAR."

  (if (> n 1)
      (format "%d %s" n (or plural
                            (pel-plural-of singular)))
    (format "%d %s" n singular)))

;; ---------------------------------------------------------------------------
;; Symbol value extraction
;; -----------------------

(defun pel--symbol-value (symbol)
  "Return SYMBOL value or a list with the symbol and a string if not bound."
  (if (boundp symbol)
      (symbol-value symbol)
    (list symbol "**is currently unbound!**")))

(defun pel-symbol-value (symbol &optional buffer)
  "Return SYMBOL value in current or specified BUFFER."
  (if buffer
      (with-current-buffer buffer
        (pel--symbol-value symbol))
    (pel--symbol-value symbol)))

(defun pel-as-symbol (s)
  "Return the symbol for S, which can either be a string or a symbol."
  (if (symbolp s)
      s
    (intern s)))

(defun pel-as-string (s)
  "Return the string for S, which can either be a string or a symbol.
Caution: if a number is passed, the number is returned."
  (if (symbolp s)
      (symbol-name s)
    s))

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

(defun pel-symbol-on-off-string (symbol &optional on-string off-string
                                        void-string)
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
;; - `pel-string-or-nil'
;; - `pel-string-for'
;; - `pel-string-when'
;; - `pel-string-spread'
;; - `pel-list-str'
;; - `pel-title-case-to-dash-separated'
;; - `pel-grp-regex'

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

(defun pel-string-for (text)
  "Return TEXT if it's a string.  If nil return empty string."
  (if text text ""))

(defun pel-string-when (condition text)
  "Return TEXT when CONDITION is non-nil, empty string otherwise."
  (if condition text ""))

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
ELISP>"
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
;; Operation on auto-mode-alist
;; ----------------------------

(defun pel-delete-from-auto-mode-alist (mode)
  "Delete MODE specific entries from `auto-mode-alist'.
Modifies `auto-mode-alist'."
  (while (rassoc mode auto-mode-alist)
    (setq auto-mode-alist
          (assq-delete-all (car (rassoc mode auto-mode-alist))
                           auto-mode-alist))))

;; ---------------------------------------------------------------------------
;; Lazy loading and package installation:
;; - `pel-require-at-load-deferred'
;; - `pel-require-at-load'
;; - `pel-require'
;;   - `pel-package-installed-p'
;;   - `pel-package-install'
;; - `pel-require-after-init'
;; - `pel-eval-after-load'
;; - `pel-set-auto-mode'
;; - `pel-autoload-file'
;; - `pel-declare-file'
;; - `pel-ensure-package'
;;   - `pel-ensure-pkg'

(defun pel-package-install (pkg)
  "Install package PKG, return t on success, nil otherwise.

PKG must be a symbol naming one of the available packages in one
of the archives listed in variable `package-archives'.

If the first attempt fails, the function refreshes the package
list and tries again.  This prevents failing to install a package
when its version identified in the package list identifies an
obsolete version no longer supported by the Elpa archive site.

If the second attempt fails, then a error-level warning is logged
and the function returns nil"
  ;; package.el is part of Emacs but it's not loaded until required.
  ;; Load it lazily and check if the required functions are bounded
  ;; to prevent byte-compiler warnings.
  (let ((package-was-installed nil))
    (if (and (require 'package nil :no-error)
             (fboundp 'package-install))
        (condition-case-unless-debug err
            (progn
              (package-install pkg)
              (setq package-was-installed t))
          (error
           (if (and (fboundp 'package-refresh-content)
                    (fboundp 'package-read-all-archive-contents)
                    (boundp  'package-pinned-packages))
               (progn
                 (message (format "Failed to install %s: %s
  Refreshing package list and re-trying..."
                                  pkg
                                  (error-message-string err)))
                 (package-refresh-content)
                 (when (assoc pkg (bound-and-true-p package-pinned-packages))
                   (condition-case-unless-debug err
                       (progn
                         (package-read-all-archive-contents)
                         (package-install pkg)
                         (setq package-was-installed t))
                     (error
                      (display-warning
                       'pel-package-install
                       (format "After refresh, failed to install %s: %s"
                               pkg
                               (error-message-string err))
                       :error)))))
             (display-warning
              'pel-package-install
              (format "The package.el is not loaded properly.
Failed installation of %s.
Please verify the validity of your package-archives setup!"
                      pkg)
              :error))))
      (display-warning
       'pel-package-install
       (format  "package-install is void. Can't install %s!
Please verify the validity of your package-archives setup!"
                pkg)
       :error))
    package-was-installed))

(defun pel-package-installed-p (feature)
  "Return t if FEATURE is installed, nil otherwise.
Load the package library if that's not already done."
  (if (and (require 'package nil :no-error)
           (fboundp 'package-installed-p))
      (package-installed-p feature)
    (display-warning 'pel--package-installed-p
                     "Failed loading package.el to use package-installed-p!"
                     :error)
    nil))

(defun pel-require (feature &optional package with-pel-install fname
                            url-fname)
  "Load FEATURE if not already loaded, optionally try to install PACKAGE.

FEATURE is a symbol.

If optional PACKAGE is specified (non-nil) and FEATURE is not loaded,
try to install the specified package if it is not already available
and try checking for the presence of FEATURE again, with the same behaviour.

The specified package is specified by the PACKAGE argument.  It can be either:

- The special symbol `:install-when-missing' to indicate that the package to
  install has the same name as the FEATURE.
- Another symbol that identifies the name of the required package.

If WITH-PEL-INSTALL is non-nil it should be a user-project-branch
of the format used by `pel-install-github-file' that we be used
to perform the installation with FNAME and URL-FNAME argument
passed to that function.

Generate a warning when failing to load the FEATURE.
Otherwise return the loading state of the FEATURE."
  (unless (featurep feature)
    (let ((feature-is-loaded (require feature nil :noerror)))
      (unless feature-is-loaded
        ;; required failed - if package specified try installing it
        ;; when not already present
        (if package
            (if with-pel-install
                ;; install using specified GitHub repository
                (if (and (require 'pel-net nil :no-error)
                         (fboundp 'pel-install-github-file))
                    (pel-install-github-file with-pel-install fname url-fname)
                  (display-warning 'pel-require
                                   (format
                                    "Failed loading pel-net to install %s"
                                    with-pel-install)))
              ;; install using Elpa package system
              (let ((package (if (eq package :install-when-missing)
                                 feature
                               package)))
                (unless (pel-package-installed-p package)
                  (pel-package-install package)
                  (require feature nil :noerror)
                  (unless (featurep feature)
                    (display-warning 'pel-require
                                     (format "\
Failed loading %s even after installing package %s!"
                                             feature package))))))
          (display-warning 'pel-require
                           (format "pel-require(%s) failed. No request to install."
                                   feature)
                           :error)))))
  (featurep feature))

(defmacro pel-require-at-load (feature)
  "Require specified FEATURE when loading only, not when compiling.

FEATURE must be an unquoted symbol representing the required
feature."
  `(cl-eval-when 'load
     (unless (require (quote ,feature) nil :no-error)
       (display-warning 'pel-require-at-load
                        (format "Failed loading %s" (quote ,feature))
                        :error))))

(defmacro pel-require-after-init (feature secs)
  "Require specified FEATURE some SECS after initializing Emacs.

Don't require the feature when compiling.
FEATURE must be an unquoted symbol representing the required
feature.
SECS may be an integer, a floating point number, or the internal
time format returned by, e.g., ‘current-idle-time’."
  `(cl-eval-when 'load
     (run-with-idle-timer ,secs nil
                          (function require)
                          (quote ,feature) nil :no-error)))

(defmacro pel-eval-after-load (feature &rest body)
  "Evaluate BODY after the FEATURE has been loaded.
FEATURE is an unquoted symbol.
Use this for the configuration phase, like the :config of use-package."
  (declare (indent 1))
  `(with-eval-after-load (quote ,feature)
     (condition-case-unless-debug err
         (progn
           ,@body)
       (error
        (display-warning 'pel-eval-after-load
                         (format "Failed configuring %s: %s"
                                 (quote ,feature)
                                 err)
                         :error)))))

(defmacro pel-set-auto-mode (mode for: &rest regexps)
  "Activate automatic MODE for the list of file REGXEPS.
MODE must be an un-quoted symbol.
FOR: separator must be present.  It is cosmetic only.
REGEXPS is on or several regular expression strings."
  (declare (indent 0))
  (ignore for:)
  (let ((forms '()))
    (setq forms
          (dolist (regxp regexps (reverse forms))
            (push `(add-to-list 'auto-mode-alist
                                (quote (,regxp . ,mode)))
                  forms)))
    `(progn
       ,@forms)))


(defmacro pel-autoload-file (fname for: &rest commands)
  "Schedule the autoloading of FNAME for specified COMMANDS.
FNAME is either a string or an unquoted symbol.
The autoload is generated only when the command is not already bound.
Argument FOR: just a required separator keyword to make code look better.

The macro also generates a `declare-function' for each function in
COMMANDS preventing byte-compiler warnings on code referencing these
functions."
  (declare (indent 0))
  (ignore for:)
  (let ((fname     (if (stringp fname) fname (symbol-name fname)))
        (decl-fcts '()))
    (dolist (fct commands)
      (push `(declare-function ,fct ,fname) decl-fcts))
    (if (> (length commands) 1)
        `(progn
           (dolist (fct (quote (,@commands)))
             (unless (fboundp fct)
               (autoload fct ,fname nil :interactive)))
           ,@decl-fcts)
      `(progn
         (unless (fboundp (quote ,@commands))
           (autoload (quote ,@commands) ,fname nil :interactive))
         ,@decl-fcts))))

(defmacro pel-declare-file (fname defines: &rest commands)
  "Declare one or several COMMANDS to be defined in specified FNAME.
This does not generate any code.  It prevents byte-compiler warnings.
DEFINES: is a cosmetic only argument that must be present."
  (declare (indent 0))
  (ignore defines:)
  (let ((fname     (if (stringp fname) fname (symbol-name fname)))
        (decl-fcts '()))
    (dolist (fct commands)
      (push `(declare-function ,fct ,fname) decl-fcts))
    `(progn
       ,@decl-fcts)))

;;
;; The following code defines the `pel-ensure-package' macro that is
;; used below as a replacement for the `use-package' ``:ensure t`` mechanism.
;;
;; This is done to:
;; - install a package when the appropriate pel-use variable is turned on,
;; - but do NOT install it when byte-compiling the code, something the
;;   use-package :ensure t does, unfortunately.
;; - Allow the selection of a Elpa site, just as the use-package :pin does.
;; - Prevent loading use-package when nothing needs to be installed.
;;
;; The `pel-ensure-package' macro uses the `pel-ensure-pkg' function to
;; reduce the amount of code generated and executed to the expense of one
;; function call.
;;
;; Credit: the package installation code is heavily influenced by the
;; very popular use-package library found at
;; https://github.com/jwiegley/use-package


(defun pel-archive-exists-p (archive)
  "Return t if the specified package ARCHIVE is being used, nil otherwise.
The ARCHIVE argument may be a string or a symbol."
  (let ((archive (pel-as-string archive))
        (found nil))
    (if (boundp 'package-archives)
        (dolist (pa-entry package-archives)
          (when (string= archive (car pa-entry))
            (setq found 't)))
      (display-warning 'pel-archive-exists-p
                       "package.el is not loaded: package-archives is void"
                       :error))
    found))

(defvar pel--pinned-packages nil
  "List of packages that are associated with  a specific Elpa archive.")

(defun pel--pin-package (package archive)
  "Pin PACKAGE to ARCHIVE."
  (if (pel-archive-exists-p archive)
      (add-to-list 'pel--pinned-packages
                   (cons package (pel-as-string archive)))
    (error "\
Archive '%S' requested for package '%S' is not listed in package-archives!"
           archive package))
  (unless (bound-and-true-p package--initialized)
    (package-initialize t)))


(defun pel--package-install (package)
  "Install a PACKAGE.  On failure refresh ELPA content and try again.

Packages in the Elpa archive sites are regularly updated and old
versions purged.  Requesting an old version of a package may
occur when our local list is outdated.  When a failure occurs,
refresh the local list and try again."
  ;; this function is only called when package is loaded: prevent
  ;; byte-compiler warnings.
  (declare-function package-install                   "package")
  (declare-function package-refresh-contents          "package")
  (declare-function package-read-all-archive-contents "package")
  (defvar package-archive-contents)
  ;;
  (condition-case-unless-debug err
      (package-install package)
    (error
     (message "Error trying to install %s : %s.  \
Refreshing package list and trying again." package err)
     (package-refresh-contents)
     (package-read-all-archive-contents)
     (if (assoc package package-archive-contents)
         (package-install package)
       (display-warning 'pel--install-package
                        (format "Failed locating package %s" package)
                        :error)))))

(defun pel--package-ensure-elpa (package)
  "Install specified Emacs Lisp PACKAGE.
PACKAGE must be a symbol.

DO NOT use this function directly inside your code.
Instead, use the macro function `pel-ensure-package'.

Issue an error when the installation fails."
  (if (and (require 'package nil :no-error)
           (boundp 'package-archive-contents)
           (fboundp 'package-read-all-archive-contents))
      (condition-case-unless-debug err
          (progn
            (when (assoc package (bound-and-true-p
                                  pel--pinned-packages))
              (package-read-all-archive-contents))
            (if (assoc package package-archive-contents)
                (pel--package-install package)
              (package-refresh-contents)
              (when (assoc package (bound-and-true-p
                                    pel--pinned-packages))
                (package-read-all-archive-contents))
              (pel--package-install package))
            t)
        (error
         (display-warning 'pel-ensure-package
                          (format "Failed trying to install %s: %s"
                                  package (error-message-string err))
                          :error)))
    (display-warning 'pel-ensure-package
                     (format
                      "Cannot install %s: package.el is not properly loaded."
                      package)
                     :error)))

(defun pel-ensure-pkg (pkg &optional elpa-site)
  "Install package PKG.
PKG must be a symbol.
If ELPA-SITE is non-nil it should be a string holding the name of one of the
Elpa repositories identified in the variable `package-archives'."
  (when elpa-site
    (pel--pin-package pkg elpa-site))
  (pel--package-ensure-elpa pkg))

(defmacro pel-ensure-package (pkg &optional from: pinned-site)
  "Install package named PKG, optionally from specified PINNED-SITE.
PKG must be an unquoted symbol.
When PINNED-SITE (a unquoted symbol) is specified use this as the Elpa
repository, which must be listed in the variable `package-archives'.

The FROM: argument must be present.  It is cosmetics only.

The package list is refreshed before attempting installation to
prevent trying to install an obsolete version of a package that
is no longer present on the Elpa site.

However, if the variable `pel-running-in-unpackage-mode' exists,
the macro creates code that does not attempt to load anything."
  (declare (indent 1))
  (ignore from:)
  (unless (boundp 'pel-running-in-unpackage-mode)
    (let* ((pin-site-name (when pinned-site (symbol-name pinned-site))))
      `(unless (pel-package-installed-p (quote ,pkg))
         (pel-ensure-pkg (quote ,pkg) ,pin-site-name)))))

;; ---------------------------------------------------------------------------
;; Mode argument interpretation
;; ----------------------------

(defun pel-action-for (action current-state)
  "Return 'activate, 'deactivate or nil for requested ACTION on CURRENT-STATE.

Interpret requested ACTION according to its value:
 - nil or 0        : toggle,
 - > 0 or a list   : activate,
 - < 0             : deactivate.

Interpreting a list as positive allows action to be taken from
the argument of an (interactive \"P\") function without the need
to call `prefix-numeric-value' on the argument.

The CURRENT-STATE is either:
- nil (currently deactivated) or
- non-nil (currently activated)

The returned value is:
- nil         : nothing to do: the current-state is what is requested,
- 'activate   : need to activate it
- 'deactivate : need to deactivate it."
  (cond
   ;; action requested: toggle
   ((or (not action)
        (eq action 0))
    (if current-state 'deactivate 'activate))
   ;; action requested: activate
   ((or (listp action)
        (> action 0))
    (if current-state nil 'activate))
   ;; action requested: deactivate
   (t
    (if current-state 'deactivate nil))))

;; ---------------------------------------------------------------------------
;; Toggle a local mode
;; -------------------


(defun pel-autoload-p (fct)
  "Return file to load if FCT is an autoloaded function not yet loaded.
Return nil otherwise."
  (when (and (fboundp fct)
             (eq 'autoload (car (symbol-function fct))))
    (cadr (symbol-function fct))))

(defun pel-toggle-mode (mode)
  "Toggle the specified MODE (a symbol).
Return the new state of the mode: t if active, nil otherwise.
If the mode function is an autoload and not yet loaded the file
is loaded and the mode activated."
  (unless (symbolp mode)
    (error "Nothing done: pel-toggle-mode expects a symbol as argument"))
  ;; Some modes define their state variables only when they are first ran.
  ;; For those allow calling the function with an argument 1 when their
  ;; variable is still not yet bound.
  (let ((file-to-load (pel-autoload-p mode)))
    (when file-to-load
      (load file-to-load))
    (funcall (symbol-function mode) (if (and (boundp mode)
                                             (symbol-value mode))
                                        -1
                                      1))))

(defun pel-toggle-mode-and-show (mode &optional on-string off-string)
  "Toggle specified MODE (a symbol), and show it's new value.
If ON-STRING and OFF-STRING arguments are specified use them as the
on/off value, otherwise use \"on\" and \"off\".
The function issue an error if the argument is not a symbol."
  (pel-toggle-mode mode)
  (message (pel-symbol-text mode on-string off-string)))

(defun pel-toggle-syntax-check-mode (selector)
  "Toggle the active state of syntax checker mode identified by SELECTOR.

SELECTOR must be the symbol of a (often defcustom) variable.
That variable must have one of the following values:

- nil
- 'with-flymake
- 'with-flycheck

These values identify the syntax checker to control.
When the value of the SELECTOR symbol is nil nothing is done.
If the value is 'with-flymake, then flymake is toggled.
If the value is 'with-flycheck then flycheck is toggled."
  (let ((syntax-checker (symbol-value selector)))
    (cond
     ((eq syntax-checker 'with-flycheck)
      (pel-toggle-mode 'flycheck-mode))
     ((eq syntax-checker 'with-flymake)
      (pel-toggle-mode 'flymake-mode)))))

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

(defun pel-add-hook-for (modes-list-symbol func &optional allowed-modes)
  "Add the FUNC hook to all modes listed in the MODES-LIST-SYMBOL.
When ALLOWED-MODES is specified the accepted mode list symbols in
MODES-LIST-SYMBOL is restricted the ones in ALLOWED-MODES.
If another mode is included in MODES-LIST-SYMBOL a warning is issued."
  (dolist (mode (eval modes-list-symbol))
    (if (and mode                       ; make sure the mode is a valid symbol
             (symbolp mode)
             (or (null allowed-modes)
                 (memq mode allowed-modes)))
        (add-hook (pel-hook-symbol-for mode)
                  func)
      (display-warning
       'pel-mode-hooks
       (format "Invalid mode %s in the list %s?
Should the mode be added to the `pel-allowed-modes-for-lispy'?
If this mode should be allowed please report the issue.
For now change its customized value with ``M-x customize %s``
and activate the mode manually with M-x lispy-mode."
               mode
               modes-list-symbol
               modes-list-symbol)
       :error))))

;; ---------------------------------------------------------------------------

(defun pel--check-minor-modes-in (list-var minor-modes)
  "Check validity of all MINOR-MODES specified in the LIST-VAR.
LIST-VAR is the symbol of the variable holding MINOR-MODES.
MINOR-MODES is he list of minor modes symbols.
Generate a warning if any symbol in the MINOR-MODES list is not a valid
mode switching symbol."
  (let ((error-count 0))
    (dolist (minor-mode minor-modes)
      (unless (and (symbolp minor-mode)
                   (fboundp minor-mode)
                   (commandp minor-mode))
        (setq error-count (1+ error-count))
        (display-warning 'pel-invalid-mode-symbol
                         (format "Invalid mode symbol in %s: %S"
                                 list-var minor-mode)
                         :error)))
    (when (> error-count 0)
      (display-warning
       'pel-invalid-mode-symbol
       (format "Please fix the above errors in the %s customization user-option." list-var)
       :error))
    error-count))

(defmacro pel-check-minor-modes-in (minor-modes)
  "Check validity of minor-modes listed in MINOR-MODES list.
The MINOR-MODES argument must be an unquoted symbol."
  `(pel--check-minor-modes-in (quote ,minor-modes) ,minor-modes))

(defun pel-turn-on-minor-modes-in (minor-modes)
  "Turn all MINOR-MODES on."
  (dolist (minor-mode minor-modes)
    (funcall minor-mode 1)))

;; ---------------------------------------------------------------------------
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

(defun pel-toggle-and-show (symbol &optional on-string off-string locally)
  "Toggle value of SYMBOL from nil to/from t, and show it's new value.
If ON-STRING and OFF-STRING arguments are specified use them as the
on/off value, otherwise use \"on\" and \"off\".
By default the setting is considered global unless LOCALLY is set,
which indicates that the setting is for the current buffer only.
For example, to toggle the value of a variable named isok,
the caller must pass it quoted.
The function issue an error if the argument is not a symbol."
  (pel-toggle symbol)
  (message "%s%s"
           (pel-symbol-text symbol on-string off-string)
           (if locally " (in current buffer)" "")))

(defun pel-toggle-and-show-user-option (user-option &optional globally on-string off-string)
  "Toggle the behaviour of USER-OPTION for current buffer or GLOBALLY.
Display the new state.
If ON-STRING and OFF-STRING arguments are specified use them as the
on/off value, otherwise use \"on\" and \"off\".
USER-OPTION must be a variable symbol."
  (unless globally
    (with-current-buffer (current-buffer)
      (unless (local-variable-p user-option)
        (make-local-variable user-option))))
  (pel-toggle-and-show user-option on-string off-string (not globally)))

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
;; Swap 2 values
;; -------------

(defmacro pel-swap (var-a var-b)
  "Swap the content of VAR-A and VAR-B. Return value of VAR-A."
  `(setq ,var-a (prog1 ,var-b (setq ,var-b ,var-a))))

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

(defun pel-normalize-fname (name)
  "Normalize file (or directory) NAME.

Normalize a directory or file name.  Ensure that the directory
name does not end with a slash, that it uses the file true name
and use Unix-style formatting.  The function replaces a symlink
by the file it points to.  The function does *not* expand
environment variables that may be in the string."
  (file-truename (directory-file-name name)))

(defsubst pel-parent-dirpath (pathname)
  "Return parent directory of PATHNAME true name."
  (file-name-directory (pel-normalize-fname pathname)))


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

(defun pel-path-strip (text)
  "Strip whitespace and forward slash(es) from beginning & end of TEXT."
  (string-trim text "[ \t\n\r/]+" "[ \t\n\r/]+"))

(defun pel-url-join (&rest parts)
  "Join PARTS of URL strings into a single URL string."
  (mapconcat (function pel-path-strip)
             parts
             "/"))

(defun pel-url-location (url)
  "Return a description string for the URL.
Either \"Local\" or \"Remote\"."
  (if (pel-string-starts-with-p url "file:")
      "Local"
    "Remote"))

(defun pel-same-fname-p (name1 name2)
  "Return t when the file or directory names NAME1 and NAME2 are similar.

The directory names are considered identical despite differences
in slash separator or termination or use of the ~ , . or
.. character sequences character to identify the home, current or
above directory in one of them."
  (string= (pel-normalize-fname name1)
           (pel-normalize-fname name2)))

(defun pel-symlink-points-to-p (symlink target)
  "Return t if SYMLINK points to TARGET, return nil otherwise.

The SYMLINK argument should be the absolute file-path-name of the
symlink file.  The TARGET should be the expected file-path or
dir-path where for the symlink.

Symlinks can be created with a link that is absolute or relative.
This function handles both."
  (let ((symlink-pointer (file-symlink-p symlink)))
    (when symlink-pointer
      (if (file-name-absolute-p symlink-pointer)
          (pel-same-fname-p target symlink-pointer)
        (pel-same-fname-p
         target
         (expand-file-name symlink-pointer
                           (file-name-directory symlink)))))))

;; ---------------------------------------------------------------------------
;; Insertion of text in current buffer
;; -----------------------------------

(defun pel-insert-symbol-content (symbol &optional buffer on-same-line)
  "Insert the name followed by the content of the specified SYMBOL.

By default SYMBOL must be a global symbol as its value is read in the scope
of the output buffer.  If the SYMBOL is a buffer local symbol, specify the
buffer in the optional BUFFER argument.
By default, the value is printed on the line after the variable name, unless
ON-SAME-LINE is set."
  (let ((value (pel-symbol-value symbol buffer)))
    (insert (format "\n- %-40s:%s%S"
                    (symbol-name symbol)
                    (if on-same-line " " "\n")
                    value))))

(defun pel-insert-list-content (symbol &optional buffer without-index)
  "Insert a description of the content of the list identified by its SYMBOL.

By default SYMBOL must be a global symbol as its value is read in the scope
of the output buffer.  If the SYMBOL is a buffer local symbol, specify the
buffer in the optional BUFFER argument.

By default, each element of the list is printed on a new line preceded by an
element index number unless WITHOUT-INDEX is non-nil."
  (insert (format "\n- %s:\n"
                  (symbol-name symbol)))
  (let ((idx 0)
        (list-value (pel-symbol-value symbol buffer)))
    (if list-value
        (dolist (elem list-value)
          (setq idx (1+ idx))
          (if without-index
              (insert (format "%S\n" elem))
            (insert (format "%3d - %S\n" idx elem))))
      (insert "nil\n"))))

;; ---------------------------------------------------------------------------
;; Print in dedicated buffer
;; -------------------------

(defun pel-print-in-buffer (bufname title text)
  "Print TITLE than TEXT inside specified buffer BUFNAME.

TEXT is either a string or a function that calls insert
to insert the strings into the buffer."
  (let ((current-buffer-name (buffer-name))
        (outbuf (get-buffer-create bufname)))
    (with-current-buffer outbuf
      (goto-char (point-max))
      (insert (propertize
               (format "----%s from %s --- %s -----\n"
                       title
                       current-buffer-name
                       (format-time-string "%A, %B %d, %Y @ %T"))
               'face 'bold))
      (cond ((stringp text)
             (insert (format "%s\n\n"text)))
            ((functionp text)
             (funcall text))
            (t (error "Invalid type for text: %S" text)))
      (insert "\n"))
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
  "Set the tab width used in current buffer to the value N.

The change is temporary and affects the current buffer only.
Return the new `tab-width' or nil if unchanged."
  (interactive "nNew tab-width: ")
  (while (not (and (< n 9) (> n 1)))
    (setq n  (read-number "Enter valid tab-width in 2-8 range: " tab-width)))
  (when (not (= n tab-width))
    (message "Changed buffer's tab-width from %d to %d" tab-width n)
    (setq tab-width n)))

;; ---------------------------------------------------------------------------
;; Speedbar Support
;; ----------------

(defun pel-add-speedbar-extension (extension)
  "Add Speedbar support for the specified file EXTENSION.
EXTENSION is either a string or a list of strings.
Each string is either:
-  a complete filename,
- a the file extension starting with a (non-quoted) period,
- a regular expression to express the above.

`pel-add-speedbar-extension' is a direct proxy to
`speedbar-add-supported-extension' with the ability to load the
speedbar file."
  (pel-require 'speedbar)
  (declare-function speedbar-add-supported-extension "speedbar")
  (speedbar-add-supported-extension extension))

;; ---------------------------------------------------------------------------
;; Byte-compilation
;; ----------------

(defun pel-modtime-of (filename)
  "Return the modification time of FILENAME."
  (file-attribute-modification-time (file-attributes filename)))

(defun pel-byte-compile-if-needed (el-filename &rest other-dependencies)
  "Byte-compile Emacs Lisp source EL-FILENAME if it's needed.
The EL-FILENAME string must be the name of a Emacs Lisp file and must
include the .el extension.  The name of the file may be relative
or absolute.
The file is byte compiled if it is newer than its byte-compiled
output file (a file with the .elc extension) or if the .elc file
does not exists.
It is also possible to pass OTHER-DEPENDENCIES, that are name of files that
if newer than the EL-FILENAME, force byte-compilation of the EL-FILENAME."
  (let ((elc-filename (concat el-filename "c"))
        elc-modtime)
    (when (or (not (file-exists-p elc-filename))
              (time-less-p (setq elc-modtime (pel-modtime-of elc-filename))
                           (pel-modtime-of el-filename))
              (and other-dependencies
                   (or (mapcar
                        (lambda (fname)
                          (time-less-p elc-modtime (pel-modtime-of fname)))
                        other-dependencies))))
      (byte-compile-file el-filename))))

;; ---------------------------------------------------------------------------
;; Imenu Utilities
;; ---------------

(defun pel-add-imenu-sections-to (title-rule-keywords list-var)
  "Add rules to extract imenu indices to the specified LIST-VAR.

The LIST-VAR argument must be a symbol.  For example, to add the
definitions to the imenu variable used for Common Lisp, you would
pass the quoted `lisp-imenu-generic-expression' symbol.

The SYMBOL-REGEXP argument must be a regexp string used to
extract the name of the defined symbol.  For example, use
`lisp-mode-symbol-regexp' when parsing a Lisp-like buffer.

Each entry in the TITLE-RULE-KEYWORDS list must consist of:

- string: Title : a short descriptive string that will be used in
  the imenu as title.
- rule: one of:
  - `lisp-mode-symbol-regexp' which identifies the standard Lisp symbol
    extraction regexp.
  - a list of 2 elements:
    - string: a different, explicit symbol extraction regexp,
    - integer: identifies the regexp group extracting the symbol name.
- list of one or several:
  - string: Function : a string corresponding to the Common Lisp
    function symbol.

Return the new value of LIST-VAR."
  (dolist (title-rule-keyword title-rule-keywords)
    (let* ((title    (car title-rule-keyword))
           (rule     (cadr title-rule-keyword))
           (keywords (caddr title-rule-keyword))
           (group-n  (if (symbolp rule)
                         2
                       (cadr rule)))
           (symbol-regexp (if (symbolp rule)
                              (symbol-value rule)
                            (car rule))))
      (add-to-list list-var (list title
                                  (concat
                                   "^\\s-*("
                                   (regexp-opt keywords t)
                                   "\\s-+\\(" symbol-regexp "\\)")
                                  group-n))))
  (symbol-value list-var))

;; ---------------------------------------------------------------------------
;; Tags support
;; ------------
(defun pel-visit-tags (tags-files)
  "Visit the TAGS files identified in the TAGS-FILES list and the local one."
  (let ((local-tags-fname (locate-dominating-file default-directory "TAGS")))
    (when local-tags-fname
      (visit-tags-table local-tags-fname)))
  (dolist (fname tags-files)
    (when (file-exists-p fname)
      (visit-tags-table fname))))

;;; --------------------------------------------------------------------------
(provide 'pel--base)

;;; pel--base.el ends here

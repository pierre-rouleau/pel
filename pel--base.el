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
;;  - `pel-in-fast-startup-p'
;;  - `pel-buffers-in-mode'
;;    - `pel-major-mode-of'
;;  - `pel-current-buffer-filename'
;;  - `pel-current-buffer-file-extension'
;;
;; Function alias
;; - `λc'
;;
;; Emacs Lisp Development support:
;; - `pel-add-dir-to-loadpath'
;;
;; Base predicates:
;; - `pel-expression-p'
;;
;; Conditional variable set:
;; - `pel-set-if-non-nil'
;;
;; Check for Zero:
;;  - `pel-!0'
;;
;; Bitwise Operations:
;;  - `pel-all-bitset-p'
;;
;; File System Type:
;;  - `pel-unix-socket-p'
;;  - `pel-file-type'
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
;;  - `pel-pluralize'
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
;; - `pel-as-string'
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
;; Message List formatting
;; - `pel-format-problem-messages'
;;   - `pel--format-problem-messages'
;; - `pel-message-for'
;;
;; Value check:
;; - `pel-use-or'
;;
;; Operations on sequences:
;; - `pel-concat-strings-in-list'
;; - `pel-push-fmt'
;; - `pel-prepend'
;; - `pel-cons-alist-at'
;;
;; Operation on auto-mode-alist
;;  - `pel-delete-from-auto-mode-alist'
;;
;; File System Checks
;; - `pel-file-problems'
;; - `pel-dir-problems'
;; - `pel-symlink-problems'
;;   - `pel--problem-format'
;;
;; Lazy loading and package installation:
;; - `pel-require-at-load'
;;   - `pel--require-at-load'
;; - `pel-require-after-init'
;;   - `pel--require-after-init'
;; - `pel-eval-after-load'
;; - `pel-set-auto-mode'
;; - `pel-autoload-file'
;; - `pel-declare-file'
;;
;; - `pel-install-github-file'
;;   - `pel--install-github-file'
;; - `pel-install-github-files'
;;   - `pel--install-github-files'
;;     - `pel-install-files'
;;       - `pel-install-file'
;;
;; - `pel-require'
;;   - `pel-package-installed-p'
;;   - `pel-package-install'
;;
;; - `pel-ensure-package'
;;   - `pel-ensure-pkg'
;;     - `pel--pin-package'
;;       - `pel-archive-exists-p'
;;    - `pel--package-ensure-elpa'
;;      - `pel--package-install'
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
;; - `pel-dec'
;; - `pel-inc'
;;
;; Assignment operators:
;; - `pel+='
;; - `pel-='
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
;; - `pel-sibling-dirpath'
;; - `pel-expand-url-file-name'
;; - `pel-path-strip'
;; - `pel-url-join'
;; - `pel-url-location'
;; - `pel-same-fname-p'
;;   - `pel-normalize-fname'
;; - `pel-point-symlink-to'
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
(eval-when-compile
  (require 'subr-x)              ; use: split-string, string-join, string-trim
  (require 'cl-macs))  ; use: cl-eval-when

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

(defconst pel-emacs-27-or-later-p (>= emacs-major-version 27)
  "Predicate: t when Emacs version 27 or later is running, nil otherwise.")

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
  (let ((version "0.4.1"))
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

(eval-and-compile
  (defun pel-in-fast-startup-p ()
    "Return non-nil when PEL runs in fast startup operation."
    (bound-and-true-p pel-running-in-fast-startup-p)))

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
;; Function alias
;; --------------
;; - `λc'
;;


(defmacro λc (fct &rest args)
  "Funcall lambda function FCT with ARGS.
This is an alias for `funcall'.

Note: this, so far, is the *only* PEL symbol whose name does not start with
      the 'pel' prefix.  If this clashes with something you use, please
      accept my apologies and please let me know.  Hopefully the use of
      a Unicode symbol in the name will reduce this possibility."
  `(funcall ,fct ,@args))

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
;; Conditional variable set
;; ------------------------
(defun pel-set-if-non-nil (symbol value)
  "Set SYMBOL to VALUE only if VALUE is non-nil.

If VALUE is nil do nothing."
  (when value
    (set symbol value)))

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
;; File System Type
;; ----------------

(defun pel-unix-socket-p (fname)
  "Return t if FNAME is a Unix Socket, nil otherwise.
FNAME must exists otherwise an error is raised."
  (eq (string-to-char (file-attribute-modes (file-attributes fname))) ?s))

(defun pel-file-type-str (path)
  "Return a string describing the type of file system element at PATH.

PATH must identify an existing file system object otherwise an
error is raised."
  (cond
   ((file-symlink-p path)    "symbolic link")
   ((file-directory-p path)  "directory")
   ((file-regular-p path)    "file")
   ((not (file-exists-p path)) (error "%s does not exists" path))
   ((pel-unix-socket-p path) "UNIX socket")
   (t "unknown file system object")))

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

(defun pel-count-string (n singular &optional plural no-count-for-1)
  "Return a formatted string for N in SINGULAR form or PLURAL form.
If N is 0 or 1, use the singular form.
If N > 2: use the PLURAL form if specified,
          otherwise use `pel-plural-of' to compute the plural
          form of SINGULAR.
By default, display the count of 1 unless NO-COUNT_FOR-1 is set."

  (if (> n 1)
      (format "%d %s" n (or plural
                            (pel-plural-of singular)))
    (if no-count-for-1
        singular
      (format "%d %s" n singular))))

(defun pel-pluralize (n singular &optional plural)
  "Return the plural of SINGULAR when N is larger than 1.

Use `pel-plural-of' for the plural form unless PLURAL is specified
in which case return PLURAL."
  (if (> n 1)
      (or plural (pel-plural-of singular))
    singular))

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
;; - `pel-as-string'
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

(defun pel-as-string (val)
  "Return a string for the simple object value VAL.
VAL may a string, as symbol, a number or a character.
Otherwise an error is raised."
  (cond
   ((stringp val) val)
   ((symbolp val) (symbol-name val))
   ((numberp val) (number-to-string val))
   ((characterp val) (char-to-string val))
   (t (error "pel-as-string does not support type of specified argument: %S" val))))

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

(defun pel-string-when (condition &optional text)
  "Return TEXT (or CONDITION) when CONDITION is non-nil, empty string otherwise.
TEXT is optional, if its nil CONDITION must be a string or nil."
  (if condition (or text condition) ""))

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
;; Message List formatting
;; -----------------------
;;
;; - `pel-format-problem-messages'
;;   - `pel--format-problem-messages'
;; - `pel-message-for'
;;

(defun pel--format-problem-messages (problems intro &optional extra-intro)
  "Return string describing problems.

The generated string starts with an introduction created using the
INTRO-FMT format string and its ARGS arguments if any.
The next line starts with the EXTRA-INFO string if non-nil.
Then it lists the provided PROBLEMS list.

The function returns the formatted string.

Don't use this function directly; use the
`pel-format-problem-messages' macro instead: it simplifies
caller's code."
  (let ((problem-count (length problems)))
    (format "%s\n %she following %s %s:\n - %s"
            intro
            (if extra-intro (format "%s t" extra-intro) "T")
            (pel-count-string problem-count "problem" nil :no-count-for-1)
            (pel-pluralize problem-count "remains" "remain")
            (string-join problems "\n - "))))

(defmacro pel-format-problem-messages (problems extra-intro
                                                intro-fmt &rest args)
  "Return string formatted with provided information.

The generated string starts with an introduction created using the
INTRO-FMT format string and its ARGS arguments if any.
The next line starts with the EXTRA-INFO string if non-nil.
Then it lists the provided PROBLEMS list.

This macro uses the function `pel--format-problem-messages' which
returns the formatted string."
  `(pel--format-problem-messages
    ,problems
    (format ,intro-fmt ,@args) ,extra-intro ))

;; --
(defun pel-message-for (intro messages &optional separator)
  "Print a message starting with the INTRO followed by all MESSAGES.

Each of the MESSAGES is separated from the next by \"\\n - \" unless a
separator is specified by the SEPARATOR argument."
  (let ((separator (or separator "\n - ")))
    (message "%s%s%s"
             intro
             separator
             (string-join messages separator))))

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

(defmacro pel-push-fmt (lst fmt &rest args)
  "Push string FMT formatted with ARGS to the list LST."
  (declare (indent 2))
  `(push (format ,fmt ,@args) ,lst))

(defmacro pel-prepend-to (lst elems)
  "Prepend the  to the beginning of THE-LIST."
  `(setq ,lst (append ,elems ,lst)))

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
;; File System Checks
;; ------------------
;;
;; The following functions check validity of file, directory or symlink.  They
;; return a list of the string describing the problems discovered or nil if
;; all is OK.  Problem description message are padded with the format padding
;; integer identified by the variable `pel-problems-text-length' if non-nil.
;; To impose the same padding to all problems checking function let-bind that
;; variable to the padding value required and call the functions inside the
;; scope of the let-bound value.
;;
;; - `pel-file-problems'
;; - `pel-dir-problems'
;; - `pel-symlink-problems'
;;   - `pel--problem-format'

(defvar pel-problems-text-length nil
  "If non-nil it must be the minimum length of problem message.

Used by `pel-file-problems', `pel-dir-problems' and `pel-symlink-problems'
to align the messages they generate.")

(defun pel--problem-format (msg)
  "Return a format string for MSG filled by `pel-problems-text-length'."
  (when pel-problems-text-length
    (let ((fmt (format "%%%ds" pel-problems-text-length)))
      (setq msg (format fmt msg))))
  (format "%s : %%s" msg))

(defun pel-file-problems (fname)
  "Check for the presence of the file FNAME.

Return nil if all OK, otherwise return a list of strings describing
detected problems.  Error descriptions can be padded if
`pel-problems-text-length' is set."
  (let ((issues nil))
    (unless (file-exists-p fname)
      (pel-push-fmt issues (pel--problem-format "File missing") fname))
    issues))

(defun pel-dir-problems (dname)
  "Check for presence of DNAME directory, and that it is a directory.

Return nil if all OK, otherwise return a list of strings describing
detected problems.  Error descriptions can be padded if
`pel-problems-text-length' is set."
  (let ((issues nil))
    (if (file-exists-p dname)
        (unless (file-directory-p dname)
          (pel-push-fmt issues (pel--problem-format "Is not a directory") dname))
      (pel-push-fmt issues (pel--problem-format "Directory missing") dname))
    issues))

(defun pel-symlink-problems (lname &optional target-type-name)
  "Check for presence of symlink LNAME and its target.

If TARGET-TYPE-NAME is specified it must be a string that
describes the target of the expected symlink target.

Return nil if all OK, otherwise return a list of strings
describing detected problems. Error descriptions can be padded if
`pel-problems-text-length' is set."
  (let ((issues nil))
    (if (file-exists-p lname)
        (unless (file-symlink-p lname)
          (pel-push-fmt issues (pel--problem-format "Is not a symlink") lname))
      (pel-push-fmt issues
          (pel--problem-format
           (format "%symlink %s"
                   (if target-type-name
                       (format "%s s" target-type-name)
                     "S")
                   (if (file-symlink-p lname)
                       "target is missing"
                     "missing")))
        lname))
    issues))

;; ---------------------------------------------------------------------------
;; Lazy loading and package installation:

;; The first set of functions and macros provide mechanism to require, load,
;; autoload and byte-compiler declaration facilities.
;;
;; -> - `pel-require-at-load'
;;      - `pel--require-at-load'
;; -> - `pel-require-after-init'
;;      - `pel--require-after-init'
;; -> - `pel-eval-after-load'
;; -> - `pel-set-auto-mode'
;; -> - `pel-autoload-file'
;; -> - `pel-declare-file'
;;
;; The second set of functions and macros in this group provide the logic to
;; download and install Emacs Lisp files into PEL's "utils" utility directory
;; stored in the directory identified by the variable `user-emacs-directory'.
;; PEL uses these functions to get Emacs files not supported by Elpa compliant
;; sites, but instead stored in secure and well established sites such as
;; GitHub.
;;
;; - `pel-install-file'  downloads and installs one file.
;; - `pel-install-files' downloads and installs one or several files from the
;;   same web site.
;; - `pel-install-github-files' downloads and installs one or several files
;;    from GitHub specified user project branch.
;; - `pel-install-github-file' downloads and installs one file.  That file
;;   may have a name that differs from the URL used to download it.  This is
;;   mostly used when a file name has a character that cannot be part of a URL
;;   and must be encoded differently.

;; -> - `pel-install-github-file'
;;      - `pel--install-github-file'
;;          `pel-install-file'
;; -> - `pel-install-github-files'
;;      - `pel--install-github-files'
;;        - `pel-install-files'
;;          - `pel-install-file'

;; The third set of functions and macros provide logic ins install Elpa
;; compliant packages and to require Emacs packages.
;;
;; -> - `pel-require'
;;      - `pel-package-installed-p'
;;      - `pel-package-install'
;;        `pel-install-github-file'

;; -> - `pel-ensure-package'
;;      - `pel-ensure-pkg'
;;        - `pel--pin-package'
;;          - `pel-archive-exists-p'
;;       - `pel--package-ensure-elpa'
;;         - `pel--package-install'
;;

(defun pel--require-at-load (feature)
  "Require specified FEATURE when loading only, not when compiling.
FEATURE must be a quoted symbol.
This is normally used by the macro `pel-require-at-load'."
  (unless (require feature nil :no-error)
    (display-warning 'pel-require-at-load
                     (format "Failed loading %s" feature)
                     :error)))

(defmacro pel-require-at-load (feature)
  "Require specified FEATURE when loading only, not when compiling.

FEATURE must be an unquoted symbol representing the required
feature."
  `(cl-eval-when 'load
     (pel--require-at-load (quote ,feature))))

;; --
(defun pel--require-after-init (feature secs)
  "Require specified FEATURE some SECS after initializing Emacs.
FEATURE must be a quoted symbol.
This is normally used by the macro `pel-require-after-init'."
  (run-with-idle-timer secs nil
                       (function require)
                       feature nil :no-error))

(defmacro pel-require-after-init (feature secs)
  "Require specified FEATURE some SECS after initializing Emacs.

Don't require the feature when compiling.
FEATURE must be an unquoted symbol representing the required
feature.
SECS may be an integer, a floating point number, or the internal
time format returned by, e.g., ‘current-idle-time’."
  `(cl-eval-when 'load
     (pel--require-after-init (quote ,feature) ,secs)))

;; --

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

;; --
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

;; --
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

;; --
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

;; -------

(defun pel-url-copy-file (url newname &optional ok-if-already-exists)
  "Same as url-copy-file but detects URL to non-existing file.
Raise an error if the request generates a HTTP 404 error.
Returns t if all is OK."
  (require 'url-handlers nil :no-error)
  (if (fboundp 'url-copy-file)
      ;; Try to download the file identified by the URL.
      ;; That function does not detect invalid URLS so we could get a "404:
      ;; Not Found"
      (let ((tmp-fname (make-temp-file "pel-url-copy-file"))
            (error-msg nil)
            (err-car nil)
            (err-cdr nil))
        (condition-case err
            (when (url-copy-file url tmp-fname t)
              ;; Check that the file was properly downloaded.downloaded file
              ;; url-copy-file places "404: Not Found" in the file when the
              ;; URL pointed to an invalid location of a valid server.
              (with-temp-buffer
                (insert-file-contents tmp-fname)
                (when (string= (buffer-substring-no-properties 1 4) "404")
                  (setq error-msg (format "Requested URL does not exist: %s"
                                          url))))
              (unless error-msg
                (copy-file tmp-fname newname ok-if-already-exists))
              (delete-file tmp-fname))
          (progn
            ;; this block is here to prevent byte compiler warning on
            ;;   (signal (car err) (cdr err))
            (setq err-car (car err))
            (setq err-cdr (cdr err))
            (signal err-car err-cdr)))
        (if error-msg
            (error error-msg)
          t))
    (error "url-handlers file is not loaded!")))

(defun pel-install-file (url fname &optional refresh)
  "Download and install a file FNAME from URL into the PEL's utility directory.
Also byte compile that file.
This is the 'utils' sub-directory of the directory identified by
the Emacs variable `user-emacs-directory'.
If this directory does not exist, the function creates it.

If the file already exists in the destination, no download
is done unless REFRESH is non-nil, in which case the function
prompts for confirmation.

The function returns t if the file was
downloaded, nil otherwise.  Permission errors are raised."
  (let ((utils-dirname (expand-file-name "utils" user-emacs-directory)))
    (unless (file-exists-p utils-dirname)
      (make-directory utils-dirname :make-parents-if-needed))
    (let ((subdir (file-name-directory fname)))
      (when subdir
        (setq subdir (expand-file-name subdir utils-dirname))
        (unless (file-exists-p subdir)
          (make-directory subdir :make-parents-if-needed))))
    (let ((target-fname (expand-file-name fname utils-dirname)))
      (when (or (not (file-exists-p target-fname)) refresh)
        (message "Downloading %s" url)
        (when (and (pel-url-copy-file url target-fname refresh)
                   (string= (file-name-extension target-fname) "el"))
          (message "Byte compiling it to %s" target-fname)
          (byte-compile-file target-fname))))))

(defun pel-install-files (url-base fnames &optional refresh)
  "Download & install files identified by their URL-BASE and FNAMES.

The URL-BASE is the common URL for the location of all files.

The FNAMES is a file name string or list of file name strings
identifying the name of the file located at that URL-BASE and
also the name of the file save locally into the PEL Emacs 'utils'
directory.  See `pel-install-file' for more info.

If a file already exists in the destination, no download
is done unless REFRESH is non-nil, in which case the function
prompts for confirmation.

The function returns t if the file was
downloaded, nil otherwise.  Permission errors are raised."
  (let ((fnames (if (listp fnames)
                    fnames
                  (list fnames))))
    (dolist (fname fnames)
      (pel-install-file (pel-url-join url-base fname)
                        fname
                        refresh))))

(defun pel--install-github-files (user-project-branch
                                  fnames
                                  &optional refresh)
  "Download & install FNAMES from GitHub USER-PROJECT-BRANCH.
REFRESH if required.

The function returns t if the file was
downloaded, nil otherwise.  Permission errors are raised.

This is normally called by the `pel-install-github-files' macro."
  (pel-install-files (pel-url-join "https://raw.githubusercontent.com"
                                   user-project-branch)
                     fnames
                     refresh))

(defmacro pel-install-github-files (user-project-branch
                                    fnames
                                    &optional refresh)
  "Download & install FNAMES from GitHub USER-PROJECT-BRANCH.

- USER-PROJECT-BRANCH is a GitHub user/project/branch name path
  string.  Something like \"pierre-rouleau/pel/master\".
  If a depot file is stored in a depot sub-directory, include the
  path of depot directory inside USER-PROJECT-BRANCH.
- FNAMES is a file name string or list of file names.

If a file already exists in the destination, no download is done
unless REFRESH is non-nil, in which case the function prompts for
confirmation.

The macro generates code that runs only at load time.  However,
when PEL runs in fast startup the macro creates no code and
expands to nil which will be optimized out by the byte compiler."
  (unless (pel-in-fast-startup-p)
    `(cl-eval-when 'load
       (pel--install-github-files ,user-project-branch
                                  ,fnames
                                  ,refresh))))

(defun pel--install-github-file (user-project-branch
                                 fname
                                 &optional url-fname refresh)
  "Download & install FNAME from GitHub USER-PROJECT-BRANCH/URL-FNAME.
REFRESH if required.
The function returns t if the file was
downloaded, nil otherwise.  Permission errors are raised.
This is normally called by the `pel-install-github-file' macro."
  (pel-install-file (pel-url-join "https://raw.githubusercontent.com"
                                   user-project-branch
                                   (or url-fname fname))
                     fname
                     refresh))

(defmacro pel-install-github-file (user-project-branch
                                   fname
                                   &optional url-fname refresh)
  "Download & install FNAME from GitHub USER-PROJECT-BRANCH/URL-FNAME.

- USER-PROJECT-BRANCH is a GitHub user/project/branch name path
  string.  Something like \"pierre-rouleau/pel/master\".
  If a depot file is stored in a depot sub-directory, include the
  path of depot directory inside USER-PROJECT-BRANCH.
- FNAME is the name of the file, with its .el extension.
- URL-FNAME is the name of the file as it appears in the
  URL. This argument is only required when it differs from FNAME.

If a file already exists in the destination, no download
is done unless REFRESH is non-nil, in which case the function
prompts for confirmation.

The macro generates code that runs only at load time.  However,
when PEL operates in fast startup the macro creates no code and
expands to nil which will be optimized out by the byte compiler."
  (unless (pel-in-fast-startup-p)
    `(cl-eval-when 'load
       (pel--install-github-file ,user-project-branch
                                 ,fname
                                 ,url-fname
                                 ,refresh))))

;; -------
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
of the format used by `pel-install-github-file' that will be used
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
                (pel-install-github-file with-pel-install fname url-fname)
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

;; -------
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
Elpa repositories identified in the variable `package-archives'.

However, when PEL operates in fast startup, nothing is done."
  (unless (pel-in-fast-startup-p)
    (when elpa-site
      (pel--pin-package pkg elpa-site))
    (pel--package-ensure-elpa pkg)))

(defmacro pel-ensure-package (pkg &optional from: pinned-site)
  "Install package named PKG, optionally from specified PINNED-SITE.
PKG must be an unquoted symbol.
When PINNED-SITE (a unquoted symbol) is specified use this as the Elpa
repository, which must be listed in the variable `package-archives'.

The FROM: argument must be present.  It is cosmetics only.

The package list is refreshed before attempting installation to
prevent trying to install an obsolete version of a package that
is no longer present on the Elpa site.

However, when PEL operates in fast startup, the macro creates no code."
  (declare (indent 1))
  (ignore from:)
  (unless (pel-in-fast-startup-p)
    (let* ((pin-site-name (when pinned-site (symbol-name pinned-site))))
      `(unless (pel-package-installed-p (quote ,pkg))
         (pel-ensure-pkg (quote ,pkg) ,pin-site-name)))))

;; ---------------------------------------------------------------------------
;; Delay activation of Modes after processing of command line arguments
;; --------------------------------------------------------------------
(eval-and-compile
  (defmacro pel-after-startup-do (&rest body)
    "Schedule BODY execution after processing of command line arguments."
    `(add-hook 'emacs-startup-hook
               (lambda ()
                 ,@body)
               :append)))

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


(defun pel-turn-on-global-minor-modes-in (minor-modes)
  "Turn all *global* MINOR-MODES on for all buffers.

MINOR-MODES must be a symbol. In PEL that should be
`pel-activates-global-minor-modes'.

The function generates a warning describing the problem if a
local minor mode is specified instead of a local minor mode."
  (dolist (minor-mode (symbol-value minor-modes))
    (when (and (boundp minor-mode)
               (local-variable-if-set-p minor-mode))
      (display-warning
       'pel-turn-on-global-minor-modes-in
       (format
        "Problem detected in your customization data:
 User-option `%s' requests activation of *local* minor-mode %s for all buffers.
 The minor-mode is instead activated only for %s buffers.
 Please remove `%s' from `%s'.
 Instead add it to `pel-<mode>activates-minor-modes' for specific minor modes."
        (symbol-name minor-modes) minor-mode major-mode
        minor-mode (symbol-name minor-modes))
       :warning))
    (funcall minor-mode 1)))

(defun pel-turn-on-local-minor-modes-in (minor-modes)
  "Turn all *local* MINOR-MODES on for the buffer's major mode.

MINOR-MODES must be a symbol. In PEL that should be one of the
`pel-<mode>-activates-minor-modes' symbols.

This must be called within the scope of a buffer using the major mode
where we want to activate the local minor mode.
The function generates a warning describing the problem if a
global minor mode is specified instead of a local minor mode."
  (dolist (minor-mode (symbol-value minor-modes))
    (when (and (boundp minor-mode)
               (not (local-variable-if-set-p minor-mode)))
      (display-warning
       'pel-turn-on-local-minor-modes-in
       (format
        "Problem detected in your customization data in buffer %s:
 User-option `%s' requests activation of *global* minor-mode %s in %s buffers.
 The minor-mode is instead activated globally for all buffers.
 Please remove `%s' from `%s'.
 Instead add it to `pel-activates-global-minor-modes'."
        (current-buffer)
        (symbol-name minor-modes) minor-mode major-mode
        minor-mode (symbol-name minor-modes))
       :warning))
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

(defun pel-toggle-and-show (symbol &optional on-string off-string locally name)
  "Toggle value of SYMBOL from nil to/from t, and show it's new value.
If ON-STRING and OFF-STRING arguments are specified use them as the
on/off value, otherwise use \"on\" and \"off\".
By default the setting is considered global unless LOCALLY is set,
which indicates that the setting is for the current buffer only.
For example, to toggle the value of a variable named isok,
the caller must pass it quoted.
Use NAME instead symbol name in the message if specified.
The function issue an error if the argument is not a symbol."
  (pel-toggle symbol)
  (message "%s%s"
           (if name
               (format "%s is now: %s" name
                       (pel-symbol-on-off-string symbol
                                                 on-string off-string))

               (pel-symbol-text symbol on-string off-string))
           (if locally " (in current buffer)" "")))

(defun pel-toggle-and-show-user-option (user-option
                                        &optional globally
                                        on-string off-string name)
  "Toggle the behaviour of USER-OPTION for current buffer or GLOBALLY.

Display the new state.
If ON-STRING and OFF-STRING arguments are specified use them as the
on/off value, otherwise use \"on\" and \"off\".
USER-OPTION must be a variable symbol.
Note that USER-OPTION is misnamed.  It can be the symbol of any variable.
Use NAME instead symbol name in the message if specified."
  (unless globally
    (with-current-buffer (current-buffer)
      (unless (local-variable-p user-option)
        (make-local-variable user-option))))
  (pel-toggle-and-show user-option on-string off-string (not globally) name))

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
;; Assignment operators
;; --------------------
;;
;; Just the 2 mostly used ones.  May add the others on need basis.

(defmacro pel+= (var value)
  "Increment variable VAR by VALUE."
  `(setq ,var
         (+ ,var ,value)))

(defmacro pel-= (var value)
  "Decrement variable VAR by VALUE."
  `(setq ,var
         (- ,var ,value)))

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
  (directory-file-name (file-truename name)))

(defsubst pel-parent-dirpath (pathname)
  "Return parent directory of PATHNAME true name."
  (file-name-directory (pel-normalize-fname pathname)))


(defun pel-sibling-dirname (dirpath sibling)
  "Return directory path name of SIBLING directory of DIRPATH directory."
  (expand-file-name sibling
                    (file-name-directory (directory-file-name dirpath))))

(defun pel-sibling-dirpath (dirpath sibling)
  "Return directory path of SIBLING directory of DIRPATH directory."
  (file-name-as-directory (pel-sibling-dirname dirpath sibling)))

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

(defun pel-point-symlink-to (source target
                                    &optional dont-follow-symlink-target)
  "Set or turn SOURCE into a symbolic link that points to specified TARGET.

The true name of the TARGET is used: if TARGET is a symbolic link
its final target is used unless DONT-FOLLOW-SYMLINK-TARGET is set.

If the TARGET is a directory the function ensures that the link
uses the directory format; it ensures the path ends with a
directory separator character (forward slash in Unix).

The SOURCE argument may identify an existing symlink which may
point to a file or a directory.  The SOURCE symlink is not
followed and is the one modified if it exists.

If the SOURCE already exists, either as a symbolic link to a file or
directory, the function changes its target to the specified TARGET.
If SOURCE does not already exist the function creates a new
symbolic link there that points to the specified TARGET.

SOURCE may not identify existing real file, directory or other
file system object, if it does the function will raise an error
describing the problem.

Returns the true name of the target."
  ;; Note: file-symlink-p only works when the path passed
  ;;       does NOT end with a slash separator.
  ;;       Otherwise it always returns nil.
  (when (and (not (file-symlink-p (directory-file-name source)))
             (file-exists-p source))
    (error
     "(pel-point-symlink-to %S %S) arguments are invalid.
  Cannot turn the %s %s into a symlink!"
     source target (pel-file-type-str source) source))
  ;;
  ;; remove existing symlink; it will be re-created
  (when (file-exists-p source)
    (delete-file (directory-file-name source)))
  ;; adjust target to ensure its validity: use fully expanded
  ;; and absolute path for target except if the target is already a symlink
  ;; and dont-follow-symlink-target is set. In this case, however ensure that a
  ;; target that starts with "~" is expanded because symlinks that have a
  ;; target that starts with "~" do not work (at least on some systems).
  (setq target (if (file-symlink-p (directory-file-name target))
                   (if dont-follow-symlink-target
                       (if (pel-string-starts-with-p target "~")
                           (expand-file-name target)
                         target)
                     ;; symlink but must follow it
                     (file-truename target))
                 ;; not a symlink
                 (file-truename target)))
  ;; Use the directory syntax for directory targets.
  (when (file-directory-p target)
    (setq target (file-name-as-directory target)))
  (make-symbolic-link target source))

(defun pel-symlink-points-to-p (symlink target)
  "Return t if SYMLINK points to TARGET, return nil otherwise.

The SYMLINK argument should be the absolute file-path-name of the
symlink file.  The TARGET should be the expected file-path or
dir-path for the symlink.

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

(defun pel-insert-url-link (title url &optional extra-text)
  "Insert a TITLE hyperlink button to specified URL."
  (if (and (require 'button nil :no-error)
           (fboundp 'insert-button)
           (fboundp 'browse-url)
           (fboundp 'button-get))
      (insert-button title 'action
                     (lambda (x)  (browse-url (button-get x 'url)))
                     'url url)
    (insert title))
  (when extra-text
    (insert extra-text)))

(defun pel-insert-symbol (symbol &optional no-button)
  "Insert the SYMBOL name at point.

Insert the SYMBOL name as a clickable button unless NO-BUTTON is non-nil."
  (let ((name (symbol-name symbol)))
    (if no-button
        (insert name)
      (if (and (require 'button nil :no-error)
               (fboundp 'insert-button))
          (insert-button name 'action (lambda (_s)
                                        (describe-symbol symbol)))
        (insert name)))))

(defun pel-insert-symbol-content (symbol
                                  &optional buffer on-same-line no-button)
  "Insert the name followed by the content of the specified SYMBOL.

Insert the SYMBOL name as a clickable button unless NO-BUTTON is non-nil.
By default SYMBOL must be a global symbol as its value is read in the scope
of the output buffer.  If the SYMBOL is a buffer local symbol, specify the
buffer in the optional BUFFER argument.
By default, the value is printed on the line after the variable name, unless
ON-SAME-LINE is set."
  (let ((value (pel-symbol-value symbol buffer))
        (name  (symbol-name symbol)))
    (insert "\n- ")
    (pel-insert-symbol symbol no-button)
    (insert (format "%s:%s%S"
                    (make-string (max 0 (- 40 (length name))) ?\s)
                    (if on-same-line " " "\n")
                    value))))

(defun pel-line-prefixed-with (text prefix)
  "Return TEXT with each line prefixed with PREFIX string."
  (mapconcat (lambda (line) (concat prefix line))
             (split-string text "\n")
             "\n"))

(defun pel--pp (object &optional stream prefix)
  "Pretty-print OBJECT on STREAM or standard-output."
  (if (and (require 'pp nil :no-error)
           (fboundp 'pp-to-string))
      (let ((text (string-trim (pp-to-string object))))
        (princ (pel-line-prefixed-with text (or prefix ""))
               stream))
    (princ object stream)))

(defun pel-insert-list-content (symbol
                                &optional buffer without-index no-button)
  "Insert a description of the content of the list identified by its SYMBOL.

Insert the SYMBOL name as a clickable button unless NO-BUTTON is non-nil.
By default SYMBOL must be a global symbol as its value is read in the scope
of the output buffer.  If the SYMBOL is a buffer local symbol, specify the
buffer in the optional BUFFER argument.

By default, each element of the list is printed on a new line preceded by an
element index number unless WITHOUT-INDEX is non-nil."
  (let ((list-value (pel-symbol-value symbol buffer)))
    (if (null list-value)
        (pel-insert-symbol-content symbol buffer :on-same-line no-button)
      (insert "\n- ")
      (pel-insert-symbol symbol no-button)
      (insert ":")
      (let ((idx 0))
        (dolist (elem list-value)
          (setq idx (1+ idx))
          (unless without-index
            (insert (format "\n%3d -\n" idx)))
          (pel--pp elem (current-buffer) "   "))))))

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

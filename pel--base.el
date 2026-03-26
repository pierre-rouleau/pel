;;; pel--base.el --- PEL base utilities  -*-lexical-binding: t; -*-

;; Copyright (C) 2020-2026  Pierre Rouleau

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
;; in hierarchical calling order.  Functions defined elsewhere and repeated in
;; the hierarchy are preceded by a '.' instead of '-'.
;;
;; PEL version
;;  * `pel-version'
;;
;; Assignment operator macros
;;  - `pel+='
;;  - `pel-='
;;
;; Function alias macro
;;  - `λc'
;;
;; Base predicates:
;;  - `pel-expression-p'
;;  - `pel-user-option-p'
;;
;; Set variable conditionally
;;  - `pel-set-if-non-nil'
;;
;; Basic value checks:
;;  - `pel-!0'
;;  - `pel-as-boolean'
;;
;; Bitwise Operations:
;;  - `pel-all-bitset-p'
;;
;; List Handling:
;;  - `pel-list-of'
;;  - `pel-transpose-alist'
;;
;; Environment Querying functions
;;  - `pel-in-fast-startup-p'
;;
;; Checking Major Mode
;;  - `pel-major-mode-must-be'
;;  - `pel-derived-mode-p'
;;  - `pel-dired-buffer-p'
;;  - `pel-string-with-major-mode'
;;    - `pel-file-type-for'
;;    - `pel-major-mode-of'
;;  - `pel-buffers-in-mode'
;;    - `pel-major-mode-of'
;;
;; Minor and Major Mode Utilities
;;  - `pel-minor-mode-state'
;;  - `pel-major-mode-symbol-value-or'
;;    - `pel-major-mode-symbol-value'
;;      - `pel-major-mode-symbol-for'
;;  - `pel-set-major-mode-symbol'
;;
;; Buffer Information
;;  - `pel-current-buffer-filename'
;;  - `pel-current-buffer-file-extension'
;;  - `pel-current-buffer-eol-type'
;;
;; Current Directory
;;  * `pel-cd-to-current'
;;
;; OS Environment Utilities
;;  - `pel-terminal-is-macos-terminal-p'
;;  - `pel-running-under-ssh-p'
;;
;; Emacs Environment Utilities
;;  - `pel-locate-user-emacs-file'
;;  * `pel-add-dir-to-loadpath'
;;
;; File System Type
;;  - `pel-unix-socket-p'
;;  - `pel-file-type-str'
;;
;; String predicates
;;  - `pel-whitespace-in-str-p'
;;  - `pel-ends-with-space-p'
;;  - `pel-starts-with-space-p'
;;  - `pel-string-ends-with-p'
;;  - `pel-string-starts-with-p'
;;  - `pel-lowercase-p'
;;  - `pel-uppercase-p'
;;  - `pel-alnum-p'
;;
;; Pluralizer
;;  - `pel-count-string'
;;  - `pel-pluralize'
;;    - `pel-plural-of'
;;
;; Symbol value extraction
;;  - `pel-symbol-value'
;;    - `pel--symbol-value'
;;  - `pel-as-symbol'
;;
;; Symbol at point
;;  - `pel-symbol-at-point'
;;
;; String generation utilities:
;;  - `pel-symbol-text'
;;    - `pel-symbol-on-off-string'
;;      - `pel-on-off-string'
;;  - `pel-value-on-off-text'
;;    . `pel-symbol-on-off-string'
;;  - `pel-symbol-value-or'
;;  - `pel-yes-no-string'
;;  - `pel-key-binding-string'
;;
;; Automated Mode Activation Check
;;  - `pel-minor-mode-auto-activated-by'
;;    - `pel-modes-activating-symbol-name-for'
;;  - `pel-option-mode-state'
;;    - `pel-activated-in-str'
;;
;; String transformation utilities
;;  - `pel-as-string'
;;  - `pel-end-text-with-period'
;;  - `pel-hastext'
;;  - `pel-when-text-in'
;;  - `pel-string-or-nil'
;;  - `pel-string-for'
;;  - `pel-string-when'
;;  - `pel-string-spread'
;;  - `pel-list-str'
;;  - `pel-title-case-to-dash-separated'
;;  - `pel-grp-regex'
;;
;; Message List formatting
;;  - `pel-format-problem-messages'
;;    - `pel--format-problem-messages'
;;  - `pel-message-for'
;;
;; Value check:
;;  - `pel-use-or'
;;
;; Operations on sequences:
;;  - `pel-concat-strings-in-list'
;;  - `pel-prepend-to'
;;  - `pel-cons-alist-at'
;;  - `pel-nth-elt'
;;  - `pel-list-insert-before'
;;  - `pel-list-prepend-nth'
;;  - `pel-list-insert-car-at'
;;  - `pel-delqs'
;;
;; Operation on auto-mode-alist
;;  - `pel-delete-from-auto-mode-alist'
;;
;; PEL utils rebuild
;;  - `pel-rebuild-utils'
;;
;; Tree-sitter major mode support
;;  - `pel-major-mode-use-tree-sitter'
;;  - `pel-major-ts-mode-supported-p'
;;  - `pel-ts-language-grammar-status-for'
;;    - `pel-file-md5'
;;    - `pel-treesit-ready-p'
;;    - `pel-ts-language-grammar-filename-for'
;;
;; Mode argument interpretation
;;  - `pel-action-for'
;;
;; Toggle a local mode:
;;  - `pel-toggle-mode-and-show'
;;    - `pel-toggle-mode'
;;
;; Toggle of values and variables
;;  - `pel-toggle-and-show-user-option'
;;    - `pel-toggle-and-show'
;;      - `pel-toggle'
;;  - `pel-val-or-default'
;;
;; Symbol processing
;;  - `pel-hook-symbol-for'
;;  - `pel-map-symbol-for'
;;
;; Hook control
;;  - `pel-add-hook-for'
;;
;; Minor mode activation
;;  - `pel-check-minor-modes-in'
;;    - `pel--check-minor-modes-in'
;;  - `pel-turn-on-global-minor-modes-in'
;;  - `pel-turn-on-local-minor-modes-in'
;;
;; Argument converter:
;;  - `pel-multiplier' and `pel-mode-toggle-arg'
;;
;; Iteration helpers:
;;  - `pel-dec'
;;  - `pel-inc'
;;
;; Swap 2 values:
;;  - `pel-swap'
;;
;; Text at point:
;;  - `pel-at-lowercase-p'
;;  - `pel-at-uppercase-p'
;;    - `pel-at-letter-p'
;;  - `pel-chars-at-point'
;;
;; Calling functions:
;;  - `pel-n-funcall-to'
;;
;;  Moving Point:
;;   - `pel-goto-position'
;;   - `pel-goto-line'
;;
;; Line position:
;;  - `pel-same-line-p'
;;
;; Identifying region:
;;  - `pel-region-for'
;;
;; Insert or overwrite text
;;  - `pel-insert-or-overwrite'
;;
;; Extract text from buffer
;;  - `pel-text-from-beginning-of-line'
;;
;; Check text in buffer
;;  - `pel-line-has-only-newline-p'
;;  - `pel-line-has-only-whitespace-p'
;;  - `pel-inside-code'
;;  - `pel-has-shebang-line'
;;
;; File Path processing
;;  - `pel-file-in'
;;  - `pel-is-subdir-of'
;;    - `pel-normalize-fname'
;;  - `pel-parent-dirpath'
;;    - `pel-normalize-fname'
;;  - `pel-sibling-dirpath'
;;  - `pel-expand-url-file-name'
;;  - `pel-path-strip'
;;  - `pel-url-join'
;;  - `pel-url-location'
;;  - `pel-same-fname-p'
;;    - `pel-normalize-fname'
;;  - `pel-point-symlink-to'
;;  - `pel-symlink-points-to-p'
;;
;; Insertion of text in current buffer
;;  - `pel-insert-bold'
;;  - `pel-insert-url-link'
;;  - `pel-insert-mode-symbol-content-line-when-bound'
;;  - `pel-insert-mode-symbol-content-line'
;;    - `pel-insert-symbol-content-line'
;;      - `pel-insert-symbol-content'
;;        - `pel-insert-symbol'
;;  - `pel-insert-list-value'
;;  - `pel-insert-list-content'
;;    - `pel--pp'
;;      - `pel-line-prefixed-with'
;;
;; Move point right, optionally inserting spaces
;;  - `pel-move-right-by'
;;
;; Print in dedicated buffer
;;  - `pel-print-in-buffer'
;;
;; Code parsing support
;;  - `pel-point-in-comment-or-docstring'
;;
;; Byte Compilation
;;  - `pel-byte-compile-if-needed'
;;    - `pel-modtime-of'
;;
;; Imenu Utilities
;;  - `pel-add-imenu-sections-to'
;;
;; Tags support
;;  - `pel-visit-tags'
;;
;; Portability
;;  - `pel-executable-find'
;;  - `pel-treesit-language-available-p'
;;  - `pel-emacs-config-features-string'
;;  - `pel-hardware-model-string'
;;  - `pel-eglot-active-p'

;;; --------------------------------------------------------------------------
;;; Dependencies:
;; subr (always loaded) ; use: called-interactively-p
(require 'pel-comp)
(eval-when-compile
  (require 'subr-x))    ; use: `split-string', `string-join', `string-trim'

;;; --------------------------------------------------------------------------
;;; Code:

;; ---------------------------------------------------------------------------
;;* Environment Constants
;;  =====================
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

(defconst pel-system-is-FreeBSD-p
  (eq system-type 'gnu/kfreebsd)
  "Predicate: t if running under a FreeBSD Operating System, nil otherwise.")

(defconst pel-system-is-windows-p
  (memq system-type '(windows-nt ms-dos))
  "Predicate: t if running under a Windows Operating System, nil otherwise.")

(defconst pel-emacs-has-dynamic-module-support-p
  (and (functionp 'module-load)
       module-file-suffix)
  "Predicate: non-nil when Emacs has dynamic module support enabled.

The non-nil value of the predicate is the `module-file-suffix'.")

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

(defconst pel-emacs-28-or-later-p (>= emacs-major-version 28)
  "Predicate: t when Emacs version 28 or later is running, nil otherwise.")

(defconst pel-emacs-29-or-later-p (>= emacs-major-version 29)
  "Predicate: t when Emacs version 29 or later is running, nil otherwise.")

(defconst pel-emacs-30-or-later-p (>= emacs-major-version 30)
  "Predicate: t when Emacs version 30 or later is running, nil otherwise.")

(defconst pel-emacs-31-or-later-p (>= emacs-major-version 31)
  "Predicate: t when Emacs version 31 or later is running, nil otherwise.")

(defconst pel-filesep (if pel-system-is-windows-p "\\" "/")
  "String directory/file separator character for this OS.")

(defconst pel-emacs-with-native-comp-p (and
                                        (fboundp 'native-comp-available-p)
                                        (native-comp-available-p))
  "Predicate: t if Emacs supports native compilation.")

(defconst pel-os-lib-file-extension (cond
                                     (pel-system-is-macos-p "dylib")
                                     (pel-system-is-windows-p "dll")
                                     (t "so"))
  "File extension (without leading period) of OS library files.")

;;* Variables
;;  =========
(defvar pel-uses-tree-sitter nil
  "Set to t when PEL currently uses tree-sitter, nil otherwise.

It is set to t only by the logic of pel_keys.el which is
executed by `pel-init' on startup.")

;; ---------------------------------------------------------------------------
;;* Code Style Buffer Local Variables
;;  =================================

(defvar-local pel-comment-prefix nil
  "String identifying the comment start; set by specific modes only.")

;; ---------------------------------------------------------------------------
;;* PEL version
;;  ===========

(defun pel-version (&optional insert)
  "Return PEL package version string and also echoes it.
Optionally insert it at point if INSERT is non-nil."
  (interactive "P")
  (let ((version "0.4.1"))
    (if insert
        (insert version))
    (message "PEL version: %s" version)
    version))

;; ---------------------------------------------------------------------------
;;* Support for future Emacs versions
;;  =================================

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
;;* Assignment operator macros
;;  ==========================
;;
;; Just the 2 mostly used ones.  May add the others on need basis.

(defmacro pel+= (var value)
  "Increment variable VAR by VALUE.  Return VAR new content."
  (if (eq value 1)
      `(setq ,var (1+ ,var))
    `(setq ,var
           (+ ,var ,value))))

(defmacro pel-= (var value)
  "Decrement variable VAR by VALUE.  Return VAR new content."
  (if (eq value 1)
      `(setq ,var (1- ,var))
    `(setq ,var
           (- ,var ,value))))

;; ---------------------------------------------------------------------------
;;* Function alias macro
;; =====================

(defmacro λc (fct &rest args)
  "Funcall lambda function FCT with ARGS.
This is a short alias for `funcall'.

Caution: this, so far, is the *only* PEL symbol whose name does not
         start with the \\='pel\\=' prefix.

If this clashes with something you use, please accept my apologies and
please let me know.  Hopefully the use of a Unicode symbol in the name
will reduce this possibility."
  `(funcall ,fct ,@args))

;; ---------------------------------------------------------------------------
;;* Base predicates
;;  ===============
;;
;; I looked for the following predicate function and did not find it.
;; If there is something like this already, let me know.

(defun pel-expression-p (val)
  "Return non-nil if VAL is an expression, nil if it is a value.
Return nil for t and nil.
Return t for \\='some-symbols or \\='(some expressions), nothing else.
Meant to be used to identify code that is quoted (for delayed
code execution)."
  (declare (pure t) (side-effect-free error-free))
  (and (not (eq val t))
       (not (eq val nil))
       (or (symbolp val)
           (consp val))))

(defun pel-user-option-p (symbol)
  "Return t when SYMBOL is a valid PEL user-option, nil otherwise."
  (declare (side-effect-free t))
  (and (custom-variable-p symbol)
       (eq t (compare-strings "pel-use-" nil nil
                              (symbol-name symbol) 0 8))))

;; ---------------------------------------------------------------------------
;;* Set variable conditionally
;;  ==========================

(defun pel-set-if-non-nil (symbol value)
  "Set SYMBOL to VALUE only if VALUE is non-nil.

If VALUE is nil do nothing."
  (when value
    (set symbol value)))

;; ---------------------------------------------------------------------------
;;* Check for Zero
;;  ==============
;;
;; In Lisp, nil is the only \\='false\\=' value.  Even 0 is an equivalent to
;; \\='true\\='.  The following inline help checking for a zero-value result.

(defsubst pel-!0 (number)
  "Return nil if NUMBER is 0, t otherwise."
  (declare (pure t) (side-effect-free t))
  (null (zerop number)))

(defun pel-as-boolean (value)
  "Return t for non-nil VALUE, nil otherwise."
  (declare (pure t) (side-effect-free error-free))
  (null (null value)))

;; ---------------------------------------------------------------------------
;;* Bitwise Operations
;;  ==================

(defun pel-all-bitset-p (value &rest bits)
  "Return t when all and only those BITS are set in VALUE, nil otherwise."
  (let ((bitmask 0))
    (dolist (bit bits bitmask)
      (setq bitmask (logior bitmask bit)))
    (equal 0 (logxor value bitmask))))

;; ---------------------------------------------------------------------------
;;* List Handling
;;  =============

(defun pel-list-of (val)
  "Return VAL if it is a list, (list val) otherwise."
  (declare (side-effect-free error-free))
  (if (listp val)
      val
    (list val)))

(defun pel-transpose-alist (alist)
  "Transpose the (a . b) ALIST into the (b . a) alist."
  (mapcar (lambda (pair)
            (cons (cdr pair) (car pair)))
          alist))

;; ---------------------------------------------------------------------------
;;* Environment Querying functions
;;  ==============================
;;
;; The following functions provide information about the Emacs environment.

(eval-and-compile
  (defun pel-in-fast-startup-p ()
    "Return non-nil when PEL runs in fast startup operation."
    (bound-and-true-p pel-running-in-fast-startup-p)))

;; ---------------------------------------------------------------------------
;;* Checking Major Mode
;;  ===================

(defun pel-major-mode-must-be (modes)
  "Check that the current buffer major mode is one of MODES.
MODES is either a major-mode symbol or a list of major-mode symbols.
Raise an user error if the current buffer is not using one of the MODES;
the message state that the current command is not appropriate."
  (unless (memq major-mode (pel-list-of modes))
    (user-error "This command is not meant for %s; use it in %S"
                major-mode
                modes)))

(defun pel-derived-mode-p (buffer-or-name &rest modes)
  "Non-nil if major mode of BUFFER-OR-NAME is derived from one of MODES.
If BUFFER-OR-NAME is nil, use current buffer."
  (if buffer-or-name
      (with-current-buffer buffer-or-name
        (apply (function derived-mode-p) modes))
    (apply (function derived-mode-p) modes)))

(defun pel-dired-buffer-p (&optional buffer-or-name strict)
  "Return mode if mode of BUFFER-OR-NAME is a Dired buffer, nil otherwise.

Accepts mode derived from `dired-mode' unless STRICT is non-nil."
  (if buffer-or-name
      (with-current-buffer buffer-or-name
        (or (eq major-mode 'dired-mode)
            (unless strict
              (derived-mode-p 'dired-mode))))
    (or (eq major-mode 'dired-mode)
        (unless strict
          (derived-mode-p 'dired-mode)))))

(defun pel-major-mode-of (&optional buffer-or-name)
  "Return the major mode symbol of the specified BUFFER-OR-NAME.
If not specified (or nil) return the major mode of the current buffer."
  (if buffer-or-name
      (with-current-buffer buffer-or-name
        major-mode)
    major-mode))

(defun pel-file-type-for (major-mode-symbol &optional suffix)
  "Return the file type name string for the specified MAJOR-MODE-SYMBOL.

By default that's the symbol name stripped off the '-mode' or '-ts-mode'
suffix unless SUFFIX is specified (like \"-minor-mode\")."
  (let ((sname (symbol-name major-mode-symbol)))
    (substring sname
               0
               (- (length (or suffix
                              (if (string-match "-ts-mode" sname)
                                  "-ts-mode"
                                "-mode")))))))

(defun pel-string-with-major-mode (symbol-format-string
                                   &optional buffer-or-name)
  "Return a string formatted with the single %s replaced by the major mode.

The \"%s\" in the SYMBOL-FORMAT-STRING is replaced by the name of the
major-mode.  That's the prefix string before the \"-mode\" portion of
the major mode name of the current buffer or the one specified by
BUFFER-OR-NAME."
  (format symbol-format-string
          (pel-file-type-for (pel-major-mode-of buffer-or-name))))

(defun pel-buffers-in-mode (wanted-major-mode)
  "Return a list of buffers with specified WANTED-MAJOR-MODE, nil if none open.
WANTED-MODE is a symbol; something like \\='emacs-lisp-mode"
  (let ((buffers-in-wanted-mode ()))
    (dolist (buffer (buffer-list) (nreverse buffers-in-wanted-mode))
      (with-current-buffer buffer
        (when (eq major-mode wanted-major-mode)
          (push buffer buffers-in-wanted-mode))))))

;; ---------------------------------------------------------------------------
;;* Minor and Major Mode Utilities
;;  ==============================

(defvar pel-insert-symbol-content-context-buffer nil
  "Contextual value for the buffer argument of `pel-insert-symbol-content'.

Let-bind this variable in functions that need to call
`pel-insert-symbol-content' repetitively always passing the same value
for its buffer argument.")

(defun pel-minor-mode-state (minor-mode &optional activator-symbol buffer)
  "Return a string describing the state of the MINOR-MODE, a symbol.

If ACTIVATOR-SYMBOL is nil, this is a built-in minor mode, otherwise
ACTIVATOR-SYMBOL is the symbol that activates the installation and use
of that MINOR-MODE.

The returned value is the state of the mode in the buffer identified
by BUFFER or `pel-insert-symbol-content-context-buffer'."
  (with-current-buffer (or buffer
                           pel-insert-symbol-content-context-buffer
                           (current-buffer))
    (pel-symbol-on-off-string
     minor-mode nil nil
     (if activator-symbol
         (if (symbol-value activator-symbol)
             (format "Activated by %s." activator-symbol)
           (format "Not loaded. Activate it by turning %s on then restart."
                   activator-symbol))
       "Built-in but not loaded, use a command to load it"))))

;;** Read/Set variable with a formatted name derived from major mode

(defun pel-major-mode-symbol-for (symbol-format-string
                                  &optional buffer-or-name)
  "Return the major-mode specific symbol for specified buffer.

The symbol name is identified by the SYMBOL-FORMAT-STRING which must
contain one \"%s\" that is replaced by the prefix string before the
\"-mode\" (or \"-ts-mode\") of the major mode of the current buffer
or of the buffer specified by the BUFFER-OR-NAME argument or the variable
`pel-insert-symbol-content-context-buffer'.

The BUFFER argument value takes precedence to the value of the variable
`pel-insert-symbol-content-context-buffer'. If both are nil, then the
value is read from the context of the current buffer, which may be a
local or global."
  (intern
   (pel-string-with-major-mode symbol-format-string
                               (or buffer-or-name
                                   pel-insert-symbol-content-context-buffer))))

(defun pel-major-mode-symbol-value (symbol-format-string
                                    &optional buffer-or-name)
  "Return the value of major-mode specific symbol for specified buffer.

The symbol name is identified by the SYMBOL-FORMAT-STRING which must
contain one \"%s\" that is replaced by the prefix string before the
\"-mode\" (or \"-ts-mode\") of the major mode of the current buffer
or of the buffer specified by the BUFFER-OR-NAME argument or the variable
`pel-insert-symbol-content-context-buffer'.

The BUFFER argument value takes precedence to the value of the variable
`pel-insert-symbol-content-context-buffer'. If both are nil, then the
value is read from the context of the current buffer, which may be a
local or global."
  (symbol-value
   (pel-major-mode-symbol-for
    symbol-format-string
    (or buffer-or-name pel-insert-symbol-content-context-buffer))))

(defun pel-major-mode-symbol-value-or (symbol-format-string
                                       default-value
                                       &optional buffer-or-name)
  "Return value or default of major-mode specific symbol for specified buffer.

The symbol name is identified by the SYMBOL-FORMAT-STRING which must
contain one \"%s\" that is replaced by the prefix string before the
\"-mode\" (or \"-ts-mode\") of the major mode of the current buffer
or of the buffer specified by the BUFFER-OR-NAME argument or the variable
`pel-insert-symbol-content-context-buffer'.

The BUFFER argument value takes precedence to the value of the variable
`pel-insert-symbol-content-context-buffer'. If both are nil, then the
value is read from the context of the current buffer, which may be a
local or global.

If the symbol name does not exist for the specified SYMBOL-FORMAT-STRING
for the current major mode, then return the specified DEFAULT-VALUE."
  (condition-case nil
      (pel-major-mode-symbol-value
       symbol-format-string
       (or buffer-or-name pel-insert-symbol-content-context-buffer))
    (error default-value)))

(defun pel-set-major-mode-symbol (symbol-format-string
                                  value
                                  &optional buffer-or-name)
  "Set symbol identified by SYMBOL-FORMAT-STRING to specified VALUE.

The symbol name is identified by the SYMBOL-FORMAT-STRING.  The \"%s\"
in the SYMBOL-FORMAT-STRING is replaced by the name of the major-mode.
That's the prefix string before the \"-mode\" portion of the major mode
name of the current buffer or the one specified by BUFFER-OR-NAME."
  (let ((symbol (intern (pel-string-with-major-mode symbol-format-string
                                                    buffer-or-name))))
    (set symbol value)))

;; ---------------------------------------------------------------------------
;;* Buffer Information
;;  ==================

(defun pel-current-buffer-filename (&optional sans-directory
                                              sans-extension
                                              no-error)
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

(defconst pel-eol-mode-name '((0 . unix)
                              (1 . dos)
                              (2 . mac)
                              (t . nil))
  "Association list of `buffer-file-coding-system' value to its symbolic name.")

(defun pel-current-buffer-eol-type ()
  "Return line ending of current buffer content: \\='unix, \\='dos, \\='mac or nil.

The nil value means that the type is unknown."
  (let ((eol-type (coding-system-eol-type buffer-file-coding-system)))
    (when (vectorp eol-type)
      (setq eol-type (coding-system-eol-type (aref eol-type 0))))
    (cdr (assoc eol-type pel-eol-mode-name))))

;; ---------------------------------------------------------------------------
;;* Current Directory
;;  =================

(defun pel-cd-to-current (&optional silent)
  "Change current directory to the directory holding visited file.

Print message showing the new current working directory if it changed unless
SILENT is non-nil (can be requested by prefix argument)."
  (interactive "P")
  (let* ((original-cwd (cd "."))
         (new-cwd (cd (file-name-directory (pel-current-buffer-filename)))))
    (unless (string= new-cwd original-cwd)
      (unless silent
        (message "Current directory back to: %s" new-cwd)))))

;; ---------------------------------------------------------------------------
;;* OS Environment Utilities
;;  ========================

(defun pel-terminal-is-macos-terminal-p ()
  "Return t if Emacs is running under the macOS Terminal.app, else nil."
  (declare (side-effect-free t))
  (string-equal (getenv "TERM_PROGRAM") "Apple_Terminal"))

(defun pel-running-under-ssh-p ()
  "Return t if Emacs is invoked through SSH, nil otherwise."
  (declare (side-effect-free t))
  (when (getenv "SSH_CLIENT")
    t))

;; ---------------------------------------------------------------------------
;;* Emacs Environment Utilities
;;  ===========================

(defun pel-locate-user-emacs-file (fname)
  "Return the absolute/canonical path of FNAME inside Emacs user directory.

The directory is identified by `user-emacs-directory'.
If the directory does not exist the function creates it.
This is the same as `locate-user-emacs-file' with the path made absolute and
canonized."
  (expand-file-name (locate-user-emacs-file fname)))

(defun pel-add-dir-to-loadpath (dir)
  "Add directory DIR to Emacs variable `load-path' if not already in the list.
When called interactively, it also displays the number of directories in the
list and whether the operation succeeded or not.
Return non-nil on success when it was added, nil otherwise."
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
    (/= new-length original-length)))

;; ---------------------------------------------------------------------------
;;* File System Type
;;  ================

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
   ((not (file-exists-p path)) (error "%s does not exist" path))
   ((pel-unix-socket-p path) "UNIX socket")
   (t "unknown file system object")))

;; ---------------------------------------------------------------------------
;;* String predicates
;;  =================

(defun pel-whitespace-in-str-p (text)
  "Return non-nil if any whitespace character is inside TEXT, nil otherwise.
The index of the first whitespace character is returned when one is present."
  (string-match "[ \t\n\r]" text))

(defun pel-ends-with-space-p (text)
  "Return t if TEXT ends with a space character, nil otherwise."
  (declare (side-effect-free t))
  (let ((len (length text)))
    (when (> len 0)
      (string= (substring text (- len 1) len) " "))))

(defun pel-starts-with-space-p (text)
  "Return t if TEXT has space character(s) at beginning, nil otherwise."
  (declare (side-effect-free t))
  (when (> (length text) 0)
    (string= (substring text 0 1) " ")))

(defun pel-string-ends-with-p (text suffix &optional ignore-case)
  "Return t if TEXT string does end with SUFFIX string, nil otherwise.
Ignore case differences if IGNORE-CASE is non-nil."
  (declare (side-effect-free t))
  (let ((text-len (length text))
        (suffix-len (length suffix)))
    (and (>= text-len suffix-len)
         (eq t (compare-strings suffix nil nil
                                text (- text-len suffix-len) nil
                                ignore-case)))))

(defun pel-string-starts-with-p (text prefix &optional ignore-case)
  "Return t if TEXT string does start with PREFIX string, nil otherwise.
Ignore case differences if IGNORE-CASE is non-nil."
  (declare (side-effect-free t))
  (eq t (compare-strings prefix nil nil
                         text nil (length prefix)
                         ignore-case)))

(defun pel-lowercase-p (string)
  "Return t if all characters in STRING are lowercase, nil otherwise."
  (declare (side-effect-free t))
  (let ((case-fold-search nil))
    (not (string-match-p "[[:upper:]]" string))))

(defun pel-uppercase-p (string)
  "Return t if all characters in STRING are uppercase, nil otherwise."
  (declare (side-effect-free t))
  (let ((case-fold-search nil))
    (not (string-match-p "[[:lower:]]" string))))

(defun pel-alnum-p (string)
  "Return t if all characters in STRING are letters or digits, nil otherwise."
  (declare (side-effect-free t))
  (when (and (stringp string)
             (not (equal string "")))
    (let ((case-fold-search nil))
      (and (not (string-match-p "[[:punct:]]" string))
           (not (string-match-p "[[:space:]]" string))
           (not (string-match-p "[[:cntrl:]]" string))))))

;; ---------------------------------------------------------------------------
;;* Pluralizer
;;  ==========

(defun pel-plural-of (word)
  "Return the plural of the specified WORD.
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
- If N is 0 or 1, use the singular form.
- If N > 2: use the PLURAL form if specified,
            - otherwise use `pel-plural-of' to compute the plural
              form of SINGULAR.
By default, display the count of 1 unless NO-COUNT-FOR-1 is set."

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
;;* Symbol value extraction
;;  =======================

(defun pel--symbol-value (symbol &optional quiet)
  "Return SYMBOL value if it is bound.

If it is not bound, then return a list with the symbol and a
string describing that it is not bound, unless QUIET is non-nil.  If QUIET is
non-nil, just return nil when SYMBOL is not bound."
  (declare (side-effect-free t))
  (if (boundp symbol)
      (symbol-value symbol)
    (unless quiet
      (list symbol "**is currently unbound!**"))))

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
;;* Symbol at point
;;  ===============

(defun pel-symbol-at-point ()
  "Return symbol at point; return nil if there are none."
  (if (and (require 'thingatpt nil 'noerror)
           (fboundp 'thing-at-point))
      (thing-at-point 'symbol :no-properties)
    (error "Function thing-at-point not loaded!")))

;; ---------------------------------------------------------------------------
;;* String generation utilities
;;  ===========================

(defun pel-on-off-string (boolean &optional on-string off-string)
  "Return \"off\" for nil, \"on\" for non-nil BOOLEAN argument.
If ON-STRING and OFF-STRING arguments are specified use them as the
on/off value, otherwise use \"on\" and \"off\"."
  (declare (pure t) (side-effect-free t))
  (if boolean
      (or on-string "on")
    (or off-string "off")))

(defun pel-symbol-on-off-string (symbol &optional on-string off-string
                                        void-string buffer)
  "Return representation of SYMBOL value and whether it is bound.
When SYMBOL is not bound: return VOID-STRING or \"void\" if it's nil,
When it is bound, return:
- the OFF-STRING or \"off\" for nil,
- the ON-STRING or \"on\" for SYMBOL boolean value.

The returned value is the state of the mode in the buffer identified
by BUFFER or `pel-insert-symbol-content-context-buffer'."
  (with-current-buffer (or buffer
                           pel-insert-symbol-content-context-buffer
                           (current-buffer))
    (if (boundp symbol)
        (pel-on-off-string (eval symbol) on-string off-string)
      (or void-string (format "unknown - `%S' is not bound!" symbol)))))

(defvar pel--prompt-separator ":")

(defun pel-symbol-text (symbol &optional on-string off-string void-string)
  "Return a string with an interpretation of SYMBOL value.
If SYMBOL is not bound: show \"void\".
If SYMBOL is set to t: show ON-STRING if defined, \"on\" otherwise.
If SYMBOL is nil: show OFF-STRING if defined, \"off\" otherwise.
If SYMBOL is not defined, show VOID-STRING if defined, \"void\" otherwise."
  (format "%s is now%s %s"
          symbol
          pel--prompt-separator
          (pel-symbol-on-off-string symbol on-string off-string void-string)))

;; --

(defun pel-value-on-off-text (symbol &optional on-string off-string)
  "Return a string describing SYMBOL as a boolean value.
If SYMBOL value if non-nil: show ON-STRING if defined, \"on\" otherwise.
If SYMBOL value  is nil   : show OFF-STRING if defined, \"off\" otherwise."
  (format "%s is now%s %s"
          (symbol-name symbol)
          pel--prompt-separator
          (pel-symbol-on-off-string symbol on-string off-string)))

;; --

(defun pel-symbol-value-or (symbol &optional replacement formatter buffer)
  "Return SYMBOL value if non void, otherwise its REPLACEMENT.

If SYMBOL is void and there is no REPLACEMENT return a string
created by (format \"unknown - %S is not loaded\" symbol).
If SYMBOL is void and replacement is :nil-for-void, return nil.
If SYMBOL is bound and FORMATTER is non nil it's a function that
takes the symbol and returns a string.

The returned value is the state of the mode in the buffer identified
by BUFFER or `pel-insert-symbol-content-context-buffer'."
  (with-current-buffer (or buffer
                           pel-insert-symbol-content-context-buffer
                           (current-buffer))
    (if (boundp symbol)
        (if formatter
            (funcall formatter symbol)
          (symbol-value symbol))
      (if replacement
          (if (eq replacement :nil-for-void)
              nil
            replacement)
        (format "unknown - `%S' is not bound!" symbol)))))

;; --

(defun pel-yes-no-string (test &optional true-string false-string)
  "Return TRUE-STRING when boolean TEST is non-nil, otherwise FALSE_STRING.
By default or when these arguments are nil:
- TRUE_STRING is \"yes\" and
- FALSE_STRING is \"no\"."
  (declare (pure t) (side-effect-free error-free))
  (if test
      (or true-string "yes")
    (or false-string "no")))

;; --

(defun pel-key-binding-string (command)
  "Return a string describing the key binding for COMMAND."
  (require 'help)
  (substitute-command-keys (format "\\[%s]" command)))

;; ---------------------------------------------------------------------------
;;* Automated Mode Activation Check
;;  ===============================

(defun pel-modes-activating-symbol-name-for (minor-mode)
  "Return user-option symbol that set which major mode activates MINOR-MODE.

This is typically a symbol like:

- \\='pel-modes-activating-subword-mode : controls `subword-mode'
- \\='pel-modes-activating-dumb-jump    : controls `dumb-jump-mode'
- \\='pel-modes-activating-ggtags       : controls `ggtags-mode'

Ideally all minor-mode controlling PEL user-options would have a name that
ends with \\='-mode\\=' but it\\='s unfortunately not the case.
Use this function to return the appropriate symbol if one exists, otherwise
raise an error because the caller is trying to retrieve information that does
not exists."
  (let ((full-symbol (intern (format "pel-modes-activating-%s" minor-mode))))
    (if (boundp full-symbol)
        full-symbol
      (let ((partial-symbol (intern
                             (format "pel-modes-activating-%s"
                                     (pel-file-type-for minor-mode)))))
        (if (boundp partial-symbol)
            partial-symbol
          (progn
            (setq partial-symbol (intern
                                  (format "pel-modes-activating-%s"
                                          (pel-file-type-for minor-mode
                                                             "-minor-mode"))))
            (if (boundp partial-symbol)
                partial-symbol
              (error "\
CODE ERROR!! There's no PEL user-option that controls activation of %s!"
                     minor-mode))))))))


;; Note: `pel-activates-global-minor-modes' is a defcustom variable
;;       defined inside pel--options.el which requires pel--base.el (this
;;       file).  To prevent byte-compiler warnings the symbol is declared in a
;;       defvar form here.
(defvar pel-activates-global-minor-modes) ; forward declaration of user-option

(defun pel-minor-mode-auto-activated-by (minor-mode &optional
                                                    maj-mode
                                                    nil-return
                                                    show-all)
  "Check if the minor mode is auto-activated for specified major mode.

Return a string describing what PEL option, if any, forces
automatic activation of the MINOR_MODE in the major mode
specified by the MAJ-MODE, or the current major mode.  Also check
if the MINOR_MODE is activated globally via
`pel-activates-global-minor-modes'.

Note: the MINOR-MODE and MAJOR-MODE must evaluate to a valid mode
symbols.  These are normally symbols that have a name that ends with '-mode'.

if SHOW-ALL optional argument is non-nil, also list all major modes that
automatically activates this minor-mode.

If nothing automatically activates this minor mode, then return
nil or the value specified by NIL-RETURN if it is specified."
  (let* ((maj-mode (or maj-mode major-mode))
         (activating-it-option-symbol
          (pel-modes-activating-symbol-name-for minor-mode))
         (major-modes-activating-it (pel--symbol-value
                                     activating-it-option-symbol :quiet))
         (maj-mode-activates-minor-option-symbol (intern
                                                  (format
                                                   "pel-%s-activates-minor-modes"
                                                   (pel-file-type-for maj-mode))))
         (major-mode-activates (pel--symbol-value
                                maj-mode-activates-minor-option-symbol :quiet))
         (description ""))
    (when (and major-modes-activating-it
               (memq maj-mode major-modes-activating-it))
      (setq description
            (format "Activated by: %s%s"
                    activating-it-option-symbol
                    (if show-all
                        (format " : %s" major-modes-activating-it)
                      ""))))
    (when (and major-mode-activates
               (memq minor-mode major-mode-activates))
      (setq description (concat description
                                (format "%s%s%s."
                                        (if (string= description "")
                                            "Activated by: "
                                          " and: ")
                                        maj-mode-activates-minor-option-symbol
                                        (if show-all
                                            (format " : %s" major-mode-activates)
                                          "")))))
    (when (memq minor-mode pel-activates-global-minor-modes)
      (let* ((empty-description (string= description ""))
             (new-text
              (format
               "%s globally activated by pel-activates-global-minor-modes%s."
               (if empty-description
                   "Is"
                 "and is")
               (if show-all
                   (format " : %s" pel-activates-global-minor-modes)
                 ""))))
        (setq description (concat description new-text))))
    (or (unless (string= description "")
          description)
        nil-return)))

;; --

(defun pel-activated-in-str (activated-in)
  "Return a string describing ACTIVATED-IN list, empty string if it is nil.

When ACTIVATED_IN is not nil return a string that starts with
\" Auto-loaded in: \" followed by the elements of ACTIVATED-IN separated by
commas."
  (if activated-in
      (format " Auto-loaded in: %s"
              (pel-list-str activated-in))
    ""))

(defun pel-option-mode-state (mode user-option &optional activated-in buffer)
  "Return description of MODE status controlled by USER_OPTION.
USER-OPTION is a symbol.  A non-nil value of that symbol
identifies whether the mode is made available, nil that it is not made
available and most probably not loaded.
MODE is the mode symbol, indicating whether the mode is active or not.
If ACTIVATED-IN is specified that's the list of major modes where MODE
is automatically activated; this is included in the description.
The returned value is the state of the mode in the buffer identified
by BUFFER or `pel-insert-symbol-content-context-buffer'."
  (with-current-buffer (or buffer
                           pel-insert-symbol-content-context-buffer
                           (current-buffer))
    (let ((autoloaded-str (pel-activated-in-str activated-in)))
      (if (boundp user-option)
          (if (eval user-option)
              (if (boundp mode)
                  (format "%s%s"
                          (pel-symbol-on-off-string mode
                                                    "on"
                                                    "Available but off")
                          (if (pel-hastext autoloaded-str)
                              (format ". %s" autoloaded-str)
                            ""))
                (format "Available but not loaded, use a command to load it.%s"
                        autoloaded-str))
            (format "Not available. Activate %s first.%s"
                    (symbol-name user-option)
                    autoloaded-str))
        (format "%s symbol unknown" (symbol-name user-option))))))

;; ---------------------------------------------------------------------------
;;* String transformation utilities
;;  ===============================

(defun pel-as-string (val &optional as-character)
  "Return a string for the simple object value VAL.
VAL may be a string, a symbol, a number or a character.

Note: by default (pel-as-string ?A) returns \"65\", the character value
      because characters are numbers in Emacs Lisp.
      (pel-as-string ?A t) returns \"A\".

Signal an error if val is an object of another type."
  (cond
   ((stringp val) val)
   ((symbolp val) (symbol-name val))
   ((characterp val)
    ;; In Emacs Lisp characters are implemented as numbers.
    ;; (characterp ?a) and (numberp ?A) are both t.
    ;; User must set as-character to non-nil to get the character implementation.
    (if as-character
        (char-to-string val)
      (number-to-string val)))
   ((numberp val) (number-to-string val))
   (t (error "The pel-as-string does not support type of specified argument: %S" val))))

(defun pel-end-text-with-period (text)
  "Append a period character to TEXT if none is present at end of TEXT.
Return empty string if TEXT is the empty string."
  (declare (side-effect-free t))
  (if (> (length text) 0)
      (if (string= (substring text -1) ".")
          text
        (concat text "."))
    ""))

(defun pel-hastext (string)
  "Return t if STRING hold text, nil otherwise."
  (declare (pure t) (side-effect-free t))
  (not (string= string "")))

(defun pel-when-text-in (string value)
  "Return VALUE if STRING is a non-empty string.
Otherwise return nil."
  (declare (pure t) (side-effect-free t))
  (unless (string= string "")
    value))

(defun pel-string-or-nil (string)
  "Return a non-empty STRING unchanged, nil if string is empty."
  (declare (pure t) (side-effect-free t))
  (if (string= string "")
      nil
    string))

(defun pel-string-for (text)
  "Return TEXT if it's a string.  If nil return empty string."
  (declare (pure t) (side-effect-free t))
  (if text text ""))

(defun pel-string-when (condition &optional text)
  "Return TEXT (or CONDITION) when CONDITION is non-nil, empty string otherwise.
TEXT is optional, if it's nil CONDITION must be a string or nil."
  (declare (pure t) (side-effect-free t))
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

For TEXT, the returned string is \"\\\\(TEXT\\\\)\",
unless TAIL is specified, in which case tail is appended
after the closing parenthesis."
  (declare (side-effect-free t))
  (let ((str (format "\\(%s\\)" text)))
    (if tail
        (concat str tail)
      str)))


;; ---------------------------------------------------------------------------
;;* Message List formatting
;;  =======================

(defun pel--format-problem-messages (problems intro &optional extra-intro)
  "Return string describing PROBLEMS for INTRO.

The generated string starts with an introduction created using the
INTRO-FMT format string and its ARGS arguments if any.
The next line starts with the EXTRA-INTRO string if non-nil.
Then it lists the provided PROBLEMS list.

The function returns the formatted string.

Don't use this function directly; use the
`pel-format-problem-messages' macro instead: it simplifies
caller's code.

Example:

  ELISP> (pel--format-problem-messages \\='(\"problem 1\" \"problem 2\")
                                       \"System Test Report:\")
  \"System Test Report:
    The following 2 problems remain:
    - problem 1
    - problem 2\"

  ELISP> (pel--format-problem-messages \\='(\"problem 1\" \"problem 2\")
                                       \"System Test Report:\"
                                       \"The final report is that\")
  \"System Test Report:
    The final report is that the following 2 problems remain:
    - problem 1
    - problem 2\"

The second example shows where the EXTRA-INFO text is placed."
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
The next line starts with the EXTRA-INTRO string if non-nil.
Then it lists the provided PROBLEMS list.

This macro uses the function `pel--format-problem-messages' which
returns the formatted string."
  `(pel--format-problem-messages
    ,problems
    (format ,intro-fmt ,@args) ,extra-intro))

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
;;* Value check
;;  ===========

(defun pel-use-or (value check-function alternative &rest transform-functions)
  "Return VALUE if (CHECK-FUNCTION VALUE) is non-nil, else return ALTERNATIVE.

If there are any TRANSFORM-FUNCTIONS return a transformed VALUE by
calling the first function with VALUE as argument and then the second, etc...

For example, if t1, t2 and t3 are specified, the returned value is the result
of the following call sequence:

\(t1 (t2 (t3 VALUE))

Example:

    ELISP> (require \\='pel-text-transform)
    pel-text-transform

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
    0 (#o0, #x0, ?\\C-@)
    ELISP>"
  (if (funcall check-function value)
      (dolist (transform-fun transform-functions value)
        (setq value (funcall transform-fun value)))
    alternative))

;; ---------------------------------------------------------------------------
;;* Operations on sequences
;;  =======================

(defun pel-concat-strings-in-list (list)
  "Return the concatenation of all strings in the LIST of strings."
  (let ((acc "")
        elem)
    (while list
      (setq elem (car list))
      (setq list (cdr list))
      (setq acc (concat acc elem)))
    acc))

(defmacro pel-prepend-to (lst elems)
  "Prepend ELEMS to the beginning of LST.

Example:

  ELISP> (setq my-list \\='(1 2 3 4))
  (1 2 3 4)

  ELISP> (pel-prepend-to  my-list \\='(22))
  (22 1 2 3 4)

  ELISP> (pel-prepend-to  my-list \\='(33 44 55))
  (33 44 55 22 1 2 3 4)"
  `(setq ,lst (append ,elems ,lst)))

(defun pel-cons-alist-at (alist key val)
  "Prepend VAL to ALIST of list members at KEY.
If ALIST has no KEY, create an entry for KEY with (VAL) as KEY value.
REQUIREMENT:  ALIST must be a non-empty list.
SIDE EFFECT:  ALIST is modified destructively.
Return new value of ALIST.

Usage Example:

 ELISP> (setq al \\='((one (\"..\" \"[..]\"))))
 ((one
   (\"..\" \"[..]\")))

 ELISP> al
 ((one
   (\"..\" \"[..]\")))

 ELISP> (pel-cons-alist-at al \\='two \\='(\"aa\" \"[aa]\"))
 ((one
   (\"..\" \"[..]\"))
  (two
   (\"aa\" \"[aa]\")))

 ELISP> al
 ((one
   (\"..\" \"[..]\"))
  (two
   (\"aa\" \"[aa]\")))

 ELISP> (pel-cons-alist-at al \\='tree \\='(\"AA\" \"[AA]\"))
 ((one
   (\"..\" \"[..]\"))
  (two
   (\"aa\" \"[aa]\"))
  (tree
   (\"AA\" \"[AA]\")))

 ELISP> (pel-cons-alist-at al \\='one \\='(\",,\" \"[,,]\"))
 ((one
   (\",,\" \"[,,]\")
   (\"..\" \"[..]\"))
  (two
   (\"aa\" \"[aa]\"))
  (tree
   (\"AA\" \"[AA]\")))

 ELISP> (pel-cons-alist-at al \\='tree \\='(\"BB\" \"[BB]\" and-something-else))
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
  (if (> (length alist) 0)
      (let* ((value (assq key alist))
             (new-value (cons val (cdr value))))
        (if value
            (progn
              (setcdr (assq key alist) new-value)
              alist)
          (nconc alist (list (list key val)))))
    (error "Call to pel-cons-alist-at given an empty ALIST argument!")))


;; (credit to Drew Adams for this one)
(defun pel-nth-elt (element elements)
  "Return zero-indexed position of ELEMENT in ELEMENTS list, or nil if absent.

Example:
  ELISP> (pel-nth-elt \\='b \\='(a b c d))
  1"
  (let ((idx  0))
    (catch 'nth-elt
      (dolist (x elements)
        (when (equal element x) (throw 'nth-elt idx))
        (pel+= idx 1))
      nil)))

;; (credit to Metamorphic to the following 3 functions)
(defun pel-list-insert-before (lst idx item)
  "Return new list with ITEM before 0-base position index IDX of list LST.

Example:
  ELISP> (setq my-list \\='(a b c d))
  (a b c d)

  ELISP> (pel-list-insert-before my-list 1 \\='new)
  (a new b c d)

  ELISP> my-list
  (a b c d)

  ELISP> (pel-list-insert-before my-list -10 \\='other)
  (other a b c d)

  ELISP> (pel-list-insert-before my-list 0 \\='other)
  (other a b c d)

Note that with IDX 0 or smaller the item is placed at the beginning of the
list. The original LST is untouched."
  (if (>= idx (length lst))
      (error "Out-of-range index: idx:%d >= length(lst):%s" idx (length lst))
    (if (<= idx 0)
        (cons item lst)
      (cons (car lst)
            (pel-list-insert-before (cdr lst) (- idx 1) item)))))

(defun pel-list-prepend-nth (lst idx)
  "Return list LST with item at 0-base position IDX as first element.

Example:
   ELISP> (setq my-list \\='(a b c d))
   (a b c d)

   ELISP> (pel-list-prepend-nth my-list 2)
   (c a b d)

   ELISP> my-list
   (a b c d)

The original LST is untouched."
  (if (>= idx (length lst))
      (error "Out-of-range index: idx:%d >= length(lst):%s" idx (length lst))
    (if (<= idx 0)
        lst
      (let ((lx (pel-list-prepend-nth (cdr lst) (- idx 1))))
        (cons (car lx)
              (cons (car lst)
                    (cdr lx)))))))

(defun pel-list-insert-car-at (lst idx)
  "Return list LST with first element moved to the 0-base position IDX.

Example:
  ELISP> (setq my-list \\='(a b c d))
  (a b c d)

  ELISP> (pel-list-insert-car-at my-list 2)
  (b c a d)

  ELISP> my-list
  (a b c d)

The original LST is untouched."
  (pel-list-insert-before (cdr lst) idx (car lst)))

(defun pel-delqs (symbols seq)
  "Delete all SYMBOLS from SEQ, return modified SEQ."
  (dolist (s symbols seq)
    (setq seq (delq s seq))))

;; ---------------------------------------------------------------------------
;;* Operation on auto-mode-alist
;;  ============================

(defun pel-delete-from-auto-mode-alist (mode)
  "Delete MODE specific entries from `auto-mode-alist'.
Modifies `auto-mode-alist'."
  (while (rassoc mode auto-mode-alist)
    (setq auto-mode-alist
          (assq-delete-all (car (rassoc mode auto-mode-alist))
                           auto-mode-alist))))

;; ---------------------------------------------------------------------------
;;* PEL utils rebuild
;;  =================

(defun pel-rebuild-utils ()
  "Byte compile all elisp files inside PEL utils directory."
  (let ((utils-dirname (expand-file-name "utils" user-emacs-directory)))
    (if (file-directory-p utils-dirname)
        ;; byte-recompile-directory got a 4th argument in Emacs 28.1 as result of
        ;; fixing bug #10292.
        (if pel-emacs-28-or-later-p
            (with-no-warnings
              (byte-recompile-directory utils-dirname 0 :force :follow-symlink))
          (byte-recompile-directory utils-dirname 0 :force))
      (display-warning 'pel-rebuild-utils
                       (format "\
Skipping rebuild of utils file: utils directory does not exist: %s"
                               utils-dirname)
                       :warning))))

;; ---------------------------------------------------------------------------
;;* Tree-sitter major mode support
;;  ==============================

(defun pel-major-mode-use-tree-sitter (mode ts-mode)
  "Activate replacement of a MODE by a TS-MODE when tree-sitter is used.

MODE is the symbol of the standard, default mode.
TS-MODE is the symbol of the corresponding mode that uses tree-sitter.

Normally the difference between the 2 is the ts- prefix before
the word mode in the symbol name.  The function adds the pair to the
`major-mode-remap-alist' when it is available and returns the new value,
otherwise it returns nil."
  (when (and (boundp 'major-mode-remap-alist)
             pel-uses-tree-sitter)
    (when (not (assoc mode major-mode-remap-alist))
      (push (cons mode ts-mode) major-mode-remap-alist))
    ;; Always return the resulting alist when a mode was added or is in.
    major-mode-remap-alist))

(defun pel-major-ts-mode-supported-p (mode)
  "Return t when the specified tree-sitter major MODE is supported.

MODE must be a symbol that does NOT end with -mode.
The function returns nil when tree-sitter mode is not supported."
  (let ((the-ts-mode (intern (format "%s-ts-mode" mode))))
    (if (fboundp the-ts-mode)
        t
      (when (boundp 'major-mode-remap-alist)
        (let ((mode-symbol (intern  (format "%s-mode" (symbol-name mode)))))
          (assoc mode-symbol major-mode-remap-alist))))))

(defun pel-treesit-ready-p (language &optional quiet)
  "Check whether tree-sitter is ready to be used for MODE and LANGUAGE.

LANGUAGE is the language symbol to check for availability.
It can also be a list of language symbols.

If Emacs < 30 or tree-sitter is not ready, emit a warning and return
nil.  If the user has chosen to activate tree-sitter for LANGUAGE and
tree-sitter is ready, return non-nil.  If QUIET is t, don't emit a
warning in either case; if quiet is `message', display a message instead
of emitting a warning."
  (if (and pel-emacs-30-or-later-p
           (require 'treesit nil 'noerror)
           (fboundp 'treesit-ready-p))
      (treesit-ready-p language quiet)
    (unless quiet
      (display-warning 'pel-treesit-support
                       "Tree-Sitter is not supported in this Emacs."))
    nil))

(defun pel-ts-language-grammar-filename-for (mode)
  "Return the full path of the Tree-Sitter grammar file for MODE.

MODE must be a symbol that does NOT end with -mode.
Return nil if none found, or when tree-sitter is not supported."
  (let* ((fname (format "libtree-sitter-%s.%s" mode pel-os-lib-file-extension))
         (found-path nil)
         (ts-dirpath (when (boundp 'treesit-extra-load-path)
                       treesit-extra-load-path)))
    (dolist (rootdir ts-dirpath)
      (unless found-path
        (with-no-warnings
          ;; directory-files-recursively has only 5 args in later
          ;; versions of emacs, but in older ones, tree-sitter is
          ;; not supported.
          (setq found-path (directory-files-recursively rootdir fname
                                                        nil nil
                                                        :follow-symlinks)))))
    (when found-path
      (file-truename (car-safe found-path)))))

(defun pel-file-md5 (filename)
  "Return the MD5 hash digest of FILENAME.
FILENAME is the file path."
  (with-temp-buffer
    (insert-file-contents filename)
    (secure-hash 'md5 (current-buffer))))

(defun pel-ts-language-grammar-status-for (mode &optional line-sep)
  "Return a string describing Tree-Sitter language grammar state for MODE.

MODE must be a symbol that does NOT end with -mode.
If a LINE-SEP is specified, print the name of the tree-sitter grammar file
following that line separator."
  (if (and (pel-treesit-ready-p mode :quiet)
           (fboundp 'treesit-language-abi-version))
      (let ((lines nil))
        (push (format
               "Tree-Sitter language grammar for %s uses: ABI version %d."
               mode
               (treesit-language-abi-version mode))
              lines)
        (when line-sep
          (let ((grammar-fname (pel-ts-language-grammar-filename-for mode))
                (attrs nil))
            (push line-sep lines)
            (push (format "Tree-Sitter language grammar file for %s: %s"
                          mode
                          (or grammar-fname "not found!"))
                  lines)
            (when grammar-fname
              (setq attrs (file-attributes grammar-fname 'string))
              (push (format
                     "
\t\t\t\t\t\tfile size: %s bytes, modified: %s, modes: %s
\t\t\t\t\t\t      md5: %s"
                     (file-attribute-size attrs)
                     (format-time-string "%Y-%m-%d %H:%M:%S"
                                         (file-attribute-modification-time
                                          attrs))
                     (file-attribute-modes attrs)
                     (pel-file-md5 grammar-fname))
                    lines))))
        (mapconcat #'identity (nreverse lines) "")) ; sep needed in old emacs
    (format "Tree-Sitter language grammar is NOT available for %s." mode)))

;; ---------------------------------------------------------------------------
;;* Mode argument interpretation
;;  ============================

(defun pel-action-for (action current-state)
  "Return \\='activate, \\='deactivate, nil for requested ACTION on CURRENT-STATE.

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
- \\='activate   : need to activate it
- \\='deactivate : need to deactivate it."
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
;;* Toggle a local mode
;;  ===================

(defun pel-toggle-mode (mode)
  "Toggle the specified MODE (a symbol).
Return the new state of the mode: t if active, nil otherwise.
If the mode function is an autoload and not yet loaded the file
is loaded and the mode activated.
Note that MODE must NOT be a symbol of a lexical bound variable,
it must be a global mode symbol."
  (unless (symbolp mode)
    (error "Nothing done: pel-toggle-mode expects a symbol as argument"))
  ;; Some modes define their state variables only when they are first ran.
  ;; For those allow calling the function with an argument 1 when their
  ;; variable is still not yet bound.
  (let ((mode-function (symbol-function mode)))
    (when (and mode-function
               (autoloadp mode-function))
      (autoload-do-load mode-function))
    (funcall mode-function (if (and (boundp mode)
                                    (symbol-value mode))
                               -1
                             1))))

(defun pel-toggle-mode-and-show (mode &optional on-string off-string)
  "Toggle specified MODE (a symbol), and show it\\='s new value.
If ON-STRING and OFF-STRING arguments are specified use them as the
on/off value, otherwise use \"on\" and \"off\".
The function issue an error if the argument is not a symbol."
  (pel-toggle-mode mode)
  (message (pel-symbol-text mode on-string off-string)))

;; ---------------------------------------------------------------------------
;;* Toggle of values and variables
;;  ==============================
;;
;; To toggle the value of variable that would have
;; the hypothetical name is-acceptable we can use
;; the following calls:
;;                     (pel-toggle 'is-acceptable)
;;                     (pel-toggle-and-show 'is-acceptable)
;;
;; Notice the required quoting.

(defun pel-toggle (symbol)
  "Toggle value of SYMBOL from nil to/from t. Return SYMBOL\\='s new value.
For example, to toggle the value of a variable  named isok,
the caller must pass it quoted.
Return the new SYMBOL value.
The function issue an error if the argument is not a symbol."
  (if (symbolp symbol)
      (set symbol (not (eval symbol)))
    (error "Nothing done: pel-toggle expects a symbol as argument")))

(defun pel-toggle-and-show (symbol &optional on-string off-string locally name)
  "Toggle value of SYMBOL from nil to/from t, and show it\\='s new value.
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
               (format "%s is now%s %s"
                       name
                       pel--prompt-separator
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
  (declare (pure t) (side-effect-free t))
  (or val default))

;; ---------------------------------------------------------------------------
;;* Symbol processing
;;  =================

(defun pel-hook-symbol-for (mode)
  "Return the hook symbol for the specified MODE symbol."
  (intern (format "%s-hook" (symbol-name mode))))


(defun pel-map-symbol-for (mode)
  "Return the map symbol for the specified MODE symbol."
  (intern (format "%s-map" (symbol-name mode))))

;; ---------------------------------------------------------------------------
;;* Hook control
;;  ============

(defun pel-add-hook-for (modes-list-symbol func &optional allowed-modes)
  "Add the FUNC hook to all modes listed in the MODES-LIST-SYMBOL.

Do not use a lambda for FUNC as `add-hook' won't be able to prevent duplicate
entries of lambda inside the hook list.

When ALLOWED-MODES is specified the accepted mode list symbols in
MODES-LIST-SYMBOL is restricted to the ones in ALLOWED-MODES.
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
       (format "\
The mode %s identified in the list %s is not allowed,
according to the list of allowed-modes (%s) identified.
Please update the content of the %s user-option.
You can do it with \\='M-x customize %s\\='."
               mode
               modes-list-symbol
               allowed-modes
               modes-list-symbol
               modes-list-symbol)
       :error))))

;; ---------------------------------------------------------------------------
;;* Minor mode activation
;;  =====================

(defun pel--check-minor-modes-in (list-var minor-modes)
  "Check validity of all MINOR-MODES specified in the LIST-VAR.
LIST-VAR is the symbol of the variable holding MINOR-MODES.
MINOR-MODES is the list of minor modes symbols.
Generate a warning if any symbol in the MINOR-MODES list is not a valid
mode switching symbol."
  (let ((error-count 0))
    (dolist (minor-mode minor-modes)
      (unless (and (symbolp minor-mode)
                   (fboundp minor-mode)
                   (commandp minor-mode))
        (pel+= error-count 1)
        (display-warning 'pel-invalid-mode-symbol
                         (format "Invalid mode symbol in %s: %S"
                                 list-var minor-mode)
                         :error)))
    (when (> error-count 0)
      (display-warning
       'pel-invalid-mode-symbol
       (format
        "Please fix the above errors in the %s customization user-option."
        list-var)
       :error))
    error-count))

(defmacro pel-check-minor-modes-in (minor-modes)
  "Check validity of minor-modes listed in MINOR-MODES list.
The MINOR-MODES argument must be an unquoted symbol."
  `(pel--check-minor-modes-in (quote ,minor-modes) ,minor-modes))


(defun pel-turn-on-global-minor-modes-in (minor-modes)
  "Turn all *global* MINOR-MODES on for all buffers.

MINOR-MODES must be a symbol.  In PEL that should be
`pel-activates-global-minor-modes'.

The function generates a warning describing the problem if a
local minor mode is specified instead of a global minor mode."
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
  "Turn all *local* MINOR-MODES on for the buffer\\='s major mode.

MINOR-MODES must be a symbol.  In PEL that should be one of the
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
;;* Argument converter
;;  ==================

(defun pel-multiplier (positive)
  "Return a positive value 1 if POSITIVE is non-nil, -1 otherwise."
  (declare (pure t) (side-effect-free error-free))
  (if positive 1 -1))

(defalias 'pel-mode-toggle-arg 'pel-multiplier
  "Convert a boolean value to the value required by mode toggling functions.")

;; ---------------------------------------------------------------------------
;;* Iteration helpers
;;  =================
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
N can be a symbol.  In that case it\\='s value is updated.
If FLOOR is specified that\\='s the smallest allowed value for N.
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
Return N when it\\='s smaller than CEILING.
Return nil if symbol N value is CEILING or larger."
  (let ((oldvalue (eval n)))
    (if (< oldvalue (or ceiling most-positive-fixnum))
        (set n (1+ oldvalue)))))

;; ---------------------------------------------------------------------------
;;* Swap 2 values
;;  =============

(defmacro pel-swap (var-a var-b)
  "Swap the content of VAR-A and VAR-B.  Return value of VAR-A."
  `(setq ,var-a (prog1 ,var-b (setq ,var-b ,var-a))))

;; ---------------------------------------------------------------------------
;;* Text at point
;;  =============
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
  (declare (side-effect-free t))
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
;;* Calling functions
;;  =================

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
;;* Moving Point
;;  ============
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
  (if line
      (pel-goto-line line))
  (if column
      (move-to-column column)))

;; ---------------------------------------------------------------------------
;;* Line position
;;  =============

(defun  pel-same-line-p (p1 p2)
  "Return t if P1 and P2 positions are on the same line, nil otherwise."
  (save-excursion
    (eq (progn (goto-char p1)
               (forward-line 0)
               (point))
        (progn (goto-char p2)
               (forward-line 0)
               (point)))))

;; ---------------------------------------------------------------------------
;;* Identifying region
;;  ==================

(defun pel-region-for (start-str end-str &optional pos)
  "Return the position of the beginning of delimited region.
The delimiters are START-STR and END-STR.
Search at POS if specified, otherwise search around point.
Include whole lines.
Return a (start . end) cons cell if found, otherwise return nil."
  (setq pos (or pos (point)))
  (save-excursion
    (let (beg end)
      (when (search-backward start-str nil 'noerror)
        (beginning-of-line 1)
        (setq beg (point))
        (when (search-forward end-str nil 'noerror)
          (end-of-line 1)
          (setq end (point))
          (cons beg end))))))

;; ---------------------------------------------------------------------------
;;* Insert or overwrite text
;;  ========================

(defun pel-insert-or-overwrite (text)
  "Insert or overwrite TEXT depending of variable `overwrite-mode' status.
TEXT can be a single character or a string.
Multi-byte characters are handled properly."
  (when overwrite-mode
    (if (stringp text)
        (delete-char (length text))
      (delete-char 1)))
  (insert text))

;; ---------------------------------------------------------------------------
;;* Extract text from buffer
;;  ========================

(defun pel-text-from-beginning-of-line (&optional with-properties)
  "Return text string between beginning of line and point.
If WITH-PROPERTIES is non-nil the returned value includes the text properties,
otherwise it does not."
  (declare (side-effect-free t))
  (let ((begin (line-beginning-position))
        (end   (point)))
    (if with-properties
        (buffer-substring begin end)
      (buffer-substring-no-properties begin end))))

;; ---------------------------------------------------------------------------
;;* Check text in buffer
;;  ====================

(defun pel-line-has-only-newline-p (&optional pos)
  "Return t if current line (or line at POS) is empty, nil otherwise."
  (save-excursion
    (when pos (goto-char pos))
    (= (progn (beginning-of-line) (point))
       (progn (end-of-line) (point)))))

(defun pel-line-has-only-whitespace-p (&optional pos)
  "Return t if current line (or line at POS) contain only whitespace.
Return nil otherwise.
Whitespace characters are specified by the syntax table of the
current major mode."
  (save-excursion
    (when pos (goto-char pos))
    (beginning-of-line)
    (let ((eol (save-excursion (progn (end-of-line)
                                      (point)))))
      (not (re-search-forward "[^ \t]" eol t)))))

(defun pel-inside-code (&optional pos)
  "Return non-nil when point or POS is in code, nil if in comment or string.
Note that this changes the search match data!"
  (let ((syntax (syntax-ppss (or pos (point)))))
    (and (not (nth 3 syntax))
         (not (nth 4 syntax)))))

(defun pel-has-shebang-line ()
  "Return t if buffer has shebang line, nil if it has none."
  ;; Simply check if the first 2 characters in the buffer are "#!"
  (save-excursion
    (goto-char (point-min))
    (looking-at "#!")))

;; ---------------------------------------------------------------------------
;;* File Path processing
;;  ====================

(defun pel-file-in (fname file-dir-list)
  "Check if FNAME matches FILE-DIR-LIST, an inclusion list.

The FILE-DIR-LIST is a list of file names and directory tree names, where
each entry has a absolute path possibly with the tilde (~) to identify the
home directory.  Check if FNAME is found in the list or is inside one of the
specified directory tree, returning the file name or directory name where it
is found.  Return nil if it is not found."
  (let ((fpath (expand-file-name fname)))
    (catch 'match
      (dolist (found-in file-dir-list)
        (let ((found-in-abs (expand-file-name found-in)))
          (if (file-directory-p found-in-abs)
              ;; When checking if fname is the directory or in the tree
              ;; ensure that both strings end with a directory separator
              ;; to reject a fname that would have the same path name as
              ;; the directory but would not have the trailing separator.
              (when (string-match-p
                     (format "^%s"
                             (regexp-quote (file-name-as-directory found-in-abs)))
                     (file-name-as-directory fpath))
                (throw 'match found-in))
            (when (string= found-in-abs fpath)
              (throw 'match found-in))))))))


(defun pel-normalize-fname (name)
  "Normalize file (or directory) NAME.

Normalize a directory or file name.  Ensure that the directory
name does not end with a slash, that it uses the file true name
and use Unix-style formatting.  The function replaces a symlink
by the file it points to.  The function does *not* expand
environment variables that may be in the string."
  (directory-file-name (file-truename name)))

(defun pel-is-subdir-of (path1 path2)
  "Return t if PATH1 is a sub-directory of PATH2."
  (let ((npath1 (pel-normalize-fname path1))
        (npath2 (pel-normalize-fname path2)))
    (when (string-match-p (regexp-quote npath2) npath1)
      t)))

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
  (declare (side-effect-free t))
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
;;* Insertion of text in current buffer
;;  ===================================

(defun pel-insert-bold (text)
  "Insert bold TEXT at point."
  (insert (propertize text 'face 'bold)))

(defun pel-insert-url-link (title url &optional extra-text)
  "Insert a TITLE hyperlink button to specified URL.

If EXTRA-TEXT is non-nil it should be a string and is inserted after the
button."
  (if (and (require 'button nil 'noerror)
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
      (if (and (require 'button nil 'noerror)
               (fboundp 'insert-button))
          ;; When a button is required, ensure that it describes the variable
          ;; in the context of the original buffer, not the info buffer.
          (let ((context-buffer pel-insert-symbol-content-context-buffer))
            (insert-button name 'action (lambda (_s)
                                          (describe-symbol symbol
                                                           context-buffer))))
        (insert name)))))



(defun pel-insert-symbol-content (symbol &optional
                                         buffer on-same-line
                                         no-button description no-value)
  "Insert the name followed by the content of the specified SYMBOL.

Insert the SYMBOL name as a clickable button unless NO-BUTTON is non-nil.
Print the symbol value unless NO-VALUE is non-nil.

By default SYMBOL must be a global symbol as its value is read in
the scope of the output buffer.  If the SYMBOL is a buffer local
symbol, specify the buffer in the optional BUFFER argument or
let-bind the variable `pel-insert-symbol-content-context-buffer'
to the value of the buffer you need.  A non-nil BUFFER argument value
takes precedence to the value of the variable
`pel-insert-symbol-content-context-buffer'.  If both are nil, then
the value is read from the context of the current buffer, which
may be a local or global.

By default:
- the value is printed on the line after the variable name, unless
  ON-SAME-LINE is set,
- the variable name is a clickable button, unless NO-BUTTON is set,
- the symbol name is printed as the title unless a DESCRIPTION is
  specified (in that case the NO-BUTTON is also set: the description
  is never made clickable)."
  (let ((value (pel-symbol-value
                symbol
                (or buffer pel-insert-symbol-content-context-buffer)))
        (name  (or description (symbol-name symbol))))
    (insert "\n- ")
    (if description
        (insert description)
      (pel-insert-symbol symbol (or no-button description)))
    (unless no-value
      (insert (format "%s:%s"
                      (make-string (max 0 (- 40 (length name))) ?\s)
                      (if on-same-line " " "\n")))
      (if (and (symbolp value)
               (not (memq value '(t nil)))
               (or (boundp value)
                   (fboundp value)))
          (pel-insert-symbol value no-button)
        (insert (format "%s" value))))))

(defun pel-insert-symbol-content-line (symbol &optional buffer extra-text)
  "Insert the name followed by the content of the specified SYMBOL.

Insert the SYMBOL name as a clickable button inside current buffer.
By default SYMBOL must be a global symbol as its value is read in the scope
of the output buffer.  If the SYMBOL is a buffer local symbol, specify the
buffer in the optional BUFFER argument.  You can also let-bind the variable
`pel-insert-symbol-content-context-buffer' to the value of the buffer you
need.

If EXTRA-TEXT is non-nil, it can be a string or a function:
- A string is inserted after the symbol name.
- A function is used to convert the value into a description string
  and that string is printed after the symbol."
  (pel-insert-symbol-content
   symbol
   buffer
   :on-same-line)
  (when extra-text
    (if (stringp extra-text)
        (insert (format  "  : %s" extra-text))
      (insert " : ")
      (if (boundp symbol)
          (progn
            (insert (funcall
                     extra-text
                     (pel-symbol-value
                      symbol
                      (or buffer pel-insert-symbol-content-context-buffer)))))
        (insert (funcall extra-text symbol))))))

(defun pel-insert-mode-symbol-content-line (symbol-format-string)
  "Insert the name and value of a major mode specific symbol.

Create a major mode specific symbol using SYMBOL-FORMAT-STRING string.
That symbol is assumed bound in the current major mode.
The SYMBOL-FORMAT-STRING string must contain one \"%s\" format specifier
and is used by a call to `pel-major-mode-symbol-for' to build the major
mode symbol.
The inferred mode-specific symbol is assumed bound in the current major mode.
If it is not bound, the use of this function will signal a symbol unbound
error."
  (let ((mode-symbol (pel-major-mode-symbol-for symbol-format-string)))
    (pel-insert-symbol-content-line mode-symbol)))

(defun pel-insert-mode-symbol-content-line-when-bound (symbol-format-string)
  "Insert the name and value of a major mode specific symbol if it is bound.

Create a major mode specific symbol using SYMBOL-FORMAT-STRING string.
The SYMBOL-FORMAT-STRING string must contain one \"%s\" format specifier
and is used by a call to `pel-major-mode-symbol-for' to build the major
mode symbol.  If that symbol is bound: insert a line showing the name
and its value.  If that symbol is not bound, do nothing."
  (let ((mode-symbol (pel-major-mode-symbol-for symbol-format-string)))
    (when (boundp mode-symbol)
      (pel-insert-symbol-content-line mode-symbol))))

(defun pel-line-prefixed-with (text prefix)
  "Return TEXT with each line prefixed with PREFIX string."
  (mapconcat (lambda (line) (concat prefix line))
             (split-string text "\n")
             "\n"))

(defun pel--pp (object &optional stream prefix)
  "Pretty-print OBJECT on STREAM if specified, standard output otherwise.

A non-nil PREFIX should a be a string that is inserted before the
OBJECT."
  (if (and (require 'pp nil 'noerror)
           (fboundp 'pp-to-string))
      (let ((text (string-trim (pp-to-string object))))
        (princ (pel-line-prefixed-with text (or prefix ""))
               stream))
    (princ object stream)))

(defun pel-insert-list-value (list-name list-value
                                        &optional without-index on-same-line)
  "Insert a description of a list variable with a name and value.

- LIST-NAME  : the name of the list.
- LIST-VALUE : the value of the list.
- WITHOUT-INDEX : by default an index is printed unless this is non-nil.
- ON-SAME-LINE : prints all list elements on the same line by default, unless
                 this is non-nil, which forces placing each element on
                 its own line."
  (insert (format "\n- %s: " list-name))
  (let ((idx 0))
    (dolist (elem list-value)
      (pel+= idx 1)
      (unless without-index
        (insert (format "\n%3d -%s" idx (if on-same-line "" "\n"))))
      (pel--pp elem (current-buffer) "   "))))

(defun pel-insert-symbol-list (symbol-list &optional
                                           line-width )
  "Insert a list of symbols over a set of wrapped lines.
The list of symbol is in SYMBOL-LIST and the maximum line width is LINE-WIDTH."
  (let ((w 12)
        (scount (length symbol-list)))
    (insert (format "- %2d modes: " scount))
    (dolist (s symbol-list)
      (setq w (+ w (length (symbol-name s)) 2))
      (pel-insert-symbol s)
      (setq scount (1- scount))
      (if (> scount 0)
          (insert ", ")
        (insert "."))
      (when (>= w line-width)
        (setq w 1)
        (insert "\n ")))))
(defun pel-insert-list-content (symbol &optional
                                       buffer without-index
                                       no-button on-same-line)
  "Insert a description of the content of the list identified by its SYMBOL.

Insert the SYMBOL name as a clickable button unless NO-BUTTON is non-nil.
By default SYMBOL must be a global symbol as its value is read in the scope
of the output buffer.  If the SYMBOL is a buffer local symbol, specify the
buffer in the optional BUFFER argument.

By default, each element of the list is printed on a new line preceded by an
element index number unless WITHOUT-INDEX is non-nil.
By default, the index is printed on a line above the value, unless
ON-SAME-LINE is non-nil"
  (let ((list-value (when (boundp symbol)
                      (pel-symbol-value
                       symbol
                       (or buffer
                           pel-insert-symbol-content-context-buffer)))))
    (if (null list-value)
        (pel-insert-symbol-content
         symbol
         (or buffer
             pel-insert-symbol-content-context-buffer)
         :on-same-line
         no-button)
      (insert "\n- ")
      (pel-insert-symbol symbol no-button)
      (insert ":")
      (let ((idx 0))
        (dolist (elem list-value)
          (pel+= idx 1)
          (unless without-index
            (insert (format "\n%3d -%s" idx (if on-same-line "" "\n"))))
          (if (and (symbolp elem)
                   (not (eq elem t))
                   (not (eq elem nil))
                   (or (fboundp elem)
                       (boundp elem)))
              (progn
                (insert "   ")
                (pel-insert-symbol elem no-button))
            (pel--pp elem (current-buffer) "   ")))))))

;; ---------------------------------------------------------------------------
;;* Move point right, optionally inserting spaces
;;  =============================================

(defun pel-move-right-by (n)
  "Move point N columns to the right, inserting spaces if needed.
The spaces are inserted when current line is not long enough.
At the end of buffer, a newline and spaces are inserted to reach
the relatively specified column."
  (let* ((current-col (current-column))
         (target-col (+ current-col n))
         (end-of-buffer-p (eobp)))
    (save-excursion
      ;; Ensure the line is long enough or extend with spaces
      (move-to-column target-col t)
      (let ((actual-col (current-column)))
        ;; If we are still not at the target column, or if we were at the EOB
        ;; and need to move further, insert more spaces and newlines if necessary.
        (when (< actual-col target-col)
          (let ((spaces-to-insert (- target-col actual-col)))
            (when end-of-buffer-p
              ;; Insert a newline if it's the very end of the buffer to start a new line
              (insert "\n"))
            (insert (make-string spaces-to-insert ?\s))))))
    ;; Finally, move the cursor to the target column
    (move-to-column target-col t)))

;; ---------------------------------------------------------------------------
;;* Print in dedicated buffer
;;  =========================

(defun pel-print-in-buffer (bufname
                            title
                            text
                            &optional clear-buffer use-help-mode show-top)
  "Print TITLE and TEXT inside specified buffer BUFNAME.

TEXT can be:
- A *string* that is printed verbatim inside the buffer.
- A *function* that calls insert to insert the strings into the buffer.
  Such a function can take use the `pel-insert-symbol-content-line'
  function to insert clickable buttons for the name of user-options.
  This way the user can see the value and click on these buttons to
  change the values of the user options.

By default, `pel-print-in-buffer' appends the text in buffer
unless CLEAR-BUFFER is non-nil, in which case, previous buffer
content is first erased.

When USE-HELP-MODE is non-nil, activate the `help-mode' in the
buffer, otherwise use the `fundamental-mode'.  Activating the
`help-mode' in the buffer allows quick navigation to the variable
buttons, a very useful feature.

By default `pel-print-in-buffer' moves point to the top of the last
report.  If SHOW-TOP is non nil it moves point to the top of buffer
instead."
  (let* ((current-buffer-name (buffer-name))
         (outbuf (get-buffer-create bufname))
         (top-line (format "----%s from %s --- %s -----\n"
                           title
                           current-buffer-name
                           (format-time-string "%A, %B %d, %Y @ %T"))))
    (with-current-buffer outbuf
      (when (and use-help-mode
                 (eq major-mode 'help-mode))
        (fundamental-mode)
        (when buffer-read-only
          (read-only-mode -1)))
      (when clear-buffer
        (erase-buffer))
      (goto-char (point-max))
      (insert (propertize top-line 'face 'bold))
      (cond ((stringp text)
             ;; using format twice because it prints an escaped % ("%%")
             ;; with 2 percent characters instead of only one if it is not
             ;; processed by another format call.
             (insert (format (format "%s\n\n" text))))
            ((functionp text)
             (funcall text))
            (t (error "Invalid type for text: %S" text)))
      (insert "\n")
      (when use-help-mode
        (help-mode)))
    ;; display the end part of the buffer showing comment variables
    ;; move the last line of text to the bottom line of the window
    (with-selected-window (display-buffer outbuf)
      (if show-top
          (goto-char (point-min))
        ;; when not showing top, show the beginning of the last created report.
        (goto-char (point-max))
        (search-backward top-line)
        (recenter-top-bottom 0)))))

;; ---------------------------------------------------------------------------
;;* Code Parsing Support
;;  ====================

(defun pel-point-in-comment-or-docstring (&optional move-fct)
  "Return position of start of comment or docstring surrounding point.
Return nil when point is outside comment and docstring.  If a
MOVE-FCT function is specified, call it before checking the state
of point."
  (save-excursion
    (when move-fct
      (funcall move-fct))
    (nth 8 (parse-partial-sexp (point-min) (point)))))


;; ---------------------------------------------------------------------------
;;* Byte-compilation
;;  ================

(defun pel-modtime-of (filename)
  "Return the modification time of FILENAME."
  (file-attribute-modification-time (file-attributes filename)))

(defun pel-byte-compile-if-needed (el-filename &rest other-dependencies)
  "Byte-compile Emacs Lisp source EL-FILENAME if it is needed.
The EL-FILENAME string must be the name of a Emacs Lisp file and must
include the .el extension.  The name of the file may be relative
or absolute.
The file is byte compiled if it is newer than its byte-compiled
output file (a file with the .elc extension) or if the .elc file
does not exist.
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
;;* Imenu Utilities
;;  ===============

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
;;* Tags support
;;  ============

(defun pel-visit-tags (tags-files)
  "Visit the TAGS files identified in the TAGS-FILES list and the local one."
  (let ((local-tags-fname (locate-dominating-file default-directory "TAGS")))
    (when local-tags-fname
      (visit-tags-table local-tags-fname)))
  (dolist (fname tags-files)
    (when (file-exists-p fname)
      (visit-tags-table fname))))

;; ---------------------------------------------------------------------------
;;* Portability
;;  ===========
;;
;; Use the following functions to prevent warnings.
;; Select one that supports all required versions:
;;
;; - `with-no-warnings':         introduced in Emacs 22.1
;; - `with-suppressed-warnings': introduced in Emacs 27.1

(defun pel-executable-find (command &optional is-remote)
  "Search COMMAND in variable `exec-path', return its absolute file name.

Return nil if COMMAND is not found in the identified directories.
If IS-REMOTE is non-nil:
- on Emacs >= 27: search on the remote host indicated by `default-directory'.
- on Emacs < 27: issue user error describing limitation."
  (if pel-emacs-27-or-later-p
      (with-no-warnings
        (executable-find command is-remote))
    (if is-remote
        (user-error "On Emacs %s can't search for remote file [%s]"
                    emacs-major-version
                    command)
      (executable-find command))))

(defun pel-treesit-language-available-p (language)
  "Return non-nil if tree-sitter LANGUAGE exists and is loadable.
Returns nil when Emacs does not support tree-sitter."
  (and (fboundp 'treesit-language-available-p)
       (treesit-language-available-p language)))

(defun pel-emacs-config-features-string ()
  "Print the names of all Emacs configured compilation features."
  (format "%s. With%s native compilation.%s" system-configuration-features
          (if pel-emacs-with-native-comp-p "" "out")
          (format
           " With%s D-Bus support." (if  (featurep 'dbusbind) "" "out"))))

(defun pel-hardware-model-string ()
  "Return a string describing the computer hardware model."
  ;; This uses OS-specific commands via shell
  (cond
   ;; macOS/FreeBSD
   ((or pel-system-is-macos-p
        pel-system-is-FreeBSD-p)
    (format "%s:  %s"
            (pel-path-strip (shell-command-to-string "sysctl -n hw.model"))
            (shell-command-to-string "uname -a")))
   ;;
   ;; Linux
   (pel-system-is-linux-p
    (format "%s:  %s"
            (pel-path-strip (shell-command-to-string "cat /sys/class/dmi/id/product_name"))
            (shell-command-to-string "uname -a")))
   ;;
   ;; Windows NT (not MS-DOS)
   ((eq system-type 'windows-nt)
    (shell-command-to-string "wmic computersystem get model"))
   ;;
   ;; No other support yet
   (t
    (format "Hardware model retrieval not yet supported for %s." system-type))))

(defun pel-eglot-active-p ()
  "Return t if `eglot' is used in the current buffer, nil otherwise."
  (and (fboundp 'eglot-managed-p)
       (eglot-managed-p)))

;;; --------------------------------------------------------------------------
(provide 'pel--base)

;;; pel--base.el ends here

;;; pel--options.el --- PEL Customization Options -*-lexical-binding: t-*-

;; Copyright (C) 2020, 2021, 2022, 2023, 2024, 2025  Pierre Rouleau

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
;; Defines the PEL options.  All the options using the  'pel-use-' prefix
;; identify functionality (and potentially also a Emacs Lisp library) that can
;; be used by PEL.  It is used when the option is set to `t' and not used when
;; set to `nil'.  This way, the user can choose what is provided by PEL simply
;; by customizing PEL.
;;
;; The pel group customization tree, inside emacs/convenience, contains the
;; following group hierarchy:
;;
;; - pel
;;   - pel-base-emacs
;;     - pel-pkg-for-parser
;;       - pel-pkg-for-tree-sitter
;;   - pel-syntax-tools
;;   - pel-fast-startup
;;   - pel-package-use
;;     - pel-pkg-package-mng
;;     - pel-pkg-for-align
;;     - pel-pkg-for-bookmark
;;     - pel-pkg-for-buffer
;;       - pel-pkg-for-ibuffer
;;     - pel-pkg-for-completion
;;     - pel-pkg-for-cursor
;;       - pel-pkg-for-iedit
;;     - pel-pkg-for-cut-and-paste
;;     - pel-pkg-for-data-files
;;     - pel-pkg-for-diff-merge
;;     - pel-pkg-for-dired
;;     - pel-pkg-for-expand
;;     - pel-pkg-for-conf-file
;;     - pel-pkg-for-filemng
;;       - pel-pkg-for-browse
;;         - pel-pkg-for-file-browse
;;           - pel-pkg-for-neotree
;;           - pel-pkg-for-ztree
;;         - pel-pkg-for-web-browse
;;       - pel-pkg-for-ssh
;;       - pel-pkg-for-x509-file
;;     - pel-pkg-for-frame
;;     - pel-pkg-for-graphics-emacs
;;       - pel-pkg-for-graphics-cursor
;;     - pel-pkg-for-grep
;;     - pel-pkg-for-help
;;     - pel-pkg-for-hide-show
;;     - pel-pkg-for-highlight
;;       - pel-pkg-for-iedit
;;       - pel-pkg-for-modeline
;;     - pel-pkg-for-imenu
;;     - pel-pkg-for-indentation
;;     - pel-pkg-for-insertions
;;       - pel-pkg-for-license-insertion
;;       - pel-text-insertions
;;         - pel-date-time-insertion
;;       - pel-pkg-for-parens
;;     - pel-pkg-for-kbmacro
;;     - pel-pkg-for-key-chord
;;     - pel-pkg-for-keys
;;     - pel-keypad-keys
;;     - pel-pkg-for-log-file
;;     - pel-pkg-for-object-file
;;     - pel-pkg-for-marking
;;     - pel-pkg-for-markup
;;       - pel-pkg-for-asciidoc
;;       - pel-pkg-for-drawing-markup
;;         - pel-pkg-for-graphviz-dot
;;         - pel-pkg-for-mscgen
;;         - pel-pkg-for-plantuml
;;       - pel-pkg-for-markdown
;;       - pel-pkg-for-outline
;;         - pel-pkg-for-org-mode
;;       - pel-pkg-for-rst
;;         - pel-rst-style
;;       - pel-pkg-for-xml
;;       - pel-pkg-for-yaml
;;         - pel-pkg-for-cwl
;;     - pel-pkg-for-modeline
;;     - pel-pkg-for-navigation
;;       - pel-pkg-for-xref
;;     - pel-pkg-for-parser
;;     - pel-pkg-for-programming-languages
;;       - pel-pkg-for-all-languages
;;         - pel-pkg-for-parens
;;         - pel-pkg-for-syntax-check
;;         - pel-pkg-for-language-server
;;         - pel-pkg-generic-code-style
;;           - pel-sh-script-skeleton-control
;;       - pel-pkg-for-hardware-description-languages
;;         - pel-use-verilog
;;         - pel-use-vhdl
;;       - pel-pkg-for-software-programming-languages
;;         - pel-pkg-for-ada
;;         - pel-pkg-for-applescript
;;         - pel-pkg-for-cc
;;           - pel-pkg-for-awk
;;           - pel-pkg-for-c
;;             - pel-c-code-style
;;               - pel-c-skeleton-control
;;                 - pel-c-module-header-skeleton-control
;;                 - pel-c-function-header-skeleton-control
;;             - pel-pkg-for-bison
;;           - pel-pkg-for-c++
;;             - pel-c++-code-style
;;               - pel-c++-skeleton-control
;;                 - pel-c++-module-header-skeleton-control
;;                 - pel-c++-function-header-skeleton-control
;;           - pel-pkg-for-d
;;             - pel-d-code-style
;;         - pel-pkg-for-eiffel
;;         - pel-pkg-for-javascript
;;         - pel-pkg-for-go
;;         - pel-pkg-for-haskell
;;         - pel-pkg-for-lisp
;;           - pel-pkg-for-clisp
;;             - pel-clisp-code-style
;;             - pel-sexp-form-navigation
;;           - pel-pkg-for-elisp
;;             - pel-elisp-code-style
;;             - pel-sexp-form-navigation
;;           - pel-pkg-for-arc
;;           - pel-pkg-for-clojure
;;           - pel-pkg-for-janet
;;           - pel-pkg-for-hy
;;           - pel-pkg-for-scheme
;;             - pel-pkg-for-racket
;;             - pel-pkg-for-gambit
;;             - pel-pkg-for-gerbil
;;         - pel-pkg-for-beam-vm
;;           - pel-pkg-for-elixir
;;           - pel-pkg-for-erlang
;;             - pel-erlang-environment
;;             - pel-erlang-ide
;;             - pel-erlang-code-style
;;               - pel-erlang-skeleton-control
;;           - pel-pkg-for-lfe
;;           - pel-pkg-for-gleam
;;         - pel-pkg-for-forth
;;         - pel-pkg-for-julia
;;         - pel-pkg-for-lua
;;         - pel-pkg-for-m4
;;         - pel-pkg-for-modula-2
;;         - pel-pkg-for-nim
;;         - pel-pkg-for-objc
;;         - pel-pkg-for-ocaml
;;         - pel-pkg-for-odin
;;         - pel-pkg-for-pascal
;;         - pel-pkg-for-perl
;;           - pel-pkg-for-perl-general
;;           - pel-pkg-for-perl-perl-mode
;;           - pel-pkg-for-perl-cperl-mode
;;         - pel-pkg-for-pike
;;         - pel-pkg-for-python
;;         - pel-pkg-for-rexx
;;         - pel-pkg-for-ruby
;;         - pel-pkg-for-rust
;;         - pel-pkg-for-sh-scripting
;;           - pel-sh-script-skeleton-control
;;         - pel-pkg-for-seed7
;;         - pel-pkg-for-smalltalk
;;         - pel-pkg-for-swift
;;         - pel-pkg-for-tcl
;;         - pel-pkg-for-v
;;         - pel-pkg-for-zig
;;     - pel-pkg-for-project-mng
;;     - pel-pkg-for-regexp
;;     - pel-pkg-for-scrolling
;;     - pel-pkg-for-search
;;     - pel-pkg-for-session
;;     - pel-pkg-for-shells
;;       - pel-pkg-for-vterm-mode
;;       - pel-pkg-for-eat-mode
;;     - pel-pkg-for-skeletons
;;       - pel-pkg-generic-code-style
;;         - pel-sh-script-skeleton-control
;;       - pel-c-module-header-skeleton-control
;;         - pel-c-module-header-skeleton-control
;;         - pel-c-function-header-skeleton-control
;;       - pel-erlang-skeleton-control
;;     - pel-pkg-for-spec-definition
;;     - pel-pkg-for-speedbar
;;     - pel-pkg-for-spelling
;;     - pel-pkg-for-sw-build
;;       - pel-pkg-for-cmake
;;       - pel-pkg-for-make
;;       - pel-pkg-for-meson
;;       - pel-pkg-for-ninja
;;     - pel-pkg-for-text-mode
;;     - pel-pkg-for-time-tracking
;;     - pel-pkg-for-text-translation
;;     - pel-pkg-for-undo
;;     - pel-pkg-for-vcs
;;       - pel-pkg-for-git
;;       - pel-pkg-for-mercurial
;;       - pel-pkg-for-perforce
;;       - pel-pkg-for-subversion
;;     - pel-pkg-for-window
;;       - pel-pkg-for-scrolling
;;       - pel-pkg-for-session
;;       - pel-pkg-for-speedbar
;;

;; Naming conventions:
;;
;; For the following, replace <mode> by the name of the major mode.
;; for example, for python-mode, replace <mode> by python.
;;
;; - pel-pkg-for-<package-name or topic>
;; - pel-use-<package name>
;; - pel-startup-<thing to activate at startup>
;; - pel-modes-activating-<package name>
;; - pel-<mode>-activates-minor-modes
;; - pel-<mode>-use-tabs
;; - pel-<mode>-tab-width


;;
;; The `pel-use-' user-options are either t/nil boolean types or tri-state
;; types that can be set to nil, t or 'use-from-start.  When set to t, they
;; are only activated once a PEL command for that effect is executed by the
;; user.

;; Properties applied to defcustom variables
;; -----------------------------------------
;;
;; Extra information is stored inside attributes of the `pel-use-' defcustom
;; user-option variables.  This information is used in the logic PEL uses to
;; disable/remove packages when a `pel-use-' user option is turned off and the
;; function `pel-cleanup' is executed.
;;
;; The following properties are applied to the `pel-use-' user-option
;; variables only:
;;
;;  - `:also-required-when'
;;  - `:requires'
;;  - `:package-is'
;;  - `:restricted-to'
;;
;; They are described below.

;; `:also-required-when'
;; --------------------
;;
;; The `:also-required-when' property indicates that the package controlled
;; by the `pel-use-' user-option is also activated by something else.  The
;; condition that also activates it is identified by the quoted form that is
;; used as the value for this property.  Any valid form can be used.
;; As an example, look at the `pel-use-lice' user-option: the lice package is
;; installed when `pel-use-lice' is non-nil but also when one of the
;; programming language skels requires the use of code license.
;;

;; `:requires'
;; ----------
;;
;; The `:requires' property indicates package dependencies.  The attribute
;; value might be:
;;
;; - A single `pel-use-' symbol: stating that this parent must also be
;;   activated for the package to be installed.  The parent may be a 'gate'
;;   package (a `pel-use-' symbol that has the :package-is :a-gate property),
;;   or a `pel-use-' symbol for a normal package.
;;
;; - A list of several `pel-use-' symbols, stating that any of the specified
;;   parent must be installed for this package to be installed.
;;
;; - A list of several `pel-use-' symbols with `:all' in the first element,
;;   stating that *all* parent packages must be activated for this package to
;;   be installed.
;;

;; `:requires-package'
;; ------------------
;;
;; If a package is taken from an Elpa-compliant repo it is packaged and it
;; already identifies its dependencies.   The `pel-cleanup' function reads the
;; package dependencies and takes them into account.
;;
;; However, there are cases where the dependencies are not fully identified.
;;
;; - Case 1: the pel-init call in pel_keys.el sometimes installs and activates
;; several packages for a single `pel-use-' variable.  These other packages
;; must be identified.
;;
;; - Case 2: the package dependency of some packages do not identify *all* of
;; their dependencies.  In some case they do that because the package
;; activates extra functionality only when the extra dependency is present.
;;
;; In both cases, when these extra packages are present you want to keep them
;; during a `pel-cleanup' operation.  To identify those extra dependencies use
;; the `:requires-package' property and identify the required package.  The
;; semantics is the same as for the `:requires' property above except for the
;; fact that if it is not present nothing is inferred.
;;

;; `:package-is'
;; ------------
;;
;; In most case the name of the package that is controlled by the `pel-use-'
;; variables is the string that follows the "pel-use-" prefix in the symbol
;; name.  But this is not always the case.  There are the other possibilities
;; that are identified by the `:package-is' property:
;;
;;  - the `:a-gate' property value means that this user-option acts as a
;;    gate and does not install anything.  Other user-options use it as a
;;    parent to gate their installation.  This property is often used for
;;    programming languages, when another pel-use symbol controls the
;;    installation of a Emacs major mode package.  That is useful in cases
;;    where several implementation of major modes for a specific programming
;;    language exist and we need two pel-use flags to control the
;;    installation.
;;  - the `:builtin-emacs' property value indicates that the package is
;;    distributed with Emacs and cannot be de-installed,
;;  - the `:in-utils' property value indicates that the package is installed
;;    by PEL into the ~/.emacs.d/utils directory and not managed by the Emacs
;;    `package' library.  The package name is identified by the suffix of the
;;    pel-use- symbol.
;;  - A symbol which is the real name of a package that is downloaded
;;    from an Elpa-compliant package management site and managed trough Emacs
;;    `package' library.
;;  - a consp form that must be evaluated dynamically to compute the symbol or
;;    list of symbols representing the Elpa packages that are used.  The result
;;    must be a cons cell or a list of cons cell where the car is a symbol that
;;    identifies the packaging mechanism and the cdr is the package name
;;    symbol.  The package mechanisms supported are 'elpa and 'utils.  The
;;    'elpa symbol identifies a package that is downloaded and managed by the
;;     Emacs package library and comes from an Elpa-compliant repository.  The
;;     'utils symbol identifies a file that is downloaded from a web-site and
;;     stored into PEL's utils directory.  See `pel-use-ripgrep' for an example.
;;  - In the absence of the `:package-is' property, the name of the package is
;;    extracted from the name of the `pel-use-' symbol.

;; `:restricted-to'
;; ----------------
;;
;; Some packages are restricted to a specific mode of Emacs operation.  The
;; value of this property is a variable symbol that identifies when this
;; package is used.
;;
;; Currently, this property is only used to identify whether a package is
;; restricted to Emacs running in graphics or TTY mode.  It is possible to use
;; PEL with multiple customization files.  There could be one for Emacs
;; running in graphics mode and another for Emacs running in TTY mode.  The
;; selection must be done in your init file before pel-init is ever called.
;; We use this property to prevent `pel-cleanup' from removing a
;; graphics-mode-only package when `pel-cleanup' is invoked from Emacs running
;; in TTY mode.  The `pel-cleanup' disables packages only when the value for
;; this attribute evaluates to non-nil.  In other words, `pel-cleanup' will
;; only remove a package associated to a user-option that has a
;; `:restricted-to' property set to `pel-emacs-is-graphic-p' when Emacs is
;; running in graphics mode.

;; `:choices'
;; ----------
;;
;; This property is currently used only for the code template user-options to
;; identify a sample of valid choices for the user-option.  These are used in
;; the test code that generate the file sample examples stored inside the
;; example/templates directory.  These files provide samples of what is
;; possible to generate with the PEL code generation.


;; * Development Tip *:
;; ===================
;;
;; When writing support for a new package that is coming from and
;; Elpa-compliant repo:
;; - write the specification code here and the code calling
;;   the `pel-ensure-package' inside pel_keys.el,
;; - turn the option on and execute `pel-init' to get PEL to install the new
;;   package.
;; - Inspect the source file(s) of the newly install package to identify its
;;   dependencies.
;; - Execute the function `pel-elpa-pkg-dependencies' on the package and see
;;   if it identifies all its dependencies.
;;   - If that's not the case, then add a `:requires-package' property to the
;;     package `pel-use-' user-option that identifies the unspecified
;;     dependencies.
;; - Activate the package `pel-use-' variable and run `pel-init' to ensure
;;   that all required packages are installed.
;; - With the variable still active, run a dry-run `pel-cleanup' to check that
;;   nothing it requires would be un-installed.
;; - Deactivate the variable and run a dry-run `pel-cleanup' to verify that it
;;   removes only what should be removed.
;;
;;  IELM is your friend for this (too):
;;
;;  - To list what would be removed from utils, execute: (pel-utils-unrequired)
;;  - To list what would be removed from elpa, execute:  (pel-elpa-unrequired)

;;; --------------------------------------------------------------------------
;;; Dependency
(require 'pel--base)                    ; use: `pel-expression-p'
;;                                      ;      `pel-user-option-p'
(eval-when-compile
  (require 'cl-macs))                   ; use: `cl-case'.

;;; --------------------------------------------------------------------------
;;; Code:


;; Validation Utilities
;; --------------------

(defun pel-indent-valid-p (n)
  "Return t if N is a valid indentation integer in 2-8 range, nil otherwise."
  (and (integerp n) (< n 9) (> n 1)))

(defun pel-c-style-valid-p (style)
  "Return non-nil if STYLE is one of the valid CC Mode styles, nil otherwise."
  (require 'cc-vars nil :noerror)
  (if (boundp 'c-style-alist)
      (member style (mapcar 'car c-style-alist))
    (error "Failed loading cc-vars!")))

;; Property setter with validation
;; -------------------------------
;; In the code below, use the `pel-put' macro instead of the `put' function.
;; The `pel-put' macro validates the arguments and generates code only when
;; the arguments are correct.  This provides compilation time code checking
;; with no impact to load and run time.

(defmacro pel-put (symbol propname value)
  "Store SYMBOL's PROPNAME property with value VALUE.
Validate at `byte-compile' time."
  (if (cond
       ((eq propname :also-required-when)
        (and (consp value)
             (eq (car value) 'quote)
             (pel-expression-p value)))
       ((memq propname '(:requires :requires-package))
        (and (consp value)
             (eq (car value) 'quote)))
       ((eq propname :package-is)
        (or (memq value '(:a-gate :builtin-emacs :in-utils))
            (and (consp value)
                 (eq (car value) 'quote)
                 (or (eq (length (cdr value)) 1)
                     (and (consp (cdr value))
                          (pel-expression-p value))))))
       ((eq propname :restricted-to)
        (and (consp value)
             (eq (car value) 'quote)
             (pel-expression-p value)))
       ;; choices is to help testing - accept them
       ((eq propname :choices)
        t)
       (t nil))
      `(put ,symbol ,propname ,value)
    `(error "Invalid %s property value %S for symbol %s"
            ,propname ,value ,symbol)))
;; ---------------------------------------------------------------------------
;; File Path build utility
;; -----------------------

(defun pel-pdf-directory ()
  "Return the full path of the directory where PDF files are stored.
Last character is a forward slash."
  (expand-file-name
   (format "%s/doc/pdf/"
           (file-name-directory (locate-library "pel")))))

(defun pel-pdf-file-url (topic &optional on-web category)
  "Return the full path of a PEL pdf table for TOPIC in CATEGORY.

TOPIC is the file name body (no path, no extension).
CATEGORY identifies the PDF category directory name:
- if nil (the default), return main PEL mode PDF file name.
- otherwise, the CATEGORY identifies the name of a sub-directory
  where other PDF files are stored and the function returns the
  name of that file. This allows managing a set of PDF files for
  a given topic.  For instance, CATEGORY set to \"lang\",
  identifies PDF files that contain programming language specific
  syntax and reference information.

By default return the local file url.
If ON-WEB is non-nil return the web URL for the file hosted in GitHub."
  (let ((subdir (if category
                    (format "%s/" category)
                  "")))
    (if on-web
        ;; Return GitHub directory location.
        (format "https://raw.githubusercontent.com/pierre-rouleau/pel/\
master/doc/pdf/%s%s.pdf" subdir topic)
      ;; Return URL for local PDF file
      (format "file:%s"
              (expand-file-name
               (format "%s%s.pdf" subdir topic)
               (pel-pdf-directory))))))

;; ---------------------------------------------------------------------------
;; User Option Data Definition
;; ---------------------------

(defgroup pel nil
  "Pragmatic Emacs Library.
A collection of facilities designed to integrate and complement a large
set of Emacs libraries while providing key bindings that mainly use function
keys as key prefixes, leaving the standard Emacs keys untouched.
PEL comes with a manual and a large set of PDF files, each documenting the
commands and key bindings of a specific aspect of Emacs.  The PDF files document
the standard Emacs key bindings as well as PEL's specific key bindings."
  :group 'convenience
  :link `(file-link :tag "Directory of PDF table files" ,(pel-pdf-directory))
  :link `(url-link  :tag "PEL key maps PDF" ,(pel-pdf-file-url "-pel-key-maps"))
  :link '(url-link  :tag "PEL @ GitHub" "https://github.com/pierre-rouleau/pel")
  :package-version '(pel . "0.4.1"))

;; ---------------------------------------------------------------------------
(defgroup pel-base-emacs nil
  "PEL Emacs basic configuration."
  :group 'pel)

(defcustom pel-has-alias-as t
  "Activate `as' alias to `pel-as' for convenience."
  :group 'pel-base-emacs
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-future-proof nil
  "Activate future-proofing package download when set to t.

This activates the automatic download and installation of packages that
protect your Emacs installation against time changes to Emacs and to its
ecosystem.  See the logic inside pel_keys.el "
  :group 'pel-base-emacs
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-emacs-source-directory nil
  "Absolute path of Emacs repository root directory.

- Identify the Emacs repo root directory.
  That should hold a src sub-directory.
- The name can start with \"~\".

PEL sets the `source-directory' variable to the value of
`pel-emacs-source-directory' when it starts, allowing Emacs help
to open Emacs C source code files."
  :group 'pel-base-emacs
  :group 'pel-pkg-for-help
  :type 'directory)

(defcustom pel-auto-mode-alist nil
  "Alist of filename patterns vs corresponding major mode functions to set.
These associations are added to Emacs variable `auto-mode-alist' at
initialization time.
See `auto-mode-alist' for more information.
Use the INS and DEL buttons to add associations:
- the first element is a string regxp pattern identifying the
  file name to identify (which may only be matching it by its extension),
- the second is a major mode symbol."
  :group 'pel-base-emacs
  :type
  '(repeat
    (list
     (string :tag "file pattern regex")
     (symbol :tag "major mode        "))))

(defcustom pel-prompt-accept-y-n nil
  "Accept \"y\" or \"n\" instead of \"yes\" or \"no\" as answers to prompts."
  :group 'pel-base-emacs
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-modes-activating-syntax-check  nil
  "List of major modes that automatically activate their syntax checker.

PEL controls what syntax checker is used for each major mode.
This includes flymake and flycheck and if others exist they will
also be added to the support.  A user may want to use flymake with
one language and flycheck with another.  PEL supports that.  By
default PEL does not activate a syntax checker when the file is
opened, allowing Emacs to run a little faster.  PEL provides a
command (bound to the ``<f12> !`` key of that mode) for each
supported major mode to toggle the syntax checker on or off.

If you prefer to activate a syntax checker for a specific major
mode right when the file is opened, add the name of the major
mode to this list.  PEL will then activate the syntax checker
right when the file is opened.  Each entry must be the symbol
name of a major mode.

For example, to activate it in Erlang, add a line with
`erlang-mode' without the quotes."
  :group 'pel-base-emacs
  :type '(repeat symbol))

(defcustom pel-activates-global-minor-modes nil
  "List of *global* minor-modes automatically activated for all buffers.
Enter *global* minor-mode activating function symbols.
Do not enter lambda expressions."
  :group 'pel-base-emacs
  :type '(repeat function))

;; ---------------------------------------------------------------------------
(defgroup pel-pkg-for-parser nil
  "PEL customization for parser generator support packages.

Note: currently nothing is placed under this group, BUT several other
      defgroup are children of this one; it acts as a group to get access
      to the various parser support packages."
  :group 'pel-base-emacs)

;; ---------------------------------------------------------------------------
;; Tree Sitter
;; -----------
(defgroup pel-pkg-for-tree-sitter nil
  "Tree Sitter support under PEL."
  :link '(url-link :tag "Tree-Sitter @ GitHub"
                   "https://github.com/tree-sitter/tree-sitter?tab=readme-ov-file#readme")
  :link '(url-link :tag "Tree Sitter Presentation at Strange Loop"
                   "https://www.thestrangeloop.com/2018/tree-sitter---a-new-parsing-system-for-programming-tools.html")
  :group 'pel-pkg-for-parser)

(defcustom pel-use-tree-sitter nil
  "Activate the tree-sitter support.

NOTES: - PEL only supports tree-sitter for Emacs 30.1 and later and
         automatically turns this off (set to nil) when running under
         an earlier version of Emacs.

           Emacs 29.1 supports tree-sitter, however Emacs 30.1 introduces
           a breaking change that simplifies the way tree-sitter major modes
           are implemented. PEL code is compatible with the new logic to
           enhance user-experience and automate the installation.

       - To use tree-sitter modes under PEL you must setup your ~/.emacs.d
         directory according to the PEL Tree-Sitter Setup Instructions.
         (see the link inside the customize buffer)."
  :link '(url-link :tag "Emacs tree-sitter @ GitHub.io"
                   "https://emacs-tree-sitter.github.io/")
  :link '(url-link :tag "elisp-tree-sitter @ GitHub"
                   "https://github.com/emacs-tree-sitter/elisp-tree-sitter")
  :link '(url-link :tag "tree-sitter-langs @ GitHub"
                   "https://github.com/emacs-tree-sitter/tree-sitter-langs")
  :link '(url-link :tag "PEL Tree-Sitter Setup Instructions"
                   "https://github.com/pierre-rouleau/pel/blob/master/doc/using-tree-sitter.rst.txt")
  :group 'pel-pkg-for-tree-sitter
  :type 'boolean
  :safe #'booleanp)
(unless pel-emacs-30-or-later-p (setq pel-use-tree-sitter nil))

(defcustom pel-treesit-load-path nil
  "List of directories to look for tree-sitter language definition.

The directories should hold tree-sitter definition dynamic library files.
The list of directory will be appended to Emacs `treesit-extra-load-path',
allowing Emacs tree-sitter support to find the required language dynamic
libraries when it is required by tree-sitter support for a specific major
mode.

IMPORTANT NOTE:

I noticed the following:

- The tree-sitter-langs package installs the language dynamic
  libraries inside the ~/.emacs.d/elpa/tree-sitter-langs-YYYYMMDD.vvv.bin
  directory, where YYYMMMDD.vvv depends on the moment the installation
  was done.
- The files stored inside that directory have a name like cmake.dylib or
  cmake.so (depending on your OS extension for the dynamic libraries).
- Some packages look for language dynamic library files that have a name
  that starts with \\='libtree-sitter-\\=',
  like \\='libree-sitter-cmake.dylib\\='.

So it seems that some work is needed to properly support tree-sitter.
Here's what I do in my environments:

- Create a symbolic link named tree-sitter-langs-bin that points to the
  appropriate ~/.emacs.d/elpa/tree-sitter-langs-YYYYMMDD.vvv.bin directory.
- Create a ~/.emacs.d/libtree-sitter directory.
- Inside that directory I create symbolic links named
  \\='libree-sitter-MODE.EXT\\=' for each MODE.EXT file located inside the
  ~/.emacs.d/elpa/tree-sitter-langs-YYYYMMDD.vvv.bin directory via the
  ~/.emacs.d/tree-sitter-langs-bin symbolic link.
- Add the following 2 directories to the list here:
  - ~/.emacs.d/tree-sitter-langs-bin
  - ~/.emacs.d/libtree-sitter"
  :group 'pel-pkg-for-tree-sitter
  :type '(repeat string))

(defcustom pel-use-combobulate nil
  "Whether PEL uses the combobulate package.

NOTES: - PEL only supports tree-sitter for Emacs 30.1 and later and
         automatically turns this off (set to nil) when running under
         an earlier version of Emacs because combobulate requires
         tree-sitter support. "
  :link '(url-link :tag "combobulate @ Github"
                   "https://github.com/mickeynp/combobulate")
  :link '(url-link :tag )
  :group 'pel-pkg-for-tree-sitter
  :type 'boolean
  :safe #'booleanp)
;; combobulate uses Tree-Sitter technology and is installed with quelpa
(pel-put 'pel-use-combobulate :requires '(:all pel-use-tree-sitter
                                               pel-use-quelpa))
(unless pel-use-tree-sitter  (setq pel-use-tree-sitter nil))

;; ---------------------------------------------------------------------------
(defgroup pel-syntax-tools nil
  "PEL syntax investigation tools."
  :group 'pel)

(defcustom pel-syntax-text-properties '(category ; used by erlang.el
                                        syntax-table)
  "List of text properties used for syntax definitions.

The text properties listed here are included in the message displayed by the
`pel-syntax-at-point' command.

Use this is a tool to help debug syntax processing of major modes."
  :group 'pel-syntax-tools
  :type '(repeat symbol))

;; ---------------------------------------------------------------------------
(defgroup pel-fast-startup nil
  "PEL fast startup options."
  :link `(url-link :tag "Fast Startup PDF" ,(pel-pdf-file-url "fast-startup"))
  :group 'pel)

(defcustom pel-shell-detection-envvar "_"
  "Name of envvar used to detect that Emacs was launched by a shell.

The default is \"_\", the environment variable that Bash uses to
identify the name of the executable that launched it.  This
environment variable is not part of the process environment when
Emacs is launched from a GUI program such as macOS Finder.

Change this value when using another shell or when running on
other operating system such as Windows.  If you cannot find a
suitable environment variable that is defined when Emacs is
launched, then define an environment variable that will be
present in all instances of your shell but not inside the OS
process environment.  For instance you could use the environment
name \"PEL_SHELL\"."
  :group 'pel-fast-startup
  :type 'string)

(defcustom pel-gui-process-environment nil
  "Environment variables to set when Emacs launched from GUI program.

When Emacs is launched from a shell it inherit the environment
variables from that shell parent process.  It is then possible to
create several specialized shells and set environment variables
in the shell initialization script that will be used by Emacs.

However you can also launch Emacs from a GUI program such as
Window Explorer or macOS Finder.  These use a minimal environment
set for the OS.  In many way that will not be sufficient for that
Emacs process as you may want to add more directories to its PATH
and define various environment variables.

PEL provides the `pel-gui-process-environment' user-option to
specify a set of environment variables that the GUI Emacs will
then be able to use.

That may not be as flexible as using multiple specialized shells
but it will provide what is needed for the GUI Emacs.  For more
flexibility use a shell launched Emacs as both terminal and
graphics modes can be launched by shell scripts.

For each variable you can use to use the variable as specified,
which is always done when the variable does not exists.  But for
some variables like PATH, MANPATH, LIBPATH and others you may
want to replace, append or prepend the value to any existing
value.  In that case, change the action specified for the
variable.

***************
IMPORTANT NOTES
***************

These environment variables are only used for a GUI-launched
Emacs session; an Emacs session that has not been launched by a
shell.  PEL detects this type of Emacs session when it detects
that the environment variable identified by
`pel-shell-detection-envvar' is not present inside the process
environment.

PEL sets Emacs environment variable process **once** per process
execution session, during the **first** call of function
`pel-init'.  Therefore if you change this you **must** restart
Emacs for the new values to take effect.

The PATH environment variable is treated specially: setting,
appending, prepending to it also sets the value of the variable
`exec-path' and variable `eshell-path-env'."
  :group 'pel-fast-startup
  :type '(repeat
          (list :tag "Environment variable"
                (string :tag "name ")
                (string :tag "value")
                (choice :tag "use"
                        (const :tag "as is, replacing exiting value if any." use-as-is)
                        (const :tag "append to existing value if any."    append)
                        (const :tag "prepend to existing value if any."   prepend)))))

(defcustom pel-compile-pel-bundle-autoload nil
  "Whether `pel-setup-fast' byte compiles pel-bundle autoloads.el file.

Set this to t to instruct `pel-setup-fast' to force the byte-compilation of the
pel-bundle-autoloads.el file even though the autoloads.el files of Elpa
packages are not byte compiled.  Byte compilation of that file may generate
byte compiler warnings but that will also speed the Emacs startup a little."
  :group 'pel-fast-startup
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-support-dual-environment nil
  "Whether independent environment for terminal/TTY and graphics is enabled.

Turn it on if you want Emacs in terminal/TTY mode and Emacs in
graphics mode to use independent sets of customization and
packages.  By default this feature is turned off.

It is best to turn it on by executing the function
`pel-setup-dual-environment' because that will create all
necessary files and will update the user-option in both
customization files."
  :group 'pel-fast-startup
  :type 'boolean
  :safe #'booleanp)

;; ----
(defcustom pel-compile-emacs-init nil
  "Whether PEL setup commands that update init.el also `byte-compile' it.

Note that this assumes that your init.el file does not prevent byte
compilation.  If your file defines the `no-byte-compile' variable, remove that
or force it nil if you want PEL to `byte-compile' it.

Unlike the pel-bundle and package-quickstart files, PEL will not change the
value of `no-byte-compile' file variable in your init.el file."
  :group 'pel-fast-startup
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-compile-emacs-early-init nil
  "Whether PEL setup commands that update early-init.el also `byte-compile' it.

Note that this assumes that your early-init.el file does not prevent byte
compilation.  If your file defines the `no-byte-compile' variable, remove that
or force it nil if you want PEL to `byte-compile' it.

Unlike the pel-bundle and package-quickstart files, PEL will not change the
value of `no-byte-compile' file variable in your early-init.el file."
  :group 'pel-fast-startup
  :type 'boolean
  :safe #'booleanp)

;; ----
(defcustom pel-support-package-quickstart nil
  "When non-nil: activate package-quickstart, otherwise don't activate it.

This is only used in Emacs 27 and later."
  :group 'pel-fast-startup
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-compile-package-quickstart nil
  "Whether PEL byte-compiles package-quickstart file.

Set this to t to instruct PEL to force the byte-compilation of
the package-quickstart.el file even though these files are
normally not byte compiled.  Byte compilation of that file may
generate byte compiler warnings but that will also speed the
Emacs startup a little.

This is only used for Emacs 27 and later."
  :group 'pel-fast-startup
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-early-init-template
  (expand-file-name "example/init/early-init.el"
                    (file-name-directory load-file-name))
  "Path name of PEL-compatible early-init.el file template.

To be compatible with PEL your early-init.el file MUST contain
the code located in that file.  You may use another file which
adds extra code for your own needs but you must include the code
located in that file template.  If you do not need any extra
logic, leave the default and use the template file unmodified.

This is only used in Emacs 27 and later."
  :group 'pel-fast-startup
  :type '(file :must-match t))

;; ---------------------------------------------------------------------------
(defgroup pel-package-use nil
  "List of external packages that can be used by PEL."
  :group 'pel)

(defconst pel-elpa-obsolete-packages '(parinfer)
  "Lists PEL supported ELPA packages that are no longer available.")

(defcustom pel-elpa-packages-to-keep '(dash
                                       elisp-lint
                                       package-lint)
  "List of Elpa package names that should not be removed by `pel-cleanup'.

Put the names of the packages you install manually in this list.
PEL will not remove them when it performs a cleanup.
By default, PEL identifies the following packages:
- benchmark-init: use this to measure your initialization time.
- elisp-lint:     check your Emacs Lisp code.  PEL's ``make lint`` uses it.
                  elisp-lint requires package-lint and dash, which are also
                  in the default list."
  :group 'pel-package-use
  :type '(repeat symbol))

(defcustom pel-utils-packages-to-keep '("benchmark-init-modes.el"
                                        "benchmark-init.el")
  "List of utils file names that should not be removed by `pel-cleanup'.

If you manually install Emacs Lisp files in your utils directory, you should
put their names in this list to prevent their removal by `pel-cleanup'."
  :group 'pel-package-use
  :type '(repeat string))

(defcustom pel-utils-dirname "utils"
  "Name of the PEL utils directory, where non-Elpa Emacs files are stored.

The directory is always located in the directory identified by the Emacs
variable `user-emacs-directory'.

PEL uses \"utils\" by default but you may want to specify another
directory for whatever reason."
  :group 'pel-package-use
  :type 'string)

(defcustom pel-use-editor-config nil
  "Control whether PEL activates EditorConfig plugin for Emacs."
  :link '(url-link :tag "EditorConfig home page"
                   "https://editorconfig.org")
  :link '(url-link :tag "editorconfig-emacs @ GitHub"
                   "https://github.com/editorconfig/editorconfig-emacs")
  :group 'pel-base-emacs
  :group 'pel-package-use
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-editor-config :package-is 'editorconfig)
;; TODO: pel-cleanup currently does not remove the following lines from the
;;       `custom-set-variable' form:
;;        -  '(editorconfig-mode t)
;;        -  '(editorconfig-mode-lighter " 🎛 ")
;;      I probably need to add another property-driven concept in pel-cleanup
;;      support to get it to identify forms that must be removed. Perhaps the
;;      lighter config could stay, but setting the mode to true should be
;;      removed.  Note however, that these will not activate the mode once
;;      the current cleanup has been done, they will just be left in the
;;      `custom-set-variable' which has no impact once `pel-cleanup' ran and
;;      uninstalled editorconfig.

;; ---------------------------------------------------------------------------
;; Package Management Extensions
;; -----------------------------
(defgroup pel-pkg-package-mng nil
  "Package Management Extensions - more ways to get Emacs packages."
  :group 'pel-package-use
  :link `(url-link :tag "Package Management PDF" ,(pel-pdf-file-url
                                                   "packages")))
(defcustom pel-use-quelpa nil
  "Control whether PEL activates and uses the quelpa package manager."
  :link '(url-link :tag "quelpa @ github"
                   "https://github.com/quelpa/quelpa")
  :link '(url-link :tag "quelpa readme"
                   "https://github.com/quelpa/quelpa/blob/master/README.org")
  :group 'pel-pkg-package-mng
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-quelpa :also-required-when '(and pel-use-tree-sitter
                                                   pel-use-combobulate))

;; ---------------------------------------------------------------------------
;; Alignment Support
;; -----------------
(defgroup pel-pkg-for-align nil
  "Customization of PEL alignment support."
  :group 'pel-package-use
  :link `(url-link :tag "Align PDF" ,(pel-pdf-file-url "align")))

(defcustom pel-modes-activating-align-on-return nil
  "List of major modes that automatically activate alignment on M-RET.
For these modes the buffer local variable `pel-newline-does-align' is
automatically set to t.  This activates the automatic alignment of contiguous
lines when the function `pel-newline-and-indent-below' executes.
The alignment is controlled by a set of regular-expression based rules
stored in the variable `align-rules-list'.

These rules support the alignment of C and Python assignment
statements, where the alignment is done on the equal sign
character.  Also for C++ \"//\" style comments.
See `align-rules-list'.

By default PEL does not activate it on any mode.  To activate a mode,
add the mode name in the list.
For example, to activate it for C, add the c-mode symbol to the list."
  :group 'pel-pkg-for-align
  :type  '(repeat symbol)
  :link '(emacs-commentary-link :tag "commentary" "align.el")
  :link `(url-link :tag "Align PDF" ,(pel-pdf-file-url "align")))

;; ---------------------------------------------------------------------------
;; Bookmark Support
;; ----------------
(defgroup pel-pkg-for-bookmark nil
  "List of external packages that PEL can use to manage bookmarks."
  :group 'pel-package-use
  :link `(url-link :tag "Bookmarks PDF" ,(pel-pdf-file-url "bookmarks"))
  :link '(custom-manual "(emacs)Bookmarks"))

(defcustom pel-use-bm nil
  "Control whether PEL uses the bm (Visible Bookmarks) package.
With this activated PEL binds the following keys:
- <f2>     : `bm-next'
- <f11> '  : `bm-toggle'
- <f11> n  : `bm-next'
- <f11> p  : `bm-previous'"
  :group 'pel-pkg-for-bookmark
  :type 'boolean
  :safe #'booleanp
  :link '(url-link :tag "bm @ GitHub" "https://github.com/joodland/bm"))

;; ---------------------------------------------------------------------------
;; Buffer Management
;; -----------------
(defgroup pel-pkg-for-buffer nil
  "List of external packages that PEL can use to manage buffers."
  :group 'pel-package-use
  :link `(url-link :tag "Buffers PDF" ,(pel-pdf-file-url "buffers"))
  :link '(custom-manual "(emacs)Buffers"))

(defcustom pel-use-uniquify nil
  "Control whether PEL uses the uniquify package.
With this activated PEL changes the way Emacs displays the names
of the buffers that visit identically-named files.
It sets up the post-forward method for the buffers except the
special buffers (which have their own disambiguation method) and
forces rationalization of the names when buffers are killed.

With the post-forward method, if you have 3 files opened, like:
- ~/projects/p1/hello.c
- ~/projects/p2/hello.c
- ~/some/other/place/somedir/hello.c

The buffers will respectively be named:
- hello.c|p1
- hello.c|p2
- hello.c|somedir

If you kill 2 of these buffers, the remaining buffer will be named
hello.c"
  :group 'pel-pkg-for-buffer
  :type 'boolean
  :safe #'booleanp
  :link '(custom-manual "(emacs)Uniquify"))
(pel-put 'pel-use-uniquify :package-is :builtin-emacs)

(defcustom pel-use-ascii-table nil
  "Control whether the `ascii-table' package is available.
When set PEL activates the ``<f11> ? A`` key sequence to
open the ASCII table in the current buffer."
  :group 'pel-pkg-for-buffer
  :type 'boolean
  :safe #'booleanp
  :link '(url-link :tag "ascii-table @ MELPA"
                   "https://melpa.org/#/ascii-table"))

(defcustom pel-use-nhexl-mode nil
  "Control whether PEL uses the package and function `nhexl-mode'.
This mode supports editing a file in hexadecimal dump mode.
When set, PEL activates the following key sequences:
- <f11> t O  : `nhexl-overwrite-only-mode'
- <f11> b x  : `nhexl-mode'
- <f11> b X  : `nhexl-nibble-edit-mode'"
  :group 'pel-pkg-for-buffer
  :type 'boolean
  :safe #'booleanp
  :link '(url-link :tag "nhexl @ Elpa"
                   "https://elpa.gnu.org/packages/nhexl-mode.html"))

(defcustom pel-use-iflipb nil
  "Control whether PEL provides access to the iflipb package."
  :link '(url-link :tag "iflipb @GitHub"
                   "https://github.com/jrosdahl/iflipb")
  :group 'pel-pkg-for-buffer
  :type 'boolean
  :safe #'booleanp)

;; ---------------------------------------------------------------------------
(defgroup pel-pkg-for-ibuffer nil
  "List of external packages that PEL can use to extend ibuffer-mode."
  :group 'pel-pkg-for-buffer
  :link `(url-link :tag "ibuffer-mode PDF" ,(pel-pdf-file-url
                                             "ibuffer-mode")))

(defcustom pel-use-ibuffer-vc nil
  "Control whether PEL uses & activates the ibuffer-vc package."
  :group 'pel-pkg-for-ibuffer
  :link '(url-link :tag "ibuffer-vc @ Github"
                   "https://github.com/purcell/ibuffer-vc")
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-use-ibuffer-tramp nil
  "Control whether PEL uses & activates the ibuffer-tramp package."
  :group 'pel-pkg-for-ibuffer
  :link '(url-link :tag "ibuffer-tramp @ GitHub"
                   "https://github.com/pierre-rouleau/ibuffer-tramp")
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-ibuffer-tramp :package-is :in-utils)

;; ---------------------------------------------------------------------------
;; Completion Support
;; ------------------
(defgroup pel-pkg-for-completion nil
  "List of external packages that PEL can use to manage completion.

PEL allows selecting input completion mechanism dynamically.
Aside from Emacs tab-based completion default, you can also use
Ido, Ivy, or Helm.

Ido is very flexible and powerful.  It is built-in Emacs but can
be extended with several external packages which can select
various input geometries, activate 'flx' fuzzy matching, etc...

If you prefer using a simpler drop-down menu with search
capabilities built-in you may prefer Ivy.

And if you want a input completion mechanism that has tons of
functionality and are willing to learn its power, you can use
Helm.

Note that with PEL you can select the input completion used by
default and change it dynamically during an editing session, as
long as it is activated by its user-option.

- Activate Ido with : `pel-use-ido'.
- Activate ivy with : `pel-use-ivy'.
- Activate Helm with: `pel-use-helm'.

To provide smart input completion of commands related to current
major mode, use Smex and activate it with `pel-use-smex'.

Ido extensions include the following:

- Ido prompt geometries:

  - Ido default (linear)
  - Ido Grid:
    - Select between two incompatible grid mechanisms:
      - `pel-use-ido-grid', which provides a simpler all-in-one ido-grid, or
      - `pel-use-ido-grid-mode' which provides a ido-grid-mode that has two
         sub-modes: ability to start collapsed or fully extended.
  - Ido Vertical mode: activate with `pel-use-ido-vertical-mode'

  If `pel-use-ido-grid' is turned on, it forces `pel-use-ido-grid-mode' off
  because these two packages are incompatible in such a way that starting
  the `ido-grid-mode' feature prevents the `ido-grid' feature from working
  properly.

  - Select initial Ido geometry with: `pel-initial-ido-geometry'. If you set
   `pel-use-ido-grid' then the modes related to `pel-use-ido-grid-mode' are
   automatically replaced by the ido-grid value.

- Ido 'flx' fuzzy matching: activate with `pel-use-flx'
- Ido completion is multiple prompt commands: activate it
  with `pel-use-ido-ubiquitous'.

There is also a set of extension packages for Ivy. PEL supports:

- Counsel. If you want to be able to use Ivy completion mode in most
  prompting commands, then activate Counsel with `pel-use-counsel'.
- Counsel OSX App on macOS to launch macOS application via en Emacs prompt
  with Ivy completion: activate with `pel-use-counsel-osx-app'.
- Ivy-hydra is activated when `pel-use-ivy-hydra' is set.

Select the input completion mode used at startup with:
`pel-initial-completion-mode'.

PEL also provides 2 commands to navigate into the current buffer and all
opened buffers using the symbols detected by the Emacs imenu symbol parsing of
the buffer major modes.

- The `pel-goto-symbol' moves point to a symbol in current buffer. PEL
  provides  several user interface mechanisms for it, various input
  completions or popup menu systems. Select its user interface with
 `pel-initial-goto-symbol-UI'.
- The `pel-goto-symbol-any-buffer', which does the same but for
  all currently opened buffer.  It uses imenu-anywhere which you
  must activate with `pel-use-imenu-anywhere'.  This user option
  also selects the user interface used when Emacs starts.

PEL provides commands you can use to dynamically change the user interface
used by these 2 commands during an editing session without affecting the
initial user interface:

- `pel-select-goto-symbol-UI'
- `pel-select-goto-symbol-any-buffer-UI'.

Since imenu is used as user interface candidates for these
commands, if you select imenu specific modes you must also
configure what imenu extension is activated via the
`pel-pkg-for-imenu' customization group.  Use the link to quickly
move to that buffer."
  :group 'pel-package-use
  :link '(custom-group-link "pel-pkg-for-imenu")
  :link `(url-link :tag "Input Completion PDF"
                   ,(pel-pdf-file-url "completion-input")))


(defcustom pel-allow-recursive-minibuffer nil
  "Control whether minibuffer recursive editing is allowed."
  :link '(url-link :tag "Making The Emacs Minibuffer Recursive @ Irreal"
                   "https://irreal.org/blog/?p=314")
  :group 'pel-pkg-for-completion
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-use-ido nil
  "Control whether PEL uses the Ido package.

The IDO package is distributed with Emacs.  It provides very efficient
completion mechanism that is preferred by many people."
  :link '(custom-manual "(ido)Overview")
  :link '(url-link :tag "Introduction to Ido Mode @ Mastering Emacs"
                   "https://www.masteringemacs.org/article\
/introduction-to-ido-mode")
  :link '(url-link :tag "Interactively Do things @ EmacsWiki"
                   "https://www.emacswiki.org/emacs/InteractivelyDoThings")
  :link '(custom-group-link "ido")
  :group 'pel-pkg-for-completion
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-ido :package-is :builtin-emacs)

(defcustom pel-use-idomenu nil
  "Control  whether PEL uses the idomenu package."
  :link '(url-link :tag "idomenu @ Github"
                   "https://github.com/birkenfeld/idomenu")
  :group 'pel-pkg-for-completion
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-idomenu :requires 'pel-use-ido)

(defcustom pel-use-smex nil
  "Control whether PEL uses the smex package.

The smex package adds Ido completion to the `execute-extended-command'
and provides the \"M-X\" which does completion commands related to active
for major mode only.

To use this you must also have `pel-use-ido' set to t."
  :link '(url-link :tag "smex @ GitHub"
                   "https://github.com/nonsequitur/smex")
  :group 'pel-pkg-for-completion
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-smex :requires 'pel-use-ido)

(defcustom pel-use-ido-grid-mode nil
  "Control whether PEL uses the `ido-grid-mode' package.

This modifies the presentation geometry of the Ido completion
prompt: it shows candidates in multiple columns.

To use this you must also have `pel-use-ido' set to t.
The initial Ido geometry is set by `pel-initial-ido-geometry'."
  :link '(url-link :tag "ido-grid-mode @ GitHub"
                   "https://github.com/larkery/ido-grid-mode.el")
  :group 'pel-pkg-for-completion
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-use-ido-grid nil
  "Control whether PEL uses the ido-grid package.

The ido-grid is a re-implementation of `ido-grid-mode' made to go
faster and easier to use.

This modifies the presentation geometry of the Ido completion
prompt: it shows candidates in multiple columns.

To use this you must also have `pel-use-ido' set to t.
The initial Ido geometry is set by `pel-initial-ido-geometry'."
  :link '(url-link :tag "ido-grid @ GitHub"
                   "https://github.com/larkery/ido-grid.el")
  :group 'pel-pkg-for-completion
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-ido-grid :requires 'pel-use-ido)
(pel-put 'pel-use-ido-grid :package-is :in-utils)

(defcustom pel-use-ido-vertical-mode nil
  "Control whether PEL uses the `ido-vertical-mode' package.

This modifies the presentation geometry of the Ido completion
prompt: it shows candidates in multiple lines, like ivy does.

To use this you must also have `pel-use-ido' set to t.
The initial Ido geometry is set by `pel-initial-ido-geometry'."
  :link '(url-link :tag "ido-vertical-mode @ GitHub"
                   "https://github.com/creichert/ido-vertical-mode.el")
  :group 'pel-pkg-for-completion
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-ido-vertical-mode :requires 'pel-use-ido)

(defcustom pel-use-ido-ubiquitous nil
  "Control whether the ido-completing-read+ package is used.

Note that the original name of that package used to be
ido-ubiquitous.  The new name is ido-completing-read+.  However,
the name of the user-option uses the old name as it's shorter and
better represents the concept.

With this package used, IDO completion is made available for a
larger number of prompt functions and more can be activated via
its customization. The functions that now support IDO completion
will also use the ivy or helm completion if they are selected.

To activate this you must also activate `pel-use-ido'."
  :link '(url-link :tag "ido-completing-read+ @ Github"
                   "https://github.com/DarwinAwardWinner\
/ido-completing-read-plus")
  :group 'pel-pkg-for-completion
  :type '(choice
          (const :tag "Do not use" nil)
          (const :tag "Use, activate later by command"  t)
          (const :tag "Use, activate globally when Emacs starts"
                 use-from-start)))
(pel-put 'pel-use-ido-ubiquitous :package-is 'ido-completing-read+)
(pel-put 'pel-use-ido-ubiquitous :requires 'pel-use-ido)

(defcustom pel-use-flx nil
  "Control whether PEL uses the flx matching package.

The flx completion fuzzy pattern matching engine takes a large
amount of memory but is well suite for matching file names inside
a long path easily.  It is similar to what is available on the
Sublime editor.

To use this you must also have `pel-use-ido' or `pel-use-ivy' set to t."
  :link '(url-link :tag "flx @ Github"
                   "https://github.com/lewang/flx")
  :group 'pel-pkg-for-completion
  :type '(choice
          (const :tag "Do not use" nil)
          (const :tag "Use, activate later by command"  t)
          (const :tag "Use, activate globally when Emacs starts"
                 use-from-start)))
(pel-put 'pel-use-flx :package-is 'flx-ido)
(pel-put 'pel-use-flx :requires '(pel-use-ido pel-use-ivy))

;; --

(defcustom pel-use-ivy nil
  "Control whether PEL uses the Ivy package.

Ivy is another popular interactive completion mechanism for Emacs using menu
lists and designed for speed of selection.
The initial completion mode is set by `pel-initial-completion-mode'."
  :link '(url-link :tag "Ivy @ GitHub" "https://github.com/abo-abo/swiper")
  :link '(url-link :tag "Ivy User Manual" "https://oremacs.com/swiper/")
  :link '(custom-group-link "ivy")
  :group 'pel-pkg-for-completion
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-ivy :also-required-when '(or pel-use-ivy-xref
                                               pel-use-lsp-ivy
                                               pel-use-ivy-erlang-complete))

(defcustom pel-use-counsel nil
  "Control whether Counsel is used when Ivy is used.
You must also activate the user option variable  `pel-use-ivy' to use counsel."
  :link '(url-link :tag "counsel/swiper @ GitHub"
                   "https://github.com/abo-abo/swiper")
  :link '(url-link :tag "Description of counsel-linux-app"
                   "https://oremacs.com/2016/03/16/counsel-linux-app/")
  :link '(url-link :tag "Ivy, Counsel and Swiper Tutorial"
                   "https://writequit.org/denver-emacs/presentations\
/2017-04-11-ivy.html")
  :link '(custom-group-link "counsel")
  :group 'pel-pkg-for-completion
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-counsel :package-is '(when pel-use-ivy
                                         '((elpa . counsel))))
;; counsel uses the request package but does not identify it as part
;; of its dependencies. Therefore I add the dependency info here.
(pel-put 'pel-use-counsel :requires-package '(quote ((elpa . lv)
                                                     (elpa . request))))
(pel-put 'pel-use-counsel :also-required-when 'pel-use-ivy-erlang-complete)

(defcustom pel-use-counsel-osx-app nil
  "Control whether `counsel-osx-app' is used when counsel is used on macOS.

With this package activated, PEL provides the ``<f11> A`` key sequence to
`counsel-osx-app'.  This allows selection of an macOS application using ivy
completion.

On Linux, the `counsel-linux-app` is bound to the same key if user option
variable `pel-use-counsel' is set to t."
  :link '(url-link :tag "counsel-osx-app @ GitHub"
                   "https://github.com/d12frosted/counsel-osx-app")
  :group 'pel-pkg-for-completion
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-use-ivy-hydra nil
  "Control whether `ivy-hydra' is used when counsel is used.

Using this also activates `pel-use-hydra'."
  :link '(url-link :tag "ivy-hydra @ MELPA"
                   "https://melpa.org/#/ivy-hydra")
  :group 'pel-pkg-for-completion
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-use-helm nil
  "Control whether PEL uses the Helm package.

Helm is a very powerful interactive incremental completion and
selection package which provides a large number of commands you
can execute on the completion list.
The initial completion mode is set by `pel-initial-completion-mode'.

Note that the following user options indirectly activates `pel-use-helm':
- `pel-use-helm-cscope',
- `pel-use-helm-lsp',
- `pel-use-helm-xref',
- `pel-use-indent-tools'`."

  :link '(url-link :tag "Helm home page"
                   "https://emacs-helm.github.io/helm/")
  :link '(url-link :tag "A package in a league of its own: Helm"
                   "https://tuhdo.github.io/helm-intro.html")
  :group 'pel-pkg-for-completion
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-helm :also-required-when '(or pel-use-helm-cscope
                                                pel-use-helm-xref
                                                pel-use-helm-lsp
                                                pel-use-indent-tools
                                                pel-use-helm-descbinds))

(defconst pel-USE-IDO     1 "Bitmask identifying Ido.      DON'T CHANGE!")
(defconst pel-USE-IVY     2 "Bitmask identifying Ivy.      DON'T CHANGE!")
(defconst pel-USE-COUNSEL 4 "Bitmask identifying Counsel.  DON'T CHANGE!")
(defconst pel-USE-HELM    8 "Bitmask identifying Helm.     DON'T CHANGE!")

(defcustom pel-initial-ido-geometry 'emacs-default
  "Select Ido prompt geometry used when Emacs starts.

PEL supports several Ido extension modes that modify the
Ido prompt geometry.  The following modes are available:

- `emacs-default'  : standard, linear IDO
- `ido-grid'       : show candidates in a grid, expanded.  Uses ido-grid.
- `grid-collapsed' : show candidates in a grid.  Collapsed on 1 line
                     at first.  Press tab to expand the grid on multiple
                     lines.
- `grid-expanded'  : show candidates in a grid.  Expanded right away.
- `vertical'       : show vertical list.

The `ido-grid' mode requires an activated `pel-use-ido-grid'.
The `grid-collapsed' and `grid-expanded' require that `pel-use-ido-grid' is off
an `pel-use-ido-grid-mode' activated.  If you select one of them when
`pel-use-ido-grid' is on, then PEL automatically selects `ido-grid' instead.
The vertical mode requires an activated `pel-use-ido-vertical-mode'."
  :group 'pel-pkg-for-completion
  :type '(choice
          (const :tag "Use Emacs default" emacs-default)
          (const :tag "Use grid (one state)" ido-grid)
          (const :tag "Use grid - collapsed" grid-collapsed)
          (const :tag "Use grid - expanded"  grid-expanded)
          (const :tag "Use vertical"         vertical)))

(defcustom pel-initial-completion-mode 'emacs-default
  "Select the main text completion mode used when Emacs starts.

PEL supports several completion engines.
This option selects which engine used when Emacs starts.
The available options are:
- nil           : Use Emacs default.
- `helm'        : Use Helm, when `pel-use-helm' is t.
- `ido'         : Use Ido, when `pel-use-ido' is t.
- `ido/helm'    : Use Ido with Helm, if both `pel-use-ido' and `pel-use-helm'
                  are t.
- `ivy'         : Use Ivy, when `pel-use-ivy' is t.
- `ivy/counsel' : Use Ivy with Counsel, when `pel-use-ivy' & `pel-use-counsel'
                  are both t."
  :group 'pel-pkg-for-completion
  :type '(choice
          (const :tag "Use Emacs default" emacs-default)
          (const :tag "Use Helm. Requires `pel-use-helm'." helm)
          (const :tag "Use Ido.  Requires `pel-use-ido'." ido)
          (const :tag "Use Ido with Helm. Needs `pel-use-ido' & `pel-use-helm'"
                 ido/helm)
          (const :tag "Use Ivy. Requires `pel-use-ivy'." ivy)
          (const :tag "Use Ivy & Counsel. Needs both `pel-use-ivy' and \
`pel-use-counsel'." ivy/counsel)))

(defcustom pel-initial-goto-symbol-UI 'emacs-default
  "Select the User Interface used for prompting for local symbol.

This is used by the command `pel-goto-symbol'.

Further customization is available for several selections:
- For Emacs default - imenu:
  - default is to use the tab-based completion buffer,
  - a pop-up imenu is available if you activate the following:
    - set `imenu-use-popup-menu' to \\='always
    - activate `pel-use-popup-imenu'.
- For Ido:
  - you can add Ido flx fuzzy matching by activating: `pel-use-flx'.

Warning: both `popup-imenu' and popup-switcher seem to be affected by
         bugs that prevent them to show the complete list of items
         reliably."
  :group 'pel-pkg-for-completion
  :type '(choice
          (const :tag "Use Emacs default - imenu" emacs-default)
          (const :tag "Use Ido.  Requires `pel-use-ido' and `pel-use-ido-ubiquitous'." ido)
          (const :tag "Use Ivy.  Requires `pel-use-ivy' and `pel-use-counsel'." ivy)
          (const :tag "Use helm. Requires `pel-use-helm'." helm)
          (const :tag "Use popup-imenu. Requires `pel-use-popup-imenu'."
                 popup-imenu)
          (const :tag "Use popup-switcher. Requires `pel-use-popup-switcher'."
                 popup-switcher)))

(defcustom pel-use-imenu-anywhere nil
  "Whether PEL uses the imenu-anywhere external package.

The imenu-anywhere package provides navigation for the imenu tags across
all buffers, not just the current one.  It supports several completion methods:

- Emacs default
- Ido
- Ivy
- Helm

Select the completion method you want as default when activating this package."
  :group 'pel-pkg-for-completion
  :group 'pel-pkg-for-imenu
  :link '(url-link :tag "imenu-anywhere @ GitHub"
                   "https://github.com/vspinu/imenu-anywhere")
  :type '(choice
          (const :tag "Not used." nil)
          (const :tag "Use Emacs default completion."      emacs-default)
          (const :tag "Use Ido.  Requires `pel-use-ido'."  ido)
          (const :tag "Use Ivy.  Requires `pel-use-ivy'."  ivy)
          (const :tag "Use Helm. Requires `pel-use-helm'." helm)))

(defcustom pel-prompt-read-method nil
  "Method used by PEL prompts listing file names and other lists."
  :group 'pel-pkg-for-completion
  :type '(choice
          (const :tag "Built-in (default)" nil)
          (const :tag "Use ivy vertical completion" ivy)))

;; ---------------------------------------------------------------------------
;; pel-pkg-for-cursor
;; ------------------
(defgroup pel-pkg-for-cursor nil
  "List of external packages for cursor management that may be used with PEL."
  :group 'pel-package-use
  :group 'cursor
  :link `(url-link :tag "Cursor PDF" ,(pel-pdf-file-url "cursor")))

(defcustom pel-use-multiple-cursors nil
  "Control whether PEL uses the multiple cursors package.

Activating the `pel-use-lispy' user-option indirectly activates
`pel-use-multiple-cursors'."
  :group 'pel-pkg-for-cursor
  :type 'boolean
  :safe #'booleanp
  :link '(url-link :tag "multiple-cursors @ GitHub"
                   "https://github.com/magnars/multiple-cursors.el"))
(pel-put 'pel-use-multiple-cursors :also-required-when 'pel-use-lispy)



;; ---------------------------------------------------------------------------
;; pel-pkg-for-cut-and-paste
;; -------------------------
(defgroup pel-pkg-for-cut-and-paste nil
  "List of external packages that PEL can use to control cut/paste, kill/yank."
  :group 'pel-package-use
  :link `(url-link :tag "Cut & Paste -- Copy/Delete/Kill/Yank PDF"
                   ,(pel-pdf-file-url "cut-paste")))

(defcustom pel-activate-overwrite-yank nil
  "Activate pel-overwrite-yank in place of yank.

When overwrite-yank is active a yank while overwrite-mode
is active overwrites text instead of inserting it.

Toggle this behaviour dynamically with `pel-toggle-overwrite-yank'."

  :group 'pel-pkg-for-cut-and-paste
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-with-cua-paste nil
  "Activate CUA-compliant C-v: bind C-v to yank.

When this is set to t, C-v is bound to yank instead of the default."
  :group 'pel-pkg-for-cut-and-paste
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-use-popup-kill-ring nil
  "Control whether PEL uses the `popup-kill-ring' package.
With this package selective yanking can be done with the
the Meta-y key which pops-up a menu listing the kill ring entries.

Notes:
- Unfortunately it does not work reliably in terminal (TTY) mode, so PEL
  only activates it in graphics mode.
- The version of this package on MELPA is version 0.2.8 and obsolete.
- The author maintains its latest version (0.2.11) in the EmacsWiki.
- PEL uses the EmacsWiki version."
  :link '(url-link :tag "popup-kill-ring @ EmacsWiki"
                  "https://www.emacswiki.org/emacs/popup-kill-ring.el")
  :link '(url-link :tag "Uncle Dave's YouTube video on popup-kill-ring"
                  "https://www.youtube.com/watch?v=LFXA089Tx38")
  :group 'pel-pkg-for-cut-and-paste
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-popup-kill-ring :restricted-to 'pel-emacs-is-graphic-p)

(defcustom pel-show-copy-cut-text t
  "Set whether PEL commands that copy, cut or kill text show it in echo area.
By default it is set to t.
If you find this display annoying set it to nil.

Like all user options, this setting is persistent.
If you want to modify this setting for the current session without
making the change persistent you can use the `pel-toggle-show-copy-cut-text'
command to change it either for the local buffer or globally."
  :group 'pel-pkg-for-cut-and-paste
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-use-browse-kill-ring nil
  "Whether PEL uses the `browse-kill-ring' package.

This adds the `browse-kill-ring' command to easily see the kill ring content."
  :group 'pel-pkg-for-cut-and-paste
  :type 'boolean
  :safe #'booleanp
  :link '(url-link :tag "@Github"
                   "https://github.com/browse-kill-ring/browse-kill-ring"))

(defcustom pel-kill-symbol-at-point-terminal-binding nil
  "Optional key binding in terminal mode for pel-kill-symbol-at-point.

On most terminals the [M-kp-subtract] binding does not work.
You can define another key binding for the command, ideally one
that will describe the same physical key as (kbd \"M-O 3 m\")
does on the the Ubuntu terminal configured as described for PEL.
If not are available you may also select something else that is
convenient.

When entering a string, that string is passed to the kbd function
to create the binding."
  :group 'pel-pkg-for-cut-and-paste
  :type '(choice
          (const  :tag "Default binding" nil)
          (string :tag "Use following kbd string argument")))

;; ---------------------------------------------------------------------------
;; pel-pkg-for-data-files
;; ----------------------
(defgroup pel-pkg-for-data-files nil
  "List of packages supporting data file formats."
  :group 'pel-package-use)

(defcustom pel-use-csv-mode nil
  "Whether PEL activates the csv-mode.

Requires Emacs 27.1 or later."
  :group 'pel-pkg-for-data-files
  :link '(url-link :tag "csv-mode @GNU Elpa"
                   "https://elpa.gnu.org/packages/csv-mode.html")
  :type 'boolean
  :safe #'booleanp)

;; ---------------------------------------------------------------------------
;; pel-pkg-for-diff-merge
;; ----------------------
(defgroup pel-pkg-for-diff-merge nil
  "List of packages activated to diff and merge buffer, files, directories."
  :group 'pel-package-use
  :link `(url-link :tag "Diff/Merge PDF" ,(pel-pdf-file-url "diff-merge")))

(defcustom pel-use-smerge nil
  "Whether PEL activates bindings for the built-in smerge package.

The smerge package provides simple 3-way merge commands dealing with the
standard diff annotated files."
  :group 'pel-pkg-for-diff-merge
  :type '(choice
          (const :tag "Don't use smerge" nil)
          (const :tag "Use smerge" t)
          (const :tag "Use smerge-mode automatically on diff annotated files"
                 auto)))

(defcustom pel-use-diffview-mode nil
  "Whether PEL activates diffview-mode package."
  :group 'pel-pkg-for-diff-merge
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-diffview-mode :package-is '(quote ((elpa . diffview))))

;; ---------------------------------------------------------------------------
;; pel-pkg-for-dired
;; -----------------
(defgroup pel-pkg-for-dired nil
  "List of packages activated for Dired support that may be used with PEL."
  :group 'pel-package-use
  :group 'dired
  :link `(url-link :tag "Dired PDF" ,(pel-pdf-file-url "mode-dired")))

(defcustom pel-use-dired-narrow nil
  "Control whether PEL uses the `dired-narrow' package."
  :group 'pel-pkg-for-dired
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-use-dired-sidebar nil
  "Control whether PEL activates the dired-sidebar."
  :link '(url-link :tag "dired-sidebar @ GitHub"
                   "https://github.com/jojojames/dired-sidebar")
  :group 'pel-pkg-for-dired
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-use-dired-x nil
  "Control whether PEL activates the Dired-X features in `dired-mode'."
  :group 'pel-pkg-for-dired
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-dired-x :package-is :builtin-emacs)

(defcustom pel-use-emacs-ls-emulation nil
  "Control whether Emacs ls emulation is used.
When activated, Emacs emulation of ls is used.
This provides better Dired support for environment where
the GNU Coreutils ls is not available.  The GNU Coreutils ls
can parse file names with leading spaces and supports the \"--dired\"
option.  See `dired-use-ls-dired' for more information."
  :group 'pel-pkg-for-dired
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-use-dired-hide-dotfiles nil
  "Control whether PEL activates dired-hide-dotfiles package.
The choices are:
- Do not use               : nil , default
- Use, activate later      : t
- Use, activate on startup : hide-dot-files-by-default"
  :link '(url-link :tag "dired-hide-dotfiles @ Github"
                   "https://github.com/mattiasb/dired-hide-dotfiles")
  :group 'pel-pkg-for-dired
  :type '(choice
          (const :tag "Do not use" nil)
          (const :tag "Use, activate later" t)
          (const :tag "Use, hide dot-files by default"
                 hide-dot-files-by-default)))

(defcustom pel-use-dired-git-info nil
  "Control whether dired-git-info package is made available."
  :group 'pel-pkg-for-dired
  :type '(choice
          (const :tag "Do not use" nil)
          (const :tag "Use, activate later" t)
          (const :tag "Use, activate automatically for Git directory"
                 on-for-git-directories)))

;; Possibly for the future.  If I have time to fix the code.
;; (defcustom pel-use-dired-toggle-sudo nil
;;   "Control whether PEL activates dired-toggle-sudo."
;;   :group 'pel-pkg-for-dired
;;   :type 'boolean
;;   :safe #'booleanp)

;; ---------------------------------------------------------------------------
;; Text Abbreviation, Code Completion and Expansion
;; ------------------------------------------------
(defgroup pel-pkg-for-expand nil
  "Text abbreviation, code completion and expansion control

- List of external packages that PEL can use to complete code or expand text.
- Automatic activation of abbreviation mode.

Note that auto-complete and company can both be activated.
However, PEL only allow one of them to be used per buffer.
The Hippie Expand can be used together with any."
  :group 'pel-package-use
  :link `(url-link :tag "Auto-Completion PDF"
                   ,(pel-pdf-file-url "auto-completion")))

(defcustom pel-use-auto-complete nil
  "Control whether PEL supports the {auto-complete} package."
  :group 'pel-pkg-for-expand
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-auto-complete :also-required-when 'pel-use-ac-geiser)

(defcustom pel-use-company nil
  "Control whether PEL supports the company package."
  :group 'pel-pkg-for-expand
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-company :also-required-when 'pel-use-company-erlang)

(defcustom pel-use-hippie-expand nil
  "Control whether PEL uses the {hippie-expand} package."
  :group 'pel-pkg-for-expand
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-hippie-expand :package-is :builtin-emacs)

(defcustom pel-modes-activating-abbrev-mode nil
  "List of major modes that automatically activate abbrev-mode."
  :group 'pel-pkg-for-expand
  :type '(repeat symbol))

;; ---------------------------------------------------------------------------
;; pel-pkg-for-conf-file
;; ---------------------
(defgroup pel-pkg-for-conf-file nil
  "List of external packages that manage configuration files."
  :group 'pel-package-use
  :group 'files)

(defcustom pel-use-ini nil
  "Whether PEL supports the ini package to read/write .ini files."
  :group 'pel-pkg-for-conf-file
  :link '(url-link :tag "ini @ Github"
                   "https://github.com/pierre-rouleau/ini.el")
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-ini :package-is :in-utils)
(pel-put 'pel-use-ini :also-required-when '(or (eq pel-c-file-finder-method
                                                   (quote pel-ini-file))
                                               (eq pel-c++-file-finder-method
                                                   (quote pel-ini-file))))

(defcustom pel-use-emacs-toml nil
  "Whether PEL supports the emacs-toml to read/write .toml files.

Note that is `pel-use-tomlparse' is t, `pel-use-tree-sitter' is t and
tree-sitter is available, then tomlparse is used instead of emacs-toml."
  :group 'pel-pkg-for-conf-file
  :link '(url-link :tag "emacs-toml @ Github"
                   "https://github.com/gongo/emacs-toml")
  :link '(url-link :tag "TOML @ Github"
                   "https://github.com/toml-lang/toml")
  :link '(url-link :tag "TOML manual"
                   "https://toml.io/en/")
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-tree-sitter :package-is '(quote ((elpa . tree-sitter-langs))))

(defcustom pel-use-tomlparse nil
  "Whether PEL supports tomlparse when tree-sitter is available.

This is only available when pel-use-tree-sitter is t (on)."
  :group 'pel-pkg-for-conf-file
  :link '(url-link :tag "tomlparse.el @ Github"
                   "https://github.com/johannes-mueller/tomlparse.el")
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-tomlparse :package-is :in-utils)

(defcustom pel-use-kconfig-mode nil
  "Whether PEL supports kconfig-mode for the Linux kernel Konfig files."
  :group 'pel-pkg-for-conf-file
  :link '(url-link :tag "kconfig-mode @ Github"
                   "https://github.com/delaanthonio/kconfig-mode#readme")
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-kconfig-mode :package-is :in-utils)

;; Docker Support
;; --------------

(defcustom pel-use-dockerfile-mode nil
  "Whether PEL supports the dockerfile-mode."
  :group 'pel-pkg-for-conf-file
  :link '(url-link :tag "dockerfile-mode @ Github"
                   "https://github.com/spotify/dockerfile-mode")
  :type 'boolean
  :safe #'booleanp)

;; ---------------------------------------------------------------------------
;; pel-pkg-for-filemng
;; -------------------
(defgroup pel-pkg-for-filemng nil
  "List of external packages that can be used to manage file/directory."
  :group 'pel-package-use
  :group 'files
  :link `(url-link :tag "File Management PDF" ,(pel-pdf-file-url "file-mngt")))

(defcustom pel-open-file-at-point-dir  nil
  "Root directory from where `pel-open-file-at-point' opens file.
This user-option acts as the default for a buffer.  You can change
the buffer\\='s local value with `pel-set-open-file-at-point-dir'
Can be one of the following:
- nil     : use parent directory of currently visited file, the default.
            If buffer is not visiting a file, then use the buffer\\='s current
            working directory.
- \\='cwd    : use buffer\\='s current working directory
- a string: the name of a specific directory."
  :group 'pel-pkg-for-filemng
  :type '(choice
          (const :tag "Use visited file's parent directory"  nil)
          (const :tag "Use buffer's current working directory" cwd)
          (directory :tag "Specific directory")))

(defcustom pel-delete-trailing-whitespace t
  "Controls whether whitespaces are automatically deleted when file is saved.
Deleted automatically when non-nil, don't otherwise or when the current
major-mode is one identified by the value of the
`pel-modes-preventing-delete-trailing-whitespace' user option."
  :group 'pel-pkg-for-filemng
  :type 'boolean
  :safe #'booleanp)

(defcustom  pel-modes-preventing-delete-trailing-whitespace '(diff-mode)
  "List of major modes that prevent automatic deletion of trailing whitespace.

Identify the major modes where you do *NOT* want automatic deletion of
trailing whitespace when the file is saved when the
`pel-delete-trailing-whitespace' user-option is turned on.
By default the `diff-mode' is in this list: it prevents removing the trailing
whitespace from diff files identifying differences between a file that had
trailing whitespace and one that does not have them."
  :group 'pel-pkg-for-filemng
  :type '(repeat symbol))

(defcustom pel-update-time-stamp t
  "Controls whether file timestamp is updated automatically on file save.
Update timestamp automatically when non-nil, don't otherwise.
See the time stamp format and location constraints in the Emacs manual
by executing:  M-: (info \"(emacs) Time Stamps\")."
  :group 'pel-pkg-for-filemng
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-update-time-stamp-pattern-regexps nil
  "List of regular expressions identifying time-stamps to update.
Each regular expression **must**:
- have **one** match group,
- use the string syntax.

The time stamp will be placed inside the identified match group.
The rest of the regular expression must be as complete as possible
to uniquely identify a text area inside the file to update."
  :group 'pel-pkg-for-filemng
  :type '(repeat
          (string :tag "regexp")))

(defcustom pel-update-copyright t
  "Controls whether copyright notice is updated automatically on file save.
Update copyright notice automatically when non-nil, don't otherwise."
  :group 'pel-pkg-for-filemng
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-make-script-executable t
  "Controls whether script files are automatically made executable when saved.
make script files executable on save when non-nil, don't otherwise."
  :group 'pel-pkg-for-filemng
  :group 'pel-pkg-for-sh-scripting
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-use-ffap    nil
  "Control whether PEL activates ffap bindings.
3 available choices:
- 1:  nil :=  ffap is not used.
- 2:  t   :=  use pel:ffap bindings, keeping default Emacs bindings for
              C-x C-f and other intact.
- 3:  ffap-bindings := Use the bindings documented by ffap.el by executing
      (ffap-bindings).  This replaces the bindings of several file finding
      commands and cannot be undone until this is changed and Emacs is
      re-started."
  :group 'pel-pkg-for-filemng
  :type '(choice
          (const :tag "Do not use" nil)
          (const :tag "Use pel:ffap bindings" t)
          (const :tag "Activate standard ffap bindings" ffap-bindings)))
(pel-put 'pel-use-ffap :package-is :builtin-emacs)

(defcustom pel-use-recentf nil
  "Control whether PEL activates the recentf built-in package.
This activates the \"Open Recent\" File menu.

PEL supports several functions that can be used with PEL's
`pel-find-recent-file' command and are selected by the
`pel-initial-recentf-function' user-option.

Note that activating that feature imposes a small impact on Emacs
init time because the recentf-mode must be activated during Emacs
initialization time."
  :group 'pel-pkg-for-filemng
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-recentf :package-is :builtin-emacs)


(defcustom pel-initial-recentf-function 'ido-recentf-open
  "Interactive function used to display recently opened files.
Note that:

- `pel-use-counsel' must be turned on to be able to use
  counsel-recentf,
- `pel-use-popup-switcher' must be turned on to be able to use
  psw-switch-recentf."
  :group 'pel-pkg-for-filemng
  :group 'pel-pkg-for-buffer
  :type '(choice
          (const :tag "ido-recentf-open   - Using ido"     ido-recentf-open)
          (const :tag "counsel-recentf    - Using counsel" counsel-recentf)
          (const :tag "psw-switch-recentf - Using popup-switcher"
                 psw-switch-recentf)))

(defcustom pel-ffind-executable 'find
  "Name of the file finding executable that pel-ffind uses.

Select one of:
- find    : the Unix ubiquitous find utility.
- fd      : the faster fd utility from https://github.com/sharkdp/fd.
- command : Any command line that has the following keywords that will
            be replaced by their corresponding string:
  -  {FNAME}    : the base name of the file.
  -  {DIRNAMES} : a space separated list of directory names to search.

  When this is selected an example sis placed in the input field.
"
  :group 'pel-pkg-for-filemng
  :group 'pel-pkg-for-c++
  :group 'pel-pkg-for-c
  :type '(choice
          (const :tag "Use Unix find" find)
          (const :tag "Use fd" fd)
          (string :tag "command line" :value "find -L {DIRNAMES} -name {FNAME} -type f")))


(defcustom pel-use-fzf nil
  "Control whether PEL activates the fzf.el package for fast fuzzy search."
  :group 'pel-pkg-for-filemng
  :group 'pel-pkg-for-grep
  :group 'pel-pkg-for-search
  :link '(url-link :tag "fzf.el @ Github"
                   "https://github.com/bling/fzf.el")
  :link '(url-link :tag "my fzf fork @ Github, used by PEL"
                   "https://github.com/pierre-rouleau/fzf.el")
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-fzf :package-is :in-utils)

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
(defgroup pel-pkg-for-rpm nil
  "PEL Archive File Browsing and Management."
  :group 'pel-pkg-for-filemng)

(defcustom pel-use-archive-rpm nil
  "Control if PEL supports the archive-rpm package."
  :group 'pel-pkg-for-rpm
  :link '(url-link :tag "used archive-rpm @ Github"
                   "https://github.com/nbarrientos/archive-rpm")
  :link '(url-link :tag "original archive-rpm @ Github"
                   "legoscia/archive-rpm")
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-use-rpm-spec-mode nil
  "Control if PEL supports the rpm-spec-mode package."
  :group 'pel-pkg-for-rpm
  :link '(url-link :tag "rpm-spec-mode @ Github"
                   "https://github.com/pierre-rouleau/rpm-spec-mode")
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-rpm-spec-mode :package-is :in-utils)
(pel-put 'pel-use-rpm-spec-mode :also-required-when 'pel-use-archive-rpm)

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; Directory Tree Browsing and Management
;; --------------------------------------

(defgroup pel-pkg-for-browse nil
  "PEL Directory Tree Browsing and Management."
  :group 'pel-pkg-for-filemng)

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
(defgroup pel-pkg-for-file-browse nil
  "PEL Local File And Directory Tree Browsing and Management."
  :group 'pel-pkg-for-browse)

(defcustom pel-use-rfc-mode nil
  "Control whether PEL provides access to rfc-mode external package."
  :group 'pel-pkg-for-file-browse
  :type 'boolean
  :safe #'booleanp
  :link '(url-link :tag "rfc-mode @ Github"
                   "https://github.com/galdor/rfc-mode"))

(defcustom pel-use-dir-treeview nil
  "Control whether PEL supports the dir-treeview package."
  :group 'pel-pkg-for-file-browse
  :type 'boolean
  :safe #'booleanp
  :link '(url-link :tag "emacs-dir-treeview @ Github"
                   "https://github.com/tilmanrassy/emacs-dir-treeview"))

(defcustom pel-use-treemacs nil
  "Control whether PEL uses the treemacs package."
  :group 'pel-pkg-for-file-browse
  :type 'boolean
  :safe #'booleanp
  :link '(url-link :tag "treemacs @ GitHub"
                   "https://github.com/Alexander-Miller/treemacs"))
(pel-put 'pel-use-treemacs :also-required-when '(or pel-use-lsp-treemacs
                                                    pel-use-treemacs-projectile
                                                    pel-use-treemacs-magit))

(defcustom pel-use-treemacs-projectile nil
  "Control whether PEL activates projectile extension for treemacs."
  :group 'pel-pkg-for-file-browse
  :group 'pel-pkg-for-project-mng
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-use-treemacs-magit nil
  "Control whether PEL activates magit extension for treemacs."
  :group 'pel-pkg-for-file-browse
  :group 'pel-pkg-for-git
  :type 'boolean
  :safe #'booleanp)

;;   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -
(defgroup pel-pkg-for-neotree nil
  "PEL extra configuration for NeoTree package."
  :group 'pel-pkg-for-file-browse)

(defcustom pel-use-neotree nil
  "Control whether PEL uses the Emacs NeoTree search package."
  :group 'pel-pkg-for-neotree
  :type 'boolean
  :safe #'booleanp
  :link '(url-link :tag "NeoTree @ GitHub"
                  "https://github.com/jaypei/emacs-neotree"))

(defcustom pel-neotree-font-in-terminal nil
  "NeoTree view font for directory node in terminal mode.
Default is to use + and - ."
  :group 'pel-pkg-for-neotree
  :type  '(choice
           (const :tag "Default" nil)
           (const :tag "Arrows" arrows)))

(defcustom pel-neotree-font-in-graphics nil
  "NeoTree view font for directory node in graphics mode.
Default is to use [+] and [-] .
The Icons choice uses the icons from the package all-the-icons."
  :group 'pel-pkg-for-neotree
  :type  '(choice
           (const :tag "Default" nil)
           (const :tag "Icons" icons))
  :link '(url-link :tag "all-the-icons @ GitHub"
                  "https://github.com/domtronn/all-the-icons.el"))

;;   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -
(defgroup pel-pkg-for-ztree nil
  "PEL extra configuration for ztree packages."
  :group 'pel-pkg-for-diff-merge
  :group 'pel-pkg-for-file-browse)

(defcustom pel-use-ztree nil
  "Control whether the ztree package is used."
  :group 'pel-pkg-for-ztree
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-ztree-dir-move-focus nil
  "Defines if move focus to opened window on hard-action command on a file.
Hard actions like RETURN.
PEL set `ztree-dir-move-focus' with this value."
  :group 'pel-pkg-for-ztree
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-ztree-dir-filter-list nil
  "List of regexp file names to filter out.

For example, if you want to ignore the pyc files, and the elc files use the
following list of regexp: (\"^.*\\.pyc\" \"^.*\\.elc\").

This list is prepended to the ztree-dir-filter-list,
a non-customizable variable."
  :group 'pel-pkg-for-ztree
  :type '(repeat string))

(defcustom pel-ztree-dir-show-filtered-files nil
  "Show or not files from the filtered list."
  :group 'pel-pkg-for-ztree
  :type 'boolean
  :safe #'booleanp)

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
(defgroup pel-pkg-for-web-browse nil
  "PEL Web Browsing Options.

See the documentation of the function `pel-help-pdf' for a description
of the impact these user-option variable have."
  :group 'pel-pkg-for-browse)

(defcustom pel-browser-used nil
  "Define which web browser to use when opening PEL PDF files."
  :group 'pel-pkg-for-web-browse
  :group 'pel-pkg-for-help
  :type '(choice
          (const :tag "Use browser selected by `browse-url-browser-function'" nil)
          (const :tag "Use Firefox." firefox)
          (const :tag "Use Chrome" chrome)))


(defcustom pel-flip-help-pdf-arg nil
  "When set, flip value of the pel-help-pdf argument.
When pel-flip-help-pdf-arg is set, the argument of the function `pel-help-pdf'
is reversed before used.

By default, `pel-help-pdf' opens the local PDF file when no
argument is passed, and the remote PDF file is opened when the
argument is specified.  If `pel-flip-help-pdf-arg' is set it's the other way
around."
  :group 'pel-pkg-for-web-browse
  :group 'pel-pkg-for-help
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-open-pdf-method 'pdf-viewer
  "Defines main/default method of opening local PEL PDF files.

The main method is either:

- pdf-viewer, or
- web-browser: the web browser identified by `pel-browser-used'.

The alternate method is the other one."
  :group 'pel-pkg-for-web-browse
  :group 'pel-pkg-for-help
  :type '(choice
          (const :tag "Open PEL PDF with PDF viewer." pdf-viewer)
          (const :tag "Open PEL PDF with web browser." web-browser)))


;; TODO: once PEL explicitly supports TCL, move the next user option into it.
(defcustom pel-tcl-man-section (if pel-system-is-linux-p "3tcl" "n")
  "Section of the Man pages for tcl major mode"
  :group 'pel-pkg-for-help
  :type 'string)

;; ---------------------------------------------------------------------------
;; Frame Control
;; -------------
(defgroup pel-pkg-for-frame nil
  "Frame Management Control."
  :group 'pel-package-use
  :group 'pel-pkg-for-window
  :link `(url-link :tag "Frames PDF" ,(pel-pdf-file-url "frames")))

(defcustom pel-use-framemove nil
  "Control whether PEL uses the framemove package.
It is similar to the Emacs builtin windmove and extends it to frames
when Emacs is used in graphics mode.

- Multiple frames *can* be used in terminal (TTY) mode but only one can
  be displayed at a time in the terminal window, therefore this package
 is not needed in terminal mode.
- This file is not available on MELPA (as of March 2021).
- PEL installs the copy available on Emacsmirror, which is version 0.10.
- The version 0.10 is also available via the relevant EmacsWiki page.
- Older version 0.9 used an obsolete function, that was fixed in
  version 0.10."
  :link '(url-link :tag "framemove @ EmacsMirror"
                   "https://github.com/emacsmirror/framemove")
  :link '(url-link :tag "framemove.el @ EmacsWiki"
                   "https://www.emacswiki.org/emacs/framemove.el")
  :link '(url-link :tag "Author's site: Emacs Tip# 35: framemove"
                   "http://trey-jackson.blogspot.com/2010/02/\
emacs-tip-35-framemove.html")
  :link '(url-link :tag "EmacsWiki framemove page"
                   "https://www.emacswiki.org/emacs/FrameMove")
  :link '(url-link :tag "Youtube video on windmove and framemove"
                   "https://www.youtube.com/watch?v=f3th2jyv35c")
  :group 'pel-pkg-for-frame
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-framemove :package-is :in-utils)
(pel-put 'pel-use-framemove :restricted-to 'pel-emacs-is-graphic-p)

;; ---------------------------------------------------------------------------
;; Support for Emacs Running in Graphics Mode
;; ------------------------------------------
(defgroup pel-pkg-for-graphics-emacs nil
  "List of external packages that PEL can use for Emacs in graphics mode."
  :group 'pel-package-use)

(defcustom pel-use-all-the-icons nil
  "Control whether PEL uses the all-the-icons package.
This is only used when Emacs runs in graphics mode."
  :group 'pel-pkg-for-graphics-emacs
  :type 'boolean
  :safe #'booleanp
  :link '(url-link :tag "all-the-icons @ GitHub"
                  "https://github.com/domtronn/all-the-icons.el"))
(pel-put 'pel-use-all-the-icons :restricted-to 'pel-emacs-is-graphic-p)

(defcustom pel-use-all-the-icons-ibuffer nil
  "Control whether PEL uses the all-the-icons package in ibuffer.
This is only used when Emacs runs in graphics mode."
  :group 'pel-pkg-for-graphics-emacs
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-all-the-icons-ibuffer :restricted-to 'pel-emacs-is-graphic-p)

(defcustom pel-use-all-the-icons-dired nil
  "Control whether PEL uses the all-the-icons package in dired.
This is only used when Emacs runs in graphics mode."
  :group 'pel-pkg-for-graphics-emacs
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-all-the-icons-dired :restricted-to 'pel-emacs-is-graphic-p)

(defcustom pel-use-all-the-icons-ivy nil
  "Control whether PEL uses the all-the-icons package in ivy.
This is only used when Emacs runs in graphics mode."
  :group 'pel-pkg-for-graphics-emacs
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-all-the-icons-ivy :restricted-to 'pel-emacs-is-graphic-p)

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; Graphics Cursor Control
;; -----------------------

(defgroup pel-pkg-for-graphics-cursor nil
  "Control graphics mode cursor."
  :group 'pel-pkg-for-graphics-emacs)

(defface pel-cursor-overwrite-mode-color
  '((((background light)) :background "black")
    (((background dark))  :background "white"))
  "Cursor face of cursor in overwrite-mode.
Takes effects only when Emacs runs in graphics mode."
  :group 'pel-pkg-for-graphics-cursor
  :group 'pel-pkg-for-cursor
  :group 'cursor)

(defcustom pel-cursor-type-when-mark nil
  "Cursor type used when the mark is active.
If nil, the cursor type does not change when mark is active.
Otherwise the choices are:
 - bar
 - box
 - hollow
These only take effect when Emacs is running in graphics mode."
  :group 'pel-pkg-for-graphics-cursor
  :group 'pel-pkg-for-cursor
  :group 'cursor
  :type '(choice
          (const :tag "No change - use default cursor type" nil)
          (const :tag "bar" bar)
          (const :tag "box" box)
          (const :tag "hollow" hollow)))

;; ---------------------------------------------------------------------------
;; pel-pkg-for-grep
;; ----------------
(defgroup pel-pkg-for-grep nil
  "List of external packages that PEL can use for grep operations."
  :group 'pel-package-use
  :group 'grep
  :link `(url-link :tag "Grep PDF" ,(pel-pdf-file-url "grep")))

(defcustom pel-use-ag nil
  "Control whether PEL uses the ag tool."
  :group 'pel-pkg-for-grep
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-use-deadgrep nil
  "Control whether PEL uses the deadgrep external package.
The deadgrep Emacs packages uses the ripgrep command line utility
and implements a search mechanism that uses a dedicated deadgrep
buffer with buttons that provide extra commands.
It works well with Emacs in graphics mode with the mouse.
It also supports the terminal mode.

For the moment PEL uses my fork, which provides more features."
  :link '(url-link :tag "my fork of deadgrep @ GitHub"
                   "https://github.com/pierre-rouleau/deadgrep")
  :link '(url-link :tag "deadgrep @ GitHub"
                   "https://github.com/Wilfred/deadgrep")
  :group 'pel-pkg-for-grep
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-deadgrep :package-is :in-utils)

(defcustom pel-use-ripgrep nil
  "Control whether PEL uses the ripgrep tool and its associated packages.
Ripgrep is a very fast grep utility, and two packages support ripgrep:

- the rg package,
- the ripgrep package.

Setting `pel-use-ripgrep' to t indicates that you want to use ripgrep, so
it identifies the installation of the `rg` package.  If you also set
`pel-use-projectile' to non-nil, then the installation of the `ripgrep`
package is also required because `projectile` uses the `ripgrep` package."
  :link '(url-link :tag "rg @ Github"
                  "https://github.com/dajva/rg.el")
  :link '(url-link :tag "ripgrep @ GitHub"
                  "https://github.com/BurntSushi/ripgrep")
  :link '(url-link :tag "Emacs ripgrep @ GitHub"
                  "https://github.com/nlamirault/ripgrep.el")
  :group 'pel-pkg-for-grep
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-ripgrep :package-is '(if pel-use-projectile
                                           '((elpa . rg)
                                             (elpa . ripgrep))
                                         '((elpa . rg))))

(defcustom pel-use-wgrep nil
  "Control whether PEL uses the wgrep external package."
  :link '(url-link :tag "wgrep @ Github"
                   "https://github.com/mhayashi1120/Emacs-wgrep")
  :group 'pel-pkg-for-grep
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-wgrep :also-required-when'(or pel-use-ripgrep
                                                pel-use-ivy))

;; ---------------------------------------------------------------------------
(defgroup pel-pkg-for-help nil
  "PEL help/documentation control support."
  :group 'pel-package-use
  :link `(url-link :tag "Help PDF" ,(pel-pdf-file-url "help")))

(defcustom pel-help-under-ssh nil
  "Control whether PEL opens PDF or web Help when running under SSH.

By default PEL won't open a local PDF or Web-based PEL help PDF
when running under SSH, since most of the time you won't be able to see it.
If you can still see it because you have access to your host through another
mechanism and still want to allow Emacs to open these PEL help PDF file, then
turn this on (set to t)."
  :group 'pel-pkg-for-help
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-use-debbugs nil
  "Control whether PEL installs and activates debbugs external package.

This package is used to browse the GNU Bug Tracker within Emacs."
  :link '(url-link :tag "debbugs @ GNU Elpa"
                   "https://elpa.gnu.org/packages/debbugs.html")
  :group 'pel-pkg-for-help
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-use-helpful nil
  "Control whether PEL provides access to the helpful external package."
  :group 'pel-pkg-for-help
  :link '(url-link :tag "helpful @ GitHub"
                   "https://github.com/Wilfred/helpful")
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-use-helpful-with-counsel nil
  "Control whether helpful use counsel commands."
  :group 'pel-pkg-for-help
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-emacs-refcard-dirpath nil
  "Path name of a directory holding Emacs PDF reference cards.

If the function `pel-open-emacs-refcard' is not able to find the location
of the GNU Emacs reference cards, download them from the site linked below,
store them inside a directory and identify that directory here."
  :link '(url-link :tag "GNU Emacs Reference Cards home page"
                   "https://www.gnu.org/software/emacs/refcards/index.html")
  :group 'pel-base-emacs
  :type '(choice
          (const :tag "Locate automatically" nil)
          (string :tag "Use files in directory")))

;; ---------------------------------------------------------------------------
;; Hide/Show support
;; -----------------

(defgroup pel-pkg-for-hide-show nil
  "PEL hide/show generic control support."
  :group 'pel-package-use
  :link `(url-link :tag "Hide/Show PDF" ,(pel-pdf-file-url "hide-show-code")))

(defcustom pel-use-hide-lines nil
  "Control whether PEL uses the hide-lines package."
  :group 'pel-pkg-for-hide-show
  :link '(url-link :tag "hide-lines @ GitHub"
                   "https://github.com/vapniks/hide-lines")
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-use-origami nil
  "Control whether PEL uses the origami package.
This is a text folding package that supports several major mode
and is also supported by LSP servers."
  :group 'pel-pkg-for-hide-show
  :link '(url-link :tag "origami @ GitHub"
                   "https://github.com/gregsexton/origami.el")
  :link '(url-link :tag "My fork - until work is merged"
                   "https://github.com/pierre-rouleau/origami.el")
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-origami :package-is '(quote ((utils . origami)
                                               (utils . origami-parsers))))
(pel-put 'pel-use-origami :requires-package '(quote ((elpa . dash)
                                                     (elpa . s))))
(pel-put 'pel-use-origami :also-required-when 'pel-use-lsp-origami)

;; ---------------------------------------------------------------------------
;; Highlight Support
;; -----------------

(defgroup pel-pkg-for-highlight nil
  "PEL highlight control support."
  :group 'pel-package-use
  :link `(url-link :tag "Highlighting PDF" ,(pel-pdf-file-url "highlight")))

(defcustom pel-use-beacon-mode nil
  "Control whether PEL uses and activates the beacon-mode.

PEL currently restricts it to Emacs 27 and later."
  :group 'pel-pkg-for-highlight
  :link '(url-link :tag "beacon-mode @ GitHub"
                   "https://github.com/Malabarba/beacon")
  :type '(choice
          (const :tag "Do not use" nil)
          (const :tag "Use, activate later by command"  t)
          (const :tag "Use, activate globally when Emacs starts"
                 use-from-start)))

(defcustom pel-use-fill-column-indicator nil
  "Control whether PEL uses fill-column-indicator package.

For Emacs versions earlier than 27.1 set it to activate the ability to
highlight the current `fill-column', the column where automatic line
wrapping occurs and to activate the PEL key bindings for it.
Not used nor needed for Emacs 27.1 or later: the PEL key bindings for
that command are always enabled for Emacs 27.1 or later.

The activated PEL key sequences are:
- <f11> b h \\
- <f11> 8"
  :group 'pel-pkg-for-highlight
  :type 'boolean
  :safe #'booleanp
  :link '(url-link :tag "fill-column-indicator @ GitHub"
                   "https://github.com/alpaker/fill-column-indicator"))

(defcustom pel-use-vline nil
  "Control whether PEL uses the `vline' package.
When set, PEL binds the following key sequences `vline-mode'
which toggles the highlighting of the current column in the
current window:
- <f11> 9
- <f11> b h |"
  :group 'pel-pkg-for-highlight
  :type 'boolean
  :safe #'booleanp
  :link '(url-link :tag "vline.el @ emacsmirror"
                   "https://github.com/emacsmirror/vline")
  :link '(url-link :tag "vline @ EmacsWiki"
                   "https://www.emacswiki.org/emacs/VlineMode"))
(pel-put 'pel-use-vline :package-is :in-utils)

(defcustom pel-use-rainbow-mode nil
  "Control whether PEL uses the `rainbow-mode' package.
When activated the color code value constant strings in code file
are highlighted with the color represented by the color code in the string.

PEL binds command `rainbow-mode' to ``<f11> b h c``."
  :link '(url-link :tag "rainbow-mode example"
                   "https://jblevins.org/log/rainbow-mode")
  :link '(url-link :tag "rainbow-mode @ Elpa"
                   "https://elpa.gnu.org/packages/rainbow-mode.html")
  :group 'pel-pkg-for-highlight
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-use-auto-highlight-symbol nil
  "Control whether PEL activates the auto-highlight-symbol external package."
  :link '(url-link :tag "auto-highlight-symbol @ GitHub"
                   "https://github.com/jcs-elpa/auto-highlight-symbol")
  :group 'pel-pkg-for-highlight
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-use-highlight-indentation nil
  "Control whether PEL activates the highlight-indentation external package."
  :link '(url-link :tag "highlight-indentation @ Github"
                   "https://github.com/antonj/Highlight-Indentation-for-Emacs")
  :group 'pel-pkg-for-highlight
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-highlight-color-default "lightgreen"
  "Default color used by `pel-highlight-line'."
  :group 'pel-pkg-for-highlight
  :type 'string)

;; ---------------------------------------------------------------------------
(defgroup pel-pkg-for-iedit nil
  "PEL customization of the iedit-mode."
  :group 'pel-pkg-for-all-languages
  :group 'pel-pkg-for-cursor
  :group 'pel-pkg-for-highlight)

(defcustom pel-use-iedit nil
  "Control whether PEL uses the iedit package.

When set PEL activates the iedit mode when one of the following key
sequences are typed:

- C-;
- <f11> e
- <f11> m i

Activating the `pel-use-lispy' user-option indirectly activates
`pel-use-iedit'."
  :group 'pel-pkg-for-iedit
  :type 'boolean
  :safe #'booleanp
  :link '(url-link :tag "Active fork of lispy while abo-abo is inactive on it"
                   "https://github.com/enzuru/lispy")
  :link '(url-link :tag "iedit @ GitHub"
                   "https://github.com/victorhge/iedit"))
(pel-put 'pel-use-iedit :also-required-when 'pel-use-lispy)

(defcustom pel-iedit-use-alternate-keys t
  "Activate PEL alternate keys for navigation and selection toggle."
  :group 'pel-pkg-for-iedit
  :type 'boolean
  :safe #'booleanp)

;; ---------------------------------------------------------------------------
;; iMenu extension support
;; -----------------------
;;
(defgroup pel-pkg-for-imenu nil
  "List of external packages and variables that PEL use to extend imenu."
  :group 'pel-package-use
  :group 'imenu
  :link '(custom-group-link "pel-pkg-for-completion")
  :link `(url-link :tag "Menu/iMenu PDF"
                   ,(pel-pdf-file-url "menus")))

(defcustom pel-imenu-index-follows-order-p nil
  "Control how imenu index entries are listed:
- nil: the entries with sub-menus are shown at the top (original, standard
       behaviour).
- t:   the entries are shown as an outline: in the exact same other
       as they appear in the buffer/file."
  :group 'pel-pkg-for-imenu
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-use-imenu+ nil
  "Control whether PEL provides access to imenu+ external package."
  :link '(url-link :tag "Imenu+ description @ emacswiki"
                   "https://www.emacswiki.org/emacs/ImenuMode#h5o-10")
  :link '(url-link :tag "imenu+.el @ emacswiki"
                   "https://www.emacswiki.org/emacs/imenu%2b.el")
  :group 'pel-pkg-for-imenu
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-imenu+ :package-is :in-utils)

(defcustom pel-use-imenu-extra nil
  "Control whether PEL provides access to imenu-extra external package."
  :link '(url-link :tag "imenu-extra @ GitHub"
                   "https://github.com/redguardtoo/imenu-extra")
  :group 'pel-pkg-for-imenu
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-imenu-extra :package-is :in-utils)

(defcustom pel-use-flimenu nil
  "Control whether PEL provides access to the flimenu package.
It provides a local and a global minor mode that flattens the
iMenu lists.  Useful even in other completion prompts by
eliminating the hierarchical access and provide a flat list."
  :link '(url-link :tag "flimenu @ GitHub"
                   "https://github.com/IvanMalison/flimenu")
  :group 'pel-pkg-for-imenu
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-use-popup-imenu nil
  "Control whether PEL provides access to the popup-imenu package.

When available and when `pel-use-popup-switcher' is not active,
PEL uses it for the pel-goto-symbol when the popup-imenu
mode is selected."
  :link '(url-link :tag "popup-imenu @ GitHub"
                   "https://github.com/ancane/popup-imenu")
  :link '(url-link :tag "popup-imenu fork with customization support, used by PEL"
                   "https://github.com/pierre-rouleau/popup-imenu")
  :group 'pel-pkg-for-imenu
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-popup-imenu :package-is :in-utils)
(pel-put 'pel-use-popup-imenu :requires-package '(quote ((elpa . dash)
                                                         (elpa . popup)
                                                         (elpa . flx-ido))))

(defcustom pel-use-popup-switcher nil
  "Control whether PEL provides access to the popup-switcher package.

This package provides popup menu for navigating across:
  - functions/methods
  - buffers
  - files
  - recent files
  - projectile files
  - projectile projects

When available PEL uses it for the pel-goto-symbol when the popup-imenu
mode is selected.
Note: popup-switcher 2.14 has several bugs I fixed in my fork, which PEL
      uses until they're integrated."
  :link '(url-link :tag "popup-switcher @ GitHub"
                   "https://github.com/kostafey/popup-switcher")
  :link '(url-link :tag "popup-switcher fork wit bug fixed"
                   "https://github.com/pierre-rouleau/popup-switcher")
  :group 'pel-pkg-for-imenu
  :group 'pel-pkg-for-completion
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-popup-switcher :package-is :in-utils)

;; ---------------------------------------------------------------------------
;; Indentation Control
;; -------------------

(defgroup pel-pkg-for-indentation nil
  "List of packages that PEL can use to support indentation facilities."
  :group 'pel-package-use
  :link `(url-link :tag "Indentation PDF" ,(pel-pdf-file-url "indentation")))

(defcustom pel-use-indent-tools nil
  "Whether PEL activates the indent-tools external package."
  :group 'pel-pkg-for-indentation
  :link '(url-link :tag "indent-tools @ GitLab"
                   "https://gitlab.com/emacs-stuff/indent-tools/")
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-indent-tools-key-bound nil
  "If set `C-c >` is bound to `indent-tools-hydra/body' globally or for Python."
  :group 'pel-pkg-for-indentation
  :type '(choice
          (const :tag "Not available" nil)
          (const :tag "Available globally" globally)
          (const :tag "Available in python-mode" python)))

(defcustom pel-use-smart-shift nil
  "Whether PEL activates the smart-shift external package."
  :group 'pel-pkg-for-indentation
  :link '(url-link :tag "smart-shift @ Github"
                   "https://github.com/hbin/smart-shift")
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-smart-shift-keybinding nil
  "Control key binding of the smart-shift commands.
By default the binding set up by smart-shift is used.
You can select one of 2 alternatives."
  :group 'pel-pkg-for-indentation
  :type '(choice
          (const :tag "smart-shift default" nil)
          (const :tag "Use C-c C-<cursor>" control-cursor)
          (const :tag "Use <f9> <cursor>"  f9)))

;; ---------------------------------------------------------------------------
;; Insertion of Text & Templates
;; -----------------------------
(defgroup pel-pkg-for-insertions nil
  "List of external packages that PEL can use to provide easy text insertion."
  :group 'pel-package-use
  :link `(url-link :tag "Inserting Text PDF"
                   ,(pel-pdf-file-url "inserting-text")))

(defcustom pel-activate-hydra-for-greek nil
  "Control whether PEL activates the Hydra for Greek letters.

When activated, the Greek Hydra is started with ``<f7> <f6> <f6>``.

If you activate that feature, the `pel-use-hydra' user-option will
automatically turned on."
  :group 'pel-pkg-for-insertions
  :group 'pel-pkg-for-text-mode
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-activate-f9-for-greek nil
  "Control whether <f9> key is used as key prefix to insert greek letters.
This is a PEL built-in logic that is not activated by default, leaving the
<f9> key unbound.  When turned on, the <f9> key is the key prefix to other
keys that insert Greek letters, with correspondences between ASCII letters and
Greek letters.
Note that PEL also provides the ``<f6> g`` key binding for Greek letters."
  :group 'pel-pkg-for-insertions
  :group 'pel-pkg-for-text-mode
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-use-smart-dash nil
  "Control whether PEL activates the smart-dash package.
This helps inserting underscore characters by typing the dash key without
having to hit the Shift key.
See the author site at URL http://malsyned.net/smart-dash.html"
  :group 'pel-pkg-for-insertions
  :group 'pel-pkg-for-text-mode
  :group 'pel-pkg-for-c
  :group 'pel-pkg-for-c++
  :group 'pel-pkg-for-d
  :group 'pel-pkg-for-go
  :group 'pel-pkg-for-erlang
  :group 'pel-pkg-for-elixir
  :group 'pel-pkg-for-pike
  :group 'pel-pkg-for-python
  :group 'pel-pkg-for-rust
  :group 'pel-pkg-for-sh-scripting
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-modes-activating-smart-dash-mode
  '(c-mode
    c++-mode
    d-mode
    elixir-mode
    pike-mode
    python-mode
    shell-script-mode)
  "List of major modes that automatically activate the smart dash mode.
Use this only when the major mode is not supported by PEL.  For modes
supported by PEL use the appropriate pel-<mode>-activates-minor-modes
user-option instead.
Used when `pel-use-smart-dash' user option is t."
  :group 'pel-pkg-for-insertions
  :group 'pel-pkg-for-text-mode
  :type  '(repeat symbol))

(defcustom pel-use-yasnippet nil
  "Control whether PEL uses yasnippet package."
  :group 'pel-pkg-for-insertions
  :type '(choice
          (const :tag "Do not use" nil)
          (const :tag "Use, activate later by command"  t)
          (const :tag "Use, activate globally when Emacs starts"
                 use-from-start)))
(pel-put 'pel-use-yasnippet :also-required-when 'pel-use-verilog-ext)

(defcustom pel-use-yasnippet-snippets nil
  "Control whether PEL uses the yasnippet-snippets package.
That package loads a set of snippets for yasnippet.
PEL activates it only if variable `pel-use-yasnippet' is non-nil."
  :group 'pel-pkg-for-insertions
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-yasnippet-snippets :requires 'pel-use-yasnippet)

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
(defgroup pel-pkg-for-license-insertion nil
  "Customization of software License control."
  :group 'pel-pkg-for-insertions)

(defcustom pel-use-lice nil
  "Control whether PEL uses the lice package to insert software license text.
Note that the lice package is installed when its used in PEL tempo skeletons
as requested by their respective user-options."
  :group 'pel-pkg-for-license-insertions
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-lice :also-required-when
         '(or (eq pel-c-skel-with-license t)
              (eq pel-clisp-skel-with-license t)
              (eq pel-elisp-skel-with-license t)
              (eq pel-erlang-skel-with-license t)))

(defcustom pel-use-spdx nil
  "Control whether PEL provides support for spdx.el."
  :link '(url-link :tag "spdx.el @ Github"
                   "https://github.com/condy0919/spdx.el")
  :group 'pel-pkg-for-license-insertions
  :type 'boolean
  :safe #'booleanp)

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
(defgroup pel-text-insertions nil
  "Customization of text insertions."

  :group 'pel-pkg-for-insertions)

(defcustom pel-insert-filename-root nil
  "Absolute path to strip from path inserted by `pel-insert-filename'.

This is a default used as the initialization value of a buffer local
variable used by the `pel-insert-filename' command unless the
PEL_INSERT_FILENAME_ROOT environment variable exists.  In that case the
initial value is taken from environment variable."

  :group 'pel-text-insertions
  :type 'string)

(defcustom pel-todo-note-text "[:todo (DATE), by (USER): ]"
  "Format string for to-do notes.

The string may contain the following 2 replacement string codes
that are expanded when the note is inserted:

- \"(DATE)\" : replaced by a UTC date using the format specified
  by `pel-todo-note-date-format',
- \"(USER)\" : replaced by the value of `user-full-name'.
"
  :group 'pel-text-insertions
  :type 'string)


(defun pel--date-time-doc (fmt doc)
  (format
   "%s

The current default was: '%s'.
 which generated the following text at the time this was built:
 - for local time: %s
 - for UTC time:   %s"
   doc
   fmt
   (format-time-string fmt)
   (format-time-string fmt nil t)))

(defcustom pel-todo-note-date-format "%F"
  (pel--date-time-doc "%F"
                      "Date format inserted by `pel-insert-todo-note' command.

Used as specified in `pel-todo-note-text'")
  :group 'pel-text-insertions
  :type 'string)

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; Date/time insertion format selection
;; ------------------------------------
;;
;; There are 8 date/time insertion commands. Each of these commands support
;; user-specified text format and the ISO-8601 text format.  The user-selected
;; format are specified by the following customization variables.

;; Format               |  Date only  | Date & Week-day | Date & Time    | Date, Week-day & Time
;; ————————————————-————|-——————————-—|————————————————-|————————————————|——————————————————————
;; User-selected format | <f11> i d   | <f11> i D       | <f11> i t      | <f11> i T
;; ISO-8601 format      | <f11> i M-d | <f11> i M-D     | <f11> i M-t    | <f11> i M-T

(defgroup pel-date-time-insertion nil
  "Customization of date and time insertion.

All strings must be compatible with the `format-time-string'
format strings.  The iso format strings are meant to generate
ISO-8601 compliant date/time format strings."

  :group 'pel-text-insertions
  :link '(url-link :tag "ISO-8601 @ Wikipedia"
                   "https://en.wikipedia.org/wiki/ISO_8601"))


(defcustom pel-date-only-format "%F"
  (pel--date-time-doc "%F"
                      "Date-only `format-time-string' used by the `pel-insert-date' command.")
  :group 'pel-date-time-insertion
  :type 'string)

(defcustom pel-date-wkd-format "%A, %B %d %Y"
  (pel--date-time-doc "%A, %B %d %Y"
                      "Date & Week-day `format-time-string' used by the `pel-insert-date-wkd' command.")
  :group 'pel-date-time-insertion
  :type 'string)

(defcustom pel-date-wkd-short-format "%a %b %d %Y"
  (pel--date-time-doc "%a %b %d %Y"
                      "Short date & Week-day `format-time-string' used by the `pel-insert-date-wkd-short' command.")
  :group 'pel-date-time-insertion
  :type 'string)

(defcustom pel-date-time-format "%F %T %Z"
  (pel--date-time-doc "%F %T %Z"
                      "Date & Time `format-time-string' used by the `pel-insert-date-time' command.")
  :group 'pel-date-time-insertion
  :type 'string)

(defcustom pel-date-wkd-time-format "%A, %B %d %Y at %T %Z"
  (pel--date-time-doc "%A, %B %d %Y at %T %Z"
                      "Date & Time `format-time-string' used by the `pel-insert-date-wkd-time' command.")
  :group 'pel-date-time-insertion
  :type 'string)

(defcustom pel-date-wkd-time-short-format "%a %b %d %Y at %T %Z"
  (pel--date-time-doc "%a %b %d %Y at %T %Z"
                      "Short Date & Time `format-time-string' used by the `pel-insert-date-wkd-time-short' command.")
  :group 'pel-date-time-insertion
  :type 'string)

(defcustom pel-date-only-iso-format "%F"
  (pel--date-time-doc "%F"
                      "Date-only `format-time-string' used by the `pel-insert-iso-date' command.")
  :group 'pel-date-time-insertion
  :type 'string)

(defcustom pel-date-wkd-iso-format "%F-W%V-%u"
  (pel--date-time-doc "%F-W%V-%u"
                      "Date & Week-day `format-time-string' used by the `pel-insert-iso-date-wkd' command.")
  :group 'pel-date-time-insertion
  :type 'string)

(defcustom pel-date-time-iso-format "%FT%T%z"
  (pel--date-time-doc "%FT%T%z"
                      "Date & Time `format-time-string' used by the `pel-insert-iso-date-time' command.")
  :group 'pel-date-time-insertion
  :type 'string)

(defcustom pel-date-wkd-time-iso-format "%FT%T%z W%V-%u"
  (pel--date-time-doc "%FT%T%z W%V-%u"
                      "Date-only `format-time-string' used by the `pel-insert-iso-date-wkd-time' command.")
  :group 'pel-date-time-insertion
  :type 'string)

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; Parens block management
;; -----------------------

(defgroup pel-pkg-for-parens nil
  "List of external packages that PEL can use to help deal with parens.

The word \"parens\" is a generic term that describes the following
grouping characters:
- parenthesis:     '(' and ')',
- braces:          '{' and '}'
- square brackets: '[' and ']'
- angle brackets:  '<' and '>'
- and potentially the quote characters:
  - single quote:    '
  - double quote:    \""
  :group 'pel-pkg-for-insertions
  :group 'pel-pkg-for-all-languages
  :link `(url-link :tag "Emacs Lisp PDF" ,(pel-pdf-file-url "pl-emacs-lisp"))
  :link `(url-link :tag "Common Lisp PDF" ,(pel-pdf-file-url "pl-common-lisp"))
  :link `(url-link :tag "Diff & Merge PDF" ,(pel-pdf-file-url "diff-merge")))

(defcustom pel-use-parinfer nil
  "Control whether PEL uses the parinfer package.

Note that this package is obsolete, the author failed to complete a fast
enough implementation using Emacs Lisp.  There is, however, a successor,
implemented in Rust, parinfer-rust-mode that can be used instead.  Activate
that with `pel-use-parinfer-rust-mode'.

If you have an old installation of parinfer downloaded via Melpa, and you
request a PEL cleanup, your copy will be stored inside your
~/.emacs.d/elpa-attic directory.

New installations will be done using the files from EmacsAttic.

To activate this package select one of the options:
- use-local-elpa-attic-copy but only if you have an old copy in your
  elpa-attic.
- use-emacs-attic for all other case."
  :link '(url-link :tab "parinfer manual"
                   "https://shaunlebron.github.io/parinfer/")
  :link '(url-link :tab "parinfer @ EmacsAttic"
                   "https://github.com/emacsattic/parinfer")
  :link '(url-link :tab "parinfer @ GitHub, archived."
                   "https://github.com/shaunlebron/parinfer")
  :group 'pel-pkg-for-parens
  :type '(choice
          (const :tag "Don't use" nil)
          (const :tag "Use emacsattic site files" t)
          (const :tag "Use local Elpa attic copy" use-pel-elpa-attic-copy)))
(pel-put 'pel-use-parinfer :package-is '(if (eq pel-use-parinfer
                                                'use-pel-elpa-attic-copy)
                                            '((elpa . parinfer))
                                          '((utils . parinfer))))
;; parinfer is no longer available in MELPA.
;; If you have it in an attic directory it will be used.
;; The dependencies are no longer retrievable trough MELPA,
;; so they are identified here.
(pel-put 'pel-use-parinfer :requires-package '(quote ((elpa . dash))))

(defcustom pel-use-rainbow-delimiters nil
  "Control whether PEL uses the rainbow-delimiters package."
  :group 'pel-pkg-for-highlight
  :group 'pel-pkg-for-parens
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-use-smartparens nil
  "Control whether PEL activates the smartparens package."
  :group 'pel-pkg-for-parens
  :type 'boolean
  :safe #'booleanp)

;; ---------------------------------------------------------------------------
;; pel-pkg-for-kbmacro
;; -------------------
(defgroup pel-pkg-for-kbmacro nil
  "List of external packages that PEL can use to handle keyboard macros."
  :group 'pel-package-use
  :group 'kmacro
  :link `(url-link :tag "Keyboard Macros PDF"
                   ,(pel-pdf-file-url "keyboard-macros"))
  :link `(url-link :tag "Function Keys Usage PDF"
                   ,(pel-pdf-file-url "keys-fn")))

(defcustom pel-kbmacro-prompts nil
  "Prompt before overriding existing keyboard macro?
By default it does not prompt.
If on (t) the keyboard macro recording will prompt before overriding the
  previously recorded keyboard macro."
  :group 'pel-pkg-for-kbmacro
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-use-centimacro nil
  "Whether PEL use the centimacro package.
See repository at URL https://github.com/abo-abo/centimacro"
  :group 'pel-pkg-for-kbmacro
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-centimacro :package-is :in-utils)

(defcustom pel-centi-assign-key "<C-f5>"
  "Default key binding for function `centi-assign'.
Its value is stored in `centi-assign-key' when PEL starts
centimacro.  The <C-f5> binding is used by default to prevent
overriding the f5 key used by PEL to repeat.

Do NOT use a function key that is already used by PEL.
So, do NOT use f1,f2,f3,f4,f5,f6,f7,f8,f10,f11 or f12.
That leaves f9 and some function keys with qualifiers.
Make sure your selection works in terminal mode if you plan
to use Emacs in terminal mode.
For more information see the  function key usage PDF."
  :group 'pel-pkg-for-kbmacro
  :type  'string)

(defcustom pel-use-elmacro nil
  "Whether PEL use the elmacro package.
Repository: https://github.com/Silex/elmacro"
  :group 'pel-pkg-for-kbmacro
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-use-emacros nil
  "Whether PEL use the emacro package."
  :group 'pel-pkg-for-kbmacro
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-emacros :package-is :in-utils)

;; ---------------------------------------------------------------------------
;; pel-pkg-for-key-chord
;; ---------------------
(defgroup pel-pkg-for-key-chord nil
  "PEL support for key-chords."
  :group 'pel-package-use
  :group 'keyboard
  :link `(url-link :tag "Key-chords PDF" ,(pel-pdf-file-url "key-chords"))
  :link '(url-link :tag "key-chord.el"
                   "https://github.com/emacsorphanage/key-chord/\
blob/master/key-chord.el"))

(defcustom pel-use-key-chord nil
  "Control whether PEL uses the key-chord external package.
With it, it's possible to activate binding actions to two keys
pressed simultaneously or a single key quickly pressed twice.

This can be set to:
- 0: nil: Do not use.  key-chord is not required nor loaded.
- 1: t: Use, activate by command.  key-chord loaded when the function
        `key-chord-mode' is executed.  Not before.
- 2: use-from-start:  Use, load and activate  1 second after Emacs starts."
  :group 'pel-pkg-for-key-chord
  :link '(url-link :tag "key-chord @ MELPA" "https://melpa.org/#/key-chord")
  :type '(choice
          (const :tag "Do not use" nil)
          (const :tag "Use, activate later by command"  t)
          (const :tag "Use, activate when Emacs starts" use-from-start)))

(defcustom pel-use-key-seq nil
  "Control whether PEL key-chord is also using key-seq.
If t, the boolean field `key-seq' key-chords definitions in
`pel-key-chords' is honoured: instead of declaring them a
key-chord PEL declares them key-seq, making the order of the keys
relevant.
To use key-seq you must also activate key-chords via `pel-use-key-chord'."
  :group 'pel-pkg-for-key-chord
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-key-seq :requires 'pel-use-key-chord)

(defcustom pel-key-chord-two-keys-delay 0.1
  "Max time delay between two key press to be considered a key chord.
A good value is 0.05 or 0.1."
  :group 'pel-pkg-for-key-chord
  :type 'float
  :safe #'floatp)

(defcustom pel-key-chord-one-key-delay 0.2
  "Max time delay between 2 press of the same key to be considered a key chord.
This should normally be a little longer than `key-chord-two-keys-delay'.
A value of 0.2 or 0.3 is used to avoid first auto-repeat."
  :group 'pel-pkg-for-key-chord
  :type 'float
  :safe #'floatp)

(defcustom pel-key-chord-in-macros t
  "If nil, don't expand key chords when executing keyboard macros.
If non-nil, expand chord sequences in macros, but only if a similar chord was
entered during the last interactive macro recording.  (This carries a bit of
guesswork.  We can't know for sure when executing whether two keys were
typed quickly or slowly when recorded.)"
  :group 'pel-pkg-for-key-chord
  :type 'boolean
  :safe #'booleanp)

;; (defmacro def-key-chord-lambda (key-chord mode feature &rest body)
;;   "Declare a KEY-CHORD for MODE using specified FEATURE running BODY.
;; With:
;; - key-chord: a string of length 2
;; - mode:      a symbol (unquoted) for the mode.  Use global for creating
;;              a global key-chord.
;; - feature:   a symbol (unquoted) identifying the required feature. nil
;;              if no feature is required.
;; - body:      the form of code that must be evaluated when the key-chord
;;              is typed."
;;   `(,mode
;;     ,key-chord
;;     (lambda ()
;;       (interactive)
;;       (if (require (quote ,feature) nil :noerror)
;;           (progn
;;             ,@body)
;;         (insert ,key-chord)))))

;; Define the wrapper functions instead of lambdas.
;; This way help will identify the function names.

(defun pel-kc-windmove-up ()
  "Execute function `windmove-up' if available."
  (interactive)
  (if (require 'windmove nil :noerror)
      (windmove-up)
    (user-error "Failed loading windmove!")))

(defun pel-kc-windmove-down ()
  "Execute function `windmove-down' if available."
  (interactive)
  (if (require 'windmove nil :noerror)
      (windmove-down)
    (user-error "Failed loading windmove!")))

(defun pel-kc-windmove-left ()
  "Execute function `windmove-left' if available."
  (interactive)
  (if (require 'windmove nil :noerror)
      (windmove-left)
    (user-error "Failed loading windmove!")))

(defun pel-kc-windmove-right ()
  "Execute function `windmove-right' if available."
  (interactive)
  (if (require 'windmove nil :noerror)
      (windmove-right)
    (user-error "Failed loading windmove!")))

(defun pel-kc-indent-rigidly (&optional n)
  "Execute function `pel-indent-rigidly' if available.
Optional argument N specifies lines to indent as in `pel-indent-rigidly'."
  (interactive "*P")
  (if (and (require 'pel-indent nil :noerror)
           (fboundp 'pel-indent-rigidly))
      (pel-indent-rigidly n)
    (user-error "Failed loading pel-indent!")))

(defun pel-kc-correct-word-before-point ()
  "Execute function `flyspell-correct-word-before-point' if available."
  (interactive)
  (if (and (require 'flyspell nil :noerror)
           (fboundp 'flyspell-correct-word-before-point))
      (flyspell-correct-word-before-point)
    (user-error "Failed loading flyspell!")))

(defun pel-kc-open-at-point (&optional n)
  "Execute function `pel-open-at-point' if available.
See `pel-open-at-point' for description of argument N."
  (interactive "P")
  (if (and (require 'pel-open nil :noerror)
           (fboundp 'pel-open-at-point))
      (pel-open-at-point n)
    (user-error "Failed loading pel-open!")))

(defun pel-kc-browse-filename-at-point ()
  "Execute function `pel-browse-filename-at-point' if available."
  (interactive)
  (if (and (require 'pel-open nil :noerror)
           (fboundp 'pel-browse-filename-at-point))
      (pel-browse-filename-at-point)
    (user-error "Failed loading pel-open!")))

(defun pel-kc-browse-url-at-point ()
  "Execute function `browse-url-at-point' if available."
  (interactive)
  (if (and (require 'browse-url nil :noerror)
           (fboundp 'browse-url-at-point))
      (browse-url-at-point)
    (user-error "Failed loading browse-url!")))

(defun pel-kc-search-word-from-top (&optional n)
  "Execute function `pel-search-word-from-top' if available.
See `pel-search-word-from-top' for description of argument N."
  (interactive "P")
  (if (and (require 'pel-search nil :noerror)
           (fboundp 'pel-search-word-from-top))
      (pel-search-word-from-top n)
    (user-error "Failed loading pel-search!")))

(defcustom pel-key-chords
  '((global        ""         key-chord   "<>"      "<>\C-b")
    (global        ""         key-chord   "[]"      "[]\C-b")
    (c-mode        "cc-mode"  key-chord   "{}"      "{\n\n}\C-p\C-p")
    (c++-mode      "cc-mode"  key-chord   "{}"      "{\n\n}\C-p\C-p")
    (global        ""         key-chord   "yu"      pel-kc-windmove-up)
    (global        ""         key-chord   "bn"      pel-kc-windmove-down)
    ; prevent 'fg' to trigger windmove as it appears often in words.
    (global        ""         key-seq     "gf"      pel-kc-windmove-left)
    (global        ""         key-chord   "jk"      pel-kc-windmove-right)
    (global        ""         key-chord   "	q"  pel-kc-indent-rigidly)
    ;; activate the "4r" key-chord in modes where flyspell-mode
    ;; or flyspell-prog-mode is active.
    (flyspell-mode "flyspell" key-chord   "4r"
                   pel-kc-correct-word-before-point)
    (flyspell-prog-mode "flyspell" key-chord  "4r"
                        pel-kc-correct-word-before-point)
    (global    ""            key-chord   "6y" pel-kc-open-at-point)
    (global    ""            key-chord   "6u" pel-kc-browse-filename-at-point)
    (global    ""            key-chord   "7u" pel-kc-browse-url-at-point)
    (global    ""            key-chord   ".;" pel-kc-search-word-from-top))
  "List of key-chords activated when the key-chord-mode is turned on.
PEL provides a set of defaults.  You can replace, delete or add new
key-chord definitions to this default.

You can define *global* key-chords and mode-specific key-chords.
- Global key-chords are stored in the global key-map.
- Mode-specific key-chords are stored in the mode key-map of
  the specified mode when the mode is entered.

The `pel-key-chords' value is a list of objects.
- Each object is a list of 5 items.
  - The first item is either:
    - global   : this key-chord is global
    - A mode name that identifies the major or minor mode
      where the key-chord must be activated.
      For example:  c++-mode or flyspell-mode
  - The second is a string identifying the Emacs Lisp file
    that provides the mode identified in the first item.
    This is empty when the first item is set to global.
    It is required when the first item identifies a mode.
    Identify a name that (load FILE) will be able to load, ie.
    the name of a Emacs Lisp file accessible in Emacs load path.
    Do not identify a file extension.  For most files, the file
    base name is sufficient and more portable.
    So, for flyspell-mode use \"flyspell\" in this field.
  - The third item identifies if the key sequence must be treated
    as a normal key-chord or as a key-seq.  The default is
    key-chord but you can also use key-seq to impose an order
    in the way the keys are recognized.  If the value is key-seq
    but the key-seq package is not loaded, key-chord is used
    instead.
  - The fourth item is the 2 characters used for the key-chord
    or key-seq. Do not quote the characters.
    For key-chord you can identify control characters in this
    way:
    - for <tab>, type: C-q C-i
    - for RET,   type: C-q C-m
    For key-seq you can only use printable ASCII characters
    in the decimal range 32 to 126 inclusive (ie, you cannot
    identify control characters for key-seq).
    - It\\='s not possible to identify the function keys or
      the cursor keys in a portable way here.
  - The fifth item describes the action for the key-chord
    or key-seq.  The action can be expressed using one of 3 ways,
    selected by its Value Menu:
    - 0: expansion string:
         Type the keys you want as replacement. You can
         place several keys on a line, or spread them on several
         lines.  You can identify control keys by entering the
         kbd-style like C-b (by typing \\='C\\=', \\='-\\=', then \\='b\\=')
         or by placing the control code by typing C-q C-b.
         Unfortunately it is currently not possible to identify
         a keystroke involving other modifiers or combination of
         modifiers; the PEL code is not able to properly recognize it
         to pass it to the key-chord function.
         Use the lambda form instead.
         BTW, if you know how to fix that please don\\='t hesitate
         to either let me know or submit a pull-request.
    - 1: command name:
         Type the name of the Emacs interactive function you want
         to execute when the key-chord is hit.
         IMPORTANT: use only functions that you know are *always*
         bound.  If you try to edit the customization while one
         of the symbol used is not bound, the customize UI will
         fail, declare a mismatch and will not allow editing the
         `pel-key-chords' user option.
         If you need to use a function that may no be bound
         then use a lambda form and write code that requires the
         feature providing the function.
         See the default for \"4r\" in the default above as an example.
    - 2: lambda form:
         This is the most flexible way.  Here you write any elisp
         code you need inside a lambda expression that take no
         argument.   You can call any elisp function in here.
         With lambda you can also allow arguments to be passed to the
         key-chord invoked code.  This way you can use numeric arguments
         and you can also prevent the key-chord to modify a read-only buffer.
         See the default as examples."
  :group 'pel-pkg-for-key-chord
  :type
  '(repeat
    (list
     (symbol   :tag "mode  " :value global)
     (string   :tag "file  " :value "")
     (choice   (const :tag "key-chord" key-chord)
               (const :tag "key-seq"   key-seq))
     (string   :tag "2-keys")
     (choice (string   :tag "expansion string")
             (symbol   :tag "command name    ")
             (function :tag "lambda form     "
                       :value (lambda () (interactive) <YOUR CODE HERE>))))))

;; ---------------------------------------------------------------------------
;; Log Files & Systems
;; -------------------
(defgroup pel-pkg-for-logging nil
  "List of external packages that PEL can use to deal with logging."
  :group 'pel-package-use)

(defcustom pel-use-log-support nil
  "Control whether PEL supports any of the log support packages."
  :group 'pel-pkg-for-logging
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-log-support :package-is :a-gate)

(defcustom pel-use-logview nil
  "Control whether PEL uses the logview external package.
To activate it you must also activate `pel-use-log-support'"
  :link '(url-link :tag "logview @ GitHub"
                   "https://github.com/doublep/logview")
  :group 'pel-pkg-for-logging
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-logview :requires 'pel-use-log-support)

(defcustom pel-use-log4j-mode nil
  "Control whether PEL uses the log4j-mode external package.
To activate it you must also activate `pel-use-log-support'"
  :link '(url-link :tag "log4j-mode @ MELPA"
                   "https://melpa.org/#/log4j-mode")
  :group 'pel-pkg-for-logging
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-log4j-mode :requires 'pel-use-log-support)

(defcustom pel-use-rails-log-mode nil
  "Control whether PEL uses the rails-mode external package.
To activate it you must also activate `pel-use-log-support'"
  :link '(url-link :tag "rails-log-mode @ GitHub"
                   "https://github.com/ananthakumaran/rails-log-mode")
  :group 'pel-pkg-for-logging
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-rails-log-mode :requires 'pel-use-log-support)

(defcustom pel-use-syslog-mode nil
  "Control whether PEL uses the syslog-mode external package.
To activate it you must also activate `pel-use-log-support'"
  :link '(url-link :tag "syslog-mode @ GitHub"
                   "https://github.com/vapniks/syslog-mode")
  :group 'pel-pkg-for-logging
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-syslog-mode :requires 'pel-use-log-support)

(defcustom pel-use-vlf nil
  "Control whether PEL uses the vlf external package.
This package helps Emacs deal with very large files.
To activate it you must also activate `pel-use-log-support'"
  :link '(url-link :tag "vlf @ GitHub"
                   "https://github.com/m00natic/vlfi")
  :group 'pel-pkg-for-logging
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-vlf :requires 'pel-use-log-support)

;; ---------------------------------------------------------------------------
;; Keys & Prompts
;; --------------
(defgroup pel-pkg-for-keys nil
  "List of external packages that PEL can use to help with keys and prompts."
  :group 'pel-package-use
  :link `(url-link :tag "AppleScript PDF" ,(pel-pdf-file-url "pl-applescript"))
  :link `(url-link :tag "Function Keys PDF" ,(pel-pdf-file-url "keys-fn"))
  :link `(url-link :tag "Hide/Show PDF" ,(pel-pdf-file-url "hide-show-code"))
  :link `(url-link :tag "Windows PDF" ,(pel-pdf-file-url "windows")))

(defcustom pel-force-normal-erase-is-backspace-off-in-terminal nil
  "When non-nil force normal-erase-is-backspace to off in terminal mode.
Use this when you use Emacs in terminal mode and in graphics mode, use
only one customization file and your delete key deletes forward instead
of backward in terminal mode.  This is sometimes required to run Emacs on
macOS Terminal.

When this is set nil, PEL does not modify the value selected by the
`normal-erase-is-backspace' user-option.
If it is set to a integer, PEL forces `normal-erase-is-backspace' off in
terminal mode by executing the necessary command some time after the complete
initialization.  The amount of time is specified by the integer value, in
seconds.  If your Emacs initialization is relatively quick a value of 1 second
is enough. Increase the value if your Emacs initialization exceeds 1 second
and the setting does not seem to have any impact on the behaviour of the
delete key."
  :group 'pel-pkg-for-keys
  :type '(choice
          (const :tag "Do not force" nil)
          (integer :tag "Force it on startup after specified # of seconds"
                   :value 1)))

(defcustom pel-use-hydra nil
  "Control whether PEL uses the hydra package."
  :group 'pel-pkg-for-keys
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-hydra :also-required-when '(or pel-use-indent-tools
                                                 pel-use-iflipb
                                                 pel-use-treemacs
                                                 pel-activate-hydra-for-greek
                                                 pel-use-ivy-hydra
                                                 pel-use-verilog-ext))

(defcustom pel-use-which-key t
  "Control whether PEL uses the which-key package."
  :group 'pel-pkg-for-keys
  :group 'pel-pkg-for-help              ; make it easy to access
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-use-helm-descbinds nil
  "Control whether PEL uses the helm-descbinds package.

When selecting \\='dont-bind-to-F1-b, activate
help-descbind-mode to invoke it via C-h b or <f1> b"
  :link '(url-link :tag "helm-descbinds @ GitHub"
                   "https://github.com/emacs-helm/helm-descbinds")
  :group 'pel-pkg-for-keys
  :group 'pel-pkg-for-help              ; make it easy to access
  :type '(choice
          (const :tag "Do not use." nil)
          (const :tag "Use and bind to C-h b and <f1> b" bind-to-F1-b)
          (const :tag "Use without specific key binding" dont-bind-to-F1-b)))

(defcustom pel-use-free-keys nil
  "Control whether PEL uses the {free-keys} package."
  :group 'pel-pkg-for-keys
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-use-bind-key nil
  "Control whether PEL uses the {bind-key} package.
*NOTE*: {bind-key} is used (but the PEL key-bindings for it are not set),
 regardless of this setting if any of the following variables is set non-nil:
 - `set-use-bm`."
  :group 'pel-pkg-for-keys
  :group 'pel-pkg-for-help              ; make it easy to access
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-use-keycast nil
  "Control whether the keycast package is made available."
  :group 'pel-pkg-for-keys
  :type 'boolean
  :safe #'booleanp
  :link '(url-link :tag "keycast @ GitHub"
                   "https://github.com/tarsius/keycast"))

(defcustom pel-use-command-log-mode nil
  "Control whether PEL uses the command-log package.

For the moment this uses a fork of the lewang/command-log-mode
waiting for activity to resume in the lewang's repo."
  :group 'pel-pkg-for-keys
  :type 'boolean
  :safe #'booleanp
  :link '(url-link :tag "pierre-rouleau/command-log-mode @ GitHub"
                   "https://github.com/pierre-rouleau/command-log-mode"))
(pel-put 'pel-use-command-log-mode :package-is :in-utils)

(defcustom pel-use-interaction-log-mode nil
  "Control whether PEL uses the interaction-log-mode package.
This package  logs all keys typed in a buffer, showing the command name and
using colored fonts.  Similar to command-log-mode but with more colors.

Note: the author is planning to move the package to GNU elpa;
the previous Github site was removed but MELPA-based installation
still works."
  :group 'pel-pkg-for-keys
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-interaction-log-mode
         :package-is '(quote
                       ((elpa . interaction-log))))

(defcustom pel-select-key-is-end nil
  "When set the <select> key is bound as the <end> key.

This is useful in systems where Emacs runs under a GNU Screen
session which modifies the behaviour of the <end> key to register as <select>.
For those system, set `pel-select-key-is-end' to t and PEL will bind the
<select> key to the `pel-end' command to circumvent the problem."
  :group 'pel-pkg-for-keys
  :type 'boolean
  :safe #'booleanp)

;; ---------------------------------------------------------------------------
(defgroup pel-keypad-keys nil
  "Keypad key control."
  :group 'pel-base-emacs
  :group 'pel-pkg-for-keys)

(defcustom pel-keypad-0-is-kp-yank nil
  "Whether PEL enforce using keypad 0 as yank when not in numlock mode.

This setting is normally not required. It is, however, required in
some situations like when running Emacs on a Linux system accessed through
ssh."
  :group 'pel-keypad-keys
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-keypad-esc-0-is-overwrite-mode nil
  "Whether PEL enforce ESC keypad 0 as overwrite-mode if not in numlock mode.

This setting is normally not required. It is, however, required in
some situations like when running Emacs on a Linux system accessed through
ssh."
  :group 'pel-keypad-keys
  :group 'pel-pkg-for-cut-and-paste
  :group 'pel-pkg-for-text-mode
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-keypad-+-is-kp-add nil
  "Whether Keypad + is seen as <kp-add> or not.

Set it t on system where the key is seen as <kp-add>.
This is not the case on macOS but is the case on some Linux
distributions.  It allows using the keypad + key to execute the
`pel-copy-marked-or-whole-line' command."
  :group 'pel-keypad-keys
  :group 'pel-pkg-for-cut-and-paste
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-keypad-meta+-special-sequence (when pel-system-is-linux-p
                                               "M-O 3 k")
  "Key sequence for system where Meta Keypad + is not seen as <M-kp-add>.

A string that is passed to the `kbd' function to generate the key sequence, or
nil when this is not needed (like on macOS).

Special key mapping assigned to the `pel-copy-symbol-at-point' command
for Emacs running in terminal mode on systems that do not recognize the
`Meta Keypad+' key sequence as <M-kp-add>.

On macOS, the Meta Keypad + is seen as <M-kp-add>.
This is not the case for Emacs running in terminal mode in several
environments, such as several Linux distributions.

PEL tries to set the value of this user-option to the key binding
value required for the key sequence in this user-option when not
running on macOS.  If this value does not work for Emacs in
terminal mode for your environment, change the value."
  :group 'pel-keypad-keys
  :type '(choice
          (const :tag "No special key needed: <M-kp-add> is recognized." nil)
          (string :tag "Special key sequence string to use in TTY mode.")))

;; ---------------------------------------------------------------------------
(defgroup pel-pkg-for-unix-cfg-files nil
  "Unix/Linux configuration file supporting modes."
  :group 'pel-package-use)

(defcustom pel-use-crontab-mode nil
  "Whether PEL activates the crontab-mode to edit crontab files."
  :group 'pel-pkg-for-unix-cfg-files
  :link '(url-link :tag "crontab-mode @ GitHub"
                   "https://github.com/emacs-pe/crontab-mode")
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-crontab-mode :package-is :in-utils)

;; ---------------------------------------------------------------------------
(defgroup pel-pkg-for-log-file nil
  "Log File control options."
  :group 'pel-package-use)

(defcustom pel-use-strace-mode nil
  "Whether PEL uses the strace-mode package."
  :group 'pel-pkg-for-log-file
  :link '(url-link :tag "strace-mode @ Github"
                   "https://github.com/pkmoore/strace-mode")
  :link '(url-link :tag "my strace-mode fork @ Github"
                   "https://github.com/pierre-rouleau/strace-mode")
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-strace-mode :package-is :in-utils)

(defgroup pel-pkg-for-gnu-screen nil
  "GNU Screen log file support."
  :group 'pel-pkg-for-log-file)

(defcustom pel-screen-log-delete-all-consecutive-blank-lines nil
  "Remove all consecutive blank lines from GNU Screen log files.

Activate this to remove the extra empty line applied to each line of the log
file created by GNU Screen."
  :group 'pel-pkg-for-gnu-screen
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-screen-log-fix-regexp-pairs nil
  "Pairs of regexps to fix content of GNU Screen log file.

Used by the `pel-screenlog-fix' function to replace all instances of the
first pair element by the second.
Use this when log files created by GNU Screen leaves artifacts not properly
handled by the ANSI Escape sequences rendering performed by
`pel-screenlog-fix'.
The string replacement supports the same regexp expressions
supported by `replace-regexp' including the expressions in its
TO-STRING argument:
  In TO-STRING, ‘\\&’ stands for whatever matched the whole of REGEXP,
  and ‘\\N’ (where N is a digit) stands for whatever matched
  the Nth ‘\\(...\\)’ (1-based) in REGEXP.
  ‘\\?’ lets you edit the replacement text in the minibuffer
  at the given position for each replacement."
  :group 'pel-pkg-for-gnu-screen
  :link '(url-link :tag "GNU Screen @ Wikipedia"
                   "https://en.wikipedia.org/wiki/GNU_Screen")
  :link '(url-link :tag "GNU Screen Log Command in its HTML Manual"
                   "https://www.gnu.org/software/screen/manual/screen.html#Log")
  :type '(repeat
          (list
           (regexp :tag "From regexp")
           (string :tag "Replacement"))))


;; ---------------------------------------------------------------------------
(defgroup pel-pkg-for-changelog-file nil
  "Packages that support object file formats."
  :group 'pel-pkg-for-log-file)

(defcustom pel-use-changelog-mode nil
  "Whether PEL adds special support for the change-log-mode."
  :group 'pel-pkg-for-changelog-file
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-change-log-activates-minor-modes nil
  "List of *local* minor-modes automatically activated for change-log buffers.
Enter *local* minor-mode activating function symbols.
Do not enter lambda expressions."
  :group 'pel-pkg-for-changelog-file
  :type '(repeat function))

(defcustom pel-change-log-tab-width 8
  "Column width display rendering of hard tab for asciidoc buffers.

PEL stores this in `tab-width' when opening change-log buffers.

This does *NOT* control the indentation in change-log
files, it only controls the column width display rendering of hard tabs
in buffers and tab stop positions for commands such as `tab-to-tab-stop'."
  :group 'pel-pkg-for-changelog-file
  :type 'integer
  :safe 'pel-indent-valid-p)

(defcustom pel-change-log-use-tabs t
  "Value of `indent-tabs-mode' for editing change-log files.
- If set to nil: only spaces are used for indentation.
- If set to t: hard tabs are used when possible."
  :group 'pel-pkg-for-changelog-file
  :type 'boolean
  :safe #'booleanp)

;; ---------------------------------------------------------------------------
;; Object File Format Support
;; --------------------------
(defgroup pel-pkg-for-object-file nil
  "Packages that support object file formats."
  :group 'pel-pkg-for-filemng)

(defcustom pel-use-intel-hex-mode nil
  "Control whether PEL uses the external intel-hex-mode package.

This package allows easy editing of Intel HEX object files."
  :group 'pel-pkg-for-object-file
  :link '(url-link :tag "intel-hex-mode @ Github"
                   "https://github.com/mschuldt/intel-hex-mode")
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-intel-hex-activates-minor-modes nil
  "List of *local* minor-modes automatically activated for intel-hex buffers.
Enter *local* minor-mode activating function symbols.
Do not enter lambda expressions."
  :group 'pel-pkg-for-object-file
  :type '(repeat function))


(defcustom pel-use-elf-mode nil
  "Control whether ELF files are opened in elf-mode buffer showing symbols."
  :link '(url-link :tag "ELF files @ Wikipedia"
                   "https://en.wikipedia.org/wiki/Executable_and_Linkable_Format")
  :link '(url-link :tag "elf-mode @ MELPA"
                   "https://melpa.org/#/elf-mode")
  :group 'pel-pkg-for-object-file
  :type 'boolean
  :safe #'booleanp)

;; ---------------------------------------------------------------------------
;; SSH Support
;; -----------

(defgroup pel-pkg-for-ssh nil
  "Packages that support editing SSH configuration files."
  :group 'pel-pkg-for-filemng)

(defcustom pel-use-emacs-ssh-file-modes nil
  "Control whether PEL uses the emacs-ssh-file-modes.

It provides support for the authorized_keys and know_hosts files."
  :group 'pel-pkg-for-ssh
  :link '(url-link :tag "emacs-ssh-file-modes @ Github"
                   "https://github.com/petere/emacs-ssh-file-modes")
  :link '(url-link :tag "my fork of emacs-ssh-file-modes @ Github"
                   "https://github.com/pierre-rouleau/emacs-ssh-file-modes")
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-emacs-ssh-file-modes :package-is '(quote ((utils . ssh-file-modes))))

(defcustom pel-ssh-authorized-keys-activates-minor-modes nil
  "List of *local* minor-modes automatically activated for authorized_keys buffers.
Enter *local* minor-mode activating function symbols.
Do not enter lambda expressions."
  :group 'pel-pkg-for-ssh
  :type '(repeat function))

(defcustom pel-ssh-known-hosts-activates-minor-modes nil
  "List of *local* minor-modes automatically activated for known_host buffers.
Enter *local* minor-mode activating function symbols.
Do not enter lambda expressions."
  :group 'pel-pkg-for-ssh
  :type '(repeat function))

;; ---------------------------------------------------------------------------
;; X.509 File format Support
;; -------------------------
(defgroup pel-pkg-for-x509-file nil
  "Packages that support X.509 file viewing."
  :group 'pel-pkg-for-filemng)

(defcustom pel-use-x509-mode nil
  "Control whether PEL uses the x509-mode package."
  :group 'pel-pkg-for-x509-file
  :link '(url-link :tag "509-mode @ Github"
                   "https://github.com/jobbflykt/x509-mode")
  :type 'boolean
  :safe #'booleanp)

;; ---------------------------------------------------------------------------
;; SELinux Policy Definition Files Support
;; ---------------------------------------
(defgroup pel-pkg_for-selinux nil
  "Packages supporting SELinux policy development."
  :group 'pel-pkg-for-filemng)

(defcustom pel-use-selinux-policy nil
  "Control whether PEL uses the selinux-policy package."
  :group 'pel-pkg_for-selinux
  :link '(url-link :tag "selinux-policy @ Github"
                   "https://github.com/pierre-rouleau/selinux-policy")
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-selinux-policy :package-is :in-utils)


(defcustom pel-selinuxpolicy-activates-minor-modes nil
  "List of *local* minor-modes automatically activated for selinuxpolicy buffers.
Enter *local* minor-mode activating function symbols.
Do not enter lambda expressions."
  :group 'pel-pkg-for-selinux
  :type '(repeat function))

(defcustom pel-selinuxpolicy-tab-width 2
  "Column width display rendering of hard tab for asciidoc buffers.

PEL stores this in `tab-width' when opening selinuxpolicy buffers.

This does *NOT* control the indentation in selinuxpolicy
files, it only controls the column width display rendering of hard tabs
in buffers and tab stop positions for commands such as `tab-to-tab-stop'."
  :group 'pel-pkg-for-selinux
  :type 'integer
  :safe 'pel-indent-valid-p)

(defcustom pel-selinuxpolicy-use-tabs nil
  "Value of `indent-tabs-mode' for editing selinuxpolicy files.
- If set to nil: only spaces are used for indentation.
- If set to t: hard tabs are used when possible."
  :group 'pel-pkg-for-selinux
  :type 'boolean
  :safe #'booleanp)

;; ---------------------------------------------------------------------------
;; Marking Management
;; ------------------
(defgroup pel-pkg-for-marking nil
  "List of external packages that PEL can use to help deal with regions."
  :group 'pel-package-use
  :link `(url-link :tag "Marking PDF" ,(pel-pdf-file-url "marking"))
  :link `(url-link :tag "Emacs Lisp PDF" ,(pel-pdf-file-url "pl-emacs-lisp"))
  :link `(url-link :tag "Common Lisp PDF"
                   ,(pel-pdf-file-url "pl-common-lisp")))

(defcustom pel-use-expand-region nil
  "Control whether PEL uses the expand-region package."
  :group 'pel-pkg-for-marking
  :link '(url-link :tag "expand-region @ GitHub"
                   "https://github.com/magnars/expand-region.el")
  :type 'boolean
  :safe #'booleanp)

;; ---------------------------------------------------------------------------
;; Markup Language Support
;; -----------------------
(defgroup pel-pkg-for-markup nil
  "Markup Language editing packages PEL can use."
  :group 'pel-package-use)

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; AsciiDoc Support
;; ----------------
(defgroup pel-pkg-for-asciidoc nil
  "PEL AsciiDoc support."
  :group 'pel-pkg-for-markup
  :link `(url-link :tag "AsciiDoc PDF" ,(pel-pdf-file-url "asciidoc")))

(defcustom pel-use-asciidoc nil
  "Control whether PEL activates support for Asciidoc with adoc mode."
  :link '(url-link :tag "adoc-mode @GitHub"
                   "https://github.com/sensorflo/adoc-mode")
  :group 'pel-pkg-for-asciidoc
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-asciidoc :package-is 'adoc-mode)

(defcustom pel-adoc-activates-minor-modes nil
  "List of *local* minor-modes automatically activated for AsciiDoc buffers.
Enter *local* minor-mode activating function symbols.
Do not enter lambda expressions."
  :group 'pel-pkg-for-asciidoc
  :type '(repeat function))

(defcustom pel-adoc-tab-width 2
  "Column width display rendering of hard tab for asciidoc buffers.

PEL stores this in `tab-width' when opening asciidoc  buffers.

This does *NOT* control the indentation in asciidoc
files, it only controls the column width display rendering of hard tabs
in buffers and tab stop positions for commands such as `tab-to-tab-stop'."
  :group 'pel-pkg-for-asciidoc
  :type 'integer
  :safe 'pel-indent-valid-p)

(defcustom pel-adoc-use-tabs nil
  "Value of `indent-tabs-mode' for editing asciidoc files.
- If set to nil: only spaces are used for indentation.
- If set to t: hard tabs are used when possible."
  :group 'pel-pkg-for-asciidoc
  :type 'boolean
  :safe #'booleanp)

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; pel-pkg-for-draw-markup
;; -----------------------
(defgroup pel-pkg-for-drawing-markup nil
  "PEL drawing markup support."
  :group 'pel-pkg-for-markup
  :link `(url-link :tag "Drawing PDF" ,(pel-pdf-file-url "drawing")))

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; GraphViz-Dot Support
;; --------------------
(defgroup pel-pkg-for-graphviz-dot nil
  "PEL Graphviz-DOT support."
  :group 'pel-pkg-for-drawing-markup
  :link `(url-link :tag "Graphviz Dot PDF" ,(pel-pdf-file-url "graphviz-dot")))

(defcustom pel-use-graphviz-dot nil
  "Control whether PEL uses the Graphviz Dot tool and its associated package.
It supports the Graphviz Dot file format and the ability to create graphics
images from their Graphviz Dot files."
  :link '(url-link :tag "Graphviz home page"
                   "https://www.graphviz.org")
  :link '(url-link :tag "DOT Language"
                   "https://www.graphviz.org/doc/info/lang.html")
  :link '(url-link :tag "graphviz-dot-mode @ MELPA"
                   "https://melpa.org/#/graphviz-dot-mode")
  :group 'pel-pkg-for-graphviz-dot
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-graphviz-dot :package-is 'graphviz-dot-mode)

(defcustom pel-graphviz-dot-activates-minor-modes nil
  "List of *local* minor-modes automatically activated for Graphviz Dot buffers.
Enter *local* minor-mode activating function symbols.
Do not enter lambda expressions."
  :group 'pel-pkg-for-graphviz-dot
  :type '(repeat function))

(defcustom pel-graphviz-dot-tab-width 2
  "Column width display rendering of hard tab for graphviz-dot buffers.

PEL stores this in `tab-width' when opening graphviz-dot  buffers.

This does *NOT* control the indentation in graphviz-dot
files, it only controls the column width display rendering of hard tabs
in buffers and tab stop positions for commands such as `tab-to-tab-stop'."
  :group 'pel-pkg-for-graphviz-dot
  :type 'integer
  :safe 'pel-indent-valid-p)

(defcustom pel-graphviz-dot-use-tabs nil
  "Value of `indent-tabs-mode' for editing graphviz-dot files.
- If set to nil: only spaces are used for indentation.
- If set to t: hard tabs are used when possible."
  :group 'pel-pkg-for-graphviz-dot
  :type 'boolean
  :safe #'booleanp)

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; MscGen Support
;; --------------

(defgroup pel-pkg-for-mscgen nil
  "PEL MscGen Support."
  :group 'pel-pkg-for-drawing-markup
  :link `(url-link :tag "MscGen PDF" ,(pel-pdf-file-url "mscgen"))
  :link '(url-link :tag "MscGen @ Wikipedia"
                   "https://en.wikipedia.org/wiki/MscGen")
  :link '(url-link :tag "mscgen-mode @ Github"
                   "https://github.com/thomsten/mscgen-mode#readme"))

(defcustom pel-use-mscgen-mode nil
  "Control whether PEL activates support for mscgen-mode."
  :group 'pel-pkg-for-mscgen
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-mscgen-mode :package-is :in-utils)

(defcustom pel-mscgen-activates-minor-modes nil
  "List of *local* minor-modes automatically activated for Mscgen buffers.
Enter *local* minor-mode activating function symbols.
Do not enter lambda expressions."
  :group 'pel-pkg-for-mscgen
  :type '(repeat function))

(defcustom pel-mscgen-tab-width 2
  "Column width display rendering of hard tab for mscgen buffers.

PEL stores this in `tab-width' when opening mscgen  buffers.

This does *NOT* control the indentation in mscgen
files, it only controls the column width display rendering of hard tabs
in buffers and tab stop positions for commands such as `tab-to-tab-stop'."
  :group 'pel-pkg-for-mscgen
  :type 'integer
  :safe 'pel-indent-valid-p)

(defcustom pel-mscgen-use-tabs nil
  "Value of `indent-tabs-mode' for editing mscgen files.
- If set to nil: only spaces are used for indentation.
- If set to t: hard tabs are used when possible."
  :group 'pel-pkg-for-mscgen
  :type 'boolean
  :safe #'booleanp)

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; PlantUML Support
;; ----------------

(defgroup pel-pkg-for-plantuml nil
  "PEL UML support."
  :group 'pel-pkg-for-drawing-markup
  :link `(url-link :tag "PlantUML PDF" ,(pel-pdf-file-url "plantuml"))
  :link '(url-link :tag "PlantUML @ GitHub"
                   "https://github.com/skuro/plantuml-mode.")
  :link '(url-link :tag "PlantUML home page" "https://plantuml.com")
  :link '(url-link :tag "PlantUML @ wikipedia"
                   "https://en.wikipedia.org/wiki/PlantUML"))

(defcustom pel-use-plantuml nil
  "Control whether PEL activates support for PlantUML to draw UML diagrams.
This uses the `plantuml-mode' package.

The `plantuml-mode' can be used locally, using a local PlantUML
Java application (plantuml.jar).  You can also use PlantUML web
server and if you do not mind sending your information across the
internet.

To use PlantUML locally you must have Java installed on your
system and have the PlantUML Java application installed and
its plantuml.jar file must be accessible.

Note that this value overrides the value selected by the
`plantuml-default-exec-mode' user option."
  :group 'pel-pkg-for-plantuml
  :type '(choice
          (const :tag "Not used" nil)
          (const :tag "Use local plantuml.jar application" t)
          (const :tag "Use the remote PlantUML server" server)))
(pel-put 'pel-use-plantuml :package-is 'plantuml-mode)

(defcustom pel-plantuml-activates-minor-modes nil
  "List of *local* minor-modes automatically activated for PlantUML buffers.
Enter *local* minor-mode activating function symbols.
Do not enter lambda expressions."
  :group 'pel-pkg-for-plantuml
  :type '(repeat function))

(defcustom pel-use-flycheck-plantuml nil
  "Control the flycheck-plantuml PlantUML checker package is used with PEL."
  :link '(url-link :tag "flycheck-plantuml @ GitHub"
                   "https://github.com/alexmurray/flycheck-plantuml")
  :group 'pel-pkg-for-plantuml
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-flycheck-plantuml :requires 'pel-use-plantuml)

(defcustom pel-plantuml-tab-width 2
  "Column width display rendering of hard tab for plantuml buffers.

PEL stores this in `tab-width' when opening plantuml  buffers.

This does *NOT* control the indentation in plantuml
files, it only controls the column width display rendering of hard tabs
in buffers and tab stop positions for commands such as `tab-to-tab-stop'."
  :group 'pel-pkg-for-plantuml
  :type 'integer
  :safe 'pel-indent-valid-p)

(defcustom pel-plantuml-use-tabs nil
  "Value of `indent-tabs-mode' for editing plantuml files.
- If set to nil: only spaces are used for indentation.
- If set to t: hard tabs are used when possible."
  :group 'pel-pkg-for-plantuml
  :type 'boolean
  :safe #'booleanp)

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; Markdown support
;; ----------------
(defgroup pel-pkg-for-markdown nil
  "PEL markdown support."
  :group 'pel-pkg-for-markup
  :link `(url-link :tag "markdown PDF" ,(pel-pdf-file-url "mode-markdown")))

(defcustom pel-use-markdown nil
  "Control whether PEL activates markdown markup support."
  :group 'pel-pkg-for-markdown
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-markdown :package-is :a-gate)

(defcustom pel-markdown-activates-minor-modes nil
  "List of *local* minor-modes automatically activated for markdown buffers.
Enter *local* minor-mode activating function symbols.
Do not enter lambda expressions."
  :group 'pel-pkg-for-markdown
  :type '(repeat function))

(defcustom pel-use-markdown-mode nil
  "Control whether PEL activates the markdown-mode external package.
The `pel-use-markdown' user-option must also be turned on to
activate this package."
  :link '(url-link "markdown-mode @ Github"
                   "https://jblevins.org/projects/markdown-mode/")
  :group 'pel-pkg-for-markdown
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-markdown-mode :requires 'pel-use-markdown)
(pel-put 'pel-use-markdown-mode :also-required-when 'pel-use-cargo)
(pel-put 'pel-use-markdown-mode :requires-package '(quote
                                                    ((elpa . edit-indirect))))

(defcustom pel-use-edit-indirect nil
  "Control whether PEL activates the edit-indirect external package.
This package provides the ability to edit code blocks of specified programming
languages located inside markdown file via indirect buffers operating in the
major mode of that programming language."
  :link '(url-link "edit-indirect @ GitHub"
                   "https://github.com/Fanael/edit-indirect/")
  :group 'pel-pkg-for-markdown
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-use-grip-mode nil
  "Control whether PEL activates grip-mode external package.
This package provides HTML rendering of markdown and org-mode buffers.
It requires Python and the grip python package that you can install
using the command \\='pip install grip\\='."
  :link '(url-link "grip-mode @ GitHub"
                   "https://github.com/seagle0128/grip-mode")
  :link '(url-link "grip @ GitHub"
                   "https://github.com/joeyespo/grip")
  :group 'pel-pkg-for-markdown
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-use-impatient-showdown nil
  "Control whether PEL activates the impatient-showdown package.
The `pel-use-markdown' user-option must also be turned on to
activate this package.
This package gives you the ability to see the HTML rendering of the markdown
buffer inside your default browser.  The rendering is updated automatically as
the buffer is updated."
  :link '(url-link :tag "impatient-showdown @ GitHub"
                   "https://github.com/jcs-elpa/impatient-showdown")
  :group 'pel-pkg-for-markdown
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-impatient-showdown :requires 'pel-use-markdown)
(pel-put 'pel-use-impatient-showdown :requires-package
         '(quote ((elpa . htmlize)
                  (elpa . simple-httpd))))

(defcustom pel-use-markdown-preview-eww nil
  "Control whether PEL activates the markdown-preview-eww package.
The `pel-use-markdown' user-option must also be turned on to
activate this package.

NOTE: ⚠️  not recommended: markdown-live-preview-mode from
         markdown-mode does the same and does not need the setup
         required by markdown-preview-eww."
  :link '(url-link :tag "markdown-preview-eww @ GitHub"
                   "https://github.com/niku/markdown-preview-eww")
  :group 'pel-pkg-for-markdown
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-markdown-preview-eww :requires 'pel-use-markdown)

(defcustom pel-use-markdown-preview-mode nil
  "Control whether PEL activates the markdown-preview-mode package.
The `pel-use-markdown' user-option must also be turned on to
activate this package."
  :link '(url-link :tag "markdown-preview-mode @ GitHub"
                   "https://github.com/ancane/markdown-preview-mode")
  :group 'pel-pkg-for-markdown
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-markdown-preview-mode :requires 'pel-use-markdown)
(pel-put 'pel-use-markdown-preview-mode
         :requires-package '(quote ((elpa . seq))))

(defcustom pel-use-markdown-toc nil
  "Control whether PEL activates the markdown-toc package.
The `pel-use-markdown' user-option must also be turned on to
activate this package."
  :link '(url-link :tag "markdown-toc/pierre-rouleau @ GitHub"
                   "https://github.com/pierre-rouleau/markdown-toc")
  :link '(url-link :tag "markdown-toc @ GitHub"
                   "https://github.com/ardumont/markdown-toc")
  :group 'pel-pkg-for-markdown
  :type '(choice
          (const :tag "Use markdown-toc" t)
          (const :tag "Use markdown-toc and update TOC on save"
                 update-toc-on-save)))
(pel-put 'pel-use-markdown-toc :requires 'pel-use-markdown)
(pel-put 'pel-use-markdown-toc :package-is :in-utils)

(defcustom pel-use-vmd-mode nil
  "Control whether PEL activates the vmd-mode package.
The `pel-use-markdown' user-option must also be turned on to
activate this package."
  :link '(url-link :tag "vmd-mode @ GitHub"
                   "https://github.com/blak3mill3r/vmd-mode")
  :group 'pel-pkg-for-markdown
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-vmd-mode :requires 'pel-use-markdown)

(defcustom pel-use-remark-mode nil
  "Control whether PEL activates the remark-mode package.
The `pel-use-markdown' user-option must also be turned on to
activate this package."
  :link '(url-link :tag "remark-mode @ GitHub"
                   "https://github.com/torgeir/remark-mode.el")
  :group 'pel-pkg-for-markdown
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-remark-mode :requires 'pel-use-markdown)

(defcustom pel-markdown-tab-width 2
  "Column width display rendering of hard tab for markdown buffers.

PEL stores this in `tab-width' when opening markdown  buffers.

This does *NOT* control the indentation in markdown
files, it only controls the column width display rendering of hard tabs
in buffers and tab stop positions for commands such as `tab-to-tab-stop'."
  :group 'pel-pkg-for-markdown
  :type 'integer
  :safe 'pel-indent-valid-p)

(defcustom pel-markdown-use-tabs nil
  "Value of `indent-tabs-mode' for editing markdown files.
- If set to nil: only spaces are used for indentation.
- If set to t: hard tabs are used when possible."
  :group 'pel-pkg-for-markdown
  :type 'boolean
  :safe #'booleanp)

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; Outline Mode Support
;; --------------------

(defgroup pel-pkg-for-outline nil
  "PEL Outline Mode Support."
  :group 'pel-pkg-for-markup
  :link `(url-link :tag "Outline Mode PDF" ,(pel-pdf-file-url "outline")))

(defcustom pel-outline-activates-minor-modes nil
  "List of *local* minor-modes automatically activated for Outline-Mode buffers.
Enter *local* minor-mode activating function symbols.
Do not enter lambda expressions."
  :group 'pel-pkg-for-outline-mode
  :type '(repeat function))

(defcustom pel-use-outshine nil
  "Control whether PEL provides access to the outshine package.

Outshine improves upon the outline minor mode to provide outlining for
programming language files."
  :link '(url-link :tag "outshine @ GitHub"
                   "https://github.com/alphapapa/outshine")
  :group 'pel-pkg-for-outline
  :type 'boolean
  :safe #'booleanp)

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; Org Mode Support
;; ----------------

(defgroup pel-pkg-for-org-mode  nil
  "PEL Org Mode support."
  :group 'pel-pkg-for-outline
  :link `(url-link :tag "Org Mode PDF" ,(pel-pdf-file-url "mode-org-mode")))

(defcustom pel-use-org-mode nil
  "Control whether PEL supports Org-Mode."
  :group 'pel-pkg-for-org-mode
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-org-mode :package-is :builtin-emacs)

(defcustom pel-org-activates-minor-modes nil
  "List of *local* minor-modes automatically activated for Org-Mode buffers.
Enter *local* minor-mode activating function symbols.
Do not enter lambda expressions."
  :group 'pel-pkg-for-org-mode
  :type '(repeat function))

(defcustom pel-org-use-electric-markup nil
  "Whether org markup keys are electric.

When turned on, the *, /, =, _, ~ and + keys are electric in org-mode.
- Typing one of these keys inserts the other after the cursor.
- Typing a key when an area is marked inserts the keys around the area."
  :group 'pel-pkg-for-org-mode
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-org-tab-width 2
  "Column width display rendering of hard tab for org buffers for Emacs < 30.

For Emacs >= 30.1, org-mode only supports a tab-width of 8,
For Emacs < 30, PEL set `tab-width' with this value when
opening org buffers.

This does *NOT* control the indentation in org files, only for commands
that mode point to tab stop positions such as `tab-to-tab-stop', and the
display of hard TAB characters."
  :group 'pel-pkg-for-org
  :type 'integer
  :safe 'pel-indent-valid-p)

(defcustom pel-org-use-tabs nil
  "Value of `indent-tabs-mode' for editing org files.
- If set to nil: only spaces are used for indentation.
- If set to t: hard tabs are used when possible."
  :group 'pel-pkg-for-org
  :type 'boolean
  :safe #'booleanp)

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; reStructuredText support
;; ------------------------
(defgroup pel-pkg-for-rst nil
  "PEL reStructuredText support."
  :group 'pel-pkg-for-markup
  :link `(url-link :tag "reStructuredText PDF" ,(pel-pdf-file-url "mode-rst")))

(defcustom pel-use-rst-mode nil
  "Control whether PEL supports {rst-mode} (reStructuredText)."
  :group 'pel-pkg-for-rst
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-rst-mode :package-is :builtin-emacs)


(defcustom pel-rst-compiler "pel-rst2html"
  "Command line use to compile reStructuredText file into final format.

Specify a command line that can be used to convert the reStructuredText file
into the format of your choice, like HTML. PEL appends the name of the
file to the command line to compile it.

The command you select can have several arguments.
- The last argument MUST be the name of the reStructuredText file to process.
  - The default pel-rst2html is provided as a front end to rst2hrtml
    as an example.  If you use that the one provided by PEL, inside its bin
    directory is used.  To use another script, change the name or specify
    the full path of the script."
  :group 'pel-pkg-for-rst
  :type 'string)

(defcustom pel-rst-activates-minor-modes nil
  "List of *local* minor-modes automatically activated for rst buffers.

Note: rst := reStructuredText

Enter *local* minor-mode activating function symbols.
Do not enter lambda expressions."
  :group 'pel-pkg-for-rst
  :type '(repeat function))

(defcustom pel-rst-adornment-style 'CRiSPer
  "Select the section adornment style.
Identifies the number of levels supported and their adornment.
- `default' is Emacs `rst-mode' default.  A title and 7 levels.
- `Sphinx-Python' is what Sphinx uses: 6 levels:
  - parts,
  - chapters,
  - sections,
  - subsections,
  - sub-subsections,
  - paragraphs.
- `CRiSPer', a title and 12-level mode previously developed for CRiSP."
  :group 'pel-pkg-for-rst
  :type '(choice (const :tag "default" default)
                 (const :tag "Sphinx-Python" Sphinx-Python)
                 (const :tag "CRiSPer" CRiSPer)))

(defcustom pel-rst-use-single-underscore-for-single-word-ref nil
  "Whether PEL creates references to a single word with a single underscore.

The `pel-rst-makelink' command creates a reference for a single \"word\"
using the syntax \"<word>_\" if it is nil, otherwise it uses \"word_\"
style.

Using the fully enclosing style facilitates Emacs navigation to the referenced
document, something that does not work with a trailing underscore."
  :group 'pel-pkg-for-rst
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-rst-tab-width 2
  "Column width display rendering of hard tab for reStructuredText buffers.

PEL stores this in `tab-width' when opening reStructuredText buffers.

This does *NOT* control the indentation in reStructuredText
files, it only controls the column width display rendering of hard tabs
in buffers and tab stop positions for commands such as `tab-to-tab-stop'."
  :group 'pel-pkg-for-rst
  :type 'integer
  :safe 'pel-indent-valid-p)

(defcustom pel-rst-use-tabs nil
  "Value of `indent-tabs-mode' for editing reStructuredText files.
- If set to nil: only spaces are used for indentation.
- If set to t: hard tabs are used when possible."
  :group 'pel-pkg-for-rst
  :type 'boolean
  :safe #'booleanp)

;;   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -
(defgroup pel-rst-style nil
  "PEL reStructuredText skeleton format control."
  :group 'pel-pkg-for-rst)

;; style - 0 : use-separators : not used for rst templates

;; style - 1
(defcustom pel-rst-skel-insert-file-timestamp t
  "Specifies whether a timestamp is inserted inside file module header block."
  :group 'pel-rst-style
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-rst-skel-insert-file-timestamp :choices '(nil t))

;; style - 2
(defcustom pel-rst-skel-with-license nil
  "Control if copyright & license is inserted in code file module header block.

The available choices are:

- 0: No copyright, no license.                 nil
- 1: Copyright only, no license.               \\='only-copyright
- 2: Copyright, with selected license name.    a string

For choice 3, you specify the name of the license in the string
field.  Something like MIT or GPL-3.0, or anything appropriate.
That string will be entered verbatim inside a file header line.

For reStructuredText, the ability to include the license text
itself is not available."
  :group 'pel-rst-style
  :type
  '(choice
    (const :tag  "No license, no copyright." nil)
    (const :tag  "Copyright only." only-copyright)
    (string :tag "Copyright with specified license name.")))
(pel-put 'pel-rst-skel-with-license :choices '(nil only-copyright "MIT"))

;; style - 3 : package names: not used for rst templates
;; style - 4 : file variable: not used for rst templates
;; style - 5 : doc section titles: not used for rst templates.

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; XML support
;; -----------

(defgroup pel-pkg-for-xml nil
  "PEL XML and derivative support."
  :group 'pel-pkg-for-markup)

(defcustom pel-use-osx-plist nil
  "Control whether PEL activates the osx-plist file to parse macOS plist files."
  :group 'pel-pkg-fox-xml
  :link '(url-link :tag "mac osxPlist @ Emacs Wiki"
                   "https://www.emacswiki.org/emacs/MacOSXPlist")
  :link '(url-link :tag "osx-plist @ GitHub"
                   "https://github.com/gonewest818/osx-plist")
  :type 'boolean
  :safe #'booleanp)

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; YAML support
;; ------------
(defgroup pel-pkg-for-yaml nil
  "PEL YAML support."
  :group 'pel-pkg-for-markup
  :link `(url-link :tag "yaml PDF" ,(pel-pdf-file-url "yaml")))

(defcustom pel-use-yaml-mode nil
  "Control whether PEL provides access to the yaml-mode external package."
  :group 'pel-pkg-for-yaml
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-yaml-activates-minor-modes nil
  "List of *local* minor-modes automatically activated for YAML buffers.
Enter *local* minor-mode activating function symbols.
Do not enter lambda expressions."
  :group 'pel-pkg-for-yaml
  :type '(repeat function))

(defcustom pel-yaml-tab-width 2
  "Column width display rendering of hard tab for yaml buffers.

PEL stores this in `tab-width' when opening yaml  buffers.

This does *NOT* control the indentation in yaml
files, it only controls the column width display rendering of hard tabs
in buffers and tab stop positions for commands such as `tab-to-tab-stop'."
  :group 'pel-pkg-for-yaml
  :type 'integer
  :safe 'pel-indent-valid-p)

(defcustom pel-yaml-use-tabs nil
  "Value of `indent-tabs-mode' for editing yaml files.
- If set to nil: only spaces are used for indentation.
- If set to t: hard tabs are used when possible."
  :group 'pel-pkg-for-yaml
  :type 'boolean
  :safe #'booleanp)

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; Common Workflow Language support
;; --------------------------------

(defgroup pel-pkg-for-cwl nil
  "PEL Common Workflow Language (CWL) support."
  :group 'pel-pkg-for-yaml
  :link `(url-link :tag "cwl PDF" ,(pel-pdf-file-url "cwl")))

(defcustom pel-use-cwl-mode nil
  "Control whether PEL activates the cwl-mode external package."
  :group 'pel-pkg-for-cwl
  :link '(url-link :tag "cwl-mode @ GitHub"
                   "https://github.com/tom-tan/cwl-mode")
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-cwl-activates-minor-modes nil
  "List of *local* minor-modes automatically activated for CWL buffers.
Enter *local* minor-mode activating function symbols.
Do not enter lambda expressions."
  :group 'pel-pkg-for-cwl
  :type '(repeat function))

;; ---------------------------------------------------------------------------
;; pel-pkg-for-modeline
;; --------------------
(defgroup pel-pkg-for-modeline nil
  "List of external packages that PEL can use to control the modeline."
  :group 'pel-package-use
  :group 'pel-pkg-for-highlight)

(defcustom pel-modeline-display-time nil
  "Activate time display on modeline when set to t."
  :group 'pel-pkg-for-modeline
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-use-delight nil
  "Control whether PEL activates the delight external package.
This provides ability to control the lighter of major and minor modes."
  :group 'pel-pkg-for-modeline
  :link '(url-link :tag "delight @ GNU"
                   "https://savannah.nongnu.org/projects/delight")
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-delight-specs
  '((origami-mode "⋱⋯" "origami")
    (abbrev-mode " Abv" abbrev)
    (rainbow-mode)
    (overwrite-mode " Ov" t)
    (emacs-lisp-mode ("ELisp" (lexical-binding "" ":Dyn")) :major))
  "Identify delight specifications to create/modify/disable lighters.

Specify the mode-line lighters modifications you need here.

The data stored in this user-option corresponds to the data type
accepted by the function `delight'. See its documentation for
more details.

Some are provided by default to help show the possibilities of
the supported syntax.

PEL applies them when the `pel-use-delight' is turned on.

NOTE: lighter for electric-pair-mode must be activated separately
      by the `pel-electric-pair-lighter'."
  :group 'pel-pkg-for-modeline
  :link '(url-link :tag "delight @ Emacs Wiki"
                   "https://www.emacswiki.org/emacs/DelightedModes")
  :type '(repeat sexp))

(defcustom pel-electric-pair-lighter " ℇ⒧ "
  "Mode-line lighter for the electric-pair-mode.

To disable it replace it by an empty string."
  :group 'pel-pkg-for-modeline
  :type 'string)

(defcustom pel-use-show-point-mode nil
  "Control whether PEL activates the show-point-mode.

That mode prints the current point value on the mode line."
  :group 'pel-pkg-for-modeline
  :link '(url-link :tag "Show Point Mode @ Emacs Wiki"
                   "https://www.emacswiki.org/emacs/ShowPointMode")
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-show-point-mode :package-is :in-utils)

;; ---------------------------------------------------------------------------
;; pel-pkg-for-navigation
;; ----------------------
(defgroup pel-pkg-for-navigation nil
  "List of external packages that PEL can use to help navigation."
  :group 'pel-package-use
  :link `(url-link :tag "Navigation PDF" ,(pel-pdf-file-url "navigation")))

(defcustom pel-use-ace-link nil
  "Control activation of the ace link package."
  :link '(url-link :tag "ace-link @ GitHub"
                   "https://github.com/abo-abo/ace-link")
  :group 'pel-pkg-for-navigation
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-use-avy nil
  "Control activation of the avy package."
  :link '(url-link :tag "avy @ GitHub" "https://github.com/abo-abo/avy")
  :group 'pel-pkg-for-navigation
  :type 'boolean
  :safe #'booleanp)
;; There's no specific PEL user-option for ivy-avy, but it is installed
;; when both pel-use-avy and pel-use-ivy are set. It is identified in the
;; following property.  It could also be located in the pel-use-ivy with
;; adjusted logic. I selected the one here.
(pel-put 'pel-use-avy :package-is '(when pel-use-ivy
                                     '((elpa . ivy-avy))))

;; ---------------------------------------------------------------------------
(defgroup pel-pkg-for-programming-languages nil
  "PEL customization for programming languages."
  :group 'pel-package-use)

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
(defgroup pel-pkg-for-hardware-description-languages nil
  "PEL customization for hardware description languages."
  :group 'pel-pkg-for-programming-languages)

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; Verilog Language Support
;; ------------------------

(defgroup pel-pkg-for-verilog nil
  "PEL Verilog language support."
  :group 'pel-pkg-for-hardware-description-languages
  :link '(url-link :tag "Verilog @ Wikipedia" "https://en.wikipedia.org/wiki/Verilog"))

(defcustom pel-use-verilog nil
  "Control whether PEL supports the Verilog Programming Language Development.
On Emacs >= 30.1, if `pel-use-tree-sitter' is on, this also activates the
verilog-ts-mode package and automatically install and build the required
Tree-Sitter syntax grammar."
  :group 'pel-pkg-for-verilog
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-verilog :package-is '(if pel-use-tree-sitter
                                       (quote ((elpa . verilog-mode)
                                               (elpa . verilog-ts-mode)))
                                     (quote ((elpa . verilog-mode)))))

(defcustom pel-verilog-activates-minor-modes nil
  "List of *local* minor-modes automatically activated for Verilog buffers.
Enter *local* minor-mode activating function symbols.
Do not enter lambda expressions."
  :group 'pel-pkg-for-verilog
  :type '(repeat function))

(defcustom pel-verilog-tab-width 4
  "Column width display rendering of hard tab for verilog buffers.

PEL stores this in `tab-width' when opening verilog buffers.

This does *NOT* control the indentation in verilog
files, it only controls the column width display rendering of hard tabs
in buffers and tab stop positions for commands such as `tab-to-tab-stop'."
  :group 'pel-pkg-for-verilog
  :type 'integer
  :safe 'pel-indent-valid-p)

(defcustom pel-verilog-use-tabs nil
  "Value of `indent-tabs-mode' for editing verilog files.
- If set to nil: only spaces are used for indentation.
- If set to t: hard tabs are used when possible."
  :group 'pel-pkg-for-verilog
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-use-verilog-ext nil
  "Whether PEL uses the verilog-ext package."
  :link '(url-link :tag "verilog-ext @ Github"
                   "https://github.com/gmlarumbe/verilog-ext")
  :group 'pel-pkg-for-verilog
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-use-veri-kompass nil
  "Whether PEL uses the ver-kompass package."
  :link '(url-link :tag "veri-kompass @ Gitlab"
                   "https://gitlab.com/koral/veri-kompass")
  :link '(url-link :tag "My clone of veri-kompass @ Gitlab"
                   "https://gitlab.com/prouleau/veri-kompass")
  :group 'pel-pkg-for-verilog
  :type 'boolean
  :safe #'booleanp)

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; VHDL Language Support
;; ---------------------

(defgroup pel-pkg-for-vhdl nil
  "PEL VHDL language support."
  :group 'pel-pkg-for-hardware-description-languages
  :link '(url-link :tag "VHDL @ Wikipedia" "https://en.wikipedia.org/wiki/VHDL"))

(defcustom pel-use-vhdl nil
  "Control whether PEL supports the VHDL Programming Language Development."
  :group 'pel-pkg-for-vhdl
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-vhdl :package-is '(if pel-use-tree-sitter
                                       (quote ((elpa . vhdl-mode)
                                               (elpa . vhdl-ts-mode)))
                                     (quote ((elpa . vhdl-mode)))))

(defcustom pel-vhdl-activates-minor-modes nil
  "List of *local* minor-modes automatically activated for VHDL buffers.
Enter *local* minor-mode activating function symbols.
Do not enter lambda expressions."
  :group 'pel-pkg-for-vhdl
  :type '(repeat function))

(defcustom pel-vhdl-tab-width 4
  "Column width display rendering of hard tab for vhdl buffers.

PEL stores this in `tab-width' when opening vhdl buffers.

This does *NOT* control the indentation in vhdl
files, it only controls the column width display rendering of hard tabs
in buffers and tab stop positions for commands such as `tab-to-tab-stop'."
  :group 'pel-pkg-for-vhdl
  :type 'integer
  :safe 'pel-indent-valid-p)

(defcustom pel-vhdl-use-tabs nil
  "Value of `indent-tabs-mode' for editing vhdl files.
- If set to nil: only spaces are used for indentation.
- If set to t: hard tabs are used when possible."
  :group 'pel-pkg-for-vhdl
  :type 'boolean
  :safe #'booleanp)


;; ---------------------------------------------------------------------------
;; Programming Language Support
;; ============================
(defgroup pel-pkg-for-software-programming-languages nil
  "PEL customization for programming languages."
  :group 'pel-pkg-for-programming-languages)

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; Generic Programming Support
;; ---------------------------
(defgroup pel-pkg-for-all-languages nil
  "PEL Generic Programming support."
  :group 'pel-pkg-for-languages
  :link `(url-link :tag "Comments PDF" ,(pel-pdf-file-url "comments"))
  :link `(url-link :tag "Inserting Text PDF"
                   ,(pel-pdf-file-url "inserting-text")))

(defcustom pel-use-eldoc-box nil
  "Control whether PEL supports the eldoc-box package.

The eldoc-box various modes display Eldoc information inside a
child frame over the current window, covering text in the current
buffer.  It is possible to select between 2 modes:

- eldoc-box-hover-mode:
        Display documentation of the symbol at point
        in a childframe on upper corner.

- eldoc-box-hover-at-point-mode:
        Same as eldoc-box-hover-mode except the childframe
        is displayed at point, instead of on the upper corner.

Note: eldoc-box only works in graphics mode, not in terminal (tty)
      mode.  In terminal-mode it is not activated even if this option
      is activated.

If you do not like seeing the eldoc information inside the echo
area at the bottom of the Emacs screen, use this.  Otherwise it's
of minimum value.  It can be useful when the echo area displays
information you are interested in and do not want to have it
replaced by eldoc text (however you can always disable eldoc with
M-x eldoc-mode)."
  :group 'pel-pkg-for-all-languages
  :type 'boolean
  :safe #'booleanp
  :link '(url-link :tag "eldoc-box @ GitHub"
                   "https://github.com/casouri/eldoc-box")
  :link '(custom-group-link "eldoc"))
(pel-put 'pel-use-eldoc-box :restricted-to 'pel-emacs-is-graphic-p)

(defcustom pel-use-hide-comnt nil
  "Control whether PEL activates Drew Adams' hide-cmnt package.
This package provides the ability to hide comments."
  :group 'pel-pkg-for-all-languages
  :group 'pel-pkg-for-hide-show
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-hide-comnt :package-is :in-utils)

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; Syntax Checking
;; ---------------

(defgroup pel-pkg-for-syntax-check nil
  "PEL support for syntax checking."
  :group  'pel-pkg-for-all-languages)

(defcustom pel-use-flycheck nil
  "Whether PEL activates the flycheck external package.
This may get activated indirectly by other user-options."
  :group 'pel-pkg-for-syntax-check
  :link '(url-link :tag "flycheck home"
                   "https://www.flycheck.org/en/latest/")
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-flycheck :also-required-when
         '(or (and pel-use-erlang
                   (or pel-use-flycheck-rebar3
                       (eq pel-use-erlang-syntax-check 'with-flycheck)))
              (and pel-use-go
                   (or pel-use-flycheck-golangci-lint
                       (eq pel-use-goflymake 'with-flycheck)))
              (and pel-use-odin
                   pel-use-flycheck-odin)
              (and pel-use-plantuml
                   pel-use-flycheck-plantuml)
              (and pel-use-rust
                   pel-use-flycheck-rust)
              (and pel-use-sh
                   (or (memq pel-use-shellcheck
                             '(flycheck-manual flycheck-automatic))))))

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; Language Server Protocol (LSP) Support
;; --------------------------------------

(defgroup pel-pkg-for-language-server nil
  "PEL support for language server protocol."
  :group  'pel-pkg-for-all-languages)


(defgroup pel-pkg-for-lsp-mode nil
  "PEL support for LSP Mode, a language server protocol."
  :group  'pel-pkg-for-language-server)

(defcustom pel-use-lsp-mode nil
  "Control whether PEL activates lsp-mode, a language server."
  :group 'pel-pkg-for-lsp-mode
  :link '(url-link :tag "lsp-mode @ GitHub"
                   "https://github.com/emacs-lsp/lsp-mode")
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-lsp-mode :package-is '(quote ((elpa . ccls))))
(pel-put 'pel-use-lsp-mode :also-required-when 'pel-use-emacs-ccls)
(pel-put 'pel-use-lsp-mode :also-required-when 'pel-use-lsp-java)


(defcustom pel-use-lsp-ui nil
  "Control whether PEL activates lsp-ui, a lsp-mode UI support."
  :group 'pel-pkg-for-lsp-mode
  :link '(url-link :tag "lsp-ui @ GitHub"
                   "https://github.com/emacs-lsp/lsp-ui")
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-lsp-ui :also-required-when 'pel-use-emacs-ccls)

(defcustom pel-use-lsp-treemacs nil
  "Control whether PEL activates the lsp extension for treemacs."
  :group 'pel-pkg-for-lsp-mode
  :link '(url-link :tag "lsp-treemacs @ GitHub"
                   "https://github.com/emacs-lsp/lsp-treemacs")
  :type 'boolean
  :safe #'booleanp)

;; TODO: add the following when I add support for Selectrum
;; (defcustom pel-use-consult-lsp nil
;;   "Control whether PEL activates the lsp extension for consult."
;;   :group 'pel-pkg-for-lsp-mode
;;   :group 'pel-pkg-for-completion
;;   :link '(url-link :tag "consult-lsp @ GitHub"
;;                    "https://github.com/gagbo/consult-lsp")
;;   :type 'boolean
;;   :safe #'booleanp)

(defcustom pel-use-lsp-ivy nil
  "Control whether PEL activates the lsp extension for ivy."
  :group 'pel-pkg-for-lsp-mode
  :group 'pel-pkg-for-completion
  :link '(url-link :tag "lsp-ivy @ GitHub"
                   "https://github.com/emacs-lsp/lsp-ivy/")
  :type 'boolean
  :safe #'booleanp)


(defcustom pel-use-helm-lsp nil
  "Control whether PEL activates the lsp extension for helm."
  :group 'pel-pkg-for-lsp-mode
  :group 'pel-pkg-for-completion
  :link '(url-link :tag "helm-lsp @ GitHub"
                   "https://github.com/emacs-lsp/helm-lsp")
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-use-lsp-origami nil
  "Control whether PEL activates the lsp extension for origami."
  :group 'pel-pkg-for-lsp-mode
  :group 'pel-pkg-for-hide-show
  :link '(url-link :tag "lsp-origami @ GitHub"
                   "https://github.com/emacs-lsp/lsp-origami/")
  :type 'boolean
  :safe #'booleanp)

;; TODO:  FUTURE
;; (defcustom pel-use-eglot nil
;;   "Control whether PEL supports the eglot package.
;; eglot is a client for Language Server Protocol servers."
;;   :group 'pel-pkg-for-language-server
;;   :type 'boolean
;;   :safe #'booleanp)

(defcustom pel-use-emacs-ccls nil
  "Control whether PEL supports ccls, language server for C,C++,Objective-C.

This forces the `pel-use-lsp-mode' to t."
  :group 'pel-pkg-for-lsp-mode
  :link '(url-link :tag "emacs-ccls @ GitHub"
                   "https://github.com/emacs-lsp/emacs-ccls")
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-emacs-ccls :also-required-when '(or pel-use-emacs-ccls-for-c
                                                      pel-use-emacs-ccls-for-c++
                                                      pel-use-emacs-ccls-for-objc))

;; -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -
(defgroup pel-pkg-generic-code-style nil
  "PEL Generic code style configuration."
  :group 'pel-pkg-for-all-languages
  :group 'pel-pkg-for-skeletons
  :group 'pel-pkg-for-insertions
  :link `(url-link :tag "Inserting Text PDF"
                   ,(pel-pdf-file-url "inserting-text")))

(defcustom pel-generic-skel-module-header-block-style nil
  "Specifies the style of the generic code file module header block.
You can use one of the following:

- The default, controlled by PEL\\='s code.
- A user-specified one.  For this you have to write Emacs Lisp code.
  You have to write a function `pel-skels-generic-header-module-block/custom'
  inside a file and store the name of that file inside the box that
  appear when you select this option.   The function must accept
  3 arguments:
  - arg 1: string : the file name (without path)
  - arg 2: boolean: is non-nil when the file is a secondary file type
                     such as a header file or a sourced file,,
                    and nil when it is the main file type, such as
                    the code file or the script file.
  - arg 3: a list of 3 strings: (cb cc ce)
            - cb : comment begin string
            - cc : comment continuation string
            - ce : comment end string.

  The function can use or ignore these arguments.
  See the code of function `pel-skels-generic-header-module-block'
  for an example of how these arguments are used to create the standard
  header-module block skeleton.

  You can start by using the example that is stored inside the file
  \\='custom/skeleton/custom-c-skel.el\\='.
  The file name can be an absolute file name but it can also be a relative
  file name.  On Unix systems you can use \\='~\\=' to identify your home directory."
  :group 'pel-pkg-generic-code-style
  :type '(choice
          (const  :tag "Default, controlled by PEL." nil)
          (string :tag "Use your own custom definition\n inside file")))

;; style - 0
(defcustom pel-generic-skel-use-separators t
  "Specifies whether generic code block include separators line.
If nil no separator line comment is used, otherwise separator line
comments of length controlled by variable `fill-column' are inserted."
  :group 'pel-pkg-generic-code-style
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-generic-skel-use-separators :choices '(nil t))

;; style - 1
(defcustom pel-generic-skel-insert-file-timestamp t
  "Specifies whether a timestamp is inserted inside file module header block."
  :group 'pel-pkg-generic-code-style
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-generic-skel-insert-file-timestamp :choices '(nil t))

;; style - 2
(defcustom pel-generic-skel-with-license nil
  "Control if copyright & license is inserted in code file module header block.

The available choices are:

- 0: No copyright, no license.                 nil
- 1: Copyright only, no license.               \\='only-copyright
- 2: Copyright, with selected license name.    a string
- 3: Copyright, with complete license text.    t

For choice 3, you specify the name of the license in the string
field.  Something like MIT or GPL-3.0, or anything appropriate.
That string will be entered verbatim inside a file header line.

For choice 4, the type of the license and its text are selected
the function `lice' taken from the external library with the same name.

The text of the inserted license is selected by the
`lice:default-license' user option, normally configured inside
the directory\\='s .dir-locals.el file written inside the global
setting like this:

   ((nil . ((fill-column . 80)
            (lice:default-license  . \"gpl-3.0\")
            (lice:copyright-holder . \"Your Name\")))

Replace \"gpl-3.0\" with the license you want and write your name
inside the copyright holder value.

When the user-option is t PEL activates the `pel-use-lice'
user-option if it is not activated already."
  :group 'pel-pkg-generic-code-style
  :type
  '(choice
    (const :tag  "No license, no copyright." nil)
    (const :tag  "Copyright only." only-copyright)
    (string :tag "Copyright with specified license name.")
    (const :tag
           "Copyright with license text selected by `lice:default-license'"
           t)))
(pel-put 'pel-generic-skel-with-license :choices '(nil only-copyright t "MIT"))

;; style - 3 : package names: not used for generic templates
;; style - 4 : file variable: not used for generic templates

;; style - 5
(defcustom pel-generic-skel-module-section-titles '("Module Description"
                                                    "Dependencies"
                                                    "Code")
  "Specifies whether code sections are inserted.

The choices are:
- nil: no section titles are inserted.
- a list of sections.

List of section titles to add in the module comment block.
`pel-generic-skel-insert-module-sections' is t.

The sections are placed inside the module documentation block in
the order of appearance in the list with the string as it appears
in the list.

Empty strings can be used to specify section with a tempo marker
with no text."
  :group 'pel-pkg-generic-code-style
  :type '(choice
          (const  :tag "No code section titles." nil)
          (repeat :tag "Section titles" string)))
(pel-put 'pel-generic-skel-module-section-titles
         :choices '(nil
                    ("Module Description"
                     "Dependencies"
                     "Code")))

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
(defgroup pel-sh-script-skeleton-control nil
  "PEL generic shell-script skeleton style control.
Controls what the ``<f6> h`` command does."
  :group 'pel-pkg-for-sh-scripting
  :group 'pel-pkg-generic-code-style)

(defcustom pel-shell-script-shebang-line "!/bin/sh"
  "Shebang file placed in Shell scripts by function `pel-generic-file-header'.
Do not include the comment character '#' in the string."
  :group 'pel-sh-script-skeleton-control
  :type 'string)

(defcustom pel-shell-sourced-script-first-line
  " Sourced script: %s  -*- mode: sh; -*- "
  "Format of the sourced script first line.

This string is inserted by the function `pel-generic-file-header', used
to format a shell sourced file.

It *must* have ONE string argument in the form of \"%s\" which will be
replaced by the file name."
  :group 'pel-sh-script-skeleton-control
  :type 'string)

(defcustom pel-shell-script-extensions '("sh" "bash")
  "Recognized extensions for shell script.

PEL code generation function `pel-generic-file-header' is able to generate
special code for shell script meant to be sourced. It recognizes the files
meant to be sourced when the file is recognized as a shell script file that
has a name prefix identifiable by `pel-shell-sourced-script-file-name-prefix'.
But you can force some files to have that prefix and still be regular shell
script files by using a file extension that is one of the extensions given
here."
  :group 'pel-sh-script-skeleton-control
  :group 'pel-pkg-for-sh-scripting
  :type '(repeat string))

(defcustom pel-shell-sourced-script-file-name-prefix nil
  "Identifies base names of shell files that are meant to be sourced.
If non-nil, PEL treats the files with no extensions that match the specified
requirements as shell script meant to be sourced and adjusts the skeleton
used by the ``<f6> h`` command accordingly.

For example, if the shell files that are sourced all have a name
that begin with an underscore, then enter \"\\=`_\" as the
regexp (backslash followed by back-quote without the double
quotes).

See also: `pel-shell-script-extensions'."
  :group 'pel-sh-script-skeleton-control
  :group 'pel-pkg-for-sh-scripting
  :type '(choice
          (const :tag  "Not specified" nil)
          (regexp :tag "File name regexp" :var "\\`_")))

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; Ada Programmaing Language Support
;; ---------------------------------

(defgroup pel-pkg-for-ada nil
  "PEL customization for Ada."
  :group 'pel-pkg-for-software-programming-languages
  :link `(url-link :tag "Ada PDF"
                   ,(pel-pdf-file-url "pl-ada")))

(defcustom pel-use-ada nil
  "Whether PEL activates Ada support.
When turned on,
- PEL automatically activate `pel-use-ada-mode'
- the `ada-mode' is associated with the PEL ``<f12>`` key."
  :group 'pel-pkg-for-ada
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-ada :package-is :a-gate)

(defcustom pel-use-ada-mode nil
  "Whether PEL uses the ada-mode external package."
  :link '(url-link :tag "Ada mode homepage"
                   "https://www.nongnu.org/ada-mode/")
  :group 'pel-pkg-for-ada
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-ada-mode :also-required-when 'pel-use-ada)

(defcustom pel-ada-activates-minor-modes nil
  "List of *local* minor-modes automatically activated for Ada buffers.
Enter *local* minor-mode activating function symbols.
Do not enter lambda expressions."
  :group 'pel-pkg-for-ada
  :type '(repeat function))

(defcustom pel-ada-indent-width 4
  "Number of columns for Ada source code indentation.
Values in the [2, 8] range are accepted."
  :group 'pel-pkf-for-ada
  :type 'integer
  :safe 'pel-indent-valid-p)

(defcustom pel-ada-tab-width 4
  "Column width display rendering of hard tab for buffers in `ada-mode'.

PEL stores this in `tab-width' when opening Ada buffers.

This does *NOT* control the indentation in Ada files.
It is used, however, to control the display rendering of hard tab
characters inserted inside source code and by commands that move
point to tab stop positions such as `tab-to-tab-stop', and the
display of hard TAB characters.

Values in the [2, 8] range are accepted."
  :group 'pel-pkg-for-ada
  :type 'integer
  :safe 'pel-indent-valid-p)

(defcustom pel-ada-use-tabs nil
  "Value of `indent-tabs-mode' for editing ada files.
- If set to nil: only spaces are used for indentation.
- If set to t: hard tabs are used when possible."
  :group 'pel-pkg-for-ada
  :type 'boolean
  :safe #'booleanp)

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; AppleScript support
;; -------------------
(defgroup pel-pkg-for-applescript nil
  "PEL customization for AppleScript."
  :group 'pel-pkg-for-software-programming-languages
  :group 'apples
  :link `(url-link :tag "AppleScript PDF"
                   ,(pel-pdf-file-url "pl-applescript")))

(defcustom pel-use-applescript nil
  "Control whether PEL supports the AppleScript mode."
  :group 'pel-pkg-for-applescript
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-applescript :package-is '(quote ((utils . apples-mode))))

(when (eq system-type 'darwin)
  (defcustom  pel-mac-voice-name nil
    "Name of the macOS voice used for narration.
- If the string is empty: use the System's selected voice.
- A name with 2 characters or less identifies the System's selected voice.
- To specify another name the string must have 3 or more characters."
    :group 'pel-pkg-for-applescript
    :type 'string))

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; CC Mode Language support
;; ------------------------
(defgroup pel-pkg-for-cc nil
  "PEL customization for curly-bracket programming languages."
  :group 'pel-pkg-for-software-programming-languages)

(defcustom pel-cc-auto-newline t
  "Set the default state of CC Mode electric auto-newline for all CC Modes.
This includes modes for AWK, C, C++, D.
If set to nil: disables auto-newline
If set to t:   activates auto-newline
PEL calls `c-toggle-auto-newline' to set to requested default state
for buffers in `d-mode'.  The command can be used to change the state
of auto-newline while editing."
  :group 'pel-pkg-for-cc
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-use-call-graph nil
  "Control whether PEL activates the call-graph package."
  :link '(url-link :tag "call-graph @ GitHub"
                   "https://github.com/emacsattic/call-graph")
  :group 'pel-pkg-for-cc
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-c-file-searched-extra-dir-trees nil
  "List of directory trees also searched by the C header file search.

This complements the header search method identified by the
`pel-c-file-finder-method' user-option."
  :group 'pel-pkg-for-c
  :type '(repeat string))

(defcustom pel-c-file-finder-method 'generic
  "Specify method used by `pel-open-at-point' to search C header files.

The following 4 methods listed below are supported, with the ability to
also identify an extra list of directory trees to search in the
`pel-c-file-searched-extra-dir-trees' user-option (which are searched by
the tool identified by the `pel-ffind-executable' user-option.)

The 4 methods are:

1: generic (the default):

   The parent directory tree of the current file is searched.
   The parent directory root is first identified and the file is searched
   in all its sub-directories.  The root is identified by the presence
   of one of the files identified by the `pel-project-root-identifiers'
   user option.

   The entire directory tree under the root is searched with the file finder
   tool identified by the `pel-ffind-executable' user-option.

2: Use the [file-finder] section of pel.ini file:

  Use a file named pel.ini inside the project\\='s directory tree.
  The function `pel-open-at-point' searches for that file in the parent
  directory tree identified by the same method as above.
  The pel.ini file is a .INI file using the format described
  in https://en.wikipedia.org/wiki/INI_file.  It must have the following
  elements:

  - section: \\='[file-finder]\\='
  - key:     \\='project-path\\=' : a list of directories where files are searched
                              for the current project.

  - extra tool-specific path key(s):

    There can be several other keys with a name made of
    - the tool chain name
    - the file type
    - the suffix string \"-path\"

    Something like \"IAR-c-path\", \"gcc-c-path\", \"vs-c++-path\", etc...

    These keys identify a set of directories that contain header files that
    are specific to the specified tool chain for the specific programming
    language.

    The function `pel-open-at-point' will search these extra
    directories when one of the following condition is met:

    - The PEL_CC_FIND_TOOLCHAIN environment variable is set and holds the name
      of the tool chain of one of these keys (like \"IAR\", \"gcc\", \"vs\" in
      the example above).
    - The user-option corresponding to the name of the programming language of
      the file in the current buffer identifies a tool name.  Currently PEL
      has support for C and C++ and has the following user-options:

      - `pel-c-file-finder-ini-tool-name' for searching headers from C files
      - `pel-c++-file-finder-ini-tool-name' for searching headers from C++
         files

     It searches each of the directory trees with the file finder
     tool identified by the `pel-ffind-executable' user-option.

    The command `pel-cc-set-file-finder-ini-tool-name' can be used to
    modify the value of `pel-c-file-finder-ini-tool-name' and
    `pel-c++-file-finder-ini-tool-name' dynamic value of the variable
    (but not the persistent value of the user-option which must be modified by
    the customization mechanism).

3: Environment variable string:

  The name of an environment variable (such as \"INCLUDE\") that identifies
  the directories to search.  Use this when your OS environment set up
  environment variables that inform the C compiler where header files are
  located.  The directories identified this way are searched locally, not
  recursively into their sub-directories.

4: Two lists of directories: one for the project and one for the compiler tool:

  This specifies two lists of directories. The first list identifies the
  project directories and the second list identifies the directories where the
  compiler and libraries headers are stored.

  Note that for this option, the files are searched inside the directories,
  but are *not* searched in their sub-directories.  Each directory searched
  must be identified explicitly in the list.

  Each string in the lists can use environment variables as part of the
  path-name and MUST use the $VARNAME syntax.  That can be quite useful
  when the location of the project of the tools may vary from user to user
  or computers.  For example \"$HOME/foo\" will be expanded to the foo
  sub-directory under the user\\='s home directory.

You may want to store this value inside a .dir-local.el directory
local-variable file with your C source code to control the behaviour
of the file search based on your project.

CAUTION: when changing this user-option, you may have to restart Emacs to
         activate the new behaviour in various commands like when opening
         file at point in various major modes. Executing pel-init may not
         be sufficient."
  :group 'pel-pkg-for-c
  :safe 't
  :type '(choice
          (const :tag "Generic tree search" generic)
          (const :tag "Use [file-finder] section in pel.ini" pel-ini-file)
          (string :tag "Name of environment variable that identify directories"
                  :value "INCLUDE")
          (list :tag "Explicit lists of directories for project and tool"
                (repeat :tag "Project directories" (string :tag "Project directory"))
                (repeat :tag "Tool directories"    (string :tag "Tool directory")))))

(defcustom pel-c-file-finder-ini-tool-name nil
  "Default file-finder tool name used by C projects.

This name, if specified, is the name of a key inside the
[file-finder] section of an INI configuration file for PEL,
called pel.ini and located in the project directory tree and
located automatically by PEL.

The key identifies the C language tool-chain (a compiler, an IDE,
etc...) and the value associated with that key is a of
compiler/IDE tool-chain specific directories that contain headers
and are searched when the `pel-open-at-point' command is issued
to search for a header file.

This value overrides the name of the tool-chain identified by the
PEL_CC_FIND_TOOLCHAIN environment variable, when the environment
variable is either not defined or when you want to dynamically
override the tool chain during an Emacs editing session.

When the tool chain name is identified this way, PEL looks for a
list of directories that are compiler-specific and that are
identified in the value of a [file-finder] key named after the
tool chain name and the file type.

  For example, for a C file, if the tool chain name identified by
  the PEL_CC_FIND_TOOLCHAIN environment variable or the
  `pel-c-file-finder-ini-tool-name' user-option is \"IAR\", PEL
  looks for the key named `IAR-c-path' for a list of extra
  directory names to search into.

The search inside these directories is performed recursively by
the file searching tool identified by the `pel-ffind-executable'
user-option.

See `pel-c-file-finder-method' option 2 for more information
about the expected file format of the pel.ini file."
  :group 'pel-pkg-for-c
  :type '(choice
          (const  :tag "Unused; no specific compiler identified." nil)
          (string :tag "pel.ini [file-finder] tool-chain name key"))
  :safe 't)

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; Awk Language support
;; --------------------

(defgroup pel-pkg-for-awk nil
  "PEL customization for Awk."
  :group 'pel-pkg-for-cc
  :link `(url-link :tag "Awk PDF" ,(pel-pdf-file-url "pl-awk")))

(defcustom pel-use-awk nil
  "Controls whether PEL implements extra functionality for Awk support."
  :group 'pel-pkg-for-awk
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-awk-activates-minor-modes nil
  "List of *local* minor-modes automatically activated for Awk buffers.
Enter *local* minor-mode activating function symbols.
Do not enter lambda expressions."
  :group 'pel-pkg-for-awk
  :type '(repeat function))

(defcustom pel-awk-bracket-style "awk"
  "Set the bracket style for the AWK programming language.
PEL stores this value associated with the `awk-mode' into the
`c-default-style' user option variable.
If you want to use something else, please select one of the
CC Mode Built-in Styles, which include the following:
- gnu
- k&r
- bsd
- whitesmith
- stroustrup
- ellemtel
- linux
- python
- java
- awk
- user"
  :link '(url-link
          :tag "Bracket styles @ Emacs Manual"
          "https://www.gnu.org/software/emacs/manual/html_node/\
ccmode/Built_002din-Styles.html#Built_002din-Styles")
  :link '(url-link :tag "Indentation styles @ wikipedia"
                   "https://en.wikipedia.org/wiki/Indentation_style")
  :group 'pel-pkg-for-awk
  :type 'string
  :safe 'pel-c-style-valid-p)

(defcustom pel-awk-fill-column 80
  "Column beyond which automatic line-wrapping should happen in AWK code.
Can either be nil or an integer value.
When set to nil, Emacs user option variable `fill-column' value
is used for `c-mode' buffers, otherwise the integer value specified by
`pel-c-fill-column' is stored in the variable `fill-column' for
`c-mode' buffers.  The default is 80."
  :group 'pel-pkg-for-awk
  :type '(choice
          (const   :tag "Use the default fill-column value." nil)
          (integer :tag "Use a value specific for awk-mode buffers:")))

(defcustom  pel-awk-newline-mode 'context-newline
  "Set default newline mode for c-mode buffers.

This may be one of the following values:

- context-newline : the default : the RET key is bound to
  the function `c-context-line-break' with the extra ability to
  repeat its execution with an argument.
- newline-and-indent: RET uses the function `newline' passing ARG
  and t for its arguments to insert newline and indent.
- just-newline-no-indent: RET uses the function
  `electric-indent-just-newline` with the specified argument (if
  any).  This only insert a newline; it does not indent."
  :group 'pel-pkg-for-awk
  :type '(choice
          (const :tag "context-newline: use c-context-line-break.\n\
Does all what newline does plus more."
                 context-newline)
          (const :tag "newline-and-indent: use newline. This inserts\n\
a newline and then indent the new line."
                 newline-and-indent)
          (const :tag "just-newline-no-indent: use\
 electric-indent-just-newline.\n\
Does not indent."
                 just-newline-no-indent)))

(defcustom pel-awk-indent-width 4
  "Number of columns for AWK source code indentation.
PEL stores this in `c-basic-offset' when editing buffers with C code.
Values in the [2, 8] range are accepted."
  :group 'pel-pkg-for-awk
  :type 'integer
  :safe 'pel-indent-valid-p)

(defcustom pel-awk-tab-width 4
  "Column width display rendering of hard tab for awk-mode buffers.

PEL stores this in `tab-width' when opening awk buffers.

This does *NOT* control the indentation in AWK
files, it only controls the column width display rendering of hard tabs
in buffers and tab stop positions for commands such as `tab-to-tab-stop'."
  :group 'pel-pkg-for-awk
  :type 'integer
  :safe 'pel-indent-valid-p)

(defcustom pel-awk-use-tabs nil
  "Value of `indent-tabs-mode' for editing AWK files.
- If set to nil: only spaces are used for indentation.
- If set to t: hard tabs are used when possible."
  :group 'pel-pkg-for-awk
  :type 'boolean
  :safe #'booleanp)

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; C Language Support
;; ------------------

(defgroup pel-pkg-for-c nil
  "PEL customization for C."
  :group 'pel-pkg-for-cc
  :group 'c
  :link `(url-link :tag "C PDF" ,(pel-pdf-file-url "pl-c")))

(defcustom pel-use-c nil
  "Controls whether PEL implements extra functionality for C support."
  :group 'pel-pkg-for-c
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-c :package-is :builtin-emacs)
(pel-put 'pel-use-c :also-required-when 'pel-use-bison-mode)

(defcustom pel-use-emacs-ccls-for-c nil
  "Control whether PEL activates ccls lsp for C.

This automatically activates `pel-use-emacs-ccls', `pel-use-lsp-mode'.
and `pel-use-lsp-ui-mode'."
  :group 'pel-pkg-for-c
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-c-activates-minor-modes nil
  "List of *local* minor-modes automatically activated for C buffers.
Enter *local* minor-mode activating function symbols.
Do not enter lambda expressions."
  :group 'pel-pkg-for-c
  :type '(repeat function))

(defcustom pel-use-c-eldoc nil
  "Control whether PEL supports the c-eldoc package.

c-eldoc is a minor mode that provides function prototype information
on the echo area when point is located on a function name.
c-eldoc uses the eldoc infrastructure.
Compared to language servers capabilities c-eldoc is limited.
c-eldoc uses a local compiler to pre-process the current buffer in order
to provide the information.  It can slow down cursor movement.

If you activate this option PEL provides a command to toggle this mode
via the ``<f12> ? e`` sequence."
  :group 'pel-pkg-for-c
  :type 'boolean
  :safe #'booleanp
  :link '(url-link :tag "c-eldoc @ GitHub"
                   "https://github.com/pierre-rouleau/c-eldoc"))
(pel-put 'pel-use-c-eldoc :package-is :in-utils)
(pel-put 'pel-use-c-eldoc :requires '(pel-use-c pel-use-c++))


(defcustom pel-awk-file-searched-extra-dir-trees nil
  "List of directory trees also searched by the AWK file search.

This complements the header search method identified by the
`pel-awk-file-finder-method' user-option."
  :group 'pel-pkg-for-awk
  :type '(repeat string))

(defcustom pel-awk-file-finder-method 'generic
  "Specify method used by `pel-open-at-point' to search AWK files.

The following 4 methods listed below are supported, with the ability to
also identify an extra list of directory trees to search in the
`pel-awk-file-searched-extra-dir-trees' user-option (which are searched by
the tool identified by the `pel-ffind-executable' user-option.)

The 4 methods are:

1: generic (the default):

   The parent directory tree of the current file is searched.
   The parent directory root is first identified and the file is searched
   in all its sub-directories.  The root is identified by the presence
   of one of the files identified by the `pel-project-root-identifiers'
   user option.

   The entire directory tree under the root is searched with the file finder
   tool identified by the `pel-ffind-executable' user-option.

2: Use the [file-finder] section of pel.ini file:

  Use a file named pel.ini inside the project\\='s directory tree.
  The function `pel-open-at-point' searches for that file in the parent
  directory tree identified by the same method as above.
  The pel.ini file is a .INI file using the format described
  in https://en.wikipedia.org/wiki/INI_file.  It must have the following
  elements:

  - section: \\='[file-finder]\\='
  - key:     \\='project-path\\=' : a list of directories where files are searched
                              for the current project.

  - extra tool-specific path key(s):

    There can be several other keys with a name made of
    - the tool chain name
    - the file type
    - the suffix string \"-path\"

    Something like \"IAR-awk-path\", \"gawk-awk-path\", etc...

    These keys identify a set of directories that contain header files that
    are specific to the specified tool chain for the specific programming
    language.

    The function `pel-open-at-point' will search these extra
    directories when one of the following condition is met:

    - The PEL_CC_FIND_TOOLCHAIN environment variable is set and holds the name
      of the tool chain of one of these keys (like \"IAR\", \"gawk\" in
      the example above).
    - The user-option corresponding to the name of the programming language of
      the file in the current buffer identifies a tool name.  Currently PEL
      has support for C, C++ and AWK and has the following user-options:

      - `pel-c-file-finder-ini-tool-name' for searching headers from C files
      - `pel-c++-file-finder-ini-tool-name' for searching headers from C++
         files
      - `pel-awk-file-finder-ini-tool-name' for searching included AWK files.

     It searches each of the directory trees with the file finder
     tool identified by the `pel-ffind-executable' user-option.

    The command `pel-cc-set-file-finder-ini-tool-name' can be used to
    modify the value of `pel--file-finder-ini-tool-name',
    `pel-c++-file-finder-ini-tool-name' and
    `pel-awk-file-finder-ini-tool-name'
     dynamic value of the variable (but not the persistent value of
     the user-option which must be modified by the customization mechanism).

3: Environment variable string:

  The name of an environment variable (such as \"INCLUDE\") that identifies
  the directories to search.  Use this when your OS environment set up
  environment variables that inform the C compiler where header files are
  located.  The directories identified this way are searched locally, not
  recursively into their sub-directories.

4: Two lists of directories: one for the project and one for the compiler tool:

  This specifies two lists of directories. The first list identifies the
  project directories and the second list identifies the directories where the
  compiler and libraries headers are stored.

  Note that for this option, the files are searched inside the directories,
  but are *not* searched in their sub-directories.  Each directory searched
  must be identified explicitly in the list.

  Each string in the lists can use environment variables as part of the
  path-name and MUST use the $VARNAME syntax.  That can be quite useful
  when the location of the project of the tools may vary from user to user
  or computers.  For example \"$HOME/foo\" will be expanded to the foo
  sub-directory under the user\\='s home directory.

You may want to store this value inside a .dir-local.el directory
local-variable file with your C source code to control the behaviour
of the file search based on your project.

CAUTION: when changing this user-option, you may have to restart Emacs to
         activate the new behaviour in various commands like when opening
         file at point in various major modes. Executing pel-init may not
         be sufficient."
  :group 'pel-pkg-for-awk
  :safe 't
  :type '(choice
          (const :tag "Generic tree search" generic)
          (const :tag "Use [file-finder] section in pel.ini" pel-ini-file)
          (string :tag "Name of environment variable that identify directories"
                  :value "AWKPATH")
          (list :tag "Explicit lists of directories for project and tool"
                (repeat :tag "Project directories" (string :tag "Project directory"))
                (repeat :tag "Tool directories"    (string :tag "Tool directory")))))

(defcustom pel-awk-file-finder-ini-tool-name nil
  "Default file-finder tool name used by AWK projects.

This name, if specified, is the name of a key inside the
[file-finder] section of an INI configuration file for PEL,
called pel.ini and located in the project directory tree and
located automatically by PEL.

The key identifies the AWK language tool-chain (a compiler, an IDE,
etc...) and the value associated with that key is a of
compiler/IDE tool-chain specific directories that contain headers
and are searched when the `pel-open-at-point' command is issued
to search for a header file.

This value overrides the name of the tool-chain identified by the
PEL_CC_FIND_TOOLCHAIN environment variable, when the environment
variable is either not defined or when you want to dynamically
override the tool chain during an Emacs editing session.

When the tool chain name is identified this way, PEL looks for a
list of directories that are compiler-specific and that are
identified in the value of a [file-finder] key named after the
tool chain name and the file type.

  For example, for a C file, if the tool chain name identified by
  the PEL_CC_FIND_TOOLCHAIN environment variable or the
  `pel-awk-file-finder-ini-tool-name' user-option is \"IAR\", PEL
  looks for the key named `IAR-c-path' for a list of extra
  directory names to search into.

The search inside these directories is performed recursively by
the file searching tool identified by the `pel-ffind-executable'
user-option.

See `pel-awk-file-finder-method' option 2 for more information
about the expected file format of the pel.ini file."
  :group 'pel-pkg-for-awk
  :type '(choice
          (const  :tag "Unused; no specific compiler identified." nil)
          (string :tag "pel.ini [file-finder] tool-chain name key"))
  :safe 't)

;; -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -
(defgroup pel-c-code-style nil
  "C Source Code Style options."
  :group 'pel-pkg-for-c
  :link `(url-link :tag "C PDF" ,(pel-pdf-file-url "pl-c")))

(defcustom pel-use-linux-kernel-code-style-support t
  "Activates the Linux Kernel Code Style on appropriate C files when active."
  :group 'pel-c-code-style
  :link '(custom-group-link "C")
  :link '(custom-manual "(ccmode)Built-in Styles")
  :link '(url-link :tag "Linux kernel coding style @ Github"
                   "https://github.com/torvalds/linux/blob/master/Documentation/process/coding-style.rst")
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-c-bracket-style "linux"
  "Set the bracket style for the C programming language.
PEL stores this value associated with the `c-mode' into the
`c-default-style' user option variable.
If you want to use something else, please select one of the
CC Mode Built-in Styles, which include the following:
- gnu
- k&r
- bsd
- whitesmith
- stroustrup
- ellemtel
- linux
- python
- java
- awk
- user"
  :link '(custom-group-link "C")
  :link '(custom-manual "(ccmode)Built-in Styles")
  :link '(url-link
          :tag "Bracket styles @ Emacs Manual"
          "https://www.gnu.org/software/emacs/manual/html_node/\
ccmode/Built_002din-Styles.html#Built_002din-Styles")
  :link '(url-link :tag "Indentation styles @ wikipedia"
                   "https://en.wikipedia.org/wiki/Indentation_style")
  :group 'pel-c-code-style
  :type 'string
  :safe 'pel-c-style-valid-p)

(defcustom pel-c-fill-column 80
  "Column beyond which automatic line-wrapping should happen in C code.
Can either be nil or an integer value.
When set to nil, Emacs user option variable `fill-column' value
is used for `c-mode' buffers, otherwise the integer value specified by
`pel-c-fill-column' is stored in the variable `fill-column' for
`c-mode' buffers.  The default is 80."
  :group 'pel-c-code-style
  :type '(choice
          (const   :tag "Use the default fill-column value." nil)
          (integer :tag "Use a value specific for c-mode buffers:")))

(defcustom  pel-c-newline-mode 'context-newline
  "Set default newline mode for c-mode buffers.

This may be one of the following values:

- context-newline : the default : the RET key is bound to
  the function `c-context-line-break' with the extra ability to
  repeat its execution with an argument.
- newline-and-indent: RET uses the function `newline' passing ARG
  and t for its arguments to insert newline and indent.
- just-newline-no-indent: RET uses the function
  `electric-indent-just-newline` with the specified argument (if
  any).  This only insert a newline; it does not indent."
  :group 'pel-c-code-style
  :type '(choice
          (const :tag "context-newline: use c-context-line-break.\n\
Does all what newline does plus more."
                 context-newline)
          (const :tag "newline-and-indent: use newline. This inserts\n\
a newline and then indent the new line."
                 newline-and-indent)
          (const :tag "just-newline-no-indent: use\
 electric-indent-just-newline.\n\
Does not indent."
                 just-newline-no-indent)))

(defcustom pel-c-indent-width 4
  "Number of columns for C source code indentation.
PEL stores this in `c-basic-offset' when editing buffers with C code.
Values in the [2, 8] range are accepted."
  :group 'pel-c-code-style
  :type 'integer
  :safe 'pel-indent-valid-p)

(defcustom pel-c-tab-width 4
  "Column width display rendering of hard tab for C source code.

PEL stores this in `tab-width' when editing buffer with C source.

This does *NOT* control the indentation in C source code.
It is used, however, to control the display rendering of hard tab
characters inserted inside source code and by commands that move
point to tab stop positions such as `tab-to-tab-stop', and the
display of hard TAB characters.

It is often the same value as `pel-c-indent-width', if it is
different it should probably be a multiple of
`pel-c-indent-width'.

Values in the [2, 8] range are accepted."
  :group 'pel-c-code-style
  :type 'integer
  :safe 'pel-indent-valid-p)

(defcustom pel-c-use-tabs nil
  "Value of `indent-tabs-mode' for editing C source code.
- If set to nil: only spaces are used for indentation.
- If set to t: hard tabs are used when possible."
  :group 'pel-c-code-style
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-c-multiline-comments nil
  "If non-nil, single multi-line comments are created by `pel-c-comment-dwim'.

If nil, `comment-dwim' is always used for comment control.
If non-nil `pel-c-comment-dwim' is used.  That command is able to create
multi-line comments on any column.  The style of comment is one of the
following:

- 2-stars  /* Two stars
           ** continuation
           ** comment.
           */

- 1-star   /* One star
            * continuation
            * comment.
            */

"
  :group 'pel-c-code-style
  :type '(choice
          (const :tag "No multi-line comments" nil)
          (const :tag "One star continuation multi-line comments"  1)
          (const :tag "Two stars continuation multi-line comments" 2)))

;;    -       -       -       -       -       -       -       -       -       -
(defgroup pel-c-skeleton-control nil
  "Control Skeleton that generate C source code."
  :group 'pel-c-code-style
  :group 'pel-pkg-for-skeletons
  :link `(url-link :tag "C PDF" ,(pel-pdf-file-url "pl-c")))

;;    .       .       .       .       .       .       .       .       .       .
(defgroup pel-c-module-header-skeleton-control nil
  "Control Skeleton that generate C source code."
  :group 'pel-c-skeleton-control
  :link `(url-link :tag "C PDF" ,(pel-pdf-file-url "pl-c")))

;; style - 0
(defcustom pel-c-skel-use-separators t
  "Specifies whether C code block include separators line.
If nil no separator line comment is used, otherwise separator line
comments of length controlled by variable `fill-column' are inserted."
  :group 'pel-c-module-header-skeleton-control
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-c-skel-use-separators :choices '(nil t))

;; style - 1
(defcustom pel-c-skel-insert-file-timestamp t
  "Specifies whether a timestamp is inserted inside C file header block."
  :group 'pel-c-module-header-skeleton-control
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-c-skel-insert-file-timestamp :choices '(nil t))

;; style - 2
(defcustom pel-c-skel-with-license nil
  "Control whether a license text is inserted in C file header.

The available choices are:

- 0: No copyright, no license.                 nil
- 1: Copyright only, no license.               \\='only-copyright
- 2: Copyright, with selected license name.    a string
- 3: Copyright, with complete license text.    t

For choice 3, you specify the name of the license in the string
field.  Something like MIT or GPL-3.0, or anything appropriate.
That string will be entered verbatim inside a file header line.

For choice 4, the type of the license and its text are selected
the function `lice' taken from the external library with the same name.

The text of the inserted license is selected by the
`lice:default-license' user option, normally configured inside
the directory\\='s .dir-locals.el file written inside the global
setting like this:

   ((nil . ((fill-column . 80)
            (lice:default-license  . \"gpl-3.0\")
            (lice:copyright-holder . \"Your Name\")))

Replace \"gpl-3.0\" with the license you want and write your name
inside the copyright holder value.

When the user-option is t PEL activates the `pel-use-lice'
user-option if it is not activated already."
  :group 'pel-c-module-header-skeleton-control
  :type
  '(choice
    (const :tag  "No license, no copyright." nil)
    (const :tag  "Copyright only." only-copyright)
    (string :tag "Copyright with specified license name.")
    (const :tag
           "Copyright with license text selected by `lice:default-license'"
           t)))
(pel-put 'pel-c-skel-with-license :choices '(nil only-copyright t "MIT"))

;; style - 3 : no package name support for C
;; style - 4 : no file variable support for C

;; style - 5
(defcustom pel-c-skel-cfile-section-titles '("Module Description"
                                             "Header Inclusion"
                                             "Local Types"
                                             "Local Variables"
                                             "Code")
  "Specifies whether code sections are inserted inside C file comment block.

The choices are:
- nil: no section titles are inserted.
- a list of sections.

The default includes fives sections.  You can add, replace and remove them.

These section titles are placed inside the module documentation
block in the order of appearance in the list with the string as
it appears in the list.  The default is to add the following
sections:

- Module Description
- Header Inclusion,
- Local Types,
- Local Variables,
- Code.

Empty strings can be used to specify section with a tempo marker with no text."
  :group 'pel-c-module-header-skeleton-control
  :type '(choice
          (const :tag "No code section titles." nil)
          (repeat :tag "Section titles" string)))
(pel-put 'pel-c-skel-cfile-section-titles
         :choices
         '(nil
           ("Module Description"
            "Header Inclusion"
            "Local Types"
            "Local Variables"
            "Code")))

;; style - 5.1
(defcustom pel-c-skel-hfile-section-titles '("Description"
                                             "."
                                             "Header Inclusion"
                                             ".#if defined(__cplusplus)\nextern \"C\" {\n#endif\n"
                                             "Public Types"
                                             "Public Constants"
                                             "Inline Functions"
                                             ".#if defined(__cplusplus)\n}\n#endif\n")
  "Specifies whether code sections are inserted inside C file comment block.

The choices are:
- nil: no section titles are inserted.
- a list of sections.

When section titles are identified, the first character of the string
may be \\='.\\=' or \\=',\\=' to identify special cases:

- A special section string \".\" identifies where the include guard
  code must be inserted.
- A string that starts with \\='.\\=' is inserted verbatim, skipping the
  first character and preceded by a separator line.
- A string that starts with \\=',\\=' is inserted verbatim, skipping
  the first character (and not preceded by a separator).

The default includes fives sections and the position of the
include guard is located between the first and the second
section.  It also insert support for C++ \\='extern \"C\"\\=' code.

You can customize the sections to generate what you prefer.
You can change, add, replace and remove the pre-defined sections.

These section titles are placed inside the module documentation
block in the order of appearance in the list with the string as
it appears in the list.  The default is to add the following
sections:

- Description
- Header Inclusion,
- Types,
- Variables,
- Code.

The Code section is mean to store template and inline definitions.

Empty strings can be used to specify section with a tempo marker with no text."
  :group 'pel-c-module-header-skeleton-control
  :type '(choice
          (const :tag "No code section titles." nil)
          (repeat :tag "Section titles" string)))
(pel-put 'pel-c-skel-hfile-section-titles
         :choices
         '(nil
           ("Description"
            "."
            "Header Inclusion"
            "Types"
            "Constants"
            "Inline Functions")
           ("Description"
            "Header Inclusion"
            "Types"
            "Constants"
            "Inline Functions")
           ))

;; style - 6
(defcustom pel-c-skel-doc-markup nil
  "Specifies the documentation markup system used for C source code."
  :group 'pel-c-module-header-skeleton-control
  :type '(choice
          (const :tag "No documentation markup inserted in templates." nil)
          (const :tag "Insert Doxygen markup in templates." doxygen)))
(pel-put 'pel-c-skel-doc-markup :choices '(nil doxygen))

;; style - 7
(defcustom pel-c-skel-comment-with-2stars t
  "Specifies whether multi-line C comments continuation use 2 stars.
If set to t (the default), C comments in generated code
use the following style comment format:   /*
                                          **
                                          */

If set to nil, the comment style is:      /*
                                           *
                                           */"
  :group 'pel-c-module-header-skeleton-control
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-c-skel-comment-with-2stars :choices '(nil t))

;; style - 8
(defcustom pel-c-skel-use-include-guards t
  "Controls if include guards are inserted inside C header file.
Also identifies what type of include guard are used.
The choices are:

- insert no include guard:         nil
- insert pragma-once:             \\='pragma-once
- insert classic include-guard:    t
- insert include-guard with UUID: \\='with-uuid

An include guard with-uuid is the most portable and it is safe since
it prevents re-use of the same C pre-processor symbol."
  :group 'pel-c-module-header-skeleton-control
  :type '(choice
          (const :tag "No include guard" nil)
          (const :tag "Use pragma-once" pragma-once)
          (const :tag "Classic include-guard" t)
          (const :tag "Include-guard with UUID" with-uuid)))
(pel-put 'pel-c-skel-use-include-guards :choices '(nil
                                                   pragma-once
                                                   t
                                                   with-uuid))

(defcustom pel-c-skel-module-header-block-style nil
  "Specifies the style of the C file module header block.
You can use one of the following:

- The default (nil) controlled by PEL\\='s code.
- A user-specified one.  For this you have to write Emacs Lisp code.
  You have to write a function `pel-skels-c-header-module-block/custom'
  inside a file and store the name of that file inside the box that
  appear when you select this option.   The function must accept
  3 arguments:
  - arg 1: string : the file name (without path)
  - arg 2: boolean: is non-nil when the file is a C header file,
                    and nil when it is a .c file.
  - arg 3: a list of 3 strings: (cb cc ce)
            - cb : comment begin string
            - cc : comment continuation string
            - ce : comment end string.

  The function can use or ignore these arguments.
  See PEL\\='s function `pel-skels-c-header-module-block' source code
  for an example of how these arguments are used to create the standard
  header-module block skeleton.

  You can start by using the example that is stored inside the file
  \\='custom/skeleton/custom-c-skel.el\\='.
  The file name can be an absolute file name but it can also be a relative
  file name.  On Unix systems you can use \\='~\\=' to identify your home directory."
  :group 'pel-c-module-header-skeleton-control
  :type '(choice
          (const  :tag "Default, controlled by PEL." nil)
          (string :tag "Use your own custom definition\n inside file")))

;;    .       .       .       .       .       .       .       .       .       .
(defgroup pel-c-function-header-skeleton-control nil
  "Control Skeleton that generate C source code."
  :group 'pel-c-skeleton-control
  :link `(url-link :tag "C PDF" ,(pel-pdf-file-url "pl-c")))

(defcustom pel-c-skel-insert-function-sections t
  "Specifies whether code sections are inserted in C function comment block.
This includes the DESCRIPTION section and sections with titles
identified by the variable `pel-c-skel-function-section-titles'."
  :group 'pel-c-function-header-skeleton-control
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-c-skel-function-section-titles '("DIAGNOSTIC"
                                                "SEE ALSO")
  "List of section titles to add in the function comment block.
These section names are added when the variable
`pel-c-skel-insert-function-sections' is t, after the DESCRIPTION
section.  The sections are placed inside the function
documentation block in the order of appearance in the list with
the string as it appears in the list.  The default is to add the
sections DIAGNOSTIC and SEE ALSO.  Empty strings can be used to
specify section with a tempo marker with no text."
  :group 'pel-c-function-header-skeleton-control
  :type '(repeat string))

(defcustom pel-c-skel-function-define-style nil
  "Specifies the style of C function definition comment blocks.
Several styles are provided with ability to load a style from
a separately provided skeleton file.

The choices are:

- No documentation comment inserted.
- Basic documentation comment just above the function definition.
  This includes a function purpose.
- Man-page style documentation above the function definition.
  If variable `pel-c-skel-insert-function-sections' is t, the comment also
  include a DESCRIPTION section and other sections as defined by the
  value of the variable `pel-c-skel-function-section-titles'.
- A user defined skeleton.  For this you need to write Emacs Lisp code.
  You have to write a function `pel-skels-c-function-def/custom'
  inside a file and store the name of that file inside the box that
  appear when you select this option.  You can start by using the
  example that is stored inside the file \\='custom/skeleton/custom-c-skel.el\\='.
  The file name can be an absolute file name but it can also be a relative
  file name.  On Unix systems you can use \\='~\\=' to identify your home directory."
  :group 'pel-c-function-header-skeleton-control
  :type '(choice
          (const :tag "Just code, no comment block." nil)
          (const :tag "Basic documentation block above function definition."
                 basic-style)
          (const :tag "Man-page style documentation block above function \
definition." man-style)
          (string :tag "Use your own custom definition\n inside file")))

(defcustom pel-c-skel-function-name-on-first-column nil
  "Set whether defined function name is on the beginning of the line.
If non-nil, the return type of a function definition is located
on a line by itself, above the function name that starts at the
beginning of next line.  When nil, the return type of the
function definition is located on the same line as the function
name.

For example, if t, the style is:

int*
some_function(int some_arg)
{
   some_code();
}

If the value is nil, the style is this instead:

int* some_function(int some_arg)
{
   some_code();
}

This affects all styles specified by variable
`pel-c-skel-function-define-style' potentially except the user
defined ones, which could use that variable too."
  :group 'pel-c-function-header-skeleton-control
  :type 'boolean
  :safe #'booleanp)

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; Bison, Lex, Yacc Support
;; ------------------------
;;
;; These modes extend the C mode that is described below.
;; Most commands available for C are available in bison-mode.

(defgroup pel-pkg-for-bison nil
  "PEL customization for Bison, Lex and YACC."
  :group 'pel-pkg-for-cc
  :group 'c
  :group 'pel-pkg-for-c
  :group 'pel-pkg-for-parser
  :link `(url-link :tag "C PDF" ,(pel-pdf-file-url "pl-c")))

(defcustom pel-use-bison-mode nil
  "Control whether PEL activates the bison-mode.
When active it is associated to the .lex and .yacc files,
taking over the default association with c-mode."
  :group 'pel-pkg-for-bison
  :link '(url-link :tag "bison-mode @ GitHub"
                   "https://github.com/Wilfred/bison-mode")
  :type 'boolean
  :safe #'booleanp)

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; C++ Language Support
;; --------------------

(defgroup pel-pkg-for-c++ nil
  "PEL customization for C++."
  :group 'pel-pkg-for-cc
  :link `(url-link :tag "C++ PDF" ,(pel-pdf-file-url "pl-c++")))

(defcustom pel-use-c++ nil
  "Controls whether PEL implements extra functionality for C++ support."
  :group 'pel-pkg-for-c++
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-c++ :package-is :builtin-emacs)

(defcustom pel-use-emacs-ccls-for-c++ nil
  "Control whether PEL activates ccls lsp for C++.

This automatically activates `pel-use-emacs-ccls', `pel-use-lsp-mode'.
and `pel-use-lsp-ui-mode'."
  :group 'pel-pkg-for-c++
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-c++-activates-minor-modes nil
  "List of *local* minor-modes automatically activated for C++ buffers.
Enter *local* minor-mode activating function symbols.
Do not enter lambda expressions."
  :group 'pel-pkg-for-c++
  :type '(repeat function))

(defcustom pel-c++-file-searched-extra-dir-trees nil
  "List of directory trees also searched by the C++ header file search.

This complements the header search method identified by the
`pel-c++-file-finder-method' user-option."
  :group 'pel-pkg-for-c++
  :type '(repeat string))

(defcustom pel-c++-file-finder-method 'generic
  "Specify method used by `pel-open-at-point' to search C/C++ header files.

See `pel-c-file-finder-method' docstring for more information."
  :group 'pel-pkg-for-c++
  :safe 't
  :type '(choice
          (const :tag "Generic tree search" generic)
          (const :tag "Use [file-finder] section in pel.ini" pel-ini-file)
          (string :tag "Name of environment variable that identify directories"
                  :value "INCLUDE")
          (list :tag "Explicit lists of directories for project and tool"
                (repeat :tag "Project directories" (string :tag "Project directory"))
                (repeat :tag "Tool directories"    (string :tag "Tool directory")))))

(defcustom pel-c++-file-finder-ini-tool-name nil
  "Default file-finder tool name used by C++ projects.

This name, if specified, is the name of a key inside the
[file-finder] section of an INI configuration file for PEL,
called pel.ini and located in the project directory tree and
located automatically by PEL.

The key identifies the C++ language tool-chain (a compiler, an IDE,
etc...) and the value associated with that key is a of
compiler/IDE tool-chain specific directories that contain headers
and are searched when the `pel-open-at-point' command is issued
to search for a header file.

This value overrides the name of the tool-chain identified by the
PEL_CC_FIND_TOOLCHAIN environment variable, when the environment
variable is either not defined or when you want to dynamically
override the tool chain during an Emacs editing session.

When the tool chain name is identified this way, PEL looks for a
list of directories that are compiler-specific and that are
identified in the value of a [file-finder] key named after the
tool chain name and the file type.

  For example, for a C++ file, if the tool chain name identified by
  the PEL_CC_FIND_TOOLCHAIN environment variable or the
  `pel-c-file-finder-ini-tool-name' user-option is \"IAR\", PEL
  looks for the key named `IAR-c++-path' for a list of extra
  directory names to search into.

The search inside these directories is performed recursively by
the file searching tool identified by the `pel-ffind-executable'
user-option.

See `pel-c-file-finder-method' option 2 for more information
about the expected file format of the pel.ini file."
  :group 'pel-pkg-for-c++
  :type '(choice
          (const  :tag "Unused; no specific compiler identified." nil)
          (string :tag "pel.ini [file-finder] tool-chain name key"))
  :safe 't)

;; -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -
(defgroup pel-c++-code-style nil
  "C++ Source Code Style options."
  :group 'pel-pkg-for-c++
  :link `(url-link :tag "C++ PDF" ,(pel-pdf-file-url "pl-c++")))

(defcustom pel-c++-bracket-style "stroustrup"
  "Set the bracket style for the C++ programming language.
PEL stores this value associated with the `c-mode' into the
`c-default-style' user option variable.
If you want to use something else, please select one of the
CC Mode Built-in Styles."
  :group 'pel-c++-code-style
  :type 'string
  :safe 'pel-c-style-valid-p
  :link '(custom-group-link "pel-pkg-for-c")
  :link '(custom-manual "(ccmode)Built-in Styles")
  :link '(url-link
          :tag "Bracket styles @ Emacs Manual"
          "https://www.gnu.org/software/emacs/manual/html_node/\
ccmode/Built_002din-Styles.html#Built_002din-Styles")
  :link '(url-link :tag "Indentation styles @ wikipedia"
                   "https://en.wikipedia.org/wiki/Indentation_style"))

(defcustom pel-c++-fill-column 80
  "Column beyond which automatic line-wrapping should happen in C++ code.
Can either be nil or an integer value.
When set to nil, Emacs user option variable `fill-column' value
is used for `c++-mode' buffers, otherwise the integer value specified by
`pel-c++-fill-column' is stored in the variable `fill-column' for
`c++-mode' buffers.  The default is 80."
  :group 'pel-c++-code-style
  :type '(choice
          (const   :tag "Use the default fill-column value." nil)
          (integer :tag "Use a value specific for c++-mode buffers:")))

(defcustom  pel-c++-newline-mode 'context-newline
  "Set default newline mode for c++-mode buffers.

This may be one of the following values:

- context-newline : the default : the RET key is bound to
  the function `c-context-line-break' with the extra ability to
  repeat its execution with an argument.
- newline-and-indent: RET uses the function `newline' passing ARG
  and t for its arguments to insert newline and indent.
- just-newline-no-indent: RET uses the function
  `electric-indent-just-newline` with the specified argument (if
  any).  This only insert a newline; it does not indent."
  :group 'pel-c++-code-style
  :type '(choice
          (const :tag "context-newline: use c-context-line-break.\n\
Does all what newline does plus more."
                 context-newline)
          (const :tag "newline-and-indent: use newline. This inserts\n\
a newline and then indent the new line."
                 newline-and-indent)
          (const :tag "just-newline-no-indent: use\
 electric-indent-just-newline.\n\
Does not indent."
                 just-newline-no-indent)))

(defcustom pel-c++-indent-width 3
  "Number of columns for C++ source code indentation.
PEL stores this in `c-basic-offset' when editing buffers with C++ source.
Values in the [2, 8] range are accepted."
  :group 'pel-c++-code-style
  :type 'integer
  :safe 'pel-indent-valid-p)

(defcustom pel-c++-tab-width 3
  "Column width display rendering of hard tab for C++ source code.

PEL stores this in `tab-width' when editing buffer with C++ source.
It is used, however, to control the display rendering of hard tab
characters inserted inside source code and by commands that move
point to tab stop positions such as `tab-to-tab-stop', and the
display of hard TAB characters.

It is often the same value as `pel-c++-indent-width', if it is
different it should probably be a multiple of
`pel-c++-indent-width'.

Values in the [2, 8] range are accepted."
  :group 'pel-c++-code-style
  :type 'integer
  :safe 'pel-indent-valid-p)

(defcustom pel-c++-use-tabs nil
  "Value of `indent-tabs-mode' for editing C++ source code.
- If set to nil: only spaces are used for indentation.
- If set to t: hard tabs are used when possible."
  :group 'pel-c++-code-style
  :type 'boolean
  :safe #'booleanp)

;;    -       -       -       -       -       -       -       -       -       -
(defgroup pel-c++-skeleton-control nil
  "Control Skeleton that generate C++ source code."
  :group 'pel-c++-code-style
  :group 'pel-pkg-for-skeletons
  :link `(url-link :tag "C++ PDF" ,(pel-pdf-file-url "pl-c++")))

;;    .       .       .       .       .       .       .       .       .       .
(defgroup pel-c++-module-header-skeleton-control nil
  "Control Skeleton that generate C++ source code."
  :group 'pel-c++-skeleton-control
  :link `(url-link :tag "C++ PDF" ,(pel-pdf-file-url "pl-c++")))

;; style - 0
(defcustom pel-c++-skel-use-separators t
  "Specifies whether C++ code block include separators line.
If nil no separator line comment is used, otherwise separator line
comments of length controlled by variable `fill-column' are inserted."
  :group 'pel-c++-module-header-skeleton-control
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-c++-skel-use-separators :choices '(nil t))

;; style - 1
(defcustom pel-c++-skel-insert-file-timestamp t
  "Specifies whether a timestamp is inserted inside C++ file header block."
  :group 'pel-c++-module-header-skeleton-control
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-c++-skel-insert-file-timestamp :choices '(nil t))

;; style - 2
(defcustom pel-c++-skel-with-license nil
  "Control whether a license text is inserted in C++ file header.

The available choices are:

- 0: No copyright, no license.                 nil
- 1: Copyright only, no license.               \\='only-copyright
- 2: Copyright, with selected license name.    a string
- 3: Copyright, with complete license text.    t

For choice 3, you specify the name of the license in the string
field.  Something like MIT or GPL-3.0, or anything appropriate.
That string will be entered verbatim inside a file header line.

For choice 4, the type of the license and its text are selected
the function `lice' taken from the external library with the same name.

The text of the inserted license is selected by the
`lice:default-license' user option, normally configured inside
the directory\\='s .dir-locals.el file written inside the global
setting like this:

   ((nil . ((fill-column . 80)
            (lice:default-license  . \"gpl-3.0\")
            (lice:copyright-holder . \"Your Name\")))

Replace \"gpl-3.0\" with the license you want and write your name
inside the copyright holder value.

When the user-option is t PEL activates the `pel-use-lice'
user-option if it is not activated already."
  :group 'pel-c++-module-header-skeleton-control
  :type
  '(choice
    (const :tag  "No license, no copyright." nil)
    (const :tag  "Copyright only." only-copyright)
    (string :tag "Copyright with specified license name.")
    (const :tag
           "Copyright with license text selected by `lice:default-license'"
           t)))
(pel-put 'pel-c++-skel-with-license :choices '(nil only-copyright t "MIT"))

;; style - 3 : no package name support for C++
;; style - 4 : no file variable support for C++

;; style - 5
(defcustom pel-c++-skel-cppfile-section-titles '("Module Description"
                                                 "Header Inclusion"
                                                 "Local Types"
                                                 "Local Variables"
                                                 "Code")
  "Specifies whether code sections are inserted inside C++ file comment block.

The choices are:
- nil: no section titles are inserted.
- a list of sections.

The default includes fives sections.  You can add, replace and remove them.

These section titles are placed inside the module documentation
block in the order of appearance in the list with the string as
it appears in the list.  The default is to add the following
sections:

- Module Description
- Header Inclusion,
- Local Types,
- Local Variables,
- Code.

Empty strings can be used to specify section with a tempo marker with no text."
  :group 'pel-c++-module-header-skeleton-control
  :type '(choice
          (const :tag "No code section titles." nil)
          (repeat :tag "Section titles" string)))

;; style - 5.1
(defcustom pel-c++-skel-hppfile-section-titles '("Description"
                                                 "."
                                                 "Header Inclusion"
                                                 "Types"
                                                 "Variables"
                                                 "Code")
  "Specifies whether code sections are inserted inside C++ file comment block.

The choices are:
- nil: no section titles are inserted.
- a list of sections.

When section titles are identified a special section string \".\"
identifies where the include guard code must be inserted.

The default includes fives sections and the position of the
include guard is located between the first and the second
section.  You can add, replace and remove them.

These section titles are placed inside the module documentation
block in the order of appearance in the list with the string as
it appears in the list.  The default is to add the following
sections:

- Description
- Header Inclusion,
- Types,
- Variables,
- Code.

The Code section is mean to store template and inline definitions.

Empty strings can be used to specify section with a tempo marker with no text."
  :group 'pel-c++-module-header-skeleton-control
  :type '(choice
          (const :tag "No code section titles." nil)
          (repeat :tag "Section titles" string)))
(pel-put 'pel-c++-skel-hppfile-section-titles
         :choices
         '(nil
           ("Description"
            "."
            "Header Inclusion"
            "Types"
            "Variables"
            "Code")))

;; style - 6
(defcustom pel-c++-skel-doc-markup nil
  "Specifies the documentation markup system used for C++ source code."
  :group 'pel-c++-module-header-skeleton-control
  :type '(choice
          (const :tag "No documentation markup inserted in templates." nil)
          (const :tag "Insert Doxygen markup in templates." doxygen)))
(pel-put 'pel-c++-skel-doc-markup :choices '(nil doxygen))

;; style - 7 : C++ templates comments only support //

;; style - 8
(defcustom pel-c++-skel-use-include-guards t
  "Controls if include guards are inserted inside C++ header file.
Also identifies what type of include guard are used.
The choices are:

- insert no include guard:         nil
- insert pragma-once:             \\='pragma-once
- insert classic include-guard:    t
- insert include-guard with UUID: \\='with-uuid

An include guard with-uuid is the most portable and it is safe since
it prevents re-use of the same C pre-processor symbol."
  :group 'pel-c++-module-header-skeleton-control
  :type '(choice
          (const :tag "No include guard" nil)
          (const :tag "Use pragma-once" pragma-once)
          (const :tag "Classic include-guard" t)
          (const :tag "Include-guard with UUID" with-uuid)))
(pel-put 'pel-c++-skel-use-include-guards :choices '(nil
                                                     pragma-once
                                                     t
                                                     with-uuid))

(defcustom pel-c++-skel-module-header-block-style nil
  "Specifies the style of the C++ file module header block.
You can use one of the following:

- The default (nil) controlled by PEL\\='s code.
- A user-specified one.  For this you have to write Emacs Lisp code.
  You have to write a function `pel-skels-c++-header-module-block/custom'
  inside a file and store the name of that file inside the box that
  appear when you select this option.   The function must accept
  2 arguments:
  - arg 1: string : the file name (without path)
  - arg 2: boolean: is non-nil when the file is a C++ header file,
                    and nil when it is a code file.

  The function can use or ignore these arguments.
  See PEL\\='s function `pel-skels-c++-header-module-block' source code
  for an example of how these arguments are used to create the standard
  header-module block skeleton.

  You can start by using the example that is stored inside the file
  \\='custom/skeleton/custom-c++-skel.el\\='.
  The file name can be an absolute file name but it can also be a relative
  file name.  On Unix systems you can use \\='~\\=' to identify your home directory."
  :group 'pel-c++-module-header-skeleton-control
  :type '(choice
          (const  :tag "Default, controlled by PEL." nil)
          (string :tag "Use your own custom definition\n inside file")))

;;    .       .       .       .       .       .       .       .       .       .
(defgroup pel-c++-function-header-skeleton-control nil
  "Control Skeleton that generate C++ function definition source code."
  :group 'pel-c++-skeleton-control
  :link `(url-link :tag "C++ PDF" ,(pel-pdf-file-url "pl-c++")))

(defcustom pel-c++-skel-insert-function-sections t
  "Specifies whether code sections are inserted in C++ function comment block.
This includes the DESCRIPTION section and sections with titles
identified by the variable `pel-c++-skel-function-section-titles'."
  :group 'pel-c++-function-header-skeleton-control
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-c++-skel-function-section-titles '("DIAGNOSTIC"
                                                  "SEE ALSO")
  "List of section titles to add in the function comment block.
These section names are added when the variable
`pel-c++-skel-insert-function-sections' is t, after the DESCRIPTION
section.  The sections are placed inside the function
documentation block in the order of appearance in the list with
the string as it appears in the list.  The default is to add the
sections DIAGNOSTIC and SEE ALSO.  Empty strings can be used to
specify section with a tempo marker with no text."
  :group 'pel-c++-function-header-skeleton-control
  :type '(repeat string))

(defcustom pel-c++-skel-function-define-style nil
  "Specifies the style of C++ function definition comment blocks.
Several styles are provided with ability to load a style from
a separately provided skeleton file.

The choices are:

- nil: No documentation comment inserted.
- basic-style: Basic documentation comment just above the
  function definition. This includes a function purpose.
- man-page: Man-page style documentation above the function definition.
  If variable `pel-c++-skel-insert-function-sections' is t, the comment also
  include a DESCRIPTION section and other sections as defined by the
  value of the variable `pel-c++-skel-function-section-titles'.
- A user defined skeleton.  For this you need to write Emacs Lisp code.
  You have to write a function `pel-skels-c++-function-def/custom'
  inside a file and store the name of that file inside the box that
  appear when you select this option.  You can start by using the
  example that is stored inside the file \\='custom/skeleton/custom-c++-skel.el\\='.
  The file name can be an absolute file name but it can also be a relative
  file name.  On Unix systems you can use \\='~\\=' to identify your home directory."
  :group 'pel-c++-function-header-skeleton-control
  :type '(choice
          (const :tag "Just code, no comment block." nil)
          (const :tag "Basic documentation block above function definition."
                 basic-style)
          (const :tag "Man-page style documentation block above function \
definition." man-style)
          (string :tag "Use your own custom definition\n inside file")))

(defcustom pel-c++-skel-function-name-on-first-column nil
  "Set whether defined function name is on the beginning of the line.
If non-nil, the return type of a function definition is located
on a line by itself, above the function name that starts at the
beginning of next line.  When nil, the return type of the
function definition is located on the same line as the function
name.

For example, if t, the style is:

int*
some_function(int some_arg)
{
   some_code();
}

If the value is nil, the style is this instead:

int* some_function(int some_arg)
{
   some_code();
}

This affects all styles specified by variable
`pel-c++-skel-function-define-style' potentially except the user
defined ones, which could use that variable too."
  :group 'pel-c++-function-header-skeleton-control
  :type 'boolean
  :safe #'booleanp)

;;    .       .       .       .       .       .       .       .       .       .
(defgroup pel-c++-class-skeleton-control nil
  "Control Skeleton that generate C++ class definition source code."
  :group 'pel-c++-skeleton-control
  :link `(url-link :tag "C++ PDF" ,(pel-pdf-file-url "pl-c++")))

(defcustom pel-c++-class-has-doc-block nil
  "Specifies whether a description block is placed before class definition."
  :group 'pel-c++-class-skeleton-control
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-c++-class-doc-section-titles '("Description"
                                              "Diagnostic"
                                              "Example"
                                              "See Also")
  "Specifies C++ class documentation block section titles.

The choices are:
- nil: no section titles are inserted.
- a list of section titles, inserted above the C++ class definition code."
  :group 'pel-c++-class-skeleton-control
  :type '(choice
          (const :tag "No section titles." nil)
          (repeat :tag "Section"
                  (string :tag "Title"))))

(defcustom pel-c++-class-members-sections
  '((public    "exception class" )
    (public    "types" )
    (protected "types" )
    (private   "types" )
    (public    "static class constant data members" )
    (public    "static class function members")
    (public    "class construction/destruction/operators"
               ("// Default constructor. Allows:"
                "//                      - $class-name value;"
                "//                      - $class-name arr[10];"
                "//                      - $class-name* pt = new $class-name[20];"
                "$class-name();"
                ""
                "// $class-name destructor: cleanup"
                "virtual ~$class-name();"
                "$$"
                ""))
    (public    "function members")
    (protected "function members")
    (private   "function members")
    (public    "static class data members")
    (protected "static class data members")
    (private   "static class data members")
    (private   "Forbidden operators"
               (
                "// For pre-C++11:"
                "//lint -save"
                "//lint -esym(1526, $class-name::$class-name )"
                "//lint -esym(1704, $class-name::$class-name )"
                "//lint -esym(1714, $class-name::$class-name )"
                "// copy constructor. "
                "//   supports : $class-name a_$class-name = another_$class-name;"
                "//   and      : some_function($class-name value) "
                "$class-name($class-name const& other);"
                ""
                "//lint -esym(1526, $class-name::operator= )"
                "//lint -esym(1704, $class-name::operator= )"
                "//lint -esym(1714, $class-name::operator= )"
                "// assignment: cleanup and copy.    "
                "//   supports:  a_$class-name = another_$class-name = yetanother_$class-name;"
                "$class-name& operator=($class-name const& other);"
                "$$"
                ""
                "// assignment from other type.      Prevents: a_$class-name = a_Other;"
                "$class-name& operator=(Other const& other);"
                "$$"
                ""
                "//lint -restore")))
  "Specifies the member sections of a class definition.

Specify any number of member group code blocks.
Each one has:
- access keyword: public, protected or private,
- a title (empty string means no title): the access keyword
  is placed in front of the title in the expansion, unless
  the first letter of the title is upper-case.  Every word
 is capitalized in the expansion.
- 0 or more strings of code:
  - If no string is specified, a 3 line entry is generated with
    a tempo marker on the second line.
  - If one or more string is specified, each string correspond
    to a line and only these lines are entered.
    - An empty string identify just a new line.
    - A string mys contain comment and or C++ code.
    - The following meta-symbols have special meaning:
      - $$ inside a string identify the position of a
        tempo marker.
      - $class-name is replaced with the actual class name
        during expansion.

Each line is indented according to the `pel-c++-indent-width'.

The default is a showcase of what can be done with this template.
Adjust it to your needs."
  :group 'pel-c++-class-skeleton-control
  :type '(repeat
          (list
           (choice :tag "Access"
                   (const :tag "public" public)
                   (const :tag "protected" protected)
                   (const :tag "private" private))
           (string :tag "Title")
           (repeat :tag "Code"
                   (string :tag "line")))))

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; D Language Support
;; ------------------
;; Note: support is for D 2.x (as opposed to the older, different and now
;; obsolete first implementation of D called D 1).

(defgroup pel-pkg-for-d nil
  "PEL customization for D."
  :group 'pel-pkg-for-cc
  :link `(url-link :tag "D PDF" ,(pel-pdf-file-url "pl-d")))

(defcustom pel-use-d nil
  "Control whether PEL supports the D programming language.
- Activates the use of: Emacs D Mode
- Required to activate the use of:
 - Auto-Complete for D
 - Company mode for D"
  :group 'pel-pkg-for-d
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-d :package-is 'd-mode)

(defcustom pel-d-activates-minor-modes nil
  "List of *local* minor-modes automatically activated for D buffers.
Enter *local* minor-mode activating function symbols.
Do not enter lambda expressions."
  :group 'pel-pkg-for-d
  :type '(repeat function))

(defcustom pel-d-shebang-line "#!/usr/bin/env rdmd"
  "Default shebang line to add in extension-less D files."
  :group 'pel-pkg-for-d
  :type 'string)

;; -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -
(defgroup pel-d-code-style nil
  "D Source Code Style options."
  :group 'pel-pkg-for-d
  :link `(url-link :tag "D PDF" ,(pel-pdf-file-url "pl-d")))

(defcustom pel-d-bracket-style "bsd"
  "Set the bracket style for the D programming language.
PEL stores this value associated with the `d-mode' into the
`c-default-style' user option variable.
The BSD style (also called Allman style) is recommended by the
D community, see URL https://dlang.org/dstyle.html#phobos_brackets .
If you want to use something else, please select one of the
CC Mode Built-in Styles."
  :group 'pel-d-code-style
  :type 'string
  :safe 'pel-c-style-valid-p)

(defcustom pel-d-fill-column 80
  "Column beyond which automatic line-wrapping should happen in D code.
Can either be nil or an integer value.
When set to nil, Emacs user option variable `fill-column' value
is used for `d-mode' buffers, otherwise the integer value specified by
`pel-d-fill-column' is stored in the variable `fill-column' for
`d-mode' buffers.  The default is 80."
  :group 'pel-d-code-style
  :type '(choice
          (const   :tag "Use the default fill-column value." nil)
          (integer :tag "Use a value specific for d-mode buffers:")))

(defcustom  pel-d-newline-mode 'context-newline
  "Set default newline mode for d-mode buffers.

This may be one of the following values:

- context-newline : the default : the RET key is bound to
  the function `c-context-line-break' with the extra ability to
  repeat its execution with an argument.
- newline-and-indent: RET uses the function `newline' passing ARG
  and t for its arguments to insert newline and indent.
- just-newline-no-indent: RET uses the function
  `electric-indent-just-newline` with the specified argument (if
  any).  This only insert a newline; it does not indent."
  :group 'pel-d-code-style
  :type '(choice
          (const :tag "context-newline: use c-context-line-break.\n\
Does all what newline does plus more."
                 context-newline)
          (const :tag "newline-and-indent: use newline. This inserts\n\
a newline and then indent the new line."
                 newline-and-indent)
          (const :tag "just-newline-no-indent: use\
 electric-indent-just-newline.\n\
Does not indent."
                 just-newline-no-indent)))

(defcustom pel-d-indent-width 4
  "Number of columns for D source code indentation.
PEL stores this in `c-basic-offset' when editing buffers in `d-mode'.
The D community recommends using 4 spaces for indentation
therefore that's PEL's default.
Values in the [2, 8] range are accepted."
  :group 'pel-d-code-style
  :type 'integer
  :safe 'pel-indent-valid-p)

(defcustom pel-d-tab-width 4
  "Column width display rendering of hard tab for D source code.

PEL stores this in `tab-width' when editing buffer in `d-mode'.

This does *NOT* control the indentation in D source code.
It is used, however, to control the display rendering of hard tab
characters inserted inside source code and by commands that move
point to tab stop positions such as `tab-to-tab-stop', and the
display of hard TAB characters.

It is often the same value as `pel-d-indent-width', if it is
different it should probably be a multiple of
`pel-d-indent-width'."
  :group 'pel-d-code-style
  :type 'integer
  :safe 'pel-indent-valid-p)

(defcustom pel-d-use-tabs nil
  "Value of `indent-tabs-mode' for editing D source code.
- If set to nil: only spaces are used for indentation.
- If set to t: hard tabs are used when possible."
  :group 'pel-d-code-style
  :type 'boolean
  :safe #'booleanp)

;;-- Tools for D

(defcustom pel-use-d-ac-dcd nil
  "Control whether AutoComplete/DCD based code completion is used for D.

When set to t:
- the ac-dcd package is used for code completion,
  - it uses flycheck-dmd-dub package, which uses the D package
    registry called DUB to retrieve all D dependencies information.
    - which uses DCD (the D Completion Daemon) written in D
      which must be installed separately.

An alternative to AutoComplete/DCD is the Company/DCD, controlled
by the `pel-use-d-company-dcd'."
  :group 'pel-pkg-for-d
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-d-ac-dcd :requires 'pel-use-d)

(defcustom pel-use-d-company-dcd nil
  "Control whether Company/DCD based code completion is used for D.

When set to t:
- the `company-dcd' package is used for code completion,
  - it uses flycheck-dmd-dub package, which uses the D package
    registry called DUB to retrieve all D dependencies information.
    - which uses DCD (the D Completion Daemon) written in D
      which must be installed separately.

An alternative to AutoComplete/DCD is the Company/DCD, controlled
by the `pel-use-d-ac-dcd'."
  :group 'pel-pkg-for-d
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-d-company-dcd :package-is 'company-dcd)
(pel-put 'pel-use-d-company-dcd :requires 'pel-use-d)

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; Eiffel Programmaing Language Support
;; ------------------------------------

(defgroup pel-pkg-for-eiffel nil
  "PEL customization for Eiffel."
  :group 'pel-pkg-for-software-programming-languages
  :link `(url-link :tag "Eiffel PDF"
                   ,(pel-pdf-file-url "pl-eiffel")))

(defcustom pel-use-eiffel nil
  "Whether PEL activates Eiffel support.
When turned on,
- PEL automatically activate `pel-use-eiffel-mode'
- the `eiffel-mode' is associated with the PEL ``<f12>`` key."
  :group 'pel-pkg-for-eiffel
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-eiffel :package-is :a-gate)

(defcustom pel-use-eiffel-mode nil
  "Whether PEL activates eiffel-mode.
Note that Eiffel support is not fully working and needs to be
fixed.  I'm using my fork while I'm trying to make it work,but it
is a low priority item for me at the moment."
  :link '(url-link :tag "Eiffel mode @ Github"
                   "https://github.com/pierre-rouleau/eiffel-mode")
  :group 'pel-pkg-for-eiffel
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-eiffel-mode :also-required-when 'pel-use-eiffel)
(pel-put 'pel-use-eiffel :package-is '(quote ((utils . eiffel-mode))))

(defcustom pel-eiffel-activates-minor-modes nil
  "List of *local* minor-modes automatically activated for Eiffel buffers.
Enter *local* minor-mode activating function symbols.
Do not enter lambda expressions."
  :group 'pel-pkg-for-eiffel
  :type '(repeat function))

(defcustom pel-eiffel-indent-width 4
  "Number of columns for Eiffel source code indentation.
Values in the [2, 8] range are accepted."
  :group 'pel-pkf-for-eiffel
  :type 'integer
  :safe 'pel-indent-valid-p)

(defcustom pel-eiffel-tab-width 4
  "Column width display rendering of hard tab for buffers in `eiffel-mode'.

PEL stores this in `tab-width' when opening Eiffel buffers.

This does *NOT* control the indentation in Eiffel files.
It is used, however, to control the display rendering of hard tab
characters inserted inside source code and by commands that move
point to tab stop positions such as `tab-to-tab-stop', and the
display of hard TAB characters.

Values in the [2, 8] range are accepted."
  :group 'pel-pkg-for-eiffel
  :type 'integer
  :safe 'pel-indent-valid-p)

(defcustom pel-eiffel-use-tabs nil
  "Value of `indent-tabs-mode' for editing eiffel files.
- If set to nil: only spaces are used for indentation.
- If set to t: hard tabs are used when possible."
  :group 'pel-pkg-for-eiffel
  :type 'boolean
  :safe #'booleanp)

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; Java support
;; ------------
(defgroup pel-pkg-for-java nil
  "PEL support for Java."
  :group 'pel-pkg-for-software-programming-languages)

(defcustom pel-use-java nil
  "Control whether PEL enhancement for Java support are active."
  :group 'pel-pkg-for-java
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-java-activates-minor-modes nil
  "List of *local* minor-modes automatically activated for Java buffers.
Enter *local* minor-mode activating function symbols.
Do not enter lambda expressions."
  :group 'pel-pkg-for-java
  :type '(repeat function))

(defcustom pel-java-tab-width 4
  "Column width display rendering of hard tab for buffers in `java-mode'.

PEL stores this in `tab-width' when opening Java buffers.

This does *NOT* control the indentation in Java files.
It is used, however, to control the display rendering of hard tab
characters inserted inside source code and by commands that move
point to tab stop positions such as `tab-to-tab-stop', and the
display of hard TAB characters.

Values in the [2, 8] range are accepted."
  :group 'pel-pkg-for-java
  :type 'integer
  :safe 'pel-indent-valid-p)

(defcustom pel-java-use-tabs nil
  "Value of `indent-tabs-mode' for editing java files.
- If set to nil: only spaces are used for indentation.
- If set to t: hard tabs are used when possible."
  :group 'pel-pkg-for-java
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-use-lsp-java nil
  "Control whether PEL use lsp-java."
  :link '(url-link :tag "lsp-java @ Github"
                   "https://github.com/emacs-lsp/lsp-java")
  :group 'pel-pkg-for-java
  :type 'boolean
  :safe #'booleanp)

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; Javascript support
;; ------------------
(defgroup pel-pkg-for-javascript nil
  "PEL support for Javascript. Experimental."
  :group 'pel-pkg-for-software-programming-languages)


(defcustom pel-use-js nil
  "Control whether PEL supports Javascript development.

When set, identifies what mode is used to support Javascript."
  :group 'pel-pkg-for-javascript
  :type '(choice
          (const :tag "Emacs basic support using built-in \
js-mode and js-ts-mode." nil)
          (const :tag "Emacs basic + \
PEL additions using built-in js-mode and js-ts-mode." js-mode)
          (const :tag "Supported by the js2-mode external package." js2-mode)))
(pel-put 'pel-use-js :package-is '(when (eq pel-use-js 'js2-mode)
                                            '((elpa . js2-mode))))

(defcustom pel-js-activates-minor-modes nil
  "List of *local* minor-modes automatically activated for js-mode buffers.
Enter *local* minor-mode activating function symbols.
Do not enter lambda expressions."
  :group 'pel-pkg-for-javascript
  :type '(repeat function))

(defcustom pel-js-tab-width 4
  "Column width display rendering of hard tab for javascript buffers.

PEL stores this in `tab-width' when opening javascript  buffers.

This does *NOT* control the indentation in javascript
files, it only controls the column width display rendering of hard tabs
in buffers and tab stop positions for commands such as `tab-to-tab-stop'."
  :group 'pel-pkg-for-javascript
  :type 'integer
  :safe 'pel-indent-valid-p)

(defcustom pel-js-use-tabs nil
  "Value of `indent-tabs-mode' for editing javascript files.
- If set to nil: only spaces are used for indentation.
- If set to t: hard tabs are used when possible."
  :group 'pel-pkg-for-javascript
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-js2-activates-minor-modes nil
  "List of *local* minor-modes automatically activated for js2-mode buffers.
Enter *local* minor-mode activating function symbols.
Do not enter lambda expressions."
  :group 'pel-pkg-for-javascript
  :type '(repeat function))

(defcustom pel-js2-tab-width 4
  "Column width display rendering of hard tab for javascript buffers.

PEL stores this in `tab-width' when opening javascript  buffers.

This does *NOT* control the indentation in javascript
files, it only controls the column width display rendering of hard tabs
in buffers and tab stop positions for commands such as `tab-to-tab-stop'."
  :group 'pel-pkg-for-javascript
  :type 'integer
  :safe 'pel-indent-valid-p)

(defcustom pel-js2-use-tabs nil
  "Value of `indent-tabs-mode' for editing javascript files.
- If set to nil: only spaces are used for indentation.
- If set to t: hard tabs are used when possible."
  :group 'pel-pkg-for-javascript
  :type 'boolean
  :safe #'booleanp)

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; Go language support
;; -------------------

(defgroup pel-pkg-for-go nil
  "PEL customization for tools supporting the Go programming language."
  :group 'pel-pkg-for-software-programming-languages
  :link `(url-link :tag "Go PDF" ,(pel-pdf-file-url "pl-go"))  )

(defcustom pel-use-go nil
  "Controls whether PEL supports the Go programming language.
This *must* be activated to allow any other package for Go.
However it is automatically activated when `pel-use-go-mode' is activated.
When activating it you can select between the following values:
- t                : use `go-mode'
- with-tree-sitter : use `go-ts-mode'"
  :link '(url-link :tag "Go @ wikipedia"
                   "https://en.wikipedia.org/wiki/Go_(programming_language)")
  :group 'pel-pkg-for-go
  :type '(choice
          (const :tag "Do not use Go" nil)
          (const :tag "Use classic mode: go-mode" t)
          (const :tag "Use tree-sitter mode: go-ts-mode" with-tree-sitter)))
(pel-put 'pel-use-go :package-is :a-gate)
(pel-put 'pel-use-go :also-required-when 'pel-use-go-mode)

(defcustom pel-use-go-mode nil
  "Controls whether PEL use the gomode package."
  :link '(url-link :tag "gomode @ Github"
                   "https://github.com/dominikh/go-mode.el")
  :group 'pel-pkg-for-go
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-go-mode :requires 'pel-use-go)
(when pel-use-go-mode
  (unless pel-use-go
    (setq pel-use-go t)))

(defcustom pel-go-activates-minor-modes nil
  "List of *local* minor-modes automatically activated for Go buffers.
Enter *local* minor-mode activating function symbols.
Do not enter lambda expressions."
  :group 'pel-pkg-for-go
  :type '(repeat function))

(defcustom pel-go-tab-width 8
  "Hard-tab display width used for Go source code.

The Go programming language requires that Go source code file be
processed by the gofmt utility which re-formats the file
according to official Go style. That style requires that hard tab
be used for indentation. By default, a hard-tab is displayed with
a width of 8 columns. You can safely change it to any number
between 2 and 8. It changes the way the code looks inside an
Emacs Go buffer, but does not change the content of the file."
  :group 'pel-pkg-for-go
  :type 'integer
  :safe 'pel-indent-valid-p)

(defcustom pel-go-run-gofmt-on-buffer-save t
  "Control if gofmt is executed automatically when Go buffer saved.

Turn on (set to t) to have gofmt run automatically when the Go buffer
is saved. Turn off if you do not want it done.
Turned on by default.

Note that the command `pel-go-toggle-gofmt-on-buffer-save' changes the
setting dynamically allowing you to disable the execution of gofmt for a while
even when the user-option sets it on."
  :group 'pel-pkg-for-go
  :type 'boolean
  :safe #'booleanp)



(defcustom pel-use-goflymake nil
  "Controls whether PEL use the goflymake package.

The following choices are available:

- Not used (nil), the default.
- Use with flycheck.
- Use with flymake.

Note that flymake is built-in Emacs, flycheck is not.  flycheck
seems to be the engine preferred by many people.  If you select
flycheck, PEL will install it if it is not already installed.

When either is used you will also require the Go utility goflymake.
Use the following command line to install it:

    go get -u github.com/dougm/goflymake

By default PEL does not automatically activate the syntax checker
when a Go file is visited. If you want it activated
automatically, then you must add the symbol `go-mode' to the
defcustom variable `pel-modes-activating-syntax-check'."
  :link '(custom-group-link "pel-base-emacs")
  :link '(url-link :tag "goflymake @ Github"
                   "https://github.com/dougm/goflymake")
  :group 'pel-pkg-for-go
  :type '(choice
          (const :tag "Not used" nil)
          (const :tag "Use with flycheck" with-flycheck)
          (const :tag "Use with flymake"  with-flymake)))
(pel-put 'pel-use-goflymake :requires 'pel-use-go)
(pel-put 'pel-use-goflymake :package-is
         '(cond ((eq pel-use-goflymake 'with-flycheck)
                 '((utils . go-flycheck)
                   (elpa . flycheck)))
                ((eq pel-use-goflymake 'with-flymake)
                 ;; flymake is part of Emacs
                 '((utils . go-flymake)))))

(defcustom pel-use-gocode nil
  "Controls whether PEL use the gocode package."
  :link '(url-link :tag "gocode @ Github"
                   "https://github.com/mdempsky/gocode")
  :group 'pel-pkg-for-go
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-gocode :requires 'pel-use-go)

(defcustom pel-use-gopls nil
  "Controls whether PEL use the gopls package."
  :link '(url-link :tag "gopls @ Github"
                   "https://github.com/golang/tools/blob/master/gopls/doc/emacs.md")
  :link '(url-link :tag "Go Tools @ Github"
                   "https://github.com/golang/tools")
  :group 'pel-pkg-for-go
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-gopls :requires 'pel-use-go)

(defcustom pel-use-go-errcheck nil
  "Controls whether PEL use the go-errcheck package."
  :link '(url-link :tag "go-errcheck @ Github"
                   "https://github.com/dominikh/go-errcheck.el")
  :link '(url-link :tag "Go errcheck @ GitHub"
                   "https://github.com/kisielk/errcheck")
  :group 'pel-pkg-for-go
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-go-errcheck :requires 'pel-use-go)

(defcustom pel-use-go-playground nil
  "Controls whether PEL use the go-playground package."
  :link '(url-link :tag "go-playground @ Github"
                   "https://github.com/grafov/go-playground")
  :group 'pel-pkg-for-go
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-go-playground :requires 'pel-use-go)

(defcustom pel-use-gorepl-mode nil
  "Controls whether PEL use the gorepl-mode package.

Requires gocode."
  :link '(url-link :tag "gorepl-mode @ Github"
                   "https://github.com/manute/gorepl-mode")
  :link '(url-link :tag "gore @ GitHub"
                   "https://github.com/motemen/gore")
  :group 'pel-pkg-for-go
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-gorepl-mode :requires 'pel-use-go)

(defcustom pel-use-gotest nil
  "Controls whether PEL use the gotest package."
  :link '(url-link :tag "gotest @ Github"
                   "https://github.com/nlamirault/gotest.el")
  :group 'pel-pkg-for-go
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-gotest :requires 'pel-use-go)

(defcustom pel-use-emacs-go-tag nil
  "Controls whether PEL use the emacs-go-tag package."
  :link '(url-link :tag "emacs-go-tag @ Github"
                   "https://github.com/brantou/emacs-go-tag")
  :link '(url-link :tag "gomodifytags @ GitHub"
                   "https://github.com/fatih/gomodifytags")
  :group 'pel-pkg-for-go
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-emacs-go-tag :requires 'pel-use-go)

(defcustom pel-use-flycheck-golangci-lint nil
  "Controls whether PEL use the flycheck-golangci-lint package."
  :link '(url-link :tag "flycheck-golangci-lint @ Github"
                   "https://github.com/weijiangan/flycheck-golangci-lint")
  :group 'pel-pkg-for-go
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-flycheck-golangci-lint :requires 'pel-use-go)

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; Haskell Support
;; ---------------
(defgroup pel-pkg-for-haskell nil
  "PEL support for the Haskell programming language."
  :group 'pel-pkg-for-software-programming-languages
  :link `(url-link :tag "Haskell PDF" ,(pel-pdf-file-url "pl-haskell")))

(defcustom pel-use-haskell nil
  "Control whether PEL supports the Haskell programming language.

When turned on the haskell-mode is associated with the PEL ``<f12>`` key."
  :group 'pel-pkg-for-haskell
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-haskell :package-is :a-gate)

(defcustom pel-use-haskell-mode nil
  "Control whether PEL activates the haskell-mode external package."
  :group 'pel-pkg-for-haskell
  :link '(url-link :tag "haskell-mode @ GitHub"
                   "https://github.com/haskell/haskell-mode")
  :link '(url-link :tag "haskell mode manual"
                   "https://haskell.github.io/haskell-mode/manual/latest/")
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-haskell-mode :requires 'pel-use-haskell)

(defcustom pel-haskell-activates-minor-modes nil
  "List of *local* minor-modes automatically activated for Haskell buffers.
Enter *local* minor-mode activating function symbols.
Do not enter lambda expressions."
  :group 'pel-pkg-for-haskell
  :type '(repeat function))

(defcustom pel-haskell-tab-width 2
  "Column width display rendering of hard tab for haskell buffers.

PEL stores this in `tab-width' when opening haskell  buffers.

This does *NOT* control the indentation in haskell
files, it only controls the column width display rendering of hard tabs
in buffers and tab stop positions for commands such as `tab-to-tab-stop'."
  :group 'pel-pkg-for-haskell
  :type 'integer
  :safe 'pel-indent-valid-p)

(defcustom pel-haskell-use-tabs nil
  "Value of `indent-tabs-mode' for editing haskell files.
- If set to nil: only spaces are used for indentation.
- If set to t: hard tabs are used when possible."
  :group 'pel-pkg-for-haskell
  :type 'boolean
  :safe #'booleanp)

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; Lisp-like language support
;; --------------------------
;;
;; The group pel-pkg-for-lisp has settings for tools that can be used for
;; several Lisp-like programming languages like Emacs-Lisp. Common-Lisp,
;; Clojure, Scheme, LFE, etc...

(defgroup pel-pkg-for-lisp nil
  "PEL customization for tools supporting LISP-like programming languages."
  :group 'pel-pkg-for-software-programming-languages)

(defcustom pel-use-lispy nil
  "Control whether PEL uses the lispy package.

Note that lispy has commands that use find-file-in-project
or projectile.  PEL supports these package with, respectively:
- User-option variable `pel-use-find-file-in-project'
- User-option variable `pel-use-projectile'

I strongly recommend using projectile.  ffip takes an enormous
amount of time to search for a project (even with fd)."
  :type 'boolean
  :safe #'booleanp
  :link `(url-link :tag "Lispy PDF" ,(pel-pdf-file-url "plm-lispy"))
  :link '(url-link :tag "abo-abo lispy" "https://github.com/abo-abo/lispy")
  :link '(url-link :tag "enzuru fork; temporary maintenance"
                   "https://github.com/enzuru/lispy"))

(defconst pel-allowed-modes-for-lispy
  '(emacs-lisp-mode                     ; Emacs Lisp  - Lisp 2 for Emacs
    ielm-mode                           ;  repl := inferior-emacs-lisp-mode
    lisp-mode                           ; Common Lisp - Lisp 2
    inferior-lisp-mode                  ;  repl
    slime-repl-mode                     ;  repl
    sly-mrepl-mode                      ;  repl
    lfe-mode                            ; LFE         - Lisp 2 for BEAM
    ;; inferior-lfe-mode does not work well with lispy -
    ;; See: https://github.com/abo-abo/lispy/issues/592
    clojure-mode                        ; Clojure     - Lisp 1 for the JVM
    scheme-mode                         ; Scheme      - Lisp 1
    racket-mode                         ; Racket      - Lisp 1, Scheme family
    gerbil-mode                         ; Gerbil      - Lisp 1, Scheme family
    arc-mode                            ; Arc         - Lisp 1, experimental
    fennel-mode                         ; Fennel      - A lisp-like to Lua 🚧
    hy-mode)                            ; Hy          - Lisp 1 for Python

  "List of major modes that can use lispy.")
;; TODO: Add the various REPL for the all the Lisp languages above: several
;;       are missing.
;; If the mode you want is not listed above, please let me know: I will
;; add it or will accept PRs proposing it.  Thanks.

(defcustom pel-modes-activating-lispy  nil
  "List of major modes that automatically activate `lispy-mode'.

The lispy mode is a powerful editing mode for Lisp programming languages.
You can enable it for the major modes listed in the user-option
variable `pel-allowed-modes-for-lispy'.

PEL will ignore other modes."
  :group 'pel-pkg-for-lisp
  :type '(repeat symbol))

(defcustom pel-enable-lispy-meta-return nil
  "Enable the lispy-meta-return binding to M-RET when on.

It is off by default, since it conflicts with PEL global binding to the M-RET
key."
  :group 'pel-pkg-for-lisp
  :link '(url-link :tag "Lispy meta-return @ Lispy manual"
                   "http://oremacs.com/lispy/#lispy-shifttab")
  :type 'boolean
  :safe #'booleanp)

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; Common Lisp Support
;; -------------------
(defgroup pel-pkg-for-clisp nil
  "PEL customization for Common Lisp."
  :group 'pel-pkg-for-lisp
  :link `(url-link :tag "Common Lisp PDF"
                   ,(pel-pdf-file-url "pl-common-lisp")))

(defcustom pel-use-common-lisp nil
  "Control whether PEL supports Common Lisp development.

When turning this on, you will probably also want to activate one
of the following minor modes that will help with Common Lisp
editing:

- Lispy, by setting `pel-use-lispy'
- One of:
  - Slime, by setting `pel-use-slime', or
  - Sly, by setting `pel-use-sly'.

You will also want to use a Common Lisp REPL and want to use it
inside Emacs. For that you can identify the executable inside
`pel-inferior-lisp-program'."
  :group 'pel-pkg-for-clisp
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-inferior-lisp-program nil
  "Name (with optional path) of the Common Lisp REPL to use.

PEL will copy this inside the variable `inferior-lisp-program',
which defaults to \"lisp\".

Another way would be to create an executable shell script called \\='lisp\\='
that executes the a Common Lisp REPL selected by some external criteria or use
a symlink to the one you want and ensure that is on your path.
In any case, you can override it by setting the name here."
  :group 'pel-pkg-for-clisp
  :type 'string)

(defcustom pel-lisp-activates-minor-modes nil
  "List of *local* minor-modes automatically activated for Common Lisp buffers.
Enter *local* minor-mode activating function symbols.
Do not enter lambda expressions."
  :group 'pel-pkg-for-clisp
  :type '(repeat function))

(defcustom pel-clisp-extra-files nil
  "Optional files and file extensions for Common Lisp.
Add regexp describing Common Lisp files or file extensions that are not
already identified by Emacs or PEL as Common Lisp files."
  :group 'pel-pkg-for-clisp
  :type '(repeat string))

(defcustom pel-clisp-define-forms
  '(("Classes"              lisp-mode-symbol-regexp ("define-class"))
    ("Commands"             lisp-mode-symbol-regexp ("define-command"))
    ("FFI-Methods"          lisp-mode-symbol-regexp ("define-ffi-method"))
    ("Functions"            lisp-mode-symbol-regexp ("define-function"))
    ("Modes"                lisp-mode-symbol-regexp ("define-mode"))
    ("Parenscripts"         lisp-mode-symbol-regexp ("define-parenscript"))
    ("User-Classes"         lisp-mode-symbol-regexp ("define-user-class"))
    ("ASDF System Define"   ("\\\"\\(\\(?:\\sw\\|\\s_\\)+\\)\\\"" 3)  ("defsystem")))
  "A list of lists identifying Common Lisp define forms for imenu parsing.

Each list consist of:
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

These entries will be added to the `lisp-imenu-generic-expression' used by
the `imenu' function to parse Common Lisp source and extract indices."
  :type '(repeat
          (list
           (string :tag "Title"    :value "")
           (choice
            (const :tag "lisp-symbol" lisp-mode-symbol-regexp)
            (list
             (string :tag "symbol extraction regexp")
             (integer :tag "extraction group" :value 2)))
           (repeat
            (string :tag "Function" :value "")))))

(defcustom pel-clisp-hyperspec-root
  "http://www.lispworks.com/documentation/HyperSpec/"
  "Location of the Common Lisp HyperSpec HTML documentation, a URL.

The URL must identify the location of the HyperSpec root directory
and must end with a / character.  It may identify a remote directory access
through HTTP or HTTPS as in the default value, or a local directory with
a \"file://\" prefix.  The path must be absolute. The special '~' character
may be used.

The default is: http://www.lispworks.com/documentation/HyperSpec/

See the Common Lisp Hyperspec link if you want to download a copy of the
latest version (currently version 7) of the LispWorks HyperSpec files to
install them locally."
  :link '(url-link :tag "Common Lisp Hyperspec"
                   "http://www.lispworks.com/documentation/common-lisp.html")
  :group 'pel-pkg-for-clisp
  :type 'string)

(defcustom pel-clisp-ide nil
  "Control what Common Lisp IDE is used, if any.
The following IDE are supported:  Slime and SLY."
  :group 'pel-pkg-for-clisp
  :type '(choice
          (const :tag "No IDE" nil)
          (const :tag "Use Slime - `pel-use-slime' must be set" slime)
          (const :tag "Use SLY   - `pel-use-sly' must be set"   sly)))

(defcustom pel-use-slime nil
  "Control whether PEL activates SLIME for Common Lisp.

The value can be:
- 0: not used
- 1: Use - no extra contrib
- 2: Use with extra contrib.

Select 2 to specify extra slime features to activate, as symbols
that have a name that starts with \\='slime-\\=' and corresponds to
slime extra contributions, like: slime-fancy, slime-quicklisp and
slime-asdf.  By default, slime activates only slime-fancy
contribution.

Also note that to activate Slime you must also set:
- `pel-clisp-ide' to slime.
- `pel-use-common-lisp' to t."
  :link '(url-link :tag "Slime @ Wikipedia"
                   "https://en.wikipedia.org/wiki/SLIME")
  :link '(url-link :tag "Slime home page"
                   "https://common-lisp.net/project/slime/")
  :link '(url-link :tag "Slime @ GitHub"
                   "https://github.com/slime/slime")
  :group 'pel-pkg-for-clisp
  :type '(choice
          (const :tag "Not used" nil)
          (const :tag "Use - no extra contribs" t)
          (repeat :tag "Use with extra contribs"
                  (symbol :tag "extra contrib"))))
(pel-put 'pel-use-slime :requires 'pel-use-common-lisp)

(defcustom pel-use-sly nil
  "Control whether PEL activates SLY for Common Lisp.

Also note that to activate SLY you must also set:
- `pel-clisp-ide' to sly.
- `pel-use-common-lisp' to t."
  :link '(url-link :tag "Sly @ GitHub"
                   "https://github.com/joaotavora/sly")
  :group 'pel-pkg-for-clisp
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-sly :requires 'pel-use-common-lisp)

;; -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -
(defgroup pel-clisp-code-style nil
  "Common Lisp Code Style options."
  :group 'pel-pkg-for-clisp)

(defcustom pel-clisp-fill-column 100
  "Maximum length of Common Lisp source code line.
Column beyond which automatic line-wrapping should occur for
Common Lisp code.
Can either be nil or an integer value.
When set to nil, Emacs user option variable `fill-column' value
is used for `lisp-mode' buffers, otherwise the integer value specified by
`pel-clisp-fill-column' is stored in the variable `fill-column' for
`lisp-mode' buffers.  The default is 100."
  :group 'pel-clisp-code-style
  :type '(choice
          (const   :tag "Use the default fill-column value." nil)
          (integer :tag "Use a value specific for lisp-mode buffers:")))

;; style - 0
(defcustom pel-clisp-skel-use-separators t
  "Specifies whether Common Lisp code block include separators line.
If nil no separator line comment is used, otherwise separator line
comments of length controlled by variable `fill-column' are inserted."
  :group 'pel-clisp-code-style
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-clisp-skel-use-separators :choices '(nil t))

;; style - 1
(defcustom pel-clisp-skel-insert-file-timestamp nil
  "Set whether a timestamp is inserted inside Common Lisp file header block."
  :group 'pel-clisp-code-style
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-clisp-skel-insert-file-timestamp :choices '(nil t))

;; style - 2
(defcustom pel-clisp-skel-with-license nil
  "Control whether a license text is inserted in Common Lisp file header.

The available choices are:

- 0: No copyright, no license.                 nil
- 1: Copyright only, no license.               \\='only-copyright
- 2: Copyright, with selected license name.    a string
- 3: Copyright, with complete license text.    t

For choice 3, you specify the name of the license in the string
field.  Something like MIT or GPL-3.0, or anything appropriate.
That string will be entered verbatim inside a file header line.

For choice 4, the type of the license and its text are selected
the function `lice' taken from the external library with the same name.

The text of the inserted license is selected by the
`lice:default-license' user option, normally configured inside
the directory\\='s .dir-locals.el file written inside the global
setting like this:

   ((nil . ((fill-column . 80)
            (lice:default-license  . \"gpl-3.0\")
            (lice:copyright-holder . \"Your Name\")))

Replace \"gpl-3.0\" with the license you want and write your name
inside the copyright holder value.

When the user-option is t PEL activates the `pel-use-lice'
user-option if it is not activated already."
  :group 'pel-clisp-code-style
  :type
  '(choice
    (const :tag  "No license, no copyright." nil)
    (const :tag  "Copyright only." only-copyright)
    (string :tag "Copyright with specified license name.")
    (const :tag
           "Copyright with license text selected by `lice:default-license'"
           t)))
(pel-put 'pel-clisp-skel-with-license :choices '(nil t only-copyright "MIT"))

;; style - 3
(defcustom pel-clisp-skel-package-name 'extract-from-file-name
  "Specifies whether a package name ownership note is inserted.
If you want to insert one, it can either be extracted from the file name (in
this case it's the first word of the file name, fully up-cased) or can be
specified as a string."
  :group 'pel-clisp-code-style
  :type '(choice
          (const :tag "No, don't add package ownership note." nil)
          (const :tag "Add package ownership note extracted from file name."
                 extract-from-file-name)
          (string :tag "Use this specified string.")))
(pel-put 'pel-clisp-skel-package-name :choices '(nil extract-from-file-name "foo"))

;; style -4
(defcustom pel-clisp-emacs-filevar-line nil
  "Identifies the Emacs File Variable setting string for top of files.
If this string is specified, it is placed on the very first line of
Common Lisp source code files when the PEL skeleton is used.
The string is placed between the two -*- tags."
  :group 'pel-clisp-code-style
  :type '(choice
          (const :tag "No file variable file." nil)
          (string :tag "Use specified string.")))
(pel-put 'pel-clisp-emacs-filevar-line
         :choices
         '(nil "Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10"))

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; Emacs Lisp Support
;; ------------------
(defgroup pel-pkg-for-elisp nil
  "PEL customization for Emacs Lisp."
  :group 'pel-pkg-for-lisp
  :link `(url-link :tag "Emacs Lisp PDF" ,(pel-pdf-file-url "pl-emacs-lisp")))

(defcustom pel-elisp-activates-minor-modes nil
  "List of *local* minor-modes automatically activated for Emacs Lisp buffers.
  Enter *local* minor-mode activating function symbols.
  Do not enter lambda expressions."
  :group 'pel-pkg-for-elisp
  :type '(repeat function))

(defcustom pel-use-macrostep nil
  "Control whether PEL uses the macrostep package."
  :group 'pel-pkg-for-elisp
  :type 'boolean
  :safe #'booleanp
  :link '(url-link :tag "macrostep @ GitHub"
                   "https://github.com/joddie/macrostep")
  :link '(custom-group-link "macrostep"))

(defcustom pel-use-esup nil
  "Control whether PEL uses the esup package.

  ESUP - Emacs Start Up Profiler, a tool to profile
  execution.  Profiles the init.el by default.
  Only works when Emacs runs in Graphics mode.
  PEL therefore only activates it when Emacs runs
  in Graphics mode."
  :link '(url-link :tag "esup @ GitHub"
                   "https://github.com/jschaf/esup")
  :group 'pel-pkg-for-elisp
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-esup :restricted-to 'pel-emacs-is-graphic-p)

(defcustom pel-use-highlight-defined nil
  "Control whether PEL uses the {highlight-defined} package."
  :group 'pel-pkg-for-elisp
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-use-eros nil
  "Control whether PEL uses the eros package.

  eros ≡ Evaluation Result OverlayS.

  With eros mode enabled, \\[eval-last-sexp] displays the result
  as an overlay instead of showing it inside the minibuffer."
  :link '(url-link :tag "eros @ GitHub"
                   "https://github.com/xiongtx/eros")
  :group 'pel-pkg-for-elisp
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-use-suggest nil
  "Control whether PEL uses the suggest package.

  With it you can open a suggest buffer where you identify inputs
  and wanted output of an hypothetical function and request suggestions
  for functions that provide the requested functionality.
  A great tool for learning new Emacs Lisp functions."
  :link '(url-link :tag "suggest @ GitHub"
                   "https://github.com/Wilfred/suggest.el")
  :group 'pel-pkg-for-elisp
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-use-elisp-refs nil
  "Control whether PEL installs and activates elisp-refs external package.

This user-option is also set to t  when the `pel-use-helpful' user-option
is set: it is used by the helpful package."
  :link '(url-link :tag "elisp-refs @ GitHub"
                   "https://github.com/Wilfred/elisp-refs")
  :group 'pel-pkg-for-elisp
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-elisp-refs :also-required-when 'pel-use-helpful)

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; Arc Support
;; -----------
(defgroup pel-pkg-for-arc nil
  "PEL customization for the Arc programming language support."
  :group 'pel-pkg-for-lisp
  :link `(url-link :tag "Arc PDF" ,(pel-pdf-file-url "pl-arc")))

(defcustom pel-arc-activates-minor-modes nil
  "List of *local* minor-modes automatically activated for Arc buffers.
  Enter *local* minor-mode activating function symbols.
  Do not enter lambda expressions."
  :group 'pel-pkg-for-arc
  :type '(repeat function))

(defcustom pel-use-arc nil
  "Control whether PEL supports the Arc programming language.

  When this is activated, PEL activates the packages that support Arc and
  provide arc-mode and Arc inferior mode."
  :group 'pel-pkg-for-arc
  :link '(url-link :tag "Arc anarki @ GitHub"
                   "https://github.com/arclanguage/anarki")
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-arc :package-is '(quote ((utils . arc)
                                           (utils . inferior-arc))))
;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; Clojure Support
;; ---------------
(defgroup pel-pkg-for-clojure nil
  "PEL customization for the Clojure programming language support."
  :group 'pel-pkg-for-lisp
  :link `(url-link :tag "Clojure PDF" ,(pel-pdf-file-url "pl-clojure")))

(defcustom pel-clojure-activates-minor-modes nil
  "List of *local* minor-modes automatically activated for Clojure buffers.
  Enter *local* minor-mode activating function symbols.
  Do not enter lambda expressions."
  :group 'pel-pkg-for-clojure
  :type '(repeat function))

(defcustom pel-use-clojure nil
  "Control whether PEL supports the Clojure programming language."
  :group 'pel-pkg-for-clojure
  :link '(url-link :tag "Clojure Mode @ GitHub"
                   "https://github.com/clojure-emacs/clojure-mode")
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-clojure :package-is 'clojure-mode)

(defcustom pel-use-cider nil
  "Control whether PEL activates the Cider Clojure IDE package.

  To activate it you must activate the user-option variable `pel-use-clojure'."
  :group 'pel-pkg-for-clojure
  :link '(url-link :tag "Cider @ GitHub"
                   "https://github.com/clojure-emacs/cider")
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-cider :requires 'pel-use-clojure)

(defcustom pel-use-clj-refactor nil
  "Control whether PEL activates the clj-refactor package.

  To activate it you must activate the user-option variable `pel-use-clojure'."
  :group 'pel-pkg-for-clojure
  :link '(url-link :tag "clj-refactor @ GitHub"
                   "https://github.com/clojure-emacs/clj-refactor.el")
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-clj-refactor :requires 'pel-use-clojure)

(defcustom pel-use-clojure-snippets nil
  "Control whether PEL activates clojure-snippets package.

  This package provides Yasnippet snippets for Clojure.
  To use it you must also turn on:
  - the variable `pel-use-clojure',
  - the variable `pel-use-yasnippet'."
  :group 'pel-pkg-for-clojure
  :link '(url-link :tag "clojure-snippets @ GitHub"
                   "https://github.com/mpenet/clojure-snippets")
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-clojure-snippets :requires 'pel-use-clojure)

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; Janet Support
;; -------------

(defgroup pel-pkg-for-janet nil
  "PEL customization for the Janet programming language support.

Janet is a Lisp-like language implemented in C, embeddable,
relatively small footprint with several interesting features."
  :group 'pel-pkg-for-lisp
  :link `(url-link :tag "Janet PDF" ,(pel-pdf-file-url "pl-janet")))

(defcustom pel-use-janet nil
  "Control whether PEL provides support for the Janet programming language."
  :group 'pel-pkg-for-janet
  :link '(url-link :tag "Janet homepage" "https://janet-lang.org")
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-janet :package-is :a-gate)

(defcustom pel-use-janet-mode nil
  "Control whether PEL uses janet-mode external package.

Provides access to the major mode `janet-mode'.
Activating this automatically turns `pel-use-janet' on."
  :group 'pel-pkg-for-janet
    :link '(url-link :tag "My janet-mode @ GitHub"
                   "https://github.com/pierre-rouleau/janet-mode")
  :link '(url-link :tag "janet-mode @ GitHub"
                   "https://github.com/ALSchwalm/janet-mode")
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-janet-mode :package-is :in-utils)
(pel-put 'pel-use-janet-mode :requires 'pel-use-janet)


(defcustom pel-use-ijanet-mode nil
  "Control whether PEL uses ijanet-mode external package.

Activating this automatically turns `pel-use-janet' on."
  :group 'pel-pkg-for-janet
  :link '(url-link :tag "ijanet-mode @Github"
                   "https://github.com/SerialDev/ijanet-mode")
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-ijanet-mode :package-is '(quote ((utils . ijanet))))
(pel-put 'pel-use-ijanet-mode :requires 'pel-use-janet)

(defcustom pel-use-inf-janet nil
  "Control whether PEL uses ijanet-mode external package.

Activating this automatically turns `pel-use-janet' on."
  :group 'pel-pkg-for-janet
  :link '(url-link :tag "inf-janet @Github"
                   "https://github.com/velkyel/inf-janet")
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-inf-janet :package-is :in-utils)
(pel-put 'pel-use-inf-janet :requires 'pel-use-janet)

(defcustom pel-janet-activates-minor-modes nil
  "List of *local* minor-modes automatically activated for Janet buffers.
  Enter *local* minor-mode activating function symbols.
  Do not enter lambda expressions."
  :group 'pel-pkg-for-janet
  :type '(repeat function))

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; Hy Support
;; -----------
(defgroup pel-pkg-for-hy nil
  "PEL customization for the Hy programming language support.
  Hy is a Lisp in Python."
  :group 'pel-pkg-for-lisp
  :link `(url-link :tag "HyPDF" ,(pel-pdf-file-url "pl-hy")))

(defcustom pel-hy-activates-minor-modes nil
  "List of *local* minor-modes automatically activated for Hy  buffers.
  Enter *local* minor-mode activating function symbols.
  Do not enter lambda expressions."
  :group 'pel-pkg-for-hy
  :type '(repeat function))

(defcustom pel-use-hy nil
  "Control whether PEL supports the Hy programming language.
  Hy is a Lisp in Python."
  :group 'pel-pkg-for-hy
  :link '(url-link :tag "Hy Homepage"
                   "https://docs.hylang.org/en/stable/")
  :link '(url-link :tag "hy-mode @ GitHub"
                   "https://github.com/hylang/hy-mode")
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-hy :package-is 'hy-mode)

(defcustom pel-hy-tab-width 4
  "Column width display rendering of hard tab for hy buffers.

PEL stores this in `tab-width' when opening hy  buffers.

This does *NOT* control the indentation in hy
files, it only controls the column width display rendering of hard tabs
in buffers and tab stop positions for commands such as `tab-to-tab-stop'."
  :group 'pel-pkg-for-hy
  :type 'integer
  :safe 'pel-indent-valid-p)

(defcustom pel-hy-use-tabs nil
  "Value of `indent-tabs-mode' for editing hy files.
- If set to nil: only spaces are used for indentation.
- If set to t: hard tabs are used when possible."
  :group 'pel-pkg-for-hy
  :type 'boolean
  :safe #'booleanp)

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; Scheme Support
;; --------------
(defgroup pel-pkg-for-scheme nil
  "PEL customization for the Scheme programming languages support."
  :link `(url-link :tag "Scheme PDF" ,(pel-pdf-file-url "pl-scheme"))
  :group 'pel-pkg-for-lisp)

(defcustom pel-scheme-activates-minor-modes nil
  "List of *local* minor-modes automatically activated for Scheme buffers.
  Enter *local* minor-mode activating function symbols.
  Do not enter lambda expressions."
  :group 'pel-pkg-for-scheme
  :type '(repeat function))

(defcustom pel-use-scheme nil
  "Control whether PEL supports the Scheme programming language."
  :group 'pel-pkg-for-scheme
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-scheme :package-is :a-gate)

(defcustom pel-use-geiser nil
  "Control whether PEL supports the Geiser IDE for Scheme support.

  The user-option variable `pel-use-scheme' must be turned on to activate this."
  :link '(url-link :tag "Geiser Homepage"
                   "https://www.nongnu.org/geiser/")
  :link '(url-link :tag "Geiser @ Melpa"
                   "https://melpa.org/#/geiser")
  :link '(url-link :tag "Geiser source @ Gitlab"
                   "https://gitlab.com/jaor/geiser")
  :group 'pel-pkg-for-scheme
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-geiser :requires 'pel-use-scheme)
(pel-put 'pel-use-geiser :also-required-when '(and pel-use-scheme
                                                   (or pel-use-macrostep-geiser
                                                       pel-use-ac-geiser
                                                       pel-use-geiser-chez
                                                       pel-use-geiser-chibi
                                                       pel-use-geiser-chicken
                                                       pel-use-geiser-gambit
                                                       pel-use-geiser-guile
                                                       pel-use-geiser-mit
                                                       pel-use-geiser-racket)))

(defcustom pel-use-macrostep-geiser nil
  "Control whether PEL activates the macrostep-geiser external package.
Provides in-buffer macro expansion, using the macrostep package."
  :link '(url-link :tag "macrostep-geiser @ GitHub"
                   "https://github.com/nbfalcon/macrostep-geiser")
  :group 'pel-pkg-for-scheme
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-macrostep-geiser :package-is :in-utils)

(defcustom pel-use-ac-geiser nil
  "Control whether PEL activates the ac-geiser external package.
Provides auto-completion for Geiser using the auto-complete-mode. "
  :link '(url-link :tag "ac-geiser @ GitHub"
                   "https://github.com/xiaohanyu/ac-geiser/")
  :group 'pel-pkg-for-scheme
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-use-quack nil
  "Control whether PEL supports the Quack Enhance Scheme editing package.

  The user-option variable `pel-use-scheme' must be turned on to activate this."
  :group 'pel-pkg-for-scheme
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-quack :requires 'pel-use-scheme)
(pel-put 'pel-use-quack :package-is :in-utils)

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; Chez Scheme Support
;; ---------------------
(defgroup pel-pkg-for-chez nil
  "PEL customization for the Chez Scheme programming language support."
  :link `(url-link :tag "Chez PDF" ,(pel-pdf-file-url "pl-chez-scheme"))
  :group 'pel-pkg-for-scheme)

(defcustom pel-chez-activates-minor-modes nil
  "List of *local* minor-modes automatically activated for Chez buffers.
  Enter *local* minor-mode activating function symbols.
  Do not enter lambda expressions."
  :group 'pel-pkg-for-chez
  :type '(repeat function))

(defcustom pel-use-chez nil
  "Control whether PEL supports the Chez Scheme-based programming language.
Note that activating Chez also activates Scheme support."
  :group 'pel-pkg-for-chez
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-chez :package-is :a-gate)

(defcustom pel-use-geiser-chez nil
  "Control whether PEL activates the geiser-chez external package."
  :link '(url-link :tag "geiser-chez @ GitLab"
                   "https://gitlab.com/emacs-geiser/chez")
  :group 'pel-pkg-for-chez
  :type 'boolean
  :safe #'booleanp)

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; Chibi Scheme Support
;; --------------------
(defgroup pel-pkg-for-chibi nil
  "PEL customization for the Chibi Scheme programming language support."
  :link `(url-link :tag "Chibi PDF" ,(pel-pdf-file-url "pl-chibi-scheme"))
  :group 'pel-pkg-for-scheme)

(defcustom pel-chibi-activates-minor-modes nil
  "List of *local* minor-modes automatically activated for Chibi buffers.
  Enter *local* minor-mode activating function symbols.
  Do not enter lambda expressions."
  :group 'pel-pkg-for-chibi
  :type '(repeat function))

(defcustom pel-use-chibi nil
  "Control whether PEL supports the Chibi Scheme-based programming language.
Note that activating Chibi also activates Scheme support."
  :group 'pel-pkg-for-chibi
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-chibi :package-is :a-gate)

(defcustom pel-use-geiser-chibi nil
  "Control whether PEL activates the geiser-chibi external package."
  :link '(url-link :tag "geiser-chibi @ GitLab"
                   "https://gitlab.com/emacs-geiser/chibi")
  :group 'pel-pkg-for-chibi
  :type 'boolean
  :safe #'booleanp)

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; Chicken Scheme Support
;; ----------------------
(defgroup pel-pkg-for-chicken nil
  "PEL customization for the Chicken Scheme programming language support."
  :link `(url-link :tag "Chicken PDF" ,(pel-pdf-file-url "pl-chicken-scheme"))
  :group 'pel-pkg-for-scheme)

(defcustom pel-chicken-activates-minor-modes nil
  "List of *local* minor-modes automatically activated for Chicken buffers.
  Enter *local* minor-mode activating function symbols.
  Do not enter lambda expressions."
  :group 'pel-pkg-for-chicken
  :type '(repeat function))

(defcustom pel-use-chicken nil
  "Control whether PEL supports the Chicken Scheme-based programming language.
Note that activating Chicken also activates Scheme support."
  :group 'pel-pkg-for-chicken
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-chicken :package-is :a-gate)

(defcustom pel-use-geiser-chicken nil
  "Control whether PEL activates the geiser-chicken external package."
  :link '(url-link :tag "geiser-chicken @ GitLab"
                   "https://gitlab.com/emacs-geiser/chicken")
  :group 'pel-pkg-for-chicken
  :type 'boolean
  :safe #'booleanp)

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; Gambit Scheme Support
;; ---------------------
(defgroup pel-pkg-for-gambit nil
  "PEL customization for Gambit Scheme programming language support.
Gambit is a Scheme implementation with its own tools."
  :link `(url-link :tag "Gambit PDF" ,(pel-pdf-file-url "pl-gambit-scheme"))
  :link '(url-link :tag "Gambit @ GitHub"
                   "https://github.com/gambit/gambit")
  :group 'pel-pkg-for-scheme)

(defcustom pel-gambit-activates-minor-modes nil
  "List of *local* minor-modes automatically activated for Gambit buffers.
  Enter *local* minor-mode activating function symbols.
  Do not enter lambda expressions."
  :group 'pel-pkg-for-gambit
  :type '(repeat function))

(defcustom pel-use-gambit nil
  "Control whether PEL provides supports Gambit Scheme and Emacs gambit-mode.
Downloads gambit.el from the GitHub repo instead of relying on a
local installation of Gambit.  This allows PEL to manage Gambit
like its other packages."
  :group 'pel-pkg-for-gambit
  :link '(url-link :tag "gambit.el @ Github"
                   "https://github.com/gambit/gambit/blob/master/misc/gambit.el")
  :link '(url-link :tag "pierre-rouleau gambit.el fork @ Github"
                    "https://github.com/pierre-rouleau/gambit/blob/master/misc/gambit.el")
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-gambit :package-is '(quote ((utils . gambit))))
(pel-put 'pel-use-gambit :also-required-when 'pel-use-gerbil)

(defcustom pel-gambit-repl "gsi"
  "File or file path name of Gambit REPL executable.
If the program is not in your PATH but located on your system
then its best to identify the full absolute path to the executable.
Use UNIX-style forward slash, even on Windows.
The default specifies the gxi program without a path.
If the program is available in your PATH that is good enough."
  :group 'pel-pkg-for-gambit
  :type 'string)

(defcustom pel-use-geiser-gambit nil
  "Control whether PEL activates the geiser-gambit external package."
  :link '(url-link :tag "geiser-gambit @ GitLab"
                   "https://gitlab.com/emacs-geiser/gambit")
  :group 'pel-pkg-for-gambit
  :type 'boolean
  :safe #'booleanp)

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; Gerbil Scheme Support
;; ---------------------
(defgroup pel-pkg-for-gerbil nil
  "PEL customization for the Gerbil Scheme programming language support."
  :link `(url-link :tag "Gerbil PDF" ,(pel-pdf-file-url "pl-gerbil-scheme"))
  :group 'pel-pkg-for-scheme)

(defcustom pel-gerbil-activates-minor-modes nil
  "List of *local* minor-modes automatically activated for Gerbil buffers.
  Enter *local* minor-mode activating function symbols.
  Do not enter lambda expressions."
  :group 'pel-pkg-for-gerbil
  :type '(repeat function))

(defcustom pel-use-gerbil nil
  "Control whether PEL supports the Gerbil Scheme-based programming language.
Note that activating Gerbil also activates Gambit support."
  :link '(url-link :tag "Gerbil Homepage"
                   "https://cons.io")
  :link '(url-link :tag "Gerbil @ Github"
                   "https://github.com/vyzo/gerbil")
  :link '(url-link :tag "gerbil-mode @ Github"
                   "https://github.com/vyzo/gerbil/blob/master/etc/gerbil-mode.el")
  :group 'pel-pkg-for-gerbil
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-gerbil :package-is '(quote ((utils . gerbil-mode))))

(defcustom pel-gerbil-repl "gxi"
  "File or file path name of Gerbil REPL executable.
If the program is not in your PATH but located on your system
then its best to identify the full absolute path to the executable.
Use UNIX-style forward slash, even on Windows.
The default specifies the gxi program without a path.
If the program is available in your PATH that is good enough."
  :group 'pel-pkg-for-gerbil
  :type 'string)

(defcustom pel-gerbil-base-tags '("~/.gerbil/pkg/TAGS"
                                  "~/gerbil/src/TAGS")
  "List of TAGS files with Gerbil source tag table files.
The default identifies the common location of Gerbil tag files.
You can change, delete or add more.
These file are visited upon entering the gerbil-mode."
  :group 'pel-pkg-for-gerbil
  :type '(repeat file))

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; Guile Scheme Support
;; ---------------------
(defgroup pel-pkg-for-guile nil
  "PEL customization for the Guile Scheme programming language support."
  :link `(url-link :tag "Guile PDF" ,(pel-pdf-file-url "pl-guile-scheme"))
  :group 'pel-pkg-for-scheme)

(defcustom pel-guile-activates-minor-modes nil
  "List of *local* minor-modes automatically activated for Guile buffers.
  Enter *local* minor-mode activating function symbols.
  Do not enter lambda expressions."
  :group 'pel-pkg-for-guile
  :type '(repeat function))

(defcustom pel-use-guile nil
  "Control whether PEL supports the Guile Scheme-based programming language.
Note that activating Guile also activates Gambit support."
  :group 'pel-pkg-for-guile
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-guile :package-is :a-gate)

(defcustom pel-use-geiser-guile nil
  "Control whether PEL activates the geiser-guile external package."
  :link '(url-link :tag "geiser-guile @ GitLab"
                   "https://gitlab.com/emacs-geiser/guile")
  :group 'pel-pkg-for-guile
  :type 'boolean
  :safe #'booleanp)

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; Mit-Scheme Scheme Support
;; -------------------------
(defgroup pel-pkg-for-mit-scheme nil
  "PEL customization for the Mit-Scheme Scheme programming language support."
  :link `(url-link :tag "Mit-Scheme PDF" ,(pel-pdf-file-url "pl-mit-scheme-scheme"))
  :group 'pel-pkg-for-scheme)

(defcustom pel-mit-scheme-activates-minor-modes nil
  "List of *local* minor-modes automatically activated for Mit-Scheme buffers.
  Enter *local* minor-mode activating function symbols.
  Do not enter lambda expressions."
  :group 'pel-pkg-for-mit-scheme
  :type '(repeat function))

(defcustom pel-use-mit-scheme nil
  "Control whether PEL supports the Mit-Scheme Scheme-based programming language.
Note that activating Mit-Scheme also activates Scheme support."
  :group 'pel-pkg-for-mit-scheme
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-mit-scheme :package-is :a-gate)

(defcustom pel-use-geiser-mit nil
  "Control whether PEL activates the geiser-mit external package."
  :link '(url-link :tag "geiser-mit @ GitLab"
                   "https://gitlab.com/emacs-geiser/mit")
  :group 'pel-pkg-for-mit-scheme
  :type 'boolean
  :safe #'booleanp)

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; Racket Support
;; --------------
(defgroup pel-pkg-for-racket nil
  "PEL customization for the Racket programming language support."
  :group 'pel-pkg-for-scheme
  :link `(url-link :tag "Racket PDF" ,(pel-pdf-file-url "pl-racket")))

(defcustom pel-racket-activates-minor-modes nil
  "List of *local* minor-modes automatically activated for Racket buffers.
  Enter *local* minor-mode activating function symbols.
  Do not enter lambda expressions."
  :group 'pel-pkg-for-racket
  :type '(repeat function))

(defcustom pel-use-racket nil
  "Control whether PEL supports the Racket programming language.

  When this is activated, PEL activates the racket-mode
  package."
  :group 'pel-pkg-for-racket
  :link '(url-link :tag "racket-mode @ GitHub"
                   "https://github.com/greghendershott/racket-mode")
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-racket :package-is 'racket-mode)

(defcustom pel-use-geiser-racket nil
  "Control whether PEL activates the geiser-racket external package."
  :link '(url-link :tag "geiser-racket @ GitLab"
                   "https://gitlab.com/emacs-geiser/racket")
  :group 'pel-pkg-for-racket
  :type 'boolean
  :safe #'booleanp)

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; Scsh Scheme Support
;; -------------------
(defgroup pel-pkg-for-scsh nil
  "PEL customization for the Scsh Scheme programming language support."
  :link `(url-link :tag "Scsh PDF" ,(pel-pdf-file-url "pl-scsh-scheme"))
  :group 'pel-pkg-for-scheme)

(defcustom pel-scsh-activates-minor-modes nil
  "List of *local* minor-modes automatically activated for Scsh buffers.
  Enter *local* minor-mode activating function symbols.
  Do not enter lambda expressions."
  :group 'pel-pkg-for-scsh
  :type '(repeat function))

(defcustom pel-use-scsh nil
  "Control whether PEL supports the Scsh Scheme-based programming language.
Note that activating Scsh also activates Scheme support."
  :group 'pel-pkg-for-scsh
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-scsh :package-is :a-gate)

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; Inter-S-Expression Navigation
;; -----------------------------
(defgroup pel-sexp-form-navigation nil
  "Control navigation across S-expression forms with PEL keys.

  Includes user-option variables that control which of the S-expression forms
  the following commands move:

  - `pel-elisp-beginning-of-next-form'
  - `pel-elisp-beginning-of-previous-form'

  These commands are mostly used when editing code written in Lisp-like
  programming languages such as Emacs Lisp and Common Lisp.

  PEL binds these commands to the ``<f12>`` key prefix in Emacs-Lisp and
  Common-Lisp major modes. They can also be used in any other Lisp-like text.

  The main user-option variable is `pel-elisp-target-forms'.  With it you
  specify the group of targets.  The last possible choice is a user-specified
  list identified in the other user-option variable:
  `pel-elisp-user-specified-targets'."
  :group 'pel-pkg-for-elisp
  :group 'pel-pkg-for-clisp)

(defcustom pel-elisp-target-forms 'all-top-level-forms
  "Identify target of form navigation.

  Use as the target by the following commands:
  - `pel-elisp-beginning-of-next-form'
  - `pel-elisp-beginning-of-previous-form'

  The target is specified using one of the following:

  0) All top-level forms. The default.
1) Top-level defun forms only.
2) All defun forms, any level.
3) All of the following forms, any level
   - defun
   - defsubst
   - defmacro
   - cl-defmacro
   - defalias
   - defadvice
4) All defmacro and cl-defmacro forms, any level.
5) All of the following forms, any level
   - defun
   - defsubst
   - defmacro
   - cl-defmacro
   - defalias
   - defadvice
   - defclass
   - defmethod
   - defgeneric
6) All of the following forms, any level
   - defun
   - defsubst
   - defmacro
   - cl-defmacro
   - defalias
   - defadvice
   - defclass
   - defmethod
   - defgeneric
   - defvar
   - defvaralias
   - defvar-local
   - defvar-mode-local
   - defconst
   - defconst-mode-local
   - defface
   - deftheme
   - defcustom
   - defgroup
7) All of the following variable definition forms, at any level:
   - defvar
   - defvaralias
   - defvar-local
   - defvar-mode-local
   - defconst
   - defconst-mode-local
   - defface
   - deftheme
   - defcustom
   - defgroup
8) All forms specified by the list defined by:
   - `pel-elisp-user-specified-targets'
   - `pel-elisp-user-specified-targets2'

  Note that each entry in `pel-elisp-user-specified-targets' has a checkbox.
  Only the lines activated are searched.  The entries in that list correspond
  to valid Emacs Lisp function or macro symbols. Their docstrings are
  available except for those defined in files not yet loaded.  More free
  format regular expressions can be defined in the second user-option
  `pel-elisp-user-specified-targets2' to complement the first list.


You can modify the buffer local value of `pel-elisp-target-forms' by using the
`pel-elisp-set-navigate-target-form' command."
  :group 'pel-sexp-form-navigation
  :type '(choice
          (const  :tag "All top-level forms" all-top-level-forms)
          (const  :tag "All top-level defun" top-level-defun-forms)
          (const  :tag "All defun forms, any level"
                  defun-forms)
          (const  :tag "All defun, defmacro, defsubst,... forms, any level"
                  all-defun-defmacro-defsubst-forms)
          (const  :tag "All defmacro"
                  all-defmacro-forms)
          (const  :tag "All of 3 + eieio class and method definition forms, \
any level"
                  all-functions-macros-eieio-def-forms)
          (const  :tag "All of 5 + all variables define forms, any level"
                  all-functions-variables-def-forms)
          (const  :tag "Only variable forms, any level"
                  all-variables-def-forms)
          (repeat :tag "User specified"
                  user-specified)))

(defcustom pel-elisp-user-specified-targets '(defun
                                              defsubst
                                              defmacro
                                              defalias
                                              defadvice
                                              defclass
                                              defmethod
                                              defgeneric
                                              defvar
                                              defvaralias
                                              defvar-local
                                              defvar-mode-local
                                              defconst
                                              defconst-mode-local
                                              defface
                                              deftheme
                                              defcustom
                                              defgroup
                                              def-edebug-spec
                                              defmath
                                              defimage
                                              defezimage
                                              defun-gmm
                                              deffoo
                                              defvoo
                                              defhydra
                                              defhydra+
                                              defhydradio)
  "User selected forms used as movement target.
Use the check-box buttons to deactivate or activate any of them.
You can also insert others.

These, along with the symbols identified by
`pel-elisp-user-specified-targets2',  are used as the target
by the following commands when `pel-elisp-target-forms' is set to
\\='user-specified:
- `pel-elisp-beginning-of-next-form'
- `pel-elisp-beginning-of-previous-form'"
  :group 'pel-sexp-form-navigation
  :type 'hook
  :options '(defun
             defsubst
             defmacro
             defalias
             defadvice
             defclass
             defmethod
             defgeneric
             defvar
             defvaralias
             defvar-local
             defvar-mode-local
             defconst
             defconst-mode-local
             defface
             deftheme
             defcustom
             defgroup
             def-edebug-spec
             defmath
             defimage
             defezimage
             defun-gmm
             deffoo
             defvoo
             defhydra
             defhydra+
             defhydradio))

(defcustom pel-elisp-user-specified-targets2 nil
  "List of regexp strings identifying more movement targets.

These, along with the symbols identified by
`pel-elisp-user-specified-targets', are used as the target by the
following commands when `pel-elisp-target-forms' is set to
\\='user-specified:
- `pel-elisp-beginning-of-next-form'
- `pel-elisp-beginning-of-previous-form'

Unlike the symbols listed inside `pel-elisp-user-specified-targets'
here you need to write the complete regular expression for searching
an element.  For example, if you want to target all interactive commands
you can place the following regex string in the list:

   \"(interactive\"

without the enclosing quotes.  Note that you must identify the leading
parenthesis.

Also note that the commands will search for your regex as the
first non-blank character on a line, and will exclude all matches
inside comments and docstrings."
  :group 'pel-sexp-form-navigation
  :type '(repeat string))

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
(defgroup pel-elisp-code-style nil
  "Emacs Lisp Source Code Style options."
  :group 'pel-pkg-for-elisp
  :link `(url-link :tag "Emacs Lisp PDF" ,(pel-pdf-file-url "pl-emacs-lisp")))

;; style - 0
(defcustom pel-elisp-skel-use-separators t
  "Specifies whether Elisp code block include separators line.
If nil no separator line comment is used, otherwise separator line
comments of length controlled by variable `fill-column' are inserted."
  :group 'pel-elisp-code-style
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-elisp-skel-use-separators :choices '(nil t))

;; style - 1
(defcustom pel-elisp-skel-insert-file-timestamp nil
  "Specifies whether a timestamp is inserted inside Elisp file header block."
  :group 'pel-elisp-code-style
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-elisp-skel-insert-file-timestamp :choices '(nil t))

;; style - 2
(defcustom pel-elisp-skel-with-license nil
  "Control whether a license text is inserted in Elisp file header.

The available choices are:

- 0: No copyright, no license.                 nil
- 1: Copyright only, no license.               \\='only-copyright
- 2: Copyright, with selected license name.    a string
- 3: Copyright, with complete license text.    t

For choice 3, you specify the name of the license in the string
field.  Something like MIT or GPL-3.0, or anything appropriate.
That string will be entered verbatim inside a file header line.

For choice 4, the type of the license and its text are selected
the function `lice' taken from the external library with the same name.

The text of the inserted license is selected by the
`lice:default-license' user option, normally configured inside
the directory\\='s .dir-locals.el file written inside the global
setting like this:

   ((nil . ((fill-column . 80)
            (lice:default-license  . \"gpl-3.0\")
            (lice:copyright-holder . \"Your Name\")))

Replace \"gpl-3.0\" with the license you want and write your name
inside the copyright holder value.

When the user-option is t PEL activates the `pel-use-lice'
user-option if it is not activated already."
  :group 'pel-elisp-code-style
  :type
  '(choice
    (const :tag  "No license, no copyright." nil)
    (const :tag  "Copyright only." only-copyright)
    (string :tag "Copyright with specified license name.")
    (const :tag
           "Copyright with license text selected by `lice:default-license'"
           t)))
(pel-put 'pel-elisp-skel-with-license :choices '(nil t only-copyright "MIT"))

;; style - 3
(defcustom pel-elisp-skel-package-name 'extract-from-file-name
  "Specifies whether a package name ownership note is inserted.
If you want to insert one, it can either be extracted from the file name (in
this case it's the first word of the file name, fully up-cased) or can be
specified as a string."
  :group 'pel-elisp-code-style
  :type '(choice
          (const :tag "No, don't add package ownership note." nil)
          (const :tag "Add package ownership note extracted from file name."
                 extract-from-file-name)
          (string :tag "Use this specified string.")))
(pel-put 'pel-elisp-skel-package-name :choices '(nil extract-from-file-name "foo"))

;; ---------------------------------------------------------------------------
;; BEAM Programming Languages
;; --------------------------
(defgroup pel-pkg-for-beam-vm nil
  "PEL customization for BEAM Virtual Machine programming languages."
  :group 'pel-pkg-for-software-programming-languages)

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; Elixir Support
;; --------------
;; Note: Elixir is a BEAM VM programming language.

(defgroup pel-pkg-for-elixir nil
  "PEL customization for Elixir."
  :group 'pel-pkg-for-beam-vm
  :link `(url-link :tag "Elixir PDF" ,(pel-pdf-file-url "pl-elixir")))

(defcustom pel-use-elixir nil
  "Control whether PEL supports Elixir development."
  :group 'pel-pkg-for-elixir
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-elixir :package-is 'elixir-mode)

(defcustom pel-elixir-activates-minor-modes nil
  "List of *local* minor-modes automatically activated for Elixir buffers.
Enter *local* minor-mode activating function symbols.
Do not enter lambda expressions."
  :group 'pel-pkg-for-elixir
  :type '(repeat function))

(defcustom pel-use-alchemist nil
  "Control whether PEL supports Elixir Alchemist package.
IMPORTANT:
  To use this feature you must also activate `pel-use-elixir'."
  :group 'pel-pkg-for-elixir
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-alchemist :requires 'pel-use-elixir)

(defcustom pel-use-elixir-exunit nil
  "Control whether PEL supports Elixir Unit Test development.
IMPORTANT:
  To use this feature you must also activate `pel-use-elixir'."
  :group 'pel-pkg-for-elixir
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-elixir-exunit :package-is 'exunit)
(pel-put 'pel-use-elixir-exunit :requires 'pel-use-elixir)

(defcustom pel-use-elixir-lsp nil
  "Control whether PEL supports Lsp-Elixir package: Language Server Protocol.
This activates the use of the lsp-elixir package, and the lsp-mode
package which provides the client/library for LSP."
  :group 'pel-pkg-for-elixir
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-elixir-lsp :package-is 'lsp-elixir)
(pel-put 'pel-use-elixir-lsp :requires 'pel-use-elixir)

(defcustom pel-elixir-tab-width 4
  "Column width display rendering of hard tab for elixir buffers.

PEL stores this in `tab-width' when opening elixir  buffers.

This does *NOT* control the indentation in elixir
files, it only controls the column width display rendering of hard tabs
in buffers and tab stop positions for commands such as `tab-to-tab-stop'."
  :group 'pel-pkg-for-elixir
  :type 'integer
  :safe 'pel-indent-valid-p)

(defcustom pel-elixir-use-tabs nil
  "Value of `indent-tabs-mode' for editing elixir files.
- If set to nil: only spaces are used for indentation.
- If set to t: hard tabs are used when possible."
  :group 'pel-pkg-for-elixir
  :type 'boolean
  :safe #'booleanp)

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; Erlang Support
;; --------------
;; Note: Erlang, is a BEAM VM programming language.
;;

(defgroup pel-pkg-for-erlang nil
  "PEL customization for Erlang."
  :group 'pel-pkg-for-beam-vm
  :link `(url-link :tag "Erlang PDF" ,(pel-pdf-file-url "pl-erlang")))

(defcustom pel-use-erlang nil
  "Control whether PEL supports Erlang development."
  :group 'pel-pkg-for-erlang
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-erlang-activates-minor-modes nil
  "List of *local* minor-modes automatically activated for Erlang buffers.
Enter *local* minor-mode activating function symbols.
Do not enter lambda expressions."
  :group 'pel-pkg-for-erlang
  :type '(repeat function))

(defcustom pel-erlang-shell-prevent-echo nil
  "Set to t if the `erlang-shell-mode' shell echoes back commands.
When set to t PEL activates code that prevent echo of the typed commands."
  :group 'pel-pkg-for-erlang
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-use-erlang-syntax-check  nil
  "Controls whether PEL use ta syntax checker for Erlang.

The following choices are available:

- Not used (nil), the default.
- Use with flycheck.
- Use with flymake.

Select one of the last 2 choices to activate either flycheck or
flymake.  Note that flymake is built-in Emacs, flycheck is not.
flycheck seems to be the engine preferred by many people.  If you
select flycheck it, PEL will install it if it is not already
installed.

By default PEL does not automatically activate the syntax checker
when an Erlang file is visited. If you want it activated
automatically, then you must add the symbol `erlang-mode' to the
defcustom variable `pel-modes-activating-syntax-check'.

Note that if `pel-use-erlang-ls' is set, then flycheck is activated
regardless of the value of this user-option."
  :link '(custom-group-link "pel-base-emacs")
  :group 'pel-pkg-for-erlang
  :type '(choice
          (const :tag "Not used" nil)
          (const :tag "Use with flycheck" with-flycheck)
          (const :tag "Use with flymake"  with-flymake)))
(pel-put 'pel-use-erlang-syntax-check :requires 'pel-use-erlang)
(pel-put 'pel-use-erlang-syntax-check :package-is
         ;; flymake is built-in but not flycheck
         '(when (or pel-use-erlang-ls
                    (eq pel-use-erlang-syntax-check 'with-flycheck))
            '((elpa . flycheck))))


(defcustom pel-erlang-xref-engine 'etag
  "Cross Reference Back-End Engine to use for M-. command.

Except for \\='etags, all other choices require activation of a PEL
user option.  If you select one of those PEL will automatically activate the
corresponding user option to activate that package. If you select \\='edts, then
EDTS will be activated automatically when an Erlang buffer is opened."
  :group 'pel-pkg-for-erlang
  :type '(choice
          (const :tag "Use built-in etag system" etag)
          (const :tag "Use dumb-jump"            dumb-jump)
          (const :tag "Use Gnu Global ggtags"    ggtags)
          (const :tag "Use ivy-erlang-complete"  ivy-erlang-complete)
          (const :tag "Use EDTS"                 edts)
          (const :tag "Use erlang-ls/LSP"        erlang-ls)))

(defcustom pel-erlang-code-formatter-command nil
  "Erlang code formatter command.

Identify the command line PEL uses to reformat Erlang code.
By default none is used.
If you select erlfmt, the \"erlfmt -w\" command is used to reformat a file in
place.  You can also specify a command line expression by writing it as a
string.  That command should specify all its arguments including one that
is \"%s\" to hold the name of the temporary file where the Erlang code is
stored for processing."
  :group 'pel-pkg-for-erlang
  :type '(choice
          (const :tag "None used." nil)
          (const :tag "Use erlfmt"  erlfmt)
          (string :tag "Use specified command")))

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
(defgroup pel-erlang-environment nil
  "PEL specific environment to support Erlang development."
  :group 'pel-pkg-for-erlang)

;; The PEL configuration for Erlang attempts to control several aspects of the
;; various Emacs packages supporting Erlang: there's what I currently perceive
;; as a lack of commonality of control between them (erlang.el, edts, etc..)
;; and I want to simplify the identification of the various Erlang files for
;; several Erlang versions and for these tools.  The concepts controlled are:
;;
;; - The location of Erlang root directory:
;;   - Erlang directory tree includes:
;;     - Erlang libraries in erlang-root-dir/lib/[library] with:
;;       - beam files in     erlang-root-dir/lib/[library]/ebin
;;       - Erlang source in  erlang-root-dir/lib/[library]/src
;;                           erlang-root-dir/lib/[library]/include
;;       - Doc files in      erlang-root-dir/lib/[library]/doc/...
;;     - Erlang man files:   erlang-root-dir/man/man[13467]
;;
;;     The library files are used by the ivy-erlang-complete package
;;     to identify the functions and types of each Erlang libraries
;;     and to open documentation about a given function.
;;
;;     The man files are used by erlang.el package to open the man files
;;     when a Emacs man command is issued.
;;
;; Note, however, that the man files are not always present inside the Erlang
;; root directory; some distributions do not install the man files.  Erlang
;; for Windows also normally does not install the man files.
;;
;;   - erlang.el:
;;     - Supports only one version of Erlang at a time in Emacs.
;;     - Uses the user-option variable `erlang-root-dir' to
;;       name, identifies the directory where the man[13] directory holding
;;       the various man files are located.  If `erlang-root-dir' is nil,
;;       then erlang.el uses the man files inside
;;       ~/.emacs.d/cache/erlang_mode_man_pages
;;
;;  - EDTS:
;;    - Supports several versions of Erlang at a time in Emacs, allowing
;;      files from different directories to be associated as a project and
;;      identified as using a specific version of Erlang.
;;    - Downloads the Erlang man files for a specific Erlang version V into
;;      ~/.emacs.d/edts/doc/V
;;
;; PEL:
;;  - Supports multiple versions of Erlang, supports erlang.el and EDTS
;;    models, supports the use of MANPATH in the parent shell to identify the
;;    path where the man files are stored and also supports the
;;    ivy-erlang-complete  package.
;;
;;    PEL provides independent management of the Erlang root directory for man
;;    files used by erlang.el and the use of Erlang root directory for
;;    everything else so it is possible to request man pages for a version of
;;    Erlang different than what is currently being used.  This is useful when
;;    checking for backward or forward compatibility.
;;
;;    PEL provides two user-options to identify the Erlang root directory:
;;    - `pel-erlang-man-parent-rootdir' which identifies the Erlang root
;;      directory or how to detect it, for the value that is used by the
;;      erlang.el logic to open Erlang Man files: the function
;;      `erlang-man-dir'.  PEL implements the `pel-erlang-man-dir' wrapper
;;      that sets the `erlang-root-dir' to the value identified by
;;      `pel-erlang-man-parent-rootdir' user-option.


;;    `pel-erlang-version-detection-method'
;;  - If `pel-erlang-man-parent-rootdir' is non-nil it either identifies the
;;    parent directory of the Erlang man directory or an environment variable
;;    that identifies it. When the identified directory exists PEL sets the
;;    value of `erlang-root-dir' with it.
;;
;;       To support both erlang.el and EDTS, you can either store all the man
;;       parent directories inside ~/.emacs.d/edts/doc or place it somewhere
;;       else, like ~/docs/Erlang and create symlink ~/.emacs.d/edts/doc/V
;;       pointing to the directory ~/docs/Erlang/V that holds the man
;;       directory for the version V of Erlang.
;;
;;  - The `pel-erlang-version-detection-method' selects one of 3 method to
;;    detect Erlang's version. They are:
;;
;;    - auto-detect : automatic detection using the `version-erl' command
;;                    line utility (provided in PEL's bin directory).
;;    - specified   : `pel-erlang-version-detection-method' is a string that
;;                    identifies the Erlang version number.
;;                    user option.
;;    - by-envvar   : the version is read in the content of an environment
;;                    variable specified by name.
;;
;;  - PEL provides the version-erl command line utility (stored in PEL's bin
;;    directory).  This is a short Erlang script that prints Erlang's version
;;    on stdout.  The PEL Erlang initialization code uses that utility to
;;    automatically detects the version of Erlang available in Emacs parent
;;    process.

(defcustom pel-erlang-man-parent-rootdir nil
  "Root directory holding OTP man/man1 directory.

Set the user-option variable to hold (or detect) the absolute
path of the directory that holds Erlang/OTP man directory.  That
man directory should hold a man1, man3, man4 and man6 holding
Erlang man files.

The directory identified by `pel-erlang-man-parent-rootdir'
can differ from the real Erlang root directory.  PEL wraps
erlang.el `erlang-man-dir' with a function that sets the
value of `erlang-root-dir' to the value identified by
`pel-erlang-man-parent-rootdir' user-option as returned by
the function `pel-erlang-man-parent-rootdir'.

Available choices:

- nil : do not set it, use the value in `erlang-root-dir' user-option,
  which in turn is set according to the method selected by the
  `pel-erlang-path-detection-method' user-option.
- Enter a string, such as \"/usr/local/otp\", that holds the man
  directory which holds man1, man3, man4 and man6.
- Use an environment variable, such as PEL_ERLANG_MAN_PARENT_DIR, that
  contains the name of the directory to use.  This is more flexible, as it
  allows several OS shells to be defined with different values of that
  environment variable that identifies the location of the man file directory
  for the version of Erlang each shell activates."
  :group 'pel-erlang-environment
  :type '(radio
          (const  :tag "Use path stored in `erlang-root-dir'" nil)
          (string :tag "Use this absolute directory path"
                  :value "/usr/local/otp")
          (cons
           (const  :tag "Path set by content of environment variable"
                   in-envvar)
           (string :tag "Use this environment variable   "
                   :value "PEL_ERLANG_MAN_PARENT_DIR"))))

(defcustom pel-erlang-exec-path nil
  "Directory where the Erlang binaries are located.

In some cases, you may want to identify the directory where Erlang binaries
are located instead of depending on the execution path extracted from the OS
PATH environment variable.  You can do it here.

The choices are:

- nil   : use the execution path identified by the PATH OS
          environment variable.
- string: Identify the absolute path explicitly, ignoring PATH.
- environment variable: specify the name of an environment variable that holds
          the absolute path where Erlang binaries are located."
  :group 'pel-erlang-environment
  :type '(radio
          (const :tag "Use execution path identified by PATH" nil)
          (string :tag "Use this absolute directory path"
                  :value "/usr/local/otp/bin")
          (cons
           (const  :tag "Path set by content of environment variable"
                   in-envvar)
           (string :tag "Use this environment variable   "
                   :value "PEL_ERLANG_EXECPATH"))))


(defcustom pel-erlang-version-detection-method 'auto-detect
  "Identifies the method PEL uses to determine Erlang's version available.

This value is used by the function `pel-erlang-version' which attempts to
identify the available version of Erlang.

The choices are:

- auto-detect : PEL extracts the version of Erlang by running the
                bin/version-erl Erlang script.  This is the default.
- by-envvar   : A string that is the name of en OS environment variable that
                holds the version of Erlang.  The default environment
                variable name is PEL_ERLANG_VERSION, you can select another one.
- Fixed string  that represents the Erlang version number.
                Use this to impose a specific version.
                Useful for storing in a .dir-locals.el file.
                Something like \"22.3.4.2\" or \"23.0\" or \"R16B03\"."
  :group 'pel-erlang-environment
  :type '(radio
          (const :tag "Automatic detection using PEL's bin/version-erl script."
                 auto-detect)
          (cons
           (const :tag "Version set by content of environment variable."
                  by-envvar)
           (string :tag "Use this environment variable"
                   :value "PEL_ERLANG_VERSION"))
          (string :tag "Set Erlang version to")))

(defcustom pel-erlang-path-detection-method 'auto-detect
  "Identifies the method PEL uses to determine Erlang's root path.

This value is used by the function `pel-erlang-root-path' which attempts to
identify the available version of Erlang to set the user-options of various
packages, including:
- `erlang-root-dir'                 from erlang.el
- `ivy-erlang-complete-erlang-root' from ivy-erlang-complete.el

The choices are:

- auto-detect : PEL extracts Erlang root directory by running the
                bin/erlang-root-dir Erlang script.  This is the default.
- by-envvar   : A string that is the name of the OS environment variable that
                holds Erlang's root path.  The default environment
                variable is PEL_ERLANG_ROOT_DIR, you can select another one.
- Fixed string  that represents the Erlang root path.
                Use this to impose a specific version.
                Useful for storing in a .dir-locals.el file.
                Something like \"~/.asdf/installs/erlang/21.3\"."
  :group 'pel-erlang-environment
  :type '(radio
          (const :tag "Automatic detection using PEL's bin/erlang-root-dir script."
                 auto-detect)
          (cons
           (const :tag "PATH set by content of environment variable."
                  by-envvar)
           (string :tag "Use this environment variable"
                   :value "PEL_ERLANG_ROOT_DIR"))
          (string :tag "Set Erlang version to")))

(defcustom pel-erlang-project-root-identifiers '("rebar.config")
  "File names that identify the root of an Erlang project directory tree.

This is used by the function `pel-erlang-source-directories' which in turn is
used by several functions that look for Erlang source code files.

These files are added to the files identified by the generic
`pel-project-root-identifiers'."
  :group 'pel-erlang-environment
  :type '(repeat string))

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
(defgroup pel-erlang-ide nil
  "PEL customization of the IDE for Erlang."
  :group 'pel-pkg-for-erlang)

;; TODO: add support for Distel and Wrangler.

(defcustom pel-use-erlstack-mode nil
  "Control whether PEL uses erlstack-mode.

To use it, `pel-use-erlang' must be on (t)."
  :group 'pel-erlang-ide
  :link '(url-link "erlstack-mode @ GitHub"
                   "https://github.com/k32/erlstack-mode")
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-erlstack-mode :requires 'pel-use-erlang)

(defcustom pel-use-ivy-erlang-complete nil
  "Control whether PEL uses ivy-erlang-complete.

Provides context sensitive completion for Erlang without
connecting to Erlang nodes.
To use it, `pel-use-erlang' must be on (t)."
  :group 'pel-erlang-ide
  :link '(url-link "ivy-erlang-complete @ GitHub"
                   "https://github.com/s-kostyaev/ivy-erlang-complete")
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-ivy-erlang-complete :requires 'pel-use-erlang)
(pel-put 'pel-use-ivy-erlang-complete :requires 'pel-use-ivy)
(pel-put 'pel-use-ivy-erlang-complete :requires 'pel-use-counsel)
(pel-put 'pel-use-ivy-erlang-complete :also-required-when 'pel-use-company-erlang)

(defcustom pel-use-company-erlang nil
  "Control whether PEL uses company-erlang."
  :group 'pel-erlang-ide
  :link '(url-link "company-erlang @ GitHub"
                   "https://github.com/s-kostyaev/company-erlang")
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-company-erlang :requires 'pel-use-ivy-erlang-complete)
(pel-put 'pel-use-company-erlang :requires 'pel-use-company)


(defcustom pel-use-edts nil
  "Control whether PEL uses EDTS when `pel-use-erlang' is t.
EDTS := Erlang Development Tool Suite."
  :group 'pel-erlang-ide
  :link '(url-link "edts @ GitHub"
                   "https://github.com/sebastiw/edts")
  :link '(custom-group-link :tag "edts group (when loaded)" "edts")
  :type '(choice
          (const :tag "Do not use" nil)
          (const :tag "Use, activate manually" t)
          (const :tag "Use, activate automatically" start-automatically)))
(pel-put 'pel-use-edts :requires 'pel-use-erlang)

(defcustom pel-use-erlang-ls nil
  "Control whether PEL uses the Erlang Language Server.

Note that when set this activates the flycheck-mode, regardless
of the value of the `pel-use-erlang-syntax-check' user-option."
  :link '(url-link :tag "erlang_ls @ GitHub"
                   "https://github.com/erlang-ls/erlang_ls")
  :link '(custom-group-link :tag "lsp-erlang group (when loaded)" "lsp-erlang")
  :group 'pel-erlang-ide
  :group 'pel-pkg-for-lsp-mode
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-erlang-ls :requires 'pel-use-erlang)
(pel-put 'pel-use-erlang-ls :requires-package
         '(append (quote
                   ((elpa . lsp-mode )
                    (elpa . lsp-ui)
                    (elpa . lsp-origami)
                    ;; lsp-origami requires origami but won't recognize
                    ;; the version since it is in PEL utils.  To prevent
                    ;; Emacs from complaining about lsp-origami missing
                    ;; origami version 1.0 make sure the origami installed
                    ;; in elpa is not removed by pel-cleanup.
                    ;; TODO: remove all that once my PR in origami
                    ;;       is merged.
                    (elpa . origami)))
                  (when pel-use-helm
                    (quote
                     ((elpa . helm-lsp))))))

(defcustom pel-erlang-ls-with-separate-session nil
  "If nil join existing session server if it exists.

If t, create new Language Server for each new Emacs instance
editing Erlang code."
  :group 'pel-erlang-ide
  :group 'pel-pkg-for-lsp-mode
  :type 'boolean
  :safe #'booleanp)

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
(defgroup pel-erlang-code-style nil
  "Erlang Source Code Style options."
  :group 'pel-pkg-for-erlang
  :link `(url-link :tag "Erlang PDF" ,(pel-pdf-file-url "pl-erlang")))

(defcustom pel-erlang-fill-column 100
  "Column beyond which automatic line-wrapping should happen in Erlang code.

Can either be nil or an integer value.

When set to nil, Emacs user option variable `fill-column' value
is used for `erlang-mode' buffers, otherwise the integer value
specified by this value is stored in `fill-column' for Erlang
source code files.

The default is 100, a value recommended by the Inaka's Erlang
Coding Standards & Guidelines."
  :group 'pel-pkg-for-erlang
  :group 'pel-erlang-code-style
  :type '(choice
          (const   :tag "Use the default fill-column value." nil)
          (integer :tag "Use a specific value for erlang-mode buffers:"))
  :link '(url-link :tag "Inka Erlang Guideline"
                   "https://github.com/inaka/erlang_guidelines#\
100-column-per-line"))


(defcustom pel-erlang-tab-width 8
  "Column width display rendering of hard tab for Erlang source code.

PEL stores this in `tab-width' when editing buffer with Erlang source.

This does *NOT* control the indentation in Erlang source code.
It is used, however, to control the display rendering of hard tab
characters inserted inside source code and by commands that move
point to tab stop positions such as `tab-to-tab-stop', and the
display of hard TAB characters.

The indentation of Erlang code is mostly controlled by
`erlang-indent-level'. If `pel-erlang-tab-width' differs
`erlang-indent-level' then `pel-erlang-tab-width' should be a
multiple of `erlang-indent-level' in case hard tabs have been
inserted inside the source code.

Values in the [2, 8] range are accepted."
  :group 'pel-erlang-code-style
  :type 'integer
  :safe 'pel-indent-valid-p)

(defcustom pel-erlang-use-tabs nil
  "Value of `indent-tabs-mode' for editing Erlang source code.
- If set to nil: only spaces are used for indentation.
- If set to t: hard tabs are used when possible."
  :group 'pel-pkg-for-erlang
  :group 'pel-erlang-code-style
  :type 'boolean
  :safe #'booleanp)


;; Note: the symbol names used in the following defcustom MUST
;;       correspond to the ones used by erlang.el.
;;       See code in pel_keys.el and pel-erlang.el manipulating
;;       `erlang-electric-commands'.
(defcustom pel-erlang-electric-keys  '(erlang-electric-comma
                                       erlang-electric-gt
                                       erlang-electric-newline
                                       pel-erlang-electric-period
                                       erlang-electric-semicolon)
  "List of keys that should have electric behaviour for Erlang buffers.

By default PEL activates the electric behaviour of the 4 characters controlled
by the erlang.el package:  comma, gt, newline and semicolon.  PEL also
provides electric behaviour to the period, allowing typing '->' in code by
typing '-.'.

To disable the electric behaviour of a character un-check the
corresponding box.

PEL also supports dynamically toggling the electric behaviour of
a key with the ``<M-f12> M-``` prefix followed by the specific key.

Additionally PEL activates another electric behaviour to the comma,
by automatically inserting a space after a colon typed inside blocks.
This is controlled by `pel-erlang-space-after-comma-in-blocks'."
  :group 'pel-pkg-for-erlang
  :group 'pel-erlang-code-style
  :type '(set
          (const :tag "comma    : ,"   erlang-electric-comma)
          (const :tag "gt       : >"   erlang-electric-gt)
          (const :tag "newline  : RET" erlang-electric-newline)
          (const :tag "period   : ."   pel-erlang-electric-period)
          (const :tag "semicolon: ;"   erlang-electric-semicolon)))

(defcustom pel-erlang-space-after-comma-in-blocks nil
  "When set the `erlang-electric-comma' inserts a space after comma in blocks.

The `pel-erlang-electric-keys' must activate the electric comma.

You can also dynamically toggle the electric behaviour of the comma key by
using the \\[pel-erlang-comma] command, mapped to \"<M-f12> M-` ,\" in Erlang buffers.

If you want to keep the electric behaviour of the comma key, but temporary
want to disable adding spaces after a comma inside the current Erlang buffer,
use the \"<M-f12> M-` M,\" to toggle this behaviour off and on."
  :group 'pel-pkg-for-erlang
  :group 'pel-erlang-code-style
  :type 'boolean
  :safe #'booleanp)

;; style - 0
(defcustom pel-erlang-skel-use-separators t
  "Specifies whether Erlang code block include separators line.
If nil no separator line comment is used, otherwise separator line
comments of length controlled by variable `fill-column' are inserted."
  :group 'pel-erlang-code-style
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-erlang-skel-use-separators :choices '(nil t))

;; style 0.1
(defcustom pel-erlang-skel-use-secondary-separators t
  "Specifies whether Erlang code block include secondary separators line.

Secondary separator lines are:
- the first line of a header block,
- the second separator used inside a comment block such as function comment
  blocks.

If non-nil, the secondary line separators are included, otherwise they are not
included, reducing the comments overhead in files."
  :group 'pel-erlang-code-style
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-erlang-skel-use-secondary-separators :choices '(nil t))

;; style - 1
(defcustom pel-erlang-skel-insert-file-timestamp nil
  "Specifies whether a timestamp is inserted inside Erlang file header block."
  :group 'pel-erlang-code-style
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-erlang-skel-insert-file-timestamp :choices '(nil t))

;; style - 2
(defcustom pel-erlang-skel-with-license nil
  "Control whether a license text is inserted in file header comment block.

The available choices are:

- 0: No copyright, no license.                 nil
- 1: Copyright only, no license.               \\='only-copyright
- 2: Copyright, with selected license name.    a string
- 3: Copyright, with complete license text.    t

For choice 3, you specify the name of the license in the string
field.  Something like MIT or GPL-3.0, or anything appropriate.
That string will be entered verbatim inside a file header line.

For choice 4, the type of the license and its text are selected
the function `lice' taken from the external library with the same name.

The text of the inserted license is selected by the
`lice:default-license' user option, normally configured inside
the directory\\='s .dir-locals.el file written inside the global
setting like this:

   ((nil . ((fill-column . 80)
            (lice:default-license  . \"gpl-3.0\")
            (lice:copyright-holder . \"Your Name\")))

Replace \"gpl-3.0\" with the license you want and write your name
inside the copyright holder value.

When the user-option is t PEL activates the `pel-use-lice'
user-option if it is not activated already."
  :group 'pel-erlang-code-style
  :type
  '(choice
    (const :tag  "No license, no copyright." nil)
    (const :tag  "Copyright only." only-copyright)
    (string :tag "Copyright with specified license name.")
    (const :tag
           "Copyright with license text selected by `lice:default-license'"
           t)))
(pel-put 'pel-erlang-skel-with-license :choices '(nil t only-copyright "MIT"))

;; style - 3 : no package name support for Erlang
;; style - 4 : no file variable support for Erlang
;; style - 5 : no module section support for Erlang

;; style - 6
(defcustom pel-erlang-skel-with-edoc t
  "Control whether Edoc comments are placed inside generated Erlang code."
  :group 'pel-erlang-code-style
  :type '(choice
          (const :tag "Do not insert Edoc comment." nil)
          (const :tag "Insert Edoc comments everywhere." t)
          (const :tag "Insert Edoc comments only in functions, \
not in file header." in-function-only)))
(pel-put 'pel-erlang-skel-with-edoc :choices '(nil t in-function-only))

;; -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -
(defgroup pel-erlang-skeleton-control nil
  "Control Skeleton that generate Erlang source code."
  :group 'pel-erlang-code-style
  :group 'pel-pkg-for-skeletons
  :link `(url-link :tag "Erlang PDF" ,(pel-pdf-file-url "pl-erlang")))

(defcustom pel-erlang-skel-prompt-for-purpose t
  "Control whether skeleton insertions prompt for purpose strings."
  :group 'pel-erlang-skeleton-control
  :type '(choice
          (const :tag "Never prompt for purpose." nil)
          (const :tag "Always prompt for purpose (and function name)." t)
          (const :tag "Only prompt for file purpose." in-file-only)
          (const :tag "Only prompt for function purpose (and function name)."
                 in-function-only)))

(defcustom pel-erlang-skel-prompt-for-function-name t
  "Control whether skeleton insertions prompt for function name."
  :group 'pel-erlang-skeleton-control
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-erlang-skel-prompt-for-function-arguments t
  "Control whether skeleton insertions prompt for function arguments."
  :group 'pel-erlang-skeleton-control
  :type 'boolean
  :safe #'booleanp)

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; LFE - Lisp Flavoured Erlang - Support
;; -------------------------------------
;; Note: LFE is a BEAM VM programming language.

(defgroup pel-pkg-for-lfe nil
  "PEL customization for LFE (Lisp Flavoured Erlang)."
  :group 'pel-pkg-for-beam-vm
  :group 'pel-pkg-for-lisp
  :link `(url-link :tag "LFE PDF" ,(pel-pdf-file-url "pl-lfe")))

(defcustom pel-use-lfe nil
  "Control whether PEL supports LFE development.
LFE is Lisp Flavored Erlang, a Lisp language for the BEAM."
  :link `(url-link :tag "LFE PDF" ,(pel-pdf-file-url "pl-lfe"))
  :group 'pel-pkg-for-lfe
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-lfe :package-is 'lfe-mode)

(defcustom pel-lfe-activates-minor-modes nil
  "List of *local* minor-modes automatically activated for LFE buffers.
Enter *local* minor-mode activating function symbols.
Do not enter lambda expressions."
  :group 'pel-pkg-for-lfe
  :type '(repeat function))

(defcustom pel-inferior-lfe-activates-minor-modes nil
  "List of *local* minor-modes automatically activated for LFE shell buffers.
Enter *local* minor-mode activating function symbols.
Do not enter lambda expressions."
  :group 'pel-pkg-for-lfe
  :type '(repeat function))

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; Gleam Support
;; -------------
(defgroup pel-pkg-for-gleam nil
  "PEL customization for GLEAM - a BEAM programming language."
  :link `(url-link :tag "Gleam PDF" ,(pel-pdf-file-url "pl-gleam"))
  :link '(url-link :tag "Gleam Home" "https://gleam.run")
  :group 'pel-pkg-for-beam-vm)

(defcustom pel-use-gleam nil
  "Control whether PEL supports Gleam development.
Gleam is a functional static-type checking language for the BEAM.
Activating this automatically activates `pel-use-gleam-mode'."
  :group 'pel-pkg-for-gleam
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-gleam :package-is :a-gate)

(defcustom pel-use-gleam-mode nil
  "Control whether PEL supports the gleam-mode package.

PEL supports both `gleam-mode' and `gleam-ts-mode' with the user-options
in this user-option group when the `pel-use-gleam-mode' user option is
turned on.  The `gleam-mode' was the original code from this package which was
then deprecated and replaced by `gleam-ts-mode'. They can co-exist for
debugging purpose (by installing both) but PEL will select `gleam-ts-mode'.
You can manually change the mode by executing `gleam-mode'.

Note that tree-sitter support is now required for this and PEL only
supports tree-sitter for Emacs 30.1 and later.  Therefore support for Gleam is
only available with PEL for Emacs 30.1 and later."
  :link '(url-link :tag "gleam-mode @ Github"
                   "https://github.com/gleam-lang/gleam-mode")
  :group 'pel-pkg-for-gleam
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-gleam-mode :package-is :in-utils)
(pel-put 'pel-use-gleam-mode :requires 'pel-use-gleam)
(pel-put 'pel-use-gleam-mode :requires 'pel-use-tree-sitter)

(defcustom pel-gleam-activates-minor-modes nil
  "List of *local* minor-modes automatically activated for GLEAM buffers.
Enter *local* minor-mode activating function symbols.
Do not enter lambda expressions."
  :group 'pel-pkg-for-gleam
  :type '(repeat function))

(defcustom pel-gleam-tab-width 2
  "Column width display rendering of hard tab for gleam buffers.

PEL stores this in `tab-width' when opening gleam  buffers.

This does *NOT* control the indentation in gleam files, it only controls
the column width display rendering of hard tabs in buffers and tab stop
positions for commands such as `tab-to-tab-stop'.

Note that the Gleam community decided that all Gleam file should use
a 2-space character indentation level and no hard tab."
  :group 'pel-pkg-for-gleam
  :type 'integer
  :safe 'pel-indent-valid-p)

(defconst pel-gleam-use-tabs nil
  "Value of `indent-tabs-mode' for editing gleam files.
Gleam does not use hard tabs, as decided by the Gleam community.")

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; Emacs Tools for BEAM languages
;; ------------------------------
;;
;; The packages here can be used with more than on BEAM language and they
;; are explicitly declared children of multiple packages to ease their access
;; from all of the respective BEAM language groups.

(defcustom pel-use-flycheck-rebar3 nil
  "Control whether PEL supports the flycheck-rebar3 external package."
  :link '(url-link :tag "flycheck-rebar3 @ GitHub"
                   "https://github.com/joedevivo/flycheck-rebar3")
  :group 'pel-pkg-for-erlang
  :group 'pel-pkg-for-elixir
  :group 'pel-pkg-for-lfe
  :group 'pel-pkg-for-gleam
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-flycheck-rebar3 :requires '(pel-use-erlang
                                              pel-use-elixir
                                              pel-use-lfe
                                              pel-use-gleam))

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; Factor Support
;; --------------

(defgroup pel-pkg-for-factor nil
  "PEL customization for Factor."
  :group 'pel-pkg-for-software-programming-languages
  :link `(url-link :tag "Factor PDF" ,(pel-pdf-file-url "pl-factor")))

(defcustom pel-use-factor nil
  "Control whether PEL supports Factor development."
  :group 'pel-pkg-for-factor
  :link '(url-link :tag "FUEL, Factor's Ultimate Emacs Library"
                   "https://github.com/mcandre/fuel#readme-ov-file")
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-factor :package-is 'fuel)

(defcustom pel-factor-activates-minor-modes nil
  "List of *local* minor-modes automatically activated for Factor buffers.
Enter *local* minor-mode activating function symbols.
Do not enter lambda expressions."
  :group 'pel-pkg-for-factor
  :type '(repeat function))

(defcustom pel-factor-tab-width 4
  "Column width display rendering of hard tab for factor buffers.

PEL stores this in `tab-width' when opening factor buffers.

This does *NOT* control the indentation in factor
files, it only controls the column width display rendering of hard tabs
in buffers and tab stop positions for commands such as `tab-to-tab-stop'."
  :group 'pel-pkg-for-factor
  :type 'integer
  :safe 'pel-indent-valid-p)

(defcustom pel-factor-use-tabs nil
  "Value of `indent-tabs-mode' for editing factor files.
- If set to nil: only spaces are used for indentation.
- If set to t: hard tabs are used when possible."
  :group 'pel-pkg-for-factor
  :type 'boolean
  :safe #'booleanp)

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; Forth support
;; -------------
(defgroup pel-pkg-for-forth nil
  "PEL customization for Forth."
  :group 'pel-pkg-for-software-programming-languages
  :link `(url-link :tag "Forth PDF" ,(pel-pdf-file-url "pl-forth")))

(defcustom pel-use-forth nil
  "Control whether PEL supports Forth development."
  :group 'pel-pkg-for-forth
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-forth :package-is 'forth-mode)

(defcustom pel-forth-activates-minor-modes nil
  "List of *local* minor-modes automatically activated for Forth buffers.
Enter *local* minor-mode activating function symbols.
Do not enter lambda expressions."
  :group 'pel-pkg-for-forth
  :type '(repeat function))

(defcustom pel-forth-tab-width 4
  "Column width display rendering of hard tab for forth buffers.

PEL stores this in `tab-width' when opening forth  buffers.

This does *NOT* control the indentation in forth
files, it only controls the column width display rendering of hard tabs
in buffers and tab stop positions for commands such as `tab-to-tab-stop'."
  :group 'pel-pkg-for-forth
  :type 'integer
  :safe 'pel-indent-valid-p)

(defcustom pel-forth-use-tabs nil
  "Value of `indent-tabs-mode' for editing forth files.
- If set to nil: only spaces are used for indentation.
- If set to t: hard tabs are used when possible."
  :group 'pel-pkg-for-forth
  :type 'boolean
  :safe #'booleanp)

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; Julia Support
;; -------------
(defgroup pel-pkg-for-julia nil
  "PEL customization for Julia."
  :group 'pel-pkg-for-software-programming-languages
  :link `(url-link :tag "Julia PDF" ,(pel-pdf-file-url "pl-julia")))

(defcustom pel-use-julia  nil
  "Control whether PEL supports Julia development.
IMPORTANT:
  You *must* also activate `pel-use-vterm' to be able to use Julia
  development as this uses the `julia-snail' package which includes both the
  `julia-mode' but also a fast Julia REPL that uses the vterm."
  :group 'pel-pkg-for-julia
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-julia :package-is 'julia-snail)
(pel-put 'pel-use-julia :requires 'pel-use-vterm)

(defcustom pel-julia-activates-minor-modes nil
  "List of *local* minor-modes automatically activated for Julia buffers.
Enter *local* minor-mode activating function symbols.
Do not enter lambda expressions."
  :group 'pel-pkg-for-julia
  :type '(repeat function))

(defcustom pel-julia-tab-width 4
  "Column width display rendering of hard tab for julia buffers.

PEL stores this in `tab-width' when opening julia  buffers.

This does *NOT* control the indentation in julia
files, it only controls the column width display rendering of hard tabs
in buffers and tab stop positions for commands such as `tab-to-tab-stop'."
  :group 'pel-pkg-for-julia
  :type 'integer
  :safe 'pel-indent-valid-p)

(defcustom pel-julia-use-tabs nil
  "Value of `indent-tabs-mode' for editing julia files.
- If set to nil: only spaces are used for indentation.
- If set to t: hard tabs are used when possible."
  :group 'pel-pkg-for-julia
  :type 'boolean
  :safe #'booleanp)

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; Lua Support
;; -----------
(defgroup pel-pkg-for-lua nil
  "PEL customization for Lua."
  :group 'pel-pkg-for-software-programming-languages
  :link `(url-link :tag "Lua PDF" ,(pel-pdf-file-url "pl-lua")))

(defcustom pel-use-lua  nil
  "Control whether PEL supports Lua development."
  :group 'pel-pkg-for-lua
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-lua :package-is '(quote ((elpa . lua-mode))))

(defcustom pel-lua-activates-minor-modes nil
  "List of *local* minor-modes automatically activated for Lua buffers.
Enter *local* minor-mode activating function symbols.
Do not enter lambda expressions."
  :group 'pel-pkg-for-lua
  :type '(repeat function))

(defcustom pel-lua-tab-width 4
  "Column width display rendering of hard tab for lua buffers.

PEL stores this in `tab-width' when opening lua  buffers.

This does *NOT* control the indentation in lua
files, it only controls the column width display rendering of hard tabs
in buffers and tab stop positions for commands such as `tab-to-tab-stop'."
  :group 'pel-pkg-for-lua
  :type 'integer
  :safe 'pel-indent-valid-p)

(defcustom pel-lua-use-tabs nil
  "Value of `indent-tabs-mode' for editing lua files.
- If set to nil: only spaces are used for indentation.
- If set to t: hard tabs are used when possible."
  :group 'pel-pkg-for-lua
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-lua-shebang-line "#!/usr/bin/env lua"
  "Default shebang line to add in extension-less Lua files."
  :group 'pel-pkg-for-lua
  :type 'string)

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; M4 Support
;; ----------
(defgroup pel-pkg-for-m4 nil
  "PEL support for the M4 preprocessor programming language."
  :group 'pel-pkg-for-software-programming-languages
  :link `(url-link :tag "M4 PDF" ,(pel-pdf-file-url "pl-m4")))

(defcustom pel-use-m4 nil
  "Control whether PEL supports the M4 programming language.

When turned on the m4b-mode is associated with the PEL ``<f12>`` key."
  :group 'pel-pkg-for-m4
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-m4 :package-is :a-gate)

(defcustom pel-m4-activates-minor-modes nil
  "List of *local* minor-modes automatically activated for M4 buffers.
Enter *local* minor-mode activating function symbols.
Do not enter lambda expressions."
  :group 'pel-pkg-for-m4
  :type '(repeat function))

(defcustom pel-m4-tab-width 4
  "Column width display rendering of hard tab for M4 source code.

PEL stores this in `tab-width' when editing buffer with M4 source.

This only controls the indentation in M4 source code when the
`pel-m4-use-tabs` is nil.

Values in the [2, 8] range are accepted."
  :group 'pel-pkg-for-m4
  :type 'integer
  :safe 'pel-indent-valid-p)

(defcustom pel-m4-use-tabs nil
  "Value of `indent-tabs-mode' for editing M4 source code.
- If set to nil: only spaces are used for indentation.
- If set to t: hard tabs are used when possible."
  :group 'pel-pkg-for-m4
  :type 'boolean
  :safe #'booleanp)

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; Modula2 Support
;; ---------------
(defgroup pel-pkg-for-modula-2 nil
  "PEL customization for Modula2 programming language."
  :group 'pel-pkg-for-software-programming-languages
  :group 'modula2
  :link `(url-link :tag "Modula2 PDF" ,(pel-pdf-file-url "pl-modula2")))

(defcustom pel-use-modula-2 nil
  "Control whether PEL supports the Modula2 programming language."
  :group 'pel-pkg-for-modula-2
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-m2-activates-minor-modes nil
  "List of *local* minor-modes automatically activated for Modula2 buffers.
Enter *local* minor-mode activating function symbols.
Do not enter lambda expressions."
  :group 'pel-pkg-for-modula-2
  :type '(repeat function))

(defcustom pel-m2-use-tabs nil
  "Value of `indent-tabs-mode' for editing modula2 files.
- If set to nil: only spaces are used for indentation.
- If set to t: hard tabs are used when possible."
  :group 'pel-pkg-for-modula-2
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-m2-tab-width 4
  "Number of columns rendered in a buffer for a tab in Modula2 buffers."
  :group 'pel-pkg-for-modula-2
  :type 'integer
  :safe 'pel-indent-valid-p)

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; Nim Support
;; -----------

(defgroup pel-pkg-for-nim nil
  "PEL support for the Nim programming language."
  :group 'pel-pkg-for-software-programming-languages
  :link `(url-link :tag "Nim PDF" ,(pel-pdf-file-url "pl-nim")))

(defcustom pel-use-nim nil
  "Control whether PEL supports the Nim programming language.

When turned on:
- PEL automatically activate `pel-use-nim-mode'
- the `nim-mode' is associated with the PEL ``<f12>`` key."
  :group 'pel-pkg-for-nim
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-nim :package-is :a-gate)

(defcustom pel-use-nim-mode nil
  "Control whether PEL activates the nim-mode external package."
  :group 'pel-pkg-for-nim
  :link '(url-link :tag "nim-mode @ GitHub"
                   "https://github.com/nim-lang/nim-mode")
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-nim-mode :also-required-when 'pel-use-nim)

(defcustom pel-nim-activates-minor-modes nil
  "List of *local* minor-modes automatically activated for Nim buffers.
Enter *local* minor-mode activating function symbols.
Do not enter lambda expressions."
  :group 'pel-pkg-for-nim
  :type '(repeat function))

(defcustom pel-nim-tab-width 4
  "Column width display rendering of hard tab for nim buffers.

PEL stores this in `tab-width' when opening nim  buffers.

This does *NOT* control the indentation in nim
files, it only controls the column width display rendering of hard tabs
in buffers and tab stop positions for commands such as `tab-to-tab-stop'."
  :group 'pel-pkg-for-nim
  :type 'integer
  :safe 'pel-indent-valid-p)

(defcustom pel-nim-use-tabs nil
  "Value of `indent-tabs-mode' for editing nim files.
- If set to nil: only spaces are used for indentation.
- If set to t: hard tabs are used when possible."
  :group 'pel-pkg-for-nim
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-nim-shebang-line "#!/usr/bin/env -S nim --hints:off"
  "Default shebang line to add in extension-less Nim files."
  :group 'pel-pkg-for-nim
  :type 'string)

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; Objective-C Support
;; -------------------
(defgroup pel-pkg-for-objc nil
  "PEL support for the Objective-C programming language."
  :group 'pel-pkg-for-software-programming-languages
  :link `(url-link :tag "Objective-C PDF" ,(pel-pdf-file-url
                                            "pl-objc")))

(defcustom pel-use-objc nil
  "Control whether PEL supports the objc-mode for GNU Objective-C language."
  :group 'pel-pkg-for-objc
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-use-emacs-ccls-for-objc nil
  "Control whether PEL activates ccls lsp for Objective-C.

This automatically activates `pel-use-emacs-ccls', `pel-use-lsp-mode'.
and `pel-use-lsp-ui-mode'."
  :group 'pel-pkg-for-objc
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-objc-activates-minor-modes nil
  "List of *local* minor-modes automatically activated for Objective-C buffers.
Enter *local* minor-mode activating function symbols.
Do not enter lambda expressions."
  :group 'pel-pkg-for-objc
  :type '(repeat function))

(defcustom pel-objc-indent-width 4
  "Number of columns for Objective-C source code indentation.
PEL stores this in `c-basic-offset' when editing buffers with Objective-C code.
Values in the [2, 8] range are accepted."
  :group 'pel-pkf-for-pike
  :type 'integer
  :safe 'pel-indent-valid-p)

(defcustom pel-objc-tab-width 4
  "Column width display rendering of hard tab for objc buffers.

PEL stores this in `tab-width' when opening Objective-C buffers.

This does *NOT* control the indentation in Objective-C
files, it only controls the column width display rendering of hard tabs
in buffers and tab stop positions for commands such as `tab-to-tab-stop'."
  :group 'pel-pkg-for-objc
  :type 'integer
  :safe 'pel-indent-valid-p)

(defcustom pel-objc-use-tabs nil
  "Value of `indent-tabs-mode' for editing Objective-C files.
- If set to nil: only spaces are used for indentation.
- If set to t: hard tabs are used when possible."
  :group 'pel-pkg-for-objc
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-objc-bracket-style "bsd"
  "Set the bracket style for the Objective-C programming language.
PEL stores this value associated with the `objc-mode' into the
`c-default-style' user option variable in objc buffers.
If you want to use something else, please select one of the
CC Mode Built-in Styles, which include the following:
- gnu
- k&r
- bsd
- whitesmith
- stroustrup
- ellemtel
- linux
- python
- java
- awk
- user"
  :link '(custom-group-link "C")
  :link '(custom-manual "(ccmode)Built-in Styles")
  :link '(url-link
          :tag "Bracket styles @ Emacs Manual"
          "https://www.gnu.org/software/emacs/manual/html_node/\
ccmode/Built_002din-Styles.html#Built_002din-Styles")
  :link '(url-link :tag "Indentation styles @ wikipedia"
                   "https://en.wikipedia.org/wiki/Indentation_style")
  :group 'pel-pkg-for-objc
  :type 'string
  :safe 'pel-c-style-valid-p)

(defcustom pel-objc-fill-column 80
  "Column beyond which automatic line-wrapping should happen in Objective-C code.
Can either be nil or an integer value.
When set to nil, Emacs user option variable `fill-column' value
is used for `c-mode' buffers, otherwise the integer value specified by
`pel-c-fill-column' is stored in the variable `fill-column' for
`c-mode' buffers.  The default is 80."
  :group 'pel-pkg_for-objc
  :type '(choice
          (const   :tag "Use the default fill-column value." nil)
          (integer :tag "Use a value specific for c-mode buffers:")))


(defcustom  pel-objc-newline-mode 'context-newline
  "Set default newline mode for c-mode buffers.

This may be one of the following values:

- context-newline : the default : the RET key is bound to
  the function `c-context-line-break' with the extra ability to
  repeat its execution with an argument.
- newline-and-indent: RET uses the function `newline' passing ARG
  and t for its arguments to insert newline and indent.
- just-newline-no-indent: RET uses the function
  `electric-indent-just-newline` with the specified argument (if
  any).  This only insert a newline; it does not indent."
  :group 'pel-pkg-for-objc
  :type '(choice
          (const :tag "context-newline: use c-context-line-break.\n\
Does all what newline does plus more."
                 context-newline)
          (const :tag "newline-and-indent: use newline. This inserts\n\
a newline and then indent the new line."
                 newline-and-indent)
          (const :tag "just-newline-no-indent: use\
 electric-indent-just-newline.\n\
Does not indent."
                 just-newline-no-indent)))


;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; Ocaml Support
;; -------------
(defgroup pel-pkg-for-ocaml nil
  "PEL support for the Ocaml programming language."
  :group 'pel-pkg-for-software-programming-languages
  :link `(url-link :tag "Ocaml PDF" ,(pel-pdf-file-url "pl-ocaml")))

(defcustom pel-use-ocaml nil
  "Control whether PEL supports the Ocaml programming language.

When turned on the ocaml-mode is associated with the PEL ``<f12>`` key.
Activating this activated the following user-options:

- `pel-use-caml-mode'
- `pel-use-merlin'
- `pel-use-tuareg'"
  :group 'pel-pkg-for-ocaml
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-ocaml :package-is :a-gate)

(defcustom pel-use-caml-mode nil
  "Control whether PEL activates the caml-mode external package."
  :group 'pel-pkg-for-ocaml
  :link '(url-link :tag "caml-mode @ GitHub"
                   "https://github.com/ocaml/caml-mode")
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-caml-mode :also-required-when 'pel-use-ocaml)

(defcustom pel-use-tuareg nil
  "Control whether PEL activates the tuareg external package.
This provides a major mode for OCaml files"
  :group 'pel-pkg-for-ocaml
  :link '(url-link :tag "tuareg @ GitHub"
                   "https://github.com/ocaml/tuareg")
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-tuareg :also-required-when 'pel-use-ocaml)

(defcustom pel-use-merlin nil
  "Control whether PEL activates the merlin external package.
This provides an assistant for OCaml."
  :group 'pel-pkg-for-ocaml
  :link '(url-link :tag "merlin @ GitHub"
                   "https://github.com/ocaml/merlin")
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-merlin :also-required-when 'pel-use-ocaml)

(defcustom pel-tuareg-activates-minor-modes nil
  "List of *local* minor-modes automatically activated for Ocaml buffers.
Enter *local* minor-mode activating function symbols.
Do not enter lambda expressions."
  :group 'pel-pkg-for-ocaml
  :type '(repeat function))

(defcustom pel-tuareg-tab-width 4
  "Column width display rendering of hard tab for ocaml buffers.

PEL stores this in `tab-width' when opening ocaml  buffers.

This does *NOT* control the indentation in ocaml
files, it only controls the column width display rendering of hard tabs
in buffers and tab stop positions for commands such as `tab-to-tab-stop'."
  :group 'pel-pkg-for-ocaml
  :type 'integer
  :safe 'pel-indent-valid-p)

(defcustom pel-tuareg-use-tabs nil
  "Value of `indent-tabs-mode' for editing ocaml files.
- If set to nil: only spaces are used for indentation.
- If set to t: hard tabs are used when possible."
  :group 'pel-pkg-for-ocaml
  :type 'boolean
  :safe #'booleanp)

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; Odin Support
;; ------------
(defgroup pel-pkg-for-odin nil
  "PEL customization for Odin programming language."
  :group 'pel-pkg-for-software-programming-languages
  :group 'odin
  :link `(url-link :tag "Odin PDF" ,(pel-pdf-file-url "pl-odin"))
  :link '(url-link :tag "Odin Programming Language"
                   "https://odin-lang.org/"))

(defcustom pel-use-odin nil
  "Control whether PEL supports the odin programming language."
  :link '(url-link :tag "odin-mode @ Github"
                   "https://github.com/mattt-b/odin-mode")
  :link '(url-link :tag "my odin-mode fork @ Github"
                   "https://github.com/pierre-rouleau/odin-mode")
  :group 'pel-pkg-for-odin
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-odin :package-is '(quote ((utils . odin-mode))))


(defcustom pel-use-flycheck-odin nil
  "Control whether PEL supports flycheck for odin.

When activated, automatically activates `pel-use-flycheck'."
  :link '(url-link :tag "flycheck-odin @ Github"
                   "https://github.com/mattt-b/flycheck-odin")
  :group 'pel-pkg-for-odin
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-flycheck-odin :package-is :in-utils)


(defcustom pel-odin-activates-minor-modes nil
  "List of *local* minor-modes automatically activated for Odin buffers.
Enter *local* minor-mode activating function symbols.
Do not enter lambda expressions."
  :group 'pel-pkg-for-odin
  :type '(repeat function))

(defcustom pel-odin-use-tabs nil
  "Value of `indent-tabs-mode' for editing odin files.
- If set to nil: only spaces are used for indentation.
- If set to t: hard tabs are used when possible."
  :group 'pel-pkg-for-odin
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-odin-tab-width 4
  "Number of columns rendered in a buffer for a tab in Odin."
  :group 'pel-pkg-for-odin
  :type 'integer
  :safe 'pel-indent-valid-p)

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; Pascal Support
;; --------------
(defgroup pel-pkg-for-pascal nil
  "PEL customization for Pascal  programming language."
  :group 'pel-pkg-for-software-programming-languages
  :group 'pascal
  :link `(url-link :tag "Pascal PDF" ,(pel-pdf-file-url "pl-pascal")))

(defcustom pel-use-pascal nil
  "Control whether PEL supports the Pascal programming language."
  :group 'pel-pkg-for-pascal
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-pascal-activates-minor-modes nil
  "List of *local* minor-modes automatically activated for Pascal buffers.
Enter *local* minor-mode activating function symbols.
Do not enter lambda expressions."
  :group 'pel-pkg-for-pascal
  :type '(repeat function))

(defcustom pel-pascal-use-tabs nil
  "Value of `indent-tabs-mode' for editing pascal files.
- If set to nil: only spaces are used for indentation.
- If set to t: hard tabs are used when possible."
  :group 'pel-pkg-for-pascal
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-pascal-tab-width 4
  "Number of columns rendered in a buffer for a tab in Pascal buffers."
  :group 'pel-pkg-for-pascal
  :type 'integer
  :safe 'pel-indent-valid-p)

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; Perl Support
;; ------------
(defgroup pel-pkg-for-perl nil
  "PEL customization for Perl.

PEL has 3 sub-groups of Perl customization user options:

- PEL General Perl settings, which apply to perl and cperl.
- perl-mode settings,
- cperl-mode settings.

The perl-mode and cperl-mode settings exits because PEL code depend
on their presence.  However, the PEL General Perl setting user-options that
use the same name are stored into the perl and cperl specific ones.  Therefore
you should set the general user options to set the other two."
  :group 'pel-pkg-for-software-programming-languages
  :link `(url-link :tag "Perl PDF" ,(pel-pdf-file-url "pl-perl")))

(defcustom pel-use-perl nil
  "Control whether PEL supports the Perl programming language.

When turned on the perl-mode is associated with the PEL ``<f12>`` key."
  :group 'pel-pkg-for-perl
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-perl :package-is :a-gate)
(pel-put 'pel-use-perl :also-required-when 'pel-use-perl-repl)
(pel-put 'pel-use-perl :package-is '(when (eq pel-perl-mode
                                              'HaraldJoerg/cperl-mode)
                                      (quote ((utils . cperl-mode)
                                              (utils . perl-tidy-ediff)))))

(defcustom pel-perl-mode 'HaraldJoerg/cperl-mode
  "Selects the major-mode used for Perl files.

3 different major-modes are supported:

- The `perl-mode' distributed with Emacs.  The simplest
  mode. Emacs (but not PEL's) default.
- The `cperl-mode' distributed with Emacs. More powerful, with
  more features than `perl-mode'.
- A newer `cperl-mode' implementation from Harald Jörg, the
  maintainer of Emacs official `cperl-mode'.  This uses the
  upstream branch `cperl-mode', which corresponds to the latest features
  available and what is available on the next Emacs version to be released.
  It also installs the `perl-tidy-ediff' module that provides a useful buffer
  only perltidy support with ediff session.
  This is the PEL default because it provides the best available Perl support.

When using HaraldJoerg/cperl mode, PEL stores it cperl-mode.el
and perl-tidy-ediff.el files inside PEL utils directory, and
since PEL places that directory at the beginning of Emacs
`load-path' it is used instead of Emacs own cperl.el file.

If later you change your mind and want to use Emacs own cperl.el,
then you must first delete the cperl-mode and perl-tidy-ediff
files from the PEL utils directory."
  :group 'pel-pkg-for-perl
  :link '(url-link :tag "Harald Jörg cperl-mode"
                   "https://github.com/HaraldJoerg/cperl-mode")
  :type '(choice
          (const :tag "Use perl-mode" nil)
          (const :tag "Use cperl-mode" cperl-mode)
          (const :tag "Use HaraldJoerg/cperl-mode" HaraldJoerg/cperl-mode)))

(defcustom pel-use-perl-repl nil
  "Control whether PEL supports the perl-repl package."
  :link '(url-link
          :tag "perl-repl @Github"
          "https://github.com/pierre-rouleau/perl-repl-el/blob/master/perl-repl.el")
  :group 'pel-pkg-for-perl
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-perl-repl :package-is :in-utils)

;; [:todo 2025-02-04, by Pierre Rouleau: Add perlenv support once
;;                    I have time to look into a top-level integration
;;                    strategy that would work with customization. ]
;; (defcustom pel-use-perlenv nil
;;   "Control whether PEL supports the perlenv package."
;;   :link '(url-link
;;           :tag "perlenv @Github"
;;           "https://github.com/corund/perlenv")
;;   :group 'pel-pkg-for-perl
;;   :type 'boolean
;;   :safe #'booleanp)
;; (pel-put 'pel-use-perlenv :package-is :in-utils)

(defcustom pel-use-perl-live-coding nil
  "Control whether PEL supports the perl-live-coding."
  :group 'pel-pkg-for-perl
  :link '(url-link :tag "perl live coding @ Github"
		           "https://github.com/vividsnow/perl-live")
  :link '(url-link :tag "my fork @ Github"
		           "https://github.com/pierre-rouleau/perl-live")
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-perl-live-coding :package-is '(quote ((utils . perl-live))))

;; - - - - - - - - - - - - - - - -
(defgroup pel-pkg-for-perl-general nil
  "The PEL General PEL settings, used to set both perl and cperl ones."
  :group 'pel-pkg-for-perl)

(defcustom pel-general-perl-indent-level 4
  "Indentation of CPerl statements with respect to containing block.
This is applied to `perl-indent-level' and `cperl-indent-level'."
  :group 'pel-pkg-for-perl-general
  :type 'integer
  :safe 'pel-indent-valid-p)

(defcustom pel-general-perl-use-tabs nil
  "Value of `indent-tabs-mode' for editing perl files.
- If set to nil: only spaces are used for indentation.
- If set to t: hard tabs are used when possible."
  :group 'pel-pkg-for-perl-general
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-perl-extra-project-root-directories nil
  "List of directories searched by `pel-open-point-at' in Perl buffers.

PEl appends this list to the directory list specified by Perl @INC array
variable.  Each string should represent an absolute path.
Path expansion is done for ~ . "
  :group 'pel-pkg-for-perl-general
  :type '(repeat string))

(defcustom pel-perl-shebang-line "#!/usr/bin/perl -T"
  "Default shebang line to add in extension-less Perl files."
  :group 'pel-pkg-for-perl-general
  :type 'string)

;; - - - - - - - - - - - - - - - -
(defgroup pel-pkg-for-perl-perl-mode nil
  "The PEL perl-mode settings."
  :group 'pel-pkg-for-perl)

(defcustom pel-perl-activates-minor-modes nil
  "List of *local* minor-modes automatically activated for Perl buffers.
Enter *local* minor-mode activating function symbols.
Do not enter lambda expressions."
  :group 'pel-pkg-for-perl-perl-mode
  :type '(repeat function))

(defcustom pel-perl-use-tabs pel-general-perl-use-tabs
  "Value of `indent-tabs-mode' for editing perl files.
- If set to nil: only spaces are used for indentation.
- If set to t: hard tabs are used when possible."
  :group 'pel-pkg-for-perl-perl-mode
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-perl-tab-width pel-general-perl-indent-level
  "Number of columns rendered in a buffer for a hard tab in Perl."
  :group 'pel-pkg-for-perl-perl-mode
  :type 'integer
  :safe 'pel-indent-valid-p)

;; - - - - - - - - - - - - - - - -
(defgroup pel-pkg-for-perl-cperl-mode nil
  "The PEL cperl-mode settings."
  :group 'pel-pkg-for-perl)

;; activation for cperl-mode to mimic the perl-mode
(defcustom pel-cperl-activates-minor-modes nil
  "List of *local* minor-modes automatically activated for cperl buffers.
Enter *local* minor-mode activating function symbols.
Do not enter lambda expressions."
  :group 'pel-pkg-for-perl-cperl-mode
  :type '(repeat function))

(defcustom pel-cperl-use-tabs pel-general-perl-use-tabs
  "Value of `indent-tabs-mode' for editing perl files in cperl-mode.
- If set to nil: only spaces are used for indentation.
- If set to t: hard tabs are used when possible."
  :group 'pel-pkg-for-perl-cperl-mode
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-cperl-tab-width pel-general-perl-indent-level
  "Number of columns rendered in a buffer for a hard tab cperl-mode buffers.

This is currently unused, adjusted to the value of
`pel-general-perl-indent-level' for consistency."
  :group 'pel-pkg-for-perl-cperl-mode
  :type 'integer
  :safe 'pel-indent-valid-p)

(defcustom pel-cperl-show-trailing-whitespace-normally t
  "If set, shows trailing space using standard face instead of underscore.

By default `cperl-mode' shows trailing spaces with underscore face,
but the character is still a space.  This is surprising, since it
differs from the standard way of representing it with the
`trailing-whitespace' face.

By setting this user-option to t, PEL sets `cperl-invalid-face' to
nil and activates the `show-trailing-whitespace' minor mode to
show the trailing spaces as usual."
  :group 'pel-pkg-for-perl-cperl-mode
  :type 'boolean
  :safe #'booleanp)

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; Pike Support
;; ------------
(defgroup pel-pkg-for-pike nil
  "PEL customization for Pike."
  :group 'pel-pkg-for-software-programming-languages
  :link `(url-link :tag "Pike PDF" ,(pel-pdf-file-url "pl-pike")))

(defcustom pel-use-pike  nil
  "Control whether PEL supports Pike development."
  :group 'pel-pkg-for-pike
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-pike :package-is :builtin-emacs)

(defcustom pel-pike-activates-minor-modes nil
  "List of *local* minor-modes automatically activated for Pike buffers.
Enter *local* minor-mode activating function symbols.
Do not enter lambda expressions."
  :group 'pel-pkg-for-pike
  :type '(repeat function))

(defcustom pel-pike-indent-width 4
  "Number of columns for Pike source code indentation.
PEL stores this in `c-basic-offset' when editing buffers with Pike code.
Values in the [2, 8] range are accepted."
  :group 'pel-pkf-for-pike
  :type 'integer
  :safe 'pel-indent-valid-p)

(defcustom pel-pike-tab-width 4
  "Column width display rendering of hard tab for buffers in `pike-mode'.

PEL stores this in `tab-width' when opening Pike buffers.

This does *NOT* control the indentation in Pike files.
It is used, however, to control the display rendering of hard tab
characters inserted inside source code and by commands that move
point to tab stop positions such as `tab-to-tab-stop', and the
display of hard TAB characters.

Values in the [2, 8] range are accepted."
  :group 'pel-pkg-for-pike
  :type 'integer
  :safe 'pel-indent-valid-p)

(defcustom pel-pike-use-tabs nil
  "Value of `indent-tabs-mode' for editing pike files.
- If set to nil: only spaces are used for indentation.
- If set to t: hard tabs are used when possible."
  :group 'pel-pkg-for-pike
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-pike-bracket-style "bsd"
  "Set the bracket style for the Pike programming language.
PEL stores this value associated with the `pike-mode' into the
`c-default-style' user option variable in pike buffers.
If you want to use something else, please select one of the
CC Mode Built-in Styles, which include the following:
- gnu
- k&r
- bsd
- whitesmith
- stroustrup
- ellemtel
- linux
- python
- java
- awk
- user"
  :link '(custom-group-link "C")
  :link '(custom-manual "(ccmode)Built-in Styles")
  :link '(url-link
          :tag "Bracket styles @ Emacs Manual"
          "https://www.gnu.org/software/emacs/manual/html_node/\
ccmode/Built_002din-Styles.html#Built_002din-Styles")
  :link '(url-link :tag "Indentation styles @ wikipedia"
                   "https://en.wikipedia.org/wiki/Indentation_style")
  :group 'pel-pkg-for-pike
  :type 'string
  :safe 'pel-c-style-valid-p)

(defcustom pel-pike-fill-column 80
  "Column beyond which automatic line-wrapping should happen in Pike code.
Can either be nil or an integer value.
When set to nil, Emacs user option variable `fill-column' value
is used for `c-mode' buffers, otherwise the integer value specified by
`pel-c-fill-column' is stored in the variable `fill-column' for
`c-mode' buffers.  The default is 80."
  :group 'pel-pkg_for-pike
  :type '(choice
          (const   :tag "Use the default fill-column value." nil)
          (integer :tag "Use a value specific for c-mode buffers:")))


(defcustom  pel-pike-newline-mode 'context-newline
  "Set default newline mode for c-mode buffers.

This may be one of the following values:

- context-newline : the default : the RET key is bound to
  the function `c-context-line-break' with the extra ability to
  repeat its execution with an argument.
- newline-and-indent: RET uses the function `newline' passing ARG
  and t for its arguments to insert newline and indent.
- just-newline-no-indent: RET uses the function
  `electric-indent-just-newline` with the specified argument (if
  any).  This only insert a newline; it does not indent."
  :group 'pel-pkg-for-pike
  :type '(choice
          (const :tag "context-newline: use c-context-line-break.\n\
Does all what newline does plus more."
                 context-newline)
          (const :tag "newline-and-indent: use newline. This inserts\n\
a newline and then indent the new line."
                 newline-and-indent)
          (const :tag "just-newline-no-indent: use\
 electric-indent-just-newline.\n\
Does not indent."
                 just-newline-no-indent)))

(defcustom pel-pike-shebang-line "#!/usr/bin/env pike"
  "Default shebang line to add in extension-less Pike files."
  :group 'pel-pkg-for-pike
  :type 'string)

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; Python Support
;; --------------
(defgroup pel-pkg-for-python nil
  "PEL customization for Python."
  :group 'pel-pkg-for-software-programming-languages
  :link `(url-link :tag "Python PDF" ,(pel-pdf-file-url "pl-python")))

(defcustom pel-use-python  nil
  "Control whether PEL supports Python development."
  :group 'pel-pkg-for-python
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-python :package-is :builtin-emacs)

(defcustom pel-python-activates-minor-modes nil
  "List of *local* minor-modes automatically activated for Python buffers.
Enter *local* minor-mode activating function symbols.
Do not enter lambda expressions."
  :group 'pel-pkg-for-python
  :type '(repeat function))

(defcustom pel-python-tab-width 4
  "Column width display rendering of hard tab for buffers in `python-mode'.

PEL stores this in `tab-width' when opening Python buffers.

This does *NOT* control the indentation in Python files.
It is used, however, to control the display rendering of hard tab
characters inserted inside source code and by commands that move
point to tab stop positions such as `tab-to-tab-stop', and the
display of hard TAB characters.

Values in the [2, 8] range are accepted."
  :group 'pel-pkg-for-python
  :type 'integer
  :safe 'pel-indent-valid-p)

(defcustom pel-python-use-tabs nil
  "Value of `indent-tabs-mode' for editing python files.
- If set to nil: only spaces are used for indentation.
- If set to t: hard tabs are used when possible."
  :group 'pel-pkg-for-python
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-use-external-python-mode nil
  "Control whether PEL uses this external python-mode package is used.

⚠️ CAUTION ⚠️ : This is no longer supported as it causes too many
problems.  I strongly recommend you stay away from using this as
it clashes with Emacs native Python support and other tools.
This external package would require a large amount of cleanup.

If you have this activated, please:

- turn this off,
- delete the python-mode package from you ~/.emacs.d/elpa directory,
- delete python-mode from the package-selected-packages list inside
  your ~/.emacs.d/emacs-customization.el file."
  :group 'pel-pkg-for-python
  :type 'boolean
  :safe #'booleanp
  :link '(url-link :tag "python-mode external package"
                   "https://gitlab.com/python-mode-devs/python-mode"))

(defcustom pel-use-lpy nil
  "Control whether PEL supports lpy package.
The lpy package provides lispy-style modal editing for Python.
Note: `pel-use-python' must be t for this to be effective."
  :group 'pel-pkg-for-python
  :type 'boolean
  :safe #'booleanp
  :link '(url-link :tag "lpy @ GitHub"
                  "https://github.com/abo-abo/lpy"))
(pel-put 'pel-use-lpy :requires 'pel-use-python)


(defcustom pel-use-elpy nil
  "Control whether PEL activates the elpy package."
  :link '(url-link :tag "elpy @ GitHub"
                   "https://github.com/jorgenschaefer/elpy")
  :group 'pel-pkg-for-python
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-lpy :requires 'pel-use-python)

(defcustom pel-python-shebang-line "#!/usr/bin/env python3"
  "Default shebang line to add in extension-less Python files."
  :group 'pel-pkg-for-python
  :type 'string)

;; TODO: add support for several Python supporting packages:
;; - elpy           : complete the support
;; - jedi           : https://github.com/tkf/emacs-jedi
;;   - company-jedi : https://github.com/emacsorphanage/company-jedi


;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; REXX Support
;; ------------
(defgroup pel-pkg-for-rexx nil
  "PEL customization for REXX Programming language."
  :group 'pel-pkg-for-software-programming-languages
  :link `(url-link :tag "REXX PDF" ,(pel-pdf-file-url "pl-rexx")))

(defcustom pel-use-rexx nil
  "Control whether PEL supports REXX development."
  :group 'pel-pkg-for-rexx
  :link '(url-link :tag "REXX programming language"
                   "https://en.wikipedia.org/wiki/Rexx")
  :link '(url-link :tag "rexx-mode @ GitHub"
                   "https://github.com/pierre-rouleau/rexx-mode")
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-rexx :package-is '(quote ((utils . rexx-mode)
                                            (utils . rexx-debug))))

(defcustom pel-rexx-activates-minor-modes nil
  "List of *local* minor-modes automatically activated for REXX buffers.
Enter *local* minor-mode activating function symbols.
Do not enter lambda expressions."
  :group 'pel-pkg-for-rexx
  :type '(repeat function))

(defcustom pel-rexx-tab-width 2
  "Column width display rendering of hard tab for rexx buffers.

PEL stores this in `tab-width' when opening rexx  buffers.

This does *NOT* control the indentation in rexx
files, it only controls the column width display rendering of hard tabs
in buffers and tab stop positions for commands such as `tab-to-tab-stop'."
  :group 'pel-pkg-for-rexx
  :type 'integer
  :safe 'pel-indent-valid-p)

(defcustom pel-rexx-use-tabs nil
  "Value of `indent-tabs-mode' for editing rexx files.
- If set to nil: only spaces are used for indentation.
- If set to t: hard tabs are used when possible."
  :group 'pel-pkg-for-rexx
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-use-netrexx nil
  "Control whether PEL supports Net-Rexx development."
  :group 'pel-pkg-for-rexx
  :link '(url-link :tag "NetRexx home" "http://www.netrexx.org/index.nsp")
  :link '(url-link :tag "netrexx-mode.el @ netrexx.org"
                   "http://www.netrexx.org/files/netrexx-mode.el")
  :link '(url-link :tag "Original netrexx-mode.el @ GitHub"
                   "https://github.com/emacsattic/netrexx-mode")
  :link '(url-link :tag "Used netrexx-mode.el @ GitHub"
                   "https://github.com/pierre-rouleau/netrexx-mode")
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-netrexx :package-is '(quote ((utils . netrexx-mode))))

(defcustom pel-netrexx-activates-minor-modes nil
  "List of *local* minor-modes automatically activated for Net-Rexx buffers.
Enter *local* minor-mode activating function symbols.
Do not enter lambda expressions."
  :group 'pel-pkg-for-rexx
  :type '(repeat function))

(defcustom pel-netrexx-tab-width 2
  "Column width display rendering of hard tab for netrexx buffers.

PEL stores this in `tab-width' when opening netrexx  buffers.

This does *NOT* control the indentation in netrexx
files, it only controls the column width display rendering of hard tabs
in buffers and tab stop positions for commands such as `tab-to-tab-stop'."
  :group 'pel-pkg-for-rexx
  :type 'integer
  :safe 'pel-indent-valid-p)

(defcustom pel-netrexx-use-tabs nil
  "Value of `indent-tabs-mode' for editing netrexx files.
- If set to nil: only spaces are used for indentation.
- If set to t: hard tabs are used when possible."
  :group 'pel-pkg-for-rexx
  :type 'boolean
  :safe #'booleanp)

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; Ruby Support
;; ------------
(defgroup pel-pkg-for-ruby nil
  "PEL support for the Ruby programming language."
  :group 'pel-pkg-for-software-programming-languages
  :link `(url-link :tag "Ruby PDF" ,(pel-pdf-file-url "pl-ruby")))

(defcustom pel-use-ruby nil
  "Control whether PEL supports the Ruby programming language.

When turned on the ruby-mode is associated with the PEL ``<f12>`` key."
  :group 'pel-pkg-for-ruby
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-ruby :package-is :a-gate)

(defcustom pel-ruby-activates-minor-modes nil
  "List of *local* minor-modes automatically activated for Ruby buffers.
Enter *local* minor-mode activating function symbols.
Do not enter lambda expressions."
  :group 'pel-pkg-for-ruby
  :type '(repeat function))

(defcustom pel-ruby-tab-width 4
  "Column width display rendering of hard tab for ruby buffers.

PEL stores this in `tab-width' when opening ruby  buffers.

This does *NOT* control the indentation in ruby
files, it only controls the column width display rendering of hard tabs
in buffers and tab stop positions for commands such as `tab-to-tab-stop'."
  :group 'pel-pkg-for-ruby
  :type 'integer
  :safe 'pel-indent-valid-p)

(defcustom pel-ruby-use-tabs nil
  "Value of `indent-tabs-mode' for editing ruby files.
- If set to nil: only spaces are used for indentation.
- If set to t: hard tabs are used when possible."
  :group 'pel-pkg-for-ruby
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-ruby-shebang-line "#!/usr/bin/env ruby"
  "Default shebang line to add in extension-less Ruby files."
  :group 'pel-pkg-for-ruby
  :type 'string)

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; Rust Support
;; ------------
(defgroup pel-pkg-for-rust nil
  "PEL customization for Rust."
  :group 'pel-pkg-for-software-programming-languages
  :link `(url-link :tag "Rust PDF" ,(pel-pdf-file-url "pl-rust")))

(defcustom pel-use-rust  nil
  "Control whether PEL supports Rust development.

This *must* be turned on to allow other packages for Rust.
However it is automatically activated when `pel-use-rust-mode' is activated.
When activating it you can select between the following values:
- t                : use `rust-mode'
- with-tree-sitter : use `rust-ts-mode'"
  :group 'pel-pkg-for-rust
  :type '(choice
          (const :tag "Do not use Rust" nil)
          (const :tag "Use classic mode: rust-mode" t)
          (const :tag "Use tree-sitter mode: rust-ts-mode" with-tree-sitter)))
(pel-put 'pel-use-rust :package-is :a-gate)
(pel-put 'pel-use-rust :also-required-when 'pel-use-rust-mode)

(defcustom pel-use-rust-mode nil
  "Control whether rust-mode is activated.
Requires the user-option variable `pel-use-rust' to be on (t)."
  :link '(url-link :tag "rust-mode @ GitHub"
                   "https://github.com/rust-lang/rust-mode")
  :group 'pel-pkg-for-rust
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-rust-mode :requires 'pel-use-rust)
(when pel-use-rust-mode
  (unless pel-use-rust
    (setq pel-use-rust t)))

(defcustom pel-rust-activates-minor-modes nil
  "List of *local* minor-modes automatically activated for Rust buffers.
Enter *local* minor-mode activating function symbols.
Do not enter lambda expressions."
  :group 'pel-pkg-for-rust
  :type '(repeat function))

(defcustom pel-rust-use-tabs nil
  "Value of `indent-tabs-mode' for editing rust files.
- If set to nil: only spaces are used for indentation.
- If set to t: hard tabs are used when possible."
  :group 'pel-pkg-for-rust
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-use-rustic nil
  "Control whether rustic is activated.
Requires the user-option variable `pel-use-rust' to be on (t)."
  :link '(url-link :tag "rustic @ GitHub"
                   "https://github.com/brotzeit/rustic")
  :group 'pel-pkg-for-rust
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-rustic :requires 'pel-use-rust)

(defcustom pel-use-flycheck-rust nil
  "Control whether flycheck-rust is activated.
Requires the user-option variable `pel-use-rust' to be on (t)."
  :link '(url-link :tag "flycheck-rust @ GitHub"
                   "https://github.com/flycheck/flycheck-rust")
  :group 'pel-pkg-for-rust
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-flycheck-rust :requires '(:all pel-use-rust
                                                 pel-use-rust-mode))

(defcustom pel-use-emacs-racer nil
  "Control whether emacs-racer is activated.
Requires the user-option variable `pel-use-rust' to be on (t)."
  :link '(url-link :tag "Emacs racer @ GitHub"
                   "https://github.com/racer-rust/emacs-racer")
  :group 'pel-pkg-for-rust
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-emacs-racer :package-is 'racer)
(pel-put 'pel-use-emacs-racer :requires 'pel-use-rust)

(defcustom pel-use-cargo nil
  "Control whether cargo is activated.
Requires the user-option variable `pel-use-rust' to be on (t)."
  :link '(url-link :tag "cargo.el @ GitHub"
                   "https://github.com/kwrooijen/cargo.el")
  :group 'pel-pkg-for-rust
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-cargo :requires 'pel-use-rust)

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; Unix Shell Scripting Support
;; ----------------------------
(defgroup pel-pkg-for-sh-scripting nil
  "PEL support for Unix shell scripting."
  :group 'pel-pkg-for-software-programming-languages)

(defcustom pel-use-sh nil
  "Control whether PEL supports the Shell programming language.

When turned on the shell-mode is associated with the PEL ``<f12>`` key."
  :group 'pel-pkg-for-sh-scripting
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-sh :package-is :a-gate)

(defcustom pel-use-flymake-shellcheck nil
  "Control whether flymake-shellcheck is activated.
The flymake-shellcheck package provides interactive checking of shell script
code using the flymake backend.

NOTE: THIS IS OBSOLETE and will be removed eventually.
      Please use `pel-use-shellcheck'."
  :group 'pel-pkg-for-sh-scripting
  :link '(url-link :tag "flymake-shellcheck @Github"
                   https://github.com/federicotdn/flymake-shellcheck)
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-flymake-shellcheck :requires 'pel-use-sh)

(defcustom pel-use-shellcheck nil
  "Control whether PEL use shellcheck for shell script syntax checking.

This can be set to one of the following values:"
  :group 'pel-pkg-for-sh-scripting
  :link '(url-link :tag "flymake-shellcheck @Github"
                   https://github.com/federicotdn/flymake-shellcheck)
  :type
  '(choice
    (const :tag "No shellcheck support." nil)
    (const :tag "Use flymake-shellcheck. Manually activated." flymake-manual)
    (const :tag "Use flymake-shellcheck automatically." flymake-automatic)
    (const :tag "Use flycheck-shellcheck. Manually activated." flycheck-manual)
    (const :tag "Use flycheck-shellcheck automatically." flycheck-automatic)))

(defcustom pel-sh-activates-minor-modes nil
  "List of *local* minor-modes automatically activated for Shell buffers.
Enter *local* minor-mode activating function symbols.
Do not enter lambda expressions."
  :group 'pel-pkg-for-sh-scripting
  :type '(repeat function))

(defcustom pel-sh-tab-width 2
  "Column width display rendering of hard tab for sh buffers.

PEL stores this in `tab-width' when opening sh buffers.

This controls the number of space characters used in for
indentation level in sh files when the `pel-sh-use-tabs' is set
to nil. It also controls the number of columns shown to render a
hard tab when one `pel-sh-use-tabs' is set to t."
  :group 'pel-pkg-for-sh-scripting
  :type 'integer
  :safe 'pel-indent-valid-p)

(defcustom pel-sh-use-tabs nil
  "Value of `indent-tabs-mode' for editing sh files.
- If set to nil: only spaces are used for indentation.
- If set to t: hard tabs are used when possible."
  :group 'pel-pkg-for-sh-scripting
  :type 'boolean
  :safe #'booleanp)

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; Seed7 Language Support
;; ----------------------
(defgroup pel-pkg-for-seed7 nil
  "PEL Seed7 language support."
  :group 'pel-pkg-for-software-programming-languages
  :link `(url-link :tag "Seed7 PDF" ,(pel-pdf-file-url "pl-seed7")))

(defcustom pel-use-seed7 nil
  "Control whether PEL supports the Seed7 Programming Language Development."
  :group 'pel-pkg-for-seed7
  :link '(url-link :tag "seed7-mode @ Github"
                   "https://github.com/pierre-rouleau/seed7-mode")
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-seed7 :package-is '(quote ((utils . seed7-mode))))

(defcustom pel-seed7-activates-minor-modes nil
  "List of *local* minor-modes automatically activated for Seed7 buffers.
Enter *local* minor-mode activating function symbols.
Do not enter lambda expressions."
  :group 'pel-pkg-for-seed7
  :type '(repeat function))

;; The seed-mode fully controls indentation and tab-width
;; via the `seed7-indent-width' user-option.
;; Therefore, there is no need for a `pel-seed7-tab-width'
;; and a `pel-seed7-use-tabs' user options.
;;
;; The `pel--tab-controlling-major-modes' list includes 'seed7
;; to prevent PEL from generating mode setup code that would use
;; PEL specific user options for indent-with and tab control.
;;

(defcustom pel-seed7-shebang-line "#!/usr/bin/env -S s7 -q"
  "Default shebang line to add in extension-less Seed7 script files."
  :group 'pel-pkg-for-seed7
  :type 'string)

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; Smalltalk Language Support
;; --------------------------
(defgroup pel-pkg-for-smalltalk nil
  "PEL Smalltalk language support."
  :group 'pel-pkg-for-software-programming-languages
  :link `(url-link :tag "Smalltalk PDF" ,(pel-pdf-file-url "pl-smalltalk")))

(defcustom pel-use-smalltalk nil
  "Control whether PEL supports the smalltalk-mode for GNU Smalltalk language."
  :link '(url-link :tag "smalltalk-mode @ GNU elpa"
                   "https://elpa.gnu.org/packages/smalltalk-mode.html")
  :group 'pel-pkg-for-smalltalk
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-smalltalk :package-is '(quote ((elpa . smalltalk-mode))))

(defcustom pel-smalltalk-activates-minor-modes nil
  "List of *local* minor-modes automatically activated for Smalltalk buffers.
Enter *local* minor-mode activating function symbols.
Do not enter lambda expressions."
  :group 'pel-pkg-for-smalltalk
  :type '(repeat function))

(defcustom pel-smalltalk-tab-width 4
  "Column width display rendering of hard tab for smalltalk buffers.

PEL stores this in `tab-width' when opening smalltalk buffers.

This does *NOT* control the indentation in smalltalk
files, it only controls the column width display rendering of hard tabs
in buffers and tab stop positions for commands such as `tab-to-tab-stop'."
  :group 'pel-pkg-for-smalltalk
  :type 'integer
  :safe 'pel-indent-valid-p)

(defcustom pel-smalltalk-use-tabs nil
  "Value of `indent-tabs-mode' for editing smalltalk files.
- If set to nil: only spaces are used for indentation.
- If set to t: hard tabs are used when possible."
  :group 'pel-pkg-for-smalltalk
  :type 'boolean
  :safe #'booleanp)

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; Swift Language Support
;; -----------------------
(defgroup pel-pkg-for-swift nil
  "PEL Swift language support."
  :group 'pel-pkg-for-software-programming-languages
  :link `(url-link :tag "Swift PDF" ,(pel-pdf-file-url "pl-swift")))

(defcustom pel-use-swift nil
  "Control whether PEL supports the Swift Programming Language Development."
  :group 'pel-pkg-for-swift
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-swift :package-is '(if pel-use-tree-sitter
                                         (quote ((elpa . swift-mode)
                                                 (elpa . swift-ts-mode)))
                                       (quote ((elpa . swift-mode)))))


(defcustom pel-swift-activates-minor-modes nil
  "List of *local* minor-modes automatically activated for Swift buffers.
Enter *local* minor-mode activating function symbols.
Do not enter lambda expressions."
  :group 'pel-pkg-for-swift
  :type '(repeat function))

(defcustom pel-swift-tab-width 4
  "Column width display rendering of hard tab for swift buffers.

PEL stores this in `tab-width' when opening swift buffers.

This does *NOT* control the indentation in swift
files, it only controls the column width display rendering of hard tabs
in buffers and tab stop positions for commands such as `tab-to-tab-stop'."
  :group 'pel-pkg-for-swift
  :type 'integer
  :safe 'pel-indent-valid-p)

(defcustom pel-swift-use-tabs nil
  "Value of `indent-tabs-mode' for editing swift files.
- If set to nil: only spaces are used for indentation.
- If set to t: hard tabs are used when possible."
  :group 'pel-pkg-for-swift
  :type 'boolean
  :safe #'booleanp)

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; Tcl  Language Support
;; ---------------------
(defgroup pel-pkg-for-tcl nil
  "PEL Tcl language support."
  :group 'pel-pkg-for-software-programming-languages)

(defcustom pel-use-tcl nil
  "Control whether PEL supports the Tcl programming language."
  :group 'pel-pkg-for-tcl
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-tcl :package-is :builtin-emacs)

(defcustom pel-tcl-activates-minor-modes nil
  "List of *local* minor-modes automatically activated for Tcl buffers.
Enter *local* minor-mode activating function symbols.
Do not enter lambda expressions."
  :group 'pel-pkg-for-tcl
  :type '(repeat function))

(defcustom pel-tcl-indent-width 4
  "Number of columns for Tcl source code indentation.
Values in the [2, 8] range are accepted.

EXPERIMENTAL: this may be removed once I know more about Tcl support."
  :group 'pel-pkg-for-tcl
  :type 'integer
  :safe 'pel-indent-valid-p)

(defcustom pel-tcl-tab-width 4
  "Column width display rendering of hard tab for Tcl buffers.

PEL stores this in `tab-width' when opening Tcl buffers.

This does *NOT* control the indentation in Tcl files, only for
commands that mode point to tab stop positions such as
`tab-to-tab-stop', and the display of hard TAB characters."
  :group 'pel-pkg-for-tcl
  :type 'integer
  :safe 'pel-indent-valid-p)

(defcustom pel-tcl-use-tabs nil
  "Value of `indent-tabs-mode' for editing Tcl files.
- If set to nil: only spaces are used for indentation.
- If set to t: hard tabs are used when possible."
  :group 'pel-pkg-for-tcl
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-tcl-shebang-line "#!/usr/bin/env tclsh"
  "Default shebang line to add in extension-less Tcl files."
  :group 'pel-pkg-for-tcl
  :type 'string)

(defcustom pel-tcl-expect-shebang-line "#!/usr/bin/env expect"
  "Default shebang line to add in extension-less Tcl expect files."
  :group 'pel-pkg-for-tcl
  :type 'string)

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; V Language Support
;; ------------------
(defgroup pel-pkg-for-v nil
  "PEL V language support. Experimental."
  :group 'pel-pkg-for-software-programming-languages
  :link '(url-link :tag "V Language home page" "https://vlang.io"))


(defcustom pel-use-v nil
  "Control whether PEL supports V Programming Language Development.

When set, identifies what mode is used to support V.

NOTE:
 - The vlang-mode has minimal support: font locking only.
 - The v-mode is more mature and is recommended."
  :group 'pel-pkg-for-v
  :type '(choice
          (const :tag "No support."  nil)
          (const
           :tag "Supported by vlang-mode minimal/experimental package."
           vlang-mode)
          (const :tag "Supported by v-mode, a more mature mode." v-mode))
  :link '(url-link :tag "v-mode @ GitHub"
                   "https://github.com/damon-kwok/v-mode")
  :link '(url-link :tag "v-mode @ MELPA"
                   "https://melpa.org/#/v-mode")
  :link '(url-link :tag "vlang-mode @ GitHub"
                   "https://github.com/pierre-rouleau/vlang-mode"))
(pel-put 'pel-use-v :package-is '(cond ((eq pel-use-v 'v-mode)
                                        '((elpa . v-mode)))
                                       ((eq pel-use-v 'vlang-mode)
                                        '((utils . vlang-mode)))))

(defcustom pel-v-activates-minor-modes nil
  "List of *local* minor-modes automatically activated for V buffers.
Enter *local* minor-mode activating function symbols.
Do not enter lambda expressions."
  :group 'pel-pkg-for-v
  :type '(repeat function))

(defcustom pel-v-tab-width 4
  "Column width display rendering of hard tab for v buffers.

PEL stores this in `tab-width' when opening v  buffers.

This does *NOT* control the indentation in v
files, it only controls the column width display rendering of hard tabs
in buffers and tab stop positions for commands such as `tab-to-tab-stop'."
  :group 'pel-pkg-for-v
  :type 'integer
  :safe 'pel-indent-valid-p)

(defcustom pel-v-use-tabs nil
  "Value of `indent-tabs-mode' for editing v files.
- If set to nil: only spaces are used for indentation.
- If set to t: hard tabs are used when possible."
  :group 'pel-pkg-for-v
  :type 'boolean
  :safe #'booleanp)

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; Zig  Language Support
;; ---------------------

(defgroup pel-pkg-for-zig nil
  "PEL Zig language support."
  :group 'pel-pkg-for-software-programming-languages
  :link '(url-link :tag "Zig Language home page" "https://ziglang.org/"))

(defcustom pel-use-zig nil
  "Control whether PEL supports the Zig Programming Language Development."
  :group 'pel-pkg-for-zig
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-zig :package-is '(if pel-use-tree-sitter
                                       (quote ((elpa . zig-mode)
                                               (elpa . zig-ts-mode)))
                                     (quote ((elpa . zig-mode)))))

(defcustom pel-zig-activates-minor-modes nil
  "List of *local* minor-modes automatically activated for Zig buffers.
Enter *local* minor-mode activating function symbols.
Do not enter lambda expressions."
  :group 'pel-pkg-for-zig
  :type '(repeat function))

(defcustom pel-zig-tab-width 4
  "Column width display rendering of hard tab for zig buffers.

PEL stores this in `tab-width' when opening zig buffers.

This does *NOT* control the indentation in zig
files, it only controls the column width display rendering of hard tabs
in buffers and tab stop positions for commands such as `tab-to-tab-stop'."
  :group 'pel-pkg-for-zig
  :type 'integer
  :safe 'pel-indent-valid-p)

(defcustom pel-zig-use-tabs nil
  "Value of `indent-tabs-mode' for editing zig files.
- If set to nil: only spaces are used for indentation.
- If set to t: hard tabs are used when possible."
  :group 'pel-pkg-for-zig
  :type 'boolean
  :safe #'booleanp)

;; ---------------------------------------------------------------------------
;; Project Manager Support
;; =======================
(defgroup pel-pkg-for-project-mng nil
  "PEL customization for project managers."
  :group 'pel-package-use
  :link `(url-link :tag "Projectile PDF" ,(pel-pdf-file-url "projectile")))

(defcustom pel-use-find-file-in-project nil
  "Control whether PEL supports the find-file-in-project package.

CAUTION: This package needs major tuning!  It takes forever searching for a
         project.  For the moment, Projectile is MUCH better!"
  :link '(url-link :tag "find-file-in-project @ Github"
                   "https://github.com/redguardtoo/find-file-in-project")
  :group 'pel-pkg-for-project-mng
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-find-file-in-project :package-is :in-utils)

(defcustom pel-use-projectile nil
  "Control whether PEL supports the projectile project manager."
  :group 'pel-pkg-for-project-mng
  :type '(choice
          (const :tag "Do not use" nil)
          (const :tag "Use, activate later by command"  t)
          (const :tag "Use, activate when Emacs starts" use-from-start)))
(pel-put 'pel-use-projectile :also-required-when '(or pel-use-projectile-speedbar
                                                      pel-use-treemacs-projectile))

(defcustom pel-project-root-identifiers '(".git" ".hg" ".projectile" ".pel-project")
  "File names that identify the root of a project directory tree."
  :group 'pel-pkg-for-project-mng
  :type '(repeat string))

;; ---------------------------------------------------------------------------
;; pel-pkg-for-regexp
;; ------------------
(defgroup pel-pkg-for-regexp nil
  "List of external packages that PEL can use for regular expressions."
  :group 'pel-package-use
  :link `(url-link :tag "Search/Replace PDF"
                   ,(pel-pdf-file-url "search-replace")))

(defcustom pel-bind-keys-for-regexp nil
  "If set to t, PEL binds several keys in the C-c prefix.
It binds:
- \\='C-c r\\=' : to replace-regexp or pel-replace-regexp
- \\='C-c q\\=' : to query-replace-regexp or pel-query-replace-regexp
- \\='C-c Q\\=' : to isearch-query-replace-regexp"
  :group 'pel-pkg-for-regexp
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-initial-regexp-engine 'emacs
  "Select the search/replace regexp engine used when Emacs starts.
Select one that is available according to the package installed.
The possible choices are:
- \\='emacs             : plain emacs
- \\='pel-vr            : visual-regexp
- \\='pel-vr/emacs      : visual-regexp-steroids emacs
- \\='pel-vr/emacs-plain: visual-regexp-steroids emacs-plain
- \\='pel-vr/pcre2el    : visual-regexp-steroids pcre2el
- \\='pel-vr/python     : visual-regexp-steroids python
- \\='pel-vr/custom     : visual-regexp-steroids custom

The first choice is the default.  The other choices
can be made only if `pel-use-visual-regexp' is t (for the second)
or pel-use-regexp-steroids is t (for the others)."
  :group 'pel-pkg-for-regexp
  :type '(choice
          (const :tag "Use Emacs default" emacs)
          (const :tag "Use visual-regexp." vr)
          (const :tag "Use visual-regexp-steroids emacs.      " vr/emacs)
          (const :tag "Use visual-regexp-steroids emacs-plain." vr/emacs-plain)
          (const :tag "Use visual-regexp-steroids pcre2el.    " vr/pcre2el)
          (const :tag "Use visual-regexp-steroids python.     " vr/python)
          (const :tag "Use visual-regexp-steroids custom.     " vr/custom)))

(defcustom pel-use-regex-tool nil
  "Control whether PEL uses the external `regex-tool' library."
  :link '(url-link :tag "regex-tool @ GitHub"
                   "https://github.com/jwiegley/regex-tool")
  :group 'pel-pkg-for-regexp
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-use-pcre2el nil
  "Control whether PEL uses the external pcre2el library."
  :link '(url-link :tag "pcre2el @ GitHub" "https://github.com/joddie/pcre2el")
  :group 'pel-pkg-for-regexp
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-use-visual-regexp nil
  "Control whether PEL uses the external visual-regexp library.

This is indirectly activated by `pel-use-visual-regexp-steroids' user-option."
  :link '(url-link :tag "visual-regexp @ GitHub"
                   "https://github.com/benma/visual-regexp.el")
  :group 'pel-pkg-for-regexp
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-visual-regexp :also-required-when
         'pel-use-visual-regexp-steroids)

(defcustom pel-use-visual-regexp-steroids nil
  "Control whether PEL uses the external visual-regexp-steroids library."
  :link '(url-link :tag "visual-regexp-steroids @ GitHub"
                   "https://github.com/benma/visual-regexp-steroids.el")
  :group 'pel-pkg-for-regexp
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-use-xr nil
  "Control whether PEL uses the external xr library.

xr : Emacs regexp parser and analyser.
Use it to describe regular expressions using lisp forms.
PEL provide xr commands bound to the ``<f11> s x`` key prefix."
  :link '(url-link :tag "xr @ elpa" "https://elpa.gnu.org/packages/xr.html")
  :group 'pel-pkg-for-regexp
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-use-easy-escape nil
  "Control whether PEL uses the easy-escape package.

Once activated, you can customize it further via the easy-escape
customization group."
  :link '(custom-group-link "easy-escape")
  :link '(url-link :tag "easy-escape @ GitHub"
                   "https://github.com/cpitclaudel/easy-escape")
  :group 'pel-pkg-for-regexp
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-modes-activating-easy-escape nil
  "List of major modes that automatically activate easy-escape minor mode.
Prime candidates for this would be the following:
- `prog-mode        : to activate it on all programming language modes,
- `emacs-lisp-mode  : to activate for Emacs Lisp code,
- `lisp-mode        : to activate for Lisp mode.

The minor mode can also be activated manually using the
command `easy-escape-minor-mode'."
  :group 'pel-pkg-for-regexp
  :type  '(repeat symbol))

(defcustom pel-use-relint nil
  "Controls whether PEL uses the relint package.

The relint package is a regexp lint, with commands that verify the regular
expressions of buffer, file, or directory."
  :link '(url-link :tag "relint @ GitHub"
                   "https://github.com/mattiase/relint")
  :group 'pel-pkg-for-regexp
  :type 'boolean
  :safe #'booleanp)

;; ---------------------------------------------------------------------------
;; Scrolling Control
;; -----------------
(defgroup pel-pkg-for-scrolling nil
  "PEL window scrolling control."
  :group 'pel-package-use
  :group 'pel-pkg-for-window
  :link `(url-link :tag "Windows PDF" ,(pel-pdf-file-url "scrolling")))

(defcustom pel-use-smooth-scrolling nil
  "Control whether PEL provides the smooth-scrolling capability."
  :group 'pel-pkg-for-scrolling
  :type 'boolean
  :safe #'booleanp
  :link '(url-link :tag "smooth-scrolling @ GitHub"
                   "https://github.com/aspiers/smooth-scrolling/"))

;; ---------------------------------------------------------------------------
;; pel-pkg-for-search
;; ------------------
(defgroup pel-pkg-for-search nil
  "List of external packages that PEL can use for searching text."
  :group 'pel-package-use
  :link `(url-link :tag "Search/Replace PDF"
                   ,(pel-pdf-file-url "search-replace")))

(defcustom pel-search-from-top-in-other nil
  "Force function `pel-search-word-from-top' search in other of 2 windows.

If set to t, the function `pel-search-word-from-top' search in
the other window if there are only 2 non-dedicated window by
default.  To force it to search in the current buffer the numeric
argument of 3 or 5 must be specified.

If set to nil, the function `pel-search-word-from-top' search in
the current buffer when no numeric argument is specified,
regardless of the number of non-dedicated windows in the current
frame.  To search in the other window you must use a numeric
argument (0 identifies the other window, but you can also specify
the window by its position with the other numbers)."
  :group 'pel-pkg-for-search
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-use-anzu nil
  "Control whether PEL uses the Anzu."
  :group 'pel-pkg-for-search
  :link '(url-link :tag "Anzu" "https://melpa.org/#/anzu'")
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-use-cexp nil
  "Control whether PEL uses cexp, combined expression search."
  :group 'pel-pkg-for-search
  :link '(url-link :tag "cexp" "https://github.com/TobiasZawada/cexp")
  :link '(url-link :tag "emacs regex to match balanced parenthesis"
                   "https://emacs.stackexchange.com/questions/45387/\
emacs-regex-to-match-balanced-parenthesis")
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-cexp :package-is '(quote ((utils . cexp)
                                            (utils . cexp-test))))

(defcustom pel-use-swiper nil
  "Control whether PEL uses the Swiper search package."
  :group 'pel-pkg-for-search
  :link '(url-link :tag "Swiper" "https://github.com/abo-abo/swiper#swiper")
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-initial-search-tool nil
  "Select the search tool used when Emacs starts.
PEL supports the following tools:
- nil     : use Emacs default.
- `anzu'  : use iSearch with Anzu globally to show match counts in modeline.
- `swiper': use Swiper to show search matches list in minibuffer."
  :group 'pel-pkg-for-search
  :type '(choice
          (const :tag "Use Emacs default" nil)
          (const :tag "Use Anzu" anzu)
          (const :tag "Use Swiper" swiper)))

(defcustom pel-use-isearch-mb nil
  "Control whether PEL uses isearch-mb."
  :group 'pel-pkg-for-search
  :link '(url-link :tag "isearch-mb" "https://github.com/astoff/isearch-mb")
  :type '(choice
          (const :tag "Do not use" nil)
          (const :tag "Use, activate later by command"  t)
          (const :tag "Use, activate globally when Emacs starts"
                 use-from-start)))

;; ---------------------------------------------------------------------------
;; pel-pkg-for-session
;; -------------------
(defgroup pel-pkg-for-session nil
  "PEL window session management."
  :group 'pel-package-use
  :group 'pel-pkg-for-window
  :link `(url-link :tag "Sessions PDF" ,(pel-pdf-file-url "sessions"))
  :link '(custom-manual "(emacs)Saving Emacs Sessions"))

(defcustom pel-use-desktop nil
  "Control whether desktop feature is used for session management.

When session management controlled by desktop feature, then
identify whether the built-in desktop.el is used alone or whether
one of the desktop-registry or desktop+ is also used.

The value can be:

- nil : nothing is used.
- t:                             Use built-in desktop but do NOT
                                 activate the desktop save mode.
- `with-desktop-automatic':      Use built-in desktop and
                                 activate desktop save mode.
- `with-desktop-registry':       Use desktop and the desktop-registry
                                 external package.
- `with-desktop-registry-automatic': Use desktop, the desktop-registry
                                     and activate desktop auto-save mode.
- `with-desktop+':               Use desktop and the desktop+ external package.
                                 *Recommended* for new users."
  :group 'pel-pkg-for-session
  :type '(choice
          (const :tag "Not used" nil)
          (const :tag "Use built-in desktop - do NOT activate \
desktop-save-mode" t)
          (const :tag "Use built-in desktop and ACTIVATE desktop-save-mode"
                 with-desktop-automatic)
          (const :tag "Use desktop with desktop-registry \
- do NOT activate desktop-save-mode " with-desktop-registry)
          (const :tag "Use desktop with desktop-registry \
and ACTIVATE desktop-save-mode" with-desktop-registry-automatic)
          (const :tag "Use desktop with desktop+" with-desktop+)))
(pel-put 'pel-use-desktop :package-is
         '(cond ((memq pel-use-desktop '(t with-desktop-automatic))
                 nil)
                ((memq pel-use-desktop '(with-desktop-registry
                                         with-desktop-registry-automatic))
                 '((elpa . desktop-registry)))
                ((eq pel-use-desktop 'with-desktop+)
                 '((elpa . desktop+)))))

;; desktop  user options:
;; - desktop-save-mode
;; - desktop-restore-frames
;; - desktop-files-not-to-save
;; - frameset-filter-alist
;; - desktop-path
;; - desktop-restore-eager
;; - desktop-globals-to-clear
;; - desktop-clear-preserve-buffers-regexp
;; - desktop-auto-save-timeout
;; - desktop-load-locked-desktop

;; ---------------------------------------------------------------------------
;; Shell & Terminal Support
;; ------------------------
(defgroup pel-pkg-for-shells nil
  "List of external packages that PEL can use to support shells and terminals."
  :group 'pel-package-use
  :link `(url-link :tag "Shells PDF" ,(pel-pdf-file-url "shells")))

(defcustom pel-shell-activates-minor-modes nil
  "List of *local* minor-modes automatically activated for *shell* buffers.

Enter *local* minor-mode activating function symbols.
Do not enter lambda expressions."
  :group 'pel-pkg-for-shells
  :type '(repeat function))


(defcustom pel-shell-prompt-line-regexp "^>"
  "A regexp used to search for the shell prompt.

The regexp must identify (at the very least) the *end* of the prompt,
to allow placing the point at the very first character where a command was or
will be typed. "
  :group 'pel-pkg-for-shells
  :type 'string)

;; ------------------------------
;; PEL-specific term-mode support
;; ------------------------------
(defgroup pel-pkg-for-term-mode nil
  "PEL-specific customization for the `term-mode'."
  :group 'pel-pkg-for-shells
  :link `(url-link :tag "term-mode PDF" ,(pel-pdf-file-url "term-mode")))

(defcustom pel-term-activates-minor-modes nil
  "List of *local* minor-modes automatically activated for *term* buffers.

Also used for *ansi-term* buffers.
Enter *local* minor-mode activating function symbols.
Do not enter lambda expressions."
  :group 'pel-pkg-for-shells
  :type '(repeat function))

(defcustom pel-term-use-shell-prompt-line-regexp t
  "When t, term-prompt-regexp set to pel-shell-prompt-line-regexp value."
  :type 'boolean
  :safe #'booleanp)

;; -------------------------------
;; PEL-specific vterm-mode support
;; -------------------------------

(defcustom pel-use-vterm nil
  "Control whether the vterm shell is available.
The vterm package used the libvterm library to provide a very fast
and usable shell for Emacs.
It requires Emacs 25.1 or later built with module support!"
  :group 'pel-pkg-for-shells
  :type 'boolean
  :safe #'booleanp)

(defgroup pel-pkg-for-vterm-mode nil
  "PEL-specific customization for the `vterm-mode'."
  :group 'pel-pkg-for-shells
  :link `(url-link :tag "vterm-mode PDF" ,(pel-pdf-file-url "vterm-mode")))

(defcustom pel-vterm-supports-f12-keys t
  "Whether the F12 key prefix for PEL commands is available in vterm-mode."
  :group 'pel-pkg-for-vterm-mode
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-vterm-supports-f12-navkeys t
  "Whether F12 keys for PEL navigation commands are available in vterm-mode.

To activate it the `pel-vterm-supports-f12-keys' must also be activated."
  :group 'pel-pkg-for-vterm-mode
  :type 'boolean
  :safe #'booleanp)

;; -------------------------------
;; PEL-specific emacs-eat  support
;; -------------------------------

(defcustom pel-use-emacs-eat nil
  "Control whether the emacs-eat is available.
The emacs-eat (Emulate A Terminal) is a flexible terminal
with 4 input modes.
It requires Emacs 26.1 or later."
  :link '(url-link :tag "emacs-eat @ codedeberg"
                   "https://codeberg.org/akib/emacs-eat")
  :group 'pel-pkg-for-shells
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-emacs-eat :package-is '(quote ((elpa . eat))))

(defgroup pel-pkg-for-eat-mode nil
  "PEL-specific customization for the `eat-mode'."
  :group 'pel-pkg-for-shells
  :link `(url-link :tag "eat-mode PDF" ,(pel-pdf-file-url "eat-mode")))

(defcustom pel-emacs-eat-activates-minor-modes nil
  "List of *local* minor-modes automatically activated for *eat* buffers.

Enter *local* minor-mode activating function symbols.
Do not enter lambda expressions."
  :group 'pel-pkg-for-eat-mode
  :type '(repeat function))

;; ---------------------------------------------------------------------------
;; pel-pkg-for-skeletons
;; ---------------------
(defgroup pel-pkg-for-skeletons nil
  "Holds options for controlling skeleton code generation."
  :group 'pel-package-use
  :group 'pel-pkg-for-insertions)
;; The children of that group are scattered in the respective programming or
;; markup language code sections. Keep this group here: a group may have
;; several parent groups and all skeleton control groups refer to this one as
;; their parent.

;; ---------------------------------------------------------------------------
;; pel-pkg-for-spec-definition
;; ---------------------------
(defgroup pel-pkg-for-spec-definition nil
  "PEL Specification Definition Languages."
  :group 'pel-package-use)

(defcustom pel-use-asn1-mode nil
  "Whether PEL provides access to the asn1-mode external package."
  :group 'pel-pkg-for-spec-definition
  :type 'boolean
  :safe #'booleanp
  :link '(url-link :tag "asn1-mode @ Github"
                   "https://github.com/kawabata/asn1-mode"))

(defcustom pel-asn1-activates-minor-modes nil
  "List of *local* minor-modes automatically activated for ASN.1 buffers.
Enter *local* minor-mode activating function symbols.
Do not enter lambda expressions."
  :group 'pel-pkg-for-spec-definition
  :type '(repeat function))

(defcustom pel-use-yang-mode nil
  "Whether PEL provides access to the yang-mode external package."
  :group 'pel-pkg-for-spec-definition
  :type 'boolean
  :safe #'booleanp
  :link '(url-link :tag "yang-mode @ Github"
                   "https://github.com/mbj4668/yang-mode"))

(defcustom pel-yang-activates-minor-modes nil
  "List of *local* minor-modes automatically activated for Yang buffers.
Enter *local* minor-mode activating function symbols.
Do not enter lambda expressions."
  :group 'pel-pkg-for-spec-definition
  :type '(repeat function))

(defcustom pel-yang-tab-width 2
  "Column width display rendering of hard tab for yang buffers.

PEL stores this in `tab-width' when opening yang  buffers.

This does *NOT* control the indentation in yang
files, it only controls the column width display rendering of hard tabs
in buffers and tab stop positions for commands such as `tab-to-tab-stop'."
  :group 'pel-pkg-for-yang
  :type 'integer
  :safe 'pel-indent-valid-p)

(defcustom pel-yang-use-tabs nil
  "Value of `indent-tabs-mode' for editing yang files.
- If set to nil: only spaces are used for indentation.
- If set to t: hard tabs are used when possible."
  :group 'pel-pkg-for-yang
  :type 'boolean
  :safe #'booleanp)

;; ---------------------------------------------------------------------------
;; pel-pkg-for-speedbar
;; --------------------
(defgroup pel-pkg-for-speedbar nil
  "PEL Speedbar management."
  :group 'pel-package-use
  :group 'pel-pkg-for-navigation
  :group 'pel-pkg-for-window
  :group 'speedbar
  :link '(custom-group-link speedbar)
  :link '(custom-group-link speedbar-vc)
  :link '(custom-group-link speedbar-faces)
  :link `(url-link :tag "Speedbar PDF" ,(pel-pdf-file-url "speedbar"))
  :link '(custom-manual "(emacs)Speedbar")
  :link '(url-link :tag "Speedbar @ CEDET"
                   "http://cedet.sourceforge.net/speedbar.shtml"))

(defcustom pel-use-speedbar nil
  "Control whether PEL uses the Speedbar and SR-Speedbar packages.

When set, PEL activates the keys that you can use to toggle the
speedbar window on and off.  In terminal mode the SR-Speedbar is
used if `pel-prefer-sr-speedbar-in-terminal' is set."
  :group 'pel-pkg-for-speedbar
  :type 'boolean
  :safe #'booleanp)
;; speedbar is built-in Emacs but when `pel-use-speedbar' is active
;; sr-speedbar is installed.
(pel-put 'pel-use-speedbar :package-is '(quote ((utils . sr-speedbar))))
(pel-put 'pel-use-speedbar :also-required-when 'pel-use-projectile-speedbar)

(defcustom pel-prefer-sr-speedbar-in-terminal t
  "Prefer using Sr-Speedbar in terminal mode (when available) over Speedbar."
  :group 'pel-pkg-for-speedbar
  :type  'boolean
  :safe  #'booleanp
  :link '(url-link :tag "SR-Speedbar @ EmacsWiki"
                   "https://www.emacswiki.org/emacs/SrSpeedbar"))

(defcustom pel-sr-speedbar-move-point-to-target-on-select t
  "When on, a SR-Speedback select moves point to the target buffer window.
Otherwise it leaves point inside the SR-Speedbar buffer window.
This behaviour can be modified dynamically by the command
`pel-sr-speedbar-toggle-select-behaviour'."
  :group 'pel-pkg-for-speedbar
  :type  'boolean
  :safe  #'booleanp)

(defcustom pel-use-projectile-speedbar nil
  "Control whether PEL uses projectile-speedbar package.
This provides the command `pel-speedbar-focus-current-file'.
Setting this non-nil also sets up the use of speedbar and projectile."
  :group 'pel-pkg-for-speedbar
  :type  'boolean
  :safe  #'booleanp
  :link '(custom-group-link "pel-pkg-for-project-mng")
  :link '(url-link :tag "projectile + speedbar @ GitHub"
                   "https://github.com/anshulverma/projectile-speedbar"))
;; (pel-put 'pel-use-projectile-speedbar :requires '(:all
;;                                                   pel-use-speedbar
;;                                                   pel-use-projectile))

;; ---------------------------------------------------------------------------
;; Spelling Support
;; ----------------

;; '(pel-spell-check-tools
;;    (quote
;;     ((gnu "aspell" "~/.emacs.d/.ispell")
;;      (gnu/linux "aspell" "~/.emacs.d/.ispell")
;;      (darwin "aspell" "~/.emacs.d/.ispell")
;;      (windows-nt "c:/pg/aspell/0.50.3-w32/bin/aspell.exe" ""))))


(defgroup pel-pkg-for-spelling nil
  "PEL Spelling Support."
  :group 'pel-package-use
  :link `(url-link :tag "Spell Checking PDF"
                   ,(pel-pdf-file-url "spell-checking")))


(defcustom pel-spell-prevent-flyspell nil
  "Prevent automatic activation of flyspell-mode and flyspell-prog-mode.

When this is non-nil, it prevents the automatic activation of
flyspell-mode and flyspell-prog-mode in the major modes where
they would be automatically activated by hooks set to
`pel-spell-maybe-activate-flyspell' and
`pel-spell-maybe-activate-flyspell-prog'.

If the value is nil (the default), then activation of automated
spell checking is allowed.

This mechanism helps when you want to reduce the CPU load and also when you
want to debug spell checking mechanism and don't want it to be triggered
every time you open a new buffer.

Toggled this dynamically with the `pel-spell-toggle-prevent-flyspell'
command."
  :group 'pel-pkg-for-spelling
  :type  'boolean
  :safe  #'booleanp)

(defcustom pel-spell-check-tool nil
  "Spell Checking tool program.

The spell-checking tool must be an ispell-compatible program.

On Unix-like OS you normally identify that program by its name, as the
program's executable file will normally be available on the PATH available
when Emacs runs.  Then select the second option and identify just the name
of the ispell-compatible program.  Something like:

- ispell
- aspell
- hunspell
- enchant

If the program will not be available on PATH available to Emacs, as it may
happen on Windows, then use the last option and identify the complete absolute
path of the ispell-compatible program.  That file must exist when you make the
selection."
  :group 'pel-pkg-for-spelling
  :type '(choice
          (const :tag "No spell check" nil)
          (string :tag "ispell-compatible program name")
          (file   :tag "Use executable file specified by this absolute path"
                  :must-match t)))

(defcustom  pel-spell-personal-dictionary-directory nil
  "Directory that hold your ispell-compatible personal dictionary files.

The personal directory files should be stored inside that directory and
should have a name that corresponds to the name of the natural language
used by the base dictionary/ies that you use with this personal dictionary.

The Aspell program accepts a personal dictionnary as long as it is identified
using the same natural language as the base language specifeid by emacs ispell
logic.

The default location is inside the ispell-personal-dictionary located inside
the `user-emacs-directory', which is \"~/.emacs.d\" by default.

On Windows you may want to identify the directory used by the tool,
something not located inside Emacs user directory."
  :group 'pel-pkg-for-spelling
  :type '(choice
          (const  :tag "Use default: ispell-personal-dictionary inside user-emacs-directory" nil)
          (directory :tag "Use specified directory")))

(defcustom pel-modes-activating-flyspell-mode
  '(log-edit-mode
    markdown-mode
    org-mode
    rst-mode
    vc-git-log-edit-mode)
  "List of major modes that automatically activate `flyspell-mode'.
To activate the changes for this you must \\='Apply and Save\\=' and restart Emacs."
  :group 'pel-pkg-for-spelling
  :type '(repeat symbol))

(defcustom pel-modes-activating-flyspell-prog-mode
  '(c-mode
    c++-mode
    d-mode
    emacs-lisp-mode
    elixir-mode
    erlang-mode
    graphviz-dot-mode
    julia-mode
    lfe-mode
    lisp-mode
    plantuml-mode
    python-mode
    shell-script-mode)
  "List of major modes that automatically activate `flyspell-prog-mode'.
To activate the changes for this you must \\='Apply and Save\\=' and restart Emacs."
  :group 'pel-pkg-for-spelling
  :type '(repeat symbol))

;; ---------------------------------------------------------------------------
;; Software Build Support
;; ----------------------

(defgroup pel-pkg-for-sw-build nil
  "PEL support for software build systems."
  :group 'pel-package-use)

(defcustom pel-use-tup nil
  "Control activation of support for Tup files."
  :group 'pel-pkg-for-sw-build
  :link '(url-link :tag "Tup Home" "http://gittup.org/tup/")
  :link '(url-link :tag "tup-mode @ GitHub"
                   "https://github.com/pierre-rouleau/tup-mode")
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-tup :package-is '(quote ((utils . tup-mode))))

(defcustom pel-tup-activates-minor-modes nil
  "List of *local* minor-modes automatically activated for tup buffers.
Enter *local* minor-mode activating function symbols.
Do not enter lambda expressions."
  :group 'pel-pkg-for-sw-build
  :type '(repeat function))

(defcustom pel-use-nix-mode nil
  "Control whether PEL activates support for the Nix package manager files."
  :link '(url-link :tag "nix-mode @ Github"
                   "https://github.com/NixOS/nix-mode")
  :group 'pel-pkg-for-sw-build
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-nix-activates-minor-modes nil
  "List of *local* minor-modes automatically activated for nix buffers.
Enter *local* minor-mode activating function symbols.
Do not enter lambda expressions."
  :group 'pel-pkg-for-sw-build
  :type '(repeat function))

;; ---------------------------------------------------------------------------
;; CMake file support
;; ------------------
(defgroup pel-pkg-for-cmake nil
  "PEL support for CMake software build system."
  :group 'pel-pkg-for-sw-build)

(defcustom pel-use-cmake-mode nil
  "Control whether PEL provides extra support for CMake."
  :group 'pel-pkg-for-cmake
  :group 'cmake
  :link '(url-link :tag "cmake-mode @ GitHub"
                   "https://github.com/Kitware/CMake/blob/master/Auxiliary/cmake-mode.el")
  :link '(url-link :tag "cmake-mode @ MELPA"
                   "https://melpa.org/#/cmake-mode")
  :type 'boolean
  :safe #'booleanp)

;; ---------------------------------------------------------------------------
;; Make file Modes
;; ---------------
(defgroup pel-pkg-for-make nil
  "PEL support for make software build systems."
  :group 'pel-pkg-for-sw-build)

(defcustom pel-use-makefile t
  "Control whether PEL provides extra support for makefile.
On by default. Turn it off if you don't need it."
  :group 'pel-pkg-for-make
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-makefile :package-is :builtin-emacs)

(defcustom pel-makefile-activates-minor-modes nil
  "List of *local* minor-modes automatically activated for makefile buffers.
Enter *local* minor-mode activating function symbols.
Do not enter lambda expressions."
  :group 'pel-pkg-for-make
  :type '(repeat function))

(defcustom pel-make-mode-alist '(("\\.mak\\'" . makefile-nmake-mode))
  "Alist of file name regexp to make major modes.
This list is added to the list Emacs already supports for Make."
  :group 'pel-pkg-for-make
  :type
  '(repeat
    (cons
     (string :tag "file pattern regex")
     (symbol :tag "major mode        "))))

;; ---------------------------------------------------------------------------
;; Support for Meson build system
;; ------------------------------
(defgroup pel-pkg-for-meson nil
  "PEL support for meson software build systems."
  :link `(url-link :tag "Meson PDF" ,(pel-pdf-file-url "pl-meson"))
  :group 'pel-pkg-for-sw-build)

(defcustom pel-use-meson-mode nil
  "Control whether PEL provides support for meson file via meson-mode."
  :link '(url-link :tag "meson-mode @ Github"
                   "https://github.com/wentasah/meson-mode")
  :group 'pel-pkg-for-meson
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-meson-activates-minor-modes nil
  "List of *local* minor-modes automatically activated for meson buffers.
Enter *local* minor-mode activating function symbols.
Do not enter lambda expressions."
  :group 'pel-pkg-for-meson
  :type '(repeat function))

(defcustom pel-meson-tab-width 4
  "Column width display rendering of hard tab for meson buffers.

PEL stores this in `tab-width' when opening meson buffers.

This does *NOT* control the indentation in meson files, it only controls
the column width display rendering of hard tabs in buffers and tab stop
positions for commands such as `tab-to-tab-stop'.

Indentation in Meson build buffers controlled by `meson-indent-basic'."
  :group 'pel-pkg-for-meson
  :type 'integer
  :safe 'pel-indent-valid-p)

(defcustom pel-meson-use-tabs nil
  "Value of `indent-tabs-mode' for editing meson files.
- If set to nil: only spaces are used for indentation.
- If set to t: hard tabs are used when possible."
  :group 'pel-pkg-for-meson
  :type 'boolean
  :safe #'booleanp)

;; ---------------------------------------------------------------------------
;; Support for Ninja build back-end
;; --------------------------------
(defgroup pel-pkg-for-ninja nil
  "PEL support for ninja software build systems."
  :link `(url-link :tag "Ninja PDF" ,(pel-pdf-file-url "pl-ninja"))
  :group 'pel-pkg-for-sw-build)

(defcustom pel-use-ninja-mode nil
  "Control whether PEL provides support for ninja file via ninja-mode."
  :link '(url-link :tag "ninja-emacs @ Github"
                   "https://github.com/ninja-build/ninja-emacs")
  :group 'pel-pkg-for-ninja
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-ninja-mode :package-is :in-utils)

(defcustom pel-ninja-activates-minor-modes nil
  "List of *local* minor-modes automatically activated for ninja buffers.
Enter *local* minor-mode activating function symbols.
Do not enter lambda expressions."
  :group 'pel-pkg-for-ninja
  :type '(repeat function))

(defcustom pel-ninja-tab-width 2
  "Column width display rendering of hard tab for ninja buffers.

PEL stores this in `tab-width' when opening ninja buffers.

This does *NOT* control the indentation in ninja files, it only controls
the column width display rendering of hard tabs in buffers and tab stop
positions for commands such as `tab-to-tab-stop'.

Indentation in ninja buffers is controlled by `ninja-indent-offset'."
  :group 'pel-pkg-for-ninja
  :type 'integer
  :safe 'pel-indent-valid-p)

(defcustom pel-ninja-use-tabs nil
  "Value of `indent-tabs-mode' for editing ninja files.
- If set to nil: only spaces are used for indentation.
- If set to t: hard tabs are used when possible."
  :group 'pel-pkg-for-ninja
  :type 'boolean
  :safe #'booleanp)

;; ---------------------------------------------------------------------------
;; Text Mode support
;; -----------------
(defgroup pel-pkg-for-text-mode nil
  "PEL support for text mode."
  :group 'pel-package-use
  :link `(url-link :tag "Text Modes PDF" ,(pel-pdf-file-url "text-modes")))

(defcustom pel-modes-activating-superword-mode
  '(c-mode
    c++-mode
    d-mode
    erlang-mode
    elixir-mode
    python-mode
    emacs-lisp-mode
    lisp-mode
    makefile-mode)
  "List of major modes that automatically activate the `superword-mode'.

PEL activates several of these modes.
Add or remove any.  Use the `superword-mode' command to toggle this
mode during an editing session."
  :group 'pel-pkg-for-text-mode
  :type '(repeat symbol))

(defcustom pel-modes-activating-subword-mode nil
  "List of major modes that automatically activate the `subword-mode'."
  :group 'pel-pkg-for-text-mode
  :type '(repeat symbol))

(defcustom pel-modes-activating-glasses-mode nil
  "List of major modes that automatically activate the `glasses-mode'."
  :group 'pel-pkg-for-text-mode
  :type '(repeat symbol))
(defcustom pel-modes-activating-auto-fill-mode nil
  "List of major modes that automatically activate the `auto-fill-mode'."
  :group 'pel-pkg-for-text-mode
  :type '(repeat symbol))

(defcustom pel-modes-activating-whitespace-mode nil
  "List of major modes that automatically activate the `whitespace-mode'.

Good candidates:
- diff-mode     To show whitespace differences (tabs vs. spaces),
                as well as trailing whitespace."
  :group 'pel-pkg-for-text-mode
  :type '(repeat symbol))

(defcustom pel-modes-activating-electric-quote-local-mode nil
  "List of major modes that automatically activate the `electric-quote-local-mode'."
  :group 'pel-pkg-for-text-mode
  :type '(repeat symbol))

;; ---------------------------------------------------------------------------
;; Time Tracking
;; -------------
(defgroup pel-pkg-for-time-tracking nil
  "PEL support for time tracking."
  :group 'pel-package-use
  :link `(url-link :tag "Time Tracking PDF" ,(pel-pdf-file-url
                                              "time-tracking")))

(defcustom pel-use-timeclock nil
  "Control whether PEL activates builtin timeclock package for time tracking."
  :group 'pel-pkg-for-time-tracking
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-timeclock :package-is :builtin-emacs)
(pel-put 'pel-use-timeclock :also-required-when 'pel-use-timeclock-timelog)

(defcustom pel-use-timeclock-timelog nil
  "Control whether PEL activates the timelog extension to timeclock."
  :group 'pel-pkg-for-time-tracking
  :link '(url-link :tag "timelog @ GitHub"
                   "https://github.com/pierre-rouleau/timelog")
  :link '(url-link :tag "flambard/timelog @ Github Gist"
                   "https://gist.github.com/flambard/419770#file-timelog-el")
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-timeclock-timelog :package-is '(quote ((utils . timelog))))
(pel-put 'pel-use-timeclock-timelog :requires 'pel-use-timeclock)

;; TODO: add chronometrist when it's stable enough and compiles cleanly.
;; For now it does not identify the spark dependency, fails to compile
;; cleanly, depends on tildegit.org which is difficult to register.
;; (defcustom pel-use-chronometrist nil
;;   "Control whether PEL activates the external chronometrist package for time tracking."
;;   :group 'pel-pkg-for-time-tracking
;;   :link '(url-link :tag "chronometrist @ tildegit"
;;                    "https://tildegit.org/contrapunctus/chronometrist")
;;   :type 'boolean
;;   :safe #'booleanp)

(defcustom pel-use-tzc nil
  "Control whether PEL activates the tzc time zone tool.

Currently this tool does not work properly.
This requires Emacs 27.1 or later."
  :group 'pel-pkg-for-time-tracking
  :link '(url-link :tag "tzc @ GitHub"
                   "https://github.com/md-arif-shaikh/tzc")
  :type 'boolean
  :safe #'booleanp)

;; ---------------------------------------------------------------------------
;; Text Translation
;; ----------------
(defgroup pel-pkg-for-text-translation nil
  "List of packages that PEL can use to perform natural text translation."
  :group 'pel-package-use)


(defcustom pel-use-go-translate nil
  "Control whether PEL activates the go-translate external package.

This requires Emacs 27.1 or later.  On previous versions PEL automatically
turns it off."
  :group 'pel-pkg-for-text-translation
  :group 'pel-pkg-for-spelling
  :link '(url-link :tag "Go-Translate @ GitHub"
                   "https://github.com/lorniu/go-translate")
  :type 'boolean
  :safe #'booleanp)

;; ---------------------------------------------------------------------------
;; Undo Mechanism Management
;; -------------------------
(defgroup pel-pkg-for-undo nil
  "List of external packages that PEL can use to control the undo mechanisms."
  :group 'pel-package-use
  :link `(url-link :tag "Undo/Redo/Repeat PDF"
                   ,(pel-pdf-file-url "undo-redo-repeat")))

(defcustom pel-use-undo-tree nil
  "Control whether PEL uses the undo-tree package.

Be aware that intensive of undo/redo with this package
may lead to buffer CORRUPTION and LOST DATA! Use with care.

You may want to use undo-propose with on Emacs 27 and earlier.

On Emacs 28 and later, if any of pel-use-simple-undo or
pel-use-vundo is active, pel-use-undo-tree is automatically
turned off."
  :group 'pel-pkg-for-undo
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-use-undo-propose nil
  "Control whether PEL uses the undo-propose package.

Use it only when using Classic Emacs undo.  You can also use it with simple
undo on Emacs >= 28 but it will not allow to redo anything in simple undo so
there's limited added value in that case."
  :link '(url-link :tag "undo-propose @Github"
                   "https://github.com/jackkamm/undo-propose-el")
  :group 'pel-pkg-for-undo
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-use-simple-undo nil
  "Control whether PEL uses the undo-only and undo-redo commands.

This requires Emacs 28.1 or later. On previous versions PEL automatically
turn this off."
  :group 'pel-pkg-for-undo
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-use-vundo nil
  "Control whether PEL uses the vundo external package.

This requires Emacs 28.1 or later. On previous versions PEL automatically
turn this off."
  :link '(url-link :tag "vundo @ GNU Elpa"
                   "https://elpa.gnu.org/packages/vundo.html")
  :group 'pel-pkg-for-undo
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-use-goto-last-change nil
  "Control whether PEL uses the {goto-last-change} package."
  :group 'pel-pkg-for-undo
  :type 'boolean
  :safe #'booleanp)

;; ---------------------------------------------------------------------------
;; Version Control System Support
;; ------------------------------
(defgroup pel-pkg-for-vcs nil
  "List of external packages that PEL can use to support use of (D)VCS."
  :group 'pel-package-use)

;; ------------------------------
;; Git
(defgroup pel-pkg-for-git nil
  "PEL customization group for Git."
  :group 'pel-pkg-for-vcs)

(defcustom pel-use-magit nil
  "Control whether PEL provides access to the Magit package."
  :group 'pel-pkg-for-git
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-magit :also-required-when 'pel-use-treemacs-magit)

(defcustom pel-use-gitignore-mode nil
  "Control whether PEL provides access to the git-modes package.
It provides access to gitignore-mode, gitconfig-mode and gitattributes-mode."
  :link '(url-link :tag "git-modes @ GitHub"
                   "https://github.com/magit/git-modes")
  :group 'pel-pkg-for-git
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-gitignore-mode :package-is '(quote ((elpa . git-modes))))

;; ------------------------------
;; Mercurial
(defgroup pel-pkg-for-mercurial nil
  "PEL customization group for Mercurial."
  :group 'pel-pkg-for-vcs
  :link `(url-link :tag "Mercurial PDF" ,(pel-pdf-file-url "vcs-mercurial")))

(defcustom pel-use-hgignore-mode nil
  "Determines whether PEL activates the hgignore-mode for .hgignore files."
  :link '(url-link :tag "hgignore-mode @ GitHub"
                   "https://github.com/omajid/hgignore-mode")
  :group 'pel-pkg-for-mercurial
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-use-monky nil
  "Control whether PEL provides access to the Monky package."
  :group 'pel-pkg-for-mercurial
  :type 'boolean
  :safe #'booleanp)

;; ------------------------------
;; Perforce
(defgroup pel-pkg-for-perforce nil
  "PEL customization group for Perforce support."
  :group 'pel-pkg-for-vcs)

(defcustom pel-use-perforce nil
  "Determines whether PEL provides Perforce support and what it uses.

Two packages can be used to provide Perforce support:

- vc-p4.el: provides Perforce support through Emacs VC mode.
            Useful if you know VC and want to use it with Perforce.
- p4.el   : An independent implementation. Provides more Perforce
            commands.

PEL provides the ability to use one or the other and also to use a mix of
them, where vc-p4 commands are used for operation on the current buffer but
other Perforce commands are available through p4.el

The selection is made by what you select here:

- Do not use.    The default.  nil.
- Use vc-p4.el:  Use Perforce back-end to VC.  vc-p4.
- Use p4.el:     Use p4.el support directly.  p4.
- Use both:      Use Perforce back-end to VC for buffer interaction.
                 Access other Perforce commands through p4.el"
  :group 'pel-pkg-for-perforce
  :type '(choice
          (const :tag "Do not use"    nil)
          (const :tag "Use vc-p4.el"  vc-p4)
          (const :tag "Use p4.el"     p4)
          (const :tag "Use both"      both)))

;; Subversion
(defgroup pel-pkg-for-subversion nil
  "PEL customization group for Subversion support."
  :group 'pel-pkg-for-vcs)

(defcustom pel-use-dsvn nil
  "Determines whether PEL provides support for the dsvn external package"
  :link '(url-link :tag "dsvm @ EmacsMirror"
                   "https://github.com/emacsmirror/dsvn")
  :group 'pel-pkg-for-subversion
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-use-psvn nil
  "Determines whether PEL provides support for the psvn external package"
  :link '(url-link :tag "psvn @ Emacs Wiki"
                   "https://www.emacswiki.org/emacs/SvnStatusMode")
  :link '(url-link :tag "psvn @ Github"
                   "https://github.com/pierre-rouleau/psvn")
  :group 'pel-pkg-for-subversion
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-psvn :package-is :in-utils)


(defcustom pel-vcs-svn-verbose-log nil
  "Set to activate --verbose switch for the svn log commands.

The `vc-svn-global-switches' user option allows you to add the --version
switch, unfortunately some svn commands do not accept the --verbose switch and
the related command will fail. If it is the case for the version of Subversion
you use, then set `pel-vcs-svn-verbose-log' to non-nil.  PEL will dynamically
append the --verbose switch when the svn log commands are executed, but will
not modify other commands."
  :group 'pel-pkg-for-subversion
  :type 'boolean
  :safe #'booleanp)

;; ---------------------------------------------------------------------------
;; Windows Management
;; ------------------
(defgroup pel-pkg-for-window nil
  "List of external packages that PEL can use to manage windows."
  :group 'pel-package-use
  :group 'windows
  :link `(url-link :tag "Windows PDF" ,(pel-pdf-file-url "windows")))

;; Note: some other windows modules are used by PEL and are loaded
;;       regardless of the options since they are relatively small
;;       and inexpensive:
;;       - windmove
;;       - winner

(defcustom pel-use-golden-ratio nil
  "Controls whether PEL uses the `golden-ratio' package."
  :link '(url-link :tag "golden-ratio @ Github"
                   "https://github.com/roman/golden-ratio.el")
  :group 'pel-pkg-for-window
  :type '(choice
          (const :tag "Do not use" nil)
          (const :tag "Use, activate later by command"  t)
          (const :tag "Use, activate globally when Emacs starts"
                 use-from-start)))

(defcustom pel-use-ace-window  nil
  "Control whether PEL uses the `ace-window' package.
When set PEL activates key bindings to move point (the cursor)
to a window identified by a number that shows up on the
top-left corner of the windows, allowing you to move to windows
far away quickly.
See the key bindings in the Windows PDF."
  :group 'pel-pkg-for-window
  :type 'boolean
  :safe #'booleanp
  :link `(url-link :tag "Windows PDF" ,(pel-pdf-file-url "windows")))

(defcustom pel-use-window-purpose nil
  "Control whether PEL uses the `window-purpose' package."
  :group 'pel-pkg-for-windows
  :type 'boolean
  :safe #'booleanp
  :link '(url-link :tag "window-purpose @ Github"
                   "https://github.com/bmag/emacs-purpose"))

(defcustom pel-use-winner nil
  "Control whether PEL uses the `winner' package.
When set PEL activates key bindings you can use to restore
Emacs window layout previously used:
- winner-undo:
   - C-c <left>
   - <f11> w p
- winner-redo:
   - C-c <right>
   - <f11> w n"
  :group 'pel-pkg-for-window
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-winner :package-is :builtin-emacs)

(defcustom pel-use-winum nil
  "Control whether PEL uses the `winum' package."
  :link '(url-link :tag "winum @ GitHub"
                   "https://github.com/deb0ch/emacs-winum")

  :group 'pel-pkg-for-window
  :type '(choice
          (const :tag "Do not use" nil)
          (const :tag "Use, activate later by command"  t)
          (const :tag "Use, activate globally when Emacs starts"
                 use-from-start)))

(defcustom pel-windmove-on-esc-cursor (not (eq system-type 'gnu/linux))
  "Control whether the Esc-cursor keys are bound to windmove commands.

When set to t, PEL activates the following four key bindings:

- ESC <up>    : windmove-up
- ESC <down>  : windmove-down
- ESC <right> : windmove-right
- ESC <left>  : windmove-left

If it set to nil, these keys are not bound.
When using Org-mode often it's probably best to set this off (nil)."
  :group 'pel-pkg-for-window
  :group 'pel-base-emacs
  :group 'pel-pkg-for-keys
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-use-transpose-frame nil
  "Control whether PEL uses the `transpose-frame' package."
  :group 'pel-pkg-for-windows
  :type 'boolean
  :safe #'booleanp
  :link '(url-link :tag "transpose-frame @ MELPA"
                   "https://melpa.org/#/transpose-frame"))

(defcustom pel-windmove-on-f1-cursor t
  "Control whether the F1-cursor keys are bound to windmove commands.

When set to t, PEL activates the following four key bindings:

- <f1> <up>    : windmove-up
- <f1> <down>  : windmove-down
- <f1> <right> : windmove-right
- <f1> <left>  : windmove-left

If it set to nil, these keys are not bound."
  :group 'pel-pkg-for-window
  :group 'pel-base-emacs
  :group 'pel-pkg-for-keys
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-map-meta-left-right-to-Y-Z t
  "If set <M-left> and <M-right> are mapped to M-Y and M-Z in terminal mode.

When set to t, PEL assumes that Emacs running in terminal (TTY/termcap) mode
receives the following key sequence for the following typed keys:

- M-Y   : when <M-right> is typed.
- M-Z   : when <M-left> is typed.

If it set to nil, no such assumptions are made."
  :group 'pel-pkg-for-window
  :group 'pel-base-emacs
  :group 'pel-pkg-for-keys
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-use-windresize nil
  "Control whether PEL provides the windresize external package."
  :link '(url-link :tag "windresize @ Elpa"
                   "https://elpa.gnu.org/packages/windresize.html")
  :group 'pel-pkg-for-window
  :type 'boolean
  :safe #'booleanp)


(defcustom pel-use-layout-restore nil
  "Control whether PEL uses layout-restore.

This allows saving and restoring Emacs window layouts.

USE WITH CAUTION! It's old code that clashes with many modes."
  :link '(url-link :tag "layout-restore @ EmacsWiki"
                   "https://www.emacswiki.org/emacs/LayoutRestore")
  :group 'pel-pkg-for-window
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-layout-restore :package-is :in-utils)

;; ---------------------------------------------------------------------------
;; pel-pkg-for-xref
;; ----------------
(defgroup pel-pkg-for-xref nil
  "List of external packages PEL can use for handling cross references."
  :group 'pel-package-use
  :group 'pel-pkg-for-navigation
  :group 'pel-pkg-for-software-programming-languages
  :link '(custom-group-link "pel-pkg-for-project-mng")
  :link '(custom-group-link "projectile")
  :link '(custom-group-link "speedbar")
  :link `(url-link :tag "Xref PDF" ,(pel-pdf-file-url "xref")))

(defcustom pel-bind-m-dot-to-xref-find-custom-definition t
  "If set, the M-.  key is bound to special xref find in Custom buffers.

When this is set to t, the M-.  key is bound to the
function `pel-xref-find-custom-definition-at-line' when point is
in a custom-mode buffer.  This way you can find the source of
a user-option variable by moving point over the name of a user-option
and type the M-. key.
This is the default.  To prevent this binding, set it to nil."
  :group 'pel-pkg-for-xref
  :type 'boolean
  :safe #'booleanp)

;; -- cscope
(defcustom pel-use-xcscope nil
  "Control whether PEL uses the xcscope package.

Activates the xcscope package which provides commands
to interact with the CScope built databases, via the
cscope-minor-mode.

This requires the CScope command line utility.
Note: on macOS you can install cscope with Homebrew
      with: brew install cscope."
  :group 'pel-pkg-for-xref
  :link '(url-link :tag "xcscope @ GitHub"
                   "https://github.com/dkogan/xcscope.el")
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-xcscope :also-required-when 'pel-use-helm-cscope)

(defcustom pel-use-helm-cscope nil
  "Control whether PEL uses the helm-cscope package.

Note: activating `pel-use-helm-cscope' and `pel-use-xcscope'
implicitly activates `pel-use-helm'."
  :group 'pel-pkg-for-xref
  :link '(url-link :tag "helm-cscope @ GitHub"
                   "https://github.com/alpha22jp/helm-cscope.el")
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-helm-cscope :requires 'pel-use-xcscope)

(defcustom pel-modes-activating-cscope nil
  "List of major modes that automatically activate cscope-minor-mode.

CScope minor mode only supports the following major modes, so only
put the following in the list:

- c-mode
- c-mode-common
- c++-mode
- dired-mode"
  :group 'pel-pkg-for-xref
  :type '(repeat symbol))

(defcustom pel-modes-activating-helm-cscope nil
  "List of major modes that automatically activate helm-cscope mode.

The list of modes should be equal or a sub-set of the list of modes
identified in the variable `pel-modes-activating-cscope' since this
mode adds key bindings for cscope-mode operations.

CScope minor mode only supports the following major modes, so only
put the following in the list:

- c-mode
- c-mode-common
- c++-mode
- dired-mode"
  :group 'pel-pkg-for-xref
  :type '(repeat symbol))

;; -- dumb-jump
(defcustom pel-use-dumb-jump nil
  "Control whether PEL uses the dumb-jump package.
With dumb-jump, the M-.  command will use dumb-jump to
identify symbol in several programming languages."
  :group 'pel-pkg-for-xref
  :link '(url-link :tag "dump-jump @ GitHub"
                   "https://github.com/jacktasia/dumb-jump")
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-modes-activating-dumb-jump nil
  "List of major modes that automatically activate dumb-jump.

Each entry must be the symbol name of a major mode.
For example, to activate it in Python, add a line with `python-mode'
without the quotes.

Note that you can also toggle dumb-jump for a major mode by
using the function `pel-xref-toggle-dumb-jump-mode' which is bound
to \\[pel-xref-toggle-dumb-jump-mode], regardless of the initial state."
  :group 'pel-pkg-for-xref
  :type '(repeat symbol))

;; -- ggtags
(defcustom pel-use-ggtags nil
  "Control whether PEL uses the ggtags package."
  :link '(url-link :tag "ggtags @ GitHub" "https://github.com/leoliu/ggtags")
  :link '(url-link :tag "Instructions for GNU Global & plugins installation"
                   "https://github.com/pierre-rouleau/pel/blob/master\
/doc/pel-manual.rst#51111gnu-global-source-code-tagging-system---gtags")
  :group 'pel-pkg-for-xref
  :group 'pel-pkg-for-cc
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-modes-activating-ggtags nil
  "List of major modes that automatically activate ggtags-mode.

Each entry must be the symbol name of a major mode.
For example, to activate it in Python, add a line with `python-mode'
without the quotes.

The function `ggtags-mode' is a cross referencing mode using
the GNU Global tag system, one of the ctags-type cross referencing systems
supported by Emacs.

Note:
Automatic loading of ggtags-mode will incur processing time and will
mask the M-= key binding of er/expand-region (but the <f11> . = binding
remains available).

As an alternative you can quickly toggle ggtags-mode with the <f11> X B G
key sequence."
  :group 'pel-pkg-for-xref
  :group 'pel-pkg-for-cc
  :type '(repeat symbol)
  :link '(url-link :tag "ggtags @ GitHub"
                   "https://github.com/leoliu/ggtags")
  :link '(url-link :tag "GNU Global home page"
                   "https://www.gnu.org/software/global/"))

;; -- gxref
(defcustom pel-use-gxref nil
  "Control whether PEL uses the gxref package."
  :link '(url-link :tag "gxref @ GitHub"
                   "https://github.com/dedi/gxref")
  :group 'pel-pkg-for-xref
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-modes-activating-gxref nil
  "List of major modes that automatically activate gxref-mode.

Each entry must be the symbol name of a major mode.
For example, to activate it in Python, add a line with `python-mode'
without the quotes.

The gxref package is a xref backend using GNU GLOBAL  cross referencing
system.

As an alternative you can quickly toggle the use of gxref xref backend
with gxref-mode with the <f11> X B g key sequence."
  :group 'pel-pkg-for-xref
  :type '(repeat symbol)
  :link '(url-link :tag "gxref @ GitHub"
                   "https://github.com/dedi/gxref"))

;; -- jtags
(defcustom pel-use-jtags nil
  "Control whether PEL uses the jtags package."
  :link '(url-link :tag "jtags @ MELPA"
                   "https://melpa.org/#/jtags")
  :group 'pel-pkg-for-xref
  :type 'boolean
  :safe #'booleanp)

;; -- rtags
;; TODO: complete the implementation: it's currently not installed
(defcustom pel-use-rtags nil
  "Control whether PEL uses the rtags package.
This is required for the rtag-xref xref backend.

NOTE: when `pel-use-rtags' is set to t, PEL activates it for all
C/C++ modes."
  :link '(url-link :tag "rtags @ GitHub"
                   "https://github.com/Andersbakken/rtags")
  :group 'pel-pkg-for-xref
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-use-rtags-xref nil
  "Control whether PEL uses the rtags-xref package."
  :link '(url-link :tag "rtags-xref @ MELPA"
                   "https://melpa.org/#/rtags-xref")
  :group 'pel-pkg-for-xref
  :type '(choice
          (const :tag "Do not use" nil)
          (const :tag "Use, activate later by command"  t)
          (const :tag "Use, activate when Emacs starts" use-from-start)))
(pel-put 'pel-use-rtags-xref :requires 'pel-use-rtags)

;; -- ivy-xref
(defcustom pel-use-ivy-xref nil
  "Control whether PEL uses the ivy-xref package.

The ivy-xref package is a front-end for xref, allowing selection
of multiple selection using ivy instead of the default *xref*
buffer.

When it is available the `pel-xref-set-front-end' command will
allow selection of that front end for xref search result.

NOTE: activating `pel-use-ivy-xref' forces the implicit
activation of `pel-use-ivy': `pel-use-ivy' is not set to t but
the ivy package will be activated regardless."
  :link '(url-link :tag "ivy-xref @ GitHub"
                   "https://github.com/alexmurray/ivy-xref")
  :group 'pel-pkg-for-xref
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-ivy-xref :requires 'pel-use-ivy)

;; -- helm-xref
(defcustom pel-use-helm-xref nil
  "Control whether PEL uses the helm-xref package.

The helm-xref package is a front-end for xref, allowing selection
of multiple selection using helm instead of the default *xref*
buffer.

When it is available the `pel-xref-set-front-end' command will
allow selection of that front end for xref search result.

NOTE: activating `pel-use-helm-xref' forces the implicit
activation of `pel-use-helm': `pel-use-helm' is not set to t but
the helm package will be activated regardless."
  :link '(url-link :tag "helm-xref @ GitHub"
                   "https://github.com/brotzeit/helm-xref")
  :group 'pel-pkg-for-xref
  :type 'boolean
  :safe #'booleanp)
(pel-put 'pel-use-helm-xref :package-is '(quote ((elpa . helm)
                                                 (elpa . helm-xref))))

(defcustom pel-startup-xref-front-end nil
  "Identifies which xref front-end to activate on startup.

This identifies how a multiple choice is shown."
  :group 'pel-pkg-for-xref
  :type '(choice
          (const :tag "Leave default."  nil)
          (const :tag "Use xref buffer" xref)
          (const :tag "Use ivy-xref"    ivy-xref)
          (const :tag "Use helm-xref"   helm-xref)))

;; -- opengrok
(defcustom pel-use-eopengrok nil
  "Control whether PEL uses the eopengrok package.

The eopengrok package provides access to the opengrok code
indexing system."
  :link '(url-link :tag "eopengrok @ GitHub"
                   "https://github.com/youngker/eopengrok.el")
  :link '(url-link :tag "OpenGrok @ Wikipedia"
                   "https://en.wikipedia.org/wiki/OpenGrok")
  :link '(url-link :tag "OpengGrok home page"
                   "https://oracle.github.io/opengrok/")
  :group 'pel-pkg-for-xref
  :type 'boolean
  :safe #'booleanp)

;; ---------------------------------------------------------------------------
;; Incompatible selection Management
;; ---------------------------------
;; Some packages are incompatible with others.  The following logic catch
;; those and modifies the values of the appropriate user-options to prevent
;; these incompatibilities to show up during execution by turning some options
;; off.

;; ido-grid does not work properly when ido-grid-mode is also being used:
;; as soon as ido-grid-mode runs the ido-grid key-map does not activate
;; properly for a reason I have not identified yet. So make sure that only 1
;; of the 2 is ever active: give priority to ido-grid.
;; Note: the `pel-initial-ido-geometry' is adjusted by the function
;; `pel-initial-ido-geometry--adjusted' used by `pel-ido-mode', there's no
;; need to adjust it here and the cost at startup.
(when (and pel-use-ido-grid
           pel-use-ido-grid-mode)
  (setq pel-use-ido-grid-mode nil))

;; ---------------------------------------------------------------------------
;; Process indirect activation
;; ---------------------------
;;
;; When some of the pel-use- user-options are activated, they implicitly
;; activate another PEL user option.  Since logic below take actions based on
;; the values of the PEL user-options, check for those and set their
;; dependent.  This way we simplify the logic of all other files that require
;; the `pel--options' feature and ensure that the pel-use- of a package that
;; is activated indirectly is set to reflect that it is active.
;;
;; Note: When adding such dependencies, ensure that these dependencies are
;;       also reflected by the `:also-required-when' property of the
;;       `pel-use-' user-option of the package(s) that get activated
;;       indirectly.

;; quelpa is used to install some packages. Identify them first.
(when pel-use-tree-sitter
  (when pel-use-combobulate
    (setq pel-use-quelpa t)))

(cl-case pel-erlang-xref-engine
  (ivy-erlang-complete (setq pel-use-ivy-erlang-complete t))
  (dumb-jump           (setq pel-use-dumb-jump t))
  (ggtags              (setq pel-use-ggtags t))
  (edts                (setq pel-use-edts 'start-automatically))
  (erlang-ls           (setq pel-use-erlang-ls t)))

(when (eq pel-c-file-finder-method 'pel-ini-file)
  (setq pel-use-ini t))
(when (eq pel-c++-file-finder-method 'pel-ini-file)
  (setq pel-use-ini t))

(when (eq pel-prompt-read-method 'ivy)
  (setq pel-use-ivy t))

(when pel-use-company-erlang
  (setq pel-use-ivy-erlang-complete t
        pel-use-company             t))

(when pel-use-projectile-speedbar
  (setq pel-use-projectile t)       ; t:= activate projectile later by command
  (setq pel-use-speedbar t))

(when pel-use-lispy
  (setq pel-use-iedit t
        pel-use-multiple-cursors t))

(when pel-use-visual-regexp-steroids
  (setq pel-use-visual-regexp t))

(when pel-use-helm-descbinds
  (setq pel-use-helm t))

(when pel-use-helm-cscope
  (setq pel-use-xcscope t
        pel-use-helm t))

(when (or pel-use-helm-xref
          pel-use-helm-lsp
          pel-use-indent-tools)
  (setq pel-use-helm t))

(when (or pel-use-ivy-xref
          pel-use-lsp-ivy)
  (setq pel-use-ivy t))

(when pel-use-ivy-erlang-complete
  (setq pel-use-ivy t
        pel-use-counsel t))

(when pel-use-bison-mode
  (setq pel-use-c t))

(when (or pel-use-indent-tools
          pel-use-iflipb
          pel-use-treemacs
          pel-activate-hydra-for-greek
          pel-use-ivy-hydra
          pel-use-verilog-ext)
  (setq pel-use-hydra t))

(when pel-use-verilog-ext
  (setq pel-use-yasnippet t))

(when pel-use-lsp-origami
  (setq pel-use-origami t))

(when (or pel-use-lsp-treemacs
          pel-use-treemacs-projectile
          pel-use-treemacs-magit)
  (setq pel-use-treemacs t))

(when pel-use-treemacs-projectile
  (setq pel-use-projectile t))

(when pel-use-treemacs-magit
  (setq pel-use-magit t))

(when (or (and pel-use-erlang
               (or pel-use-flycheck-rebar3
                   (eq pel-use-erlang-syntax-check 'with-flycheck)))
          (and pel-use-go
               (or pel-use-flycheck-golangci-lint
                   (eq pel-use-goflymake 'with-flycheck)))
          (and pel-use-plantuml
               pel-use-flycheck-plantuml)
          (and pel-use-rust
               pel-use-flycheck-rust))
  (setq pel-use-flycheck t))

(when (or pel-use-janet-mode
          pel-use-ijanet-mode
          pel-use-inf-janet)
  (setq pel-use-janet t))

(when pel-use-gerbil
  (setq pel-use-gambit t))

(when pel-use-perl-repl
  (setq pel-use-perl t))

(when (or pel-use-chez
          pel-use-chibi
          pel-use-chicken
          pel-use-gambit
          pel-use-gerbil
          pel-use-guile
          pel-use-mit-scheme
          pel-use-racket
          pel-use-scsh)
  (setq pel-use-scheme t)
  (when (or pel-use-macrostep-geiser
            pel-use-ac-geiser
            pel-use-geiser-chez
            pel-use-geiser-chibi
            pel-use-geiser-chicken
            pel-use-geiser-gambit
            pel-use-geiser-guile
            pel-use-geiser-mit
            pel-use-geiser-racket)
    (setq pel-use-geiser t))
  (when pel-use-ac-geiser
    (setq pel-use-auto-complete t)))

(when pel-use-timeclock-timelog
  (setq pel-use-timeclock t))

(when (or pel-use-ripgrep
          pel-use-ivy)
  (setq pel-use-wgrep t))

(when pel-use-helpful
  (setq pel-use-elisp-refs t)) ; helpful uses elisp-refs

;; De-activate any requests that cannot be honoured based on Emacs version.
(when (version< emacs-version "25.1")
  (setq pel-use-vterm nil))
(when (version< emacs-version "27.1")
  (setq pel-use-go-translate nil)
  (setq pel-use-tzc nil)
  (setq pel-use-csv-mode nil))
(when (version< emacs-version "28.1")
  (setq pel-use-simple-undo nil)
  (setq pel-use-vundo nil))

(when pel-use-archive-rpm
  (setq pel-use-rpm-spec-mode t))

(unless pel-use-tree-sitter
  (setq pel-use-tomlparse nil))
(when pel-use-tomlparse
  (setq pel-use-emacs-toml nil))

(when (or pel-use-emacs-ccls-for-c
          pel-use-emacs-ccls-for-c++
          pel-use-emacs-ccls-for-objc)
  (setq pel-use-emacs-ccls t))
(when pel-use-emacs-ccls
  (setq pel-use-lsp-mode t)
  (setq pel-use-lsp-ui t))
(when pel-use-lsp-java
  (setq pel-use-lsp-mode t))

;; Automatically disable undo-tree in Emacs >= 28 when pel-use-simple-undo or
;; pel-use-vundo because undo-tree is not robust and may corrupt a buffer and
;; also because these other settings provide a better solution and share key
;; bindings.
(when (or pel-use-simple-undo pel-use-vundo)
  (setq pel-use-undo-tree nil))

(when pel-use-ada
  ;; Only one major mode for Ada, auto-activate it.
  (setq pel-use-ada-mode t))

(when pel-use-eiffel
  ;; Only one major mode for Eiffel, auto-activate it.
  (setq pel-use-eiffel-mode t))

(when pel-use-nim
  ;; There's only one mode for Nim, auto-activate it.
  (setq pel-use-nim-mode t))

(when pel-use-ocaml
  (setq pel-use-caml-mode t)
  (setq pel-use-tuareg t)
  (setq pel-use-merlin t))
;; ---------------------------------------------------------------------------
(provide 'pel--options)

;;; pel--options.el ends here

; LocalWords:  cscope xcscope CScope quickstart PEL combobulate

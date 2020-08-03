;;; pel-erlang-skels.el --- Erlang specific tempo skeletons

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

(require 'pel--base)          ; use: pel-current-buffer-filename
(require 'pel--options)       ; use: pel-erlang-skel-use-separators
;;                            ;      pel-erlang-skel-use-secondary-separators
(require 'pel--macros)
(require 'pel-list)           ; use: pel-insert-list-in-list
(require 'pel-tempo)          ; use: pel-tempo-mode,
;;                            ;      pel-tempo-install-pel-skel
(require 'pel-skels)


;; -----------------------------------------------------------------------------
;; Functions used in Erlang tempo skeletons

;; --
;; Line separators

;;; Code:

(defun pel-erlang-skel-separator (&optional percent char)
  "Return a comment separator line of `fill-column' length.
The comment uses PERCENT number of '%'.
The line is made with '-' unless another CHAR is specified.
Note: the smallest allowed `fill-column' value is 70."
  (let ((percent (or percent 3))
        (char    (or ?- char)))
    (concat (make-string percent ?%)
            (make-string (- (max fill-column 70) percent) char)
            "\n")))

(defun pel-erlang-skel-optional-separator (&optional percent char)
  "Return a comment line separator if the end separators are used.
Return the same as `pel-erlang-skel-separator' if the
`pel-erlang-skel-use-secondary-separators' is non-nil otherwise return an empty
string.
Use this to create optional separator lines used by people that like them.
For those that prefer a lighter style these lines are not inserted."
  (if pel-erlang-skel-use-secondary-separators
      (pel-erlang-skel-separator percent char)
    ""))

(defun pel-erlang-skel-separator-start (&optional percent char)
  "Return a comment separator line if required by customized style.
The comment uses PERCENT number of '%'.
The line is made with '-' unless another CHAR is specified."
  (if pel-erlang-skel-use-separators
      (pel-erlang-skel-separator percent char)
    ""))

(defun pel-erlang-skel-separator-end (&optional percent char)
  "Return a comment end separator line if required by customized style.
The line length is set by `fill-column' with 70 being the smallest allowed.
The comment uses PERCENT number of '%'.
The line is made with '-' unless another CHAR is specified."
  (if (and pel-erlang-skel-use-separators
           pel-erlang-skel-use-secondary-separators)
      (concat "%% @end\n" (pel-erlang-skel-separator percent char))
    ""))

;; --
;; Extract Erlang function name & arguments

(defun pel-erlang-skel-get-function-name ()
  "Extract and return the name of the current Erlang function."
  (pel-with-required
      ('erlang)
      ('erlang-beginning-of-function
       'erlang-get-function-name)
      nil
    (save-excursion
      (erlang-beginning-of-function -1)
      (erlang-get-function-name))))

(defun pel-erlang-skel-get-function-args ()
  "Extract and return the arguments of the current Erlang function."
  (pel-with-required
      ('erlang)
      ('erlang-beginning-of-function
       'erlang-get-function-arguments)
      nil
    (save-excursion
      (erlang-beginning-of-function -1)
      (erlang-get-function-arguments))))

;; -----------------------------------------------------------------------------
;; Erlang Tempo Skeletons
;; ----------------------

(defvar pel-erlang-skel-export
  '(& "-export([" p "/"  n> "])." > n )
  "*The skeleton of a `export' declaration.
Please see the function `tempo-define-template'.")

(defvar pel-erlang-skel-import
  '((pel-skel-skip-blank) o >
    "-import(" (P "module: ") p ", [" p "/" n>
    "])." > n )
  "*The skeleton of a `import' declaration.
Please see the function `tempo-define-template'.")

(defvar pel-erlang-skel-try
  '((pel-skel-skip-blank) o >
    "try "  n>
    p   n>
    "catch" > n>
    p "oops         -> got_throw_oops;"     > n>
    p "throw:Other  -> {got_throw, Other};" > n>
    p "exit:Reason  -> {got_exit, Reason};" > n>
    p "error:Reason -> {got_error, Reason}" > n>
    "end." > n)
  "*The skeleton of a `try' expression.
Please see the function `tempo-define-template'.")

(defvar pel-erlang-skel-try-of
  '((pel-skel-skip-blank) o >
    "try "n>
    p n>
    " of" > n>
    > p " when " p " ->" >  n>
    p  n>
    "catch" > n>
    p "oops         when " p "  -> got_throw_oops;"     > n>
    p "throw:Other  when " p "  -> {got_throw, Other};" > n>
    p "exit:Reason  when " p "  -> {got_exit, Reason};" > n>
    p "error:Reason when " p "  -> {got_error, Reason}" > n>
    "after" > n>
    p  n>
    "end." > n)
  "*The skeleton of a `try' expression.
Please see the function `tempo-define-template'.")

(defvar pel-erlang-skel-function
  '((pel-erlang-skel-separator-start 2)
    "%% @doc " p n
    (pel-erlang-skel-separator-end 2)
    p "() " p "->"  n>
    p "." > n )
    "*The template of a function skeleton.
Please see the function `tempo-define-template'.")

(defvar pel-erlang-skel-spec
  '("-spec "
    (erlang-skel-get-function-name) "(" (erlang-skel-get-function-args)
    ") -> " p "undefined." n)
    "*The template of a -spec for the function following point.
Please see the function `tempo-define-template'.")

(defvar pel-skel-file-created
  '(& "%%% Created: " (pel-date) " by "
      (user-full-name) " <" erlang-skel-mail-address ">" n)
  "*The template for the \"Created:\" comment line.")

(defun pel--filename ()
  "Insert name of current file."
  (concat "%%% File: "
          (pel-current-buffer-filename :sans-directory)
          "\n"))

(defun pel--maybe-timestamp (event)
  "Insert time stamp if required."
  (when pel-erlang-skel-insert-file-timestamp
    (concat "%%% "
            (pel-time-stamp event "by ")
            "\n")))

(defvar pel-skel-large-header
  '(o (pel-erlang-skel-optional-separator)
      (pel--filename)
      "%%%" n
      (erlang-skel-include erlang-skel-author-comment)
      (erlang-skel-include erlang-skel-copyright-comment)
      "%%%" n
      (erlang-skel-include erlang-skel-created-comment)
      (pel--maybe-timestamp "last modified") ; this must be in the first 8 lines!
      "%%%" n
      "%%% @doc " p n
      "%%%      " p n
      "%%% @end" n
      (pel-erlang-skel-separator)
      (erlang-skel-include erlang-skel-small-header) )
  "*The template of a large header.
Please see the function `tempo-define-template'.")

;; -----------------------------------------------------------------------------
;; Installation of Erlang Tempo Skeletons
;; --------------------------------------
;;
;; The code in this section improves the official Erlang skeletons and assign
;; key bindings for them.  The Erlang skeletons provided here are improvements
;; on the official Erlang skeletons with provided customization and extended
;; tempo marks and Erlang code to simplify and enhance writing Erlang code.
;; The code below installs replacement inside the erlang.el where needed.
;;
;; Ideally this code would be incorporated inside the official erlang.el library.
;; I might try to do that once this code stabilizes.


;; -------
;; Install Erlang Skeletons as key-bound commands
;;
;; Add extra tempo skeletons for Erlang to complement what erlang.el already
;; have, add key bindings to allow inserting the template text with a keystroke
;; and provide a minor mode to help navigating inside the template.

(defvar pel--erl-skel-key '(("if"                      . "i")
                            ("case"                    . "c")
                            ("export"                  . "x")
                            ("import"                  . "I")
                            ("try"                     . "t")
                            ("try-of"                  . "T")
                            ("receive"                 . "r")
                            ("after"                   . "a")
                            ("loop"                    . "l")
                            ("module"                  . "m")
                            ("function"                . "f")
                            ("author"                  . "`")
                            ("spec"                    . "s")
                            ("small-header"              "M-h" pel-skel-header)
                            ("normal-header"             "M-H" pel-skel-header)
                            ("large-header"              "C-h" pel-skel-header)
                            ("small-server"              "M-s" pel-skel-header)
                            ("application"               "M-a" pel-skel-header)
                            ("supervisor"                "M-u" pel-skel-header)
                            ("supervisor-bridge"         "M-b" pel-skel-header)
                            ("generic-server"            "M-g" pel-skel-header)
                            ("gen-event"                 "M-e" pel-skel-header)
                            ("gen-fsm"                   "M-f" pel-skel-header)
                            ("gen-statem-StateName"      "M-S" pel-skel-header)
                            ("gen-statem-handle-event"   "M-E" pel-skel-header)
                            ("wx-object"                 "M-w" pel-skel-header)
                            ("gen-lib"                   "M-l" pel-skel-header)
                            ("gen-corba-cb"              "M-c" pel-skel-header)
                            ("ct-test-suite-s"           "M-1" pel-skel-header)
                            ("ct-test-suite-l"           "M-2" pel-skel-header)
                            ("ts-test-suite"             "M-3" pel-skel-header))
  "Key mapping for skeletons defined in erlang-skel.el
Each element of the list has one of the 2 following forms:
- a 2 element (name . key) cons cell,
- a 3 element (name key preliminary-function) list.
The first element is always the template name.
The second element is always a key sequence string.
The third element is optional. If present, it is the
symbol of a preparation function to call with the tempo skeleton code.
That's often the `pel-skel-header' used to insert the skeleton at
the beginning of the buffer instead of at point, the default.")

;; The standard Erlang mode support does not define skeleton for all statements.
;; Add more skeletons using the tempo package here to complement the official ones.

;; Functions used inside the skeleton descriptions below.

(defvar pel--more-erlang-skel
  '(("Export"    "export"    pel-erlang-skel-export)
    ("Import"    "import"    pel-erlang-skel-import)
    ("Try"       "try"       pel-erlang-skel-try)
    ("Try-of"    "try-of"    pel-erlang-skel-try-of))
  "Extra template entries to inserted by PEL inside `erlang-skel'.")

(defun pel--update-erlang-skel ()
  "Update the list of Erlang skeletons."
    (when (and (require 'erlang nil :noerror)
           (boundp 'erlang-skel-file)
           (load erlang-skel-file :noerror)
           (boundp 'erlang-skel)
           (boundp 'erlang-skel-function)
           (boundp 'erlang-skel-created-comment)
           (boundp 'erlang-skel-large-header)
           (fboundp 'erlang-skel-separator))
      ;; Update some Erlang skeletons with more flexible ones
      ;; - separator line length controlled by `fill-column'
      (fset 'erlang-skel-separator      'pel-erlang-skel-separator)
      ;; - function skel: second separator line is optional
      (setq erlang-skel-function        pel-erlang-skel-function)
      ;; time uses a YYYY-MM-DD format
      (setq erlang-skel-created-comment pel-skel-file-created)
      ;; large header:
      ;; The separator on the first line is optional.
      ;; The second line shows the file name.
      ;; Writes all date/time at the top, with an optional
      ;; auto-updated timestamp (must be in the first 8 lines)
      ;; Edoc text at the end of block, has marks and is indented.
      ;; It also expects the first @doc line to be a self-contained abstract.
      (setq erlang-skel-large-header    pel-skel-large-header)
      ;; Install the extra skeletons inside the erlang.el list of skeletons:
      ;; the list erlang-skel
      (setq erlang-skel (pel-insert-list-in-list
                         pel--more-erlang-skel 2 erlang-skel))))

;;-pel-autoload
(defun pel--erlang-mode-setup ()
  "Provide extra functionality to the Erlang mode.
Add extra tempo templates.
This function is meant to be used as an `advice-add'
to execute *before* `erlang-mode'."
  (pel--update-erlang-skel))

;; --

;;-pel-autoload
(defun pel--install-erlang-skel (key-map)
  "Create PEL Erlang skeleton functions and bind them in the KEY-MAP specified.
This function is meant to be called by pel-init() only."
  (if (and (require 'erlang nil :noerror)
               (boundp 'erlang-skel))
      (pel-tempo-install-pel-skel
       "erlang" erlang-skel key-map pel--erl-skel-key "erl")
    (user-error "The erlang.el package is not loaded!")))

;; -----------------------------------------------------------------------------
(provide 'pel-erlang-skels)

;;; pel-erlang-skels.el ends here

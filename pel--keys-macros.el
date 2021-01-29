;;; pel--keys-macros.el --- Key binding macros.  -*- lexical-binding: t; -*-

;; Created   : Tuesday, September  1 2020.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2021-01-28 20:47:52, updated by Pierre Rouleau>

;; This file is part of the PEL package.
;; This file is not part of GNU Emacs.

;; Copyright (C) 2020, 2021  Pierre Rouleau
;;
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

;;; --------------------------------------------------------------------------
;;; Commentary:
;;
;; The functions and macros defined in this file are used by pel_keys.el to
;; create specialized key bindings and to manipulate customization groups.

;; To have a name show up in which-key 'menu', a named function is required,
;; otherwise all we see is 'prefix' which is not meaningful.
;; The macros in this file help simplify/reduce the lines of code used to
;; create the key bindings for functions that open the customization groups
;; PEL configuration and for Emacs groups.
;;
;; The following lists the functions ('-'), and macros ('@') provided
;; and their calling hierarchy:
;;
;; @ `pel--cfg-emacs'
;; @ `pel--cfg-ext-pkg'
;; @ `pel--cfg'
;;    - `pel-prefixed'
;; @ `pel--cfg-pkg'
;;    - `pel--customize-groups'
;;       - `pel--customize-group'
;;         - `pel--group-isin-libfile'
;;         - `pel--isa-custom-group-p'

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'pel--options)
(require 'seq)          ; use: seq-concatenate, seq-drop, seq-subseq
(eval-when-compile
  (require 'cl-lib))    ; use: cl-dolist and cl-return

;;; --------------------------------------------------------------------------
;;; Code:
;;

;; ---------------------------------------------------------------------------
;; PEL Key Sequences Prefix and their F1, F2 and F3 topics
;; -------------------------------------------------------

(defconst pel--prefix-to-topic-alist
  ;; key sequence    F1: Help PDF fname F2: PEL custom group F3: lib custom
  ;;                                                             group
  ;; ------------    ------------------ -------------------- -----------------
  `(
    ([f6]            "inserting-text"   pel-pkg-for-insertions)
    ([f7 f8]         "pl-applescript"   pel-pkg-for-applescript)
    ([f8]            "projectile"       pel-pkg-for-project-mng (projectile
                                                                 projectile-speedbar))
    ([f11]           "-pel-key-maps"    nil)
    ([f11 f10]       "menus"            nil                     menu)
    ([f11 f2]        "customize"        nil                     customize)
    ([f11 f8]        "projectile"       pel-pkg-for-project-mng (projectile
                                                                 projectile-speedbar))
    ([f11 ?$]        "spell-checking"   pel-pkg-for-spelling    (ispell
                                                                 flyspell))
    ([f11 ?']        "bookmarks"        pel-pkg-for-bookmark    (bookmark
                                                                 bm))
    ([f11 ?,]        "auto-completion"  pel-pkg-for-expand   (auto-complete
                                                              company
                                                              hippie-expand))
    ([f11 ?-]        "cut-paste"  pel-pkg-for-cut-and-paste (cua-mode
                                                             killing
                                                             popup-kill-ring))
    ([f11 ?.]        "marking"          pel-pkg-for-marking  expand-region)
    ([f11 ?=]        "cut-paste"        pel-pkg-for-cut-and-paste)
    ([f11 59]        ("comments"
                      "hide-show-code") pel-pkg-for-programming (comment
                                                                 hideshow))
    ([f11 ??]        "help"             nil                    command-log)
    ([f11 9]         "indentation"      nil                     indent)
    ;; 2 different possible key sequences for speedbar
    ([f11 134217843] "speedbar"         pel-pkg-for-speedbar    (speedbar
                                                                 sr-speedbar
                                                                 projectile-speedbar))
    ([f11 27 ?s]     "speedbar"         pel-pkg-for-speedbar    (speedbar
                                                                 sr-speedbar
                                                                 projectile-speedbar))
    ([f11 32 ?C]     "pl-c++"           pel-pkg-for-c++         (cpp
                                                                 c-macro))
    ([f11 32 ?C ?#]  "pl-c++"           pel-pkg-for-c++         hide-ifdef)
    ([f11 32 ?D]     "pl-d"             pel-pkg-for-d           d-mode)
    ([f11 32 ?L]     "pl-common-lisp"   pel-pkg-for-clisp       (lisp
                                                                 lispy
                                                                 slime))
    ([f11 32 ?M]     "pl-make")
    ([f11 32 ?R]     "pl-rexx"          pel-pkg-for-rexx        rexx-mode)
    ([f11 32 ?N]     "pl-rexx"          pel-pkg-for-rexx        netrexx)
    ([f11 32 ?a]     "pl-applescript"   pel-pkg-for-applescript apples)
    ([f11 32 ?c]     "pl-c"             pel-pkg-for-c           (c
                                                                 c-macro))
    ([f11 32 ?c ?#]  "pl-c"             pel-pkg-for-c           hide-ifdef)
    ([f11 32 ?e]     "pl-erlang"   pel-pkg-for-erlang  (erlang
                                                        erldoc
                                                        edts
                                                        auto-highlight-symbol))
    ([f11 32 ?f]     "pl-forth"         pel-pkg-for-forth)
    ([f11 32 ?g]     "graphviz-dot"     pel-pkg-for-graphviz-dot graphviz)
    ([f11 32 ?j]     "pl-julia"         pel-pkg-for-julia       (julia
                                                                 julia-mode
                                                                 julia-snail))
    ([f11 32 ?l]     "pl-emacs-lisp"    pel-pkg-for-elisp       (checkdoc
                                                                 editing-basics
                                                                 elint
                                                                 eros
                                                                 lisp
                                                                 lispy
                                                                 suggest))
    ([f11 32 ?l ??]  "pl-emacs-lisp"    pel-pkg-for-all-languages (eldoc
                                                                   eldoc-box))

    ([f11 32 ?p]     "pl-python"    pel-pkg-for-python      (python
                                                             python-flymake))
    ([f11 32 ?r]     "mode-rst"         pel-pkg-for-reST        rst)
    ([f11 32 ?u]     "plantuml"         pel-pkg-for-plantuml    plantuml-mode)
    ([f11 32 ?x]     "pl-elixir"        pel-pkg-for-elixir      elixir)
    ;; ([f11 ?C]
    ([f11 ?D]        "drawing"          pel-pkg-for-drawing-markup)
    ([f11 ?D ?u]     "plantuml"         pel-pkg-for-plantuml    plantuml-mode)
    ([f11 ?F]        "frames"           pel-pkg-for-frame       frames)
    ([f11 ?S]        "sessions"         pel-pkg-for-sessions    desktop)
    ;; ([f11 ?S ?R]
    ([f11 ?X]        "xref"             pel-pkg-for-xref        (cscope
                                                                 dumb-jump
                                                                 eopengrok
                                                                 etags
                                                                 ggtags
                                                                 gxref
                                                                 helm
                                                                 helm-cscope
                                                                 helm-xref
                                                                 ivy
                                                                 ivy-xref
                                                                 projectile
                                                                 speedbar
                                                                 xref))
    ([f11 ?_]        "inserting-text")
    ([f11 ?a]        "abbreviations"    pel-pkg-for-expand      abbrev)
    ([f11 ?b]        "buffers"          pel-pkg-for-buffer      (Buffer-menu
                                                                 ibuffer
                                                                 minibuffer
                                                                 hexl
                                                                 nhexl))
    ;; ([f11 ?b ?I]
    ([f11 ?b ?h]
     "highlight"
     (pel-pkg-for-highlight
      pel-pkg-for-parens)  ,(let ((items
                                   (list
                                    'auto-highlight-symbol
                                    'iedit
                                    'rainbow-delimiters
                                    'vline)))
                              (if (version< emacs-version "27.1")
                                  (append items (list 'fill-column-indicator))
                                items)))

    ([f11 ?c]        "counting"         nil)
    ([f11 ?d]        "diff-merge"       pel-pkg-for-ztree)
    ([f11 ?d ?e]     "diff-merge"       nil                     ediff)
    ([f11 ?f ?v]     "file-variables"   nil)
    ([dired]         "mode-dired"       pel-pkg-for-dired       dired)
    ([f11 ?f]        ("file-mngt"
                      "mode-dired"
                      "web")            (pel-pkg-for-filemng
                                         pel-kg-for-dired)     (files
                                                                dired
                                                                recentf))

    ;; no PDF for browse yet, the info is  in file-mngt.
    ([f11 ?B]        "file-mngt"        (pel-pkg-for-browse
                                         pel-pkg-for-ztree)     (treemacs
                                                                 Ztree))
    ([f11 ?B ?N]     "file-mngt"        pel-pkg-for-neotree      neotree)
    ([f11 ?f ?a]     "file-mngt"        nil                      ffap)
    ([f11 ?f ?r]     "file-mngt"        nil                      auto-revert)
    ([f11 ?f ?v]     "file-variables")
    ([f11 ?f ?v ?D]  "file-variables")
    ([f11 ?g]        "grep"             pel-pkg-for-grep        (grep
                                                                 ag
                                                                 rg
                                                                 ripgrep
                                                                 wgrep))
    ([f11 ?i]        "inserting-text"   pel-pkg-for-insertions  (lice
                                                                 smart-dash
                                                                 tempo
                                                                 time-stamp
                                                                 yanippet))
    ([f11 ?k]        "keyboard-macros"  pel-pkg-for-kbmacro     (kmacro
                                                                 centimacro))
    ([f11 ?k ?e]     "keyboard-macros"  pel-pkg-for-kbmacro     emacros)
    ([f11 ?k ?l]     "keyboard-macros"  pel-pkg-for-kbmacro     elmacro)
    ([f11 ?l]        "display-lines"    nil                     visual-line)
    ([f11 ?m]        "cursor"     pel-pkg-for-cursor      (cursor
                                                           multiple-cursors))
    ([f11 ?o]        "sorting"          nil)
    ([f11 ?r]        "registers"        nil)
    ([f11 ?s]        "search-replace"   pel-pkg-for-search      (isearch
                                                                 anzu
                                                                 swiper
                                                                 iedit
                                                                 easy-escape))
    ([f11 ?s ?m]     "search-replace"   nil)
    ([f11 ?s ?w]     "search-replace"   nil)
    ([f11 ?s ?x]     "search-replace"   pel-pkg-for-regexp    (rxt ; for pcre
                                                               re-builder
                                                               visual-regexp))
    ([f11 ?t]        ("case-conversion"
                      "input-method"
                      "text-modes")     nil)
    ([f11 ?t ?a]     "align"            pel-pkg-for-align       align)
    ([f11 ?t ?e]     "enriched-text"    nil                     enriched)
    ([f11 ?t ?f]     "filling-justification" nil               fill)
    ([f11 ?t ?j]     "filling-justification" nil               fill)
    ([f11 ?t ?m]     "text-modes"       pel-pkg-for-text-mode)
    ([f11 ?t ?t]     "transpose"        nil)
    ([f11 ?t ?w]     "whitespaces"      nil                     whitespace)
    ([f11 ?u]        "undo-redo-repeat" pel-pkg-for-undo        (undo
                                                                 undo-tree))
    ([f11 ?v]        "vcs-mercurial"    pel-pkg-for-vcs         (vc
                                                                 vc-hg
                                                                 vc-git
                                                                 magit
                                                                 monky))
    ([f11 ?w]        "windows"        pel-pkg-for-window  (windows
                                                           ace-window
                                                           ace-window-display
                                                           winner
                                                           windmove))
    ([f11 ?w ?d]     "windows"          pel-pkg-for-window)
    ([f11 ?w ?s]     "windows"          pel-pkg-for-window)
    ([f11 ?x]        "shells"           pel-pkg-for-shells      (term
                                                                 terminals
                                                                 vterm))
    ([f11 ?y]  "inserting-text"   pel-pkg-for-insertions  (yasnippet
                                                           yasnippet-snippets
                                                           yas-minor))
    ([f11 ?|]        "scrolling"  pel-pkg-for-scrolling   (follow
                                                           smooth-scrolling)))
  "Map from key prefix array to topic string.
The topic string correspond to the base name of the PDF file
stored inside the doc/pdf directory.")

;; PDF files not identified by the key sequences above
;;   "autosave-backup"
;;   "closing-suspending"
;;   "completion-input"
;;   "cua"
;;   "ert"
;;   "faces-fonts"
;;   "hooks"
;;   "key-chords"
;;   "keys-f11"
;;   "keys-fn"
;;   "macOS-terminal-settings"
;;   "mode-org-mode"
;;   "modifier-keys"
;;   "mouse"
;;   "narrowing"
;;   "navigation"
;;   "numkeypad"
;;   "packages"
;;   "rectangles"

;; --

(defconst pel--mode-letter-alist
  '(("dired"           [dired])
    ("apples"          [f11 32 ?a])
    ("c++"             [f11 32 ?C])
    ("c"               [f11 32 ?c])
    ("lisp"            [f11 32 ?L])
    ("common-lisp"     [f11 32 ?L])     ; an alias for lisp.
    ("d"               [f11 32 ?D])
    ("elixir"          [f11 32 ?x])
    ("lisp-interaction" [f11 32 ?l])    ; for scratch buffer
    ("suggest"         [f11 32 ?l])     ; suggest -> emacs-lisp help
    ("emacs-lisp"      [f11 32 ?l])
    ("erlang"          [f11 32 ?e])
    ("forth"           [f11 32 ?f])
    ("julia"           [f11 32 ?j])
    ("makefile"        [f11 32 ?M])
    ("makefile-bsdmake" [f11 32 ?M])
    ("makefile-gmake"  [f11 32 ?M])
    ("makefile-makepp" [f11 32 ?M])
    ("makefile-automake" [f11 32 ?M])
    ("makefile-imake"  [f11 32 ?M])
    ("python"          [f11 32 ?p])
    ("rexx"            [f11 32 ?R])
    ("netrexx"         [f11 32 ?N])
    ("rst"             [f11 32 ?r])
    ("graphviz-dot"    [f11 32 ?g])
    ("plantuml"        [f11 32 ?u]))
  "Maps the name of a major mode (without the -mode suffix)
to a symbol or key sequence array to use as map key inside
`pel--prefix-to-topic-alist' table.")

(defun pel--major-mode-keyseq (keyseq)
  "Return global mode index for major mode KEYSEQ.
The KEYSEQ is a sequence that starts with f12, used as a
short cut in a major mode.
It may have only one or several keys.
Its meaning depend on the currently active major mode.
Return the corresponding global key sequence that means the same
thing so it can be used as an index inside variable
`pel--prefix-to-topic-alist'."
  (unless (eq (elt keyseq 0) 'f12)
    (error "Logic error!! keyseq should start with f12.  It is %s" keyseq))
  (let* ((mode-str (substring (symbol-name major-mode) 0 -5))
         (keyidx (cadr (assoc mode-str pel--mode-letter-alist))))
    (if keyidx
        (seq-concatenate 'vector keyidx (seq-drop keyseq 1))
      (error "Missing entry for %s in pel--mode-letter-alist" mode-str))))

(defun pel--kte-for (keyseq)
  "Return the table entry for the specified KEYSEQ.
The KEYSEQ should start with either f11 or f12.
The f11 is a full key sequence.
The f12 key sequence is a mode-specific key sequence,
where f12 abbreviates the full f11 key sequence for the
current major mode.
Check the key sequences.  Expand the f12 key sequence into
the full f11 key sequence.  Report invalid key sequence."
  (let ((prefix-key (elt keyseq 0)))
    (unless (memq prefix-key '(f6 f7 f8 f11 f12 M-f12))
      (user-error "This command can only be invoked via \
F6, F7, F8, F11, F12 or M-F12 prefix.\n\
 Not %s in %s" prefix-key keyseq))
    ;; Replace M-f12 by f12:
    ;;  all other logic has no knowledge of M-f12 bindings.
    (when (eq prefix-key 'M-f12)
      (setq prefix-key 'f12)
      (aset keyseq 0 'f12))
    (assoc (if (eq prefix-key 'f12)
               (pel--major-mode-keyseq keyseq)
             keyseq)
           pel--prefix-to-topic-alist)))


(defun pel--kte-pdfs (table-entry)
  "Return a list of partial names of PDF files in TABLE-ENTRY.
Return strings: the partial names of PDF files for the TABLE-ENTRY.
Return nil if there are none."
  (let ((elem (nth 1 table-entry)))
    (if (stringp elem)
        (list elem)
      elem)))

(defun pel--kte-pel-groups (table-entry)
  "Return a list of symbols of PEL group for the TABLE-ENTRY, or nil if none."
  (let ((elem (nth 2 table-entry)))
    (if (and (symbolp elem) elem)
        (list elem)
      elem)))

(defun pel--kte-lib-groups (table-entry)
  "Return the library customization group for the TABLE-ENTRY.
Return a list of groups if there are several.
Return nil if there are none."
  (nth 3 table-entry))

(defun pel--keyseq ()
  "Return the key sequence that invoked the command.
Drop the last key: it's either f1, f2 or f3, because a binding
allowed the command to be invoked."
  (seq-subseq (this-command-keys) 0 -1))

(defun pel--kte-select-topic (prompt strings)
  "PROMPT the user for one of the STRINGS and return the selected one."
  (if (< (length strings) 2)
      (car strings)
    (require 'pel-prompt nil :noerror)
    (if (fboundp 'pel-select-string-from)
        (pel-select-string-from prompt strings)
      (error "Cannot load pel-prompt!"))))

;;-pel-autoload
(defun pel-help-pdf (&optional open-web-page)
  "Open the PEL PDF file(s) for the current context.
By default it opens the local PDF file, but if the OPEN-WEB-PAGE argument
is non-nil it opens the web-based PDF copy hosted on Github.
The topic is determined by the key sequence typed.
This command should be bound to a PEL key sequence that ends with f1."
  (interactive "P")
  (let* ((keyseq (pel--keyseq))
         (kte    (pel--kte-for keyseq)) ; pel--prefix-to-topic-alist entry
         (pdfs   (pel--kte-pdfs kte)))
    (unless pdfs
      (error "No PDF entry in pel--prefix-to-topic-alist for %s.\n\
There should be no key binding!" keyseq))
    (browse-url (pel-pdf-file-url
                 (pel--kte-select-topic "Open the PDF file: " pdfs)
                 open-web-page))))

(defconst pel--topic-alias
  '(
    ;; programming languages alias: all of their PDF files start with 'pl-'
    ("applescript"      . "pl-applescript")
    ("c++"              . "pl-c++")
    ("c"                . "pl-c")
    ("common-lisp"      . "pl-common-lisp")
    ("d"                . "pl-d")
    ("elixir"           . "pl-elixir")
    ("emacs-lisp"       . "pl-emacs-lisp")
    ("erlang"           . "pl-erlang")
    ("forth"            . "pl-forth")
    ("julia"            . "pl-julia")
    ("make"             . "pl-make")
    ("python"           . "pl-python")
    ("rexx"             . "pl-rexx")
    ("netrexx"          . "pl-rexx")
    ;; mode names aliases
    ("mercurial"        . "vcs-mercurial")
    ("lispy"            . "plm-lispy")
    ("dired"            . "mode-dired")
    ("org-mode"         . "mode-org-mode")
    ("rst"              . "mode-rst")
    ;; topic related aliases, ordered by file names
    ("hippie-expand"    . "abbreviations")
    ("dabbrev-expand"   . "abbreviations")
    ("text-align"       . "align")
    ("completion"       . "auto-completion")
    ("auto-complete"    . "auto-completion")
    ("company-mode"     . "auto-completion")
    ("binary"           . "buffers")
    ("hexadecimal"      . "buffers")
    ("input-completion" . "completion-input")
    ("copy"             . "cut-paste")
    ("delete"           . "cut-paste")
    ("kill"             . "cut-paste")
    ("yank"             . "cut-paste")
    ("lines"            . "display-lines")
    ("artist-mode"      . "drawing")
    ("picture-mode"     . "drawing")
    ("ascii-table"      . "help")
    ("prefix-keys"      . "help")
    ("keycast"          . "help")
    ("command-log"      . "help")
    ("log"              . "help")
    ("info"             . "help")
    ("man"              . "help")
    ("emacs"            . "help")
    ("benchmark"        . "help")
    ("encoding"         . "input-method")
    ("unicode"          . "input-method")
    ("commented-lines"  . "inserting-text")
    ("copyright"        . "inserting-text")
    ("license"          . "inserting-text")
    ("date"             . "inserting-text")
    ("timestamp"        . "inserting-text")
    ("file-name"        . "inserting-text")
    ("smart-dash"       . "inserting-text")
    ("yasnippet"        . "inserting-text")
    ("centimacro"       . "keyboard-macros")
    ("elmacros"         . "keyboard-macros")
    ("emacros"          . "keyboard-macros")
    ("terminal-settings" . "macOS-terminal-settings")
    ("iedit"            . "search-replace")
    ("desktop"          . "sessions"))
  "List of alias for PEL PDF file names.")

(defvar pel--prompt-history-for-help-pdf nil
  "History list for function `pel-help-pdf-select'.")

;;-pel-autoload
(defun pel-help-pdf-select (&optional open-web-page)
  "Prompt for a PEL PDF and open it.
By default it opens the local PDF file, but if the OPEN-WEB-PAGE argument
is non-nil it opens the web-based PDF copy hosted on Github.
Supports completion and history.  The presented list includes
some aliases to the file names.
If enter is typed with no entry it defaults to the PEL key maps pdf."
  (interactive "P")
  (let* ((topics (mapcar
                  (lambda (fn)
                    (substring fn 0 -4))
                  (directory-files (pel-pdf-directory) nil  "\\.pdf\\'")))
         (topic  (completing-read
                  "PEL topic: " ; prompt
                  (sort         ; collection including aliases
                   (append topics
                           (mapcar (function car) pel--topic-alias))
                   (function string<))

                  nil           ; predicate
                  t             ; require-match
                  nil           ; initial
                  'pel--prompt-history-for-help-pdf ; history
                  '("-pel-key-maps")))             ; default
         ;; since aliases are included in the list presented to user,
         ;; translate a selected alias back to its real file name
         (topic (alist-get topic pel--topic-alias topic nil (function equal))))
    (browse-url (pel-pdf-file-url topic open-web-page))))

;; --

(defun pel--isa-custom-group-p (group-name)
  "Return t if GROUP-NAME string is the name of an existing customize group."
  (let (custom-groups)
    (mapatoms (lambda (symbol)
                (when (or (and (get symbol 'custom-loads)
                               (not (get symbol 'custom-autoload)))
                          (get symbol 'custom-group))
                  (push (symbol-name symbol) custom-groups))))
    (not (null (member group-name custom-groups)))))


(defun pel--found (regxp)
  "Search for REGXP regular expression from the top of buffer.
Return non-nil if found, nil otherwise."
  (goto-char (point-min))
  (re-search-forward regxp nil :noerror))

(defun pel--which-el-file (file-path)
  "Return the Emacs Lisp source file that corresponds to the given FILE-PATH.
FILE-PATH is a complete file name with a .elc extension.
Return the name of the .el or .gz.el file that is present.
Return nil if nothing found."
  (cl-dolist (ext '(".el" ".el.gz"))
    (let ((file-path  (concat (file-name-sans-extension file-path) ext)))
      (when (file-exists-p file-path)
        (cl-return file-path)))))

(defconst pel--group-library-names
  '(("rxt"   . "pcre2el")
    ("Ztree" . "ztree-view")
    ("command-log" . "command-log-mode")
    ("netrexx"     . "netrexx-mode"))
  "Maps a group name for the library that defines it.
This is only required for the libraries that cannot be found
with the existing code, such as when the group name is different
enough from the feature name.")

(defun pel--locate-library-for (group)
  "Attempts to locate a library for the specified GROUP.
Return the file-path of the library if found, nil otherwise.
Attempts to find a library that has the same name as the group,
if that fails, it tires to see if this library is in the list
of `pel--group-library-names' associated list and tries with that
instead."
  ;; If a specified library name exists for a group, use that before
  ;; trying to parse a file with the same group name.
  (let ((libname (cdr (assoc group pel--group-library-names))))
    (if libname
        (locate-library libname)
      ;; if nothing is in the table try using a file name with the
      ;; same name as the group
      (locate-library group))))

(defun pel--group-isin-libfile (group)
  "Return non-nil if customize GROUP is defined in an accessible ELisp file.
Return the path to the source file containing the group.
GROUP must be a string.
Return nil otherwise."
  (let ((file-path (pel--locate-library-for group)))
    (when file-path
      (let ((file-path (pel--which-el-file file-path)))
        (when file-path
          (with-temp-buffer
            (insert-file-contents file-path)
            (when
                (or (pel--found (format "^ *?(defgroup +?%s " group))
                    (pel--found (format "^ +?:group +?'%s)?\\( ?\\|$\\)"
                                        group)))
              file-path)))))))

(defun pel--customize-group (group &optional other-window)
  "Customize a specified GROUP.
GROUP can be a string or a symbol.
If the GROUP is unknown, check if it is defined in
a library file with the same name and if so prompt the
user to load it before customizing it.
If OTHER-WINDOW is non-nil display in other window."
  (when (symbolp group)
    (setq group (symbol-name group)))

  (if (pel--isa-custom-group-p group)
      (customize-group group other-window)
    (let ((file-path (pel--group-isin-libfile group)))
      (if file-path
        (let ((library-name (file-name-base file-path)))
          (if (y-or-n-p
               (format
                "Group %s is from a non loaded %s.  Load it first? "
                group
                library-name))
            (when (load-library library-name)
              (customize-group group other-window))
            ;; user entered no: clear the message area
            (message nil)))
        (user-error "Customization group '%s' currently unknown.\n\
PEL cannot locate a file that defines this group.\n\
Is it installed? If not set PEL user option to activate it.\n\
To customize it manually load the library where this group is defined"
                    group)))))

;;-pel-autoload
(defun pel-customize-pel (&optional other-window)
  "Open the PEL customize group(s) for the current context.
If argument OTHER-WINDOW is specified, open in the other window.
The context is determined by the key sequence typed.
This command should be bound to a PEL key sequence that ends with f2."
  (interactive "P")
  (let* ((keyseq (pel--keyseq))
         (kte    (pel--kte-for keyseq)) ; pel--prefix-to-topic-alist entry
         (groups (pel--kte-pel-groups kte)))
    (unless groups
      (error "No PEL customization group entry in \
pel--prefix-to-topic-alist for %s\n\
There should be no key binding!" keyseq))
    (pel--customize-group
     (pel--kte-select-topic "Customize group: " groups)
     other-window)))

;;-pel-autoload
(defun pel-customize-library (&optional other-window)
  "Open the customize group of a library related to the current context.
If argument OTHER-WINDOW is specified, open in the other window.
The context is determined by the key sequence typed.
This command should be bound to a PEL key sequence that ends with f3."
  (interactive "P")
  (let* ((keyseq (pel--keyseq))
         (kte    (pel--kte-for keyseq)) ; pel--prefix-to-topic-alist entry
         (groups (pel--kte-lib-groups kte)))
    (unless groups
      (error "No library customization group entry in \
pel--prefix-to-topic-alist for %s\n\
There should be no key binding!" keyseq))
    (if (symbolp groups)
        (pel--customize-group groups other-window)
      ;; There are several groups.  Prompt for one and open it.
      ;; First build a choice list with numbers as the choice selector.
      (require 'pel-prompt nil :noerror)
      (if (fboundp 'pel-select-symbol-from)
          (pel--customize-group
           (pel-select-symbol-from "Select group: " groups)
           other-window)
       (error "Failed loading pel-prompt!")))))

;;----------------------------------------------------------------------------

(defun pel--customize-groups (pel-group group-list other-window)
  "Customize one of the group in PEL-GROUP or groups named in the GROUP-LIST.
If OTHER-WINDOW is non-nil (use \\[universal-argument]), \
display in other window and open the related group(s) that exist.
If a group is unknown, check if the group is defined in
a library file with the same name and if so prompt the
user to load it before customizing the group."
  (pel--customize-group
   (pel--kte-select-topic "Customize group: "
                          (cons pel-group group-list))
   other-window))

(defmacro pel--cfg-pkg (pel-group prefix key &rest other-groups)
  "Define a function and key binding to customize specified PEL-GROUP.
The PEL-GROUP is mapped to PREFIX KEY.  Optionally, OTHER-GROUPS
is one or several other customization groups that will also be
opened when the command is invoked with a prefix argument."
  (let ((fct (intern (format "pel-cfg-pkg-%s" pel-group)))
        (group (intern (format "pel-pkg-for-%s" pel-group)))
        (docstring (format "Customize PEL %s support.\n\
If OTHER-WINDOW is non-nil (use \\[universal-argument]), \
display in other window and open the related group(s) that exist."
                           (capitalize pel-group))))
    `(progn
       ;; first declare the function
       (defun ,fct (&optional other-window)
         ,docstring
         (interactive "P")
         (pel--customize-groups
          (quote ,group)
          (quote ,other-groups)
          other-window))
       ;; then define the global key
       (define-key ,prefix ,key (quote ,fct)))))

;; --

(defun pel-prefixed (str &optional prefix)
  "Return the STR string prefixed with PREFIX (or space) if not empty.
Pass empty string unchanged."
  (if (string= str "")
      ""
    (format "%s%s"
            (or prefix " ")
            str)))

(defmacro pel--cfg (pel-group prefix key)
  "Define function & key binding to customize PEL-GROUP mapped to PREFIX KEY."
  (let ((fct (intern (format "pel-cfg%s" (pel-prefixed pel-group "-"))))
        (group (intern (format "pel%s" (pel-prefixed pel-group "-"))))
        (docstring (format "Customize PEL%s support.\n\
If OTHER-WINDOW is non-nil (use \\[universal-argument]), \
display in other window." (pel-prefixed
                           (capitalize pel-group)))))
    `(progn
       ;; first declare the function
       (defun ,fct (&optional other-window)
         ,docstring
         (interactive "P")
         (customize-group (quote ,group) other-window))
       ;; then define the global key
       (define-key ,prefix ,key (quote ,fct)))))

;; --

(defmacro pel--cfg-ext-pkg (prefix key group)
  "Define a function to customize an external package GROUP.
Bind the function to a specified key-map identified by a PREFIX and a KEY."
  (let ((fct (intern (format "pel-cfge-%s" group)))
        (docstring   (format "Customize external package %s group.\n\
If OTHER-WINDOW is non-nil (use \\[universal-argument]), \
display in other window." group)))
    `(progn
       ;; declare the function
       (defun ,fct (&optional other-window)
         ,docstring
         (interactive "P")
         (pel--customize-group (quote ,group) other-window))
       ;; define the global key mapping
       (define-key ,prefix ,key (quote ,fct)))))

;; --

(defmacro pel--cfg-emacs (prefix key group)
  "Define a function to customize an Emacs GROUP.
Bind the function to a specified key-map identified by a PREFIX and a KEY."
  (let ((fct (intern (format "pel-cfge-%s" group)))
        (docstring   (format "Customize Emacs %s group.\n\
If OTHER-WINDOW is non-nil (use \\[universal-argument]), \
display in other window." group)))
    `(progn
       ;; declare the function
       (defun ,fct (&optional other-window)
         ,docstring
         (interactive "P")
         (customize-group (quote ,group) other-window))
       ;; define the global key mapping
       (define-key ,prefix ,key (quote ,fct)))))

;; --

(defmacro define-pel-global-prefix (prefix key)
  "Define a PREFIX key name for KEY sequence on the global key map.
If the variable `pel--prefix-to-topic-alist' identifies the
KEY sequence then create function bindings under the PREFIX
 corresponding to what is specified in the table entry:
- Bind f1 to function `pel-help-pdf' if there is help pdf identified.
- Bind f2 to function `pel-customize-pel' if there is PEL customization
  group identified in the entry.
- Bind f3 to the function `pel-customize-library' if there are library
  customization group(s) in the entry."
  (let* ((keyseq   (eval key))  ; key is a kbd expression.
         (kte      (assoc keyseq pel--prefix-to-topic-alist))
         (with-f1  (pel--kte-pdfs kte))
         (with-f2  (pel--kte-pel-groups kte))
         (with-f3  (pel--kte-lib-groups kte))
         (code
          `(progn
             ;; declare the prefix variable to avoid compiler warnings.
             (defvar ,prefix)
             ;; define the prefix key as a global prefix
             (define-prefix-command (quote ,prefix))
             (global-set-key ,key (quote ,prefix)))))
    (when with-f1
      (setq code
            (append code
                    (list
                     `(define-key ,prefix (kbd "<f1>") 'pel-help-pdf)))))
    (when with-f2
      (setq code
            (append code
                    (list
                     `(define-key ,prefix (kbd "<f2>") 'pel-customize-pel)))))
    (if with-f3
        (append code
                (list
                 `(define-key ,prefix (kbd "<f3>") 'pel-customize-library)))
      code)))

;; --

(defun pel--mode-hook-maybe-call (fct mode hook &optional append)
  "Use FCT as the MODE HOOK and call it if buffer is currently in that MODE.
The function FCT is added at the beginning of the hook list unless the
optional argument APPEND is non-nil, in which case it is added at the end."
  (add-hook hook fct append)
  (if (eq major-mode mode)
      (funcall fct)))

;; --

(defun pel-local-set-f12 (prefix &optional key)
  "Assign the <f12> or <f12> KEY to PREFIX."
  (if key
      (local-set-key (kbd (format "<f12> %s" key))   prefix)
    (local-set-key (kbd "<f12>")   prefix)))

(defun pel-local-set-f12-M-f12 (prefix &optional key)
  "Assign the <f12>/<M-f12> or <f12>/<M-f12> KEY to PREFIX."
  (if key
      (progn
        (local-set-key (kbd (format "<f12> %s" key))   prefix)
        (local-set-key (kbd (format "<M-f12> %s" key)) prefix))
    (local-set-key (kbd "<f12>")   prefix)
    (local-set-key (kbd "<M-f12>") prefix)))

;; --

;;-pel-autoload
(defun pel-help-pdfs-dir ()
  "Open a Dired buffer on the PEL PDF directory."
  (interactive)
  ;; TODO: if the buffer is already opened, move point to that buffer and
  ;; make that buffer visible, don't open a new buffer or
  ;; don't use the current window
  (find-file (pel-pdf-directory)))

;;; --------------------------------------------------------------------------
(provide 'pel--keys-macros)

;;; pel--keys-macros.el ends here

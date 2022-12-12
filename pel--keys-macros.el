;;; pel--keys-macros.el --- Key binding macros.  -*- lexical-binding: t; -*-

;; Created   : Tuesday, September  1 2020.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2022-12-12 17:01:31 EST, updated by Pierre Rouleau>

;; This file is part of the PEL package.
;; This file is not part of GNU Emacs.

;; Copyright (C) 2020, 2021, 2022  Pierre Rouleau
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
;;         - `pel--multi-file-customization-p'
;;         - `pel--load-all-libs-for'
;;           - `pel--load-all-in'

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'pel--base)    ; use: macroexp-file-name
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
;;
;; Some packages have a large set of package dependencies with customize
;; groups. For those define their list first and inject them inside the
;; `pel--prefix-to-topic-alist' below.  Specially those that have duplicated
;; entries.

(defconst pel--dired-groups '(dired
                              dired-git-info
                              dired-hide-dotfiles
                              ls-lisp
                              wdired)
  "List of groups used related to dired.")


(defconst pel--highligh-groups (let ((items '(auto-highlight-symbol
                                              electricity
                                              iedit
                                              highlight-indentation
                                              paren-showing
                                              rainbow
                                              rainbow-delimiters
                                              smartparens
                                              vline)))
                                 (if (version< emacs-version "27.1")
                                     (append items '(fill-column-indicator))
                                   items)))

(defconst pel--markdown-groups '(markdown
                                 grip
                                 impatient-showdown
                                 markdown-preview
                                 markdown-toc
                                 vmd
                                 edit-indirect
                                 htmlize
                                 simple-httpd)
  "List of groups for packages used by markdown.")

(defconst pel--yaml-groups '(yaml
                             flycheck
                             indent-tools
                             smartparens)
  "List of groups for YAML and CWL.")

(defconst pel--lsp-groups '(lsp-mode
                            lsp-ui
                            helm-lsp
                            lsp-ivy
                            lsp-origami
                            lsp-treemacs))

(defconst pel--scheme-groups '(scheme
                               geiser
                               macrostep-geiser
                               quack
                               lispy))
(defconst pel--spell-group (if (version< emacs-version "27.1")
                               '(ispell
                                 flyspell)
                             '(ispell
                               flyspell
                               go-translate)))
(defconst pel--shell-terminal-groups '(shell
                                       term
                                       terminals
                                       vter))

;; TODO: add logic in the processing of that table to allow the first element
;;       of a row to be a list of key sequences.
;;       This will help reduce duplication when several key sequences lead
;;       to the same data.
(defconst pel--prefix-to-topic-alist
  ;; key sequence    F1: Help PDF fname F2: PEL custom group F3: lib custom
  ;;                                                             group
  ;; ------------    ------------------ -------------------- -----------------
  `(
    (,(kbd "M-g <f4>") nil              pel-pkg-for-completion (imenu
                                                                flimenu
                                                                popup-imenu
                                                                popup-switcher))
    ([27 103 f4]       nil              pel-pkg-for-completion (imenu
                                                                flimenu
                                                                popup-imenu
                                                                popup-switcher))

    ([f6]            "inserting-text"   pel-pkg-generic-code-style)
    ([f7 f8]         "pl-applescript"   pel-pkg-for-applescript)
    ([f8]            "projectile"       pel-pkg-for-project-mng (projectile
                                                                 projectile-speedbar))
    ([f11]           "-index"       nil)
    ([f11 f10]       "menus"            pel-pkg-for-imenu       (menu
                                                                 imenu
                                                                 Imenu-Plus
                                                                 flimenu
                                                                 popup-imenu
                                                                 popup-switcher))
    ([f11 f2]        "customize"        nil                     customize)
    ([f11 f2 ?S]     "fast-startup")
    ([f11 f8]        "projectile"       pel-pkg-for-project-mng (projectile
                                                                 projectile-speedbar))

    ([f11 f5 ?k]     "key-chords"        pel-pkg-for-key-chord   key-chord)
    ([f11 ?$]        "spell-checking"   pel-pkg-for-spelling    ,pel--spell-group)
    ([f11 ?']        "bookmarks"        pel-pkg-for-bookmark    (bookmark
                                                                 bm))
    ([f11 ?,]        "auto-completion"  pel-pkg-for-expand   (auto-complete
                                                              company
                                                              hippie-expand))
    ([f11 ?-]        "cut-paste"  pel-pkg-for-cut-and-paste (browse-kill-ring
                                                             cua-mode
                                                             killing
                                                             popup-kill-ring))
    ([f11 ?.]        "marking"          pel-pkg-for-marking  expand-region)
    ([f11 ?=]        "cut-paste"        pel-pkg-for-cut-and-paste)
    ([f11 ?\;]       "comments"         pel-pkg-for-programming comment)
    ([f11 ??]        "help"             (pel-pkg-for-help
                                         pel-syntax-tools)
     (apropos
      command-log
      debbugs
      help
      helpful
      hydra
      keycast
      interaction-log
      man
      which-func
      which-key))
    ([f11 ?? ?k]     "help"             pel-pkg-for-keys        (command-log
                                                                 interaction-log
                                                                 hydra
                                                                 keycast
                                                                 which-func
                                                                 which-key))
    ([f11 9]         "indentation"      pel-pkg-for-indentation (indent
                                                                 indent-tools
                                                                 smart-shift))

    ;; 2 different possible key sequences for speedbar,
    ;; because M-s can also be typed ``Esc s``
    (,(kbd "<f11> M-s") "speedbar"      pel-pkg-for-speedbar    (speedbar
                                                                 sr-speedbar
                                                                 projectile-speedbar))
    ([f11 27 ?s]     "speedbar"         pel-pkg-for-speedbar    (speedbar
                                                                 sr-speedbar
                                                                 projectile-speedbar))
    ([f11 32 ?C]     "pl-c++"           pel-pkg-for-c++         (cpp
                                                                 c-macro
                                                                 electricity))
    ([f11 32 ?C f12] "pl-c++"           pel-c++-skeleton-control)
    ([f11 32 ?C ?#]  "pl-c++"           pel-pkg-for-c++         hide-ifdef)
    ([f11 32 ?D]     "pl-d"             pel-pkg-for-d           (d-mode
                                                                 electricity))
    ([f11 32 ?L]     "pl-common-lisp"   pel-pkg-for-clisp       (lisp
                                                                 lispy
                                                                 slime
                                                                 sly))
    ([f11 32 ?L f12] "pl-common-lisp"   pel-clisp-code-style)
    ([f11 32 ?M]     "pl-make"          pel-pkg-for-make        makefile)
    ([f11 32 ?R]     "pl-rexx"          pel-pkg-for-rexx        rexx-mode)
    ([f11 32 ?N]     "pl-rexx"          pel-pkg-for-rexx        netrexx)
    ([f11 32 ?a]     "pl-applescript"   pel-pkg-for-applescript apples)
    ([f11 32 ?c]     "pl-c"             pel-pkg-for-c           (c
                                                                 c-macro
                                                                 bison-mode
                                                                 electricity))
    ([f11 32 ?c f12] "pl-c"             pel-c-skeleton-control)
    ([f11 32 ?c ?#]  "pl-c"             pel-pkg-for-c           hide-ifdef)
    ([f11 32 ?e]     "pl-erlang"        pel-pkg-for-erlang      (erlang
                                                                 erldoc
                                                                 erlstack
                                                                 edts
                                                                 ivy-erlang-complete
                                                                 lsp-erlang
                                                                 lsp-mode
                                                                 lsp-treemacs
                                                                 auto-highlight-symbol
                                                                 electricity
                                                                 smart-dash
                                                                 smartparens
                                                                 treemacs))
    ([f11 32 ?e f12] "pl-erlang"        pel-erlang-code-style)
    ([f11 32 ?e ?L]  "pl-erlang"        pel-pkg-for-lsp-mode    ,(cons 'lsp-erlang pel--lsp-groups))
    ([f11 32 ?e ?w]  "pl-erlang"        pel-pkg-for-lsp-mode    (treemacs
                                                                 lsp-treemacs))
    ([f11 32 ?f]     "pl-forth"         pel-pkg-for-forth)
    ([f11 32 ?g]     "pl-go"            pel-pkg-for-go          (go
                                                                 go-cover
                                                                 godoc
                                                                 go-dot-mod
                                                                 electricity))
    ([f11 32 ?h]     "pl-haskell"       pel-pkg-for-haskell     haskell)
    ([f11 32 ?T]     "pl-janet"         pel-pkg-for-janet       (janet
                                                                 ijanet
                                                                 inf-janet))
    ([f11 32 ?j]     "pl-julia"         pel-pkg-for-julia       (julia
                                                                 julia-mode
                                                                 julia-snail
                                                                 electricity))
    ([f11 32 ?l]     "pl-emacs-lisp"    pel-pkg-for-elisp       (checkdoc
                                                                 editing-basics
                                                                 elint
                                                                 eros
                                                                 lisp
                                                                 lispy
                                                                 suggest))
    ([f11 32 ?l f12] "pl-emacs-lisp"    pel-elisp-code-style)
    ([f11 32 ?l ??]  "pl-emacs-lisp"    pel-pkg-for-all-languages (eldoc
                                                                   eldoc-box))

    ([f11 32 ?n]     "pl-nim"       pel-pkg-for-nim         (nim
                                                             electricity))
    ([f11 32 ?o]     "pl-ocaml"     pel-pkg-for-ocaml       (merlin
                                                             tuareg
                                                             tuareg-opam))
    ([f11 32 ?p]     "pl-python"    pel-pkg-for-python      (python
                                                             python-flymake
                                                             electricity))
    ([f11 32 ?r]     "pl-rust"      pel-pkg-for-rust        (rust-mode
                                                             rustic
                                                             racer
                                                             cargo
                                                             electricity))
    ([f11 32 ?P]     "pl-perl"      pel-pkg-for-perl         (perl
                                                              cperl
                                                              electricity))
    ([f11 32 ?U]     "pl-ruby"      pel-pkg-for-ruby         (ruby
                                                              electricity))
    ([f11 32 ?H]     "pl-sh"        pel-pkg-for-sh           (sh
                                                              sh-script
                                                              sh-indentation
                                                              electricity))
    ([f11 32 ?v]     "pl-v"          pel-pkg-for-v              (v-mode
                                                                 electricity))

    ([f11 32 ?x]     "pl-elixir"        pel-pkg-for-elixir      (elixir
                                                                 electricity))
    (,(kbd "<f11> SPC C-a") nil         pel-pkg-for-arc         (arc
                                                                 lispy))
    (,(kbd "<f11> SPC C-h") "pl-hy"     pel-pkg-for-hy)
    (,(kbd "<f11> SPC C-j") "pl-clojure" pel-pkg-for-clojure    (clojure
                                                                 cider
                                                                 cljr
                                                                 lispy))
    (,(kbd "<f11> SPC C-l") "pl-lfe"    pel-pkg-for-lfe         (lfe
                                                                 lispy))
    (,(kbd "<f11> SPC SPC C-l") "pl-lfe" pel-pkg-for-lfe        (lfe
                                                                 lispy))
    ;; Scheme Dialect Languages
    (,(kbd "<f11> SPC C-s C-s") "pl-scheme"        pel-pkg-for-scheme  ,pel--scheme-groups)
    (,(kbd "<f11> SPC C-s C-z") "pl-chez-scheme"   pel-pkg-for-chez    ,pel--scheme-groups)
    (,(kbd "<f11> SPC C-s C-i") "pl-chibi-scheme"  pel-pkg-for-chibi   ,pel--scheme-groups)
    (,(kbd "<f11> SPC C-s C-k") "pl-chicken-scheme" pel-pkg-for-chicken ,pel--scheme-groups)
    (,(kbd "<f11> SPC C-s C-b") "pl-gambit-scheme" pel-pkg-for-gambit  ,(cons 'gambit
                                                                              pel--scheme-groups))
    (,(kbd "<f11> SPC C-s C-e") "pl-gerbil-scheme" pel-pkg-for-gerbil  ,(cons 'gerbil-mode
                                                                              pel--scheme-groups))
    (,(kbd "<f11> SPC C-s C-g") "pl-guile-scheme"  pel-pkg-for-guile   ,pel--scheme-groups)
    (,(kbd "<f11> SPC C-s C-m") "pl-mit-scheme-scheme"   pel-pkg-for-mit-scheme  ,pel--scheme-groups)
    (,(kbd "<f11> SPC C-s C-r") "pl-racket" pel-pkg-for-racket  ,(cons 'racket
                                                                       pel--scheme-groups) )
    (,(kbd "<f11> SPC C-s C-h") "pl-scsh-scheme"   pel-pkg-for-scsh    ,pel--scheme-groups)
    ;;
    ;; ([f11 ?C]
    ([f11 ?D]        "drawing"          pel-pkg-for-drawing-markup)
    ([f11 ?D ?u]     "plantuml"         pel-pkg-for-plantuml    plantuml-mode)
    ([f11 ?F]        "frames"           pel-pkg-for-frame       frames)
    ([f11 ?T]        "time-tracking"    pel-pkg-for-time-tracking (display-time
                                                                   timeclock
                                                                   timelog))
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

    (,(kbd "<f11> SPC SPC b") "ibuffer-mode"  nil               ibuffer)
    ([f11 ?b]        "buffers"          pel-pkg-for-buffer      (Buffer-menu
                                                                 bs
                                                                 ibuffer
                                                                 iflipb
                                                                 minibuffer
                                                                 hexl
                                                                 nhexl
                                                                 popup-switcher))
    ;; ([f11 ?b ?I]
    ([f11 ?h]       "highlight"  (pel-pkg-for-highlight
                                  pel-pkg-for-modeline
                                  pel-pkg-for-parens)
     ,pel--highligh-groups)

    ([f11 ?c]        "counting"         nil)

    (,(kbd "<f11> SPC SPC d d") "diff-merge" pel-pkg-for-diff-merge  diff)
    ([f11 ?d]        "diff-merge"       pel-pkg-for-diff-merge  (diff
                                                                 ediff
                                                                 emerge
                                                                 smerge
                                                                 ztree))
    ([f11 ?d ?e]     "diff-merge"       pel-pkg-for-diff-merge   ediff)
    (,(kbd "<f11> SPC SPC d e") "diff-smerge"  pel-pkg-for-diff-merge   ediff)
    ([f11 ?d ?s]     "diff-merge"       pel-pkg-for-diff-merge   smerge)
    (,(kbd "<f11> SPC SPC d s") "diff-smerge"  pel-pkg-for-diff-merge   smerge)

    ([f11 ?f ?v]     "file-variables"   nil)
    (,(kbd "<f11> SPC M-D") "mode-dired" pel-pkg-for-dired      ,pel--dired-groups)
    ([f11 32 27 ?D]  "mode-dired"       pel-pkg-for-dired       ,pel--dired-groups)
    ([dired]         "mode-dired"       pel-pkg-for-dired       ,pel--dired-groups)
    ([f11 ?f]        ("file-mngt"
                      "web")            pel-pkg-for-filemng     (files
                      recentf
                      popup-switcher))

    ;; no PDF for browse yet, the info is  in file-mngt.
    ([f11 ?B]        "file-mngt"        (pel-pkg-for-browse
                                         pel-pkg-for-ztree)
     (treemacs
      lsp-treemacs
      ztree
      rfc-mode-group))
    ([f11 ?B ?N]     "file-mngt"        pel-pkg-for-neotree      neotree)
    ([f11 ?f ?a]     "file-mngt"        nil                      ffap)
    ([f11 ?f ?p]     "file-mngt"        pel-pkg-for-project-mng  ffip)
    ([f11 ?f ?r]     "file-mngt"        nil                      auto-revert)
    ([f11 ?f ?v]     "file-variables")
    ([f11 ?f ?v ?D]  "file-variables")
    ([f11 ?g]        "grep"             pel-pkg-for-grep        (grep
                                                                 ag
                                                                 deadgrep
                                                                 rg
                                                                 ripgrep
                                                                 wgrep))
    ([f11 ?\(]       "smartparens"      pel-pkg-for-parens      (electricity
                                                                 rainbow-delimiters
                                                                 smartparens))
    ([f11 ?i]        "inserting-text"   pel-pkg-for-insertions  (electricity
                                                                 lice
                                                                 smart-dash
                                                                 smartparens
                                                                 tempo
                                                                 time-stamp
                                                                 yanippet))
    ([f11 ?k]        "keyboard-macros"  pel-pkg-for-kbmacro     (kmacro
                                                                 centimacro))
    ([f11 ?k ?e]     "keyboard-macros"  pel-pkg-for-kbmacro     emacros)
    ([f11 ?k ?l]     "keyboard-macros"  pel-pkg-for-kbmacro     elmacro)
    ([f11 ?l]        "display-lines"    nil                     (display-line-numbers
                                                                 visual-line))
    ([f11 ?m]        "cursor"           pel-pkg-for-cursor      (cursor
                                                                 display
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
    ([f11 ?!]        "syntax-checking"  pel-pkg-for-syntax-check (flymake
                                                                  flycheck))
    ([f11 ?t]        ("case-conversion"
                      "input-method"
                      "text-modes")     pel-pkg-for-text-mode  (editing-basics
                      glasses
                      whitespace))
    ([f11 ?t ?a]     "align"            pel-pkg-for-align       align)
    ([f11 ?t ?e]     "enriched-text"    nil                     enriched)
    ([f11 ?t ?f]     "filling-justification" nil               fill)
    ([f11 ?t ?j]     "filling-justification" nil               fill)
    ([f11 ?t ?m]     "text-modes"       pel-pkg-for-text-mode  (editing-basics
                                                                glasses
                                                                whitespace))
    ([f11 ?t ?t]     "transpose"        nil)
    ([f11 ?t ?w]     "whitespaces"      nil                     whitespace)
    ([f11 ?u]        "undo-redo-repeat" pel-pkg-for-undo        (undo
                                                                 undo-tree))
    ([f11 ?v]        "vcs-mercurial"    pel-pkg-for-vcs         (vc
                                                                 vc-hg
                                                                 vc-git
                                                                 magit
                                                                 monky))
    (,(kbd "<f11> SPC SPC v") "vcs-mercurial" pel-pkg-for-vcs   (vc
                                                                 vc-hg
                                                                 vc-git))
    ([f11 ?w]        "windows"        pel-pkg-for-window  (windows
                                                           ace-window
                                                           ace-window-display
                                                           winner
                                                           windmove
                                                           windresize))
    ([f11 ?w ?d]     "windows"          pel-pkg-for-window)
    ([f11 ?w ?s]     "windows"          pel-pkg-for-window)
    ([f11 ?y]  "inserting-text"   pel-pkg-for-insertions  (yasnippet
                                                           yasnippet-snippets
                                                           yas-minor))

    (,(kbd "<f11> SPC SPC z") "shells"  pel-pkg-for-shells ,pel--shell-terminal-groups)
    ([f11 32 32 ?z]           "shells"  pel-pkg-for-shells ,pel--shell-terminal-groups)
    (,(kbd "<f11> SPC SPC s") "shells"  pel-pkg-for-shells      shell)
    ([f11 32 32 ?s]           "shells"  pel-pkg-for-shells      shell)
    (,(kbd "<f11> SPC SPC t") "shells"  pel-pkg-for-shells      term)
    ([f11 32 32 ?t]           "shells"  pel-pkg-for-shells      term)

    ([f11 ?|]        "scrolling"  pel-pkg-for-scrolling   (follow
                                                           smooth-scrolling))

    ;; For keys with Meta, make sure the Esc equivalent is also entered
    ;; to allow the F1, F2, F3 entries to be accessible via the Esc key.
    ;; Because: in Emacs ``M-a`` can also be typed ``Esc a``
    (,(kbd "<f11> M-S")     "fast-startup"     pel-fast-startup)
    ([f11 27 ?S]            "fast-startup"     pel-fast-startup)
    (,(kbd "<f11> M-/")     "hide-show-code"   pel-pkg-for-hide-show  (hideshow
                                                                       hide-lines
                                                                       origami))
    ([f11 27 ?/]            "hide-show-code"   pel-pkg-for-hide-show  (hideshow
                                                                       hide-lines
                                                                       origami))
    (,(kbd "<f11> M-c")     "completion-input" pel-pkg-for-completion (helm
                                                                       ido
                                                                       ido-completing-read-plus
                                                                       ido-grid
                                                                       ido-grid-mode
                                                                       ivy
                                                                       counsel
                                                                       minibuffer
                                                                       smex))
    ([f11 27 99]            "completion-input" pel-pkg-for-completion (helm
                                                                       ido
                                                                       ido-completing-read-plus
                                                                       ido-grid
                                                                       ido-grid-mode
                                                                       ivy
                                                                       counsel
                                                                       minibuffer
                                                                       smex))

    ([f11 27 ?d]            "mode-line"        pel-pkg-for-modeline   (display-time
                                                                       mode-line))
    (,(kbd "<f11> M-d")     "mode-line"        pel-pkg-for-modeline   (display-time
                                                                       mode-line))

    (,(kbd "<f11> SPC M-g") "graphviz-dot"     pel-pkg-for-graphviz-dot graphviz)
    ([f11 32 27 ?g]         "graphviz-dot"     pel-pkg-for-graphviz-dot graphviz)
    (,(kbd "<f11> SPC M-r") "mode-rst"         pel-pkg-for-reST        rst)
    ([f11 32 27 ?r]         "mode-rst"         pel-pkg-for-reST        rst)
    (,(kbd "<f11> SPC M-m") "mode-markdown"    pel-pkg-for-markdown   ,pel--markdown-groups)
    ([f11 32 27 ?m]         "mode-markdown"    pel-pkg-for-markdown   ,pel--markdown-groups)

    (,(kbd "<f11> SPC M-l") "outline"          pel-pkg-for-outline     outlines)
    ([f11 32 27 ?l]         "outline"          pel-pkg-for-outline     outlines)

    (,(kbd "<f11> SPC M-o") "mode-org-mode"    pel-pkg-for-org-mode    org)
    ([f11 32 27 ?o]         "mode-org-mode"    pel-pkg-for-org-mode    org)

    (,(kbd "<f11> SPC M-u") "plantuml" pel-pkg-for-plantuml    plantuml-mode)
    ([f11 32 27 ?u]         "plantuml" pel-pkg-for-plantuml    plantuml-mode)

    (,(kbd "<f11> SPC M-M") "mscgen" pel-pkg-for-mscgen    mscgen)
    ([f11 32 27 ?M]         "mscgen" pel-pkg-for-mscgen    mscgen)

    (,(kbd "<f11> SPC M-c") "cwl"              pel-pkg-for-cwl     ,pel--yaml-groups)
    ([f11 32 27 ?c]         "cwl"              pel-pkg-for-cwl     ,pel--yaml-groups)
    (,(kbd "<f11> SPC M-y") "yaml"             pel-pkg-for-yaml    ,pel--yaml-groups)
    ([f11 32 27 ?y]         "yaml"             pel-pkg-for-yaml    ,pel--yaml-groups)

    (,(kbd "<f11> SPC M-Y") "yang"      pel-pkg-for-spec-definition)
    ([f11 32 27 ?Y]         "yang"      pel-pkg-for-spec-definition))
  "Map from key prefix array to topic string.
The topic string correspond to the base name of the PDF file
stored inside the doc/pdf directory.")

;; PDF files not identified by the key sequences above
;;   "autosave-backup"
;;   "closing-suspending"
;;   "cua"
;;   "ert"
;;   "faces-fonts"
;;   "hooks"
;;   "keys-f11"
;;   "keys-fn"
;;   "macOS-terminal-settings"
;;   "modifier-keys"
;;   "mouse"
;;   "narrowing"
;;   "navigation"
;;   "numkeypad"
;;   "packages"
;;   "rectangles"

;; --

(defconst pel--mode-letter-alist
  '(("Custom"          [f11 f2])
    ("dired"           [dired])
    ("apples"          [f11 32 ?a])
    ("c++"             [f11 32 ?C])
    ("c"               [f11 32 ?c])
    ("lisp"            [f11 32 ?L])
    ("common-lisp"     [f11 32 ?L])     ; an alias for lisp.
    ("clojure"         [f11 32 10])
    ("d"               [f11 32 ?D])
    ("elixir"          [f11 32 ?x])
    ("lisp-interaction" [f11 32 ?l])    ; for scratch buffer
    ("suggest"         [f11 32 ?l])     ; suggest -> emacs-lisp help
    ("emacs-lisp"      [f11 32 ?l])
    ("erlang"          [f11 32 ?e])
    ("forth"           [f11 32 ?f])
    ("go"              [f11 32 ?g])
    ("janet"           [f11 32 ?T])
    ("julia"           [f11 32 ?j])
    ("makefile"        [f11 32 ?M])
    ("makefile-bsdmake" [f11 32 ?M])
    ("makefile-gmake"  [f11 32 ?M])
    ("makefile-makepp" [f11 32 ?M])
    ("makefile-automake" [f11 32 ?M])
    ("makefile-imake"  [f11 32 ?M])
    ("makefile-nmake"  [f11 32 ?M])
    ("python"          [f11 32 ?p])
    ("arc"             [f11 32 1])
    ("haskell"         [f11 32 ?h])
    ("hy"              [f11 32 8])
    ("lfe"             [f11 32 12])
    ("inferior-lfe"    [f11 32 32 12])
    ("ibuffer"         [f11 32 32 ?b])
    ("vc-dir"          [f11 32 32 ?v])
    ("nim"             [f11 32 ?n])
    ("ocaml"           [f11 32 ?o])
    ("tuareg"          [f11 32 ?o])
    ("perl"            [f11 32 ?P])
    ("rexx"            [f11 32 ?R])
    ("ruby"            [f11 32 ?U])
    ("rust"            [f11 32 ?r])
    ;;
    ("scheme"          [f11 32 19 19])
    ("chez"            [f11 32 19 26])
    ("chibi"           [f11 32 19 9])
    ("chicken"         [f11 32 19 11])
    ("gambit"          [f11 32 19 2])
    ("gerbil"          [f11 32 19 5])
    ("guile"           [f11 32 19 7])
    ("mit-scheme"      [f11 32 19 13])
    ("racket"          [f11 32 19 18])
    ("scsh"            [f11 32 19 8])
    ;;
    ("sh"              [f11 32 ?H])
    ("v"               [f11 32 ?v])
    ("markdown"        [f11 32 27 ?m])
    ("netrexx"         [f11 32 ?N])
    ("rst"             [f11 32 27 ?r])
    ("cwl"             [f11 32 27 ?c])
    ("outline"         [f11 32 27 ?l])
    ("org"             [f11 32 27 ?o])
    ("graphviz-dot"    [f11 32 27 ?g])
    ("mscgen"          [f11 32 27 ?M])
    ("plantuml"        [f11 32 27 ?u])
    ("yaml"            [f11 32 27 ?y])
    ("yang"            [f11 32 27 ?Y])
    ;; shells and terminals
    ("shell"           [f11 32 32 ?s])
    ("term"            [f11 32 32 ?t])
    ;; diff modes
    ("diff"            [f11 32 32 ?d ?d])
    ("ediff"           [f11 32 32 ?d ?e])
    ("smerge"          [f11 32 32 ?d ?s])
    )
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
The KEYSEQ should start with either f11 or f12, but also other prefix keys
such as f6, f7, f8 and some key sequences.
The f11 is a full key sequence.
The f12 key sequence is a mode-specific key sequence,
where f12 abbreviates the full f11 key sequence for the
current major mode.
Check the key sequences.  Expand the f12 key sequence into
the full f11 key sequence.  Report invalid key sequence."
  (let ((prefix-key (elt keyseq 0)))
    (unless (or (memq prefix-key '(f6 f7 f8 f11 f12 M-f12))
                ;; special case command (for now)
                (equal keyseq [27 103 f4])
                (equal keyseq (kbd "M-g <f4>")))
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
(defun pel-help-pdf (&optional open-github-page)
  "Open the PEL PDF file(s) for the current context.

By default the function opens the local PDF file unless the
OPEN-GITHUB-PAGE is specified, in which case it opens the GitHub
hosted raw PDF file.  However, if the user-option variable
`pel-flip-help-pdf-arg' is set, it's the other way around: the
GitHub remote file is opened by default.

The function uses Emacs default browse mechanism specified by the
user-option variable `browse-url-browser-function' unless the
user-option variable `pel-browser-used' forces the use of a
specific browser.

If your system default browser can not render PDF files directly
and downloads them, then you can force the use of the Firefox
browser (which renders PDF) by setting `pel-browser-used' to
'firefox.

Using a browser that is capable of direct rendering of PDF
produces a much better user experience: you will be able to
quickly navigate through PEL documentation inside the browser.

The `pel-help-pdf' function determines the requested PDF topic by the key
sequence that led to the execution of the command.  These key sequences
normally end with the F1 key."
  (interactive "P")
  (let* ((keyseq (pel--keyseq))
         (kte    (pel--kte-for keyseq)) ; pel--prefix-to-topic-alist entry
         (pdfs   (pel--kte-pdfs kte)))
    (unless pdfs
      (error "No PDF entry in pel--prefix-to-topic-alist for %s.\n\
There should be no key binding!" keyseq))
    (let* ((open-github-file (if pel-flip-help-pdf-arg
                                 (not open-github-page)
                               open-github-page))
           (url (pel-pdf-file-url (pel--kte-select-topic "Open the PDF file: "
                                                         pdfs)
                                  open-github-file)))
      (pel--help-browse url))))

(defconst pel--topic-alias
  '(
    ;; programming languages alias: all of their PDF files start with 'pl-'
    ("applescript"      . "pl-applescript")
    ("arc"              . "pl-arc")
    ("c++"              . "pl-c++")
    ("c"                . "pl-c")
    ("common-lisp"      . "pl-common-lisp")
    ("clojure"          . "pl-clojure")
    ("d"                . "pl-d")
    ("elixir"           . "pl-elixir")
    ("emacs-lisp"       . "pl-emacs-lisp")
    ("erlang"           . "pl-erlang")
    ("forth"            . "pl-forth")
    ("go"               . "pl-go")
    ("haskell"          . "pl-haskell")
    ("hy"               . "pl-hy")
    ("janet"            . "pl-janet")
    ("julia"            . "pl-julia")
    ("lfe"              . "pl-lfe")
    ("make"             . "pl-make")
    ("nim"              . "pl-nim")
    ("ocaml"            . "pl-ocaml")
    ("perl"             . "pl-perl")
    ("python"           . "pl-python")
    ("rexx"             . "pl-rexx")
    ("ruby"             . "pl-ruby")
    ("rust"             . "pl-rust")
    ("netrexx"          . "pl-rexx")
    ;; Scheme dialects
    ("scheme"           . "pl-scheme")
    ("chez"             . "pl-chez-scheme")
    ("chibi"            . "pl-chibi-scheme")
    ("chicken"          . "pl-chicken-scheme")
    ("gambit"           . "pl-gambit-scheme")
    ("gerbil"           . "pl-gerbil-scheme")
    ("guile"            . "pl-guile-scheme")
    ("mit-scheme"       . "pl-mit-scheme-scheme")
    ("racket"           . "pl-racket")
    ("scsh"             . "pl-scsh-scheme")
    ;;
    ("sh"               . "pl-sh")
    ("v"                . "pl-v")
    ;; repl
    ;; ("sly"              . "repl-cl-sly")
    ;; ("slime"            . "repl-cl-slime")
    ;; mode names aliases
    ("git"              . "vcs-git")
    ("mercurial"        . "vcs-mercurial")
    ("subversion"       . "vcs-subversion")
    ("lispy"            . "plm-lispy")
    ("dired"            . "mode-dired")
    ("org-mode"         . "mode-org-mode")
    ("markdown"         . "mode-markdown")
    ("rst"              . "mode-rst")
    ;; topic related aliases, ordered by file names
    ("repeat"           . "undo-redo-repeat")
    ("redo"             . "undo-redo-repeat")
    ("hippie-expand"    . "abbreviations")
    ("dabbrev-expand"   . "abbreviations")
    ("text-align"       . "align")
    ("completion"       . "auto-completion")
    ("auto-complete"    . "auto-completion")
    ("company-mode"     . "auto-completion")
    ("binary"           . "buffers")
    ("hexadecimal"      . "buffers")
    ("input-completion-availability" . "completion-input-availability")
    ("input-completion" . "completion-input")
    ("copy"             . "cut-paste")
    ("delete"           . "cut-paste")
    ("kill"             . "cut-paste")
    ("yank"             . "cut-paste")
    ("lines"            . "display-lines")
    ("artist-mode"      . "drawing")
    ("picture-mode"     . "drawing")
    ("fonts"            . "faces-fonts")
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
    ("regxp"            . "search-replace")
    ("iedit"            . "search-replace")
    ("order"            . "sorting")
    ("desktop"          . "sessions")
    ("diff"             . "diff-merge"))
  "List of alias for PEL PDF file names.")

(defvar pel--prompt-history-for-help-pdf nil
  "History list for function `pel-help-pdf-select'.")

(defun pel--help-browse (url)
  "Browse the specified URL using the method selected by user-options."
  (if (and (pel-running-under-ssh-p)
           (not pel-help-under-ssh))
      (user-error "When running under SSH, external help is not available")
    (if (and (require 'pel-browse nil :no-error)
             (fboundp 'pel-browse-url))
        (pel-browse-url url)
      (browse-url url)
      (user-error "Failed loading pel-browse, used Emacs browse-url instead!"))))

(defun pel-help-open-pdf (topic &optional open-github-page)
  "Open PDF help for TOPIC string potentially OPEN-WEB-PAGE."
  (pel--help-browse (pel-pdf-file-url topic open-github-page)))

;;-pel-autoload
(defun pel-help-on-completion-input (&optional open-github-page)
  "Open the input completion help PDF, in a browser if arg OPEN-WEB-PAGE set.

By default the function opens the local PDF file unless the
OPEN-GITHUB-PAGE is specified, in which case it opens the GitHub
hosted raw PDF file.  However, if the user-option variable
`pel-flip-help-pdf-arg' is set, it's the other way around: the
GitHub remote file is opened by default."
  (interactive "P")
  (pel-help-open-pdf "completion-input" (if pel-flip-help-pdf-arg
                                            (not open-github-page)
                                          open-github-page)))

;;-pel-autoload
(defun pel-help-on-outline (&optional open-github-page)
  "Open the outline help PDF, in a browser if arg OPEN-WEB-PAGE set.

By default the function opens the local PDF file unless the
OPEN-GITHUB-PAGE is specified, in which case it opens the GitHub
hosted raw PDF file.  However, if the user-option variable
`pel-flip-help-pdf-arg' is set, it's the other way around: the
GitHub remote file is opened by default."
  (interactive "P")
  (pel-help-open-pdf "outline" (if pel-flip-help-pdf-arg
                                   (not open-github-page)
                                 open-github-page)))

;;-pel-autoload
(defun pel-help-pdf-select (&optional open-github-page)
  "Prompt for a PEL PDF and open it.
By default it opens the local PDF file, but if the OPEN-WEB-PAGE argument
is non-nil it opens the web-based PDF copy hosted on Github.
Supports completion and history.  The presented list includes
some aliases to the file names.
If enter is typed with no entry it defaults to the PEL key maps pdf.
If Emacs runs under SSH, and `pel-help-under-ssh' is set,
it issues an error instead to prevent you from opening a file you
won't be able to see anyway"
  (interactive "P")
  (let* ((topics (mapcar
                  (lambda (fn)
                    (substring fn 0 -4))
                  (directory-files (pel-pdf-directory) nil  "\\.pdf\\'")))
         (topic  (completing-read
                  "PEL topic: "       ; prompt
                  (sort               ; collection including aliases
                   (append topics
                           (mapcar (function car) pel--topic-alias))
                   (function string<))

                  nil                             ; predicate
                  t                               ; require-match
                  nil                             ; initial
                  'pel--prompt-history-for-help-pdf ; history
                  '("-pel-key-maps")))              ; default
         ;; since aliases are included in the list presented to user,
         ;; translate a selected alias back to its real file name
         (topic (alist-get topic pel--topic-alias topic nil (function equal))))
    (pel-help-open-pdf topic open-github-page)))

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
  '(("rxt"         . "pcre2el")
    ("Imenu-Plus"  . "imenu+")
    ("Ztree"       . "ztree-view")
    ("command-log" . "command-log-mode")
    ("clojure"     . "clojure-mode")
    ("cljr"        . "clj-refactor")
    ("edts"        . "edts-mode")
    ("electricity" . "electric")
    ("erlstack"    . "erlstack-mode")
    ("grip"        . "grip-mode")
    ("janet"       . "janet-mode")
    ("lfe"         . ("lfe-indent"
                      "lfe-mode"))   ; several files - defgroup is in lfe-mode
    ("markdown"    . "markdown-mode")
    ("netrexx"     . "netrexx-mode")
    ("racket"      . "racket-custom")
    ("rainbow"     . "rainbow-mode")
    ("rfc-mode-group" . "rfc-mode")
    ("ffip"        . "find-file-in-project"))
  "Maps a group name for the library that defines it.
This is only required for the libraries that cannot be found
with the existing code, such as when the group name is different
enough from the feature name.")

(defconst pel--group-for-library-without-group
  '(("windresize"  . "convenience"))
  "Maps a feature name to the customization group it uses.
Some libraries unfortunately do not define their own customization group and
instead place their user-option variables into a generic Emacs customization
group.  This maps the feature name of such a library, which PEL uses in that
case, to the group name it uses.")

(defun pel--locate-library-for (group)
  "Attempts to locate a library for the specified GROUP.
Return the file-path of the library if found, nil otherwise.
Attempts to find a library that has the same name as the group,
if that fails, it tries to see if this library is in the list
of `pel--group-library-names' associated list and tries with that
instead."
  ;; If a specified library name exists for a group, use that before
  ;; trying to parse a file with the same group name.
  (let ((libname (cdr (assoc group pel--group-library-names))))
    (if libname
        (if (listp libname)
            (locate-library (car (last libname)))
          (locate-library libname))
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

(defun pel--multi-file-customization-p (group)
  "Return non-nil if GROUP customization specified in several files.
Return NIL otherwise."
  (let ((libname (cdr (assoc group pel--group-library-names))))
    (and libname
         (listp libname))))

(defun pel--load-all-in (libs)
  "Load all Emacs Lisp libraries that in LIBS list.
Return t when all libraries have been loaded, nil otherwise."
  (let ((all-loaded t))
    (dolist (lib libs all-loaded)
      (unless (load-library lib)
        (setq all-loaded nil)))))

(defun pel--load-all-libs-for (group)
  "Load all Emacs Lisp libraries that customize specified GROUP."
  (pel--load-all-in (cdr (assoc group pel--group-library-names))))

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
          (let* ((library-name (file-name-base file-path))
                 (in-multi-files (pel--multi-file-customization-p group))
                 (prompt
                  (if in-multi-files
                      "Group %s and customization is from %s and other files \
that are not all loaded.  Load them first? "
                    "Group %s is from a non-loaded %s.  Load it first? " )))
            (if (y-or-n-p
                 (format prompt group library-name))
                (when (if in-multi-files
                          (pel--load-all-libs-for group)
                        (load-library library-name))
                  (customize-group group other-window))
              ;; user entered no: clear the message area
              (message nil)))
        ;; Nothing found for the requested group.  Perhaps the library
        ;; does not use its own group but unfortunately uses one of Emacs
        ;; default groups.  Use `pel--group-for-library-without-group'
        ;; association for that.  If it's not there raise an error.
        (let ((candidate (cdr (assoc group
                                     pel--group-for-library-without-group))))
          (if candidate
              (progn
                (unless (featurep (intern group))
                  (if (y-or-n-p (format
                                 "There is no specific customization for %s.
Unfortunately it places its user-options inside the group %s instead.
You may be able to see its user-options there.
If you don't see them try searching with a prefix starting with \"%s-\"
without the quotes.
The %s library is not yet loaded.  Load it first and open the %s group? "
                                 group candidate group group candidate))
                      (load-library group)
                    ;; user entered no: clear the message area
                    (message nil)))
                (when (featurep (intern group))
                  (customize-group candidate other-window)
                  (setq group candidate)))
            (user-error "Customization group '%s' currently unknown.\n\
PEL cannot locate a file that defines this group.\n\
 It is also not identified in pel--group-library-names.\n\
Is it installed? If not set PEL user option to activate it.\n\
To customize it manually load the library where this group is defined"
                        group)))))))

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
           (pel-select-symbol-from "Select group" groups)
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

(defmacro define-pel-simple-global-prefix (prefix key)
  "Define a simple PREFIX key name for KEY sequence on the global key map.

No special keys are bound in this map by the macro."
  `(progn
     ;; declare the prefix variable to avoid compiler warnings.
     (defvar ,prefix)
     ;; define the prefix key as a global prefix
     (define-prefix-command (quote ,prefix))
     (global-set-key ,key (quote ,prefix))))

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
  "Schedule FCT as the MODE HOOK: call it if buffer is currently in that MODE.
The function FCT is added at the beginning of the hook list unless the
optional argument APPEND is non-nil, in which case it is added at the end."
  (add-hook hook fct append)
  ;; if the current mode is the required mode also run the specified function
  (if (eq major-mode mode)
      (funcall fct)))

(defconst pel--tab-controlling-major-modes '(makefile tup nix
                                                      intel-hex go
                                                      lisp arc clojure janet
                                                      scheme chez chibi
                                                      chicken gambit gerbil
                                                      guile mit-scheme racket
                                                      scsh
                                                      lfe inferior-lfe
                                                      shell term
                                                      perl rust
                                                      cwl)
  "List of major mode that fully control the tab behaviour and width.

These modes do not have both `pel-<mode>-tab-width' and a `pel-<mode>-use-tabs'
user-options variables.")

;; TODO: pel-config-major-mode does not seem to support shell-mode and
;;       term-mode properly.  Investigate and fix.

(defmacro pel-config-major-mode (target-mode &optional key-prefix &rest body)
  "Setup the major mode identified by TARGET-MODE.

TARGET-MODE is an unquoted symbol identifying the mode: it's the
major mode name without the -mode suffix.  Something like
emacs-lisp, c, python, etc...

The KEY-PREFIX argument is a PEL mode-specific key-prefix
unquoted symbol.  Something like pel:for-c and pel-for-make.  That
symbol must already been defined prior to the macro invocation,
and it should have been defined with a `define-pel-global-prefix'
form.  If KEY-PREFIX is nil or has the value :no-f12-keys then
no <f12> and <M-f12> PEL key prefixes are created for the major mode.

The BODY is a set of forms to execute when the major mode hook
executes, at the moment when a buffer with that major mode opens
and after the local variables have been loaded."
  (declare (indent 2))
  (let ((gn-fct1 (intern (format "pel--setup-for-%s-with-local-vars"
                                 target-mode)))
        (gn-docstring1
         (format "Activate %s setup, take local variables into account."
                 target-mode))
        (gn-fct2 (intern (format "pel--setup-for-%s" target-mode)))
        (gn-docstring2 (format "Set the environment for %s buffers."
                               target-mode))
        (gn-mode-name (intern (format "%s-mode" target-mode)))
        (gn-mode-hook (intern (format "%s-mode-hook" target-mode)))
        (gn-minor-modes (intern (format "pel-%s-activates-minor-modes"
                                        target-mode)))
        (gn-use-tabs (intern (format "pel-%s-use-tabs"
                                     target-mode)))
        (gn-tab-width (intern (format "pel-%s-tab-width"
                                      target-mode)))
        (gn-fname       (file-name-base (macroexp-file-name))))
    ;; When the <f12> key prefixes are defined, set them up first
    ;; in the function body to ensure they are available and will not shadow
    ;; another call to `pel-local-set-f12-M-f12' that wants to install a
    ;; sub-prefix.
    (when (and key-prefix
               (not (eq key-prefix :no-f12-keys)))
      (push `(pel-local-set-f12-M-f12 (quote ,key-prefix)) body))
    ;; Add the code that activates the minor modes identified by the
    ;;`pel-<mode>-activates-minor-modes' user-option.
    (setq body (append body `((pel-turn-on-local-minor-modes-in
                               (quote ,gn-minor-modes)))))
    ;; If the major mode is not one of the modes that do not need
    ;; to support hard-tab control and width create code that set them
    (unless (memq target-mode pel--tab-controlling-major-modes)
      (setq body (append body
                         `((setq-local tab-width ,gn-tab-width)
                           (setq-local indent-tabs-mode ,gn-use-tabs)))))
    ;; return the following generated code:
    `(progn
       (defun ,gn-fct2 ()
         ,gn-docstring2
         (progn
           ,@body))
       (declare-function ,gn-fct2 ,gn-fname)
       ;;
       (defun ,gn-fct1 ()
         ,gn-docstring1
         (add-hook 'hack-local-variables-hook (function ,gn-fct2) nil t))
       (declare-function ,gn-fct1 ,gn-fname)
       ;;
       (pel-check-minor-modes-in ,gn-minor-modes)
       (pel--mode-hook-maybe-call (function ,gn-fct1)
                                  (quote ,gn-mode-name)
                                  (quote ,gn-mode-hook)))))

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

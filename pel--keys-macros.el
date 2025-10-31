;;; pel--keys-macros.el --- Key binding macros.  -*- lexical-binding: t; -*-

;; Created   : Tuesday, September  1 2020.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2025-10-31 15:08:42 EDT, updated by Pierre Rouleau>

;; This file is part of the PEL package.
;; This file is not part of GNU Emacs.

;; Copyright (C) 2020, 2021, 2022, 2023, 2024, 2025  Pierre Rouleau
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

;; Lazy loading and package installation:

;; The first set of functions and macros provide mechanism to require, load,
;; autoload and byte-compiler declaration facilities.
;;
;;
;; @ `pel-require-at-load'
;;   - `pel--require-at-load'
;; @ `pel-require-after-init'
;;   - `pel--require-after-init'
;; @ `pel-eval-after-load'
;; @ `pel-set-auto-mode'
;; @ `pel-autoload-file'
;; @ `pel-declare-file'

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'pel--base)    ; use: `macroexp-file-name'
(require 'pel--macros)  ; use: `pel-append-to'
(require 'pel--options) ; use: `pel-use-call-graph', `pel-use-tree-sitter', ...
(require 'seq)          ; use: `seq-concatenate', `seq-drop', `seq-subseq'
(eval-when-compile
  (require 'cl-lib))    ; use: `cl-dolist', `cl-return'

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

(defconst pel--c-groups (let ((items '(c
                                       c-macro
                                       bison-mode
                                       electricity)))
                          (when pel-use-call-graph
                            (append items '(call-graph)))))

(defconst pel--c++-groups (let ((items '(cpp
                                         c-macro
                                         electricity)))
                            (when pel-use-call-graph
                              (append items '(call-graph)))))

(defconst pel--objc-groups (let ((items '(c
                                          c-macro
                                          electricity)))
                             (when pel-use-call-graph
                               (append items '(call-graph)))))

(defconst pel--awk-groups '(c
                            electricity))

(defconst pel--dired-groups '(dired
                              dired-git-info
                              dired-hide-dotfiles
                              dired-narrow
                              dired-sidebar
                              dired-x
                              files
                              ls-lisp
                              wdired)
  "List of groups used related to dired.")

;; Unfortunately the group used by elixir-ts-mode is not elixir but `elixir-ts'
(defconst pel--elixir-groups (if pel-use-tree-sitter
                                 '(elixir
                                   elixir-ts
                                   electricity)
                               '(elixir
                                 electricity))
  "List of groups related to Elixir.")

;; Unfortunately the group used by lua-ts-mode is not lua but lua-ts
(defconst pel--lua-groups (if pel-use-tree-sitter
                                 '(lua
                                   lua-ts)
                            '(lua)))

(defconst pel--highligh-groups (let ((items '(auto-highlight-symbol
                                              electricity
                                              iedit
                                              highlight-indentation
                                              hl-line
                                              paren-showing
                                              rainbow
                                              rainbow-delimiters
                                              smartparens
                                              vline)))
                                 (if (version< emacs-version "27.1")
                                     (append items '(fill-column-indicator))
                                   (append items '(beacon)))))

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
(defconst pel--spell-groups (if (version< emacs-version "27.1")
                               '(ispell
                                 flyspell)
                             '(ispell
                               flyspell
                               go-translate)))
;; Shells
(defconst pel--shell-launch-groups '(shell
                                     term
                                     terminals
                                     vterm))
(defconst pel--sh-scripting-groups '(sh
                                     sh-script
                                     sh-indentation
                                     electricity))


(defconst pel--undo-groups (let ((items '(undo
                                          undo-propose
                                          undo-tree)))
                             (when pel-emacs-28-or-later-p
                               (setq items (append items '(vundo))))
                             items))

(defconst pel--ruby-groups (if pel-use-tree-sitter
                               '(ruby
                                 ruby-ts
                                 electricity)
                             '(ruby
                              electricity)))

(defconst pel--verilog-groups (let ((items (if (fboundp 'verilog-ts-mode)
                                               '(verilog-mode verilog-ts)
                                             '(verilog-mode))))
                                (when pel-use-verilog-ext
                                  (setq items (append items '(verilog-ext))))
                                (when pel-use-veri-kompass
                                  (setq items (append items '(veri-kompass))))
                                items))

;; Unfortunately the group used by zig-ts-mode is not zig but zig-ts
(defconst pel--zig-groups (if pel-use-tree-sitter
                                 '(zig-mode
                                   zig-ts)
                            '(zig-mode)))



;; TODO: add logic in the processing of that table to allow the first element
;;       of a row to be a list of key sequences.
;;       This will help reduce duplication when several key sequences lead
;;       to the same data.
(defconst pel---prefix-to-topic-alist-1
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
    ([f11 ?$]        "spell-checking"   pel-pkg-for-spelling    ,pel--spell-groups)
    ([f11 ?']        "bookmarks"        pel-pkg-for-bookmark    (bookmark
                                                                 bm))
    ([f11 ?,]        "auto-completion"  pel-pkg-for-expand   (auto-complete
                                                              company
                                                              hippie-expand))
    ([f11 ?-]        "cut-paste"  pel-pkg-for-cut-and-paste (editing-basics
                                                             browse-kill-ring
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
      info
      interaction-log
      man
      minibuffer
      which-func
      which-key))
    ([f11 ?? ?k]     "help"             pel-pkg-for-keys        (command-log
                                                                 interaction-log
                                                                 hydra
                                                                 keycast
                                                                 which-func
                                                                 which-key))
    ([f11 9]         "indentation"      pel-pkg-for-indentation (indent
                                                                 indent-bars
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
    ([f11 32 ?A]     "pl-ada"           pel-pkg-for-ada         (ada ada-ts))
    ([f11 32 ?W]     "pl-awk"           pel-pkg-for-awk         ,pel--awk-groups)
    ([f11 32 ?C]     "pl-c++"           pel-pkg-for-c++         ,pel--c++-groups)
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
    ([f11 32 ?c]     "pl-c"             pel-pkg-for-c           ,pel--c-groups)
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
    (,(kbd "<f11> SPC M-f") "pl-factor" pel-pkg-for-factor      factor)
    ([f11 32 27 ?f]         "pl-factor" pel-pkg-for-factor      factor)
    ([f11 32 ?f]     "pl-forth"         pel-pkg-for-forth       (forth
                                                                 forth-smie
                                                                 forth-spec))
    ([f11 32 ?g]     "pl-go"            pel-pkg-for-go          (go
                                                                 go-cover
                                                                 godoc
                                                                 go-dot-mod
                                                                 electricity))
    ([f11 32 ?h]     "pl-haskell"       pel-pkg-for-haskell     haskell)
    ([f11 32 ?u]     "pl-lua"           pel-pkg-for-lua         ,pel--lua-groups)
    ([f11 32 ?T]     "pl-janet"         pel-pkg-for-janet       (janet
                                                                 ijanet
                                                                 inf-janet))
    ([f11 32 ?J]     "pl-java"          pel-pkg-for-java        (java c))
    ;; [:todo 2025-10-08, by Pierre Rouleau: Add ability to select the js or
    ;; js2-mode group automatically, corresponding to the major mode being
    ;; used for Javascript.]
    ([f11 32 ?i]     "pl-javascript"    pel-pkg-for-javascript  (js
                                                                 js2-mode
                                                                 js3-mode
                                                                 js-comint
                                                                 js2-refactor
                                                                 flow-minor-mode
                                                                 xref-js2))
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

    ([f11 32 ?0]     "ssh-files"    pel-pkg-for-ssh         ssh-file)
    ([f11 32 ?1]     "ssh-files"    pel-pkg-for-ssh         ssh-file)
    ([f11 32 ?2]     "pl-modula2"   pel-pkg-for-modula-2    modula2)
    ([f11 32 ?3]     "pl-meson"     pel-pkg-for-meson       meson)
    ([f11 32 ?4]     "pl-m4"        pel-pkg-for-m4          m4)
    ([f11 32 ?5]     "pl-ninja"     pel-pkg-for-ninja       ninja)
    ([f11 32 ?n]     "pl-nim"       pel-pkg-for-nim         (nim
                                                             electricity))
    ([f11 32 ?o]     "pl-ocaml"     pel-pkg-for-ocaml       (merlin
                                                             tuareg
                                                             tuareg-opam))
    ([f11 32 ?O]     "pl-odin"      pel-pkg-for-odin        (odin
                                                             flycheck-odin))
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
                                                              electricity
                                                              perl-repl
                                                              perl-live))
    ([f11 32 ?U]     "pl-ruby"      pel-pkg-for-ruby         ,pel--ruby-groups)
    ([f11 32 ?7]     "pl-seed7"     pel-pkg-for-seed7        seed7)
    ([f11 32 ?:]     "pl-smalltalk" pel-pkg-for-smalltalk    smalltalk-mode)
    ([f11 32 ?s]     "pl-swift"     pel-pkg-for-swift         swift)
    ([f11 32 ?t]     "pl-tcl"       pel-pkg-for-tcl           tcl)
    ([f11 32 ?v]     "pl-v"         pel-pkg-for-v            (v-mode
                                                              electricity))
    ([f11 32 ?V]     "hdl-verilog"  pel-pkg-for-verilog       ,pel--verilog-groups)
    ([f11 32 ?H]     "hdl-vhdl"     pel-pkg-for-vhdl          vhdl)

    ([f11 32 ?x]     "pl-elixir"    pel-pkg-for-elixir        ,pel--elixir-groups)
    ([f11 32 ?Z]     "pl-sh"        pel-pkg-for-sh-scripting ,pel--sh-scripting-groups)
    (,(kbd "<f11> SPC C-a") nil         pel-pkg-for-arc         (arc
                                                                 lispy))
    (,(kbd "<f11> SPC C-e") "pl-eiffel" pel-pkg-for-eiffel      eiffel)

    (,(kbd "<f11> SPC M-G") "pl-gleam"  pel-pkg-for-gleam       gleam-ts)
    ([f11 32 27 ?G]         "pl-gleam"  pel-pkg-for-gleam       gleam-ts)

    (,(kbd "<f11> SPC C-h") "pl-hy"     pel-pkg-for-hy)
    (,(kbd "<f11> SPC C-j") "pl-clojure" pel-pkg-for-clojure    (clojure
                                                                 cider
                                                                 cljr
                                                                 lispy))
    (,(kbd "<f11> SPC C-l") "pl-lfe"    pel-pkg-for-lfe         (lfe
                                                                 lispy))
    (,(kbd "<f11> SPC SPC C-l") "pl-lfe" pel-pkg-for-lfe        (lfe
                                                                 lispy))

    (,(kbd "<f11> SPC C-o") "pl-objc"   pel-pkg-for-objc
     ,pel--objc-groups)

    (,(kbd "<f11> SPC C-p") "pl-pike"   pel-pkg-for-pike  c)
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

    (,(kbd "<f11> SPC SPC b") "ibuffer-mode"  pel-pkg-for-ibuffer (ibuffer ibuffer-vc))
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
    ([f11 ?f]
     ("file-mngt"
      "web")
     pel-pkg-for-filemng     (files
                              fzf
                              recentf
                              popup-switcher
                              x509))

    ;; no PDF for browse yet, the info is  in file-mngt.
    ([f11 ?B]        "file-mngt"        (pel-pkg-for-file-browse
                                         pel-pkg-for-web-browse
                                         pel-pkg-for-ztree)
     (dir-treeview
      rfc-mode-group
      treemacs
      lsp-treemacs
      ztree
      ))
    ([f11 ?B ?N]     "file-mngt"        pel-pkg-for-neotree      neotree)
    ([f11 ?f ?a]     "file-mngt"        nil                      ffap)
    ([f11 ?f ?p]     "file-mngt"        pel-pkg-for-project-mng  ffip)
    ([f11 ?f ?r]     "file-mngt"        nil                      auto-revert)
    ([f11 ?f ?v]     "file-variables")
    ([f11 ?f ?v ?D]  "file-variables")
    ([f11 ?g]        "grep"             pel-pkg-for-grep        (grep
                                                                 ag
                                                                 deadgrep
                                                                 fzf
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
    )
  "Map from key prefix array to topic string.
The topic string correspond to the base name of the PDF file
stored inside the doc/pdf directory.")

(defconst pel---prefix-to-topic-alist-2
  `(([f11 ?o]        "sorting"          nil)
    ([f11 ?r]        "registers"        nil)
    ([f11 ?s]        "search-replace"   pel-pkg-for-search      (isearch
                                                                 anzu
                                                                 iedit
                                                                 easy-escape
                                                                 fzf
                                                                 swiper))
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
    ([f11 ?u]        "undo-redo-repeat" pel-pkg-for-undo        ,pel--undo-groups)
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
                                                           golden-ratio
                                                           winner
                                                           windmove
                                                           windresize
                                                           winum))
    (,(kbd "<M-f11> M-=") "tab-bar" nil tab-bar)
    ([M-f11 27 61] "tab-bar" nil tab-bar)
    ([f11 ?w ?P] "windows"  pel-pkg-for-windows purpose)
    ([f11 ?y]  "inserting-text"   pel-pkg-for-insertions  (yasnippet
                                                           yasnippet-snippets
                                                           yas-minor))

    ([f11 ?z]                 "shells"  pel-pkg-for-shells  ,pel--shell-launch-groups)
    ([f11 32 ?z ?s]           "shell-mode"  pel-pkg-for-shells      (shell comint))
    ([f11 32 ?z ?t]           "term-mode"   pel-pkg-for-term-mode   term)
    ([f11 32 ?z ?f]           "eat-mode"    pel-pkg-for-eat-mode    eat)
    ([f11 32 ?z ?v]           "vterm-mode"  pel-pkg-for-vterm-mode  vterm)

    ([f11 ?|]        "scrolling"  pel-pkg-for-scrolling   (frame
                                                           follow
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

    (,(kbd "<f11> SPC M-a") "asciidoc"         pel-pkg-for-asciidoc    adoc)
    ([f11 32 27 ?a]         "asciidoc"         pel-pkg-for-asciidoc    adoc)

    (,(kbd "<f11> SPC M-r") "mode-rst"         pel-pkg-for-rst        rst)
    ([f11 32 27 ?r]         "mode-rst"         pel-pkg-for-rst        rst)
    (,(kbd "<f11> SPC M-m") "mode-markdown"    pel-pkg-for-markdown   ,pel--markdown-groups)
    ([f11 32 27 ?m]         "mode-markdown"    pel-pkg-for-markdown   ,pel--markdown-groups)

    (,(kbd "<f11> SPC M-l") "outline"          pel-pkg-for-outline     outlines)
    ([f11 32 27 ?l]         "outline"          pel-pkg-for-outline     outlines)

    (,(kbd "<f11> SPC M-o") "mode-org-mode"    pel-pkg-for-org-mode    org)
    ([f11 32 27 ?o]         "mode-org-mode"    pel-pkg-for-org-mode    org)

    (,(kbd "<f11> SPC M-p") "pl-pascal" pel-pkg-for-pascal    pascal)
    ([f11 32 27 ?p]         "pl-pascal" pel-pkg-for-pascal    pascal)

    (,(kbd "<f11> SPC M-u") "plantuml" pel-pkg-for-plantuml    plantuml-mode)
    ([f11 32 27 ?u]         "plantuml" pel-pkg-for-plantuml    plantuml-mode)

    (,(kbd "<f11> SPC M-z") "pl-zig" pel-pkg-for-zig    ,pel--zig-groups)
    ([f11 32 27 ?z]         "pl-zig" pel-pkg-for-zig    ,pel--zig-groups)

    (,(kbd "<f11> SPC M-M") "mscgen" pel-pkg-for-mscgen    mscgen)
    ([f11 32 27 ?M]         "mscgen" pel-pkg-for-mscgen    mscgen)

    (,(kbd "<f11> SPC M-R") "rpm"       pel-pkg-for-rpm    rpm-spec)
    ([f11 32 27 ?R]         "rpm"       pel-pkg-for-rpm    rpm-spec)

    ;; (,(kbd "<f11> SPC M-S") "rpm"       pel-pkg-for-rpm    rpm-spec)
    ;; ([f11 32 27 ?S]         "rpm"       pel-pkg-for-rpm    rpm-spec)

    (,(kbd "<f11> SPC M-c") "cwl"              pel-pkg-for-cwl     ,pel--yaml-groups)
    ([f11 32 27 ?c]         "cwl"              pel-pkg-for-cwl     ,pel--yaml-groups)
    (,(kbd "<f11> SPC M-y") "yaml"             pel-pkg-for-yaml    ,pel--yaml-groups)
    ([f11 32 27 ?y]         "yaml"             pel-pkg-for-yaml    ,pel--yaml-groups)

    (,(kbd "<f11> SPC M-Y") "yang"      pel-pkg-for-spec-definition)
    ([f11 32 27 ?Y]         "yang"      pel-pkg-for-spec-definition)))

(defconst pel--prefix-to-topic-alist
  (append pel---prefix-to-topic-alist-1 pel---prefix-to-topic-alist-2))

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
;; [:todo 2025-10-08, by Pierre Rouleau: Add logic to search for the classic
;; mode when the ts-mode is requested and a mapping is done for it in the
;; `major-mode-remap-alist'. Once this is in place, remove the -ts specific entries.]
(defconst pel--mode-letter-alist
  '(("Custom"          [f11 f2])
    ("dired"           [dired])
    ("dired-sidebar"   [dired])
    ("ada"             [f11 32 ?A])
    ("ada-ts"          [f11 32 ?A])
    ("apples"          [f11 32 ?a])
    ("awk"             [f11 32 ?W])
    ("c++"             [f11 32 ?C])
    ("c++-ts"          [f11 32 ?C])
    ("c"               [f11 32 ?c])
    ("c-ts"            [f11 32 ?c])
    ("lisp"            [f11 32 ?L])
    ("common-lisp"     [f11 32 ?L])     ; an alias for lisp.
    ("clojure"         [f11 32 10])
    ("d"               [f11 32 ?D])
    ("elixir"          [f11 32 ?x])
    ("elixir-ts"       [f11 32 ?x])
    ("lisp-interaction" [f11 32 ?l])    ; for scratch buffer
    ("suggest"         [f11 32 ?l])     ; suggest -> emacs-lisp help
    ("emacs-lisp"      [f11 32 ?l])
    ("erlang"          [f11 32 ?e])
    ("erlang-ts"       [f11 32 ?e])
    ("factor"          [f11 32 27 ?f])
    ("forth"           [f11 32 ?f])
    ("go"              [f11 32 ?g])
    ("go-ts"           [f11 32 ?g])
    ("janet"           [f11 32 ?T])
    ("julia"           [f11 32 ?j])
    ("meson"           [f11 32 ?3])
    ("m4"              [f11 32 ?4])
    ("ninja"           [f11 32 ?5])
    ("makefile"        [f11 32 ?M])
    ("makefile-bsdmake" [f11 32 ?M])
    ("makefile-gmake"  [f11 32 ?M])
    ("makefile-makepp" [f11 32 ?M])
    ("makefile-automake" [f11 32 ?M])
    ("makefile-imake"  [f11 32 ?M])
    ("makefile-nmake"  [f11 32 ?M])
    ("python"          [f11 32 ?p])
    ("python-ts"       [f11 32 ?p])
    ("arc"             [f11 32 1])
    ("eiffel"          [f11 32 5])
    ("gleam"           [f11 32 27 ?G])
    ("gleam-ts"        [f11 32 27 ?G])
    ("haskell"         [f11 32 ?h])
    ("lua"             [f11 32 ?u])
    ("lua-ts"          [f11 32 ?u])
    ("vhdl"            [f11 32 ?H])
    ("hy"              [f11 32 8])
    ("java"            [f11 32 ?J])
    ("java-ts"         [f11 32 ?J])
    ("js"              [f11 32 ?i])     ; javascript
    ("js-ts"           [f11 32 ?i])     ; javascript
    ("js2"             [f11 32 ?i])     ; javascript
    ("js3"             [f11 32 ?i])     ; javascript
    ("lfe"             [f11 32 12])
    ("inferior-lfe"    [f11 32 32 12])
    ("ibuffer"         [f11 32 32 ?b])
    ("vc-dir"          [f11 32 32 ?v])
    ("m2"              [f11 32 ?2])     ; modula-2
    ("nim"             [f11 32 ?n])
    ("nimscript"       [f11 32 ?n])
    ("ocaml"           [f11 32 ?o])
    ("odin"            [f11 32 ?O])
    ("tuareg"          [f11 32 ?o])
    ("perl"            [f11 32 ?P])
    ("cperl"           [f11 32 ?P])
    ("objc"            [f11 32 15])
    ("objective-c"     [f11 32 15])
    ("pike"            [f11 32 16])
    ("rexx"            [f11 32 ?R])
    ("rpm"             [f11 32 27 ?R])
    ;; ("rpmspec"         [f11 32 27 ?S])
    ("ruby"            [f11 32 ?U])
    ("ruby-ts"         [f11 32 ?U])
    ("rust"            [f11 32 ?r])
    ("rust-ts"         [f11 32 ?r])
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
    ("sh"              [f11 32 ?Z])
    ("seed7"           [f11 32 ?7])
    ("smalltalk"       [f11 32 ?:])
    ("swift"           [f11 32 ?s])
    ("tcl"             [f11 32 ?t])
    ("v"               [f11 32 ?v])
    ("verilog"         [f11 32 ?V])
    ("verilog-ts"      [f11 32 ?V])
    ("adoc"            [f11 32 27 ?a])
    ("markdown"        [f11 32 27 ?m])
    ("netrexx"         [f11 32 ?N])
    ("rst"             [f11 32 27 ?r])
    ("cwl"             [f11 32 27 ?c])
    ("outline"         [f11 32 27 ?l])
    ("org"             [f11 32 27 ?o])
    ("graphviz-dot"    [f11 32 27 ?g])
    ("mscgen"          [f11 32 27 ?M])
    ("pascal"          [f11 32 27 ?p])
    ("plantuml"        [f11 32 27 ?u])
    ("yaml"            [f11 32 27 ?y])
    ("yang"            [f11 32 27 ?Y])
    ("zig"             [f11 32 27 ?z])
    ("zig-ts"          [f11 32 27 ?z])
    ;; shells and terminals
    ("shell"           [f11 32 ?z ?s])
    ("term"            [f11 32 ?z ?t])
    ("vterm"           [f11 32 ?z ?v])
    ("eat"             [f11 32 ?z ?f])
    ;; diff modes
    ("diff"            [f11 32 32 ?d ?d])
    ("ediff"           [f11 32 32 ?d ?e])
    ("smerge"          [f11 32 32 ?d ?s])
    ("ssh-authorized-keys" [f11 32 ?0])
    ("ssh-known-hosts" [f11 32 ?1])
    )
  "Maps the name of a major mode (without the -mode suffix)
to a symbol or key sequence array to use as map key inside
`pel--prefix-to-topic-alist' table.")

(defun pel--major-mode-keyseq (keyseq)
  "Return global mode index for major mode KEYSEQ.
The KEYSEQ is a sequence prefix that starts with f12, used as a
short cut in a major mode.  It may have only one or several keys.
Its meaning depend on the currently active major mode.  Return
the corresponding global key sequence that means the same thing
so it can be used as an index inside variable
`pel--prefix-to-topic-alist'."
  (unless (eq (elt keyseq 0) 'f12)
    (error "Logic error!! keyseq should start with f12.  It is %s" keyseq))
  ;; (message "pel--major-mode-keyseq keyseq=%s" keyseq)
  (let* ((mode-str (substring (symbol-name major-mode) 0 -5))
         (keyidx (cadr (assoc mode-str pel--mode-letter-alist))))
    ;; (message "pel--major-mode-keyseq --> mode-str = %s" mode-str)
    ;; (message "pel--major-mode-keyseq --> keyidx   = %s" keyidx)
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
    ;; (message "pel--kte-for: prefix-key : %s" prefix-key)
    (unless (or (memq prefix-key '(f6 f7 f8 f11 f12 M-f11 M-f12))
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


(defun pel-shell-scripting-language ()
  "Return string identifying the shell scripting language for current buffer.

Something like \"sh\", \"bash\", or \"zsh\".
If the current buffer "
  (unless (fboundp 'sh-shell)
    (require 'sh-script))
  (defvar sh-shell)
  (symbol-name sh-shell))

(defun pel-lang-pdf ()
  "Return list of language specific PDF file for current buffer.

Each entry of the list is file base name without file extension."
  (let ((major-mode-str (symbol-name major-mode)))
    (cond
     ((eq major-mode 'sh-mode)
      ;; return "sh", "bash", zsh", based on shell scripting language used.
      (list (pel-shell-scripting-language)))

     ((memq major-mode '(emacs-lisp-mode lisp-interaction-mode))
      ;; Special case: these old files are located in the doc/pdf directory.
      ;; Because the files are located right in the doc/pdf and not inside the
      ;; doc/pdf/lang directory, and because the files returned by this
      ;; function are supposed to be in the lang subdir, return a file name
      ;; that force looking into the above dir.
      (list "../plm-lispy" "../emacs-lisp-types"))

     ((pel-string-starts-with-p major-mode-str "makefile-")
      ;; Return MMMM for the makefile-MMMM-mode modes.
      ;; Example: for makefile-gmake-mode: return "gmake".
      (list (substring major-mode-str 9 -5)))

     (t  (user-error "No language specific for this major mode.")))))

;;-pel-autoload
(defun pel-help-pdf (&optional n)
  "Open the PEL PDF file(s) for the current context.

The argument N controls the selection of the PDF file and way it is opened.

======= ======================== ============================================
N value Content                  Method
======= ======================== ============================================
none    Mode-specific PDF        Open local PDF
 1      Mode-specific PDF        Open local PDF with PDF reader
-1      Mode-specific PDF        Open GitHub raw PDF with default web browser
>= 2    Language/Syntax/Ref PDF  Open local PDF
<= -2   Language/Syntax/Ref PDF  Open GitHub raw PDF with default web browser
======= ======================== ============================================

The local PDF is opened according to the value selected by the
`pel-open-pdf-method' user-option; which defaults to the local
PDF viewer.

The sign of the numeric value identifies whether the local PDF
file or the corresponding GitHub hosted raw PDF file is opened.
However, if the user-option variable `pel-flip-help-pdf-arg' is
set, it's the other way around: the GitHub remote file is opened
by default.

To open the web file, the function uses Emacs default browse
mechanism specified by the user-option variable
`browse-url-browser-function' unless the user-option variable
`pel-browser-used' forces the use of a specific browser.

If your system default browser can not render PDF files directly
and downloads them, then you can force the use of the Firefox
browser (which renders PDF) by setting `pel-browser-used' to
`firefox'.

Using a browser that is capable of direct rendering of PDF
produces a much better user experience: you will be able to
quickly navigate through PEL documentation inside the browser.

The `pel-help-pdf' function determines the requested PDF topic by the key
sequence that led to the execution of the command.  These key sequences
normally end with the F1 key."
  (interactive "p")
  ;; (message "N is %s" n)
  (let* ((open-github-page-p (< n 0))
         (category  (when (>= (abs n) 2) "lang"))
         (keyseq (pel--keyseq))         ; identify the first key(s)
         (kte    (pel--kte-for keyseq)) ; pel--prefix-to-topic-alist entry
         (pdfs   (if category
                     (pel-lang-pdf)
                   (pel--kte-pdfs kte))))
    ;; (message "pel--keyseq   :--> %s" keyseq)
    ;; (message "pel--kte-for  :--> %s" kte)
    ;; (message "pel--kte-pdfs :--> %s" pdfs)
    (unless pdfs
      (error "No PDF entry in pel--prefix-to-topic-alist for %s.\n\
There should be no key binding!" keyseq))
    (let* ((open-github-file-p (if pel-flip-help-pdf-arg
                                   (not open-github-page-p)
                                 open-github-page-p))
           (url (pel-pdf-file-url
                 (pel--kte-select-topic "Open the PDF file: " pdfs)
                 open-github-file-p
                 category)))
      ;; (message "url: %s" url)
      (pel--help-browse url))))

(defconst pel--topic-alias
  '(
    ;; programming languages alias: all of their PDF files start with 'pl-'
    ("applescript"      . "pl-applescript")
    ("ada"              . "pl-ada")
    ("arc"              . "pl-arc")
    ("awk"              . "pl-awk")
    ("c++"              . "pl-c++")
    ("c"                . "pl-c")
    ("common-lisp"      . "pl-common-lisp")
    ("clojure"          . "pl-clojure")
    ("d"                . "pl-d")
    ("eiffel"           . "pl-eiffel")
    ("elixir"           . "pl-elixir")
    ("emacs-lisp"       . "pl-emacs-lisp")
    ("erlang"           . "pl-erlang")
    ("factor"           . "pl-factor")
    ("forth"            . "pl-forth")
    ("gleam"            . "pl-gleam")
    ("go"               . "pl-go")
    ("haskell"          . "pl-haskell")
    ("hy"               . "pl-hy")
    ("janet"            . "pl-janet")
    ("java"             . "pl-java")
    ("javascript"       . "pl-javascript")
    ("js"               . "pl-javascript")
    ("julia"            . "pl-julia")
    ("lfe"              . "pl-lfe")
    ("lua"              . "pl-lua")
    ("m4"               . "pl-m4")
    ("make"             . "pl-make")
    ("m2"               . "pl-modula2")
    ("modula2"          . "pl-modula2")
    ("meson"            . "pl-meson")
    ("ninja"            . "pl-ninja")
    ("nim"              . "pl-nim")
    ("ocaml"            . "pl-ocaml")
    ("odin"             . "pl-odin")
    ("pascal"           . "pl-pascal")
    ("perl"             . "pl-perl")
    ("cperl"            . "pl-perl")
    ("objc"             . "pl-objc")
    ("objective-c"      . "pl-objc")
    ("pike"             . "pl-pike")
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
    ("seed7"            . "pl-seed7")
    ("smalltalk"        . "pl-smalltalk")
    ("swift"            . "pl-swift")
    ("tcl"              . "pl-tcl")
    ("v"                . "pl-v")
    ("verilog"          . "hdl-verilog")
    ("vhdl"             . "hdl-vhdl")
    ("zig"              . "pl-zig")
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
    ("adoc"             . "asciidoc")
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
    ("diff"             . "diff-merge")
    ("execute"          . "pl-emacs-lisp")
    ("ssh"              . "ssh-files")
    ("x.509"            . "x509")
    ("rpm"              . "rpm")
    ;; OS keys
    ("macOS"            . "macOS-keys")
    ("mint"             . "linux-mint-20-desktop-keys")
    ("rocky"            . "rockylinux8-desktop-keys" )
    ("ubuntu"           . "ubuntu-16-04-desktop-keys" )
    ;; lang sub-directory PDF
    ("GNU make"         . "lang/gmake")
    ("GNU readline"     . "lang/gnu-readline")
    ("bash-info"        . "lang/bash")
    ("sh-info"          . "lang/sh")
    ("zsh-info"         . "lang/zsh")
    ("perl5"            . "lang/perl5")
    ("perl-info"        . "lang/perl5")
    ;; cmd sub-directory PDF
    ("ls"               . "cmd/ls")
    )
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

(defun pel-help-open-pdf (topic &optional open-github-page-p)
  "Open PDF help for TOPIC string potentially OPEN-WEB-PAGE."
  (pel--help-browse (pel-pdf-file-url topic open-github-page-p)))

;;-pel-autoload
(defun pel-help-on-completion-input (&optional open-github-page-p)
  "Open the input completion help PDF, in a browser if arg OPEN-WEB-PAGE set.

By default the function opens the local PDF file unless the
OPEN-GITHUB-PAGE is specified, in which case it opens the GitHub
hosted raw PDF file.  However, if the user-option variable
`pel-flip-help-pdf-arg' is set, it's the other way around: the
GitHub remote file is opened by default."
  (interactive "P")
  (pel-help-open-pdf "completion-input" (if pel-flip-help-pdf-arg
                                            (not open-github-page-p)
                                          open-github-page-p)))

;;-pel-autoload
(defun pel-help-on-outline (&optional open-github-page-p)
  "Open the outline help PDF, in a browser if arg OPEN-WEB-PAGE set.

By default the function opens the local PDF file unless the
OPEN-GITHUB-PAGE is specified, in which case it opens the GitHub
hosted raw PDF file.  However, if the user-option variable
`pel-flip-help-pdf-arg' is set, it's the other way around: the
GitHub remote file is opened by default."
  (interactive "P")
  (pel-help-open-pdf "outline" (if pel-flip-help-pdf-arg
                                   (not open-github-page-p)
                                 open-github-page-p)))

;;-pel-autoload
(defun pel-help-pdf-select (&optional open-github-page-p)
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
    (pel-help-open-pdf topic open-github-page-p)))

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
  '(("ada"         . "ada-mode")
    ("ada-ts"      . "ada-ts-mode")
    ("rxt"         . "pcre2el")
    ("Imenu-Plus"  . "imenu+")
    ("Ztree"       . "ztree-view")
    ("command-log" . "command-log-mode")
    ("clojure"     . "clojure-mode")
    ("cljr"        . "clj-refactor")
    ("edts"        . "edts-mode")
    ("electricity" . "electric")
    ("erlstack"    . "erlstack-mode")
    ("factor"      . "factor-mode")
    ("grip"        . "grip-mode")
    ("janet"       . "janet-mode")
    ("lfe"         . ("lfe-indent"
                      "lfe-mode"))   ; several files - defgroup is in lfe-mode
    ("markdown"    . "markdown-mode")
    ("netrexx"     . "netrexx-mode")
    ("odin"        . "odin-mode")
    ("purpose"     . ("window-purpose-configuration"
                      "window-purpose-layout"
                      "window-purpose-switch"
                      "window-purpose-utils"
                      "window-purpose-x"
                      "window-purpose-core")) ; several files - defgroup in -core
    ("racket"      . "racket-custom")
    ("rainbow"     . "rainbow-mode")
    ("rfc-mode-group" . "rfc-mode")
    ("ffip"        . "find-file-in-project")
    ("x509"        . "x509-mode")
    ("ssh-file"    . "ssh-file-modes"))
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
If OTHER-WINDOW is non-nil (use \\[universal-argument]),
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
        (docstring (format "Customize PEL %s support.
If OTHER-WINDOW is non-nil (use \\[universal-argument]),
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

(defun pel--get-kte (keyseq)
  ""
  (assoc keyseq pel--prefix-to-topic-alist))

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
  (let* ((keyseq   (eval key))          ; key is a kbd expression.
         (kte      (pel--get-kte keyseq))
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

;; ---------------------------------------------------------------------------

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

;; --

(defun pel--eval-after-load-error (feature error)
  "Display warning for FEATURE loaded by `pel-eval-after-load'."
  (display-warning 'pel-eval-after-load
                   (format "Failed configuring %s: %s"
                           feature
                           error)
                   :error))

(defconst pel--ts-mode-with-fixer '(ada-ts-mode
                                    elixir-ts-mode
                                    erlang-ts-mode
                                    go-ts-mode
                                    js-ts-mode
                                    rust-ts-mode
                                    zig-ts-mode)
  "List of Tree Sitter modes that require execution of a mode fixer function.

The fixer mode function has a name that has a format like
pel--MODE-fixer with where MODE corresponds to the name of the mode
taken from this list.")

(defmacro pel-eval-after-load (features &rest body)
  "Evaluate BODY after the FEATURES has been loaded.
FEATURE is either a feature symbol or a list of feature symbols.
Both must be unquoted.
A list of feature symbol is useful, for example, when the tree-sitter
mode is provided by a different file them the classic major mode,
and the tree-sitter mode file does not load the classic mode file."
  (declare (indent 1))
  (let ((code nil)
        (feature-body nil))
    (dolist (the-feature (if (listp features) features (list features)))
      (setq feature-body nil)
      (when (memq the-feature pel--ts-mode-with-fixer)
        (let ((fixer-fct (intern (format "pel--%s-fixer" the-feature))))
          (pel-append-to feature-body
            `((when (fboundp (quote ,fixer-fct))
                (,fixer-fct))))))
      (pel-append-to feature-body
        `((condition-case-unless-debug err
              (progn ,@body)
            (error (pel--eval-after-load-error (quote ,the-feature)
                                               err)))))
      (pel-append-to code
        `((with-eval-after-load (quote ,the-feature)
            ,@feature-body))))
    ;; Return the generated code for all features.
    `(progn
       ,@code)))

;; --

(defun pel--mode-hook-maybe-call (fct mode hook &optional append)
  "Schedule FCT as the MODE HOOK: call it if buffer is currently in that MODE.
The function FCT is added at the beginning of the hook list unless the
optional argument APPEND is non-nil, in which case it is added at the end."
  (add-hook hook fct append)
  ;; if the current mode is the required mode also run the specified function
  (if (eq major-mode mode)
      (funcall fct)))

(defconst pel--tab-controlling-major-modes
  '(cwl
    ;; Gleam community decided against hard tab and decided to fix
    ;; indentation to 2 spaces but PEL still provides the user-options for
    ;; tab-width and hard tab control for other editing purposes.
    go
    go-dot-mod
    go-mod                              ; for go-mod-ts-mode
    intel-hex
    janet
    js2 js3
    lfe inferior-lfe
    lisp arc clojure
    makefile
    nimscript
    nix
    perl
    rust
    scheme chez chibi chicken gambit gerbil guile mit-scheme racket scsh
    seed7
    shell
    ssh-authorized-keys ssh-known-hosts
    term
    tup)
  "List of major mode that fully control the tab behaviour and width.

These modes do not have both `pel-<mode>-tab-width' and a `pel-<mode>-use-tabs'
user-options variables.")

(defun pel-treesit-remap-available-for (mode)
  "Return non-nil when treesit is available the ts MODE can use MODE.
MODE is a symbol like \\='c or \\='lisp identifying the major mode."
  (and pel-use-tree-sitter
       (pel-treesit-language-available-p mode)
       (boundp 'major-mode-remap-alist)))

;; TODO: pel-config-major-mode does not seem to support shell-mode and
;;       term-mode properly.  Investigate and fix.

(defmacro pel-config-major-mode (target-mode key-prefix ts-option &rest body)
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

The TS-OPTION control how tree-sitter mode is supported.
This can only be one of the following:
- :no-ts          : no special tree-sitter support
- :ts-only        : support for tree-sitter specific mode only is requested,
                    but no support for the classic mode
- :same-for-ts    : when the tree-sitter-based mode derives from the normal
                    mode and PEL must support both.
- :independent-ts : when the ts-sitter mode exists but does not derive from
                    the normal mode and PEL must support both.

The BODY is a set of forms to execute when the major mode hook
executes, at the moment when a buffer with that major mode opens
and after the local variables have been loaded."
  (declare (indent 3))
  (unless (memq ts-option '(:no-ts :ts-only :same-for-ts :independent-ts))
    (display-warning 'Invalid-PEL-code
                     (format
                      "Invalid (pel-config-major-mode %S %S %S)"
                      target-mode key-prefix ts-option)))
  (let ((gn-fct1 (intern (format "pel--setup-for-%s-with-local-vars"
                                 target-mode)))
        (gn-docstring1
         (format "\
Activate %s setup, take local variables into account.
Function created by the `pel-config-major-mode' macro."
                 target-mode))
        (gn-fct2 (intern (format "pel--setup-for-%s" target-mode)))
        (gn-docstring2 (format "Set the environment for %s buffers."
                               target-mode))
        (gn-mode-name (intern (format "%s-mode" target-mode)))
        (gn-ts-mode-name (intern (format "%s-ts-mode" target-mode)))
        (gn-mode-hook (intern (format "%s-mode-hook" target-mode)))
        (gn-ts-mode-hook (intern (format "%s-ts-mode-hook" target-mode)))
        (gn-minor-modes (intern (format "pel-%s-activates-minor-modes"
                                        target-mode)))
        (gn-use      (intern (format "pel-use-%s" target-mode)))
        (gn-use-tabs (intern (format "pel-%s-use-tabs"
                                     target-mode)))
        (gn-tab-width (intern (format "pel-%s-tab-width"
                                      target-mode)))
        (gn-tie-indent-2-tab (intern
                              (format "pel-%s-tie-indent-to-tab-width"
                               target-mode)))
        (gn-fname       (file-name-base (macroexp-file-name)))
        (newbody nil)
        (hook-body nil))
    ;; Add code to newbody in order: some code is placed *before* BODY
    ;; to allow BODY to see the values and possibly modify them.
    ;; Some code is added *after* the BODY.  BODY is a list.
    ;;
    ;; 1 - Code before BODY
    ;; If the major mode is not one of the modes that do not need
    ;; to support hard-tab control and width create code that set them
    (unless (memq target-mode pel--tab-controlling-major-modes)
      ;; Starting with Emacs 30, org-mode only supports a tab-width of 8
      (unless (and pel-emacs-30-or-later-p
                   (eq target-mode 'org))
        (pel-append-to newbody
          `((unless (assoc 'tab-width file-local-variables-alist)
              (setq-local tab-width ,gn-tab-width)))))
      (pel-append-to newbody
        `((unless (assoc 'indent-tabs-mode file-local-variables-alist)
            (setq-local indent-tabs-mode ,gn-use-tabs))))
      (when (boundp gn-tie-indent-2-tab)
        (pel-append-to newbody
          `((pel--set-indent-control-variables ,gn-tie-indent-2-tab)))))

    ;; - Add tree sitter control if necessary
    (when (and (eq ts-option :same-for-ts)
               (boundp 'major-mode-remap-alist))
      ;; There are no reasons to use major-mode when the major-ts-mode
      ;; mode is available and working.  Therefore, when the tree-sitter mode
      ;; is requested by the user for this major mode, ensure that whenever
      ;; major-mode is requested, major-ts-mode is used.
      ;; See: https://cgit.git.savannah.gnu.org/cgit/emacs.git/tree/etc/NEWS?h=emacs-30#n123
      (pel-append-to newbody
        `((when (pel-treesit-remap-available-for (quote ,target-mode))
            (if (eq ,gn-use 'with-tree-sitter)
                (add-to-list (quote major-mode-remap-alist)
                             (quote
                              (,gn-mode-name . ,gn-ts-mode-name)))
              (add-to-list (quote major-mode-remap-alist)
                           (quote (,gn-mode-name))))))))
    ;;
    ;; 2 - Include BODY
    (pel-append-to newbody body)
    ;;
    ;; 3 - Include code that must be done *after* BODY:
    ;;
    ;; When the <f12> key prefixes are defined, set them up first
    ;; in the function body to ensure they are available and will not shadow
    ;; another call to `pel-local-set-f12-M-f12' that wants to install a
    ;; sub-prefix.
    (when (and key-prefix
               (not (eq key-prefix :no-f12-keys)))
      (pel-append-to newbody
        `((pel-local-set-f12-M-f12 (quote ,key-prefix)))))

    ;; Add the code that activates the minor modes identified by the
    ;;`pel-<mode>-activates-minor-modes' user-option.
    (pel-append-to newbody
      `((pel-turn-on-local-minor-modes-in
         (quote ,gn-minor-modes))
        (pel-check-minor-modes-in ,gn-minor-modes)))

    ;; 4 - Prepare the code that is invoked after the newbody
    (pel-append-to hook-body
      `((declare-function ,gn-fct2 ,gn-fname)
        (defun ,gn-fct1 ()
          ,gn-docstring1
          (add-hook 'hack-local-variables-hook
                    (function ,gn-fct2) nil t))
        (declare-function ,gn-fct1 ,gn-fname)))

    ;; 4.1 - Append support for classic mode if necessary
    (unless (eq ts-option :ts-only)
      (pel-append-to hook-body
        `((pel--mode-hook-maybe-call (function ,gn-fct1)
                                     (quote ,gn-mode-name)
                                     (quote ,gn-mode-hook)))))
    ;; 4.1 - Append support for ts-mode if necessary
    (when (memq ts-option '(:ts-only :same-for-ts :independent-ts))
      (pel-append-to hook-body
        `((pel--mode-hook-maybe-call (function ,gn-fct1)
                                     (quote ,gn-ts-mode-name)
                                     (quote ,gn-ts-mode-hook)))))

    ;; 5 - Return the following generated code:
    `(progn
       (defun ,gn-fct2 ()
         ,gn-docstring2
         (progn
           ,@newbody))
       (progn
         ,@hook-body))))

;; [:todo 2025-05-17, by Pierre Rouleau: Add support for packages that
;;  have a same symbols for mode and features, like Ada, which is
;;  supported by : ada-mode in the file feature ada-mode
;;           and : ada-ts-mode in the file feature ada-ts-mode
;;  while keeping the ability to support modes where the feature name
;;  does not end with '-mode'.
;;  Perhaps the code should accept 4 symbols in case the code is implemented
;;  in files that have several functions in them and a feature name that
;;  differs completely.]

;; [:todo 2025-10-08, by Pierre Rouleau: Update the following: it may not be
;; required anymore]
(defmacro pel-config-major-mode-with-ts (target-mode
                                         &optional key-prefix
                                         &rest body)
  "Setup the major-mode and tree-sitter major mode for TARGET_MODE.

See `pel-config-major-mode' for the description of the arguments;
this uses the exact same arguments.
"
  (declare (indent 2))
  (let ((gn-mode    (intern (format "%s-mode"    target-mode)))
        (gn-ts-mode (intern (format "%s-ts-mode" target-mode)))
        (gn-ts-target-mode (intern (format "%s-ts" target-mode)))
        (gn-tab-width    (intern (format "pel-%s-tab-width" target-mode)))
        (gn-ts-tab-width (intern (format "pel-%s-ts-tab-width" target-mode)))
        (gn-use-tabs     (intern (format "pel-%s-use-tabs" target-mode)))
        (gn-ts-use-tabs  (intern (format "pel-%s-ts-use-tabs" target-mode)))
        (gn-activates-mm (intern (format "pel-%s-activates-minor-modes" target-mode)))
        (gn-ts-activates-mm (intern (format "pel-%s-ts-activates-minor-modes" target-mode))))
    ;; return the following generated code:
    `(progn
       (pel-major-mode-use-tree-sitter (quote ,gn-mode) (quote ,gn-ts-mode))
       (pel-eval-after-load ,target-mode
         ;; 1- setup for the ts-mode when it is available
         (when (pel-major-ts-mode-supported-p (quote ,target-mode))
           ;; create and set ts-mode mirroring variables
           (progn
             (defvar ,gn-ts-tab-width)
             (defvar ,gn-ts-use-tabs)
             (defvar ,gn-ts-activates-mm)
             (setq ,gn-ts-tab-width ,gn-tab-width)
             (setq ,gn-ts-use-tabs  ,gn-use-tabs)
             (setq ,gn-ts-activates-mm ,gn-activates-mm))
           ;; activate
           (pel-config-major-mode ,gn-ts-target-mode ,key-prefix :no-ts ,@body))
         ;; 2- setup for the standard mode
         (pel-config-major-mode ,target-mode ,key-prefix :no-ts ,@body)))))

;; --

(defun pel-local-set-f12 (prefix &optional key)
  "Assign the <f12> or <f12> KEY to PREFIX."
  (if key
      (local-set-key (kbd (format "<f12> %s" key))   prefix)
    (local-set-key (kbd "<f12>")   prefix)))

(defun pel-local-set-f12-M-f12 (prefix &optional key)
  "Assign the <f12>/<M-f12> or <f12>/<M-f12> KEY to PREFIX."
  ;; Bind the M-F12 first and F12 last so F12 shows up in menu.
  (if key
      (progn
        (local-set-key (kbd (format "<M-f12> %s" key)) prefix)
        (local-set-key (kbd (format "<f12> %s" key))   prefix))
    (local-set-key (kbd "<M-f12>") prefix)
    (local-set-key (kbd "<f12>")   prefix)))

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

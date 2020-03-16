;;; pel--options.el --- Customization options of the PEL package

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
;; Defines the PEL options.  All the options using the  'pel-use-' prefix
;; identify functionality (and potentially also a Emacs Lisp library) that can
;; be used by PEL.  It is used when the option is set to `t' and not used when
;; set to `nil'.  This way, the user can choose what is provided by PEL simply
;; by customizing PEL.

;;; Code:

(defgroup pel nil
  "Pragmatic Environment Library."
  :group 'convenience
  :link `(file-link :tag "Directory of PDF table files"
                    ,(expand-file-name "./docs/pdf/"))
  :package-version '(pel . "0.1"))


;; -----------------------------------------------------------------------------
(defgroup pel-identification nil
  "PEL author (the Emacs user) identification values."
  :group 'pel)

(defcustom pel-author-name nil
  "Name of the source code author name to use."
  :type 'string)

(defun pel-abbrev-valid-p (str)
  "Return t if STR is a valid abbreviation, nil otherwise"
  (= (length str) 4))

(defcustom pel-author-abbrev nil
  "4 character abbreviation of author name."
  :type 'string
  :safe #'pel-abbrev-valid-p)

(defcustom pel-author-email nil
  "Email address of the author to report in files."
  :type 'string)

;; -----------------------------------------------------------------------------
(defgroup pel-kbmacro nil
  "PEL keboard macro management utilities."
  :group 'pel)

(defcustom pel-kbmacro-prompts t
  "Prompt before overriding existing keyboard macro?"
  :group 'pel-kbmacro
  :type 'boolean
  :safe #'booleanp)

;; -----------------------------------------------------------------------------
(defgroup pel-text-insert nil
  "PEL Text insertion utilities."
  :group 'pel)

(defcustom pel-linelen 77
  "Length of line inserted by `pel-insert-line'."
  :group 'pel-text-insert
  :type 'integer
  :safe #'integerp)

;; -----------------------------------------------------------------------------
(defgroup pel-rst nil
  "PEL reStructuredText support."
  :group 'pel)

(defcustom pel-rst-adornment-style 'CRiSPer
  "Select the section adornment style.
Identifies the number of levels supported and their adornment.
- `default' is Emacs rst-mode default. A title and 7 levels.
- `Sphinx-Python' is what Sphinx uses: 6 levels:
  - parts,
  - chapters,
  - sections,
  - subsections,
  - subsubsections,
  - paragraphs.
- `CRiSPer', a 10-level mode I developed in the past for CRiSP."
  :group 'pel-rst
  :type '(choice (const :tag "default" default)
                 (const :tag "Sphinx-Python" Sphinx-Python)
                 (const :tag "CRiSPer" CRiSPer)))

;; -----------------------------------------------------------------------------
(defgroup pel-speedbar nil
  "PEL Speedbar management."
  :group 'pel)

(defcustom pel-prefer-sr-speedbar-in-terminal t
  "Prefer using Sr-Speedbar in terminal mode (when it's available) over Speedbar."
  :group 'pel-speedbar
  :type  'boolean
  :safe  #'booleanp)

;; -----------------------------------------------------------------------------
(defgroup pel-package-use nil
  "List of external packages that can be used by PEL."
  :group 'pel)

;; -----------------------------------------------------------------------------
(defgroup pel-pkg-for-grep nil
  "List of external packages that PEL can use to help search files and in files."
  :group 'pel-package-use)

(defcustom pel-use-ripgrep nil
  "Control whether PEL uses the ripgrep tool and its associated package.
Ripgrep is a very fast grep utility, and the rg package fully supports
the Emacs grep interface to use it.

References:
- ripgrep: URL `https://github.com/BurntSushi/ripgrep'
- ripgrep support package: rg: URL `https://melpa.org/#/rg'"
  :group 'pel-pkg-for-grep
  :type 'boolean
  :safe #'booleanp)

;; -----------------------------------------------------------------------------
;; Windows Management
;; ------------------
(defgroup pel-pkg-for-window nil
  "List of external packages that PEL can use to manage windows."
  :group 'pel-package-use)

;; Note: some other windows modules are used by PEL and are loaded
;;       regardless of the options since they are relatively small
;;       and inexpensive:
;;       - windmove
;;       - winner

(defcustom pel-use-ace-window  nil
  "Control whether PEL uses the ace-window packages."
  :group 'pel-pkg-for-window
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-use-speedbar nil
  "Control whether PEL uses the Speedbar and SR-Speedbar packages."
  :group 'pel-pkg-for-window
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-use-smooth-scrolling nil
  "Control whether PEL provides the smooth-scrolling capability."
  :group 'pel-pkg-for-window
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-use-framemove nil
  "Control whether PEL uses the framemove package.
It is similar to windmove and ties with it.

Notes:
- With PEL, if framemove is used windmove must also be used
  (see `pel-use-windmove').
- This package work with Emacs in graphics mode.  Multiple
  frames *can* be used in terminal (TTY) mode but only one can
  be displayed at a time in the terminal window.
- This file is not available MELPA (yet, as of Jan 2020).
- The version 0.10 is available via the relevant EmacsWiki page
  URL `https://www.emacswiki.org/emacs/framemove.el'
- Older version 0.9 used an obsolete function, that was fixed in version 0.10.

References:
- Author's site: Emacs Tip# 35: framemove:
  URL `http://trey-jackson.blogspot.com/2010/02/emacs-tip-35-framemove.html'.
- EmacsWiki framemove page: URL `https://www.emacswiki.org/emacs/FrameMove'.
- Youtube video on windmove and framemove: URL
  `https://www.youtube.com/watch?v=f3th2jyv35c'."
  :group 'pel-pkg-for-window
  :type 'boolean
  :safe #'booleanp)

;; -----------------------------------------------------------------------------
;; Buffer Management
;; -----------------
(defgroup pel-pkg-for-buffer nil
  "List of external packages that PEL can use to manage buffers."
  :group 'pel-package-use)

(defcustom pel-use-uniquify nil
  "Control whether PEL uses the uniquify package."
  :group 'pel-pkg-for-buffer
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-use-dired-narrow nil
  "Control whether PEL uses the dired-narrow package."
  :group 'pel-pkg-for-buffer
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-use-nhexl-mode nil
  "Control whether PEL uses the nhexl-mode and nhexl-nibble-edit-mode package."
  :group 'pel-pkg-for-buffer
  :type 'boolean
  :safe #'booleanp)

;; -----------------------------------------------------------------------------
;; Text Insertion / Templates
;; --------------------------
(defgroup pel-pkg-for-insertions nil
  "List of external packages that PEL can use to provide easy text insertion."
  :group 'pel-package-use)

(defcustom pel-use-lice nil
  "Control whether PEL uses the lice package to insert software license text."
  :group 'pel-pkg-for-insertions
  :type 'boolean
  :safe #'booleanp)

;; -----------------------------------------------------------------------------
;; Undo Mechanism Management
;; -------------------------
(defgroup pel-pkg-for-undo nil
  "List of external packages that PEL can use to control the undo mechanisms."
  :group 'pel-package-use)

(defcustom pel-use-popup-kill-ring nil
  "Control whether PEL uses the `popup-kill-ring' package.
With this package selective yanking can be done with the
the Meta-y key which pops-up a menu listing the kill ring entries.

Notes:
- Unfortunately it does not work reliably in terminal (TTY) mode, so PEL
  only activates it in graphics mode.
- The version of this package on MELPA is version 0.2.8 and obsolete.
- The author maintains its latest version (0.2.11) in the EmacsWiki.
- PEL uses the EmacsWiki version.

References:
- `popup-kill-ring' source @ EmacsWiki: URL
  `https://www.emacswiki.org/emacs/popup-kill-ring.el'
- Uncle Dave's YouTube video on it: URL
  `https://www.youtube.com/watch?v=LFXA089Tx38'"
  :group 'pel-pkg-for-undo
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-use-undo-tree nil
  "Control whether PEL uses the undo-tree package."
  :group 'pel-pkg-for-undo
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-use-goto-last-change nil
  "Control whether PEL uses the {goto-last-change} package."
  :group 'pel-pkg-for-undo
  :type 'boolean
  :safe #'booleanp)

;; -----------------------------------------------------------------------------
;; Parens block management
;; -----------------------
;;
;; Parens, a generic term to describe the following grouping characters:
;; - parenthesis:     '(' and ')',
;; - braces:          '{' and '}'
;; - square brackets: '[' and ']'
;; - angle brackets:  '<' and '>'
;; also potentially the quote characters:
;; - single quote:    '
;; - double quote:    "
;;
(defgroup pel-pkg-for-parens nil
  "List of external packages that PEL can use to help deal with parens."
  :group 'pel-package-use)

(defcustom pel-use-parinfer nil
  "Control whether PEL uses the parinfer package."
  :group 'pel-pkg-for-parens
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-use-rainbow-delimiters  nil
  "Control whether PEL uses the rainbow-delimiters package."
  :group 'pel-pkg-for-parens
  :type 'boolean
  :safe #'booleanp)

;; -----------------------------------------------------------------------------
;; Region / Selection Management
;; -----------------------------
(defgroup pel-pkg-for-region nil
  "List of external packages that PEL can use to help deal with regions."
  :group 'pel-package-use)

(defcustom pel-use-expand-region nil
  "Control whether PEL uses the expand-region package."
  :group 'pel-pkg-for-region
  :type 'boolean
  :safe #'booleanp)

;; -----------------------------------------------------------------------------
;; Keys & Prompts
;; --------------
(defgroup pel-pkg-for-keys nil
  "List of external packages that PEL can use to help deal keys and prompts."
  :group 'pel-package-use)

(defcustom pel-use-ido-mode nil
  "Control whether PEL uses the {ido-mode} package."
  :group 'pel-pkg-for-keys
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-use-which-key t
  "Control whether PEL uses the which-key package."
  :group 'pel-pkg-for-keys
  :type 'boolean
  :safe #'booleanp)

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
  :type 'boolean
  :safe #'booleanp)

;; -----------------------------------------------------------------------------
;; Text and Code Completion and Expansion
;; --------------------------------------
(defgroup pel-pkg-for-expand nil
  "List of external packages that PEL can use to complete code or expand text.

Note that auto-complete and company can both be activated.
However, PEL only allow one of them to be used per buffer.
The Hippie Expand can be used together with any."
  :group 'pel-package-use)

(defcustom pel-use-auto-complete nil
  "Control whether PEL supports the {auto-complete} package."
  :group 'pel-pkg-for-expand
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-use-company nil
  "Control whether PEL supports the company package."
  :group 'pel-pkg-for-expand
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-use-hippie-expand nil
  "Control whether PEL uses the {hippie-expand} package."
  :group 'pel-pkg-for-expand
  :type 'boolean
  :safe #'booleanp)

;; -----------------------------------------------------------------------------
;; Bookmark Support
;; ----------------
(defgroup pel-pkg-for-bookmark nil
  "List of external packages that PEL can use to manage bookmarks."
  :group 'pel-package-use)

(defcustom pel-use-bm nil
  "Control whether PEL uses the bm (Visible Bookmarks) package."
  :group 'pel-pkg-for-bookmark
  :type 'boolean
  :safe #'booleanp)

;; -----------------------------------------------------------------------------
;; Programming Language Support
;; ============================
(defgroup pel-pkg-for-programming nil
  "List of external packages that PEL can use to support programming."
  :group 'pel-package-use)

(defcustom pel-use-eldoc-box nil
  "Control whether PEL supports the eldoc-box package.
edox-box displays Eldoc information inside a child frame.
Note: eldoc-box only works in graphics mode, not in terminal (tty)
      mode.  In terminal-mode it is not activated even if this option
      is activated."
  :group 'pel-pkg-for-programming
  :type 'boolean
  :safe #'booleanp)

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; Language Server Protocol (LSP) Support
;; --------------------------------------
;; (defcustom pel-pkg-for-LSP nil
;;   "Language

(defcustom pel-use-eglot nil
  "Control whether PEL supports the eglot package.
eglot is a client for Language Server Protocol (LSP) servers."
  :group 'pel-pkg-for-programming
  :type 'boolean
  :safe #'booleanp)

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; C-like Language support
;; -----------------------
(defgroup pel-pkg-for-cc nil
  "C/C-like development packages PEL can use."
  :group 'pel-pkg-for-programming)

(defcustom pel-use-cc-vars nil
  "Control whether PEL supports the cc-vars package."
  :group 'pel-pkg-for-cc
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-use-c-eldoc nil
  "Control whether PEL supports the c-eldoc package."
  :group 'pel-pkg-for-cc
  :type 'boolean
  :safe #'booleanp)

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; Emacs Lisp Support
;; ------------------
(defgroup pel-pkg-for-elisp nil
  "Emacs Lisp development packages PEL can use."
  :group 'pel-pkg-for-programming)

(defcustom pel-use-macrostep nil
  "Control whether PEL uses the macrostep package."
  :group 'pel-pkg-for-elisp
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-use-esup nil
  "Control whether PEL uses the esup package."
  :group 'pel-pkg-for-elisp
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-use-re-builder nil
  "Control whether PEL uses the {re-builder} package."
  :group 'pel-pkg-for-elisp
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-use-highlight-defined nil
  "Control whether PEL uses the {highlight-defined} package."
  :group 'pel-pkg-for-elisp
  :type 'boolean
  :safe #'booleanp)

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; Common Lisp Support
;; -------------------
(defgroup pel-pkg-for-clisp nil
  "Common Lisp development packages PEL can use."
  :group 'pel-pkg-for-programming)

(defcustom pel-use-common-lisp nil
  "Control whether PEL supports Common Lisp development."
  :group 'pel-pkg-for-clisp
  :type 'boolean
  :safe #'booleanp)

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; Erlang Support
;; --------------
(defgroup pel-pkg-for-erlang nil
  "Erlang development packages PEL can use."
  :group 'pel-pkg-for-programming)

(defcustom pel-use-erlang nil
  "Control whether PEL supports Erlang development."
  :group 'pel-pkg-for-erlang
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-use-erlang-start nil
  "Control whether PEL uses the erlang-start package when `pel-use-erlang' is t."
  :group 'pel-pkg-for-erlang
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-use-erlang-flymake nil
  "Control whether PEL uses erlang-flymake when `pel-use-erlang' is t."
  :group 'pel-pkg-for-erlang
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-use-edts nil
  "Control whether PEL uses EDTS (Erlang Development Tool Suite) when `pel-use-erlang' is t."
  :group 'pel-pkg-for-erlang
  :type 'boolean
  :safe #'booleanp)

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; Python Support
;; --------------
(defgroup pel-pkg-for-python nil
  "Python development packages PEL can use."
  :group 'pel-pkg-for-programming)

(defcustom pel-use-python  nil
  "Control whether PEL supports Python development."
  :group 'pel-pkg-for-python
  :type 'boolean
  :safe #'booleanp)

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; Rust Support
;; ------------
(defgroup pel-pkg-for-rust nil
  "Rust development packages PEL can use."
  :group 'pel-pkg-for-programming)

(defcustom pel-use-rust  nil
  "Control whether PEL supports Rust development.
When set to non-nil, 3 packages are used:
- {rust-mode}
- {racer}
- {cargo}."
  :group 'pel-pkg-for-rust
  :type 'boolean
  :safe #'booleanp)

;; -----------------------------------------------------------------------------
;; Markup Language Support
;; -----------------------
(defgroup pel-pkg-for-markup nil
  "Markup Language editing packages PEL can use."
  :group 'pel-package-use)

(defcustom pel-use-org-mode nil
  "Control whether PEL supports Org-Mode."
  :group 'pel-pkg-for-markup
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-use-rst-mode nil
  "Control whether PEL supports {rst-mode} (reStructuredText).
Once you activate this, see the pel-rst group for more options."
  :group 'pel-pkg-for-markup
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-use-graphviz-dot nil
  "Control whether PEL uses the Graphviz Dot tool and its associated package.
It supports the Graphviz Dot file format and the ability to create graphics
images from their Graphviz Dot files.

References:
- Graphviz: URL `https://www.graphviz.org'
- DOT Language URL `https://www.graphviz.org/doc/info/lang.html'
- `graphviz-dot-mode' MELPA URL `https://melpa.org/#/graphviz-dot-mode'"
  :group 'pel-pkg-for-markup
  :type 'boolean
  :safe #'booleanp)

;; -----------------------------------------------------------------------------
(provide 'pel--options)

;;; pel--options.el ends here

;;; pel--options.el --- PEL Customization Options -*-lexical-binding: t-*-

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
;;   - pel-identification
;;   - pel-kbmacro
;;   - pel-text-insert
;;   - pel-package-use
;;     - pel-pkg-for-filemng
;;       - pel-pkg-for-ztree
;;     - pel-pkg-for-grep
;;     - pel-pkg-for-search
;;     - pel-pkg-for-window
;;       - pel-pkg-for-speedbar
;;       - pel-pkg-for-session
;;     - pel-pkg-for-buffer
;;     - pel-pkg-for-completion
;;     - pel-pkg-for-insertions
;;     - pel-pkg-for-undo
;;     - pel-pkg-for-parens
;;     - pel-pkg-for-region
;;     - pel-pkg-for-keys
;;     - pel-pkg-for-expand
;;     - pel-pkg-for-bookmarks
;;     - pel-pkg-for-shells
;;     - pel-pkg-for-vcs
;;     - pel-pkg-for-programming
;;       - pel-pkg-for-applescript
;;       - pel-pkg-for-cc
;;         - pel-pkg-for-c
;;         - pel-pkg-for-c++
;;         - pel-pkg-for-d
;;       - pel-pkg-for-elisp
;;       - pel-pkg-for-clisp
;;       - pel-pkg-for-beam-vm
;;         - pel-pkg-for-erlang
;;         - pel-pkg-for-lfe
;;         - pel-pkg-for-elixir
;;       - pel-pkg-for-julia
;;       - pel-pkg-for-python
;;       - pel-pkg-for-rust
;;     - pel-pkg-for-markup
;;       - pel-pkg-for-reST
;;       - pel-pkg-for-graphviz-dot

;;; Code:


;; -----------------------------------------------------------------------------
;; Validation Utilities
;; --------------------

(defun pel-indent-valid-p (n)
  "Return t if N is a valid indentation integer, nil otherwise."
  (and (integerp n) (< n 9) (> n 1)))

(defun pel-c-style-valid-p (style)
  "Return non-nil if STYLE is one of the valid CC Mode styles, nil otherwise."
  (require 'cc-vars nil :noerror)
  (if (boundp 'c-style-alist)
      (member style (mapcar 'car c-style-alist))
    (error "The file cc-vars should have been loaded and it's not!")))

;; -----------------------------------------------------------------------------
;; User Option Data Definition
;; ---------------------------

(defgroup pel nil
  "Pragmatic Environment Library."
  :group 'convenience
  :link `(file-link :tag "Directory of PDF table files"
                    ,(expand-file-name "./docs/pdf/"))
  :package-version '(pel . "0.1.1"))


;; -----------------------------------------------------------------------------
(defgroup pel-identification nil
  "PEL author (the Emacs user) identification values."
  :group 'pel)

(defcustom pel-author-name nil
  "Name of the source code author name to use."
  :group 'pel-identification
  :type 'string)

(defun pel-abbrev-valid-p (str)
  "Return t if STR is a valid abbreviation, nil otherwise."
  (= (length str) 4))

(defcustom pel-author-abbrev nil
  "4 character abbreviation of author name."
  :group 'pel-identification
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
(defgroup pel-package-use nil
  "List of external packages that can be used by PEL."
  :group 'pel)

;; -----------------------------------------------------------------------------
(defgroup pel-pkg-for-filemng nil
  "List of external packages that can be used to manage file/directory."
  :group 'pel-package-use)

(defcustom pel-use-neotree nil
  "Control whether PEL uses the Emacs NeoTree search package.
See: https://github.com/jaypei/emacs-neotree"
  :group 'pel-pkg-for-filemng
  :type 'boolean
  :safe #'booleanp)

(defgroup pel-pkg-for-ztree nil
  "PEL extra configuration for ztree packages."
    :group 'pel-pkg-for-filemng)

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

;; -----------------------------------------------------------------------------
(defgroup pel-pkg-for-grep nil
  "List of external packages that PEL can use for grep operations."
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
(defgroup pel-pkg-for-search nil
  "List of external packages that PEL can use for searching text."
  :group 'pel-package-use)

(defcustom pel-use-swiper nil
  "Control whether PEL uses the Swiper search package.
See: https://github.com/abo-abo/swiper#swiper"
  :group 'pel-pkg-for-search
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-search-with-swiper nil
  "Search with swiper if set to t, otherwise use function `isearch-forward'."
  :group 'pel-pkg-for-search
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
  "Control whether PEL uses the `ace-window' package."
  :group 'pel-pkg-for-window
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-use-winner nil
  "Control whether PEL uses the `winner' package."
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


;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
(defgroup pel-pkg-for-speedbar nil
  "PEL Speedbar management."
  :group 'pel-pkg-for-window)

(defcustom pel-use-speedbar nil
  "Control whether PEL uses the Speedbar and SR-Speedbar packages."
  :group 'pel-pkg-for-speedbar
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-prefer-sr-speedbar-in-terminal t
  "Prefer using Sr-Speedbar in terminal mode (when available) over Speedbar."
  :group 'pel-pkg-for-speedbar
  :type  'boolean
  :safe  #'booleanp)

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
(defgroup pel-pkg-for-session nil
  "PEL window session management."
  :group 'pel-pkg-for-window)

(defcustom pel-use-desktop nil
  "Control whether desktop feature is used for session management.

When session management controlled by desktop feature, then
identify whether the built-in desktop.el is used alone or whether
one of the desktop-registry or desktop+ is also used.

The value can be:

- nil : nothing is used.

- t:                             use built-in desktop but do NOT
                                 activate the desktop save mode.

- `with-desktop-automatic':      use built-in desktop and
                                 activate desktop save mode.

- `with-desktop-registry':       use desktop and the desktop-registry
                                 external package.

- `with-desktop-registry-automatic': use desktop, the desktop-registry
                                     and activate desktop auto-save mode.

- `with-desktop+':               use desktop and the desktop+ external package."
  :group 'pel-pkg-for-session
  :type '(choice
          (const :tag "Not used" nil)
          (const :tag "Use built-in desktop - do NOT activate desktop-save-mode" t)
          (const :tag "Use built-in desktop and ACTIVATE desktop-save-mode"
                 with-desktop-automatic)
          (const :tag "Use desktop with desktop-registry \
- do NOT activate desktop-save-mode " with-desktop-registry)
          (const :tag "Use desktop with desktop-registry \
and ACTIVATE desktop-save-mode" with-desktop-registry-automatic)
          (const :tag "Use desktop with desktop+" with-desktop+)))

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
  "Control whether PEL uses the `dired-narrow' package."
  :group 'pel-pkg-for-buffer
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-use-nhexl-mode nil
  "Control whether PEL uses the package and function `nhexl-mode'."
  :group 'pel-pkg-for-buffer
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-use-vline nil
  "Control whether PEL uses the `vline' package."
  :group 'pel-pkg-for-buffer
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-use-ascii-table nil
  "Control whether the `ascii-table' package is available."
  :group 'pel-pkg-for-buffer
  :type 'boolean
  :safe #'booleanp)

;; -----------------------------------------------------------------------------
;; Completion Support
;; ------------------
(defgroup pel-pkg-for-completion nil
  "List of external packages that PEL can use to manage completion."
  :group 'pel-package-use)

(defcustom pel-use-ido nil
  "Control whether PEL uses the Ido package."
  :group 'pel-pkg-for-completion
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-use-ivy nil
  "Control whether PEL uses the Ivy package."
  :group 'pel-pkg-for-completion
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-use-counsel nil
  "Control whether Counsel is used when Ivy is used."
  :group 'pel-pkg-for-completion
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-use-counsel-osx-app nil
  "Control whether `counsel-osx-app' is used when counsel is used on macOS."
  :group 'pel-pkg-for-completion
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-use-helm nil
  "Control whether PEL uses the Helm package."
  :group 'pel-pkg-for-completion
  :type 'boolean
  :safe #'booleanp)

(defconst pel-USE-IDO     1 "Bitmask identifying Ido.     DON'T CHANGE!")
(defconst pel-USE-IVY     2 "Bitmask identifying Ivy.     DON'T CHANGE!")
(defconst pel-USE-COUNSEL 4 "Bitmask identifying Counsel. DON'T CHANGE!")
(defconst pel-USE-HELM    8 "Bitmask identifying Helm.    DON'T CHANGE!")

(defcustom pel-initial-completion-mode nil
  "Select the main text completion mode used when Emacs starts.

PEL supports several completion engines.
This option selects which engine used when Emacs starts.
The available options are:
- nil   : use Emacs default
- `helm': use Helm (when pel-use-helm is t).
- `ido' : use Ido (when pel-use-ido is t)
- `ivy' : Use Ivy (when pel-use-ivy is t)
- `ivy/counsel' : Use Ivy with Counsel (when pel-use-ivy and pel-use-counsel are
                  both t)."
  :group 'pel-pkg-for-completion
  :type '(choice
          (const :tag "Emacs default" nil)
          (const :tag "Use Helm (requitres pel-use-helm to be t)" helm)
          (const :tag "Use Ido (requires pel-use-ido to be t)" ido)
          (const :tag "Use Ivy (requires pel-use-ivy to be t)" ivy)
          (const :tag "use Ivy & Counsel (requires both pel-use-ivy and \
pel-use-counsel to be t" ivy/counsel)))

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

(defcustom pel-use-hydra nil
  "Control whether PEL uses the hydra package."
  :group 'pel-pkg-for-keys
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-use-which-key nil
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
;; Shell & Terminal Support
;; ------------------------
(defgroup pel-pkg-for-shells nil
  "List of external packages that PEL can use to support shells and terminals."
  :group 'pel-package-use)

(defcustom pel-use-vterm nil
  "Control whether the vterm shell is available.
The vterm package used the libvterm library to provide a very fast
and usable shell for Emacs."
  :group 'pel-pkg-for-shells
  :type 'boolean
  :safe #'booleanp)

;; -----------------------------------------------------------------------------
;; Version Control System Support
;; ------------------------------
(defgroup pel-pkg-for-vcs nil
  "List of external packages that PEL can use to support use of (D)VCS."
  :group 'pel-package-use)

(defcustom pel-use-magit nil
  "Control whether PEL provides access to the Magit package."
  :group 'pel-pgk-for-vcs
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-use-monky nil
  "Control whether PEL provides access to the Monky package."
  :group 'pel-pgk-for-vcs
  :type 'boolean
  :safe #'booleanp)

;; -----------------------------------------------------------------------------
;; Programming Language Support
;; ============================
(defgroup pel-pkg-for-programming nil
  "PEL customization for programming languages."
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

(defcustom pel-use-hide-comnt nil
  "Control whether PEL activates Drew Adams' hide-cmnt package.
This package provides the ability to hide comments.
IMPORTANT:
 If you activate this you must install the package manually!
 Download it either from the EmacsWiki or EmacsMirror page
 and store it in a directory that is in your Emacs load path."
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
;; AppleScript support
;; -------------------
(defgroup pel-pkg-for-applescript nil
  "PEL customization for AppleScript."
  :group 'pel-pkg-for-programming)

(defcustom pel-use-applescript nil
  "Control whether PEL support the AppleScript mode."
  :group 'pel-pkg-for-applescript
  :type 'boolean
  :safe #'booleanp)

(when (eq system-type 'darwin)
  (defcustom  pel-mac-voice-name nil
    "Name of the macOS voice used for narration.
- If the string is empty: use the System's selected voice.
- A name with 2 characters or less identifies the System's selected voice.
- To specify another name the string must have 3 or more characters."
    :group 'pel-pkg-for-applescript
    :type 'string))


;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; CC Mode Language support
;; ------------------------
(defgroup pel-pkg-for-cc nil
  "PEL customization for curly-bracket programming languages."
  :group 'pel-pkg-for-programming)

(defcustom pel-use-c-eldoc nil
  "Control whether PEL supports the c-eldoc package."
  :group 'pel-pkg-for-cc
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-cc-auto-newline t
  "Set the default state of CC Mode electric auto-newline for all CC Modes.
This includes modes for C, C++, D.
If set to nil: disables auto-newline
If set to t:   activates auto-newline
PEL calls `c-toggle-auto-newline' to set to requested default state
for buffers in `d-mode'.  The command can be used to change the state
of auto-newline while editing."
  :group 'pel-pkg-for-cc
  :type 'boolean
  :safe #'booleanp)

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; C Language Support
;; ------------------

(defgroup pel-pkg-for-c nil
  "PEL customization for C."
  :group 'pel-pkg-for-cc)

(defcustom pel-c-indentation 3
  "Number of columns for C source code indentation.
PEL stores this in `c-basic-offset' when editing buffers with C code.
Values in the [2, 8] range are accepted."
  :group 'pel-pkg-for-c
  :type 'integer
  :safe 'pel-indent-valid-p)

(defcustom pel-c-tab-width 3
  "Distance between tab stop for C source code.
PEL stores this in `tab-width' when editing buffer with C source.
This does *NOT* control the indentation in C source code, it is used
only for commands that mode point to tab stop positions
such as `tab-to-tab-stop', and the display of hard TAB characters.
It is often the same value as `pel-c-indentation'.
Values in the [2, 8] range are accepted."
  :group 'pel-pkg-for-c
  :type 'integer
  :safe 'pel-indent-valid-p)

(defcustom pel-c-use-tabs nil
  "Value of `indent-tabs-mode' for editing C source code.
- If set to nil: only spaces are used for indentation.
- If set to t: hard tabs are used when possible."
  :group 'pel-pkg-for-c
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-c-bracket-style "linux"
  "Set the bracket style for the C programming language.
PEL stores this value associated with the `c-mode' into the
`c-default-style' user option variable.
If you want to use something else, please select one of the
CC Mode Built-in Styles."
  :group 'pel-pkg-for-c
  :type 'string
  :safe 'pel-c-style-valid-p)

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; C++ Language Support
;; --------------------

(defgroup pel-pkg-for-c++ nil
  "PEL customization for C++."
  :group 'pel-pkg-for-cc)

(defcustom pel-c++-indentation 3
  "Number of columns for C++ source code indentation.
PEL stores this in `c-basic-offset' when editing buffers with C++ source.
Values in the [2, 8] range are accepted."
  :group 'pel-pkg-for-c++
  :type 'integer
  :safe 'pel-indent-valid-p)

(defcustom pel-c++-tab-width 3
  "Distance between tab stop for C++ source code.
PEL stores this in `tab-width' when editing buffer with C++ source.
This does *NOT* control the indentation in C++ source code, it is used
only for commands that mode point to tab stop positions
such as `tab-to-tab-stop', and the display of hard TAB characters.
It is often the same value as `pel-c++-indentation'.
Values in the [2, 8] range are accepted."
  :group 'pel-pkg-for-c++
  :type 'integer
  :safe 'pel-indent-valid-p)

(defcustom pel-c++-use-tabs nil
  "Value of `indent-tabs-mode' for editing C++ source code.
- If set to nil: only spaces are used for indentation.
- If set to t: hard tabs are used when possible."
  :group 'pel-pkg-for-c++
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-c++-bracket-style "stroustrup"
  "Set the bracket style for the C++ programming language.
PEL stores this value associated with the `c-mode' into the
`c-default-style' user option variable.
If you want to use something else, please select one of the
CC Mode Built-in Styles."
  :group 'pel-pkg-for-c++
  :type 'string
  :safe 'pel-c-style-valid-p)

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; D Language Support
;; ------------------
;; Note: support is for D 2.x (as opposed to the older, different and now
;; obsolete first implementation of D called D 1).

(defgroup pel-pkg-for-d nil
  "PEL customization for D."
  :group 'pel-pkg-for-cc)

(defcustom pel-use-d nil
  "Control whether PEL supports the D programming language.
- Activates the use of: Emacs D Mode
- Required to activate the use of:
 - Auto-Complete for D
 - Company mode for D"
  :group 'pel-pkg-for-d
  :type 'boolean
  :safe #'booleanp)

;-- D Style

(defcustom pel-d-indentation 4
  "Number of columns for D source code indentation.
PEL stores this in `c-basic-offset' when editing buffers in `d-mode'.
The D community recommends using 4 spaces for indentation
therefore that's PEL's default.
Values in the [2, 8] range are accepted."
  :group 'pel-pkg-for-d
  :type 'integer
  :safe 'pel-indent-valid-p)

(defcustom pel-d-tab-width 4
  "Distance between tab stop for D source code.
PEL stores this in `tab-width' when editing buffer in `d-mode'.
This does *NOT* control the indentation in D source code, it is used
only for commands that mode point to tab stop positions and the
display of hard TAB characters.
It is often the same value as `pel-d-indentation'."
  :group 'pel-pkg-for-d
  :type 'integer
  :safe 'pel-indent-valid-p)

(defcustom pel-d-use-tabs nil
  "Value of `indent-tabs-mode' for editing D source code.
- If set to nil: only spaces are used for indentation.
- If set to t: hard tabs are used when possible."
  :group 'pel-pkg-for-d
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-d-bracket-style "bsd"
  "Set the bracket style for the D programming language.
PEL stores this value associated with the `d-mode' into the
`c-default-style' user option variable.
The BSD style (also called Allman style) is recommended by the
D community, see URL https://dlang.org/dstyle.html#phobos_brackets .
If you want to use something else, please select one of the
CC Mode Built-in Styles."
  :group 'pel-pkg-for-d
  :type 'string
  :safe 'pel-c-style-valid-p)

;;-- Tools for D

(defcustom pel-use-d-ac-dcd nil
  "Control whether AutoComplete/DCD based code completion is used for D.

When set to t:
- the ac-dcd package is used for code completion,
  - it uses flycheck-dmd-dub package, which uses the D package
    registry called DUB to retreive all D dependencies information.
    - which uses DCD (the D Completion Daemon) written in D
      which must be installed separately.

An alternative to AutoComplete/DCD is the Company/DCD, controlled
by the `pel-use-d-company-dcd'."
  :group 'pel-pkg-for-d
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-use-d-company-dcd nil
  "Control whether Company/DCD based code completion is used for D.

When set to t:
- the `company-dcd' package is used for code completion,
  - it uses flycheck-dmd-dub package, which uses the D package
    registry called DUB to retreive all D dependencies information.
    - which uses DCD (the D Completion Daemon) written in D
      which must be installed separately.

An alternative to AutoComplete/DCD is the Company/DCD, controlled
by the `pel-use-d-ac-dcd'."
  :group 'pel-pkg-for-d
  :type 'boolean
  :safe #'booleanp)

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; Emacs Lisp Support
;; ------------------
(defgroup pel-pkg-for-elisp nil
  "PEL customizaton for Emacs Lisp."
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
  "PEL customization for Common Lisp."
  :group 'pel-pkg-for-programming)

(defcustom pel-use-common-lisp nil
  "Control whether PEL supports Common Lisp development."
  :group 'pel-pkg-for-clisp
  :type 'boolean
  :safe #'booleanp)

;; -----------------------------------------------------------------------------
;; BEAM Programming Languages
;; --------------------------
(defgroup pel-pkg-for-beam-vm nil
  "PEL customization for BEAM Virtual Machine programming languages."
  :group 'pel-pkg-for-programming)

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; Erlang Support
;; --------------
;; Note: Erlang, is a BEAM VM programming language.

(defgroup pel-pkg-for-erlang nil
  "PEL customization for Erlang."
  :group 'pel-pkg-for-beam-vm)

(defcustom pel-use-erlang nil
  "Control whether PEL supports Erlang development."
  :group 'pel-pkg-for-erlang
  :type 'boolean
  :safe #'booleanp)

;; TODO: find a way to indicate either a path or the name of an environment variable
(defcustom pel-erlang-rootdir "/usr/local/otp"
  "Root directory of Erlang OTP."
  :group 'pel-pkg-for-erlang
  :type 'string)

;; TODO: find a way to indicate either a path or the name of an environment variable
(defcustom pel-erlang-exec-path "/usr/local/otp/bin"
  "Directory where Erlang binaries are located."
  :group 'pel-pkg-for-erlang
  :type 'string)

(defcustom pel-use-erlang-start nil
  "Control whether PEL uses erlang-start package when `pel-use-erlang' is t."
  :group 'pel-pkg-for-erlang
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-use-erlang-flymake nil
  "Control whether PEL uses erlang-flymake when `pel-use-erlang' is t."
  :group 'pel-pkg-for-erlang
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-use-erlang-flycheck nil
  "Control whether PEL uses flycheck for Erlang when `pel-use-erlang' is t.
If `pel-use-erlang-flymake' is t, `pel-use-erlang-flycheck' is ignored:
ie. priority is given to flymake because it is part of Emacs."
  :group 'pel-pkg-for-erlang
  :type 'boolean
  :safe #'booleanp)


(defcustom pel-use-edts nil
  "Control whether PEL uses EDTS when `pel-use-erlang' is t.
EDTS := Erlang Development Tool Suite."
  :group 'pel-pkg-for-erlang
  :type 'boolean
  :safe #'booleanp)

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; LFE - Lisp Flavoured Erlang - Support
;; -------------------------------------
;; Note: LFE is a BEAM VM programming language.

(defgroup pel-pkg-for-lfe nil
  "PEL customization for LFE (Lisp Flavoured Erlang)."
  :group 'pel-pkg-for-beam-vm)

(defcustom pel-use-lfe nil
  "Control whether PEL supports Elixir development."
  :group 'pel-pkg-for-lfe
  :type 'boolean
  :safe #'booleanp)

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; Elixir Support
;; --------------
;; Note: Elixir is a BEAM VM programming language.

(defgroup pel-pkg-for-elixir nil
  "PEL customization for Elixir."
  :group 'pel-pkg-for-beam-vm)

(defcustom pel-use-elixir nil
  "Control whether PEL supports Elixir development."
  :group 'pel-pkg-for-elixir
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-use-alchemist nil
  "Control whether PEL supports Elixir Alchemist package.
IMPORTANT:
  To use this feature you must also activate `pel-use-elixir'."
  :group 'pel-pkg-for-elixir
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-use-elixir-exunit nil
  "Control whether PEL supports Elixir Unit Test development.
IMPORTANT:
  To use this feature you must also activate `pel-use-elixir'."
  :group 'pel-pkg-for-elixir
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-use-elixir-lsp nil
  "Control whether PEL supports Lsp-Elixir package: Language Server Protocol.
This activates the use of the lsp-elixir package, and the lsp-mode
package which provides the client/library for LSP."
  :group 'pel-pkg-for-elixir
  :type 'boolean
  :safe #'booleanp)

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; Julia Support
;; --------------
(defgroup pel-pkg-for-julia nil
  "PEL customization for Julia."
  :group 'pel-pkg-for-programming)

(defcustom pel-use-julia  nil
  "Control whether PEL supports Julia development.
IMPORTANT:
  You *must* also activate `pel-use-vterm' to be able to use Julia
  development as this uses the `julia-snail' package which includes both the
  `julia-mode' but also a fast Julia REPL that uses the vterm."
  :group 'pel-pkg-for-julia
  :type 'boolean
  :safe #'booleanp)

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; Python Support
;; --------------
(defgroup pel-pkg-for-python nil
  "PEL customization for Python."
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
  "PEL customization for Rust."
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


;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; reStructuredText support
;; ------------------------
(defgroup pel-pkg-for-reST nil
  "PEL reStructuredText support."
  :group 'pel-pkg-for-markup)

(defcustom pel-use-rst-mode nil
  "Control whether PEL supports {rst-mode} (reStructuredText)."
  :group 'pel-pkg-for-reST
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-rst-adornment-style 'CRiSPer
  "Select the section adornment style.
Identifies the number of levels supported and their adornment.
- `default' is Emacs `rst-mode' default.  A title and 7 levels.
- `Sphinx-Python' is what Sphinx uses: 6 levels:
  - parts,
  - chapters,
  - sections,
  - subsections,
  - subsubsections,
  - paragraphs.
- `CRiSPer', a title and 12-level mode previously developed for CRiSP."
  :group 'pel-pkg-for-reST
  :type '(choice (const :tag "default" default)
                 (const :tag "Sphinx-Python" Sphinx-Python)
                 (const :tag "CRiSPer" CRiSPer)))

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; GraphViz-Dot Support
;; --------------------
(defgroup pel-pkg-for-graphviz-dot nil
  "PEL Graphviz-DOT support."
  :group 'pel-pkg-for-markup)

(defcustom pel-use-graphviz-dot nil
  "Control whether PEL uses the Graphviz Dot tool and its associated package.
It supports the Graphviz Dot file format and the ability to create graphics
images from their Graphviz Dot files.

References:
- Graphviz: URL `https://www.graphviz.org'
- DOT Language URL `https://www.graphviz.org/doc/info/lang.html'
- `graphviz-dot-mode' MELPA URL `https://melpa.org/#/graphviz-dot-mode'"
  :group 'pel-pkg-for-graphviz-dot
  :type 'boolean
  :safe #'booleanp)

;; -----------------------------------------------------------------------------
(provide 'pel--options)

;;; pel--options.el ends here

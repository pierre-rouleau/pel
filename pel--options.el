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
;;   - pel-base-emacs
;;   - pel-package-use
;;     - pel-pkg-for-align
;;     - pel-pkg-for-bookmark
;;     - pel-pkg-for-buffer
;;     - pel-pkg-for-completion
;;     - pel-pkg-for-cursor
;;     - pel-pkg-for-cut-and-paste
;;     - pel-pkg-for-dired
;;     - pel-pkg-for-expand
;;     - pel-pkg-for-filemng
;;       - pel-pkg-for-browse
;;         - pel-pkg-for-ztree
;;     - pel-pkg-for-frame
;;     - pel-pkg-for-graphics-emacs
;;       - pel-pkg-for-graphics-cursor
;;     - pel-pkg-for-grep
;;     - pel-pkg-for-highlight
;;       - pel-pkg-for-parens
;;     - pel-pkg-for-insertions
;;     - pel-pkg-for-kbmacro
;;     - pel-pkg-for-key-chord
;;     - pel-pkg-for-keys
;;     - pel-pkg-for-markup
;;       - pel-pkg-for-asciidoc
;;       - pel-pkg-for-drawing-markup
;;         - pel-pkg-for-graphviz-dot
;;         - pel-pkg-for-plantuml
;;       - pel-pkg-for-reST
;;     - pel-pkg-for-navigation
;;       - pel-pkg-for-xref
;;     - pel-pkg-for-programming
;;       - pel-pkg-for-all-languages
;;       - pel-pkg-for-applescript
;;       - pel-pkg-for-cc
;;         - pel-pkg-for-c
;;         - pel-pkg-for-c++
;;         - pel-pkg-for-d
;;       - pel-pkg-for-javascript
;;       - pel-pkg-for-lisp
;;         - pel-pkg-for-clisp
;;         - pel-pkg-for-elisp
;;       - pel-pkg-for-beam-vm
;;         - pel-pkg-for-elixir
;;         - pel-pkg-for-erlang
;;         - pel-pkg-for-lfe
;;       - pel-pkg-for-julia
;;       - pel-pkg-for-python
;;       - pel-pkg-for-rexx
;;       - pel-pkg-for-rust
;;       - pel-pkg-for-v
;;     - pel-pkg-for-project-mng
;;     - pel-pkg-for-regexp
;;     - pel-pkg-for-region
;;     - pel-pkg-for-scrolling
;;     - pel-pkg-for-search
;;     - pel-pkg-for-session
;;     - pel-pkg-for-speedbar
;;     - pel-pkg-for-shells
;;     - pel-pkg-for-spelling
;;     - pel-pkg-for-text-mode
;;     - pel-pkg-for-undo
;;     - pel-pkg-for-vcs
;;     - pel-pkg-for-window
;;       - pel-pkg-for-scrolling
;;       - pel-pkg-for-session
;;       - pel-pkg-for-speedbar
;;

;; Naming conventions:
;;
;; - pel-pkg-for-<package-name or topic>
;; - pel-use-<package name>
;; - pel-modes-activating-<package name>
;; - pel-startup-<thing to activate at startup>

;; -----------------------------------------------------------------------------
;;; Code:


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
    (error "Failed loading cc-vars!")))

;; -----------------------------------------------------------------------------
;; File Path build utility
;; -----------------------

(defun pel-pdf-directory ()
  "Return the full path of the directory where PDF files are stored.
Last character is a forward slash."
  (expand-file-name
   (format "%s/doc/pdf/"
           (file-name-directory (locate-library "pel")))))

(defun pel-pdf-file-url (topic &optional web-url)
  "Return the full path of a pdf table for TOPIC.
TOPIC is the file name body (no path, no extension).
By default return the local file url.
If WEB-URL is non-nil return the web URL for the file hosted in GitHub."
  (if web-url
      (format "https://raw.githubusercontent.com/pierre-rouleau/pel/\
master/doc/pdf/%s.pdf" topic)
    (format "file:%s"
            (expand-file-name
             (format "%s.pdf" topic)
             (pel-pdf-directory)))))

;; -----------------------------------------------------------------------------
;; User Option Data Definition
;; ---------------------------

(defgroup pel nil
  "Pragmatic Environment Library.
A collection of facilities designed to integrate and complement a large
set of Emacs libraries while providing key bindings that mainly use function
keys as key prefixes, leaving the standard Emacs keys untouched.
PEL comes with a manual and a large set of PDF files, each documenting the
commands and key bindings of a specific aspect of Emacs.  The PDF files document
the standard Emacs key bindings as well as PEL's specific key bindings."
  :group 'convenience
  :link `(file-link :tag "Directory of PDF table files" ,(pel-pdf-directory))
  :link `(url-link  :tag "PEL key maps PDF" ,(pel-pdf-file-url "-pel-key-maps"))
  :link `(file-link :tag "PEL @ GitHub" "https://github.com/pierre-rouleau/pel")
  :package-version '(pel . "0.3.1"))

;; -----------------------------------------------------------------------------
(defgroup pel-base-emacs nil
  "PEL Emacs basic configuration."
  :group 'pel)

(defcustom pel-prompt-accept-y-n nil
  "Accept 'y' or 'n' instead of 'yes' or 'no' as answers to prompts."
  :group 'pel-base-emacs
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

;; -----------------------------------------------------------------------------
(defgroup pel-package-use nil
  "List of external packages that can be used by PEL."
  :group 'pel)

;; -----------------------------------------------------------------------------
;; Alignment Support
;; -----------------
(defgroup pel-pkg-for-align nil
  "Customization of PEL alignment support."
  :group 'pel-package-use
  :link `(url-link :tag "Align PDF" ,(pel-pdf-file-url "align")))

(defcustom pel-modes-activating-align-on-M-RET nil
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

;; -----------------------------------------------------------------------------
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

;; -----------------------------------------------------------------------------
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
- <f11> t O  : nhexl-overwrite-only-mode
- <f11> b x  : nhexl-mode
- <f11> b X  : nhexl-nibble-edit-mode"
  :group 'pel-pkg-for-buffer
  :type 'boolean
  :safe #'booleanp
  :link '(url-link :tag "nhexl @ Elpa"
                   "https://elpa.gnu.org/packages/nhexl-mode.html"))

;; -----------------------------------------------------------------------------
;; Completion Support
;; ------------------
(defgroup pel-pkg-for-completion nil
  "List of external packages that PEL can use to manage completion.

PEL allows selecting completion mechanism dynamically.  You can use Ido, Ivy,
Ivy-With-Counsel, Helm, or none of them at any time.  To use any of them you
must first activate these engines in this configuration buffer."
  :group 'pel-package-use
  :link `(url-link :tag "Input Completion PDF" ,(pel-pdf-file-url "completion-input")))

(defcustom pel-use-ido nil
  "Control whether PEL uses the Ido package.

The IDO package is distributed with Emacs.  It provides very efficient
completion mechanism that is preferred by many people."
  :link '(custom-manual "(ido)Overview")
  :link '(url-link :tag "Introduction to Ido Mode @ Mastering Emacs"
                   "https://www.masteringemacs.org/article/introduction-to-ido-mode")
  :link '(custom-group-link "ido")
  :group 'pel-pkg-for-completion
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-use-ivy nil
  "Control whether PEL uses the Ivy package.

Ivy is another popular interactive completion mechanism for Emacs using menu
lists and designed for speed of selection."
  :link '(url-link :tag "Ivy @ GitHub"
                   "")
  :link '(url-link :tag "Ivy User Manual"
                   "https://oremacs.com/swiper/")
  :link '(custom-group-link "ivy")
  :group 'pel-pkg-for-completion
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-use-counsel nil
  "Control whether Counsel is used when Ivy is used.
You must also activate the user option variable  `pel-use-ivy' to use counsel."
  :link '(url-link :tag "counsel/swiper @ GitHub"
                   "https://github.com/abo-abo/swiper")
  :link '(url-link :tag "Description of counsel-linux-app"
                   "https://oremacs.com/2016/03/16/counsel-linux-app/")
  :link '(url-link :tag "Ivy, Counsel and Swiper Tutorial"
                   "https://writequit.org/denver-emacs/presentations/2017-04-11-ivy.html")
  :link '(custom-group-link "counsel")
  :group 'pel-pkg-for-completion
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-use-counsel-osx-app nil
  "Control whether `counsel-osx-app' is used when counsel is used on macOS.

With this package activated, PEL provides the ``<f11> A`` key sequence to
counsel-osx-app.  This allows selection of an macOS application using ivy
completion.

On Linux, the `counsel-linux-app` is bound to the same key if user option
variable `pel-use-counsel' is set to t."
  :link '(url-link :tag "counsel-osx-app @ GitHub"
                   "https://github.com/d12frosted/counsel-osx-app")
  :group 'pel-pkg-for-completion
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-use-helm nil
  "Control whether PEL uses the Helm package.

Helm is a very powerful interactive incremental completion and
selection package which provides a large number of commands you
can execute on the completion list."
  :link '(url-link :tag "Helm home page"
                   "https://emacs-helm.github.io/helm/")
  :link '(url-link :tag "A package in a league of its own: Helm"
                   "https://tuhdo.github.io/helm-intro.html")
  :group 'pel-pkg-for-completion
  :type 'boolean
  :safe #'booleanp)

(defconst pel-USE-IDO     1 "Bitmask identifying Ido.      DON'T CHANGE!")
(defconst pel-USE-IVY     2 "Bitmask identifying Ivy.      DON'T CHANGE!")
(defconst pel-USE-COUNSEL 4 "Bitmask identifying Counsel.  DON'T CHANGE!")
(defconst pel-USE-HELM    8 "Bitmask identifying Helm.     DON'T CHANGE!")

(defcustom pel-initial-completion-mode 'emacs-default
  "Select the main text completion mode used when Emacs starts.

PEL supports several completion engines.
This option selects which engine used when Emacs starts.
The available options are:
- nil           : Use Emacs default.
- `helm'        : Use Helm, when `pel-use-helm' is t.
- `ido'         : Use Ido, when `pel-use-ido' is t.
- `ido/helm'    : Use Ido with Helm, if both `pel-use-ido' and `pel-use-help'
                  are t.
- `ivy'         : Use Ivy, when `pel-use-ivy' is t.
- `ivy/counsel' : Use Ivy with Counsel, when `pel-use-ivy' and `pel-use-counsel'
                  are both t."
  :group 'pel-pkg-for-completion
  :type '(choice
          (const :tag "Use Emacs default" emacs-default)
          (const :tag "Use Helm. Requires `pel-use-helm'." helm)
          (const :tag "Use Ido.  Requires `pel-use-ido'." ido)
          (const :tag "Use Ido with Helm. Needs `pel-use-ido' & `pel-use-helm'"
                 ido/helm)
          (const :tag "Use Ivy. Requires pel-use-ivy." ivy)
          (const :tag "Use Ivy & Counsel. Needs both `pel-use-ivy' and \
`pel-use-counsel'." ivy/counsel)))

;; -----------------------------------------------------------------------------
;; pel-pkg-for-cursor
;; ------------------
(defgroup pel-pkg-for-cursor nil
  "List of external packages for cursor management that may be used with PEL."
  :group 'pel-package-use
  :group 'cursor
  :link `(url-link :tag "Cursor PDF" ,(pel-pdf-file-url "cursor")))

(defcustom pel-use-multiple-cursors nil
  "Control whether PEL uses the multiple cursors package."
  :group 'pel-pkg-for-cursor
  :type 'boolean
  :safe #'booleanp
  :link `(url-link :tag "multiple-cursors @ GitHub"
                   "https://github.com/magnars/multiple-cursors.el"))

(defcustom pel-use-iedit nil
  "Control whether PEL uses the iedit package.

When set PEL activates the iedit mode when one of the following key
sequences are typed:

- C-;
- <f11> e
- <f11> m i"
  :group 'pel-pkg-for-cursor
  :group 'pel-pkg-for-all-languages
  :group 'pel-pkg-for-highlight
  :type 'boolean
  :safe #'booleanp
  :link `(url-link :tag "iedit @ GitHub"
                   "https://github.com/victorhge/iedit"))

;; ---------------------------------------------------------------------------
;; pel-pkg-for-cut-and-paste
;; -------------------------
(defgroup pel-pkg-for-cut-and-paste nil
  "List of external packages that PEL can use to control cut/paste, kill/yank."
  :group 'pel-package-use
  :link `(url-link :tag "Cut & Paste -- Copy/Delete/Kill/Yank PDF"
                   ,(pel-pdf-file-url "cut-paste")))

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
  :link `(url-link :tag "popup-kill-ring @ EmacsWiki"
                   "https://www.emacswiki.org/emacs/popup-kill-ring.el")
  :link `(url-link :tag "Uncle Dave's YouTube video on popup-kill-ring"
                   "https://www.youtube.com/watch?v=LFXA089Tx38")
  :group 'pel-pkg-for-cut-and-paste
  :type 'boolean
  :safe #'booleanp)

;; -----------------------------------------------------------------------------
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

(defcustom pel-use-dired-x nil
  "Control whether PEL activates the Dired-X features in `dired-mode'."
  :group 'pel-pkg-for-dired
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
  :group 'pel-package-use
  :link `(url-link :tag "Auto-Completion PDF" ,(pel-pdf-file-url "auto-completion")))

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
;; pel-pkg-for-filemng
;; -------------------
(defgroup pel-pkg-for-filemng nil
  "List of external packages that can be used to manage file/directory."
  :group 'pel-package-use
  :group 'files
  :link `(url-link :tag "File Management PDF" ,(pel-pdf-file-url "file-mngt")))


(defcustom pel-delete-trailing-whitespace t
  "Controls whether whitespaces are automatically deleted when file is saved.
Deleted automatically when non-nil, don't otherwise."
  :group 'pel-pkg-for-filemng
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-update-time-stamp t
  "Controls whether file timestamp is updated automatically on file save.
Update timestamp automatically when non-nil, don't otherwise.
See the time stamp format and location constraints in the Emacs manual
by executing:  M-: (info \"(emacs) Time Stamps\")."
  :group 'pel-pkg-for-filemng
  :type 'boolean
  :safe #'booleanp)

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

(defcustom pel-use-recentf nil
  "Control whether PEL activates the recentf built-in package.
This activates the \"Open Recent\" File menu.

When the `pel-use-ido' is also activated, PEL maps ``<f11> f f``
to the function `ido-recentf-open'.

Note that activating that feature imposes a small impact on Emacs
init time.  The feature cannot be delayed without impacting its
ability to detect files opened on startup."
  :group 'pel-pkg-for-filemng
  :type 'boolean
  :safe #'booleanp)

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; Directory Tree Browsing and Management
;; --------------------------------------

(defgroup pel-pkg-for-browse nil
  "PEL Directory Tree Browsing and Management."
    :group 'pel-pkg-for-filemng)

(defcustom pel-use-treemacs nil
  "Control whether PEL uses the treemacs package."
  :group 'pel-pkg-for-browse
  :type 'boolean
  :safe #'booleanp
  :link `(url-link :tag "treemacs @ GitHub"  "https://github.com/Alexander-Miller/treemacs"))

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
(defgroup pel-pkg-for-neotree nil
  "PEL extra configuration for NeoTree package."
    :group 'pel-pkg-for-browse)

(defcustom pel-use-neotree nil
  "Control whether PEL uses the Emacs NeoTree search package."
  :group 'pel-pkg-for-neotree
  :type 'boolean
  :safe #'booleanp
  :link `(url-link :tag "NeoTree @ GitHub"  "https://github.com/jaypei/emacs-neotree"))

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
  :link `(url-link :tag "all-the-icons @ GitHub"
                   "https://github.com/domtronn/all-the-icons.el"))

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defgroup pel-pkg-for-ztree nil
  "PEL extra configuration for ztree packages."
    :group 'pel-pkg-for-browse)

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

;; ----------------------------------------------------------------------------
;; Frame Control
;; -------------
(defgroup pel-pkg-for-frame nil
  "Frame Management Control."
  :group 'pel-package-use
  :group 'pel-pkg-for-window
  :link `(url-link :tag "Frames PDF" ,(pel-pdf-file-url "frames")))

(defcustom pel-use-framemove nil
  "Control whether PEL uses the framemove package.
It is similar to windmove and ties with it.

Notes:
- With PEL, if framemove is used windmove must also be used
  (see `pel-use-windmove').
- This package work with Emacs in graphics mode.  Multiple
  frames *can* be used in terminal (TTY) mode but only one can
  be displayed at a time in the terminal window.
- This file is not available MELPA (as of Sept 2020).
- The version 0.10 is available via the relevant EmacsWiki page.
- Older version 0.9 used an obsolete function, that was fixed in version 0.10."
  :link `(url-link :tag "framemove.el @ EmacsWiki"
                   "https://www.emacswiki.org/emacs/framemove.el")
  :link `(url-link :tag "Author's site: Emacs Tip# 35: framemove"
                   "http://trey-jackson.blogspot.com/2010/02/\
emacs-tip-35-framemove.html")
  :link `(url-link :tag "EmacsWiki framemove page"
                   "https://www.emacswiki.org/emacs/FrameMove")
  :link `(url-link :tag "Youtube video on windmove and framemove"
                   "https://www.youtube.com/watch?v=f3th2jyv35c")
  :group 'pel-pkg-for-frame
  :type 'boolean
  :safe #'booleanp)

;; -----------------------------------------------------------------------------
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
  :link `(url-link :tag "all-the-icons @ GitHub"
                   "https://github.com/domtronn/all-the-icons.el"))

(defcustom pel-use-all-the-icons-ibuffer nil
  "Control whether PEL uses the all-the-icons package in ibuffer.
This is only used when Emacs runs in graphics mode."
  :group 'pel-pkg-for-graphics-emacs
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-use-all-the-icons-dired nil
  "Control whether PEL uses the all-the-icons package in dired.
This is only used when Emacs runs in graphics mode."
  :group 'pel-pkg-for-graphics-emacs
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-use-all-the-icons-ivy nil
  "Control whether PEL uses the all-the-icons package in ivy.
This is only used when Emacs runs in graphics mode."
  :group 'pel-pkg-for-graphics-emacs
  :type 'boolean
  :safe #'booleanp)

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
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

;; -----------------------------------------------------------------------------
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

(defcustom pel-use-ripgrep nil
  "Control whether PEL uses the ripgrep tool and its associated packages.
Ripgrep is a very fast grep utility, and two packages support ripgrep:

- the rg package,
- the ripgrep package.

Setting `pel-use-ripgrep' to t indicates that you want to use ripgrep, so
it identifies the installation of the `rg` package.  If you also set
`pel-use-projectile' to non-nil, then the installation of the `ripgrep`
package is also required becuase `projectile` uses the `ripgrep` package,
it does not uses `rg`."
  :link `(url-link :tag "ripgrep @ GitHub"
                   "https://github.com/BurntSushi/ripgrep")
  :link `(url-link :tag "Emacs rg  package"
                   "https://melpa.org/#/rg")
  :link `(url-link :tag "Emacs ripgrep @ GitHub"
                   "https://github.com/nlamirault/ripgrep.el")
  :group 'pel-pkg-for-grep
  :type 'boolean
  :safe #'booleanp)

;; ---------------------------------------------------------------------------
;; Highlight Support
;; -----------------

(defgroup pel-pkg-for-highlight nil
  "PEL highlight control support."
  :group 'pel-package-use
  :link `(url-link :tag "Highlighting PDF" ,(pel-pdf-file-url "highlight")))

(defcustom pel-use-fill-column-indicator nil
  "Control whether PEL uses fill-column-indicator package.

For Emacs versions earlier than 27.1 set it to activate the ability to
highlight the current fill-column, the column where automatic line
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

;; -----------------------------------------------------------------------------
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
  :group 'pel-pkg-for-highlight
  :group 'pel-pkg-for-all-languages
  :link `(url-link :tag "Emacs Lisp PDF" ,(pel-pdf-file-url "pl-emacs-lisp"))
  :link `(url-link :tag "Common Lisp PDF" ,(pel-pdf-file-url "pl-common-lisp"))
  :link `(url-link :tag "Diff & Merge PDF" ,(pel-pdf-file-url "diff-merge")))

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
;; Insertion of Text & Templates
;; -----------------------------
(defgroup pel-pkg-for-insertions nil
  "List of external packages that PEL can use to provide easy text insertion."
  :group 'pel-package-use
  :link `(url-link :tag "Inserting Text PDF" ,(pel-pdf-file-url "inserting-text")))

(defcustom pel-use-lice nil
  "Control whether PEL uses the lice package to insert software license text."
  :group 'pel-pkg-for-insertions
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-use-smart-dash nil
  "Control whether PEL activates the smart-dash package.
This helps inserting underscore characters by typing the dash key without
having to hit the Shift key.
See the author site at URL http://malsyned.net/smart-dash.html"
  :group 'pel-pkg-for-insertions
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-modes-activating-smart-dash-mode
  '(c-mode
    c++-mode
    d-mode
    elixir-mode
    erlang-mode
    python-mode
    shell-script-mode)
  "List of major modes that automatically activate the smart dash mode.
Used when `pel-use-smart-dash' user option is t.
To activate the changes for this you must 'Apply and Save' and restart Emacs."
  :group 'pel-pkg-for-insertions
  :type
  '(repeat symbol))

(defcustom pel-use-yasnippet nil
  "Control whether PEL uses yasnippet package."
  :group 'pel-pkg-for-insertions
  :type '(choice
          (const :tag "Do not use" nil)
          (const :tag "Use, activate later by command"  t)
          (const :tag "Use, activate globally when Emacs starts" use-from-start)))

(defcustom pel-use-yasnippet-snippets nil
  "Control whether PEL uses the yasnippet-snippets package.
That package loads a set of snippets for yasnippet.
PEL activates it only if variable `pel-use-yasnippet' is non-nil."
  :group 'pel-pkg-for-insertions
  :type 'boolean
  :safe #'booleanp)

;; -----------------------------------------------------------------------------
;; pel-pkg-for-kbmacro
;; -------------------
(defgroup pel-pkg-for-kbmacro nil
  "List of external packages that PEL can use to handle keyboard macros."
  :group 'pel-package-use
  :group 'kmacro
  :link `(url-link :tag "Keyboard Macros PDF" ,(pel-pdf-file-url "keyboard-macros"))
  :link `(url-link :tag "Function Keys Usage PDF" ,(pel-pdf-file-url "keys-fn")))

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

;; -----------------------------------------------------------------------------
;; pel-pkg-for-key-chord
;; ---------------------
(defgroup pel-pkg-for-key-chord nil
  "PEL support for key-chords."
  :group 'pel-package-use
  :group 'keyboard
  :link `(url-link :tag "Key-chords PDF" ,(pel-pdf-file-url "key-chords"))
  :link `(url-link :tag "key-chord.el"
                   "https://github.com/emacsorphanage/key-chord/\
blob/master/key-chord.el"))

(defcustom pel-use-key-chord nil
  "Control whether PEL uses the key-chord external package.
With it, it's possible to activate binding actions to two keys
pressed simultaneously or a single key quicly pressed twice.

This can be set to:
- 0: nil: Do not use.  key-chord is not required nor loaded.
- 1: t: Use, activate by command.  key-chord loaded when the function
        `key-chord-mode' is executed.  Not before.
- 2: use-from-start:  Use, load and activate  1 second after Emacs starts."
  :group 'pel-pkg-for-key-chord
  :link `(url-link :tag "key-chord @ MELPA" "https://melpa.org/#/key-chord")
  :type '(choice
          (const :tag "Do not use" nil)
          (const :tag "Use, activate later by command"  t)
          (const :tag "Use, activate when Emacs starts" use-from-start)))

(defcustom pel-use-key-seq nil
  "Control whether PEL key-chord is also using key-seq.
If t, the boolean field 'key-seq' key-chords definitions in
`pel-key-chords' is honoured: instead of declaring them a
key-chord PEL declares them key-seq, making the order of the keys
relevant.
To use key-seq you must also activate key-chords via `pel-use-key-chord'."
  :group 'pel-pkg-for-key-chord
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-key-chord-two-keys-delay 0.1	; 0.05 or 0.1
  "Max time delay between two key press to be considered a key chord."
  :group 'pel-pkg-for-key-chord
  :type 'float
  :safe #'floatp)

(defcustom pel-key-chord-one-key-delay 0.2	; 0.2 or 0.3 to avoid first autorepeat
  "Max time delay between 2 press of the same key to be considered a key chord.
This should normally be a little longer than `key-chord-two-keys-delay'."
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


(defcustom pel-key-chords
  '((global    ""         key-chord      "<>"  "<>\C-b")
    (global    ""         key-chord      "[]"  "[]\C-b")
    (c-mode    "cc-mode"  key-chord      "{}"  "{\n\n}\C-p\C-p")
    (c++-mode  "cc-mode"  key-chord      "{}"  "{\n\n}\C-p\C-p")
    (global    ""         key-chord
               "yu" (lambda ()
                      (interactive)
                      (if (require 'windmove nil :noerror)
                          (windmove-up)
                        (insert "yu"))))

    (global    ""         key-chord
               "bn" (lambda ()
                      (interactive)
                      (if (require 'windmove nil :noerror)
                          (windmove-down)
                        (insert "bn"))))

    (global    ""         key-seq ; prevent 'fg', often in words.
               "gf" (lambda ()
                      (interactive)
                      (if (require 'windmove nil :noerror)
                          (windmove-left)
                        (insert "gf"))))

    (global    ""         key-chord
               "jk" (lambda ()
                      (interactive)
                      (if (require 'windmove nil :noerror)
                          (windmove-right)
                        (insert "jk"))))

    (global ""            key-chord
            "	q" (lambda (&optional n)
                     (interactive "*P")
                     (if (require 'pel-indent nil :noerror)
                         (pel-indent-rigidly n)
                       (insert "	q"))))

    (flyspell-mode "flyspell" key-chord
                   "4r" (lambda ()
                          (interactive)
                          (if (require 'flyspell nil :noerror)
                              (flyspell-correct-word-before-point)
                            (insert "4r"))))

    (flyspell-prog-mode "flyspell" key-chord
                        "4r" (lambda ()
                               (interactive)
                               (if (require 'flyspell nil :noerror)
                                   (flyspell-correct-word-before-point)
                                 (insert "4r"))))

    (global    ""         key-chord
               "6y"  (lambda (&optional n)
                       (interactive "P")
                       (if (require 'pel-open nil :noerror)
                           (pel-open-at-point n)
                         (insert "6y"))))

    (global    ""         key-chord
               "6u"  (lambda ()
                       (interactive)
                       (if (require 'pel-open nil :noerror)
                           (pel-browse-filename-at-point)
                         (insert "6u"))))

    (global    ""         key-chord
               "7u"  (lambda ()
                       (interactive)
                       (if (require 'browse-url nil :noerror)
                           (browse-url-at-point)
                         (insert "7u"))))

    (global    ""         key-chord
               ".;"  (lambda (&optional n)
                       (interactive "P")
                       (if (require 'pel-search nil :noerror)
                           (pel-search-word-from-top n)
                         (insert ".;")))))
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
    Identify a name that (load FILE) will be able to load, ie
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
    - It's not possible to identify the function keys or
      the cursor keys in a portable way here.
  - The fifth item describes the action for the key-chord
    or key-seq.  The action can be expressed using one of 3 ways,
    selected by its Value Menu:
    - 0: expansion keys:
         Type the keys you want as replacement. You can
         place several keys on a line, or spread them on several
         lines.  You can identify control keys by entering the
         kbd-style like C-b (by typing 'C', '-', then 'b')
         or by placing the control code by typing C-q C-b.
         Unfortunately it is currently not possible to identify
         a keystroke involving other modifiers or combination of
         modifiers; the PEL code is not able to properly recognize it
         to pass it to the key-chord function.
         Use the lambda form instead.
         BTW, if you know how to fix that please don't hesitate
         to either let me know or submit a pull-request.
    - 1: command:
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
    - 2: lambda:
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
     (choice (string   :tag "expansion")
             (function :tag "function")
             (function :tag "lambda" :value (lambda () (interactive) <YOUR CODE HERE>))))))

;; -----------------------------------------------------------------------------
;; Keys & Prompts
;; --------------
(defgroup pel-pkg-for-keys nil
  "List of external packages that PEL can use to help deal keys and prompts."
  :group 'pel-package-use
  :link `(url-link :tag "AppleScript PDF" ,(pel-pdf-file-url "pl-applescript"))
  :link `(url-link :tag "Function Keys PDF" ,(pel-pdf-file-url "keys-fn"))
  :link `(url-link :tag "Hide/Show PDF" ,(pel-pdf-file-url "hide-show-code"))
  :link `(url-link :tag "Windows PDF" ,(pel-pdf-file-url "windows")))

(defcustom pel-use-hydra nil
  "Control whether PEL uses the hydra package."
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

(defcustom pel-use-keycast nil
  "Control whether the keycast package is made available."
  :group 'pel-pkg-for-keys
  :type 'boolean
  :safe #'booleanp
  :link '(url-link :tag "keycast @ GitHub"
                   "https://github.com/tarsius/keycast"))

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
;; AsciiDoc Support
;; ----------------
(defgroup pel-pkg-for-asciidoc nil
  "PEL AsciiDoc support."
  :group 'pel-pkg-for-markup
  :link `(url-link :tag "AsciiDoc PDF" ,(pel-pdf-file-url "asciidoc")))

(defcustom pel-use-asciidoc nil
  "Control whether PEL activates support for Asciidoc with adoc mode."
  :group 'pel-pkg-for-asciidoc
  :type 'boolean
  :safe #'booleanp)

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; pel-pkg-for-draw-markup
;; -----------------------
(defgroup pel-pkg-for-drawing-markup nil
  "PEL drawing markup support."
  :group 'pel-pkg-for-markup
  :link `(url-link :tag "Drawing PDF" ,(pel-pdf-file-url "drawing")))

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
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
  :link `(url-link :tag "Graphviz home page"
                   "https://www.graphviz.org")
  :link `(url-link :tag "DOT Language"
                   "https://www.graphviz.org/doc/info/lang.html")
  :link `(url-link :tag "graphviz-dot-mode @ MELPA"
                   "https://melpa.org/#/graphviz-dot-mode")
  :group 'pel-pkg-for-graphviz-dot
  :type 'boolean
  :safe #'booleanp)

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; PlantUML Support
;; ----------------

(defgroup pel-pkg-for-plantuml nil
  "PEL UML support."
  :group 'pel-pkg-for-drawing-markup
  :link `(url-link :tag "PlantUML PDF" ,(pel-pdf-file-url "plantuml"))
  :link `(url-link :tag "PlantUML @ GitHub" "https://github.com/skuro/plantuml-mode.")
  :link `(url-link :tag "PlantUML home page" "https://plantuml.com")
  :link `(url-link :tag "PlantUML @ wikipedia" "https://en.wikipedia.org/wiki/PlantUML"))

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

(defcustom pel-use-flycheck-plantuml nil
  "Control the flycheck-plantuml PlantUML checker package is used with PEL.
See info at URL https://github.com/alexmurray/flycheck-plantuml"
  :group 'pel-pkg-for-plantuml
  :type 'boolean
  :safe #'booleanp)

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; reStructuredText support
;; ------------------------
(defgroup pel-pkg-for-reST nil
  "PEL reStructuredText support."
  :group 'pel-pkg-for-markup
  :link `(url-link :tag "reStructuredText PDF" ,(pel-pdf-file-url "mode-rst")))

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
  - sub-subsections,
  - paragraphs.
- `CRiSPer', a title and 12-level mode previously developed for CRiSP."
  :group 'pel-pkg-for-reST
  :type '(choice (const :tag "default" default)
                 (const :tag "Sphinx-Python" Sphinx-Python)
                 (const :tag "CRiSPer" CRiSPer)))

(defcustom pel-rst-tab-width 2
  "Distance between tab stop for reStructuredText buffers.
PEL stores this in `tab-width' when opening reStructuredText buffers.
This does *NOT* control the indentation in reStructuredText files,
only for commands that mode point to tab stop positions
such as `tab-to-tab-stop', and the display of hard TAB characters."
  :group 'pel-pkg-for-reST
  :type 'integer
  :safe 'pel-indent-valid-p)

;; -----------------------------------------------------------------------------
;; pel-pkg-for-navigation
;; ----------------------
(defgroup pel-pkg-for-navigation nil
  "List of external packages that PEL can use to help navigation."
  :group 'pel-package-use
  :link `(url-link :tag "Navigation PDF" ,(pel-pdf-file-url "navigation")))

(defcustom pel-use-ace-link nil
  "Control activation of the ace link package."
  :link `(url-link :tag "ace-link @ GitHub" "https://github.com/abo-abo/ace-link")
  :group 'pel-pkg-for-navigation
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-use-avy nil
  "Control activation of the avy package."
  :link `(url-link :tag "avy @ GitHub" "https://github.com/abo-abo/avy")
  :group 'pel-pkg-for-navigation
  :type 'boolean
  :safe #'booleanp)

;; -----------------------------------------------------------------------------
;; pel-pkg-for-xref
;; ----------------
(defgroup pel-pkg-for-xref nil
  "List of external packages PEL can use for handling cross references."
  :group 'pel-pkg-for-navigation
  :group 'pel-package-use
  :link '(custom-group-link "pel-pkg-for-project-mng")
  :link '(custom-group-link "projectile")
  :link '(custom-group-link "speedbar")
  :link `(url-link :tag "Xref PDF" ,(pel-pdf-file-url "xref")))

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
  :link '(url-link :tag "xcscope @ GitHub" "https://github.com/dkogan/xcscope.el")
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-use-helm-cscope nil
  "Control whether PEL uses the helm-cscope package.

Note: activating `pel-use-helm-cscope' and `pel-use-xcscope'
implicitly activates `pel-use-helm'."
  :group 'pel-pkg-for-xref
  :link '(url-link :tag "helm-cscope @ GitHub" "https://github.com/alpha22jp/helm-cscope.el")
  :type 'boolean
  :safe #'booleanp)

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
With dumb-jump, the M-. command will use dumb-jump to
identify symbol in several programming languages."
  :group 'pel-pkg-for-xref
  :link '(url-link :tag "dump-jump @ GitHub" "https://github.com/jacktasia/dumb-jump")
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

As an alternative you can quickly toggle ggtags-mode with the <f11> X G
key sequence."
  :group 'pel-pkg-for-xref
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
with gxref-mode with the <f11> X R key sequence."
  :group 'pel-pkg-for-xref
  :type '(repeat symbol)
  :link '(url-link :tag "gxref @ GitHub"
                   "https://github.com/dedi/gxref"))

;; -- rtags
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

;; -----------------------------------------------------------------------------
;; Programming Language Support
;; ============================
(defgroup pel-pkg-for-programming nil
  "PEL customization for programming languages."
  :group 'pel-package-use
  :link `(url-link :tag "Comments PDF" ,(pel-pdf-file-url "comments")))

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; Generic Programming Support
;; ---------------------------
(defgroup pel-pkg-for-all-languages nil
  "PEL Generic Programming support."
  :group 'pel-pkg-for-programming
  :link `(url-link :tag "Comments PDF" ,(pel-pdf-file-url "comments"))
  :link `(url-link :tag "Inserting Text PDF" ,(pel-pdf-file-url "inserting-text")))

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
  :link '(url-link :tag "eldoc-box @ GitHub" "https://github.com/casouri/eldoc-box")
  :link '(custom-group-link "eldoc"))

(defcustom pel-use-hide-comnt nil
  "Control whether PEL activates Drew Adams' hide-cmnt package.
This package provides the ability to hide comments."
  :group 'pel-pkg-for-all-languages
  :type 'boolean
  :safe #'booleanp)

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; Language Server Protocol (LSP) Support
;; --------------------------------------

(defgroup pel-pkg-for-language-server nil
  "PEL support for language server protocol."
  :group  'pel-pkg-for-all-languages)

(defcustom pel-use-eglot nil
  "Control whether PEL supports the eglot package.
eglot is a client for Language Server Protocol servers."
  :group 'pel-pkg-for-language-server
  :type 'boolean
  :safe #'booleanp)

;; -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -
(defgroup pel-pkg-generic-code-style nil
  "PEL Generic code style configuration."
  :group 'pel-pkg-for-all-languages
  :link `(url-link :tag "Inserting Text PDF" ,(pel-pdf-file-url "inserting-text")))

(defcustom pel-generic-skel-use-separators t
  "Specifies whether generic code block include separators line.
If nil no separator line comment is used, otherwise separator line
comments of length controlled by variable `fill-column' are inserted."
  :group 'pel-pkg-generic-code-style
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-generic-skel-insert-file-timestamp t
  "Specifies whether a timestamp is inserted inside file module header block."
  :group 'pel-pkg-generic-code-style
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-generic-skel-with-license nil
  "Control whether a license text is inserted in code file module header block.

When t, the licence inserted is controlled by the function `lice' taken
from the external library with the same name.
If t this activates `pel-use-lice' if it is not activated already.

The text of the inserted license is selected by the `lice:default-license'
user option, normally configured inside the directory's '.dir-locals.el'
file written inside the global setting like this:

   ((nil   .      ((fill-column . 80)
                   (lice:default-license  . \"gpl-3.0\")
                   (lice:copyright-holder . \"Your Name\")))

Replace the gpl-3.0 with the license you want and write your name inside
the copyright holder value."
  :group 'pel-pkg-generic-code-style
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-generic-skel-module-header-block-style nil
  "Specifies the style of the code file module header block.
You can use one of the following:

- The default, controlled by PEL's code.
- A user-specified one.  For this you have to write Emacs Lisp code.
  You have to write a function `pel-skels-generic-header-module-block/custom'
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
  See the code of function `pel-skels-generic-header-module-block'
  for an example of how these arguments are used to create the standard
  header-module block skeleton.

  You can start by using the example that is stored inside the file
  'custom/skeleton/custom-c-skel.el'.
  The file name can be an absolute file name but it can also be a relative
  file name.  On Unix systems you can use '~' to identify your home directory."
  :group 'pel-pkg-generic-code-style
  :type '(choice
          (const  :tag "Default, controlled by PEL." nil)
          (string :tag "Use your own custom definition\n inside file")))


(defcustom pel-generic-skel-insert-module-sections t
  "Specifies whether code sections are inserted inside code file comment block.
This includes the \"Module Description\" section and sections
with titles identified by the variable
`pel-generic-skel-module-section-titles'."
  :group 'pel-pkg-generic-code-style
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-generic-skel-module-section-titles '("Dependencies"
                                                    "Code")
  "List of section titles to add in the module comment block.
These section names are added when the variable
`pel-generic-skel-insert-module-sections' is t, after the
\"Module Description\" section. The sections are placed inside
the module documentation block in the order of appearance in the
list with the string as it appears in the list.  The default is
to add the following sections:

- Header Inclusion,
- Local Types,
- Local Variables,
- Code.

Empty strings can be used to specify section with a tempo marker with no text."
  :group 'pel-pkg-generic-code-style
  :type '(repeat string))

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; AppleScript support
;; -------------------
(defgroup pel-pkg-for-applescript nil
  "PEL customization for AppleScript."
  :group 'pel-pkg-for-programming
  :group 'apples
  :link `(url-link :tag "AppleScript PDF" ,(pel-pdf-file-url "pl-applescript")))

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
  :group 'pel-pkg-for-cc
  :group 'c
  :link `(url-link :tag "C PDF" ,(pel-pdf-file-url "pl-c")))

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
  :link '(custom-manual "(ccmode)Built-in Styles")
  :link `(url-link
          :tag "Bracket styles @ Emacs Manual"
          "https://www.gnu.org/software/emacs/manual/html_node/\
ccmode/Built_002din-Styles.html#Built_002din-Styles")
  :link `(url-link :tag "Indentation styles @ wikipedia"
                   "https://en.wikipedia.org/wiki/Indentation_style")
  :group 'pel-pkg-for-c
  :type 'string
  :safe 'pel-c-style-valid-p)

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

;; -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -
(defgroup pel-c-code-style nil
  "Emacs Lisp Source Code Style options."
  :group 'pel-pkg-for-c
  :link `(url-link :tag "C PDF" ,(pel-pdf-file-url "pl-c")))


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

(defcustom pel-c-skel-comment-with-2stars t
  "Specifies whether multi-line C comments continuation use 2 stars.
If set to t (the default), C comments in generated code
use the following style comment format:   /*
                                          **
                                          */

If set to nil, the comment style is:      /*
                                           *
                                           */"
  :group 'pel-c-code-style
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-c-skel-use-separators t
  "Specifies whether C code block include separators line.
If nil no separator line comment is used, otherwise separator line
comments of length controlled by variable `fill-column' are inserted."
  :group 'pel-c-code-style
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-c-skel-insert-file-timestamp t
  "Specifies whether a timestamp is inserted inside C file header block."
  :group 'pel-c-code-style
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-c-skel-use-uuid-include-guards t
  "Specifies whether UUID-based include guards are inserted inside C header file."
  :group 'pel-c-code-style
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-c-skel-module-header-block-style nil
  "Specifies the style of the C file module header block.
You can use one of the following:

- The default, controlled by PEL's code.
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
  See PEL's function `pel-skels-c-header-module-block' source code
  for an example of how these arguments are used to create the standard
  header-module block skeleton.

  You can start by using the example that is stored inside the file
  'custom/skeleton/custom-c-skel.el'.
  The file name can be an absolute file name but it can also be a relative
  file name.  On Unix systems you can use '~' to identify your home directory."
  :group 'pel-c-code-style
  :type '(choice
          (const  :tag "Default, controlled by PEL." nil)
          (string :tag "Use your own custom definition\n inside file")))

(defcustom pel-c-skel-insert-module-sections t
  "Specifies whether code sections are inserted inside C file comment block.
This includes the \"Module Description\" section and sections
with titles identified by the variable `pel-c-skel-module-section-titles'."
  :group 'pel-c-code-style
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-c-skel-module-section-titles '("Header Inclusion"
                                              "Local Types"
                                              "Local Variables"
                                              "Code")
  "List of section titles to add in the module comment block.
These section names are added when the variable
`pel-c-skel-insert-module-sections' is t, after the \"Module
Description\" section. The sections are placed inside the module
documentation block in the order of appearance in the list with
the string as it appears in the list.  The default is to add the
following sections:

- Header Inclusion,
- Local Types,
- Local Variables,
- Code.

Empty strings can be used to specify section with a tempo marker with no text."
  :group 'pel-c-code-style
  :type '(repeat string))

(defcustom pel-c-skel-insert-function-sections t
  "Specifies whether code sections are inserted in C function comment block.
This includes the DESCRIPTION section and sections with titles
identified by the variable `pel-c-skel-function-section-titles'."
  :group 'pel-c-code-style
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
  :group 'pel-c-code-style
  :type '(repeat string))

(defcustom pel-c-skel-use-uuid-include-guards t
  "Specifies whether UUID-based include guards are inserted in C header file."
  :group 'pel-c-code-style
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-c-skel-with-license nil
  "Control whether a license text is inserted in C file header.

When t, the licence inserted is controlled by the function `lice' taken
from the external library with the same name.
If t this activates `pel-use-lice' if it is not activated already.

The text of the inserted license is selected by the `lice:default-license'
user option, normally configured inside the directory's '.dir-locals.el'
file written inside the global setting like this:

   ((nil   .      ((fill-column . 80)
                   (lice:default-license  . \"gpl-3.0\")
                   (lice:copyright-holder . \"Your Name\")))

Replace the gpl-3.0 with the license you want and write your name inside
the copyright holder value."
  :group 'pel-c-code-style
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-c-skel-function-define-style nil
  "Specifies the style of C function definition comment blocks.
Several styles are provided with ability to load a style from
a separately provided skeleton file.

The choices are:

- No documentation comment inserted.
- Basic documentation comment just above the function definition.
  This includes a function purpose.
- Man-page style documentation above the function definition with
  a DESCRIPTION section and other sections as defined by the value of
  the variable `pel-c-skel-function-section-titles'.
- A user defined skeleton.  For this you need to write Emacs Lisp code.
  You have to write a function `pel-skels-c-function-def/custom'
  inside a file and store the name of that file inside the box that
  appear when you select this option.  You can start by using the
  example that is stored inside the file 'custom/skeleton/custom-c-skel.el'.
  The file name can be an absolute file name but it can also be a relative
  file name.  On Unix systems you can use '~' to identify your home directory."
  :group 'pel-c-code-style
  :type '(choice
          (const :tag "Just code, no comment block." nil)
          (const :tag "Basic documentation block above function definition." basic-style)
          (const :tag "Man-page style documentation block above function definition." man-style)
          (string :tag "Use your own custom definition\n inside file")))

(defcustom pel-c-skel-doc-markup nil
  "Specifies the documentation markup system used for C source code."
    :group 'pel-c-code-style
  :type '(choice
          (const :tag "No documentation markup inserted in templates." nil)
          (const :tag "Insert Doxygen markup in templates." doxygen)))

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

This affects all styles specified by variable `pel-c-skel-function-define-style'
potentially except the user defined ones, which could use that variable too."
  :group 'pel-c-code-style
  :type 'boolean
  :safe #'booleanp)

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; C++ Language Support
;; --------------------

(defgroup pel-pkg-for-c++ nil
  "PEL customization for C++."
  :group 'pel-pkg-for-cc
  :link `(url-link :tag "C++ PDF" ,(pel-pdf-file-url "pl-c++")))

(defcustom pel-c++-bracket-style "stroustrup"
  "Set the bracket style for the C++ programming language.
PEL stores this value associated with the `c-mode' into the
`c-default-style' user option variable.
If you want to use something else, please select one of the
CC Mode Built-in Styles."
  :group 'pel-pkg-for-c++
  :type 'string
  :safe 'pel-c-style-valid-p
  :link '(custom-group-link "pel-pkg-for-c")
  :link '(custom-manual "(ccmode)Built-in Styles")
  :link `(url-link
          :tag "Bracket styles @ Emacs Manual"
          "https://www.gnu.org/software/emacs/manual/html_node/\
ccmode/Built_002din-Styles.html#Built_002din-Styles")
  :link `(url-link :tag "Indentation styles @ wikipedia"
                   "https://en.wikipedia.org/wiki/Indentation_style"))

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

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
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
;; Javascript support
;; ------------------
(defgroup pel-pkg-for-javascript nil
  "PEL support for Javascript. Experimental."
  :group 'pel-pkg-for-programming)


(defcustom pel-use-javascript nil
  "Control whether PEL supports Javascript development.

When set, identifies what mode is used to support Javascript."
  :group 'pel-pkg-for-javascript
  :type '(choice
          (const :tag "Emacs basic support." nil)
          (const :tag "Future: Emacs basic + PEL additions using built-in js-mode." js-mode)
          (const :tag "Supported by the js2-mode external package." js2-mode)))

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; Lisp-like language support
;; --------------------------
;;
;; The group pel-pkg-for-lisp has settings for tools that can be used for
;; several Lisp-like programming languages like Eamcs-Lisp. Common-Lisp,
;; Clojure, Scheme, LFE, etc...

(defgroup pel-pkg-for-lisp nil
  "PEL customization for tools supporting LISP-like programming languages."
  :group 'pel-pkg-for-programming)

(defcustom pel-use-lispy nil
  "Control whether PEL uses the lispy package."
  :type 'boolean
  :safe #'booleanp
  :link `(url-link :tag "abo-abo lispy" "https://github.com/abo-abo/lispy"))

(defcustom pel-modes-activating-lispy  nil
  "List of major modes that automatically activate `lispy-mode'."
  :group 'pel-pkg-for-lisp
  :type '(repeat symbol))

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; Common Lisp Support
;; -------------------
(defgroup pel-pkg-for-clisp nil
  "PEL customization for Common Lisp."
  :group 'pel-pkg-for-lisp
  :link `(url-link :tag "Common Lisp PDF" ,(pel-pdf-file-url "pl-common-lisp")))

(defcustom pel-use-common-lisp nil
  "Control whether PEL supports Common Lisp development."
  :group 'pel-pkg-for-clisp
  :type 'boolean
  :safe #'booleanp)

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; Emacs Lisp Support
;; ------------------
(defgroup pel-pkg-for-elisp nil
  "PEL customization for Emacs Lisp."
  :group 'pel-pkg-for-lisp
  :link `(url-link :tag "Emacs Lisp PDF" ,(pel-pdf-file-url "pl-emacs-lisp")))

(defcustom pel-use-macrostep nil
  "Control whether PEL uses the macrostep package."
  :group 'pel-pkg-for-elisp
  :type 'boolean
  :safe #'booleanp
  :link '(url-link :tag "macrostep @ GitHub" "https://github.com/joddie/macrostep")
  :link '(custom-group-link "macrostep"))

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
(defgroup pel-elisp-code-style nil
  "Emacs Lisp Source Code Style options."
  :group 'pel-pkg-for-elisp
  :link `(url-link :tag "Emacs Lisp PDF" ,(pel-pdf-file-url "pl-emacs-lisp")))

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

(defcustom pel-elisp-skel-use-separators t
  "Specifies whether Elisp code block include separators line.
If nil no separator line comment is used, otherwise separator line
comments of length controlled by variable `fill-column' are inserted."
  :group 'pel-elisp-code-style
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-elisp-skel-insert-file-timestamp nil
  "Specifies whether a timestamp is inserted inside Elisp file header block."
  :group 'pel-elisp-code-style
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-elisp-skel-with-license nil
  "Control whether a license text is inserted in Elisp file header.

When t, the licence inserted is controlled by the function `lice' taken
from the external library with the same name.
If t this activates `pel-use-lice' if it is not activated already.

The text of the inserted license is selected by the `lice:default-license'
user option, normally configured inside the directory's '.dir-locals.el'
file written inside the global setting like this:

   ((nil   .      ((fill-column . 80)
                   (lice:default-license  . \"gpl-3.0\")
                   (lice:copyright-holder . \"Your Name\")))

Replace the gpl-3.0 with the license you want and write your name inside
the copyright holder value."
  :group 'pel-elisp-code-style
  :type 'boolean
  :safe #'booleanp)

;; -----------------------------------------------------------------------------
;; BEAM Programming Languages
;; --------------------------
(defgroup pel-pkg-for-beam-vm nil
  "PEL customization for BEAM Virtual Machine programming languages."
  :group 'pel-pkg-for-programming)

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
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
;; Erlang Support
;; --------------
;; Note: Erlang, is a BEAM VM programming language.
;; The PEL configuration for Erlang attempts to control several aspects of the
;; various Emacs packages supporting Erlang: there's what I currently perceive
;; as a lack of commonality of control between them (erlang.el, edts, etc..) and
;; I want to simplify the identification of the various Erlang files for several
;; Erlang versions and for these tools.  The concepts controlled are:
;;
;; - The location of Erlang man page files:
;;   - erlang.el:
;;     - Supports only one version of Erlang at a time in Emacs.
;;     - Downloads the Erlang man files inside
;;       ~/.emacs.d/cache/erlang_mode_man_pages
;;     - Has `erlang-root-dir' that should point to that location.
;;
;;  - edts:
;;    - Supports several versions of Erlang at a time in Emacs, allowing
;;      files from different directories to be associated as a project and
;;      identified as using a specific version of Erlang.
;;    - Downloads the Erlang man files for a specific Erlang version V into
;;      ~/.emacs.d/edts/doc/V
;;
;; PEL:
;;  - Supports multiple versions of Erlang, supports both erlang.el and edts
;;    models, supports the use of MANPATH in the parent shell to identify the
;;    path where the man files are stored.
;;  - Use `pel-erlang-man-rootdir' that identifies a directory that must hold
;;    sub-directories (or symlinks to the directories) named after the Erlang
;;    version (such as 23.0) where each of them as holding the content of the
;;    Erlang man tarball content:
;;       - COPYRIGHT
;;       - PR.template
;;       - README.md
;;       - man directory holding man1, man3, etc...
;;    By default `pel-erlang-man-rootdir' is set to ~/.emacs.d/edts/doc.
;;
;;  - The `pel-erlang-version-detection-method' selects one of 3 method to
;;    detect Erlang's version. They are:
;;
;;    - auto-detect : automatic detection using the `version-erl' command
;;                    line utility (provided in PEL's bin directory).
;;    - specified   : the version is specified in the `pel-erlang-version'
;;                    user option.
;;    - by-envvar   : the version is read in the content of an environment
;;                    variable. The name of the environment variable is
;;                    identified by `pel-erlang-version-envvar'.

;;  - PEL provides the version-erl command line utility (stored in PEL's bin
;;    directory).  This is a short Erlang script that prints Erlang's version on
;;    stdout.  The PEL Erlang initialization code uses that utility to
;;    automatically detects the version of Erlang available in Emacs parent
;;    process. PEL uses that to identify which version of man pages should be used.
;;
;;  - Use `pel-erlang-version' to identify the Erlang version used by default.
;;    It can be  a specific number string (like "23.0" or "21.3.8.7") or can
;;    specify an environment variable that holds that string. PEL uses the
;;    environment variable PEL_ERLANG_VERSION by default.  A different
;;    environment variable name can be identified in
;;    `pel-erlang-version-envvar'.
;;
;;  - To help support both edts and erlang.el model, PEL creates symbolic links:
;;   - ``~/.emacs.d/cache/erlang_mode_man_pages`` is a symlink that points to
;;     ``~/.emacs.d/edts/doc/V`` where V is the Erlang version determined by
;;     the method identified by `pel-erlang-version-detection-method'.
;;   - Creates ``~/.emacs.d/edts/doc/V`` symlinks for all V versions of Erlang
;;     identified in the directory pointed by `pel-erlang-man-rootdir'.
;;
;;  This way for each version of Erlang one set of files is needed and used via
;;  1 or 2 symlinks.
;;
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

(defcustom pel-erlang-man-rootdir "~/.emacs.d/edts/doc"
  "Path to the directory holding Erlang man files for several Erlang versions.
This identifies a directory that must hold
sub-directories (or symlinks to the directories) named after the Erlang
version (such as 23.0) where each of them as holding the content of the
Erlang man tarball content, something like:
  - COPYRIGHT
  - PR.template
  - README.md
  - man directory holding man1, man3, etc..."
  :group 'pel-pkg-for-erlang
  :type 'string)

(defcustom pel-erlang-version-detection-method 'auto-detect
  "Identifies the method PEL uses to determine Erlang's version to use."
  :group 'pel-pkg-for-erlang
  :type '(choice
          (const :tag "Automatic detection using PEL's bin/version-erl."
                 auto-detect)
          (const :tag "Version number specified explicitly in \
`pel-erlang-version'."  specified)
          (const :tag "Version number defined in environment variable named \
in `pel-erlang-version-envvar'." by-envvar)))

(defcustom pel-erlang-version nil
  "Erlang version used.
Controls which directory in `pel-erlang-man-rootdir' is used to identify
Erlang man files.
`pel-erlang-version' can be a specific number string (like
\"23.0\" or \"21.3.8.7\") or nil.  If set nil, PEL uses  an environment variable
to identify the Erlang version. PEL uses the environment variable
PEL_ERLANG_VERSION by default.  A different environment variable
name can be identified in `pel-erlang-version-envvar'."
  :group 'pel-pkg-for-erlang
  :type 'string)

(defcustom pel-erlang-version-envvar "PEL_ERLANG_VERSION"
  "Name of environment variable used to identify the active Erlang version.
Used when `pel-erlang-version' is nil."
  :group 'pel-pkg-for-erlang
  :type 'string)

(defcustom pel-erlang-shell-prevent-echo nil
  "Set to t if the `erlang-shell-mode' shell echoes back commands.
When set to t PEL activates code that prevent echo of the typed commands."
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

(defcustom pel-activate-edts-automatically nil
  "Control whether EDTS is activated automatically for Erlang files.
Activates EDTS automatically on Erlang files if set to t, otherwise
you must activate it manually with \\[edts-mode].
Starting EDTS takes some time and will slow down opening Erlang files
if configured to activate automatically."
  :group 'pel-pkg-for-erlang
  :type 'boolean
  :safe #'booleanp)

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
(defgroup pel-erlang-code-style nil
  "Erlang Source Code Style options."
  :group 'pel-pkg-for-erlang
  :link `(url-link :tag "Erlang PDF" ,(pel-pdf-file-url "pl-erlang")))

(defcustom pel-erlang-fill-column 100
  "Column beyond which automatic line-wrapping should happen in Erlang code.
Can either be nil or an integer value.
When set to nil, Emacs user option variable `fill-column' value
is used for `erlang-mode' buffers, otherwise the integer value specified by
this value is stored in `fill-column' for Erlang source code files.
The default is 100, a value recommended by the Inaka's Erlang Coding
Standards & Guidelines."
  :group 'pel-erlang-code-style
  :type '(choice
          (const   :tag "Use the default fill-column value." nil)
          (integer :tag "Use a specific value for erlang-mode buffers:"))
  :link '(url-link :tag "Inka Erlang Guideline"
                   "https://github.com/inaka/erlang_guidelines#100-column-per-line"))

(defcustom pel-erlang-skel-use-separators t
  "Specifies whether Erlang code block include separators line.
If nil no separator line comment is used, otherwise separator line
comments of length controlled by variable `fill-column' are inserted."
  :group 'pel-erlang-code-style
  :type 'boolean
  :safe #'booleanp)

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

(defcustom pel-erlang-skel-insert-file-timestamp nil
  "Specifies whether a timestamp is inserted inside Erlang file header block."
  :group 'pel-erlang-code-style
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-erlang-skel-with-edoc t
  "Control whether Edoc comments are placed inside generated Erlang code."
  :group 'pel-erlang-code-style
  :type '(choice
          (const :tag "Do not insert Edoc comment." nil)
          (const :tag "Insert Edoc comments everywhere." t)
          (const :tag "Insert Edoc comments only in functions, not in file header." in-function-only)))

(defcustom pel-erlang-skel-prompt-for-purpose t
  "Control whether skeleton insertions prompt for purpose strings."
  :group 'pel-erlang-code-style
  :type '(choice
          (const :tag "Never prompt for purpose." nil)
          (const :tag "Always prompt for purpose (and function name)." t)
          (const :tag "Only prompt for file purpose." in-file-only)
          (const :tag "Only prompt for function purpose (and function name)." in-function-only)))

(defcustom pel-erlang-skel-prompt-for-function-name t
  "Control whether skeleton insertions prompt for function name."
  :group 'pel-erlang-code-style
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-erlang-skel-prompt-for-function-arguments t
  "Control whether skeleton insertions prompt for function arguments."
  :group 'pel-erlang-code-style
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-erlang-skel-with-license nil
  "Control whether a license text is inserted in file header comment block.

When t, the licence inserted is controlled by the function `lice' taken
from the external library with the same name.
If t this activates `pel-use-lice' if it is not activated already.

The text of the inserted license is selected by the `lice:default-license'
user option, normally configured inside the directory's '.dir-locals.el'
file written inside the global setting like this:

   ((nil   .      ((fill-column . 80)
                   (lice:default-license  . \"gpl-3.0\")
                   (lice:copyright-holder . \"Your Name\")))

Replace the gpl-3.0 with the license you want and write your name inside
the copyright holder value."
  :group 'pel-erlang-code-style
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
;; Forth support
;; -------------
(defgroup pel-pkg-for-forth nil
  "PEL customization for Forth."
  :group 'pel-pkg-for-programming
  :link `(url-link :tag "Forth PDF" ,(pel-pdf-file-url "pl-forth")))

(defcustom pel-use-forth nil
  "Control whether PEL supports Forth development."
  :group 'pel-pkg-for-forth
  :type 'boolean
  :safe #'booleanp)

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; Julia Support
;; --------------
(defgroup pel-pkg-for-julia nil
  "PEL customization for Julia."
  :group 'pel-pkg-for-programming
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

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; Python Support
;; --------------
(defgroup pel-pkg-for-python nil
  "PEL customization for Python."
  :group 'pel-pkg-for-programming
  :link `(url-link :tag "Python PDF" ,(pel-pdf-file-url "pl-python")))

(defcustom pel-use-python  nil
  "Control whether PEL supports Python development."
  :group 'pel-pkg-for-python
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-python-tab-width 4
  "Distance between tab stop for buffers in `python-mode'.
PEL stores this in `tab-width' when opening Python buffers.
This does *NOT* control the indentation in Python files,
only for commands that mode point to tab stop positions
such as `tab-to-tab-stop', and the display of hard TAB characters."
  :group 'pel-pkg-for-python
  :type 'integer
  :safe 'pel-indent-valid-p)

(defcustom pel-use-lpy nil
  "Control whether PEL supports lpy package.
The lpy package provides lispy-style modal editing for Python.
Note: `pel-use-python' must be t for this to be effective."
  :group 'pel-pkg-for-python
  :type 'boolean
  :safe #'booleanp
  :link `(url-link :tag "lpy @ GitHub"
                   "https://github.com/abo-abo/lpy"))

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; REXX Support
;; ------------
(defgroup pel-pkg-for-rexx nil
  "PEL customization for REXX Programming language."
  :group 'pel-pkg-for-programming
  :link `(url-link :tag "REXX PDF" ,(pel-pdf-file-url "pl-rexx")))

(defcustom pel-use-rexx nil
  "Control whether PEL support REXX development."
  :group 'pel-pkg-for-rexx
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

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; V Language Support
;; ------------------
(defgroup pel-pkg-for-v nil
  "Pel V language support. Experimental."
  :group 'pel-pkg-for-programming
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

;; -----------------------------------------------------------------------------
;; Project Manager Support
;; =======================
(defgroup pel-pkg-for-project-mng nil
  "PEL customization for project managers."
  :group 'pel-package-use
  :link `(url-link :tag "Projectile PDF" ,(pel-pdf-file-url "projectile")))

(defcustom pel-use-projectile nil
  "Control whether PEL supports the projectile project manager."
  :group 'pel-pkg-for-project-mng
  :type '(choice
          (const :tag "Do not use" nil)
          (const :tag "Use, activate later by command"  t)
          (const :tag "Use, activate when Emacs starts" use-from-start)))

;; -----------------------------------------------------------------------------
;; pel-pkg-for-regexp
;; ------------------
(defgroup pel-pkg-for-regexp nil
  "List of external packages that PEL can use for regular expressions."
  :group 'pel-package-use
  :link `(url-link :tag "Search/Replace PDF" ,(pel-pdf-file-url "search-replace")))

(defcustom pel-bind-keys-for-regexp nil
  "If set to t, PEL binds several keys in the C-c prefix.
It binds:
- 'C-c r' : to replace-regexp or pel-replace-regexp
- 'C-c q' : to query-replace-regexp or pel-query-replace-regexp"
  :group 'pel-pkg-for-regexp
  :type 'boolean
  :safe #'booleanp)


(defcustom pel-initial-regexp-engine 'emacs
  "Select the search/replace regexp engine used when Emacs starts.
Select one that is available according to the package installed.
The possible choices are:
- 'emacs             : plain emacs
- 'pel-vr            : visual-regexp
- 'pel-vr/emacs      : visual-regexp-steroids emacs
- 'pel-vr/emacs-plain: visual-regexp-steroids emacs-plain
- 'pel-vr/pcre2el    : visual-regexp-steroids pcre2el
- 'pel-vr/python     : visual-regexp-steroids python
- 'pel-vr/custom     : visual-regexp-steroids custom

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
  :link `(url-link :tag "regex-tool @ GitHub"
                   "https://github.com/jwiegley/regex-tool")
  :group 'pel-pkg-for-regexp
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-use-pcre2el nil
  "Control whether PEL uses the external pcre2el library."
  :link `(url-link :tag "pcre2el @ GitHub" "https://github.com/joddie/pcre2el")
  :group 'pel-pkg-for-regexp
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-use-visual-regexp nil
  "Control whether PEL uses the external visual-regexp library."
  :link `(url-link :tag "visual-regexp @ GitHub" "https://github.com/benma/visual-regexp.el")
  :group 'pel-pkg-for-regexp
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-use-visual-regexp-steroids nil
  "Control whether PEL uses the external visual-regexp-steroids library."
  :link `(url-link :tag "visual-regexp-steroids @ GitHub"
                   "https://github.com/benma/visual-regexp-steroids.el")
  :group 'pel-pkg-for-regexp
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-use-xr nil
  "Control whether PEL uses the external xr library."
  :link `(url-link :tag "xr @ elpa" "https://elpa.gnu.org/packages/xr.html")
  :group 'pel-pkg-for-regexp
  :type 'boolean
  :safe #'booleanp)

;; -----------------------------------------------------------------------------
;; Region / Selection Management
;; -----------------------------
(defgroup pel-pkg-for-region nil
  "List of external packages that PEL can use to help deal with regions."
  :group 'pel-package-use
  :link `(url-link :tag "Marking PDF" ,(pel-pdf-file-url "marking"))
  :link `(url-link :tag "Emacs Lisp PDF" ,(pel-pdf-file-url "pl-emacs-lisp"))
  :link `(url-link :tag "Common Lisp PDF" ,(pel-pdf-file-url "pl-common-lisp")))

(defcustom pel-use-expand-region nil
  "Control whether PEL uses the expand-region package."
  :group 'pel-pkg-for-region
  :type 'boolean
  :safe #'booleanp)

;; ----------------------------------------------------------------------------
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

;; -----------------------------------------------------------------------------
;; pel-pkg-for-search
;; ------------------
(defgroup pel-pkg-for-search nil
  "List of external packages that PEL can use for searching text."
  :group 'pel-package-use
  :link `(url-link :tag "Search/Replace PDF" ,(pel-pdf-file-url "search-replace")))

(defcustom pel-search-from-top-in-other nil
  "Force function `pel-search-word-from-top' search in other of 2 windows.

If set to t, the function `pel-search-word-from-top' search in
the other window if there are only 2 non-dedicated window by
default. To force it to search in the current buffer the numeric
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
  "Control whether PEL uses the Anzu.
See: URL `https://melpa.org/#/anzu'"
  :group 'pel-pkg-for-search
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-use-swiper nil
  "Control whether PEL uses the Swiper search package.
See: URL `https://github.com/abo-abo/swiper#swiper'"
  :group 'pel-pkg-for-search
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-initial-search-tool nil
  "Select the search tool used when Emacs starts.

PEL supports the following tools:
- nil      : use Emacs default
- `anzu'   : use Anzu globally to display search match counts in modeline.
- `swiper' : use Swiper to display search mathes list in minibuffer."
  :group 'pel-pkg-for-search
  :type '(choice
          (const :tag "Use Emacs default" nil)
          (const :tag "Use Anzu" anzu)
          (const :tag "Use Swiper" swiper)))

;; ----------------------------------------------------------------------------
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

;; ----------------------------------------------------------------------------
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

(defcustom pel-prefer-sr-speedbar-in-terminal t
  "Prefer using Sr-Speedbar in terminal mode (when available) over Speedbar."
  :group 'pel-pkg-for-speedbar
  :type  'boolean
  :safe  #'booleanp
  :link '(url-link :tab "SR-Speedbar @ EmacsWiki"
                   "https://www.emacswiki.org/emacs/SrSpeedbar"))

;; -----------------------------------------------------------------------------
;; Shell & Terminal Support
;; ------------------------
(defgroup pel-pkg-for-shells nil
  "List of external packages that PEL can use to support shells and terminals."
  :group 'pel-package-use
  :link `(url-link :tag "Shells PDF" ,(pel-pdf-file-url "shells")))

(defcustom pel-use-vterm nil
  "Control whether the vterm shell is available.
The vterm package used the libvterm library to provide a very fast
and usable shell for Emacs."
  :group 'pel-pkg-for-shells
  :type 'boolean
  :safe #'booleanp)

;; -----------------------------------------------------------------------------
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
  :link `(url-link :tag "Spell Checking PDF" ,(pel-pdf-file-url "spell-checking")))

(defcustom pel-spell-check-tool nil
  "Spell Checking tool and local dictionary.
The spell-checking tool must be an ispell-compatible
command line tool.  This includes:

- ispell
- aspell
- hunspell
- enchant

The second line identifies the file where the local dictionary
will be stored.  Often stored in ~/.emacs.d/.ispell"
  :group 'pel-pkg-for-spelling
  :type '(choice
          (const :tag "No spell check" nil)
          (string :tag "ispell-compatible program name")))

(defcustom  pel-spell-check-personal-dictionary nil
  "Location of you personal dictionary file.

If not specified, the location selected is \"~/.ispell\".

However the recommend location is: \"~/.emacs.d/.ispell\" if your init.el
file is located in the \"~/.emacs.d\" directory."
  :group 'pel-pkg-for-spelling
  :type '(choice
          (const  :tag "Use default: ~/.ispell" nil)
          (string :tag "Use specified file")))

(defcustom pel-modes-activating-flyspell-mode
  '(log-edit-mode
    markdown-mode
    org-mode
    rst-mode
    vc-git-log-edit-mode)
  "List of major modes that automatically activate `flyspell-mode'.
To activate the changes for this you must 'Apply and Save' and restart Emacs."
  :group 'pel-pkg-for-spelling
  :type
  '(repeat symbol))

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
To activate the changes for this you must 'Apply and Save' and restart Emacs."
  :group 'pel-pkg-for-spelling
  :type
  '(repeat symbol))

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
    lisp-mode)
  "List of major modes that automatically activate the `superword-mode'."
  :group 'pel-pkg-for-text-mode
  :type '(repeat symbol))

(defcustom pel-modes-activating-subword-mode nil
  "List of major modes that automatically activate the `subword-mode'."
  :group 'pel-pkg-for-text-mode
  :type '(repeat symbol))

;; -----------------------------------------------------------------------------
;; Undo Mechanism Management
;; -------------------------
(defgroup pel-pkg-for-undo nil
  "List of external packages that PEL can use to control the undo mechanisms."
  :group 'pel-package-use
  :link `(url-link :tag "Undo/Redo/Repeat PDF" ,(pel-pdf-file-url "undo-redo-repeat")))

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
;; Version Control System Support
;; ------------------------------
(defgroup pel-pkg-for-vcs nil
  "List of external packages that PEL can use to support use of (D)VCS."
  :group 'pel-package-use
  :link `(url-link :tag "Mercurial PDF" ,(pel-pdf-file-url "vcs-mercurial")))

(defcustom pel-use-magit nil
  "Control whether PEL provides access to the Magit package."
  :group 'pel-pkg-for-vcs
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-use-monky nil
  "Control whether PEL provides access to the Monky package."
  :group 'pel-pkg-for-vcs
  :type 'boolean
  :safe #'booleanp)

;; -----------------------------------------------------------------------------
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


(defcustom pel-windmove-on-esc-cursor t
  "Control whether the Esc-cursor keys are bound to windmove commands.

When set to t, PEL activates the following four key bindings:

- ESC <up>    : windmove-up
- ESC <down>  : windmove-down
- ESC <right> : windmove-right
- ESC <left>  : windmove-left

If it set to nil, these keys are not bound.
When using Org-mode often it's probably best to set this off (nil)."
  :group 'pel-pkg-for-window
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-windmove-on-f1-cursor t
  "Control whether the F1-cursor keys are bound to windmove commands.

When set to t, PEL activates the following four key bindings:

- <f1> <up>    : windmove-up
- <f1> <down>  : windmove-down
- <f1> <right> : windmove-right
- <f1> <left>  : windmove-left

If it set to nil, these keys are not bound."
  :group 'pel-pkg-for-window
  :type 'boolean
  :safe #'booleanp)

;; -----------------------------------------------------------------------------
(provide 'pel--options)

;;; pel--options.el ends here

; LocalWords:  cscope xcscope CScope

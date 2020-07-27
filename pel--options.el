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
;;     - pel-pkg-for-cursor
;;     - pel-pkg-for-dired
;;     - pel-pkg-for-filemng
;;       - pel-pkg-for-ztree
;;     - pel-pkg-for-grep
;;     - pel-pkg-for-key-chord
;;     - pel-pkg-for-modeline
;;     - pel-pkg-for-regexp
;;     - pel-pkg-for-search
;;     - pel-pkg-for-tags
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
;;     - pel-pkg-for-project-mng
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
;;       - pel-pkg-for-drawing-markup
;;         - pel-pkg-for-plantuml
;;         - pel-pkg-for-graphviz-dot
;;     - pel-pkg-for-spelling
;;
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
;; pel-pkg-for-cursor
;; ------------------
(defgroup pel-pkg-for-cursor nil
  "List of external packages for cursor management that may be used with PEL."
  :group 'pel-package-use)

(defface pel-cursor-overwrite-mode-color
  '((((background light)) :background "black")
    (((background dark))  :background "white"))
  "Cursor face of cursor in overwrite-mode.
Takes effects only when Emacs runs in graphics mode."
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
  :group 'pel-pkg-for-cursor
  :group 'cursor
  :type '(choice
          (const :tag "No change - use default cursor type" nil)
          (const :tag "bar" bar)
          (const :tag "box" box)
          (const :tag "hollow" hollow)))

(defcustom pel-use-multiple-cursors nil
  "Control whether PEL uses the multiple cursors package.
See URL `https://github.com/magnars/multiple-cursors.el'."
  :group 'pel-pkg-for-cursor
  :type 'boolean
  :safe #'booleanp)

;; -----------------------------------------------------------------------------
;; pel-pkg-for-dired
;; -----------------
(defgroup pel-pkg-for-dired nil
  "List of packages activated for Dired support that may be used with PEL."
    :group 'pel-package-use)

(defcustom pel-use-dired-x nil
  "Control whether PEL activates the Dired-X features in `dired-mode'."
  :group 'pel-pkg-for-dired
  :type 'boolean
  :safe #'booleanp)

;; -----------------------------------------------------------------------------
;; pel-pkg-for-filemng
;; -------------------
(defgroup pel-pkg-for-filemng nil
  "List of external packages that can be used to manage file/directory."
  :group 'pel-package-use)

(defcustom pel-use-ffap    nil
  "Control whether PEL activates ffap bindings.
3 available choices:
- 1:  nil :=  ffap is not used.
- 2:  t   :=  use pel:ffap bindings, keeping default Emacs bindings for
              C-x C-f and other intact.
- 3:  ffap-bindings := Use the bindings documented by ffap.el by executing
      (ffap-bindings).  This replaces the bindings several file finding
      commands and cannot be undone until this is changed and Emacs is
      re-started."
  :group 'pel-pkg-for-filemng
  :type '(choice
          (const :tag "Do not use" nil)
          (const :tag "Use pel:ffap bindings" t)
          (const :tag "Activate standard ffap bindings" ffap-bindings)))

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
;; pel-pkg-for-grep
;; ----------------
(defgroup pel-pkg-for-grep nil
  "List of external packages that PEL can use for grep operations."
  :group 'pel-package-use)

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
it does not uses `rg`.

References:
- ripgrep: URL `https://github.com/BurntSushi/ripgrep'
- Emacs rg  package: URL `https://melpa.org/#/rg'
- Emacs ripgrep package: https://github.com/nlamirault/ripgrep.el"
  :group 'pel-pkg-for-grep
  :type 'boolean
  :safe #'booleanp)

;; -----------------------------------------------------------------------------
;; pel-pkg-for-key-chord
;; ---------------------
(defgroup pel-pkg-for-key-chord nil
  "PEL support for key-chords."
  :group 'pel-package-use)

(defcustom pel-use-key-chord nil
  "Control whether PEL uses the key-chord external package.
With it, it's possible to activate binding actions to two keys
pressed simultaneously or a single key quicly pressed twice.
See URL https://github.com/emacsorphanage/key-chord/blob/master/key-chord.el
This can be set to:
- 0: nil: Do not use.  key-chord is not required nor loaded.
- 1: t: Use, activate by command.  key-chord loaded when the function
        `key-chord-mode' is executed.  Not before.
- 2: use-from-start:  Use, load and activate  1 second after Emacs starts."
  :group 'pel-pkg-for-key-chord
  :type '(choice
          (const :tag "Do not use" nil)
          (const :tag "Use, activate later by command"  t)
          (const :tag "Use, activate when Emacs starts" use-from-start)))

(defcustom pel-use-key-seq nil
  "Control whether PEL key-chord is also using key-seq.
If t, the boolean field 'key-seq' key-chords definitions in
`pel-key-chords' is honoured: instead of declaring hem a
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

    (global    ""         key-chord
               "fg" (lambda ()
                      (interactive)
                      (if (require 'windmove nil :noerror)
                          (windmove-left)
                        (insert "fg"))))

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
    - A major mode name that identifies the major mode
      where the key-chord must be activated.
      For example:  c++-mode
  - The second is a string identifying the Emacs Lisp file
    that provides the major mode identified in the first item.
    This is empty when the first item is set to global.
    It is required when the first item identifies a major mode.
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
;; pel-pkg-for-modeline
;; --------------------
(defgroup pel-pkg-for-modeline nil
  "List of external packages that PEL can use to modify the modeline."
  :group 'pel-package-use)

(defcustom pel-use-keycast nil
  "Control whether the keycast package is made available.
For more info, see URL https://github.com/tarsius/keycast"
  :group 'pel-pkg-for-modeline
  :type 'boolean
  :safe #'booleanp)

;; -----------------------------------------------------------------------------
;; pel-pkg-for-regexp
;; ------------------
(defgroup pel-pkg-for-regexp nil
  "List of external packages that PEL can use for regular expressions."
  :group 'pel-package-use)

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
  "Control whether PEL uses the external `regex-tool' library.
See URL `https://github.com/jwiegley/regex-tool'."
  :group 'pel-pkg-for-regexp
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-use-pcre2el nil
  "Control whether PEL uses the external pcre2el library.
See URL `https://github.com/joddie/pcre2el'."
  :group 'pel-pkg-for-regexp
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-use-visual-regexp nil
  "Control whether PEL uses the external visual-regexp library.
See URL `https://github.com/benma/visual-regexp.el'."
  :group 'pel-pkg-for-regexp
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-use-visual-regexp-steroids nil
  "Control whether PEL uses the external visual-regexp-steroids library.
See URL `https://github.com/benma/visual-regexp-steroids.el'."
  :group 'pel-pkg-for-regexp
  :type 'boolean
  :safe #'booleanp)

(defcustom pel-use-xr nil
  "Control whether PEL uses the external xr library.
See URL `https://elpa.gnu.org/packages/xr.html'."
  :group 'pel-pkg-for-regexp
  :type 'boolean
  :safe #'booleanp)

;; -----------------------------------------------------------------------------
;; pel-pkg-for-search
;; ------------------
(defgroup pel-pkg-for-search nil
  "List of external packages that PEL can use for searching text."
  :group 'pel-package-use)

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

;; -----------------------------------------------------------------------------
;; pel-pkg-for-tags
;; ----------------
(defgroup pel-pkg-for-tags nil
  "List of external packages that PEL can use to manage Tags cross-references."
  :group 'pel-pkg-for-tags)

(defcustom pel-use-ggtags nil
  "Control whether PEL uses the ggtags package.
See URL https://github.com/leoliu/ggtags"
  :group 'pel-pkg-for-tags
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
;; pel-pkg-for-speedbar
;; --------------------
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
;; pel-pkg-for-session
;; -------------------
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

(defconst pel-USE-IDO     1 "Bitmask identifying Ido.      DON'T CHANGE!")
(defconst pel-USE-IVY     2 "Bitmask identifying Ivy.      DON'T CHANGE!")
(defconst pel-USE-COUNSEL 4 "Bitmask identifying Counsel.  DON'T CHANGE!")
(defconst pel-USE-HELM    8 "Bitmask identifying Helm.     DON'T CHANGE!")

(defcustom pel-initial-completion-mode nil
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
  "List of major modes that automatically activate smart-dash-mode.
Used when `pel-use-smart-dash' user option is t.
To activate the changes for this you must 'Apply and Save' and restart Emacs."
  :group 'pel-pkg-for-spelling
  :type
  '(repeat symbol))
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
;; Project Manager Support
;; =======================
(defgroup pel-pkg-for-project-mng nil
  "PEL customization for project managers."
    :group 'pel-package-use)

(defcustom pel-use-projectile nil
  "Control whether PEL supports the projectile project manager."
  :group 'pel-pkg-for-project-mng
  :type '(choice
          (const :tag "Do not use" nil)
          (const :tag "Use, activate later by command"  t)
          (const :tag "Use, activate when Emacs starts" use-from-start)))

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
  :group 'pel-pkg-for-beam-vm)

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
  "Name of environment variable used to identify the active Erlang version
when `pel-erlang-version' is nil."
  :group 'pel-pkg-for-erlang
  :type 'string)

(defcustom pel-erlang-shell-prevent-echo nil
  "Set to t if the erlang-shell-mode shell echoes back commands.
When set to t PEL activtaes code that prevent echo of the typed commands."
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

;; -----------------------------------------------------------------------------
;; pel-pkg-for-draw-markup
;; -----------------------
(defgroup pel-pkg-for-drawing-markup nil
  "PEL drawing markup support."
  :group 'pel-package-for-markup)

(defgroup pel-pkg-for-plantuml nil
  "PEL UML support."
  :group 'pel-pkg-for-drawing-markup)

(defcustom pel-use-plantuml nil
  "Control whether PEL activates support for PlantUML to draw UML diagrams.
This uses the `plantuml-mode' package.
See URL https://github.com/skuro/plantuml-mode.

The `plantuml-mode' can be used locally, using a local PlantUML
Java application (plantuml.jar).  You can also use PlantUML web
server and if you do not mind sending your information across the
internet.

To use PlantUML locally you must have Java installed on your
system and have the PlantUML Java application installed and
its plantuml.jar file must be accessible.

See URL https://plantuml.com
Also see general info at URL https://en.wikipedia.org/wiki/PlantUML

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
;; GraphViz-Dot Support
;; --------------------
(defgroup pel-pkg-for-graphviz-dot nil
  "PEL Graphviz-DOT support."
  :group 'pel-pkg-for-drawing-markup)

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
;; Spelling Support
;; ----------------
(defgroup pel-pkg-for-spelling nil
  "PEL Spelling Support."
  :group 'pel-package-use)

(defcustom pel-modes-activating-flyspell-mode
  '(log-edit-mode
    markdown-mode
    org-mode
    rst-mode
    vc-git-log-edit-mode)
  "List of major modes that automatically activate flyspell-mode.
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
  "List of major modes that automatically activate flyspell-prog-mode.
To activate the changes for this you must 'Apply and Save' and restart Emacs."
  :group 'pel-pkg-for-spelling
  :type
  '(repeat symbol))

;; -----------------------------------------------------------------------------
(provide 'pel--options)

;;; pel--options.el ends here

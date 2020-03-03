==============================================
pel -- Pragmatic Environment Library for Emacs
==============================================

.. contents::
.. sectnum::


Overview
========

PEL is an Emacs Lisp package that provides access to a set of small convenience
features but also provides easy access to features implemented by several other
great Emacs packages, as long as they are activated via the mechanism described
in the `PEL Customization`_ section.

In its current form, PEL does not implement any minor or major Emacs mode.
Instead it provides a set of functions that can be accessed globally, with some
of them specialized for some modes and other available everywhere.

PEL defines a large set of key bindings which are mostly extensions to what
is available with standard GNU Emacs.
The PEL key bindings mostly do not conflict with standard GNU Emacs key bindings
except for a few exceptions, identified in the `Key Binding Documentation`_ tables.
As of this version, the PEL key bindings mostly use function prefix keys as
described in the `PEL Key Bindings`_ section.

PEL  integrates with a set of third party Emacs packages
(see the list in the `Credits`_ section below) and provides extra key bindings
to use the feature of those packages, sometime through extension functions
provided by PEL code.
In several cases PEL provides the logic to install these third party Emacs
packages, the logic to configure them and the logic to load them as lazily
as possible to reduce the Emacs initialization start time to a minimum.

The use of PEL features and PEL uses of other third party Emacs packages is
controlled by the `PEL customization`_.  By default, no third party package not
already included in the standard GNU Emacs distribution is installed or used.
To get PEL to use them and provide the functionality (along with their published
key bindings) you must first activate them via the `PEL Customization`_
mechanism.

The reason for PEL
------------------

PEL attempts to make Emacs easier for new users by providing already made
configuration that is controlled by Emacs customization system.  It reduces the
need for writing Emacs Lisp configuration code for the packages it supports.

Emacs supports a number of great packages. Some are easy to install, others
require more knowledge, knowledge that is often not readily available to new
users and will require a time investment you may not be willing to make.

Instead of having to write Emacs Lisp code inside an Emacs init file for each
new package you want to use, you'd use PEL, select the features you want
via `PEL Customization`_ and then execute ``pel-init`` to activate what you want
to use.  PEL contains the logic for configuring the packages it supports.  In
some cases it also contains the logic to install the package if it is not
already installed.

This is an early version of PEL. It will grow over time and will support more
packages. PEL essentially came out as a desire to be able to use an Emacs
configuration on several systems, both in terminal (TTY) mode and in Graphics
mode while trying to keep  Emacs initialization as fast as possible and reducing
the repetitive writing of package initialization code.

I started writing PEL while learning Emacs, Emacs Lisp and the amazing packages
that have been written for Emacs.  PEL encapsulates the knowledge about various
tweaks in the use and configuration of several built-in Emacs features and
several of the third party packages.

While learning Emacs and various packages I created a set of tables
that each list and describe a specific topic, the commands and key bindings
related to that topic.
There are several topics; Emacs navigation, Emacs
buffers, windows and frames, how to undo, redo, work with Emacs Lisp, etc...
See the `Key Binding Documentation`_ section.
The commands and key bindings described in those table include what is provided
by the plain vanilla GNU Emacs but also the bindings PEL adds and
the bindings provided by the third party packages that PEL integrates.


PEL Goals
---------

- Ease introduction to Emacs.
- Keep as many standard Emacs key bindings as possible.
- Provide easy to remember key bindings via a key binding tree, key prefixes and
  the use of key choice visualization with package such as which-key_, especially
  for commands that are seldom used.
- Minimize the amount of Emacs Lisp code to write inside Emacs init file to
  support various external Emacs packages.

  - Provide all logic necessary to install and configure external Emacs packages.

- Minimize Emacs initialization time even when a large number of packages are
  present on the computer.
- Document what's available: the key bindings, the special considerations, the
  documents that should be read to deepen user's understanding.
- Allow use of PEL even when someone has an extensive Emacs init file.
- Add support for several programming languages integrating many packages that
  support these programming languages.  Support for C, C++, Rust, Go,
  Python, Erlang, Elixir, Haskell, OCaml and several are planned
  (but... no schedule yet!).


Essentially, PEL is my first Emacs Lisp project.  I wrote it while learning
Emacs.  I keep using the documentation whenever I forgot the key bindings
of one of the many Emacs features.  There are still a lot of things to do to add
support for external packages and increase the customization. And even more to
transform the documentation format (see the `PDF Documentation`_ section for
that.)


Using Portions of PEL Manually
------------------------------

There's another, manual way to use portions of PEL.
PEL code is split across several files.
Its keymap and installation logic is located inside the `pel.el`_ file
exclusively.
If you only want to use the feature of one or several other files, then simply
use them and never call ``pel-init``.
You can then create any key binding that you wish by writing your own
initialization code.

..
   -----------------------------------------------------------------------------


How to Setup PEL
================

Unfortunately *some* Emacs Lisp code must be written to your
`Emacs initialization file`_.

**Configure How to Download Packages**

PEL uses
ELPA_ (GNU Emacs Lisp Package Archive)
and MELPA_ (Milkypostman's Emacs Lisp Package Archive)
sites to download and install packages.

To activate their use, place the following code inside your Emacs init file if
it is not already present:

.. code:: elisp

          (when (>= emacs-major-version 24)
            (require 'package)
            (setq package-enable-at-startup nil)
            (if (version=  emacs-version "26.2")
                (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

            (let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                                (not (gnutls-available-p))))
                   (proto (if no-ssl "http" "https")))
              (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)

              (when (< emacs-major-version 24)
                ;; For important compatibility libraries like cl-lib
                (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))

            (package-initialize))

**Select the location of Emacs Persistent Customization Data**

By default, Emacs stores its persistent customization data inside your Emacs
init file.  If you want to store it somewhere else, you to add something like
the following code, which places it inside the file ``~/.emacs-custom.el``:

.. code:: elisp

          (setq custom-file "~/.emacs-custom.el")
          (load custom-file)

**To start PEL when Emacs Starts**

If you want PEL to be available right after Emacs starts, write the following
inside your Emacs init file:

.. code:: elisp

          (require 'pel)
          (pel-init)

If you do not want PEL to start when Emacs start, then you don't need the above
code. To use PEL later simply execute the **pel-init** command by typing:
``M-x pel-init``


**To identify the location of your Ispell local dictionary**

With the current version of PEL, when you want to select the spell check
program used by
Ispell or Flyspell and the location of your personnal dictionary you need to
write Emacs Lisp code in your Emacs init file that calls the pel-spell-init
function.

The following is an example. It selects the ``aspell`` program
and identifies the path for the personal dictionary.

.. code:: elisp

          (eval-after-load "ispell"
            '(pel-spell-init “aspell" "~/.emacs.d/.ispell"))

In future versions of PEL, this code will not be necessary; the spell check
selection, optional path to it and path to the personal dictionary will be
selected via PEL customization.

..
   -----------------------------------------------------------------------------


PEL Key Bindings
================

PEL key bindings are mostly use function key prefixes.
It currently uses the **F2**, **F6**, **F11** and **F12** keys as prefix keys.
It also binds **F5** as the repeat key.
In this version these bindings are hard-coded.

**Note:**
         Future version of PEL will allow customization of the prefix keys and the
         ability to control whether **F5** is bound by PEL.

The best way to quickly see the list of PEL prefix key is right inside Emacs.
Type the prefix key (like **F11**) and then quickly type
either **C-h** or **F1**.
Emacs will open a ``*help*`` buffer that lists all keys available.  You can
navigate this buffer and follow the links to the described commands. To get the
list of the keys for a sub-prefix type it and again follow with
either **C-h** or **F1**.

The following table lists the **F11** keymap as an example.
As described in the `Naming Conventions`_ section the names in the binding
column that use the "pel:" prefix are sub key-maps.
The commands use the prefix "pel-".
As you can see some of the commands are accessible right after the **F11**
prefix, but there's a large number of sub-prefix following.
The keymap names were chosen to be as descriptive as possible and use keys that
mnemonically associate to the related concept if at all possible.

=============================== ===========================================
key                             binding
=============================== ===========================================
``<f11> TAB``                   pel:indent
``<f11> SPC``                   Prefix Command
``<f11> #``                     pel-toggle-mac-numlock
``<f11> $``                     pel:spell
``<f11> '``                     pel:bookMark
``<f11> +``                     pel-copy-marked-or-whole-line
``<f11> ,``                     pel:auto-completion
``<f11> -``                     pel:kill
``<f11> .``                     pel:mark
``<f11> 0``                     hl-line-mode
``<f11> ;``                     pel:comment
``<f11> =``                     pel:copy
``<f11> ?``                     pel:help
``<f11> C``                     pel:clipboard
``<f11> F``                     pel:frame
``<f11> S``                     pel:speedbar
``<f11> [``                     pel-cua-move-rectangle-left
``<f11> ]``                     pel-cua-move-rectangle-right
``<f11> a``                     pel:abbrev
``<f11> b``                     pel:buffer
``<f11> c``                     pel:count
``<f11> d``                     pel:draw
``<f11> f``                     pel:file
``<f11> g``                     pel:grep
``<f11> i``                     pel:insert
``<f11> k``                     pel:kbmacro
``<f11> l``                     pel:linectrl
``<f11> o``                     pel:order
``<f11> r``                     pel:register
``<f11> s``                     pel:search-replace
``<f11> t``                     pel:text
``<f11> u``                     pel:undo
``<f11> w``                     pel:window
``<f11> x``                     pel:eXecute
``<f11> y``                     yank-pop
``<f11> |``                     pel-toggle-dual-scroll
``<f11> <C-S-down>``            pel-close-window-down
``<f11> <C-S-left>``            pel-close-window-left
``<f11> <C-S-right>``           pel-close-window-right
``<f11> <C-S-up>``              pel-close-window-up
``<f11> <C-down>``              pel-create-window-down
``<f11> <C-left>``              pel-create-window-left
``<f11> <C-right>``             pel-create-window-right
``<f11> <C-up>``                pel-create-window-up
``<f11> <M-left>``              pel-backward-syntaxchange-start
``<f11> <M-right>``             pel-forward-syntaxchange-start
``<f11> <C-f10>``               menu-bar-mode
``<f11> <down>``                windmove-down
``<f11> <f10>``                 pel:menu
``<f11> <f11>``                 pel-toggle-frame-fullscreen
``<f11> <f12>``                 xterm-mouse-mode
``<f11> <left>``                windmove-left
``<f11> <right>``               windmove-right
``<f11> <up>``                  windmove-up
=============================== ===========================================

PEL Mode Sensitive Key-maps
---------------------------

In the above table,
the ``<f11> SPC`` is a special case. It's the top key-map of all mode sensitive
key-maps.
PEL uses the **F12** as the key prefix for a keymap that contains
commands for the major mode of the current buffer.

For example, when the current buffer is using the rst-mode for editing
reStructuredText files,
the **F12** key has the following bindings.

=============================== ===========================================
key                             binding
=============================== ===========================================
``<f12> .``                     pel-rst-makelink
``<f12> g``                     pel-rst-goto-ref-bookmark
``<f12> s``                     pel-rst-set-ref-bookmark
=============================== ===========================================

However, when the current buffer uses Emacs-Lisp mode for working on Emacs Lisp
code,
the **F12** key has the following, different bindings.

=============================== ===========================================
key                             binding
=============================== ===========================================
``<f12> .``                     pel-find-thing-at-point
``<f12> D``                     toggle-debug-on-error
``<f12> a``                     pel:elisp-analyze
``<f12> c``                     pel:elisp-compile
``<f12> d``                     pel:elisp-debug
``<f12> e``                     pel:elisp-eval
``<f12> f``                     pel:elisp-function
``<f12> i``                     parinfer-auto-fix
``<f12> l``                     pel:elisp-lib
``<f12> m``                     pel:elisp-mode
=============================== ===========================================

If you edit a reStructuredText file and want to use one of the commands
available in the Emacs-Lisp key-map, then you can use the longer PEL key-map
that uses the ``<f11> SPC`` prefix.
The following table shows that for Emacs Lisp (abbreviated "elisp") you'd type
``<f11> SPC l`` to get to the same key-map that ``<f12>`` provides when you're
already using the Emacs-Lisp major mode.

=============================== ===========================================
key                             binding
=============================== ===========================================
``<f11> SPC C``                 pel:for-C++
``<f11> SPC L``                 pel:for-lisp
``<f11> SPC c``                 pel:for-C
``<f11> SPC g``                 pel:for-graphviz-dot
``<f11> SPC l``                 pel:for-elisp
``<f11> SPC p``                 pel:for-python
``<f11> SPC r``                 pel:for-reST
=============================== ===========================================

This is a very early version of PEL.
Support for programming and markup languages is currently very sparse.
More to come.

Key Binding Documentation
-------------------------

PEL comes with a set of tables listing and describing both the standard Emacs
commands and key bindings for a given type of activity along with the extra
commands provided by PEL.
These tables are inside PDF documents.
See the `PDF Documentation`_ section for more info on why PDF files were used.
The tables have a format that is something between a quick sheet format and
a full blown manual.

Each PDF file holds a table that list commands related to a specific topic and
holds overview above a list of rows on:

#. the command name with several hyperlinks to the related section of the
   GNU Emacs manuals or other rappropriate resource
#. the key bindings for that command including:

   - the standard Emacs key bindings
   - the bindings for integrated packages
   - the bindings specific to PEL

#. the Emacs Lisp function form for the command, with the function name in
   bold and the arguments in Emacs help style
#. A description of the command, with lots of the text taken directly from
   Emacs help for what relates to the interactive use of the function but also
   with extra notes and references.

Several of these documents also a list of reference table listing relevant topics.
These references include hyperlinks to the relevant GNU
Emacs manuals but also to several sites devoted to Emacs including several
demonstration videos hosted on various platforms.

The tables are heavily marked up using colors and icons (actually Unicode
character symbols) to represent various concepts. For example key bindings that
do not work when Emacs is running in terminal (TTY) mode are displayed in
orange, commands that require external Emacs package are show in blue and use a the
package character (📦), etc...  The full list of conventions are listed in the
`Document Legend`_ table.  The list of tables follow below.
This is the very first release of PEL.
As PEL evolves, it will cover more topics, more
programming languages, major modes and will integrate with more of the external
Emacs packages and more tables will describe how to use them.

- `Document Legend`_
- Emacs base operations

  - `Abbreviations`_
  - `Align`_
  - `Bookmarks`_
  - `Buffers`_
  - `Case Conversion`_
  - `Closing and Suspending`_
  - `Comments`_
  - `Counting`_
  - `Cut, Delete, Copy and Paste`_ (killing and yanking)
  - `Display Lines`_
  - `Enriched Text`_
  - `Faces and Fonts`_
  - `File Management`_
  - `File and Directory Local Variables`_
  - `Filling and Justification`_
  - `Frames`_
  - `Grep`_
  - `Help`_
  - `Highlight`_
  - `Hooks`_
  - `Indentation`_
  - `Input Method`_
  - `Inserting Text`_
  - `Keyboard Macros`_
  - `Marking`_
  - `Menus`_
  - Modes:

    - `Dired`_
    - `Graphviz Dot`_
    - `Org mode`_
    - `reStructuredText mode`_

  - `Modifier Keys`_
  - `Narrowing`_
  - `Navigation`_
  - `Packages`_
  - Programming Language Support:

    - `Common Lisp`_

    - `Emacs Lisp`_

      - `ERT`_ (Emacs Lisp Regression Testing system)

  - `Registers`_
  - `Scrolling`_
  - `Search and Replace`_
  - `Shells`_
  - `Sorting`_
  - `Speedbar`_
  - `Spell Checking`_
  - `Text-modes`_
  - `Transpose`_
  - `Undo, Redo, Repeat and Prefix Arguments`_
  - Version Control Systems:

    - `Mercurial`_

  - `Web`_
  - `Whitespaces`_
  - `Windows`_


PEL Customization
=================

PEL is heavily customizable using the `Emacs customization`_ facility.

To customize PEL:

#. Decide where you want to store the persistent customization information.

   - By default it is stored inside your Emacs init file.
     If this is good for you, then continue to step 3.
   - You may want to store it inside a separate file, to decouple it from your
     Emacs initialization if you use several environments or computers.
     For example if you want it stored inside ``~/.emacs-custom.el`` then
     place the following Emacs Lisp code inside your Emacs init file::

       (setq custom-file "~/.emacs-custom.el")
       (load custom-file)

#. Once the location of the customization information is identified and set start
   Emacs.
#. Execute the customize command by typing: ``M-x customize``
#. This will open the ``*Customize Apropos*`` buffer.
#. Inside that buffer, move point to the search field and
   search for the Pel group by typing ``Pel$`` inside the search
   field and hitting the Search button.
#. Emacs will show the *Pel Group*.

   - Currently, the *Pel group* has the following subgroups:

     - *Pel Identification*
     - *Pel Kbmacro*
     - *Pel Package Use*
     - *Pel Text Insert*

   To select the packages you want PEL to use select the *Pel Package Use*
   subgroup.
   This is the root of another set of subgroups, organized by topics.
   These define a set of customization variables that activate the features either
   provided by PEL code or provided by other packages which PEL uses.
   All of these variables have a name that begin with the ``pel-use-`` prefix.
   The list of these variables is available below in `Pel Use Variables`_.

#. Select the *Pel Package Use* subgroup, then the subgroup that interests you
   and activate the feature that you want to use by setting the corresponding
   ``pel-use-`` variable to **t**.
#. Save and apply you new settings.
#. Restart PEL by either executing ``M-x pel-init`` or by restarting Emacs and
   then executing ``M-x pel-init`` (unless it is already executed in you Emacs
   init file).








Pel Use Variables
-----------------

The following table contains the list of the ``pel-use-`` customize variables
currently available.

============================== =============================================================
Variable                       Purpose
============================== =============================================================
pel-use-ace-window             Activate and enable use of the `ace-window package`_ to
                               be able to navigate across windows easily.

                               - This package is not distributed with Emacs.
                               - The first time PEL is initialized after this is set,
                                 PEL takes advantage of `use-package`_ and attempts
                                 to install it from MELPA_ if it is not already installed.
                                 If you prefer to install it yourself, install it before
                                 setting this variable to ``t``.



pel-use-auto-complete          Activate and enable use of the `auto-complete package`_
                               which provides auto-completion while typing.

                               - This package is not distributed with Emacs.
                               - The first time PEL is initialized after this is set,
                                 PEL takes advantage of `use-package`_ and attempts
                                 to install it from MELPA_ if it is not already installed.
                               - If you prefer to install it yourself, install it before
                                 setting this variable to ``t``.


pel-use-bind-key               Activate and enable use of the `bind-key`_ package for some
                               PEL commands that use it.

                               - This package is not distributed with Emacs.
                               - It is, however installed when you install PEL because
                                 PEL depends on `use-package`_ which depends on `bind-key`_.


pel-use-bm                     Activates and enable use of the bm_ package, which provides
                               visible bookmarks.  When enabled, PEL provides some key
                               bindings for it.

                               - This package is not distributed with Emacs.
                               - The first time PEL is initialized after this is set,
                                 PEL takes advantage of `use-package`_ and attempts
                                 to install it from MELPA_ if it is not already installed.
                               - If you prefer to install it yourself, install it before
                                 setting this variable to ``t``.

pel-use-c-eldoc                Activates and enable use of the `c-eldoc`_ package which
                               provides helpful descriptions of the arguments to C functions
                               when editing a buffer in c-mode.  PEL sets the hook required
                               for this.

                               - This package is not distributed with Emacs.
                               - The first time PEL is initialized after this is set,
                                 PEL takes advantage of `use-package`_ and attempts
                                 to install it from MELPA_ if it is not already installed.
                               - If you prefer to install it yourself, install it before
                                 setting this variable to ``t``.

pel-use-cc-vars                Activates and enable use of the cc-vars standard Emacs
                               library for the cc mode.  PEL sets some values for C
                               development.

                               *Note*: support for this is underway.
                               More options to be documented once C development is described
                               in the PEL documentation.

pel-use-common-lisp            Activates and enable use of Common Lisp development within
                               Emacs using a Common Lisp system such as SBCL_  (Steel Bank
                               Common Lisp).

                               When activated PEL attempts to install the `slime package`_.

                               - This package is not distributed with Emacs.
                               - The first time PEL is initialized after this is set,
                                 PEL takes advantage of `use-package`_ and attempts
                                 to install it from MELPA_ if it is not already installed.
                               - If you prefer to install it yourself, install it before
                                 setting this variable to ``t``.
pel-use-company                .
pel-use-dired-narrow           .
pel-use-edts                   .
pel-use-eglot                  .
pel-use-eldoc-box              .
pel-use-erlang                 .
pel-use-erlang-flymake         .
pel-use-erlang-start           .
pel-use-esup                   .
pel-use-expand-region          .
pel-use-framemove              .
pel-use-free-keys              .
pel-use-goto-last-change       .
pel-use-graphviz-dot           .
pel-use-highlight-defined      .
pel-use-hippie-expand          .
pel-use-ido-mode               .
pel-use-lice                   .
pel-use-macrostep              .
pel-use-nhexl-mode             .
pel-use-org-mode               .
pel-use-parinfer               .
pel-use-popup-kill-ring        .
pel-use-python                 .
pel-use-rainbow-delimiters     .
pel-use-re-builder             .
pel-use-ripgrep                .
pel-use-rst-mode               .
pel-use-rust                   .
pel-use-speedbar               .
pel-use-undo-tree              .
pel-use-uniquify               .
pel-use-which-key              .
============================== =============================================================

.. References

.. _ace-window package:        https://melpa.org/#/ace-window
.. _auto-complete package:     https://melpa.org/#/auto-complete
.. _MELPA:                     https://melpa.org/
.. _use-package:               https://melpa.org/#/use-package
.. _bind-key:                  https://melpa.org/#/bind-key
.. _bm:                        https://melpa.org/#/bm
.. _c-eldoc:                   https://melpa.org/#/?q=c-eldoc
.. _SBCL:                      https://en.wikipedia.org/wiki/Steel_Bank_Common_Lisp
.. _slime:                     https://melpa.org/#/slime
.. _slime package:             https://melpa.org/#/slime

.. _Emacs customization:       https://www.gnu.org/software/emacs/manual/html_node/emacs/Customization.html#Customization
.. _Emacs initialization file: https://www.gnu.org/software/emacs/manual/html_node/emacs/Init-File.html
.. _ELPA:                      https://elpa.gnu.org
.. _which-key:                 https://melpa.org/#/which-key






Implementation Notes
====================

Emacs Lisp Files
----------------

PEL code is placed in several Emacs Lisp files.
The file `pel.el`_ defines all PEL key bindings required by customization and
the ``pel-init`` function.
The convenience features PEL provides are implemented in separate files.
These files are loaded only when their features are used.

For example the file `pel-navigate.el`_ provides extra navigation facilities
such as the use of multi-hit ``<home>`` and ``<end>`` keys similar to what is
available by editors in the Brief family (such as CRiSP) but also aware of Emacs
concepts such as text fields, `shift-key selection`_ and Emacs `mark and region`_.

It's possible to use part of PEL without using its key bindings.
Just use the files that contain the features you need and write your own key
bindings for them inside your Emacs init file.  Just don't call ``pel-init``.

PEL provides autoloading of the ``pel-init`` function using the Emacs standard
packaging mechanism, written inside the `pel-pkg.el`_ file.
This is the only function marked with the Emacs magic autoload comment.
All other functions use a different command used to build another, secondary
autoload scheduling stored inside the pel-autoload.el
(as opposed to pel-autoloads.el).
The ``pel-init`` function calls ``pel--autoload-init`` which set the
autoloading of the PEL functions.  the `pel-autoloads.el`_ and
`pel-autoload.el`_ form a 2-step autoloading mechanism for PEL.




.. _pel.el:               https://github.com/pierre-rouleau/pel/blob/master/pel.el
.. _pel-navigate.el:      https://github.com/pierre-rouleau/pel/blob/master/pel-navigate.el
.. _pel-pkg.el:           https://github.com/pierre-rouleau/pel/blob/master/pel-pkg.el
.. _pel-autoload.el:      https://github.com/pierre-rouleau/pel/blob/master/pel-autoload.el
.. _pel-autoloads.el:     https://github.com/pierre-rouleau/pel/blob/master/pel-autoloads.el
.. _shift-key selection:  https://www.gnu.org/software/emacs/manual/html_node/emacs/Shift-Selection.html#Shift-Selection
.. _mark and region:      https://www.gnu.org/software/emacs/manual/html_node/emacs/Mark.html#Mark


Naming Conventions
------------------

- All PEL "*public*" functions and variables have a name that start with the
  prefix "pel-".

  - This includes all PEL commands.

- All PEL "*private*" functions and variables have a name that start with the
  prefix "pel--".

  - Those are  meant to be used from with PEL code exclusively.

- All PEL customization variables that control whether PEL uses or provides a
  given feature have a name that starts with the prefix "pel-use-".

- Most PEL key-maps have a name.  All of those name start with the prefix "pel:".

  - Using named key-maps help shows the key prefix purpose when using
    `which-key`_ to display the available key following a prefix or typing
    ``C-h`` or ``<f1>`` after typing a prefix key to see the list of available
    keys and their meanings.


PDF Documentation
-----------------

The list of documentation files are currently published as PDF files.
Although this is not the best way since this is an output format file as opposed
to the source of the document, these files were created in PDF format because I
wanted to be able to pack a lot of information about Emacs as I was learning
it.  I considered using a markup language like markdown or reSTructuredText. The
latter is more powerful, and it would have been possible to generate tables with
some of the attributes of what I was able to generate but it would have most
probably needed its own web site to be able to completely control the CSS as
well as write extensions in Python for what I needed.  And I did not have time
for that.  I needed to concentrate on Emacs and jot down notes on something
that, at the beginning of my learning period, was *not* Emacs. So I compromised
and used the macOS Numbers program to create a large spreadsheet with multiple
tabs and tables.  I used that to generate the PDF files.

This is far from ideal. I know. And once PEL gets to the point where support for
several other programming languages is integrated, I might find ways to use a
markup language that might be flexible enough to generate the same kind of
output.

As an temporary work-around, I tried
to export the file to CSV or TSV (tab separated value).  That generates the text
but the hyperlinks are not part of the CSV/TSV output files.  I might consider
producing those files if there is any interest, but I'd prefer to be able to
publish the source of something that can generate the kind of output that's
available in those PDF files.

I am open to suggestions. And can provide the Numbers file on request.

..
   -----------------------------------------------------------------------------


.. _Document Legend:                          doc/pdf/-legend.pdf
.. _Abbreviations:                            doc/pdf/abbreviations.pdf
.. _Align:                                    doc/pdf/align.pdf
.. _Bookmarks:                                doc/pdf/bookmarks.pdf
.. _Buffers:                                  doc/pdf/buffers.pdf
.. _Case Conversion:                          doc/pdf/case-conversion.pdf
.. _Closing and Suspending:                   doc/pdf/closing-suspending.pdf
.. _Comments:                                 doc/pdf/comments.pdf
.. _Counting:                                 doc/pdf/counting.pdf
.. _Cut, Delete, Copy and Paste:              doc/pdf/cut-paste.pdf
.. _Display Lines:                            doc/pdf/display-lines.pdf
.. _Enriched Text:                            doc/pdf/enriched-text.pdf
.. _ERT:                                      doc/pdf/ert.pdf
.. _Faces and Fonts:                          doc/pdf/faces-fonts.pdf
.. _File Management:                          doc/pdf/file-mngt.pdf
.. _File and Directory Local Variables:       doc/pdf/file-variables.pdf
.. _Filling and Justification:                doc/pdf/filling-justification.pdf
.. _Frames:                                   doc/pdf/frames.pdf
.. _Graphviz Dot:                             doc/pdf/graphviz-dot.pdf
.. _Grep:                                     doc/pdf/grep.pdf
.. _Help:                                     doc/pdf/help.pdf
.. _Highlight:                                doc/pdf/highlight.pdf
.. _Hooks:                                    doc/pdf/hooks.pdf
.. _Indentation:                              doc/pdf/indentation.pdf
.. _Input Method:                             doc/pdf/input-method.pdf
.. _Inserting Text:                           doc/pdf/inserting-text.pdf
.. _Keyboard Macros:                          doc/pdf/keyboard-macros.pdf
.. _Marking:                                  doc/pdf/marking.pdf
.. _Menus:                                    doc/pdf/menus.pdf
.. _Dired:                                    doc/pdf/mode-dired.pdf
.. _Org mode:                                 doc/pdf/mode-org-mode.pdf
.. _reStructuredText mode:                    doc/pdf/mode-rst.pdf
.. _Modifier Keys:                            doc/pdf/modifier-keys.pdf
.. _Narrowing:                                doc/pdf/narrowing.pdf
.. _Navigation:                               doc/pdf/navigation.pdf
.. _Packages:                                 doc/pdf/packages.pdf
.. _Common Lisp:                              doc/pdf/pl-common-lisp.pdf
.. _Emacs Lisp:                               doc/pdf/pl-emacs-lisp.pdf
.. _Registers:                                doc/pdf/registers.pdf
.. _Scrolling:                                doc/pdf/scrolling.pdf
.. _Search and Replace:                       doc/pdf/search-replace.pdf
.. _Shells:                                   doc/pdf/shells.pdf
.. _Sorting:                                  doc/pdf/sorting.pdf
.. _Speedbar:                                 doc/pdf/speedbar.pdf
.. _Spell Checking:                           doc/pdf/spell-checking.pdf
.. _Text-modes:                               doc/pdf/text-modes.pdf
.. _Transpose:                                doc/pdf/transpose.pdf
.. _Undo, Redo, Repeat and Prefix Arguments:  doc/pdf/undo-redo-repeat.pdf
.. _Mercurial:                                doc/pdf/vsc-mercurial.pdf
.. _Web:                                      doc/pdf/web.pdf
.. _Whitespaces:                              doc/pdf/whitespaces.pdf
.. _Windows:                                  doc/pdf/windows.pdf



Credits
=======

PEL integrates with several great Emacs Lisp packages.  Some of them are
required, the others are used if they are present and are activated by the PEL
customization.

- PEL uses the following libraries distributed with GNU Emacs:

  - bookmark
  - cc-vars
  - cua-rect
  - delsel
  - elint
  - ert
  - flyspell
  - hl-line
  - imenu
  - isearch
  - ispell
  - kmacro
  - paragraphs
  - simple
  - subr-x
  - subword
  - thingatpt

- PEL has the following dependencies on the following external Emacs packages:

  - `use-package`_ 2.4, by John Wiegley, GPL V3.0.

- Finally PEL can integrate and use the following external Emacs packages, when
  they are activated by PEL customize variables:

  - ace-window
  - bind-key
  - erlang-flymake
  - erlang-start
  - popup
  - pos-tip
  - sr-speedbar

..
   -----------------------------------------------------------------------------

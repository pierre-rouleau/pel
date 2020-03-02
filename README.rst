==============================================
pel -- Pragmatic Environment Library for Emacs
==============================================

.. contents::
.. sectnum::


Overview
========

PEL is a Emacs Lisp package that provides access to a set of features
implemented in the PEL code but also to features implemented by several other
great Emacs packages, as long as they are activated via the mechanism described
in the `PEL Customization`_ section.

In its current form, PEL does not implement any minor or major Emacs mode.
Instead it provides a set of functions that can be accessed globally, with some
of them specialized for some modes and other available everywhere.

PEL defines a large set of key bindings which are mostly extensions to what
is available within standard Emacs.
As of this version, the PEL key bindings mostly use function prefix keys as
described in the `PEL Key Bindings`_ section.

PEL also integrates with a set of third party Emacs packages
(see the list in the `Credits`_ section below).
In several cases PEL contains the logic to install these other packages, the
logic to configure them and the logic to load them as lazily as possible to
reduce the Emacs initialization start time to a minimum.

The use of PEL features and PEL uses of other third party Emacs packages is
controlled by the `PEL customization`_.  By default, no third party package not
already included in the standard GNU Emacs distribution is installed or used.
To get PEL to use them and provide the functionality (along with their published
key bindings) you must first activate them via the `PEL Customization`_
mechanism.

The reason for PEL
------------------

PEL attempts to make Emacs easy to use for new users by providing already made
configuration that is controlled by Emacs customization system, as opposed to
writing lots of Emacs Lisp code.

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
packages. It essentially came out as a desire to be able to use my Emacs
configuration on several systems, both in terminal (TTY) mode and in Graphics
mode while trying to keep  Emacs initialization as fast as possible.
I started writing it while learning Emacs, Emacs Lisp and the amazing packages
that have been written for Emacs.

While doing that, I also needed to document
what I was learning, so I created a set of tables that each list and describe the key
bindings for a specific Emacs topic, like how to navigate, deal with Emacs
buffers, windows and frames, how to undo, redo, work with Emacs Lisp, etc...
See the `Key Binding Documentation`_ section.  The tables list the key bindings
that are available in plain vanilla GNU Emacs but also the bindings PEL adds and
the bindings for the packages PEL integrates with.


PEL Goals
---------

- Ease introduction to Emacs.
- Keep as many standard Emacs key bindings as possible.
- Provide easy to remember key bindings via a key binding tree, key prefixes and
  the use of key choice visualization with package such as which-key, especially
  for commands that are seldom used.
- Minimize the amount of Emacs Lisp code to write inside Emacs init file to
  support various external Emacs packages.

  - Provide all logic necessary to install and configure external Emacs packages.

- Minimize Emacs initialization time even when a large number of packages are
  present on the computer.
- Document what's available: the key bindings, the special considerations, the
  documents that should be read to deepen user's understanding.
- Allow use of PEL even when someone has an extensive Emacs init file.

Essentially, PEL is my first Emacs Lisp project.  I wrote it while learning
Emacs.  I keep using the documentation whenever I forgot the key bindings
of one of the many Emacs features.  There are still a lot of things to do to add
support for external packages and increase the customization. And even more to
transform the documentation format (see the `PDF Documentation`_ section for
that.)

..
   -----------------------------------------------------------------------------


How to Use
==========

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
It currently uses the F2, F6, F11 and F12 keys.
In this version these prefixes are hard-coded; they cannot be user selected via
customization.  That is something that will be added later.



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






Implementation Notes
====================

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

# -----------------------------------------------------------------------------

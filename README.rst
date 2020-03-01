==============================================
pel -- Pragmatic Environment Library for Emacs
==============================================

Overview
========



How to Use
==========


Documentation
=============

PEL comes with a relatively large set of documentation in the form of several
PDF document tables listing the Emacs commands for a given type of
activity.

I created those PDF files with a non-free program because I was
learning Emacs and I needed to be able to document what I was learning and
creating with a tool that did not have the same level of power Emacs has but
also did not have as steep a learning curve.  So I decided to create a set of
tables that are something between a simple quick sheet and a full blown manual,
just becuase I remember better when I write something about what I'm learning.
If I learn better ways to create the same documentation using Emacs and I get
some free time I might convert them.  For now I just included the PDF output
files.

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

Several of these documents also contain a table listing various references on
the specific topic.  These references include hyperlinks to the relevant GNU
Emacs manuals but also to several sites devoted to Emacs including several
demonstration videos hosted on various platforms.

After a while, I ended up with a large set of information and needed to quickly
identify their nature, so I started using special characters showing like icons
to represent a set of concepts, using colors to identify if a specific command
is a standard Emacs command, part of PEL or parts of another package that PEL
integrates with.  These conventions are evolving and are described in the first
of the document listed below; the `Document Legend`_.  The other documents are
mostly listed below by alphabetical order of the topic they describe aside for
some of the topics that have sub-topics.  The section on programming language
and VCS will be expanded as I integrate the Emacs packages that support those
programming languages into PEL, learn how to use them and document them.

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


Customization
=============

PEL provides a set of stand alone features that you get when you install it.
But PEL also integrates several other packages and extend these package by
providing extra key bindings for these extra packages.  In same cases PEL also
provides extra commands that extend those packages.  Some of the packages are
part of the standard Emacs distribution, others are third party Emacs Lisp
packages that are available from web accessible Emacs package archive sites
like MELPA_.

By default PEL will not attempt to use or install those other packages. If you
want to use them you must customize PEL using the ``M-x customize`` command and
select the **Pel Use Package** customize subgroup of the **Pel** customize
group.

PEL provides a set of Emacs customize variable with  names that start with
the ``pel-use-`` prefix to activate the various packages PEL integrates with.

You can set the ``pel-use-`` variables that interest you to **t** and then
re-initialize PEL (by executing ``M-x pel-init``) or by restarting Emacs and
then execute ``M-x pel-init``.   For some of these variables, PEL will attempt
to install the required packages if they are not present and then will activate
the PEL commands that take advantage of the corresponding package.

Currently PEL uses the `use-package`_ system to perform the installation.

You may not want PEL to install packages for you.  In that case install the
package you need first and then activate the ``pel-use-`` variable.

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

.. _ace-window package:       https://melpa.org/#/ace-window
.. _auto-complete package:    https://melpa.org/#/auto-complete
.. _MELPA:                    https://melpa.org/
.. _use-package:              https://melpa.org/#/use-package
.. _bind-key:                 https://melpa.org/#/bind-key
.. _bm:                       https://melpa.org/#/bm
.. _c-eldoc:                  https://melpa.org/#/?q=c-eldoc
.. _SBCL:                     https://en.wikipedia.org/wiki/Steel_Bank_Common_Lisp
.. _slime:                    https://melpa.org/#/slime
.. _slime package:            https://melpa.org/#/slime



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

==============================================
pel -- Pragmatic Environment Library for Emacs
==============================================

Overview
--------



How to Use
----------


Documentation
-------------

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
  - `Faces and fonts`_
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
  -  Modes:

    - `Dired`_
    - `Graphviz-dot`_
    - `Org mode`_
    - `reStructuredText mode`_

  - `Modifier Keys`_
  - `Narrowing`_
  - `Navigation`_
  - `Packages`_
  - Programming Language Support:

    - `Common Lisp`_

      - `Emacs Lisp`_

        - `Ert`_ (Emacs Regression Testing system)

  - `Registers`_
  - `Scrolling`_
  - `Search and Replace`_
  - `Shells`_
  - `Sorting`_
  - `Speedbar`_
  - `Spell Checking`_
  - `Text-modes`_
  - `Transpose`_
  - `Undo, Redo and Repeat`_
  - Version Control Systems:

    - `Mercurial`_

  - `Web`_
  - `Whitespaces`_
  - `Windows`_

..
   -----------------------------------------------------------------------------


.. _Document Legend:                     doc/pdf/-legend.pdf
.. _Abbreviations:                       doc/pdf/abbreviations.pdf
.. _Align:                               doc/pdf/align.pdf
.. _Bookmarks:                           doc/pdf/bookmarks.pdf
.. _Buffers:                             doc/pdf/buffers.pdf
.. _Case Conversion:                     doc/pdf/case-conversion.pdf
.. _Closing and Suspending:              doc/pdf/closing-suspending.pdf
.. _Comments:                            doc/pdf/comments.pdf
.. _Counting:                            doc/pdf/counting.pdf
.. _Cut, Delete, Copy and Paste:         doc/pdf/cut-paste.pdf
.. _Display Lines:                       doc/pdf/display-lines.pdf
.. _Enriched Text:                       doc/pdf/enriched-text.pdf
.. _Ert:                                 doc/pdf/ert.pdf
.. _Faces and Fonts:                     doc/pdf/faces-fonts.pdf
.. _File Management:                     doc/pdf/file-mngt.pdf
.. _File and Directory Local Variables:  doc/pdf/file-variables.pdf
.. _Filling and Justification:           doc/pdf/filling-justification.pdf
.. _Frames:                              doc/pdf/frames.pdf
.. _Graphviz-dot:                        doc/pdf/graphviz-dot.pdf
.. _Grep:                                doc/pdf/grep.pdf
.. _Help:                                doc/pdf/help.pdf
.. _Highlight:                           doc/pdf/highlight.pdf
.. _Hooks:                               doc/pdf/hooks.pdf
.. _Indentation:                         doc/pdf/indentation.pdf
.. _Input Method:                        doc/pdf/input-method.pdf
.. _Inserting Text:                      doc/pdf/inserting-text.pdf
.. _Keyboard Macros:                     doc/pdf/keyboard-macros.pdf
.. _Marking:                             doc/pdf/marking.pdf
.. _Menus:                               doc/pdf/menus.pdf
.. _Dired:                               doc/pdf/mode-dired.pdf
.. _Org mode:                            doc/pdf/mode-org-mode.pdf
.. _reStructuredText mode:               doc/pdf/mode-rst.pdf
.. _Modifier Keys:                       doc/pdf/modifier-keys.pdf
.. _Narrowing:                           doc/pdf/narrowing.pdf
.. _Navigation:                          doc/pdf/navigation.pdf
.. _Packages:                            doc/pdf/packages.pdf
.. _Common Lisp:                         doc/pdf/pl-common-lisp.pdf
.. _Emacs Lisp:                          doc/pdf/pl-emacs-lisp.pdf
.. _Registers:                           doc/pdf/registers.pdf
.. _Scrolling:                           doc/pdf/scrolling.pdf
.. _Search and Replace:                  doc/pdf/search-replace.pdf
.. _Shells:                              doc/pdf/shells.pdf
.. _Sorting:                             doc/pdf/sorting.pdf
.. _Speedbar:                            doc/pdf/speedbar.pdf
.. _Spell Checking:                      doc/pdf/spell-checking.pdf
.. _Text-modes:                          doc/pdf/text-modes.pdf
.. _Transpose:                           doc/pdf/transpose.pdf
.. _Undo, Redo and Repeat:               doc/pdf/undo-redo-repeat.pdf
.. _Mercurial:                           doc/pdf/vsc-mercurial.pdf
.. _Web:                                 doc/pdf/web.pdf
.. _Whitespaces:                         doc/pdf/whitespaces.pdf
.. _Windows:                             doc/pdf/windows.pdf

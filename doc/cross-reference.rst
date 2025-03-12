=====================
Cross-reference tools
=====================

:Home URL: https://github.com/pierre-rouleau/pel
:Project: `PEL -- Pragmatic Emacs Library`_
:Created:  Wednesday, May 12 2021.
:Author:  Pierre Rouleau <prouleau001@gmail.com>
:Modified: 2025-03-12 00:03:59 EDT, updated by Pierre Rouleau.
:Copyright: © 2021-2025, Pierre Rouleau

.. warning:: This is a preliminary version, and is a early form of a *work in progress*.

Requirements:

- Handle multiple directory tree
- Handle directory tree with zipped files of various format
- Handle zip files with complete directory tree of source files



etags with multiple directories:

.. code:: shell

    find . \( -name "*[tT]est*"
              -o -name "CVS"
              -o -name "*#*"
              -o -name "html"
              -o -name "*~"
              -o -name "*.ca*" \)
              -prune -o \( -iname "*.c"
              -o -iname "*.cpp"
              -o -iname "*.cxx"
              -o -iname "*.h"
              -o -iname "*.hh" \) -exec etags -a {} \;

However, see the various shell scripts in the bin directory.
They are much more complete than this.


PEL Emacs Lisp code Tags
------------------------

The directories that contain Emacs Lisp code include:

- ``~/dev/elisp/pel``  : ``*.el`` in single directory
- ``~/.emacs.d/elpa``  : ``*.el`` in all sub-directories
- Emacs source code directory, like:

  - ``/usr/local/Cellar/emacs/26.3/share/emacs`` : ``*.el``, ``*.el.gz`` in all directory tree

.. ---------------------------------------------------------------------------
.. links:

.. _PEL -- Pragmatic Emacs Library: https://github.com/pierre-rouleau/pel#readme

.. ---------------------------------------------------------------------------

..
       Local Variables:
       time-stamp-line-limit: 10
       time-stamp-start: "^:Modified:[ \t]+\\\\?"
       time-stamp-end:   "\\.$"
       End:

=====================
Cross-reference tools
=====================


warning::
  This is a preliminary version, and is a early form of a *work in progress*.

Requirements:

- Handle multiple directory tree
- Handle directory tree with zipped files of various format
- Handle zip files with complete directory tree of source files



etags with multiple directories

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




PEL Emacs Lisp code Tags
------------------------

The directories that contain Emacs Lisp code include:

- ~/dev/elisp/pel  : *.el in single directory
- ~/.emacs.d/elpa  : *.el in all sub-directories
- Emacs source code directory, like:

  - /usr/local/Cellar/emacs/26.3/share/emacs : *.el *.el.gz in all directory tree

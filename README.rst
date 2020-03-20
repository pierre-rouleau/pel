==============================================
pel -- Pragmatic Environment Library for Emacs
==============================================

.. [from:  README ]

PEL is an hybrid package. It is:

- an Emacs key binding documentation project
  (see `Key Bindings Documentation`_),
- an overall configuration and setup system with:

  - the implementation of a function-key based key binding map tree to commands,
    keeping most Emacs key bindings untouched,
  - several files that implement `PEL convenience features`_,
  - `PEL Customization`_ that control the use of the features of several
    external packages (see `Credits`_),
    their loading, configuration and key bindings.
    This conforms to the
    `Emacs easy customization`_ system and reduces your need
    to write custom Emacs Lisp code.

.. _Emacs easy customization:
.. _Emacs customization:       https://www.gnu.org/software/emacs/manual/html_node/emacs/Easy-Customization.html#Easy-Customization


PEL provides:

- A set of already-written configuration for fast loading of several
  useful Emacs packages with:

  - Package activation done through  `Emacs easy customization`_ system,
    see `PEL customization`_.  No extra Emacs Lisp code needed.
  - Extensive use of auto-loading and deferred loading techniques to speed
    up Emacs initialization time.

- Dynamic control of several packages and their commands.
  For example, PEL support both `auto-complete`_ and `company`_ auto-completion
  packages, providing commands to activate one mode in a buffer and
  another mode inside another and while preventing dual use in a buffer.
- `PEL key bindings`_ avoid modification of most Emacs keys, instead
  PEL uses several function keys (**F2**, **F5**, **F6**, **F11** and **F12**)
  as described in the section titled `PEL Function Keys Bindings`_.
- `PEL convenience features`_ include:

  - The key-bindings under **F6**, **F11** and **F12** function keys.
  - A set of small Emacs Lisp files that implement
    PEL's features that deal with several
    aspects of Emacs like windows and frame, scrolling control,  buffer,
    navigation, opening files
    or web pages from file name or URL at point, numeric keypad handling,
    etc...

    - Note that these files can be used as *mostly*
      independent Emacs Lisp *libraries* if you prefer to use a specific
      PEL features without PEL's key bindings.

- Several `PDF Document Tables`_ that describe the key bindings for
  specific topics.
  Each table provides an overview, related Emacs,
  external packages and PEL command descriptions and key bindings
  and links to related on-line documents.

You can either start PEL during Emacs initialization by including the
following Emacs Lisp code inside your `Emacs initialization file`_ :

.. code:: elisp

      (require 'pel)
      (pel-init)

You can place you own customization after the call to ``pel-init``.
This way you can overwrite specific PEL's key bindings if needed.

You can also can start or re-start PEL interactively by typing::

  M-x pel-init


.. _Emacs initialization file: https://www.gnu.org/software/emacs/manual/html_node/emacs/Init-File.html#Init-File





.. [ endof: README ]

More information is available in `PEL's Manual`_.

.. links:

.. _PEL's Manual:               doc/pel-manual.rst
.. _Key Bindings Documentation: doc/pel-manual.rst#pel-key-bindings
.. _PEL convenience features:   doc/pel-manual.rst#pel-convenience-features
.. _PEL Customization:          doc/pel-manual.rst#pel-customization
.. _Credits:                    doc/pel-manual.rst#credits
.. _PEL key bindings:           doc/pel-manual.rst#pel-key-bindings
.. _PDF Document tables:        doc/pel-manual.rst#pdf-document-tables
.. _PEL Function Keys Bindings: doc/pel-manual.rst#pel-function-keys-bindings

.. _auto-complete:              https://melpa.org/#/auto-complete
.. _company:                    https://melpa.org/#/company
.. _visible bookmarks:          https://melpa.org/#/bm
.. _which-key:                  https://elpa.gnu.org/packages/which-key.html


..
   -----------------------------------------------------------------------------

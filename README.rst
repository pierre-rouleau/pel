==============================================
pel -- Pragmatic Environment Library for Emacs
==============================================

- Tired of writing Emacs configuration code? 🤯
- Ever declared `.emacs bankruptcy`_? 😰
- Don't want to spend your time writing Emacs Lisp code? 😳
- Want to learn Emacs? 😇

PEL might be for you!

PEL is an Emacs configuration system that allows you to pick and choose what you
want to use via `Emacs easy customization`_ system.

It integrates a large and growing set of built-in and external Emacs packages,
and adds a large number of key bindings that do not interfere with the standard
Emacs key bindings.

You select the packages you want to use via the `PEL Customization`_ and PEL's
code controls the loading of the required packages, with an emphasis of reducing
the Emacs initialization time to a minimum.

PEL also implements its own `PEL convenience features`_ sometimes extending or
integrating existing packages.

And PEL comes with lots of documentation:

- `PEL's Manual`_,
- `Key Bindings Documentation`_, a large set of PDF *semi-quick sheets*, each
  one is about a specific editing topic or Emacs mode or package, describing all
  related Emacs, package, and PEL specific key bindings and the related Emacs
  command, with lots of hyperlinks to Emacs manual documentation, external
  packages and articles.

PEL would not exist without the great software available for Emacs listed in the
`Credits`_ section of the manual_.

PEL is still evolving, with new packages being integrated, new documentation
created and new features developed.  Comments, criticism (constructive of
course) suggestions are always welcome.  Drop me a note in the wiki_!

.. links

.. _Emacs easy customization:
.. _Emacs customization:       https://www.gnu.org/software/emacs/manual/html_node/emacs/Easy-Customization.html#Easy-Customization
.. _Emacs initialization file: https://www.gnu.org/software/emacs/manual/html_node/emacs/Init-File.html#Init-File
.. _manual:
.. _PEL's Manual:               doc/pel-manual.rst
.. _Key Bindings Documentation: doc/pel-manual.rst#key-bindings-documentation
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
.. _.emacs bankruptcy:          https://www.emacswiki.org/emacs/DotEmacsBankruptcy
.. _wiki:                       https://github.com/pierre-rouleau/pel/wiki


..
   -----------------------------------------------------------------------------

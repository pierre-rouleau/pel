==============================================
PEL -- Pragmatic Environment Library for Emacs
==============================================

.. image:: http://img.shields.io/:license-gpl3-blue.svg
   :alt: License
   :target: http://www.gnu.org/licenses/gpl-3.0.html

- Tired of writing Emacs configuration code? 🤯
- Ever declared `.emacs bankruptcy`_? 😰
- Don't want to spend your time writing Emacs Lisp code? 😳
- Want to learn Emacs? 😇

PEL might be for you!

- PEL builds on `Emacs easy customization`_ to let you pick and choose various
  behaviours and packages without the need for writing Emacs Lisp code.
- PEL integrates a large and growing set of `built-in and external Emacs packages`_,
  and adds a large number of key bindings that do not interfere with the standard
  Emacs key bindings (except for a very small few documented ones).
- PEL controls the activation of the loading of the selected packages, with
  the emphasis of reducing Emacs initialization time to a minimum.

  - On my system with 182 packages with benchmark-init on I get 0.3 to 0.4 seconds
    on a 2014 iMac running macOS Mojave in terminal mode and 0.6 seconds in
    graphics mode, and get similar times on Linux running inside VMs.

- PEL documentation and code emphasizes the ability to execute commands in
  both terminal and graphics mode.
- PEL also implements its own `PEL convenience features`_ sometimes extending or
  integrating existing packages.

PEL comes with lots of overview and reference documentation:

- `PEL's Manual`_,

  - Read the section on `How to install PEL`_.

- `Key Bindings Documentation`_, over 80 `PDF reference sheets`_, each
  one is about a specific editing topic or Emacs mode or package, describing all
  related Emacs, packages, and PEL specific key bindings and commands.
  The PDF files have lots of hyperlinks to other PEL PDF files,
  Emacs manual documentation, external packages and articles.

**Credits**

PEL would not exist without the great software available for Emacs listed in the
`Credits`_ section of the manual_.

**What's New**

PEL is still evolving, with new packages being integrated, new documentation
created and new features developed.  The evolution is described in the `NEWS file`_.

Comments, constructive criticism, suggestions and requests are always welcome.
Drop me a note in the wiki_!

.. links

.. _Emacs easy customization:
.. _Emacs customization:       https://www.gnu.org/software/emacs/manual/html_node/emacs/Easy-Customization.html#Easy-Customization
.. _Emacs initialization file: https://www.gnu.org/software/emacs/manual/html_node/emacs/Init-File.html#Init-File
.. _manual:
.. _PEL's Manual:               doc/pel-manual.rst
.. _Key Bindings Documentation: doc/pel-manual.rst#key-bindings-documentation
.. _PDF reference sheets:       doc/pel-manual.rst#pdf-document-tables
.. _PEL convenience features:   doc/pel-manual.rst#pel-convenience-features
.. _PEL Customization:          doc/pel-manual.rst#pel-customization
.. _built-in and external Emacs packages:
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
.. _How to install PEL:         doc/pel-manual.rst#how-to-install-pel
.. _NEWS file:                  NEWS


..
   -----------------------------------------------------------------------------

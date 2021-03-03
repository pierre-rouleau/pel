==============================================
PEL -- Pragmatic Environment Library for Emacs
==============================================

.. image:: https://img.shields.io/:license-gpl3-blue.svg
   :alt: License
   :target: https://www.gnu.org/licenses/gpl-3.0.html

.. image:: https://img.shields.io/badge/Version->V0.3.1-teal
   :alt: Version
   :target: https://github.com/pierre-rouleau/pel/blob/master/NEWS#changes-since-version-031


- Tired of writing Emacs configuration code? 🤯
- Afraid of or ever declared `.emacs bankruptcy`_? 😰
- Don't want to spend your time writing Emacs Lisp code? 😳
- Need to quickly access help now and later on specific topic? 🤔
- Want to learn Emacs and try several built-in and external packages? 😇

PEL might be for you!

- PEL builds on `Emacs easy customization`_ to let you pick and choose various
  behaviours and packages without the need for writing Emacs Lisp code.

  - PEL controls installation, configuration and activation of **over 150**
    *top-level* [1]_ external packages through the use of easy-to-setup
    customization user-options.  These have a name that start with the
    "``pel-use-``" prefix.
    See the manual `built-in and external Emacs packages`_ section.

    - PEL integrates these packages and adds a large number of key bindings
      **that mostly do not interfere with the standard Emacs key bindings**
      (except for a very small few documented ones).

    - PEL controls the activation of the loading of the selected packages, with
      emphasis on:

      - reducing Emacs initialization time to a minimum [2]_ and,
      - providing key bindings when Emacs is running in both graphics mode and
        terminal mode.

  - PEL provides several context-sensitive key sequences to access
    customization groups for PEL, Emacs and external packages, to help you
    fine tune your Emacs customization quickly.

    - PEL provides access to customization groups for features not even loaded
      yet.  Increasing your awareness of available features.

  - PEL saves your customization data inside a file separate from your
    ``init.el`` file, providing an extra degree of freedom and control when
    you same these files in a (D)VCS. Save and restore your configurations
    from Git, Mercurial or any VCS.

- PEL documentation and code emphasizes the ability to execute commands in
  both terminal and graphics mode.
- PEL also implements its own `PEL convenience features`_ sometimes extending or
  integrating existing packages.

PEL comes with lots of overview and reference documentation:

- `PEL's Manual`_,

  - Read the section on `How to install PEL`_.

- `Key Bindings Documentation`_, over 95 `PDF reference sheets`_, each
  one is about a specific editing topic or Emacs mode or package, describing all
  related Emacs, packages, and PEL specific key bindings and commands.
  The PDF files have lots of hyperlinks to other PEL PDF files,
  Emacs manual documentation, external packages and articles.

  - All of these PDF files have a large number of hyperlinks to Emacs manual,
    Emacs Wiki, external package home on Github, MELPA and the other PDF
    that contain relevant information.  If your browser is configured to
    display PDF files online (like Firefox does by default) you can easily
    browse the PDF files by accessing the raw version of these files on
    GitHub.  See the `raw version of the PEL Key Maps PDF`_ as a starting point.

**Credits**

PEL would not exist without the great software available for Emacs listed in the
`Credits`_ section of the manual_.

**What's New**

PEL is still evolving, with new packages being integrated, new documentation
created and new features developed.  The evolution is described in the `NEWS file`_.

Questions, comments, constructive criticism, suggestions and requests are always welcome.
Drop me a note in the wiki_ or in the `discussion board`_!


*Notes*:

.. [1] An external package may have dependencies.  The dependencies are also installed.
.. [2] **Quick initialization**: On my system with 182 packages with benchmark-init on I get 0.3 to 0.4 seconds
       on a 2014 iMac running macOS Mojave in terminal mode and 0.6 seconds in
       graphics mode, and get similar times on Linux running inside VMs.

.. links

.. _raw version of the PEL Key Maps PDF: https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/-pel-key-maps.pdf

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
.. _discussion board:           https://github.com/pierre-rouleau/pel/discussions


..
   -----------------------------------------------------------------------------

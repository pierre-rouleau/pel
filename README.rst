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

PEL might be for you!  Then go ahead, `install it`_ [1]_
or `update it`_ [2]_ !

- PEL provide **customization-driven package management** built on `Emacs easy
  customization`_ to pick and chose packages and behaviours, all without
  having to write Emacs Lisp code.  PEL enhances Emacs customization system:
  PEL provides keys to quickly access customization groups of Emacs built-in
  and external Emacs Lisp libraries even if they are not even loaded.

- PEL controls the download, installation, configuration and activation of
  **over 200** *top-level* [3]_ external packages through the use of
  easy-to-setup customization user-options that have a name that start with
  the "``pel-use-``" prefix.  See the manual `built-in and external Emacs
  packages`_ section.

- PEL can install packages from `GNU Elpa`_, MELPA_, and simple GitHub or
  Gitlab repositories even when the files have not been setup as an
  elpa-compliant package.

- Use the **pel-cleanup** command to **remove** deactivated packages not
  requested by PEL user-options, moving those packages from the ``elpa``
  or ``utils`` directory to the corresponding *attic* directories for
  backup and later re-activation.

- PEL integrates these packages and adds a large number of key bindings
  **that mostly do not interfere with the standard Emacs key bindings**
  (except for a very small few documented ones).

- PEL saves your customization data inside a file separate from your
  ``init.el`` file, providing an extra degree of freedom and control when you
  same these files in a (D)VCS. Save and restore your configurations from Git,
  Mercurial or any VCS.

- PEL controls the activation of the loading of the selected packages, with
  emphasis on:

  - reducing Emacs initialization time to a minimum [4]_ and,
  - providing key bindings when Emacs is running in both graphics mode and
    terminal mode,
  - adding extra commands that complement Emacs commands and the commands of
    external packages.

- PEL **extends Emacs documentation**:

  - PEL provides **Over 110 PDF topics oriented reference sheets** with
    hyperlinks to Emacs manuals, external packages, articles and other
    references.

    - See the `PEL Index PDF`_ as a starting point.  For the best user
      experience, use a browser, like Firefox, that can render the PDF inline
      (as opposed to downloading it) so you can use the links quickly.  Inside
      Emacs use ``<f11> <f1>`` to open the `PEL Index PDF`_ and then navigate
      from it, or use ``<f12> <f1>`` to open the PDF describing the major mode
      of the current buffer and its key bindings.  All of these PDF files are
      created from a macOS Number file.  It is located in a separate Git repo:
      the `pel-pdf-spreadsheet repo`_.


  - `PEL's Manual`_ describes PEL features in more details. See:

    - `How to install PEL`_,
    - `PEL convenience features`_ ,
    - `Key Bindings Documentation`_,
    - `PDF reference sheets`_.





**Credits**

PEL would not exist without the great software available for Emacs listed in the
`Credits`_ section of the manual_.

**What's New**

PEL is still evolving, with new packages being integrated, new documentation
created and new features developed.  The evolution is described in the `NEWS file`_.

Questions, comments, constructive criticism, suggestions and requests are always welcome.
Drop me a note in the wiki_ or in the `discussion board`_!


*Notes*:

.. [1] The instructions for installing PEL are located inside `section 2`_ of `PEL manual`_.
       The manual is large and some browser do not always move to the right location.
.. [2] The instructions for updating PEL are located inside `section 3`_ of `PEL
       manual`_.  You can also use the `manual's table of contents`_ to move around.
       The manual is large and some browser do not always move to the right location.
.. [3] An external package may have dependencies.  The dependencies are also
       installed. PEL currently provides access to over 190 top-level
       packages. The actual number of packages is larger when counting their dependencies.

.. [4] **Quick initialization**: On my system with 182 packages with benchmark-init on I get 0.3 to 0.4 seconds
       on a 2014 iMac running macOS Mojave in terminal mode and 0.6 seconds in
       graphics mode, and get similar times on Linux running inside VMs.  I
       can reduce this further by reducing the number of features supported by
       turning them off and run **M-x pel-cleanup** to remove them from Emacs
       path and internal processing.

.. links

.. _PEL Key Maps PDF:   https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/-pel-key-maps.pdf
.. _PEL Index PDF:      https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/-index.pdf
.. _Emacs easy customization:
.. _Emacs customization:       https://www.gnu.org/software/emacs/manual/html_node/emacs/Easy-Customization.html#Easy-Customization
.. _Emacs initialization file: https://www.gnu.org/software/emacs/manual/html_node/emacs/Init-File.html#Init-File
.. _manual:
.. _PEL manual:
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
.. _install it:
.. _section 2:
.. _How to install PEL:         doc/pel-manual.rst#how-to-install-pel
.. _section 3:
.. _update it:                  doc/pel-manual.rst#updating-pel
.. _NEWS file:                  NEWS
.. _discussion board:           https://github.com/pierre-rouleau/pel/discussions
.. _GNU Elpa:                   https://elpa.gnu.org
.. _MELPA:                      https://melpa.org/#/
.. _manual's table of contents: doc/pel-manual.rst
.. _pel-pdf-spreadsheet repo:   https://github.com/pierre-rouleau/pel-pdf-spreadsheet#readme


..
   -----------------------------------------------------------------------------

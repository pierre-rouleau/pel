==============================
PEL -- Pragmatic Emacs Library
==============================

.. image:: https://img.shields.io/:license-gpl3-blue.svg
   :alt: License
   :target: https://www.gnu.org/licenses/gpl-3.0.html

.. image:: https://img.shields.io/badge/Version->V0.3.1-teal
   :alt: Version
   :target: https://github.com/pierre-rouleau/pel/blob/master/NEWS#changes-since-version-031

.. image:: https://img.shields.io/badge/Fast-startup-green
   :alt: Version
   :target: https://github.com/pierre-rouleau/pel#quick

.. image:: https://img.shields.io/badge/PDF_Files-133-blue
   :alt: Version
   :target: https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/-index.pdf

.. image:: https://img.shields.io/badge/PEL_Manual-Updates_needed-orange
   :alt: Version
   :target: https://github.com/pierre-rouleau/pel/blob/master/doc/pel-manual.rst


- Tired of writing Emacs configuration code? 🤯
- Afraid of or ever declared `.emacs bankruptcy`_? 😰
- Don't want to spend your time writing Emacs Lisp code? 😳
- Need to quickly access help now and later on specific topic? 🤔
- Want to learn Emacs and try several built-in and external packages? 😇
- Want a fast startup even with a large number of external packages installed? 😃

PEL might be for you!  Then go ahead, `install it`_ [#install]_
or `update it`_ [#update]_ !

➣ **Supports Launching Emacs from GUI and Shell**

- PEL supports terminal Emacs launched from a shell and graphics Emacs
  launched from a shell or a GUI program like Windows Explorer, macOS Finder,
  Linux file managers, etc...

➣ **Emacs Fast Startup**

- PEL supports the `package-quickstart feature`_ available on Emacs version 27
  and later. PEL provides `early-init.el file template`_ and provides the
  **pel-setup-with-quickstart** command (``<f11> M-S q``) to create and refresh
  all the package-quickstart files [#quickst]_ and **pel-setup-no-quickstart**
  (``<f11> M-S M-q``) to disable it.

- PEL also provides further startup speedup with PEL **fast startup mode**.
  See `⅀ Fast Startup PDF`_ (see [#firefox]_).  Easily speed up Emacs startup
  with a single command!  PEL supports 2 different Emacs startup operation
  modes:

  - The **normal Emacs startup setup**, using Emacs' standard package.el
    package manager with PEL's extensions that provide **customization-driven
    package management** built on `Emacs easy customization`_ to pick and
    chose packages and behaviours, all without having to write Emacs Lisp
    code.  PEL enhances Emacs customization system: PEL provides keys to
    quickly access customization groups of Emacs built-in and external Emacs
    Lisp libraries even if they are not even loaded.

  - The **fast startup** mode. It can achieve 0.1 second startup with over 230
    external packages, see [#quick]_.  In fast startup you can use all
    external packages you have already installed in normal startup mode but
    now Emacs starts much faster.  In fast startup PEL does not support
    download and installation of new external packages but just return to
    normal mode to do so.  PEL provides 2 commands to switch modes:

    - The **pel-setup-fast** (``<f11> M-S f``) activates the fast startup
      mode. It bundles all external packages that use a single directory
      inside a single directory to reduce Emacs ``load-path`` and sets up your
      Elpa directory for a fast startup.
    - The **pel-setup-normal** (``<f11> M-S n``) restores the normal Emacs
      setup with package management capabilities and PEL's automatic package
      installation via customization.

➣ **Customization Driven**

- PEL saves your customization data inside a file separate from your
  ``init.el`` file, providing an extra degree of freedom and control.

- PEL supports the ability to have 2 independent customization files: one for
  Emacs running in terminal mode and another for Emacs running in graphics
  mode. PEL supports two sets of elpa directories to store the packages used
  for each mode when this independent dual customization mode is used.  PEL
  provides the **pel-setup-dual-environment** command to activate this.
  Follow the installation instructions and see the `⅀ Customize PDF`_ for more
  details.

  - Some modifications to your ``init.el`` file and to ``early-init.el``
    is required for this as described by OPTION A in:

    - `example/init/init-5.el`_
    - `example/init/early-init.el`_ (for Emacs 27 and later only).

  - Once your init files have been instrumented for PEL, use the
    **pel-setup-dual-environment** command (or ``<f11> <f2> M-d``) to
    activate dual independent customization.  It will create the required
    customization files.

➣ **Download, Installation and Setup of External Packages**

- In normal mode, PEL controls the download, installation, configuration and
  activation of **over 230** *top-level* [#externp]_ external packages through the
  use of easy-to-setup customization user-options that have a name that start
  with the "``pel-use-``" prefix.

  - PEL can install packages from `GNU Elpa`_, MELPA_, and simple GitHub or
    Gitlab repositories even when the files have not been setup as an
    elpa-compliant package.

  - Use the **pel-cleanup** command to **remove** deactivated packages not
    requested by PEL user-options, moving those packages from the ``elpa``
    or ``utils`` directory to the corresponding *attic* directories for
    backup and later re-activation.

  - PEL controls the activation and loading of selected packages, with
    emphasis on:

    - reducing Emacs initialization time to a minimum [#quick]_ in all modes,
    - providing key bindings when Emacs is running in both graphics mode and
      terminal mode,
    - adding extra commands that complement Emacs and external packages.

- PEL integrates these packages and adds a large number of key bindings
  **that mostly do not interfere with the standard Emacs key bindings**
  (except for a very small few documented ones).

➣ **Extends Emacs Documentation**:

- PEL provides **over 130** `PDF topic-oriented reference sheets`_ packed
  with symbol annotated, colour coded key bindings and command descriptions,
  with hyperlinks to Emacs manuals, external packages, articles and other
  useful references.

  - See the `PEL Index PDF`_ [#firefox]_ as a starting point.  For the best
    user experience, use a browser, like Firefox, that can render the PDF
    inline (as opposed to downloading it) so you can use the links quickly.
    Inside Emacs use ``<f11> <f1>`` to open the `PEL Index PDF`_ and then
    navigate from it, or use ``<f12> <f1>`` to open the PDF describing the
    major mode of the current buffer and its key bindings.  All of these PDF
    files are created from a macOS Number file.  It is located in a separate
    Git repo: the `pel-pdf-spreadsheet repo`_.

- `PEL's Manual`_ describes PEL features in more details. See:

  - `How to install PEL`_,
  - `How to setup GUI Emacs environment`_ to control GUI launched Emacs PATH
    and other environment variables easily with a fast starting Emacs,
  - `PEL setup commands`_ for `dual customization`_ and `fast startup mode`_,
  - `PEL initialization command`_ and `PEL cleanup command`_,
  - `PEL Customization`_,
  - `PEL Key Bindings`_,
  - `PEL convenience features`_.

➣ **Credits**

PEL would not exist without the great software available for Emacs.  Some of
them are listed in the `Credits`_ section of the manual_ but the list is
unfortunately incomplete as it grows continuously when PEL supports new
packages.  The external packages are identified in the PDF tables with the box
symbol: 📦 with hyper-links to the author's project.

➣ **What's New**

PEL is still evolving, with new packages being integrated, new documentation
created and new features developed.  The evolution is described in the `NEWS file`_.

Questions, comments, constructive criticism, suggestions and requests are always welcome.
Drop me a note in the wiki_ or in the `discussion board`_!


*Notes*:

.. [#install] The instructions for installing PEL are located inside `section 2`_ of `PEL manual`_.
              The manual is large and some browser do not always move to the
              right location.

.. [#update]  The instructions for updating PEL are located inside `section 5`_ of `PEL
              manual`_.  You can also use the `manual's table of contents`_ to move around.
              The manual is large and some browser do not always move to the
              right location.

.. [#externp] An external package may have dependencies.  The dependencies are also
              installed. PEL currently provides access to over 190 top-level
              packages. The actual number of packages is larger when counting their dependencies.

.. [#firefox] All `PEL PDF files`_ have a large number of hyperlinks to other
              PDF files, Emacs manual pages, external packages and articles.
              Use a browser that is capable of rendering PDF files for the best user
              experience.  The `Mozilla Firefox`_ browser does an excellent job
              at it since its version 78, under all operating system and is
              highly recommended.

.. [#quick] **Quick initialization**: On my 2014 iMac running macOS Mojave in
            terminal mode running Emacs 26.3 I get the following startup time:

            - with 182 packages, benchmark-init reports about 0.4 second startup-time,
            - with 238 packages, benchmark-init reports about 0.6 second
              startup-time, see the `benchmark-init report for it <doc/res/normal-startup-001.png>`_
            - with 238 packages, in **fast startup** operation mode, benchmark-init
              reports about **0.1 second startup-time**, see the
              `benchmark-init report for that <doc/res/fast-startup-001.png>`_.

            It's possible to reduce this further by restricting the number of used
            package. This must be done in normal operation mode (as opposition to
            the fast startup operation mode) by changing the PEL user-options and
            then running the **M-x pel-cleanup** command.  Once done, return to
            fast startup operation mode.

            Use the following commands to switch operation modes and then restart Emacs:

            - ``M-x pel-setup-fast`` (bound to ``<f11> M-S f``)
            - ``M-x pel-setup-normal`` (bound to ``<f11> M-S n``)

            The time reduction of fast startup mode depends on the number of
            packages that can be bundled by PEL.  Those that have all their files
            in the same directory can be bundled.

.. [#quickst] **Package Quickstart Support**:

              The **pel-setup-with-quickstart** command creates the package
              quickstart files and the early-init.el file if it's not already
              present.

              The ``pel-early-init-with-package-quickstart`` user-option
              (which defaults to `example/init/early-init.el`_) allows you to
              identify the early-init.el file to use with package-quickstart
              feature activated.

              The ``pel-early-init-without-package-quickstart`` user-option
              (which defaults to nil) allows you to identify whether one
              early-init.el file is used when the package quickstart feature
              is disabled.

              Emacs ``package-quickstart-file`` user-option identifies the
              name of your package quickstart file.
              When using PEL's independent customization for terminal/TTY and graphics
              Emacs PEL manages 2 package quickstart files: one for the
              terminal/TTY mode with that name and one for the graphics mode
              which has "-graphics" appended to its name.

.. links


.. _PEL Key Maps PDF:   https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/-pel-key-maps.pdf
.. _PEL Index PDF:      https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/-index.pdf
.. _Emacs easy customization:
.. _Emacs customization:       https://www.gnu.org/software/emacs/manual/html_node/emacs/Easy-Customization.html#Easy-Customization
.. _Emacs initialization file: https://www.gnu.org/software/emacs/manual/html_node/emacs/Init-File.html#Init-File
.. _manual:
.. _PEL manual:
.. _PEL's Manual:               doc/pel-manual.rst
.. _PEL Key Bindings:           doc/pel-manual.rst#pel-key-bindings
.. _PEL convenience features:   doc/pel-manual.rst#pel-convenience-features
.. _PEL Customization:          doc/pel-manual.rst#pel-customization
.. _Credits:                    doc/pel-manual.rst#credits
.. _PEL key bindings:           doc/pel-manual.rst#pel-key-bindings
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
.. _section 5:
.. _update it:                  doc/pel-manual.rst#updating-pel
.. _NEWS file:                  NEWS
.. _discussion board:           https://github.com/pierre-rouleau/pel/discussions
.. _GNU Elpa:                   https://elpa.gnu.org
.. _MELPA:                      https://melpa.org/#/
.. _manual's table of contents: doc/pel-manual.rst
.. _pel-pdf-spreadsheet repo:   https://github.com/pierre-rouleau/pel-pdf-spreadsheet#readme
.. _example/init/init-5.el:     example/init/init-5.el
.. _PEL PDF files:
.. _PDF topic-oriented reference sheets: doc/pdf
.. _PEL setup commands:        doc/pel-manual.rst#pel-setup-commands
.. _PEL initialization command: doc/pel-manual.rst#pel-initialization-command
.. _PEL cleanup command:        doc/pel-manual.rst#pel-cleanup-command
.. _PEL Customization:          doc/pel-manual.rst#pel-customization
.. _dual customization:         doc/pel-manual.rst#independent-customization-for-terminal-and-graphics-modes
.. _fast startup mode:          doc/pel-manual.rst#normal-startup-and-fast-startup-modes
.. _⅀ Fast Startup PDF:         https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/fast-startup.pdf
.. _⅀ Customize PDF:            https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/customize.pdf
.. _Mozilla Firefox:            https://en.wikipedia.org/wiki/Firefox
.. _How to setup GUI Emacs environment: doc/pel-manual.rst#prepare-using-gui-launched-emacs-running-in-graphics-mode
.. _early-init.el file template:
.. _example/init/early-init.el: example/init/early-init.el
.. _package-quickstart feature: https://git.savannah.gnu.org/cgit/emacs.git/commit/etc/NEWS?id=6dfdf0c9e8e4aca77b148db8d009c862389c64d3

..
   -----------------------------------------------------------------------------

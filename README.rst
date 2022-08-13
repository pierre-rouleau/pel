==============================
PEL -- Pragmatic Emacs Library
==============================

.. image:: https://img.shields.io/:License-gpl3-blue.svg
   :alt: License
   :target: https://www.gnu.org/licenses/gpl-3.0.html

.. image:: https://img.shields.io/badge/Version->V0.4.1-teal
   :alt: Version
   :target: https://github.com/pierre-rouleau/pel/blob/master/NEWS#changes-since-version-041

.. image:: https://img.shields.io/badge/Fast-startup-green
   :alt: Version
   :target: https://github.com/pierre-rouleau/pel#quick

.. image:: https://img.shields.io/badge/package_quickstart-Compatible-green
   :alt: Version
   :target: https://github.com/pierre-rouleau/pel#quickst

.. image:: https://img.shields.io/badge/Direct_Installable_Packages-237-teal
   :alt: Version
   :target: `➣ Automatic Download, Installation and Setup of External Packages`_

.. image:: https://img.shields.io/badge/PEL_Commands-520-teal
   :alt: Version
   :target: https://github.com/pierre-rouleau/pel#commands

.. image:: https://img.shields.io/badge/PDF_Files-138-blue
   :alt: Version
   :target: https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/-index.pdf

.. image:: https://img.shields.io/badge/PEL_Manual-Αlpha_state-blue
   :alt: Version
   :target: https://github.com/pierre-rouleau/pel/blob/master/doc/pel-manual.rst


- Tired of writing Emacs configuration code? `🤯`_
- Afraid of or ever declared `.emacs bankruptcy`_? 😰
- Don't want to spend your time writing Emacs Lisp code? 😳 [#elispfun]_
- Need to quickly access help now and later on specific topic? `🤔`_
- Want to learn Emacs and try several built-in and external packages? `😇`_
- Want a fast startup even with a large number of external packages installed?
  `😃`_

PEL might be for you!  Then go ahead, `install it`_ [#install]_
or `update it`_ [#update]_ !


➣ Supports Launching Emacs from GUI and Shell
---------------------------------------------

- PEL supports terminal Emacs launched from a shell and graphics Emacs
  launched from a shell or a GUI program like Windows Explorer, macOS Finder,
  Linux file managers, etc...

➣ Emacs Fast Startup
--------------------

- Speed Emacs startup further with PEL `fast startup mode`_ command
  (see also `⅀ Fast Startup PDF`_ [#firefox]_).
  PEL supports 2 different Emacs startup operation modes:

  - The **normal startup** mode, using Emacs' standard package.el
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

- PEL supports Emacs 26 and later.

  - For Emacs 27 and later the ``pel-early-init-template`` user-option (which
    defaults to `example/init/early-init.el`_) allows you to identify a
    PEL-compatible early-init.el file that PEL edits to control whether fast
    startup and `package-quickstart feature`_ are used.

    - PEL provides the **pel-setup-with-quickstart** command (``<f11> M-S q``)
      to create and refresh all the package-quickstart files [#quickst]_ and
      **pel-setup-no-quickstart** (``<f11> M-S M-q``) to disable it.

➣ Customization Driven Package Management & Configuration
---------------------------------------------------------

- PEL uses Emacs customization facility to control everything: the selection,
  installation and configuration of packages, the activation of various major
  and minor modes, the improvement of behaviour of various major and minor
  modes, the use of various features provided by PEL.

- PEL saves your customization data inside a file separate from your
  ``init.el`` file, providing an extra degree of freedom and control. This
  also gives you the ability to easily revision and clone your Emacs
  environment to other computers.

- PEL provides enhanced and easy access to relevant customization buffers for
  your specific context.  Each supported topic or major mode context has a PEL
  prefix key and each supported major mode uses the ``<f12>`` key as the main
  prefix key.  For instance in a C buffer, use ``<f12> <f1>`` to access the
  C-specific PEL PDF, ``<f12> <f2>`` to access the PEL customization buffer to
  activate C features, and ``f12> <f3>`` to the customization buffers
  controlling the major mode and related features.  If they are not loaded PEL
  will load them for you after prompting.  Even if the code was not written to
  support auto-loading of customization variables.

- The classical way to control Emacs behaviour has been to write Emacs Lisp
  code to set variables.  Although you can still do it with PEL you will
  benefit from PEL's integration better if you control all via the
  customization buffers and customization files.
  You can modify the behaviour by applying
  customization modification to test your changes and then save them to a file
  once you are happy with it.

- PEL supports the ability to have 2 independent customization files: one for
  Emacs running in terminal mode and another for Emacs running in graphics
  mode. PEL supports two sets of elpa directories to store the packages used
  for each mode when this independent dual customization mode is used.  PEL
  provides the **pel-setup-dual-environment** command to activate this.
  Follow the installation instructions and see the `⅀ Customize PDF`_ for more
  details.

  - A `PEL compatible Emacs init.el`_ is available for you to use right from the
    beginning.  `Instructions are inside`_.  You can use it as is or modify some
    of the options.
  - For Emacs ≥ 27, PEL also provides a `PEL compatible early-init.el`_.
    Instructions are inside.  PEL will automatically install it if you want to
    use Emacs package quickstart feature available on Emacs ≥ 27.  You can
    also modify it and add your own code.  Instructions are located in the
    file.
  - PEL comes with `samples of Emacs custom files`_ you can use with PEL to
    quickly activate features.  After following the PEL installation steps,
    Copy one of these files into your ``~/.emacs.d/emacs-customization.el``
    file then start Emacs and watch PEL download, install and activates the
    packages identified in the file you selected.
  - If you want to manage 2 set of customization files and package directory,
    one for Emacs in terminal mode and another for Emacs in graphics mode, use the
    **pel-setup-dual-environment** command (or ``<f11> <f2> M-d``) to
    activate dual independent customization.  It will create the required
    customization files.

➣ PEL Package Integration and Enhancements
------------------------------------------

Emacs gets its power from the large set of built-in and external packages
available for it.  A large number of external packages are available from
package repositories like `GNU Elpa`_, MELPA_.  There are also Emacs Lisp
files available on Github and Gitlab.  PEL provides access to a growing number
of these packages as described in the next section.

One of PEL's goals is to enhance the cohesion and the integration of these
packages to provide a more pleasant and customizable user experience.  PEL
implements various convenience commands, easy-to-remember key bindings and
glue control code to enhance several minor and major modes, allowing behaviour
selection through customization and PEL use-option variables.

Some of the improvements created for PEL are fed back to the original project(s)
but it's available inside PEL if the code has not yet been merged in the
original project.

PEL also implements various template-driven text insertion for various
programming and markup languages.

As PEL evolves the goal is to support for programming languages will increase
and each fully supported programming language will come with a topic-oriented
help PDF, enhanced electric key behaviours, enhanced navigation integrating
various packages, etc...


➣ Automatic Download, Installation and Setup of External Packages
-----------------------------------------------------------------

- In normal mode, PEL controls the download, installation, configuration and
  activation of **229** *top-level* [#externp]_ external packages through the
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

➣ Extends Emacs Documentation
-----------------------------

- PEL provides **over 135** `PDF topic-oriented reference sheets`_ [#doc]_ packed
  with symbol annotated, colour coded key bindings and command descriptions,
  with hyperlinks to Emacs manuals, external packages, articles and other
  useful references.

  - See the `PEL Index PDF`_ [#firefox]_ as a starting point.  For the best
    user experience, use a browser, like Firefox, that can render the PDF
    inline (as opposed to downloading it) so you can use the links quickly.
    Inside Emacs use ``<f11> <f1>`` to open the `PEL Index PDF`_ and then
    navigate from it, or use ``<f12> <f1>`` to open the PDF describing the
    major mode of the current buffer and its key bindings.

- `PEL's Manual`_ describes PEL features in more details. See:

  - `How to install PEL`_,
  - `How to setup GUI Emacs environment`_ to control GUI launched Emacs PATH
    and other environment variables easily with a fast starting Emacs,
  - `PEL setup commands`_ for `dual customization`_ and `fast startup mode`_,
  - `PEL initialization command`_ and `PEL cleanup command`_,
  - `PEL Customization`_,
  - `PEL Key Bindings`_,
  - `PEL convenience features`_ with PEL commands that extend several aspects
    of Emacs.



➣ PEL Convenience Commands
--------------------------


- PEL provides over 465 extra commands.  PEL commands extend the packages
  it supports and provides some of its own features.
- PEL provides 7 Hydra key bindings when the ``pel-use-hydra`` and the topic
  specific user-options are activated.


➣ Credits
---------


PEL would not exist without the great software available for Emacs.  Some of
them are listed in the `Credits`_ section of the manual_ but the list is
unfortunately incomplete as it grows continuously when PEL supports new
packages.  The external packages are identified in the PDF tables with the box
symbol: 📦 with hyper-links to the author's project.

➣ What's New
------------


PEL is still evolving, with new packages being integrated, new documentation
created and new features developed.  The evolution is described in the `NEWS file`_.

Questions, comments, constructive criticism, suggestions and requests are always welcome.
Drop me a note in the wiki_ or in the `discussion board`_!


Notes
-----


.. [#elispfun] Writing Emacs Lisp code is actually quite fun and rewarding.
               Lisp is a powerful programming language and Emacs opens up the door to
               that world if it's foreign to you.  PEL should ease the
               introduction and you may decide to go your own way later.
               For those that prefer to stay away from Emacs Lisp and
               concentrate on other tasks you can use PEL and configure Emacs
               using its powerful customization system.

.. [#install] Unfortunately PEL installation is manual but it's not difficult.
              Detailed instructions for installing PEL are located inside `section
              2`_ of `PEL manual`_.

              - You essentially have to clone PEL's repo,
                start with a fresh ``~/.emacs.d`` directory, open Emacs twice
                and run make in the PEL directory to byte compile PEL's
                files.  You will have to update the init.el and early-init.el
                if you want to modify some options.  And you may want to use
                some canned customization files.  PEL installs and configure
                what you activate through customization.

              - See `PEL manual table of contents`_ to move around the manual.


.. [#update]  The instructions for updating PEL are located inside the
              `Updating PEL`_ section of the `PEL manual`_.

              - Essentially what's required is to get the latest changes using
                Git (with ``git pull``) and then run ``make`` again.


.. [#externp] An external package may have dependencies.  The dependencies are also
              installed. PEL currently provides access to 229 top-level external packages.
              The actual number of packages is larger when counting their
              dependencies (currently 48 extra packages installed as dependencies).
              From within PEL execute the **pel-package-info**
              command with prefix argument to get a full report (or just type
              ``C-u <f11> ? e ?``).  Also notice the short report printed at
              the end the make-driven build of PEL.

.. [#firefox] All `PEL PDF files`_ have a large number of hyperlinks to other
              PDF files, Emacs manual pages, external packages and articles.
              Use a browser that is capable of rendering PDF files for the best user
              experience.  The `Mozilla Firefox`_ browser does an excellent job
              at it since its version 78, under all operating system and is
              highly recommended.

.. [#doc]     The source of the `PEL PDF files`_ is a single macOS Number
              spreadsheet file.  It's also available in the Git
              `pel-pdf-spreadsheet repo`_.  I would have liked  to find a way
              to create this with a markup flexible enough but I did not find
              one. Let me know if you know one that can build the same output.

.. [#quick] **Fast initialization**:  PEL code uses all the techniques to
            improve initialization speed.  By default it starts quickly,
            delaying code as much as possible.

            On my 2014 iMac running macOS Mojave in terminal mode running
            Emacs 26.3 I get the following startup time in normal startup
            mode (and without package-quickstart):

            - with 182 packages, `benchmark-init`_ reports about 0.4 second startup-time,
            - with 238 packages, benchmark-init reports about 0.6 second
              startup-time, see the `benchmark-init report for it
              <doc/res/normal-startup-001.png>`_

            Activate PEL **fast startup mode** to experience **much faster**
            initialization times:

            - with 238 packages, in **fast startup** operation mode, benchmark-init
              reports about **0.1 second startup-time**, see the
              `benchmark-init report for that
              <doc/res/fast-startup-001.png>`_.
            - The time reduction of fast startup mode depends on the number of
              packages that can be bundled by PEL.  Those that have all their files
              in the same directory can be bundled.

            With PEL it's possible to reduce this further by removing packages
            you do not need, without loosing their configuration:

            - go to normal startup mode,
            - disable un-required packages by setting their corresponding
              ``pel-use-`` user-option to nil,
            - run the `pel-cleanup command`_ (with ``M-x pel-cleanup``).  It
              will disable those packages by putting their packages inside an
              *attic* directory where you can retrieve them later.

              - If the removed packages are multi-directory package their
                removal will speed-up initialization in normal and
                fast-startup mode, otherwise it will only speed it up in
                normal mode.

.. [#quickst] **Package Quickstart Support**:

              The **pel-setup-with-quickstart** command creates the package
              quickstart files and the early-init.el file if it's not already
              present.


              Emacs ``package-quickstart-file`` user-option identifies the
              name of your package quickstart file.
              When using PEL's independent customization for terminal/TTY and graphics
              Emacs PEL manages 2 package quickstart files: one for the
              terminal/TTY mode with that name and one for the graphics mode
              which has "-graphics" appended to its name.

.. [#commands] **PEL Commands**:

              PEL implements its own commands.  These complements the packages
              PEL supports and also provide a layer that unifies mechanisms
              implemented by several built-in and external packages.

              Some PEL commands act according to the key sequences that
              invoked them.  For example, the **pel-help-pdf** command is
              bound to  ``<f11> a <f1>``, ``<f11> b <f1>`` and several others
              and the ``<f12> <f1>`` key sequences of several major modes.
              The command detects the key sequence and the context to
              identify which PEL PDF to open.

              PEL currently implements 520 interactive commands.  Some are
              always loaded.  Several are auto-loaded when needed.  This count
              exclude the template commands that are dynamically constructed
              and loaded only for major modes, such as Erlang, that support
              extensive tempo skeleton templates.

              Navigate the PDF files starting from the `PEL Index PDF`_ to get more
              information.



.. links

.. _😇: `➣ PEL Package Integration and Enhancements`_
.. _🤯: `➣ Customization Driven Package Management & Configuration`_
.. _😃: `➣ Emacs Fast Startup`_
.. _PEL Key Maps PDF:   https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/-pel-key-maps.pdf
.. _🤔: `➣ Extends Emacs Documentation`_
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
.. _Updating PEL:
.. _update it:                  doc/pel-manual.rst#updating-pel
.. _NEWS file:                  NEWS
.. _discussion board:           https://github.com/pierre-rouleau/pel/discussions
.. _GNU Elpa:                   https://elpa.gnu.org
.. _MELPA:                      https://melpa.org/#/
.. _PEL manual table of contents: doc/pel-manual.rst
.. _pel-pdf-spreadsheet repo:   https://github.com/pierre-rouleau/pel-pdf-spreadsheet#readme
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
.. _PEL compatible Emacs init.el: example/init/init.el
.. _PEL compatible early-init.el:
.. _early-init.el file template:
.. _example/init/early-init.el: example/init/early-init.el
.. _package-quickstart feature: https://git.savannah.gnu.org/cgit/emacs.git/commit/etc/NEWS?id=6dfdf0c9e8e4aca77b148db8d009c862389c64d3
.. _benchmark-init:             https://github.com/dholm/benchmark-init-el#readme
.. _pel-cleanup command:        doc/pel-manual.rst#pel-cleanup-command
.. _fast startup mode:          doc/pel-manual.rst#normal-startup-and-fast-startup-modes
.. _samples of Emacs custom files: sample/emacs-customization
.. _Instructions are inside:   doc/pel-manual.rst#further-configure-the-init-el-file

..
   -----------------------------------------------------------------------------

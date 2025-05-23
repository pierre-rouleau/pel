==============================
PEL -- Pragmatic Emacs Library
==============================

:URL: https://github.com/pierre-rouleau/pel/blob/master/doc/pel-manual.rst
:Project:  `PEL Project home page`_
:Modified: 2025-05-15 14:47:49 EDT, updated by Pierre Rouleau.
:License:
    Copyright (c) 2020, 2021, 2022, 2023, 2024, 2025 Pierre Rouleau <prouleau001@gmail.com>


    You can redistribute this document and/or modify it under the terms of the GNU
    General Public License as published by the Free Software Foundation, either
    version 3 of the License, or (at your option) any later version.


    This document is distributed in the hope that it will be useful, but WITHOUT ANY
    WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
    PARTICULAR PURPOSE. See the GNU General Public License for more details.



.. _PEL Project home page:  https://github.com/pierre-rouleau/pel


.. contents::  **Table Of Contents**
.. sectnum::


-----------------------------------------------------------------------------

Overview
========

PEL is an hybrid package. It is:

- an Emacs key binding documentation project with **over 130** `PDF
  topic-oriented reference sheets`_ packed with symbol annotated, colour coded
  key bindings and command descriptions, with hyperlinks to Emacs manuals,
  external packages, articles and other useful references.

  - access these PDF files directly from Emacs using the
    **pel-help-pdf-select** command bound to ``<f11> ? p``, or with the
    ``<f12> <f1>`` key sequence of supported major modes.  PEL can open the
    local PDF file or open them in your default web browser. See the `Help PDF`_
    for more details.

  - You can also open the `PEL Index PDF`_ that lists all the PEL PDF files
    and provides a good overview of what is available and a good starting
    point.  For the best user experience use a good web browser like Firefox
    [#firefox]_ version 78 or later that can render the PDF files inline.

- a collection of `PEL convenience features`_ which:

  - are implemented in several mostly independent files (which can be used
    independently if you want to use just a couple of PEL features),
  - have key bindings mostly using the function keys to provide
    key map trees that do not interfere with standard Emacs key bindings,
  - provides access to over 230 external Emacs packages via `PEL
    Customization`_.  You select what you want to use then let **pel-init**
    download, install and configure them, dealing with dependencies and key
    binding activation.

    - PEL conforms to the `Emacs easy customization`_ system and reduces your
      need to write custom Emacs Lisp configuration code,
    - PEL optionally supports two customization files: one for Emacs running
      in terminal/TTY mode and another one for Emacs running in graphics mode.

  - makes extensive use of auto-loading and deferred loading techniques to speed
    up Emacs initialization time.
  - provides dynamic control of several packages and their commands.
    For example, PEL support both `auto-complete`_ and `company`_ auto-completion
    packages, providing commands to activate one mode in a buffer and
    another mode inside another and while preventing dual use in a buffer.
  - `PEL key bindings`_ avoid modification of most Emacs key bindings. Instead
    PEL uses several function keys (**F2**, **F5**, **F6**, **F11** and
    **F12**) as key prefixes as described in the section titled `PEL Function
    Keys Bindings`_.  It also uses the **F9** key as a prefix to enter Greek
    letters when ``pel-activate-f9-for-greek`` is activated and for  some external packages.
  - PEL features are implemented by a `set of small Emacs Lisp files`_ and
    that deal with several aspects of Emacs like windows and frame, scrolling
    control, buffer, navigation, opening files or web pages from file name or
    URL at point, numeric keypad handling, etc...

- a mechanism that provides two Emacs startup modes:

  - the **normal startup operation mode** which corresponds to the normal
    way Emacs is used where the package.el package manager is used by PEL
    and where it is possible to:

    - use PEL's Emacs customization to select features and
      where the **pel-init** command downloads and installs the external
      packages required for those features,
    - disable features no longer needed and remove them by executing the
      **pel-cleanup** command, reducing startup time accordingly.

  - the **fast-startup operation mode** that does not allow installation
    of any new package but provides a much faster Emacs init startup time,
    even with Emacs 26.3.  In this mode, PEL bundles all external Elpa
    packages that have files in a single directory, reducing the number of
    elpa directories which reduces Emacs init startup time furthermore.
    With it its possible to reduce Emacs startup time to 0.1 second even
    when using over 230 external packages with no loss of functionality
    (except ability to install new packages).

    - PEL also supports the package quickstart mechanism for Emacs 27 and
      later.
    - PEL use techniques similar to those used by `use-package`_ but goes
      further by providing the extra mode of operation. The use-package
      describes being able to use over 80 packages with a start time of 2
      seconds.  With PEL's fast startup of 0.15 second for 238 packages,
      on Emacs 26.3, that's about 40 times faster!

To use PEL you must set your `Emacs initialization file`_ appropriately.
If you are using Emacs ≥ 27 with quick-startup you must also setup your
``early-init`` file appropriately.

The section titled `How to install PEL`_ below will guide you through the
process.


.. _use-package library:
.. _use-package:               https://github.com/jwiegley/use-package#readme
.. _Emacs easy customization:
.. _Emacs customization:       https://www.gnu.org/software/emacs/manual/html_node/emacs/Easy-Customization.html#Easy-Customization
.. _Emacs initialization file: https://www.gnu.org/software/emacs/manual/html_node/emacs/Init-File.html#Init-File
.. _set of small Emacs Lisp files: https://github.com/pierre-rouleau/pel
.. _Tech Radar:                https://www.techradar.com/best/browser
.. _Top Ten Reviews:           https://www.toptenreviews.com/best-internet-browser-software
.. _Mozilla's browser comparison: https://www.mozilla.org/en/firefox/browsers/compare/
.. _PCMag:                     https://www.pcmag.com/picks/chrome-edge-firefox-opera-or-safari-which-browser-is-best
.. _PDF topic-oriented reference sheets: pdf
.. _Mozilla Firefox: https://en.wikipedia.org/wiki/Firefox

.. [#firefox] I highly recommend the `Mozilla Firefox`_ browser.  Firefox
              version 78 or later renders the PDF files in the browser window
              by default, a *very* useful feature when browsing PEL PDF files.
              PEL provides a user-option (``pel-browser-used``) to force the
              use of Firefox to open PEL PDF files even when you use another
              browser as your default browser for that reason.

              In 2021 Firefox is a highly rated web browser.  See the
              following articles:

              - `Tech Radar`_ (Firefox is their best overall)
              - `Top Ten Reviews`_  (Firefox is best for privacy)
              - PCMag_ describes the main browsers
              - It's also worth reading `Mozilla's browser comparison`_.


The reason for PEL
------------------

PEL attempts to make Emacs easier for new users by providing already made
configuration that is controlled by Emacs customization system.  It reduces the
need for writing Emacs Lisp configuration code for the packages it supports and
incorporates the knowledge inside files that can be byte-compiled for further
speed enhancements.

There are several great packages for Emacs. Some are easy to install, others
require more knowledge, knowledge that is often not readily available to new
users and will require a time investment you may not be willing to make.

Instead of having to write Emacs Lisp code inside an Emacs init file for each
new package you want to use, you can use PEL, select the features you want
via `PEL Customization`_ and then execute ``pel-init`` to activate them.
PEL has the logic for configuring the packages it supports.  In
some cases it also contains the logic to install the package if it is not
already installed.

PEL essentially came out as a desire to be able to use an Emacs
configuration on several systems, both in terminal (TTY) mode and in Graphics
mode while trying to keep  Emacs initialization as fast as possible and reducing
the repetitive writing of package initialization code.

I started writing PEL while learning Emacs, Emacs Lisp and various packages.
PEL encapsulates some knowledge about various
tweaks to use and configure several built-in Emacs features and
third party packages.

I also created a set of `PDF topic-oriented reference sheets`_
that each list and describe a specific topic, the commands and key bindings
related to that topic and a topic specific entry inside this manual that also
refers to the corresponding PDF file.  These are all accessible via the `PEL Index PDF`_.
There are several topics; Emacs navigation, Emacs
buffers, windows and frames, how to undo, redo, work with Emacs Lisp, etc...
See the `Key Bindings Documentation`_ section.
The commands and key bindings described in those PDF files include what is provided
by the plain vanilla GNU Emacs as well by the third party packages PEL integrates and the
PEL implemented commands.


PEL Goals
---------

- Ease introduction to Emacs.
- Simplify and speed up Emacs initialization and configuration.

  - Minimize Emacs initialization time even when a large number of packages are
    present on the computer.
  - Minimize the amount of Emacs Lisp code to write inside Emacs init file to
    support various external Emacs packages.
  - Provide all logic necessary to install and configure external Emacs
    packages.

- Provide easy to remember key bindings via a key binding tree, key prefixes and
  the use of key choice visualization with package such as which-key_, especially
  for commands that are seldom used.

  - Keep as many standard Emacs key bindings as possible.

- Document what's available: the key bindings, the special considerations, the
  documents to read for further learning.
- Allow use of PEL even when someone has an extensive Emacs init file.
- Add support for several programming languages integrating many packages that
  support these programming languages.  Support for C, C++, Rust, Go,
  Python, Erlang, Elixir, Haskell, OCaml and several are planned
  (but... no schedule yet!).

**Note**:
   PEL will grow with time, incorporating more documentation,
   support for more Emacs packages related to editing and
   programming tasks.  Don't hesitate to report problems and
   recommend better ways!


Using Portions of PEL Manually
------------------------------

If you prefer not using PEL's key bindings you can `override them`_.
You can also just use the `PEL features`_ you want and create your own key
bindings. In that case, don't call ``pel-init``, require the respective PEL
source code file and create your own key bindings.
Most PEL files are listed in each of the corresponding
`PEL Convenience Features`_ section but the manual is not yet complete and
will also evolve over time.

.. _override them: `Override or change PEL key bindings`_
.. _PEL features:  `PEL Convenience Features`_


..
   -----------------------------------------------------------------------------

How to Install PEL
==================

PEL is not yet available through MELPA_ (Milkypostman's Emacs Lisp Package
Archive) or any Elpa-compatible Emacs package archive. It might be one day,
although the nature of the PEL project might not be compatible with
MELPA_ or ELPA_.

Therefore semi-automated installation instruction are detailed in the
following sections.

**Requirements**

Before you can install PEL, you need to ensure that you have the
required tools.  They are described in the following sections:

#. `Install Emacs 26.1 or later`_. If you are new to Emacs, the easiest is to use
   one of the latest stable releases.
#. `Install other required command line tools`_


**Installation Instructions**

The fully detailed instructions are described in the following sections:

#. `Clone the PEL Git repository`_ on your computer.
#. `Prepare Emacs Initialization directory`_ where lots of Emacs files will
   go.
#. `Byte Compile PEL Files`_.  PEL features require byte-compilation.
#. `Further Configure the init.el File`_
#. Learn `PEL Customization`_ mechanism you will use to configure Emacs.
#. `Optional Installation Steps`_ you may want to do now or later

   - `Further PEL Configuration`_.

     - `Configure Spell Checking`_
     - `Disable Emacs Startup splash screen and echo area message`_
     - `Simpler Prompts`_
     - `More Emacs Customization`_

   -  Optionally, `create command line shortcuts for Emacs`_.
   -  Optionally, to launch a graphics Emacs process from a GUI app like Linux
      file manager, macOS Finder or Windows Explorer:
      `Prepare using GUI-launched Emacs running in graphics mode`_.
   - And also, `Prepare using shell-launched Emacs running in graphics mode`_,
   - And finally `Prepare using shell-launched Emacs running in terminal mode`_.


.. _Emacs Lisp Packages: https://www.gnu.org/software/emacs/manual/html_node/emacs/Packages.html#Packages

Install Emacs 26.1 or later
---------------------------

Install a version of Emacs you can run from the command line.
It will be used to build PEL later.
Once PEL is built on your system you can use either a terminal based
version of Emacs or a graphical version of Emacs with PEL.

To check if you have Emacs on your system, open a shell and execute the
following commands:

- ``which emacs`` to see if Emacs is available.

  - On macOS, if this is ``/usr/bin/emacs``, you most probably have
    the old version of Emacs that Apple installed, and that is most
    probably not version 26.1 or later.

- Check the version of Emacs you have with the following command line:

  ``emacs --version``

Make sure you have Emacs version 26.1 or later.  If not, install it.

For macOS
~~~~~~~~~

You can use Homebrew_, a command line utility, to install a command line
version of Emacs.

- See Homebrew_ home page for how to install Homebrew.
- Once homebrew is installed, you can use the following commands inside a
  terminal shell:

  - ``brew search emacs`` to list Homebrew package names that include "emacs".
  - ``brew info emacs`` to see what version of emacs is available.
  - ``brew install emacs`` to download and install Emacs.

.. _Homebrew: https://brew.sh/

Install other required command line tools
-----------------------------------------

If the following tools are not already installed on your system install them now:

- **git**
- **make**

You should also install spell checking utilities.  There are several.  The
default is ispell. It's best to install it at the beginning if it is not
already there, so also install:

- **ispell**

If you want to use the fast vterm_ shell into Emacs, you also need to install:

- **cmake**
- and possibly **libtool**.



Clone the PEL Git repository
----------------------------

**Description:**

Clone the `PEL's Github repo`_ somewhere in your home directory but outside your
"~/.emacs.d" directory.  The instruction assumes that you store it inside
"~/projects" to create "~/projects/pel".

Note:
     You could use anything, but then you'll have to remember to update the
     instructions appropriately, replacing "``~/projects``" with whatever you want
     to use.

The commands will create the "``~/projects/pel``" directory tree with all
PEL's files inside it, including all `PDF topic-oriented reference sheets`_
that document the key bindings of Emacs and the libraries you can activate with
PEL.

.. _PEL's Github repo: https://github.com/pierre-rouleau/pel

**Do this:**

Open a terminal shell or command line window and execute the following
commands:

.. code:: shell

          cd
          mkdir projects
          cd projects
          git clone https://github.com/pierre-rouleau/pel.git



Prepare Emacs Initialization directory
--------------------------------------

**Description:**

There are several ways to set up `Emacs initialization file`_.

You will have to store several Emacs-related files in your system:

- PEL itself,
- Emacs init file, ``init.el``.
- Emacs customization file,
- Emacs bookmarks file,
- Emacs abbreviation files,
- External Emacs Lisp libraries downloaded from Elpa-compliant sites like
  ELPA_, MELPA_ or MELPA-STABLE_,
- External Emacs Lisp libraries from the EmacsAttics_ or EmacsMirror_ that are
  not Elpa-protocol compliant are supported by PEL and must be stored into the
  *utils* directory.
- etc...

It is best to use the "``~/.emacs.d``" directory where all of these files will
be stored.

**Note**:

If you currently store all your emacs configuration inside the ``~/.emacs``
file, rename that file and keep it.  The information it contains will have to
be moved somewhere else, most probably inside the ~/.emacs.d/init.el if PEL
does not already handle your logic.


**Windows users**:
  Under Windows, your ".emacs.d" directory should be stored inside your HOME
  directory. See `Emacs Windows init location FAQ`_ for more information.

**Do this:**

- Open a terminal shell or command line window and execute the following
  commands:

.. code:: shell

          cd
          emacs

This should open a graphical version of Emacs.  If that does not work check
your Emacs installation.  You may also want to try to execute Emacs in
terminal mode:

.. code:: shell

          cd
          emacs -nw

- Close Emacs:  type ``C-x C-c``.  That is hold the ``control`` key and type ``x``
  followed by ``c`` and then release the control key.

- If it did not already exist, this should have created the ``~/.emacs.d``
  directory.  If not create it using the ``mkdir ~/.emacs.d`` command.

- Copy PEL's example/init/init.el into your ``~/.emacs.d`` using the following
  shell commands (assuming you stored PEL inside ``~/projects/pel``:


.. code:: shell

          cd ~/project/pel
          cp example/init/init.el ~/.emacs.d/init.el

- Run emacs again:

.. code:: shell

          emacs


This time PEL initialization file should have taken over.

- PEL  will create missing files and will access MELPA to install the popup and
  which-key_ packages.
- PEL will then display a warning message to inform you about the fact that
  Emacs customization file did not originally exists before PEL created it.
  This message is expected when installing PEL and should not show up later.
  You should see something like this:

.. figure:: res/pel-install-01.png
   :scale: 50 %

- Close Emacs (with ``C-x C-c``).  Open it again.  Now you should see no
  warnings, just the standard Emacs screen:

.. figure:: res/pel-install-02.png
   :scale: 50 %

If you still see errors, and if that error contains something like::

  File is missing: Cannot open load file, No such file or directory, pel

Emacs cannot find the pel file.  If you did not clone the pel repo into
your ``~/project`` directory you now have to edit the ``~/.emacs.d/init.el`` file.

Look for the following text and change the path of the pel directory inside
the string:

.. code:: elisp

          ;; OPTION B:  if PEL is stored somewhere else change the following value.
          (defconst pel-home-dirpath-name (expand-file-name "~/projects/pel")
            "Directory where PEL Emacs Lisp source files are stored.")

Update the string inside the defconst call; identify where you stored pel.
Then save the updated ``~/.emacs.d/init.el`` file (with ``C-x C-s``) and
exit Emacs (with ``C-x C-c``).

Start Emacs again, now all should be fine.

To activate PEL you must byte-compile all PEL Emacs Lisp source code.
This is described in the next section.

.. _Emacs Windows init location FAQ: https://www.gnu.org/software/emacs/manual/html_node/efaq-w32/Location-of-init-file.html

Byte Compile PEL Files
----------------------

**Description:**

PEL features require byte compilation of PEL's Emacs Lisp files.  Not only it
provides faster execution but byte compilation allows PEL Emacs Lisp macros to
properly setup the environment.

Use the provided Makefile script to byte-compile all required PEL Emacs Lisp
source code files located in the ``pel`` directory.
It will also run some regression tests.
Nothing will be downloaded during this byte compilation.


**Do this:**

.. code:: shell

          cd ~/projects/pel
          make clean
          make

The make script should terminate with an exit code of 0
and you should see no error or warning.

**In case of Errors:**

  If the make script stopped at an error, check the following:

  - Check the content of your Emacs user directory.  Several files and
    directories are required by Emacs, some are added by PEL and some are old
    PEL files that should be removed.  Check the list of files and directories
    in the section titles `Emacs and PEL Files and Directories`_.  Add missing
    files, remove `obsolete PEL files`_.

  - Run make again.

  If the problem persists, or if you see an error or a warning during the build
  or when you start Emacs, please `create an issue`_ describing the problem and
  your environment and I will get to it.

**At this point you can use Emacs with PEL**

You should have a working version of PEL with all PEL
files byte-compiled for efficiency 😅!

**Quick Test PEL**

PEL download and activates the which-key_ external package by default.
PEL uses the **F11** key extensively.

- Make sure your environment provide access to that key.  In terminal mode you
  may have to configure your terminal application to prevent it from using the
  F11 key.

- Once that's done open Emacs in terminal mode with ``emacs -nw`` from a
  shell, or open Emacs in graphics mode with ``emacs``, then:

- Hit the **F11** key.  That's PEL main key prefix. After one or two seconds
  the which-key_ package should display the keys that can be typed after F11
  like this:

.. figure:: res/pel-install-03.png
   :scale: 50 %

- The key keys following key prefixes will be shown at the bottom of the Emacs
  screen by which-key_ (because ``pel-use-which-key`` is turned on by default)
  and any package(s) which-key_ may require.
- Type ``<f11> ? p`` and a topic to open one of the PEL PDF files.
  Use tab to complete what you type.  Type tab at first to see a complete
  list of PDF files.  Or type ``<f11> <f1>`` to open the local copy of the
  `PEL Index PDF`_.
  This file has lots of hyperlinks.  They all lead to the
  Github-hosted raw PDF you can quickly navigate with a browser that renders
  PDF inline, like Firefox [#firefox]_ version 78 or later.  That's a quick
  way to navigate PEL's key documentation and access relevant information.
- As usual in Emacs, type ``C-x C-c`` to close it.

**Next Steps**

To fully take advantage of PEL continue with the following steps:

- `Update any option in PEL init.el file`_ if required.
- Learn `PEL Customization`_ mechanism you will use to configure Emacs.
- Improve your Emacs user experience by selecting Emacs behaviour and packages
  that suits your needs with the `Optional Installation Steps`_.

  - Note that PEL comes with a set of already-made Emacs customization files
    that you can copy into your ``~/.emacs.d/emacs-customization.el`` file to
    quickly get a taste of working with PEL.  These files activate a set of
    user-options including several ``pel-use-`` user-options that identify
    packages that PEL must download, install, configure and activate.

    - If you copy one of the files from the `sample/emacs-customization`_
      directory into your ``~/.emacs.d/emacs-customization.el``, the next time
      you start Emacs with PEL-supporting init file, PEL will download the
      associated files, byte compile them and activate the key bindings
      associated with them.


More information about PEL configuration is in the next section.


.. _create an issue: https://github.com/pierre-rouleau/pel/issues
.. _Update any option in PEL init.el file: `Further Configure the init.el File`_
.. _sample/emacs-customization: ../sample/emacs-customization
.. _PEL Index PDF: https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/-index.pdf

.. ---------------------------------------------------------------------------

Further Configure the init.el File
----------------------------------

PEL's ``init.el`` file contain the code required to run PEL with Emacs.  There
are values set in that file you may want to change.  These are described in
this section.

Use Emacs to edit the ``~/.emacs.d/init.el`` file.

- Start Emacs.
- To open the file type ``C-x f ~/.emacs.d/init.el``
- Read the instructions located inside the ``User Configuration`` section on
  top of the file.
- Use ``C-s OPTION RET`` to search for the word "OPTION" and modify the code
  according to what you need:

  - **OPTION B**: If you want to store PEL's source code somewhere else than
    ``~/projects/pel`` change the name of the directory at that option.

  - **OPTION C**: is where you control the use of the benchmark-init package
    to identify what code takes time during Emacs startup.  You should do this
    only once you feel comfortable with Emacs.

  - **OPTION D**: By default Emacs displays generic information about GNU
    and Emacs on startup.  After reading it once or twice you may want to
    prevent this information from showing up.  For that un-comment the line
    shown below the OPTION C text and replace the string YOUR_USER_NAME by
    your OS user name.  On Unix-like OS, this is what the **who** command
    displays.

  - **OPTION E**: this is a small section of code that activates or
    de-activates various global Emacs settings.  It starts with a commented
    line that disables the tool bar of Emacs running in graphics mode.  If
    you do not want to use that tool-bar un-comment the corresponding line
    of code.  Read the code in that section.  You may want to modify some of
    this.  However remember that PEL controls Emacs behaviour through
    customization, not by code invoked through the init.el file: it's best
    to minimize what you add the this section of code if you want to take
    advantage of what PEL offers and to minimize Emacs startup time.

  - **OPTION F**: is where you will want to store the code that used to be
    inside your old init.el file if you had one.  However if that code was
    selecting Emacs behaviour and configuring packages that are already under
    control from PEL, its best to keep this code out of init.el and leave PEL
    manage it.  If you want PEL to support a feature it does not please write
    a ticket to report it and I'll try to accommodate the request quickly.

  - **OPTION G** identifies a set of Emacs functions that are normally
    disabled because they tend to confuse new Emacs users.  You can activate
    them here.

- Save your modifications back to the init.el file by typing ``C-x C-s``
- Keep Emacs opened on your init.el file.

- Open a new terminal shell.

  - Open Emacs in that new shell.  If all is OK, Emacs should start fine and
    should not display any error message.  If it does display an error message
    then something is probably wrong in your init.el file.  Modify it, save
    the file and try again.

- Once Emacs starts properly close all Emacs sessions.
  You can type ``C-x C-c`` to save all buffers and terminate all Emacs
  sessions.


.. ---------------------------------------------------------------------------

Optional Installation Steps
---------------------------

The following steps described in this section are optional but strongly
recommended if you want to take advantage of:

- ability to launch Emacs from a GUI program (see
  `Prepare using GUI-launched Emacs running in graphics mode`_),
- and other improvements.


Further PEL Configuration
~~~~~~~~~~~~~~~~~~~~~~~~~

The following sections describe optional optimizations or modifications
that can be done after the first complete and successful installation of PEL.

Configure Spell Checking
^^^^^^^^^^^^^^^^^^^^^^^^

To use spell checking features in Emacs, you must use a spell
checking program available from the command line.  Emacs Ispell and Flyspell can
use a Ispell-compatible program like:

- `ispell <https://en.wikipedia.org/wiki/Ispell>`_,
- aspell_,
- hunspell_, or
- enchant_.


.. _aspell:    https://en.wikipedia.org/wiki/GNU_Aspell
.. _hunspell:  https://en.wikipedia.org/wiki/Hunspell
.. _enchant:   https://en.wikipedia.org/wiki/Enchant_(software)

If none is available on your system you will have to install it manually.

Identify the program to use in PEL customization user option variable
``pel-spell-check-tools``. This user option allow you to define one program per
Operating System.  You can also identify the location of your personal
dictionary file.

To quickly gain access to the customization buffer for the
``pel-pkg-for-spelling`` group where that user option is located type
the ``<f11> <f2> $`` key sequence.

For the changes to take effect, save the changes and execute pel-init
(with ``M-x pel-init``) or restart Emacs.

More information on PEL support of spell checking is available
in the `PEL Spell Checking Support`_ section and the `Spell Checking`_ PDF sheet.

Disable Emacs Startup splash screen and echo area message
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

By default Emacs displays its splash screen on startup and displays a message on
the echo area telling you about Emacs in general and the concept of free
software. Once you have read this information, you may want to prevent Emacs
from showing it again.

The file `example/init/init.el`_ contains the code that disables the splash
screen. the code that disable the message is still commented out.

To disable it, edit your init.el file, which contain the same code.
Look for ``OPTION D``.  You can search by typing ``C-s`` then the string you
search for.  You should see the following comment:

.. code:: elisp

          ;; OPTION D: Don't display Emacs startup help message, at least for me.
          ;; This variable is treated specially.  Don't group its setting with others.
          ;;   Replace YOUR_USER_NAME by your systems' login user name in the line
          ;;   below and un-comment it:
          ;; (eval '(setq inhibit-startup-echo-area-message "YOUR_USER_NAME"))

Remove the ``;;`` to un-comment the last line and
replace ``YOUR_USER_NAME_HERE`` by your user name.  The same user name
displayed by the ``who`` utility on a Unix-like operating system.

Emacs was written to allow multiple users from having access to the same
configuration, and this identifies the user that will not be reminded of Emacs
concepts and principles every time Emacs starts.  So, to take advantage of that
small speed up make sure you put your user name there.


.. _example/init/init.el: ../example/init/init.el

Simpler Prompts
^^^^^^^^^^^^^^^

Emacs prompts that require you to type ``yes`` or ``no`` might be annoying.  If
you would prefer being able to just type ``y`` or ``n`` instead, as most
people do, set the ``pel-prompt-accept-y-n`` user option to **t**.  There are
several ways you can do this:

- Execute:  ``M-x customize-option`` then type ``pel-prompt-accept-y-n``, hit
  return to open the customization buffer and change the user option value.
  Then apply and save it.
- Use the PEL key sequence for the above: ``<f11> <f2> o`` and type the name.

More Emacs Customization
^^^^^^^^^^^^^^^^^^^^^^^^

If this is the first time you use Emacs you will also want to customize the
following options.  Use ``<f11> <f2> o`` or ``M-x customize-option`` for each
of those because their corresponding file should already been loaded in Emacs.

======================================== ======================================
Emacs user option                        Description
======================================== ======================================
column-number-mode                       Set it to **t** to activate the
                                         display of the column number on the
                                         mode line.

comment-empty-lines                      Set it to **t** if you want to
                                         comment empty lines when commenting
                                         a region of lines.

confirm-nonexistent-file-or-buffer       Set it to **nil** (never) to prevent
                                         confirmation prompts every time you
                                         want to edit a file that does not
                                         exist to create it.

fill-column                              Set the default maximum line length.
                                         A good number is **78**.  For source
                                         code, PEL provides another set of
                                         user options for each programming
                                         language, allowing you to control
                                         that user option from the file type.

hl-line-sticky-flag                      Set it to **nil** if you only want to
                                         highlight the text in the current
                                         window when the buffer shows in
                                         multiple windows.


truncate-lines                           Set it to **t** if you want Emacs to
                                         truncate long lines instead of
                                         wrapping them.  You can change this
                                         behaviour by using ``<f11> l t`` or
                                         ``M-x toggle-truncate-line``.

user-full-name                           Your full name.
                                         PEL uses it in various file skeletons.

user-mail-address                        Your email address.
                                         PEL uses it in various file skeletons.
======================================== ======================================


You will probably want to configure some other behaviour.  Since PEL promotes
the use of the Emacs customization we'll do it through that.  You need to know
that some Emacs packages are loaded by default and they can be customized
right away. Others define auto-loaded customization variables.  Yet others do
not autoload their customizable variables.

With PEL you activate packages via PEL customization variables that are always
loaded.  And you can customize variables of installed packages that are not
yet loaded and have not been auto-loaded by using PEL commands.

The imenu package is built-in but its customization variables are not
auto-loaded. To modify the following user-option (the other name for
customization variables) you can type ``<f11> <f10> <f3> 2``.  That will load
the imenu package and open the customization buffer for imenu where you can
edit the following user-option.


======================================== ======================================
Emacs user option                        Description
======================================== ======================================
imenu-max-items                          Set the maximum number of entries in
                                         the imenu list if the default of 25
                                         does not correspond to what you like.
======================================== ======================================

Make sure to save the values in a file by pressing the ``Apply and Save``
button at the top of the buffer window.  You may also have to confirm at a
prompt shown at the bottom of the screen.


To modify bookmark specific user options, you can type ``<f11> ' <f3> 1`` to
open the bookmark customize buffer and then modify the following user-options:


======================================== ======================================
Emacs user option                        Description
======================================== ======================================
bookmark-save-flag                       Set it to **1** to get Emacs to save
                                         the bookmarks inside the bookmark
                                         file every time a bookmark is set.

bookmark-default-file                    Set the location of the bookmark
                                         file. The default is
                                         ``~/.emacs.d/bookmarks``
======================================== ======================================

If a package is not installed you can't customize it. Set the PEL ``pel-use-``
user-option, restart Emacs to install the package and then you can customize it.

.. ---------------------------------------------------------------------------

Create command line shortcuts for Emacs
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

New Emacs users may be interested by command line commands to start Emacs in
various ways:

- starting an independent emacs process in terminal (TTY) mode from a shell,
- starting an independent emacs process in graphics mode from a shell,
- starting an emacs client to an emacs daemon (the Emacs server).

PEL provides 3 short-named scripts to do this on macOS and Linux:

= =========================================== ================================================
  Command                                     Description
= =========================================== ================================================
. e_\ ``[OPTIONS..] [FILE..]``                Start Emacs in text mode.

                                              - Supports emacs options and file arguments.
                                                See emacs man page.

. ge_ :                                       Start Emacs in graphics mode.

  - ``ge [-h | --help]``                      - Supports emacs options and file arguments.
  - ``ge [OPTIONS..] [FILE..]``                 See emacs man page.


. ec_ :                                       Start an Emacs client for an Emacs daemon.

  - ``ec [-h|--help]``                        - Use ``ec`` to start an Emacs client to the
  - ``ec [-g|gui] [--name=NAME] [FILE...]``     default Emacs daemon, starting the daemon
  - ``ec --start-daemon [--name=NAME]``         if it is not already running.  The client
  - ``ec --check-daemon [--name=NAME]``         is a terminal based client by default.
                                              - To start a graphical (GUI) Emacs client
                                                instead, use ``ec -g`` or ``ec --gui``.
                                                This also starts the Emacs daemon if it is not
                                                already running.
                                              - By default the ``ec`` command launches and
                                                connects to the default socket name.
                                                Use the ``--name`` option to specify another
                                                name (for both the daemon and its clients).
                                                For example, use ``ec --name=PEL`` to start
                                                and connect to a Emacs daemon that uses the
                                                socket named ``PEL``.

                                                - With this option it is possible to create
                                                  several independent *groups* of Emacs
                                                  daemon/clients.

                                              - Use ``ec --start-daemon`` (optionally
                                                specifying the daemon name with ``--name``) to
                                                start the Emacs daemon *without* starting the
                                                client. If the Emacs daemon is already running
                                                the command prints a message and exits with
                                                the exit code 1.

                                              - To explicitly check if the Emacs daemon for
                                                the default socket is running use the
                                                ``ec --check-daemon`` command.
                                                To check for the Emacs daemon that uses a
                                                named socket use the same command and specify
                                                the ``--name=NAME`` option.
= =========================================== ================================================

PEL also provides a `quick installation script`_ to do it.

.. _e:  ../bin/e
.. _ge: ../bin/ge
.. _ec: ../bin/ec
.. _quick installation script: ./install_e_ge_ec.rst.txt


Using short command names to use from the shell goes hand in hand with the
desire to make Emacs start faster. And you probably don't have to
worry about a clash with the `1970s E editor`_.

.. _1970s E editor: https://en.wikipedia.org/wiki/E_(1970s_text_editor)

Start Emacs in Terminal/TTY mode
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Emacs can run directly in a terminal emulator application window to take
advantage of the fact that in general, Emacs starts faster when running in
terminal (TTY) mode than when it runs in graphics mode.


Extend available keys to Emacs running under a terminal emulator
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


However, several key combinations and `Emacs Modifier Keys`_ may not be available
by default. For instance the functions keys might be inaccessible.  In most
cases the Control key can only be used to compose the `basic ASCII control
codes`_, nothing else.

Fortunately most terminal emulator applications on Unix-like operating system
provide ways to increase the number of recognized `Emacs key sequences`_ recognized.

The following sub-sections provide more information.

.. _basic ASCII control codes: https://en.wikipedia.org/wiki/C0_and_C1_control_codes#Basic_ASCII_control_codes
.. _Emacs key sequences: https://www.gnu.org/software/emacs/manual/html_node/elisp/Key-Sequences.html#Key-Sequences


Prepare Keyboard Support for Emacs on macOS Terminal
****************************************************

The macOS provides the `Terminal built-in application`_.
Several important keys used by PEL are lacking from the default Terminal key
settings but the can be added via the Terminal Preferences by specifying
`ANSI escape codes`_ sequence for specific key combinations and to provide key
bindings to cursor keys and numerical keypad keys.

See the `macOS-terminal-settings PDF`_ for more information.

.. _Terminal built-in application: https://en.wikipedia.org/wiki/Terminal_(macOS)
.. _macOS-terminal-settings PDF: https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/macOS-terminal-settings.pdf
.. _ANSI escape codes: https://en.wikipedia.org/wiki/ANSI_escape_code


Prepare Keyboard Shortcuts on Linux Distributions
*************************************************

In general Linux-based terminal applications provide a larger number of key
sequences by default.  However, several function keys, such as the **F11** key
are often reserved by either the terminal application or the OS windowing
system.

But they can be modified easily using the terminal application preferences
such as the Debian 10 Terminal Preference dialog shown below:

.. figure:: res/debian-terminal-preference.png
   :scale: 25 %


Scripts to Launch Emacs in Terminal mode
++++++++++++++++++++++++++++++++++++++++

Use the Emacs ``-nw`` command line option to start Emacs in terminal/TTY mode,
or the PEL supplied ``e`` script.

If you have not already done so, use PEL `quick installation script`_
to create the ``e`` command.

With these you will be able to open any file(s) with Emacs from the command
line, doing something like this:

.. code:: shell

          cd ~/my_development_directory
          e hello.c
          e hello.c hello.h
          e *.c

Launching graphics mode Emacs from a shell
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Under Unix-like Operating Systems like Linux and macOS when you run Emacs in
graphics mode, Emacs may not get the complete environment variables that you get
in your shell.  That happens quite often in macOS as explained by
`Steve Purcell in the readme file of his exec-path-from-shell`_ package.
His package provides a way to fix the problem.  PEL, however, does not
integrate that library because it slows Emacs startup.

PEL uses another method based on environment variables and described in
`Identify Types of Emacs Processes`_ and its sub-sections.

The method promoted by PEL requires setting up a small shell (or Window
command) script that sets up an environment variable identifying that Emacs
runs in graphics mode.

There are several advantages to that method:

- Emacs starts up faster,
- the graphical Emacs inherits the complete environment of the shell from which
  it is launched, without having to add yet another Emacs package (remember
  that as the number of Emacs external packages increases so does Emacs startup time),
- you can launch several instances of graphics Emacs, from the same or different
  shells, where different shells may have different values for important
  environment variables, and that might include different versions of important
  programming languages related yo your project.
- inside the script you can set environment variables to identify the fact
  that Emacs is running in graphics mode.  That's only necessary when Emacs 27
  or later is used with an early-init.el file and you need to set up something
  differently for Graphics mode during the execution of early-init.

PEL provides a fully working `ge`_ shell inside  the `pel/bin directory`_
that can be used under macOS and Linux.

If you have not already done so, use PEL `quick installation script`_
to create the ``ge`` command.

With these you will be able to open any file(s) with Emacs from the command
line, doing something like this:

.. code:: shell

          cd ~/my_development_directory
          ge hello.c
          ge hello.c hello.h
          ge *.c



.. _pel/bin directory: ../bin
.. _ge:                ../bin/ge

.. _Steve Purcell in the readme file of his exec-path-from-shell: https://github.com/purcell/exec-path-from-shell#readme
.. _Steve Purcell's exec-path-from-shell:                        https://github.com/purcell/exec-path-from-shell

.. ---------------------------------------------------------------------------

Identify Types of Emacs Processes
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

PEL distinguish 3 types of Emacs process modes:

#. GUI launched Emacs running in graphics mode.  Examples of this is Emacs
   launched from Windows Explorer, macOS Finder, Linux file managers, etc...
#. Shell launched Emacs running in graphics mode.  When a command typed in a
   shell starts Emacs in graphics mode.
#. Shell launched Emacs running in terminal mode.

PEL also supports two ways of handling Emacs customization:

- **Unique customization file**: the usual way where the same customization
  file and external packages and their setup is used by Emacs running in
  terminal/TTY mode and Emacs running in graphics mode.
- **Dual customization files**: a mode where 2 independent customization files
  and Elpa package directories are used: one for Emacs running in terminal/TTY
  mode and a different one for Emacs running in graphics mode.

These apply to the 3 types of Emacs processes listed above.

PEL uses environment variables to distinguish the types of Emacs process during
execution and to determine if PEL uses the same or different customization
files for Emacs running terminal/TTY and graphics modes.  The environment
variables are read by PEL's code but also by the code in the ``early-init.el``
[#early-init-graphics]_ and ``init.el`` files.

Although this method requires an initial manual setup it runs quickly and does
not slow Emacs startup [#purcell]_.

PEL's method requires 2 environment variables and logic inside both
early-init.el and inside init.el.

The environment variables are used like this:

========== ========== ==============================================
Variable 1 Variable 2 Detected mode
========== ========== ==============================================
Not set    N/A        GUI launched Emacs running in graphics mode.
Not set    Set to "1" Shell launched Emacs running in graphics mode.
Set        Not set    Shell launched Emacs running in terminal mode.
========== ========== ==============================================

- **Variable 1**: identified by the ``pel-shell-detection-envvar`` user
  option.

  - The default value of the user-option is the specially reserved "_"
    environment variable used by Bash. If you do not use Bash to launch Emacs
    you will have to use something else.  In the worst case, use ``PEL_SHELL``
    and set that environment variable inside your shell startup script
    (something like ``~/.bash_profile``).

- **Variable 2**: ``PEL_EMACS_IN_GRAPHICS`` environment variable.
  This variable must be set to ``"1"`` by the shell script that launches the
  shell launched Emacs in graphics mode.  See the script examples in the
  sub-sections of `Launching graphics mode Emacs from a shell`_.


If you plan to use PEL support for package quickstart, you must use an
``early-init.el`` file that identify whether Emacs is running in graphics mode
or terminal mode using the environment variables.  PEL provides an example of
early-init.el that provides the required logic: `example/init/early-init.el`_.

.. [#early-init-graphics] Emacs 27 and later support the package quickstart
                          mechanism.  This requires setting information in the
                          file ``early-init.el``.  At the moment Emacs process
                          the content of ``early-init.el`` its graphics
                          support code has not yet been initialized and Emacs
                          Lisp code cannot detect whether it is running in
                          terminal mode or in graphics mode by calling
                          ``display-graphic-p``: that function is not
                          available at that time.  One way around this is to
                          use the ``getenv`` function to read the content of
                          an environment variables, a method PEL promotes
                          in the use of early-init.el.

.. [#purcell] PEL uses an alternative to `Steve Purcell's
              exec-path-from-shell`_ method
              which unfortunately slows Emacs startup.


Prepare using GUI-launched Emacs running in graphics mode
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

If you plan to launch Emacs from a GUI application like Windows Explorer or
macOS Finder you will find that Emacs process environment will not include
everything you need to get everything working.  As described in the previous
section PEL provides a solution to this problem, a solution that does not slow
down Emacs startup and requires you to set one or two PEL customization
user-option variables:

- ``pel-shell-detection-envvar`` to identify an environment variable whose
  presence identifies that Emacs was launched by a shell and absence
  identifies that Emacs was launched from a GUI application such as Windows
  Explorer, macOS Finder or something like that.

- ``pel-gui-process-environment`` is where you define the environment
  variables for the GUI Emacs.  You can define any environment variable name
  and value and specify whether you want PEL to use the value as-is or to
  prepend or append it to the value of variable if it exists.

You will most likely need to specify an extra set of directories to prepend to
your ``PATH`` to allow Emacs to execute some of the programs you want to use
because the OS ``PATH`` inherited by the GUI-launched Emacs is minimal and will
probably not include the directory where several tools need such as the spell
checker program (aspell, hunspell or ispell) or several compilers.

- One trick you may want to use is to create a directory where you will store
  several symlinks to the programs you need to use and put that single
  directory in the ``PATH`` specified in ``pel-gui-process-environment`` user
  option with the ``prepend`` action.
- You can add any other environment variables this way.

**To support dual independent customization files do the following:**

- Make sure that your ``init.el`` file contains the logic identified in the
  `example/init/init.el`_ file:

  - Set OPTION A: set ``pel-init-support-dual-environment-p`` to **t**
  - Set OPTION B: set ``pel-ei-shell-detection-envvar`` to the name of the
    environment variable your shell always set or the one you always set
    inside your shell startup script (something like ``PEL_SHELL``).
    Its value should be the same as what is defined by the
    ``pel-shell-detection-envvar`` user-option.

- For Emacs 27 and later, to support the package quickstart feature you must
  also create an ``early-init.el`` file that has the logic shown inside the
  `example/init/early-init.el`_ file:

  - Set OPTION A: set ``pel-early-init-support-dual-environment-p`` to
    **t**.

**To support GUI-launched GUI Emacs**

If you want to use Emacs in graphics mode launched from a GUI application
like Windows Explorer or macOS Finder you must also do the following:

- Start Emacs with PEL support in graphics mode from a shell.
- Type the ``<f11> M-s <f2>`` key sequence to open the customization buffer
  where you can set these two user-option variables.

  - Set the values of ``pel-shell-detection-envvar`` and ``environment``.
    Save the customization file.

- Restart Emacs for these to take effect.


Prepare using shell-launched Emacs running in graphics mode
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

If you plan to launch Emacs from the shell and want to use two independent
customization files, one for Emacs running in graphics and another for Emacs
running in terminal mode, you need to create a shell script that will launch
Emacs in graphics mode.  That script must set the ``PEL_EMACS_IN_GRAPHICS``
environment variable to ``1`` ( a string value).  See the script examples in the
sub-sections of `Launching graphics mode Emacs from a shell`_.


Prepare using shell-launched Emacs running in terminal mode
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Aside from invoking Emacs with the ``-nw`` command line there is nothing
special to do for  PEL to launch a terminal-mode Emacs from a shell.

See example of scripts in the section titled `Scripts to Launch Emacs in
Terminal mode`_.

.. ---------------------------------------------------------------------------


.. ---------------------------------------------------------------------------

PEL Customization
=================

With PEL installed and built, as
described in the installation procedure above you can run Emacs and select the
packages you want to use by customizing Emacs and setting the PEL user options
to activate the packages you want to use.


Activate PEL Features - Customize PEL
-------------------------------------

You customize PEL by using the flexible `Emacs easy customization`_ system.  PEL
controls the activation of external packages and their key bindings via a set
of customize variables (also called *user options*) that have names that start
with ``pel-use-``, the `PEL use variables`_.


PEL Use Variables
~~~~~~~~~~~~~~~~~

PEL controls activation of packages via customization user option variables that
have a name that starts with ``pel-use-``.  The number of these user option
variables grows as PEL evolves.

**Note**

    If you prefer installing an external package yourself, instead of letting PEL
    install it for you, install that package before setting the corresponding
    ``pel--use-`` user option.

To get the list of these user options, use Emacs completion when executing the
``describe-symbol`` command: type ``<f1> o`` followed by ``pel-use-`` and the
tab key.  Emacs will show the available list of user options that have a name
that starts with ``pel-use-``.  It will look like this:

The following table contains the list of the ``pel-use-`` user options
currently available.

.. figure:: res/pel-use-completion.png
   :scale: 40 %

If you search ``pel-use-`` in a customization buffer, Emacs will also list all
corresponding user options in alphabetical order. The following screen shot
shows the buffer split in 2 windows synchronized with the follow-mode:

.. figure:: res/pel-use-cfg.png
   :scale: 50 %

.. _Elixir programming language: https://en.wikipedia.org/wiki/Elixir_(programming_language)
.. _Julia Programming language:  https://en.wikipedia.org/wiki/Julia_(programming_language)
.. _LFE (Lisp Flavored Erlang) programming language: https://en.wikipedia.org/wiki/LFE_(programming_language)
.. _Emacs-libvterm vterm: https://github.com/akermu/emacs-libvterm

There are several ways to customize PEL and key sequences to access the
various customization buffers.

With a global view - use the PEL customization tree
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

At first, the easiest way to learn PEL customization of various features is to
use the customization browser on PEL tree.  You can then inspect each group
one by one and activate what you want to use.

To do that, type ``<f11> <f2> P B`` or type ``M-x pel-browse-pel``.
This will open the customization tree at to root of PEL.  You can split the
window into 4 vertical windows using ``C-x 3`` tree times, make them all the
same size with the ``balance-window`` command bound to ``C-x +`` or with the
PEL ``<f11> w s =`` key binding.  Use `Emacs follow-mode`_ on the 4 windows to
make them all display the consecutive content of one buffer (the ``*Customize
Browser*``).

When you click on an option link, Emacs opens another buffer from where you
can select the value for the customize variable (also called *user-option*).
Make your selection and then click the **Apply and Save** button to register
your selection inside the customization file.

Here's a screen capture of that activity:


.. figure:: res/pel-start-customizing.png
   :scale: 25 %

To learn more about Emacs customization, read the `Customization PDF`_ and the
various documents identified by the links it contains.

You can open that PDF via the ``<f11> ? p>`` key sequence.  This opens a
prompt at the bottom of the Emacs window.  This prompt, like several other,
supports tab-completion.  Type ``cust``, then the tab key which will complete
it to ``customize`` then hit the return key to open the local copy of the PDF.
Use the same key sequence with a `prefix command argument key`_ like ``C-u``
or ``M--`` to open the PDF file inside your default or Firefox browser instead
(see [#firefox]_).

You can also open that specific PDF file with its dedicated PEL key sequence:
``<f11> <f2> <f1>`` (and in the browser with something like ``C-u <f11> <f2>
<f1>``.)


.. _Emacs follow-mode: https://www.gnu.org/software/emacs/manual/html_node/emacs/Follow-Mode.html
.. _prefix command argument key: https://www.gnu.org/software/emacs/manual/html_node/elisp/Prefix-Command-Arguments.html

Access PEL Customization Root
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

You can access the same information from the customization buffer of the top
level PEL group by executing the ``pel-cfg`` command by typing the ``<f11>
<f2> P !`` key sequence.  When running Emacs in graphics mode, you'll see
something like what is shown in the window in the top left corner of the
following screen shot:


.. figure:: res/pel-cfg.png
   :scale: 25 %

Emacs opens a buffer in ``Custom-mode`` and shows the top level of PEL
configuration.  Click on a subgroup link to open its customize buffer. The
customize buffer for the ``Pel Package Use`` is shown above in the right hand
side window.  The above screen shot also shows the entire PEL customize tree
in a customize browser buffer in the middle window.

All PEL package control user options have a name that starts with ``pel-use-``
and they are part of the ``pel-package-use`` customization group.  If you select
that group Emacs will open it and you will see something like the buffer shown
on the right hand side window above.

If you want to see all ``pel-use-`` variables, you can also type ``pel-use-`` in
the field to the right of the **Search** button and press that button.  Emacs
will list all ``pel-use-`` user option variables by alphabetical order, as shown
below.  Set the ones you want to activate.  Then save your configuration and
restart Emacs.

The following show a lot of options **on**.  Most of them are turned off by
default when you first get PEL.  Turn them on, save the customization and
execute ``pel-init`` or restart Emacs to activate them.  When you restart
Emacs, some more packages might be automatically downloaded when required.

Note:  In Emacs Lisp the value **t**, is the symbol for truth and **nil** is
used for the empty list and represent falsehood.

You can repeat the operation several times.  If you saved the customization, you
can exit Emacs: the new features will be available the next time you start it.

**Links to PDF Documentation in Customization Buffers**

PEL includes links to the PDF *reference* sheet files relevant
to the PEL customization group inside the customization buffer.
You can open your local PDF file by clicking on
the button to the right of the "*See also*" note as shown here:

.. figure:: res/pel-link-to-pdf.png
   :scale: 30 %


Identify PEL User Option
~~~~~~~~~~~~~~~~~~~~~~~~

PEL controls download, activation and configuration of Emacs external packages
and features via PEL user options:  the main ones are the `PEL Use
Variables`_, but PEL also defines other user-options for several of these
features.

The PEL user options are named after the package or feature they
control and are members of customization group organized by topics and
hierarchies.


Identify PEL user option via PEL Topic PDF
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

PEL provides an extensive set of  `PDF topic-oriented reference sheets`_. All
of these PDF files are identified inside the top-level `PEL Index PDF`_.  Each
PDF file describes a specific topic:

- an Emacs specific topic like navigation commands, Emacs buffers, Emacs
  windows, etc..
- support for specific programming language, markup language, etc...
- PEL specific conventions.

The PDF files identify the PEL user option that activate and control a
specific feature.  External packages are marked with the 📦 icon and PEL user
options with the 🛃 icon.

For example, if you want to add or control a feature related to navigation start looking
into the `⅀ Navigation PDF`_.  You can open it via the ``<f11> ? P`` key
sequence and type navigation.  You can also open it by opening the `PEL Index
PDF`_ with a browser that renders PDF files (see [#firefox]_) and click on the `⅀ Navigation`_
link there.

Identify PEL User Option by Name
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Once you know the name of the PEL user option or its customization group you
can use Emacs commands to open the customization buffer where you can see the
value of the user option and change it.  The following commands are useful:

- Use the **customize-option** command via ``M-x customize-option`` or through the PEL
  key binding ``<f11> <f2> o`` to open the
  customize buffer for the corresponding user option variable.
- Use the **customize-group** command via ``M-x customize-group`` or through the PEL
  key binding ``<f11> <f2> g`` to open the
  customize buffer for the corresponding group of user option variables.

See the `⅀ Customize`_ PDF for more information on Emacs customization and all
PEL commands that provide quick access to the customization buffers.


Navigating PEL User Option Groups
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Emacs customization information is organized as a tree of customization
group which is mainly organized by topic.  A group can be the child of more
than one group.  A user option variable can be the member of several groups as
well. PEL takes advantage of that capability to provide access the user
options from different, related, topics.

The PEL customization group is a child of Emacs Convenience customization
group.  Browse Emacs customization tree from the top with the ``<f11> <f2> B``
key sequence or from the top of the PEL customization group with
``<f11> <f2> P B``.  Click on the ``[+]`` nodes to expand them down to the
topic of interest.  This works with Emacs running in graphics mode as well as
in terminal mode.

See sections `With a global view - use the PEL customization tree`_ and
`Access PEL Customization Root`_ for examples of the way this looks.

.. -----------------------------------------------------------------------------

PEL Key Bindings
================

Emacs makes extensive use of `key prefixes`_.

PEL key bindings mostly use function keys as key prefixes:
the **F6**, **F7**, **F8**, **F11** and **F12** keys
are all PEL prefix keys:

- **F6** is a prefix for some often used commands.
- **F7** is used to access all PEL `Hydra heads`_.
- **F8** is used as the prefix for the `Projectile Project Interaction
  Manager`_ commands.  It is available when the ``pel-use-projectile`` user-option
  is active and the key is made available after activating the projectile-mode
  which can be done with the ``<f11> <f8> <f8>`` key sequence.
- **F11** is PEL's main global prefix key.  It is always available and
  provide access to most of the PEL key maps.
- The **F12** key is also available in some major modes as described in the section titled
  `PEL Mode Sensitive Key-maps`_.

PEL also binds:

- **F2**, used to jump to bookmarked locations when the ``pel-use-bm``
  user-option is active.
- **F5** as the repeat key.

All PEL key prefixes are used for PEL key maps.  These all have names that
start with ``pel:``.

More information on keys in Emacs with PEL include:

- The `PEL Key Maps PDF file`_.  To open a local copy it from Emacs type
  **F11** followed by **F1** (normally identified in this manual as ``<f11>
  <f1>``).
- The `⅀ ⌨︎ Modifier Keys PDF`_ which describes Emacs modifier keys.  Open the
  local copy of that file in Emacs with ``<f11> ? p modifier-keys`` followed by
  the return key.
- The `⌨︎Keys - Fn PDF`_  that has a table that shows how function keys are
  used by PEL.  From within Emacs use ``<f11> ? p keys-fn``  to open it.
- The `⌨︎Keys - F11`_ which lists the key bindings under the **F11** key.
- From Emacs you can list keys under a the prefix key (like **F11**) in the
  ``*Help*`` buffer:  type that prefix key and then type **C-h** quickly.

As described in the `Naming Conventions`_ section the names in the binding
column that use the ``pel:`` prefix are sub key-maps.
All PEL command functions have a name that starts with the ``pel-`` name prefix.


**Note: for Emacs running in terminal (termcap) mode**

If you are using Emacs inside a terminal emulator program such as Linux `GNOME
Terminal`_ you may have to update your terminal keyboard shortcuts to free the
function keys and some other keys that PEL uses.
The section titled `How to Modify the Terminal Keyboard Shortcut Keys`_
describes how to do that under some environments.


.. _PEL Key Maps PDF file: https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/-pel-key-maps.pdf
.. _key prefixes:          https://www.gnu.org/software/emacs/manual/html_node/emacs/Keys.html#Keys
.. _⅀ ⌨︎ Modifier Keys PDF: https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/modifier-keys.pdf
.. _⌨︎Keys - Fn PDF:        https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/keys-fn.pdf
.. _⌨︎Keys - F11:           https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/keys-f11.pdf
.. _Hydra heads:           https://github.com/abo-abo/hydra#readme
.. _GNOME Terminal:        https://en.wikipedia.org/wiki/GNOME_Terminal


Key Binding Help
----------------


**Using the which-keys package**

By default, PEL configuration activates the which-key_ external package.
When ``which-key-mode`` is active Emacs displays a list of key bindings after
you type a `prefix key`_.

For example, after typing the **F11** key, wait a little and Emacs should
display something like the following at the bottom of your Emacs window,
called the echo area:

.. figure:: res/pel-which-key.png
   :scale: 30 %

This lists all keys and further key prefixes. If the list is long as it's the
case here type ``C-h`` to show a menu of possible actions and then you will see
you can type ``n`` to display the next set of key bindings.  As soon as you
hit a key related to a command the which-key window is closed.

If you type command keys fast enough the which-key window does not show up.
You can control which-key parameters through Emacs customization.  Use one of
the following commands to access which-key customization group:

- ``M-x customization-group RET which-key RET``
- ``<f11> <f2> g which-key RET``
- ``<f11> ? <f3> 5``

If the which-key_ package is not enabled, you can enable it by setting the
``pel-use-which-key`` user-option to **t**.  You can access it by opening the
customization buffer for help by typing the ``<f11> ? <f2>`` key sequence.

Once set, it must be activated: execute ``pel-init`` by typing ``M-x pel-init
RET`` to re-initialize PEL.


**Displaying keys without the which-keys package**

You can also see the list of commands without using the which-keys
package. For example, you can see PEL's use of the **F11** function key by
hitting in sequence the **F11** key quickly followed by the **C-h** key.  Emacs
will list PEL's **F11** key bindings inside the ``*Help*`` buffer.

**Open the PEL PDF reference sheet**

Open the local copy of the PDF *reference* sheet file that describes the
commands and key bindings accessible through a given key prefix by using the
**F1** key inside that key prefix.

The following table is a partial list of the key sequences you can use to open
a specific PEL PDF file.

============================= ===================== ==========================
Context                       Key sequence           Opened PEL PDF
============================= ===================== ==========================
From any buffer               ``<f11> <f1>``        `PEL Index PDF`_
From any buffer               ``<f11> ? <f1>``      `Help PDF`_
From any buffer               ``<f11> ! <f1>``      `Syntax Check PDF`_
From any buffer               ``<f11> M-c <f1>``    `Input Completion PDF`_
From any buffer               ``<f11> SPC c <f1>``  `C language PDF`_
From a C buffer               ``<f12> <f1>``        `C language PDF`_
From any buffer               ``<f11> SPC e <f1>``  `Erlang language PDF`_
From a Erlang buffer          ``<f12> <f1>``        `Erlang language PDF`_
============================= ===================== ==========================


As listed above and inside the `PEL Help Support`_, the PEL key prefix
for help and information commands is **pel:help** bound to the ``<f11> ?`` key
sequence.  To open the `HELP`_ PDF file, type ``<f11> ? <f1>``.

Most PEL key prefixes have a ``<f1>`` key that opens the PDF describing the
commands accessible through that key prefix.  This includes the PDF that
describe support for a major mode like support for programming languages.  To
access the PDF when the current buffer is using that major mode the keys
sequence is always ``<f12> <f1>``.  If you want to access the PDF from a
buffer that is not in the specific major mode type ``<f11> SPC`` and wait for
the list of major modes to appear.  Then select the key for the major mode and
then complete it with the ``<f1>`` key.


**Open the PEL PDF files in the browser**

By default, PEL opens the PEL PDF files using the PDF reader application
available on your operating system, for example Preview in macOS.

The PDF files are filled with hyperlinks to other PEL PDF files and various
topics.  Navigating through these hyperlinks with such an application may be
cumbersome.

This is why PEL provides the ability to open PEL PDF files directly in
your local browser.  There are several ways to do that:

- Type an argument key (like ``C-u`` or ``M--``) just before typing the key
  sequence. For example typing ``C-u <f11> <f1>`` or ``M-- <f11> <f1>`` opens
  the `PEL Index PDF`_ raw PDF from Github inside your browser.
- You can invert the meaning of the argument key presence such that typing
  ``<f11> <f1>`` opens the Github file in your browser and ``C-u <f11> <f1>``
  opens your local PDF file.  For that set the ``pel-flip-help-pdf-arg``
  user-option to **t**.
  You can access its customization buffer with ``<f11> ? <f2>``.


For the best user experience use a web browser that can render PDF
files inline, like the excellent Mozilla Firefox [#firefox]_ browser version
78 or later.

To force PEL to use Firefox even if your default web browser is something
else, set the ``pel-browser-used`` user-option to ``'firefox``.
You can access its customization buffer with ``<f11> ? <f2>``.


.. _prefix key: https://www.gnu.org/software/emacs/manual/html_node/elisp/Prefix-Keys.html


PEL Mode Sensitive Key-maps
---------------------------

The first element of the table in the previous section lists
the ``<f11> SPC`` special prefix.
It's the top key-map of all PEL mode sensitive key-maps.
It has several sub-maps, once for each of the major mode explicitly supported by
PEL.  Some of them are shown in the following table:

=============================== ===========================================
key                             binding
=============================== ===========================================
``<f11> SPC C``                 pel:for-C++
``<f11> SPC D``                 pel:for-D
``<f11> SPC L``                 pel:for-lisp
``<f11> SPC c``                 pel:for-C
``<f11> SPC g``                 pel:for-graphviz-dot
``<f11> SPC l``                 pel:for-elisp
``<f11> SPC p``                 pel:for-python
``<f11> SPC r``                 pel:for-reST
=============================== ===========================================


If you are editing a buffer in one of the mode explicitly supported by PEL,
the **F12** key is bound to the mode-specific prefix.
For example inside a buffer using the *elisp-mode* major mode,
typing ``<f12>`` is the same
as typing ``<f11> SPC l``.
Inside a buffer containing Python source code,
typing ``<f12>`` is the same
as typing ``<f11> SPC p``.

When the current buffer is using the ``rst-mode``
for `editing reStructuredText files`_,
the **F12** key has the following bindings and more.

=============================== ===========================================
key                             binding
=============================== ===========================================
``<f12> .``                     **pel-rst-makelink**
``<f12> g``                     **pel-rst-goto-ref-bookmark**
``<f12> s``                     **pel-rst-set-ref-bookmark**
=============================== ===========================================


However, when the current buffer uses Emacs-Lisp mode for working on Emacs Lisp
code,
the **F12** key has the following, different, bindings.


=============================== ===========================================
key                             binding
=============================== ===========================================
``<f12> .``                     **pel-find-thing-at-point**
``<f12> D``                     **toggle-debug-on-error**
``<f12> a``                     pel:elisp-analyze
``<f12> c``                     pel:elisp-compile
``<f12> d``                     pel:elisp-debug
``<f12> e``                     pel:elisp-eval
``<f12> f``                     pel:elisp-function
``<f12> i``                     **parinfer-auto-fix**
``<f12> l``                     pel:elisp-lib
``<f12> m``                     pel:elisp-mode
=============================== ===========================================

If you edit a reStructuredText file and want to use one of the commands
available in the Emacs-Lisp key-map, then you can use the longer PEL key-map
that uses the ``<f11> SPC l`` prefix.


.. _editing reStructuredText files: `PEL reStructuredText Support Utilities`_

Key Bindings Documentation
--------------------------

PEL comes with a set of tables listing and describing:

- the **standard GNU Emacs** commands and key bindings for a given
  type of activity,
- the commands and key bindings provided by PEL for the same type of activity,
- the commands and key bindings for commands provided by external packages that
  PEL supports and can download and install.

These tables are inside PDF documents; the `PDF topic-oriented reference
sheets`_.  They are listed in the `PEL Index PDF`_.

Open PEL PDF files quickly from Emacs:

- PEL provides a set of key bindings that open you local copy of
  the file (or the Github-hosted copy) inside most key prefixes.
- For example to open your local copy of the `Search and Replace`_ PDF file
  that describes the search and replace features available under Emacs type
  ``<f11> s <f1>``.  To open the same file but from the Github site prefix
  these keys with ``C-u``.
- For topics such as `Narrowing`_ and `Navigation`_, that do not have a
  specific PEL key map prefix, type ``<f11> ? p``
  followed by the topic name or a portion of the name followed by tab to
  activate Emacs completion, then hit return once you selected the topic.


See the `PDF Documentation`_ section for more info on why these are PDF files.
The format of these files makes them something between a set of quick-sheets and
a full blown manual.

Each PDF file holds a table that list commands related to a specific topic and
holds overview above a list of rows on:

#. The command name with several hyperlinks to the related section of the
   GNU Emacs manuals or other appropriate resource.
#. The key bindings for that command including:

   - the standard Emacs key bindings,
   - the bindings for integrated packages,
   - the bindings specific to PEL.

#. The Emacs Lisp function form for the command, with the function name in
   bold and the arguments in Emacs help style.
#. A description of the command, with lots of the text taken directly from
   Emacs help for what relates to the interactive use of the function but also
   with extra notes and references.

Several of these documents also a list of reference table with relevant topics.
These references include hyperlinks to the relevant GNU
Emacs manuals but also to several sites devoted to Emacs including several
demonstration videos hosted on various platforms.

The tables are heavily marked up using colors and icons (actually Unicode
character symbols) to highlight various concepts. For example key bindings that
do not work when Emacs is running in terminal (TTY) mode are shown in
orange, commands that require external Emacs package are shown in blue and use the
package character (📦), etc...  The full list of conventions are listed in the
`Document Legend`_ table.

The list of tables follow below.
As PEL evolves, it will cover more topics, more
programming languages, major modes and will integrate with more of the external
Emacs packages and more tables will describe how to use them.



.. _doc/pdf github directory: https://github.com/pierre-rouleau/pel/tree/master/doc/pdf
.. _pel-pdf-spreadsheet repo:   https://github.com/pierre-rouleau/pel-pdf-spreadsheet#readme

.. ---------------------------------------------------------------------------

Control External Package Installation and Emacs Initialization Speed
====================================================================

PEL controls the download and installation of external Emacs files and
packages as well as their configuration and key bindings in some cases.

The value of the `PEL Use Variables`_ identify your configuration and the
``pel-init`` command (which runs on Emacs startup) activates what you
requested.

You can disable packages you no longer need by using the ``pel-cleanup``
command.  It moves the un-required packages off the elpa directory into the
attic directory making them invisible to Emacs.

In normal startup mode, all of this is available and works automatically.

However, if you want to speed Emacs initialization more than what is normally
possible, you can activate PEL fast startup mode.  In fast startup mode PEL
package management controlled by ``pel-init`` and ``pel-cleanup`` is no longer
available.

On Emacs 27 or later you can also activate Emacs package quickstart.

You can also decide to manage the configuration of Emacs running in terminal
mode independently from the one used when Emacs runs in graphics mode.

PEL provides commands to control these various setups.  The following
sub-sections provide more information.

PEL Initialization Command
--------------------------

When PEL is used, the init.el file calls the ``pel-init`` command after
loading the Emacs customization file.  If PEL operates in normal startup mode
the ``pel-init`` command downloads and installs any un-installed external package
identified by the activated `PEL use variable`_ .

With PEL you add a supported package by setting the corresponding `PEL use variable`_
user-option customize variable, making sure the new value(s) is stored in the
customization file, and then either:

- execute ``pel-init`` by typing ``M-x pel-init RET``, or
- restart Emacs.

For more information on customization see:

- `Customization PDF`_,
- `PEL Configuration/Customization Support`_,
- `PEL Customization`_.


PEL Cleanup Command
-------------------

As you start using more and more external packages, you will notice that Emacs
startup time increases.  PEL delays execution as much as possible, using the
same techniques you will find in other packages such as the popular
`use-package`_ and some other.  If these were not used Emacs startup time
would be much longer.  But still, for a large number of packages Emacs startup
time will still increase and that startup time might be noticeable.

PEL provides the fast startup mode to help.  But you may also have installed
several external packages you no longer need.  Identify those and reset to
their corresponding `PEL use variable`_.  Then execute the
``pel-cleanup`` command to move the un-required packages out of the ``elpa`` and
``utils`` directories and into their corresponding attic directories.

If you want to get a report of what would be removed (and not remove anything)
instead, then type something like ``C-u M-x pel-cleanup RET``.  This performs
a ``pel-cleanup`` dry run and displays a report in the *pel-cleanup* buffer.
Something like this:

.. figure::  res/pel-cleanup-dry-run.png
   :scale: 50 %

When you're OK with what is going to be remove, execute the real thing with
``M-x pel-cleanup RET``.

Remember: no file is deleted, ``pel-cleanup`` moves them into the attic
directory.  Later, you can copy them back into the elpa directory manually
before re-activating the corresponding `PEL use variable`_ and run
``pel-init`` to activate them again.  Or just activate the `PEL use variable`_
and run ``pel-init`` again to download a new copy.  This way you
keep a copy of the old version of the package in the attic directory.  If
something got broken you have the old one handy!

When using the dual independent customization then you can reduce the number
of packages used in terminal or graphics mode by identifying the ones used in
each mode.

See the list of important files in the section titled `Activate dual
independent customization`_.

It's also a good idea to place your customization file, the elpa and
the attic directory under DVCS control.

.. _PEL use variable: `PEL Use Variables`_


PEL Setup Commands
------------------

PEL provides several commands that control important aspect of Emacs
behaviour:

- `Independent customization for terminal/TTY and graphics mode`_,
- `PEL fast-startup mode`_,
- `Emacs package quickstart`_.

They are described in the following sub-sections and in the
`Customization PDF`_.

.. _Independent customization for terminal/TTY and graphics mode:  `Independent Customization for Terminal and Graphics Modes`_
.. _PEL fast-startup mode:     `Normal Startup and Fast Startup Modes`_
.. _Emacs package quickstart:  `Package Quickstart Mode for Emacs 27 and later`_

Independent Customization for Terminal and Graphics Modes
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Emacs can be used in terminal (TTY/termcap) mode and in graphics mode.
Usually Emacs customization file is used in both modes.

It may be quite useful to control the packages used by Emacs when it runs in
terminal mode and use a different set of package when Emacs runs in graphics
mode.  That way you use the mode most appropriate with the job and use only
the packages required for each modes, reducing the Emacs startup time in each
mode.

PEL controls the name of Emacs customization file and the directories where
Elpa-compliant packages and packages that are not Elpa compliant are stored:

PEL expects these files and directories to be located in the
``user-emacs-directory`` which is ``~/.emacs.d`` by default for Unix-like
operating systems.  PEL uses:

- The ``elpa`` directory (or symlink when using fast startup) to sore the
  Elpa-compliant packages,
- The ``utils`` directory to store the Emacs Lisp files that are not part of
  Elpa-compliant packages.

The PEL installation instructions requires installing a PEL-compatible init.el
file which will create the necessary files and directories for normal startup
in a standard, single environment, Emacs setup.

PEL provides the extra ability to use 2 independent environment.   One
environment for Emacs running in terminal/TTY mode and another running in
graphics mode.  Each have access to its own Emacs customization file and
package directories.  This feature is called the **PEL dual environment**.

In this, PEL uses the following files and directories, located inside the user-emacs-directory:

====================== =============================== =============================================
For terminal/tty | all For graphics mode in dual mode  Description
====================== =============================== =============================================
emacs-customization.el emacs-customization-graphics.el Customization data file
elpa                   elpa-graphics                   Symlink to complete or reduced directory
elpa-complete/         elpa-complete-graphics/         Stores all Elpa packages in normal startup
elpa-reduced/          elpa-reduced-graphics/          Stores reduced and pel-bundle in fast startup
elpa-attic/            elpa-attic-graphics/            Stores Elpa-packages removed by pel-cleanup
utils/                 utils-graphics/                 Stores non-Elpa external Emacs Lisp files
utils-attic/           utils-attic-graphics/           Stores non-Elpa files removed by pel-cleanup
====================== =============================== =============================================

PEL commands create the files and directories when they are required.

Comparing the two customization files with Emacs
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The easiest way to compare the terminal/TTY customization file
(``emacs-customization/.el``) from the one used in graphics mode when the dual
environment is used (``emacs-customization-graphics.el``) is to open the 2
files, each one in its own buffer window and show only these 2 windows.

Then execute the ``pel-ediff-2files`` command by typing the PEL ``<f11> d 2``
key sequence.

Type ``?`` to display ediff help and the commands to navigate through the
files and their differences.  You can copy one set of changes from one file to
the other this way.  It's a quick way to duplicate customization and also a
good way to review the recent changes to your customization.

Check customization state
^^^^^^^^^^^^^^^^^^^^^^^^^

The command ``pel-setup-info-dual-environment``, bound to the ``<f11> <f2> ?``
and to the ``<f11> ? e <f2>``
key sequences, displays the name of the customization file or files used by PEL.
If the symbol ``pel-init-support-dual-environment-p`` is set to **t**,
requesting the use of two independent customization files it also checks if
the environment is properly configured to support two independent
customization files and reports all detected problems.


Activate dual independent customization
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Use the ``pel-setup-dual-environment`` command, bound to the ``<f11> <f2>
M-d`` key sequence, to configure your Emacs environment to support two
independent customization files.  The command will prepare all required files
and directories.  It will then perform a final check and will report any
remaining problem.

Supporting two independent customization requires creating 2 sets files and
directories for storing the Elpa-compliant packages, since PEL selects the
active packages through customization: PEL uses the ``pel-use-...``
user-options customization variable and their values are stored inside the
customization files.

PEL also support the fast-startup mode which also uses two directories to
store the Elpa-compliant packages and a symlink to the one that is used.

When properly setup for dual independent customization and for fast-startup
support, the Emacs user directory (normally ``~/.emacs.d`` unless configured
otherwise) contains the files and directories list in the following table.

====================== =============================== =============================================
For terminal/tty | all For graphics mode in dual mode  Description
====================== =============================== =============================================
emacs-customization.el emacs-customization-graphics.el Customization data file
elpa                   elpa-graphics                   Symlink to complete or reduced directory
elpa-complete/         elpa-complete-graphics/         Stores all Elpa packages in normal startup
elpa-reduced/          elpa-reduced-graphics/          Stores reduced and pel-bundle in fast startup
elpa-attic/            elpa-attic-graphics/            Stores Elpa-packages removed by pel-cleanup
utils/                 utils-graphics/                 Stores non-Elpa external Emacs Lisp files
utils-attic/           utils-attic-graphics/           Stores non-Elpa files removed by pel-cleanup
====================== =============================== =============================================


The first column identifies the file, symlink and directories used when Emacs
operates normally (with no dual environment as it's normally the case for
Emacs) or when it operates in terminal/TTY mode.

The second column identifies the file, symlink and directories used when PEL
is used in dual customization files and Emacs runs in graphics mode.

When you first install PEL only some of what is identified in the first column
is created, requested by the PEL installation instructions:

- ``elpa``; the directory that stores the Elpa-compliant package directories.
- ``utils``; the directory where single files from non-Elpa compliant packages are
  stored.

The ``pel-cleanup`` command creates the ``attic`` directories when it needs to
remove packages from Emacs sight.

If you want to use PEL fast startup mode, and execute the ``pel-setup-fast``
command to activate it, PEL renames the ``elpa`` directory to
``elpa-complete``, creates the ``elpa-reduced`` directory to hold the
Elpa-compliant directories that have several sub-directories and the
``pel-bundle`` sub-directory that holds the Emacs Lisp files of all  single
directory packages.  Then PEL creates a symlink with the same ``elpa`` name
and make it point to ``elpa-reduced``.  Later when the ``pel-setup-normal``
command is used, the ``elpa`` symlink is made to point to the
``elpa-complete`` directory.

When dual independent customization is used the same set of file, symlink and
directories are created for the graphics specific mode.  They all have the
same name with the additional ``-graphics`` suffix.

Normal Startup and Fast Startup Modes
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Once you have installed and configured all external packages you need you may
find that Emacs startup time has increased too much for your liking.  That
will be the case if you use a large number of external Elpa-compliant packages
because Emacs must process the autoloads package information inside each of
the elpa package directory and that takes time.  PEL uses several techniques
to reduce Emacs startup time.  As the number of package used grows you these
techniques may not be sufficient.

To speed the startup further, PEL provides a **fast-startup mode** of operation.
In that mode PEL bundles the Emacs Lisp code files of all single directory
packages inside a single package directory (the pel-bundle *pseudo package*)
and forces Emacs to use it as if it was a single package.  Doing this reduces
the number of packages Emacs see, which reduces the length of Emacs load-path
and reduces the startup time substantially.

PEL provides the following 3 commands to deal with this:

- **pel-setup-info**, bound to ``<f11> M-S ?`` and to ``<f11> ? e M-S`` key sequences.  It displays the current
  operation mode and also check if the emacs directory layout is appropriate
  for PEL fast startup mode.
- **pel-setup-fast**, bound to ``<f11> M-S f``.  This commands reorganizes
  the content of your ``user-emacs-directory`` to bundle the elpa external
  packages to provide a faster Emacs init startup time.  In this mode you
  cannot add new external packages though.
- **pel-setup-normal**, bound to ``<f11> M-S n``.  This command restores
  the content of your ``user-emacs-directory`` the way it was, allowing you to
  use Emacs as before and with the ability to add or remove external packages.

These commands are described in the `Fast Startup PDF Sheet`_.

The Emacs initialization speedup you will experience depends on several
factors:

- One main factor is the number of Elpa-compliant packages that PEL can
  bundle.  PEL will be able to bundle all those packages that put all their
  files inside a single directory.  PEL will then build a single
  pel-bundle-autoloads.el file and one pel-bundle-pkg.el for all of these
  packages.  By doing so, and by adding extra code to make the whole thing
  work, by delaying package initialization in the init.el file, PEL reduces
  Emacs load-path and overall startup processing.

- Another significant factor is the init.el code. The execution of
  ``package-init`` must be delayed.  See the file `example/init/init.el`_
  for a setup that properly supports PEL fast-startup.

It's possible to reduce the startup time down such that benchmark-init report
it to be 0.1 second, even with a relatively large number of external package.

Speedup Examples
^^^^^^^^^^^^^^^^


238 external packages in about 0.1 second
+++++++++++++++++++++++++++++++++++++++++

- Startup time:

  - with PEL in normal startup mode: about 0.6 second
  - with PEL in fast startup mode:  about 0.1 second

- Environment:

  - 238 external packages.
  - Computer: 2014 iMac computer with 4GHz Intel Core i7 CPU and Flash storage
    memory running macOS Mojave.
  - Emacs 26.3 running in terminal mode inside a macOS Terminal window running Bash.

The following screen shots show the `benchmark-init`_ reports for Emacs
running in this environment in normal startup mode and in fast-startup mode.

Screen Shot #1: Emacs 26.3 in terminal mode using 238 external packages in
normal mode exhibiting a 0.6 second startup:

.. figure::  res/normal-startup-001.png
   :scale: 50 %

Screen Shot #2: With the same setup as above but now running under PEL's
fast-startup operation mode: Emacs startup time is now around 0.1 second.

.. figure::  res/fast-startup-001.png
   :scale: 50 %

.. _Fast Startup PDF Sheet: https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/fast-startup.pdf


Measuring Emacs Startup Time
^^^^^^^^^^^^^^^^^^^^^^^^^^^^


Using benchmark-init
++++++++++++++++++++

If you want to know the time each loaded file takes during Emacs initialization
time you can use the benchmark-init_ package. This is not controlled by PEL
because it must be launched as as early as possible inside your init.el file.

To install it type ``M-x list-packages`` then hit the return key to get a list
of all elpa-compliant packages. Search for ``benchmark-init``, select it and
install it.  You can also type: ``M-x package-install benchmark-init``.

After installing it, move the .el and .elc out of the ``benchmark-init-...``
sub-directory of ``~/.emacs.d/elpa`` and copy them inside the
``~/.emacs.d/utils`` directory.  Then remove the entire ``benchmark-init-...``
directory from ``~/.emacs.d/elpa``.

We don't want to add yet another package to the elpa directory.  That will
slow down Emacs startup time.  By moving the code into PEL's utils directory
Emacs will be able to find it faster and we don't add an extra directory.

Then add the following code as close as possible to the top of your init.el file:

.. code:: elisp

  (require 'benchmark-init
           (expand-file-name "~/.emacs.d/utils/benchmark-init"))
  (add-hook 'after-init-hook 'benchmark-init/deactivate)

This code is present but commented out inside the file
`example/init/init.el`_.

With the above code in your init.el file, you can then execute the PEL command
``pel-show-init-time`` (or using the ``<M-S-f9>`` keystroke for it) Emacs will
open 2 buffers and will show something like this:

.. figure:: res/pel-benchmark.png
   :scale: 50 %

This is a snapshot taken on GNU Emacs running in terminal mode on a 2014 macOS
computer with PEL running with 96 packages selected by customization giving 156
lines inside the benchmark-init buffers.

Here's another snapshot taken after installing PEL on Mint 20 Linux running
inside Parallels Desktop VM under macOS host:

.. figure:: res/pel-benchmark-mint20.png
   :scale: 50 %

The next time you run **pel-cleanup** while Emacs runs in normal startup mode,
PEL will remove the benchmark-init files from ``~/.emacs.d/utils`` and place
them into the ``~/.emacs.d/utils-attic`` where you can restore them when
needed.


Package Quickstart Mode for Emacs 27 and later
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Emacs 27 introduced the package quickstart feature.  When this feature is
used, Emacs creates the file ``package-quickstart.el`` in the Emacs user
directory.  This file holds the auto-load logic extracted from all package
files.  When Emacs ≥ 27 detects the presence of this file it uses it instead
of scanning the information from all elpa package directories.  This speeds
Emacs startup.

On Emacs ≥ 27 , PEL supports the 4 different combination of setups, listed
below in decreasing order of startup time:

- normal mode
- normal mode with Emacs package quickstart
- PEL fast startup
- PEL fast startup with Emacs package quickstart

To activate Emacs package quickstart you must create a early-init.el file and
create the package-quickstart.el file using the package-quickstart-refresh
command.  PEL provides logic that takes cares of this and can create all the
files while also supporting the PEL fast startup mode and the dual independent
terminal/graphics customization.

- PEL provides a fully functional copy of early-init.el,
  `example/init/early-init.el`_ that will work with PEL’s features
  example/init directory.  PEL modifies the values of some of the ``defconst``
  variables inside the early-init.el file to control its behaviour.

  - That file name is identified by the ``pel-early-init-template``
    user-option.  If you want to create your own copy of that file and add
    your content then update the user-option with new file name.  Your file
    but you must include the code located in the file
    `example/init/early-init.el`_.  If you do not need any extra logic, leave
    the default and use the template file unmodified.

  - Use the ``<f11> M-S <f2>`` key sequence to open the appropriate
    customization buffer.

PEL provides the following 2 commands to
setup Emacs ≥ 27 environment to support package quickstart or to remove it:

- ``pel-setup-with-quickstart`` (bound to ``<f11> M-S q``) to activate
  package-quickstart and create all necessary files.  You can also use it to
  refresh package quickstart files after installing new packages (which you
  can do in PEL normal startup mode),
- ``pel-setup-no-quickstart`` (bound to ``<f11> M-S M-q``) to disable
  package-quickstart.

These commands handle the PEL fast-startup mode and the dual independent
customization for terminal and graphics mode: ``pel-setup-with-quickstart``
activates the package quickstart mechanism and refreshes the files.
``pel-setup-no-quickstart`` disables the package quickstart mechanism.

**Manual Setup May be Required**

If your Emacs customization file is not what PEL normally uses,
``emacs-customization.el`` then you will have to create your modified copy of
PEL's `example/init/early-init.el`_ and modify the value of
``pel-early-init-custom-file`` inside that file.  See the complete
instructions inside the ``User configuration`` section of the file
`example/init/early-init.el`_.


.. _example/init/early-init.el:               ../example/init/early-init.el


Update Packages and Keep Older Versions
---------------------------------------

To get a later version of package already installed do the following:

- Make sure PEL is running in normal startup mode.  Type ``<f11> M-S ?`` to
  check and ``<f11> M-S n`` to activate the normal startup mode if necessary.
- Move the package directory out of the elpa or utils directory where it resides to the
  corresponding elpa-attic or utils-attic directory.

  - These directories are located in the directory identified by
    ``user-emacs-directory`` which is often ``~/.emacs.d`` on Unix-like OS.
  - If you use independent customization for terminal and graphics mode, do
    the same for both set if the package is used in both sets.
  - After a while the elpa-attic directories will contain several older version
    of the various packages as you move them in since each Elpa-compliant
    package directory name identifies its version.

    - These older versions can later be restored by placing them back into
      their corresponding elpa directory.
    - The files in the utils directories have no version identified.  Only one
      copy is available for backup unless you use a versioning scheme when
      copying them into the attic.
  - See the table below for the list of directories.

- Exit Emacs and restart it.  It will download and install the new version of
  the packages you just moved out of the elpa and utils directories.

For upgrading external packages you do not need to perform a
pel-cleanup/pel-init cycle which would require you to modify your PEL
user-options twice.  It's faster and more convenient to simply move the
package directories as described above.


The various directories (and symlinks) you will find inside your
``user-emacs-directory`` are listed here:

====================== =============================== =============================================
For terminal/tty | all For graphics mode in dual mode  Description
====================== =============================== =============================================
elpa                   elpa-graphics                   Symlink to complete or reduced directory
elpa-complete/         elpa-complete-graphics/         Stores all Elpa packages in normal startup
elpa-attic/            elpa-attic-graphics/            Stores Elpa-packages removed by pel-cleanup
utils/                 utils-graphics/                 Stores non-Elpa external Emacs Lisp files
utils-attic/           utils-attic-graphics/           Stores non-Elpa files removed by pel-cleanup
====================== =============================== =============================================

.. ---------------------------------------------------------------------------

Emacs and PEL Files and Directories
===================================

PEL Directory
-------------

PEL is written in over 100 Emacs Lisp files stored inside the PEL directory.
That directory must be identified in your init.el file in the
``pel-home-dirpath-name`` variable.  All PEL Emacs Lisp files are
byte-compiled using make with the `Makefile`_ script.

The `pel_keys.el`_ file is byte-compiled by the `Makefile`_ script but also
when you execute the commands ``pel-setup-fast`` and ``pel-setup-normal``.
`pel_keys.el`_ is byte-compiled by these commands because these commands
modify the behaviour of Lisp macros that control external package downloading
and installation.  Normally the macros generate code that checks for the
presence of external package required by the ``pel-use-`` user-options and
force a download and installation if the package is not present. That
behaviour is disabled when PEL operates in fast-startup mode speeding up the
execution of ``pel-init`` since PEL knows that all packages required are
present because it does not support automatic download and installation in
fast-startup mode.  When returning in normal-startup mode, the macros have
code that detect presence of external packages and automatic download and
installation of external packages is re-enabled.

The `pel__hydra.el`_ is also byte-compiled dynamically by ``pel-init`` to
activate support of `Hydra`_ feature when ``pel-use-hydra`` is turned on.

No other PEL Emacs Lisp file is byte-compiled outside of the `Makefile`_ script.

.. _Makefile: ../Makefile
.. _pel_keys.el:   ../pel_keys.el
.. _pel__hydra.el:  ../pel__hydra.el

User Emacs Directory
--------------------

The User Emacs Directory is normally ``~/.emacs.d``.
With PEL that directory holds the following files and sub-directories.

=============================== =========================================================
Name                            Description
=============================== =========================================================
init.el                         Emacs initialization required for PEL.
                                Use a copy of `example/init/init.el`_.

elpa                            The directory that holds Elpa-compliant Emacs
                                external packages. When using PEL fast-startup
                                feature this is replaced by a symlink to the
                                elpa-complete directory or to elpa-reduced
                                directory.

utils                           PEL specific directory.  PEL stores Emacs Lisp
                                files that are not Elpa-compliant packages
                                here.

**PEL Cleanup Attics**          The ``pel-cleanup`` command moves un-required
                                packages out of their Elpa directory and into
                                an *attic* directory where they can later be
                                retrieved.  See `PEL Cleanup Command`_.

elpa-attic                      Directory that holds package
                                directories moved out of the elpa directory
                                by ``pel-cleanup``.

utils-attic                     Directory that holds files moved
                                out of the utils directory by ``pel-cleanup``.


**Package Quickstart**          The following files are used for the support
                                of the package-quickstart feature.  This is
                                only available for Emacs 27 and later.

early-init.el                   Emacs early initialization file where package
                                quickstart and some other PEL features must be
                                activated.
                                PEL provides the `example/init/early-init.el`_
                                which it uses by default when it activates the
                                package-quickstart feature.
                                See the section titled
                                `Package Quickstart Mode for Emacs 27 and
                                later`_.

package-quickstart.el           Emacs package quickstart file that holds the
                                autoload logic of all packages.


**PEL fast-startup**            The following files and directories are used
                                by PEL for the fast-startup and normal startup
                                modes.  See `PEL Cleanup Command`_.


elpa-complete                   Directory used when PEL fast-startup
                                feature has been initialized once.  In PEL normal
                                startup mode this stores the Elpa-compliant
                                packages that are were stored in the elpa
                                directory, and elpa is turned into a symlink that
                                points to elpa-complete.

elpa-reduced                    Directory used when PEL fast-startup
                                has been turned on.  It holds the pel-bundle
                                pseudo package and all multi-directory Elpa
                                packages.  The elpa symlink points to
                                elpa-reduced when PEL operates in fast startup
                                mode.

pel-fast-startup-init.el        File created by ``pel-startup-fast`` that acts
                                as a flag to early-init.el and init.el
                                indicating request to use the fast startup
                                mode and which identifies the name of the
                                pel-bundle directory.

**Dual Custom files**           The following files and directories are only used
                                when the dual independent customization files for
                                terminal/TTY and graphics mode is used.
                                These are all PEL specific.
                                See `Independent Customization for Terminal
                                and Graphics Modes`_.


emacs-customization-graphics.el Holds Emacs customization for Emacs running in
                                graphics mode. PEL specific.

elpa-graphics                   Symlink that points to elpa-complete-graphics
                                in normal startup mode or to
                                elpa-reduced-graphics in fast startup mode.

utils-graphics                  Directory that holds Emacs Lisp files of non
                                Elpa-compliant packages used in graphics mode.

elpa-complete-graphics          Directory that holds graphics mode specific
                                Elpa-compliant packages used when PEL operates
                                in normal startup mode.

elpa-reduced-graphics           PEL specific directory. Used when dual independent
                                terminal/TTY and graphics mode is used. Holds
                                terminal/TTY mode specific Elpa compliant
                                packages used when PEL operates in normal startup
                                mode.

elpa-attic-graphics             PEL specific directory that holds package
                                directories moved out of the elpa-graphic
                                directory by ``pel-cleanup``.

utils-attic-graphics            PEL specific directory that holds files moved
                                out of the utils-graphic directory by
                                ``pel-cleanup``.

package-quickstart-graphics.el  Package quickstart file for Emacs running in
                                graphics mode.
=============================== =========================================================

Obsolete PEL Files
~~~~~~~~~~~~~~~~~~

- The ``pel-setup-package-builtin-versions.el`` is the old name of the
  ``pel-fast-startup-init.el`` file.   If you have that file in your Emacs
  user directory, remove it and manually force the ``elpa`` and
  ``elpa-graphics`` symlink to point to ``elpa-complete`` and
  ``elpa-complete-graphics`` directory respectively.


.. ---------------------------------------------------------------------------

Updating PEL
============

After your first successful build of PEL, you can easily update PEL by pulling
from the Git depot and running make to update the byte-compile code files used
by PEL.

Execute the following commands inside the pel directory.

.. code:: shell

     cd ~/projects/pel
     git pull
     make

If you want to force byte-recompilation of all PEL code do a ``make clean``
before issuing the ``make`` command.

To use a new feature that was introduced in the new PEL code, set the
corresponding PEL activation user-option variable through the relevant Emacs
customization buffer, save the new customization data and run pel-init by
using ``M-x pel-init`` or by restarting Emacs.

As usual with PEL if new packages are identified by your new configuration
they will be downloaded, installed, byte-compiled and configured automatically.

**Updating Emacs**

When updating Emacs to a new **major** version, you need to re-compile all
Emacs Lisp files: the files from PEL but also all files from the packages you
are using. With PEL this is very simple`.
See
`How to recompile all Emacs Lisp files after updating Emacs to a different major version`_.

.. _How to recompile all Emacs Lisp files after updating Emacs to a different major version: upgrading-emacs.rst.txt

.. ---------------------------------------------------------------------------

PEL Convenience Features
========================

PEL implements a set of small utilities that complement what's already available
in standard GNU Emacs and some other packages. The code is spread into several
small files.  Each of those file is described in the following subsections.
PEL comes with a set of PDF files that describe key bindings , including the
standard GNU Emacs bindings, the bindings of the external packages integrated
by PEL and the bindings for PEL commands.  The sections below contain link to
the relevant PDF files.  The complete list of PDF files is shown in the
`Key Bindings Documentation`_ section.


PEL Abbreviation Management Support
-----------------------------------

:PDF Sheet: `Abbreviations`_.
:PEL Customization: ``pel-use-hippie-expand``.
:PEL Key Prefix: **pel:abbrev** : ``<f11> a``

PEL provides automatic activation of Hippie expansion when the
``pel-use-hippie-expand`` `user option`_ is set to **t**.  Otherwise
it defaults to Dabbrev_ expansion.
PEL also provides the **pel:abbrev** key map which provides access to some
abbreviation related commands.  PEL binds it to ``<f11> a``.

All code provided by PEL about
abbreviations
is located inside the file `pel.el`_.


.. _user option: https://www.gnu.org/software/emacs/manual/html_node/emacs/Easy-Customization.html
.. _Dabbrev:     https://www.gnu.org/software/emacs/manual/html_node/emacs/Dynamic-Abbrevs.html#Dynamic-Abbrevs




PEL Auto-Completion Support
---------------------------

:PDF Sheet: `Auto-completion`_.
:PEL Customization: ``pel-use-auto-complete``, ``pel-use-company``.
:PEL Key Prefix: **pel:auto-completion** : ``<f11> ,``

The file `pel-autocomplete.el`_ manages the activation and enabling of
auto-completion systems globally and per buffer so that you can install several
auto-completion packages and then select one of them either globally or per
buffer. The selection logic does not allow more than one auto-completion
mechanism to be used for a single buffer.

This version of PEL currently supports the following auto-completion packages:

- `Auto Complete`_
- `Company`_

It provides the following commands:

- ``pel-global-auto-complete-mode`` toggles the global Auto Complete mode
  on/off if it can.  Activation is not allowed when Company Mode is active.
- ``pel-auto-complete-mode`` toggles the Auto Complete mode for the current
  buffer if it can.  Activation is not allowed when Company mode is active
  for the current buffer.
- ``pel-global-company-mode`` toggles the global Company mode on/off if it
  can. Activation is not allowed when Auto Complete mode is active.
- ``pel-company-mode`` toggles the Company mode on/off for the current buffer if
  it can.  Activation is not allowed when Auto Complete mode is active for the
  current buffer.
- ``pel-completion-info`` shows the state of the auto completion global and
  buffer specific modes.  It displays if the packages are available and whether
  they are enabled on not.
- ``pel-complete`` performs an explicit completion using the completion mode
  enabled in the current buffer.

PEL Autosave & Backup Documentation
-----------------------------------

:PDF Sheet: `Autosave & Backups`_.
:PEL Customization: *none*
:PEL Key Prefix: *none*

PEL provides a table describing the autosave and backup commands in the
`Autosave & Backups`_ PDF file.

PEL Buffer Management Support
-----------------------------

:PDF Sheet: `Buffers`_.
:PEL Customization: *none*.
:PEL Key Prefix: **pel:buffer** : ``<f11> b``

PEL provides extra key bindings to Emacs commands that manage Emacs buffers: the
keys are bound under the **pel:buffer** key prefix (``<f11> b``).

See the `Buffers`_ PDF sheet for more information.

PEL Bookmark Management Utilities
---------------------------------

:PDF Sheet: `Bookmarks`_.
:PEL Customization: ``pel-use-bm``.
:PEL Key Prefix: **pel:bookmark** : ``<f11> '``

The file `pel-bookmark.el`_ does not contain much.  It only provides the
utility function `pel-bookmark-in-current-file-p`` which checks if a bookmark of
a given name is present in the currently edited file.  This is used in other
parts of PEL.

For supporting bookmarks PEL provides the following:

- PEL provides a set of key bindings under the
  **pel:bookmark** key prefix set to ``<f11> '`` by default.
- If the ``pel-use-bm`` user option is set to **t** PEL add bindings to
  the visible bookmark commands and binds the **F2** key to ``bm-next`` which
  moves point to the next visible bookmark. PEL sets it to support bookmarks in
  several files and moving across files.
- Also, the project provides the `Bookmarks`_ PDF table which lists several
  bookmark related functions from various sources and their key bindings.

PEL Comments Utilities
----------------------

:PDF Sheet: `Comments`_, `Cut, Delete, Copy and Paste`_, `Narrowing`_.
:PEL Customization: ``pel-use-hide-comnt``
:PEL Key Prefix: **pel:comment** : ``<f11> ;``

The `pel-comment`_ file provides a collection of commands to help manage file
comment management.

- The following commands allow you to display the strings used to control comments
  in the current buffer and change them:

  - ``pel-comment-start``  display/set the string used to start a comment.
  - ``pel-comment-middle`` display/set the string used to continue a comment.
  - ``pel-comment-end``    display/set the string used to end a comment.

- With ``pel-toggle-comment-auto-fill-only-comments``  you control whether
  automatic filling is done inside source code comments.
- The ``pel-delete-all-comments`` deletes all comments in current buffer.
  Use `narrowing`_ to reduce the area where comments are deleted.
- The ``pel-kill-all-comments`` kills all comments in current buffer.
  Each killed comment group is retained in the kill ring, as a separate kill
  ring entry.  That allows selective restoration of comments later with yank
  operations.  See the `Cut, Delete, Copy and Paste`_ document.
- When ``pel-use-hide-cmnt`` user option is **t** the `hide-comnt.el`_ file,
  written by `Drew Adams`_ is used and provides 2 commands to hide the comments
  in the buffer or just in a marked region.


.. _Drew Adams:    https://www.emacswiki.org/emacs/DrewAdams
.. _hide-comnt.el: https://github.com/emacsmirror/hide-comnt



PEL Closing and Suspending Table
--------------------------------

:PDF Sheet: `Closing and Suspending`_
:PEL Customization: *none*
:PEL Key Prefix: *none*

PEL provides the `Closing and Suspending`_ PDF table listing the Emacs commands
to close and suspend.

PEL (Input) Completion Mode Control
-----------------------------------

:PDF Sheet: - `Input Completion`_
            - `User Option Customization`_.
:PEL Key Prefix: *none*

Emacs support several input completion mechanisms.  These help you enter
something at a prompt.  For example when entering the name of a file you can
type the beginning of the file name and then hit a key to complete it.  Emacs
allows this kind of input completion to be used in several context and PEL
provides you with the ability to try and select various methods for various
prompts.  You can select what you want to start with and then change it during
an editing session to adapt to the situation or to test various ways of doing
things.

Emacs has one native input completion mechanism (called Emacs default) that
kicks into action whenever you use a command that prompts for something like a
file name, a buffer name, a command name, etc...  It's a minimal input
completion mechanism that works by extending the currently typed string with
the *tab* key.  Emacs then show possible completion in a completion buffer.


Several other completion modes exist.
The IDO completion mode is shipped with Emacs.  It is very powerful and
supports several ways of filtering the suggestions. The `Input Completion`_
PDF describes the various keystrokes it supports.  It is well worth the effort
to learn these.

IDO can also be extended in various ways.  PEL supports several packages:

- that extend the way it presents available choices:

  - in a grid, originally collapsed in 1 line
  - in a grid fully extended on start
  - vertically, similar to what Ivy does (see below).

- that extend the way it searches for potential choices:

  - flx-ido

- that extend what prompt command provide IDO input completion:

  - ido-ubiquitous.

PEL also provide access to The Ivy, Counsel and Helm that are other popular
completion modes.  They can be installed via PEL and activated by PEL
customization and the mode that should be used when Emacs starts is identified
by the ``pel-initial-completion-mode`` user option.

Once more than one completion mechanism is activated, PEL provides a command
to select another completion mode: ``pel-select-completion-mode``.  PEL maps this to
``<f11> M-c``.  To see which input completion is currently active use ``<f11> ? c``.

With this you can start with Ido mode, then quickly switch to using Ivy mode for
a while and return to Ido mode when you're done. Or use whatever you want at the
moment you want without having to change and reload your Emacs initialization code.

Persistence of your choices across Emacs editing sessions is provided by Emacs
customization mechanism.
See the `Input Completion`_ and
`Customization <https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/customize.pdf>`_
PDF files for more information.

The management of completion mode is provided by the `pel-completion`_ file.



PEL Configuration/Customization Support
---------------------------------------

:PDF Sheet: `User Option Customization`_.
:PEL Key Prefix: - **pel:cfg** : ``<f11> <f2>``
                 - **pel:cfg-pl** : ``<f11> <f2> SPC``
:See Also: `Activate PEL Features - Customize PEL`_

PEL is designed to help you control Emacs through Emacs Easy Customization
system instead of explicit Emacs Lisp code.  You can always write more Emacs
Lisp code to specialize it further but in most cases it might not even be necessary.
PEL controls what package is activated and how the features are configured
through the Emacs Customization interface.  The information is stored in a file
and if you followed the instructions inside the section `How to Install PEL`_,
your selections will be stored inside the file
"``~/.emacs.d/emacs-customization.el``".  You can even copy that file and keep
multiple revision around.

Since customization is central to PEL, PEL also provides a set of key bindings
that you can use to quickly open a buffer to customize a specific user option or
a group or all of Emacs.  These key bindings use
the **pel:cfg** prefix, which by default is bound to the ``<f11> <f2>`` key sequence.

PEL customization user options are organized in Emacs customization groups with
several general groups but also groups for each programming language and markup
languages supported by PEL.  When point is inside the buffer editing a file in
one of the programming or markup language mode supported by PEL you can quickly
open the customization buffer for the group of that language by using the
``<f12> <f2>`` key.

You can also use the global prefix **pel:cfg-pl** bound to
``<f11> <f2> SPC`` followed by the character identifying the language to open
the customization group for a specific language.
For example, if you want to change the indentation style and width for your C++
files, use the command associated to the ``<f11> <f2> SPC C`` key sequence.  This
will open the PEL customization group for configuring C++ editing.  If point is
already inside a buffer in C++ mode, then ``<f12> <f2>`` does the same.

After customizing something, you can type the ``q`` key to close  the
customization buffer and come back where you were.  And to activate your changes
then run ``pel-init`` by issuing the ``M-x pel-init`` command.

Use Emacs customize data browser to see the customize data in a tree form.
Type ``<f11> <f2> M-1`` to browse PEL's customize data tree.  Here's a snapshot
of a portion of PEL's data tree:

.. image:: res/pel-custom-tree.png


See the `User Option Customization`_ *reference* sheet for key bindings and the
section titled `Activate PEL Features - Customize PEL`_ for relevant information.


PEL Counting Support
--------------------

:PDF Sheet: `Counting`_.
:PEL Customization: *none*
:PEL Key Prefix: **pel:count** : ``<f11> c``

PEL provides the **pel:count** key prefix (``<f11> c``) to Emacs commands that
count text and display results in the echo area.


PEL Cross Reference Support
---------------------------

:PDF Sheet: `Cross-Referencing`_
:PEL Customization: *none*
:PEL Key Prefix: **pel:xref**

The file `pel-xref.el`_ holds utilities related to Etags based cross-reference
support and other methods.

Tag-based systems
~~~~~~~~~~~~~~~~~

Emacs supports several tag based systems back-ends for the creation,
management and use of those tags for cross referencing purposes. The supported
system include:

- GNU ctags (ctags).
- Universal CTags (ctags) , the successor of Exuberant CTags.
- Emacs Etags (etags).
- GNU GLOBAL gtags.

The following sub-sections describe how to install, setup and use those.

GNU GLOBAL source code tagging system - gtags
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The `GNU GLOBAL gtags homepage`_ describes this powerful system.  It combines
the capabilities of `Universal Ctags`_, `Pygments`_ with its own logic when to parse
and tag a large set of programming languages.  To use it effectively you must
therefore install 3 programs and do a small script to configure them. You must
also have Python installed since `Pygments`_ is written in Python.

**Installation Instructions**

- Install Python 3.5 or later if it is not already available on your platform.

  - For macOS, you can follow the `Installing Python on macOS instructions`_
    on my about-python_ documentation project.

- Install GNU GLOBAL

  - For macOS, use: ``brew install global``.
  - On Fedora Linux, use: ``sudo dnf install global``
  - On Mint Linux, use: ``sudo apt install global``
  - On Ubuntu Linux, use: ``sudo apt install global``
  - etc ...

- Install Universal-Ctags (it will replace GNU ctags):

  - for macOS, use: ``brew install universal-ctags``
  - On Fedora Linux, use: ``sudo dnf install universal-ctags``
  - On Mint Linux, use: ``sudo apt install universal-ctags``
  - On Ubuntu Linux, use: ``sudo apt install universal-ctags``
  - etc ...

- Install Pygments:

  - Use: ``pip install pygments``

**Create a script to activate the Pygments plugin of GNU GLOBAL**

Store a copy of the `envfor-gtags`_ shell script inside one of your
directories.  Ideally a directory on your PATH but that's not absolutely
necessary.

Then, to activate GNU GLOBAL Pygment/Universal-CTags plugin source that script
in the shell you want to use gtags in.

For example you could store the script inside a ``~/bin`` directory and then
use::

     . envfor-gtags

if ``~/bin`` is on your PATH, or::

     . ~/bin/envfor-gtags

if ``~/bin`` is not on your path.

If you want to provide access to global in all shells, you can place this
command in your startup script (e.g. ``~/.bash_profile`` for Bash).

.. _GNU GLOBAL gtags homepage: https://www.gnu.org/software/global/
.. _Pygments: https://pygments.org
.. _Universal Ctags: https://ctags.io
.. _Installing Python on macOS instructions: https://github.com/pierre-rouleau/about-python/blob/master/doc/installing-python.rst
.. _about-python: https://github.com/pierre-rouleau/about-python
.. _envfor-gtags: ../bin/envfor-gtags

PEL Utilities that build TAGS File
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

PEL provides a set of POSIX-compliant shell scripts that can be
used to find the source code files inside specified directory trees and build
TAGS for a given set of programming languages.

More information on it is available in the page titled
`Using ctags and etags for cross referencing with TAGS`_.


.. _Using ctags and etags for cross referencing with TAGS:  ./cross-reference-with-ctags.rst.txt#the-ctags-programs



PEL CUA Mode Extension Utilities
--------------------------------

:PDF Sheet: `CUA <https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/cua.pdf>`_
:PEL Customization: *none*
:PEL Key Prefix: *none*
:Status: Experimental

**Note:**
   🚧  While CUA Mode itself is a stable Emacs feature, PEL's additions are experimental.

I'd like to find ways to easily manage rectangles of text without having to
activate the CUA mode and
the file `pel-cua.el`_ holds some experimental and unfinished code for going in
that direction.  Some of the commands are bound to PEL keys and described in the
PDF tables. But this work is in very early stage.


PEL Cursor Management Support
-----------------------------

:PDF Sheet: `Cursor`_
:PEL Customization: - ``pel-cursor-overwrite-mode-color``
                    - ``pel-cursor-type-when-mark``
                    - ``pel-use-multiple-cursors``
                    - ``pel-use-visual-regexp-steroids``
                    - ``pel-use-visual-regexp``

:PEL Key Prefix: *none*

The `pel-cursor.el`_ file the logic required to control the cursor color and
type when Emacs is running in graphical mode.

- With the ``pel-cursor-overwrite-mode-color`` user option, you can select a color
  different than what is normally used by the cursor to change cursor color when
  the overwrite-mode is active.
- With ``pel-cursor-type-when-mark`` you can set a different cursor type
  (shape) used when the mark is active.

When ``pel-use-multiple-cursors`` is set to **t** the popular
`multiple-cursors`_ mode is made available and PEL provides a set of key
bindings for this.  The ``pel-use-visual-regexp`` and
``pel-use-visual-regexp-steroids`` activate the ability to perform a search
which yields to multiple cursors activated at the match locations.

See the PDF `Cursor`_ document for more information.




PEL Cut, Delete, Kill, Copy, Paste and Yank Utilities
-----------------------------------------------------

:PDF Sheet: `Cut, Delete, Copy and Paste`_, `Marking`_.
:PEL Customization: ``pel-use-popup-kill-ring``.
:PEL Key Prefix: - **pel:clipboard** : ``<f11> C``
                 - **pel:copy** : ``<f11> =``
                 - **pel:kill** : ``<f11> -``
                 - **pel:text-whitespace** : ``<f11> t w``

The `pel-ccp.el`_ file provides a collection of commands to perform Emacs
style kill/yank and otherwise copy/cut/paste operations on various parts of the
text, targeting specific syntax entities or other simpler parts.

- The following commands copy the specified syntax entities at point into the
  kill ring:

  - ``pel-copy-word-at-point``
  - ``pel-copy-symbol-at-point``
  - ``pel-copy-sentence-at-point``
  - ``pel-copy-function-at-point``
  - ``pel-copy-sexp-at-point``
  - ``pel-copy-whitespace-at-point``
  - ``pel-copy-filename-at-point``
  - ``pel-copy-url-at-point``
  - ``pel-copy-list-at-point``
  - ``pel-copy-paragraph-at-point``
  - ``pel-copy-paragraph-start``
  - ``pel-copy-paragraph-end``
  - ``pel-copy-line-start``
  - ``pel-copy-line-end``
  - ``pel-copy-char-at-point``
  - The command ``pel-copy-marked-or-whole-line`` copy a marked region if any or
    the entire line (including the line termination) into the kill ring.

- The following commands kill the specified syntax entities at point:

  - ``pel-kill-word-at-point``
  - ``pel-kill-symbol-at-point``
  - ``pel-kill-sentence-at-point``
  - ``pel-kill-function-at-point``
  - ``pel-kill-sexp-at-point``
  - ``pel-kill-whitespace-at-point``
  - ``pel-kill-filename-at-point``
  - ``pel-kill-url-at-point``
  - ``pel-kill-list-at-point``
  - ``pel-kill-paragraph-at-point``
  - ``pel-kill-char-at-point``
  - ``pel-kill-from-beginning-of-line``
  - The command ``pel-kill-or-delete-marked-or-whole-line`` is a flexible command
    that can kill or delete the current line, multiple lines or the currently marked
    region.

- The following commands delete text at point and don't store the text in the
  kill ring:

  - The ``pel-delete-whole-line`` command delete the current line, including the
    line termination.
  - The ``pel-delete-to-next-visible`` delete all whitespace characters between
    point and the next non-whitespace character.

- The ``pel-mark-whole-line`` marks the complete current line excluding the line
  termination.


PEL Diff and Merge
------------------

:PDF Sheet: `Diff and Merge`_.
:PEL Customization: ``pel-use-ztree``
:PEL Key Prefix: - **pel:diff** : ``<f11> d``
                 - **pel:ediff** : ``<f11> e``

                   - **pel:ediff-buffer**  : ``<f11> e b``
                   - **pel:ediff-dirs**    : ``<f11> e d``
                   - **pel:ediff-files**   : ``<f11> e f``
                   - **pel:ediff-merge**   : ``<f11> e m``
                   - **pel:ediff-patch**   : ``<f11> e p``
                   - **pel:ediff-regions** : ``<f11> e r``

PEL provides key bindings to the following software packages that you may use
for compare 2 files, and perform a 2-way merge (comparing 2 files and moving
text from one file to the other) and a `3-way merge`_ (such as what's used
when merging with your preferred DVCS):

- Emacs built-in diff_ package that supports simple diff,
- Emacs built-in ediff_ package, a more powerful package capable of performing
  interactive 2-way and 3-way merge operations.



.. _diff:        https://www.gnu.org/software/emacs/manual/html_node/emacs/Comparing-Files.html
.. _ediff:       https://www.gnu.org/software/emacs/manual/html_node/ediff/index.html
.. _3-way merge: https://en.wikipedia.org/wiki/Merge_(version_control)#Three-way_merge


Merging Files with ediff-merge
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Merge the content of two different files inside a ``*ediff-merge*`` buffer
with the **ediff-merge** command (bound to ``<f11> d e m f`` command.

The following example shows the merging of two different versions of the file
``pel--base.el`` stored into 2 different directories.

- First issue the command and identify the 2 different files to the command
  prompt.
- When running Emacs in terminal mode the ``ediff-merge`` will show its menu
  at the end or a message stating that you can type **?** to show it. In
  graphics mode the ``ediff-merge`` menu may show up inside a different,
  floating frame.
- The command shows 2 windows at the top, with file A on the left side and
  file B on the right side.  The ``*ediff-merge*`` buffer that will hold the
  result of the merge is show under these two windows, above the
  ``ediff-merge`` ``*Ediff Control Panel*`` window.

.. figure:: res/ediff-merge-00.png
   :scale: 50 %

- Type **n** or Space to move to the next difference.  The select which of the
  text to use in the merged: type **a** to take text from A or **b** to take
  text from the B file.

The snapshot shows the first difference.  The merge buffer shows the content
of file A as ``variant A`` and the content of B as ``variant B`` in the
yellow highlighted area.

.. figure:: res/ediff-merge-01.png
   :scale: 50 %

After typing **b**, the content of file B is copied into the merge, as shown here:

.. figure:: res/ediff-merge-02.png
   :scale: 50 %

Type **n** to move to the next difference.

.. figure:: res/ediff-merge-03.png
   :scale: 50 %

Continue until you reach the end of the files. When you're done, type **q** to
terminate the ``ediff-merge`` session.  The result of the merge is inside the
``*ediff-merge*`` buffer.  You can then save the content of the buffer into a
file.

During the merge session you can decide to go back to a previously merged area
and make a different choice.  Emacs will prompt to confirm and will merge in
you new test selection.

Merge Files with ediff-merge-with-ancestor
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The ``ediff-merge-with-ancestor`` prompts for the name of 3 files, file A and
B and then file C, the ancestor of those files.  It opens four windows showing
file A and B at the top, and then showing the result of the merge inside the
``*ediff-merge*`` buffer on the right and the content of the ancestor file on
the right.

The process is very similar to the use of ``ediff-merge`` described in the
previous section.

After issuing the ``ediff-merge-with-ancestor`` command
(bound to ``<f11> e d m F``), Emacs shows the 4 buffers: file A, File B on the
top and the ``*ediff-merge*`` buffer and the ancestor file at the bottom.  The
``Ediff Control Panel*`` buffer is the current buffer.

.. figure:: res/ediff-merge-wa-00.png
   :scale: 50 %

Press **?** to bring the help menu.

.. figure:: res/ediff-merge-wa-01.png
   :scale: 50 %

Press **n** to move to the next difference to merge.  Emacs highlights the
areas in all buffers.  Press **a** or **b** to select the change from file A
or B.

.. figure:: res/ediff-merge-wa-02.png
   :scale: 50 %

Press **n** to go to the next change.

.. figure:: res/ediff-merge-wa-03.png
   :scale: 50 %

Next is the result shown after pressing **b** once. Notice that the content of
file B was placed inside the ``*ediff-merge*`` buffer on the left while the
ancestor shows the original text.

.. figure:: res/ediff-merge-wa-04.png
   :scale: 50 %

Next is the result after pressing **b** again, completing the merge.  The
original file still shows its original text.

.. figure:: res/ediff-merge-wa-05.png
   :scale: 50 %

Press **q** to complete the merge session and answer the prompt.  The result
is inside the ``*ediff-merge*`` buffer.

.. figure:: res/ediff-merge-wa-06.png
   :scale: 50 %

The changes are merged inside the ``*ediff-merge*`` buffer.  The buffer
showing the ancestor file is in read-only mode and cannot be modified.  Once
you compete the merge you can save the content of the ``*ediff-merge*`` buffer
into a file with the usual ``C-x C-s``.  Emacs will prompt for the file
destination which defaults to the name of file A, which would normally be the
local file in a DVCS merge operation.


.. figure:: res/ediff-merge-wa-07.png
   :scale: 50 %


PEL Drawing Support
-------------------

:PDF Sheet: `Drawing`_, `PlantUML-Mode`_.
:PEL Customization: - ``pel-use-plantuml``,
                    - ``pel-use-flycheck-plantuml``.
:PEL Key Prefix: - **pel:draw** : ``<f11> D``
                 - **pel:plantuml**: ``<f11> D u``

PEL provides key bindings to enter the Emacs text drawing modes:

- ``<f11> D a``: toggle artist mode.
- ``<f11> D p``: enter picture-mode.

☝️  The picture-mode can be quite useful to edit tabular data as well as editing
tables for markup languages like reStructuredText or even for lining text
vertically in any other type of file; for example lining up text vertically.

Drawing with PlantUML
~~~~~~~~~~~~~~~~~~~~~

If you need to draw UML diagram, you can use the plantuml-mode to write the
diagram in PLantUML syntax and then generate the diagram.  If Emacs is running
in graphics mode, the preview shown is an image. If Emacs is running in text
mode the preview is a text-based drawing that can easily be inserted inside a
source code file.  PEL defines the **pel:plantuml** key prefix (``<f11> D u``)
for the PlantUML-mode commands.  See the `PlantUML-Mode`_ PDF document for more
information.

PEL activates support for PlantUML with the plantuml-mode when the
``pel-use-plantuml`` user options is either set to **t** or to **server**.
When set to **t** you use a local instance of the PlantUML Java application.
You need to install PlantUML.  If set to **server** Emacs communicates with a
remote PlantUML server to crete the image.  Your data is sent to that external
server, so make sure you set this to what you need and do not sent proprietary
information across the Internet by mistake!


PEL Enriched Text Support
-------------------------

:PDF Sheet: `Enriched Text`_.
:PEL Customization: *none*
:PEL Key Prefix: **pel:textmodes** : ``<f11> t m``

PEL `Enriched Text`_ PDF table shows the Emacs commands available for
enriched text.



PEL File Management Utilities
-----------------------------

:PDF Sheet: `File Management`_, `Dired`_.
:PEL Customization: - **pel-pkg-for-completion**:

                      - ``pel-use-ido-mode``.

                    - **pel-pkg-for-filemng**:

                      - ``pel-use-ffap``
                      - ``pel-use-neotree``
                      - **pel-pkg-for-ztree**:

                        - ``pel-use-ztree``
                        - ``pel-ztree-dir-move-focus``
                        - ``pel-ztree-dir-filter-list``
                        - ``pel-ztree-dir-show-filtered-files``

:PEL Key Prefix: - **pel:file** : ``<f11> f``

                   - **pel:ffap**:         ``<f11> f a``
                   - **pel:file-revert** : ``<f11> f r``
                   - **pel:filevar** :     ``<f11> f v``



The `pel-file.el`_ file provides logic to extra the name of a file or a URL from
text at point and visit (open) that file inside an Emacs buffer or launch a web
browser to load the specified URL.

- The main command is ``pel-find-file-at-point-in-window`` which opens the file
  or URL. When opening a file, the command accepts a
  wide range of numeric arguments to specify the window to use.
  When the file name is followed by a line number the point is moved at that
  line.  If the line number is followed by a column number point is moved to
  that column.  The command supports several formats.
- Two other utility commands are provided:

  - ``pel-show-filename-at-point`` which simply shows the name of the file
    extracted from point.
  - ``pel-show-filename-parts-at-point`` which displays the components extracted
    from point. It's mainly used for debugging when unexpected formats are
    encountered.

PEL also provides the ability to use the ffap (find file at
point) standard library which complements the PEL command that can also open a file or
URL at point (but can also specify a window by coordinates and handle line and
column numbers). PEL activates the special ``pel:ffap`` binding when
``pel-use-ffap`` user option is set to **t**. If you prefer the standard ffap binding, then
set ``pel-use-ffap`` user option to **ffap-bindings**.

When the ``pel-use-ido-mode`` user option is set to **t** ``pel-init``
activates IDO-mode_ everywhere, enables flex matching and prevents prompt when
creating new buffers with ``C-x b``.

.. _IDO-mode: https://www.gnu.org/software/emacs/manual/html_node/ido/index.html

In Dired mode, the normally
unassigned 'z'  key is mapped to the command ``pel-open-in-os-app``.
It opens the
file or directory at point with the OS-registered application.
The ``<f11> f f`` key sequence is also mapped to that command.
The current implementation (in `pel-filex.el`_) only supports Linux, macOS and Windows.

- On Linux, the command uses xdg-open_.
- On macOS it uses `macOS open`_.
- On Windows, it uses the Explorer open command via `Emacs Windows subprocess support`_.

*Credits:*

  Thanks to Jason Blevins for the idea taken from `his blog
  <https://jblevins.org/log/dired-open>`_ and to
  Xah Lee for ideas from
  his `Open File in External App`_ page.

To see a textual representation of a directory tree, PEL provides access to the
neotree and z-tree packages.  They are activated by the ``pel-use-neotree`` and
``pel-use-ztree`` user option respectively.

PEL provides the ``<f11> <1> f`` key binding to quickly access the
**pel-pkg-for-filemng** customization group editing buffer to modify the
relevant user options.




.. _macOS open: https://ss64.com/osx/open.html
.. _Emacs Windows subprocess support: https://www.gnu.org/software/emacs/manual/html_node/emacs/Windows-Processes.html
.. _xdg-open: https://ss64.com/bash/xdg-open.html
.. _Open File in External App: http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html



PEL File and Directory Local Variable Control Support
-----------------------------------------------------

:PDF Sheet: `File and Directory Local Variables`_
:PEL Customization: *none*
:PEL Key Prefix: - **pel:filevar** : ``<f11> f v``
                 - **pel:dirvar**  : ``<f11> f v d``

PEL provides a set of key bindings to manage local file variables and local
directory variables.

See the `File and Directory Local Variables`_ PDF table.


PEL Font Management Utilities
-----------------------------

:PDF Sheet: `Faces and Fonts`_.
:PEL Customization: *none*
:PEL Key Prefix: *none*

The file `pel-font.el`_ provides utilities to control the font size of all
windows when Emacs runs in graphics mode, to complement what already exists in
standard GNU Emacs.  The available commands are:

- ``pel-font-increase-size-all-buffers``, bound to ``<s-kp-add>``.
  On the macOS keyboard: ``⌘-+``, using the ``+`` on the keypad.
- ``pel-font-decrease-size-all-buffers``, bound to ``<s-kp-subtract>``
  On the macOS keyboard: ``⌘--``, using the ``-`` on the keypad.
- ``pel-font-reset-size-all-buffers``, bound to ``<s-kp-0>``.
  On the macOS keyboard: ``⌘-0``.

The key binding selected correspond to what is used on macOS for
manipulating the font size of the Terminal.app application when the
*super* modifier key is set to the macOS command (⌘ ) key.


PEL Frame Management Utilities
------------------------------

:PDF Sheet: `Frames`_.
:PEL Customization: ``pel-use-framemove``
:PEL Key Prefix: **pel:frame** : ``<f11> F``

The file `pel-frame-control.el`_ provides a set of utilities to manage Emacs
frames.  Emacs supports frames both when it operates in graphics mode and in
terminal (TTY) mode.  In terminal mode, you can only see one frame at a time;
all frames of one instance of Emacs running in terminal mode share the same
terminal OS window (called a frame in Emacs nomenclature).

This PEL file provides the following commands:

- ``pel-toggle-frame-fullscreen`` toggles the frame to and back full screen mode
  when Emacs is running in graphics mode.  If Emacs is running in terminal mode
  the command prints a message.  For some environments the message describes
  what must be done to toggle the terminal window to full-screen and back.
  At the moment PEL is able to describe what must be done on macOS with the
  Terminal.app and iTerm.app.
- ``pel-show-frame-count``  displays the total number of frames used by this
  instance of Emacs and the number of those frames that are currently visible.
- The following two commands work when several frames are used by Emacs whether
  it is running in graphics mode or terminal mode:

  - ``pel-next-frame`` moves point to the next frame.
  - ``pel-previous-frame`` moves point to the previous frame.


PEL Function Keys Bindings
--------------------------

:PDF Sheet: `Function Keys`_, `F11 Keys`_.
:PEL Customization: *none*
:PEL Key Prefix: *N/A*

PEL avoids remapping most standard GNU Emacs key bindings.
Instead PEL uses the following function keys:

- **F2**, bound to ``bm-next`` (from `visible bookmarks`_)
  to quickly move to next visible bookmark
  when the ``pel-use-bm`` user option is **t**.
- **F5**, bound to ``repeat``.
- **F6**, the ``pel:f6`` prefix, which provides quick access to some
  often used commands, and generic template insertion commands that work
  in all major modes.
- **F7** is used for `Hydra keys`_
  key sequences when ``pel-use-hydra`` is **t**.
- **F11**, the ``pel:`` prefix , is the main prefix key for PEL, providing
  access to a large set of key bindings and second-level key prefixes.
-  **F12** is a mode-sensitive key prefix with quick access bindings for the
   current major mode.  The **F12 F12** key sequence (normally referred to as
   ``<f12> <f12>`` in this document and PDF tables) provides access to
   specialized text template insertion in the major modes of several programming
   and markup languages.


The use of function keys and Emacs modifier keys is shown in the `Function
Keys`_ PDF table, reproduced here:

.. image:: res/fn-keys.png


The **F11** acts as a the main prefix for PEL: the prefix ``pel:``.
Several sub-prefixes are available after ``<f11>`` but also some command
bindings using other keys, like cursor keys with or without modifiers.

To easily see what's available it's best to activate the
`which-key`_ package to show the available keys following a prefix key, like the
**F11** key.  Here's what the echo area looks like after pressing the **F11**
key when `which-key`_ is installed and activated:

.. image:: res/pel-which-key.png

To install and activate it, you must set the ``pel-use-which-key`` customize
variable to **t**.  Use the ``M-x customize`` command and search
for ``pel-use-which-key``.  Set it to **t**.  The restart PEL by using
``M-x pel-init``.  PEL will download and install the `which-key`_ package
and will activate it.

.. _Hydra keys: https://github.com/abo-abo/hydra#readme


PEL Grep Support
----------------

:PDF Sheet: `Grep`_, `Projectile Project Interaction Manager`_.
:PEL Customization: - ``pel-use-ripgrep``
                    - ``pel-use-ag``
                    - ``pel-use-projectile``

:PEL Key Prefix: **pel:grep** : ``<f11> g``

PEL provides the **pel:grep** (``<f11> g``) key map to regroup grep commands.
If the ``pel-use-ripgrep`` user option is **t** that includes access to
the ``rg`` command that uses the fast ripgrep_ executable.

When ``pel-use-projectile`` is set to **t**, the `ripgrep.el`_ package is also
used because `projectile`_ uses `ripgrep.el`_ instead of the `rg`_ package.
Both provide access to the ripgrep_ executable.

When ``pel-use-ag`` is set to **t**, the `ag`_ (`ag, the silver searcher`_)  is also
available.  This is another fast grep alternative that requires the ag command
line.

You must install the ripgrep and ag command line utilities separately.


.. _ripgrep: https://github.com/BurntSushi/ripgrep
.. _ag, the silver searcher: https://github.com/ggreer/the_silver_searcher


PEL Help Support
----------------

:PDF Sheet: `Help`_
:PEL Customization: - ``pel-use-ascii-table``
                    - ``pel-use-free-keys``
                    - ``pel-use-which-key``

:PEL Key Prefix: - **pel:help** : ``<f11> ?``

                   - **pel:apropos** : ``<f11> ? a``
                   - **pel:describe** : ``<f11> ? d``
                   - **pel:emacs** : ``<f11> ? e``
                   - **pel:info** : ``<f11> ? i``
                   - **pel:keys** : ``<f11> ? k``

PEL provides a set of key bindings to request help information, bound to the
**pel:help** key prefix (``<f11> ?``) and it sub-prefixes.  Several of these
commands are accessible via standard Emacs bindings of the ``<f1>`` and the
``C-h`` keys.  There are also some other, as shown in the `Help`_ PDF table.
The customization include the ``pel-use-free-keys`` and ``pel-use-which-key``
variables.  The latter is enabled by default; it help see the available bindings
following key prefixes.

You can also open the local copy of the  PDF *reference* sheet file that
describes the commands and key bindings accessible through a given key prefix by
using the **F1** key inside that key prefix.
For example, the PEL key prefix
for help and information commands is **pel:help** bound to the ``<f11> ?`` key
sequence.  To open the `HELP`_ PDF file, type ``<f11> ? <f1>``.
Not all PEL key prefixes have this key, but most have.

PEL Hide/Show Code Block Support
--------------------------------

:PDF Sheet: `Hide/Show Code Block`_
:PEL Customization: *none*
:PEL Key Prefix: **pel:hideShow** : ``<f11> /``

Emacs provides the `Hide/Show minor mode`_ to collapse and expand blocks of
source code.  To use its commands it you have to activate the minor mode first.
PEL provides commands that automatically activates the Hide/Show minor mode and
provides easy to use key-bindings provided by the **pel:hideShow** ``<f11> /``
key-map.  The `Hide/Show Code Block`_ PDF document describes the keys and
commands provided by PEL as well as the standard Emacs commands and key bindings.



.. _Hide/Show minor mode: https://www.gnu.org/software/emacs/manual/html_node/emacs/Hideshow.html#Hideshow



PEL Highlight and Whitespace Management Support
-----------------------------------------------

:PDF Sheet: `Highlight`_ , `Whitespaces`_.
:PEL Customization: ``pel-use-rainbow-delimiters``, ``pel-use-vline``.
:PEL Key Prefix: - **pel:highlight** : ``<f11> b h``
                 - **pel:whitespace** : ``f11> t w``
                 - **pel:align** : ``<f11> t a``

The file `pel-highlight.el`_ provides the following simple utility commands.

- The following help manage current line background highlighting, useful to
  quickly identify the location of the cursor on a large display:

  - With ``pel-set-highlight-color`` you can select the color of the highlight
    line by name. Use the ``list-colors-display`` command
    (bound to ``<f11> ? d c`` in PEL)
    to list all colours and their names.
  - The ``pel-toggle-hl-line-sticky`` command toggles line highlighting
    of only the current window or all windows that hold the current buffer.

- It also provides the following whitespace management commands:

  - ``pel-toggle-show-trailing-whitespace`` toggles the highlight of trailing
    whitespaces in the current buffer.
  - ``pel-toggle-indicate-empty-lines`` toggles highlighting of empty lines.
  - ``pel-toggle-indent-tabs-mode`` toggles the use of hard tabs and whitespace
    for indentation inside the current buffer (but does *not* tabify or untabify
    existing content.) It displays what's being used now.

- When ``pel-use-vline`` user option is **t** the ``<f11> b h v`` key is bound
  to vline-mode_ which toggles a vertical bar across the current window at
  the cursor location.

.. _vline-mode: https://www.emacswiki.org/emacs/VlineMode


PEL Indentation Support Utilities
---------------------------------

:PDF Sheet: `Indentation`_.
:PEL Customization: *none*
:PEL Key Prefix: **pel:tab** : ``<f11> <tab>``

The file `pel-indent.el`_ includes some very basic utilities for simple
indentation control, complementing what is available in Emacs.
The available commands are:

- ``pel-indent-lines`` indent the current line or all lines included in the
  current marked region, regardless of the point and mark position.
- ``pel-unindent-lines`` un-indent the current line or all lines included in the
  current marked region, regardless of the point and mark position.
- ``pel-indent-rigidly`` indents the current line or marked region, this command
  extends the Emacs indent-rigidly command.

The PEL support for indentation will evolve as support form various types of
files, programming languages and markup languages evolves.


PEL Input Method Control
------------------------

:PDF Sheet: `Input Method`_.
:PEL Customization: *none*
:PEL Key Prefix: **pel:text** : ``<f11> t``

PEL rebinds the ``C-\`` prime key, normally bound to ``toggle-input-method``,
used to select another `Emacs input method`_, to ``pel-kill-from-beginning-of-line``.
PEL binds ``toggle-input-method`` to ``<f11> t i`` instead.  And to change the
alternate input method, it binds ``set-input-method`` to ``<f11> t I``.
To lists all input methods, PEL provides ``<f11> ? d i`` bound to ``list-input-methods``.


.. _Emacs input method: https://www.gnu.org/software/emacs/manual/html_node/emacs/Input-Methods.html#Input-Methods

PEL Key Chords Support
----------------------

:PDF Sheet: `Key Chords`_
:PEL Customization: **pel-pkg-for-key-chord**:

                    - ``pel-use-key-chord``
                    - ``pel-use-key-seq``
                    - ``pel-key-chords``
                    - ``pel-key-chord-two-keys-delay``
                    - ``pel-key-chord-one-key-delay``
                    - ``pel-key-chord-in-macros``

:PEL Key Prefix: *none*

PEL provides access to the `key-chord`_ external library when the
``pel-use-key-chord`` user option is set to either **t** (to activate key-chords
when the key-chord-mode is ttuned on) or to **use-from-start** (to activate the
key-chord mode and all defined global key-chords when Emacs starts).

If you set the ``pel-use-key-seq`` suer option to **t**, PEL also
provides access to the `key-seq`_ external library and allow you to identify
your *key-chord* to be a *key-seq* instead.

A *key-chord* is two characters typed simultaneously quickly or the same key
typed twice quickly that trigger a specified action.  The action may be
inserting some other text, moving the piint, executing a specified function or
executing a specified Emacs command expressed as an Emacs Lisp lambda
expression.  The *key-chord* can be made of any ASCII printable characters and
ASCII control characters.  These keys must be type quickly; the order into
which they are typed does not matter.

For some fast typist using two keys that might be inside normal words in one
order but not the other, it might be interesting to be able to specify the key
order for a special action.  This is what `key-seq`_ does: it imposes an oorder
for the 2 characters tyoped quickly.  Different order is not triggerring the
special action.  Note that *key-seq* only accepts ASCII printable characters
(ie. in the range decimal 32 to 26 inclusive.)

Both *key-chord* and *key-seq* can be global, where they are always accessible
in Emacs, and mode-specific.  A mode specific *key-chord* or *key-seq* is only
available in buffers where the specific mode is active. For mode-specific
*key-chord* or *key-seq*, PEL schedule the loading of the definitions when the
file identifying the mode is loaded in Emacs.

With PEL, you define the *key-chord* and *key-seq* via customization.
They are stored inside the ``pel-key-chords`` user option.

PEL provides a set of key-chords by default which you can modify via the Emacs
customize buffer for the **pel-pkg-for-key-chord** customize group.  These also
provide examples of how to specify your own key-chords or key-seqs.

PEL provides the ``<f11> <f2> K`` key binding to quickly access this customize
group and the ``<f11> M-K`` binding to toggle the key-chord-mode on and off.

PEL defines several default key-chords that use Emacs Lisp lambda form. This is
the most flexible way to define a key-chord. It allows you to perform anything
with you command, just as if you were writing Emacs Lisp code in your
initialization file.  It also allows the use of keyboard prefix argument keys,
just like any other Emacs command.  With this you can prevent the execution of
code associated with a key-chord in read-only buffer, or pass numeric arguments
that modify the behaviour of the code.  You have the full flexibility of Emacs
Lisp at your disposal.

Be careful with this if you do not know Emacs Lisp: if you
change a setting that refer to a symbol that is not known when you open Emacs's
customize UI, Emacs customize UI will report a mismatch error and you will not
be able to make any modification.  If this happens to you, edit your
customization file and delete the entry for ``pel-key-chords`` from the file,
save the file back and restart Emacs.  If you followed the installation instructions
the name of this file is "``~/.emacs.d/emacs-customization.el``".

The logic for managing key-chord definitions stored in customization user option
is stored in the file `pel-key-chord.el`_.  The default values for the
``pel-key-chords`` user option is in the `pel--options.el`_ file.

For more information see the `Key Chords`_ PDF Documentation.


PEL Keyboard Macro Utilities
----------------------------

:PDF Sheet: `Keyboard Macros`_.
:PEL Customization: ``pel-kmacro-prompts``.
:PEL Key Prefix: *none*

The file `pel-kbmacros.el`_ implements ``pel-kmacro-start-macro-or-insert-counter``
used to replace the standard ``kmacro-start-macro-or-insert-counter`` to record
a keyboard macro.  If the user option ``pel-kmacro-prompts`` is set to
**t**, the PEL function checks if the macro is already defined and if it is,
prompts before allowing to replace the existing keyboard macro with a new one.
It just offer a little protection.  And this protection can be reset by
executing the second command: ``pel-forget-recorded-keyboard-macro``.  In some
case that level of protection might be annoying, to disable it completely and
restore the normal Emacs keyboard macro recording without any protective
prompting, just set the ``pel-kmacro-prompts`` to *nil*.


PEL Line Control Utilities
--------------------------

:PDF Sheet: `Display Lines`_.
:PEL Customization: *none*
:PEL Key Prefix: **pel:linectrl** : ``<f11> l``

The file `pel-line-control.el`_ contains:

- 2 commands that move the cursor to the previous and next logical lines, lines
  that when wider than the current window, wrap around:

  - ``pel-lc-previous-logical-line`` and
  - ``pel-lc-next-logical-line``.

- 1 command to toggle the display of the current line and column on the mode
  line.  In some cases the mode line is too short to display all information,
  removing the display of point's line and column frees real-estate to allow
  seeing more of the remainder of the mode line.

PEL provides a set of commands under the **pel:linectrl** key prefix, ``<f11>
l``, which deal with those commands and other Emacs line control related commands.


PEL Mark Management Utilities
-----------------------------

:PDF Sheet: `Marking`_.
:PEL Customization: ``pel-use-expand-region``.
:PEL Key Prefix: **pel:mark** : ``<f11> .``

The file `pel-mark.el`_ provides utilities to help manage the mark and the mark
ring buffer.

- ``pel-mark-ring-stats`` displays information on global and buffer local mark
  and mark rings.
- ``pel-popoff-mark-ring`` removes the top  entry from the buffer's mark ring.
- The following 2 commands allow marking lines quickly and PEL binds these
  commands to keys that include cursor to make the operation natural.  Being
  able to mark lines this way helps on various types of operations on regions,
  like commenting lines, kill, copy, etc...  The following two commands are
  provided:

  - ``pel-mark-line-up`` mark the current line: it places point at the beginning of
    the line and the mark at the end.  If the mark is already active, the command
    extends the region one more line up.  One of the PEL key bindings for this command
    is ``M-S-<up>``.
  - ``pel-mark-line-down`` mark the current line: it places the mark at the
    beginning of the line and point at the end.  If the mark is already active,
    the command extends the region on more line down.  One of the PEL key
    bindings for this command is ``M-S-<down>``.

- The following commands correspond to code provided by Mickey Petersen in his
  great web site in the page
  `Fixing the mark commands in transient mark mode`_.
  These are:

  - ``pel-push-mark-no-activate`` pushes point to the buffer's mark-ring without
    activating the region. PEL binds ``<f11> . SPC`` to this command.
  - ``pel-jump-to-mark`` jumps to the next mark in the buffer's mark-ring and
    then rotate the ring.  PEL binds ``<f11> . ``` to this command.
  - ``pel-exchange-point-and-mark-no-activate`` does the same thing as the
    Emacs command ``exchange-point-and-mark`` but without activating the region.
    PEL binds ``<f11> . ,`` to this command.

.. _Fixing the mark commands in transient mark mode: https://www.masteringemacs.org/article/fixing-mark-commands-transient-mark-mode


PEL Markup Language Support
---------------------------

PEL markup language support assigns the **F12** key as the prefix key for
PEL markup-specific commands.  The prefix key is the same for other markup
languages (or programming languages) but the key bindings after the prefix differ,
while keeping them as similar as possible.

PEL AsciiDoc Support
--------------------

:PDF Sheet: `AsciiDoc support`_.
:PEL Customization: ``pel-use-asciidoc``
:PEL Key Prefix: *none*

When ``pel-use-asciidoc`` is set to **t**, PEL activates the adoc-mode for files
with the ``.adoc`` file extension.


PEL reStructuredText Support Utilities
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

:PDF Sheet: `reStructuredText mode`_.
:PEL Customization: - ``pel-use-rst-mode``,
                    - ``pel-rst-adornment-style``,
                    - ``rst-preferred-adornments``.
:PEL Key Prefix: - Globally: **pel:for-reST** : ``<f11> SPC r``
                 - For buffers in rst-mode: ``<f12>``

                   - Sub-keys: **pel:rst-adorn-style** : ``<f12> A``

The file `pel-rst.el`_ provides features that complement the
support for reStructuredText_ markup provided by the rst.el, which implements
the ``rst-mode`` and which is
distributed with standard GNU Emacs.

The following 3 commands simplify the creation of reStructuredText hyperlinks
and where their `external hyperlink targets`_ are located:

- First you identify a location inside the file where the next external hyperlink
  target reference will be written by using the ``pel-rst-set-ref-bookmark`` which puts an
  actual Emacs bookmark to that location.
- Then to create a hyperlink inside the text, use the ``pel-rst-makelink``.  It
  adds the relevant markup around the word or marked region at
  point, move point to the location where the
  explicit hyperlink target references are located
  (using the location you previously set)
  and enters the first portion of the hyperlink markup.
  You can then type or yank/paste the required URI to complete the statement.
  After that you  can use ``pel-jump-to-mark``
  (normally bounded to ``M-```) to jump back to where you were typing the text.
- The ``pel-rst-goto-ref-bookmark`` moves point to where the external hyperlink
  target references are located.

Note that ``pel-rst-set-ref-bookmark`` sets an Emacs bookmark to the location,
so it is retained across sessions like other bookmarks.  The bookmark has a
special name which uses the "RST-" prefix followed by the name of the current
file.
This means that only one explicit hyperlink target reference location can be
remembered per file.  You can set any number of them, but only the last one will
be retained inside the bookmark across Emacs sessions.

Section Adornment Support
^^^^^^^^^^^^^^^^^^^^^^^^^

The default support for line title adornments done by the ``rst-adjust``
function does not always work and fails when some markup is used.
PEL provides a set of simple commands that adorn the current line with the
character supported by the specified level.  The ``pel-rst-adorn`` command takes
a numeric argument to add the adornment specified by the customization
list of adornments stored in the ``rst-preferred-adornments`` variable. To make
life simple PEL also defines 10 commands to adorn the current line with the
adornment level specified by the command name and binds these commands to easy
to use keys listed in the table below.  For example, to adorn a line with the
level 2 adornment just type ``<f12> 2`` in a buffer in rst-mode.
For other buffers it's still possible to use the commands, but the key sequence
is longer, in this case it would be ``<f11> SPC r 2``, as explained here.

For all styles:

- level 0 is created with the key ``<f12> t``,
- level 1 to level 9 use ``<f12> 1`` to ``<f12> 9``,
- level 10 is using the ``<f12> 0`` key.

The following commands allow creating line adornments for sections at levels
relative to the previous section or change the section level of the current
line:

- ``pel-rst-adorn-same-level`` adorn the line at the same level as the previous
  section. If an adornment already exists it replaces it.
- ``pel-rst-adorn-increase-level`` adorn the line with a level higher than the
  previous section level (creating a sub-section) if the line has no section
  underlining adornment.  If it has one, it increases the level.
- ``pel-rst-adorn-decrease-level`` adorn the line with a level lower than the
  previous section level (creating a sub-section) if the line has no section
  underlining adornment.  If it has one, it decreases the level.
- ``pel-rst-adorn-refresh`` refreshes the adornment length of the current
  line. This is useful when changing the text of the line.

PEL supports 3 types of section adornment styles:

- rst-mode default, a style with a title (level 0) and 7 other levels
- Sphinx-Python style, a style with 6 levels supported by Sphinx.
- CRiSPer style, a style with a title level (level 0) and 10 other levels.

The default style is selected by the ``pel-rst-adornment-style`` user option.
It can be changed for the current buffer using the following commands:

- ``pel-rst-adorn-default`` selects the default style,
- ``pel-rst-adorn-Sphinx-Python`` selects the Sphinx-Python style,
- ``pel-rst-adorn-CRiSPer`` selects the CRiSPer style.


When editing a buffer that uses the rst-mode, PEL sets the mode sensitive
**F12** prefix to **pel:for-reST** so the above commands can be accessed using
the following key strokes:

=============================== ===========================================
key                             binding
=============================== ===========================================
**Hyperlink control**
``<f12> .``                     ``pel-rst-makelink``
``<f12> g``                     ``pel-rst-goto-ref-bookmark``
``<f12> s``                     ``pel-rst-set-ref-bookmark``
**Section Level Adornment**
``<f12> t``                     ``pel-rst-adorn-title``
``<f12> 1``                     ``pel-rst-adorn-1``
``<f12> 2``                     ``pel-rst-adorn-2``
``<f12> 3``                     ``pel-rst-adorn-3``
``<f12> 4``                     ``pel-rst-adorn-4``
``<f12> 5``                     ``pel-rst-adorn-5``
``<f12> 6``                     ``pel-rst-adorn-6``
``<f12> 7``                     ``pel-rst-adorn-7``
``<f12> 8``                     ``pel-rst-adorn-8``
``<f12> 9``                     ``pel-rst-adorn-9``
``<f12> 0``                     ``pel-rst-adorn-10``
**Select Adornment Style**
``<f12> A d``                   ``pel-rst-adorn-default``
``<f12> A S``                   ``pel-rst-adorn-Sphinx-Python``
``<f12> A C``                   ``pel-rst-adorn-CRiSPer``
=============================== ===========================================

The longer to type global prefix is always available: ``<f11> SPC r``.

All of the above is activated by ``pel-init`` only when the
``pel-use-rst-mode`` user option is set to **t**.

.. _reStructuredText: https://en.wikipedia.org/wiki/ReStructuredText
.. _external hyperlink targets: https://docutils.sourceforge.io/docs/user/rst/quickref.html#hyperlink-targets

Text Emphasis
^^^^^^^^^^^^^

PEL provides four commands to put emphasis markup aound the current word or
marked area. The following commands are available for reStructuredText:

============== ====================
key            Emphasis
============== ====================
``<f12> b``    Bold
``<f12> i``    Italic
``<f12> l``    Literal
``<f12> ```    Interpreted text
============== ====================



PEL Mouse Support
-----------------

:PDF Sheet: `Mouse`_.
:PEL Customization: *none*
:PEL Key Prefix: *none*

When Emacs runs in graphcsi mode it supports the mouse seemlessly.
When Emacs runs in Terminal (TTY) mode, however, the mouse events are normally
used by the terminal emulator, not Emacs.
The mouse support is available by activating the **xterm-mouse-mode**.
The PEL system
binds the ``<f11><f12>`` key sequence for this command and also activates
mouse-driven scrolling.  Turn the xterm-mouse-mode off if you want to copy and
paste text in or out of the terminal/Emacs session from/to another process.

See the `Mouse`_ PDF document for more information.


PEL Menu Index Utilities
------------------------

:PDF Sheet: `Menus`_.
:PEL Customization: *none*
:PEL Key Prefix: **pel:menu** : ``<f11><f10>``

The file `pel-imenu.el`_ provides code that changes the order of entries of the
MenuBar Index entries so that they are easier to use with source code files and
markup files.  The entries are ordered in the order of appearance inside the
file instead of placing all sub-menus at the top the way Emacs normally does it.

When ``pel-init`` is called it calls ``pel-imenu-init`` which installs the
``pel-imenu-outline--split-menu`` utility function.  That function holds the
code to change the menu entry order.

**Credit**:
  The code of that utility function is based on pdf-tools/pdf-outline
  code mentioned here_.

It is possible to restore Emacs original behaviour by executing the
command ``pel-toggle-imenu-index-follows-order`` **and then forcing a menu entry
re-scan**.

PEL provides other key bindings to manage the MenuBar but also accessing the
menu via the mini-buffer.  The key prefix for these command bindings is ``<f11><f10>``.

.. _here: http://emacs.stackexchange.com/questions/31791/order-of-items-in-imenu?noredirect=1#comment48799_31791


PEL Narrowing Documentation
---------------------------

:PDF Sheet: `Narrowing`_.
:PEL Customization: *none*
:PEL Key Prefix: *none*


PEL provides the  `Narrowing`_ PDF table listing Emacs commands dealing with
the powerful concept of narrowing.


PEL Navigation Support
----------------------

:PDF Sheet: `Navigation`_.
:PEL Customization: - Group: ``pel-pkg-for-navigation`` (``<f11> <f2> n``):

                      - ``pel-use-any``
                      - ``pel-use-ace-link``

:PEL Key Prefix: *none*

The `pel-navigate`_ file provides a collection of navigation commands that
complement the standard Emacs navigation commands.

- ``pel-beginning-of-line`` is meant to replace ``beginning-of-line`` as it does
  the same and extends it: if point is already at the beginning of the line
  then it moves it to the first non-whitespace character.
- ``pel-end-of-line`` is also replacing ``end-of-line``. If the point is
  already at the end of the line, then point moves to beginning of trailing
  whitespace if there is any (otherwise point does not move).
- ``pel-newline-and-indent-below`` is useful as a variant of the return key.
- ``pel-find-thing-at-point`` provides a search capability without the need for
  a tag database but it is limited in what it can find.  It's a poor man
  cross reference.
- ``pel-show-char-syntax`` shows the character syntax of the character at
  point.
- ``pel-forward-token-start`` and ``pel-backward-to-start`` move forward
  or backward to the beginning of a text semantic token as defined by Emacs
  character syntax for the current buffer.
- ``pel-forward-word-start`` moves point to the beginning of next word.
  This complements what's already available in standard Emacs:
  ``forward-word`` and ``backward-word``.
- ``pel-forward-syntaxchange-start`` and ``pel-backward-syntaxchange-start``
  move point forward or backward to the character syntax change character.
  This can be useful to debug syntax characters for a specific mode.
- ``pel-next-visible`` and ``pel-previous-visible`` move point to the next or
  previous visible (non whitespace) character.
- ``pel-home`` and ``pel-end`` implement a quick, multi-hit movement to the
  beginning or end of the current field, line, window and buffer.
  These commands are similar to the home and end CRiSP/Brief commands.
  They also support the multiple window scroll sync provided by the
  ``pel-scroll`` commands.
- ``pel-beginning-of-next-defun`` move point to the beginning of the
  next function definition. This complements ``beginning-of-defun`` which
  only reaches the same location by moving backwards.

PEL also provides ability to use the `avy`_ and `ace-link`_ external packages to
provide super efficient navigation inside windows and *across* windows using the
keyboard home row!

The details are available in the `Navigation`_ PDF table.


Navigation Using imenu detected definitions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Emacs provides a powerful item detection and indexing system called *imenu*.
Each major mode can support imenu to quickly identify important locations
inside the file such as the location of function or type definitions inside a
programming language file or the section headers inside a markup file.

PEL takes advantage of this and extends it with available input completion
mechanisms to provide a set of easy to use key sequences to quickly navigate
to such locations inside the current buffer or to any currently opened buffer
of the same major mode as the current one.

The imenu system parse the content of the buffers.  When you modify the buffer
you may need to manually *re-scan* the buffer to update imenu indexing
system.  PEL provide the ``<f11> <f10> r`` key sequence for that.

To pop a menu of the current index list PEL provides the ``<f11> <f10> <f10>``
key sequence.  The following screen shot shows what it does when used inside
the imenu.el file.

The provided menu is often a hierarchical menu of entries: you can
select function, types, variables, classes and then their names or sub-names, etc..

.. image:: res/m-g-h/imenu-popup.png

PEL also provide the ``M-g h`` and the ``M-g M-h`` key sequences to prompt for
one of the items by name or partial name using one of the powerful input
completion like Ido, Ido-grid, Ido-vertical or Ivy.

You can select the completion mechanism via the input completion
customization.  You can also change the mode used during an editing session
(although your choice won't persist across editing sessions).  Use the ``M-g
C-h`` key sequence to select the mode.

Here's what it looks like when using the ``M-g h`` key with Ido-grid with the
grid expanded.

.. image:: res/m-g-h/ido-grid.png

And what it looks when using Ivy:

.. image:: res/m-g-h/ivy.png


Navigation in a large file using imenu and speedbar
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

When navigating in a very large file, the imenu pop-up menu will be split into
several sections.  It does not give you an overview of the content of the file
though.  For that you can use the `Speedbar`_.  Both present information in
hierarchical fashion.  You can customize the speedbar to display all elements
of the same *type* as a continuous list.

Here's an example using the file sqlite3.c_.  This file contains **all** the C
code of the SQLite 3 database: about 231,756 lines.

The following screen shot shows the file open in Emacs running in terminal
mode with that file opened.  The imenu popup window is opened by typing  ``<f11>
<f10> <f10>``:

.. image:: res/navigation/def-lists/sqlite3-imenu-popup-top.png

The list is split in several sections.  The first section is **Class** with is
the name of a sub-section.  Later, somewhere else in that file, using the same
command and selecting the Class section, imenu shows the list of structure
names also organized in sub-sets.

.. image:: res/navigation/def-lists/sqlite3-imenu-popup-class.png

Here's what you see on the same file when opening the Speedbar in terminal
mode.  With PEL use ``<f11> M-s`` to open the speedbar.

.. image:: res/navigation/def-lists/sqlite3-speedbar-top.png

Without moving point you can scroll the speedbar window using the ``<M-S-f6>``
key stroke and see more of it.

.. image:: res/navigation/def-lists/sqlite3-speedbar-scrolled-down.png


.. _sqlite3.c: https://github.com/gittup/tup/blob/master/src/sqlite3/sqlite3.c


PEL Number Keypad Support
-------------------------

:PDF Sheet: `Number Keypad`_.
:PEL Customization: *none*
:PEL Key Prefix: *none*

The file `pel-numkpad.el`_ implements the PEL number keypad support.

The number keypad available on various keyboard differ in behaviour.
Some keyboard have a NumLock key, others, like Apple keyboards do not have it.
PEL support different keyboards thta have a number keypad, and provides its own
management of the Numlock, with the ``<f11> #`` key mapped to
``pel-toggle-mac-numlock`` to provide two set of commands: one when in Numlock
mode and another when Numlock mode is off.  In that latter mode, the commands
normally associated to cursor keys are provided, but also the ``pel-home`` and
``pel-end`` as well as several copy and kill commands.

Refer to the `Number Keypad`_ PDF document for more information.

PEL Package Management Documentation
------------------------------------

:PDF Sheet: `Packages`_.
:PEL Customization: *none*
:PEL Key Prefix: *none*

PEL provides the  `Packages`_ PDF table listing Emacs commands dealing with
Emacs package management.

---------------------------------------------------------------------------

PEL Programming Language Support
--------------------------------

PEL programming language support assigns the **F12** key as the prefix key for
the programming language.  The prefix key is the same for other programming
languages (or markup languages) but the key bindings after the prefix differ,
while keeping as similar keys as possible.

Note:
  PEL support for programming languages is still evolving.
  It will be enhanced with upcoming versions.

**Programming Language Families**

**BEAM-VM Programming Languages:**

PEL supports several BEAM programming languages, including:

- `Erlang`_
- `Elixir`_
- `LFE`_ (Lisp Flavoured Erlang)
- `Gleam`_

.. _Erlang: `PEL Support for Erlang`_
.. _Elixir: `PEL Support for Elixir`_
.. _LFE:    `PEL Support for LFE`_
.. _Gleam:  `PEL Support for Gleam`_

For the moment support for Erlang and LFE are the most evolved.
Erlang support is very evolved.  LFE support also because it inherit the
capabilities of Lisp-based languages.
Support for Elixir and Gleam is experimental.


.. _BEAM VM Programming Language:  https://en.wikipedia.org/wiki/BEAM_(Erlang_virtual_machine)

**Curly-Bracket Programming Languages:**

PEL provides explicit support for the following
`curly-bracket programming languages`_:

- `C  <https://en.wikipedia.org/wiki/C_(programming_language)>`_
- `C++ <https://en.wikipedia.org/wiki/C%2B%2B>`_
- `D <https://en.wikipedia.org/wiki/D_(programming_language)>`_

It also provides experimental support for:

- `Go <https://en.wikipedia.org/wiki/Go_(programming_language)>`_
- `Rust <https://en.wikipedia.org/wiki/Rust_(programming_language)>`_
- `V <https://vlang.io>`_

.. _Curly-Bracket Programming Language:
.. _curly-bracket programming languages: https://en.wikipedia.org/wiki/List_of_programming_languages_by_type#Curly-bracket_languages


**LISP-based Languages:**

PEL provides explicit support for the following
`LISP-based programming languages`_:

- `Common Lisp <https://en.wikipedia.org/wiki/Common_Lisp>`_
- `Emacs Lisp  <https://en.wikipedia.org/wiki/Emacs_Lisp>`_

.. _LISP Programming Language Family:
.. _LISP-based programming languages: https://en.wikipedia.org/wiki/Lisp_(programming_language)



---------------------------------------------------------------------------

PEL Apple-Script and Audio Narration Support
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

:PDF Sheet: `Apple-Script`_.
:PEL Customization: - Group: ``pel-pkg-for-programming``

                      - Activation:

                        - ``pel-use-applescript``
                        - ``pel-use-hydra``

                      - Configuration:

                        - ``pel-mac-voice-name``

:PEL Key Prefix: **pel:narrate** : ``<f8>``

PEL provides basic Apple-Script support via the apples-mode_ external package:
that does basic syntax colouring and provides a scratch-pad to write some
Apple-Script code to copy somewhere else.

If your Emacs is running a=on a macOS computer PEL also provides a set of
commands that read text from a buffer and uses the Apples system voice synthesis
to say the text out-loud.  Essentially providing a text-to-speech system.
By default the code uses the voice that is selected by default on the system but
you can also change it by setting the ``pel-mac-voice-name`` user option
variable.  Normally this would only work on the Cocoa-based (Graphics mode)
Emacs, but PEL also implements basic support for Emacs running in Terminal (TTY)
mode.

A couple of other functions are provided to issue Apple-Script commands from Emacs.

More information is available in the `Apple-Script`_ PDF table.




---------------------------------------------------------------------------


PEL Support For C
~~~~~~~~~~~~~~~~~

.. _𝕻𝔩 - C: https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/pl-c.pdf

:Language Family: `Curly-Bracket Programming Language`_
:PDF Sheet: `𝕻𝔩 - C`_
:PEL Customization: - Group: ``pel-pkg-for-c``

                      - Activation: *none*
                      - Configuration:

                        - ``pel-c-indent-width``
                        - ``pel-c-tab-width``
                        - ``pel-c-use-tabs``
                        - ``pel-c-bracket-style``
                        - ``pel-c-activates-minor-modes``

:PEL Key Prefix: - Globally: **pel:for-c** : ``<f11> SPC c``
                 - From a buffer in c-mode: ``<f12>`` and ``<M-f12>``


PEL provides support for the `C Programming Language`_ via Emacs built-in
c-mode.  No activation is necessary since the c-mode is built-in Emacs.
However, configuration  of important editor behaviour such as the
indentation/bracket style and the indentation is completely controlled by user
options listed above and can easily be changed using Emacs customize system.
PEL also provides easy access to commands that can change the CC Mode behaviour
on which the c-mode is based via the **pel:for-c** key-map, bounded to the
**F12** key for each buffer in c-mode.

.. _C Programming Language: https://en.wikipedia.org/wiki/C_(programming_language)



C Style Control
^^^^^^^^^^^^^^^

Ideally the editor should support various indentation styles for the C
preprocessor directives.  A selection of styles is shown in the next section.

I am working to implement the ability to support all of these styles.
But it is work in progress.
I will update this section once this work is done.


Indentation of C pre-processor statements
+++++++++++++++++++++++++++++++++++++++++

All examples are using the bsd indentation style.

**Style 1: no indentation**

.. code::  c

    #ifdef SOMETHING
      #if SOMETHING_ELSE
        #define SOMEONE 1


          for (...)
          {
            if (foo)
            {
              if (bar)
              {
          #ifdef __WIN32__
                c = GetTickCount();
          #else
                c = clock();
          #endif
              }
            }
          }


**Style 2: all indented**

.. code::  c

      for (...)
      {
        if (foo)
        {
          if (bar)
          {
            #ifdef __WIN32__
              c = GetTickCount();
            #else
              c = clock();
            #endif
          }
        }
      }

**Style 3: indented**

.. code::  c

      for (...)
      {
        if (foo)
        {
          if (bar)
          {
            #ifdef __WIN32__
            c = GetTickCount();
            #else
            c = clock();
            #endif
          }
        }
      }


**Style 4: anchored indented**

.. code::  c

      for (...)
      {
        if (foo)
        {
          if (bar)
          {
      #     ifdef __WIN32__
              c = GetTickCount();
      #     else
              c = clock();
      #     endif
          }
        }
      }





C Code Templates
^^^^^^^^^^^^^^^^

PEL supports yasnippet_ for your basic templating needs.
It also supports the built-in tempo skeleton system which provides a powerful
templating system but requires Emacs Lisp knowledge.  PEL provides a set of
tempo skeleton templates inside the file `pel-skels-c.el`_ that are made
available by the commands accessed via the ``pel:c-skel`` key prefix which is
mapped to the ``<f12> <f12>`` key sequence in any c-mode buffer.

See the
`C PDF <https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/pl-c.pdf>`_
and related tables for more information on the various key sequences available.

PEL Tempo Skeleton Templates Flexibility
++++++++++++++++++++++++++++++++++++++++

Most PEL tempo skeleton based templates use formats that are controlled by Emacs
user option variables.

Emacs user options are just specially identified Emacs variables.

Since Emacs can read variables from information on a file and from the
special ``.dir-locals.el`` file (which affects all files in the directory tree)
it becomes possible to have a complete control of the various style elements for
C files and you can have different ones inside different directory trees, have
special settings for some files.

This is extremely flexible.

Controlling PEL Tempo Skeletons for C
+++++++++++++++++++++++++++++++++++++

The PEL tempo skeletons used to generate source code come in two major groups:

- The generic skeletons which are programming language agnostic and produce
  generic code such as file module header blocks.  These skeletons do not
  insert programming language specific specializations.
- The programming or markup language specific skeletons.  These skeletons are
  aware of the target programming or markup language and may insert
  specialized source code constructs.

Most of the PEL tempo Skeletons are controlled by a set of customization user
option variables.  These affect the generated code, the style, the format,
etc...

To see or modify those user option variables the easiest way is to use the
``pel-browse-group`` command, bound to ``<f11> <f2> b`` and specify the name
of the PEL customization group related to C programming,
``pel-c-code-style``.

You can also look at it from the group
``pel-pkg-for-programming`` or from the ``pel-pkg-for-skeletons``.  The
screenshot below shows 2 windows with the ``*Customize Browser*`` buffers
showing these 2 groups within their context.

.. image:: res/pel-customizing-skeletons.png


C file module/header block template
+++++++++++++++++++++++++++++++++++

To insert a C file module/header block inside a C file, use
the ``<f12> <f12> h`` key sequence.  It will prompt for the purpose of the
file.  Once you hit the return key it inserts the comment block at the
beginning of the C file according to the user options that are part of the
``pel-c-code-style`` customization group.  See the example in the next section.

In a .c C file
**************

For this example, the CC mode variables and user option variables are all set
to their default values except the first 4 listed in the following table:

==================================== ======= =====================================
Variable                             Value   Description
==================================== ======= =====================================
user-full-name                       ➽       Set your full name.
user-mail-address                    ➽       Set you email address in this
                                             variable.  Emacs tries to infer it
                                             but it may only contain your user
                                             name and your computer name.
c-block-comment-flag                 nil     Emacs internal variable.
                                             Use C-style comment: ``/* */``.
                                             Change with ``<f12> M-;``.
pel-c-skel-with-license              t       Insert a source code license into
                                             the module/header block.

pel-c-skel-with-license              t       Insert an open source license
                                             inside the module header.  The
                                             license text is based on the lice_
                                             external package and the license
                                             type must be identified in the
                                             ``lice:default-license``.

pel-c-skel-with-license              string  You can also specify the name of
                                             license type as a plain string.
                                             That string will be inserted
                                             instead of the complete license
                                             text.


lice:default-license                 glp-3.0 Use the GPL V3.0 license.

pel-c-skel-comment-with-2-stars      t       Use ``**`` as continuation comment.
pel-c-skel-use-separators            t       Use separator lines.
pel-c-skel-doc-markup                nil     Identifies the markup style in
                                             comment: none used.
pel-c-skel-insert-file-timestamp     t       Insert a time stamp that is updated
                                             automatically when the file is
                                             saved.
pel-c-skel-use-include-guards        nil     Used for C header files only:
                                             if set header files have include
                                             guards. You can select between
                                             no include-guard, include-guard
                                             using ``#pragma once`` statement,
                                             classic include guard with
                                             ``#ifdef`` pre-processor
                                             statement or an include-guard
                                             that uses a ``#ifdef``
                                             pre-processor statement with a
                                             symbol that incorporates a UUID,
                                             making it unique.

pel-c-skel-module-header-block-style nil     The selected style for the module
                                             header: use PEL's default style.

pel-c-skel-module-section-titles     ➽       Use the module sections
                                             identified by PEL's default:

                                             - "Module Description",
                                             - "Header Inclusion",
                                             - "Local Types",
                                             - "Local Variables", and
                                             - "Code".

                                             The value can also be set to nil
                                             to prevent insertion of the sections.
==================================== ======= =====================================


When typing the ``<f12> <f12> h`` key sequence inside the buffer of the file
``monitor.c``, the command prompts for the file purpose, entering "process
monitoring facilities" and hitting return the command inserts the following
code in the buffer:

.. code:: c

          /* C MODULE: monitor.c
          **
          ** Purpose   : Process monitoring facilities.
          ** Created   : Thursday, August 27 2020.
          ** Author    : Pierre Rouleau <your-email@here.yougo>
          ** Time-stamp: <2020-08-27 22:28:12, by Pierre Rouleau>
          **
          ** Copyright (C) 2020  Pierre Rouleau
          **
          ** This program is free software: you can redistribute it and/or modify
          ** it under the terms of the GNU General Public License as published by
          ** the Free Software Foundation, either version 3 of the License, or
          ** (at your option) any later version.
          **
          ** This program is distributed in the hope that it will be useful,
          ** but WITHOUT ANY WARRANTY; without even the implied warranty of
          ** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
          ** GNU General Public License for more details.
          **
          ** You should have received a copy of the GNU General Public License
          ** along with this program.  If not, see <http://www.gnu.org/licenses/>.
          **
          */
          /* ------------------------------------------------------------------------ */
          /* Module Description
          ** ------------------
          **
          ** Ⓜ️
          **
          */

          /* ------------------------------------------------------------------------ */
          /* Header Inclusion
          ** ----------------
          */

          Ⓜ️

          /* ------------------------------------------------------------------------ */
          /* Local Types
          ** -----------
          */

          Ⓜ️

          /* ------------------------------------------------------------------------ */
          /* Local Variables
          ** ---------------
          */

          Ⓜ️

          /* ------------------------------------------------------------------------ */
          /* Code
          ** ----
          */

          Ⓜ️

          /* ------------------------------------------------------------------------ */

Your name is written instead of mine, and your email address is used.
Your email address is taken from the ``user-mail-address`` Emacs user option if
it is set otherwise it attempts to compute it through other Emacs user option
variables.  See `Emacs User Identification`_ documentation for more information.

The Ⓜ️  in the code above identify the tempo marker locations.
Once you use a PEL command to insert a PEL tempo
skeleton template you also automatically activate the **pel-tempo-mode**, a minor
mode where you can use ``C-c .`` to move to the next tempo marker and ``C-c ,``
to move point to the previous tempo marker.  When the **pel-tempo-mode** is
active the **‡** lighter shows in the buffer's `mode line`_.
You can toggle the **pel-tempo-mode** minor mode with the ``<f12> <f12>
<space>`` key sequence.


The comment block contains sections because the user option
``pel-c-skel-insert-module-sections`` is set to **t**.  This always includes the
section with the "Module Description" title.  The following section names
("Header Inclusion", "Local Types",  etc..) are identified by the user option
``pel-c-skel-module-section-titles``.


.. _Emacs User Identification: https://www.gnu.org/software/emacs/manual/html_node/elisp/User-Identification.html




In a C header file
******************

If you use the exact same command inside a C header (``.h``) file, but with
``pel-c-skel-with-license`` set to nil to prevent the inclusion of license
text, and with the ``pel-c-skel-use-include-guards`` set to ``with-uuid``,
the command will also prompt for the file purpose (and you can get the
previous entry by typing ``M-p`` or the ``<up>`` cursor key), then you get the
following code instead:

.. code:: c

            /* C HEADER: monitor.h
            **
            ** Purpose   : Process monitoring facilities.
            ** Created   : Thursday, August 27 2020.
            ** Author    : Pierre Rouleau <your-email@here.yougo>
            ** Time-stamp: <2020-08-27 22:49:17, by Pierre Rouleau>
            */
            /* ------------------------------------------------------------------------ */
            #ifndef MONITOR__H_H0Z79D0F_DB8F_46BJ_FBBC_855167H0439Z
            #define MONITOR__H_H0Z79D0F_DB8F_46BJ_FBBC_855167H0439Z /* include guard */
            /* ------------------------------------------------------------------------ */
            Ⓜ️
            /* ------------------------------------------------------------------------ */
            #endif

If you want to use C99 compatible line comment style instead, type ``<f12> M-;``
to toggle the comment style and then type the same command (``<f12> <f12> h``)
and you'll get something like the following:

.. code:: c

            // C HEADER: monitor.h
            //
            // Purpose   : Process monitoring facilities.
            // Created   : Thursday, August 27 2020.
            // Author    : Pierre Rouleau <your-email@here.yougo>
            // Time-stamp: <2020-08-27 22:57:51, by Pierre Rouleau>
            // ---------------------------------------------------------------------------
            #ifndef MONITOR__H_H0Z79D0F_DB8F_46BJ_IFBC_8551670H439Z
            #define MONITOR__H_H0Z79D0F_DB8F_46BJ_IFBC_8551670H439Z /* include guard */
            // ---------------------------------------------------------------------------
            Ⓜ️
            // ---------------------------------------------------------------------------
            #endif


Again, Ⓜ️  is shown where the tempo markers are placed.

The UUID-based include guards eliminate the probability of include guard clashes
when using your code with other's libraries in case the same C header file name
is used somewhere. This technique is more portable than the ``#pragma once``
technique also used.  Also note that even if you use ``//`` style comments, the
code uses a C-style (block comment) following a C pre-processor include
statement, again to increase code compatibility over compilers since the
original C-style comment is always supported by C pre-processors.

If you have ``pel-c-skel-use-include-guards`` set to nil, the include guard
code is not included, and you'll get something like this instead:

.. code:: c

            /* C HEADER: monitor.h
            **
            ** Purpose   : Process monitoring facilities.
            ** Created   : Thursday, August 27 2020.
            ** Author    : Pierre Rouleau <your-email@here.yougo>
            ** Time-stamp: <2020-08-27 22:49:17, by Pierre Rouleau>
            */
            /* ------------------------------------------------------------------------ */

or:

.. code:: c

            // C HEADER: monitor.h
            //
            // Purpose   : Process monitoring facilities.
            // Created   : Thursday, August 27 2020.
            // Author    : Pierre Rouleau <your-email@here.yougo>
            // Time-stamp: <2020-08-27 22:57:51, by Pierre Rouleau>
            // ---------------------------------------------------------------------------


Using user-specified module/header skeleton
*******************************************

The default module/header skeleton with all the user options PEL provides
might still not generate exactly what you need.  In that case, you can write
your own skeleton template and get PEL to use it.

To replace PEL's generated code with a template of your own you must do the
following:

- Read and understand the provided example in the file
  `custom/skeleton/custom-c-skel.el`_.  See the explanations below.  For this
  you will need a minimal understanding of the Emacs Lisp programming
  language.  You'll need to learn more than what you need to learn to write
  yasnippet_ templates, but you can then take advantage of all Emacs Lisp
  power and use the PEL functions to prompt for function and purpose, check
  for function name validity, transform the function name and have the whole
  action already bound to a key sequence.
- Write a Emacs Lisp Skeleton file that will define your skeleton template
  with something similar to what you found in the example:

  - Inside that file, create a Emacs Lisp defun for the function named
    ``pel-skels-c-header-module-block/custom``.

- Set the ``pel-c-skel-module-header-block-style`` user option to take a user
  defined skeleton instead of PEL's default: write the path name of the Emacs
  Lisp file where you wrote your own skeleton code.


**Description of the Custom Skeleton Example:**

- Source code: `custom/skeleton/custom-c-skel.el`_.

First, the custom example takes advantage of the functionality provide by the
PEL functions and the features it can customize further.  Which means that
your custom skeleton can be written to take advantage of PEL's user option
variables to control some of the aspects of the generated code.  These user
options are the following:

- ``pel-c-skel-insert-module-section`` which specifies whether or not code
  sections delimiters are inserted inside the generated code.  Code sections
  that describe the purpose the sections of your file. If you want to have
  those sections you can define their titles inside the
  ``pel-c-skel-module-section-titles`` user option.  The default is to include
  the following sections after the *Module Description* section:

  - Header Inclusion
  - Local Types
  - Local Variables
  - Code

- ``pel-c-skel-comment-with-2stars`` which identifies whether one or two start
  characters will be used in the C continuation comment.


Assuming you set the user options to the following values:

- ``pel-c-skel-insert-module-section`` set to nil to prevent generating sections.
- ``pel-c-skel-comment-with-2stars`` set to nil to use the single star C style
  comment.
- ``pel-c-skel-module-header-block-style`` set to the name of a file that
  contains the same code as in `custom/skeleton/custom-c-skel.el`_

Assuming also that your name is ``Same One`` and you work for ``U-FooBar``,
typing the ``<f12> <f12> h`` in the file ``ufoobar.c``, something like the
following comment would be inserted in the buffer:

.. code:: c

            /* ufoobar.c : Process monitoring facilities.
             *
             * U-FooBar restricted an and confidential. Copyright © U-FooBar 2020.
             * Created   : Saturday, August 29 2020.
             * Author    : Same One  <sameone@ufoobar.there>
             * Time-stamp: <2020-08-29 17:47:38, updated by Same One>
             */

If you know Emacs Lisp, skip this section.

Notice the first line of the `custom/skeleton/custom-c-skel.el`_ file that
specifies lexical binding.

Inside the file, the function ``pel-skels-c-header-module-block/custom`` is defined to
take 3 mandatory arguments, and as described in the function docstring these are:

- arg 1: ``fname``, a string : the file name (without path)
- arg 2: ``is-a-header``, a boolean: is non-nil when the file is a C header
  file, and nil when it is a .c file.
- arg 3: cmt-style, a list of 3 strings: (cb cc ce), where:

  - cb : comment begin string
  - cc : comment continuation string
  - ce : comment end string.

The code provided does not use the second argument, ``is-a-header``. If you
want to distinguish between code files and header files, use this
argument. PEL code does.

The first lisp form of the function is a ``let`` form that defines `local
variables`_.  The ``purpose`` variable is set with the result of the prompt
asking the user the purpose of the C file.  If no purpose is specified by the
user, the function returns its default, the symbol ``p``.  That symbol is used
in the tempo skeleton and means that the location of the purpose string will
instead be a tempo marker (shown in the output example above with the Ⓜ️
characters) to remind the user to fill the file's purpose string.

The next 3 lines uses the ``nth`` function to extract the first second and
third element of the ``cmt-style`` argument.

The last form of the function is the dynamic construction of a tempo-skeleton
compliant insertion list.  The first element of that list is the symbol ``l``,
and then each element is either a string, a function returning a string, a
variable name that evaluates to a string or the symbol ``n`` that identifies
the end of a line.  The symbols are quoted otherwise they would be evaluated.
For more information about the syntax of tempo skeleton templates see the
docstring of the ``tempo-define-template`` function.  To open a help buffer
with it, type: ``<f1> o tempo-define-template RET``.

The code calls several of PEL's functions. These functions are all
documented. To get more info about them uses Emacs help system: move your
cursor on the function name and hit ``<f1> o``.  Emacs will prompt with the
function name you selected.  Hit return and Emacs will show a help buffer with
the description of the function.  The first line of the help buffer will end
with the name of the file where the function is defined. Hit tab to move point
there and hit return again to open PEL's file where the function is defined.

.. _local variables: https://www.gnu.org/software/emacs/manual/html_node/elisp/Local-Variables.html#Local-Variables

C function template
+++++++++++++++++++

Insert a C function definition block with the ``<f12> <f12> f`` key sequence.

The function definition block style is selected by the
``pel-c-skel-function-define-style`` user option.  By default no style is
selected.  Customize that variable first.  You can use the ``<f12> <f2>`` key
sequence from a buffer in c-mode to open the PEL customization buffer for C,
select the ``pel-c-code-style`` customization group and change the value of
``pel-c-skel-function-define-style``.
The following function template styles are available:

- `C function definition with no comment block`_
- `C function definition with basic comment block`_
- `C function definition with Man-page style comment block`_

The default comment style is the C-style ``/* */`` style (also called block
style).  But you can switch to the C++ ``//`` style (also called line style)
using the ``<f12> M-;`` key sequence.

The ``pel-c-skel-function-name-on-first-column`` user option is set to **t** the
function return type is written on the line above the function name.  By default
this is set to nil and the return type precedes the function name on the same
line. The skeleton inserts ``void`` as the type but leaves a tempo marker on the
first letter.  You can quickly delete it with ``M-d`` and type the proper type.

The style selected by ``pel-c-skel-function-define-style`` identifies the style
of the function comment block placed above the function definition code (if
any).

The title of the sections included in the function comment block are identified
by the ``pel-c-skel-function-section-titles`` user option.

Several styles are supported.  You can also define your own with Emacs Lisp code
stored in a file that you identify in the user option.

The following sections provide examples of the supported styles.

Function name and purpose prompts
*********************************

All function definition template commands prompt for the function name and the
purpose of the function.  The prompt maintain an history of the accepted values
which you can inspect with the following key sequences:

- to see previous entry, use ``M-p`` or the ``<up>`` cursor,
- to see the next entry, use ``M-n`` or the ``<down>`` cursor.

The function prompt transforms your entry a little:

- it strips any leading or trailing whitespace from the entered string, and
- replace all dash (``'-'``) characters by underscores (``'-'``) to help
  speed up entry.

It accepts only syntactically valid C function names, rejecting the others by
erasing the string from the mini buffer prompt area.  You can recall it using
the previous entry history key.

The commands also prompt for a quick single sentence describing the purpose of
the function.  Before inserting that text the command strips whitespace off the
front and end of the text, converts the first character to uppercase and ends
the text with a period if it is not already present.

You can cancel any prompt with the usual ``C-g`` key.

C function definition with no comment block
*******************************************

This is the simplest style.
The ``<f12> <f12> f`` command prompts for the
function name and inserts something like this:

.. code:: c

            /* -------------------------------------------------------------------------- */
            Ⓜ️void
            register_process(Ⓜ️)
            {
               Ⓜ️
            }

            Ⓜ️


or:

.. code:: c

            /* -------------------------------------------------------------------------- */
            Ⓜ️void register_process(Ⓜ️)
            {
               Ⓜ️
            }

            Ⓜ️

or:

.. code:: c

            // -----------------------------------------------------------------------------
            Ⓜ️void
            register_process(Ⓜ️)
            {
               Ⓜ️
            }

            Ⓜ️


or:

.. code:: c

            // -----------------------------------------------------------------------------
            Ⓜ️void register_process(Ⓜ️)
            {
               Ⓜ️
            }

            Ⓜ️


It puts the type ``void`` and places a tempo marker (identified by Ⓜ️ ) just
before to let you modify it if necessary (use ``M-d`` to delete it).   It places
the function name on the same or next line depending of the value of the
``pel-c-skel-function-name-on-first-column`` user option and inserts a C-style
comment by default but you can switch to C++ style using the ``<f12> M-;`` key
sequence.

The presence of horizontal separator lines is controlled by the
``pel-c-skel-use-separators`` user option.  It is on by default.
The length of the line is controlled by the ``pel-c-fill-column`` user option of
it is set to an integer value, otherwise it is controlled by the Emacs global
``fill-column`` user option.  You can changed the user option but you can also
change the value for the current buffer with the ``C-x f`` command.

C function definition with basic comment block
**********************************************

This styles adds a comment block with the function purpose.
The ``<f12> <f12> f`` command prompts for the
function name and the function purpose and inserts something like this:

.. code:: c

            // -----------------------------------------------------------------------------
            // register_process() -- Register a process ID to the monitored list.

            Ⓜ️void
            register_process(Ⓜ️)
            {
               Ⓜ️
            }

            Ⓜ️

As for all function definition blocks the user options in the
``pel-c-code-style`` group control whether the separator line is inserted, and
whether the return type is placed on the same line as the function name or just
above.

C function definition with Man-page style comment block
*******************************************************

With C-style comments, ``pel-c-skel-function-name-on-first-column`` set to nil
(the default), ``pel-c-skel-function-section-titles`` identifying the list
``("DIAGNOSTIC" "SEE ALSO")`` (the default) typing ``<f12> <f12> f`` inside a
buffer in c-mode will insert text following text after prompting for the
function name and purpose.
For this example, I typed ``register-process`` (with a dash) and then
``register a process ID in the list of monitored processes`` at the prompt. The
command replace the dash in the function name with underscore and also converts
the purpose text by capitalizing the first letter and ending it with a period if
one is not already present.

.. code:: c

            /* -------------------------------------------------------------------------- */
            /* r e g i s t e r _ p r o c e s s ( )     -- Register a process ID in the list of monitored process.
            ** ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
            **
            ** DESCRIPTION
            **
            ** register_process() Ⓜ️
            **
            **
            ** DIAGNOSTIC
            **
            ** Ⓜ️
            **
            ** SEE ALSO
            **
            ** Ⓜ️
            **
            */

            Ⓜ️void register_process(Ⓜ️)
            {
                Ⓜ️
            }

            Ⓜ️

            /* -------------------------------------------------------------------------- */

It leaves tempo markers at the locations identified by Ⓜ️ .
The indentation is controlled by the ``pel-c-indent-width`` user option.
Use ``C-c .`` to move point to the next tempo marker and ``C-c ,`` to the previous one.

With C++ style comments and ``pel-c-skel-function-name-on-first-column`` set to
**t**, the code inserted is the following instead:

.. code:: c

            // -----------------------------------------------------------------------------
            // r e g i s t e r _ p r o c e s s ( )     -- Register a process ID in the list of monitored process.
            // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
            //
            // DESCRIPTION
            //
            // register_process() Ⓜ️
            //
            //
            // DIAGNOSTIC
            //
            // Ⓜ️
            //
            // SEE ALSO
            //
            // Ⓜ️
            //

            Ⓜ️void
            register_process(Ⓜ️)
            {
               Ⓜ️
            }

            Ⓜ️


User selected template for C function definition
************************************************

The default function definition header skeleton with all the user options PEL
provides might still not generate exactly what you need.  In that case, you
can write your own skeleton template and get PEL to use it.

To replace PEL's generated code with a template for C function definition of
your own you must do the following:

- Read and understand the provided example in the file
  `custom/skeleton/custom-c-skel.el`_.  For this
  you will need a minimal understanding of the Emacs Lisp programming
  language.  The code for creating a C function definition code is similar to
  the code that generates the C module/header, so if you're new to Emacs Lisp
  see the explanation inside the section titled
  `Using user-specified module/header skeleton`_ above.
- Write a Emacs Lisp Skeleton file that will define your skeleton template
  with something similar to what you found in the example:

  - Inside that file, create a Emacs Lisp defun for the function named
    ``pel-skels-c-function-def/custom``.

- Set the ``pel-c-skel-function-define-style`` user option to take a user
  defined skeleton instead of PEL's default: write the path name of the Emacs
  Lisp file where you wrote your own skeleton code.  The name of that file may
  be the same as what you might have defined inside the
  ``pel-c-skel-module-header-block-style`` user option.

The example taken from the file `custom/skeleton/custom-c-skel.el`_ generates
code like the following, controlled by various PEL user options that identify
whether the horizontal separator line is inserted, whether the C function
return type is on a separate line (as in the example below) or on the same
line as the function name, and you can also switch to C++ style comments. The
indentation is also controlled by customization.

.. code:: c

            /* -------------------------------------------------------------------------- */
            /* ================
             * register_process
             * ================
             *
             * Register a process ID in the list of monitored process.
             *
            */

            Ⓜ️void
            register_process(Ⓜ️)
            {
               Ⓜ️
            }

            Ⓜ️


Using a C++ style comment (via ``<f12> M-;``) and setting
``pel-c-skel-function-name-on-first-column`` set to nil the code generated becomes:

.. code:: c


        // -----------------------------------------------------------------------------
        // ================
        // register_process
        // ================
        //
        // Register a process ID in the list of monitored process.
        //

        Ⓜ️void register_process(Ⓜ️)
        {
           Ⓜ️
        }

        Ⓜ️

.. _custom/skeleton/custom-c-skel.el: ../custom/skeletons/custom-c-skel.el

And then if you set ``pel-c-skel-use-separators`` to nil, the command does not
insert the separator line, so you get the following instead:

.. code:: c


        // ================
        // register_process
        // ================
        //
        // Register a process ID in the list of monitored process.
        //

        Ⓜ️void register_process(Ⓜ️)
        {
           Ⓜ️
        }

        Ⓜ️

Finally, if you type return at the prompt for the function purpose, the purpose
text is not inserted, instead a tempo marker (represented as Ⓜ️  in the generated
code) is placed and point is left there:

.. code:: c


        // ================
        // register_process
        // ================
        //
        // Ⓜ️
        //

        Ⓜ️void register_process(Ⓜ️)
        {
           Ⓜ️
        }

        Ⓜ️

As in the other styles, once you use a PEL command to insert a PEL tempo
skeleton template you also automatically activate the **pel-tempo-mode**, a minor
mode where you can use ``C-c .`` to move to the next tempo marker and ``C-c ,``
to move point to the previous tempo marker.  When the **pel-tempo-mode** is
active the **‡** lighter shows in the buffer's `mode line`_.
You can toggle the **pel-tempo-mode** minor mode with the ``<f12> <f12>
<space>`` key sequence.

.. _mode line: https://www.gnu.org/software/emacs/manual/html_node/emacs/Mode-Line.html#Mode-Line



---------------------------------------------------------------------------

PEL Support For C++
~~~~~~~~~~~~~~~~~~~

.. _𝕻𝔩- C++: https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/pl-c++.pdf

:Language Family: `Curly-Bracket Programming Language`_
:PDF Sheet: `𝕻𝔩- C++`_
:PEL Customization: - Group: ``pel-pkg-for-c++``

                      - Activation: *none*
                      - Configuration:

                        - ``pel-c++-indent-width``
                        - ``pel-c++-tab-width``
                        - ``pel-c++-use-tabs``
                        - ``pel-c++-bracket-style``
                        - ``pel-c++-activates-minor-modes``

:PEL Key Prefix: - Globally: **pel:for-c++** : ``<f11> SPC C``
                 - From a buffer in c++-mode: ``<f12>`` and ``<M-f12>``


PEL provides support for the `C++ Programming Language`_ via Emacs built-in
c++-mode.  No activation is necessary since the c-mode is built-in Emacs.
However, configuration  of important editor behaviour such as the
indentation/bracket style and the indentation is completely controlled by user
options listed above and can easily be changed using Emacs customize system.
PEL also provides easy access to commands that can change the CC Mode behaviour
on which the c++-mode is based via the **pel:for-c++** key-map, bounded to the
**F12** key for each buffer in c++-mode.


.. _C++ Programming Language: https://en.wikipedia.org/wiki/C%2B%2B

---------------------------------------------------------------------------

PEL Support for Common Lisp
~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. _𝕻𝔩 - Common Lisp: https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/pl-common-lisp.pdf>

:Language Family: `LISP Programming Language Family`_
:PDF Sheet: `𝕻𝔩 - Common Lisp`_
:PEL Customization: - Group: ``pel-pkg-for-clisp``

                      - Activation:    ``pel-use-common-lisp``.

                      - Configuration:

                        - ``pel-lisp-activates-minor-modes``

:PEL Key Prefix: - Globally: **pel:for-lisp** : ``<f11> SPC L``
                 - From a buffer in lisp-mode: ``<f12>`` and ``<M-f12>``


The file `pel-commonlisp.el`_ is in a very early stage.
It only provides the ``pel-cl-init`` function that is used by ``pel-init`` to
initialize support for Common Lisp when the ``pel-use-common-lisp`` customize
variable is set to **t**.
The ``pel-use-common-lisp`` function sets the indentation rule to the Common
Lisp indentation style.
The ``pel-init`` function also set the variable ``common-lisp-hyperspec-root``
to the directory "~/docs/HyperSpec/".  You can then copy the HyperSpec_ files
inside this directory and Emacs can access them locally.


.. _HyperSpec: http://www.lispworks.com/documentation/HyperSpec/Front/index.htm


---------------------------------------------------------------------------

PEL Support For D
~~~~~~~~~~~~~~~~~

.. _𝕻𝔩 - D: https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/pl-d.pdf

:Language Family: `Curly-Bracket Programming Language`_
:PDF Sheet: `𝕻𝔩 - D`_
:PEL Customization: - Group: ``pel-pkg-for-d``

                      - Activation: ``pel-use-d``
                      - Configuration:

                        - ``pel-d-indent-width``
                        - ``pel-d-tab-width``
                        - ``pel-d-use-tabs``
                        - ``pel-d-bracket-style``
                        - ``pel-d-activates-minor-modes``

:PEL Key Prefix: - Globally: **pel:for-d** : ``<f11> SPC D``
                 - From a buffer in d-mode: ``<f12>`` and ``<M-f12>``


PEL provides support for the `D Programming Language`_ via the `Emacs D-mode`_ and
related external packages that it activates when the **pel-use-d** user option
is set to **t**.  The selection of important editor behaviour such as the
indentation/bracket style and the indentation is completely controlled by user
options.  The defaults are adapted to what is proposed by the `D Style code
guideline`_ and can easily be changed using Emacs customize system.
More information is available in the `PEL PDF document for D`_ .
PEL also provides easy access to commands that can change the CC Mode behaviour
on which the d-mode is based via the **pel:for-d** key-map, bounded to the
**F12** key for each buffer in d-mode.

.. _D Programming Language: https://en.wikipedia.org/wiki/D_(programming_language)
.. _Emacs D-mode: https://github.com/Emacs-D-Mode-Maintainers/Emacs-D-Mode
.. _D Style code guideline: https://dlang.org/dstyle.html
.. _PEL PDF document for D: pdf/pl_d.pdf


---------------------------------------------------------------------------

PEL Support for Elixir
~~~~~~~~~~~~~~~~~~~~~~

.. _𝕻𝔩 - Elixir: https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/pl-elixir.pdf

:Language Family: `BEAM VM Programming Language`_
:PDF Sheet: `𝕻𝔩 - Elixir`_
:PEL Customization: - Group: ``pel-pkg-for-elixir``

                      - Activation:

                        - ``pel-use-elixir``
                        - ``pel-use-alchemist``
                        - ``pel-use-elixir-exunit``
                        - ``pel-use-elixir-lsp``

                      - Configuration:

                        - ``pel-elixir-activates-minor-modes``

:PEL Key Prefix: - Globally: **pel:for-elixir** : ``<f11> SPC x``
                 - From a buffer in elixir-mode: ``<f12>`` and ``<M-f12>``

PEL provides basic support for the `Elixir programming language`_ via the
elixir-mode_ package.  With it the file extensions ``.exs``, ``.ex``, and
``.elixir`` are automatically recognized as being Elixir files.


.. _Elixir programming language: https://en.wikipedia.org/wiki/Elixir_(programming_language)


---------------------------------------------------------------------------

PEL Support for Emacs Lisp
~~~~~~~~~~~~~~~~~~~~~~~~~~

.. _⨋𝕻𝔩 - Emacs Lisp: https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/pl-emacs-lisp.pdf


:Language Family: `LISP Programming Language Family`_
:PDF Sheet: `⨋𝕻𝔩 - Emacs Lisp`_ , `ERT`_ .
:PEL Customization: - Group: ``pel-pkg-for-elisp``

                      - Activation: (*none* to use Emacs Lisp), but there are
                        for other packages:

                        - ``pel-use-macrostep``
                        - ``pel-use-esup``
                        - ``pel-use-re-builder``
                        - ``pel-use-highlight-defined``

                      - Configuration:

                        - ``pel-elisp-activates-minor-modes``

:PEL Key Prefix: - Globally: **pel:for-elisp** : ``<f11> SPC l``
                 - From a buffer in elisp-mode: ``<f12>`` and ``<M-f12>``


The file `pel-lisp.el`_ contains command utilities that help edit Emacs Lisp
code.  Some of them can also be used for other types of Lisp as well.

- ``pel-toggle-lisp-modes`` toggles between ``lisp-interaction-mode`` and
  ``emacs-lisp-mode``.
- ``pel-byte-compile-file-and-load`` byte compiles the file in the current
  buffer and then load it.
- ``pel-lint-elisp-file`` runs Emacs Lisp lint on the current file.


---------------------------------------------------------------------------


PEL Support for Erlang
~~~~~~~~~~~~~~~~~~~~~~

.. _𝕻𝔩 - Erlang:                  https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/pl-erlang.pdf

:Language Family: `BEAM VM Programming Language`_
:PDF Sheet: `𝕻𝔩 - Erlang`_
:PEL Customization: - Group: ``pel-pkg-for-erlang``.  Use ``<f12> <f1>`` from and erlang mode buffer.

                      - Activation:

                        - ``pel-use-erlang``
                        - ``pel-use-edts``
                        - ``pel-use-erlang-ls``
                        - ``pel-use-helm-lsp``
                        - ``pel-use-lsp-ivy``
                        - ``pel-use-treemacs``
                        - ``pel-use-lsp-treemacs``
                        - ``pel-use-lsp-origami``
                        - ``pel-use-erlang-syntax-check``
                        - ``pel-use-hide-comnt``
                        - ``pel-use-iedit``
                        - ``pel-use-smart-dash``
                        - ``pel-use-smartparens``

                      - Configuration:

                        - ``pel-erlang-activates-minor-modes``

:PEL Key Prefix: - Globally: **pel:for-erlang** : ``<f11> SPC e``
                 - From a buffer in erlang-mode: ``<f12>`` and ``<M-f12>``

PEL provides extensive support for the `Erlang Programming Language`_ using
and integrating the following:

- From `Erlang official Emacs support`_ it uses `erlang-mode`_ provided by the
  `erlang.el file`_,

  - PEL extends the facilities of the erlang-mode provided by the `erlang.el
    file`_.

- The `smartparens external package`_ provides extra electrical keys and block
  character pairing control.  PEL code improves its Erlang support.
- The `smart-dash external package`_ provides the ability to easily type
  underscore characters in Erlang terms by typing a dash instead.


For Erlang, PEL provides several enhancements to the default behaviour
provided by the erlang.el code, including the following:

#. `enhanced Erlang-specialized electric key behaviour`_,
#. `block sensitive deletion for Erlang`_,
#. `enhanced Erlang comment insertion`_,
#. `Erlang comments hiding control`_,
#. `outlining support for Erlang`_,
#. `Erlang-specific display rendering of hard tabs`_,
#. `Erlang-specific insertion of hard tabs for indentation`_,
#. `Erlang-specific Indentation control`_,
#. `enhanced navigation in Erlang code`_,
#. `Erlang-specific code transformation commands`_,
#. `enhanced Erlang symbol identification with superword-mode`_,
#. `enhanced tempo-based skeleton code templates`_.






PEL provides access to the Tempo skeleton and yasnippet_
template text insertion systems.  PEL adds functionality to several of the
Erlang skeletons, provides the ability to select several commenting styles via
user option variables that can be customized (use the ``<f12> <f2>`` key from a
buffer in erlang major mode to quickly gain access to the buffer to see and/or
change those variables).
Refer to the `PEL Erlang PDF`_ document for more information.


.. _smartparens external package: https://github.com/Fuco1/smartparens#readme
.. _smart-dash external package: https://melpa.org/#/smart-dash



Enhanced Erlang-specialized Electric Key Behaviour
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

PEL integrates and enhances the electric key behaviour provided by erlang.el,
smartparens and start-dash, adds electric behaviour to some keys and provides
the ability to configure the electric behaviour of the various electric keys
by customization but also dynamically during an editing session.  The electric
keys are:

**RET**:
  Electric newline: inserts a new line and indents. Activated by default.

**<**
  When smartparens-mode is active a ``<<`` automatically inserts the closing
  ``>>`` and leaves the point in between.

**>**
  When following a dash in ``->`` automatically inserts a new line and
  indents.  You can dynamically disable the electric behaviour for the
  instance of this character by typing ``M-1 >`` after the dash.  Use this for
  writing code where the returned value is short.

**.**
  If a period is typed following a dash, as in ``-.`` then ``->`` is inserted
  in code (but not in comment or a string).
  This is another way from typing ``->`` without automatically
  inserting an indented new line.   This behaviour is disabled when the dash
  follows a ``$`` so you can type ``$-.`` and it will not be converted.

**,**
  - A comma typed at the end of an Erlang expression automatically inserts a
    new indented line.
  - A comma inserted after an opening *parens* character (any of ``(``, ``[``,
    ``{`` or ``<``) or inside a balanced pair of these characters can trigger
    the automatic insertion of a space when the
    **pel-erlang-space-after-comma-in-block** uer-option is turned one.
    If the behaviour is enabled by user-option it is possible to disable it
    for the next inserted comma by typing ``M-1 ,``.

**;**
  - Insert a semicolon character and possibly a function clause head prototype
    on the next line.

    - Inserts a function clause head prototype when the selection criteria
      identified by **erlang-electric-comma-criteria** indicates that it
      should be done.

  - Behaves like the normal semicolon when supplied with a numerical arg,
    point is inside string or comment, or when there are non-whitespace
    characters following the point on the current line.

**-**
  When the smart-dash mode is active, typing a dash inserts an underscore
  following any letter, digit or underscore. Inserts a dash otherwise.


With PEL, you can temporary toggle the electric behaviour of one of the above
characters for the current Emacs editing session, either for the current
Erlang buffer or for all Erlang buffers.

- To toggle the electric behaviour of one of these characters in the current
  buffer only, type ``<M-f12> M-``` followed by the character.
- To toggle the electric behaviour for all Erlang buffers, type an argument
  prefix before that key sequence.  For example, to disable the electric
  behaviour of the semicolon in all Erlang buffers,
  type ``M-- <M-f12> M-```.
- You can also toggle the automatic insertion of spaces after comma inside
  blocks by typing ``<M-f12> M-` M-,`` or in all Erlang buffers with
  ``M-- <M-f12> M-` M-,``.

All Electric key behaviour is controlled by customization: the
**pel-erlang-electric-keys** and **pel-erlang-space-after-comma-in-block**
user-options must be enabled to activate the electric behaviour.  By default,
everything is enabled except the automatic space after comma in block.

User-Options for Erlang Electric Key Behaviour
++++++++++++++++++++++++++++++++++++++++++++++


:Group: pel-pkg-for-parens
:Key:   ``<f11> i <f2>``, select Pel Pkg For Parens.

Activate **pel-use-smartparens** to add the ``<< >>`` pair management as
well as other behaviours described in the navigation and code transformation
sections below.


:Group: erlang
:Key:   - Globally: ``<f11> SPC e <f3>``
        - From erlang-mode buffer: ``<f12> <f3>``


erlang-electric-semicolon-insert-blank-lines:
  This variable controls the behaviour of ‘erlang-electric-semicolon’
  when a new function header is generated.  When nil, no blank line is
  inserted between the current line and the new header.  When bound to a
  number it represents the number of blank lines which should be
  inserted.

erlang-new-clause-with-argument:
  Non-nil means that the arguments are cloned when a clause is generated.

  - A new function header can be generated by calls to the function
    ‘erlang-generate-new-clause’ and by use of the electric semicolon.


erlang-next-lines-empty-threshold:
  Number of blank lines required to activate an electric command.

  - Actually, this value controls the behaviour of the function
    ‘erlang-next-lines-empty-p’ which normally is a member of the
    criteria lists controlling the electric commands.  (Please see
    the variables ‘erlang-electric-semicolon-criteria’ and
    ‘erlang-electric-comma-criteria’.)

  - The variable is bound to a threshold value, a number, representing the
    number of lines which must be empty.

  - Setting this variable to zero, electric commands will always be
    triggered by ‘erlang-next-lines-empty-p’, unless inhibited by other
    rules.

  - Should this variable be nil, ‘erlang-next-lines-empty-p’ will never
    trigger an electric command.  The same effect would be reached if the
    function ‘erlang-next-lines-empty-p’ would be removed from the criteria
    lists.

  - Note that even if ‘erlang-next-lines-empty-p’ should not trigger an
    electric command, other functions in the criteria list could.

:Group: pel-erlang-code-style
:Key:   - Globally: ``<f11> SPC e <f2>``, select Pel Erlang Code Style.
        - From erlang-mode buffer: ``<f12> <f2>``, select Pel Erlang Code Style.

pel-erlang-electric-keys:
  List of keys that should have electric behaviour for Erlang buffers.

  - By default PEL activates the electric behaviour of the 4 characters controlled
    by the erlang.el package:  comma, gt, newline and semicolon.  PEL also
    provides electric behaviour to the period, allowing typing ’->’ in code
    by typing '-.' instead.
  - To disable the electric behaviour of a character un-check the
    corresponding box.
  - PEL also supports dynamically toggling the electric behaviour of
    a key with the ``<M-f12> M-``` prefix followed by the specific key.
  - Additionally PEL activates another electric behaviour to the comma,
    by automatically inserting a space after a colon typed inside blocks.
    This is controlled by ‘pel-erlang-space-after-comma-in-blocks’.

pel-erlang-space-after-comma-in-blocks:
  When set the ``erlang-electric-comma`` inserts a space after comma in blocks.

  - For this to work, the ``pel-erlang-electric-keys`` must activate the
    electric comma.
  - You can also dynamically toggle the electric behaviour of the comma key by
    using the ``pel-erlang-comma`` command,  mapped to ``<M-f12> M-` ,`` in
    Erlang buffers.
  - If you want to keep the electric behaviour of the comma key, but temporary
    want to disable adding spaces after a comma inside the current Erlang buffer,
    use the ``<M-f12> M-` M-,`` to toggle this behaviour off and on.



.. _PEL Erlang PDF: https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/pl-erlang.pdf
.. _Erlang Programming Language: https://en.wikipedia.org/wiki/Erlang_(programming_language)
.. _Erlang official Emacs support: https://melpa.org/#/erlang
.. _erlang-mode: https://github.com/erlang/otp/tree/master/lib/tools/emacs
.. _erlang.el file: https://github.com/erlang/otp/blob/master/lib/tools/emacs/erlang.el


Block Sensitive Deletion for Erlang
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

:Group: pel-pkg-for-parens
:Key:   ``<f11> i <f2>``, select Pel Pkg For Parens.

When ``smartparens-mode`` is active (as selected by the
**pel-use-smartparens** user-option), the forward and backward delete keys do
not delete a block delimiter character that is part of balanced block
expression until the content of block is empty.  Once the block is empty the
delete key deletes the entire empty block.


Enhanced Erlang Comment Insertion
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

PEL improves the ability to control the number of '%' characters placed at the
beginning of a comment based on the position into the buffer, the previous
line and also provides the ability to force a fixed number of '%' characters.

The usual command comments a line or a region with ``%`` or ``%%`` depending
on the location inside the buffer.
PEL provides its own command, **pel-erlang-comment-dwim**, bound to the usual
``M-;`` key, that does the same and provides the supplemental behaviour:

- Insert ``%%%`` comments on the very first line of a buffer and lines that
  follow a line that starts with ``%%%`` comments.
- Accepts prefix argument to explicitly identify the number of '%' characters
  to use.  For example, use ``M-3 M-;`` to use ``%%%`` comment style
  regardless of the position inside the buffer.

Erlang Comments Hiding Control
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

:Group: pel-pkg-for-hide-show
:Key: ``<f11> M-/ <f2>``

When the **pel-use-hide-comnt** user-option is turned on PEL activates the
hide-comnt library and provides the ``<f11 ; ;`` key binding to toggle the
display of all comments in the active region or the complete buffer.


Outlining Support for Erlang
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The erlang.el library supports simple outlining is supported by the
outline-minor-mode provided by Emacs built-in outline library. No IDE-like
package is required to use it; it's always available for the erlang-mode.

To us it in Erlang buffers you must first activate the **outline-minor-mode**.
PEL provides the ``<f11> M-l`` key binding for that.  Then you can use any of
the outline commands to hide or expand the body of the Erlang functions and
navigate through them easily. The default key binding for the key is under the
``C-c @`` key prefix.  PEL adds the ``<f2>`` key prefix for the same commands.

You can also modify the default key binding and select something else than
``C-c @`` by modifying the **outline-mode-minor-prefix** user option.  PEL
provides the ``<f11> SPC M-l <f3>`` key binding to access the outline
customization buffer.

More information on outline is available in the `⅀ Outline PDF`_.

.. _⅀ Outline PDF: https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/outline.pdf


Erlang-specific Display Rendering of hard tabs
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The visual horizontal rendering of hard tab present in the Erlang source code
is controlled by the value of the **tab-width** variable as seen by Emacs for
an Erlang buffer.  Emacs provides the **tab-width** global user-option for
this and PEL provides the Erlang-specific **pel-erlang-tab-width**
user-option and sets tab-width to the value of pel-erlang-tab-with in each
erlang-mode buffer to control the display rendering of hard tab characters in
Erlang source code.

:Group: editing-basics
:Key:   ``<f11> <f2> g editing-basics RET``

tab-width:
   Distance between tab stops (for display of tab characters), in columns.

   - This controls the display width of a TAB character, and not
     the size of an indentation step.
     This should be an integer greater than zero.
   - PEL overrides this value with the value of **pel-erlang-tab-width** for
     all buffers using the erlang-mode.

:Group: pel-pkg-for-erlang, pel-erlang-code-style
:Key:   - Globally: ``<f11> SPC e <f2>``
        - From erlang-mode buffer: ``<f12> <f2>``

pel-erlang-tab-width:
   Distance between tab stop for Erlang source code.

   - PEL stores this in ``tab-width`` when editing buffer with Erlang source.
   - This does *NOT* control the indentation in Erlang source code.
     It is used, however, to control the display rendering of hard tab
     characters inserted inside source code and by commands that move
     point to tab stop positions such as ``tab-to-tab-stop``, and the
     display of hard TAB characters.
   - The indentation of Erlang code is mostly controlled by
     ``erlang-indent-level``. If ``pel-erlang-tab-width`` differs
     ``erlang-indent-level`` then ``pel-erlang-tab-width`` should be a
     multiple of ``erlang-indent-level`` in case hard tabs have been
     inserted inside the source code.
   - Values in the [2, 8] range are accepted.


Erlang-specific Insertion of hard tabs for indentation
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The insertion of hard tab characters (ASCII Horizontal Tab, 0x09) when
horizontal indentation space is inserted inside the Erlang source code file is
controlled by the value of **indent-tab-mode** variable in the current
buffer.   The default value is normally controlled by the customizable value
it has globally.  However, PEL provides the **pel-erlang-use-tabs**
user-option and sets the value of ``indent-tab-mode`` with the value of
``pel-erlang-use-tabs`` in all erlang-mode buffers to explicitly control this
behaviour in Erlang source code.

:Group: indent
:Key:   ``<f11> TAB <f3> 1``

indent-tabs-mode:
  Indentation can insert tabs if this is non-nil.

:Group: pel-pkg-for-erlang, pel-erlang-code-style
:Key:   - Globally: ``<f11> SPC e <f2>``
        - From erlang-mode buffer: ``<f12> <f2>``


pel-erlang-use-tabs:
  Value of ‘indent-tabs-mode’ for editing Erlang source code.

  - If set to nil: only spaces are used for indentation.
  - If set to t: hard tabs are used when possible.

Erlang-Specific Indentation Control
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Indentation of Erlang source code is controlled by several user-options,
listed below.

PEL provides an additional control for the Emacs
**fill-column** which identifies the column where automatic wrapping is done:
the **pel-erlang-fill-column** value is used for ``fill-column``
in erlang-mode buffers.

NOTE for new Emacs users:
  New Emacs users may find the behaviour of the TAB key puzzling.
  Pressing the TAB key does not insert spaces to the location of the
  indentation level the way several editors handle it.

  Instead, pressing TAB will adjust the horizontal indentation of the current
  line if the line indentation must be adjusted and will do nothing otherwise.
  However, you can press the TAB key *anywhere* on the line to adjust its
  indentation!  You can also select a larger text section and hit TAB to
  adjust the indentation of all the lines!

  It takes some time to get used to this behaviour but soon you will find it
  invaluable. Emacs automatically adjusts the indentation of the code using
  its knowledge of the Erlang syntax and the various user-options described
  below.


:Group: erlang
:Key:   - Globally: ``<f11> SPC e <f3>``
        - From erlang-mode buffer: ``<f12> <f3>``

erlang-argument-indent:
  Indentation of the first argument in a function call.

  - When nil, indent to the column after the ‘(’ of the
    function.
  - Default: 2

erlang-icr-indent:
  Indentation of Erlang if/case/receive patterns.

  - nil means keeping default behaviour.
  - When non-nil, indent to the column of if/case/receive.
  - Default: nil

erlang-indent-guard:
  Indentation of Erlang guards.

  - Default: 2

erlang-indent-level:
  Indentation of Erlang calls/clauses within blocks.

  - Default: 4

erlang-tab-always-indent:
  Non-nil means the TAB key in Erlang mode should always re-indent the current line,
  regardless of where in the line point is when the TAB key is pressed.  This
  is stored into the global ``tab-always-indent``.


  - Default: t

:Group: pel-pkg-for-erlang, pel-erlang-code-style
:Key:   - Globally: ``<f11> SPC e <f2>``
        - From erlang-mode buffer: ``<f12> <f2>``

pel-erlang-fill-column:
  Column beyond which automatic line-wrapping should happen in Erlang code.

  - Can either be nil or an integer value.
  - When set to nil, Emacs user option variable ‘fill-column’ value
    is used for ‘erlang-mode’ buffers, otherwise the integer value specified by
    this value is stored in ‘fill-column’ for Erlang source code files.
  - The default is 100, a value recommended by the
    `Inaka’s Erlang Coding Standards & Guidelines`_.




:Group: indent
:Key:   ``<f11> TAB <f3> 1``


indent-tab-mode:
  Indentation can insert tabs if this is non-nil.


tab-stop-list:
  List of tab stop positions used by ‘tab-to-tab-stop’.

  - This should be nil, or a list of integers, ordered from smallest to largest.
  - It implicitly extends to infinity through repetition of the last step.
    For example, (1 2 5) is equivalent to (1 2 5 8 11 ...).  If the list has
    fewer than 2 elements, ‘tab-width’ is used as the "last step".
    A value of nil means a tab stop every ‘tab-width’ columns.
  - For Erlang this is nil by default.


Additionally, the following non-customizable variable is used and set by
erlang.el:

indent-line-function:
  Function to indent the current line.

  - This function will be called with no arguments.
  - If it is called somewhere where auto-indentation cannot be done
    (e.g. inside a string), the function should simply return ‘noindent’.
  - Setting this function is all you need to make TAB indent appropriately.
    Don’t rebind TAB unless you really need to.

  - Default for Erlang buffers: ``erlang-indent-command``


..
   c-default-style:

   c-basic-offset:

   c-set-offset:

.. _Inaka’s Erlang Coding Standards & Guidelines: https://github.com/inaka/erlang_guidelines#100-column-per-line


Enhanced Navigation in Erlang Code
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

:Group: pel-pkg-for-parens
:Key:   ``<f11> i <f2>``, select Pel Pkg For Parens.

PEL provides specialized navigation commands extending what erlang.el
and smartparens provide when the **pel-use-smartparens** user-option is turned
on.  The extra commands include:

- move backward and forward to blocks jumping over strings and comments,
- move down and up a block *tree* (in and out of nested lists).


See the `𝕻𝔩 - Erlang`_ PDF for more information and examples.


Erlang-specific Code Transformation Commands
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

:Group: pel-pkg-for-parens
:Key:   ``<f11> i <f2>``, select Pel Pkg For Parens.

PEL provides enhanced specialized code transformation commands for Erlang
that smartparens provide.  PEL enhances the smartparens code;  smartparens by
itself does not deal properly with comma separated blocks of values such as
Erlang lists or function argument lists.

The additional commands operate on balanced pair blocks of ``( )``, ``[ ]``,
``{ }`` and ``<< >>`` for some of them.  The commands include:

- transpose
- slurp and barf
- wrap, re-wrap and unwrap blocks,
- split and join blocks.

See the `𝕻𝔩 - Erlang`_ PDF for more information and examples.


:Group: pel-pkg-for-highlight
:Key:   ``<f11> h <f2> 1``


PEL also provides access to the iedit-mode; a very-useful minor mode you can
use to search and modify various names inside a buffer.  Activate iedit by
setting the **pel-use-iedit** user-option on.


Enhanced Erlang Symbol Identification With superword-mode
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

:Group: pel-pkg-for-text-modes
:Key: ``<f11> t <f2>``

As Erlang uses `snake-case`_, PEL automatically activates the `superword-mode`_
for Erlang by including the erlang-mode in the
**pel-modes-activating-superword-mode** user variable default.  PEL also
provides key-bindings to the command that dynamically toggles the mode.
Inside any erlang-mode buffer you can type ``<f12> M-p`` or the ``<M-f12>
M-p`` key sequences to toggle the mode on and off.

Using the superword-mode helps searching for Erlang terms.  It also helps
selecting them when using the extremely useful er/expand-region command which
is bound to the ``M-=`` key (and also the ``<f11> . =`` key sequence).

.. _snake-case: https://en.wikipedia.org/wiki/Snake_case
.. _superword-mode:  https://www.gnu.org/software/emacs/manual/html_node/emacs/Misc-for-Programs.html#Misc-for-Programs


Enhanced Tempo-based Skeleton Code Templates
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

PEL enhances the Emacs Tempo-based skeletons code templates and provides
a large set of specialized and customizable code templates for different types
of Erlang source code files.

PEL provides a large set of customizable user-options, available inside two
different customization groups:

:Group: - pel-erlang-code-style
        - pel-erlang-skeleton-control
:Keys: - globally: ``<f11> SPC e <f2>``,  select Pel Erlang Code Style
       - from erlang-mode buffer: ``<f12> <f2>``, select Pel Erlang Code Style

If you have not already set your full name and email address in Emacs
customization you will also want to set the following user-options as they are
used in all PEL Tempo Skeleton templates for files.  Use ``<f11> <f2> o`` and
type the user-option name at the prompt to quickly access those user-options.

==================================== ======= =====================================
Variable                             Value   Description
==================================== ======= =====================================
user-full-name                       ➽       Set your full name.
user-mail-address                    ➽       Set you email address in this
                                             variable.  Emacs tries to infer it
                                             but it may only contain your user
                                             name and your computer name.
==================================== ======= =====================================


The commands to insert the Erlang code template are available via the Erlang
section of Emacs menu (accessible via ``<f10>`` under PEL).  It looks like
this:

.. figure:: res/erlang-templates-menu.png

PEL also provide key bindings for all of the templates.  They are all bound
under the ``<f12> <f12>`` key prefix and listed inside the `𝕻𝔩 - Erlang`_ PDF.

Examples of some of the possible templates for Erlang are available in the
`example/templates/erlang repo directory`_.  For a complete description on how
to work with PEL tempo skeleton templates see the description of the templates
for the C language in the section titled `C Code Templates`_.  The same
control mechanism are available for Erlang with Erlang specific code generated
for various type of Erlang files.




.. _example/templates/erlang repo directory: ../example/templates/erlang

---------------------------------------------------------------------------

PEL Support for Forth
~~~~~~~~~~~~~~~~~~~~~

.. _𝕻𝔩- Forth: https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/pl-forth.pdf
.. _Concatenative Programming Language: https://en.wikipedia.org/wiki/Concatenative_programming_language

:Language Family: Stack-Based, `Concatenative Programming Language`_
:PDF Sheet: `𝕻𝔩- Forth`_
:PEL Customization: - Group: ``pel-pkg-for-forth``

                      - Activation: ``pel-use-forth``

                      - Configuration:

                        - ``pel-forth-activates-minor-modes``

:PEL Key Prefix: - Globally: **pel:for-forth** : ``<f11> SPC f``
                 - From a buffer in forth-mode: ``<f12>`` and ``<M-f12>``

PEL provides basic support for the `Forth programming language`_ via the
forth-mode_ package.  With it the file extensions ``.f``, ``.fs``, ``.fth``, and
``.4th`` are automatically recognized as being Forth files.

.. _Forth programming language: https://en.wikipedia.org/wiki/Forth_(programming_language)


---------------------------------------------------------------------------

PEL Support for Gleam
~~~~~~~~~~~~~~~~~~~~~

.. _𝕻𝔩 - Gleam: https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/pl-gleam.pdf

:Language Family: `BEAM VM Programming Language`_
:PDF Sheet: `𝕻𝔩 - Gleam`_
:PEL Customization: - Group: ``pel-pkg-for-gleam``

                      - Activation: ``pel-use-gleam``
                      - Configuration:

                        - ``pel-gleam-activates-minor-modes``

:PEL Key Prefix: - Globally: **pel:for-gleam** : ``<f11> SPC M-G``
                 - From a buffer in gleam-mode: ``<f12>`` and ``<M-f12>``

PEL support for Gleam is experimental.
See the `Gleam PDF Sheet <https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/pl-gleam.pdf>`_
for more information.

---------------------------------------------------------------------------

PEL Support for Go
~~~~~~~~~~~~~~~~~~

.. _𝕻𝔩 - Go: https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/pl-go.pdf

:Language Family: `Curly-Bracket Programming Language`_
:PDF Sheet: `𝕻𝔩 - Go`_
:PEL Customization: - Group: ``pel-pkg-for-go``

                      - Activation: ``pel-use-go``
                      - Configuration:

                        - ``pel-go-tab-width``
                        - ``pel-go-activates-minor-modes``

:PEL Key Prefix: - Globally: **pel:for-go** : ``<f11> SPC g``
                 - From a buffer in go-mode: ``<f12>`` and ``<M-f12>``

PEL support for Go is experimental using the go-mode, go-flymake and
go-flycheck.
See the `Go PDF Sheet <https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/pl-go.pdf>`_
for more information.

---------------------------------------------------------------------------

PEL Support for Janet
~~~~~~~~~~~~~~~~~~~~~

.. _𝕻𝔩 - Janet: https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/pl-janet.pdf

:Language Family: LISP-like Programming Language
:PDF Sheet: `𝕻𝔩 - Janet`_
:PEL Customization: - Group: ``pel-pkg-for-janet``

                      - Activation:

                        - ``pel-use-janet-mode``
                        - ``pel-use-ijanet-mode``
                        - ``pel-use-inf-janet``

                      - Configuration:

                        - ``pel-janet-activates-minor-modes``

:PEL Key Prefix: - Globally: **pel:for-janet** : ``<f11> SPC T``
                 - From a buffer in janet-mode: ``<f12>`` and ``<M-f12>``

PEL support for Janet is experimental.
See the `Janet PDF Sheet <https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/pl-janet.pdf>`_
for more information.


---------------------------------------------------------------------------

PEL Support for Julia
~~~~~~~~~~~~~~~~~~~~~

.. _𝕻𝔩 - Julia: https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/pl-julia.pdf

:PDF Sheet: `𝕻𝔩 - Julia`_
:PEL Customization: - Group: ``pel-pkg-for-julia``

                      - Activation: ``pel-use-julia``

                      - Configuration:

                        - ``pel-julia-activates-minor-modes``

:PEL Key Prefix: - Globally: **pel:for-julia** : ``<f11> SPC j``
                 - From a buffer in julia-mode: ``<f12>`` and ``<M-f12>``

PEL provides basic support for the `Julia programming language`_ via the
julia-mode_ package.  With it the file extensions ``.jl``
are automatically recognized as being Julia files.

.. Julia programming language: https://en.wikipedia.org/wiki/Julia_(programming_language)



---------------------------------------------------------------------------

PEL Support for LFE
~~~~~~~~~~~~~~~~~~~


.. _𝕻𝔩 - LFE: https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/pl-lfe.pdf

:Language Family: - `BEAM VM Programming Language`_
                  - `LISP Programming Language Family`_

:PDF Sheet: `𝕻𝔩 - LFE`_
:PEL Customization: - Group: ``pel-pkg-for-lfe``

                      - Activation: ``pel-use-lfe``
                      - Configuration:

                        - ``pel-lfe-activates-minor-modes``

:PEL Key Prefix: - Globally: **pel:for-lfe** : ``<f11> SPC C-l``
                 - From a buffer in lfe-mode: ``<f12>`` and ``<M-f12>``

PEL support for LFE, `Lisp Flavored Erlang`_, inherits from Emacs Lisp support and some extra feature
supported by PEL.
See the `LFE PDF Sheet <https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/pl-lfe.pdf>`_
for more information.


.. _Lisp Flavored Erlang: https://en.wikipedia.org/wiki/LFE_(programming_language)


---------------------------------------------------------------------------

PEL Support for NetRexx
~~~~~~~~~~~~~~~~~~~~~~~

.. _𝕻𝔩 - NetRexx: https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/pl-netrexx.pdf


:PDF Sheet: `𝕻𝔩 - NetRexx`_
:PEL Customization: - Group: ``pel-pkg-for-netrexx``

                      - Activation: ``pel-use-netrexx``
                      - Configuration:

                        - ``pel-netrexx-activates-minor-modes``

:PEL Key Prefix: - Globally: **pel:for-netrexx** : ``<f11> SPC N``
                 - From a buffer in netrexx-mode: ``<f12>`` and ``<M-f12>``

PEL support for NetRexx is experimental.
See the `NetRexx PDF Sheet <https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/pl-netrexx.pdf>`_
for more information.

---------------------------------------------------------------------------

PEL Support for Nim
~~~~~~~~~~~~~~~~~~~

.. _𝕻𝔩 - Nim: https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/pl-nim.pdf

:PDF Sheet: `𝕻𝔩 - Nim`_
:PEL Customization: - Group: ``pel-pkg-for-nim``

                      - Activation: ``pel-use-nim``
                      - Configuration:

                        - ``pel-nim-activates-minor-modes``

:PEL Key Prefix: - Globally: **pel:for-nim** : ``<f11> SPC n``
                 - From a buffer in nim-mode: ``<f12>`` and ``<M-f12>``

PEL support for Nim is experimental.
See the `Nim PDF Sheet <https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/pl-nim.pdf>`_
for more information.


---------------------------------------------------------------------------

PEL Support for OCaml
~~~~~~~~~~~~~~~~~~~~~

.. _𝕻𝔩 - OCaml: https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/pl-ocaml.pdf

:Language Family: ML
:PDF Sheet: `𝕻𝔩 - OCaml`_
:PEL Customization: - Group: ``pel-pkg-for-ocaml``

                      - Activation:

                        - ``pel-use-ocaml``
                        - ``pel-use-caml-mode``
                        - ``pel-use-tuareg``
                        - ``pel-use-merlin``

:PEL Key Prefix: - Globally: **pel:for-ocaml** : ``<f11> SPC o``
                 - From a buffer in caml-mode: ``<f12>`` and ``<M-f12>``

PEL support for OCaml is experimental.
See the `OCaml PDF Sheet <https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/pl-ocaml.pdf>`_
for more information.


---------------------------------------------------------------------------

PEL Support for Perl
~~~~~~~~~~~~~~~~~~~~

.. _𝕻𝔩 - Perl: https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/pl-perl.pdf

:PDF Sheet: `𝕻𝔩 - Perl`_
:PEL Customization: - Group: ``pel-pkg-for-perl``

                      - Activation: ``pel-use-perl``
                      - Configuration:

                        - ``pel-perl-activates-minor-modes``

:PEL Key Prefix: - Globally: **pel:for-perl** : ``<f11> SPC P``
                 - From a buffer in perl-mode: ``<f12>`` and ``<M-f12>``

PEL support for Perl is experimental.
See the `Perl PDF Sheet <https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/pl-perl.pdf>`_
for more information.


---------------------------------------------------------------------------

PEL Support for Python
~~~~~~~~~~~~~~~~~~~~~~

.. _𝕻𝔩 - Python: https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/pl-python.pdf

:PDF Sheet: `𝕻𝔩 - Python`_
:PEL Customization: - Group: ``pel-pkg-for-python``

                      - Activation: ``pel-use-python``

                      - Configuration:

                        - ``pel-python-activates-minor-modes``

:PEL Key Prefix: - Globally: **pel:for-python** : ``<f11> SPC p``
                 - From a buffer in python-mode: ``<f12>`` and ``<M-f12>``

PEL provides basic support for the `Python programming language`_ via the
python-mode_ package.  With it the file extensions ``.py``, ``.pyi`` and ``.pyw``
are automatically recognized as being Python files.

.. _Python programming language: https://en.wikipedia.org/wiki/Python_(programming_language)


---------------------------------------------------------------------------

PEL Support for REXX
~~~~~~~~~~~~~~~~~~~~

.. _𝕻𝔩 - REXX: https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/pl-rexx.pdf

:PDF Sheet: `𝕻𝔩 - REXX`_
:PEL Customization: - Group: ``pel-pkg-for-rexx``

                      - Activation: ``pel-use-rexx``

                      - Configuration:

                        - ``pel-rexx-activates-minor-modes``

:PEL Key Prefix: - Globally: **pel:for-rexx** : ``<f11> SPC R``
                 - From a buffer in rexx-mode: ``<f12>`` and ``<M-f12>``

PEL provides basic support for the `REXX programming language`_ via the
forth-mode_ package.  With it the file extensions ``.rexx``, ``.elx``, ``.ncomm``, and
``.cpr`` are automatically recognized as being REXX files.

.. _REXX programming language: https://en.wikipedia.org/wiki/Rexx


---------------------------------------------------------------------------

PEL Support for Ruby
~~~~~~~~~~~~~~~~~~~~

.. _𝕻𝔩 - Ruby: https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/pl-ruby.pdf

:PDF Sheet: `𝕻𝔩 - Ruby`_
:PEL Customization: - Group: ``pel-pkg-for-ruby``

                      - Activation: ``pel-use-ruby``
                      - Configuration:

                        - ``pel-ruby-activates-minor-modes``

:PEL Key Prefix: - Globally: **pel:for-ruby** : ``<f11> SPC U``
                 - From a buffer in ruby-mode: ``<f12>`` and ``<M-f12>``

PEL support for Ruby is experimental.
See the `Ruby PDF Sheet <https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/pl-ruby.pdf>`_
for more information.


---------------------------------------------------------------------------

PEL Support for Rust
~~~~~~~~~~~~~~~~~~~~

.. _𝕻𝔩 - Rust: https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/pl-rust.pdf

:PDF Sheet: `𝕻𝔩 - Rust`_
:PEL Customization: - Group: ``pel-pkg-for-rust``

                      - Activation:

                        - ``pel-use-rust``
                        - ``pel-use-rust-mode``
                        - ``pel-use-rustic``
                        - ``pel-use-flycheck-rust``
                        - ``pel-use-emacs-racer``
                        - ``pel-use-cargo``

                      - Configuration:

                        - ``pel-rust-activates-minor-modes``

:PEL Key Prefix: - Globally: **pel:for-rust** : ``<f11> SPC r``
                 - From a buffer in rust-mode: ``<f12>`` and ``<M-f12>``

PEL support for Rust is experimental using the rust-mode, rustic,
flycheck-rust, emacs-racer and cargo.

See the `Rust PDF Sheet <https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/pl-rust.pdf>`_
for more information.

---------------------------------------------------------------------------

PEL Support for Unix Shell
~~~~~~~~~~~~~~~~~~~~~~~~~~

.. _𝕻𝔩 - Unix Shell: https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/pl-sh.pdf

:PDF Sheet: `𝕻𝔩 - Unix Shell`_
:PEL Customization: - Group: ``pel-pkg-for-sh``

                      - Activation: ``pel-use-sh``
                      - Configuration:

                        - ``pel-make-script-executable``
                        - ``pel-shell-sourced-script-file-name-prefix``
                        - ``pel-shell-script-extensions``
                        - ``pel-sh-activates-minor-modes``

:PEL Key Prefix: - Globally: **pel:for-sh** : ``<f11> SPC g``
                 - From a buffer in sh-mode: ``<f12>`` and ``<M-f12>``

PEL support several Unix Shell using Emacs built-in support for them and
extending it with some extra code.

See the `Unix Shell PDF Sheet <https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/pl-sh.pdf>`_
for more information.

---------------------------------------------------------------------------

PEL Support for V
~~~~~~~~~~~~~~~~~

.. _𝕻𝔩 - V: https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/pl-v.pdf

:PDF Sheet: `𝕻𝔩 - V`_
:PEL Customization: - Group: ``pel-pkg-for-v``

                      - Activation: ``pel-use-v``

                      - Configuration:

                        - ``pel-v-activates-minor-modes``

:PEL Key Prefix: - Globally: **pel:for-v** : ``<f11> SPC v``
                 - From a buffer in v-mode: ``<f12>`` and ``<M-f12>``

PEL support for the emerging V language is experimental using the v-mode.

See the `V PDF Sheet <https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/pl-v.pdf>`_
for more information.


---------------------------------------------------------------------------

.. ---------------------------------------------------------------------------

PEL Prompt Utilities
--------------------

:PDF Sheet: *none*
:PEL Customization: *none*
:PEL Key Prefix: *none*

The file `pel-prompt.el`_ is a utility file and for now only contains one
function: ``pel-y-n-e-or-l-p`` which prompts and accepts various types of
responses.  It is used by the ``pel-find-file-at-point-in-window`` command.
It's a rather specialized prompting utility with a rather strange name...

PEL Project Management Utilities
--------------------------------

:PDF Sheet: `Projectile Project Interaction Manager`_.
:PEL Customization: ``pel-use-projectile``
:PEL Key Prefix: **projectile-command-map** : ``<f8>``

PEL supports the `projectile`_ external package when the ``pel-use-projectile``
user option is set to either **t** or **use-from-start**.  If you set it to
**t** it makes it available but does not activate it when Emacs starts.  If you
set it to **use-from-start** it activates it when Emacs starts.

PEL uses the ``<f8>`` key as the projectile prefix and adds some key bindings
into it.
You can toggle the activation of this prefix key via the ``<f11> <f8>`` key sequence.

The use of projectile activates more grep facilities, as described in the
`PEL Grep Support`_ section.

More information is available inside the `Projectile Project Interaction
Manager`_ PDF documentation.



PEL Rectangle Support
---------------------

:PDF Sheet: `Rectangles`_.
:PEL Customization: *none*
:PEL Key Prefix: *none*

PEL adds a couple of key bindings to support the rectangle area editing.
More information about Emacs rectangle area editing is available in the
`Rectangles`_ PDF document.


PEL Register Management Utilities
---------------------------------

:PDF Sheet: `Registers`_.
:PEL Customization: *none*
:PEL Key Prefix: **pel::register** : ``<f11> r``

The file `pel-register.el`_ provides the following commands to help manage
registers:

- ``pel-filename-to-register``,
- ``pel-point-to-register``,
- ``pel-copy-to-register``,
- ``pel-copy-rectangle-to-register``,
- ``pel-window-configuration-to-register``,
- ``pel-frameset-to-register``,
- ``pel-number-to-register``,
- ``pel-kmacro-to-register``.

The `Registers`_ PDF document provides more information.

PEL Scrolling
-------------

:PDF Sheet: `Scrolling`_.
:PEL Customization: ``pel-smooth-scrolling``.
:PEL Key Prefix: **pel:scroll** : ``<f11> |``

The `pel-scroll`_ file provides a set of window scrolling facilities.

The following 2 commands are used to scroll the current window, and
other windows that may be placed inside the PEL window scroll group:

- ``pel-scroll-up`` which scrolls text up,
- ``pel-scroll-down`` which scrolls text down.

The file also provides the creation and management of a group of
windows into the *PEL window scroll sync* group, a list stored inside
the ``pel-in-scroll-sync`` variable identifying windows that will be
scrolled together.

The following commands are used to activate and manage the
*PEL window scroll sync* group:

- ``pel-toggle-scroll-sync`` toggles scroll lock on/off.  When turning it on
  it locks scrolling of the current and the next window.
- ``pel-add-window-to-scroll-sync`` adds the current window to the already
  existing group of scroll locked windows.  If there is none it locks
  scrolling of the current and the next window.
- ``pel-remove-window-from-scroll-sync`` removes the current window from the
  group of scroll locked windows.  Removing the last one disables the
  window scroll sync.  If only one window is left in the group the command
  informs the user but allows it.  That way another window can be added to
  the group.

The scrolling of multiple windows is currently only performed when the
following commands are used:

- ``pel-scroll-up`` which scrolls text up,
- ``pel-scroll-down`` which scrolls text down,
- ``pel-home`` and ``pel-end``, defined in ``pel-navigation``, which move
  point the the beginning or end of current field, line, window or buffer.
  See `PEL Navigation Support`_.

When the `smooth scrolling package`_ is available and ``pel-smooth-scrolling``
user option is set to **t**, PEL provide a key binding to toggle smooth
scrolling on and off.  See the `Scrolling`_ PDF table for more info.

.. _smooth scrolling package: https://melpa.org/#/smooth-scrolling


PEL Search and Replace Support Utilities
----------------------------------------

:PDF Sheet: `Search and Replace`_.
:PEL Customization: - ``pel-initial-regexp-engine``
                    - ``pel-initial-search-tool``
                    - ``pel-use-anzu``
                    - ``pel-use-pcre2el``
                    - ``pel-use-regex-tool``
                    - ``pel-use-swiper``
                    - ``pel-use-visual-regexp-steroids``
                    - ``pel-use-visual-regexp``
                    - ``pel-use-xr``


:PEL Key Prefix: **pel:search-replace** : ``<f11> s``

The `pel-search.el`_ file provides 2 commands to change the value of two Emacs
variables that control the search behaviour: ``case-fold-search`` and
``search-upper-case``, and 1 command to display and interpret their current
value:

- ``pel-toggle-case-fold-search`` toggles search case sensitivity in the current
  buffer.
- ``pel-toggle-search-upper-case`` changes the sensitivity behaviour of yank in
  search prompt between the following:

  - *nil* : upper case don't force case sensitivity,
  - *t* : upper case force case sensitivity,
  - *not-yanks* : upper case force case sensitivity, and
    lower case text when yank in search mini-buffer.

- ``pel-show-search-case-state`` displays the search behaviour in the current
  buffer.

PEL also integrates several search enhancement packages when their corresponding
user option variable are set to **t**. See the above ``Customization`` list.
With those you can set the regular expression engine and the search tool you
want to use by default and you can change them dynamically during editing.

See the PDF `Search and Replace`_ document for more information.

PEL Session Support
-------------------

:PDF Sheet: `Sessions`_.
:PEL Customization: ``pel-use-desktop``.
:PEL Key Prefix: **pel:session** : ``<f11> S``

PEL can activate several packages that manage sessions (also called desktops): a
set of buffer and windows setting that can be stiored and restored later.  All
activation is done by the ``pel-use-desktop`` user option variable.
See the `Sessions`_ PDF document for more information and the key bindings: they
all use the **pel:session** key prefix, which PEL binds to ``<f11> S``.


PEL Shell Support
-----------------

:PDF Sheet: `Shells`_.
:PEL Customization: ``pel-use-erlang``.
:PEL Key Prefix: **pel:execute** : ``<f11> z``

PEL provides the **pel:execute** (``<f11> z``) key binding to provide access to
various types of shells, terminals and REPL buffers from within Emacs as
described in the `Shells`_ PDF table.

PEL also provides  access to several the REPL of several programming
languages, via the **pel:repl** key prefix, mapped to ``<f11> z r``.  The
programming language REPLs are also accessible via the ``<f12> z`` key of
their respective major mode.

PEL Sorting Support
-------------------

:PDF Sheet: `Sorting`_.
:PEL Customization: *none*
:PEL Key Prefix: **pel:order** : ``<f11> o``


PEL provides the **pel:order** (``<f11> o``) key binding to provide access to
Emacs commands you can use to sort (*order*) text in various ways as described
in the `Sorting`_ PDF table.


PEL Speedbar Management
-----------------------

:PDF Sheet: `Speedbar`_.
:PEL Customization: - Group: ``pel-pkg-for-speedbar``

                      - Activation: ``pel-use-speedbar``
                      - Configuration: ``pel-prefer-sr-speedbar-in-terminal``

:PEL Key Prefix: **pel:speedbar** : ``<f11> M-s``

The file `pel-speedbar.el`_ manages the accessibility and use of Emacs speed-bars:
both Emacs native Speedbar and the `SR-Speedbar`_ external package.
When the ``pel-use-speedbar`` user option is set to **t** PEL provides
key bindings for activating the speed-bars and provide some management
facilities. As shown in the PDF `Speedbar`_ table, Plus
default key bindings for those use the ``<f11> M-s`` prefix.

PEL manages what type of speed-bar is used.  And that depends on whether Emacs
is running in graphics mode or in terminal (TTY) mode and whether SR-Speedbar
is available.  Note that once one type of speed-bar has been opened inside an
Emacs session it is currently not possible to use the other type.

In graphics mode, both are equally functional, but in terminal mode
SR-speedbar is clearly superior because Speedbar will take over the
entire frame while SR-Speedbar uses only one of the windows.

- To open a speed-bar, use the ``open-close-speedbar`` command.

  - If SR-speedbar is not available, Speedbar is used.
  - If SR-speedbar is available, when Emacs runs in graphics mode, then
    ``pel-open-close-speedbar`` prompts the first time it's called to select
    which one to use.
    When Emacs runs in terminal mode, ``pel-open-close-speedbar`` prompts
    only if the customization variable ``pel-prefer-sr-speedbar-in-terminal``
    is nil, otherwise it automatically selects SR-Speedbar, which is more
    convenient.

- To close the currently opened speed-bar, use ``open-close-speedbar``
  again.
- When using a the SR-Speedbar you can use the ``pel-toggle-to-speedbar`` command to
  quickly move point between your current window and the SR-Speedbar window.
- Force a refresh of the speed-bar contents with the ``pel-speedbar-refresh``
  command.
- By default the speed-bar does not show the
  `Emacs level-1 and level-2 hidden files`_. To toggle the display of the
  level-1 hidden files, use the ``pel-speedbar-toggle-show-all-files`` command.
  It will warn if no speed-bar is opened.
- The speed-bar can display the source code file tags.
  To toggle sorting of the tags use the ``pel-speedbar-toggle-sorting`` command.
  It will warn if no speed-bar is opened.
- When Emacs is running in graphics mode, it can use icons for the speed-bar
  nodes. Toggle between the use of icons and simple ASCII characters with
  ``pel-speedbar-toggle-images``.
  It will warn if no speed-bar is opened.


.. _Emacs level-1 and level-2 hidden files: https://www.gnu.org/software/emacs/manual/html_node/speedbar/Hidden-Files.html#Hidden-Files


PEL Spell Checking Support
--------------------------

:PDF Sheet: `Spell Checking`_.
:PEL Customization: ``pel-spell-check-tools``
:PEL Key Prefix: **pel:spell** : ``<f11> $``

To use spell checking in Emacs, you must first configure it.
See the section titled `Configure Spell Checking`_.

The file `pel-spell.el`_ contains
spell checking utilities that detect and display what spell check mode is
active, and initialization code that fixes a problem with Flyspell pop-up
menu when Emacs runs in terminal (TTY) mode.

One of the goal of this file is to avoid loading either Ispell or Flyspell
until they are actually required while providing a function that can
configure these utilities with the information stored inside the
``pel-spell-check-tools`` user option variable: the function
``pel-spell-init-from-user-option``.

This sets up the path to your spell checking dictionary and if Emacs is running
in terminal (TTY) mode, it allows flyspell pop-up menus to work properly by
defining and using the function ``pel-spell-flyspell-emacs-popup-textual`` that
contains the fix.

-  *Credits*:

   Code of pel-spell-flyspell-emacs-popup-textual was taken from
   https://www.emacswiki.org/emacs/FlySpell.  In PEL it is renamed
   and defined lazily when running in terminal mode.


The file also provides the ``pel-spell-show-use`` command, which displays
information about the spell checking programs used, their version and the path
to the main dictionary and your personal dictionary

- *Limitations*:

  Extraction of spell programs version string done by the function
  ``pel-spell-program-version-string`` works if the version text is
  printed on the first line only.  That works for the followings:

  - aspell_ 0.60.6.1
  - `ispell <https://en.wikipedia.org/wiki/Ispell>`_ 3.3.0.2
  - enchant_ 2.2.7
  - hunspell_ 1.7.0

  Earlier versions of these programs were not tested, YMMV.


PEL Text Alignment Support
--------------------------

:PDF Sheet: `Align`_.
:PEL Customization: *none*
:PEL Key Prefix: - **pel:align** : ``<f11> t a``

PEL provides the **pel:align** key binding ``<f11> t a`` to Emacs text alignment
commands.



PEL Text Filling and Justification Utilities
--------------------------------------------

:PDF Sheet: `Filling and Justification`_, `Text-modes`_.
:PEL Customization: *none*
:PEL Key Prefix: - **pel:fill** : ``<f11> t f``
                 - **pel:justification** : ``<f11> t j``
                 - **pel:textmodes** : ``<f11> t m``


The `pel-fill.el`_ provides two simple utilities:

- ``pel-auto-fill-only-comments``
  activates/de-activates automatic filling in source code comments only.
- ``pel-show-fill-columns``
  displays value of relevant fill columns for current buffer.

PEL also provides several key bindings to Emacs text filling and justification
commands, as shown in the `Filling and Justification`_ PDF table.
PEL uses the ``<f11> t f``, ``<f11> t j`` and ``<f11> t m`` key prefixes
to provide access to several relevant commands.


PEL Text Insertion Utilities
----------------------------

:PDF Sheet: `Inserting Text`_.
:PEL Customization: - ``pel-use-lice``
                    - ``pel-use-spdx``
                    - ``pel-use-smart-dash``
                    - ``pel-use-smartparens``
                    - ``pel-use-yasnippet``
                    - ``pel-use-yasnippet-snippets``
                    - **pel-c-code-style-group** : ``<f11> i <f2>``
                    - **pel-pkg-generic-code-style** : ``<f6> <f2>``
                    - Also available in the style/skeleton groups of several
                      programming languages.

:PEL Key Prefix: **pel:f6**:  ``<f6>``
                 **pel:insert** : ``<f11> i``
                 **pel:yasnippet** : ``<f11> y``

PEL provides a set of generic and specialized insertion commands as well as
access to several external packages as described by the `Inserting Text`_
table.

For example, PEL provides the following commands:

- ``pel-generic-file-header``, bound to ``<f6> h``, prompts the user for the
  purpose of the file and then inserts a file header at
  the top of the buffer with that information and some other as specified by
  the user-options in the **pel-pkg-generic-code-style**.  See examples below
  in the section titled `Example of Generic File Header Generation`_ below.


- ``pel-insert-line`` inserts a (commented) line.  The length of the line is
  controlled by the ``pel-linelen`` customization variable, which defaults to 77.
- ``pel-insert-filename`` inserts the name of the file in the current or
  specified window.

Commands like ``pel-generic-file-header`` and ``pel-insert-line`` are
*generic* commands in the sense that they can be used in several major modes
and adapts to them by using the comment style of the major mode to
respectively produce a commented-out file header or line.  It works for all
major modes that identify a command style.  PEL does not have to explicitly
support the major mode for this to work.

Another **very useful** feature is the use of the ``smart-dash-mode`` provided
by the smart-dash_ external package.  PEL provides the ``<f11> M--`` binding to
toggle this useful mode on and off. When the ``smart-dash-mode`` is activated,
you can insert underscore characters by hitting the dash (``'-'``) key without
having to press the Shift key.   And for programming languages identified by the
``smart-dash-c-modes`` user option you can insert ``--`` and ``->`` normally.

The ``smartparens-mode`` allows inserting balanced block pairs in various
major modes. PEL specialized ``smartparens`` for some major mode to extend its
usefulness.

The PEL binding include more commands, some are Emacs standard commands, other
are from other packages.  All are listed in the `Inserting Text`_ PDF
documentation.

Date & Time Text Insertion
~~~~~~~~~~~~~~~~~~~~~~~~~~

- The following 8  commands insert time/date format for the local or the UTC
  time:

  - ``pel-insert-date`` inserts the current date at point.
  - ``pel-insert-date-wkd`` inserts the current date and week-day at point.
  - ``pel-insert-date-time`` inserts the current date and time at point.
  - ``pel-insert-date-wkd-time`` inserts the current date, week day and time at point.
  - ``pel-insert-iso-date`` inserts the current date at point.
  - ``pel-insert-iso-date-wkd`` inserts the current date and week-day at point.
  - ``pel-insert-iso-date-time`` inserts the current date and time at point.
  - ``pel-insert-iso-date-wkd-time`` inserts the current date, week day and time at point.

The format of the inserted strings are controlled by customized variables
in the ``pel-date-time-insertion`` group.  The default format for the
first four commands correspond to free-standing formats you can change.
The second group of fours commands are also configurable but they should
correspond to valid `ISO-8601 standard format`_ strings.


Template Text Insertion
~~~~~~~~~~~~~~~~~~~~~~~

PEL supports two different template mechanisms: the Emacs built-in tempo skeleton
system and the popular yasnippet_ external library.

Generic Tempo-based Templates
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

PEL supports insertion of generic templates that take advantage of the
comment style of the current major mode and the identification of various
template elements selected from the user-options in the
**pel-pkg-generic-code-style** group accessible via:

- ``<f11> i <f2>`` and selection of the group button at the bottom of the
  buffer,
- more directly from ``M-x customize-group pel-pkg-generic-code-style RET``,
- or its equivalent ``<f11> <f2> g pel-pkg-generic-code-style RET``.

The group allows activation of various template elements:

- header block style:

  - a default provided by PEL
  - a customized one provided by a tempo-skeleton file created by the user
    (see more information in the docstring of the user-option),

- insertion of file time-stamp,
- Insertion of open source software license,
- insertion of section titles identified by the user-option,
- insertion of section separators.

The templates identify locations where more information must be typed.
After inserting the template use ``C-c .`` to move point to the next location
and ``C-c ,`` to the previous one.  If the user types RET on the file purpose
prompt, the location for the file purpose is a target accessible with these keys.

See examples in the next sub-section.

Example of Generic File Header Generation
+++++++++++++++++++++++++++++++++++++++++

Generic file header in C
************************

The following is the generic template for C created with  the defaults
along with the sentence ``This is where the purpose goes`` typed as purpose.


..  code:: C

    /* C FILE: example.c
    **
    ** Purpose   : This is where the purpose goes.
    ** Created   : Wednesday, October  6 2021.
    ** Author    : Pierre Rouleau <prouleau001@gmail.com>
    ** Time-stamp: <2021-10-06 11:49:17, by Pierre Rouleau>
    */
    /* -------------------------------------------------------------------------- */
    /* Module Description
    ** ------------------
    **
    **
    */


    /* -------------------------------------------------------------------------- */
    /* Dependencies
    ** ------------
    **
    **
    */


    /* -------------------------------------------------------------------------- */
    /* Code
    ** ----
    **
    **
    */


    /* -------------------------------------------------------------------------- */


Note that PEL also provides specialized templates for C described
in the section titled `Controlling PEL Tempo Skeletons for C`_.

For C the comment style can be selected by customization.  For instance, the
``pel-c-skel-comment-with-2stars`` identifies whether one or two start
characters will be used in the C continuation comment.  That setting is also
affecting the generic templates, making it possible to generate this instead:


.. code:: c

    /* C FILE: another_style.c
     *
     * Purpose   :
     * Created   : Wednesday, October  6 2021.
     * Author    : Pierre Rouleau <prouleau001@gmail.com>
     * Time-stamp: <2021-10-06 14:10:02, by Pierre Rouleau>
     */
    /* -------------------------------------------------------------------------- */
    /* Module Description
     * ------------------
     *
     *
     */


    /* -------------------------------------------------------------------------- */
    /* Dependencies
     * ------------
     *
     *
     */


    /* -------------------------------------------------------------------------- */
    /* Code
     * ----
     *
     *
     */


    /* -------------------------------------------------------------------------- */




Generic file header in FORTRAN
******************************

PEL does not support FORTRAN explicitly but Emacs identifies the comment style
of FORTRAN code.

.. code:: fortran

    C FORTRAN FILE: example.for
    C
    C Purpose   : This is where the purpose goes.
    C Created   : Wednesday, October  6 2021.
    C Author    : Pierre Rouleau <prouleau001@gmail.com>
    C Time-stamp: <2021-10-06 13:46:49, by Pierre Rouleau>
    C ----------------------------------------------------------------------
    C Module Description
    C ------------------
    C
    C


    C ----------------------------------------------------------------------
    C Dependencies
    C ------------
    C
    C


    C ----------------------------------------------------------------------
    C Code
    C ----
    C
    C


    C ----------------------------------------------------------------------


Generic file header in Pascal
*****************************

.. code:: pascal

    { PASCAL FILE: example.pas
    |
    | Purpose   :
    | Created   : Wednesday, October  6 2021.
    | Author    : Pierre Rouleau <prouleau001@gmail.com>
    | Time-stamp: <2021-10-06 13:53:10, by Pierre Rouleau>
    }
    { ---------------------------------------------------------------------------}
    { Module Description
    | ------------------
    |
    |
    }


    { ---------------------------------------------------------------------------}
    { Dependencies
    | ------------
    |
    |
    }


    { ---------------------------------------------------------------------------}
    { Code
    | ----
    |
    |
    }


    { ---------------------------------------------------------------------------}


Generic file header in Ruby
***************************

.. code:: ruby

    # RUBY FILE: example.rb
    #
    # Purpose   :
    # Created   : Wednesday, October  6 2021.
    # Author    : Pierre Rouleau <prouleau001@gmail.com>
    # Time-stamp: <2021-10-06 13:58:03, by Pierre Rouleau>
    # ----------------------------------------------------------------------------
    # Module Description
    # ------------------
    #
    #


    # ----------------------------------------------------------------------------
    # Dependencies
    # ------------
    #
    #


    # ----------------------------------------------------------------------------
    # Code
    # ----
    #
    #


    # ----------------------------------------------------------------------------



Using Tempo Skeleton
^^^^^^^^^^^^^^^^^^^^


To complement the generic tempo templates, PEL also implements specialized
tempo skeletons for several major modes, including:

- C,
- C++
- erlang,
- emacs lisp,
- reStructuredText.

The PEL tempo skeletons implement relatively large templates like file header
blocks that insert the boiler plate code for a given type of file.  The format
of most of these templates are controlled by PEL user options that control
things like:

- Prompt of file and function purpose
- Insertion of comment documentation formatting like Edoc and Doxygen
- Insertion of code block separator lines
- Insertion of automatically updated file timestamp
- Insertion of open source code license
- Use and generation of unique C file header include guard (with UUIDs)

PEL binds keys to the commands that insert and expand tempo skeletons: they are
bound to major mode specific ``<f12> <f12>`` key sequence, trying to use the
same sequence for a concept in all supported modes.  For example the key
sequence ``<f12> <f12> h`` inserts a file header in the supported modes but
does it differently for each mode.

Since user options are used to control the format of the tempo skeleton
templates, you can set the format globally using customization (use the ``<f12>
<1>`` key sequence to quickly gain access to customization of the current mode).
You can also control the format of all files in a directory tree by storing the
relevant user options inside a ``.dir-locals.el`` file or control a single file
by setting the user options as Emacs file variable.

The tempo skeletons commands for each supported mode are described in the PDF
document of the relevant mode.


Using YASnippet
^^^^^^^^^^^^^^^

PEL also supports the popular yasnippet_ templating system and the official
yasnippet-snippets_ templates when the ``pel-use-yasnippet`` and
``pel-use-yasnippet-snippets`` user options are set to **t**.  If you want
yasnippet_ activated when Emacs starts set the ``pel-use-yasnippet`` user option
to **use-from-start** otherwise you can activate (and de-activate) the YASnippet
minor mode by using the ``<f11> y y`` key sequence.

Aside from keys to manage snippets, bound inside the **pel:yasnippet** key
prefix (``<f11> y``) PEL does not provide any special binding to YASnippet: just
type the snippet name abbreviation (called the Yasnippet *key*) and hit the tab
key to expand.



PEL Text Modes Support
----------------------

:PDF Sheet: `Text-modes`_.
:PEL Customization: *none*
:PEL Key Prefix: **pel:textmodes** : ``<f11> t m``

PEL provides a set of bindings to commands that activate various text modes
under the **pel:textmodes** key prefix, bound to ``<f11> t m``. This includes
the following:

- text overwrite mode,
- subword-mode,
- superword-mode,
- whitespace-mode,
- picture-mode,
- artist-mode.

See the `Text-modes`_ PDF sheet.

PEL Text Transformation Utilities
---------------------------------

:PDF Sheet: `Case Conversion`_, `Text-modes`_.
:PEL Customization: *none*
:PEL Key Prefix: *none*, standard Emacs keys rebound.

- The `pel-text-transform.el`_ file provides commands that handle case conversions
  taking the case of the word into consideration when performing the operation to
  help reduce the number of key bindings required to perform the tasks.  The
  provided commands are:

  - ``pel-upcase-word-or-region`` upcases the word(s) unless the first two
    characters are upper case characters, in which case the command capitalize the
    word(s).
  - ``pel-downcase-word-or-region`` downcases the word(s) unless the first
    two characters are already lower case characters, in which case the command
    capitalize the word(s).
  - ``pel-capitalize-word-or-region`` capitalize the word(s).

- Emacs has several text modes.  The ``pel-show-text-modes`` command provides information
  about them by displaying a description of the modes and their state.
- The ``pel-toggle-sentence-end`` command toggles the number of spaces that
  identify the end of a sentence for Emacs between 1 and 2.  It displays the new value.

PEL Text Transpose Support
--------------------------

:PDF Sheet: `Transpose`_.
:PEL Customization: *none*
:PEL Key Prefix: **pel:text-transpose** : ``<f11> t t``


PEL provides the **pel:text-transpose** key prefix (``<f11 t t``) to a set of Emacs
commands that transpose text, as shown in the `Transpose`_ PDF table.


PEL Undo Support
----------------

:PDF Sheet: `Undo, Redo, Repeat and Prefix Arguments`_.
:PEL Customization: ``pel-use-undo-tree``, ``pel-use-goto-last-change``.
:PEL Key Prefix: **pel:undo** : ``<f11> u``

PEL provides the **pel:undo** key prefix (``<f11> u``) to Emacs undo commands.
If the ``pel-use-undo-tree`` customization variable is set to **t**, it uses the
undo-tree package to control undo and binds its keys.
If the ``pel-use-goto-last-change`` customization variable is set to **t** it
also provides access to the ``goto-last-change`` command and binds it.
All key binding details are in the `Undo, Redo, Repeat and Prefix Arguments`_ PDF table.

PEL (D)VCS Support
------------------

:PDF Sheet: `Mercurial`_.
:PEL Customization: ``pel-use-magit``, ``pel-use-monky``.
:PEL Key Prefix: **pel:vcs** : ``<f11> v``

PEL documents the use of Emacs Version Control support in the `Mercurial`_
document, one of several documents that will be written on VCS support (a
document for Git will also be written in the future).
PEL provides the **pel:vcs** key-map that gives access to
Emacs standard `VC (Version Control)`_ directory command but also to Magit_ for
Git_ and Monky_ for `Mercurial Software`_.

.. _Git:                  https://en.wikipedia.org/wiki/Git
.. _Mercurial Software:   https://en.wikipedia.org/wiki/Mercurial
.. _Monky:                https://github.com/ananthakumaran/monky
.. _Magit:                https://magit.vc
.. _VC (Version Control): https://www.gnu.org/software/emacs/manual/html_node/emacs/Version-Control.html

PEL Web Browsing Support
------------------------

:PDF Sheet: `Web`_.
:PEL Customization: *none*
:PEL Key Prefix: **pel:file** : ``<f11> f``

Emacs provides several commands that launch a web browser.
PEL provides a set of bindings to commands provided by Emacs and some extra
commands implemented by PEL code to open a URL or file at point in a browser.
The list of these commands and the corresponding Emacs customization groups are
described in the `Web`_ PDF table.

PEL Window Management Support
-----------------------------

:PDF Sheet: `Windows`_.
:PEL Customization: ``pel-use-ace-window``.
:PEL Key Prefix: **pel:window** : ``<f11> w``

The file `pel-window.el`_ provides a set of window management utilities.  Some
of these utility commands use or extend the features provided by the
``windmove`` library, a library packaged with standard GNU Emacs.

The file provides the following features:

- Buffer management utilities:

  - ``pel-show-window-previous-buffer`` shows the name of the buffer that was
    previously used in the current window.
  - ``pel-switch-to-last-used-buffer`` switch the buffer in current window to
    the buffer that was previously used.

- Dedicated window management utilities:

  - ``pel-show-window-info`` displays the dedicated status of the
    current window: ie. whether the current window is dedicated or not.
  - ``pel-toggle-window-dedicated`` toggles the dedicated status of the
    current window.  Use it to dedicate the current window or turn
    dedication off.

- Creating new windows:

  The following 4 commands allow creating cursor bindings to create windows
  pointed by a cardinal direction:

  - ``pel-create-window-down``
  - ``pel-create-window-left``
  - ``pel-create-window-right``
  - ``pel-create-window-up``

- Closing windows:

  The following 4 commands allow creating cursor bindings to close windows
  pointed by a cardinal direction:

  - ``pel-close-window-down``
  - ``pel-close-window-left``
  - ``pel-close-window-right``
  - ``pel-close-window-up``

- Window splitting:

  - The function ``pel-split-window-sensibly`` attempts to improve window
    splitting logic by selecting an orientation that takes the frame size
    into account with a different heuristic than what is normally used by
    Emacs. The function is used by other PEL commands when windows are
    created. The logic gives priority to splitting vertically if the
    available area is wide *enough*.

- Changing orientation of 2 windows:

  The commands ``pel-2-vertical-windows`` and ``pel-2-horizontal-windows`` flip
  the orientation of the current and next window from horizontal to vertical
  and vice-versa.

- Moving to windows by direction or context:

  Two functions provide services to move point to other window by direction
  or to create a new one.  These functions are used by other PEL commands.
  The functions are:

  - ``pel-window-valid-for-editing-p`` move point to the identified direction
    as long as the target window can be used for editing.  This excludes the
    mini-buffer or any dedicated window.
  - ``pel-window-select`` move to the window specified by a direction argument
    or to the *other* window (the next one) or create a new window.
    This is also a utility function used by other PEL commands.

- Moving to other (next) or previous window:

  - The ``pel-other-window`` is just explicitly calling the Emacs
    ``other-window`` command that might be hidden by the use of ``ace-window``.
  - The ``pel-other-window-backward`` moves to the previous window.

- Showing information about current window:

  - ``pel-show-window-filename-or-buffer-name`` displays the name of the
    file or buffer used in the current window.
  - ``pel-show-window-info`` displays the height and width of the current
    window, whether the window is dedicated, fixed in size, etc...


.. ---------------------------------------------------------------------------



PEL Implementation
==================

Emacs Lisp Files
----------------

PEL code is spread across several Emacs Lisp files.
The file names have been selected with the following constraints:

#. Conform to the `Emacs Lisp Packaging rules`_.  However, because of the
   nature of PEL it is currently not distributed via an Elpa-compliant
   repository.  It currently provides the files `pel-pkg.el`_ and
   `pel-autoloads.el`_ but these are unused and may be removed in the future.

#. Control byte-compilation under several scenarios, including the
   following:

   - *Manual* installation by cloning the PEL Git Depot and then using
     the PEL Makefile_ to create a local package archive, and compile all files
     locally.
   - Dynamic byte-compilation of `pel_keys.el`_, `pel__hydra.el`_ and some
     files not normally byte-compiled like the autoloads.el file of the
     pel-bundle *pseudo-package* used in fast startup mode as well as the
     package-quickstart files on request.

The PEL Emacs Lisp files types are the following:

#. Local files, used by all other PEL files.

   - These files have a name that
     starts with ``pel--`` and sort before all other files.
   - These files can be byte-compiled independently within an ``emacs -Q``
     session and will not generate any warning.
   - These include:

     - `pel--base.el`_: defines low level utilities.
     - `pel--keys-macros.el`_ : defines multiple key bindings via Elisp macros.
     - `pel--options.el`_: defines all PEL customization variables.
     - `pel--macros.el`_: defines macros used by other files.

#. PEL feature files.

   - These files have a name that starts with ``pel-``.
   - These files can be byte-compiled independently within an ``emacs -Q``
     session and will not generate any warning.
   - These files implement PEL specific convenience features.
     Some are independent from external packages, others provide a logic
     layer on top of external packages and a dynamically control access
     to the external package features enabled via the PEL option
     ``pel-use-`` configuration variables as described in the `PEL Customization`_
     section.
   - The PEL features implemented by these files are described inside some of the
     sub-sections of `PEL Convenience Features`_.

     - As an example of one of the PEL convenience feature file,
       the file `pel-navigate.el`_ provides extra navigation facilities
       such as the use of multi-hit ``<home>`` and ``<end>`` keys similar to what is
       available by editors in the Brief family (such as CRiSP) but also aware of Emacs
       concepts such as text fields, `shift-key selection`_ and Emacs `mark and
       region`_. This is detailed in `PEL Navigation Support`_.

   - These files are mostly independent from each other.
   - Several of these files can be used as stand-alone *libraries*
     when someone does not want to use the entire PEL system.
   - It is possible to use one or several of these PEL features
     without using the PEL key bindings.
     To do that, just use the files that contain the features you need and write
     your own key bindings for them inside your Emacs init file.  Just don't
     call ``pel-init``.

#. PEL key binding file: `pel_keys.el`_.

   - This file has a name that starts with ``pel_``, using the unusual
     underscore for Emacs Lisp files.  The underscore is used as a simple way to
     ensure that this file has a name that sorts after the files of the other
     two types above.
   - This file is **only** loaded and used by the file `pel.el`_.
   - This file defines all PEL key bindings.
     It also contains the logic to install external packages lazily when
     the corresponding PEL option activates it.
   - This file holds content similar to what users would put inside their Emacs
     init.el file.

     - This file is, however, byte-compiled.
       But it is byte-compiled *after* every other PEL file has been
       byte-compiled.

       - The byte-compilation line for this file inside the Makefile_  loads the
         user's init.el file.  The Makefile_ identifies it with the ``EMACS_INIT``
         macro, and it is defined by default to be located inside
         ``"~/.emacs.d/init.el"``.

   - The file `pel_keys.el`_ loads the file `pel-autoload.el`_ to define the
     auto-loading of all PEL features.

#. PEL top level file: `pel.el`_.

   - This file holds the ``pel-init`` command.  That loads the `pel_keys.el`_
     file, in a similar manner that a init.el file would be loaded.
   - This is the only file auto-loaded by the standard package auto-load
     control file: `pel-autoloads.el`_.

#. The other Emacs Lisp files are not part of the PEL package itself.
   They are tools used to help automate installation of PEL:

   - The file `install-pel.el`_ controls the creation of a local Emacs package
     archive which is then used to install PEL on local computers from a cloned
     Git depot.

PEL loads only what is needed based on commands executed.
It uses several Emacs Lisp macros to control package detection and package
loading with emphasis on delayed execution whenever possible.  These are all
used in the code of `Pel_keys.el`_ which is loaded by the ``pel-init``
command.

PEL also auto-loads its code as much as possible but does not rely on the
package.el autoload cookies to do so because it does not support automatic
installation.  Instead it provides auto-loading declarations inside the
`pel_keys.el`_ and inside `pel-autoload.el`_

- The ``pel-init`` command loads `pel_keys.el`_ explicitly.
- The `pel_keys.el`_ code loads `pel-autoload.el`_ and then calls ``pel--autoload-init``.
  That function defines the auto-loading of all ``pel-``
  files, the PEL feature which are mostly independent from each other.

Currently, PEL uses its own logic on top of the built-in package library
to control the installation of missing package if the corresponding feature
is activated via `PEL customization`_ ``pel-use-`` customization variable.
PEL mechanism to control external package is similar to what the `use-package
library`_ provides but also provides support for non-Elpa packages and includes
logic specific to PEL that is aware of PEL fast-startup mode.

The list of external packages used by PEL is shown in the `Credits`_ section.




.. _install-pel.el:         ../install-pel.el
.. _pel.el:                 ../pel.el
.. _pel--options.el:        ../pel--options.el
.. _pel--base.el:           ../pel--base.el
.. _pel--keys-macros.el:    ../pel--keys-macros.el
.. _pel--macros.el:         ../pel--macros.el
.. _pel-applescript.el:     ../pel-applescript.el
.. _pel-autocomplete:
.. _pel-autocomplete.el:    ../pel-autocomplete.el
.. _pel-autoload:
.. _pel-autoload.el:        ../pel-autoload.el
.. _pel-autoloads:
.. _pel-autoloads.el:       ../pel-autoloads.el
.. _pel-bookmark:
.. _pel-bookmark.el:        ../pel-bookmark.el
.. _pel-ccp:
.. _pel-ccp.el:             ../pel-ccp.el
.. _pel-comment:
.. _pel-comment.el:         ../pel-comment.el
.. _pel-commonlisp:
.. _pel-commonlisp.el:      ../pel-commonlisp.el
.. _pel-completion:
.. _pel-completion.el:      ../pel-completion.el
.. _pel-cua:
.. _pel-cua.el:             ../pel-cua.el
.. _pel-cursor:
.. _pel-cursor.el:          ../pel-cursor.el
.. _pel-file:
.. _pel-file.el:            ../pel-file.el
.. _pel-filex:
.. _pel-filex.el:            ../pel-filex.el
.. _pel-fill:
.. _pel-fill.el:            ../pel-fill.el
.. _pel-font:
.. _pel-font.el:            ../pel-font.el
.. _pel-frame-control:
.. _pel-frame-control.el:   ../pel-frame-control.el
.. _pel-highlight:
.. _pel-highlight.el:       ../pel-highlight.el
.. _pel-imenu:
.. _pel-imenu.el:           ../pel-imenu.el
.. _pel-indent:
.. _pel-indent.el:          ../pel-indent.el
.. _pel-kbmacros:
.. _pel-kbmacros.el:        ../pel-kbmacros.el
.. _pel-key-chord:
.. _pel-key-chord.el:       ../pel-key-chord.el
.. _pel-line-control:
.. _pel-line-control.el:    ../pel-line-control.el
.. _pel-lisp:
.. _pel-lisp.el:            ../pel-lisp.el
.. _pel-mark:
.. _pel-mark.el:            ../pel-mark.el
.. _pel-navigate:
.. _pel-navigate.el:        ../pel-navigate.el
.. _pel-numkpad:
.. _pel-numkpad.el:         ../pel-numkpad.el
.. _pel-pkg:
.. _pel-pkg.el:             ../pel-pkg.el
.. _pel-prompt:
.. _pel-prompt.el:          ../pel-prompt.el
.. _pel-read:
.. _pel-read.el:            ../pel-read.el
.. _pel-register:
.. _pel-register.el:        ../pel-register.el
.. _pel-rst:
.. _pel-rst.el:             ../pel-rst.el
.. _pel-scroll:
.. _pel-scroll.el:          ../pel-scroll.el
.. _pel-search:
.. _pel-search.el:          ../pel-search.el
.. _pel-skels:
.. _pel-skels.el:           ../pel-skels.el
.. _pel-skels-c:
.. _pel-skels-c.el:         ../pel-skels-c.el
.. _pel-speedbar:
.. _pel-speedbar.el:        ../pel-speedbar.el
.. _pel-spell:
.. _pel-spell.el:           ../pel-spell.el
.. _pel-text-insert:
.. _pel-xref:
.. _pel-xref.el:            ../pel-xref.el
.. _pel-text-insert.el:     ../pel-text-insert.el
.. _pel-text-transform:
.. _pel-text-transform.el:  ../pel-text-transform.el
.. _pel-window:
.. _pel-window.el:          ../pel-window.el
.. _pel_keys:
.. _pel_keys.el:            ../pel_keys.el
.. _Emacs Lisp Packaging rules: https://www.gnu.org/software/emacs/manual/html_node/elisp/Packaging.html#Packaging
.. _shift-key selection:        https://www.gnu.org/software/emacs/manual/html_node/emacs/Shift-Selection.html#Shift-Selection
.. _mark and region:            https://www.gnu.org/software/emacs/manual/html_node/emacs/Mark.html#Mark
.. _Emacs package-install:
.. _package-install:            https://www.gnu.org/software/emacs/manual/html_node/emacs/Package-Installation.html
.. _EmacsWiki:                  https://www.emacswiki.org/


Naming Conventions
------------------

- All PEL "*public*" functions and variables have a name that start with the
  prefix "pel-".

  - This includes all PEL commands.

- All PEL "*private*" functions and variables have a name that start with the
  prefix "pel--".

  - Those are  meant to be used from with PEL code exclusively.
  - The same convention also applies to the Emacs Lisp file
    names; the files `pel--base.el`_, `pel--macros.el`_ and
    `pel--options.el`_ are used by the other PEL files.

- All PEL customization variables that control whether PEL uses or provides a
  given feature have a name that starts with the prefix "pel-use-".

- Most PEL key-maps have a name.  All of those name start with the prefix "pel:".

  - Using named key-maps help shows the key prefix purpose when using
    `which-key`_ to display the available key following a prefix or typing
    ``C-h`` or ``<f1>`` after typing a prefix key to see the list of available
    keys and their meanings.
  - The only key prefix that does not have a name is the one used for **F12**,
    the mode sensitive prefix key (I did not find a way to name it, so if you
    have an idea, let me know! :-)

- All Emacs Lisp files that are part of the PEL package have a name that starts
  with the "pel-" prefix.

- Emacs Lisp test files are stored in the `test sub-directory`_ and have names
  that have the "pel-" prefix and the "-test" suffix.

- Other Emacs Lisp files are included in this repository,
  such as ``build-pel.el`` and ``install-pel.el``.   These file are not part of PEL
  itself and may be removed in the future.
  These files contain code that is not part of PEL but are used to develop PEL.
  The names of these files do not start with "pel-" but they end with "-pel".
  That should be enough to prevent clash with other packages.
  If this is not enough for you, since these files are not required to use PEL,
  feel free to move or erase those files in your local
  directory and let me know; I'll try to find a better way.
  Note that these files, and none of the `build related files <#building-pel>`_,
  are not part of the PEL package distribution tar file.


Code Guidelines
---------------

General Guidelines
~~~~~~~~~~~~~~~~~~

**Overview**

The Emacs Lisp code is written to comply with the `standard GNU Emacs code
guidelines`_.  PEL code follows most of the points promoted by
`bbatsov/emacs-lisp-style-guide`_ except in some few places.
PEL code also follows several ideas outlined in
`Jari Aalto Emacs Lisp Guideline`_, an older but still valid guideline.

**Line Length**

The maximum line length is 80.

**Use of tabs**

PEL source code does not use hard tabs.  All indentation uses space characters.


**Settings enforced by directory local variables**

The file `.dir-locals.el`_ imposes the line length and the use of spaces for
indentation.
See the `GNU Emacs Manual Per-Directory Local Variables`_ for more information
about this file.

.. _.dir-locals.el: ../.dir-locals.el
.. _GNU Emacs Manual Per-Directory Local Variables: https://www.gnu.org/software/emacs/manual/html_node/emacs/Directory-Variables.html#Directory-Variables


**Used Tools**

To ensure conformance, the code is checked with the following tools:

- GNU Emacs ``elint-file`` command from Standard GNU Emacs elint.el_.
- The external package `elisp-lint`_ which uses the following other tools:

  - Emacs Lisp byte-compiler,
  - `Emacs Lisp checkdoc`_,
  - indent (to confirm indentation inside a file).

The file `pel.el`_ is also checked with `package-lint`_ to verify
the presence of package version.



Variable Scoping
~~~~~~~~~~~~~~~~

PEL code uses lexical scope in all Emacs Lisp files.
Each of these file sets the ``lexical-binding`` local variable to **t**.

For more information on why this is done, read
Chris Wellons' article titled
"`Some Performance Advantages of Lexical Scope`_"
written in December 2016.

Aside from the advantages outlined by the article, linting Emacs Lisp code finds
more issues when lexical scope is in effect.

Licensing
~~~~~~~~~

The license information is stored in each .el file.


Checking PEL Code
~~~~~~~~~~~~~~~~~

The included `Makefile`_ provide rules to byte-compile and lint all files and
then run the regression tests.
The following commands are used to test the code, issued from PEL root directory:

.. code:: shell

          make clean
          make compile
          make lint
          make test

There should be no error, no warning and all tests should pass.

.. _elint.el:                          https://github.com/emacs-mirror/emacs/blob/master/lisp/emacs-lisp/elint.el
.. _standard GNU Emacs code guidelines:
.. _Emacs Lisp checkdoc:               https://www.gnu.org/software/emacs/manual/html_node/elisp/Tips.html
.. _bbatsov/emacs-lisp-style-guide:    https://github.com/bbatsov/emacs-lisp-style-guide
.. _Jari Aalto Emacs Lisp Guideline:   http://www.nongnu.org/emacs-tiny-tools/elisp-coding/index-body.html
.. _Some Performance Advantages of Lexical Scope: https://nullprogram.com/blog/2016/12/22/

Emacs Lisp Regression Test
--------------------------

At this point just a small portion of PEL is covered by
`ERT-based testing`_.
The test code is located inside the `test sub-directory`_.
As everything in PEL for this early version: 🚧 more to come here...

.. _ERT-based testing:         https://www.gnu.org/software/emacs/manual/html_node/ert/index.html
.. _test sub-directory:        test


Building PEL
------------

**Note:**
         You **must** build PEL to use it.
         PEL uses the byte-compiler and macros to activate specific parts
         of the code and to speed it all up.  Building is easy with make: just
         run ``make``.

**Note 2:**
        At this moment, for this early version of PEL, I did not submit PEL
        package into Emacs Lisp archives like MELPA_.  I may do this later,
        but the nature of PEL differs from a stand-alone package.

Use the Makefile_ to control command line build of the PEL distribution
package, the byte compilation of all PEL Emacs Lisp source files.

To see what you can do with the Makefile, move to the directory where it is
located and issue the ``make help`` command which lists the available top-level
targets and their purpose.


PDF Documentation
-----------------

The list of documentation files are currently published as
several tables in `PDF topic-oriented reference sheets`_ with a top-level
`PEL Index PDF`_ with links to all the PEL PDF files.

Although this is not the best way since this is an output format file as opposed
to the source of the document, these files were created in PDF format because I
wanted to be able to pack a lot of information about Emacs as I was learning
it.  I considered using a markup language like markdown or reSTructuredText. The
latter is more powerful, and it would have been possible to generate tables with
some of the attributes of what I was able to generate but it would have most
probably needed its own web site to be able to completely control the CSS as
well as write extensions in Python for what I needed.  And I did not have time
for that.  I needed to concentrate on Emacs and jot down notes on something
that, at the beginning of my learning period, was *not* Emacs. So I compromised
and used the macOS Numbers program to create a large spreadsheet with multiple
tabs and tables.  I used that to generate the PDF files.

This is far from ideal. I know. And once PEL gets to the point where support for
several other programming languages is integrated, I might find ways to use a
markup language that might be flexible enough to generate the same kind of
output.

As an temporary work-around, I tried
to export the file to CSV or TSV (tab separated value).  That generates the text
but the hyperlinks are not part of the CSV/TSV output files.  I might consider
producing those files if there is any interest, but I'd prefer to be able to
publish the source of something that can generate the kind of output that's
available in those PDF files.

I am open to suggestions. In the mean-time, I push a copy of that Numbers file
regularly in the `pel-pdf-spreadsheet repo`_.

Planning and Future
-------------------

At this point there's no formal planning for this project.

I have mainly been writing this as a way for me to learn and remember Emacs as a
tool and Emacs Lisp as a programming language.  But I am planning to use it for
most of my upcoming work and will continue to document what I learn and what I
use when developping in various programming languages and doing various tasks.


Override or change PEL key bindings
-----------------------------------

As of this release PEL key bindings and key prefixes are hard coded.
If you want to change a key binding that PEL uses, you can define your own
key bindings after the execution of ``pel-init``.  You can also change
PEL prefix keys.

The following code re-assign the **F6** key to ``undo`` and uses the **F9** key
to be used as what PEL normally uses for **F6**:

.. code:: elisp

          (global-set-key (kbd "<f6>") 'undo)
          (global-set-key (kbd ("<f9>") pel:f6)

The `Function Keys Mappings PDF table`_ provides and overview of the way PEL uses
the function keys.  See also the section titled `PEL Function Keys Bindings`_.

Since PEL is using more keys over time, it might be difficult to change the
bindings without affecting PEL's bindings.  If you have a specific request,
please describe your request on the `PEL wiki`_, I'll take a look and see what I can do.

.. _PEL wiki:                         https://github.com/pierre-rouleau/pel/wiki
.. _Function Keys Mappings PDF table: https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/keys-fn.pdf


How to Modify the Terminal Keyboard Shortcut Keys
=================================================

PEL makes extensive use of function keys such as F11 but some other function
keys too.  In some environment these keys are used by the terminal emulator
programs you may want to use to run Emacs in terminal (termcap) mode.
Fortunately most environments provide ways to change the shortcut keys used by
their terminal.

When running in terminal mode, there are several key modifier combinations
that PEL never uses because they are normally not accessible to programs
running under a terminal.  These combinations are, however, accessible to
top-level programs such as the terminal emulator themselves and therefore good
candidate for shortcut replacements.  The following modifier key combinations
are never used by PEL:

- Control-Function key
- Shift-Control-Function key
- Super-Function key
- Shift-Super-Function key

Disable Ubuntu HUD use of the Alt Key
-------------------------------------

The Ubuntu Linux distributions that use the Unity_ user interface also support
the HUD_ (which stands for Heads-up display) which unfortunately uses the Alt
key as a hotkey.  This is *very* annoying with Emacs (and several other
applications) as it forces you to type the Alt key twice for Emacs Meta key
modifiers.

To disable the HUD shortcut key:

- Open Ubuntu **System Settings** application
- Select the **Keyboard** settings
- Open the **Launchers** tab
- Select the **Key to show the HUD** line, double click on the shortcut (which
  is Alt-L by default), then press backspace to disable the shortcut.

The Alt key will then become available inside Emacs as the Meta key.


See the popular AskUbuntu question
"`How do I modify or disable the HUD's use of the Alt key?`_" for snapshots
and more information.

.. _Unity: https://en.wikipedia.org/wiki/Unity_(user_interface)
.. _HUD: https://wiki.ubuntu.com/Unity/HUD
.. _How do I modify or disable the HUD's use of the Alt key?: https://askubuntu.com/questions/122209/how-do-i-modify-or-disable-the-huds-use-of-the-alt-key

Modify GNOME Terminal Shortcut Keys
-----------------------------------

Configure the `GNOME Terminal`_ keyboard shortcuts by opening its Preference
dialog, and select the *Shortcut* tab:

.. figure:: res/gnome-terminal-01-02.png
   :scale: 30 %

You will probably want to modify the F1, F11 and some other keys.
Select the *Action* that is assigned to the key shortcut that you want to free
and double click on the *Shortcut Key* column for it.  Then hit the key
modified combination that PEL does not use and that you want to use.

.. figure:: res/gnome-terminal-03.png
   :scale: 30 %


You also might want to configure the several key shortcuts to prevent clashes
with Emacs.  The pdf titled `Ubuntu 16.04 Linux Desktop Keys on macOS Host`_
lists a set of key shortcuts that might be useful.

..
   -----------------------------------------------------------------------------

Credits
=======

PEL integrates with several great Emacs Lisp packages.  Some of them are
required, the others are used only if they are present and are activated by the PEL
customization.  PEL's code is written to operate despite the absence of external
packages that have not been activated but it expects presence of packages that are
distributed with GNU Emacs.

First PEL would not exists without `GNU Emacs`_.
Most of the text in the PDF documentation comes from Emacs own documentation,
both from the code docstrings and from the Emacs manuals.

PEL uses the following libraries distributed with GNU Emacs and several others
listed below.  The list is unfortunately incomplete as it grows continuously
when PEL supports new packages.

#. `bookmark    <https://github.com/emacs-mirror/emacs/blob/master/lisp/bookmark.el>`_
#. `cc-vars     <https://github.com/emacs-mirror/emacs/blob/master/lisp/progmodes/cc-vars.el>`_
#. `cua-rect    <https://github.com/emacs-mirror/emacs/blob/master/lisp/emulation/cua-rect.el>`_
#. `delsel      <https://github.com/emacs-mirror/emacs/blob/master/lisp/delsel.el>`_
#. `elint       <https://github.com/emacs-mirror/emacs/blob/master/lisp/emacs-lisp/elint.el>`_
#. `ert library <https://github.com/emacs-mirror/emacs/blob/master/lisp/emacs-lisp/ert.el>`_
#. `ffap        <https://github.com/emacs-mirror/emacs/blob/master/lisp/ffap.el>`_
#. `flyspell    <https://github.com/emacs-mirror/emacs/blob/master/lisp/textmodes/flyspell.el>`_
#. `goto-addr   <https://github.com/emacs-mirror/emacs/blob/master/lisp/net/goto-addr.el>`_
#. `hippie-exp  <https://github.com/emacs-mirror/emacs/blob/master/lisp/hippie-exp.el>`_
#. `hl-line     <https://github.com/emacs-mirror/emacs/blob/master/lisp/hl-line.el>`_
#. `ido         <https://github.com/emacs-mirror/emacs/blob/master/lisp/ido.el>`_
#. `imenu       <https://github.com/emacs-mirror/emacs/blob/master/lisp/imenu.el>`_
#. `isearch     <https://github.com/emacs-mirror/emacs/blob/master/lisp/isearch.el>`_
#. `ispell      <https://github.com/emacs-mirror/emacs/blob/master/lisp/textmodes/ispell.el>`_
#. `kmacro      <https://github.com/emacs-mirror/emacs/blob/master/lisp/kmacro.el>`_
#. `lisp-mode   <https://github.com/emacs-mirror/emacs/blob/master/lisp/emacs-lisp/lisp-mode.el>`_
#. `newcomment  <https://github.com/emacs-mirror/emacs/blob/master/lisp/newcomment.el>`_
#. `org         <https://github.com/emacs-mirror/emacs/blob/master/lisp/org/org.el>`_
#. `paragraphs  <https://github.com/emacs-mirror/emacs/blob/master/lisp/textmodes/paragraphs.el>`_
#. `re-builder  <https://github.com/emacs-mirror/emacs/blob/master/lisp/emacs-lisp/re-builder.el>`_
#. `rect        <https://github.com/emacs-mirror/emacs/blob/master/lisp/rect.el>`_
#. `rst         <https://github.com/emacs-mirror/emacs/blob/master/lisp/textmodes/rst.el>`_
#. `simple      <https://github.com/emacs-mirror/emacs/blob/master/lisp/simple.el>`_
#. `subr-x      <https://github.com/emacs-mirror/emacs/blob/master/lisp/emacs-lisp/subr-x.el>`_
#. `subword     <https://github.com/emacs-mirror/emacs/blob/master/lisp/progmodes/subword.el>`_
#. `skeleton    <https://github.com/emacs-mirror/emacs/blob/master/lisp/skeleton.el>`_
#. `time-stamp  <https://github.com/emacs-mirror/emacs/blob/master/lisp/time-stamp.el>`_
#. `tempo       <https://github.com/emacs-mirror/emacs/blob/master/lisp/tempo.el>`_
#. `thingatpt   <https://github.com/emacs-mirror/emacs/blob/master/lisp/thingatpt.el>`_
#. `uniquify    <https://github.com/emacs-mirror/emacs/blob/master/lisp/uniquify.el>`_
#. `webjump     <https://github.com/emacs-mirror/emacs/blob/master/lisp/net/webjump.el>`_
#. `winner      <https://github.com/emacs-mirror/emacs/blob/master/lisp/winner.el>`_


.. _GNU Emacs: https://www.gnu.org/software/emacs/


It also provides access to the features of the libraries listed in the
following table when the corresponding PEL user option is set to **t**.
This table is currently incomplete.  The link to the original projects are
available in the source code and inside the customization buffers.

============================= ==========================
Package Name & Archive link   Archive Site
============================= ==========================
ace-link_                     MELPA_
ace-window_                   MELPA_
adoc-mode_                    MELPA_
`ag`_                         MELPA_
alchemist_                    MELPA_
all-the-icons_                MELPA_
all-the-icons-ibuffer_        MELPA_
all-the-icons-dired_          MELPA_
all-the-icons-ivy_            MELPA_
anzu_                         MELPA_
apples-mode_                  MELPA_
ascii-table_                  MELPA_
auto-complete_                MELPA_
`avy`_                        MELPA_
`bind-key`_                   MELPA_
`bm`_                         MELPA_
`c-eldoc`_                    MELPA_
`cargo`_                      MELPA_
centimacro_                   MELPA_ (see [#centimacro1]_)
`company`_                    MELPA_
counsel_                      MELPA_
d-mode_                       MELPA_
`desktop+`_                   MELPA_
`desktop-registry`_           MELPA_
`dired-narrow`_               MELPA_
`dumb-jump`_                  MELPA_
`edts`_                       MELPA_
elixir-mode_                  MELPA_
elmacro_                      MELPA_
elpy_                         MELPA_
erlang.el_                    MELPA_
esup_                         MELPA_
expand-region_                MELPA_
flycheck_                     MELPA_
flycheck-plantuml_            MELPA_
exunit_                       MELPA_
fill-column-indicator_        MELPA_
forth-mode_                   MELPA_
framemove_                    Emacsmirror_
free-keys_                    MELPA_
goto-last-change_             MELPA_
graphviz-dot-mode_            MELPA_
helm_                         MELPA_
`hide-comnt.el`_              Emacsmirror_
highlight-defined_            MELPA_
`hydra`_                      MELPA_
iedit_                        MELPA_
ivy_                          MELPA_
julia-mode_                   MELPA_
julia-snail_                  MELPA_
js2-mode_                     MELPA_
keycast_                      MELPA_
key-chord_                    MELPA_
key-seq_                      MELPA_
lfe-mode_                     MELPA_
lice_                         MELPA_
lispy_                        MELPA_
lpy_                          MELPA_
macrostep_                    MELPA_
multiple-cursors_             MELPA_
Magit_                        MELPA_
Monky_                        MELPA_
neotree_                      MELPA_
nhexl-mode_                   ELPA_
parinfer_                     MELPA_
pcre2el_                      MELPA_
`plantuml-mode.el`_           MELPA_
popup_                        MELPA-STABLE_
popup-kill-ring_              MELPA_
projectile_                   MELPA_
python-mode_                  MELPA_
racer_                        MELPA_
rainbow-delimiters_           MELPA_
regex-tool_                   MELPA_
rexx-mode_                    `EmacsAttics`_
`ripgrep.el`_                 MELPA_
`rg`_                         MELPA_
rust-mode_                    MELPA_
slime_                        MELPA_
smart-dash_                   MELPA_
smooth-scrolling_             MELPA_
sr-speedbar_                  MELPA_
swiper_                       MELPA_
undo-tree_                    ELPA_
v-mode_                       MELPA_
visual-regexp_                MELPA_
visual-regexp-steroids_       MELPA_
vline_                        Emacsmirror_
vterm_                        MELPA_
which-key_                    MELPA_
`xr`_                         ELPA_
yasnippet_                    MELPA_
yasnippet-snippets_           MELPA_
ztree_                        MELPA_
============================= ==========================


For developing PEL, the following extra packages are used.

============================= ==========================
Package Name & Archive link   Archive Site
============================= ==========================
benchmark-init_               MELPA_
elisp-lint_                   MELPA_
package-lint_                 MELPA_
============================= ==========================

Thanks to everyone that has worked in the software listed above.
Hopefully you will find PEL useful for using these packages.

*Note*: the fill-column-indicator_ package is not required
when running Emacs 27.1 and later versions.

Also, note that several ideas came from various blogs and discussion on the web.
There are references the these pages inside the PDF tables in the reference
sections, or also inside this manual.  This includes the following (non
exhaustive list):

- Jason Blevins blog: `Integrating OS X and Emacs Dired`_ .


.. _Integrating OS X and Emacs Dired: https://jblevins.org/log/dired-open

.. [#centimacro1] Currently using `my fork of centimacro`_ until a bug fix I submitted
                  gets integrated.
.. ---------------------------------------------------------------------------

.. References

.. _ace-link:                  https://melpa.org/#/ace-link
.. _ace-window:
.. _ace-window package:        https://melpa.org/#/ace-window
.. _adoc-mode:                 https://melpa.org/#/adoc-mode
.. _ag:                        https://melpa.org/#/ag
.. _all-the-icons:             https://melpa.org/#/all-the-icons
.. _all-the-icons-ibuffer:     https://melpa.org/#/all-the-icons-ibuffer
.. _all-the-icons-dired:       https://melpa.org/#/all-the-icons-dired
.. _all-the-icons-ivy:         https://melpa.org/#/all-the-icons-ivy
.. _alchemist:                 https://melpa.org/#/alchemist
.. _ascii-table:               https://melpa.org/#/ascii-table
.. _apples-mode:               https://melpa.org/#/apples-mode
.. _anzu:                      https://melpa.org/#/anzu
.. _auto-complete:
.. _Auto Complete:
.. _auto-complete package:     https://melpa.org/#/auto-complete
.. _avy:                       https://melpa.org/#/avy
.. _MELPA:                     https://melpa.org/
.. _MELPA-STABLE:              https://stable.melpa.org/
.. _bind-key:                  https://melpa.org/#/bind-key
.. _benchmark-init:            https://melpa.org/#/benchmark-init
.. _visible bookmarks:
.. _bm:                        https://melpa.org/#/bm
.. _cargo:                     https://melpa.org/#/cargo
.. _centimacro:                https://melpa.org/#/centimacro
.. _company:                   https://melpa.org/#/company
.. _counsel:                   https://melpa.org/#/counsel
.. _c-eldoc:                   https://melpa.org/#/?q=c-eldoc
.. _d-mode:                    https://melpa.org/#/?q=d-mode
.. _desktop+:                  https://melpa.org/#/?q=desktop+
.. _desktop-registry:          https://melpa.org/#/?q=desktop-registry
.. _dired-narrow:              https://melpa.org/#/dired-narrow
.. _dumb-jump:                 https://melpa.org/#/dumb-jump
.. _edts:                      https://melpa.org/#/edts
.. _elisp-lint:                https://melpa.org/#/elisp-lint
.. _elixir-mode:               https://melpa.org/#/elixir-mode
.. _elmacro:                   https://melpa.org/#/elmacro
.. _elpy:                      https://melpa.org/#/elpy
.. _erlang.el:                 https://melpa.org/#/erlang
.. _esup:                      https://melpa.org/#/esup
.. _exunit:                    https://melpa.org/#/exunit
.. _expand-region:             https://melpa.org/#/expand-region
.. _fill-column-indicator:     https://melpa.org/#/fill-column-indicator
.. _flycheck:                  https://melpa.org/#/flycheck
.. _flycheck-plantuml:         https://melpa.org/#/flycheck-plantuml
.. _forth-mode:                https://melpa.org/#/forth-mode
.. _free-keys:                 https://melpa.org/#/free-keys
.. _goto-last-change:          https://melpa.org/#/goto-last-change
.. _graphviz-dot-mode:         https://melpa.org/#/graphviz-dot-mode
.. _highlight-defined:         https://melpa.org/#/highlight-defined
.. _helm:                      https://melpa.org/#/helm-core
.. _hydra:                     https://melpa.org/#/hydra
.. _iedit:                     https://melpa.org/#/iedit
.. _ivy:                       https://melpa.org/#/ivy
.. _julia-mode:                https://melpa.org/#/julia-mode
.. _julia-snail:               https://melpa.org/#/julia-snail
.. _js2-mode:                  https://melpa.org/#/js2-mode
.. _keycast:                   https://melpa.org/#/keycast
.. _key-chord:                 https://melpa.org/#/key-chord
.. _key-seq:                   https://melpa.org/#/key-seq
.. _lfe-mode:                  https://melpa.org/#/lfe-mode
.. _lice:                      https://melpa.org/#/lice
.. _lispy:                     https://melpa.org/#/lispy
.. _lpy:                       https://melpa.org/#/lpy
.. _macrostep:                 https://melpa.org/#/macrostep
.. _multiple-cursors:          https://melpa.org/#/multiple-cursors
.. _neotree:                   https://melpa.org/#/neotree
.. _nhexl-mode:                https://elpa.gnu.org/packages/nhexl-mode.html
.. _package-lint:              https://melpa.org/#/package-lint
.. _parinfer:                  https://melpa.org/#/parinfer
.. _pcre2el:                   https://melpa.org/#/pcre2el
.. _plantuml-mode.el:          https://melpa.org/#/plantuml-mode
.. _popup:                     https://stable.melpa.org/#/popup
.. _popup-kill-ring:           https://melpa.org/#/popup-kill-ring
.. _projectile:                https://melpa.org/#/projectile
.. _python-mode:               https://melpa.org/#/python-mode
.. _racer:                     https://melpa.org/#/racer
.. _rainbow-delimiters:        https://melpa.org/#/rainbow-delimiters
.. _regex-tool:                https://melpa.org/#/regex-tool
.. _rexx-mode:                 https://github.com/emacsattic/rexx-mode
.. _rg:                        https://melpa.org/#/rg
.. _ripgrep.el:                https://melpa.org/#/ripgrep
.. _rust-mode:                 https://melpa.org/#/rust-mode
.. _slime:                     https://melpa.org/#/slime
.. _slime package:             https://melpa.org/#/slime
.. _smart-dash:                https://melpa.org/#/smart-dash
.. _smooth-scrolling:          https://melpa.org/#/smooth-scrolling
.. _sr-speedbar:               https://melpa.org/#/sr-speedbar
.. _swiper:                    https://melpa.org/#/swiper
.. _undo-tree:                 https://elpa.gnu.org/packages/undo-tree.html
.. _v-mode:                    https://melpa.org/#/v-mode
.. _visual-regexp:             https://melpa.org/#/visual-regexp
.. _visual-regexp-steroids:    https://melpa.org/#/visual-regexp-steroids
.. _vterm:                     https://melpa.org/#/vterm
.. _which-key:                 https://github.com/justbur/emacs-which-key#readme
.. _xr:                        https://elpa.gnu.org/packages/xr.html
.. _yasnippet:                 https://melpa.org/#/yasnippet
.. _yasnippet-snippets:        https://melpa.org/#/yasnippet-snippets
.. _ztree:                     https://melpa.org/#/ztree

.. _framemove:                 https://www.emacswiki.org/emacs/FrameMove
.. _vline:                     https://www.emacswiki.org/emacs/VlineMode
.. _SBCL:                      https://en.wikipedia.org/wiki/Steel_Bank_Common_Lisp
.. _ELPA:                      https://elpa.gnu.org
.. _Emacsmirror:               https://github.com/emacsmirror
.. _EmacsAttics:               https://github.com/emacsattic



.. _my fork of centimacro: https://github.com/pierre-rouleau/centimacro

.. _Abbreviations:                            https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/abbreviations.pdf
.. _Align:                                    https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/align.pdf
.. _AsciiDoc support:                         https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/asciidoc.pdf
.. _Auto-Completion:                          https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/auto-completion.pdf
.. _Autosave & Backups:                       https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/autosave-backup.pdf
.. _Bookmarks:                                https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/bookmarks.pdf
.. _Buffers:                                  https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/buffers.pdf
.. _CUA Mode Support:                         https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/cua.pdf
.. _Case Conversion:                          https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/case-conversion.pdf
.. _Closing and Suspending:                   https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/closing-suspending.pdf
.. _Comments:                                 https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/comments.pdf
.. _Counting:                                 https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/counting.pdf
.. _Cross-Referencing:                        https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/xref.pdf
.. _Cursor:                                   https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/cursor.pdf
.. _Cut, Delete, Copy and Paste:              https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/cut-paste.pdf
.. _Diff and Merge:                           https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/diff-merge.pdf
.. _Dired:                                    https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/mode-dired.pdf
.. _Display Lines:                            https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/display-lines.pdf
.. _Document Legend:                          https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/-legend.pdf
.. _Drawing:                                  https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/drawing.pdf
.. _Enriched Text:                            https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/enriched-text.pdf
.. _F11 Keys:                                 https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/keys-f11.pdf
.. _Faces and Fonts:                          https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/faces-fonts.pdf
.. _Fast Startup:                             https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/fast-startup.pdf
.. _File Management:                          https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/file-mngt.pdf
.. _File and Directory Local Variables:       https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/file-variables.pdf
.. _Filling and Justification:                https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/filling-justification.pdf
.. _Frames:                                   https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/frames.pdf
.. _Function Keys:                            https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/keys-fn.pdf
.. _Github rendering of the Buffers PDF:      pdf/buffers.pdf
.. _Graphviz Dot:                             https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/graphviz-dot.pdf
.. _Grep:                                     https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/grep.pdf
.. _Help PDF:
.. _Help:                                     https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/help.pdf
.. _Hide/Show Code Block:                     https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/hide-show-code.pdf
.. _Highlight:                                https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/highlight.pdf
.. _Hooks:                                    https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/hooks.pdf
.. _Indentation:                              https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/indentation.pdf
.. _Input Completion PDF:
.. _Input Completion:                         https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/completion-input.pdf
.. _Input Method:                             https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/input-method.pdf
.. _Inserting Text:                           https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/inserting-text.pdf
.. _Key Chords:                               https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/key-chords.pdf
.. _Keyboard Macros:                          https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/keyboard-macros.pdf
.. _Marking:                                  https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/marking.pdf
.. _Menus:                                    https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/menus.pdf
.. _Mercurial:                                https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/vcs-mercurial.pdf
.. _Mode Line PDF:                            https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/mode-line.pdf
.. _Mouse:                                    https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/mouse.pdf
.. _Narrowing:                                https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/narrowing.pdf

.. _⅀ Navigation:
.. _⅀ Navigation PDF:
.. _Navigation:                               https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/navigation.pdf
.. _Number Keypad:                            https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/numkeypad.pdf
.. _Outline:                                  https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/outline.pdf
.. _PEL Index PDF:                            https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/-index.pdf
.. _PEL Key Maps:                             https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/-pel-key-maps.pdf
.. _Packages:                                 https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/packages.pdf
.. _Projectile Project Interaction Manager:   https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/projectile.pdf
.. _Rectangles:                               https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/rectangles.pdf
.. _Registers:                                https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/registers.pdf
.. _Scrolling:                                https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/scrolling.pdf
.. _Search and Replace:                       https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/search-replace.pdf
.. _Sessions:                                 https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/sessions.pdf
.. _Shells:                                   https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/shells.pdf
.. _Sorting:                                  https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/sorting.pdf
.. _Speedbar:                                 https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/speedbar.pdf
.. _Spell Checking:                           https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/spell-checking.pdf
.. _Syntax Check PDF:
.. _Syntax Check:                             https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/syntax-checking.pdf
.. _Templates:                                https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/templates.pdf
.. _Text-modes:                               https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/text-modes.pdf
.. _Transpose:                                https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/transpose.pdf
.. _Treemacs:                                 https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/treemacs.pdf
.. _Undo, Redo, Repeat and Prefix Arguments:  https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/undo-redo-repeat.pdf

.. _⅀ Customize:
.. _Customization PDF:
.. _User Option Customization:                https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/customize.pdf
.. _Web:                                      https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/web.pdf
.. _Whitespaces:                              https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/whitespaces.pdf
.. _Windows:                                  https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/windows.pdf
.. _Ubuntu 16.04 Linux Desktop Keys on macOS Host:  https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/ubuntu-16-04-desktop-keys.pdf
.. _ISO-8601 standard format:                 https://en.wikipedia.org/wiki/ISO_8601



.. _ERT:                                      https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/ert.pdf
.. _ibuffer-mode:                             https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/ibuffer-mode.pdf
.. _macOS Terminal settings:                  https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/macOS-terminal-settings.pdf

.. _Org mode:                                 https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/mode-org-mode.pdf
.. _reStructuredText mode:                    https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/mode-rst.pdf
.. _Emacs Modifier Keys:
.. _Modifier Keys:                            https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/modifier-keys.pdf
.. _Apple-Script:                             https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/pl-applescript.pdf
..  C++
.. _C Language PDF:                           https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/pl-c.pdf

..  Common Lisp
.. _Emacs Lisp Types:                         https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/emacs-lisp-types.pdf
.. _Lispy mode support:                       https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/plm-lispy.pdf
..  D
..  elixir
..  Emacs Lisp
..  _Erlang Language PDF:                     https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/pl-erlang.pdf

..  forth
..  python
..  REXX
.. _PlantUML-Mode:                            https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/plantuml.pdf



.. -----------------------------------------------------------------------------

..
       Local Variables:
       time-stamp-start: "^:Modified:[ \t]+\\\\?"
       time-stamp-end:   "\\.$"
       End:
